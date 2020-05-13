#lang racket

(require db
         redis
         json
         threading
         racket/file
         "db.rkt"
         "groups.rkt"
         "privileges.rkt")

(provide (struct-out resource)
         get-mask-for-user
         get-resource
         resource-set-group-mask
         resource-types
         resource-actions
         serialize-resource
         deserialize-resource)


; important: the `data` field in a resource isn't the data itself,
; instead it contains whatever data is necessary for the resource's
; actions. The `type` designates what kind of resource it is, e.g.
; dataset, collection, etc. The actions available depend on the
; resource type.
(struct resource
  (name
   owner
   data
   type
   default-mask
   group-masks)
  #:transparent)

(define (serialize-resource res)
  (jsexpr->bytes (hash 'name (resource-name res)
                       'owner_id (resource-owner res)
                       'data (resource-data res)
                       'type (symbol->string (resource-type res))
                       'default_mask (resource-default-mask res)
                       'group_masks (resource-group-masks res))))

(define (deserialize-resource res)
  (let ((res-hash (bytes->jsexpr res)))
    (define (parse k)
      (dict-ref res-hash k))
    (resource (parse 'name)
              (parse 'owner_id)
              (parse 'data)
              (string->symbol (parse 'type))
              (parse 'default_mask)
              (parse 'group_masks))))

(define (get-resource dbc id)
  (~> (redis-hash-ref dbc "resources" id)
      (deserialize-resource)))

; TODO take owner mask into account
(define (get-mask-for-user dbc resource user-id)
  (let ([group-masks (resource-group-masks resource)]
        [groups (get-groups-by-member dbc user-id)]
        [default-mask (resource-default-mask resource)])
    (apply mask-join
           (dict-ref resource-types (resource-type resource))
           default-mask
           (for/list ([g groups])
      ; the redis library requires symbols for keys, but the values
      ; are bytestrings...
             (~> (group-id g)
                 (bytes->string/utf-8)
                 (string->symbol)
                 (hash-ref group-masks _ default-mask))))))

(define (new-file-resource name owner-id path meta-key default-mask)
  (resource name
            owner-id
            (hasheq 'path path
                  'metadata meta-key)
            'dataset-file
            default-mask
            (hasheq)))


; grp-id must be given as a symbol
(define (resource-set-group-mask res grp-id mask)
  (if (is-mask-for? (dict-ref resource-types (resource-type res))
                    mask)
      (let* ((old-masks (resource-group-masks res))
             (new-masks (hash-set old-masks
                                  grp-id
                                  mask)))
        (struct-copy resource
                     res
                     [group-masks new-masks]))
      (error 'incompatible-action-mask)))


(struct file-data (path metadata-key))

;; (define (no-access-action)
;;   'nothing)

(define no-access-action
  (action "no-access"
          (lambda ()
            'no-access)
          '()))

;; (define (view-file data)
;;   (file->string (hash-ref data 'path)
;;                 #:mode 'text))

;; (define (edit-file path contents)
;;   (write-to-file contents
;;                  path
;;                  #:exists 'replace))

(define view-file
  (action "view"
          (lambda (data)
            (file->string (hash-ref data 'path) #:mode 'text))
          '()))

(define edit-file
  (action "edit"
          (lambda (data contents)
            (write-to-file contents
                           (hash-ref data 'path)
                           #:exists 'replace))))



;; TODO the dbc should be passed as a Racket parameter rather than an action param


;; params should be provided as keyword arguments
(define view-metadata
  (action "view"
          (lambda (data dbc)
            (redis-bytes-get dbc
                             (hash-ref data 'key)))
          '(dbc)))

(define edit-metadata
  (action "edit"
          (lambda (data dbc value)
            (redis-bytes-set! dbc
                              (hash-ref data 'key)
                              value))
          '(dbc value)))

;; (define (view-metadata dbc key)
;;   (redis-bytes-get dbc key))

;; (define (edit-metadata dbc key value)
;;   (redis-bytes-set! dbc key value))

(define dataset-file-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-file)
        (cons "edit" edit-file)))

(define dataset-file-metadata
  (list (cons "no-access" no-access-action)
        (cons "view" view-metadata)
        (cons "edit" edit-metadata)))

(define dataset-file-actions
  (hasheq 'data dataset-file-data
          'metadata dataset-file-metadata))


; A hash mapping resource types to action sets
(define resource-types
  (hash 'dataset-file dataset-file-actions))
    ;; future resource types, for reference (c.f. genenetwork datasets etc.)
    ;; dataset-publish
    ;; dataset-probeset
    ;; dataset-geno
    ;; dataset-temp
    ;; collection


(define (resource-actions res)
  (hash-ref resource-types
            (resource-type res)))

; The owner of a resource has complete access.
(define (owner-mask res-type)
  (maximum-access-mask (dict-ref resource-types res-type)))

;; (define (select-publish dbc dataset-id trait-name)
;;   (query-row dbc
;;              "SELECT
;;                       PublishXRef.Id, InbredSet.InbredSetCode, Publication.PubMed_ID,
;;                       Phenotype.Pre_publication_description, Phenotype.Post_publication_description, Phenotype.Original_description,
;;                       Phenotype.Pre_publication_abbreviation, Phenotype.Post_publication_abbreviation,
;;                       Phenotype.Lab_code, Phenotype.Submitter, Phenotype.Owner, Phenotype.Authorized_Users,
;;                       Publication.Authors, Publication.Title, Publication.Abstract,
;;                       Publication.Journal, Publication.Volume, Publication.Pages,
;;                       Publication.Month, Publication.Year, PublishXRef.Sequence,
;;                       Phenotype.Units, PublishXRef.comments
;;                 FROM
;;                       PublishXRef, Publication, Phenotype, PublishFreeze, InbredSet
;;                 WHERE
;;                       PublishXRef.Id = ? AND
;;                       Phenotype.Id = PublishXRef.PhenotypeId AND
;;                       Publication.Id = PublishXRef.PublicationId AND
;;                       PublishXRef.InbredSetId = PublishFreeze.InbredSetId AND
;;                       PublishXRef.InbredSetId = InbredSet.Id AND
;;                       PublishFreeze.Id = ?"
;;              trait-name
;;              dataset-id))



;; (define (select-geno dbc dataset-name trait-name)
;;   (query-row dbc
;;              "SELECT Geno.name Geno.chr Geno.mb Geno.source2 Geno.sequence
;;               FROM Geno, GenoFreeze, GenoXRef
;;               WHERE GenoXRef.GenoFreezeId = GenoFreeze.Id AND
;;                     GenoXRef.GenoId = Geno.Id AND
;;                     GenoFreeze.Name = ? AND
;;                     Geno.Name = ?"
;;              dataset-name
;;              trait-name))
