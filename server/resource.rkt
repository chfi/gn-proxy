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
         access-action
         serialize-resource
         deserialize-resource)


;; NB: the `data` field in a resource isn't the data itself,
;; instead it contains whatever data is necessary for the resource's
;; actions. The `type` designates what kind of resource it is, e.g.
;; dataset, collection, etc. The actions available depend on the
;; resource type.
(struct resource
  (name
   owner
   data
   type
   default-mask
   group-masks)
  #:transparent)

;; Serializes a resource into a JSON bytestring for storage in Redis.
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


;; Given a resource and a user ID, derive the access mask for that user
;; based on their group membership as stored in Redis, and return
;; the appropriate access mask.

;; TODO take owner mask into account
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

;; Constructor for file-based resources
(define (new-file-resource name
                           owner-id
                           path
                           meta-key
                           default-mask)
  (resource name
            owner-id
            (hasheq 'path path
                    'metadata meta-key)
            'dataset-file
            default-mask
            (hasheq)))


;; Helper function for setting the access level for a group on a
;; resource. Note that this returns the updated resource, which must
;; then be put into Redis.

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


;; Return the action, as defined by a pair of a branch name and action
;; name, for a given resource, as accessible by the given user.
;; Returns #f if the user does not have access.
(define (access-action dbc res user-id action-pair)
  (let ((branch-id (car action-pair))
        (action-id (cdr action-pair))
        (mask (get-mask-for-user dbc
                                 res
                                 user-id))
        (action-set (dict-ref resource-types (resource-type res))))
    (if (string=? (hash-ref mask branch-id #f)
                  action-id)
        (let* ((branch (hash-ref action-set branch-id))
               (action (assoc action-id branch)))
          (cdr action))
        #f)))

;; The general "no access" action -- may change in the future
(define no-access-action
  (action "no-access"
          (lambda (data params)
            'no-access)
          '()))

;; Actions for file-based resources
(define view-file
  (action "view"
          (lambda (data params)
            (file->string (hash-ref data 'path) #:mode 'text))
          '()))

(define edit-file
  (action "edit"
          (lambda (data
                   params)
            (write-to-file (dict-ref params 'contents)
                           (hash-ref data 'path)
                           #:exists 'replace))
          '(contents)))


;; Placeholder metadata actions; for now the metadata is just a single
;; redis field, will definitely change.

;; TODO the dbc should be passed as a Racket parameter rather than an
;; action param params should be provided as keyword arguments
(define view-metadata
  (action "view"
          (lambda (data
                   params)
            (redis-bytes-get (dict-ref params 'dbc)
                             (hash-ref data 'key)))
          '(dbc)))

(define edit-metadata
  (action "edit"
          (lambda (data
                   params)
            (redis-bytes-set! (dict-ref params 'dbc)
                              (hash-ref data 'key)
                              (dict-ref params 'value)))
          '(dbc value)))

(define dataset-file-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-file)
        (cons "edit" edit-file)))

(define dataset-file-metadata
  (list (cons "no-access" no-access-action)
        (cons "view" view-metadata)
        (cons "edit" edit-metadata)))

;; The action set for file-based dataset resources.
(define dataset-file-actions
  (hasheq 'data dataset-file-data
          'metadata dataset-file-metadata))




(define (select-publish dbc dataset-id trait-name)
  (query-rows dbc
             "SELECT
                      PublishXRef.Id, InbredSet.InbredSetCode, Publication.PubMed_ID,
                      Phenotype.Pre_publication_description, Phenotype.Post_publication_description, Phenotype.Original_description,
                      Phenotype.Pre_publication_abbreviation, Phenotype.Post_publication_abbreviation,
                      Phenotype.Lab_code, Phenotype.Submitter, Phenotype.Owner, Phenotype.Authorized_Users,
                      Publication.Authors, Publication.Title, Publication.Abstract,
                      Publication.Journal, Publication.Volume, Publication.Pages,
                      Publication.Month, Publication.Year, PublishXRef.Sequence,
                      Phenotype.Units, PublishXRef.comments
                FROM
                      PublishXRef, Publication, Phenotype, PublishFreeze, InbredSet
                WHERE
                      PublishXRef.Id = ? AND
                      Phenotype.Id = PublishXRef.PhenotypeId AND
                      Publication.Id = PublishXRef.PublicationId AND
                      PublishXRef.InbredSetId = PublishFreeze.InbredSetId AND
                      PublishXRef.InbredSetId = InbredSet.Id AND
                      PublishFreeze.Id = ?"
             trait-name
             dataset-id))


;; The dataset-geno resource type
;; Currently only read actions

(define (new-geno-resource name
                           owner-id
                           dataset-name
                           trait-name
                           default-mask)
  (resource name
            owner-id
            (hasheq 'dataset dataset-name
                    'trait trait-name)
            'dataset-file
            default-mask
            (hasheq)))

;; TODO this should serialize into JSON to be sent by the REST API
(define (select-geno dbc dataset-name trait-name)
  (query-rows dbc
             "SELECT Geno.name, Geno.chr, Geno.mb, Geno.source2, Geno.sequence
              FROM Geno, GenoFreeze, GenoXRef
              WHERE GenoXRef.GenoFreezeId = GenoFreeze.Id AND
                    GenoXRef.GenoId = Geno.Id AND
                    GenoFreeze.Name = ? AND
                    Geno.Name = ?"
             dataset-name
             trait-name))

(define view-geno
  (action "view"
          (lambda (data
                   params)
            (select-geno (dict-ref params 'dbc)
                         (hash-ref data 'dataset)
                         (hash-ref data 'trait)))
          '(dbc)))

(define dataset-geno-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-geno)))

(define dataset-geno-actions
  (hasheq 'data dataset-geno-data))

;; The global mapping from resource type to action set.
(define resource-types
  (hash 'dataset-file dataset-file-actions
        'dataset-geno dataset-geno-actions))
    ;; future resource types, for reference (c.f. genenetwork datasets etc.)
    ;; dataset-publish
    ;; dataset-probeset
    ;; dataset-geno
    ;; dataset-temp
    ;; collection
