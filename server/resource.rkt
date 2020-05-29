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


;; The Racket JSON library can only transform hashes that have
;; symbol keys -- but Redis only deals with strings and bytestrings.
;; These functions transform the keys of a hash between the two.

(define (hash-symbol->string h)
  (for/hash ([(k v) (in-hash h)])
    (values (~> k
                (symbol->string)
                (string->bytes/utf-8))
            v)))

(define (hash-string->symbol h)
  (for/hash ([(k v) (in-hash h)])
    (values (~> k
                (bytes->string/utf-8)
                (string->symbol))
            v)))

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

(define (add-resource id res)
  (redis-hash-set! (redis-conn)
                   "resources"
                   id
                   (serialize-resource res)))


(define (get-resource id)
  (~> (redis-hash-ref (redis-conn) "resources" id)
      (deserialize-resource)))


;; Given a resource and a user ID, derive the access mask for that user
;; based on their group membership as stored in Redis, and return
;; the appropriate access mask.

;; TODO take owner mask into account
(define (get-mask-for-user resource user-id)
  (let ([group-masks (resource-group-masks resource)]
        [groups (get-groups-by-member (redis-conn) user-id)]
        [initial-mask (if (eq? (resource-owner resource) user-id)
                          (maximum-access-mask
                           (dict-ref resource-types
                                     (resource-type resource)))
                          (resource-default-mask resource))])
    (apply mask-join
           (dict-ref resource-types (resource-type resource))
           initial-mask
           (for/list ([g groups])
      ; the redis library requires symbols for keys, but the values
      ; are bytestrings...
             (~> (group-id g)
                 (bytes->string/utf-8)
                 (string->symbol)
                 (hash-ref group-masks _ initial-mask))))))

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
(define (access-action res
                       action-pair
                       #:user [user-id 'anonymous])
  (let* ((branch-id (car action-pair))
         (action-id (cdr action-pair))
         (mask (if (eq? user-id 'anonymous)
                   (resource-default-mask res)
                   (get-mask-for-user res
                                      user-id)))
         (action-set (apply-mask (dict-ref resource-types
                                           (resource-type res))
                                 mask)))
    (let ((action (assoc action-id (hash-ref action-set branch-id))))
      (if action
          (cdr action)
          no-access-action))))

    ;; (cdr (assoc action-id (hash-ref action-set branch-id)))))

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
            (redis-bytes-get (redis-conn)
                             (hash-ref data 'key)))
          '()))

(define edit-metadata
  (action "edit"
          (lambda (data
                   params)
            (redis-bytes-set! (redis-conn)
                              (hash-ref data 'key)
                              (dict-ref params 'value)))
          '(value)))

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



;; The dataset-publish resource type
;; Currently only read actions

(define (new-publish-resource name
                              owner-id
                              dataset-id
                              trait-name
                              default-mask)
  (resource name
            owner-id
            (hasheq 'dataset dataset-id
                    'trait trait-name)
            'dataset-publish
            default-mask
            (hasheq)))


;; Function that serializes an SQL result row into a stringified JSON
;; array. Probably doesn't work with all SQL types yet!!
(define (sql-result->json query-result)
  (jsexpr->bytes
   (map (lambda (x)
          (if (sql-null? x) 'null x))
        (vector->list query-result))))

(define (select-publish dataset-id trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT
                      PublishXRef.Id, InbredSet.InbredSetCode, Publication.PubMed_ID,
                      Phenotype.Pre_publication_description, Phenotype.Post_publication_description, Phenotype.Original_description,
                      Phenotype.Pre_publication_abbreviation, Phenotype.Post_publication_abbreviation, PublishXRef.mean,
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
             dataset-id)))

(define view-publish
  (action "view"
          (lambda (data
                   params)
            (select-publish (hash-ref data 'dataset)
                            (hash-ref data 'trait)))
          '()))

(define dataset-publish-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-publish)))

(define dataset-publish-actions
  (hasheq 'data dataset-publish-data))

;; The dataset-geno resource type
;; Currently only read actions

(define (new-geno-resource name
                           owner-id
                           dataset-name
                           default-mask)
  (resource name
            owner-id
            (hasheq 'dataset dataset-name)
            'dataset-geno
            default-mask
            (hasheq)))

(define (select-geno dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT Geno.Name, Geno.Chr, Geno.Mb, Geno.Source2, Geno.Sequence
                FROM Geno, GenoFreeze, GenoXRef
                WHERE GenoXRef.GenoFreezeId = GenoFreeze.Id AND
                      GenoXRef.GenoId = Geno.Id AND
                      GenoFreeze.Id = ? AND
                      Geno.Name = ?"
                dataset-name
                trait-name)))

(define view-geno
  (action "view"
          (lambda (data
                   params)
            (select-geno (hash-ref data 'dataset)
                         (dict-ref params 'trait)))
          '(trait)))

(define dataset-geno-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-geno)))

(define dataset-geno-actions
  (hasheq 'data dataset-geno-data))

;; The dataset-probeset resource type
;; Currently only read actions

(define (new-probeset-resource name
                               owner-id
                               dataset-name
                               default-mask)
  (resource name
            owner-id
            (hasheq 'dataset dataset-name)
            'dataset-probeset
            default-mask
            (hasheq)))

(define (select-probeset dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
                "SELECT ProbeSet.Name, ProbeSet.Symbol, ProbeSet.description, ProbeSet.Probe_Target_Description,
                        ProbeSet.Chr, ProbeSet.Mb, ProbeSet.alias, ProbeSet.GeneId, ProbeSet.GenbankId, ProbeSet.UniGeneId,
                        ProbeSet.OMIM, ProbeSet.RefSeq_TranscriptId, ProbeSet.BlatSeq, ProbeSet.TargetSeq, ProbeSet.ChipId,
                        ProbeSet.comments, ProbeSet.Strand_Probe, ProbeSet.Strand_Gene, ProbeSet.ProteinID, ProbeSet.UniProtID,
                        ProbeSet.Probe_set_target_region, ProbeSet.Probe_set_specificity, ProbeSet.Probe_set_BLAT_score,
                        ProbeSet.Probe_set_Blat_Mb_start, ProbeSet.Probe_set_Blat_Mb_end, ProbeSet.Probe_set_strand,
                        ProbeSet.Probe_set_Note_by_RW, ProbeSet.flag
                FROM ProbeSet, ProbeSetFreeze, ProbeSetXRef
                WHERE
                        ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id AND
                        ProbeSetXRef.ProbeSetId = ProbeSet.Id AND
                        ProbeSetFreeze.Id = ? AND
                        ProbeSet.Name = ?"
                dataset-name
                trait-name)))

(define view-probeset
  (action "view"
          (lambda (data
                   params)
            (select-probeset (hash-ref data 'dataset)
                          (dict-ref params 'trait)))
          '(trait)))

(define dataset-probeset-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-probeset)))

(define dataset-probeset-actions
  (hasheq 'data dataset-probeset-data))

;; The dataset-probe resource type
;; Currently only read actions


(define (new-probe-resource name
                            owner-id
                            dataset-name
                            trait-name
                            default-mask)
  (resource name
            owner-id
            (hasheq 'dataset dataset-name
                    'trait trait-name)
            'dataset-probe
            default-mask
            (hasheq)))

(define (select-probe dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT Probe.Sequence, Probe.Name
                FROM Probe, ProbeSet, ProbeSetFreeze, ProbeSetXRef
                WHERE ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id AND
                      ProbeSetXRef.ProbeSetId = ProbeSet.Id AND
                      ProbeSetFreeze.Name = ? AND
                      ProbeSet.Name = ? AND
                      Probe.ProbeSetId = ProbeSet.Id order by Probe.SerialOrder"
                dataset-name
                trait-name)))

(define view-probe
  (action "view"
          (lambda (data
                   params)
            (select-probe (hash-ref data 'dataset)
                          (hash-ref data 'trait)))
          '()))

(define dataset-probe-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-probe)))

(define dataset-probe-actions
  (hasheq 'data dataset-probe-data))


;; Helpers for adding new resources to Redis

(define (add-probe-resource id
                            name
                            dataset-name
                            trait-name)
  (define mask
    (hash 'data "view"))
  (let ((res (new-probe-resource name
                                 0
                                 dataset-name
                                 trait-name
                                 mask)))
    (add-resource id res)))

(define (add-probeset-resource id
                               name
                               dataset-name)
  (define mask
    (hash 'data "view"))
  (let ((res (new-probeset-resource name
                                    0
                                    dataset-name
                                    mask)))
    (add-resource id res)))

(define (add-publish-resource id
                              name
                              dataset-name
                              trait-name)
  (define mask
    (hash 'data "view"))
  (let ((res (new-publish-resource name
                                   0
                                   dataset-name
                                   trait-name
                                   mask)))
    (add-resource id res)))

(define (add-geno-resource id
                           name
                           dataset-name)
  (define mask
    (hash 'data "view"))
  (let ((res (new-geno-resource name
                                0
                                dataset-name
                                mask)))
    (add-resource id res)))

;; The global mapping from resource type to action set.
(define resource-types
  (hash 'dataset-file dataset-file-actions
        'dataset-publish dataset-publish-actions
        'dataset-geno dataset-geno-actions
        'dataset-probeset dataset-probeset-actions
        'dataset-probe dataset-probe-actions))
    ;; future resource types, for reference (c.f. genenetwork datasets etc.)
    ;; dataset-publish
    ;; dataset-probeset
    ;; dataset-probe
    ;; dataset-geno
    ;; dataset-temp
    ;; collection
