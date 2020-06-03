#lang racket

(require db
         redis
         json
         threading
         racket/file
         "db.rkt"
         "groups.rkt"
         "privileges.rkt"
         "resource/geno.rkt"
         "resource/probe.rkt"
         "resource/probeset.rkt"
         "resource/publish.rkt"
         "resource/util.rkt")

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

(define (add-resource id res)
  (redis-hash-set! (redis-conn)
                   "resources"
                   id
                   (serialize-resource res)))


(define (get-resource id)
  (let ((res (redis-hash-ref (redis-conn)
                             "resources"
                             id)))
    (if (false? res)
        (error (format "Resource not found in Redis: ~a"
                       id))
        (deserialize-resource res))))

;; Given a resource and a user ID, derive the access mask for that user
;; based on their group membership as stored in Redis, and return
;; the appropriate access mask.

;; TODO take owner mask into account
(define (get-mask-for-user resource user-id)
  (let ([group-masks (resource-group-masks resource)]
        [groups (get-groups-by-member (redis-conn) user-id)]
        [initial-mask (if (string=? (resource-owner resource) user-id)
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
;; Returns the no-access-action if the user does not have access.
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

;; Actions for file-based resources
(define view-file
  (action (lambda (data params)
            (file->string (hash-ref data 'path) #:mode 'text))
          '()))

(define edit-file
  (action (lambda (data
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
  (action (lambda (data
                   params)
            (redis-bytes-get (redis-conn)
                             (hash-ref data 'key)))
          '()))

(define edit-metadata
  (action (lambda (data
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
    ;; dataset-temp
    ;; collection
