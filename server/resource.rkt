#lang racket

(require db
         redis
         json
         threading
         "db.rkt"
         "groups.rkt"
         "privileges.rkt")

(provide (struct-out resource)
         ;; user-canonical-mask
         ;; user-masks
         ;; resource-set-mask
         ;; get-actions
         ;; pl-edit-view
         )


; important: the `data` field in a resource isn't the data itself,
; instead it contains whatever data is necessary for the resource's
; actions. The `type` designates what kind of resource it is, e.g.
; dataset, collection, etc. The actions available depend on the
; resource type.
(struct resource (name owner data type default-mask group-masks))

(define (get-resource dbc id)
  (let ((res-json (string->jsexpr (redis-hash-get dbc "resources" id))))
    (resource (dict-ref res-json "name")
              (dict-ref res-json "owner")
              (dict-ref res-json "data") ;; may change
              (dict-ref res-json "type")
              (~> (dict-ref res-json "default_mask")
                  ;; not sure if these conversions are necessary, but
                  ;; i think it is
                  (string->jsexpr))
              (~> (dict-ref res-json "group_masks")
                  (string->jsexpr)))))


;; (define (select-resources dbc)
;;   (query-rows dbc
;;               "select * from resources"))

;; (define (insert-resource dbc name owner data type)
;;   (query-exec dbc
;;               "insert into resources (name, owner_id, resource_data, resource_type)
;;                values (?,?,?,?)"
;;               name
;;               owner
;;               data
;;               type))

(define resource-types
  '(dataset-publish
    dataset-probeset
    dataset-geno
    dataset-temp
    collection))



(define dataset-publish
  #hash(("data" . ("no-access" "view" "edit") )
        ("metadata" . ("no-access" "view" "edit"))
        ("admin" . ("no-access" "edit-access" "edit-admins"))))


(define (select-publish dbc dataset-id trait-name)
  (query-row dbc
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



(define (select-geno dbc dataset-name trait-name)
  (query-row dbc
             "SELECT Geno.name Geno.chr Geno.mb Geno.source2 Geno.sequence
              FROM Geno, GenoFreeze, GenoXRef
              WHERE GenoXRef.GenoFreezeId = GenoFreeze.Id AND
                    GenoXRef.GenoId = Geno.Id AND
                    GenoFreeze.Name = ? AND
                    Geno.Name = ?"
             dataset-name
             trait-name))


; Given a resource and a user, get the masks for that user based
; on the per-group masks in the resource privileges.

; TODO rewrite
;; (define (user-masks res u)
;;   (define masks
;;     (map (lambda (x)
;;            (dict-ref (resource-group-masks res)
;;                      x
;;                      ; TODO the minimum access mask should be defined on a per-resource basis
;;                      (minimum-access-mask (resource-plines res))))
;;          (map group-id (groups-by-user u))))
;;   (if (empty? masks)
;;       (list (minimum-access-mask (resource-plines res)))
;;       masks))


; Given a resource and a mask, calculate the actual mask in case the
; mask has admin privileges. Admins have the maximum access privileges,
; except their admin privileges may be limited.

; TODO rewrite
;; (define (admin-mask res m)
;;   (define admin-level (dict-ref m 'admin))
;;   (if (eq? admin-level 'not-admin)
;;       m
;;       (dict-set (maximum-access-mask (resource-plines res))
;;                 'admin
;;                 admin-level)))

; The owner of a resource has complete access.
(define (owner-mask res)
  (maximum-access-mask (resource-plines res)))

; Given a resource and a user, calculate the user's canonical access mask
; based on the user's group membership, whether or not they're an admin,
; and whether or not they're the resource owner.

; TODO: Support default minimum access levels based on if the resource
; is public or private (perhaps just a group mask keyed to the global
; user/guest group?)

; TODO rewrite
;; (define (user-canonical-mask res u)
;;   (if (eq? u (resource-owner res))
;;       (owner-mask res)
;;       (admin-mask res
;;                   (apply mask-join
;;                          (resource-plines res)
;;                          (user-masks res u)))))

; Given a resource and a user, return all actions the user has access to perform;
; Return format is an alist of alists; it's a subset of the resource's plines field.

; TODO rewrite
;; (define (get-actions res u)
;;   (define mask (user-canonical-mask res u))
;;   (dict-map (resource-plines res)
;;             (λ (k v)
;;               (cons k (reverse
;;                        (memf (λ (x)
;;                               (eq? (dict-ref mask k) (car x)))
;;                             (reverse v)))))))

;; Given a resource, a group ID, and a mask that fits the resource,
;; returns a new resource with the corresponding group's mask updated
;; to the given mask. Returns #f if the mask doesn't fit.

; TODO rewrite
(define (resource-set-mask res gid mask)
  (if (is-mask-for? (resource-plines res) mask)
      (struct-copy resource
                   res
                   [group-masks (dict-set
                                 (resource-group-masks res)
                                 gid
                                 mask)])
      #f))
