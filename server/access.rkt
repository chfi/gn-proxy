#lang racket

(require racket/set
         threading
         "db.rkt"
         "groups.rkt"
         "privileges.rkt"
         "resource.rkt")



(define (pl-dataset-data dset)
  (define name 'data)
  (define edit (cons 'edit
                     (lambda (f)
                       (set-dataset-data! dset (f (dataset-data dset))))))
  (define view (cons 'view
                     (lambda () (dataset-data dset))))
  (define no-access (cons 'no-access
                          (lambda () 'no-access)))
  (cons name (list no-access view edit)))

(define (pl-dataset-data2 dset)
  `(data (no-access . ,(λ () 'no-access))
         (view . ,(λ ()  (dataset-data dset)))
         (edit . ,(λ (f) (set-dataset-data! dset (f (dataset-data dset)))))))


(define (pl-dataset-desc dset)
  (define name 'desc)
  (define edit (cons 'edit
                     (lambda (f)
                       (set-dataset-desc! dset (f (dataset-desc dset))))))
  (define view (cons 'view
                     (lambda () "this is some description alright")))
  (define no-access (cons 'no-access
                          (lambda () 'no-access)))
  (cons name (list no-access view edit)))

(define (pl-resource-admin resource)
  (define name 'admin)
  (define edit-admins (cons 'edit-admins
                            'undefined))
  (define edit-access (cons 'edit-access
                            'undefined))
  (define not-admin (cons 'not-admin
                          'undefined))
  (cons name (list not-admin edit-access edit-admins)))

;; (struct resource (name owner content plines group-masks))

(struct data-file (path) #:transparent)
(struct data-db (pid) #:transparent)

(define plines
  (list (pl-dataset-data empty)
        (pl-dataset-desc "test desc")
        (pl-resource-admin empty)))



;; ; Given a resource and a user, get the masks for that user based
;; ; on the per-group masks in the resource privileges.
;; (define (user-masks res u)
;;   (define masks (map (curry dict-ref (resource-group-masks res))
;;                      (map group-id (groups-by-user u))))
;;   ;; masks)
;;   (if (empty? masks)
;;       (list (minimum-access-mask (resource-plines res)))
;;       masks))

;; ; Given a resource and a mask, calculate the actual mask in case the
;; ; mask has admin privileges. Admins have the maximum access privileges,
;; ; except their admin privileges may be limited.
;; (define (admin-mask res m)
;;   (define admin-level (dict-ref m 'admin))
;;   (if (eq? admin-level 'not-admin)
;;       m
;;       (dict-set (maximum-access-mask (resource-plines res))
;;                 'admin
;;                 admin-level)))

;; ; The owner of a resource has complete access.
;; (define (owner-mask res)
;;   (maximum-access-mask (resource-plines res)))

;; ; Given a resource and a user, calculate the user's canonical access mask
;; ; based on the user's group membership, whether or not they're an admin,
;; ; and whether or not they're the resource owner.

;; ; TODO: Support default minimum access levels based on if the resource
;; ; is public or private (perhaps just a group mask keyed to the global
;; ; user/guest group?)
;; (define (user-canonical-mask res u)
;;   (if (eq? u (resource-owner res))
;;       (owner-mask res)
;;       (admin-mask res
;;                   (apply mask-join
;;                          (resource-plines res)
;;                          (user-masks res u)))))


;; ; Groups and users are the second, for now, a user is simply an ID
;; (struct user (id name))

;; ; Placeholder global user "DB" with some entries for testing
;; (define user-db
;;   (list (user 1 "admin1")
;;         (user 2 "user1")
;;         (user 3 "user2")
;;         (user 4 "admin2")))

;; (define (user-by-id id)
;;   (findf (lambda (u) (eq? id (user-id u))) user-db))

;; (define (same-user? a b)
;;   (equal? (user-id a) (user-id b)))

;; ;A `group` is a collection of users of different levels, including a
;; ; non-empty set of admin users
;; (struct group (id admins members))

;; ; Placeholder global group "DB", will be replaced by an actual DB
;; ; system etc.
;; (define group-db
;;   (mutable-set))


;; (set-add! group-db (group 0
;;                           (set (user-by-id 1))
;;                           (set (user-by-id 3))))

;; (define (group-by-id id)
;;   (findf (lambda (g) (eq? id (group-id g)))
;;          (set->list group-db)))

;; (define (groups-by-user u)
;;   (filter (lambda (g) (has-user? g u))
;;           (set->list group-db)))

;; ;
;; (define (all-members g)
;;   (set->list (set-union (group-admins g)
;;                         (group-members g))))

;; (define (mk-group id owner)
;;   (group (set owner) (list->set empty)))

;; ; Placeholder function for adding a group to the global DB; only works
;; ; if the given user ID exists in the user Db
;; (define (add-group-for-owner owner-id)
;;   (define user (user-by-id owner-id))
;;   (when user
;;     (set-add! group-db (group (set-count group-db)
;;                               (set user)
;;                               (set)))))

(add-group-for-owner 4)
(add-group-for-owner 3)

;; (define (has-user? g u)
;;   (or (set-member? (group-admins g) u)
;;       (set-member? (group-members g) u)))

;; (define (add-member g u)
;;   (if (has-user? g u)
;;       g
;;       (struct-copy group
;;                    g
;;                    [members (set-add (group-members g) u)])))

;; (define (del-member g u)
;;   (struct-copy group
;;                g
;;                [admins (set-remove (group-admins g) u)]
;;                [members (set-remove (group-members g) u)]))

;; (define (make-admin g m)
;;   (if (has-user? g m)
;;       (struct-copy group
;;                    g
;;                    [admins (set-add (group-admins g) m)]
;;                    [members (set-remove (group-members g) m)])
;;       g))


; Resources are named collections of privileges with an owner, and the
; contents that are used by the privilege actions (e.g. URL, dataset
; ID, etc.)
;; (struct resource (name owner content privileges))

;; (define (try-action res user action . args)
;;   (define priv (hash-ref (resource-privileges res)
;;                          action
;;                          (lambda () (raise 'action-not-found))))
;;   (define grp (privilege-group priv))
;;   (if (or (has-user? grp user)
;;           (equal? user (resource-owner res)))
;;       (apply (privilege-action priv) args)
;;       (error 'no-access)))

; Return a list of the privileges, and their respective user groups,
; for a resource
;; (define (resource-list-privileges res)
;;   (hash-map (resource-privileges res)
;;             (lambda (k p)
;;               (cons k
;;                     (map user-id
;;                          (all-members (privilege-group p)))))))

;; (define (admin-privilege group


(struct dataset (desc data) #:mutable)

(struct collection (metadata datasets) #:mutable)


;; (define (mk-dataset name data desc owner)
;;   (define group (mk-group owner))
;;   (define privs (make-hash))
;;   (define dset (dataset desc data))
;;   (hash-set! privs 'view-data
;;              (mk-privilege group
;;                            (lambda () (dataset-data dset))))
;;   (hash-set! privs 'edit-data
;;              (mk-privilege group
;;                            (lambda (f)
;;                              (set-dataset-data! dset (f (dataset-data dset))))))
;;   (hash-set! privs 'view-desc
;;              (mk-privilege group
;;                            (lambda () (dataset-desc dset))))
;;   (hash-set! privs 'edit-desc
;;              (mk-privilege group
;;                            (lambda (f)
;;                              (set-dataset-desc! dset (f (dataset-desc dset))))))
;;   ;; (hash-set! privs 'edit-privileges
;;   ;;            (mk-privilege group
;;   ;;                          (lambda (f)
;;   ;;                            (
;;   (resource name owner dset privs))

;; (define (mk-dataset name




;; (define (mk-resource n o ps)
;;   (resource n o ps))


(define tst-res
  (let ([plines (list (pl-dataset-data empty)
                      (pl-dataset-desc "test desc")
                      (pl-resource-admin empty))])
    (resource "test"
               (user-by-id 1)
               empty
               plines
               (list (cons 1 (maximum-access-mask plines))
                     (cons 2 (minimum-access-mask plines))
                     (cons 0 (list '(data . edit)
                                   '(desc . no-access)
                                   '(admin . not-admin)))))))
