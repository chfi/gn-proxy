#lang racket

(require racket/set)


; The `privilege` is one of the two fundamental building blocks, and
; is a group of users together with an `action`, which is a function
; that can be called by the privileged users.
(struct privilege (group action))

(define (mk-privilege g f)
  (privilege g f))


; A `privilege-line` is a set of privileges that have a monotonically
; increasing structure, e.g. no access <- can read <- can read & write
;; (struct privilege-line (actions))
(define (privilege-line name actions)
  '(name . actions))


(define (pl-dataset-data dset)
  (define name 'data)
  (define edit (cons 'edit
                     (lambda (f)
                       (set-dataset-data! dset (f (dataset-data dset))))))
  (define view (cons 'view
                     (lambda () (dataset-data dset))))
  (define no-access (cons 'no-access
                          (lambda () 'no-access)))
  (cons name '(edit view no-access)))


(define (pl-dataset-desc dset)
  (define name 'desc)
  (define edit (cons 'edit
                     (lambda (f)
                       (set-dataset-desc! dset (f (dataset-desc dset))))))
  (define view (cons 'view
                     (lambda () (dataset-desc dset))))
  (define no-access (cons 'no-access
                          (lambda () 'no-access)))
  (cons name '(edit view no-access)))

(define (combine-lines ls)
  (apply list ls))

(define (default-access pls)
  (dict-map pls
            (lambda (k v)
              (if (and (dict? pls) (list? pls))
                  (cons k (car (last v)))
                  (error 'not-privilege-line)))))


;; (define (pl-dataset-data dset)
;;   (privilege-line 'data '('edit-data

;; (define (privilege-poset lines))


; Groups and users are the second, for now, a user is simply an ID
(struct user (id))

(define (same-user? a b)
  (equal? (user-id a) (user-id b)))

;A `group` is a collection of users of different levels, including a
; non-empty set of admin users
(struct group (admins members))

(define (all-members g)
  (set->list (set-union (group-admins g)
                        (group-members g))))

(define (mk-group owner)
  (group (set owner) (list->set empty)))

(define (has-user? g u)
  (or (set-member? (group-admins g) u)
      (set-member? (group-members g) u)))

(define (add-member g u)
  (if (has-user? g u)
      g
      (struct-copy group
                   g
                   [members (set-add (group-members g) u)])))

(define (del-member g u)
  (struct-copy group
               g
               [admins (set-remove (group-admins g) u)]
               [members (set-remove (group-members g) u)]))

(define (make-admin g m)
  (if (has-user? g m)
      (struct-copy group
                   g
                   [admins (set-add (group-admins g) m)]
                   [members (set-remove (group-members g) m)])
      g))


; Resources are named collections of privileges with an owner, and the
; contents that are used by the privilege actions (e.g. URL, dataset
; ID, etc.)
(struct resource (name owner content privileges))

(define (try-action res user action . args)
  (define priv (hash-ref (resource-privileges res)
                         action
                         (lambda () (raise 'action-not-found))))
  (define grp (privilege-group priv))
  (if (or (has-user? grp user)
          (equal? user (resource-owner res)))
      (apply (privilege-action priv) args)
      (error 'no-access)))

; Return a list of the privileges, and their respective user groups,
; for a resource
(define (resource-list-privileges res)
  (hash-map (resource-privileges res)
            (lambda (k p)
              (cons k
                    (map user-id
                         (all-members (privilege-group p)))))))

;; (define (admin-privilege group


(struct dataset (desc data) #:mutable)

(struct collection (metadata datasets) #:mutable)

(define (mk-dataset name data desc owner)
  (define group (mk-group owner))
  (define privs (make-hash))
  (define dset (dataset desc data))
  (hash-set! privs 'view-data
             (mk-privilege group
                           (lambda () (dataset-data dset))))
  (hash-set! privs 'edit-data
             (mk-privilege group
                           (lambda (f)
                             (set-dataset-data! dset (f (dataset-data dset))))))
  (hash-set! privs 'view-desc
             (mk-privilege group
                           (lambda () (dataset-desc dset))))
  (hash-set! privs 'edit-desc
             (mk-privilege group
                           (lambda (f)
                             (set-dataset-desc! dset (f (dataset-desc dset))))))
  ;; (hash-set! privs 'edit-privileges
  ;;            (mk-privilege group
  ;;                          (lambda (f)
  ;;                            (
  (resource name owner dset privs))




;; (define (mk-resource n o ps)
;;   (resource n o ps))
