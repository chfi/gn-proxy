#lang racket

(require racket/set
         threading)

(provide group-by-id
         groups-by-user
         user-by-id
         has-user?
         add-group-for-owner
         (struct-out user)
         (struct-out group))

; Groups and users are the second, for now, a user is simply an ID
(struct user (id name)
  #:transparent)

; Placeholder global user "DB" with some entries for testing
(define user-db
  (list (user 1 "admin1")
        (user 2 "user1")
        (user 3 "user2")
        (user 4 "admin2")))

(define (user-by-id id)
  (findf (lambda (u) (eq? id (user-id u))) user-db))



(define (same-user? a b)
  (equal? (user-id a) (user-id b)))

;A `group` is a collection of users of different levels, including a
; non-empty set of admin users
(struct group (id admins members)
  #:transparent)

; Placeholder global group "DB", will be replaced by an actual DB
; system etc.
(define group-db
  (mutable-set))

;; (define user-groups (make-parameter group-db))


(set-add! group-db (group 0
                          (set (user-by-id 1))
                          (set (user-by-id 3))))

; Placeholder function for adding a group to the global DB; only works
; if the given user ID exists in the user Db
(define (add-group-for-owner owner-id)
  (define user (user-by-id owner-id))
  (when user
    (set-add! group-db (group (set-count group-db)
                              (set user)
                              (set)))))

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

(define (group-by-id id)
  (findf (lambda (g) (eq? id (group-id g)))
         (set->list group-db)))

(define (groups-by-user u)
  (filter (lambda (g) (has-user? g u))
          (set->list group-db)))

;
(define (all-members g)
  (set->list (set-union (group-admins g)
                        (group-members g))))
