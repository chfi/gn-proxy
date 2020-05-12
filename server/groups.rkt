#lang racket

(require db
         redis
         json
         racket/set
         threading
         "db.rkt")


(provide get-user
         get-group
         get-groups-by-member
         add-user
         add-group
         (struct-out user)
         (struct-out group))


; A simple (placeholder) user type, to be replaced by one isomorphic
; to the GeneNetwork user type.
(struct user (id name)
  #:transparent)


(define (get-user dbc id)
  (let ((user-hash (bytes->jsexpr (redis-hash-ref dbc "users" id))))
    (user id
          (dict-ref user-hash 'user_name))))

;; This is mainly for testing locally; the proxy shouldn't create
;; users in production
(define (add-user dbc id name)
  (redis-hash-set! dbc
                   "users"
                   (number->string id)
                   (jsexpr->bytes (hash 'user_name name))))

; A group is a product of two sets of users, admins and members. A
; user can be either an admin or a member, not both. Logically, for
; access control purposes, being an admin implies being a member.
(struct group (id admins members)
  #:transparent)

(define (deserialize-group id grp-bytes)
  (let ((group-hash (bytes->jsexpr grp-bytes)))
    (define (parse k)
      (~> (dict-ref group-hash k)
          (list->set)))
    (group id
           (parse 'admins)
           (parse 'members))))

(define (get-group dbc id)
  (deserialize-group id
                     (redis-hash-ref dbc
                                     "groups"
                                     (number->string id))))

;; like add-user, for testing in the REPL
(define (add-group dbc id admins members)
  (redis-hash-set! dbc
                   "groups"
                   (number->string id)
                   (jsexpr->bytes (hash 'admins (set->list admins)
                                        'members (set->list members)))))

(define test-grp
  (cons (list->set '(1 2 3 4))
        (list->set '(5 6 7 8 9 10))))


; Returns all groups that have the given user ID either as admin or member
(define (get-groups-by-member dbc id)
  (define (parse e)
    (deserialize-group (car e) (cdr e)))
  (for/list ([group (sequence-map parse (in-redis-hash dbc "groups"))]
             #:when (has-user? group id))
    group))



;; has-member? is only used by add-member and make-admin. These functions
;; should probably be replaced by an interface that makes more sense
;; for our purposes.
(define (has-admin? g uid)
  (set-member? (group-admins g) uid))

(define (has-member? g uid)
  (set-member? (group-members g) uid))

(define (has-user? g uid)
  (or (has-admin? g uid) (has-member? g uid)))

(define (all-members g)
  (set->list (set-union (group-admins g)
                        (group-members g))))
