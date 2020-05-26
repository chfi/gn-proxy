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

;; Retrieve the given user by ID from Redis; deserializes from JSON
;; TODO update this when we update the user struct
(define (get-user dbc id)
  (let ((user-hash (bytes->jsexpr
                    (redis-hash-ref dbc "users" id))))
    (user id
          (dict-ref user-hash 'user_name))))


;; Add a user with the given ID and name to the "users" hash in Redis.
;; NB: This is mainly for testing locally; the proxy shouldn't create
;; users in production.
(define (add-user dbc id name)
  (redis-hash-set! dbc
                   "users"
                   id
                   (jsexpr->bytes (hash 'user_name name))))

; A group is a product of two sets of users, admins and members. A
; user can be either an admin or a member, not both. Logically, for
; access control purposes, being an admin implies being a member.
(struct group (id admins members)
  #:transparent)

;; Deserialize a group struct from bytestringified JSON
(define (deserialize-group id grp-bytes)
  (let ((group-hash (bytes->jsexpr grp-bytes)))
    (define (parse k)
      (~> (dict-ref group-hash k)
          (list->set)))
    (group id
           (parse 'admins)
           (parse 'members))))

;; Retrieve the given group by ID from Redis
(define (get-group dbc id)
  (deserialize-group id
                     (redis-hash-ref dbc
                                     "groups"
                                     id)))

;; NB: like add-user, for testing in the REPL
(define (add-group dbc id admins members)
  (redis-hash-set! dbc
                   "groups"
                   id
                   (jsexpr->bytes
                    (hash 'admins (set->list admins)
                          'members (set->list members)))))


;; Search Redis and return all the groups that have the given user ID
;; as either an admin or a regular member.
;; TODO Redis almost certainly has tools to make this faster & better
(define (get-groups-by-member dbc user-id)
  (define (parse e)
    (deserialize-group (car e) (cdr e)))
  (for/list ([group (sequence-map parse
                                  (in-redis-hash dbc "groups"))]
             #:when (has-user? group user-id))
    group))

;; Helper functions for querying groups
(define (has-admin? g uid)
  (set-member? (group-admins g) uid))

(define (has-member? g uid)
  (set-member? (group-members g) uid))

(define (has-user? g uid)
  (or (has-admin? g uid) (has-member? g uid)))
