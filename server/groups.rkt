#lang racket

(require db
         redis
         json
         racket/set
         threading
         "db.rkt")



(provide get-user
         get-group
         (struct-out user)
         (struct-out group))



; A simple (placeholder) user type, to be replaced by one isomorphic
; to the GeneNetwork user type.
(struct user (id name)
  #:transparent)


(define (get-user dbc id)
  (let ((user-json (string->jsexpr (redis-hash-get dbc "users" id))))
    (user id
          (dict-ref user-json "user_name"))))

; A group is a product of two sets of users, admins and members. A
; user can be either an admin or a member, not both. Logically, for
; access control purposes, being an admin implies being a member.
(struct group (id admins members)
  #:transparent)

;; may end up having to deserialize the admins and members fields as
;; an additional step
(define (get-group dbc id)
  (let ((group-json (string->jsexpr (redis-hash-get dbc "groups" id))))
    (define (parse k)
      (~> (dict-ref group-json k)
          (string->jsexpr)
          (list->set)))
    (group id
           (parse "admins")
           (parse "members"))))

(define test-grp
  (cons (list->set '(1 2 3 4))
        (list->set '(5 6 7 8 9 10))))


;; TODO it *might* be useful to have in gn-proxy? But for now it's
;; fine to have GN search Redis and return the group ID, if this
;; functionality is needed

;; (define (select-groups-by-admin-id dbc id)
;;   (if (number? id)
;;       (map read-group-row
;;            (query-rows dbc
;;                        (group-query-string "admin_ids" id)))
;;       (error "tried to select groups where id is not a number")))

;; (define (select-groups-by-member-id dbc id)
;;   (if (number? id)
;;       (map read-group-row
;;            (query-rows dbc
;;                        (group-query-string "member_ids" id)))
;;       (error "tried to select groups where id is not a number")))

;; (define (select-groups-by-user-id dbc id)
;;   (~> (map read-group-row
;;            (query-rows dbc "select * from groups"))
;;       (filter (lambda (g) (has-user? g id)) _)))


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
