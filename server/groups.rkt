#lang racket

(require db
         json
         racket/set
         threading
         "db.rkt")

(provide ;group-by-id
         ;groups-by-user
         ;user-by-id
         ;; has-user?
         ;add-group-for-owner
         (struct-out user)
         (struct-out group))

; A simple (placeholder) user type, to be replaced by one isomorphic
; to the GeneNetwork user type.
(struct user (id name)
  #:transparent)

(define (read-user-row r)
  (user (vector-ref r 0)
        (vector-ref r 1)))

(define (select-users dbc)
  (map read-user-row
       (query-rows
        dbc
        "select * from users")))

(define (select-user-id dbc id)
  (read-user-row
   (query-maybe-row dbc
              "select * from users where user_id = ?"
              id)))

;; (define (select-users-id dbc ids)
;;   (map read-user-row
;;        (query-rows dbc
                   ;; "select * from users where user_id

(define (select-user-name dbc name)
  (read-user-row
   (query-maybe-row dbc
              "select * from users where name = ?"
              name)))

(define (insert-user dbc name)
  (query-exec
   dbc
   "insert into users (name) values (?)"
   name))

; A group is a product of two sets of users, admins and members. A
; user can be either an admin or a member, not both. Logically, for
; access control purposes, being an admin implies being a member.
(struct group (id admins members)
  #:transparent)


(define (insert-group dbc admins members)
  (let ([admin-str (jsexpr->string (set->list admins))]
        [mems-str (jsexpr->string (set->list members))])
    (query-exec
     dbc
     "insert into groups (admin_ids, member_ids)
      values (?, ?)"
     admin-str
     mems-str)))


(define test-grp
  (cons (list->set '(1 2 3 4))
        (list->set '(5 6 7 8 9 10))))

(define (read-group-row r)
  (define (ref i)
    (~> (vector-ref r i)
        (string->jsexpr)
        (list->set)))
  (group (vector-ref r 0)
         (ref 1)
         (ref 2)))

(define (select-groups dbc)
  (map read-group-row
       (query-rows dbc
                   "select * from groups")))

(define (select-group-id dbc id)
  (read-group-row
   (query-row dbc
              "select * from groups where group_id = ?"
              id)))

;; this is just a *terrible* way of doing it, but it's temporary and
;; i'm not about to set sqlite up with regex support just for this,
;; though for now it would probably be better to just do `select * from groups`
;; and do the filtering in racket...
(define (group-query-string column id)
  (if (and (number? id)
           (string? column))
      (let ([id (number->string id)])
        (string-append
         "select * from groups where "
         column " like '[" id "]' or "
         column " like '[" id ",%' or "
         column " like '%," id ",%' or "
         column " like '%," id "]'"))
      #f))


(define (select-groups-by-admin-id dbc id)
  (if (number? id)
      (map read-group-row
           (query-rows dbc
                       (group-query-string "admin_ids" id)))
      (error "tried to select groups where id is not a number"))

(define (select-groups-by-member-id dbc id)
  (if (number? id)
      (map read-group-row
           (query-rows dbc
                       (group-query-string "member_ids" id)))
      (error "tried to select groups where id is not a number"))


;; (define (select-groups-by-user-id dbc id)
;;   (map read-group-row




; Placeholder global user "DB" with some entries for testing
;; (define user-db
;;   (list (user 1 "admin1")
;;         (user 2 "user1")
;;         (user 3 "user2")
;;         (user 4 "admin2")))

;; TODO rewrite
;; (define (user-by-id id)
;;   (findf (lambda (u) (eq? id (user-id u))) user-db))



;; NOTE this isn't used anywhere
;; (define (same-user? a b)
;;   (equal? (user-id a) (user-id b)))


; Placeholder global group "DB", will be replaced by an actual DB
; system etc.
;; (define group-db
;;   (mutable-set))

;; (define user-groups (make-parameter group-db))


;; (set-add! group-db (group 0
;;                           (set (user-by-id 1))
;;                           (set (user-by-id 3))))

; Placeholder function for adding a group to the global DB; only works
; if the given user ID exists in the user Db

;; TODO rewrite
;; (define (add-group-for-owner owner-id)
;;   (define user (user-by-id owner-id))
;;   (when user
;;     (set-add! group-db (group (set-count group-db)
;;                               (set user)
;;                               (set)))))

;; This is only used by add-member and make-admin. These functions
;; should probably be replaced by an interface that makes more sense
;; for our purposes.
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

;; TODO rewrite
;; (define (group-by-id id)
;;   (findf (lambda (g) (eq? id (group-id g)))
;;          (set->list group-db)))

;; TODO rewrite
;; (define (groups-by-user u)
;;   (filter (lambda (g) (has-user? g u))
;;           (set->list group-db)))

;
(define (all-members g)
  (set->list (set-union (group-admins g)
                        (group-members g))))
