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

; TODO this should check that the provided user IDs point to existing users
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
      (error "tried to select groups where id is not a number")))

(define (select-groups-by-member-id dbc id)
  (if (number? id)
      (map read-group-row
           (query-rows dbc
                       (group-query-string "member_ids" id)))
      (error "tried to select groups where id is not a number")))

(define (select-groups-by-user-id dbc id)
  (~> (map read-group-row
           (query-rows dbc "select * from groups"))
      (filter (lambda (g) (has-user? g id)) _)))


;; has-member? is only used by add-member and make-admin. These functions
;; should probably be replaced by an interface that makes more sense
;; for our purposes.
(define (has-admin? g uid)
  (set-member? (group-admins g) uid))

(define (has-member? g uid)
  (set-member? (group-members g) uid))

(define (has-user? g uid)
  (or (has-admin? g uid) (has-member? g uid)))

(define (add-member dbc gid uid)
  (let ([g (select-group-id dbc gid)])
    (if (has-user? g uid)
        #f ; user exists; do nothing
        (let ([new-members (~> (group-members g)
                               (set-add _ uid)
                               (set->list)
                               (jsexpr->string))])
          (query-exec dbc
                      "update groups
                       set member_ids = ?
                       where group_id = ?"
                      new-members
                      gid)))))

(define (del-member dbc gid uid)
  (let ([g (select-group-id dbc gid)])
    (if (not (has-member? g uid))
        #f ; member doesn't exist; do nothing
        (let ([new-members (~> (group-members g)
                               (set-remove _ uid)
                               (set->list)
                               (jsexpr->string))])
          (query-exec dbc
                      "update groups
                       set member_ids = ?
                       where group_id = ?"
                      new-members
                      gid)))))


(define (promote-to-admin dbc gid uid)
  (let ([g (select-group-id dbc gid)])
    (if (not (has-member? g uid))
        #f ; member doesn't exist; do nothing
        (let ([new-members (~> (group-members g)
                               (set-remove _ uid)
                               (set->list)
                               (jsexpr->string))]
              [new-admins (~> (group-admins g)
                              (set-add _ uid)
                              (set->list)
                              (jsexpr->string))])
          (query-exec dbc
                      "update groups
                       set member_ids = ?
                           admin_ids = ?
                       where group_id = ?"
                      new-members
                      new-admins
                      gid)))))


(define (demote-to-member dbc gid uid)
  (let ([g (select-group-id dbc gid)])
    (if (not (has-admin? g uid))
        #f ; admin doesn't exist; do nothing
        (let ([new-members (~> (group-members g)
                               (set-add _ uid)
                               (set->list)
                               (jsexpr->string))]
              [new-admins (~> (group-admins g)
                              (set-remove _ uid)
                              (set->list)
                              (jsexpr->string))])
          (query-exec dbc
                      "update groups
                       set member_ids = ?
                           admin_ids = ?
                       where group_id = ?"
                      new-members
                      new-admins
                      gid)))))


(define (all-members g)
  (set->list (set-union (group-admins g)
                        (group-members g))))
