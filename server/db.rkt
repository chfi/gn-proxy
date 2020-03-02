#lang racket

(require db
         json
         threading
         "groups.rkt")

(provide connect-db
         create-users-table
         create-groups-table
         )

(define (connect-db)
  (sqlite3-connect #:database "./proxy.db"
                   #:mode 'create))

(define (create-users-table dbc)
  (query-exec
   dbc
   "create temporary table users (
      user_id integer primary key,
      name text not null)"))

(define (create-groups-table dbc)
  (query-exec
   dbc
   "create temporary table groups (
      group_id integer primary key,
      admin_ids text not null,
      member_ids text not null)"))

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
   (query-row dbc
              "select * from users where user_id = ?"
              id)))

(define (insert-user dbc name)
  (query-exec
   dbc
   "insert into users (name) values (?)"
   name))
