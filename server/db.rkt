#lang racket

(require db
         json
         threading)

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
   "create table users (
      user_id integer primary key,
      name text not null)"))

(define (create-groups-table dbc)
  (query-exec
   dbc
   "create table groups (
      group_id integer primary key,
      admin_ids text not null,
      member_ids text not null)"))

;; (define (create-resources-table dbc)
;;   (query-exec
;;    dbc
;;    "create table resources (
;;       )"))
