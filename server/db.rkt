#lang racket

(require db
         redis
         json
         threading)

(provide connect-redis)

(define (connect-redis)
  (make-redis))


;; (define (connect-db)
;;   (sqlite3-connect #:database "./proxy.db"
;;                    #:mode 'create))


;; (define (create-users-table dbc)
;;   (query-exec
;;    dbc
;;    "CREATE TABLE users (
;;       user_id INTEGER PRIMARY KEY,
;;       name TEXT NOT NULL)"))

;; (define (create-groups-table dbc)
;;   (query-exec
;;    dbc
;;    "CREATE TABLE groups (
;;       group_id INTEGER PRIMARY KEY,
;;       admin_ids TEXT NOT NULL,
;;       member_ids TEXT NOT NULL)"))

;; (define (create-resources-table dbc)
;;   (query-exec
;;    dbc
;;    "CREATE TABLE resources (
;;       resource_id INTEGER PRIMARY KEY,
;;       name TEXT NOT NULL,
;;       owner_id INTEGER NOT NULL,
;;       resource_data TEXT NOT NULL,
;;       resource_type TEXT NOT NULL,
;;       FOREIGN KEY (owner_id)
;;         REFERENCES users (user_id))"))


;; (define (create-group-masks-table dbc)
;;   (query-exec
;;    dbc
;;    "CREATE TABLE group_masks (
;;      mask_id INTEGER PRIMARY KEY,
;;      resource_id INTEGER NOT NULL,
;;      group_id INTEGER NOT NULL,
;;      mask TEXT NOT NULL,
;;      FOREIGN KEY (resource_id) REFERENCES resources (resource_id),
;;      FOREIGN KEY (group_id) REFERENCES groups (group_id))"))

;; (define (create-resources-table dbc)
;;   (query-exec
;;    dbc
;;    "create table resources (
;;       )"))
