#lang racket

(require db
         redis)

(provide connect-redis
         connect-sql
         redis-conn
         mysql-conn)


;; This should be a racket parameter
(define (connect-redis)
  (make-redis))

(define redis-conn (make-parameter (connect-redis)))

(define sql-user (getenv "SQL_USER"))
(define sql-pass (getenv "SQL_PASSWORD"))

(unless (and sql-user sql-pass)
  (error "provide SQL user information with SQL_USER and SQL_PASSWORD environment variables"))

(define (connect-sql)
  (mysql-connect #:user sql-user
                 #:password sql-pass
                 #:database "db_webqtl_s"))

(define mysql-conn (make-parameter (connect-sql)))
