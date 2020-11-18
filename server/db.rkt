#lang racket

(require db
         redis)

(provide connect-redis
         connect-sql
         redis-conn
         mysql-conn)

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

(define mysql-conn
  (make-parameter
   (virtual-connection
    (connection-pool connect-sql
                     ; these are the default arguments, for clarity's sake
                     #:max-connections +inf.0
                     #:max-idle-connections 10))))
