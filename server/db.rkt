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

(define (connect-sql)
  (mysql-connect #:user "gn2"
                 #:password "gn2"
                 #:database "db_webqtl_s"))

(define mysql-conn (make-parameter (connect-sql)))
