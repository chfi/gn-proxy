#lang racket

(require db
         redis)

(provide connect-redis
         connect-sql)

;; This should be a racket parameter
(define (connect-redis)
  (make-redis))

(define (connect-sql)
  (mysql-connect #:user "gn2"
                 #:password "gn2"
                 #:database "db_webqtl_s"))
