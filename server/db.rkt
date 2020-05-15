#lang racket

(require db
         redis)

(provide connect-redis)

;; This should be a racket parameter
(define (connect-redis)
  (make-redis))

;; (define (connect-db)
;;   (sqlite3-connect #:database "./proxy.db"
;;                    #:mode 'create))
