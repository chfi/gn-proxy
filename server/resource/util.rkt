#lang racket

(require db
         redis
         json
         threading
         "../privileges.rkt")

(provide no-access-action
         sql-result->json)



;; Function that serializes an SQL result row into a stringified JSON
;; array. Probably doesn't work with all SQL types yet!!
(define (sql-result->json query-result)
  (jsexpr->bytes
   (map (lambda (x)
          (cond
            [(sql-null? x) 'null]
            [(bytes? x) (bytes->string/utf-8 x)]
            [else x]))
        (vector->list query-result))))

;; The general "no access" action -- may change in the future
(define no-access-action
  (action (lambda (data params)
            'no-access)
          '()))
