#lang racket

(require db
         redis
         json
         threading
         "../db.rkt"
         "../privileges.rkt"
         "util.rkt")

(provide dataset-geno-actions)


(define (select-geno dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT Geno.Name, Geno.Chr, Geno.Mb, Geno.Source2, Geno.Sequence
                FROM Geno, GenoFreeze, GenoXRef
                WHERE GenoXRef.GenoFreezeId = GenoFreeze.Id AND
                      GenoXRef.GenoId = Geno.Id AND
                      GenoFreeze.Id = ? AND
                      Geno.Name = ?"
                dataset-name
                trait-name)))

(define view-geno-data
  (action (lambda (data
                   params)
            (select-geno (hash-ref data 'dataset)
                         (dict-ref params 'trait)))
          '(trait)))

(define dataset-geno-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-geno-data)
        (cons "edit" #f)))

(define dataset-geno-metadata
  (list (cons "no-access" #f)
        (cons "view" #f)
        (cons "edit" no-access-action)))

(define dataset-geno-admin
  (list (cons "not-admin" #f)
        (cons "edit-access" #f)
        (cons "edit-admins" #f)))

(define dataset-geno-actions
  (hasheq 'data dataset-geno-data
          'metadata dataset-geno-metadata
          'admin dataset-geno-admin))