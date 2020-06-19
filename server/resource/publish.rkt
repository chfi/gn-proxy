#lang racket

(require db
         redis
         json
         threading
         "../db.rkt"
         "../privileges.rkt"
         "util.rkt")

(provide dataset-publish-actions)


(define (select-publish dataset-id trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT
                      PublishXRef.Id, InbredSet.InbredSetCode, Publication.PubMed_ID,
                      Phenotype.Pre_publication_description, Phenotype.Post_publication_description, Phenotype.Original_description,
                      Phenotype.Pre_publication_abbreviation, Phenotype.Post_publication_abbreviation, PublishXRef.mean,
                      Phenotype.Lab_code, Phenotype.Submitter, Phenotype.Owner, Phenotype.Authorized_Users,
                      Publication.Authors, Publication.Title, Publication.Abstract,
                      Publication.Journal, Publication.Volume, Publication.Pages,
                      Publication.Month, Publication.Year, PublishXRef.Sequence,
                      Phenotype.Units, PublishXRef.comments
                FROM
                      PublishXRef, Publication, Phenotype, PublishFreeze, InbredSet
                WHERE
                      PublishXRef.Id = ? AND
                      Phenotype.Id = PublishXRef.PhenotypeId AND
                      Publication.Id = PublishXRef.PublicationId AND
                      PublishXRef.InbredSetId = PublishFreeze.InbredSetId AND
                      PublishXRef.InbredSetId = InbredSet.Id AND
                      PublishFreeze.Id = ?"
             trait-name
             dataset-id)))

(define view-publish-data
  (action (lambda (data
                   params)
            (select-publish (hash-ref data 'dataset)
                            (hash-ref data 'trait)))
          '()))

(define dataset-publish-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-publish-data)
        (cons "edit" #f)))

(define dataset-publish-metadata
  (list (cons "no-access" #f)
        (cons "view" #f)
        (cons "edit" #f)))

(define dataset-publish-admin
  (list (cons "not-admin" #f)
        (cons "edit-access" #f)
        (cons "edit-admins" #f)))

(define dataset-publish-actions
  (hasheq 'data dataset-publish-data
          'metadata dataset-publish-metadata
          'admin dataset-publish-admin))