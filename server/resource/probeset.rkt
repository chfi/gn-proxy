#lang racket

(require db
         redis
         json
         threading
         "../db.rkt"
         "../privileges.rkt"
         "util.rkt")

(provide dataset-probeset-actions)


(define (select-probeset dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
                "SELECT ProbeSet.Name, ProbeSet.Symbol, ProbeSet.description, ProbeSet.Probe_Target_Description,
                        ProbeSet.Chr, ProbeSet.Mb, ProbeSet.alias, ProbeSet.GeneId, ProbeSet.GenbankId, ProbeSet.UniGeneId,
                        ProbeSet.OMIM, ProbeSet.RefSeq_TranscriptId, ProbeSet.BlatSeq, ProbeSet.TargetSeq, ProbeSet.ChipId,
                        ProbeSet.comments, ProbeSet.Strand_Probe, ProbeSet.Strand_Gene, ProbeSet.ProteinID, ProbeSet.UniProtID,
                        ProbeSet.Probe_set_target_region, ProbeSet.Probe_set_specificity, ProbeSet.Probe_set_BLAT_score,
                        ProbeSet.Probe_set_Blat_Mb_start, ProbeSet.Probe_set_Blat_Mb_end, ProbeSet.Probe_set_strand,
                        ProbeSet.Probe_set_Note_by_RW, ProbeSet.flag
                FROM ProbeSet, ProbeSetFreeze, ProbeSetXRef
                WHERE
                        ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id AND
                        ProbeSetXRef.ProbeSetId = ProbeSet.Id AND
                        ProbeSetFreeze.Id = ? AND
                        ProbeSet.Name = ?"
                dataset-name
                trait-name)))

(define view-probeset-data
  (action (lambda (data
                   params)
            (select-probeset (hash-ref data 'dataset)
                          (dict-ref params 'trait)))
          '(trait)))

(define edit-probeset-data
  (lambda (data
           params)
    #f))

(define view-probeset-metadata
  (lambda (data
           params)
    #f))

(define edit-probeset-metadata
  (lambda (data
           params)
    #f))

(define dataset-probeset-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-probeset-data)
        (cons "edit" edit-probeset-data)))

(define dataset-probeset-metadata
  (list (cons "no-access" no-access-action)
        (cons "view" view-probeset-metadata)
        (cons "edit" edit-probeset-metadata)))

(define dataset-probeset-admin
  (list (cons "not-admin" (lambda (data params) #f))
        (cons "edit-access" (lambda (data params) #f))
        (cons "edit-admins" (lambda (data params) #f))))

(define dataset-probeset-actions
  (hasheq 'data dataset-probeset-data
          'metadata dataset-probeset-metadata
          'admin dataset-probeset-admin))
