#lang racket

(require db
         redis
         json
         threading
         "../db.rkt"
         "../privileges.rkt"
         "util.rkt")

(provide dataset-probe-actions)


(define (select-probe dataset-name trait-name)
  (sql-result->json
    (query-row (mysql-conn)
               "SELECT Probe.Sequence, Probe.Name
                FROM Probe, ProbeSet, ProbeSetFreeze, ProbeSetXRef
                WHERE ProbeSetXRef.ProbeSetFreezeId = ProbeSetFreeze.Id AND
                      ProbeSetXRef.ProbeSetId = ProbeSet.Id AND
                      ProbeSetFreeze.Name = ? AND
                      ProbeSet.Name = ? AND
                      Probe.ProbeSetId = ProbeSet.Id order by Probe.SerialOrder"
                dataset-name
                trait-name)))

(define view-probe
  (action (lambda (data
                   params)
            (select-probe (hash-ref data 'dataset)
                          (hash-ref data 'trait)))
          '()))

(define dataset-probe-data
  (list (cons "no-access" no-access-action)
        (cons "view" view-probe)))

(define dataset-probe-actions
  (hasheq 'data dataset-probe-data))
