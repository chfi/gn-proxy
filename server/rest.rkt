#lang racket

(require db
         redis
         json
         threading
         racket/match
         web-server/http
         web-server/http/response
         web-server/http/bindings
         web-server/servlet-dispatch
         web-server/web-server
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         "db.rkt"
         "groups.rkt"
         "privileges.rkt"
         "resource.rkt")


;;;; Endpoints

;; Query available actions for a resource, for a given user
(define (query-available-endpoint req)
  (define binds (request-bindings/raw req))
  (define (masked-actions actions)
    (for/hash ([(k v) (in-hash actions)])
      (values k (map car v))))
  (define message
    (match (list (bindings-assq #"resource" binds)
                 (bindings-assq #"user" binds))
      [(list #f #f)
       "provide resource and user id"]
      [(list (binding:form _ res-id)
             (binding:form _ user-id))
       (let* ((res (get-resource res-id))
              (mask (get-mask-for-user
                     res
                     (~> user-id
                         (bytes->string/utf-8)
                         (string->number)))))
         (~> (apply-mask (dict-ref resource-types
                                   (resource-type res))
                         mask)
             (masked-actions)
             (jsexpr->bytes)))]))
  (response/output
   (lambda (out)
     (displayln message out))))

(define (query-available-dispatcher conn req)
  (output-response conn (query-available-endpoint req)))

(define (action-params action binds)
  (for/hash ([k (action-req-params action)])
    (values k
            (~> k
                (symbol->string)
                (string->bytes/utf-8)
                (bindings-assq _ binds)
                (binding:form-value)))))

(define (run-action-endpoint req)
  (define binds (request-bindings/raw req))
  (define message
    (match (list (bindings-assq #"resource" binds)
                 (bindings-assq #"user" binds)
                 (bindings-assq #"branch" binds)
                 (bindings-assq #"action" binds))
      [(list #f #f #f #f)
       "provide resource id, user id, and action to perform"]
      [(list (binding:form _ res-id)
             (binding:form _ user-id)
             (binding:form _ branch)
             (binding:form _ action))
       (let* ((res (get-resource res-id))
              (branch (~> branch
                          (bytes->string/utf-8)
                          (string->symbol)))
              (action (bytes->string/utf-8 action)))
         (let ((action (access-action res
                                      (~> user-id
                                          (bytes->string/utf-8)
                                          (string->number))
                                      (cons branch action))))
           (if action
               (run-action action
                           (resource-data res)
                           (action-params action binds))
               "no access")))]))
  (response/output
   (lambda (out)
     (displayln message out))))

(define (run-action-dispatcher conn req)
  (output-response conn (run-action-endpoint req)))

;; Run the server (will be moved to another module later)
(define stop
  (serve
   #:dispatch (sequencer:make
               (filter:make #rx"^/available/"
                            query-available-dispatcher)
               (filter:make #rx"^/run-action/"
                            run-action-dispatcher))
   ;; #:dispatch (dispatch/servlet run-action-endpoint)
   #:listen-ip "127.0.0.1"
   #:port 8080))

(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))
