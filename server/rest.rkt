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
         web-server/dispatch
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         "db.rkt"
         "groups.rkt"
         "privileges.rkt"
         "resource.rkt")


;;;; Endpoint exception handlers

(struct
  proxy:param-error
  (expected-params))

;; For now the handler will just return a JSON list of the expected parameters
(define (response-param-error err)
  (let ((expected (proxy:param-error-expected-params err)))
    (jsexpr->bytes expected)))

(define (testing-endpoint req)
  (define binds (request-bindings/raw req))
  (define expected
    (proxy:param-error
     (list "resource" "user" "branch" "action")))
  (define message
    (let ((binds* (list (bindings-assq #"resource" binds)
                        (bindings-assq #"user" binds)
                        (bindings-assq #"branch" binds)
                        (bindings-assq #"action" binds))))
      (with-handlers ([proxy:param-error?
                       response-param-error])
        (if (ormap false? binds*)
            (raise expected)
            (match binds*
              [(list (binding:form _ res-id)
                     (binding:form _ user-id)
                     (binding:form _ branch)
                     (binding:form _ action))
               "this works"])))))
  (response/output
   (lambda (out)
     (displayln message out))))


;;;; Endpoints

;; Get a JSON representation of the action set for a resource type,
;; can be useful when resource types start changing so we know what
;; the proxy expects the masks in a redis resource to look like
(define (get-action-set-endpoint req)
  (define binds (request-bindings/raw req))
  (define message
    (match (list (bindings-assq #"resource-type" binds))
      [(list #f)
       "Provide a resource type"]
      [(list (binding:form _ res-type))
       (let ((type (dict-ref resource-types
                             (~> res-type
                                 (bytes->string/utf-8)
                                 (string->symbol)))))
         (jsexpr->bytes (action-set->hash type)))]))
  (response/output
   (lambda (out)
     (displayln message out))))


;; Query available actions for a resource, for a given user
(define (query-available-endpoint req)
  (define binds (request-bindings/raw req))
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
                     (bytes->string/utf-8 user-id))))
         (~> (apply-mask (dict-ref resource-types
                                   (resource-type res))
                         mask)
             (action-set->hash)
             (jsexpr->bytes)))]))
  (response/output
   (lambda (out)
     (displayln message out))))


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
             #f
             (binding:form _ branch)
             (binding:form _ action))
             (let* ((res (get-resource res-id))
              (branch (~> branch
                          (bytes->string/utf-8)
                          (string->symbol)))
              (action (bytes->string/utf-8 action)))
         (let ((action (access-action res
                                      (cons branch action))))
           (if action
               (run-action action
                           (resource-data res)
                           (action-params action binds))
               "no access")))]
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
                                      (cons branch action)
                                      #:user (bytes->string/utf-8 user-id))))
           (if action
               (run-action action
                           (resource-data res)
                           (action-params action binds))
               "no access")))]))
  (response/output
   (lambda (out)
     (displayln message out))))



(define-values (app reverse-uri)
  (dispatch-rules
   [("available") query-available-endpoint]
   [("run-action") run-action-endpoint]
   [("get-action-set") get-action-set-endpoint]
   [("testing") testing-endpoint]))

;; Run the server
(define stop
  (serve
   #:dispatch (sequencer:make
               (dispatch/servlet app))
   #:listen-ip "127.0.0.1"
   #:port (string->number
           (or (getenv "PORT")
               "8080"))))

(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))
