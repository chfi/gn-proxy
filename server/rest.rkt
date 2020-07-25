#lang racket

(require db
         redis
         json
         net/url
         threading
         racket/match
         web-server/http
         web-server/http/response
         web-server/http/bindings
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch
         web-server/http/response-structs
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         "db.rkt"
         "groups.rkt"
         "privileges.rkt"
         "resource.rkt")


;;;; Endpoint exceptions

(define (response-param-error params)
  (error
   (string-join (map symbol->string params)
                ", "
                #:before-first "Expected parameters: ")))

(define (extract-expected binds expected)
  (if (not (andmap (curryr exists-binding? binds) expected))
      (response-param-error expected)
      (for/hash ([x (in-list binds)])
        (values (car x) (cdr x)))))


;;;; Endpoints


(define (show-version req)
  (response/jsexpr
   #hasheq((version . "0.2.1"))))

;; Get a JSON representation of the action set for a resource type,
;; can be useful when resource types start changing so we know what
;; the proxy expects the masks in a redis resource to look like
(define (get-action-set-endpoint req)
  (define binds (request-bindings req))
  (define expected (list 'resource-type))
  (define message
    (let* ((binds* (extract-expected binds expected))
           (res-type (hash-ref binds* 'resource-type)))
      (jsexpr->bytes
           (action-set->hash
            (dict-ref resource-types
                      (string->symbol res-type))))))
  (response/output
   (lambda (out)
     (displayln message out))
   #:mime-type #"application/json; charset=utf-8"))


;; Query available actions for a resource, for a given user
(define (query-available-endpoint req)
  (define binds (request-bindings req))
  (define expected (list 'resource 'user))
  (define message
    (let* ((binds* (extract-expected binds expected))
           (res (get-resource (hash-ref binds* 'resource)))
           (mask (get-mask-for-user res
                                    (hash-ref binds* 'user))))
      (~> (apply-mask (dict-ref resource-types
                                (resource-type res))
                      mask)
          (action-set->hash)
          (jsexpr->bytes))))
  (response/output
   (lambda (out)
     (displayln message out))
   #:mime-type #"application/json; charset=utf-8"))


(define (action-params action binds)
  (for/hash ([k (action-req-params action)])
    (values k (dict-ref binds k))))

(define (run-action-endpoint req)
  (define binds (request-bindings req))
  (define expected
    (list 'resource 'branch 'action))
  (define message
    (let* ((binds* (extract-expected binds expected))
           (res (get-resource (hash-ref binds* 'resource)))
           (branch (string->symbol (hash-ref binds* 'branch)))
           (action (hash-ref binds* 'action))
           (user (hash-ref binds* 'user #f)))
      (let ((action (access-action res
                     (cons branch action)
                     #:user user)))
        (if action
            (run-action action
                        (resource-data res)
                        (action-params action binds))
            "no access"))))
  (response/output
   (lambda (out)
     (displayln message out))
   #:mime-type #"application/json; charset=utf-8"))


(define-values (app reverse-uri)
  (dispatch-rules
   [("version") show-version]
   [("available") query-available-endpoint]
   [("run-action") run-action-endpoint]
   [("get-action-set") get-action-set-endpoint]))

;; Servlet responder for error handling
(define (internal-server-error url ex)
  (log-error "~a ~~~~> ~a"
             (url->string url)
             (exn-message ex))
  (response/output
   (lambda (out)
     (displayln (exn-message ex) out))
   #:code 500
   #:mime-type #"application/json; charset=utf-8"))

;; Run the server
(define stop
  (serve
   #:dispatch (sequencer:make
               (dispatch/servlet app
                                 #:responders-servlet internal-server-error))
   #:listen-ip "127.0.0.1"
   #:port (string->number
           (or (getenv "PORT")
               "8080"))))

(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))
