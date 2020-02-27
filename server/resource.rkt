#lang racket

(require "groups.rkt"
         "privileges.rkt")

(provide (struct-out resource)
         user-canonical-mask
         pl-edit-view)

(struct resource (name owner content plines group-masks))

; Given a resource and a user, get the masks for that user based
; on the per-group masks in the resource privileges.
(define (user-masks res u)
  (define masks (map (curry dict-ref (resource-group-masks res))
                     (map group-id (groups-by-user u))))
  (if (empty? masks)
      (list (minimum-access-mask (resource-plines res)))
      masks))


; Given a resource and a mask, calculate the actual mask in case the
; mask has admin privileges. Admins have the maximum access privileges,
; except their admin privileges may be limited.
(define (admin-mask res m)
  (define admin-level (dict-ref m 'admin))
  (if (eq? admin-level 'not-admin)
      m
      (dict-set (maximum-access-mask (resource-plines res))
                'admin
                admin-level)))

; The owner of a resource has complete access.
(define (owner-mask res)
  (maximum-access-mask (resource-plines res)))

; Given a resource and a user, calculate the user's canonical access mask
; based on the user's group membership, whether or not they're an admin,
; and whether or not they're the resource owner.

; TODO: Support default minimum access levels based on if the resource
; is public or private (perhaps just a group mask keyed to the global
; user/guest group?)
(define (user-canonical-mask res u)
  (if (eq? u (resource-owner res))
      (owner-mask res)
      (admin-mask res
                  (apply mask-join
                         (resource-plines res)
                         (user-masks res u)))))

; Given a resource and a user, return all actions the user has access to perform;
; Return format is an alist of alists; it's a subset of the resource's plines field.
(define (get-actions res u)
  (define mask (user-canonical-mask res u))
  (dict-map (resource-plines res)
            (λ (k v)
              (cons k (reverse
                       (memf (λ (x)
                              (eq? (dict-ref mask k) (car x)))
                            (reverse v)))))))

;; (define (resource-set-mask res gid mask)
;;   (


;; Constructor for a pline with actions for no access, getting, and
;; modifying something.
(define (pl-edit-view name
                      no-access
                      get
                      mod)
  `(,name (no-access . ,no-access)
          (view . ,get)
          (edit . ,(λ (f) (mod f)))))
