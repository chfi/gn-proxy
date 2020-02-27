#lang racket

(require "groups.rkt"
         "privileges.rkt")

(provide (struct-out resource)
         user-canonical-mask
         user-masks
         resource-set-mask
         get-actions
         pl-edit-view)

(struct resource (name owner content plines group-masks))

; Given a resource and a user, get the masks for that user based
; on the per-group masks in the resource privileges.
(define (user-masks res u)
  (define masks
    (map (lambda (x)
           (dict-ref (resource-group-masks res)
                     x
                     ; TODO the minimum access mask should be defined on a per-resource basis
                     (minimum-access-mask (resource-plines res))))
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

;; Given a resource, a group ID, and a mask that fits the resource,
;; returns a new resource with the corresponding group's mask updated
;; to the given mask. Returns #f if the mask doesn't fit.
(define (resource-set-mask res gid mask)
  (if (is-mask-for? (resource-plines res) mask)
      (struct-copy resource
                   res
                   [group-masks (dict-set
                                 (resource-group-masks res)
                                 gid
                                 mask)])
      #f))


;; Constructor for a pline with actions for no access, getting, and
;; modifying something.
(define (pl-edit-view name
                      no-access
                      get
                      mod)
  `(,name (no-access . ,no-access)
          (view . ,get)
          (edit . ,(λ (f) (mod f)))))
