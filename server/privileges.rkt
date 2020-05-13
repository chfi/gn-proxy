#lang racket

(provide (struct-out action)
         minimum-access-mask
         maximum-access-mask
         mask-join
         apply-mask
         is-action-set?
         is-mask-for?)

(struct
  action
  (id
   fun
   params)
  #:transparent)


;; (define (run-action action resource)
  ;; TODO add check that action matches resource type
  ;; (let ((data (resource-data resource)))
  ;; (action-fun

;; An action set is the hash-of-lists-of-functions that define the
;; actions available on a resource type
(define (is-action-set? actions)
  (and (hash? actions)
       (for/and ([(k v) (in-hash actions)])
         (and (dict? v) (list? v)))))

(define (minimum-access-mask actions)
  (if (hash? actions)
      (for/hash ([(k v) (in-hash actions)])
        (values k (caar v)))
      (error 'not-action-set)))

(define (maximum-access-mask actions)
  (if (hash? actions)
      (for/hash ([(k v) (in-hash actions)])
        (values k (car (last v))))
      (error 'not-action-set)))

;; True if a given mask is a mask for the given action set; this is
;; the case if both have the same keys, and each value in the mask
;; exists in the list at the corresponding key in the action set hash.
(define (is-mask-for? actions mask)
  (for/and ([(k v) (in-hash actions)])
    (not (false? (assoc (dict-ref mask k) v)))))

(define (mask-index action-line)
  (lambda (access-level)
    (index-of action-line access-level
              (lambda (x y) (string=? (car x) y)))))

; Given an action set and a list of masks, return a mask with the
; highest access level per pline that was found in the masks.
(define (mask-join actions . masks)
  (for/hasheq ([(k v) (in-hash actions)])
    (values k (argmax (mask-index v)
                      (map (curryr dict-ref k) masks)))))

(define (apply-mask actions mask)
  (if (is-mask-for? actions mask)
      (for/hash ([(k v) (in-hash actions)])
        (let ((ix (+ 1 ((mask-index v) (dict-ref mask k)))))
          (values k (take v ix))))
      (error 'incompatible-action-mask)))
