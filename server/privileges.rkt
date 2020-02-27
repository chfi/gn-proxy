#lang racket

(provide privilege-line
         minimum-access-mask
         maximum-access-mask
         mask-join)

; A `privilege-line` is a set of privileges that have a monotonically
; increasing structure, e.g. no access <- can read <- can read & write
;; (struct privilege-line (actions))
(define (privilege-line name actions)
  `(,name . ,actions))

(define (action name fun)
  `(,name . ,fun))


(define (minimum-access-mask pls)
  (dict-map pls
            (lambda (k v)
              (if (and (dict? pls) (list? pls))
                  (cons k (caar v))
                  (error 'not-privilege-line)))))

(define (maximum-access-mask pls)
  (dict-map pls
            (lambda (k v)
              (if (and (dict? pls) (list? pls))
                  (cons k (car (last v)))
                  (error 'not-privilege-line)))))

;; True if a given mask is a mask for the given map of plines; this is
;; the case if both have the same keys, and each value in the mask
;; exists in the list at the corresponding key in the plines map.
(define (is-mask-for? plines mask)
  (andmap
   (lambda (x)
     (not (false? (assoc (dict-ref mask (car x))
                         (cdr x)))))
   plines))



; Given a plines poset and a list of masks, return a mask with the
; highest access level per pline that was found in the masks.
(define (mask-join plines . masks)
  (define (mask-index pline)
    (lambda (access-level)
      (index-of pline access-level
                (lambda (x y) (eq? (car x) y)))))
  (dict-map plines
            (lambda (k v)
              (cons k (argmax (mask-index v)
                              (map (curryr dict-ref k) masks))))))
