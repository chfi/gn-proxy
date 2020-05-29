#lang racket

(provide (struct-out action)
         action-set->hash
         minimum-access-mask
         maximum-access-mask
         run-action
         mask-join
         apply-mask
         is-action-set?
         is-mask-for?)

;; The type for actions that can be run on a resource. Actions are
;; generally, but not always, unique to a single resource type. `fun`
;; is the actual function that is to be run, and must take two
;; parameters, one with the resource data, and a hash with any
;; additional required parameters, e.g. new data when editing
;; something.
;; See resource.rkt for examples
(struct
  action
  (id
   fun
   req-params)
  #:transparent)


;; Run the given action on the provided data, with the given additional
;; parameters.
(define (run-action action data params)
  ;; TODO add check that action matches resource type by comparing
  ;; keys in params to req-params field in action
  ((action-fun action) data params))

;; An action set is the hash-of-lists-of-functions that define the
;; actions available on a resource type. The hash level are the
;; different "branches" of actions, while each branch is a list
;; of actions of increasing required privilege.
(define (is-action-set? actions)
  (and (hash? actions)
       (for/and ([(k v) (in-hash actions)])
         (and (dict? v) (list? v)))))


;; It's useful to be able to partially represent an action set as
;; a tree, to show exactly what actions are available, in general,
;; on a resource type
(define (action-set->hash actions)
  (for/hash ([(k v) (in-hash actions)])
    (values k (map car v))))


;; A mask is a map from action branches to action IDs. In a sense
;; it is a subset of the action set; though it doesn't actually
;; contain the actions, it does describe which actions can be used.


;; True if a given mask is a mask for the given action set; this is
;; the case if both have the same keys, and each value in the mask
;; exists in the list at the corresponding key in the action set hash.
(define (is-mask-for? actions mask)
  (for/and ([(k v) (in-hash actions)])
    (not (false? (assoc (dict-ref mask k) v)))))

;; Return the mask for an action set that provides the least possible
;; level of access, i.e. only the first action in each branch.
(define (minimum-access-mask actions)
  (if (hash? actions)
      (for/hash ([(k v) (in-hash actions)])
        (values k (caar v)))
      (error 'not-action-set)))

;; Return the mask for an action set that provides full access.
(define (maximum-access-mask actions)
  (if (hash? actions)
      (for/hash ([(k v) (in-hash actions)])
        (values k (car (last v))))
      (error 'not-action-set)))

;; Indexing function used to compare the access level of two actions
;; in a branch.
(define (mask-index branch)
  (lambda (access-level)
    (index-of branch access-level
              (lambda (x y) (string=? (car x) y)))))

;; Return the "total" access that any number of masks for a given
;; action set cover. The access level for each branch is set to
;; the highest among the masks.
(define (mask-join actions . masks)
  (for/hasheq ([(k v) (in-hash actions)])
    (values k (argmax (mask-index v)
                      (map (curryr dict-ref k) masks)))))

;; Return a subset for the given action set as delimited by the mask.
;; The result is the accessible action set.
(define (apply-mask actions mask)
  (if (is-mask-for? actions mask)
      (for/hash ([(k v) (in-hash actions)])
        (let ((ix (+ 1 ((mask-index v) (dict-ref mask k)))))
          (values k (take v ix))))
      (error 'incompatible-action-mask)))
