#lang typed/racket/base

(require srfi/2
         (only-in racket/function const)
         (only-in racket/set set->list set-member?))

(provide optimal-prop-map)

;; given a map from keys to sets of things that "provide" those keys, and a list
;; of keys that you want, optimal-prop-map returns a map from keys to providers
;; such that the number of providers is minimized.
;; for example, say that 'p1 is provided by endpoints A & B, and 'p2 is provided
;; by B & C; then (get-props 'p1 'p2) would return '#hash((p1 . B) (p2 . B))).
;; because #{B} < #{A C}
(: optimal-prop-map (∀ (k v) (-> (HashTable k (Setof v)) k * (HashTable k v))))
(define (optimal-prop-map m . prop-list)
  (let* ([end-sat-map ; map from enpoints to the number of requested properties that they satisfy
          : (HashTable v Natural)
          (foldl (λ ([prop : k] [acc : (HashTable v Natural)])
                   (let ([endpoints : (Setof v) (hash-ref m prop)])
                     (foldl
                      (λ ([endpoint : v] [inner-acc : (HashTable v Natural)])
                        (hash-update inner-acc endpoint
                                     ;; hash update inserts 0 if no such key, then applies the update [+1] function;
                                     ;; thus, if endpoint isn't in the hash, it'll be inserted with value of 1
                                     (λ ([n : Natural]) : Natural (+ 1 n))
                                     (const 0)))
                      acc
                      (set->list endpoints))))
                 ((inst hash v Natural))
                 prop-list)]
         ;; that map as an alist, sorted by satisfaction count
         [esal ((inst sort (Pairof v Natural) Natural)
                (hash->list end-sat-map)
                > #:key cdr)])
    ;; now we discard the counts, for each property select the first function that provides that (i.e. for each property: for each [short-circuit] in esal: if that endpoint-function is in , return it), and we can prune any whose new counts are zero
    (foldl
     (λ ([prop : k] [acc : (HashTable k v)])
       (or (and-let* ([best-endpoint:p
                      (findf (λ ([x : (Pairof v Natural)])
                               (let ([fn (car x)]
                                     [props-endpoints (hash-ref m prop)])
                                 (set-member? props-endpoints fn)))
                             esal)]
                     [best-endpoint (car best-endpoint:p)])
                    (hash-set acc prop best-endpoint))
          (error "impossible")))
     ((inst hash k v))
     prop-list)))
