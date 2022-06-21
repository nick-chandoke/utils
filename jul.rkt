#lang racket/base

;; "just use lists." this library implements that document's ideas, plus those of tacitlisp, and best paradigms,
;; namely, in summary, the paradigm: programs are structures (relations/pairs) that're (dis)integrated until they're
;; empty, which may be a base case for the program, i.e. that may be a terminal state. program form is organic, self-modifying,
;; and only loosely specified by the programmer, allowing axioms to infer control flow details.

;; idk maybe pil is the only decent choice, like racket might be doomed by its inability to mutate items in lists.

;; (atom 3) => 3
;; (atom '(3)) => 3
;; (atom '(3 . 5)) => #f
(define (atom x) (if (pair? x) (if (null? (cdr x)) (car x) #f) x))

;; apply that works on both proper & improper lists
(define (app f ss) (map (λ (s) (if (and (pair? s) (null? (cdr s))) (car s) s)) ss))

;; this works on a mix of alist & list, which is a more useful structure than strict lists or alists.
;; as this is a mix of alists & lists, i'll call them "a/lists."
;; lookup like this...i'd think that it's obviated by loop.
#;(define (massoc k s [p equal?])
  (let lp ([s s])
    (if (null? s)
        #f
        (let ([c (car s)])
          (if (or (and (pair? c) (p k (car c))) (p k c))
              c
              (lp (cdr s)))))))

(define (transpose xss) (apply map list xss))

;; structures

#| e.g.
(pointwise + '((9  1) (2  6  7) 3)
             '((3  2) (-2 6  5) 1))
--> ((12 3) (0 12 12) 4)

(pointwise + '((9  1) (2  6  7) 3)
             '((3  2) (-2 6  5) 1)
           #:comb +)
--> 43 (the sum of the flattened resultant row)
|#
(define (pointwise f s . ss #:comb [comb cons])
  (let R ([ss `(,s . ,ss)])
    (if (atom (car ss))
        (app f ss)
        (comb (R (map car ss)) (R (map cdr ss))))))

