#lang racket/base

(require (only-in racket/function curry))
(provide (all-defined-out))

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

;; returns first list element matching a predicate or tail of first pair whose car matches a predicate.
;; this works on a mix of alist & list, which is a more useful structure than flat lists or alists.
;; as this is a mix of alists & lists, i'll call them "a/lists."
;; massoc with a/lists is a common. more generally, though, you'd loop over a list [stack], taking n elements where
;; n is related to the top of the stack.
(define (massoc k s)
  (let ([k (if (procedure? k) k (curry equal? k))])
    (let lp ([s s])
      (and (pair? s)
           (let ([c (car s)])
             (or (and (pair? c) (let ([y (k (car c))]) (and y (cdr c))))
                      (and (k c) c)
                      (lp (cdr s))))))))

;; massoc fold e.g. ((nested 3 cadr) '((1 2) (3 4 5))) => 5
;; ((nested 3 cadr last) '((1 2) (3 4 5))) => #f (b/c can't get last of 5)
(define ((nested . accs) x)
  (let R ([x x]
          [accs accs])
    (cond [(null? accs) x]
          [(not (pair? x)) #f]
          [else (let* ([f (car accs)]
                       [x (if (procedure? f) (f x) (massoc f x))])
                  (and x (R x (cdr accs))))])))

(define transpose (curry apply map list))

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
(define (pointwise f s #:comb [comb cons] . ss)
  (let R ([ss `(,s . ,ss)])
    (if (atom (car ss))
        (app f ss)
        (comb (R (map car ss)) (R (map cdr ss))))))

