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

;; simple map definition. i thought that i'd needed it, but not,
;; so here it remains.
#;(define (map f . ss)
  (let R ([ss ss])
    (if (pair? (car ss))
        `(,(apply f (map car ss)) . ,(R (map cdr ss)))
        '())))

;; returns first list element matching a predicate or tail of first pair whose car matches a predicate.
;; this works on general lists e.g. (massoc even? '(1 3 (4 5 6) (a . b))) => (4 5 6). using a predicate
;; returns the whole pair, whereas looking-up by element returns the cdr of the matched list:
;; (massoc 'a '(1 3 (4 5 6) (a . b))) => 'b. this behavior is chosen because if you're looking-up by predicate,
;; then you don't know what item may match; however, if you lookup by [equality with] an object, then if the
;; match succeeds, then having the matched object in the returned list is redundant.
;; as this is a mix of alists & lists, i'll call them "a/lists."
;; massoc with a/lists is a common. more generally, though, you'd loop over a list [stack], taking n elements where
;; n is related to the top of the stack.
(define (massoc k/p S [err (λ (k s) (error (if (procedure? k)
                                               (format "massoc: no match for given predicate on ~s" k s)
                                               (format "massoc: key ~a missing from ~s" k s))))])
  (let ([k (if (procedure? k/p) k/p (curry equal? k/p))])
    (let lp ([s S])
      (if (pair? s)
          (let* ([c (car s)]
                 [d (if (pair? c)
                        (and (k (car c)) (list (if (procedure? k/p) c (cdr c))))
                        (and (k c) (list c)))]) ; k is sensible here only if it's a procedure
            (if (pair? d)
                (car d)
                (lp (cdr s))))
          (if (procedure? err)
              (err k/p S)
              err)))))

;; lookup nested keys in a structure.
;; ((nested 3 cadr) '((1 2) (3 4 5))) => 5.
;; ((nested 3 cadr last) '((1 2) (3 4 5))) => #f (b/c can't get last of 5).
;; has funny behavior if a pair is given: ((nested 'x) '(x . 45)) => 'x.
(define ((nested . accs) x)
  (let R ([x x]
          [accs accs])
    (cond [(null? accs) x]
          [(not (pair? x)) #f]
          [else (let* ([f (car accs)]
                       ;; nested handles fns its own way, using
                       ;; massoc only for non-function keys.
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

