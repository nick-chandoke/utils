#lang racket/base

;; NEXT: build parser on masssoc, nested, and split-on for parser-based programming
;; also things returning non-lists or lists (e.g. massoc) should maybe just return
;; lists. instead of list-ref generalized to it or cdr, nested, e.g. (list-ref n 'cdr s) to get cdr of nth element,
;; all fns should return a list, which means nothing by null, something by car, or list by itself. this is extra
;; symmetry, which should only be beneficial.

(require (only-in racket/function curry) (only-in racket/list split-at make-list))
(provide (except-out (all-defined-out) app atom pointwise))

;; "just use lists." this library implements that document's ideas, plus those of tacitlisp, and best paradigms,
;; namely, in summary, the paradigm: programs are structures (relations/pairs) that're (dis)integrated until they're
;; empty, which may be a base case for the program, i.e. that may be a terminal state. program form is organic, self-modifying,
;; and only loosely specified by the programmer, allowing axioms to infer control flow details.

;; idk maybe pil is the only decent choice, like racket might be doomed by its inability to mutate items in lists.

;; simple map definition. i thought that i'd needed it, but not,
;; so here it remains.
#;(define (map f . ss)
  (let R ([ss ss])
    (if (pair? (car ss))
        `(,(apply f (map car ss)) . ,(R (map cdr ss)))
        '())))

;; over a k-ary list, rather than map an f of arity n → 1, f is arity n → m, and map-many
;; returns an m×k list matrix.
;; (map-many (λ (x y z) `(,(* x z) ,(+ x y))) '(1 2 3) '(10 20 30) '(1 0 1))
;; => '((1 0 3) (11 22 33))
(define (map-many f . ss)
  (if (pair? (car ss))
      (let* ([num-rets (length (apply f (map car ss)))])
        (let R ([ss ss])
          (if (pair? (car ss))
              (map cons (apply f (map car ss)) (R (map cdr ss)))
              (make-list num-rets '()))))
      '()))

;; returns first list element matching a predicate or tail of first pair whose car matches a predicate.
;; this works on general lists e.g. (massoc even? '(1 3 (4 5 6) (a . b))) => (4 5 6). using a predicate
;; returns the whole pair, whereas looking-up by element returns the cdr of the matched list:
;; (massoc 'a '(1 3 (4 5 6) (a . b))) => 'b. this behavior is chosen because if you're looking-up by predicate,
;; then you don't know what item may match; however, if you lookup by [equality with] an object, then if the
;; match succeeds, then having the matched object in the returned list is redundant.
;; as this is a mix of alists & lists, i'll call them "a/lists."
;; massoc with a/lists is a common. more generally, though, you'd loop over a list [stack], taking n elements where
;; n is related to the top of the stack.
;; massoc works for flat alists, i.e. those of the form (k1 v1 k2 v2 ...), too:
;; (massoc 'a '(a 5 b 6)) => 5.
(define (massoc k/p S [err (λ (k s) (error (if (procedure? k)
                                               (format "massoc: no match for given predicate on ~s" k s)
                                               (format "massoc: key ~a missing from ~s" k s))))])
  (let ([k (if (procedure? k/p) k/p (curry equal? k/p))])
    (let lp ([s S])
      (if (pair? s)
          (let* ([c (car s)]
                 [d (if (pair? c)
                        (and (k (car c)) (list (if (procedure? k/p) c (cdr c))))
                        (and (k c) `(,s)))]) ; k is sensible here only if it's a procedure
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

#| returns (a . b) where (car b) is the first
   element that satisfies the given predicate/value.
   like massoc, if k/p is a procedure then its first match
   is returned in b; else it's not.
   (split-on even? '(1 3 2 6)) => ((1 3) 2 6)
   (split-on 'a '(a b c d e)) => (() 5 b 6)
   (split-on 'x '(a b c d e)) => ((a b c d e))
   null cdr => not found.
   null car => match on car of input list.
|#
(define (split-on k/p s)
  (let* ([k (if (procedure? k/p) k/p (curry equal? k/p))]
         [r '()]
         [past (let lp ([s s])
                 (if (pair? s)
                     (if (k (car s))
                         (begin (set! r (if (procedure? k/p) s (cdr s)))
                                '())
                         `(,(car s) . ,(lp (cdr s))))
                     '()))])
    `(,past . ,r)))

;; lookup key in flat alist; return n following vals or, if key is a predicate,
;; n vals including first matching predicate.
;; (extract 'x '(a 1 b 2 x 3 4 y 6) 2) => ((3 4) (a 1 b 2 y 6))
;; (extract 'y '(a 1 b 2 x 3 4 y 6)) => ((6) (a 1 b 2 x 3 4))
;; (extract 'z '(a 1 b 2 x 3 4 y 6)) => (() (a 1 b 2 x 3 4 y 6))
(define (extract k/p s [n 1])
  (let ([a (split-on k/p s)])
     (if (null? (cdr a)) ; key not in list
         `(() . ,(car a))
         (let-values ([(vs rst) (split-at (cdr a) n)])
           `(,vs . ,(append (car a) rst))))))

(define transpose (curry apply map list))

;;; structures

;; (atom 3) => 3
;; (atom '(3)) => 3
;; (atom '(3 . 5)) => #f
(define (atom x) (if (pair? x) (if (null? (cdr x)) (car x) #f) x))

;; apply that works on both proper & improper lists
(define (app f ss) (map (λ (s) (if (and (pair? s) (null? (cdr s))) (car s) s)) ss))

#| e.g.
(pointwise + '((9  1) (2  6  7) 3)
             '((3  2) (-2 6  5) 1))
--> ((12 3) (0 12 12) 4)

(pointwise + '((9  1) (2  6  7) 3)
             '((3  2) (-2 6  5) 1)
           #:comb +)
--> 43 (the sum of the flattened resultant row)

if #:comb  is unprovided, does deep map? maybe only for improper lists?
|#
(define (pointwise f s #:comb [comb cons] . ss)
  (let R ([ss `(,s . ,ss)])
    (if (atom (car ss))
        (app f ss)
        (comb (R (map car ss)) (R (map cdr ss))))))
