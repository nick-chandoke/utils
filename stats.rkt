#lang racket/base

;; in this module, because the context makes it ambiguous, the term "error" is used to mean
;; error or residual.

(require (only-in math/statistics mean stddev/mean correlation median quantile)
         (only-in racket/list takef partition)
         (only-in racket/set set set-add)
         (only-in racket/function identity)
         (only-in racket/math exact-ceiling exact-floor)
         (only-in "../common/util.rkt" n-choose-2 or/null length>=? on ~>)
         math/matrix
         (only-in math/array array-ref))

(provide (all-defined-out))

(define (sq x) (* x x))

;; solves a given non-linear unary function f(x*) = 0 by the itp method, provided that:
;; * the function's derivative at its zero is not zero
;; * ∃ [a,b] ⊂ ℝ² : sgn(f(a)) ≠ sgn(f(b)), x* ∈ [a,b]
;; converges in ⌈log₂((a-b)/2e)⌉+1 or fewer iterations.
(define (solve f [a #f] [b #f] [e 1e-5])
  ;; TODO: automatically determine a & b if they aren't provided by user: [a (- x12 (* 2 (- a x12)))] [b (+ x12 (* 2 (- b x12)))]
  ;; remember then to make a & b default to #f.
  (let-values (#;[(a b) (if (and a b)
                          (values a b)
                          (let go ()
                            (values _ _)))])
    (let ([ya (f a)]
          [yb (f b)]
          [k1 (/ 0.2 (- b a))])
      ;; TODO: if ya or yb = 0 then that's our solution.
      ;; TODO: support yb < 0 < ya; that's needed to find one of the roots of (x+3)^2-4
      ;; NEXT: if 0 ∉ (f(a),f(b)) then expand (a,b) until condition is met
      (unless (and (< ya 0) (< 0 yb)) (raise-arguments-error 'solve "0 ∉ (f(a),f(b))" "f(a)" ya "f(b)" yb))
      ;; NEXT: if, after 0 ∈ (f(a),f(b)), if f(a) > 0, then f ← -f.
      (let ([2e (* 2 e)])
        (let rec ([j 0] [a a] [b b] [ya ya] [yb yb])
          ;; TODO: this process must account for unsolveable equations e.g. x^2+1, or else return their complex solution.
          (let* ([x12 (/ (+ a b) 2)]
                 [l2 (/ (- b a) 2)]
                 [n12 (ceiling (log (/ l2 e) 2))])
            (if (< (- b a) 2e)
                x12
                (let* ([xf (/ (- (* a yb) (* b ya)) (- yb ya))]
                       [d (* k1 (sq (- b a)))]
                       [s (sgn (- x12 xf))]
                       [xt (if (<= d (abs (- x12 xf))) (+ xf (* s d)) x12)]
                       [r (* e (expt 2 (- (+ n12 1) j)))]
                       [xitp (if (<= (abs (- xt x12)) r)
                                 xt
                                 (- x12 (* s r)))]
                       [yitp (f xitp)])
                  (cond [(> yitp 0) (rec (add1 j) a xitp ya yitp)]
                        [(< yitp 0) (rec (add1 j) xitp b yitp yb)]
                        [else       (rec (add1 j) xitp xitp ya yb)])))))))))

;; returns a function that accepts a single sequence of 2 items or two singleton sequences.
(define (fold2 f z0)
  (case-lambda [(s) (for/fold ([z z0]) ([(x y) s]) (f z x y))]
               [(xs ys) (for/fold ([z z0]) ([x xs] [y ys]) (f z x y))]))

;; the angle between two slopes
;; (angle a b) returns (- (angle b) (angle a)), so if
;; if b is less than a, then result will be negative.
;; remember to convert to degrees if desired.
(define (angle a b) (- (atan b) (atan a))) ; θ = tan(y/x). x = 1 => y = slope ∴ θ = atan(slope)

;; how extreme a point is. does not tell whether unusual or not.
;; NOTE: will throw div by 0 if σ = 0 (i.e. sum of squared errors = 0)
;; if this is a concern, use
;; (with-handlers ([exn:fail:contract:divide-by-zero? ...]) (z-score ...))
(define (z-score xs x #:mean [m #f]) (let ([μ (or m (mean xs))]) (/ (- x μ) (stddev/mean μ xs))))

;;; curve fitting
;; remember to consider what your population and sample are. consider whether
;; polynomials are expressed as column matrices (of shape #(n 1)) where
;; each nth row is a coefficient of x to the n-1th power.

;; the polynomial function represented by a matrix
;; e.g. (let ([f (reg->fn (lagrange-polynomial '(1 2 3) '(2 5 10)))]) (f 4)) ; 17
;(: reg->fn (-> (Matrix Float) (-> Float Float)))
(define ((reg->fn m) x)
  (array-ref (matrix* (vandermonde-matrix (list x) (matrix-num-rows m)) m) #(0 0)))

;; uncurried version of reg->fn, for convenience
;(: reg-fn (-> (Matrix Float) Float Float))
(define (reg-fn m x) ((reg->fn m) x))

;; for arrays representing monomials
(define (intercept m) (array-ref m #(0 0)))
(define (slope m)     (array-ref m #(1 0)))

;; robust but computationally expensive linear regression
;; NOTE: doesn't require tikhonov regularization to always
;; produce a solution!
(define theil-sen
  (case-lambda [(as . rst) (let*-values ([(->x ->y) (cond [(null? rst) (values car cdr)]
                                                          [(null? (cdr rst)) (values (car rst) cdr)]
                                                          [else (values (car rst) (cadr rst))])]
                                         [(m) (median < (n-choose-2 (λ (a b) (/ ((on - ->y) b a) ((on - ->x) b a))) as))])
                                           (~> (+ (median < (map (λ (a) (- (->y a) (* m (->x a)))) as))) (* m)))]
               [(xs ys) (let ([m (median < (n-choose-2 (λ (x1 y1 x2 y2) (/ (- y2 y1) (- x2 x1))) xs ys))])
                          (~> (+ (median < (map (λ (x y) (- y (* m x))) xs ys))) (* m)))]))

;; general function regression: given a set of (x,y) pairs, computes argmin[θ](Σ(y-f(x;θ))²)
;; where f is a linear combination of other functions e.g. f(x;θ) = θ₁sin(x) + θ₂cos(x).
;; this method would not work on θ₁sin(θ₂+x) since that's not linear in θ.
;; TODO: really, a complete consideration requires that i consider linear operators, e.g. the derivative.
;; apparently putting functions inside vectors quotes them...? weird. you need to specify fs as a list, not a vector.
(define (reg+sse xs ys fs #:ridge [ridge 0.] #:Xovr [Xovr #f])
  (let* ([X (or Xovr (build-matrix (vector-length xs) (length fs) (λ (i j) ((list-ref fs j) (vector-ref xs i)))))]
         [y (->col-matrix ys)]
         [XT (matrix-transpose X)]
         [β-hat (matrix* (matrix-inverse (matrix+ (matrix* XT X) (identity-matrix (matrix-num-rows XT) ridge))) XT y)]
         [e-hat (matrix- y (matrix* X β-hat))])
    (values β-hat (array-ref (matrix* (matrix-transpose e-hat) e-hat) #(0 0)))))

(define ((lincomb θ fs) x) (foldl (λ (p f s) (+ s (* p (f x)))) 0 θ fs))

;; polynomial regression and sum of squared errors
#| assumes that the degree is less than the number of points and that all x are independent.
   positive #:ridge permits solutions to multicolinear matrices;
   see <https://en.wikipedia.org/wiki/Tikhonov_regularization>.
   try different ridges to see which one minimizes sse for your particular data.
|#
(define (polyreg+sse deg xs ys #:ridge [ridge 0.]) (reg+sse xs ys #:ridge ridge #:Xovr (vandermonde-matrix xs (add1 deg))))

;(: polyreg (∀ (a) (-> Positive-Integer (-> a Float) (-> a Float) (Listof a) [#:ridge Nonnegative-Float] (Matrix Float))))
(define (polyreg deg ->x ->y as #:ridge [ridge 0.])
  (let-values ([(p _) (polyreg+sse deg ->x ->y as #:ridge ridge)]) p))

;; smallest-degree polynomial that intersects all points
;; NOTE: works only for invertible matrices!
;(: lagrange-polynomial (∀ (a) (-> (-> a Float) (-> a Float) (Listof a) (Matrix Float))))
(define (lagrange-polynomial ->x ->y as)
  (matrix-solve (vandermonde-matrix (map ->x as) (length as)) (->col-matrix (map ->y as))))

;; returns Q₁, Q₂ (median,) and Q₃
;(: quartiles (-> (Listof Float) [#:sorted? Boolean] (Values Float Float Float)))
(define (quartiles xs #:sorted? [s? #f])
  (let ([xs (if s? xs (sort xs <))] [l (length xs)])
    (values (list-ref xs (exact-ceiling (/ l 4)))
            (list-ref xs (round (/ l 2)))
            (list-ref xs (exact-floor (/ (* 3 l) 4))))))

(define (iqr xs #:sorted? [s? #f])
  (let*-values ([(q₁ q₂ q₃) (quartiles xs #:sorted? s?)]) (- q₃ q₁)))

;; number of iqr's from median
(define (iqr-score xs x #:sorted? [s? #f])
  (let*-values ([(q₁ q₂ q₃) (quartiles xs #:sorted? s?)])
    (/ (- x q₂) (- q₃ q₁))))

;; more robust than using z-scores of error to determine outliers.
(define (outlier?/boxplot xs x #:sorted? [s? #f])
  (let*-values ([(q₁ _ q₃) (quartiles xs #:sorted? s?)]
                [(1.5iqr) (* 1.5 (- q₃ q₁))])
    (or (< x (- q₁ 1.5iqr)) (> x (+ q₃ 1.5iqr)))))

;; like (filter? (negate (curry outlier?/boxplot xs)) xs) but computes quartiles only once
(define (remove-outliers/boxplot xs #:sorted? [s? #f])
  (let*-values ([(q₁ _ q₃) (quartiles xs #:sorted? s?)]
                [(1.5iqr) (* 1.5 (- q₃ q₁))])
    (filter (λ (x) (and (>= x (- q₁ 1.5iqr)) (<= x (+ q₃ 1.5iqr)))) xs)))

;; get only the outliers. inverse of remove-outliers/boxplot.
;; TODO: use takef on the sorted list. more efficient than filter.
#;(: filter-outliers/boxplot (->* ((Listof Float)) ((U 'low 'high 'both) #:sorted? Boolean) (Listof Float)))
(define (filter-outliers/boxplot xs [type 'both] #:sorted? [s? #f])
  (let*-values ([(q₁ _ q₃) (quartiles xs #:sorted? s?)]
                [(1.5iqr) (* 1.5 (- q₃ q₁))]
                [(cmp-fn) (case type
                            [(both) (λ (x) (or (<= x (- q₁ 1.5iqr)) (>= x (+ q₃ 1.5iqr))))]
                            [(low)  (λ (x) (<= x (- q₁ 1.5iqr)))]
                            [(high) (λ (x) (>= x (+ q₃ 1.5iqr)))])])
    (filter cmp-fn xs)))

;; sum of squared errors
(define sse (fold2 (λ (Σ obs exp) (+ Σ (let ([e (- obs exp)]) (* e e)))) 0))

;; sum of absolute errors. the often-impractical (b/c non-analytical) preferred version of sse.
(define sae (fold2 (λ (Σ obs exp) (+ Σ (abs (- obs exp)))) 0))

;; not only the modes, but also the elements by which each mode was calculated
;; NOTE: the "to real" function is assumed to be inexpensive. use a caching mechanism
;; (e.g. a HashTable) if appropriate.
;; TODO: default delta should be determined by an appropriate method:
;; one that separates small values from large values. maybe even have multiple
;; deltas.
#;(: modes/verbose (∀ (a) (->* ((-> a Float) (Listof a)) ((Option Float)) (Values (HashTable Float (NonEmpty a)) Float))))
(define (modes/verbose f s [d #f])
  (if (length>=? 3 s)
      (let ([d (or d (let*-values ([(dists) (sort (n-choose-2 (λ (a b) (abs (- (f a) (f b)))) s) <)]
                                  [(q₁ _ q₃) (quartiles dists #:sorted? #t)]
                                  [(1.5iqr) (* 1.5 (- q₃ q₁))])
                      (quantile 0.1 < dists)))])
        (values (let loop ([s s])
                     (if (null? s)
                         (hash)
                         (let*-values ([(p) (car s)]
                                       [(fp) (f p)]
                                       [(nears fars) (partition (λ (r) (<= (abs (- (f r) fp)) d)) (cdr s))])
                           (if (null? nears)
                               (loop fars)
                               (let ([nears-and-p (cons p nears)]) ; (NonEmpty a)
                                 (hash-set (loop fars) (real->double-flonum (mean (map f nears-and-p))) nears-and-p))))))
                d))
      (values (hash) 0.)))

;; set of values each of which is nears many other values
#;(: modes (->* ((Listof Float)) ((Option Float)) (Listof Float)))
(define (modes s [d #f]) (let-values ([(ht _) (modes/verbose identity s d)]) (hash-keys ht)))

;; precondition: p ∈ [0,1], s is a sorted vector.
(define (percentile/linear-interpolation p s)
  (let* ([p (/ p 100)]
         [n (vector-length s)]
         [off (sub1 (cond [(<= p (/ (add1 n))) 1]
                          [(>= p (/ n (add1 n))) n]
                          [else (* p (add1 n))]))]
         [eoff (exact-floor off)])
    (if (= (sub1 n) eoff)
        (vector-ref s eoff)
        (let ([a (vector-ref s eoff)])
          (+ a (* (- off eoff) (- (vector-ref s (add1 eoff)) a)))))))
