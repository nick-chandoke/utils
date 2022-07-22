#lang racket/base

;; in this module, because the context makes it ambiguous, the term "error" is used to mean
;; error or residual.

(require (only-in math/statistics mean stddev/mean correlation median quantile)
         (only-in racket/list takef partition)
         (only-in racket/set set set-add)
         (only-in racket/function identity)
         (only-in racket/math exact-ceiling exact-floor)
         (only-in "util.rkt" n-choose-2 or/null length>=? on ~>)
         math/matrix
         (only-in math/array array-ref)
         (only-in racket/math sgn))

(provide (all-defined-out))

;; this module is typed b/c arrays are much faster when typed
(module reg typed/racket/base
        (require math/matrix math/array
                 (only-in racket/math sgn) (only-in math/statistics mean)
                 (only-in math/special-functions flgamma-inc))
        (provide (except-out (all-defined-out) logit))
        
        (: logit (-> Real Real)) ; (-> Real Nonnegative-Real) ∉ exp's case->
        (define (logit x) (/ (+ 1 (exp (- x)))))

        ;; θ should maximize likelihood.
        ;; if deviance/log < ϵ then reject the null.
        ;; the null is that the predictor(s) predicted no better than if we'd predicted the average y value.
        ;; the maximized-likelihood null still has less predictive power than other models.
        ;; TODO: how to determine whether _each_ predictor sensibly improved the prediction? this would enable
        ;; discarding redundant or unpredictive predictors. i suppose one can calculate the confidence for all
        ;; unique combinations of predictors.
        (: deviance/log (-> (Matrix Real) (Matrix Real) (Matrix Real) Number))
        (define (deviance/log x y θ)
          (* 2 (let* ([γ  : Real (mean (in-array y))]
                      [γc : Real (- 1 γ)])
                 (- (for/sum : Number ([y (in-array y)] [p (in-array (matrix-map logit (matrix* (adjoin1 x) θ)))])
                       (+ (* y (log p)) (* (- 1 y) (log (- 1 p)))))
                    (* (array-size y) (+ (* γ (log γ)) (* γc (log γc))))))))

        ;; the confidence with which we reject the null i.e. deviance percentile.
        ;; this model fits better (has lower deviance) than <confidence/log>% of sets of data.
        ;; df is the number of predictors.
        (: confidence/log (-> Float Float Float))
        (define (confidence/log d df) (flgamma-inc (/ df 2) (/ d 2) #t #t))

        ;; [[x ...] ...] -> [[1 x ...] ...]
        (: adjoin1 (-> (Matrix Real) (Matrix Real)))
        (define (adjoin1 A)
          (let* ([S (array-shape A)])
            ((inst build-array Real) (vector (vector-ref S 0) (add1 (vector-ref S 1)))
                                       (λ ([is : In-Indexes])
                                         (let ([is (cast is (Vectorof Integer))])
                                           (if (= 0 (vector-ref is 1))
                                               1.
                                               (array-ref A (vector (vector-ref is 0) (sub1 (vector-ref is 1))))))))))

        ;; x should be such that rows represent points and cols represent features
        ;; y should be a column matrix
        ;; solves by newton-raphson where
        ;; ℓ = Σ[i]yᵢ(xt*β)-log(1+exp(xt*β))
        ;; ∂ℓ/∂β = (xt)(y-p)
        ;; ∂²ℓ/∂β² = x×(wx) where w = pᵢⱼ(1-pᵢⱼ) on the diagonal. x×(wx) = -(xt)wx; x is apparently skew symmetric.
        ;; see https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
        (: logreg (->* [(Matrix Real) (Matrix Real)] [Positive-Real] (Matrix Real)))
        (define (logreg x y [ϵ 0.00005])
          (let* ([x : (Matrix Real) (adjoin1 x)]
                 [xt : (Matrix Real) (matrix-transpose x)])
            (let loop : (Matrix Real)
              ([cnt : Natural 0] ; just in case we don't converge in 100 steps. should never happen, but ridiculous ϵ may enable it.
               [β : (Matrix Real) (make-matrix (vector-ref (array-shape x) 1) 1 0.)])
              (let* ([p : (Matrix Real) (matrix-map logit (matrix* x β))]
                     ;; let w's type be inferred. specifying In-Indexes will a bad time make.
                     [w ((inst diagonal-matrix Real) ((inst matrix->list Real) (array-map (λ ([x : Real]) (* x (- 1 x))) p)))]
                     [δ (matrix-solve (matrix* xt w x) (matrix* xt (matrix- y p)))])
                (if (or (< (abs (apply max (array->list δ))) ϵ) (and (= cnt 100)
                                                                     (printf "logreg didn't converge with ϵ = ~a in 100 iterations.\n" ϵ)))
                    β
                    (loop (add1 cnt) (matrix+ β δ)))))))

        ;; the polynomial function represented by a matrix
        ;; e.g. (let ([f (reg->fn (lagrange-polynomial '(1 2 3) '(2 5 10)))]) (f 4)) ; 17
        (: reg->fn (-> (Matrix Real) (-> Real Real)))
        (define ((reg->fn m) x) (array-ref (matrix* (vandermonde-matrix (list x) (matrix-num-rows m)) m) #(0 0)))

        ;; uncurried version of reg->fn, for convenience
        (: reg-fn (-> (Matrix Real) Real Real))
        (define (reg-fn m x) ((reg->fn m) x))

        ;; general function regression: given a set of (x,y) pairs, computes argmin[θ](Σ(y-f(x;θ))²)
        ;; where f is a linear combination of other functions e.g. f(x;θ) = θ₁sin(x) + θ₂cos(x).
        ;; this method would not work on θ₁sin(θ₂+x) since that's not linear in θ.
        ;; TODO: really, a complete consideration requires that i consider linear operators, e.g. the derivative.
        ;; apparently putting functions inside vectors quotes them...? weird. you need to specify fs as a list, not a vector.
        (: reg+sse (-> (Vectorof Real) (Vectorof Real) (Listof (-> Real Real)) [#:ridge Real] [#:Xovr (Option (Matrix Real))] (Values (Matrix Real) Real)))
        (define (reg+sse xs ys fs #:ridge [ridge 0.] #:Xovr [Xovr #f])
          (let* ([X : (Matrix Real) (or Xovr (build-matrix (vector-length xs) (length fs) (λ ([i : Integer] [j : Integer]) ((list-ref fs j) (vector-ref xs i)))))]
                 [y : (Matrix Real) (->col-matrix ys)]
                 [XT : (Matrix Real) (matrix-transpose X)]
                 [β-hat : (Matrix Real) (matrix* (matrix-inverse (matrix+ (matrix* XT X) (identity-matrix (matrix-num-rows XT) ridge))) XT y)]
                 [e-hat : (Matrix Real) (matrix- y (matrix* X β-hat))])
            (values β-hat (array-ref (matrix* (matrix-transpose e-hat) e-hat) #(0 0)))))
        
        ;; polynomial regression and sum of squared errors
        #| assumes that the degree is less than the number of points and that all x are independent.
           positive #:ridge permits solutions to multicolinear matrices;
           see <https://en.wikipedia.org/wiki/Tikhonov_regularization>.
           try different ridges to see which one minimizes sse for your particular data.
        |#
        (: polyreg+sse (-> Positive-Integer (Vectorof Real) (Vectorof Real) [#:ridge Nonnegative-Real] (Values (Matrix Real) Real)))
        (define (polyreg+sse deg xs ys #:ridge [ridge 0.])
          (reg+sse xs ys #f #:ridge ridge #:Xovr (vandermonde-matrix (vector->list xs) (add1 deg))))
        
        (: polyreg (-> Positive-Integer (Vectorof Real) (Vectorof Real) [#:ridge Nonnegative-Real] (Matrix Real)))
        (define (polyreg deg xs ys #:ridge [ridge 0.])
          (let-values ([(p _) (polyreg+sse deg xs ys #:ridge ridge)]) p))
        
        ;; smallest-degree polynomial that intersects all points
        ;; NOTE: works only for invertible matrices!
        (: lagrange-polynomial (∀ (a) (-> (Listof Real) (Matrix Real) (Matrix Real))))
        (define (lagrange-polynomial xs ys) (matrix-solve (vandermonde-matrix xs (length xs)) ys))

)

(define (sq x) (* x x))

;; solves a given non-linear unary function f(x*) = 0 by the itp method, provided that:
;; * the function's derivative at its zero is not zero
;; * ∃ [a,b] ⊂ ℝ² : sgn(f(a)) ≠ sgn(f(b))
;; * ∃! x* ∈ [a,b]
;; converges in ⌈log₂((a-b)/2e)⌉+1 or fewer iterations.
;; x^2+1 returns #f b/c no real solution. x^2-6 fails if bounds aren't provided b/c interval expands
;; from [-1,1] to [-2,2] to [-3,3], an interval which contains both roots; interval continues to
;; expand indefinitely; `solve` does not return. i could make it choose a root, but that'd be arbitrary,
;; so likely not "correct." just don't specify symmetric multi-root fns with default intervals, ok?
;; TODO: add option to allow user to provide definite interval, or just starting/guess interval.
(define (solve f [a -0.7] [b 1.2] [e 1e-5] [max-iters 30])
  (let-values ([(a b f) (let expand ([a a] [b b])
                          ;; TODO: check whether f(x) ∈ {fa, fb} is complex; if so, then
                          ;; don't expand the interval on x's side; mark it as 'noexpand.
                          ;; this requires adding the base case of both a & b being 'noexpand.
                          ;; this allows easy expansion for e.g. log(x).
                          (if (= (sgn (f a)) (sgn (f b)))
                              (let ([mid (/ (+ a b) 2)])
                                ;; mid may be less than 0, and interval may need to become e.g. [-2,6],
                                ;; so we can't just multiply mid by factors to expand interval.
                                (expand (- mid (* 2 (- mid a)))
                                        (+ mid (* 2 (- b mid)))))
                              (if (> (f a) 0)
                                  (values a b (compose1 - f))
                                  (values a b f))))])
    (let ([fa (f a)] [fb (f b)])
      (let ([2e (* 2 e)])
        (let rec ([j 0] [a a] [b b] [fa fa] [fb fb])
          (and (<= j max-iters)
            (let ([x12 (/ (+ a b) 2)])
              (if (< (- b a) 2e)
                  x12
                  (let* ([xf (/ (- (* a fb) (* b fa)) (- fb fa))]
                         [d (* (/ 0.2 (- b a)) (sq (- b a)))]
                         [s (sgn (- x12 xf))]
                         [xt (if (<= d (abs (- x12 xf))) (+ xf (* s d)) x12)]
                         [r (* e (expt 2 (- (add1 (ceiling (log (exact->inexact (/ (- b a) 2 e)) 2))) j)))]
                         [xitp (if (<= (abs (- xt x12)) r)
                                   xt
                                   (- x12 (* s r)))]
                         [yitp (f xitp)])
                    (cond [(> yitp 0) (rec (add1 j) a xitp fa yitp)]
                          [(< yitp 0) (rec (add1 j) xitp b yitp fb)]
                          [else       (rec (add1 j) xitp xitp fa fb)]))))))))))

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

;; robust but computationally expensive linear regression
;; NOTE: doesn't require tikhonov regularization to always
;; produce a solution!
(define theil-sen
  (case-lambda [(xs ys) (let ([m (median < (n-choose-2 (λ (x1 y1 x2 y2) (/ (- y2 y1) (- x2 x1))) xs ys))])
                          (~> (+ (median < (map (λ (x y) (- y (* m x))) xs ys))) (* m)))]
               [(as . rst) (let*-values ([(->x ->y) (cond [(null? rst) (values car cdr)]
                                                          [(null? (cdr rst)) (values (car rst) cdr)]
                                                          [else (values (car rst) (cadr rst))])]
                                         [(m) (median < (n-choose-2 (λ (a b) (/ ((on - ->y) b a) ((on - ->x) b a))) as))])
                                           (~> (+ (median < (map (λ (a) (- (->y a) (* m (->x a)))) as))) (* m)))]))

(define ((lincomb θ fs) x) (foldl (λ (p f s) (+ s (* p (f x)))) 0 θ fs))

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
