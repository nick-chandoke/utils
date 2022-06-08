#lang racket/base
(provide (except-out (combine-out (all-defined-out)
                                  on-interval)
                     __lms))

(require (only-in "ring-buffer.rkt" in-interval)
         (for-syntax racket/base
                     (only-in racket/function curry)
                     (only-in racket/list last range drop)
                     (only-in racket/string string-join)
                     (only-in racket/syntax syntax-local-eval)
                     syntax/parse)
         compatibility/defmacro
         (only-in racket/bool symbol=?)
         (only-in racket/file make-temporary-file)
         (only-in racket/format ~a)
         (only-in racket/function curry identity const)
         (only-in racket/list make-list split-at splitf-at range shuffle dropf takef)
         (only-in racket/match match)
         (only-in racket/math exact-floor)
         (only-in racket/port copy-port port->string)
         (only-in racket/sequence sequence->list sequence-ref sequence-tail empty-sequence sequence-append)
         (only-in racket/set set-member?)
         (only-in racket/string string-split string-suffix? string-replace non-empty-string? string-prefix? string-trim string-join)
         (only-in racket/system process process/ports)
         (only-in racket/vector vector-split-at vector-append vector-member vector-copy vector-sort!)
         (only-in srfi/13 string-fold)
         srfi/2
         syntax/parse/define)

;; to read from processes, use system*; see §system processes & ipc in racket notes
;; e.g. (P #f (λ (x) (P x port->string "tr h z")) "echo" "hi")
;; if you want synchronous processes, set current input/output ports, and use system*.
;; TODO: i don't use this function at all; i use its body explicitly in order to
;; close the input port later. make that more elegant
#;(define (P in f cmd . args)
  (match (apply process*/ports #f in 'stdout (find-executable-path cmd) args)
         [(list output input _pid _err ask)
          (ask 'wait)
          (begin0 (f output)
                  (close-input-port output)
                  (when input (close-output-port input)))]))

(define (flip f) (λ (x y) (f y x)))

;; this works on a mix of alist & list, which is a more useful structure than strict lists or alists.
;; as this is a mix of alists & lists, i'll call them "a/lists."
(define (massoc k s [p equal?])
  (let lp ([s s])
    (if (null? s)
        #f
        (let ([c (car s)])
          (if (or (and (pair? c) (p k (car c))) (p k c))
              c
              (lp (cdr s)))))))

;; take or null
(define (</ n s)
  (define f #f)
  ;; if, at loop's end, (< i n), then (set! f #f)
  (let ([x (let go ([i 0] [s s])
             (if (null? s)
                 (if (< i n)
                     (set! f '())
                     '())
                 (cons (car s) (go (+ i 1) (cdr s)))))])
    (or f x)))

;; take what we can
(define (<- n s)
  (let go ([i 0] [s s])
    (if (or (>= i n) (null? s))
        '()
        (cons (car s) (go (+ i 1) (cdr s))))))

(define (alist . args0)
  (let loop ([args args0])
    (cond [(null? args) null]
          [(null? (cdr args)) (raise-argument-error 'alist "even number of arguments" args0)]
          [else (let-values ([(a b) (safe-split-at args 2)]) (cons (apply cons a) (loop b)))])))

(define (list-transpose xss) (apply map (lambda X (apply list X)) xss))

;; TODO: use in-syntax to replace all ()'s.
;; compose functions from right to left e.g.
;; ((~> (map (curry + 10)) (cons 20) cdr) (range 4)) = '(30 11 12 13)
;; ((~> (* 3) (/ () 5)) 10) = 6
;; NOTE: hole works only in lists used directly in ~> e.g. (~> (map (/ () 3)) add1)
;; is illegal syntax but (~> (map (~> (/ () 3))) add1) is legal (though cumbersome.)
(define-syntax (~> stx)
  (syntax-parse stx
    [(_ ((~and x (~not ())) ...+)) #'(curry x ...)]
    [(_ (x ...+))
     #:with (X ...) (map (λ (s)
                           (if (null? (syntax-e s))
                               #'y
                               s))
                         (attribute x))
     #'(λ (y) (X ...))]
    [(_ x xs ...+) #'(compose1 (~> x) (~> xs ...))]
    [(_ x) #'x]))

(define-syntax-rule (or-null xs body ...) (if (null? xs) null (begin body ...)))

;; return first non-null expression, or null if all null
(define-syntax (or/null stx)
  (syntax-parse stx
    [(_) #'null]
    [(_ x) #'x]
    [(_ x xs ...+) #'(let ([y x]) (if (null? y) (or/null xs ...) y))]))

(define-syntax (non-null stx)
  (syntax-parse stx
    [(_ x:id) #'(if (null? x) (error (format "supposedly non-null expression is null: ~a" #'x)) x)]
    [(_ x:expr) #'(let ([y x]) (non-null y))]))

;; defmacro def left here for easy impl in other lisps
#;(define-macro (cond-let . args)
  (let loop ([rst args])
    (if (null? rst)
        '(void)
        (let* ([clause (car rst)]
               [with-arrow? (member '=> clause)]) ; somewhy splitf-at (curry equal? '=>) fails
          (cond [(equal? 'else (car clause)) `(begin ,@(cdr clause))] ; yes, else can short the computation. why not?
                [(null? (cdr clause)) (let ([x (gensym)]) `(let ([,x ,(car clause)]) (if ,x ,x ,(loop (cdr rst)))))]
                [with-arrow? `(let ([,(cadr with-arrow?) ,(car clause)])
                                (if ,(cadr with-arrow?)
                                    (begin ,@(cddr with-arrow?))
                                    ,(loop (cdr rst))))]
                [else `(if ,(car clause) ,(cadr clause) ,(loop (cdr rst)))])))))

#| cond-let is to clojure's if-let as cond is to if.
   adds an arrow operator to cond, and allows `else` to be used anywhere, any number of times;
   the first occurence shorts the computation and returns its associated value; anything after
   the first `else` clause isn't even in the expanded expression.
   e.g. (cond-let [(and #f 3) => x (add1 x)]
                  [(values 4 6) => (x y) (>= x y) (sub1 (+ x y))]
                  [(symbol? "cat") 20]
                  [else 3])
|#
(define-syntax (cond-let stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ [(~literal else) e ...+] . _) #'(begin e ...)]
    [(_ [g (~literal =>) (x ...) p e ...+] . rst) #'(let-values ([(x ...) g]) (if p (begin e ...) (cond-let . rst)))]
    [(_ [g (~literal =>) x e ...+] . rst) #'(let ([x g]) (if x (begin e ...) (cond-let . rst)))]
    [(_ [p e ...+] . rst) #'(if p (begin e ...) (cond-let . rst))]))

;; e.g. (until ([c 0]) (>= c 10) (displayln c) (add1 c))
(define-syntax-parse-rule (until vars cond a ...+ upd)
  (let loop vars (unless cond a ... (loop upd))))

;; like `until`
(define-syntax-parse-rule (while vars cond a ...+ upd)
  (let loop vars (when cond a ... (loop upd))))

;; easily define an identifier macro
(define-syntax-rule (define-syntax:id name expr ...) ; NOTE: :id is NOT a class! this is syntax-case, not syntax-parse!
  (define-syntax (name stx)
    (syntax-case stx () [name (identifier? #'name) #'(begin expr ...)])))

(define-syntax-rule (thread: progn ...) (thread (λ () progn ...)))

#|
(no-pass-init-field [id : type] ...)
init-field but never relates subclass' constructor to superclass'.
allows subclasses to accept parameters of the same name as parent's,
passing functions of those parameters to the parent constructor, e.g.

(class (class object% (super-new)
         (no-pass-init-field x y))
  (init x y)
  (super-new [x (add1 (or (string->number x) 0))] [y (or y 0)]))
|#
;; TODO: remove typing
#;(define-macro (no-pass-init-field . args)
  (let ((inits-and-fields (for/list ([arg args])
                            (let ([gs (gensym)]
                                  [id (car arg)]
                                  [type (caddr arg)])
                              (cons (list (list gs id) ': type)
                                 (list id ': type gs))))))
    `(begin (init ,@(map car inits-and-fields))
            (field ,@(map cdr inits-and-fields)))))

#| (cons-id&val-if-truthy alist0 id ...)
   produces an alist. for each id, if its value is truthy, then the pair of the identifier-as-a-symbol and the identifier's value is (purely) consed into alist e.g.
   (let ([x #f] [y 4] [z "hi"])
     (cons-id&val-if-truthy null x y z))
   => '((y . 4) (z . "hi"))
|#
(define-syntax (cons-id&val-if-truthy stx)
  (syntax-parse stx
    [(_ alist k ...) #'(foldr (λ (p acc) (if (cdr p) (cons p acc) acc))
                              alist
                              (list (cons (quote k) k) ...))]))

;; print first cause of failure in an `and` form
;; if first arg is #t; if first arg is #f then it's ordinary `and`.
(define-syntax (and/print-fail-when stx)
  (syntax-parse stx
    [(_ switch [reason e] ...)
     (if (syntax-local-eval #'switch)
         #'(and (let ([s e]) (or s (begin (printf "fail: ~a~n" reason) #f))) ...)
         #'(and e ...))]))

;; output's a little dirty; may print an unnecessary trailing newline. lines after first are indented by one space.
(define-macro (print-vars . args)
  `(printf ,(string-append
             (string-join
              (for/list ([v args] [k (in-naturals 1)])
                (string-append (symbol->string v) ": ~a" (if (= 0 (modulo k 3)) "~n" " "))))
             "~n") ,@args))

;; (pret name expr opt-func)
(define-macro (pret . args)
  (let ([g (gensym)] [f (if (null? (cddr args)) #f (caddr args))])
    `(let ([,g ,(cadr args)]) (printf "~a: ~a~n" ,(car args) ,(if f `(,f ,g) g)) ,g)))

(define (list->maybe xs) (if (null? xs) #f (car xs)))
(define (maybe->list m) (if m (list m) null))
(define ((on bin un) a b) (bin (un a) (un b)))

;; e.g. ((λcase c [(3) 4] [else (add1 c)]) 3) => 4
(define-syntax (λcase stx)
  (syntax-parse stx [(_ var:id . rst) #'(λ (var) (case var . rst))]))

;; TODO
;; e.g. (cond/maybe [(and #f 3) => x (add1 x)] [(and #t 1) => x (sub1 x)]) => 0

;;; miscellaneous basic functions

(define (die s [ec 1]) (displayln s (current-error-port)) (exit ec))

;; usually used like (∞ (λ () ...))
(define (∞ f) (f) (∞ f)) ;; wrapping f-in-the-body in λ, i.e. (define (∞ f) (λ () (f)) (∞ f)), is NOT an alternative to wrapping f-as-a-parameter in λ! wrapping in the body will cause non-termination.

;;; iterators & sequences

;; (: sequence-last (∀ (a ...) (-> (Sequenceof a ... a) (Option (List a ... a)))))
(define (sequence-last s)
  (let-values ([(more? next) (sequence-generate (in-values-sequence s))])
    (let rec ([ret #f]) (if (more?) (rec (next)) ret))))

;; (: sequence-take (∀ (a) (-> Natural (Sequenceof a) (Listof a))))
(define (sequence-take n s)
  (let-values ([(more? next) (sequence-generate s)])
    (let rec ([k 0])
      (if (and (< k n) (more?))
          (cons (next) (rec (add1 k)))
          null))))

;; check the first output for proper length
;; (: sequence-split-at (∀ (a) (-> Natural (Sequenceof a) (Values (Listof a) (-> Boolean) (-> a)))))
(define (sequence-split-at n s)
  (let-values ([(more? next) (sequence-generate s)])
    (values (let rec ([k 0])
                 (if (and (< k n) (more?))
                     (cons (next) (rec (add1 k)))
                     null))
            more? next)))

(define (sequence-argmin <= f s)
  (let*-values ([(a) (sequence-ref s 0)]
                [(m _) (for/fold ([ex a] [e (f a)]) ([x (sequence-tail s 1)])
                         (let ([y (f x)])
                           (if (<= y e)
                               (values x y)
                               (values ex e))))])
    m))

;; like in-parallel except combines values rather than sequences
#;(: sequence-comb (∀ (a b ...) (-> (Sequenceof a) (-> (Sequenceof a) b) ... b (Values b ... b))))
#;(define (sequenceof s . fs)
  (let-values ([(more? next) (sequence-generate s)])
    (if (more?)
        _
        (values _ ...))))

;; adapted from haskell's prelude for this strict language
;; the element that first matches the predicate is not in
;; the resultant list.
;; e.g. (iterate-until (curry = 10) add1 0) is
;; effectively equal to (range 10)
;; (: iterate-until (∀ (a) (-> (-> a Boolean) (-> a a) a (Listof a))))
(define (iterate-until p f x) (let rec ([x x]) (if (p x) null (cons x (rec (f x))))))

#| uncons from list of lists as though the list were flat.
assumes that the input list contains to null elements.
loop example (effectively (for-each displayln (range 2 10))):
(let loop ([xs '((2 3 4) (5 6 7) (8 9))])
  (let ([p (uncons-xss xs)])
    (when p (displayln (car p)) (loop (cdr p)))))
|#
;; (: uncons-xss (∀ (a) (-> (Listof (Listof a)) (Option (Pairof a (Listof (Listof a)))))))
(define (uncons-xss xss) (cond [(null? xss) #f]
                               [(null? (car xss)) #f]
                               [else (let ([ε (car xss)])
                                       (cons (car ε) (if (null? (cdr ε))
                                                         (cdr xss)
                                                         (cons (cdr ε) (cdr xss)))))]))

;; break an interval [a,z] into segments each n long, except the last
;; which may be less than n.
;; you may specify an increment to space the intervals apart, e.g.
;; (break-interval/chunks-of 2 0 10) => '((0 . 2) (2 . 4)) but
;; (break-interval/chunks-of 2 0 10 1) => '((0 . 2) (3 . 5)).
;; negative increments are allowed.
;; (: break-interval/chunks-of (->* (Nonnegative-Integer Real Real) (Real) (Listof (Pairof Real Real))))
(define (break-interval/chunks-of n a z [inc 0])
  (let loop ([k a]) (if (<= k z) (cons (cons k (min z (+ k n))) (loop (+ k n inc))) null)))

#| break an interval into no more than n chunks. note the following edge case examples:
   (break-interval/n-chunks 8 1 50 1)
     => '((1 . 7) (8 . 14) (15 . 21) (22 . 28) (29 . 35) (36 . 42) (43 . 49) (50 . 50))
   or (break-interval/n-chunks 8 1 50 1)
     => '((1 . 7) (8 . 14) (15 . 21) (22 . 28) (29 . 35) (36 . 42) (43 . 49))
|#
;; (: break-interval/n-chunks (->* (Nonnegative-Integer Real Real) (Real) (Listof (Pairof Real Real))))
(define (break-interval/n-chunks n a z [inc 0]) (break-interval/chunks-of (exact-floor (abs (/ (- z a) n))) a z inc))

;; e.g. (sequence->list (sequence-map sequence->list (in-inits (in-range 5))))
;; => '((0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4))
;; (: in-inits (∀ (a) (-> (Sequenceof a) (Sequenceof (Sequenceof a)))))
#;(define (in-inits s)
  (fold/sequence ((Sequenceof a)) ([left : (Sequenceof a) empty-sequence]) ([(x) s])
                 (let ([y (sequence-append left (in-value x))]) (values y y))))

;; monomorphic in-dict
(define (in-alist xs)
  (make-do-sequence
   (λ () (values (λ (xs) (let ([p (car xs)]) (values (car p) (cdr p))))
                 cdr xs (compose1 not null?) #f #f))))

;; (: in-avec (∀ (a b) (-> (AVec a b) (Sequenceof a b))))
(define (in-avec v)
  (let ([vlen (vector-length v)])
    (make-do-sequence
     (λ () (values (λ (pos) (let ([p (vector-ref v pos)]) (values (car p) (cdr p))))
                   add1 0 (λ (pos) (< pos vlen)) #f #f)))))

;;; string transforms, formatting, and searching

;; format a real as a dollar amount (adds dollar sign, commas, and rounds to 2 decimals)
;; (: $ (-> Real String))
(define ($ n)
  (match (real->decimal-string n 2)
    [(regexp #rx"([0-9]*)[.]([0-9]+)" (list _
                                            (app (λ (s) (string->list s)) non-decimal)
                                            decimals))
     (string-append "$" (list->string (let loop ([c 1]
                                                 [acc null]
                                                 [rst (reverse non-decimal)])
                                        (if (null? rst)
                                            (if (char=? #\, (car acc)) (cdr acc) acc)
                                            (loop (add1 c)
                                                  (if (= 0 (modulo c 3))
                                                      (list* #\, (car rst) acc)
                                                      (cons (car rst) acc))
                                                  (cdr rst)))))
                    "." decimals)]))

;; split an input stream into lines each of which is no more than a given length
;; (: split-at-every (-> Natural Input-Port (Listof String)))
(define (split-at-every i p)
  (let loop ()
    (let ([x (read-string i p)])
      (if (eof-object? x)
          null
          (cons x (loop))))))

;; split a string into lines each of which is no more than a given length,
;; and split at word boundaries.
;; (: split-string-at-every/word-boundary (-> Natural String (Listof String)))
(define (split-string-at-every/word-boundary l s)
  (let ([ws (string-split s)])
    (let loop ([cur-line-str (car ws)]
               [ws (cdr ws)]
               [acc null])
      (if (null? ws)
          (reverse (cons cur-line-str acc))
          (let ([w (car ws)])
            (if (>= (+ (string-length w) (string-length cur-line-str)) l)
                (loop w (cdr ws) (cons cur-line-str acc))
                (loop (string-append cur-line-str " " w) (cdr ws) acc)))))))

;; if pred is a predicate, then that function is used; if instead pred is a character, equality with that character is used
;; you could use regexp-match or (list->string (sequence->list stop-before str pred)), but this is probably faster
;; (: take-until/string (-> (U Char (-> Char Any)) String String))
(define (take-until/string pred str)
  (let ([stop? #f]
        [pred (if (char? pred) (curry char=? pred) pred)])
    (string-fold (λ (c s)
                   (cond [stop? s]
                         [(pred c) (set! stop? #t) s]
                         [else (string-append s (string c))]))
                 "" str)))

;; also truncates decimals to 2 places, for particular convenience for this stock-portions
;; program, since doing so happens to be appropriate since we're using thousand-comma only for dollar figures
;; (: thousand-comma (-> (U String Real) String))
(define (thousand-comma n)
  (let* ([strs (string-split (if (string? n) n (number->string n)) ".")]
         [non-decimal (car strs)]
         [decimals (if (= 2 (length strs))
                       (let ([s1 (cadr strs)])
                         (string-append "." (substring s1 0 (min (string-length s1) 2))))
                       "")])
    (string-append
     (list->string (let ([c -1])
                     (foldr (λ (a b)
                              (set! c (+ 1 c))
                              (if (and (not (= c 0)) (= 0 (modulo c 3)))
                                  (cons a (cons #\, b))
                                  (cons a b)))
                            null
                            (string->list non-decimal))))
     decimals)))

;; scraps trailing ".0" if present
;; (: trim-useless-point-0 (-> String String))
(define (trim-useless-point-0 s) (if (string-suffix? s ".0") (substring s 0 (- (string-length s) 2)) s))

;; (trunc-or-pad i s) truncates s to i if i is less than or equal to s' length.
;; if i is greater than s' length, then s is padded with the given character.
;; (: trunc-or-pad (->* (Natural String) (Char) String))
(define (trunc-or-pad i s [c #\space])
  (let ([sl (string-length s)])
    (if (> i sl)
        (string-append s (make-string (- i sl) c))
        (substring s 0 i))))

;; O(mn) for an m × n table. assumes that all rows are equal length
;; (: longest-cells-per-column (∀ (a) (-> (-> a Natural) (Listof (Listof a)) (Listof Natural))))
(define (longest-cells-per-column len-fn rows)
  (for/foldr ([maxes (make-list (length (car rows)) 0)]) ([r rows])
             (map max maxes (map len-fn r))))

;; calculating column widths is a fairly expensive operation; you can specify a list of column widths to reduce amount of computation.
;; if you specify a column size list, then strings longer than their column's width are truncated
;; specifying #f for column width will auto-calculate it
;; setting #:col-widths to #f makes function much more efficient than than passing #:col-widths (make-list num-cols #f)!
;; remember that you can (let ([ws (longest-cells-per-column tbl)]) (pad-table-cells tbl #:col-widths ws))
#;(: pad-table-cells (->* ((NonEmpty (Listof String)))
                        (Char #:col-widths (Option (Listof Natural)))
                        (Listof (Listof String))))
(define (pad-table-cells rows [pad-char #\space] #:col-widths [col-widths #f])
  (map (λ (r)
         (if col-widths
             ;; not using for/fold b/c that expands into a left fold
             (foldr (λ (cell col-width acc) (cons (trunc-or-pad col-width cell) acc)) null r col-widths)
             (foldr (λ (cell col-width acc)
                      ;; auto-derived col sizes never truncate, so we're exclusively padding here
                      (cons (string-append cell (make-string (- col-width (string-length cell)) pad-char)) acc))
                    null
                    r
                    (longest-cells-per-column string-length rows))))
       rows))

;; NOTE: will fail with error if not all rows have equal length
#;(: pretty-table-str (-> (NonEmpty (Listof String))
                       [#:col-widths (Option (Listof Natural))]
                       [#:padding Natural]
                       [#:padding-char Char]
                       String))
(define (pretty-table-str rows
                          #:col-widths [col-widths #f]
                          #:padding [padding 2]
                          #:padding-char [pc #\space])
  (string-join (map (λ (row-cells) ; turn a list of cells into a printable line
                      (string-join row-cells (make-string padding pc)))
                    (pad-table-cells rows #:col-widths col-widths)) ; list of rows
               "\n"))

;; (: display-hash-tables (-> (NonEmpty HashTableTop) Void))
(define (display-hash-tables hs) (displayln (pretty-table-str (hash-tables->table hs))))

;; (: hash-tables->table (-> (NonEmpty HashTableTop) (NonEmpty (Listof String))))
(define (hash-tables->table hs)
  ;; this double-sorting can be only one by using an alist
  (let ([fs (sort (hash-keys (car hs)) string<? #:key ~a)])
    (cons (sort (map ~a fs) string<?)
          (map (λ (h) (for/list ([f fs]) (~a (hash-ref h f)))) hs))))

;; breaks s by lines, then returns list of lines that match the given regex
;; TODO: make return list of exprs from regex parens
;; (: grep (->* ((U String Regexp)) ((U Input-Port String) #:first-only? Boolean) (Listof String)))
#;(define (grep regex [s (current-input-port)] #:first-only? [first-only? #f])
  (let ([in (if (string? s) (open-input-string s) s)])
    (let loop ()
      (let ([l (read-line in)])
        (cond [(eof-object? l) null]
              [(regexp-match? regex l) (if first-only? (list l) (cons l (loop)))]
              [else (loop)])))))

#;(: grep-process (-> (U String Regexp) String [#:first-only? Boolean] (Listof String)))
#;(define (grep-process regex process-str #:first-only? [fo? #f])
  (let* ([ps (process process-str)]
         [inp (car ps)]
         ;; unused ports, but bound b/c we need to close them:
         [outp (cadr ps)]
         [errp (cadddr ps)])
    (begin0 (grep regex inp #:first-only? fo?)
      (close-input-port inp)
      (close-input-port errp)
      (close-output-port outp))))

;; function seems to work, though racket says that it can't find either the tempfile (source) or original file (dest) -- assumedly only one of the two
#;(: replace-matching-lines-in-file (-> (-> String Boolean) (-> String (Option String)) Path-String [#:first-only? Boolean] Void))
#;(define (replace-matching-lines-in-file pred endo path #:first-only? [fo? #f])
  (with-input-from-file
   path
   (λ () (let ([tmpfp (make-temporary-file "rkttmp~a" #f (current-directory))]) ;; can't create in /var/tmp in nixos, i suppose.
           (with-output-to-file
            tmpfp
            (λ ()
              (let → : Void ()
                (let ([l (read-line)])
                  (cond [(eof-object? l) (void)]
                        [(pred l) (or (and-let* ([lp (endo l)])
                                                (displayln lp))
                                      (void))
                                  (if fo? (copy-port (current-input-port) (current-output-port)) (→))]
                        [else (displayln l) (→)])
                  (rename-file-or-directory tmpfp path #t) ;; has sometime failed: "system error: Invalid cross-device link; errno=18", despite mv(1) working
                  ;; alternative if it doesn't work: (copy-file tmpfp path #t) (delete-file tmpfp)
                  )))
            #:exists 'truncate #:mode 'text)))))

;;; lists, vectors, and combinatronics

#| lookup an element (first occurence only) in a vector, and, if found, replace it by an updated version.
   works only on mutable vectors
   example:
(define v : (Vectorof Integer) (vector 1 0 2))
(define (update-v) (vector-update! (λ ([k : Integer]) (and (even? k) (add1 k))) v))
(update-v) ; v is now #(1 1 2)
(update-v) ; v is now #(1 1 3)
(update-v) ; v is still #(1 1 3)
|#
;; (: vector-update! (∀ (a) (-> (-> a (Option a)) (Mutable-Vectorof a) Void)))
(define (vector-update! f v)
  (let ([len (vector-length v)])
    (let loop ([pos 0])
      (when (< pos len)
        (cond-let [(f (vector-ref v pos)) => new-val (vector-set! v pos new-val)]
                  [else (loop (add1 pos))])))))

;; (: vector-setf*! (∀ (a) (-> (Mutable-Vectorof a) (-> a a) * Void)))
(define (vector-setf*! v . fs)
  (let loop ([pos 0] [fs fs])
    (if (null? fs)
        (void)
        (begin (vector-set! v pos ((car fs) (vector-ref v pos)))
               (loop (add1 pos) (cdr fs))))))

;; index of first element matching a predicate
;; (: vector-findf/pos (∀ (a) (-> (-> a Boolean) (Vectorof a) (Option Integer))))
(define (vector-findf/pos p v)
  (let ([len (vector-length v)])
    (let loop ([pos 0])
      (and (< pos len) (let ([curval (vector-ref v pos)]) (if (p curval) pos (loop (add1 pos))))))))

(define (vector-last v) (vector-ref v (sub1 (vector-length v))))
(define (vector-first v) (vector-ref v 0))

;; TODO: use arrays if no faster. check.
;; (: in-vector-from-right (∀ (a) (-> (Vectorof a) (Sequenceof a))))
(define (in-vector-from-right v) (in-interval/reverse v vector-ref -1 (sub1 (vector-length v))))

#| vector-copy that supports negative indices à la python, except that
   if the start is less than the end then elements are reversed, e.g.
   in python range(10)[-1,4] is empty, but here it's #(8 7 6 5 4), unless
   #:python-compat? #t (it's otherwise equal to python slices already.)
   (vector-slice v -1 0) returns v reversed, but missing its last element, e.g.
   (vector-slice (list->vector (range 10)) -1 0) => #(8 7 6 5 4 3 2 1 0).

   TIP: the slice [-n:] gets the last n elements.
   NOTE: vector-copy or vector-ref throws error if invalid indices are provided, including predicates that no elements match.
   example of #:include-right-match?:
   (vector-slice #(1 2 2.5 5 6) 0 (λ (x) (= x 5)) #:include-right-match? #t)
   returns #(1 2 2.5 5) whereas without the flag it'd not include 5.
|#
#;(: vector-slice (∀ (a) (->* ((Vectorof a)) ((U (-> a Boolean) Integer)
                                            (U (-> a Boolean) Integer)
                                            #:python-compat? Boolean
                                            #:include-right-match? Boolean) ; used only if end is predicate
                            (Vectorof a))))
(define (vector-slice v [a 0] [z (vector-length v)] #:python-compat? [py? #f] #:include-right-match? [wrm? #f])
  (let ([ma (if (exact-integer? a)
                (if (negative? a) (+ (vector-length v) a) a)
                (or (vector-findf/pos a v)
                    (error "vector-slice: invalid slice start predicate")))]
        [mz (if (exact-integer? z)
                (if (negative? z) (+ (vector-length v) z) z)
                (+ (if wrm? 1 0)
                   (or (vector-findf/pos z v)
                       (error "vector-slice: invalid slice end predicate"))))])
    (if (> ma mz)
        (if py? #() (build-vector (- ma mz) (λ (i) (vector-ref v (- ma i 1)))))
        (vector-copy v ma mz))))

;; where interval is inclusive on both sides
;; (: in-interval/reverse (∀ (s a) (-> s (-> s Integer a) Integer Integer (Sequenceof a))))
(define (in-interval/reverse s f a z) (make-do-sequence (λ () (values (curry f s) sub1 z (curry < a) #f #f))))

;; more efficient version of (in-vector (vector-slice v a b))
;; (: in-vector-on (∀ (a) (->* ((Vectorof a)) (Integer Integer #:python-compat? Boolean) (Sequenceof a))))
(define (in-vector-on v [a 0] [z (vector-length v)] #:python-compat? [py? #f])
  (let ([ma (if (negative? a) (+ (vector-length v) a) a)] [mz (if (negative? z) (+ (vector-length v) z) z)])
    (if (> ma mz)
        (if py? empty-sequence (in-interval/reverse v vector-ref (sub1 mz) (sub1 ma)))
        (in-interval v vector-ref ma mz))))

;; needs to be its own function because racket doesn't support monoids Monoid m n => (m,n), nor monoids Last & (+1)
;; (: len&last (∀ (a) (-> (Listof a) (Values Positive-Integer a))))
(define (len&last xs)
  (if (null? xs)
      (raise-argument-error 'len&last "non-empty input list" xs)
      (let loop ([l 1] [rst xs])
        (if (null? (cdr rst))
            (values l (car rst))
            (loop (add1 l) (cdr rst))))))

;; (cons-if-truthy c ... xs) conses truthy elements in c ... onto xs
;; e.g. (cons-if-truthy null #f 3 #f 4) => '(3 4)
;; if you want to cons based on a general predicate, you may try
;; (append (filter p xs) xss ...), but you may need for p to be a pred,
;; i.e. (-> Any Boolean : t) for some type t.
;; (: cons-if-truthy (∀ (a) (-> (Listof a) (Option a) * (Listof a))))
(define (cons-if-truthy z . xs) (foldr (λ (x acc) (if x (cons x acc) acc)) z xs))

;; (filter-truthy xs) is (apply (curry cons-if-truthy null) xs) but faster
;; (: filter-truthy (∀ (a) (-> (Listof (Option a)) (Listof a))))
(define (filter-truthy xs) (foldr (λ (x acc) (if x (cons x acc) acc)) null xs))

;; (: filter-map2 (∀ (a b) (-> (-> a (Option b)) (Listof a) (Listof b))))
(define (filter-map2 f xs) (foldr (λ (x acc) (let ([y (f x)]) (if y (cons y acc) acc))) null xs))

;; (: map-list (∀ (a) (-> (Listof a) (Listof (List a)))))
;; (define (map-list as) (map list as))

;; produces a list whose first element is a list and whose remaining
;; elements are not necessarily lists. thus you can still break the
;; return value of unzip into two lists via car & cdr, but that doesn't
;; curry correctly. use unzip/list for currying.
;; (: unzip (∀ (a b) (-> (AList a b) (Pairof (Listof a) (Listof b)))))
(define (unzip ps) (let-values ([(x y) (unzip/values ps)]) (cons x y)))

;; (: unzip/values (∀ (a b) (-> (AList a b) (Values (Listof a) (Listof b)))))
(define (unzip/values ps) (for/foldr ([as null] [bs null]) ([p ps]) (values (cons (car p) as) (cons (cdr p) bs))))

;; useful for currying, e.g:
;; (define (f [xs : (Listof Real)] [ys : (Listof Real)]) (/ (apply + xs) (apply + ys)))
;; (apply f (unzip/list '((1 . 4) (2 . 5) (3 . 6)))) ==> 2/5
;; (: unzip/list (∀ (a b) (-> (AList a b) (List (Listof a) (Listof b)))))
(define (unzip/list ps) (let ([x (unzip ps)]) (list (car x) (cdr x))))

;; pass even? or odd? as the predicate
;; (: every-other (∀ (a) (-> (-> Integer Boolean) (Listof a) (Listof a))))
(define (every-other p xs) (for/list ([x xs] [k (in-naturals)] #:when (p k)) x))

;; given xs & ys (of equal length), produce (list x₀ y₀ x₁ y₁ ...)
;; to interleave lists of varying lengths while ensuring an ordering,
;; just append then sort them.
;; (: interleave (∀ (a) (-> (Listof a) (Listof a) (Listof a))))
(define (interleave xs ys) (foldr (λ (x y acc) (cons x (cons y acc))) null xs ys))

#| split at m if list has at least m elements; else the 1st of values is the input list, and the 2nd is null.
   e.g. (safe-split-at (range 7) 5) => (values '(0 1 2 3 4) '(5 6))
        (safe-split-at (range 7) 7) => (values '(0 1 2 3 4 5 6) '())
   unlike split-at, accepts any real, including +inf.0, as the length.
|#
;; (: safe-split-at (∀ (a) (-> (Listof a) Real (Values (Listof a) (Listof a)))))
(define (safe-split-at xs0 m)
  (let loop
       ([count 0] [acc null] [xs xs0])
       (if (or (null? xs) (>= count m))
           (values null xs)
           (let-values ([(sp rst) (loop (add1 count) (cons (car xs) acc) (cdr xs))])
             (values (cons (car xs) sp) rst)))))

#| use n ordered predicates to split a list into sublists by those predicates in order
   e.g. (splitf-at/multiple (range 10) (curry = 4) (curry = 7))
   => '((0 1 2 3) (4 5 6) (7 8 9))
   NOTE: * unlike splitf-at, predicates break, not take!
         * former predicates must be filled first!
              (splitf-at/multiple (range 10) (curry = -1) (const #t))
           => '((0 1 2 3 4 5 6 7 8 9))
         * none of the output list's elements is null. unfortunetaly the type
           checking is not sophisticated enough to reflect this.
         * if the 1st predicate matches the 1st element, then no splitting occurs,
           e.g. (splitf-at/multiple '(1 2 3 4) (curry = 1))
             => '((1 2 3 4))
           e.g. (splitf-at/multiple '(1 2 3 4 5 6) (curry = 1) (curry = 4))
             => '((1 2 3) (4 5 6))
         * (splitf-at/multiple '() ps ...) => '()
         * you can do some clever parsing using clever predicates:
              (splitf-at/multiple '(1 2 3 4 5 6) (curry = 3) (const #t))
           => '((1 2) (3) (4 5 6))
         * #:double? #t makes the prior example return '((1 2 3) (3 4) (4 5 6))
|#
;; (: splitf-at/multiple (∀ (a) (-> (Listof a) [#:double? Boolean] (-> a Any) * (Listof (Listof a)))))
(define (splitf-at/multiple #:double? [double? #f] xs . preds)
  (let loop ([preds preds] [acc-elem null] [rst xs])
    (cond
      ;;; base cases. note that some code prevents adding empty lists to the returned value
      ;; and has no analogue in splitf-at/multiple/slices.
      [(and (null? preds) (null? rst)) (or-null acc-elem (list (reverse acc-elem)))]
      [(null? preds) (list (if (null? acc-elem) rst (append (reverse acc-elem) rst)))]
      [(null? rst) (or-null acc-elem (list (reverse acc-elem)))]
      ;;; recursive cases
      [((car preds) (car rst)) ; next predicate matches; add acc-elem to ret val then reset acc-elem.
       (if (null? acc-elem)
           (loop (cdr preds) (list (car rst)) (cdr rst))
           (cons (reverse (if double? (cons (car rst) acc-elem) acc-elem))
              (loop (cdr preds) (list (car rst)) (cdr rst))))]
      ;; add current element into acc-elem
      [else (loop preds (cons (car rst) acc-elem) (cdr rst))])))

;; like splitf-at/multiple but returns slices
;; like how splitf-at/multiple returns a list of the original list if given no predicates,
;; splitf-at/multiple/slices returns a slice [0:len]
;; (: splitf-at/multiple/slices (∀ (a) (-> (Vectorof a) [#:double? Boolean] (-> a Any) * (AList Integer Integer))))
(define (splitf-at/multiple/slices #:double? [double? #f] v . preds)
  (let ([len (vector-length v)])
    ;; whereas splitf-at/multiple's acc-elem starts at null then begins accumulating elements starting
    ;; from when the predicate matched & ending with the last element not matching (car preds),
    ;; this acc-elem starts at #f, then once a predicate matches, becomes start-pos, and when (car preds)
    ;; matches, (cons start-pos pos) is added to the ret val and acc-elem, depending on double?, becomes either
    ;; end-pos or (add1 end-pos).
    (let loop ([preds preds] [last-match-pos 0] [pos 0])
      (cond [(or (null? preds) (>= pos len)) (list (cons last-match-pos len))]
            [((car preds) (vector-ref v pos)) (cons (cons last-match-pos (if double? (add1 pos) pos)) (loop (cdr preds) pos (add1 pos)))]
            [else (loop preds last-match-pos (add1 pos))]))))

;; if predicate matches first element of input list, then returned
;; list's first element is null. this is a feature, not a bug.
;; (break-on/all odd? (range 6)) --> '((0) (1 2) (3 4) (5))
;; (: break-on/all (∀ (a) (-> (-> a Boolean) (Listof a) (Listof (Listof a)))))
(define (break-on/all p xs)
  (let loop ([acc null] [rst xs])
    (cond [(null? rst) (list (reverse acc))]
          [(p (car rst)) (cons (reverse acc) (loop (list (car rst)) (cdr rst)))]
          [else (loop (cons (car rst) acc) (cdr rst))])))

;; like in-slice (in racket/sequence) but supports padding the last slice.
;; if pad, then the last row will be padded on the right so that it's the given length
;; (: chunks-of (∀ (t) (-> Positive-Integer (Listof t) [#:pad (Option t)] (NonEmpty (Listof t)))))
(define (chunks-of i s #:pad [pad #f])
  (let loop ([s s])
    (if (< (length s) i)
        (list (if pad
                  (append s (make-list (- i (length s)) pad))
                  s))
        (let-values ([(a b) (split-at s i)])
          (if (null? b)
              (list a)
              (cons a (loop b)))))))

;; (split-at-indices (set 4 5 10) (range 11)) --> '((0 1 2 3) (4) (5 6 7 8 9) (10))
;; it's not an error to have an index out of range; these are ignored.
;; if you want to split on any of a set of values rather than on indices, then
;; do (break-on/all (curry set-member? _) _)
;; (: split-at-indices (∀ (a) (-> (Setof Index) (Listof a) (Listof (Listof a)))))
(define (split-at-indices is xs)
  (let loop ([acc null] [rst xs] [k 0])
    (cond [(null? rst) (list (reverse acc))]
          [(set-member? is k) (cons (reverse acc) (loop (list (car rst)) (cdr rst) (add1 k)))]
          [else (loop (cons (car rst) acc) (cdr rst) (add1 k))])))

;; (cmp-len n) runs in O(n) rather than O(length of list). good for long lists.
;; (: cmp-len (-> Natural (Listof Any) (U 'lt 'gt 'eq)))
(define (cmp-len n xs)
  (let loop ([k 0] [rst xs])
    (cond [(= k n) (if (null? rst) 'eq 'gt)]
          [(null? rst) 'lt]
          [else (loop (add1 k) (cdr rst))])))

(define (length<? n xs) (symbol=? 'lt (cmp-len n xs)))
(define (length>=? n xs) (not (length<? n xs)))
(define (length=? n xs) (symbol=? 'eq (cmp-len n xs)))

;; courtesy of https://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/
;; TODO: cf https://en.wikipedia.org/wiki/Patience_sorting
;; O(log(n))
;; < for increasing, > for decreasing
;; (: longest-monotonic-subsequence-length (∀ (a) (-> (Listof a) (-> a a Boolean) Natural)))
(define (longest-monotonic-subsequence-length xs cmp)
  (if (null? xs)
      0
      (let loop ([t (hash 0 (car xs))] [len 1] [k 1] [xs (cdr xs)])
           (if (null? xs)
               (begin (print-vars k) len)
               (let*-values ([(x) (car xs)]
                             [(t len) (cond [(cmp x (hash-ref t 0)) (values (hash-set t 0 x) len)]
                                            [(not (cmp x (hash-ref t (sub1 len)))) (values (hash-set t len x) (add1 len))]
                                            [else (values (hash-set t (let loop ([l -1] [r (sub1 len)])
                                                                           (if (> (- r l) 1)
                                                                               (let ([m (+ l (quotient (- r l) 2))])
                                                                                 (if (cmp (hash-ref t m) x)
                                                                                     (loop m r)
                                                                                     (loop l m)))
                                                                               r))
                                                                    x)
                                                          len)])])
                 (loop t len (add1 k) (cdr xs)))))))

;; courtesy of wikipedia https://en.wikipedia.org/wiki/Longest_increasing_subsequence
;; (: __lms (∀ (a) (-> (Vectorof a) (-> a a Boolean) (Values (HashTable Integer Integer) (HashTable Integer Integer) Integer))))
(define (__lms xs cmp)
  (let ([n (vector-length xs)])
    (let loop ([p (hash)]
               [m (hash 0 0)]
               [l 0] [i 0])
      (if (< i n)
          (let ([new-l (let bins ([lo 1] [hi l])
                            (if (<= lo hi)
                                (let ([mid (ceiling (/ (+ lo hi) 2))])
                                  (if (cmp (vector-ref xs (hash-ref m mid)) (vector-ref xs i))
                                      (bins (add1 mid) hi)
                                      (bins lo (sub1 hi))))
                                lo))])
            (loop (hash-set p i (hash-ref m (sub1 new-l)))
                  (hash-set m new-l i)
                  (if (> new-l l) new-l l)
                  (add1 i)))
          (values p m l)))))

;; < for increasing, > for decreasing
#;(: longest-monotonic-subsequence (∀ (a b) (case-> (-> (Vectorof a) (-> a a Boolean) (Listof a))
                                                  (-> (Vectorof a) (-> a a Boolean) (-> a b) (Listof b)))))
(define longest-monotonic-subsequence
  (case-lambda [(xs cmp) (let*-values ([(p m l) (__lms xs cmp)]
                                       [(s _) (for/fold ([s null] [k (hash-ref m l)])
                                                        ([_ (in-range l 0 -1)])
                                                (values (cons (vector-ref xs k) s) (hash-ref p k)))])
                           s)]
               [(xs cmp f) (let*-values ([(p m l) (__lms xs cmp)]
                                         [(s _) (for/fold ([s null] [k (hash-ref m l)])
                                                          ([_ (in-range l 0 -1)])
                                                  (values (cons (f (vector-ref xs k)) s) (hash-ref p k)))])
                             s)]))

;; not even sure how to describe this one except to say that it's used in calculating
;; convex hulls.
;; (convex-subsequence > '(95.48 95.27 95.33 95.24 95.13 95.05 95 94.92 95.12))
;; => '(95.48 95.33 95.24 95.13 95.12) whereas longest-monotonic-subsequence returns
;;    '(95.48 95.27 95.24 95.13 95.05 95 94.92).
;; returned vector is mutable.
;; < for increasing, > for decreasing
;; (: convex-subsequence (∀ (a) (-> (Sequenceof a) (-> a a Boolean) (Listof a))))
(define (convex-subsequence xs cmp)
  (longest-monotonic-subsequence ; longest monotonic subsequence of indices
   (let ([v (for/vector ([i (in-naturals)] [x xs]) (cons i x))])
     (vector-sort! v (on cmp cdr))
     v)
   (on < car)
   cdr))

;; NEXT: refactor on-interval to accept & return sequences.
;; consider takef & dropf wrt predicates vs ordered positions; intervals delimited by ordered elements are
;; a specific version of "start at predicate then take until predicate."
;; this would generalize slices from vectors to sequences, and from integral positions to predicates
;; (this is currently splitf-at/multiple[/slices]) (with refinement for faster operations when appropriate.)

;; (define-type (Interval t) (Pairof (U 'lt 'gt t) t))

#| for `(lb . ub) and `(lt . ,ub) if < is <= then bounds are included; else they'ren't.
   for `(gt . ,lb) if < is <= then lb is not includod; else it is.
e.g. (on-interval X identity Y (range 7))
X = <, Y = '(lt . 4): '(0 1 2 3)
X = <, Y = '(gt . 4): '(4 5 6)
X = <, Y = '(2 . 4): '(3)
X = <=, Y = '(lt . 4): '(0 1 2 3 4)
X = <=, Y = '(gt . 4): '(5 6)
X = <=, Y = '(2 . 4): '(2 3 4)
|#
;; (: on-interval (∀ (a b) (-> (-> a b) (-> b b Boolean) (Interval b) (Listof a) (Listof a))))
(define (on-interval ->b < i as)
  (let ([cari (car i)]
        [cdri (cdr i)])
    (if (symbol? cari)
        (case cari
          [(lt) (takef as (λ (a) (< (->b a) cdri)))]
          [(gt) (dropf as (λ (a) (< (->b a) cdri)))]
          [else (error "impossible")])
        (_on-interval ->b < cari cdri as))))

;; (: _on-interval (∀ (a b) (-> (-> a b) (-> b b Boolean) b b (Listof a) (Listof a))))
(define (_on-interval ->b < lb ub as)
  (foldr (λ (a acc)
           (let ([b (->b a)])
             (if (and (< lb b) (< b ub))
                 (cons a acc)
                 acc)))
         null as))

#| e.g. (with-seq-lists (ns ss ns2)
          (in-parallel (in-range 4) '(cat bat hat mat) (in-range 4 8))
            (printf "~a / ~a / ~a~n" ns ss ns2))
prints (0 1 2 3) / (cat bat hat skat) / (4 5 6 7) |#
(define-syntax (with-seq-lists stx)
  (syntax-parse stx
                [(_ (acc:id ...) seq . body)
                 (with-syntax ([(loop-var ...) (generate-temporaries #'(acc ...))])
                   #'(let-values ([(acc ...) (for/foldr ([acc null] ...) ([(loop-var ...) seq])
                                               (values (cons loop-var acc) ...))]) . body))]))

;;; folds

;; where's applicative Fold when ya need it, right?
;; this function is interesting b/c it calculates both min & max given only <=
;; (: min&max (∀ (a) (-> (-> a a Boolean) (Sequenceof a) (Values a a))))
(define (min&max lt xs)
  (let-values ([(1st more? next) (sequence-split-at 1 xs)])
    (if (null? 1st)
        (raise-argument-error 'min&max "non-empty sequence" xs)
        (let loop ([smol (car 1st)] [big (car 1st)])
          (if (more?)
              (let ([x (next)]) (loop (if (lt smol x) smol x) (if (lt x big) big x)))
              (values smol big))))))

;; whereas map essentially zips many lists together, then applies a function to each element,
;; multimap applies n functions to one list, returning n lists, and iterates only once.
;; TODO
#;(: multimap (∀ (a c ...) (case-> (-> (-> a c) ... (NonEmpty a) (Values (NonEmpty c) ...))
                                   (-> (-> a c) ... (Listof a) (Values (Listof c) ...)))))
;; (define (multimap xs . fs) (let loop : _ ([rst xs]) (or-null xs (map (λ ([f : _]) (f (car xs)))fs))))

;; (: map-but-last (∀ (a c) (-> (-> a c) (-> a c) (Listof a) (Listof c))))
(define (map-but-last f f/final xs)
  (or-null xs
           (let loop ([rst xs])
             (if (null? (cdr rst))
                 (list (f/final (car rst)))
                 (cons (f (car rst)) (loop (cdr rst)))))))

;; omit the last element. identity function if given the null list.
;; (: init (∀ (a) (-> (Listof a) (Listof a))))
(define (init xs) (or-null xs (let loop ([rst xs]) (or-null (cdr rst) (cons (car rst) (loop (cdr rst)))))))

;;; hash tables

#| insert key/value pairs, if their associated conditions are truthy
   the same key may be used multiple times; the last truthy one will be
   put into the map, a la hash-set. for example, in the table returned by
   (hash-set/cond (hash 'a 20 'b "yes")
                  #t 'b "canary"
                  #t 'b 452)
   , b is 452.

  CAVEAT:
  the condition, key, and value are all treated separately.
  (let ([x #f]) (hash-set/cond (hash) x 'x (or x (error "impossible"))))
  will raise the error because, even though hash-set/cond guarantees
  that x will not be added unless it's truthy, the value expression is
  evaluated before being passed to hash-set/cond! unfortunately it appears
  impossible to support (U (-> v) v) instead of v in #:rest-star (Any k v),
  so if you need thunks or promises, you'll need to use hash-set/cond with types k (-> v)
  and then evaluate when you retrieve values or iterate over the table.
|#
;; (: hash-set/cond (∀ (k v) (->* ((HashTable k v)) #:rest-star (Any k v) (HashTable k v))))
(define (hash-set/cond ht . args)
  (let loop ([ht ht] [args args])
    (if (null? args)
        ht
        (loop (if (car args)
                  (hash-set ht (cadr args) (caddr args))
                  ht)
              (cdddr args)))))

;;; numerics

(define (%chg a b) (/ (* 100 (- b a)) a))
(define (%chg-str a b) (let ([chg (%chg a b)]) (format (if (positive? chg) "+~a%" "~a%") (real->decimal-string chg 2))))

;; round a number to a given number of digits, e.g.
;; (round/num-digits 2 45.47253) --> 45.47
;; (round/num-digits 1 45.47253) --> 45.5
;; NOTE: a 5 in the last place will sometimes round down, e.g.
;; (round 0.5) → 0, (round 0.51) → 1, and (round 1.5) → 2. because
;; round/num-digits is defined in terms of round, this can affect it.
;; (: round/num-digits (-> Natural Float Float))
(define (round/num-digits num-digits x) (let ([d (expt 10 num-digits)]) (/ (round (* x d)) d)))

;; like round/num-digits but always rounds down [truncates]
;; (: truncate/num-digits (-> Natural Float Float))
(define (truncate/num-digits num-digits x) (let ([d (expt 10 num-digits)]) (/ (truncate (* x d)) d)))

;; nearest higher increment, and the distance to it:
;; (unit-ceiling&distance 5 3) --> 5 2
;; (unit-ceiling&distance 7 18) --> 21 3
;; (: unit-ceiling&distance (-> Integer Integer (Values Integer Integer)))
(define (unit-ceiling&distance u n)
  (let ([d (- u (modulo n u))])
    (values (+ n d) d)))

;;; miscellaneous specific functions

#| (for-each displayln
             (n-choose-2 (λ (x1 y1 x2 y2)
                           (format "d[(~a,~a),(~a,~a)] = ~a" x1 y1 x2 x2 (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
                         '(1 2 3)
                         '(3 4 5)))
   => d[(1,3),(2,2)] = 1.4142135623730951
      d[(1,3),(3,3)] = 2.8284271247461903
      d[(2,4),(3,3)] = 1.4142135623730951

* expects, but does not check, that all lists have equal length.
|#
(define (n-choose-2 f . xss)
  (let loop ([xss xss])
    (let ([xs (map car xss)]
          [yss (map cdr xss)])
      ;; if any list is null then they should all be null. may as well check the first.
      ;; b/c we use map below, all lists must have same length anyway.
      (if (null? (car yss))
          '()
          (foldr cons
                 (loop yss)
                 (apply map (λ ys (apply f (append xs ys))) yss))))))


;; checks whether any elements occur at least n times in a sequence.
;; uses a hash table for internal counting, so be careful when using
;; objects (cf primitives)!
;; (: n+occurrences? (∀ (a) (-> Natural (Listof a) Boolean)))
(define (n+occurrences? lim xs)
  (let loop ([cnts (hash)] [xs xs])
       (and (pair? xs)
          (let* ([x (car xs)] [new-cnts (hash-update cnts x add1 (const 0))])
            (or (>= (hash-ref new-cnts x (const 0)) lim)
               (loop new-cnts (cdr xs)))))))

;; should return #f if item is inconsiderable
;; (define-type (RatingFn a) (-> a (Option Real)))

;; determine rating for all in a list, then return the top n, or
;; however many are considerable. may return inconsiderables.
;; best/only-worthy guarantees no inconsiderables, but is less efficient.
;; e.g. (best (λ ([r : Integer]) (and (even? r) r)) '(0 1 5 6 3 7 4 2))
;; => '(6 4 2 0 1 5 3 7). note that the elements after 0 retain their positions
;; relative to each other before being passed to `best`:
;; '(1 5 3 7) = (filter odd? '(0 1 5 6 3 7 4 2)).
;; (: best (∀ (a) (-> (RatingFn a) (Listof a) [#:limit (Option Natural)] [#:cache-ratings? Boolean] (Listof a))))
(define (best r xs #:limit [lim #f] #:cache-ratings? [cr? #f])
  (let ([res (sort xs (λ (x y)
                        ;; sort requires (-> a a Boolean), not (-> a a Any)
                        ;; if #t then swap; if #f then leave as they are
                        ;; the real subset of (Option Real) is greater than the #f subset.
                        ;; the real subset is totally orderde as usual. the #f subset is unordered.
                        (cond [(and x y) (> x y)]
                              [x #t]
                              [else #f])) ; don't bother swapping two elements whose ratings are both #f
              #:cache-keys? cr?
              #:key r)])
    (if lim (or (<- lim res) null) res)))

;; e.g. (best (λ ([r : Integer]) (and (even? r) r)) '(0 1 5 6 3 7 4 2))
;; => '(6 4 2 0)
;; (: best/only-worthy (∀ (a) (-> (RatingFn a) (Listof a) [#:limit (Option Natural)] [#:cache-ratings? Boolean] (Listof a))))
(define (best/only-worthy r xs #:limit [lim #f] #:cache-ratings? [cr? #f])
  (let-values ([(x _) (splitf-at (best r xs #:limit lim #:cache-ratings? cr?) r)]) x))

;; a [probably always] less-effecient version of best/only-worthy
;; best/only-worthy exploits best's ordering. best/only-worthy2 explicitly filters-out
;; #f ratings then sorts by rating. this adds two traversals over the list.
;; written first, and left here in case it's ever needed.
;; (: best/only-worthy2 (∀ (a) (-> (RatingFn a) (Listof a) [#:limit (Option Natural)] [#:cache-ratings? Boolean] (Listof a))))
(define (best/only-worthy2 r xs #:limit [lim #f] #:cache-ratings? [cr? #f])
  (let ([res (map ; 2st additional traversal
              car
              (sort
               (for/fold ([acc null]) ([x xs]) ; 1st additional traversal
                 (let ([r (r x)]) (if r `((,x . ,r) . ,acc) acc)))
               >
               #:cache-keys? cr?
               #:key cdr))])
    (if lim (or (<- lim res) null) res)))

;; given a sequence of things of type s and a hash on s, return a hashmap from
;; the hash to the object whence the hash was computed.
;; (: hash->table (∀ (s id) (-> (-> s id) (Sequenceof s) (HashTable id s))))
(define (hash->table ->id ss) (for/hash ([s ss]) (values (->id s) s)))
