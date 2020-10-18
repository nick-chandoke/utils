#lang typed/racket/base

;;; miscellaneous functions

(provide (all-defined-out))
(require "ring-buffer.rkt"
         (for-syntax racket/base (only-in racket/function curry))
         (for-syntax racket/base)
         (only-in racket/file make-temporary-file)
         (only-in racket/format ~a)
         (only-in racket/function curry)
         (only-in racket/port copy-port port->string)
         (only-in racket/string string-split string-suffix? string-replace non-empty-string? string-prefix?)
         (only-in racket/system process)
         (only-in racket/vector vector-split-at vector-append)
         syntax/parse/define)

(require/typed srfi/13 (string-fold (All (a) (-> (-> Char a a) a String a))))

;;; types

(define-type (AList a b) (Listof (Pairof a b)))

;;; syntax

;; e.g. (reduced-let let* binds body)
;; whatever form of let, but (... () body ...+) becomes (begin body ...+), (... () body) becomes body
;; useful for macros that accept let binding parameters. makes expanded syntax cleaner.
(define-syntax (reduced-let stx)
  (let* ([args (cdr (syntax->datum stx))]
         [form (car args)]
         [args (cdr args)])
    (when (null? (cdr args)) (raise-syntax-error 'reduced-let "you must give a body!"))
    (datum->syntax stx (if (null? (car args))
                           (if (null? (cddr args))
                               (cadr args)
                               `(begin ,@(cdr args)))
                           `(,form ,@args)))))

(define-syntax-rule (until x upd) (do () (x (void)) upd))

;; easily define an identifier macro
(define-syntax-rule (define-syntax:id name expr ...)
  (define-syntax (name stx)
    (syntax-case stx () [name (identifier? #'name) #'(begin expr ...)])))

(define-syntax (thread-λ stx)
  (datum->syntax stx `(thread (λ () ,@(cdr (syntax->datum stx))))))

;; to disambiguate against consing a thing onto a list
(: pair (All (a b) (-> a b (Pairof a b))))
(define pair cons)

(define-syntax-rule (++ x) (set! x (+ 1 x)))

(: ∞ (∀ (a) (-> (-> a) a)))
(define (∞ f) (f) (∞ f))

;; like (hash), but produces an list rather than a hashmap
;; (alist 20 24 'hello 'there (+ 1 2) 'is3) => '((20 . 24) (hello . there) (3 . is3))
(define-syntax (alist stx)
  (let loop
      ;; we can't merely set k to #f on
      ;; every other iteration in order to determine
      ;; which of key or value we're iterating on, b/c
      ;; #f is a valid key!
      ([i 0]
       [k #f] ;; buffer
       [alst null]  ;; accumulator
       [args (cdr (syntax->datum stx))])
    (cond
      [(null? args) (datum->syntax stx `(reverse (list ,@alst)))]
      [(even? i) (loop (add1 i) (car args) alst (cdr args))]
      [else (loop (add1 i) #f (cons `(cons ,k ,(car args)) alst) (cdr args))])))

;; untyped version. useful in define-for-syntax
;; (define (alist@ . args)
;;   (let loop
;;       ([i 0]
;;        [k #f] ;; buffer
;;        [alst null]
;;        [args args]) ;; accumulator
;;     (cond
;;       [(null? args) (reverse alst)]
;;       [(even? i) (loop (add1 i) (car args) alst (cdr args))]
;;       [else (loop (add1 i) #f (cons (cons k (car args)) alst) (cdr args))])))

;;; filesystem

(define (read-file-into-string [path : Path-String]) : String
  (call-with-input-file path port->string))

;; mkdir -p.
(: make-directory-rec (-> Path-String Void))
(define (make-directory-rec p)
  (let* ([separator "/"] ;; TODO: get this through automated means
         [p (if (string? p) p (path->string p))]
         [segments (string-split p "/")]
         [absolute? (string-prefix? p separator)])
    (let ([final-dir ((inst foldl String String)
                      (λ ([segment : String] [path-string : String])
                        (let ([ps (string->path path-string)])
                          (unless (directory-exists? ps) (make-directory ps))
                          (string-append path-string separator segment)))
                      (if absolute? (string-append separator (car segments)) (car segments))
                      (cdr segments))])
      (unless (directory-exists? final-dir)
        (make-directory final-dir)))))

;; format a real as a dollar amount
(define ($ [n : Real]) : String
  (let* ([strs (string-split (number->string n) ".")]
         [non-decimal : (Listof Char) (string->list (car strs))]
         [decimals (if (= 2 (length strs))
                       (let* ([s1 (cadr strs)]
                              [s1l (string-length s1)]
                              [y (min s1l 2)])
                         (string-append "." (substring s1 0 y) (make-string (- 2 y) #\0)))
                       "")])
    (string-append "$"
     (list->string (cdr (foldr (λ ([a : Char] [cb : (Pairof Natural (Listof Char))])
                                 (let ([c (car cb)]
                                       [b (cdr cb)])
                                   (pair
                                    (add1 c)
                                    (cons a (if (= 0 (modulo c 3))
                                                (cons #\, b)
                                                b)))))
                                 (pair (ann 1 Natural) (list (car non-decimal)))
                               (cdr non-decimal))))
     decimals)))

(define (kabob-case->upper_snake [s : Symbol]) : String
  (string-replace (string-upcase (symbol->string s)) "-" "_"))

(define (percent-change [a : Real] [b : Real]) : Real
  (/ (* 100 (- b a)) a))

;;; iterators/sequences

;; reverse? means that each list in the iteration is reversed:
;; (sequence->list (in-inits '(1 2 3) #:reverse? #t)) => ((1) (2 1) (3 2 1))
;; less computation when reverse? is #t
;; TODO: refactor into using closures rather than mutation. update in-inits/vector, too.
(: in-inits (All (a) (-> (Listof a) [#:reverse? Boolean] (Sequenceof (Listof a)))))
(define (in-inits xs #:reverse? [reverse? #f])
  (let ([iter : (Listof a) xs]
        [left : (Listof a) null])
    (in-producer
     (λ ()
       (if (null? iter)
           null
           (let ([leftp (cons (car iter) left)])
             (set! iter (cdr iter))
             (set! left leftp)
             (if reverse? leftp (reverse leftp)))))
     null?)))

;; uses a ring buffer to produce vectors of length len (or less, before the ring buffer is filled)
;; you need to supply a dummy value for a, to fill the ring buffer with that value for its initialization.
(: in-inits/vector (All (a) (-> Natural a (Listof a) (Sequenceof (Vectorof a)))))
(define (in-inits/vector len init-val xs)
  (let ([iter : (Listof a) xs]
        [left : (RingBuffer a) (make-ring-buffer len init-val)]
        [done? : Boolean #f])
    ((inst in-producer (Vectorof a))
     (λ ()
       (if (null? iter)
           (begin0 (ring-buffer->vector left) (set! done? #t))
           (begin (ring-buffer-push! (car iter) left)
                  (set! iter (cdr iter))
                  (ring-buffer->vector left))))
     (λ ([_ : (Vectorof a)]) done?))))

;;; string transforms & formatting

;; split an input stream into lines each of which is no more than a given length
(: split-at-every (-> Natural Input-Port (Listof String)))
(define (split-at-every i p)
  (reverse
   (let loop ([acc : (Listof String) null])
     (let ([x (read-string i p)])
       (if (eof-object? x)
           acc
           (loop (cons x acc)))))))

;; split a string into lines each of which is no more than a given length,
;; and split at word boundaries.
(: split-string-at-every/word-boundary (-> Natural String (Listof String)))
(define (split-string-at-every/word-boundary l s)
  (let ([ws (string-split s)])
    (let loop ([cur-line-str : String (car ws)]
               [ws : (Listof String) (cdr ws)]
               [acc : (Listof String) null])
      (if (null? ws)
          (reverse (cons cur-line-str acc))
          (let ([w (car ws)])
            (if (>= (+ (string-length w) (string-length cur-line-str)) l)
                (loop w (cdr ws) (cons cur-line-str acc))
                (loop (string-append cur-line-str " " w) (cdr ws) acc)))))))

;; if pred is a predicate, then that function is used; if instead pred is a character, equality with that character is used
;; you could use regexp-match or (list->string (sequence->list stop-before str pred)), but this is probably faster
(: take-until/string (-> (U Char (-> Char Any)) String String))
(define (take-until/string pred str)
  (let ([stop? : Boolean #f]
        [pred (if (char? pred) (curry char=? pred) pred)])
    (string-fold (λ ([c : Char] [s : String])
                   (cond [stop? s]
                         [(pred c) (set! stop? #t) s]
                         [else (string-append s (string c))]))
                 "" str)))

(define (percent-change-str [a : Real] [b : Real] [target : (U Real #f) #f]) : String
  (let ([chg (percent-change a b)])
    (if (positive? chg)
        (format "+~a%" (real->decimal-string chg 2))
        (format  "~a%" (real->decimal-string chg 2)))))

;; also truncates decimals to 2 places, for particular convenience for this stock-portions
;; program, since doing so happens to be appropriate since we're using thousandComma only for dollar figures
(define (thousandComma [n : Real]) : String
  (let* ([strs (string-split (number->string n) ".")]
         [non-decimal (car strs)]
         [decimals (if (= 2 (length strs))
                       (let ([s1 (cadr strs)])
                         (string-append "." (substring s1 0 (min (string-length s1) 2))))
                       "")])
    (string-append
     (list->string (let ([c -1])
                     (foldr (λ ([a : Char] [b : (Listof Char)])
                              (set! c (+ 1 c))
                              (if (and (not (= c 0)) (= 0 (modulo c 3)))
                                  (cons a (cons #\, b))
                                  (cons a b)))
                            (ann null (Listof Char))
                            (string->list non-decimal))))
     decimals)))

;; scraps trailing ".0" if present
(define (trim-useless-point-0 [s : String]) : String
  (if (string-suffix? s ".0")
      (substring s 0 (- (string-length s) 2))
      s))

;;; lists

;; for a list xs of length n:
;; if m <= n: (take m xs)
;; else #false
;; runtime complexity: O(m)
(: safe-take (∀ (a) (->* (Natural (Listof a)) (Boolean) (Option (Listof a)))))
(define (safe-take m xs [no-more-than? #f])
  (define flag : Boolean #f)
  (let ([xs0
         (let loop : (Listof a)
              ([count : Natural 0]
               [xs xs])
              (if (< count m)
                  (if (null? xs)
                      (begin (set! flag #t) null) ;; set! called only once. makes algo more efficient.
                      (cons (car xs)
                            (loop (add1 count) (cdr xs))))
                  null))])
    (if flag (if no-more-than? xs #f) xs0)))

;; if (length xs) < m, xs is returned; else (take n xs). O(m)
(define-syntax-rule (take-no-more-than m xs) (safe-take m xs #t))
