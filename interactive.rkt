#lang racket/base

(provide (all-defined-out))
(require (for-syntax racket/base syntax/parse)
         (only-in racket/format ~a)
         (only-in racket/function identity const)
         (only-in racket/file make-directory* fold-files)
         (only-in racket/path file-name-from-path)
         (only-in racket/system system/exit-code)
         (only-in "util.rkt" chunks-of)
         (only-in racket/list make-list split-at split-at-right))

;; shorts when any optionally-bound item in sequence is a non-0 integer;
;; upon such an integer, 0-let*/seq returns that value.
;; e.g. (0-let*/seq [x <- 4.6] [y <- (+ x 5.0)] (displayln "here") [z <- (if (even? (exact-floor y)) 1 0)] (displayln "finished"))
;; prints "here", then "finished", and returns 0; if x were change to 4, then evaluation would stop and 4 would be returned.
(define-syntax (0-let*/seq stx)
  (syntax-parse stx #:datum-literals (<-)
    [(_) #'0] ;; everything was done successfully; return 0.
    [(_ (~or [i:id <- e] e) xs ...)
     (if (attribute i)
         #'(let ([i e]) (if (or (and (exact-integer? i) (not (= 0 i))) (string? i)) i (0-let*/seq xs ...)))
         #'(let ([E e]) (if (or (and (exact-integer? E) (not (= 0 E))) (string? E)) E (0-let*/seq xs ...))))]))

;; prints a table of data (converted to strings by ~a) with columns aligned
(define (pp-table xss)
  (if (null? xss)
      (void)
      (let ([col-sizes (apply foldr (λ X (let-values ([(ss acc) (split-at-right X 1)]) (cons (foldl (λ (s m) (max (string-length (~a s)) m)) 0 ss) (car acc)))) '() xss)])
        (for-each (λ (xs) (for-each (λ (x s) (let ([x (~a x)]) (printf "~a~a " x (make-string (- s (string-length x)) #\space)))) xs col-sizes) (printf "~n")) xss))))

;; guaranteed to have no more than the given number of columns;
;; on certain edge cases there may be one fewer than the specified number of columns.
(define (print-choices choices [cols 5])
  (pp-table (chunks-of cols (for/list ([k (in-naturals 1)] [i choices]) (format "~a. ~a" k i)) #:pad "")))

;; OTHER HELPER FUNCTIONS

;; lps : ((string . pred)).
;; returns (values matched-items unmatched-lps)
;; e.g. (find-multiple (range 10) `(("=5" . ,(curry = 5)) ("|2" . ,even?) (">40" . ,(λ (x) (> x 40)))))
;; returns (values (set '("|2" . 6) '("=5" . 5)) '((">40" . #<procedure>)))
;; UNUSED; the code that used it was discarded.
#;(define (find-multiple xs lps)
  (let loop ([matched (set)] [lps lps] [xs xs])
    (cond [(null? lps) (values matched lps)]
          [(null? xs) (values matched lps)]
          [else (if ((cdar lps) (car xs))
                    (loop (set-add matched (cons (caar lps) (car xs))) (cdr lps) (cdr xs))
                    (loop matched lps (cdr xs)))])))

;; PROMPT FUNCTIONS

;; TODO: output what the user selected, just for feedback/confirmation; this is needed if there're very
;; many items to list. however, again, if many items then an interactive interface with highlighting the
;; cursor/selection is much more appropriate than entering a number. furthermore the user must be able
;; to search through the options!
;; select one/some from many displayed options
;; returns #f if no choice taken, as signaled by ^C
;; if #:return-index? #t, then returns the index of the selected item rather
;; than the value of the item itself.
(define (select prompt choices [print-choices print-choices] #:return-index? [return-index? #f])
  (let ([m (for/hash ([k (in-naturals 1)] [c choices]) (values k c))])
    (printf "~a~n~n" prompt)
    (print-choices choices)
    (printf "~nselect a choice by number: ")
    (with-handlers ([exn:break? (λ (_) (printf "\n") #f)])
      (let loop ()
        (let ([index (let ([s (read-line)]) (if (eof-object? s) s (string->number s)))])
          (cond [(eof-object? index) #f] ;; treat ^D as ^C
                [index (let ([selection (hash-ref m index #f)])
                         (if selection
                             (if return-index?
                                 index
                                 selection)
                             (begin (displayln "invalid index. try again." (current-error-port))
                                    (loop))))]
              [else (begin (displayln "please enter a number." (current-error-port))
                           (loop))]))))))

;; select a text input (single line)
;; callback is an action that accepts the user's input string
;; prompt is itself an action
(define ((prompt p callback [default #f]))
  (display p)
  (with-handlers ([exn:break? (λ (_) (printf "\n") #f)])
    (callback (let ([l (read-line)])
              (if (string=? "" l)
                  default
                  l)))))

;; convenient/terse action definition.
;; errmsg is printed in addition to the generic error message if sys0 returns non-0 exit code.
;; if errmsg is a string, then that string is returned; if it's not, then the string "<cmd> failed" is returned.
;; if print-err?, error string is printed.
(define ((sys0 cmd [errmsg #f] #:print-err? [print-err? #f] #:required? [required? #f]))
  (with-handlers ([exn:break? (λ (_) (if required? #f 0))])
    (if (= 0 (system/exit-code cmd))
        0
        ((if print-err? displayln identity) (if (string? errmsg)
                                                errmsg
                                                (format "`~a` failed." cmd))))))

(define (read-proc cmdline)
  (let ([os (open-output-bytes)] [os-err (open-output-bytes)])
    (parameterize ([current-output-port os] [current-error-port os-err])
      (let ([ec (system/exit-code cmdline)])
        (if (= 0 ec)
            (get-output-string os)
            `(,ec . ,(get-output-string os-err)))))))

(define (write-file path str) (with-output-to-file #:exists 'truncate/replace path (λ _ (display str))))

;; returns #f if couldn't create due to permission error; other errors are not caught
(define (mkdir d)
  (with-handlers ([(λ (e) (and exn:fail:filesystem:errno?
                               (equal? '(13 . posix) (exn:fail:filesystem:errno-errno e))))
                   (const #f)])
    (make-directory* d)))

;; list of filesystem objects that descend from a given directory and optionally match
;; a given predicate. given directory is not included in returned list.
;; return value: list of paths represented by strings; these strings do not begin with
;; the argument directory's path, e.g. (descendants "path/to/dir") would return '("A.doc")
;; instead of "path/to/dir/A.doc". this behavior was chosen particularly for using
;; descendants with `select`.
;; t is of '(file dir link)
;; if #:basename? #t then only basename is put into list rather than path relative to argument directory.
(define (descendants d [pred (λ (p t) #t)] #:basename? [basename? #f])
  (unless (directory-exists? d)
    (error (format "[ERROR] invalid assumption; actually ~a does NOT exist" d)))
  (let ([wl (string-length d)])
    (fold-files (λ (p t l)
                      (if (pred p t)
                          ;; wp is path relative to d
                          (let ([wp (substring (path->string p) wl)])
                            (if (string=? "" wp)
                                l
                                (cons (if basename?
                                          (path->string (file-name-from-path p))
                                          (substring wp 1)) l)))
                          l))
                '()
                d)))

(define (dir&basename p) (split-at-right (explode-path p) 1))
(define (dirname p) (let-values ([(d _) (dir&basename p)]) d))
(define (basename p) (let-values ([(_ bn) (dir&basename p)]) bn))
