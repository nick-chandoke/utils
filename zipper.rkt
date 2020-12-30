#lang typed/racket/base

;;; simple zipper created for curses text field entry

(require srfi/2 (for-syntax racket/base))
(provide (except-out (all-defined-out) zipper uncons nzh))

;; "null zipper helper"
;; unsafely references identifier #'z in expansion context
;; and assumes that the "then" expression will be in terms of #'zc
(define-syntax (nzh stx)
  (let* ([args (cdr (syntax->datum stx))]
         [then (car args)]
         [else (cadr args)])
    (datum->syntax stx `(or (and-let* ([zc (zipper-current z)]) ,then) ,else))))

;; a bit simpler/cleaner than doing and-let*
(: uncons (∀ (a) (-> (Listof a) (Option (Pairof a (Listof a))))))
(define (uncons xs)
  (if (null? xs) #f (cons (car xs) (cdr xs))))

(struct (a) zipper
  ([before : (Listof a)] ;; before is stored in reverse order for more efficent zipper-left
   [current : (Option a)]
   [after : (Listof a)])
  #:type-name Zipper)

(: zipper-empty? (∀ (a) (-> (Zipper a) Boolean)))
(define (zipper-empty? z) (not (zipper-current z)))

(: empty-zipper (∀ (a) (-> (Zipper a))))
(define (empty-zipper) ((inst zipper a) null #f null))

;; if you do not specify the selected element and the "after" list,
;; then the cursor is set to the leftmost element.
(: make-zipper (∀ (a) (case->
                       (-> (Listof a) (Option a) (Listof a) (Zipper a))
                       (-> (Listof a) (Zipper a)))))
(define make-zipper
  (case-lambda [([l : (Listof a)] [e : (Option a)] [r : (Listof a)])
                (if (and (not e) (not (and (null? l) (null? r))))
                    (raise-arguments-error 'make-zipper "current element = #f <=> before & after are both null"
                                           "before" l
                                           "current" e
                                           "after" r)
                    ((inst zipper a) (reverse l) e r))]
               [([xs : (Listof a)]) ((inst zipper a) null (car xs) (cdr xs))]))

;; move the zipper cursor to the left by one element
;; returns original object if cursor is already at beginning of zipper
(: zipper-left (∀ (a) (-> (Zipper a) (Zipper a))))
(define (zipper-left z)
  (nzh (or (and-let* ([uc (uncons (zipper-before z))])
                     (zipper (cdr uc) (car uc) (cons (zipper-current z) (zipper-after z))))
           z)
       z))

;; move the zipper cursor to the right by one element
;; returns original object if cursor is already at end of zipper
(: zipper-right (∀ (a) (-> (Zipper a) (Zipper a))))
(define (zipper-right z)
  (nzh (or (and-let* ([uc (uncons (zipper-after z))])
                 (zipper (cons (zipper-current z) (zipper-before z)) (car uc) (cdr uc)))
           z)
       z))

;; insert before current, pushing elements to the right.
;; akin to 'i' in vim
(: zipper-insert (∀ (a) (-> a (Zipper a) (Zipper a))))
(define (zipper-insert e z)
  (nzh (struct-copy zipper z [current e] [after (cons zc (zipper-after z))])
       (zipper null e null)))

;; insert after current, pushing elements to the right.
;; if cursor is at last element, then it grows the "after" array
;; akin to 'a' in vim
(: zipper-append (∀ (a) (-> (Zipper a) a (Zipper a))))
(define (zipper-append z e)
  (nzh
   (struct-copy zipper z [after (cons e (zipper-after z))])
   (zipper null e null)))

;; append after rightmost element. zipper's cursor is ignored.
;; akin to 'A' in vim
;; moves cursor to the end of the list, at the position of the appended element
(: zipper-append* (∀ (a) (-> (Zipper a) a (Zipper a))))
(define (zipper-append* z e)
  (nzh (zipper (zipper->list z) e null)
       (zipper null e null)))

;; remove the item at the cursor
;; items before the cursor are unmodified. items after the cursor
;; are shifted leftward.
;; if cursor is at the last element, then that element is discarded
;; and the cursor shifts leftward.
(: zipper-remove (∀ (a) (-> (Zipper a) (Zipper a))))
(define (zipper-remove z)
  (nzh
   ;; nzh guarantees that current is not #f
   (let ([uca (uncons (zipper-after z))]
         [ucb (uncons (zipper-before z))])
     ;; try to remove from "after" first; if after is null, then replace current by its predecessor and drop 1 from "before"
     (cond [uca ;; z ~ (bs c (a . as)); replace current with its predecessor
            (struct-copy zipper z [current (car uca)] [after (cdr uca)])]
           [ucb ;; z ~ ((b . bs) c as); shift left
            (struct-copy zipper z [before (cdr ucb)] [current (car ucb)])]
           [else ;; current is the only element. remove it, leaving the empty zipper
            ((inst empty-zipper a))]))
   z))

(: zipper->list (∀ (a) (-> (Zipper a) (Listof a))))
(define (zipper->list z)
  (nzh (append (reverse (zipper-before z)) (cons zc (zipper-after z)))
       null))

(: zipper-size (∀ (a) (-> (Zipper a) Integer)))
(define (zipper-size z)
  (nzh
   (+ 1 (length (zipper-before z)) (length (zipper-after z)))
   0))
