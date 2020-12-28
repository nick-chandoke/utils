#lang typed/racket/base

;;; simple zipper created for curses text field entry

(provide (except-out (all-defined-out) zipper uncons))

;; a bit simpler/cleaner than doing and-let*
(: uncons (∀ (a) (-> (Listof a) (Option (Pairof a (Listof a))))))
(define (uncons xs)
  (if (null? xs) #f (cons (car xs) (cdr xs))))

(struct (a) zipper
  ([before : (Listof a)] ;; before is stored in reverse order for more efficent zipper-left
   [current : a]
   [after : (Listof a)])
  #:type-name Zipper)

(: make-zipper (∀ (a) (-> (Listof a) a (Listof a) (Zipper a))))
(define (make-zipper l e r) (zipper (reverse l) e r))

;; move the zipper cursor to the left by one element
;; returns original object if cursor is already at beginning of zipper
(: zipper-left (∀ (a) (-> (Zipper a) (Zipper a))))
(define (zipper-left z)
  (let ([uc (uncons (zipper-before z))])
    (if uc
        (zipper (cdr uc) (car uc) (cons (zipper-current z) (zipper-after z)))
        z)))

;; move the zipper cursor to the right by one element
;; returns original object if cursor is already at end of zipper
(: zipper-right (∀ (a) (-> (Zipper a) (Zipper a))))
(define (zipper-right z)
  (let ([uc (uncons (zipper-after z))])
    (if uc
        (zipper (cons (zipper-current z) (zipper-before z)) (car uc) (cdr uc))
        z)))

;; insert before current, pushing elements to the right.
;; akin to 'i' in vim
(: zipper-insert (∀ (a) (-> a (Zipper a) (Zipper a))))
(define (zipper-insert e z)
  (struct-copy zipper z [current e] [after (cons (zipper-current z) (zipper-after z))]))

;; insert after current, pushing elements to the right.
;; if cursor is at last element, then it grows the "after" array
;; akin to 'a' in vim
(: zipper-append (∀ (a) (-> (Zipper a) a (Zipper a))))
(define (zipper-append z e)
  (struct-copy zipper z [after (cons e (zipper-after z))]))

;; append after rightmost element. zipper's cursor is ignored.
;; akin to 'A' in vim
(: zipper-append* (∀ (a) (-> (Zipper a) a (Zipper a))))
(define (zipper-append* z e)
  (zipper (zipper->list z) e null))

(: zipper->list (∀ (a) (-> (Zipper a) (Listof a))))
(define (zipper->list z)
  (append (reverse (zipper-before z)) (cons (zipper-current z) (zipper-after z))))

(: zipper-size (∀ (a) (-> (Zipper a) Integer)))
(define (zipper-size z)
  (+ 1 (length (zipper-before z)) (length (zipper-after z))))
