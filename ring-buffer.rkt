#lang racket/base

;;; ring buffer emulation. uses a vector rather than a circular linked list.

(provide (except-out (all-defined-out) ring-buffer))
(require (only-in racket/vector vector-split-at vector-append vector-copy)
         (only-in racket/function curry)
         (only-in racket/match define/match)
         (only-in racket/sequence sequence-append))

;; where interval is inclusive on both sides
;(: in-interval (∀ (s a) (-> s (-> s Integer a) Integer Integer (Sequenceof a))))
(define (in-interval s f a z) (make-do-sequence (λ () (values (curry f s) add1 a (curry > z) #f #f))))

;; holds most recent n pushed elements. most recently pushed items are at the (right) end;
;; items on the left side "fall off" as new elements are added.
;; size remains constant. index increases on each push.
;; NOTE: you should not use the ring-buffer constructor for normal ring buffer usage! use make-ring-buffer or ring-buffer-from-elems
;; instead.
(struct ring-buffer (index data) #:mutable) ; mutable for index, not data

;; instantly clear buffer without actually modifying the ring buffer's contents in memory.
(define (ring-buffer-reset! rb) (set-ring-buffer-index! rb 0))

#| if you'll push no more than <size> elements to a buffer b, then b is effectively a queue.
   then (ring-buffer-data b) on [0, index] is equal to (ring-buffer->vector b) except that
   it's the underlying vector rather than a copy, and is thus more efficient.
   in-queue does this but encapsulated in a sequence.
   NOTE: vector-ref raises index out of range if in-queue is invoked on a ring buffer that's
   had more than <size> elements pushed to it.
   NOTE: when in-queue is evaluated, its size is the number of elements yet pushed, n. you can
   push more elements without affecting the sequence, PROVIDED THAT you don't push more than
   <size> - n elements! consider the following:

   (define rb (make-ring-buffer 10))
   (ring-buffer-push! rb 1 2 4 7 6 4)
   (for ([z (in-queue rb)] [i (in-naturals)])
     (ring-buffer-push! rb 100 200 300)
     (printf "~a " z))

   produces 1 2 4 200 300 100 because, even though the length of the produced sequence was 6,
   avoiding an index-out-of-bounds error, the earlier elements of the buffer were overridden
   faster than the sequence could output values! the only way to do this is by pushing at least
   more than one element before using the current list element. consider this extreme example:

   (define rb (ring-buffer-from-elems 1 2 4 7 6 4)) ; buffer is defined as being full
   ;; prints 1 2 4 7 6 4 because z is bound before the value at that position is overridden by the push
   (for ([z (in-queue rb)] [i (in-naturals)])
     (ring-buffer-push! rb 100)
     (printf "~a " z))
   (ring-buffer->list rb) ; '(100 100 100 100 100 100)
|#
(define (in-queue rb) (in-interval (ring-buffer-data rb) vector-ref 0 (ring-buffer-index rb)))

(define (ring-buffer-full? rb) (>= (ring-buffer-index rb) (vector-length (ring-buffer-data rb))))

(define (make-ring-buffer size [init-val 0]) (ring-buffer 0 (make-vector size init-val)))

(define (ring-buffer-from-elems . xs)
  (let* ([v (apply vector xs)]
         [size (vector-length v)])
    (ring-buffer size v)))

(define (vector->ring-buffer v) (ring-buffer (vector-length v) v))

;; select the nth most recently-pushed element or (a copy of) the k most recent
;; elements starting n indices back. as a special case k=+inf.0 returns
;; all elements up to and including the nth.
;; returns #f if index is out of bounds.
(define (rb@ rb [n 0] [k #f])
  (let* ([b (ring-buffer-data rb)]
         [l (- (modulo (sub1 (ring-buffer-index rb)) (vector-length b)) n)]
         [l2 (and k (if (eq? +inf.0 k) 0 (add1 (- l k))))])
    (if l2
        (and (>= l2 0) (vector-copy b l2 (add1 l)))
        (and (>= l 0) (vector-ref b l)))))

(define (list->ring-buffer xs) (apply ring-buffer-from-elems xs))

(define (ring-buffer-push! rb . es)
  (let ([i0 (ring-buffer-index rb)]
        [rbs (vector-length (ring-buffer-data rb))])
    (let loop ([k 0] [rst es])
      (if (null? rst)
          (set-ring-buffer-index! rb (+ i0 k))
          (begin (vector-set! (ring-buffer-data rb) (modulo (+ i0 k) rbs) (car rst))
                 (loop (add1 k) (cdr rst)))))))

;; a bit faster than ring-buffer->vector
(define/match (in-ring-buffer buf)
  [((ring-buffer i b)) (let*-values ([(s) (vector-length b)]
                                     [(p z) (vector-split-at b (modulo i s))])
                         (if (< i s) (in-vector p) (in-sequences (in-vector z) (in-vector p))))])

;; a bit faster than ring-buffer->list
(define/match (ring-buffer->vector buf)
  [((ring-buffer i b)) (let*-values ([(s) (vector-length b)]
                                     [(p z) (vector-split-at b (modulo i s))])
                         (if (< i s) p (vector-append z p)))])

(define/match (ring-buffer->list buf)
  [((ring-buffer i b)) (let*-values ([(s) (vector-length b)]
                                     [(p z) (vector-split-at b (modulo i s))])
                         (if (< i s)
                             (vector->list p)
                             (append (vector->list z) (vector->list p))))])
