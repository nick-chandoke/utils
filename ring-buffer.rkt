#lang typed/racket/base

(provide make-ring-buffer ring-buffer-push ring-buffer-push! ring-buffer->vector ring-buffer->list RingBuffer)
(require (only-in racket/vector vector-split-at vector-append) syntax/parse/define)

;; holds most recent n pushed elements. most recently pushed items are at the (right) end;
;; items are overwritten from the left side.
;; size remains constant. index increases on each push.
(struct (a) ring-buffer ([index : Integer] [size : Integer] [data : (Mutable-Vectorof a)]) #:type-name RingBuffer #:transparent)

(: make-ring-buffer (∀ (a) (case-> ;; the case-> is ambiguous if a ~ Natural and you want to add exactly two elements! thus here the first case-> is assumed
                            (-> Integer a (RingBuffer a))
                            (->* (a) () #:rest-star (a) (RingBuffer a)))))
(define make-ring-buffer
  (case-lambda [([size : Integer] [init-val : a])
                (let ([v ((inst make-vector a) size init-val)])
                  ((inst ring-buffer a) size size v))]
               [(x . xs) (let* ([v (apply vector (cons x xs))]
                                [size (vector-length v)])
                           ((inst ring-buffer a) size size v))]))

(: ring-buffer-push (∀ (a) (-> a (RingBuffer a) (RingBuffer a))))
(define (ring-buffer-push e buf)
  (let ([i (ring-buffer-index buf)]
        [s (ring-buffer-size buf)]
        [b (ring-buffer-data buf)])
    (vector-set! b (modulo i s) e)
    (ring-buffer (add1 i) s b)))

(define-simple-macro (ring-buffer-push! e:expr b:id)
  (set! b (ring-buffer-push e b)))

(: ring-buffer->vector (∀ (a) (-> (RingBuffer a) (Mutable-Vectorof a))))
(define (ring-buffer->vector buf)
  (let*-values ([(b) (ring-buffer-data buf)]
                [(i) (ring-buffer-index buf)]
                [(s) (ring-buffer-size buf)]
                [(p z) (vector-split-at b (modulo i s))])
    (if (< i s) p (vector-append z p))))

(: ring-buffer->list (∀ (a) (-> (RingBuffer a) (Listof a))))
(define (ring-buffer->list buf)
  (let*-values ([(b) (ring-buffer-data buf)]
                [(i) (ring-buffer-index buf)]
                [(s) (ring-buffer-size buf)]
                [(p z) (vector-split-at b (modulo i s))])
    (if (< i s)
        (vector->list p)
        (append (vector->list z) (vector->list p)))))
