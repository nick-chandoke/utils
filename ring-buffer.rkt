#lang typed/racket/base

(provide make-ring-buffer ring-buffer-push ring-buffer-push! ring-buffer->vector ring-buffer->list RingBuffer)
(require (only-in racket/vector vector-split-at vector-append) syntax/parse/define)

;; holds most recent n pushed elements. most recent items at end of vector returned by ring-buffer->vector.
;; size remains constant. index increases on each push.
(struct (a) ring-buffer ([index : Natural] [size : Natural] [data : (Mutable-Vectorof a)]) #:type-name RingBuffer)

;; you need to supply a dummy value for a, to fill the ring buffer with that value for its initialization.
(: make-ring-buffer (∀ (a) (-> Natural a (RingBuffer a))))
(define (make-ring-buffer size init-val)
  ((inst ring-buffer a) 0 size ((inst make-vector a) size init-val)))

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
    (if (< i s) (vector->list p) (append (vector->list z) (vector->list p)))))
