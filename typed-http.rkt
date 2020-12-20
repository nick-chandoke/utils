#lang typed/racket/base

#|
note that #:headers takes a hash map, so that any headers set by middleware can be overridden
NOTE: do not use const in your handler hash map! observe:
(hash-ref
 ((inst hash Natural (-> String Void)) 400 (const (displayln "hello")))
 399 #f)
prints "hello" then returns false! if instead you use (λ (x) (displayln "hello")), then the lambda is *not*
evaluated, as one would expect!
please use constr, which looks like const but expands to a lambda
|#

(provide (all-defined-out))
(require (only-in typed/json JSExpr read-json)
         (only-in typed/net/url-structs URL Path/Param path/param)
         (only-in racket/string string-split)
         (only-in racket/function const))

;; lambda whose argument is an ignored Easy-Response
(define-syntax-rule (constr e ...) (λ ([_ : Easy-Response]) e ...))
(struct exn:retry exn:fail () #:transparent #:type-name Exn:Retry)

(require/typed/provide gregor/period
  [#:opaque TimePeriod time-period?]
  [milliseconds (-> Integer TimePeriod)])

(define-type HTTP-Method (U 'delete 'head 'get 'options 'patch 'post 'put))
(define-type Query-Params (Listof (Pairof Symbol (Option String))))
(define-type Headers (HashTable Symbol (U Bytes String)))
(define-type Positive-Real (U Positive-Exact-Rational Positive-Float))
(define-type Timeout (Option Positive-Float))

(require/typed/provide net/http-easy
  [#:opaque Easy-Response response?]
  (response-status-line (-> Easy-Response Bytes)) ;; status line is just code + message
  (response-status-code (-> Easy-Response Exact-Positive-Integer))
  (response-headers (-> Easy-Response (Listof Bytes)))
  (response-body (-> Easy-Response Bytes)) ;; cf response-output, which seemingly always returns ""
  (response-json (-> Easy-Response (U EOF JSExpr)))
  [#:opaque Session session?]
  [#:opaque Pool-Config pool-config?]
  [#:opaque Timeout-Config timeout-config?]
  (make-timeout-config (-> [#:lease Timeout] [#:connect Timeout] [#:request Timeout] Timeout-Config))
  (make-pool-config (-> [#:max-size Positive-Byte] [#:idle-timeout (Option Positive-Real)] Pool-Config)) ;; positive real
  (make-session (-> [#:pool-config Pool-Config]
                    ;; [#:cookie-jar (Option cookie-jar)] ;; can't do until we can convert (is-a/c cookie-jar<%>) into a type/predicate
                    Session))
  (session-request (-> Session (U Bytes String URL)
                       [#:close? Boolean]
                       [#:stream? Boolean]
                       [#:method HTTP-Method]
                       [#:headers Headers]
                       [#:params Query-Params]
                       [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
                       [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
                       [#:form (Option (Listof (Pairof Symbol (Option String))))]
                       [#:json (Option JSExpr)]
                       [#:timeouts Timeout-Config]
                       Easy-Response)))

;; ignores cdata and valid-char
(define-type XExpr (Rec x (U String
                             (List* Symbol (Listof (List Symbol String)) (Listof x))
                             (Pairof Symbol (Listof x))
                             Symbol)))

;; commonly (RespHandler JSExpr a) or (RespHandler Bytes a)
(define-type (RespHandler b a)     (-> Easy-Response (U b a 'retry 'defer-to-catch-all)))
(define-type (CatchAllHandler b a) (-> Easy-Response (U b a 'retry)))

(: str->pparams (-> String (Listof Path/Param)))
(define (str->pparams s) (map (λ ([p : String]) (path/param p null)) (string-split s "/")))

(require/typed retry
  [#:opaque Retryer retryer?]
  (retryer-compose (-> Retryer * Retryer))
  (cycle-retryer (-> Retryer Natural Retryer))
  (retryer (-> [#:handle (-> Any Natural Void)]
               [#:should-retry? (-> Any Natural Boolean)]
               Retryer))
  (sleep-exponential-retryer (-> TimePeriod [#:exponent-base Natural] Retryer))
  (call/retry (∀ (a) (-> Retryer (-> a) a))))

(: =retry? (-> Any Boolean : 'retry))
(define (=retry? x) (equal? x 'retry))

;; pretty curious that when i defined this as a macro it seemed to not have this type...?
(: retry-or-val (∀ (a) (-> (U 'retry a) a)))
(define (retry-or-val x) (if (=retry? x) (raise (exn:retry "" (current-continuation-marks))) x))

(: session-request-with-handlers
   (∀ (b a) (-> Session (U Bytes String URL) (-> Easy-Response b)
                ;; to pass verbatim to session-request
                [#:close? Boolean]
                [#:stream? Boolean]
                [#:method HTTP-Method]
                [#:headers Headers]
                [#:query Query-Params]
                [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
                [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
                [#:timeouts Timeout-Config]
                [#:form (Option (Listof (Pairof Symbol (Option String))))]
                [#:json JSExpr]
                ;; unique to this function
                [#:handler (Option (U (RespHandler b a)
                                      (HashTable Exact-Positive-Integer (RespHandler b a))))]
                [#:catch-all (Option (CatchAllHandler b a))]
                (U b a))))
(define (session-request-with-handlers
         sess path with-response
         ;; to pass verbatim to session-request
         #:close? [close? #f]
         #:stream? [stream? #f]
         #:method [method 'get]
         #:headers [headers (ann (hasheq) (Immutable-HashTable Symbol (U Bytes String)))]
         #:query [query null]
         #:auth [auth #f]
         #:data [data #f]
         #:timeouts [timeouts (make-timeout-config)]
         #:form [form #f]
         #:json [json #f]
         ;; unique to this function
         #:handler [resp-handler-provider #f]
         #:catch-all [catch-all #f])
  (call/retry
   (retryer-compose (cycle-retryer (sleep-exponential-retryer (milliseconds 500)) 3)
                    (retryer #:should-retry? (λ ([r : Any] [n : Natural])
                                               (when (> n 0) (printf "retrying for the ~a time~n" n))
                                               (or (exn:fail:network? r) (exn:retry? r)))))
   (λ ()
     (let* ([resp (cond [form (session-request
                               sess path #:form form
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:timeouts timeouts)]
                        [json (session-request
                               sess path #:json json
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:timeouts timeouts)]
                        [data (session-request
                               sess path #:data data
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:timeouts timeouts)]
                        [else (session-request
                               sess path
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:data data     #:timeouts timeouts)])]
            [status-code (response-status-code resp)]
            ;; handler for this particular status code
            [handler (and resp-handler-provider
                          (if (hash? resp-handler-provider)
                              (hash-ref resp-handler-provider status-code
                                        ;; double const: one for hash-ref's third paramater type being (-> c) instead of just c,
                                        ;; and another to make a function from any response to 'defer-to-catch-all
                                        (const (const 'defer-to-catch-all)))
                              resp-handler-provider))])
       (cond [(= 200 status-code) (with-response resp)]
             [handler
              (let ([handler-result (handler resp)])
                (if (equal? 'defer-to-catch-all handler-result)
                    (if catch-all
                        (retry-or-val (catch-all resp))
                        (error (format "handler for HTTP ~a asked to defer to catch-all, but no #:catch-all handler was passed to session-request-with-handlers!" status-code)))
                    (retry-or-val handler-result)))]
             [catch-all (retry-or-val (catch-all resp))]
             [else (error (format "session-request-with-handlers: unhandled HTTP ~a" status-code))])))))

(: http-json-api (∀ (a) (-> Session (U Bytes String URL)
                            [#:close? Boolean]
                            [#:stream? Boolean]
                            [#:method HTTP-Method]
                            [#:headers Headers]
                            [#:query Query-Params]
                            [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
                            [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
                            [#:timeouts Timeout-Config]
                            [#:form (Option (Listof (Pairof Symbol (Option String))))]
                            [#:json JSExpr]
                            ;; unique to http-json-api
                            [#:handler (Option (U (RespHandler JSExpr a)
                                                  (HashTable Exact-Positive-Integer (RespHandler JSExpr a))))]
                            [#:catch-all (Option (CatchAllHandler JSExpr a))]
                            (U JSExpr a))))
(define (http-json-api sess path
                       #:close? [close? #f]
                       #:stream? [stream? #f]
                       #:method [method 'get]
                       #:headers [headers (ann (hasheq) (Immutable-HashTable Symbol (U Bytes String)))]
                       #:query [query null]
                       #:auth [auth #f]
                       #:data [data #f]
                       #:timeouts [timeouts (make-timeout-config)]
                       #:form [form #f]
                       #:json [json #f]
                       #:handler [resp-handler-provider #f]
                       #:catch-all [catch-all #f])
  ((inst session-request-with-handlers JSExpr a)
   sess path (λ ([resp : Easy-Response])
               (let ([j (response-json resp)])
                 (if (eof-object? j) (ann #hash() JSExpr) j)))
   #:close? close?
   #:stream? stream?
   #:method method
   #:headers (hash-set headers 'Accept "application/json")
   #:query query
   #:auth auth
   #:data data
   #:timeouts timeouts
   #:form form
   #:json json
   #:handler resp-handler-provider
   #:catch-all catch-all))

;; often used with open-input-bytes
(: session-request-stream-bytes
   (∀ (a) (-> Session (U Bytes String URL)
              [#:close? Boolean]
              [#:method HTTP-Method]
              [#:headers Headers]
              [#:query Query-Params]
              [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
              [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
              [#:timeouts Timeout-Config]
              [#:form (Option (Listof (Pairof Symbol (Option String))))]
              [#:json JSExpr]
              ;; unique to http-json-api
              [#:handler (Option (U (RespHandler Bytes a)
                                    (HashTable Exact-Positive-Integer (RespHandler Bytes a))))]
              [#:catch-all (Option (CatchAllHandler Bytes a))]
              (U Bytes a))))
(define (session-request-stream-bytes
         sess path
         #:close? [close? #f]
         #:method [method 'get]
         #:headers [headers (ann (hasheq) (Immutable-HashTable Symbol (U Bytes String)))]
         #:query [query null]
         #:auth [auth #f]
         #:data [data #f]
         #:timeouts [timeouts (make-timeout-config)]
         #:form [form #f]
         #:json [json #f]
         #:handler [resp-handler-provider #f]
         #:catch-all [catch-all #f])
  ((inst session-request-with-handlers Bytes a)
   sess path response-body
   #:close? close?
   #:stream? #t
   #:method method
   #:headers headers
   #:query query
   #:auth auth
   #:data data
   #:timeouts timeouts
   #:form form
   #:json json
   #:handler resp-handler-provider
   #:catch-all catch-all))
