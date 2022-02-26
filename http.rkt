#lang racket/base

;; TODO: after understanding how to duplicate inputs ports, use #:stream?, Input-Port, and response-output correctly, rather than working with Bytes & response-body, which should probably not be used at all in this api.
;; NOTE: you may want to use (with-handlers ([exn:fail:http-easy:timeout? (lambda (e) (displayln (exn:fail:http-easy:timeout-kind e)))]) ...)

#|
note that #:headers takes a hash map, so that any headers set by middleware can be overridden
NOTE: do not use const in your handler hash map! observe:
(hash-ref
 (hash 400 (const (displayln "hello")))
 399 #f)
prints "hello" then returns false! if instead you use (λ (_) (displayln "hello")), then the lambda is *not*
evaluated, as one would expect!
please use constr, which looks like const but expands to a lambda
|#

(provide (except-out (all-defined-out)
                     retry-or-val print-request print-response)
         (all-from-out net/http-easy))

(require (only-in racket/bytes bytes-join)
         (only-in web-server/private/connection-manager connection-o-port)
         (only-in web-server/servlet-dispatch serve/launch/wait)
         (only-in racket/function const)
         (only-in racket/port port->string)
         (only-in racket/string string-split string-join)
         (only-in json read-json jsexpr->string)
         (only-in net/uri-codec alist->form-urlencoded)
         (only-in net/url url->string)
         (only-in gregor/period milliseconds)
         (only-in retry retryer-compose cycle-retryer retryer sleep-exponential-retryer call/retry)
         (only-in net/http-easy response-status-line response-status-code response-headers response-body response-output response-json
                  exn:fail:http-easy:timeout-kind make-timeout-config make-pool-config make-session session-request)
         (only-in net/url-structs path/param))

;; lambda whose argument is an ignored Easy-Response
(define-syntax-rule (constr e ...) (λ (_) e ...))
(struct exn:retry exn:fail () #:transparent)

;; (define-type HTTP-Method (U 'delete 'head 'get 'options 'patch 'post 'put))
;; (define-type Query-Params (Listof (Pairof Symbol (Option String))))
;; (define-type Headers (HashTable Symbol (U Bytes String))) ; NOTE: Headers is used to make requests only! response-headers : (Listof Bytes)!
;; (define-type Positive-Real (U Positive-Exact-Rational Positive-Float))
;; (define-type Timeout (Option Positive-Float))

;; commonly (RespHandler JSExpr a) or (RespHandler Bytes a)
;; (define-type (RespHandler a)     (-> Easy-Response (U a 'retry 'defer-to-catch-all)))
;; (define-type (CatchAllHandler a) (-> Easy-Response (U a 'retry)))

;; (: str->pparams (-> String (Listof Path/Param)))
(define (str->pparams s) (map (λ (p) (path/param p null)) (string-split s "/")))

(define (retry-or-val x) (if (equal? 'retry? x) (raise (exn:retry "" (current-continuation-marks))) x))

;; useful for debugging. i suppose this is fine because it uses response-body rather than response-output, which would be idempotent
;; TODO: if we're streaming, can we do response-body to get the whole response w/o affecting response-output?
;; (: print-response (-> Easy-Response Void))
(define (print-response r)
  (printf "---RESPONSE---~nHTTP ~a ~a~n~a~n~a~n" (response-status-code r) (response-status-line r)
          (bytes-join (response-headers r) #"\n") (response-body r)))

#;(: print-request (-> HTTP-Method
                    (U Bytes String URL) ; path (does not necessarily include all of the query params)
                    (HashTable Symbol (U Bytes String)) ; headers
                    Query-Params
                    String ; body
                    Void))
(define (print-request method path headers query body)
  (let ([path-string (cond [(string? path) path]
                           [(bytes? path) (bytes->string/utf-8 path)]
                           [else (url->string path)])]) ; would use (url? path) if such a predicate were defined!
    (printf "~a ~a HTTP/1.1~n~a~n~n~a~n"
            (string-upcase (symbol->string method))
            (if (null? query) ; NEXT: don't only check whether query is null; also check whether there's a query in the URL; if there is, then combine them.
                path-string
                (string-append path-string "?" (alist->form-urlencoded ; alist->form-urlencoded accepts an (AList Symbol String) but Query-Params uses (Option String) instead.
                                                (map (λ (p) ; p : (Symbol, Option String)
                                                       (cons (car p) (or (cdr p) "")))
                                                     query))))
            (string-join (for/list ([(k v) headers]) (format "~a: ~a" k v)) "\n")
            body)))

#;(: session-request-with-handlers
   (∀ (a) (-> Session (U Bytes String URL) (-> Easy-Response a)
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
                [#:handler (Option (U (RespHandler a) (HashTable Exact-Positive-Integer (RespHandler a))))]
                [#:catch-all (Option (CatchAllHandler a))]
                [#:debug? Boolean] ; print request. does not print response because that's assumed to be done by the caller
                a)))
(define (session-request-with-handlers
         sess path with-response
         ;; to pass verbatim to session-request
         #:close? [close? #f]
         #:stream? [stream? #f]
         #:method [method 'get]
         #:headers [headers (hasheq)]
         #:query [query null]
         #:auth [auth #f]
         #:data [data #f]
         #:timeouts [timeouts (make-timeout-config)]
         #:form [form #f]
         #:json [json #f]
         ;; unique to session-request-with-handlers
         #:handler [resp-handler-provider #f]
         #:catch-all [catch-all #f]
         #:debug? [debug? #f])
  (call/retry
   (retryer-compose (cycle-retryer (sleep-exponential-retryer (milliseconds 500)) 3)
                    (retryer #:should-retry? (λ (r n)
                                               (when (> n 0) (printf "retrying for the ~a~a time~n" n (case (modulo n 10)
                                                                                                            [(1) "st"]
                                                                                                            [(2) "nd"]
                                                                                                            [(3) "rd"]
                                                                                                            [else "th"])))
                                               (or (exn:fail:network? r) (exn:retry? r)))))
   (λ ()
     (let* ([resp (cond [form
                         (when debug?
                           (print-request method path headers query
                                          ;; p : (Symbol, Option String)
                                          (string-join (map (λ (p) (format "~a: ~a" (car p) (or (cdr p) ""))) form) "\n")))
                         (session-request
                               sess path #:form form
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:timeouts timeouts)]
                        [json
                         (when debug?
                           (print-request method path headers query
                                          (jsexpr->string json)))
                         (session-request
                               sess path #:json json
                               #:close? close? #:stream? stream?
                               #:method method #:headers headers
                               #:params query  #:auth auth
                               #:timeouts timeouts)]
                        [data
                         (if debug?
                             (if (input-port? data)
                                 ;; TODO: don't convert to string; duplicate input port non-strictly
                                 ;; is it possible to duplicate an input port or revert it to the state that it was in before we read from it?
                                 (let ([ds (port->string data)])
                                   (print-request method path headers query ds)
                                   (session-request
                                    sess path #:data ds
                                    #:close? close? #:stream? stream?
                                    #:method method #:headers headers
                                    #:params query  #:auth auth
                                    #:timeouts timeouts))
                                 (begin
                                   (print-request method path headers query
                                                  (cond [(not data) ""]
                                                        [(procedure? data) "<PAYLOAD PROCEDURE PRINTING UNSUPPORTED>"]
                                                        [(bytes? data) (bytes->string/utf-8 data)]
                                                        [else data]))
                                   (session-request
                                    sess path #:data data
                                    #:close? close? #:stream? stream?
                                    #:method method #:headers headers
                                    #:params query  #:auth auth
                                    #:timeouts timeouts)))
                             (session-request
                              sess path #:data data
                              #:close? close? #:stream? stream?
                              #:method method #:headers headers
                              #:params query  #:auth auth
                              #:timeouts timeouts))]
                        [else
                         (when debug?
                           (print-request method path headers query ""))
                         (session-request
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
       (when debug? (print-response resp))
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

#;(: http-json-api (∀ (a) (-> Session (U Bytes String URL)
                           (-> JSExpr a) ; better than http-json-api returning (U JSExpr a), which ideally would be (Either a JSExpr) if racket supported ADTs. if we'd had Either, we could use ArrowChoice operators or treat it as a bifunctor or monad. with a mere untagged union, it may be difficult to distinguish between a correct result or a failure result, especially if they're the same type! therefore we use this function, which is effectively fmap that can be defined only before an item is put into an Either. we're basically making fmap (or `right`) mandatory then passing to (***).
                            [#:close? Boolean]
                            [#:method HTTP-Method]
                            [#:headers Headers]
                            [#:query Query-Params]
                            [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
                            [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
                            [#:timeouts Timeout-Config]
                            [#:form (Option (Listof (Pairof Symbol (Option String))))]
                            [#:json JSExpr]
                            ;; unique to session-request-with-handlers & derived functions
                            [#:handler (Option (U (RespHandler a) (HashTable Exact-Positive-Integer (RespHandler a))))]
                            [#:catch-all (Option (CatchAllHandler a))]
                            ;; unique only to http-json-api
                            [#:on-empty-resp (Option a)] ; what to return if the response body empty, i.e. there's no json to parse. if #f, then assumes that response isn't empty. if it *is* empty, then the (-> JSExpr a) function is applied to an empty hash map.
                            [#:debug? Boolean]
                            a)))
(define (http-json-api sess path on-success
                       #:close? [close? #f]
                       #:method [method 'get]
                       #:headers [headers (hasheq)]
                       #:query [query null]
                       #:auth [auth #f]
                       #:data [data #f]
                       #:timeouts [timeouts (make-timeout-config)]
                       #:form [form #f]
                       #:json [json #f]
                       #:handler [resp-handler-provider #f]
                       #:catch-all [catch-all #f]
                       #:on-empty-resp [on-empty-resp #f]
                       #:debug? [debug? #f])
  (session-request-with-handlers
   sess path (λ (resp)
               (let ([j (response-json resp)])
                 (if (eof-object? j)
                     (or on-empty-resp (on-success (hash)))
                     (on-success j))))
   #:close? close?
   #:method method
   #:headers (hash-set headers 'Accept "application/json")
   #:query query
   #:auth auth
   #:data data
   #:timeouts timeouts
   #:form form
   #:json json
   #:handler resp-handler-provider
   #:catch-all catch-all
   #:debug? debug?))

;; often used with open-input-bytes
#;(: session-request-stream-bytes
   (∀ (a) (-> Session (U Bytes String URL)
             (-> Input-Port a) ; see note about Either in http-json-api
              [#:close? Boolean]
              [#:method HTTP-Method]
              [#:headers Headers]
              [#:query Query-Params]
              [#:auth (Option (-> URL Headers Query-Params (Values Headers Query-Params)))]
              [#:data (U False Bytes String Input-Port (-> Headers (Values Headers (U Bytes String Input-Port))))]
              [#:timeouts Timeout-Config]
              [#:form (Option (Listof (Pairof Symbol (Option String))))]
              [#:json JSExpr]
              ;; unique to session-request-with-handlers & derived functions
              [#:handler (Option (U (RespHandler a) (HashTable Exact-Positive-Integer (RespHandler a))))]
              [#:catch-all (Option (CatchAllHandler a))]
              [#:debug? Boolean]
              a)))
(define (session-request-stream-bytes
         sess path on-success
         #:close? [close? #f]
         #:method [method 'get]
         #:headers [headers (hasheq)]
         #:query [query null]
         #:auth [auth #f]
         #:data [data #f]
         #:timeouts [timeouts (make-timeout-config)]
         #:form [form #f]
         #:json [json #f]
         #:handler [resp-handler-provider #f]
         #:catch-all [catch-all #f]
         #:debug? [debug? #f])
  (session-request-with-handlers
   sess path (compose1 on-success response-output)
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
   #:catch-all catch-all
   #:debug? debug?))

;; helper macro useful in defining api wrapper functions
;; retry should be a continuation bound by let loop
(define-syntax-rule (api-handler retry base-handler)
  (λ (r)
    (let ([sc (response-status-code r)])
      (if (= 401 sc)
          (retry)
          (if base-handler
              (if (hash? base-handler)
                  ((hash-ref base-handler sc) r)
                  (base-handler r))
              'defer-to-catch-all)))))

;; accept http requests until a terminal one is found, at which point
;; the server dies and a value is returned.
;; how to generate a self-signed cert & private key (which works
;; with firefox but not chromium) as given by §6.3 web-server's docs:
;; openssl genrsa -des3 -out pk.pem 1024
;; openssl rsa -in pk.pem -out pk.pem
;; chmod 400 pk.pem
;; openssl req -new -x509 -nodes -sha1 -days 365 -key pk.pem > cert.pem
;; ->resp/val is (-> request? (values response? any))
(define (serve-until port ->resp/val #:cert [cert #f] #:pk [pk #f])
  (define outval #f)
  (serve/launch/wait #:port port
                     #:ssl-cert cert
                     #:ssl-key pk
                     (λ (sem)
                         (λ (conn req)
                           (let*-values ([(o) (connection-o-port conn)]
                                         [(resp v) (->resp/val req)])
                             (display resp o) (flush-output o)
                             (when v (semaphore-post sem) (set! outval v))))))
  outval)
