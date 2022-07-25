#lang racket/base

;;; json accessors and constructors, and hashtable utils
;; NOTE: check-out the argo package for validating, pretty-printing, compressing/minifying, and extracting subexprs from json.

(provide (combine-out (all-defined-out) (all-from-out json)))

(require (only-in racket/bool symbol=?)
         (only-in srfi/1 list-index)
         (only-in racket/function const identity curry)
         (only-in racket/string string-split string-trim)
         json
         srfi/2
         (only-in racket/set set-member?)
         (only-in racket/format ~a)
         (only-in racket/list drop)
         (only-in "util.rkt" pretty-table-str)
         (only-in "jul.rkt" massoc)
         (only-in racket/list flatten)
         (only-in racket/match match)
         (only-in racket/function identity)
         racket/contract ;; why need to import even if not invoking json-struct with contracts in this module?
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax/stx stx-null?)
                     (only-in racket/syntax with-syntax* syntax-local-eval format-id)
                     (only-in racket/function negate)
                     srfi/2
                     (only-in racket/bool symbol=?)
                     (only-in racket/list splitf-at)
                     (only-in racket/match match)
                     (only-in racket/list take filter-map)
                     (only-in racket/function identity)))

#| use mhash-ref instead of get-prop-val unless you need multiple accessor chains:
   trying each chain in parameter order until one matches (or 'fail of none matches).
   in other words, you can consider a jsexpr as being one of multiple shapes
   and still access its nested properties.

   for example, suppose my-json represents [{"k1":{"k2":{"k3":"my-str"}}}];
   then (get-prop-val my-json (list car 'k1 'k2 'k3)) returns "my-str".
   this is where mhash-ref is simply better.

   now suppose that my-json is randomly chosen to be either ["erina", "ribbon"]
   or {"food":"carrot"}. then (get-prop-val my-json `(,car) '(food))
   would return either "erina" or "carrot".

   * if a list operation is tried to be applied to something that isn't a list, or a key is given where the corresponding object isn't a hash table, then get-prop-val returns 'fail
   * returns 'fail if 1) your accessors don't match the shape of the data; or 2) you're absurd enough to not specify any accessors
   * unsafe list operations like car or cdr can still raise exceptions!
|#
#;(: get-prop-val (-> JSExpr
                    (Listof (U Symbol (-> JSList (U JSExpr JSHash)))) *
                    (U 'fail JSExpr)))
(define (get-prop-val o . ass)
  (let outer ([t 'fail] [ass ass])
    (if (or (jsexpr? t) (null? ass)) ;; we want only one accessor to match; we only quit if we exhaust all accessors
        t
        (outer (let inner ([t o] [as (car ass)])
                      ;; we want all elements of the accessor as to match. if any one fails, then quit this loop. because (not (jsexpr? 'fail)), it's a suitable "failed lookup" value.
                      ;; symbol? herein means (equal? 'fail)
                      (if (or (null? as) (symbol? t))
                          t
                          (let ([a (car as)])
                            (inner (if (hash? t) ;; nested if's rather than cond b/c want occurence typing
                                       (if (symbol? a)
                                           (hash-ref t a (λ () 'fail))
                                           'fail)
                                       (if (list? t)
                                           (if (procedure? a) (a t) 'fail)
                                           'fail))
                                   (cdr as)))))
               (cdr ass))))) ;; discard this accessor; the next one will be tried on the next outer iteration, if there is any

#| extract certain values from a list or hash table whose keys are symbols, e.g.
(define-values (u v w x y z uh-oh)
  (mhash-ref (hash
              'a "sports"
              'b 6
              'c "cat"
              'd 23
              'e (list (hash 'junk 0
                             'k1 (hash 'k2 90
                                       'extra-key-b/c-why-not 'null))
                       1
                       0.452))
             ['a + ""]
             'b ;; no otherwise value given; if lookp fails, exception is raised
             ['c + #f] ;; if lookup fails, then c = #f
             'd
             ['e car 'k1 'k3 + 0] ;; assume that value at key e is a list. in its head, extract from nested hash tables.
             ['e car 'k1 'k2] ;; no "otherwise" value; if match fails, then program crashes
             car)) ;; using function accessor instead of symbol accessor. here, causes runtime exception b/c car fails on the hash table.
u ;; "sports"
v ;; 6
w ;; 'cat
x ;; 23
y ;; 0
z ;; 90
;; uh-oh crashes program

* -> fn is applied to found or default value e.g. [car -> add1] to get the first item of a list, then add1 to it before returning it
* order of + & -> matters. if + def then -> fn, then fn is applied to found value or def; if -> fn then + def, then fn is applied to a value if found, else def is returned.
* accessors with depth of 1 do not need to be enclosed in a list, as in the case of 'b, 'd, and car above.
* returns (values ...) to ensure that number of arguments exactly equals number of returned keys
* assumes whether the jsexpr is a list or hash based on what the first accessor implies, since all accessors must be of the same type i.e. if first accessor is a function, then all accessors must be functions, and the accessed item must be a list, since hash tables can be referenced only by keys. congruently if the first accessor is a symbol, then all accessors must be symbols and the accessed must be a hash table with symbol keys.
* assumes that keys are symbols, as is the case with jsexpr?'s
* if only one accessor is given, then returns a single value bindable in an ordinary let block (cf let-values)
* mhash-ref expands to get-prop-val, so import get-prop-val if you use mhash-ref

syntax: (mhash-ref app ht/list accessor ...+) where

app =
    | -> f ; f is a form to apply to the extracted attributes. defaults to #'values.

accessor = lookup-pat
         | [lookup-pat ...+ else map]

lookup-pat of (or/c symbol? (-> list? (or/c jsexpr? hash?)))

else =
     | + any/c ; if lookup fails, use def. + is a literal

map =
    | -> (-> any/c any/c) ; supply the value found from maybe-type-def to a function; that output is returned. -> is a literal

|#
(define-syntax (mhash-ref stx)
  (syntax-parse stx
    [(_ ht/list (~optional (~seq (~datum ->) collect) #:defaults ([collect #'values])) lookup-pat ...+)
     #:with (v ...) (map (λ (s) (syntax-parse s
                                  [(~and k ((~literal quote) _)) #'(hash-ref m k)]
                                  [((~and k (~not (~or* (~datum +) (~datum ->)))) ...+
                                    (~alt (~optional (~seq (~datum +) def) #:defaults ([def #'(error (format "mhash-ref: ~s failed lookup" (syntax->datum #'(k ...))))]))
                                          (~optional (~seq (~datum ->) fn) #:defaults ([fn #'identity]))) ...)
                                   #`(let ([r (get-prop-val m (list k ...))])
                                       #,(if (equal? '(+ ->) (map syntax->datum (filter (λ (s) (or (equal? '+ (syntax-e s)) (equal? '-> (syntax-e s)))) (syntax-e s))))
                                             #'(fn (if (equal? r 'fail) def r))
                                             #'(if (equal? r 'fail) def (fn r))))]
                                  [fn #'(fn m)]))
                         (attribute lookup-pat))
     #'(let ([m ht/list]) (collect v ...))]))

;;; json-struct

#| reduce multiple optional syntaxes into one. accepts (listof (or/c syntax? false?)) or syntax thereof
   where the list should be null or singleton (or 1 or 2 if #:default is provided.)
   NOTE: in the following templates, the ellipses are literal; <...> and the underscores aren't.
   useful as #:with ((~optional _)) (opt+ #'((~? var1) (~? var2) <...>) src opt <...>)
   or        #:with (((~optional _)) ...) (datum->syntax #'((~? _src-xs _dummy) ...)
                                                         (map (λ (s x) (opt+ s x opt <...>))
                                                              (syntax-e #'(((~? var1) (~? var2) <...>) ...))
                                                              (attribute _src-xs)))
   etc, depending on ellipsis depth, or if you put a syntax at the end of the list (serving as a default value,) then
   #:with ((_) ...) (datum->syntax _ (map (λ (s f) (opt+ s f opt <...> #:default? #t))       ; apply opt+ to each list of syntax options
                                          (syntax-e #'(((~? var1) (~? var2) <...> DEF) ...)) ; list of lists of syntax options
                                          (attribute _src-xs)))
   we need to use such odd syntax to deal with #:with (~optional _) _ being illegal.
|#
(define-for-syntax (opt+ stx src #:default? [def? #f] . opts)
  (let* ([xs (filter syntax? (if (syntax? stx) (syntax-e stx) stx))]
         [l (length xs)])
    (cond [(= l 0) (if def?
                       (raise-argument-error 'opt+ "non-empty syntax list (b/c #:default? #t was given)" stx)
                       #'())]
          [(= l 1) (list (car xs))]
          [(and (= l 2) def?) (list (car xs))] ; one given argument plus the default; use the given argument
          [else (raise-syntax-error #f (format "only one of ~a allowed" opts) src)])))

#| simultaneously define a struct and functions to parse or export it from or to a json object.
syntax: (json-struct struct-name (field ...) struct-opts) where
  field = [id opt-contract key import export]
  opt-contract =
               | contract?
  key = ; if omitted, assume field name as key when exporting or importing to or from json objects
      | #:as symbol? ; key as appears in json
  import = ; if omitted, then the value is read as-is from json object at appropriate key
         | #:parse (-> (or/c 'not-found jsexpr?) any/c)
         | #:parse-maybe (-> jsexpr? any/c) ; if not found, then #f; otherwise apply parsing function. almost always used with #:export-if-truthy.
         | #:unsafe-parse (-> jsexpr? any/c) ; throws "missing key" error if key not present; else parses.
         | #:or any/c ; if lookup fails, use this value.
         | #:const any/c ; doesn't even bother with lookup; just assume this value
  export = ; if omitted, then value is exported as-is from the structure at appropriate key
         | #:export (or/c #f (-> any/c (or/c 'no-export jsexpr?))) ; if #f, don't export; otherwise use export fn.
         | #:export-if-truthy (-> any/c jsexpr?) ; if truthy, apply function then export. if fn is omitted, assume identity
  struct-opts =
              | #:field->key (-> string? string?) ; function to derive json object key from struct field name
              | #:to-js (or/c #f id)   ; manually specify struct to jsexpr function name. if #f is given, then an export function won't be defined
              | #:from-js (or/c #f id) ; like #:export but for jsexpr-to-struct function.
              | other ; passed verbatim to `struct`

NOTE: result of json export function only satisfies hash?; it does not necessarily satisfy jsexpr?!

* `struct/contract` will be used instead of `struct` if any field has a contract. beware that struct/contract accepts a subset of struct's kwargs, and that fileds missing contracts will assume any/c

TIP: you may want to parse an object of unknown shape. suppose it could be one of two shapes, S1 & S2. then we can try to parse it as an S1 and, failing that, try parsing it as an S2, raising an exception if that fails:

(require (only-in racket/exn exn->string))
(define (jsexpr->s1-or-s2 j)
  (with-handlers ([(λ (e) (and (exn:fail:contract? e)
                               (string-contains? (exn->string e) "hash-ref")))
                   (const (jsexpr->s2 j))])
    (jsexpr->s1 j)))

it's a bit hacky, but it's dependable. TODO: there should be an option so that, if parse fails, then #f is returned instead of a struct.
|#
(define-syntax (json-struct stx)
  (syntax-parse stx
   [(_ name:id
     ((~or* field:id
            (~and field-tuple
                  ;; field options are depth 1
                  [field:id (~optional (~and (~not contract:keyword) contract))
                   (~alt (~optional (~seq #:as key))
                         ;; import clauses (select 0~1)
                         (~optional (~seq #:parse parse))
                         (~optional (~seq #:parse-maybe parse-maybe))
                         (~optional (~seq #:unsafe-parse unsafe-parse))
                         (~optional (~seq #:or otherwise))
                         (~optional (~seq #:const const))
                         ;; export clauses (select 0~1)
                         (~optional (~seq #:export export))
                         (~seq #:export-if-truthy (~optional export-if-truthy))) ; optional and takes an optional argument
                   ...])) ...)
              ;; struct opts have depth 0
              (~alt (~optional (~seq #:field->key field->key) #:defaults ([field->key #'identity]))
                    (~optional (~seq #:to-js to-js))
                    (~optional (~seq #:from-js from-js))
                    ;; #:constructor-name needed in parsing fn
                    (~optional (~seq #:constructor-name constructor))
                    ;; passthrough to struct
                    other) ...)
    #:with (kw-constructor ...) (if (attribute constructor) #'(#:constructor-name constructor) #'())
    #:with defstruct (if (ormap identity (attribute contract)) #'struct/contract #'struct)
    #:with fields (if (ormap identity (attribute contract)) #'([field (~? contract any/c)] ...) #'(field ...))
    (let ([import-fn (cond [(not (attribute from-js)) (format-id #'name "jsexpr->~a" (syntax-e #'name))]
                           [(syntax-e #'from-js) #'from-js]
                           [else #f])]
          [export-fn (cond [(not (attribute to-js)) (format-id #'name "~a->jsexpr" (syntax-e #'name))]
                           [(syntax-e #'to-js) #'to-js]
                           [else #f])])
      (with-syntax* ([(k ...) (map (λ (k s) (or k (datum->syntax s #`(quote #,(syntax-local-eval s)))))
                                    (attribute key)
                                    (syntax-e #'((string->symbol (field->key (symbol->string (quote field)))) ...)))]
                     ;; import & exprort exprs' optionalities are as singleton vs empty lists rather than ~optional b/c wanna continue with-syntax*
                     ;; rather than start new syntax-parse.
                     [(import-expr ...)
                      (if import-fn
                          (with-syntax ([(((it hash-else)) ...)
                                         (datum->syntax #'((~? field-tuple #'dummy) ...)
                                                        (map (λ (parse unsafe-parse parse-maybe otherwise f k loc)
                                                               (let ([uhr #`(lookup-err (quote #,f) #,k)])
                                                                 (opt+ (list (and parse #`(#,parse 'not-found))
                                                                             (and unsafe-parse #`(#,unsafe-parse #,uhr))
                                                                             (and parse-maybe #`((λ (x) (and x (#,parse-maybe x))) #f))
                                                                             (and otherwise #`(identity (λ () #,otherwise)))
                                                                             #`(identity #,uhr))
                                                                       loc
                                                                       '#:parse '#:parse-maybe '#:unsafe-parse '#:or '#:const #:default? #t)))
                                                             (attribute parse)
                                                             (attribute unsafe-parse)
                                                             (attribute parse-maybe)
                                                             (attribute otherwise)
                                                             (attribute field)
                                                             (syntax-e #'(k ...))
                                                             (attribute field-tuple)))])
                            #`((define (#,import-fn j)
                                 (let ([lookup-err (λ (f K) (error (quote #,import-fn)
                                                                (format "failed to parse field ~a of struct ~a from the following JSON because it's missing the '~s key: ~a"
                                                                        f (quote name) K (jsexpr->string j))))])
                                   ((~? constructor name) (~? const (it (hash-ref j k hash-else))) ...)))))
                          #'())]
                      [(export-expr ...) (if export-fn
                                           ;; (equal? _ 'no-export) must be done at runtime, since 'no-export is gotten only after
                                           ;; applying export transform.
                                           ;; however, we know to omit from export if #:export #f
                                            (with-syntax* ([((v) ...)
                                                            (datum->syntax #'((~? field-tuple #'dummy) ...)
                                                                           (map (λ (eit e field-accessor loc)
                                                                                  ;; nullary eit exports even if #f
                                                                                  (opt+ (list (and eit
                                                                                                   (not (null? eit))
                                                                                                   #`(let ([v (#,field-accessor o)])
                                                                                                       (if v
                                                                                                           (#,(if (car eit) (car eit) #'identity) v)
                                                                                                           'no-export)))
                                                                                              (and e (if (syntax-e e) #`(#,e (#,field-accessor o)) (syntax 'no-export)))
                                                                                              #`(real->jsexpr (#,field-accessor o)))
                                                                                        loc
                                                                                        '#:export '#:export-if-truthy #:default? #t))
                                                                                (attribute export-if-truthy)
                                                                                (attribute export)
                                                                                (map (λ (f) (and (format-id f "~a-~a" #'name f))) (attribute field))
                                                                                (attribute field-tuple)))])
                                              #`((define (#,export-fn o) (for/hash ([ek (list k ...)] [ev (list v ...)]
                                                                                   #:when (not (equal? 'no-export ev)))
                                                                                  (values ek ev)))))
                                              #'())])
        #'(begin (defstruct name fields other ... kw-constructor ...) import-expr ... export-expr ...)))]))

;; ->js & js->: substantives with attributes or attributes distributed over substantives.
;; interperable as substantives each with a stack of fns & params at least one of whose args
;; is the substantive.

;; alist may lack items listed in spec; no error is raised about missing attributes.
;; attributes present in the alist but not in the spec are ignored.
;; both of these behaviors are natural in ->js' definition, and are neither un/intentional.
(define ((->js spec spec-id [field->key identity]) alist)
  (for/fold ([r (hash)]) ([s spec])
           ;; normalize shape
    (match (match s [(list '+ attrs ids) `(,attrs . ,ids)]
                    [(cons id attrs)     `(,attrs . (,id))]
                    [else s])
           [(cons attrs ids) (for/fold ([r r]) ([id ids])
                               (let ([k (string->symbol (field->key (symbol->string id)))])
                                 (let R ([attrs attrs])
                                   (if (null? attrs)
                                       (hash-set r k (real->jsexpr (massoc id alist))) ; default export
                                       (case (car attrs)
                                         [(sub) (let ([v (massoc id alist)])
                                                  (if (pair? v)
                                                      (hash-set r k ((->js (cadr attrs) id field->key) v))
                                                      (error (format "~a should be a spec; given ~s" id v))))]
                                         [(export) (if (pair? (cdr attrs))
                                                       (hash-set r k ((cadr attrs) (massoc id alist)))
                                                       (error (format "missing required export fn arg for ~a in spec ~a" id spec-id)))]
                                         ;; e.i.t. takes an optional arg, so it must come last
                                         [(export-if-truthy) (let ([v (massoc id alist #f)])
                                                               (if v
                                                                   (hash-set r k (if (null? (cdr attrs)) v ((cadr attrs) v)))
                                                                   r))]
                                         ;; generally we'd case number to drop on (car attrs)
                                         [else (R (drop attrs 2))])))))]
           [id (hash-set r (string->symbol (field->key (symbol->string id))) (massoc id alist))])))

;; spec's order is preserved.
(define ((js-> spec spec-id [field->key identity]) j)
  (let ([err (λ (id k)
               (λ _ (error (format "failed to parse field ~a of spec ~a from the following JSON because it's missing the '~s key: ~a"
                                   id spec-id k (jsexpr->string j)))))])
    (for/foldr ([r '()]) ([s spec])
             ;; normalize shape
      (match (match s [(list '+ attrs ids) `(,attrs . ,ids)]
                      [(cons id attrs)     `(,attrs . (,id))]
                      [else s])
             [(cons attrs ids) (for/foldr ([r r]) ([id ids])
                                 (let ([k (string->symbol (field->key (symbol->string id)))])
                                      (let R ([attrs attrs])
                                        (if (null? attrs)
                                            `((,id . ,(hash-ref j k (err id k))) . ,r) ; default import
                                            (if (null? (cdr attrs))
                                                (R '()) ; (error (format "missing required arg to spec attribute ~a for ~a in spec ~a" (car attrs) id spec-id))
                                                (case (car attrs)
                                                  [(const) `((,id . ,(cadr attrs)) . ,r)]
                                                  [(or) `((,id . ,(hash-ref j k (cadr attrs))) . ,r)]
                                                  [(sub) (let ([v (hash-ref j k (err id k))])
                                                           (if (hash? v)
                                                               `((,id . ,((js-> (cadr attrs) id field->key) v)) . ,r)
                                                               (error (format "~a should be a spec; given ~a" id v))))]
                                                  [(unsafe-parse) `((,id . ,((cadr attrs) (hash-ref j k (err id k)))) . ,r)]
                                                  [(parse-maybe) (let ([x (hash-ref j k 'not-found)])
                                                                   `((,id . ,(if (equal? x 'not-found)
                                                                                 #f
                                                                                 ((cadr attrs) x)))
                                                                     . ,r))]
                                                  [(parse) `((,id . ,((cadr attrs) (hash-ref j k 'not-found))) . ,r)]
                                                  [else (R (drop attrs 2))]))))))]
             [id `((,id . ,(let ([k (string->symbol (field->key (symbol->string id)))]) (hash-ref j k (err id k)))) . ,r)]))))

;; TODO: like Validation applicative functor, collect all failed lookup keys into one error message. nested keys will be separated by dots just like in js, e.g. prop.subprop.key
;; rather than syntax & macros, de/aljs defines in terms of a/lists.
;; a/lists here are data structures supporting traversals i.e. they
;; encode actionable ideas. #stopsyntax. another example is traversing
;; an alist instead of using the `cond` macro.
;; -----------------------------------------------------------------------
;; de/aljs is itself a macro because, as an arbitrary constraint of
;; racket, dynamic defines can be done only through macros.
;; the "code" way to define dynamically is to store in a global hashtable;
;; however, racket, by its choice to scope within each module, does not
;; support global vars.
(define-syntax (de/aljs stx)
  (syntax-parse stx
    [(_ spec-id import-id export-id (~optional field->key) spec-obj) ; for each of import & export: if #t then use defult name; if #f then don't define. if identifier then bind to that.
     ;; defines a fn as #f if not implemented b/c who cares if it does instead of not defining anything?
     (let ([import-id (case (syntax-e #'import-id)
                       [(#t) (format-id #'spec-id "jsexpr->~a" (syntax-e #'spec-id))]
                       [(#f) #f]
                       [else #'import-id])]
           [export-id (case (syntax-e #'export-id)
                       [(#t) (format-id #'spec-id "~a->jsexpr" (syntax-e #'spec-id))]
                       [(#f) #f]
                       [else #'export-id])])
     #`(begin (define spec-id spec-obj)
              #,@(if import-id #`((define #,import-id (js-> spec-id (quote #,(syntax-e #'spec-id)) (~? field->key)))) '())
              #,@(if export-id #`((define #,export-id (->js spec-id (quote #,(syntax-e #'spec-id)) (~? field->key)))) '())))]))

;; TODO: rename. it's now a misnomer; it really ensures that reals are jsexprs.
;; needed for e.g. (write-json `(1 2 ,(/ 5 3))), which fails b/c 5/3 isn't a legal json value.
;; however, (write-json (map real->jsexpr `(1 2 ,(/ 5 3)))) succeeds by converting 5/3 to a float,
;; leaving 1 & 2 as they are, since exact integers are valid json already.
(define (real->jsexpr r) (if (and (number? r) (exact? r)) (exact->inexact r) r))

;; e.g. (for ([j (in-jslist some-input-port)]) (f j))
;; closes the input port when done reading the sequence
;; (: in-jslist (-> Input-Port (Sequenceof JSExpr)))
(define (in-jslist in)
  (make-do-sequence
   (λ ()
     (let ([stop? #f])
       (values (λ (in)
                 (regexp-match #px"[],[:space:]]*" in) ;; discard bytes before jsexpr to read. #\] is in
                 ;; there so that, after reading the last element,
                 ;; (read-json in) will return #<eof> instead of
                 ;; raising a read error.
                 (let ([v (read-json in)])
                   (if (eof-object? v)
                       (begin (set! stop? #t) 'null) ;; this value will be discarded from the sequence anyway
                       v)))
               identity
               (begin (read-char in) in)
               #f
               (λ (v)
                 (if stop?
                     (begin (close-input-port in) #f)
                     #t))
               #f)))))

;; e.g. (for ([(k v) (in-jshash some-input-port)]) (f k v))
;; closes the input port when done reading the sequence
;; (: in-jshash (-> Input-Port (Sequenceof Symbol JSExpr)))
(define (in-jshash in)
  (let ([stop-sym (gensym)])
    (make-do-sequence
     (λ ()
       (values (λ (in)
                 (let ([k (and-let* ([res1 (regexp-match #px"[^\"]*\"(.*?)\"[[:space:]]*:" in)]
                                     [res2 (cadr res1)])
                                    (string->symbol (bytes->string/utf-8 res2)))]
                       [v (read-json in)])
                   (if (or (not k) (eof-object? v))
                       (values stop-sym 0)
                       (values k v))))
               identity
               in
               #f
               (λ (k v)
                 (if (symbol=? stop-sym k)
                     (begin (close-input-port in) #f)
                     #t))
               #f)))))

;; jsmap is now just map
;; (: jsmap (∀ (t) (-> (-> JSExpr t) (-> JSExpr (Listof t)))))
;; (define (jsmap f) (λ ([xs : JSExpr]) (map f (cast xs JSList))))

;; very common as parameter to #:field->key
;; also discards trailing question marks, as is common in racket struct boolean fields,
;; e.g. [has-widget? : Boolean] is camelCase hasWidget
;; (: kabob-case->camelCase (-> String String))
(define (kabob-case->camelCase s)
  (let ([ss (string-split s "-")])
    (string-trim (apply string-append
                        (cons (car ss)
                              (map string-titlecase (cdr ss))))
                 "?" #:left? #f #:right? #t)))

;;; hash tables

;; recursive hash-union
;; for proper parametricity (namely over k & v), we'd need a predicate that checks for (HashTable k v). hash? only guarantees that a map is HashTableTop. unfortunately i think this currently cannot be checked by the type system; we'd need to check that v does not also include HashTableTop as one of its union elements! thus definitions are reified v = Any.
#;(: hash-merge (-> (HashTable Any Any) (HashTable Any Any) ;; (PropMap k v) (PropMap k v)
                  ;; k v v -> (Option (Pairof k v))
                  ;; function from key, original value, and new value, to new key & new value
                  [#:combine (Option (-> Any Any Any (Option (Pairof Any Any))))]
                  (HashTable Any Any))) ;; (PropMap k v)
(define (hash-merge d0 d1 #:combine [combine #f])
  (for/fold ([nd d0]) ;; nd : (PropMap k v)
            ([(k v) (in-hash d1)])
    (let ([nd@k (hash-ref nd k (const 'fail))]) ;; nd@k : (U 'fail v). we assume that (∩ 'fail v) is Nothing
      (cond [(and (hash? nd@k) (hash? v)) ;; if we ever get the refinement typing that we need, then these two predicates will each need to be with in an if form, to get proper refinement
             (hash-set nd k (hash-merge nd@k v #:combine combine))]
            ;; we combine only if the key was found
            [(and combine (not (equal? 'fail nd@k)))
             (let ([cx (combine k nd@k v)])
               (if cx
                   (hash-set nd (car cx) (cdr cx))
                   (hash-remove nd k)))]
            [else (hash-set nd k v)]))))

;; filter a hash map to a list of keys with optional key and value transforms
#;(: filter-hash-map (∀ (k v)
                      (-> (Listof k)
                          (HashTable k v)
                          (#:kv-transform (Option (-> k v (Values k v))))
                          (HashTable k v))))
(define (filter-hash-map keys m #:kv-transform [transform #f])
  (foldl (λ (k filtered-map)
           (if transform
               (let-values ([(nk nv) (transform k (hash-ref m k))])
                 (hash-set filtered-map nk nv))
               (hash-set filtered-map k (hash-ref m k))))
         (hash) keys))

;; convert a json to a list of lists, i.e. a table
;; to output this table as a csv, i recommend the csv-writing package
#;(: property-map->table
   (-> String ;; header for the column of keys from the input hashmap
      (Option (Listof Symbol)) ;; list of keys to include. if #f, gets key list from first object in provided hashmap
      (HashTable Symbol JSHash) ;; hashmap to convert
      (Listof (Listof String))))
(define (property-map->table key-field fields j)
  (or (and-let* ([first-pos (hash-iterate-first j)]
                 [fields (sort (or fields (hash-keys (hash-iterate-value j first-pos))) symbol<?)])
                ;; each row (except the header) is a list of (key-field field1 ... fieldn)
               (cons (cons key-field (map symbol->string fields))
                      (map (λ (kv)
                             (cons (symbol->string (car kv)) (map (λ (f) (~a (hash-ref (cdr kv) f))) fields)))
                           (hash->list j))))
      null))

;; convenience function: composes pretty-table-str & property-map->table
#;(: property-map->pretty-table-str
   (-> String
       (Option (Listof Symbol))
       (HashTable Symbol JSHash)
       [#:col-widths (Option (Listof Nonnegative-Fixnum))]
       String))
(define (property-map->pretty-table-str key-field fields j #:col-widths [col-widths #f])
  (let ([x (property-map->table key-field fields j)])
    (if (null? x)
        "<empty table>"
        (pretty-table-str x #:col-widths col-widths))))

#| assumes that all rows have the same number of fields
   key-field is the column string whose column comprises the resultant map's keys
   the first row of rows must be a header!!
   to parse tabular input, i suggest the csv-reading package
   NOTE: if a cell's string value can't be parsed into a json object, it'll be taken raw as a string.

Example Usage:
(table->json "name"
             (set "age")
             '(("age" "name"  "hobby")
               ("20"  "jerry" "dancing")
               ("45"  "tom"   "grinding")
               ("72"  "louis" "bootyhounding")))
=> '#hash((jerry . #hash((age . 20)))
          (louis . #hash((age . 72)))
          (tom   . #hash((age . 45))))
|#
#;(: table->json (-> String (Option (Setof String)) (Listof (Listof String)) (HashTable Symbol JSHash)))
(define (table->json key-field fields rows)
  (let* ([header (car rows)]
         [rows   (cdr rows)]
         [key-field-index (list-index (curry string=? key-field) header)]
         ;; NOTE: indices DOES NOT INCLUDE key-field! (it may, but that's arbitrary, and unlikely.)
         [indices (if fields
                      ;; get desired fields' column indcies
                      (for/fold ([m (hash)])
                                ([f header] [i (in-naturals)])
                        (if (set-member? fields f) (hash-set m i f) m))
                      ;; index all columns: zip [0..] w/columns
                      (for/hash ([f header] [i (in-naturals)]) (values i f)))])
    (if key-field-index
        (for/hash
          ([row rows])
          (let ([p (for/fold
                     ([p (cons 'you-should-not-see-me (hash))])
                     ([field-value row] [i (in-naturals)])
                     (let ([pmap (cdr p)]
                           [field-name (hash-ref indices i #f)])
                       (cons (if (= key-field-index i)
                                 (string->symbol field-value)
                                 (car p))
                             (if (and field-name (not (string=? field-name key-field)))
                                 (hash-set pmap (string->symbol field-name) (with-handlers ([exn:fail:read? (λ (_) field-value)]) (read-json (open-input-string field-value))))
                                 pmap))))])
            (values (car p) (cdr p))))
        (error (format "table->json: specified key field \"~a\" not found" key-field)))))

(define (fold-json f j)
  (let go ([j j])
    (f j (map go (cond [(hash? j) (hash-values j)]
                       [(list? j) j]
                       [else '()])))))

;; inefficient (uses `flatten`) tree search
(define (filter/json p j) (fold-json (λ (r acc) (if (p r) r (flatten acc))) j))
