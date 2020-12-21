#lang typed/racket/base

;;; json accessors and constructors, and hashtable utils
;; NOTE: check-out the argo package for validating, pretty-printing, compressing/minifying, and extracting subexprs from json.

(provide (except-out (combine-out (all-defined-out) JSExpr)))

(require (only-in racket/bool symbol=?)
         (only-in racket/function const identity)
         typed/json
         srfi/2
         (only-in racket/string string-join string-split)
         (only-in racket/list range make-list)
         (only-in racket/set set-member?)
         (only-in racket/format ~a)
         (for-syntax racket/base srfi/2
                     (only-in racket/bool symbol=?)
                     (only-in racket/list splitf-at)
                     (only-in racket/match match)
                     (only-in racket/list take filter-map)
                     (only-in racket/function identity)
                     (only-in (submod "util.rkt" untyped) get-kwarg)))

(require/typed srfi/1 (list-index (All (a) (-> (-> a Boolean) (Listof a) (Option Index)))))

;; track defined json structs for automatic #:unsafe-parse & #:export for json-struct fields whose field type
;; is itself a type defined via json-struct
;; each maps from json-struct's type name to name of parsing or exporting function (jsexpr->... or ...->jsexpr respectively)
(define-for-syntax parse-json-structs (make-hash)) ;; (HashTable Type (U 'no-parse (-> JSExpr Type)))
(define-for-syntax export-json-structs (make-hash)) ;; (HashTable Type (U 'no-export (-> JSExpr Type)))

(: real->jsexpr (-> Real JSExpr))
(define (real->jsexpr r) (if (inexact? r) (cast r Inexact-Real) (cast r Integer)))

;; TODO: support nested accessors, e.g. ('meta ('description 'homepage)) to mean ('meta 'description) ('meta 'homepage)
;; TODO: add #:hash kwarg that, for each accessor sequence, adds a k/v pair whose key is that sequence's rightmost symbol. e.g. ('k1 'k2 ('k3 last)) would crease hash map with keys 'k1 'k2 'k3, and ('k1 'k2 ('k3 last 'k4)) would name the last accessed to 'k4 instead of 'k3, since (last (filter symbol? ('k3 last 'k4))) = 'k4
;; TODO: #:list uses `list`, which returns type (Listof a). we prefer (List ...), so that we can use with `apply`.
#| extract certain values from a JSExpr, e.g.
(define-values (u v w x y z uh-oh)
  (mhash-ref (jshash
              'a "sports"
              'b 6
              'c "cat"
              'd 23
              'e (list (hash 'junk 0
                             'k1 (hash 'k2 90
                                       'extra-key-b/c-why-not 'null))
                       1
                       0.452))
             ['a : String ""] ;; otherwise value to assume if lookup fails. a : String.
             ['b : Real] ;; no otherwise value given; if lookp fails, exception is raised
             ['c : String #f] ;; if lookup fails, then c = #f, and its type is (Option String)
             'd ;; no otherwise, no type specification; no casting is attempted
             ['e car 'k1 'k3 : Natural 0] ;; assume that value at key e is a list. in its head, extract from nested hash tables. typed w/default.
             ('e car 'k1 'k2) ;; no type spec nor otherwise value
             car)) ;; asserts the hash to a list, causing runtime exception. regardless, the syntax demonstrates that we can use function accessors just like we can use symbol accessors. i could've specified the type here, too: [car : Integer]
u ;; "sports" : String
v ;; 6 : Real
w ;; 'cat : (Option String)
x ;; 23 : JSExpr
y ;; 0 : Natural
z ;; 90 : JSExpr
;; uh-oh causes the whole program to crash

* returns (Values ...) to ensure that number of arguments exactly equals number of returned keys
* unless #:list keyword is specified. #:list may be specified anywhere in mhash-ref's arg list
  * useful for using mhash-ref with apply. #:list returns type (List ...), i.e. a list of definite length
* assumes whether the jsexpr is a list or hash based on what the first accessor implies
* assumes that keys are Symbols, as is the case in JSExpr
* if only one accessor is given, then returns a single value bindable in an ordinary let block (cf let-values)
* this macro expands to get-prop-val, so you'll need to always import get-prop-val if you import mhash-ref

== accessors formal syntax

accessor = lookup-pat
         | (lookup-pat ...+ maybe-type-def maybe-map)

lookup-pat = Symbol
           | (: fn (-> JSList (U JSExpr JSHash)))

maybe-type-def =
               | : type ;; take value as-is from JSON, but still specify/cast its type
               | : type else ;; if lookup fails, use the "else" value instead
maybe-map =
          | -> (: fn (-> type other-type)) ;; supply the value found from maybe-type-def to a function; that output is returned
|#
(define-syntax (mhash-ref stx)
  (let*-values ([(args) (cdr (syntax->list stx))]
                [(ret-list?) #f]
                [(X Y) (splitf-at args (λ (x) (not (equal? (syntax->datum #'#:list) (syntax->datum x)))))]
                [(args ret-list?) (if (null? Y) ;; provided a list
                                      (values args ret-list?)
                                      (values (append X (cdr Y)) #t))])
    (datum->syntax
     stx
     (match args
       [(list m as ..1)
        ;; m : JSExpr
        ;; eval and cast m only once. useful if m is an expr (e.g. (expensive-build-map)) rather than an identifier (e.g. built-map)
        `(let ([m ,(or (and-let* ([dla (syntax->list (car as))])
                                 (cond [(null? dla) (raise-syntax-error 'mhash-ref "empty accessor")]
                                       [(equal? (syntax->datum #'quote) (syntax->datum (car dla))) `(assert ,m hash?)]
                                       [else m]))
                       `(assert ,m list?))])
           (,(if (= 1 (length as)) #'identity (if ret-list? #'list #'values))
            ,@(map (λ (a) ;; a is syntax representing an accessor, e.g. ['k : T default]
                     (or (and-let* ([dla (syntax->list a)]) ;; if a is a symbol literal, then it's seen as the list (quote <literal>). (not dla) => (procedure? a)
                                   (cond [(null? dla)
                                          (raise-syntax-error 'mhash-ref "empty accessor")]
                                         [(equal? (syntax->datum #'quote) (syntax->datum (car dla)))
                                          `((inst hash-ref Symbol JSExpr) m ,a)] ;; ref symbol literal from m
                                         [else ;; using let*-values instead of match b/c somehow (list accessors ..1 : type def) makes accessors singleton
                                          (let*-values ([(dlad) (map syntax->datum dla)] ;; e.g. ((quote prop) car (λ ([l : JSList]) ...) : String #f)
                                                        [(accessors rst1) (splitf-at dlad (λ (x) (and (not (equal? x '->)) (not (equal? x ':)))))]
                                                        [(maybe-type-def maybe-map) (splitf-at (if (null? rst1) accessors rst1) (λ (x) (not (equal? x '->))))]
                                                        [(mtdl) (length maybe-type-def)])
                                            (if (= mtdl 1)
                                                (raise-syntax-error 'mhash-ref "either illegal singleton accessor or colon without following type. solution: either add type name after colon, or do not surround accessor in a form; leave it naked." dlad)
                                                `(let* ([x (get-prop-val m (list ,@accessors))] ;; m is not cast here when passed to get-prop-val
                                                        [y (if (equal? 'fail x)
                                                               ,(if (< mtdl 3) ;; then no "else" value was provided
                                                                    `(error (format "accessor sequence ~a failed on object ~a" (list ,@accessors) m))
                                                                    (caddr maybe-type-def)) ;; the "else" value
                                                               ,(if (< mtdl 2) ;; then no type specified
                                                                    'x
                                                                    `(cast x ,(cadr maybe-type-def))))])
                                                   ,(case (length maybe-map)
                                                      [(0) 'y]
                                                      [(1) (raise-syntax-error 'mhash-ref "-> but no function following it" dlad)]
                                                      [else `(,(cadr maybe-map) y)]))))]))
                         `(,a m))) ;; funnily, if a doesn't represent a list, then it must represent a function whose domain is a list, which implies that m is a list.
                   as)))]
       [_ (raise-syntax-error 'mhash-ref "no accessors specified" stx)]))))

(define-type JSHash (HashTable Symbol JSExpr))
(define-type JSList (Listof JSExpr))
(define jshash (inst hash Symbol JSExpr)) ;; monomorphic hash
(define jshash-empty (ann #hash() JSHash))

#| usually you'll use mhash-ref instead of get-prop-val. what get-prop-val
   supports that mhash-ref does not: multiple accessor chains. it tries
   each chain in parameter order until one matches (or 'fail of none matches).
   in other words, you can consider a jsexpr as being one of multiple shapes
   and still access its nested properties.

   for example, suppose my-json represents [{"k1":{"k2":{"k3":"my-str"}}}];
   then (get-prop-val my-json (list car 'k1 'k2 'k3)) returns "my-str".
   this is where mhash-ref is simply better.

   now suppose that my-json is randomly chosen to be either ["erina", "ribbon"]
   or {"food":"carrot"}. then (get-prop-val my-json (list car) (list 'food))
   would return either "erina" or "carrot".

   * if a list operation is tried to be applied to something that isn't a list, or a key is given where the corresponding object isn't a hash table, then get-prop-val returns 'fail
   * returns 'fail if you're absurd enough to not specify any accessors
   * unsafe list operations like car or cdr can still raise exceptions!
|#
(: get-prop-val (-> JSExpr
                    (Listof (U Symbol (-> JSList (U JSExpr JSHash)))) *
                    (U 'fail JSExpr)))
(define (get-prop-val o . ass)
  (let outer ([t : (U JSExpr 'fail) 'fail] [ass ass])
    (if (or (jsexpr? t) (null? ass)) ;; we want only one accessor to match; we only quit if we exhaust all accessors
        t
        (outer (ann (let inner ([t : (U JSExpr 'fail) o] [as (car ass)])
                      ;; we want all elements of the accessor as to match. if any one fails, then quit this loop. because Symbol (viz 'fail) is not an element of the JSExpr union, it's a suitable "failed lookup" value. that it's the specific symbol 'fail does not matter here; we can still use symbol? to refine the type while signaling that we should break loops.
                      (if (or (null? as) (symbol? t))
                          t
                          (let ([a (car as)])
                            (inner (if (hash? t) ;; nested if's rather than cond b/c want occurence typing
                                       (if (symbol? a)
                                           (hash-ref (cast t JSHash) a (λ () 'fail))
                                           'fail)
                                       (if (list? t)
                                           (if (procedure? a)
                                               (a (assert t list?)) ;; dunno why t isn't refined to list? i.e. JSList here
                                               'fail)
                                           'fail))
                                   (cdr as)))))
                    (U JSExpr 'fail))
               (cdr ass))))) ;; discard this accessor; the next one will be tried on the next outer iteration, if there is any

;; e.g. (for ([j (in-jslist some-input-port)]) (f j))
(: in-jslist (-> Input-Port (Sequenceof JSExpr)))
(define (in-jslist in)
  ((inst make-do-sequence Input-Port JSExpr)
   (λ ()
     (let ([stop? : Boolean #f])
       (values (λ ([in : Input-Port])
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
               (λ ([v : JSExpr])
                 (if stop?
                     (begin (close-input-port in) #f)
                     #t))
               #f)))))

;; e.g. (for ([(k v) (in-jshash some-input-port)]) (f k v))
(: in-jshash (-> Input-Port (Sequenceof Symbol JSExpr)))
(define (in-jshash in)
  ((inst make-do-sequence Input-Port Symbol JSExpr)
   (λ ()
     (values (λ ([in : Input-Port])
               (let ([k (and-let* ([res1 (regexp-match #px"[^\"]*\"(.*?)\"[[:space:]]*:" in)]
                                   [res2 (cadr res1)])
                                  (string->symbol (bytes->string/utf-8 res2)))]
                     [v (read-json in)])
                 (if (or (not k) (eof-object? v))
                     (values 'b74zcaCa 0)
                     (values k v))))
             identity
             in
             #f
             (λ ([k : Symbol] [v : JSExpr])
               (if (symbol=? 'b74zcaCa k)
                   (begin (close-input-port in) #f)
                   #t))
             #f))))

#| insert key/value pairs, if their associated conditions are truthy
(: hash-set/cond (->* () #:rest-star (cond k v) (Immutable-HashTable k v)))
the same key may be used multiple times; the last truthy one will be
put into the map, a la hash-set. for example, in
(hash-set/cond (hash 'a 20 'b "yes")
               #t 'b "canary"
               #t 'b 452)
b is 452.
|#
(define-syntax (hash-set/cond stx)
  (let* ([args (cdr (syntax->datum stx))]
         [hash-map (car args)]
         [rst (cdr args)])
    (datum->syntax
     stx
     (if (null? rst) ;; base case
         hash-map
         (let ([cond (car rst)]
               [k (cadr rst)]
               [v (caddr rst)]
               [new-rst (cdddr rst)])
           `(hash-set/cond
             (if ,cond
                 (hash-set ,hash-map ,k ,v)
                 ,hash-map)
             ,@new-rst))))))

;; TODO: à la Validation applicative functor, collect all failed lookup keys into one error message. nested keys will be separated by dots just like in js, e.g. prop.subprop.key
;; TODO: create generic interfaces to let the programmer associate types with parsing & exporting functions.
;; TODO: write full version (that supports json object hierarches) after having working ameritrade client.
;; TODO: add support for catching unrecognized kwargs in each field. this'll catch things like #:import, which
;; doesn't exist.
#| define a struct that corresponds to a json object
   like struct in typed racket, except that it also defines functions jsexpr->structName
   and structName->jsexpr,using a given function to derive json keys from the supplied
   struct field names.

   NOTE: json-struct can produce very difficult type errors! double-check that you're
         matching the correct functions with the right keywords (e.g. don't mix-up the
         #:or and #:parse keywords) and ensure that the keywords' arguments are the
         correct types! remember also that you need to provide either #:no-export or
         #:export to any field whose type does not intersect with JSExpr!
         be careful of types like Real that are not subtypes of JSExpr (Inexact-Real,
         however, is a subtype of JSExpr!)

syntax: (json-struct struct-name field-tree struct-opts) where
  field-tree = (parent field-tree ...) ;; not yet supported
             | [field : type import export]
  parent = ;; whole parent object is currently unused
         | id
         | (id ..+)
  import = ;; if nothing given, then the JSExpr is cast to the field type
         | #:parse (-> (U 'not-found JSExpr) type)
         | #:unsafe-parse (-> JSExpr type) ;; assumes that value is found
         | #:or <inhabitant of type> ;; if lookup fails, use this value
         | #:init <inhabitant of type> ;; don't even both with lookup; just assume this value
         | #:auto-or <inhabitant of type> ;; if type was defined by json-struct without #:no-parse, then #:unsafe-parse jsexpr->type is used; if the field name key is not present in the JSON map that we're parsing, then the value provided to #:auto-or is used. if type was not defined via json-struct or was defined with the #:no-parse option, then a syntax error is raised.
         | #:key Symbol ;; specify the JSON key name for this field; makes this field an exception to #:key->field
  export =
         | #:export (-> type JSExpr)
         | #:no-export ;; do not export this in the JSON object
         | #:cast-out type ;; do not transform the field, but cast it to type (which must be (U Inexact-Real Integer)) so that it can be put in a JSON object
         | #:const-out value ;; use this value instead of whatever the struct object's field's value is

  * specifying multiple import (or export) statements has undefined behavior
  * struct-opts are passed verbatim to struct, except two kwargs specific to this macro:
    * #:key->field proc: function to derive field name from JSON object key (String -> String)
    * #:field->key proc: function to derive JSON object key from field name (String -> String)
    * #:no-export: do not define struct-name->jsexpr function
    * #:no-parse: do not define jsexpr->struct-name function
  * the order of import and export clauses does not matter
  * if import is omitted, then a failed lookup will cause an error
  * if export is omitted, then the key will be
  * if any of the parents fail lookup, an error is raised ;; again, not yet a valid concern
  * as a special case, if type = Real, and no #:export key is given for that field, then #:export real->jsexpr is used. NOTE: if you use this feature, remember to import real->jsexpr!
  * if type is the type of a previously-defined json-struct (without #:no-parse or #:no-export), then, unless otherwise specified, that field uses #:unsafe-parse jsexpr->... & #:export ...->jsexpr
|#
(define-syntax (json-struct stx)
  (let* ([args (cdr (syntax->datum stx))]
         [struct-name (car args)]
         [field-tree (cadr args)] ;; currently just list of fields
         [struct-opts (cddr args)]

         [struct-type (or (get-kwarg type-name        struct-opts) struct-name)]
         [constructor (or (get-kwarg constructor-name struct-opts) struct-name)]
         [key->field-p (or (and-let* ([x (member '#:key->field struct-opts)]) (take x 2)) null)]
         [field->key-p (or (and-let* ([x (member '#:field->key struct-opts)]) (take x 2)) null)]
         [key->field (if (null? key->field-p) identity (cadr key->field-p))]
         [field->key (if (null? field->key-p) identity (cadr field->key-p))]

         [no-parse?  (memq '#:no-parse  struct-opts)]
         [no-export? (memq '#:no-export struct-opts)]

         [struct-name-s (symbol->string struct-name)]
         [jsexpr-parse-fn  (string->symbol (string-append "jsexpr->" struct-name-s))]
         [jsexpr-export-fn (string->symbol (string-append struct-name-s "->jsexpr"))])
    (hash-set! parse-json-structs  struct-type (if no-parse? 'no-parse jsexpr-parse-fn))
    (hash-set! export-json-structs struct-type (if no-export? 'no-export jsexpr-export-fn))
    (datum->syntax
     stx
     `(begin
        (struct
          ,struct-name
          ,(map (λ (f) (take f 3)) field-tree)
          ,@(remq* (append key->field-p field->key-p '(#:no-export #:no-parse)) struct-opts))
        ;; ,@ here is basically using the list monad to optionally insert into the `begin` form or not
        ,@(if no-parse?
              null
              `((: ,jsexpr-parse-fn (-> JSExpr ,struct-type))
                (define (,jsexpr-parse-fn j)
                  ;; NEXT: have one let* statement with successive/recursive bindings being the nested parent accessors.
                  (let ([j (cast j JSHash)])
                    (,constructor
                     ,@(map (λ (f)
                              (let* ([f-type (caddr f)]
                                     [kwargs (cdddr f)]
                                     [k (or (eval (get-kwarg key kwargs)) ;; eval b/c k can validly be a symbol or identifier
                                            (eval `(string->symbol ,`(,key->field ,(symbol->string (car f))))))]
                                     [otherwise (get-kwarg or kwargs)]
                                     [init (get-kwarg init kwargs)]
                                     [parse (get-kwarg parse kwargs)]
                                     [unsafe-parse (get-kwarg unsafe-parse kwargs)]
                                     [auto-or (get-kwarg auto-or kwargs)]
                                     [json-struct-parse-f (hash-ref parse-json-structs f-type #f)]
                                     [js-hash-ref '(inst hash-ref Symbol JSExpr)]
                                     [unsafe-hash-ref `((inst hash-ref Symbol JSExpr JSExpr) ;; yes, we need to specify that the lambda encapsulating error returns a JSExpr
                                                        j ',k (λ () (error ',jsexpr-parse-fn
                                                                           (format "failed to parse field ~a of struct ~a from the following JSON object because it does not contain the '~s key: ~a"
                                                                                   ',(car f)
                                                                                   ',struct-name
                                                                                   ',k
                                                                                   (jsexpr->string j)))))])
                                (cond
                                  [init init]
                                  [otherwise ;; cast needed b/c (hash-ref j ',k if successful, is JSExpr rather than ,f-type. sensible casting requires that (∩ f-type JSExpr) is not Nothing
                                   `(cast (,js-hash-ref j ',k (λ () ,otherwise)) ,f-type)]
                                  [parse `(,parse ((inst hash-ref Symbol JSExpr 'not-found) j ',k (λ () 'not-found)))]
                                  [unsafe-parse `(,unsafe-parse ,unsafe-hash-ref)]
                                  [auto-or
                                   (if json-struct-parse-f
                                       (if (equal? 'no-parse json-struct-parse-f)
                                           (raise-syntax-error 'json-struct (format "~a was defined with #:no-parse; cannot auto-derive parsing function for this field." f-type) f)
                                           `(let ([v (,js-hash-ref j ',k #f)])
                                              (if v (,json-struct-parse-f v) ,auto-or)))
                                       (raise-syntax-error 'json-struct (format "~a was not defined via json-struct. cannot auto-derive parsing function for this field." f-type) f))]
                                  [json-struct-parse-f
                                   (if (equal? 'no-parse json-struct-parse-f)
                                       (raise-syntax-error 'json-struct (format "~a was defined with #:no-parse; cannot auto-derive parsing function for this field." f-type) f)
                                       `(,json-struct-parse-f ,unsafe-hash-ref))]
                                  [else `(cast ,unsafe-hash-ref ,f-type)])))
                            field-tree))))))
        ;; ,@ here is basically using the list monad to optionally insert into the `begin` form or not
        ,@(if no-export?
              null
              `((: ,jsexpr-export-fn (-> ,struct-type JSExpr))
                (define (,jsexpr-export-fn o)
                  ((inst make-immutable-hash Symbol JSExpr)
                   (list
                    ,@(filter-map
                       (λ (f)
                         (let* ([f-name (car f)]
                                [f-accessor-fn (string->symbol (string-append (symbol->string struct-name) "-" (symbol->string f-name)))]
                                [f-type (caddr f)]
                                [kwargs (cdddr f)]
                                [export (get-kwarg export kwargs)]
                                [no-export? (memq '#:no-export kwargs)]
                                [cast (get-kwarg cast-out kwargs)]
                                [const-out (get-kwarg const-out kwargs)]
                                [json-struct-export-f (hash-ref export-json-structs f-type #f)])
                           (if no-export?
                               #f
                               `((ann cons (∀ (a b) (-> a b (Pairof a b))))
                                 (ann ',(eval `(string->symbol ,`(,field->key ,(symbol->string f-name)))) Symbol)
                                 ,(cond [export `(,export (,f-accessor-fn o))]
                                        [const-out const-out]
                                        [cast `(cast (,f-accessor-fn o) ,cast)]
                                        [(equal? 'Real f-type) `(real->jsexpr (,f-accessor-fn o))]
                                        [json-struct-export-f
                                         (if (equal? 'no-export json-struct-export-f)
                                             (raise-syntax-error 'json-struct (format "~a was defined with #:no-export; cannot auto-derive ->jsexpr function for this field." f-type) f)
                                             `(,json-struct-export-f (,f-accessor-fn o)))]
                                        ;; TODO: use raise-type-error or smth if ann fails. can we currently do this?
                                        [else `(ann (,f-accessor-fn o) JSExpr)])))))
                       field-tree))))))))))

;; PropMap is a type that encodes the predicate about a type t: "t [is a union that] contains (HashMap k t)"
;; thus PropMap is a bit of a misnomer.
;; currently unused; see hash-merge
(define-type (PropMap k t) (∩ (HashTable k t) t))

;; for proper parametricity (namely over k & v), we'd need a predicate that checks for (HashTable k v). hash? only guarantees that a map is HashTableTop. unfortunately i think this currently cannot be checked by the type system; we'd need to check that v does not also include HashTableTop as one of its union elements! thus definitions are reified v = Any.
(: hash-merge (-> (HashTable Any Any) (HashTable Any Any) ;; (PropMap k v) (PropMap k v)
                  ;; k v v -> (Option (Pairof k v))
                  ;; function from key, original value, and new value, to new key & new value
                  [#:combine (Option (-> Any Any Any (Option (Pairof Any Any))))]
                  (HashTable Any Any))) ;; (PropMap k v)
(define (hash-merge d0 d1 #:combine [combine #f])
  (for/fold ([nd d0]) ;; nd : (PropMap k v)
            ([(k v) (in-hash d1)])
    (let ([nd@k (hash-ref nd k (const 'fail))]) ;; nd@k : (U 'fail v). we assume that (∩ 'fail v) is Nothing
      (cond [(and (hash? nd@k) (hash? v)) ;; if we ever get the refinement typing that we need, then these two predicates will each need to be with in an if form, to get proper refinement
             (hash-set nd k (hash-merge (cast nd@k (HashTable Any Any)) (cast v (HashTable Any Any)) #:combine combine))]
            ;; we combine only if the key was found
            [(and combine (not (equal? 'fail nd@k)))
             (let ([cx (combine k nd@k v)])
               (if cx
                   (hash-set nd (car cx) (cdr cx))
                   (hash-remove nd k)))]
            [else (hash-set nd k v)]))))

;; common monomorphic hash-merge
(: jshash-merge (-> JSHash JSHash
                    ;; function from key, original value, and new value, to new key & new value
                    [#:combine (Option (-> Symbol JSExpr JSExpr (Option (Pairof Symbol JSExpr))))]
                    JSHash))
(define (jshash-merge d0 d1 #:combine [combine #f])
  (for/fold ([nd d0]) ([(k v) (in-hash d1)])
    (let ([nd@k (hash-ref nd k (const 'fail))])
      (cond [(and (hash? nd@k) (hash? v)) (hash-set nd k (jshash-merge nd@k v #:combine combine))]
            ;; we combine only if the key was found
            [(and combine (not (equal? 'fail nd@k)))
             (let ([cx (combine k nd@k v)])
               (if cx
                   (hash-set nd (car cx) (cdr cx))
                   (hash-remove nd k)))]
            [else (hash-set nd k v)]))))

;; filter a hash map to a list of keys with optional key and value transforms
(: filter-hash-map (∀ (k v)
                      (-> (Listof k)
                          (HashTable k v)
                          (#:kv-transform (Option (-> k v (Values k v))))
                          (HashTable k v))))
(define (filter-hash-map keys m #:kv-transform [transform #f])
  (foldl (λ ([k : k] [filtered-map : (HashTable k v)])
           (if transform
               (let-values ([(nk nv) (transform k (hash-ref m k))])
                 (hash-set filtered-map nk nv))
               (hash-set filtered-map k (hash-ref m k))))
         (ann #hash() (HashTable k v)) keys))

;; convert a json to a list of lists, i.e. a table
;; to output this table as a csv, i recommend the csv-writing package
(: json->table (-> String ;; header for the column of keys from the input hashmap
                   (Option (Listof Symbol)) ;; list of keys to include. if #f, gets key list from first object in provided hashmap
                   (HashTable Symbol JSHash) ;; hashmap to convert
                   (Listof (Listof String))))
(define (json->table key-field fields j)
  (or (and-let* ([first-pos (hash-iterate-first j)]
                 [fields (sort (or fields (hash-keys (hash-iterate-value j first-pos))) symbol<?)]
                 [header (cons key-field (map symbol->string fields))])
                ;; each row (except the header) is a list of (key-field field1 ... fieldn)
                (cons header
                      (map (λ ([kv : (Pairof Symbol JSExpr)])
                             (cons (symbol->string (car kv)) (map (λ ([f : Symbol]) (~a (hash-ref (cast (cdr kv) JSHash) f))) fields)))
                           (hash->list j))))
      null))

;; NB. if you specify a number of columns vector, json->pretty-table-str does not wrap cells whose string length is greater than the number of specified columns
;; crashes if any of col-widths' values are shorter than the longest string in that column
(: pretty-table-str (-> (Listof (Listof String))
                        [#:col-widths (Option (Listof Nonnegative-Fixnum))] ;; calculating column widths is a fairly expensive operation;
                        ;; you can specify a vector of column widths to reduce amount of computation.
                        String))
(define (pretty-table-str rows #:col-widths [col-widths #f])
  (or (let ([col-widths (or col-widths
                            (for/fold ([maxes : (Listof Nonnegative-Fixnum) (make-list (length (car rows)) 0)])
                                      ([r rows])
                              (map (λ ([m : Nonnegative-Fixnum] [cell : String])
                                     (max m (string-length cell)))
                                   maxes r)))])
        (string-join (map (λ ([r : (Listof String)])
                            (foldl (λ ([cell : String] [rml : Nonnegative-Fixnum] [acc : String])
                                     (string-append acc cell (make-string (+ 2 (- rml (string-length cell))) #\space)))
                                   ""
                                   r
                                   col-widths))
                          rows)
                     "\n"))
      "<empty json table>"))

;; convenience function: composes pretty-table-str & json->table
(: json->pretty-table-str
   (-> String
       (Option (Listof Symbol))
       (HashTable Symbol JSHash)
       [#:col-widths (Option (Listof Nonnegative-Fixnum))]
       String))
(define (json->pretty-table-str key-field fields j #:col-widths [col-widths #f])
  (pretty-table-str (json->table key-field fields j) #:col-widths col-widths))

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
(: table->json (-> String (Option (Setof String)) (Listof (Listof String)) (HashTable Symbol JSHash)))
(define (table->json key-field fields rows)
  (let* ([header : (Listof String) (car rows)]
         [rows   : (Listof (Listof String)) (cdr rows)]
         [key-field-index : (Option Index) (list-index (λ ([s : String]) (string=? key-field s)) header)]
         ;; NOTE: indices DOES NOT INCLUDE key-field! (it may, but that's arbitrary, and unlikely.)
         [indices : (HashTable Integer String)
                  (if fields
                      ;; get desired fields' column indcies
                      (for/fold ([m : (HashTable Integer String) #hash()])
                                ([f header] [i (in-naturals)])
                        (if (set-member? fields f) (hash-set m i f) m))
                      ;; index all columns: zip [0..] w/columns
                      (for/hash : (HashTable Integer String) ([f header] [i (in-naturals)]) (values i f)))])
    (if key-field-index
        (for/hash : (HashTable Symbol JSHash)
          ([row rows])
          (let ([p (for/fold : (Pairof Symbol JSHash)
                     ([p : (Pairof Symbol JSHash) (cons 'you-should-not-see-me jshash-empty)])
                     ([field-value row] [i (in-naturals)])
                     (let ([pmap (cdr p)]
                           [field-name (hash-ref indices i #f)])
                       (cons (if (= key-field-index i)
                                 (string->symbol field-value)
                                 (car p))
                             (if (and field-name (not (string=? field-name key-field)))
                                 (hash-set pmap (string->symbol field-name) (with-handlers ([exn:fail:read? (λ (_) field-value)]) (cast (read-json (open-input-string field-value)) JSExpr)))
                                 pmap))))])
            (values (car p) (cdr p))))
        (error (format "table->json: specified key field \"~a\" not found" key-field)))))
