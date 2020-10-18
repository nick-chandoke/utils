#lang typed/racket/base

;;; json accessors and constructors, and hashtable utils

(provide (all-defined-out) JSExpr)

(require (only-in racket/bool symbol=?)
         (only-in racket/function const identity)
         typed/json
         srfi/2
         (only-in racket/string string-join)
         (only-in racket/list range make-list)
         (only-in racket/set set-member?)
         (only-in racket/format ~a)
         (for-syntax (only-in racket/function identity)
                     (only-in racket/list splitf-at)
                     (only-in racket/match match)
                     racket/base))

(require/typed srfi/1 (list-index (All (a) (-> (-> a Boolean) (Listof a) (Option Index)))))

;; TODO: support nested accessors, e.g. ('meta ('description 'homepage)) to mean ('meta 'description) ('meta 'homepage)
;; TODO: add #:hash kwarg that, for each accessor sequence, adds a k/v pair whose key is that sequence's rightmost symbol. e.g. ('k1 'k2 ('k3 last)) would crease hash map with keys 'k1 'k2 'k3, and ('k1 'k2 ('k3 last 'k4)) would name the last accessed to 'k4 instead of 'k3, since (last (filter symbol? ('k3 last 'k4))) = 'k4
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
             ['a : String ""] ;; default value to assume if lookup fails. a : String.
             ['b : Real] ;; no default value given; if lookp fails, exception is raised
             ['c : String #f] ;; if lookup fails, then c = #f, and its type is (Option String)
             'd ;; no default, no type specification; no casting is attempted
             ['e car 'k1 'k3 : Natural 0] ;; assume that value at key e is a list. in its head, extract from nested hash tables. typed w/default.
             ('e car 'k1 'k2) ;; no type spec nor default value
             car)) ;; asserts the hash to a list, causing runtime exception. regardless, the syntax demonstrates that we can use function accessors just like we can use symbol accessors
u ;; "sports" : String
v ;; 6 : Real
w ;; 'cat : (Option String)
x ;; 23 : JSExpr
y ;; 0 : Natural
z ;; 90 : JSExpr
;; uh-oh causes the whole program to crash

* returns (Values ...) to ensure that number of arguments exactly equals number of returned keys
* unless #:list keyword is specified. #:list may be specified anywhere in mhash-ref's arg list
* assumes that keys are Symbols, as is the case in JSExpr
* if only one accessor is given, then returns a value not wrapped in `values`
* this macro expands to get-prop-val, so you'll need to always import get-prop-val if you import mhash-ref

== accessors formal syntax

accessor = lookup-pat
| (lookup-pat ...+ maybe-type-def)

lookup-pat = Symbol
| (-> JSList (U JSExpr JSHash))

maybe-type-def =
| : type
| : type default
|#
(define-syntax (mhash-ref stx)
  (let ([args (cdr (syntax->list stx))]
        [ret-list? #f])
    (let-values ([(X Y) (splitf-at args (λ (x) (not (equal? (syntax->datum #'#:list) (syntax->datum x)))))])
      (unless (null? Y) ;; didn't provide #:list
        (set! ret-list? #t)
        (set! args (append X (cdr Y)))))
    (datum->syntax
     stx
     (match args
       [(list m as ..1)
        `(let ([m ,m]) ;; eval m only once. useful if m is an expr (e.g. (expensive-build-map)) rather than an identifier (e.g. built-map)
           (,(if (= 1 (length as)) #'identity (if ret-list? #'list #'values))
            ,@(map (λ (a)
                     (let ([dla (syntax->list a)])
                       (if dla ;; #f if given a procedure
                           (if (null? dla)
                               (raise-syntax-error 'mhash-ref "empty accessor")
                               (if (equal? (syntax->datum #'quote) (syntax->datum (car dla)))
                                   ;; TODO: currently this cast is present is every accessor.
                                   ;; it should be done only once per accessor level
                                   `((inst hash-ref Symbol JSExpr) (cast m JSHash) ,a)
                                   ;; using let*-values instead of match b/c somehow (list accessors ..1 : type def) makes accessors singleton
                                   (let*-values ([(dlad) (map syntax->datum dla)] ;; e.g. ((quote prop) car (λ ([l : JSList]) ...) : String #f)
                                                 [(accessors maybe-type-def) (splitf-at dlad (λ (x) (not (equal? x ':))))])
                                     (case (length maybe-type-def)
                                       [(0) `(let ([x (get-prop-val m (list ,@accessors))])
                                               (if (equal? 'fail x)
                                                   (error (format "accessor sequence ~a failed on object ~a" (list ,@accessors) m))
                                                   x))]
                                       [(1) (raise-syntax-error 'mhash-ref "you must specify a type or a type and default value following a colon" dlad)]
                                       [(2) `(let ([x (get-prop-val m (list ,@accessors))])
                                               (if (equal? 'fail x)
                                                   (error (format "accessor sequence ~a failed on object ~a" (list ,@accessors) m))
                                                   (cast x ,(cadr maybe-type-def))))]
                                       [(3) `(let ([x (get-prop-val m (list ,@accessors))]) ;; list b/c get-prop-val takes a list of accessors to try in order
                                               (if (equal? 'fail x)
                                                   ,(caddr maybe-type-def)
                                                   (cast x ,(cadr maybe-type-def))))]))))
                           `(,a (cast m JSList)))))
                   as)))]
       [_ (raise-syntax-error 'mhash-ref "no accessors specified" stx)]))))

(define-type JSHash (HashTable Symbol JSExpr))
(define-type JSList (Listof JSExpr))
(: jscons (-> Symbol JSExpr (Pairof Symbol JSExpr))) (define jscons cons) ;; monomorphic cons
(define jshash (inst hash Symbol JSExpr)) ;; monomorphic hash
(define jsmhash (inst make-hash Symbol JSExpr)) ;; monomorphic make-hash
(define jshash-empty (ann #hash() JSHash))

;; extract nested keys, trying each accessor in order specified until one matches, or 'fail if none matches. for example, suppose my-json represents [{"k1":{"k2":{"k3":"my-str"}}}].
;; then (get-prop-val my-json (list car 'k1 'k2 'k3)) would return "my-str".
;; if a list operation is tried to be applied to something that isn't a list, or a key is given where the corresponding object isn't a hash table, then get-prop-val returns 'fail
;; also returns 'fail if you're absurd enough to not specify any accessors
;; be aware that if you provide an unsafe list operation (e.g. car or cdr), that can raise an exception
(: get-prop-val (-> JSExpr
                    (Listof (U Symbol (-> JSList (U JSExpr JSHash)))) *
                    (U 'fail JSExpr)))
(define (get-prop-val o . ass)
  (do ([t : (U JSExpr 'fail) 'fail] [ass ass])
      ((or (jsexpr? t) (null? ass)) ;; we want only one accessor to match; we only quit if we exhaust all accessors
       t)

    (set! t (ann (do ([t : (U JSExpr 'fail) o] [as (car ass)])
                     ((or (null? as) (symbol? t)) t) ;; we want all elements of the accessor as to match. if any one fails, then quit this loop. because Symbol (viz 'fail) is not an element of the JSExpr union, it's a suitable "failed lookup" value. that it's the specific symbol 'fail does not matter here; we can still use symbol? to refine the type while signaling that we should break loops.
                   (let ([a (car as)])
                     (set! t (if (hash? t) ;; nested if's rather than cond b/c want occurence typing
                                 (if (symbol? a)
                                     (hash-ref (cast t JSHash) a (λ () 'fail))
                                     'fail)
                                 (if (list? t)
                                     (if (procedure? a)
                                         (a (assert t list?)) ;; dunno why t isn't refined to list? i.e. JSList here
                                         'fail)
                                     'fail)))
                     (set! as (cdr as))))
                 (U JSExpr 'fail)))

    (set! ass (cdr ass)))) ;; discard this accessor; the next one will be tried on the next loop, if there is any

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
(: jsexpr-merge (-> JSHash JSHash
                    ;; function from key, original value, and new value, to new key & new value
                    [#:combine (Option (-> Symbol JSExpr JSExpr (Option (Pairof Symbol JSExpr))))]
                    JSHash))
(define (jsexpr-merge d0 d1 #:combine [combine #f])
  (for/fold ([nd d0]) ([(k v) (in-hash d1)])
    (let ([nd@k (hash-ref nd k (const 'fail))])
      (cond [(and (hash? nd@k) (hash? v)) (hash-set nd k (jsexpr-merge nd@k v #:combine combine))]
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
                        ;; you can specify a vector of column widths to save cycles.
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
                     ([p : (Pairof Symbol JSHash) (cons 'you-should-not-see-me (ann #hash() JSHash))])
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
