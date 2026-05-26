USING: accessors arrays assocs calendar calendar.parser colors
colors.hsv combinators continuations debugger generalizations
grouping hashtables http.client http.client.post-data http.json
io io.encodings.ascii io.encodings.string io.pathnames
io.sockets io.streams.string kernel math math.functions
math.order math.parser math.vectors multiline peg peg.parsers
present sequences sequences.extras sequences.generalizations
sequences.padded sequences.private sequences.zipped sorting
stack-checker strings strings.tables threads trees.avl urls
urls.encoding vectors
! needed for ws
byte-arrays http.websockets io.encodings io.encodings.binary
io.encodings.utf8 io.streams.byte-array io.streams.duplex
json json.private logging logging.server
; IN: util

: 5spin ( a b c d e -- e d c b a ) [ rot ] 2dip rot [ 4spin ] dip ; inline

<PRIVATE
ERROR: 1p p ; ! used exclusively by search-first
PRIVATE>

! "where" from j
: I. ( mask -- idxs ) [ and ] V{ } map-index-as sift! ; inline

: %chg ( start end -- %chg ) swap / 1 - 100 * >float ; inline

: mean/disp ( seq -- mean dispersion ) ! for simd, v-n vabs would be faster than [ - abs ] map
  dup [ sum ] [ length ] bi [ / tuck [ - abs ] curry map-sum ] keep / ;

! [non-]growable assocs initialized by 2 pairs
: 2hash    ( v1 v2 k1 k2 n -- ht ) [ swapd ] dip <hashtable> [ [ set-at ] curry 2bi@ ] keep ; inline
: 2alist/v ( v1 v2 k1 k2 n -- alist/v ) [ swapd [ swap 2array ] 2bi@ ] dip <vector>
[ [ push ] curry bi@ ] keep ; inline
: 2alist   ( v1 v2 k1 k2 -- alist ) swapd [ swap 2array ] 2bi@ 2array ; inline

! assoc joins. really i should have a total full outer / inner join that works for both assocs or relations of the same order [sort]. nb. the user can easily optimize assoc-intersect-each by making assoc1 the smaller of the two. doing this automatically would require runtime-computed quotations: conditionally working `[ swap ]` into the quot passed to `assoc-each`. fortunately these concerns are easily addressed per occasion by refactoring `assoc-intersect-each`.
! ofc, the perfect solution is just a traversal over the cartesian product, optimized for e.g. common orders (so we'd do like `lij`) or, pruning slices (interval intersection).

! being an intersection (inner join), cannot be used to define outer joins.
: assoc-intersect-each ( ..a assoc1 assoc2 q: ( ..a key value1 value2 -- ..a ) -- ..a )
  [ [ 3drop ] if ] curry swap [ at* ] curry
  [ over ] prepose prepose assoc-each ; inline

! compare with `assoc-merge!` which is, over a natural full outer join, either a binary operation applied to the inner join subset, or else a simple coalescence.
: assoc-inner-join! ( ..a assoc1 assoc2 quot: ( ..a key value1 value2 -- ..a new-value ) -- ..a assoc1' )
  [ 3keep 2drop ] curry pick [ set-at ] curry compose [ assoc-intersect-each ] 3keep 2drop ; inline

! whereas mod can be used for ring buffer index increment,
! -mod can be used for that or decrement.
! can be used to implement negative offsets e.g. -2 10 -mod => 8.
: -mod ( x y -- z ) [ + ] keep mod ; inline

: timestamp>jdn ( t -- n ) >date< julian-day-number ; inline
: jdn>ymd ( n -- ymdstr )
  julian-day-number>date 3array
  [ >dec 2 CHAR: 0 <padded-head> ] map "-" join ;

! replaces first occurrence
: find-replace1 ( haystack needle replacement -- haystack' )
  -rot 2dup subseq-index [ swap dupd length + rot snip-slice swapd 3append ] [ drop nip ] if* ;

! how search should have been defined. actually reports general errors!
: safe-search ( seq peg -- parsed ) any-char [ drop f ] action 2choice repeat0 parse sift ;

! subseq from i to seq's end, or empty seq if i is at least its length.
: subseq-from ( seq -- q: ( i -- subseq ) ) [ length ] keep [ drop [ min ] curry ] [ [ subseq ] 2curry ] 2bi compose ;
: set-last ( s v -- s' ) swap [ length 1 - ] keep [ set-nth ] keep ; inline

! like peg.search [peg] but returns after finding the 1st result.
! ([^p]*)p would work, but idk how to write that in peg. (.*)p is almost right, but
! parses the whole string to .* before ever attempting p. (.|p)* is what `search` does,
! but obviously doesn't stop matching after the 1st successful parse of p.
! regardless of proving a parser solution or lack thereof, it's much easier to just use continuations.
: search-first ( input ?parser -- ?first-parsed )
  dup
  ! this parser always succeeds so `parse` never throws a parse-error. if our parser succeeds, then it throws and its result is returned.
  [ [ 1p ] action any-char 2choice repeat0 [ parse drop f ] curry [ dup 1p? [ p>> nip ] [ rethrow ] if ] recover ]
  [ 2drop f ]
  if ;

: and* ( x/f q: ( x -- y ) -- y/f ) over [ call ] [ drop ] if ; inline

! length n slice ending at i
: recent-n ( i n seq -- slice ) [ over [ - 0 max ] dip ] dip <slice> ;

: seq>hash ( ..a seq q: ( ..a x -- ..a v k ) -- ..a htable )
  over length <hashtable> [ [ set-at ] curry compose each ] keep ; inline

! TODO: consider writing reduce-* like map-filter-until: putting vector atop stack then dipping, which is more elegant (simpler)

<PRIVATE
! more efficient than `(reduce-filter) sift`. an example of converting between these forms so that the simpler form is converted into this more complex form.
: (reduce-collect) ( ..a seq q: ( ..a e -- ..a ?collectval ) ndip npick -- ..a collection ) ! actually collection ..a
  [ [ V{ } clone ] swap ndip ] ! accumulation vector
  [ npick ] bi* [ push ] curry [ when* ] curry compose each ; inline ! call-n requires inline

: (reduce-filter) ( ..a seq q: ( ..a e -- ..a ?collectval ) ndip npick -- ..a collection ) ! actually collection ..a
  [ [ V{ } clone ] swap ndip ] ! accumulation vector
  [ npick ] bi* [ push ] curry compose each ; inline ! call-n requires inline
PRIVATE>

! NOTE: first consider using `group-by` (`grouping.extras` vocab) with `map`!
! a combination of the replace/constant and additive monoids: it's the additive monoid until a predicate is satisfied, at which point the element is pushed to a collection vector. if the quotation produces `f` then it's not pushed. `reduce-collect` is basically a combination of `split-when`, `reduce`, & `filter`.
! e.g. `[ + ] reduce` but when the sum is ≥5, push that value to a vector and restart the sum at the current iteration element:
! { 1 2 3 4 1 1 8 20 } [ first ] [ 1 tail-slice ] bi
! [ over 5 >= [ swap ] [ + f ] if ] reduce-collect => V{ 6 5 9 } 20.
! you may ask, "20≥5 so why not push that, too?" it's b/c that'd be effect ( ..a -- ..a ), which does not match q's effect. i could break q into two parts, but i won't try to anticipate how that should be done. i want to keep reduce-collect flexible, and keeping it simple is the easiest way to do that.
! f [ over 5 >= [ swap ] [ + f ] if ] reduce-collect throws a stack underflow error. this is analagous to not providing an initial value to `reduce`.
! reduce-collect's quotation commonly features an `if` one of whose branches is [ swap ] and the other being a binary operation followed by `f`.
! to push into some data structure other than a vector, you must replace `V{ } clone` and `push`. a convenient generalization is to replace them with monoidal axioms `mempty` & `mappend`, and moot lifting operation such as `pure :: (Applicative f, Foldable f, Monoid (f a)) => (a -> f a)`. to make reduce-collect do that, we'd generalize the quotation to ( ..a structure e -- ..a structure ), but that's just basically just a fold. rather than write folds so, though, it's easier to do like haskell's `foldMap`. ad-hoc polymorphic, composable semigroups are the best way to fold, and folding is one of the basic programming primitives. indeed, because `Semigroup a => Semigroup (Maybe a)`, which short-circuits (just as it does with `(<*>)`), it gives us all that we need for general traversals, very elegantly! the reason is because the `Semigroup` type class associates data structures with modification operations; the modifications are expressed as semigroups: `foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m, foldMap f = foldr (mappend . f) mempty`.
MACRO: reduce-collect ( q -- q )
  dup infer in>> length 1 + dup 1 + [ (reduce-collect) ] 3 ncurry ;

! `reduce-collect` but pushes any `f`
MACRO: reduce-filter ( q -- q )
  dup infer in>> length 1 + dup 1 + [ (reduce-filter) ] 3 ncurry ;

! the following example returns no more than 4 values, and the values that it returns are 10 * some even number:
! 0 { 2 4 7 6 8 10 20 1 3 } [ drop dup 4 = ] [ dup even? [ 10 * [ 1 + ] dip ] [ drop f ] if ] map-filter-until nip . ! prints V{ 20 40 60 80 }
: map-filter-until ( ... seq stop: ( ... elt -- ... ? ) q: ( ... x -- ... ?y ) -- ... seq )
  rot
  [ [ keep swap ] curry ] 2dip
  [ length 2/ <vector> dup [ push ] curry [ when* f ] curry -rotd compose [ if ] curry
    [ drop t ] swap curry compose
  ] keep swap rot [ find 2drop ] dip ; inline

! search through a sequence until n truthy items are found or the list is exhausted
! e.g. { 2 4 7 6 8 10 20 1 3 } [ dup even? [ 10 * ] [ drop f ] if ] 4 find-n
! a vector of size n is created; do not use a large n to mean "take all possible"; use
! map-filter or filter-map for that.
! the quotation is (a -> Maybe b) because it generalizes (a -> Bool) -> (a -> b) and (a -> b) -> (b -> Bool).
: find-n ( seq q: ( x -- ?y ) n -- seq )
  ! the quotation that we'll build then pass to loop: [ i dup seq ?nth [ v # n < [ q call [ v push ] when* ] [ drop ] if 1 + t ] [ f ] if* ]
  ! the integer on the stack is the iteration number. it's incremented at the end of each loop.
  [ [ ?nth ] curry [ dup ] prepose ] 2dip
  [ [ < ] curry ] [ <vector> ] bi
  [ [ [ length ] curry prepose
    swap ]
    [ [ push ] curry [ when* ] curry compose [ [ drop ] if 1 + t ] curry compose [ [ f ] if* ] curry compose ]
    bi 0 swap loop drop
  ] keep ; inline

: split1-when* ( seq q: ( ... elt -- ... ? ) -- ... pre sep:post ) [ find drop ] keepd swap [ dup rot snip ] [ f ] if* ; inline

: no-more-than ( seq n -- slice ) over length min head-slice ; inline

: parse* ( seq parser -- ?result ) [ parse ] curry [ dup parse-error? [ 2drop f ] [ rethrow ] if ] recover ; inline
: parse-fully* ( seq parser -- ?result ) [ parse-fully ] curry [ dup unable-to-fully-parse? [ 2drop f ] [ rethrow ] if ] recover ; inline

: parse-ymd ( s -- date ) [ read-ymd ] with-string-reader 0 0 0 T{ duration f 0 0 0 0 0 0 } <timestamp> ; inline

! second precision
: time>=  ( a b -- ? ) time- duration>seconds 0 >= ; inline
: time<= ( a b -- ? ) swap time>= ; inline
: time-between? ( timestamp begin-str end-str -- ? ) [ parse-ymd time>= ] [ parse-ymd time<= ] bi-curry* bi and ;

: (wt) ( content tag -- html ) [ "<" ">" surround ] [ "</" ">" surround ] bi surround ; inline
: table>html ( table header -- html )
  [ [ [ "td" (wt) ] map concat "tr" (wt) ] map concat "tbody" (wt) ]
  [ [ "th" (wt) ] map concat "tr" (wt) "thead" (wt) ]
  bi* prepend "table" (wt) ; inline
: wrap-html ( body title ?other-head-tags -- html )
  [ "</body></html>" append ]
  [ "<!DOCTYPE html><html><head><title>" "</title>" surround ]
  [ "" or "</head><body>" append ] tri* rot 3append ; inline
: (ul) ( seq -- html ) [ "li" (wt) ] map concat "ul" (wt) ; inline

! rather than inserting only one element, inserts a sequence at the nth position
: insert-nth* ( haystack needle i -- s ) swap [ [ head-slice* ] [ tail-slice* ] bi-curry bi ] dip swap 3append ;

! real number formatted as percentage without decimal places
: pp% ( n -- s ) 1 - 100 * 1/2 + >integer >dec CHAR: % suffix ; inline

: (sigfigs) ( whole-str dec-str sigfigs -- str )
  pick length - dup 0 <=>
  { { +lt+ [ nip neg [ head-slice* ] [ CHAR: 0 <repetition> ] bi append ] } ! that many zeroes for the 1st string and drop the 2nd
    { +eq+ [ 2drop ] }                                                      ! keep 1st drop 2nd
    { +gt+ [ over length min head-slice "." glue ] } } case ;               ! take that many of 2nd (min its length) and "." glue

: sigfigs ( n sigfigs -- str )
  [ >dec CHAR: . over index [ [ head-slice ] [ 1 + tail-slice ] 2bi ] [ f ] if* ] dip (sigfigs) ; inline

: ee ( n -- mantissa exponent ) dup >dec length 1 - [ 10^ /f ] keep ; inline

: abbrev-large-num ( n sigfigs -- ?pp )
  { { [ over 1000 < ] [ drop >dec ] } ! TODO: use sigfigs
    { [ over 1,000,000,000,000,000 >= ] [ 2drop f ] } ! what, "Q" for quantillion?
    [ [ { 1,000,000,000,000 1,000,000,000 1,000,000 1,000 } over [ <= ] curry find swapd
        /f >dec CHAR: . over index [ [ head-slice ] [ 1 + tail-slice ] 2bi ] [ f ] if* ] dip
      (sigfigs)
      ! remove trailing ".0"; after sigfigs, any /.0+/ must be ".0" exactly (it seems; i haven't formally verified)
      dup length 2 >= [ dup 2 tail* ".0" = [ 2 head* ] [ ] if ] [ ] if
      swap "TBMK" nth-unsafe suffix ] } cond ;

! add a comma between each trio of digits from the right
: comma-numbers ( n-str -- w/commas ) <reversed> 3 <groups> [ reverse ] map <reversed> "," join ; inline

: fmt$ ( $ -- str )
  [ abs >dec CHAR: . over index
    [ cut-slice [ comma-numbers ] dip rest-slice 2 CHAR: 0 <padded-tail> 2 head-slice "." glue ] [ ".00" append ] if*
    CHAR: $ prefix
  ] keep neg? [ "(" ")" surround ] when ; inline

! linear interpolation methods, from <https://hackmysql.com/eng/percentiles/>
! r8-i is recommended
: r6-i ( cnt percentile-rank -- i ) [ 1 + ] dip * ; inline
: r7-i ( cnt percentile-rank -- i ) [ 1 - ] dip * 1 + ; inline
: r8-i ( cnt percentile-rank -- i ) [ 1/3 + ] dip * 1/3 + ; inline
: (percentile-rank/linear-interpolation) ( ordered-seq percentile-rank q: ( cnt pr -- i ) -- percentile )
  [ [ length dup 1 - [ min 0 max ] curry ]
    [ [ nth ] curry ]
    bi compose swap
  ] 2dip execute( x y -- z ) >fraction [ /mod ] keep / ! @seq k f
  [ [ 1 - ] keep rot bi@ over - ] dip * + ;
: percentile-rank/linear-interpolation ( sorted-seq percentile-rank -- percentile ) \ r8-i (percentile-rank/linear-interpolation) ; inline

! efficient version of [ n percentile-ranks/linear-interpolation ] accumulate*
! input seq does not need to be ordered already
: percentile-ranks/linear-interpolation ( seq percentile-rank -- percentiles )
  [ percentile-rank/linear-interpolation ] curry
  [ dup length f swap avl boa [ [ set-at ] keep keys ] curry ] dip compose [ f swap ] prepose map ;

: med ( seq -- m ) 1/2 percentile-rank/linear-interpolation ; inline

! the median, given an odd-length sequence
: oddmed ( seq n -- m ) 2/ swap sort nth ;

! in one pass, compute the min & max of a sequence of atoms, or -1/0. 1/0. if given the empty sequence
: max&min0 ( seq -- max min ) [ -1/0. 1/0. ] dip [ [ max ] [ min ] bi-curry bi* ] each ; inline

! in one pass, compute the max & min of a sequence of atoms, or -1/0. 1/0. if given the empty sequence
: max&min1 ( seq q: ( elt -- val ) -- max min ) [ -1/0. 1/0. ] 2dip [ [ max ] [ min ] bi-curry bi* ] compose each ; inline

! in one pass, compute the max & min of a sequence that can be expressed as intervals or -1/0. 1/0. if given the empty sequence
: max&min2 ( seq q: ( elt -- high low ) -- max min ) [ -1/0. 1/0. ] 2dip [ swapd [ max ] [ min ] 2bi* ] compose each ; inline

: tag-alist ( alist tags -- hashmap ) swap H{ } zip-as ; inline

! slice from 0 to earliest elt matching pred (exclusive)
: 0-to-pred ( seq pred: ( x -- ? ) -- slice )
  dupd find [ swap 0 -rot <slice> ] [ nip ] if ; inline

! slice from latest elt matching pred (inclusive) to end
: pred-to-end ( seq pred: ( x -- ? ) -- slice )
  dupd find-last [ swap [ length ] keep <slice> ] [ nip ] if ; inline

: cdf ( seq -- cdf ) [ f ] [ sort 0 [ + ] accumulate* dup last [ / ] curry map ] if-empty ;

! example valid input: "2023-04-26T10:59:00.000-0500"
: parse-iso8601+gmt ( str -- timestamp )
  [ 5 head* [ read-rfc3339 ] with-string-reader ]
  [ 5 tail-slice* [ 1 3 rot <slice> ] [ 2 tail-slice* ] [ first CHAR: + = 1 -1 ? [ * ] curry ] tri
    [ string>number ] prepose bi@ duration new swap >>minute swap >>hour ]
    bi >>gmt-offset ;

CONSTANT: MAX_REQ_ATTEMPTS 4

! if max retries is hit then resp & data are f.
! regardless, it returns the vector that collected bad responses or connection errors.
: http-request*/retry ( req -- errors resp data )
  MAX_REQ_ATTEMPTS <vector> 0 rot ! loop's ..a: vec n req
  [ over MAX_REQ_ATTEMPTS >=
    ! comparing iteration against MAX_REQ_ATTEMPTS can't be ANDed with push-then-try-again?
    ! b/c that can't account for the case wherein we don't want to try again but we do
    ! want to push.
    [ drop "max http request attempts hit" pick push f f ]
    [ [ dup http-request* drop dup code>> 500 599 between? ]
      [ dup addrinfo-error? [ t ] [ rethrow f ] if ]
      recover ! ... req resp/error push-then-try-again?
      [ [ reach push over 2^ seconds sleep ] [ nip ] if
        [ 1 + ] dip
      ] keep
    ] if
  ] loop nip [ dup body>> ] [ f f ] if* ;

: url-append-path' ( url path -- url' ) [ dup path>> ] dip url-append-path >>path ; inline

! mimics netscape cookiejar format as closely as possible;
! the difference is that factor cookies don't have a bool
! for "include subdomains". also, `cookie` has a `comment` field;
! idk what this is yet. `resource:basis/http/http-tests.factor` uses
! it as though it's a known aspect of the `Set-Cookie` header, but i
! don't see it mentioned in <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Set-Cookie>
: cookies>string ( cookies -- string )
  ! either max-age or expires should be set, but (according to mdn) max-age takes precedence if both are set.
  ! session cookies have an expire time of 0
  [ { [ domain>> ] [ path>> ] [ secure>> "TRUE" "FALSE" ? ]
      [ dup max-age>> ! idk how one is supposed to do anything with max-age; i'd think that once a Set-Cookie is received, then any max-age would be translated immediately by using `now` to a time which would be stored in `expires`...?
        [ nip duration>seconds >dec ]
        [ expires>>
          [ timestamp>millis >dec ]
          [ f ] if*
        ] if* "0" or
      ] [ name>> ] [ value>> present ] } cleave 6 narray
  ] map { "DOMAIN" "PATH" "SECURE" "EXPIRES/M.A." "NAME" "VALUE" } prefix format-table "\n" join ;

: request>string ( req -- str )
  [ >post-data ] change-data dup data>>
  [ dup params>> [ assoc>query ascii encode >>data ] when*
    dup data>> pathname?
    [ [ present "file stream: " prepend ] change-data ] when drop
  ] when*
  { [ method>> ] [ url>> present ]
    ! default url is URL" ", not f, so i check path, which must be set at least to "/" b/c otherwise
    ! factor would generate only a malformed http request from it.
    [ proxy-url>> dup path>> [ present "via proxy " prepend " " glue ] [ drop ] if ]
    [ version>> "HTTP/" prepend 3array " " join ]
    [ header>> [ ": " glue ] { } assoc>map "\n" join "\n" glue ]
    [ cookies>> [ f ] [ cookies>string ] if-empty ]
    [ data>>
      [ [ content-type>> [ "content type: " prepend ] [ f ] if* ]
        [ data>> dup length 0xfff > [ 0xffd head-slice "..." append ] [ ] if ]
        [ params>> [ format-box "\n" join ] [ f ] if* ]
        tri 3array sift "\n" join ]
      [ f ] if* ]
  } cleave 3array sift "\n\n" join ;

! words built on `http-request*`, such as like `http-get`, give the
! content atop the stack, but also store it in the response tuple's `body`
! slot, so you'd to e.g. `http-get drop response>string`.
: response>string ( resp -- str )
  { [ version>> "HTTP/" prepend ] [ code>> >dec ] [ message>> 3array " " join ]
    [ header>> [ ": " glue ] { } assoc>map "\n" join "\n" glue ] ! headers already include content's type & charset/encoding
    [ cookies>> [ f ] [ cookies>string ] if-empty ]
    [ body>> [ dup length 0xfff > [ 0xffd head-slice "..." append ] [ ] if ] [ f ] if* ]
  } cleave 3array sift "\n\n" join ;

! TODO: lij and <align-seqs> both are easily expressed in prolog, since they're both just satisfying predicates until one fails, at which point they jump forward 1 or n indices respectively to get the next thing that satisfies, then use it in the function. i want to generally define traversals implicitly in terms of generators (test predicate, and if it fails, then try next set of arguments, until there are no more to try). i don't want to use factor's `logic` vocab for this, however, because it's too verbose; i don't want to define a global fact db! i just want to specify some facts inline to be used in one traversal. generally, we can use sequences, virtual or literal, to represent "next". ...actually, <align-seqs> might already express this most-general prolog pattern, but in the case of binary predicates.

! efficient left inner join on two seqs of data sorted by their key, having approximately equal key sets; if the key sets had large gaps or mostly didn't overlap, then a binary traversal would be better than the linear one that this uses.
! if you curry your quotation to a mutable sequence, then you can mutate it by using the index provided to the quotation
! to accumulate into a new sequence given input rows whose keys are at index 0: `... [ drop [ first ] bi@ bin-op pred? nip ] selector [ lij ] dip`
! you'll want to sift the resultant sequence
! x & y must support <=>
! example:
! { { "bird" "cage" } { "cat" "hat" }                   { "mouse" "house" } } [ 1vector ] assoc-map dup
! {                   { "cat" "pat" } { "dog" "house" } { "mouse" "chees" } { "mouse" "maus" } { "parrot" "cage" } }
! [ first ] [ [ second ] bi@ swap push 2drop ] lij
! leaves { { "bird" V{ "cage" } } { "cat" V{ "hat" "pat" } } { "mouse" V{ "house" "chees" "maus" } } } on the stack.
! and
! { { "bird" "cage" } { "cat" "hat" }                   { "mouse" "house" } } [ 5 f <array> [ set-first ] keep ] assoc-map dup
! {                   { "cat" "pat" } { "dog" "house" } { "mouse" "chees" } { "mouse" "maus" } { "parrot" "cage" } }
! [ first ] [ [ second-unsafe ] bi@ nipd -rot set-nth ] lij
! leaves ! { { "bird"  { "cage"  f     f      f f } }
!            { "cat"   { "hat"   "pat" f      f f } }
!            { "mouse" { "house" f     "maus" f f } } }
! notice that for "mouse", "chees" was written at index 2, then overwritten by "maus".
! these examples extend easily into a `reduce` or `each` form, to efficiently merge multiple alists into an arbitrary structure.
: lij ( xs ys key: ( x/y -- k ) join: ( i k x y -- ) -- ) ! can't so well be a fried quot b/c uses lengths of both lists
  [ [ over ] 3dip ] prepose [ 1 + ] compose +eq+ swap 2array
  { +lt+ [ 3drop [ 1 + ] dip ] } swap { +gt+ [ 3drop 1 + ] } 3array [ case t ] curry
  [ [ bi@ [ -rot pick ] dip <=> ] curry
    [ [ [ length ] bi@ [ >= ] bi-curry@ [ bi* or [ f ] ] 2curry ]
      [ [ nth ] bi-curry@ [ bi* 2dup ] 2curry [ 2dup ] prepose ] 2bi ] dip compose
  ] dip compose [ if ] curry compose [ 2dup ] prepose [ call( i j -- i j ? ) ] curry [ 0 0 ] dip loop 2drop ;

! preconditiosn:
! * A & B are sorted sequences
! * B is non-empty
! scaffolds for iteration over A that matches each a in A with b, the
! greatest element in B less than a; then performs f(a,b).
! you'll want to nip after using the quotation to iterate.
! remember that B is absorbed into the iteration quotation.
! example:
! { 1.0 1.1 1.2 1.3 2.1 2.4 2.6 3.4 4 5 6 }
! { 0 1 3 5 7 } [ 2array ] align-seqs-by-value map nip .
! { { 1.0  1 }
!   { 1.1  1 }
!   { 1.2  1 }
!   { 1.3  1 }
!   { 2.1  1 }
!   { 2.4  1 }
!   { 2.6  1 }
!   { 3.4  3 }
!   {   4  3 }
!   {   5  5 }
!   {   6  5 } }
! note that the only reason that A is in the stack effect is so that we can put 0 under it for convenience. <align-seqs> does not concern its value at all. it does not appear in the resultant quotation.
! remember to use not [ <=> ], but [ <=> +lt+ = ], or just [ < ] if you're using numbers.
! to find indices at which B occur in A:
! [ < ] [ = ] <align-seqs> map nip ! mask
! [ and ] map-index sift ! mask to idxs
: <align-seqs> ( A B cmp: ( a b -- lt? ) f: ( a b -- c ) -- 0 A q: ( i a -- j c ) )
  pick empty? [ "<align-seqs>: empty B" throw ] when
  [ [ 0 -rot [ length ] [ [ nth ] curry ] bi ] dip
    [ compose [ dupd ] prepose ! retain <smth> on the stack for successive iterations of find-integer-from
      [ find-integer-from ] 2curry ] curry
    [ [ [ or 1 [-] dup ] curry ] dip ! curry len(B) to `or`; if find returns f, use B's last elt
      compose [ swapd ] compose
    ] 2bi compose
  ] dip compose [ swap ] prepose ! swap b/c map puts x atop i. find-integer-from needs i above.
; inline ! must be inline to be used in combinator chains; else it's run-time computed value

! wip row-polymorphic version. above version doesn't even work with map-index! e.g.
! { 1 1.5 2 3 4 10 11 12 13 }
! { 1 2 12 16 }
! [ < ] [ B [ = ] dip and ] B <align-seqs> map-index nip ...
! is wrong b/c <align-seqs> produces
! [ swap 4 [ dupd { 1 2 12 16 } nth < ] find-integer-from
!   4 or 1 [-] dup { 1 2 12 16 } nth
!   swapd
!   [ = ] dip and ]
! when it should be mostly wrapped in dip:
! [ [ swap 4 [ dupd { 1 2 12 16 } nth < ] find-integer-from
!     4 or 1 [-] dup { 1 2 12 16 } nth
!     swapd
!     =
!   ] dip and ]
! so the problem in this case is not the number of data that the quotation returns, but rather the number (and order) of args that map-index pushes to the quotation.
! p(a,b,c) [ ] [ (next) ] if*
! : <align-seqs> ( ..a A B cmp: ( ..a b -- ..a lt? ) f: ( ..a b -- ..b ) -- ..a 0 A q: ( ..a i -- ..b j ) ) ! i is used to lookup b.
!   [ [ 0 -rot [ length ] [ [ nth ] curry ] bi ] dip
!     [ compose [ dupd ] prepose ! retain x on the stack for successive iterations of find-integer-from
!       [ find-integer-from ] 2curry ] curry
!     [ [ [ or 1 [-] dup ] curry ] dip ! curry len(B) to `or`; if find returns f, use B's last elt
!       compose [ swapd ] compose
!     ] 2bi compose
!   ] dip compose
! ; inline ! must be inline to be used in combinator chains; else it's run-time computed value
! both [ _ push ] and [ + ] produce q: ( x x -- x x ).

! next: [| l s | l [ dupd s nth < ] find-integer-from l or 1 [-] dup s nth ]

! moving sum/avg can be done in one pass and/or incrementally (e.g. as data is being streamed-in) by storing the most recent n values then subtracting them from the current sum/avg.
! sum & avg are equal definitions except that avg divides the incoming datum by n.

! from "k3 idioms"
: moving-sum ( seq n -- ms )
  [ 0 [ + ] accumulate* ] dip
  [ 1 - tail-slice ] [ head-slice* 0 prefix ] 2bi v- ;

! faster, but less accurate, if input seq is of floats
: moving-avg ( seq n -- ma )
  [ [ / + ] curry 0 swap accumulate* ] keep
  [ 1 - tail-slice ] [ head-slice* 0 prefix ] 2bi v- ; inline

! 24-bit ("true color") terminal color codes. see <https://github.com/termstandard/colors> about any particular term
! rgba is the tuple of the `colors` vocab
! about other escape codes: <https://en.wikipedia.org/wiki/ANSI_escape_code>
: terminal-color-string ( text color -- escaped-str )
  >rgba [ red>> ] [ green>> ] [ blue>> ] tri [ 255 * >integer ] tri@ [ >dec CHAR: ; prefix ] tri@ 3append "\e[38;2" "m" surround "\e[0m" surround ; ! 38 -> 48 to set bg

! 40 visually distinct, bright colors that display well on a black bacground.
! useful for coloring equal elements of a multiset
: distinct-colors ( -- colors )
  40 [ 360/20 * 360 /mod [ 1/5 * 1 swap - ] dip 1 rot 1 <hsva> ] map-integers [ hue>> 108 144 between? ] reject ;

! convert a monotonically increasing sequence of naturals into
! a strictly increasing one.
! e.g. converts { 10 10 13 14 16 16 16 16 20 21 21 21 21 27 167 }
! into          { 10 11 13 14 16 17 18 19 20 21 22 23 24 27 167 }
: mono>strict ( x -- y ) 0 swap [ 2dup < [ nip ] [ drop 1 + ] if dup ] map! nip ;

! for(i=0;i<length(seq)-n;i++) seq[i]=seq[i+n]; for(i=n;i<length(seq);i++) seq[i]=x;
! i'm using this instead of array-as-ring-buffer for some reason....
: shift-seq-left ( seq n x -- )
  [ pick '[ _ swap _ set-nth ]
    [ [ [ length ] dip - ] 2keep
      '[ _ [ [ _ + ] dip nth ] 2keep set-nth ] [ each-integer ] keepd
    ] dip
  ] keepd pick + swap each-integer-from ; inline

! ORIGINALS. TESTED: SUCCESS.
! to test: 5000 5017867 randoms [ 4800 - julian-day-number>date [ ymd>jdn ] [ julian-day-number ] 3bi = ] count 5000 / 100 * >dec print
: ymd>jdn ( y m d -- jdn )
  [ 14 over - 12 /i
    [ - 4800 + float-4{ 0.25 0.0025 365 0.01 } swap v*n vfloor float-4{ 1 1 1 -1 } v* sum ]
    [ 12 * + 3 - 153 * 2 + 5 /i ] bi-curry bi*
  ] dip + + 32045 - >integer ;

: jdn>ymd ( jdn -- y m d )
  32044 + 146097 1461 [ [ [ 4 * 3 + ] dip /i ] 2keep pick * 4 /i - ] bi-curry@ [ call ] dip call ! q: ( a k -- b c )
  ! .s: b d e
  [ 100 * ] [ + 4800 - ] [ ] tri* ! f e
  dup 5 * 2 + 153 /i ! .s: f e m
  [ [ 10 /i + ] [ 153 * 2 + 5 /i - 1 + ] bi-curry bi* ] keep
  [ ] [ 10 /i 12 * - 3 + ] bi
  swap ;

! WIP's. GOAL: REDUCE AS MUCH AS POSSIBLE W/ORT JDN; ANY VALUE THAT CONVERTS BX YMD & MILLIS; AND EXPRESS BY LINEAR TRANSFORMS ONLY.
! jdn since 1970
: ymd>jdn ( y m d -- ??? )
  [ dup 3 < -1 0 ? ! old equivalent: 14 over - 12 /i
    ! the following quots turn years & months into days
    ! so put either -1 or 0 at the start of these quots, then perform them respectively on y & m
    [ - ! add 1 or 0
      ! sum the year's days plus an extra day per 4 years and 400 years, but subtract a day per 100 years.
      ! remember that floor is needed only before multiplying by an integer, or arithmetic with other reals.
      [ int-4{ 0xffffffff 4 400 -100 } swap n/v sum ] keep 365 * + ]
    [ 12 *                 ! mul month by -1 or 0
      +                    ! then add to the result of the year op above
    ] bi-curry bi* 30.6 * 92.2 - >integer
  ] dip + + ! sum. day is verbatim.
  ! 32045 -
  ! >integer ! redundant; above >integer is after y & m are combined
  ! 2440588 - ! since 1970
  ;

! unix-ms 86400000 /i ! simple #days
! 2440588 +           ! 1970 offset in jdn unit
: jdn>ymd ( ??? -- y m d )
  1 -
  ! 32044 + ! complement of `32045 -`
  146097 1461 [ [ [ 4 * 3 + ] dip /i ] 2keep pick * 4 /i - ] bi-curry@ [ call ] dip call ! q: ( a k -- b c )
  ! .s: b d e
  [ 100 * ] ! complements 0.01*Y
  [ + ] [ ] tri* ! f e
  dup 5 * 2 + 153 /i ! .s: f e m
  [ [ 10 /i + ] [ 153 * 2 + 5 /i - 1 + ] bi-curry bi* ] keep
  [ ] [ 10 /i 12 * - 3 + ] bi
  swap ;

! ws helpers

! best to define a generic method so that method selection is implied by the input object
! both are used for websockets; websockets returns a byte-array, but often we want to parse it into json.
M: byte-array json> utf8 [ read-jsons get-json ] with-byte-reader ;
M: encoder disposed>> stream>> disposed>> ; inline ! send-websocket-bytes checks `output-stream get disposed>>`

: recv-json ( -- json ) read-websocket 2drop json> ; inline
: send-json ( json -- ) >json send-masked-message ; inline

! useful for debugging: `<URL> output-stream get [ [ >string print ] with-output-stream ] curry now 1 minutes time+ [ now <=> +gt+ = ] curry compose ws-client-loop` or
!                       `<URL> output-stream get [ [ >string print ] with-output-stream ] curry                     [ continue? get-global ] compose ws-client-loop`.
: ws-client-loop ( ... url q: ( ... msg -- ... continue? ) -- ... )
  '[ binary decode-input binary encode-output
     ! iirc, these user-specified handling of a subset of cases should be part of read-websocket-loop
     [ dup 1 =  ! can't put { 1 [ ... ] } in case b/c its quot contains a '@', and `case`, being a macro, doesn't support run-time computed quotations
       [ drop @ ]
       [ { { 9 [ drop t ] }
           { 8 [ \ ws-client-loop NOTICE log-message f ] } ! redundant f b/c read-websocket-loop requires `loop?` atop the stack
           { f [ ] } ! this branch is taken whenever the server disconnects either intentionally or by its process being killed. when intentional, we should get code 1006, with a reason string which may be empty. oddly, though, wscat still shows Disconnected (code: 1006, reason: "") when i kill-9 websocketd.
                     ! `read1` in `read-websocket` returns `f`, so `read-websocket` returns `f f f`.
           [ >dec "unexpected code & payload: " prepend 32 suffix prepend \ ws-client-loop NOTICE log-message t ]
         } case
       ] if
     ] read-websocket-loop
     f t 8 t send-websocket-bytes ! formally close connection
  ] [ <get-request> [ drop ] do-http-request ] dip [ with-stream ] 2curry ! TODO: gracefully handle when do-http-request returns not a duplex stream b/c upgrade-to-websocket? failed.
  "ws-client-loop" swap [ with-logging ] 2curry home swap with-log-root ; inline
