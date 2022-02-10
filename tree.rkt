#lang racket/base

;;; rose trees
;; NB. you'll often need to inst or ann things involving trees.
;; the type checker misses what appears to be plainly obvious, ordinary
;; type inference involving trees.

(provide (except-out (all-defined-out) pad-lvls))
(require (only-in racket/function curry identity const)
         (only-in racket/list last append-map)
         (only-in racket/format ~a)
         (only-in "util.rkt" map-but-last iterate-until))

;; all trees are proper lists that support common list ops.
;; for example, last applied to a tree returns the root node's last branch.
;; tree's monadic return (η) is `list`.
;(define-type (Tree a) (Pairof a (Listof (Tree a))))

;; monomorphic car on trees
;(: root (∀ (a) (-> (Tree a) a)))
(define root car)

;(: kids (∀ (a) (-> (Tree a) (Listof (Tree a)))))
(define kids cdr)

#| (: bfs (∀ (a) (-> (-> a Boolean) (Tree a) (Option (Tree a)))))
(: bfs+level (∀ (a) (-> (-> a Boolean) (Tree a) (Option (Pairof (Tree a) Natural)))))
(: map-tree (∀ (a b) (-> (-> a b) (Tree a) (Tree b))))
(: pp-tree (∀ (a) (->* ((Tree a)) ((-> a String)) String)))
(: pp-tree/string (-> (Tree String) String))
(: build-tree (∀ (a) (-> (-> a (Listof a)) a (Tree a))))
(: build-tree/map (∀ (s a) (-> (-> s a) (-> s (Listof s)) s (Tree a))))
(: build-pp-tree (∀ (s) (-> (-> s String) (-> s (Listof s)) s String)))
(: fold-tree (∀ (a b) (-> (-> a (Listof b) b) (Tree a) b)))
(: fold-last-branches (∀ (a b) (-> (-> (Tree a) b b) b (Tree a) b)))
(: leaves (∀ (a) (-> (Tree a) (Listof a))))
(: map-leaves (∀ (a) (-> (-> a a) (Tree a) (Tree a))))
(: bind-leaves (∀ (a) (-> (-> a (Tree a)) (Tree a) (Tree a))))
(: map-last-leaf (∀ (a) (-> (-> a a) (Tree a) (Tree a))))
(: tree-and-last-branch (∀ (a) (-> (Tree a) (Values (U (Tree a) Null) (Tree a))))) ; null if only one branch. does not recurse on tree. if first returned value is null, then tree has no children.
(: first-leaf (∀ (a) (-> (Tree a) a)))
(: last-leaf (∀ (a) (-> (Tree a) a)))
(: last-branch (∀ (a) (-> (Tree a) (Tree a))))
(: last-different-branch (∀ (a) (-> (Tree a) (Option (Tree a))))) ; returns #f if given a leaf. good for recursion.
(: rm-last-leaf (∀ (a) (-> (Tree a) (Tree a))))
(: with-last-branch (∀ (a) (-> (-> a (Listof (Tree a)) (Option (Tree a))) (Tree a) (Tree a))))
(: with-last-leaf (∀ (a) (-> (-> a (Option (Tree a))) (Tree a) (Tree a))))
(: depth (-> (Tree Any) Natural)) ; the number of levels in the tere. singleton has 1 depth. this agrees with the bfs+pos' position list's length when predicate matches root.
(: levels (∀ (a) (-> (Tree a) (Listof (Listof a))))) |#

;(: pad-lvls (-> Natural String))
(define (pad-lvls lvl) (let loop ([k 0]) (if (< k lvl) (string-append "│  " (loop (add1 k))) "")))

(define (map-tree f t) (fold-tree (λ (r bs) (cons (f r) bs)) t))

;; NEXT: [optional] if last child, do not print rightmost │; replace that character with a space.
;; to implement this, we'll probably need to add a last? flag to pad-lvls. it's optional ∵ continuing
;; the vertical bars makes level counting easy.
;; NOTE: ends with a newline character
(define (pp-tree t [f ~a])
  (string-append
   (f (car t))
   "\n"
   (let loop ([lvl 0] [ts (cdr t)])
        (apply string-append
               (map-but-last
                (λ (t) (string-append (pad-lvls lvl) "├─ " (f (car t)) "\n" (loop (add1 lvl) (cdr t))))
                (λ (t) (string-append (pad-lvls lvl) (if (null? (cdr t)) "└─ " "├─ ") (f (car t)) "\n" (loop (add1 lvl) (cdr t))))
                ts)))))

;; try (display (build-pp-tree number->string range 4))
(define (build-pp-tree ->str ->kids r) (pp-tree/string (build-tree/map ->str ->kids r)))

;; consider using with-last-branch before using tree-and-last-branch.
(define (tree-and-last-branch t)
  (if (null? (cdr t))
      (values null t)
      (let-values ([(a b) (let rec ; : (Values (Listof (Tree a)) ; branches
                                   ;           (Tree a)) ; last branch
                               ([branches (cdr t)]) ; listof trees
                               (if (null? (cdr branches))
                                   (values null (car branches))
                                   (let-values ([(a b) (rec (cdr branches))])
                                     (values (cons (car branches) a) b))))])
        (values (cons (car t) a) b))))

;; apply a function to the last branch that either removes it or replaces it with a different tree.
;; commonly this function will involve with-last-branch itself, to recurse on last branches.
;; the function is not applied to singleton trees. this and your function returning false are
;; with-last-branch's two base cases.
(define (with-last-branch f t)
  (let go ([t t])
       (let*-values ([(init-bs last-br) (tree-and-last-branch t)]
                     [(last-br/mod) (f (car last-br) (cdr last-br))])
         (if (null? init-bs)
             (or last-br/mod t)
             (cons (car init-bs)
                (if last-br/mod
                    (append (cdr init-bs) (list last-br/mod))
                    (cdr init-bs)))))))

;; recursively fold through the last branches of a tree,
;; starting with the root
(define (fold-last-branches f b0 t0)
  (let rec ([t t0] [b b0]) ; t is tree or #f
       (if t (rec (last-different-branch t) (f t b)) b)))

;; these functions modify only the last leaf, leaving the rest of the tree as-is. remove does dothing on singleton trees.
(define (with-last-leaf f t) (let rec ([t t]) (with-last-branch (λ (n #;a ks #;(Listof (Tree a))) (if (null? ks) (f n) (rec (cons n ks)))) t))) ; NB. (null? (cdr t)) => t is the last leaf.
(define (rm-last-leaf t) (with-last-leaf (const #f) t))
(define (map-last-leaf f t) (with-last-leaf (λ (l) (list (f l))) t))

(define (pp-tree/string t) (pp-tree t identity))
(define (build-tree ->kids r) (let go ([r r]) (cons r (map (λ (k) (go k)) (->kids r)))))
(define (build-tree/map ->value ->kids r) (let go ([r r]) (cons (->value r) (map (λ (k) (go k)) (->kids r)))))
(define (fold-tree f r) (let go ([r r]) (f (car r) (map go (cdr r)))))
(define (leaves r) (fold-tree (λ (r rs) (if (null? rs) (list r) (apply append rs))) r))
(define (map-leaves f r) (fold-tree (λ (r as) (if (null? as) (list (f r)) (cons r as))) r))
(define (bind-leaves k r) (fold-tree (λ (r as) (if (null? as) (k r) (cons r as))) r))

(define (last-branch t) (let ([bs (cdr t)]) (if (null? bs) t (last (cdr t)))))
(define (last-different-branch t) (let ([bs (cdr t)]) (if (null? bs) #f (last (cdr t)))))
(define (last-leaf t) (let ([bs (cdr t)]) (if (null? bs) (car t) (last-leaf (last bs)))))
(define (first-leaf t) (let ([bs (cdr t)]) (if (null? bs) (car t) (first-leaf (car bs)))))

(define (bfs+level p t)
  (let rec ([ts (list t)] [level 1])
    (if (null? ts) #f (let* ([t (car ts)]
                             [kids (cdr t)]
                             [found #;(Option (Tree a)) (findf (λ (t) (p (car t))) ts)])
                        (if found
                            (cons found level)
                            (rec (append-map cdr ts)
                                 (add1 level)))))))

(define (bfs p t) (let ([v (bfs+level p t)]) (and v (car v))))

(define (depth t) (fold-tree (λ (_ ns) (if (null? ns) 1 (add1 (apply max ns)))) t))

;; copied from https://hackage.haskell.org/package/containers-0.6.5.1/docs/src/Data.Tree.html#levels
(define (levels t)
  (map (λ (t) (map car t))
       (iterate-until null?
                      (λ (ts) (append-map cdr ts))
                      (list t))))
