#lang racket
(require racket/cmdline
         (for-syntax syntax/parse))
(provide command-line/map)

#| e.g.
(parameterize ([current-command-line-arguments #("-fF" "3" "potato" "tomato")])
  (command-line/map [f flag] 0 name [F find]))
returns '(#hash((find . "3") (flag . #t)) "potato" "tomato").
|#
(define-syntax (command-line/map stx)
  (syntax-parse
      stx
    [(_ (~seq k (~optional (~and flag? 0) #:defaults ([flag? #f]))) ...)
     #:with ((ks kl) ...) (map (λ (s)
                                 (let ([k (syntax-e s)])
                                   (if (list? k)
                                       s
                                       (let ([k (symbol->string k)])
                                         #`(#,(datum->syntax s (string->symbol (substring k 0 1))) #,s)))))
                               (attribute k))
     #:with (switches ...) (map (λ (ss sl)
                                  #`(#,(string-append "-"  (symbol->string (syntax-e ss)))
                                     #,(string-append "--" (symbol->string (syntax-e sl)))))
                                (attribute ks)
                                (attribute kl))
     #:with (clauses ...) (map (λ (f? switches kl)
                                 (if f?
                                     #`[#,switches   "" (hash-set! opts (quote #,kl) #t)]
                                     #`[#,switches x "" (hash-set! opts (quote #,kl) x)]))
                               (attribute flag?)
                               (attribute switches)
                               (syntax-e #'(kl ...)))
     #`(let ([opts (make-hash)])
         (command-line
          #:once-each clauses ...
          #:args args
          (cons opts args)))]))
