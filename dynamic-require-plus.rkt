#lang racket/base
;; NOTE: the module that requires this module must, if it uses #lang racket/base, (require (for-syntax racket/base))!
(require (for-syntax racket/base))
(provide dynamic-require+)
(define-syntax (dynamic-require+ stx)
  (syntax-case stx ()
    [(_ modpath fn ...)
     #'(begin (require (only-in racket/runtime-path define-runtime-module-path-index))
              (define-namespace-anchor anchor)
              (define-runtime-module-path-index m modpath)
              (define-values (fn ...)
                (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
                              (values (dynamic-require m (quote fn)) ...))))]))
