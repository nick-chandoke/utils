#!/usr/bin/env racket
#lang racket/base
;; "run." more convenient & cleaner version of nohup
(require (only-in racket/system process*))
(module+ main
  (void (with-output-to-file "/dev/null" #:exists 'append
        (λ () (parameterize ([current-error-port (current-output-port)])
                            (apply process* (find-executable-path "nohup")
                                   (vector->list (current-command-line-arguments))))))))
