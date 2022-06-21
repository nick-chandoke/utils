#!/usr/bin/env racket
#lang racket/base
;; "run." more convenient & cleaner version of nohup
;; TODO: why does not work with swaybg (or battery-monitor)? currently no effect. using system* instead of process* affects
;; but is not a bg proc; ^C kills. 
(require (only-in racket/system process*) (only-in racket/port open-output-nowhere))
(module+ main
  (void (parameterize ([current-output-port (open-output-nowhere)]
                       [current-error-port (open-output-nowhere)])
                            (apply process* (find-executable-path "nohup")
                                   (vector->list (current-command-line-arguments))))))
