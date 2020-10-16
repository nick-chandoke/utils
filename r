#!/usr/bin/env racket
#lang racket/base
(require (only-in racket/system system))
(module+ main (system (format "nohup ~a &>/dev/null &" (vector-ref (current-command-line-arguments) 0))))
