#!/usr/bin/env racket
#lang racket/base

(require (only-in racket/path path-get-extension file-name-from-path)
         (only-in racket/list last)
         racket/system
         (only-in racket/string string-prefix?))

(define o (open-output-string))
(parameterize ([current-output-port o])
  (for ([p (in-directory (build-path (find-system-path 'home-dir) ".termcolors")
                         (λ (p) (not (string-prefix? (path->string (last (explode-path p))) "."))))]
        #:when (and (file-exists? p) (let ([ext (path-get-extension p)]) (and ext (bytes=? #".conf" ext)))))
    (let ([s (path->string (file-name-from-path p))])
      (displayln (substring s 0 (- (string-length s) 5))))))

;; TODO: add binds to like, unlike, or delete the selected scheme
;; --preview-window ",0" for no window
(parameterize ([current-input-port (open-input-string (get-output-string o))])
  (void (system* (find-executable-path "fzf") "-e" "--preview" "settermcolor {}; colors" "--preview-window" "up,1")))
