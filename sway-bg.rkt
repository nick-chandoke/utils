#!/usr/bin/env racket
#lang racket/base
;;; invoked by sway-bg (shell script)
(require racket/cmdline
         racket/match
         (rename-in racket/system [system* S])
         (only-in racket/path path-get-extension)
         (only-in racket/function curry const)
         (only-in racket/list take shuffle)
         (only-in racket/string string-split string-trim))
(define (system* x . X) (apply S (find-executable-path x) X))
(command-line
 #:args (output file)
 ;; kill running swaybg's for selected outputs
 (define in-outs? (case output [("stacked") (λ (x) (member x '("eDP-1" "HDMI-A-1")))] [else (curry string=? output)]))
 (for-each (λ (r) (system* "kill" (car r)))
             (filter (match-lambda [(list _ "swaybg" cmd) (in-outs? (caddr (string-split cmd)))] ; for caddr to match properly, -o must be the first option specified in cmd
                                   [else #f])
           (parameterize ([current-output-port (open-output-string)])
             (void (system* "ps" "-eo" "%p≣%c≣%a")) ; ≣ as sep b/c unlikely to ever appear in a cmdline or prog name
             (begin0 (cdr (map (λ (l) (map string-trim (string-split l "≣"))) (string-split (get-output-string (current-output-port)) "\n")))
                     (close-output-port (current-output-port))))))
 ;; special-case: make imgs for upper & lower monitors, then print
 ;; lines for sway-bg to eval.
 (void (when (string=? output "stacked")
         (define ext (path-get-extension file))
         (define common-path (path-add-extension "/dev/shm/stacked" ext))
         (define hdmia1-path (path-add-extension "/dev/shm/hdmia1" ext))
         (define edp1-path (path-add-extension "/dev/shm/edp1" ext))
         (system* "cp" file common-path)
         (system* "magick" "mogrify" "-resize" "x2160" common-path)
         (system* "magick" "mogrify" "-resize" "1920" common-path)
         (system* "cp" common-path hdmia1-path)
         (system* "cp" common-path edp1-path)
         (system* "magick" "mogrify" "-extract" "1920x1080+0+0" hdmia1-path)
         (system* "magick" "mogrify" "-extract" "1920x1080+0+1080" edp1-path)
         (printf "nohup swaybg -o HDMI-A-1 -i ~a &>/dev/null &~n" hdmia1-path)
         (printf "nohup swaybg -o eDP-1 -i ~a &>/dev/null &~n" edp1-path))))
