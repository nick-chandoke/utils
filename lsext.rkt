#!/usr/bin/env racket
#lang typed/racket/base

(require (only-in racket/file fold-files)
         (only-in racket/path path-get-extension)
         (only-in racket/cmdline parse-command-line)
         (only-in racket/set set set-empty? set-add list->set set->list set-member?))

(module+ main
  (define recursive : Boolean #f)
  (define dir : Path (current-directory))
  (define follow-links? : Boolean #f)
  (define exts : (Setof String)
    ((inst parse-command-line (Setof String))
     (find-system-path 'run-file)
     (current-command-line-arguments)
     `((usage-help "if you provide no extensions, then lsext will list all extensions in the directory; if you give extensions, then it'll list all files matching any of the provided extensions. DO NOT PREFIX THE EXTENSION WITH A PERIOD.")
       (once-each [("-r" "--recursive")
                   ,(λ (_) (set! recursive #t))
                   (("recurse through directory") . ())]
                  [("-d" "--dir")
                   ,(λ (flag [d : String]) (set! dir (string->path d)))
                   (("set directory to a value other than current working directory") . ("d"))]
                  [("--follow-links")
                   ,(λ (_) (set! follow-links? #t))
                   (("") . ())]))
     (λ (_ . extensions) (list->set extensions))
     '("extensions")))

  (unless (directory-exists? dir)
    (printf "directory ~a does not exist~n" dir)
    (exit 1))

  ;; can't believe there're no sorted sets (i.e. treesets rather than hashsets)
  ;; so that we're already sorted on every insert :o
  ;; sorting and converting to list is O(2n)!
  (for ([path (sort
               (set->list ((inst fold-files (Setof String))
                           (λ ([path : Path] [type : (U 'file 'dir 'link)] [s : (Setof String)])
                             (let* ([fext (path-get-extension path)]
                                    [fext-str (and fext (string-downcase (bytes->string/utf-8 (subbytes fext 1))))])
                               (values
                                (if (and fext-str (eq? type 'file))
                                    (cond [(set-empty? exts) (set-add s fext-str)]
                                          [(and fext-str (set-member? exts fext-str)) (set-add s (path->string path))]
                                          [else s])
                                    s)
                                (and (member 'read (file-or-directory-permissions path)) (or recursive (eq? dir path))))))
                           (set)
                           dir
                           follow-links?))
               string<?)])
    (displayln path)))
