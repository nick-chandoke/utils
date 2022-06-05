#lang racket/base

(provide (all-defined-out))

#| TODO's:
   * code currently assumes bourne shell (&c e.g. bash, tsh, csh); i can use system* and piping in racket to be shell-independent
     ** not only that, but if user's prompt answers have spaces, then they could screw-up some stuff!
   * ^C during an interactive prompt (e.g. passwd) has odd behavior; the proper action is passing SIGINT to the child proc.
   * package this with the actual arch flash drive (which would require the arch img to not be iso9660, RIGHT?) and have this script run on login.
   * introduce partitioning tool?
   * change sudoers file to allow wheel to use sudo
   * modify /etc/systemd/system/*wpa_supplicant*.service to add -i & -c flags to the ExecStart line
|#

#| NOTES:
   * how to install boot loader for ntfs /boot on mbr with uefi? it seems that, despite booting with uefi, mbr implies that we must install per bios install instructions.
     ** indeed, if mbr, then ensure that you're booting in bios—not uefi—mode
   see <https://wiki.archlinux.org/title/Arch_boot_process#Under_UEFI>
|#

(require (only-in racket/string string-prefix? string-contains? string-join string-split string-trim)
         (only-in racket/function identity)
         (only-in racket/port port->lines)
         (only-in racket/bool symbol=?)
         (only-in racket/format ~a)
         (only-in racket/file fold-files copy-directory/files)
         (only-in racket/system system system/exit-code)
         (only-in racket/list append-map make-list split-at split-at-right drop filter-map flatten last drop-right)
         (only-in racket/path path-get-extension file-name-from-path path-has-extension?)
         json
         (for-syntax racket/base syntax/parse))

#| the term "action" shall refer to a procedure that returns a non-zero integer
   or a string to represent an error (non-zero exit code or error message), and
   whose any other return value connotes an acceptable outcome, including aborting.

* remember to disable secure boot (in your motherboard's uefi) before booting the arch live image!
|#

;; PRETTY PRINT HELPER FUNCTIONS

;; like in-slice (in racket/sequence) but supports padding the last slice.
;; if pad, then the last row will be padded on the right so that it's the given length
(define (chunks-of i s #:pad [pad #f])
  (let loop ([s s])
    (if (< (length s) i)
        (list (if pad
                  (append s (make-list (- i (length s)) pad))
                  s))
        (let-values ([(a b) (split-at s i)])
          (if (null? b)
              (list a)
              (cons a (loop b)))))))

;; prints a table of data (converted to strings by ~a) with columns aligned
(define (pp-table xss)
  (if (null? xss)
      (void)
      (let ([col-sizes (apply foldr (λ X (let-values ([(ss acc) (split-at-right X 1)]) (cons (foldl (λ (s m) (max (string-length (~a s)) m)) 0 ss) (car acc)))) '() xss)])
        (for-each (λ (xs) (for-each (λ (x s) (let ([x (~a x)]) (printf "~a~a " x (make-string (- s (string-length x)) #\space)))) xs col-sizes) (printf "~n")) xss))))

;; guaranteed to have no more than the given number of columns;
;; on certain edge cases there may be one fewer than the specified number of columns.
(define (print-choices choices [cols 5])
  (pp-table (chunks-of cols (for/list ([k (in-naturals 1)] [i choices]) (format "~a. ~a" k i)) #:pad "")))

;; OTHER HELPER FUNCTIONS

;; lps : ((string . pred)).
;; returns (values matched-items unmatched-lps)
;; e.g. (find-multiple (range 10) `(("=5" . ,(curry = 5)) ("|2" . ,even?) (">40" . ,(λ (x) (> x 40)))))
;; returns (values (set '("|2" . 6) '("=5" . 5)) '((">40" . #<procedure>)))
;; UNUSED; the code that used it was discarded.
#;(define (find-multiple xs lps)
  (let loop ([matched (set)] [lps lps] [xs xs])
    (cond [(null? lps) (values matched lps)]
          [(null? xs) (values matched lps)]
          [else (if ((cdar lps) (car xs))
                    (loop (set-add matched (cons (caar lps) (car xs))) (cdr lps) (cdr xs))
                    (loop matched lps (cdr xs)))])))

;; PROMPT FUNCTIONS

;; select one/some from many displayed options
;; returns #f if no choice taken, as signaled by ^C
;; if #:return-index? #t, then returns the index of the selected item rather
;; than the value of the item itself.
(define (select prompt choices [print-choices print-choices] #:return-index? [return-index? #f])
  (let ([m (for/hash ([k (in-naturals 1)] [c choices]) (values k c))])
    (printf "~a~n~n" prompt)
    (print-choices choices)
    (printf "~nselect a choice by number: ")
    (with-handlers ([exn:break? (λ (_) (printf "\n") #f)])
      (let loop ()
        (let ([index (let ([s (read-line)]) (if (eof-object? s) s (string->number s)))])
          (cond [(eof-object? index) #f] ;; treat ^D as ^C
                [index (let ([selection (hash-ref m index #f)])
                         (if selection
                             (if return-index?
                                 index
                                 selection)
                             (begin (displayln "invalid index. try again." (current-error-port))
                                    (loop))))]
              [else (begin (displayln "please enter a number." (current-error-port))
                           (loop))]))))))

;; select a text input (single line)
;; callback is an action that accepts the user's input string
;; prompt is itself an action
(define ((prompt p callback [default #f]))
  (display p)
  (with-handlers ([exn:break? (λ (_) (printf "\n") #f)])
    (callback (let ([l (read-line)])
              (if (string=? "" l)
                  default
                  l)))))

;; list of filesystem objects that descend from a given directory and optionally match
;; a given predicate. given directory is not included in returned list.
;; return value: list of paths represented by strings; these strings do not begin with
;; the argument directory's path, e.g. (descendants "path/to/dir") would return '("A.doc")
;; instead of "path/to/dir/A.doc". this behavior was chosen particularly for using
;; descendants with `select`.
;; t is of '(file dir link)
;; if #:basename? #t then only basename is put into list rather than path relative to argument directory.
(define (descendants d [pred (λ (p t) #t)] #:basename? [basename? #f])
  (unless (directory-exists? d)
    (error (format "[ERROR] invalid assumption; actually ~a does NOT exist" d)))
  (let ([wl (string-length d)])
    (fold-files (λ (p t l)
                      (if (pred p t)
                          ;; wp is path relative to d
                          (let ([wp (substring (path->string p) wl)])
                            (if (string=? "" wp)
                                l
                                (cons (if basename?
                                          (path->string (file-name-from-path p))
                                          (substring wp 1)) l)))
                          l))
                '()
                d)))

;; APPLICATION-SPECIFIC FUNCTIONS

;; convenient/terse action definition.
;; errmsg is printed in addition to the generic error message if sys0 returns non-0 exit code.
;; if errmsg is a string, then that string is returned; if it's not, then the string "<cmd> failed" is returned.
;; if print-err?, error string is printed.
(define ((sys0 cmd [errmsg #f] #:print-err? [print-err? #f] #:required? [required? #f]))
  (with-handlers ([exn:break? (λ (_) (if required? #f 0))])
    (if (= 0 (system/exit-code cmd))
        0
        ((if print-err? displayln identity) (if (string? errmsg)
                                                errmsg
                                                (format "`~a` failed." cmd))))))

;; shorts when any optionally-bound item in sequence is a non-0 integer;
;; upon such an integer, 0-let*/seq returns that value.
;; e.g. (0-let*/seq [x <- 4.6] [y <- (+ x 5.0)] (displayln "here") [z <- (if (even? (exact-floor y)) 1 0)] (displayln "finished"))
;; prints "here", then "finished", and returns 0; if x were change to 4, then evaluation would stop and 4 would be returned.
(define-syntax (0-let*/seq stx)
  (syntax-parse stx #:datum-literals (<-)
    [(_) #'0] ;; everything was done successfully; return 0.
    [(_ (~or [i:id <- e] e) xs ...)
     (if (attribute i)
         #'(let ([i e]) (if (or (and (exact-integer? i) (not (= 0 i))) (string? i)) i (0-let*/seq xs ...)))
         #'(let ([E e]) (if (or (and (exact-integer? E) (not (= 0 E))) (string? E)) E (0-let*/seq xs ...))))]))

(define (write-file path str) (with-output-to-file #:exists 'truncate/replace path (λ _ (display str))))

(define (read-proc cmdline)
  (let ([os (open-output-bytes)] [os-err (open-output-bytes)])
    (parameterize ([current-output-port os] [current-error-port os-err])
      (let ([ec (system/exit-code cmdline)])
        (if (= 0 ec)
            (get-output-string os)
            `(,ec . ,(get-output-string os-err)))))))

;; like ((sys0 "mount ...")) but returns 0 if device already mounted at given mountpoint
(define (mount dev mp)
  (let ([os (open-output-bytes)])
    (parameterize ([current-error-port os])
      (let* ([ec (system/exit-code (format "mount /dev/~a ~a" dev mp))]
             [errout (get-output-string os)])
        (cond [(= 0 ec) 0]
              [(string-contains? errout "already mounted") 0]
              [else errout])))))

(define (setup-wifi)
  (0-let*/seq 
   ;; check that device is up: not soft blocked nor hard blocked, then ip set link up
   (if (find-executable-path "rfkill")
       (0-let*/seq ((sys0 "rfkill unblock wifi"))
                   (let ([y (filter/json (λ (x) (and (hash? x) (equal? "wlan" (hash-ref x 'type #f))))
                                         (string->jsexpr (read-proc "rfkill -Jo type,hard")))])
                     (if (null? y)
                         (displayln "no wifi device found (rfkill(8) didn't provide a device with type 'wlan'); cannot guarantee that any wifi device is not hard-blocked")
                         (let ([hard-blocked-devs (filter (λ (h) (equal? "blocked" (hash-ref h 'hard #f))) y)])
                           (unless (null? hard-blocked-devs)
                             (printf "the following devices are hard-blocked: ~a; if any of these is the wifi device that you want to use, please unblock it, then press <enter> to continue.~n" hard-blocked-devs)
                                   (void (read-line)))))))
       (displayln "[INFO] rfkill is not installed / in PATH. cannot guarantee that wifi is neither hard- nor soft-blocked."))
   (let* ([wifi-dev (let* ([devs (string->jsexpr (read-proc "ip -j link show"))] ;; assuming (not (list? devs)) 'cause i'm lazy and it'll probably work.
                           [ws (filter (λ (h) (string-prefix? (hash-ref h 'ifname) "w")) devs)])
                      (cond [(= 1 (length ws)) (let ([ifn (hash-ref (car ws) 'ifname)])
                                                 (printf "[INFO] assuming ~a as the wifi card~n" ifn)
                                                 (list ifn))]
                            [(null? devs) "no network devices detected! aborting."]
                            ;; if #ws > 1 then select from assumedly-wireless devices
                            [else (let ([sdev (select "select wireless device:" (map (λ (h) (hash-ref h 'ifname)) (if (null? ws) devs ws)))])
                                    (if sdev (list sdev) "no device selected. aborting network config."))]))]
          ;; TODO: mkdir /mnt/etc/ if needed before trying to write to /mnt/etc/wpa_supplicant.conf
          [start-wpa-supplicant (sys0 (format "wpa_supplicant -B -i ~a -c /mnt/etc/wpa_supplicant.conf" (if (pair? wifi-dev) (car wifi-dev) 'dummy)))])
     ;; connect to network; use /mnt/etc/wpa_supplicant.conf if available; else generate that file by connecting to a network
     (if (string? wifi-dev)
         wifi-dev
         ;; else wifi-dev is a singleton list
         (0-let*/seq ((sys0 (format "ip link set ~a up" (car wifi-dev))))
                     [scan-results <- (let rec ()
                                        (let ([v (read-proc (format "iw ~a scan" (car wifi-dev)))])
                                          (if (pair? v)
                                              (if (= -16 (car v))
                                                  (begin (displayln "device busy. retrying...")
                                                         (sleep 2)
                                                         (rec))
                                                  (cdr v))
                                              (list v))))]
                             (if (find-executable-path "wpa_supplicant")
                                 (begin (if (file-exists? "/mnt/etc/wpa_supplicant.conf")
                                            (displayln "using config from /mnt/etc/wpa_supplicant.conf.")
                                            (write-file "/mnt/etc/wpa_supplicant.conf"
                                                        (read-proc (string-append "wpa_passphrase "
                                                                                  (select "select a wireless network to connect to:"
                                                                                          ;; yes, screenscraping iw(8) despite it explicitly saying not to do so. currently
                                                                                          ;; no iw library nor json output is available, though it's planned; see
                                                                                          ;; <https://www.spinics.net/lists/linux-wireless/msg202689.html>.
                                                                                          ;; the source for iw/scanning is too complex to make a wrapper for it; see
                                                                                          ;; <https://fossies.org/linux/iw/scan.c>.
                                                                                          (filter-map (λ (s) (let ([m (regexp-match #px"SSID: (.+)$" s)])
                                                                                                               (and m (cadr m))))
                                                                                                      (string-split (car scan-results) "\n")))))))
                                        (start-wpa-supplicant))
                                 "wpa supplicant is not installed. idk how to connect to any network without it.")))) 
   ;; finally, test for internet connectivity
   ((sys0 "ping -c 1 archlinux.org" "can't ping archlinux.org"))))

(define (fold-json f j)
  (let go ([j j])
    (f j (map go (cond [(hash? j) (hash-values j)]
                       [(list? j) j]
                       [else '()])))))

;; inefficient (uses `flatten`) tree search
(define (filter/json p j) (fold-json (λ (r acc) (if (p r) r (flatten acc))) j))

(define (mkdir d) (unless (directory-exists? d) (make-directory d)))

;; MAIN PROGRAM

;; TODO: if fstype = null, then suggest formatting it via mkfs.btrfs or mkfs.ext4.
;; get fstype from fdisk -l; lsblk has said null whereas fdisk said "Linux"; still i
;; want more specific description e.g. ext4 or xfs.
(define ((root/home-sanity-check expected-amt) dev)
  (let* ([size-str (hash-ref dev 'size)]
         [class (last (string->list size-str))]
         [amt (string->number (cadr (regexp-match #px"([0-9]+).*" size-str)))]
         [expected-fstypes '("btrfs" "ext4" "jfs" "xfs" "zfs")])
    (foldl (λ (c s acc) (if c acc (cons s acc)))
           '()
           `(,(or (and (eq? #\G class) (>= amt expected-amt)) (eq? #\T class))
             ,(member (hash-ref dev 'fstype) expected-fstypes))
           `("less than 16GB"
             ,(format "unusual filesystem; expected one of ~a. (zfs highly recommended, for the tech-savvy)"
                      expected-fstypes)))))

;; block devices (including descendant partitions) minus
;; the live system's block device.
(define (lsblk)
  (filter/json (λ (h) (and (hash? h)
                           (equal? "disk" (hash-ref h 'type #f))
                           (not (memf (λ (p) (and (string? p) (string-prefix? p "/run")))
                                      (hash-ref h 'mountpoints)))))
               (string->jsexpr (read-proc "lsblk -Jo name,mountpoints,type,size,fstype"))))

;; collects all partitions from a tree of block devices.
;; NOTE: each partition's 'mountpoints property is, if empty, properly '() instead of '(null).
(define (parts [block-devs (lsblk)])
  (append-map (λ (d) (for/list ([x (hash-ref d 'children)]
                                #:when (and (hash? x) (equal? "part" (hash-ref x 'type #f))))
                       (hash-update x 'mountpoints (λ (mps) (if (equal? '(null) mps) '() mps)))))
              block-devs))

(define (part-names [ps (parts)]) (map (λ (x) (hash-ref x 'name)) ps))

(define (display-partitions ps)
  (pp-table (cons '("#" "NAME" "SIZE" "FSTYPE" "MOUNTPOINT(S)")
                  (for/list ([h ps] [k (in-naturals 1)])
                    (cons (format "~a." k)
                          (append (map (λ (k) (hash-ref h k)) '(name size fstype))
                                  `(,(string-join (hash-ref h 'mountpoints) ", ")))))))
  (printf "~n"))

(define (get-block-device)
  (let ([block-dev-ns (map (λ (x) (hash-ref x 'name)) (lsblk))])
    (if (null? (cdr block-dev-ns))
        (car block-dev-ns)
        (select "select a block device:" block-dev-ns))))

;; prepare for working in the live environment; useful when we fail to boot properly,
;; and need to tinker with bootloader in live environment.
(define stage/init
  `(("verify uefi boot mode" . ,(λ _ (or (directory-exists? "/sys/firmware/efi/efivars")
                                           "efivars missing; please check that you're booting in uefi mode")))
    ("select keyboard layout" . ,(λ _ (let ([selection (select "select keyboard layout (or ^C to keep present one): "
                                                               (map (λ (p) (substring p 0 (- (string-length p) 7))) ; 7 = #".map.gz"
                                                                    ;; TODO (2):
                                                                    ;; 1. somewhy, specifically here for /usr/share/kbd/keymaps, the 1st char of
                                                                    ;;    the actually correct output is missing.
                                                                    ;; 2. there're 1855 choices. even with 5 cols (which takes the whole screen width)
                                                                    ;;    we need paging; use fzf if possible, or otherwise allow user to filter
                                                                    ;;    results.
                                                                    (descendants #:basename? #t
                                                                                 "/usr/share/kbd/keymaps"
                                                                                 (λ (p _) (path-has-extension? p #".map.gz")))))])
                                       (if selection
                                           ((sys0 (string-append "loadkeys " selection)))
                                           (displayln "keeping current layout")))))
    #| * mkfs.fat -F 32 <esp>. if prints warning about "not enough clusters," add -s2 or -s1; see mkfs.fat(8) for supported cluster sizes
         * if esp size is less than 32MiB, mkfs.fat -F 16 or -F 12 (12 is needed for 2MiB esp)
    
    TODO: if fdisk modifies, then call "fioctl" or w/e that command is; otherwise the immediately following lsblk will say that all devices'
    fstypes are null, which is untrue and very alarming. as mentioned elsewhere, fdisk -l should be used to determine fstypes, not lsblk.
    |#
    ("check that block devices have been partitioned" . ,(prompt (format "here are your block devices:~n~n~a~n. might you need to partition? (y/n) "
                                                                         (read-proc "lsblk")) ; TODO: lsblk lists boot media mounted at /run. this should not be listed. this output, like any lsblk output, should list disk fstype. also, there should be a blank line between each disk, e.g. /dev/sda should get its own paragraph, as should /dev/sdb &c. again, use fdisk, not lsblk, to get fstypes, although again, fdisk -l does not (at least by default) tell specifically the fstype; it just says e.g. "Linux", though it DOES give the "Id" column, which seems to be an identifier for the fstype. i can easily define a map here from (id : Integer) to (type : String). also don't display parts : part.Type=Extended [fdisk -l]. TODO: does presence of Extended imply MBR? does GPT have distinguished logical/physical partitions?
                                                                (λ (ans)
                                                                 (when (string=? "y" ans)
                                                                   (0-let*/seq [bdev <- (list (get-block-device))] ; car b/c g.b.d. rets string, which would be seen as error msg; wrap in list to avoid this.
                                                                               ((sys0 (string-append "fdisk /dev/" (car bdev)))))))))
    ("ensure proper mounting" . ,(λ _
                                  (let ([parts (parts)]
                                        [fstab-mnt? #f]) ;; after mounting root, mount according to /mnt/etc/fstab if it exists; fstab-mnt? breaks loop if #t
                                    ;; have user select partitions to mount. before actually mounting, do a little sanity check that the selected drive is appropriate.
                                    (for ([part '("ROOT" "BOOT" "SWAP" "HOME")] ;; home included b/c it'll be added when we write genfstab
                                          [mandatory? '(#t #t #f #f)]
                                          [mountcmd `(,(λ (p) (mount (hash-ref p 'name) "/mnt"))
                                                      ,(λ (p) (mount (hash-ref p 'name) "/mnt/boot"))
                                                      ,(λ (p) (0-let*/seq
                                                               (when (and (not (string-contains? "swap" (string-downcase (hash-ref p 'fstype)))) ; mountpoint is "[SWAP]"; fstype is "swap"
                                                                          ;; TODO: account for select returning #f
                                                                          (= 1 (select (format "to mount ~a as swap, it MUST have a swap filesystem type. it currently has type ~a. should i:" (hash-ref p 'name) (hash-ref p 'fstype))
                                                                                  '("format it as swap then mount it, or"
                                                                                    "skip mounting it?")
                                                                                  #:return-index? #t)))
                                                                 ((sys0 ("mkswap /dev/~a"))))
                                                               ((sys0 (format "swapon /dev/~a" p)))))
                                                      ,(λ (p) (mount (hash-ref p 'name) "/mnt/home")))]
                                          ;; TODO: do these checks hold with lvm, raid, and/or encryption?
                                          [sanity-check `(,(root/home-sanity-check 16) ; root
                                                          ;; boot
                                                          ,(λ (dev) (let* ([size-str (hash-ref dev 'size)]
                                                                           [class (last (string->list size-str))]
                                                                           [amt (string->number (cadr (regexp-match #px"([0-9]+).*" size-str)))])
                                                                      (foldl (λ (c s acc) (if c acc (cons s acc)))
                                                                             '()
                                                                             `(,(or (and (eq? #\G class) (= amt 1)) (and (eq? #\M class) (>= amt 300)))
                                                                               ,(string=? "vfat" (hash-ref dev 'fstype)))
                                                                             '("outside [300MB, 1GB]" ;; TODO: apparently 2MiB should suffice for...modern bootloaders?
                                                                                                      ;; see <https://wiki.archlinux.org/title/EFI_system_partition#Create_the_partition>
                                                                               "not a FAT filesystem"))))
                                                          ;; swap
                                                          ,(λ (dev) (let* ([size-str (hash-ref dev 'size)]
                                                                           [class (last (string->list size-str))]
                                                                           [amt (string->number (cadr (regexp-match #px"([0-9]+).*" size-str)))])
                                                                      (foldl (λ (c s acc) (if c acc (cons s acc)))
                                                                             '()
                                                                             `(,(string=? "[SWAP]" (hash-ref dev 'fstype))
                                                                               ,(or (and (eq? #\G class) (>= amt 4))))
                                                                             '("not a swap partition"
                                                                               "less than 4GB"))))
                                                          ;; home
                                                          ,(root/home-sanity-check 1))])
                                         #:break fstab-mnt?
                                         ;; /mnt already exists when we boot arch live. AFTER mounting root to /mnt,
                                         ;; mkdri /mnt/{boot,home} ON THE ROOT PARTITION.
                                         (when (string=? "BOOT" part)
                                            (for-each mkdir '("/mnt/boot" "/mnt/home")))
                                      (let ret ()
                                        ;; TODO: do not prompt to mount if already mounted
                                        (let ([selection (select (format "select your ~a partition: " part) parts display-partitions)])
                                          (if selection
                                              (let ([concerns (sanity-check selection)])
                                                (unless (null? concerns)
                                                  (printf "~n")
                                                  ((prompt (format "~a has the following oddities:~n~n~a~n are you sure that this is the ~a partiton? (y/n) "
                                                                   (hash-ref selection 'name)
                                                                   (foldl (λ (s acc) (string-append acc "* " s "\n")) "" concerns)
                                                                   part)
                                                           (λ (resp) (if (string=? "y" resp)
                                                                         (0-let*/seq (mountcmd selection)
                                                                                     (when (file-exists? "/mnt/etc/fstab")
                                                                                       (set! fstab-mnt? #t)))
                                                                         (ret)))))))
                                              (if mandatory?
                                                  (begin ((prompt "\nthis is a required partition. exit the program, or select a different partition? (exit/select) "
                                                                  (λ (ans) (if (string=? "exit" ans)
                                                                               -1
                                                                               (ret)))))
                                                         (ret))
                                                  (displayln "skipping this partition"))))))
                                    (when fstab-mnt?
                                      (displayln "i see that you've already gone through some of the install process. run:\n\n* arch-chroot /mnt\n* install-arch STAGE2\n\nthen carry on." )))))
    ("setup wifi" . ,setup-wifi)))

;; set arch options (e.g. enable/configure services, set basic admin stuff)
(define stage/config
  `(("set clock to ntp"   . ,(sys0 "timedatectl set-ntp true"))
    ("set mirrorlist"     . ,(sys0 "reflector --latest 30 --sort rate --save /etc/pacman.d/mirrorlist" "reflector failed to get top 30 package-set mirrors"))
    ;; dl yay-bin at https://aur.archlinux.org/cgit/aur.git/snapshot/yay-bin.tar.gz; then cd to extracted place, makepkg, then pacman -U <smth>
    ("install packages"    . ,(sys0 (string-append "pacstrap /mnt "
                                                     (foldl (λ (line acc)
                                                              (let ([new-stuff (string-trim (cadr (regexp-match #px"([^#]*)#?" line)))])
                                                                (if (string=? new-stuff "")
                                                                    acc
                                                                    (string-append acc new-stuff " "))))
                                                            ""
                                                            (with-input-from-file "./arch-packages.txt" port->lines)))))
    ("genfstab"               . ,(sys0 "genfstab -U /mnt >> /mnt/etc/fstab"))
    ("copy to new root" ,(λ _ (let ([portable-dir-path (apply build-path (drop-right (explode-path (path->string (resolved-module-path-name (module-path-index-resolve (syntax-source-module #'0))))) 2))])
                                (copy-directory/files portable-dir-path (build-path "/mnt/" portable-dir-path)))))
    ("arch-chroot /mnt, then continue running arch-install from the new root")
    ("set time zone"      . ,(λ _ (let ([tz (select "select your time zone: " (descendants "/usr/share/zoneinfo/"))])
                                       (when tz ((sys0 (format "ln -sf /usr/share/zoneinfo/~a /etc/localtime" tz)))))))
    ("set hardware clock" . ,(sys0 "hwclock --systohc"))
    ; ("editing /etc/locale.gen")
    ("generate locales"     . ,(λ _ (0-let*/seq (write-file "/etc/locale.gen" "en_US.UTF-8 UTF-8\n") ((sys0 "locale-gen")))))
    ("set language"       . ,(prompt "language code [en_US.UTF-8]: " (λ (lang) (write-file "/etc/locale.conf" (string-append "LANG=" lang))) "en_US.UTF-8"))
    ;; TODO: identify how to generate list of acceptable console keymaps, then use `select` instead of `prompt`
    ;; see <https://wiki.archlinux.org/title/Linux_console/Keyboard_configuration#Listing_keymaps>
    ("set console keyboard layout" . ,(prompt "set console keymap (or ^C to skip): "
                                              (λ (km) (if km
                                                          (write-file "/etc/vconsole.conf" (string-append "KEYMAP=" km))
                                                          (displayln "skipping.")))))
    ("set hostname" . ,(prompt "hostname: " (λ (hostname) (if hostname (write-file "/etc/hostname" hostname) "no hostname given"))))
    ;; TODO?: if lvm, sysenc, or raid, update mkinitcpio.conf(5) then run mkinitcpio -P
    ("set the root password" . ,(sys0 "passwd"))
    ;; not putting dhcpcd is sys0 b/c they will fail if user hasn't installed dhcpcd, but are practically guaranteed to succeed if they have.
    ("configure network" . ,(λ _ (system "systemctl enable dhcpcd") ;; needed only after arch-chroot
                                 (system "systemctl start dhcpcd")
                                 (when (find-executable-path "wpa_supplicant")
                                   (system "systemctl enable wpa_supplicant"))))
    ("create user" . ,(prompt "username: " (λ (user) (and user
                                                            (0-let*/seq ((sys0 (format "useradd -G wheel -d /home/~a -m -s /bin/bash ~a" user user)
                                                                               (format "couldn't create user.")))
                                                                        ((sys0 (string-append "passwd " user))))))))
    ("run arch-install boot")))

;; returns a hash map
(define (get-boot-partition [parts (parts)])
  (findf (λ (x) (member "/boot" (hash-ref x 'mountpoints))) parts))

;; returns a string
(define (get-boot-block-device [boot-partition (get-boot-partition)])
  (let* ([boot-part-name (hash-ref boot-partition 'name)]
         [boot-dev (filter/json (λ (j)
                                  (and (hash? j)
                                       (let ([cs (hash-ref j 'children #f)])
                                         (and cs
                                              (memf (λ (x) (and (hash? x)
                                                                (equal? boot-part-name (hash-ref x 'name #f))))
                                                    cs)))))
                                (lsblk))])
    (if (null? boot-dev)
        (error "[BUG] failed to identify block device of partition mounted at /boot")
        (hash-ref (car boot-dev) 'name))))

;; efi-exe-path's backslashes must be doubled, e.g. (efibootmgr/add "Linux" "\\EFI\\systemd\\...");
;; this is racket syntax for specifying backslashes in strings. "\" is invalid syntax. (print "\\") prints like "\\",
;; but (display "\\") prints \.
(define (efibootmgr/add name efi-exe-path)
  (system/exit-code (format "efibootmgr --create --disk /dev/~a --part ~a --loader '~a' --label '~a' --verbose"
                            (get-boot-block-device)
                            (hash-ref (get-boot-partition) 'name)
                            efi-exe-path
                            name)))

#| * dual-booting linux+windows requires a bootloader that can chainload efi apps
   * systemd-boot & refind autodetect windows boot manager (\EFI\Microsoft\Boot\bootmgfw.efi) and
     show it in their menu automatically. TODO: check that this works even if a prior, falied linux install
     made it so that we can't boot windows, even though windows was installed properly.

TODO: given that i know the path to the windows efi executable, i should be able to add it via efibootmgr just as the linux thing below, right? i just specify the path and a name, e.g. "Windows"

TODO: BIOS vs UEFI is entirely separate from MBR vs GPT! except that maybe [TODO: identify] gpt does not work on bios.
|#
(define setup-bootloader
  (hash ;; these first two are the only known options for NVMe SSDs
        "systemd-boot" (λ _ (unless (= 0 (system/exit-code "bootctl install"))
                              (unless (= 0 (efibootmgr/add "Arch Linux" "\\EFI\\systemd\\systemd-bootx64.efi"))
                                      "neither bootctl nor efibootmgr could install/create systemd-boot efi boot entry")))
        ;; TODO: test the grub installer and add it to "determine the ideal boot loader" instruction in stage/boot
        "grub"         (λ _ (if uefi?
                                (begin (andmap find-executable-path '("grub" "efibootmgr")) ; must be installed
                                       (prompt "choose the boot entry name as it'll appear in the grub boot selection menu"
                                               ;; if your NVRAM is full of boot entries, then grub will fail to add its own via efibootmgr; the user
                                               ;; must then delete some boot entries before trying to add grub's again.
                                               (λ (n) ((sys0 (string-append "grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=" n))))
                                               "GRUB"))
                                (begin ((sys0 (string-append "grub-install --target=i386-pc " (get-boot-block-device))))))
                            ;; generate main config file. TODO: ask user if they want to edit /etc/default/grub before writing the grub config file
                            ;; TODO: if booting from MBR (namely, if /boot is ntfs), ensure that ntfs-3g is installed; else you won't be able to mount /boot!
                            ;; TODO: if you want to detect other oses (e.g. dual boot w/windows), do the following:
                            ;; 1. check that os-prober is installed
                            ;; 2. if those oses boot from other partitions, mount them
                            ;; 3. ensure that GRUB_DISABLE_OS_PROBER=false is present (and not commented!)
                            ;; 4. run grub-mkconfig
                            ((sys0 "grub-mkconfig -o /boot/grub/grub.cfg"))
                            ;; see <https://wiki.archlinux.org/title/GRUB#MS_Windows> for complete consideration of ensuring that windows is bootable
                            ;; and for BIOS/MBR, see <https://wiki.archlinux.org/title/GRUB#Windows_installed_in_BIOS/MBR_mode>; in this case,
                            ;; you add a text file with the menuentry to /etc/grub.d/ before running grub-mkconfig.
                            )
        "refind"       (λ _ "currently unsupported")
        "syslinux"     (λ _ "currently unsupported"))) ; might never be supported; looks like bad software

#| bootloader.
   * def esp: := efi system partition.
   * bootloaders in order of preference:
     * systemd-boot (previously known as gummiboot). uses EFISTUB, a mechanism that allows the kernel to be loaded as an efi executable by efi firmware. preferable if we're booting only arch. comes with systemd; no external package needed.

   * THE ESP MUST BE PHYSICAL (cf lvm, software raid, &c)
|#
(define stage/boot
  `(("arch-chroot /mnt, if you haven't already")
    ;; NEXT: this step appears to have done nothing. also, it somehow terminated without asking whether to restart.
    ;; it should've output an error, since `bootctl install` failed when i ran it manually. notably, the reason that
    ;; it failed is that the esp was ntfs, not fat. therefore apparently systemd-boot does not support dual-booting with
    ;; windows, at least when windows is installed before linux.
    ("determine ideal boot loader" .,(λ _ (let* ([boot-part (get-boot-partition)]
                                                   [bbd (get-boot-block-device boot-part)])
                                              (if boot-part
                                                  (0-let*/seq (let* ([bpn (hash-ref boot-part 'name)]
                                                                     [bbd-line (findf (λ (l) (string-contains? l bpn))
                                                                                      (string-split (read-proc (string-append "fdisk -o Device,Type -l /dev/" bbd))
                                                                                                    "\n"))])
                                                                (if bbd-line
                                                                    ;; or could check Type-UUID = C12A7328-F81F-11D2-BA4B-00A0C93EC93B
                                                                    (if (or (string-contains? bbd-line "EFI")
                                                                            #| could check also for (string-contains? bbd-line "HPFS/NTFS/exFAT");
                                                                               MBR uses one code for all of HPFS, NTFS, & exFAT. however, if mbr,
                                                                               then fdisk outputs a Boot column (the 2nd col) which is surer.
                                                                            |#
                                                                            (string=? "*" (cadr (string-split bbd-line))))
                                                                        0
                                                                        (format "~a's GPT type is not 'efi system partition'" bpn))
                                                                    (error "[BUG] could not find boot partition in fdisk's output")))
                                                              ;; now we know that esp is mounted properly. proceed to deciding & installing ideal boot loader
                                                              (cond [(string-prefix? (hash-ref boot-part 'name) "nvme")
                                                                     ((hash-ref setup-bootloader "systemd-boot"))] ; only systemd-boot & grub support NVMe
                                                                    ;; syslinux supports ext4 and xfs, ufs, and uncompressed single-device btrfs
                                                                    [else ((hash-ref setup-bootloader "systemd-boot"))])) ; currently only supporting systemd-boot
                                                  "there's no partition mounted at /boot"))))
    (,(prompt "done! restart? (y/n) " (λ (ans) (when (string=? "y" ans) (system "sync; restart")))))))

(module+
 main
  (let*-values ([(cmdargs) (vector->list (current-command-line-arguments))]
                ;; technically you can specify args in any order
                [(stage index0) (let loop ([stage #f] [index0 #f] [rst cmdargs])
                                 (if (null? rst)
                                     (values stage (or index0 1))
                                     (let* ([curarg (car rst)] [index0? (string->number curarg)])
                                       (if index0?
                                           (loop stage index0? (cdr rst))
                                           (loop curarg index0 (cdr rst))))))])
    (when (and (not (null? cmdargs)) (or (string=? "-h" (car cmdargs)) (string=? "--help" (car cmdargs))))
      (for-each displayln '("USAGE: install-arch [stage] [step]\n"
                            "STAGES:"
                            "init:   prepare for using the live system for administration e.g. wifi connection, mount partitions."
                            "\tthis stage is needed every time you boot the live system."
                            "config: install & configure the arch system e.g. install packages, configure systemd services, set hostname."
                            "\tthis stage is needed only once if it completes successfully."
                            "boot:   configure the bootloader."
                            "\tperform this stage each time that you boot from the live medium because your install failed to boot."
                            "all:    the default. performs init, then config, then boot."
                            "\nthe STEP is provided by install-arch upon failure; providing it resumes the stage from the step that failed."))
      (exit 0))
    ;; if you specify an invalid stage, then `drop` will raise an error. if the user does that, then they deserve it. notabug.
    (for ([index (in-naturals index0)] [action (drop (hash-ref (hash "init" stage/init
                                                                     "config" stage/config
                                                                     "boot" stage/boot
                                                                     "all" (append stage/init stage/config stage/boot))
                                                               (or stage "all")
                                                               (λ _ (raise-user-error (format "invalid stage, \"~a\"" stage))))
                                                     (sub1 index0))])
         (let ([resume-cmdline (λ ([index index])
                                 (format "install-arch ~a"
                                         (if stage
                                             (format "~a ~a" stage index)
                                             index)))])
           (cond [(procedure? (cdr action)) (printf "~a. ~a..." index (car action)) (flush-output (current-output-port))
                                            (let ([result ((cdr action))])
                                              (when (or (string? result) (and (exact-integer? result) (not (= 0 result))))
                                                (fprintf (current-error-port)
                                                         "couldn't ~a; ~a. fix that, then run ~a.~n"
                                                         (car action)
                                                         (if (string? result)
                                                             result
                                                             (format <"exit code ~a>" result))
                                                         (resume-cmdline))
                                                (exit -1)))]
                 [(procedure? (car action)) ((car action))]
                 [else (printf "~a. ~a..." index (car action))
                       (printf "you must do this manually. continue by running ~a.~n" (resume-cmdline (add1 index)))
                       (exit 0)]))
         (printf "~n")))) ; if we'ven't failed, then print newline since we'dn't printed one after announcing the current step (index) & action description.
