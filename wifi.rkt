#lang racket/base

;; NEXT: sometimes things fail; restart services wpa_supplicant, dhcpcd.
;; check ip link DOWN and/or no ip address on wireless device.
;; what about default
;; try adding static ip address

(provide (all-defined-out))
(require "interactive.rkt"
         (only-in racket/string string-prefix? string-split)
         (only-in racket/list filter-map)
         (only-in "json-extra.rkt" filter/json string->jsexpr))

(define (wpa-supplicant-config-path) (make-parameter "/etc/wpa_supplicant.conf"))

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
          [start-wpa-supplicant (sys0 (format "wpa_supplicant -B -i ~a -c ~a" (if (pair? wifi-dev) (car wifi-dev) 'dummy) (wpa-supplicant-config-path)))])
     ;; connect to network; use wpa_supplicant.conf if available; else generate that file by connecting to a network
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
                                 (begin (if (file-exists? (wpa-supplicant-config-path))
                                            (printf "[INFO] using config from ~a.~n" (wpa-supplicant-config-path))
                                            (0-let*/seq
                                             (let ([dir (basename (wpa-supplicant-config-path))])
                                               (or (mkdir dir) (format "failed to create directory ~a" dir)))
                                             (write-file (wpa-supplicant-config-path)
                                                        (read-proc (string-append "wpa_passphrase "
                                                                                  (select "select a wireless network to connect to:"
                                                                                          ;; yes, screenscraping iw(8) despite it explicitly saying not to do so. currently
                                                                                          ;; no iw library nor json output is available, though it's planned; see
                                                                                          ;; <https://www.spinics.net/lists/linux-wireless/msg202689.html>.
                                                                                          ;; the source for iw/scanning is too complex to make a wrapper for it; see
                                                                                          ;; <https://fossies.org/linux/iw/scan.c>.
                                                                                          (filter-map (λ (s) (let ([m (regexp-match #px"SSID: (.+)$" s)])
                                                                                                               (and m (cadr m))))
                                                                                                      (string-split (car scan-results) "\n"))))))))
                                        (start-wpa-supplicant))
                                 "wpa supplicant is not installed. idk how to connect to any network without it.")))) 
   ;; finally, test for internet connectivity
   ((sys0 "ping -c 1 8.8.8.8" "can't ping 8.8.8.8")) ; test ping
   ((sys0 "ping -c 1 archlinux.org" "dns error")))) ; test dns

(module+ main
 (void))
