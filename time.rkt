#lang racket/base

(provide (all-defined-out)
         (all-from-out gregor/time)
         (all-from-out gregor) ; exports renamed symbols, i.e. safes & unsafes are correct
         (all-from-out gregor/period))

(require gregor/time gregor/period
         (except-in gregor
                    iso8601->date iso8601->time iso8601->datetime iso8601->moment iso8601/tzid->moment
                    parse-time parse-date parse-datetime parse-moment)
         (rename-in gregor
                    (iso8601->date iso8601->date/unsafe)
                    (iso8601->time iso8601->time/unsafe)
                    (iso8601->datetime iso8601->datetime/unsafe)
                    (iso8601->moment iso8601->moment/unsafe)
                    (iso8601/tzid->moment iso8601/tzid->moment/unsafe)
                    (parse-time parse-time/unsafe)
                    (parse-date parse-date/unsafe)
                    (parse-datetime parse-datetime/unsafe)
                    (parse-moment parse-moment/unsafe))
         (only-in racket/function const) (only-in racket/string string-join) (only-in racket/match match)
         (for-syntax racket/base (only-in racket/syntax format-id with-syntax*)))

(define EPOCH (moment 1970 #:tz "UTC")) ;; as defined internally in gregor

(define-syntax (define-conversions stx)
  (let ([ts '("date" "time" "datetime" "moment")])
    (with-syntax* ([(defpats ...) #'((list "yyyy-MM-dd")
                                     (list "HH:mm:ss" "hh:mm:ss a")
                                     (list "yyyy-MM-dd'T'HH:mm:ss" "yyyy-MM-dd HH:mm:ss")
                                     (list "yyyy-MM-dd'T'HH:mm:ssxx" "yyyy-MM-dd'T'HH:mm:ss.Sxx"))]
                   [((_ . (defpat . _)) ...) #'(defpats ...)] ;; first _ is #'list
                   [((ps ps-sym ps/u is is/u to-str) ...) (map (λ (s) (list (format-id stx #:source stx "parse-~a" s)
                                                                            #`(quote #,(string->symbol (string-append "parse-" s)))
                                                                            (format-id stx #:source stx "parse-~a/unsafe" s)
                                                                            (format-id stx #:source stx "iso8601->~a" s)
                                                                            (format-id stx #:source stx "iso8601->~a/unsafe" s)
                                                                            (format-id stx #:source stx "~a->string" s)))
                                                               ts)])
      #'(begin (define (ps s #:raise? [raise? #f] . pats)
                 (let* ([pats (if (null? pats) defpats pats)]
                        [x (ormap (λ (pat) (with-handlers ([exn:gregor:parse? (const #f)]) (ps/u s pat)))
                                  pats)])
                   (if (and raise? (not x))
                       (raise-argument-error ps-sym (format "a pattern in ~s" pats) s)
                       x))) ...
               (define (to-str x) (~t x defpat)) ...
               (define (is s) (with-handlers ([exn:gregor:parse? (const #f)]) (is/u s))) ...))))

(define-conversions)

;; different from ->posix in that it returns the milliseconds since EPOCH
;; whereas ->posix returns the seconds since EPOCH.
;; (-> DateTimeProvider Integer)
(define (->posix/ms t) (milliseconds-between EPOCH t))

(define (iso8601/tzid->moment s)
  (with-handlers ([exn:gregor:parse? (const #f)]) (iso8601/tzid->moment/unsafe s)))

(define (in-time-interval? t i)
  (and (moment>=? t (car i)) (moment<=? t (cdr i))))

;; (: time-between (-> TimeProvider TimeProvider (U 'seconds 'minutes 'hours) Integer))
(define (time-between t1 t2 t)
  (period-ref (period-between (datetime 1 1 1 (->hours t1) (->minutes t1) (->seconds t1))
                              (datetime 1 1 1 (->hours t2) (->minutes t2) (->seconds t2))
                              (list t))
              t))

;;; regexes

(define datetime-format-str "yyyy-MM-dd HH:mm:ss")
(define number-regex #rx"[0-9.]+")
(define hhmm-regex #px"[0-9]{1,2}:[0-9]{2}")

;;; formatting

(define (to-padded-str n)
  (if (>= n 10) (number->string n) (string-append "0" (number->string n))))

(define (secs-to-hhmmss-str secs)
  (let*-values ([(tm s) (quotient/remainder secs 60)]
                [(h m) (quotient/remainder tm 60)])
    (string-join (foldr (λ (t acc) (cons (to-padded-str t) acc)) null (list h m s)) ":")))

;;; time arithmetic

;; like gregor/datetime->iso8601, but separates date and time with #\space rather than #\T
;; (: datetime->iso8601/space (-> DateTimeProvider String))
(define (datetime->iso8601/space pdt) (~t (->datetime/local pdt) datetime-format-str))

;; like gregor/iso8601->datetime, but expects date and time to be separated by #\space rather than #\T
;; (: parse-iso8601/space->datetime (-> String (Option DateTime)))
(define (parse-iso8601/space->datetime s) (parse-datetime s datetime-format-str))

;;; parsing

;; parse "hh:mm a ET"-formatted string into a time
(define (parse-hh-mm-a-z timestr)
  (match (regexp-match #px"([0-9]{2}):([0-9]{2}) (.+) ET" timestr)
         [(list _ h m a) (cond [(string=? "a.m." a) (time h m)]
                               [(string=? "p.m." a) (if (= 12 h) (time h m) (time (+ 12 h) m))]
                               [else (error "parse-hh-mm-a-z: unrecognized period: ~a in timestring ~a" a timestr)])]
         [_ (error (format "parse-hh-mm-a-z: bad parse: ~a" timestr))]))

;;; type conversion

(define (date->datetime d [h 0] [m 0] [s 0] [ms 0])
  (datetime (->year d) (->month d) (->day d) h m s ms))

;; (: date+time (-> DateProvider TimeProvider DateTime))
(define (date+time dp tp) (datetime (->year dp) (->month dp) (->day dp) (->hours tp) (->minutes tp) (->seconds tp) (->milliseconds tp)))

#| adds a timezone to a datetime. e.g. (datetime->moment (datetime 2020 01 01 10 00 00) "America/New_York")
   if you give an invalid timezone, an exception is raised.
   if you want to get the timezone-shifted moment from an existing moment, use adjust-timezone.
   IT'S ALMOST CERTAINLY A MISTAKE to use (now), (now/moment), etc with this function!
   (datetime->moment (now) "America/New_York") will create a moment at the environment's
   current time, but on the NY TZ. instead, you almost certainly really mean
   (adjust-timezone (now/moment) "America/New_York"), which will give the current time
   in New York, computed from the current time in the environment's time zone.
|#
;; (: datetime->moment (-> DateTimeProvider TZ Moment))
(define (datetime->moment dt tz)
  (moment (->year dt) (->month dt) (->day dt)
          (->hours dt) (->minutes dt) (->seconds dt) (->nanoseconds dt)
          #:tz tz))

;; like datetime->moment but for dates
;; (: date->moment (-> Date TZ Moment))
(define (date->moment d tz) (moment (->year d) (->month d) (->day d) #:tz tz))

;; (: moment->datetime (-> MomentProvider DateTime))
(define (moment->datetime m)
  (datetime (->year m) (->month m) (->day m)
            (->hours m) (->minutes m) (->seconds m) (->nanoseconds m)))

;; current date in a given timezone
(define (today/tz tz)
  (let ([m (adjust-timezone (now/moment) tz)])
    (date (->year m) (->month m) (->day m))))

;; current moment in a given timezone
(define (now/moment/tz tz) (adjust-timezone (now/moment) tz))
