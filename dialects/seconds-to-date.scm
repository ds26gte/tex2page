;last change: 2003-05-31

(define *days-in-a-mo*
  (vector 31 28 31 30 31 30
          31 31 30 31 30 31))

(define *anno-domini-at-epoch* 1970) ;epoch is Jan 1, 1970
(define *epoch-week-day* 4) ;it was a Thursday

(define leap?
  (lambda (y)
    (or (and (= (modulo y 4) 0)
             (> (modulo y 100) 0))
        (= (modulo y 400) 0))))


(define seconds->date
  ;decode-seconds-since-epoch
  (lambda (s . tz)
    (if (pair? tz) (set! s (- s (* 3600 (car tz)))))
    (let* (
           ;the day, counting from the epoch
           (d (quotient s 86400))
           ;the day of the week
           (dow (modulo (+ d *epoch-week-day*) 7))
           (s (- s (* d 86400)))
           ;the hour
           (h (quotient s 3600))
           (s (remainder s 3600))
           ;the minute
           (min (quotient s 60))
           ;the seconds after that minute
           (s (remainder s 60))
           )

      ;locate year by adding or subtracting a year's days from d,
      ;depending on which side of the epoch d is

      (let loop ((d d) (y *anno-domini-at-epoch*))
        (if (< d 0)
            ;try previous year
            (let ((y-1 (- y 1)))
              (loop (+ d (if (leap? y-1) 366 365)) y-1))
            (let* ((l? (leap? y))
                   (d2 (if l? 366 365)))
              (if (>= d d2)
                  ;try next year
                  (loop (- d d2) (+ y 1))

                  ;we're in the right year.  now let's find which month
                  ;we're in, by counting the number of subtractions needed
                  ;to be within some month's days
                  (let loop ((d d) (mo 0))
                    (let ((d2 (vector-ref *days-in-a-mo* mo)))
                      ;leap Februaries have one more day
                      (if (and (= mo 1) l?) (set! d2 (+ d2 1)))
                      (if (> d d2)
                          ;try next month
                          (loop (- d d2) (+ mo 1))
                          ;we're in the right month.  the days
                          ;not subtracted give day-of-month.
                          ;but we want days to be 1-based, not 0-based, 
                          ;so add 1.  Years are
                          ;1900-based, so subtract
                          ;1900.
                          (vector s
                                  min
                                  h
                                  (+ d 1)
                                  mo
                                  (- y 1900)
                                  dow)))))))))))

(define date-second (lambda (v) (vector-ref v 0)))
(define date-minute (lambda (v) (vector-ref v 1)))
(define date-hour (lambda (v) (vector-ref v 2)))
(define date-day (lambda (v) (vector-ref v 3)))
(define date-month (lambda (v) (vector-ref v 4)))
(define date-year (lambda (v) (vector-ref v 5)))
(define date-week-day (lambda (v) (vector-ref v 6)))

;The time struct returned follows POSIX convention. 
;In particular, January is 0, Sunday is 0, and the
;years are those since 1900 C.E.

;For comparison, PLT Scheme and Common Lisp have
;January = 1 and give years since 0 C.E.  CL Lisp
;further has Monday = 0.  Alone among Schemes, MIT
;Scheme follows CL.  Schemes other than PLT and
;MIT follow POSIX.

(define *january-number* 0)
(define *sunday-number* 0)
(define *anno-domini-at-0* 1900)

(scmxlate-include "fake-strftime.scm")
