;last change: 2003-06-02

(define *short-month-names*
  (vector "Jan" "Feb" "March" "April" "May" "June"
    "July" "Aug" "Sept" "Oct" "Nov" "Dec"))

(define *week-day-names*
  (vector "Sun" "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat"))

(define strftime-like
  (lambda (ignore-format d)
    (if (not d) ""
        (let ((h (date-hour d))
              (m (date-minute d)))
          (string-append
            (vector-ref *week-day-names*
                        (modulo (- (date-week-day d)
                                   *sunday-number*) 7))
            ", "
            (vector-ref *short-month-names* 
                        (- (date-month d) *january-number*))
            " "
            (number->string (date-day d))
            ", "
            (number->string (+ *anno-domini-at-0* (date-year d)))
            ", "
            (number->string 
              (let ((h (modulo h 12)))
                ;12h watch
                (if (= h 0) 12 h)))
            ":"
            (if (< m 10) "0" "")
            (number->string m)
            (if (<= 0 h 11) " am" " pm")
            ;incoming date is UTC, not localtime
            " UTC")))))
