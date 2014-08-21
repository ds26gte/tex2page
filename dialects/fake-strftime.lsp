;last change: 2009-04-08

(defvar *short-month-names*
  (vector "Jan" "Feb" "March" "April" "May" "June"
    "July" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defvar *week-day-names*
  (vector "Sun" "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat"))

(defvar *sunday-number* 6)
(defvar *january-number* 1)
(defvar *anno-domini-at-0* 0)

(defun strftime-like (ignore-format d)
  (if (not d) ""
      (let ((h (date-hour d))
            (m (date-minute d)))
        (format nil "~a, ~a ~a, ~a, ~a:~a~a ~a~a"
                (svref *week-day-names*
                       (mod (- (date-week-day d)
                               *sunday-number*) 7))
                (svref *short-month-names*
                       (- (date-month d) *january-number*))
                (date-day d)
                (+ *anno-domini-at-0* (date-year d))
                (let ((h (mod h 12)))
                  ;12h watch
                  (if (= h 0) 12 h))
                (if (< m 10) "0" "")
                m
                (if (<= 0 h 11) "am" "pm")
                (let ((tz (#+sbcl sb-ext:posix-getenv #-sbcl getenv "TZ")))
                  (if tz (concatenate 'string " " tz)
                      ""))))))
