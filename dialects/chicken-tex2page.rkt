; last change: 2012-04-21

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   (string-append
    "\":\";"
    "exec csi -quiet -batch $0 -eval '(tex2page \"'$1'\")'"
    "
"))))

(define *scheme-version*
  (string-append "Chicken " (chicken-version)))

;(declare (uses extras srfi-1 posix))

(use extras)
(use srfi-1)
(use posix)

(scmxlate-ignore
 main
)

(scmxlate-uncall
 require
 trace
 )

(scmxlate-rename-define
  (eval1 eval)
  (*january-number* 0)
  (*anno-domini-at-0* 1900)
  )

(scmxlate-rename
  (seconds->date seconds->local-time)
  (eof #!eof)
  )

(define strftime-like
  (lambda (ignore d)
    (let ((s (time->string d))
          (tz (get-environment-variable "TZ")))
      (string-append
        (substring s 0 (- (string-length s) 1))
        (if tz (string-append " " tz) "")))))

(define date-minute (lambda (v) (vector-ref v 1)))
(define date-hour (lambda (v) (vector-ref v 2)))
(define date-day (lambda (v) (vector-ref v 3)))
(define date-month (lambda (v) (vector-ref v 4)))
(define date-year (lambda (v) (vector-ref v 5)))

(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
        (or (f (car s)) (loop (cdr s)))))))

(define andmap
  (lambda (f s)
    (let loop ((s s))
      (if (null? s) #t
          (and (f (car s)) (loop (cdr s)))))))

; Felix says: To compile, do
; csc my-tex2page -o whatever -b -O2
; This has been incorporated into scmxlate, but it
; may be better to hand-do it from the OS
; command-line.

;(scmxlate-postamble)

;(define call-tex2page
;  (lambda ()
;    (let ((aa (command-line-arguments))
;          (n (cond-expand (compiling 1) (else 4))))
;      (tex2page
;        (or
;          (and (>= (length aa) n)
;               (list-ref aa (- n 1)))
;          "--missing-arg")))))

;(call-tex2page)
