; last change: 2017-01-03

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
  (eof #!eof)
  (remove chicken-remove)
  (seconds->date seconds->local-time)
  )

(define nreverse
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	  (let ((d (cdr s)))
            (set-cdr! s r)
	    (loop d s))))))

(define (string-is-flanked-by-stars-p s)
  (let ((n (string-length s)))
    (and (>= n 3)
         (char=? (string-ref s 0) #\*)
         (char=? (string-ref s (sub1 n)) #\*))))

(define (chicken-remove y xx)
  (let loop ((xx xx) (r '()))
    (if (null? xx) (nreverse r)
      (let ((x (car xx)))
        (loop (cdr xx)
              (if (equal? x y) r
                (cons x r)))))))

(define (string-upcase s)
  (list->string (map char-upcase (string->list s))))

(define (string-trim s)
  (let ((orig-n (string-length s)))
    (let ((i 0) (n orig-n))
      (let loop ((k i))
        (cond ((>= k n) (set! i n))
              ((char-whitespace? (string-ref s k))
               (loop (+ k 1)))
              (else (set! i k))))
      (let loop ((k (- n 1)))
        (cond ((<= k i) (set! n (+ k 1)))
              ((char-whitespace? (string-ref s k))
               (loop (- k 1)))
              (else (set! n (+ k 1)))))
      (if (and (= i 0) (= n orig-n)) s
          (substring s i n)))))

(define strftime
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
