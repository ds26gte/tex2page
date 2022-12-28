; last change: 2022-12-28

(scmxlate-insert
  (string-append
    "#!/usr/local/bin/csi -ss\n\n"))

(import
  (chicken file)
  (chicken file posix)
  (chicken io)
  (chicken platform)
  (chicken port)
  (chicken pretty-print)
  (chicken process)
  (chicken process-context)
  (chicken time)
  (chicken time posix)
  )

(define *scheme-version*
  (string-append "Chicken " (chicken-version)))

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-ignoredef-rename
  (ensure-file-deleted delete-file*))

(scmxlate-rename
  (seconds->date seconds->local-time)
  )

(define *tex2page-namespace* (interaction-environment))

(define (list* . args)
  (let ((a (car args)) (d (cdr args)))
    (if (null? d) a
        (cons a (apply list* d)))))

(define (append! l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (let loop ((r1 l1))
                (let ((cdr-r1 (cdr r1)))
                  (cond ((null? cdr-r1) (set-cdr! r1 l2) l1)
                        (else (loop cdr-r1))))))))

(define (reverse! s)
  (let loop ((s s) (r '()))
    (if (null? s) r
        (let ((d (cdr s)))
          (set-cdr! s r)
          (loop d s)))))

(define (index-of s x)
  (let loop ((s s) (i 0))
    (cond ((null? s) false)
          ((eq? (car s) x) i)
          (else (loop (cdr s) (+ i 1))))))

(define (string-is-flanked-by-stars-p s)
  ;Chicken's char=? is not variadic
  (let ((n (string-length s)))
    (and (>= n 3)
         (char=? (string-ref s 0) #\*)
         (char=? (string-ref s (sub1 n)) #\*))))

(define (remove y xx)
  (let loop ((xx xx) (r '()))
    (if (null? xx) (reverse! r)
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

(define (strftime ignore d)
    (let ((s (time->string d))
          (tz (get-environment-variable "TZ")))
      (string-append
        (substring s 0 (- (string-length s) 1))
        (if tz (string-append " " tz) ""))))

(define (date-minute v) (vector-ref v 1))
(define (date-hour v) (vector-ref v 2))
(define (date-day v) (vector-ref v 3))
(define (date-month v) (vector-ref v 4))
(define (date-year v) (vector-ref v 5))

(define (ormap f s)
  ;Returns true if f is true of some elt in s
  (let loop ((s s))
    (if (null? s) #f
        (or (f (car s)) (loop (cdr s))))))

(scmxlate-postamble)

(define (main cla)
  (tex2page
    (and (>= (length cla) 1)
         (car cla))))

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
