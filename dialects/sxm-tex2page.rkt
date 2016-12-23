; last change: 2016-12-14

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
    "\":\";echo '"
    "(define arg-one \"'$1'\")"
    "(load \"'$0'\")"
    "(main (list 1 arg-one))"
    "(exit)"
    "'|exec sxi; exit"
    "

"))

 )




(define *scheme-version* "SXM")

(scmxlate-ignore
 with-output-to-port)

(scmxlate-uncall
 require
 trace
 )

;(define get-arg1
;  (lambda ()
;    (and (top-level-bound? 'arg-one) arg-one)))

(define eof (call-with-input-string "" read)) 

(define flush-output
  (lambda z
    #f))

;sxm issues warnings for forward-references,
;which can't all be removed anyway

(warning-handler (lambda zzz #f))

(define trace-if
  (lambda (write? . args)
    #t ;noop!  But w/o this, sxm fails
    (when write?
      (write-log #\newline)
      (when *current-tex-line-number*
        (write-log "l.")
        (write-log *current-tex-line-number*)
        (write-log #\space))
      (for-each write-log args)
      (write-log #\newline))))



(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (define file-or-directory-modify-seconds
    (lambda (f)
      (let* ((x (process
                 (string-append "stat -c \"%Y\" " f)))
             (n (read (car x))))
        (close-input-port (car x))
        (close-output-port (cadr x))
        n))))
 (else
  (define file-or-directory-modify-seconds
    (lambda (f) #f))))

;(define file-or-directory-modify-seconds
;  (lambda (f) #f))

(define read-line
  (lambda (i)
    (let ((c (peek-char i)))
      (if (eof-object? c) c
          (let loop ((r '()))
            (let ((c (read-char i)))
              (if (or (eof-object? c) (char=? c #\newline))
                  (list->string (reverse! r))
                  (loop (cons c r)))))))))

;(define seconds-to-human-time
;  (lambda (secs)
;    ""))

(scmxlate-include "seconds-to-date.scm")