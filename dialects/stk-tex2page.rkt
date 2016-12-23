(scmxlate-cond 
((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "#!/bin/sh
\":\";exec snow -file $0 -- $*
")))

(define *scheme-version* "STk")

(scmxlate-uncall
 require)


(scmxlate-rename
 (flush-output flush)
 )

(define get-arg1
  (lambda ()
    (and (pair? *argv*) (car *argv*))))


(define delete-file
  (lambda (f) #t))


(define file-or-directory-modify-seconds
  (lambda (f) #f))

(define eof (call-with-input-string "" read))

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	  (let ((d (cdr s)))
            (set-cdr! s r)
	    (loop d s))))))

(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
        (or (f (car s)) (loop (cdr s)))))))

(define seconds-to-human-time
  (lambda (secs) ""))

; ex:ft=scheme