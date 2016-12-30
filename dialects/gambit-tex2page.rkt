; last change: 2017-01-03

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "#! /usr/bin/env gsi\n\n")))

(define *scheme-version* 
  (string-append "Gambit " (system-version-string)))

(define gambit-getenv
  (lambda (ev)
    (with-exception-handler (lambda (ex) #f)
                            (lambda () (getenv ev)))))

(scmxlate-ignore
 with-output-to-port)

(scmxlate-uncall
 require
 trace
 )

(scmxlate-rename
 (eof #!eof)
 (getenv gambit-getenv)
 )

(define (remove y xx)
  (let loop ((xx xx) (r '()))
    (if (null? xx) (nreverse r)
      (let ((x (car xx)))
        (loop (cdr xx)
              (if (equal? x y) r
                (cons x r)))))))

(define-macro cl-with-output-to-string 
  (lambda (ignore-wots-arg . body)
    `(with-output-to-string '() (lambda () ,@body))))

(define (string-upcase s)
  (list->string (map char-upcase (string->list s))))

(define current-seconds
  (lambda ()
    (inexact->exact (floor (time->seconds (current-time))))))

(define-macro unless
  (lambda (b . ee)
    `(if (not ,b) (begin ,@ee))))

(define-macro when
  (lambda (b . ee)
    `(if ,b (begin ,@ee))))

(define-macro fluid-let
  (lambda (xvxv . ee)
    (let ((xx (map car xvxv))
          (vv (map cadr xvxv))
          (old-xx (map (lambda (xv)
                         (string->symbol
                          (string-append "%__"
                                         (symbol->string (car xv))))) xvxv))
          (res '%_*_res))
      `(let ,(map (lambda (old-x x) `(,old-x ,x)) old-xx xx)
         ,@(map (lambda (x v)
                  `(set! ,x ,v)) xx vv)
         (let ((,res (begin ,@ee)))
           ,@(map (lambda (x old-x) `(set! ,x ,old-x)) xx old-xx)
           ,res)))))

(define nreverse
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	  (let ((d (cdr s)))
            (set-cdr! s r)
	    (loop d s))))))

(define andmap
  (lambda (f s)
    (let loop ((s s))
      (if (null? s) #t
          (and (f (car s)) (loop (cdr s)))))))

(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
        (or (f (car s)) (loop (cdr s)))))))

(define nconc
  (lambda (s1 s2)
    ;appends s1 and s2 destructively (s1 may be modified)
    (if (null? s1) s2
      (let loop ((r1 s1))
        (if (null? r1) (error 'append! s1 s2)
          (let ((r2 (cdr r1)))
            (if (null? r2)
                (begin
                  (set-cdr! r1 s2)
                  s1)
                (loop r2))))))))

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

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

(define file-or-directory-modify-seconds
  (lambda (f)
    (inexact->exact
      (floor (time->seconds
               (file-info-last-modification-time (file-info f)))))))

(scmxlate-include "seconds-to-date.scm")

(define strftime strftime-like)

(scmxlate-postamble)

;mzscheme main works just fine

