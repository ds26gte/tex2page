; last change: 2022-12-26

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

(scmxlate-ignoredef
  *tex2page-namespace*
  )

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-rename
 (getenv gambit-getenv)
 (substring subseq)
 )

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

(define index-of
  (lambda (s x)
    (let loop ((s s) (i 0))
      (cond ((null? s) false)
            ((eq? (car s) x) i)
            (else (loop (cdr s) (+ i 1)))))))

(define (remove y xx)
  (let loop ((xx xx) (r '()))
    (if (null? xx) (reverse! r)
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

(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
        (or (f (car s)) (loop (cdr s)))))))

(define (append! l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (let loop ((r1 l1))
                (let ((cdr-r1 (cdr r1)))
                  (cond ((null? cdr-r1) (set-cdr! r1 l2) l1)
                        (else (loop cdr-r1))))))))

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

(define eval1
  (lambda (e)
    (eval e (interaction-environment))))

(define (main arg1 . ignore)
  (tex2page arg1))

(scmxlate-postamble)
