; last change: 2022-12-27

(scmxlate-insert
 ";The structures
;
;c-system-function
;extended-ports
;posix
;
;need to be open before you can run the code in this file
")

(define *scheme-version* "Scheme 48")

(define *int-corresp-to-nul*
 (- (char->integer #\a) 97))

(define s48-int-to-char
  (lambda (n)
    (integer->char
     (+ n *int-corresp-to-nul*))))

(define s48-char-to-int
  (lambda (c)
    (- (char->integer c)
       *int-corresp-to-nul*)))

(scmxlate-rename
 (char->integer s48-char-to-int)
 (integer->char s48-int-to-char)
 (substring subseq)
 )

(define get-arg1
  (lambda () #f))

(scmxlate-uncall
  require
  main
  )

(define-syntax when
  (lambda (e r c)
    `(if ,(cadr e) (begin ,@(cddr e)))))

(define-syntax unless
  (lambda (e r c)
    `(if (not ,(cadr e)) (begin ,@(cddr e)))))

(define-syntax fluid-let
  (lambda (e r c)
    (let ((xvxv (cadr e))
          (ee (cddr e)))
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
             ,res))))))

;(define-macro when
;  (lambda (b . ee)
;    `(if ,b (begin ,@ee))))
;
;(define-macro unless
;  (lambda (b . ee)
;    `(if (not ,b) (begin ,@ee))))
;
;(define-macro fluid-let
;  (lambda (xvxv . ee)
;    (let ((xx (map car xvxv))
;          (vv (map cadr xvxv))
;          (old-xx (map (lambda (xv)
;                         (string->symbol
;                          (string-append "%__"
;                                         (symbol->string (car xv))))) xvxv))
;          (res '%_*_res))
;      `(let ,(map (lambda (old-x x) `(,old-x ,x)) old-xx xx)
;         ,@(map (lambda (x v)
;                  `(set! ,x ,v)) xx vv)
;         (let ((,res (begin ,@ee)))
;           ,@(map (lambda (x old-x) `(set! ,x ,old-x)) xx old-xx)
;           ,res)))))

(define (list* . args)
  (let ((a (car args)) (d (cdr args)))
    (if (null? d) a
        (cons a (apply list* d)))))

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

(define append!
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

(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
        (or (f (car s)) (loop (cdr s)))))))

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

(define read-line
  (lambda (i)
    (let ((c (peek-char i)))
      (if (eof-object? c) c
          (let loop ((r '()))
            (let ((c (read-char i)))
              (if (or (eof-object? c) (char=? c #\newline))
                  (list->string (reverse! r))
                  (loop (cons c r)))))))))

(define file-exists?
  (lambda (f)
    (accessible? f (access-mode read))))

(define flush-output (lambda z #f))

(define call-with-input-string
  (lambda (s p)
    (p (make-string-input-port s))))

(define file-or-directory-modify-seconds
  (lambda (f)
    (time-seconds
     (file-info-last-modification
      (get-file-info f)))
    ;(file-info:mtime (file-info f))
    ))

'(define seconds-to-human-time
  (lambda (secs)
    (time->string
     (make-time secs))))

(scmxlate-include "seconds-to-date.scm")

(define current-seconds
  (lambda () #f))

(define eval1
  (lambda (e)
    (eval e (scheme-report-environment 5))))

(define do-evalh
  (lambda (s)
    (let ((f "./.evalh.scm"))
      (call-with-output-file f
        (lambda (o)
          (display s o)))
      (load f)
      (unlink f)
)))

(scmxlate-include "temp-file.scm")
(scmxlate-include "with-port.scm")
