; last change: 2003-06-21

(scmxlate-cond
((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "\":\";exec scm -l $0 \"$@\"
")))

(define *scheme-version* 
  (string-append "SCM " (symbol->string *scm-version*)))

(scmxlate-rename
 (andmap every)
 (append! nconc)
 (flush-output force-output)
 (gensym gentemp)
 (ormap some)
 )

(scmxlate-ignore
 call-with-input-string
 with-output-to-port)

(scmxlate-uncall
 require)


(require 'line-i/o)
(require 'common-list-functions)


;(define get-arg1
;  (lambda ()
;    (let ((pa (program-arguments)))
;      (and (> (length pa) 3)
;           (list-ref pa 3)))))


(defmacro unless (b . ee)
  `(if (not ,b) (begin ,@ee)))

(defmacro when (b . ee)
  `(if ,b (begin ,@ee)))

(define eof
  (call-with-input-string "" read))


(scmxlate-cond
 ((not (defined? 'reverse!))
  (define reverse!
    (lambda (s)
      (let loop ((s s) (r '()))
        (if (null? s) r
            (let ((d (cdr s)))
              (set-cdr! s r)
              (loop d s))))))))


;string ports

(define *output-strings* '())

(define open-output-string
  (lambda ()
    (let* ((f (tmpnam))
           (o (open-output-file f)))
      (set! *output-strings*
        (cons (cons o f) *output-strings*))
      o)))

(define get-output-string
  (lambda (o)
    (let ((o-f (assv o *output-strings*)))
      (let ((o (car o-f))
            (f (cdr o-f)))
        (close-output-port o)
        (let ((s (call-with-input-file f
                   (lambda (o)
                     (let loop ((r '()))
                       (let ((c (read-char o)))
                         (if (eof-object? c)
                             (list->string (reverse! r))
                             (loop (cons c r)))))))))
          (delete-file f)
          s)))))

;(define kall-with-input-string call-with-input-string)

(scmxlate-cond
  ((defined? stat)
   (define file-or-directory-modify-seconds
     (lambda (f)
       (vector-ref (stat f) 9))))
  (else
    (define file-or-directory-modify-seconds
      (lambda (f) 0))))

(define andmap every)

(scmxlate-include "seconds-to-date.scm")

(scmxlate-postamble)

(let ((pa (program-arguments)))
  (and (> (length pa) 3)
       (tex2page (list-ref pa 3))))
