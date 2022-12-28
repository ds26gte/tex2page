; last change: 2022-12-28

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "#!/bin/sh\n"
   "exec guile -e main -s $0 \"$@\"\n"
   "!#\n\n"
   )))

(define *scheme-version*
  (string-append "Guile " (version)))

(define *tex2page-namespace* (interaction-environment))

(scmxlate-ignore
 call-with-input-string
 get-time-zone
 get-arg1
 string-index
 with-output-to-port)

(scmxlate-rename
 (date-day tm:mday)
 (date-hour tm:hour)
 (date-minute tm:min)
 (date-month tm:mon)
 (date-year tm:year)
 (seconds->date localtime)
 )

(scmxlate-rename-define
; (*return* #\return)
; (*tab* #\tab)
 (string-reverse-index string-rindex)
 (*january-number* 0)
 (*anno-domini-at-0* 1900)
 (strftime-like strftime)
 )

(scmxlate-uncall
 require
 )

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

(define file-or-directory-modify-seconds
  (lambda (f) (vector-ref (stat f) 9)))

(define read-line
  (lambda (i)
    (let ((c (peek-char i)))
      (if (eof-object? c) c
          (let loop ((r '()))
            (let ((c (read-char i)))
              (if (or (eof-object? c) (char=? c #\newline))
                  (list->string (reverse! r))
                  (loop (cons c r)))))))))

(define main
  (lambda (args)
    (tex2page
      (and (>= (length args) 2)
           (list-ref args 1)))))
