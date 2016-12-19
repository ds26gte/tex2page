;last change: 2003-07-17

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "\":\";exec scsh -s $0 \"$@\"
")))


(define *scheme-version* 
  (string-append "Scsh " scsh-version-string))

(scmxlate-uncall
 require
 )

(define *int-corresp-to-nul*
  (- (char->integer #\a) 97)) 

(define scsh-int-to-char
  (lambda (n)
    (integer->char
     (+ n *int-corresp-to-nul*))))

(define scsh-char-to-int
  (lambda (c)
    (- (char->integer c)
       *int-corresp-to-nul*)))

(scmxlate-rename
 (char->integer scsh-char-to-int)
 (integer->char scsh-int-to-char)
 (seconds->date date)
 ;(open-input-string make-string-input-port)
 )

(scmxlate-rename-define
  (*january-number* 0)
  (*anno-domini-at-0* 1900))

(define string->list
  ;native procedure is buggy!
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0) r
          (loop (- i 1) (cons (string-ref s i) r))))))

(define get-arg1
  (lambda ()
    (and (> (length command-line-arguments) 0)
         (car command-line-arguments))))

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

(define eval1
  (lambda (e)
    (eval e ;(scheme-report-environment 5)
          (interaction-environment))))

(define do-evalh
  (lambda (s)
    (let ((f (create-temp-file)))
      (call-with-output-file f
        (lambda (o)
          (display s o)))
      (load f)
      ;(delete-file f)
)))


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


(define call-with-input-string
  (lambda (s p)
    (p (make-string-input-port s))))

(define with-output-to-port
  (lambda (o th)
    (with-current-output-port o (th))))

(define file-or-directory-modify-seconds
  (lambda (f)
    (file-info:mtime (file-info f))))

(define date-day date:month-day)
(define date-hour date:hour)
(define date-minute date:minute)
(define date-month date:month)
(define date-year date:year)

(define strftime-like
  (lambda (fmt dt)
    (format-date 
      "~a, ~b ~d, ~Y, ~I:~M ~p ~Z" dt)))

(define skip-whitespace
  (lambda (i)
    (let loop ()
      (if (char-whitespace? (peek-char i))
          (begin (read-char i) (loop))))))

(define system
  (lambda (s)
    (let ((i (make-string-input-port s)))
      (let loop ((big-r '()) (r '()))
        (let ((c (read-char i)))
          (cond ((eof-object? c)
                 (run ,(reverse! (cons (list->string (reverse! r)) big-r))))
                ((char-whitespace? c)
                 (skip-whitespace i)
                 (loop (cons (list->string (reverse! r)) big-r) '()))
                (else
                  (loop big-r (cons c r)))))))))

;(define system
;;why doesn't this work?
;  (lambda (s)
;    (let ((i (make-string-input-port
;              (string-append "(run (" s "))"))))
;      (eval (read i) (interaction-environment))
;      (close-input-port i))))

(define kpsewhich
  (lambda (f)
    (let ((tmpf (string-append *aux-dir/* *jobname* "-Z-Z.temp")))
      (run (kpsewhich ,f)
           (> ,tmpf))
      (let ((f (call-with-input-file tmpf 
                 (lambda (i) (read-line i)))))
        (delete-file tmpf)
        (if (eof-object? f) #f 
            (let ((f (string-trim-blanks f)))
              (cond ((= (string-length f) 0) #f)
                    ((file-exists? f) f)
                    (else #f))))))))

(define ps-to-img/gif
  (lambda (ps-file f)
    (let* ((f-ppm (string-append f ".ppm"))
           (f-ppm-tmp (string-append f ".ppm.tmp"))
           (f-gif (string-append f ".gif"))
           (output-file (string-append "-sOutputFile=" f-ppm)))
      (run (,*ghostscript* -q -dBATCH -dNOPAUSE -dNO_PAUSE
            -sDEVICE=ppmraw ,output-file ,ps-file quit.ps))
      (run (pnmcrop ,f-ppm)
        (> ,f-ppm-tmp))
      (run (ppmquant 256)
        (< ,f-ppm-tmp)
        (> ,f-ppm))
      (run (ppmtogif -transparent rgb:ff/ff/ff)
        (< ,f-ppm)
        (> ,f-gif))
      (for-each
        (lambda (e)
          (ensure-file-deleted (string-append f e)))
        '(".ppm" ".ppm.tmp")))))

(define eof
  (read (make-string-input-port "")))

(scmxlate-postamble)

(tex2page (get-arg1))
