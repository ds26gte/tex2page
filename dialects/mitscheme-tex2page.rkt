;last change: 2016-12-27

(scmxlate-cond
((eqv? *operating-system* 'unix)
 (scmxlate-insert
  (string-append
   "\":\";"
   "exec scheme "
   "-eval \"(define arg-one \\\"$1\\\")\" "
   "-load $0 "
   "-eval \"(%exit)\"
"))))

;  "\":\";exec scheme -eval \"(define arg-one \\\"$1\\\")\"
;  (scmxlate-insert
;   "\":\";echo \"(define arg-one \\\"$1\\\")(load \\\"$0\\\")(exit)\"|exec /usr/local/bin/scheme
;"

(declare (usual-integrations))

(define *scheme-version* "MIT Scheme")

;(scmxlate-rename
; (file-or-directory-modify-seconds file-modification-time)
; (gensym generate-uninterned-symbol)
 ;(system unix/system) ;??
; )

(scmxlate-ignore
 with-output-to-port
)

(scmxlate-uncall
 require
 trace
 )

(scmxlate-rename
 (date-day decoded-time/day)
 (date-hour decoded-time/hour)
 (date-minute decoded-time/minute)
 (date-month decoded-time/month)
 (date-year decoded-time/year)
 (seconds->date file-time->local-decoded-time)
 )

(scmxlate-rename-define
 (seconds-to-human-time file-time->local-time-string)
 (string-index string-find-next-char)
 (string-reverse-index string-find-previous-char)
 (*january-number* 1)
 (*anno-domini-at-0* 0)
)

(scmxlate-cond
 ((environment-bound? user-initial-environment 'string-search-forward)
  (scmxlate-rename-define
   (substring? string-search-forward)))
 (else
  (scmxlate-rename
   (substring? t2p-substring?))))

(scmxlate-rename
 (sort! t2p-sort!)
 ;(substring? t2p-substring?)
 )

(define get-arg1
  (lambda ()
    (and (environment-bound? user-initial-environment 'arg-one)
         arg-one)))

(scmxlate-cond
 ((environment-bound? user-initial-environment 'rsc-macro-transformer)
  (define-syntax when
    (rsc-macro-transformer
     (lambda (e r)
       `(if ,(cadr e) (begin ,@(cddr e))))))

  (define-syntax unless
    (rsc-macro-transformer
     (lambda (e r)
       `(if (not ,(cadr e)) (begin ,@(cddr e)))))))
 (else
  (syntax-table-define system-global-syntax-table 'unless
    (macro (b . ee)
      `(if (not ,b) (begin ,@ee))))

  (syntax-table-define system-global-syntax-table 'when
    (macro (b . ee)
      `(if ,b (begin ,@ee))))
  ))

(define eof (with-input-from-string "" read))

(scmxlate-include "string-ports.scm")

(define ormap
  (lambda (f s)
    (there-exists? s f)))

'(define *line-terminating-char-set*
  (chars->char-set (list #\newline #\return)))

'(define read-line
  (lambda (i)
    (let ((c (peek-char i)))
      (if (eof-object? c) c
          (read-string *line-terminating-char-set* i)))))

(define eval1 (lambda (x) (eval x user-initial-environment)))

(define read-line
  (lambda (i)
    (let ((c (peek-char i)))
      (if (eof-object? c) c
          (let loop ((r '()))
            (let ((c (read-char i)))
              (if (or (eof-object? c) (char=? c #\newline))
                  (list->string (reverse! r))
                  (loop (cons c r)))))))))

(define current-seconds
  (lambda ()
    (universal-time->file-time
     (get-universal-time))))

(scmxlate-postamble)

(and (environment-bound? user-initial-environment 'arg-one)
     (tex2page arg-one))

(scmxlate-cond
 ((and (environment-bound? user-initial-environment 'syntax-table-define)
       ;do only for older MIT Schemes for now
       (eqv? *operating-system* 'unix))
  (scmxlate-postprocess
(unix/system
 "cp -p my-tex2page t2p.scm")

(syntax-table-define system-global-syntax-table 'unless
                     (macro (b . ee)
                            `(if (not ,b) (begin ,@ee))))

(syntax-table-define system-global-syntax-table 'when
                     (macro (b . ee)
                            `(if ,b (begin ,@ee))))

(syntax-table-define
  system-global-syntax-table
  (quote defstruct)
  (macro (s . ff)
         (let ((s-s (symbol->string s)) (n (length ff)))
           (let* ((n+1 (+ n 1))
                  (vv (make-vector n+1)))
             (let loop ((i 1) (ff ff))
               (if (< i n+1)
                   (let ((f (car ff)))
                     (vector-set! vv i (if (pair? f) (cadr f)
                                           ;would like void here
                                           #f))
                     (loop (+ i 1) (cdr ff))) #f))
             (let ((ff (map (lambda (f) (if (pair? f) (car f) f)) ff)))
               `(begin
                  (define ,(string->symbol (string-append "make-" s-s))
                    (lambda fvfv
                      (let ((st (make-vector ,n+1)) (ff ',ff))
                        (vector-set! st 0 ',s)
                        ,@(let loop ((i 1) (r '()))
                            (if (>= i n+1) r
                                (loop (+ i 1)
                                      (cons `(vector-set! st ,i
                                                          ,(vector-ref vv i))
                                            r))))
                        (let loop ((fvfv fvfv))
                          (unless (null? fvfv)
                            (vector-set! st (+ (list-position (car fvfv) ff) 1)
                                         (cadr fvfv))
                            (loop (cddr fvfv))))
                        st)))
                  ,@(let loop ((i 1) (procs '()))
                      (if (>= i n+1) procs
                          (loop (+ i 1)
                                (let ((f (symbol->string
                                           (list-ref ff (- i 1)))))
                                  (cons
                                    `(define ,(string->symbol
                                                (string-append s-s "-" f))
                                       (lambda (x) (vector-ref x ,i)))
                                    (cons
                                      `(define ,(string->symbol
                                                  (string-append
                                                    "set!" s-s "-" f))
                                         (lambda (x v) (vector-set! x ,i v)))
                                      procs))))))
                  (define ,(string->symbol (string-append s-s "?"))
                    (lambda (x)
                      (and (vector? x) (eq? (vector-ref x 0) ',s))))))))))

(cf "t2p")

;(%exit)
))) 
