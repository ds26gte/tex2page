; last change: 2017-01-07

(scmxlate-eval
 (define *chez-name*
   (if (eqv? *dialect* 'petite)
       "petite"
       "scheme" ;or "chez"?
       )))

(scmxlate-cond

 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
    "#! /usr/local/bin/"
    *chez-name*
    " --script
"
    ))

 ((eqv? *operating-system* 'windows)
  (scmxlate-insert
   (string-append
    "\":\";dosify=$(echo $0|sed -e 's,^//\\(.\\)/,\\1:/,')
\":\";echo \"(define arg-one \\\"$1\\\")(load \\\"$dosify\\\")(exit)\"|exec "
    *chez-name*
    ";exit
")))
 )

(scmxlate-cond
 ((eqv? *dialect* 'petite)
  ;(define *scheme-version* "Petite Chez Scheme")
  (define *scheme-version* (scheme-version))
  )
 ((eqv? *dialect* 'chez)
  (define *scheme-version* "Chez Scheme")))

(scmxlate-uncall
 require
 trace
 )

(scmxlate-rename
  (eof #!eof)
  (error petite-error)
  (get-char t2p-get-char)
  (substring subseq)
  )

;(define get-arg1
;  (lambda ()
;    (and (top-level-bound? 'arg-one) arg-one)))

#|
(scmxlate-cond
  ((eqv? *operating-system* 'unix-doesnt-work)
   (define (file-or-directory-modify-seconds f)
     (let* ((x (process
                 (string-append "stat -c \"%Y\" " f)))
            (n (read (car x))))
       (close-input-port (car x))
       (close-output-port (cadr x))
       n)))
  (else
    (define file-or-directory-modify-seconds
      (lambda (f) #f))))
|#

(define-syntax defstruct
  (let* ((old-datum->syntax-object datum->syntax-object)
         (datum->syntax (lambda (so output)
                          (old-datum->syntax-object
                            (syntax-case so () ((k . stuff) #'k))
                            output))))
    (lambda (%so)
      (datum->syntax %so
        (let ((%so-d (syntax->datum %so)))
          (apply (lambda (s . ff)
                   (let ((s-s (symbol->string s)) (n (length ff)))
                     (let* ((n+1 (+ n 1))
                            (vv (make-vector n+1)))
                       (let loop ((i 1) (ff ff))
                         (if (< i n+1)
                             (let ((f (car ff)))
                               (vector-set! vv i (if (pair? f) (cadr f) (not 't)))
                               (loop (+ i 1) (cdr ff)))
                             0))
                       (let* ((ff-without-colons
                                (map (lambda (f)
                                       (symbol->string (if (pair? f) (car f) f))) ff))
                              (ff-with-colons
                                (map (lambda (f)
                                       (string->symbol (string-append ":" f)))
                                     ff-without-colons)))
                         `(begin
                            (define ,(string->symbol (string-append "make-" s-s))
                              (lambda fvfv
                                (let ((st (make-vector ,n+1)) (ff ',ff-with-colons))
                                  (vector-set! st 0 ',s)
                                  ,@(let loop ((i 1) (r '()))
                                      (if (>= i n+1) r
                                          (loop (+ i 1)
                                                (cons `(vector-set! st ,i
                                                                    ,(vector-ref vv i))
                                                      r))))
                                  (let loop ((fvfv fvfv))
                                    (if (null? fvfv) 0
                                        (begin
                                          (vector-set! st (+ (list-position (car fvfv) ff) 1)
                                                       (cadr fvfv))
                                          (loop (cddr fvfv)))))
                                  st)))
                            ,@(let loop ((i 1) (procs '()))
                                (if (>= i n+1) procs
                                    (loop (+ i 1)
                                          (let* ((f-s (list-ref ff-without-colons (- i 1))))
                                            (cons
                                              `(define ,(string->symbol
                                                          (string-append s-s "-" f-s))
                                                 (lambda (x) (vector-ref x ,i)))
                                              (cons
                                                `(define ,(string->symbol
                                                            (string-append
                                                              "set!" s-s "-" f-s))
                                                   (lambda (x v) (vector-set! x ,i v)))
                                                procs))))))
                            (define ,(string->symbol (string-append s-s "?"))
                              (lambda (x)
                                (and (vector? x) (eq? (vector-ref x 0) ',s))))))))
                   )
                 (cdr %so-d)))))))

(define (petite-error . args)
  (apply error #f args))

(define (read-line i)
  (let ((c (peek-char i)))
    (if (eof-object? c) c
        (let loop ((r '()))
          (let ((c (read-char i)))
            (if (or (eof-object? c) (char=? c #\newline))
                (list->string (reverse! r))
                (loop (cons c r))))))))

;(scmxlate-include "seconds-to-date.scm")

(define (call-with-input-string s p)
  (let* ((i (open-input-string s))
         (v (p i)))
    (close-input-port i)
    v))

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

;(define seconds-to-human-time
;  (lambda (secs)
;    ""))

(define (current-seconds)
  (time-second (current-time)))

(define (file-or-directory-modify-seconds f)
  (time-second (file-modification-time f)))

(define (seconds-to-human-time s)
  (let ((d (date-and-time
             (time-utc->date
               (make-time 'time-utc 0 s)))))
    (string-append
      (substring d 0 10)
      ","
      (substring d 20 24))))

(define (set-start-time)
  (let* ((dat (date-and-time))
         (mo (substring dat 4 7))
         (dy (string->number (substring dat 8 10)))
         (hr (string->number (substring dat 11 13)))
         (mi (string->number (substring dat 14 16)))
         (yr (string->number (substring dat 20 24))))
    (set! mo
      (cdr (assoc mo
                  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
                                ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
                                ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))
    (tex-def-count "\\time" (+ (* 60 hr) mi) #t)
    (tex-def-count "\\day" dy #t)
    (tex-def-count "\\month" mo #t)
    (tex-def-count "\\year" yr #t)))

(scmxlate-cond
 ((eqv? *operating-system* 'windows)
  (define *ghostscript* "d:\\aladdin\\gs6.0\\bin\\gswin32c")
  ))

(scmxlate-ignore main)

(scmxlate-ignore-define
  decode-universal-time
  strftime)

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

(scmxlate-postamble)

(let ((pa (command-line-arguments)))
  (tex2page (car pa)))
