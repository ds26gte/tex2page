; last change: 2017-01-10

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   (string-append
    "\":\";"
    "echo '(set-repl-printer! (lambda (x . y) #f)) "
    "(define arg-one \"'$1'\")"
    "(loadq \"'$0'\")"
    "(main (list 1 arg-one))"
    "'|bigloo -s; exit\n\n"
    ))))

;(define arg1 (cadr (command-line)))

(define *scheme-version*
  (string-append "Bigloo " *bigloo-version*))

;(define *scheme-version* "Bigloo 2.5b")

(scmxlate-rename
 (current-directory chdir)
 (integer->char integer->ucs2)
; (file-or-directory-modify-seconds file-modification-time)
 )

;(define current-seconds
;  (lambda () #f))

(scmxlate-ignore
 with-output-to-port)

(scmxlate-ignore-define
  string-index)

(scmxlate-uncall
  require
 )

;(define get-arg1
;  (lambda ()
;    arg-one))

(define terror
  (lambda (where . args)
    (write-log #\newline)
    (write-log "! ")
    (for-each write-log args)
    (write-log #\newline)
    (when *current-tex-line-number*
      (write-log "l.")
      (write-log *current-tex-line-number*)
      (write-log #\space))
    (error where "" #f)))

;(define-macro (unless b . ee)
;  `(if (not ,b) (begin ,@ee)))

;(define-macro (when b . ee)
;  `(if ,b (begin ,@ee)))

(define eof (read (open-input-string "")))

(define flush-output
  (lambda z
    ;bigloo's flush-output-port always takes 1 arg
    (flush-output-port
      (if (null? z) (current-output-port) (car z)))))

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

(define-macro (fluid-let xvxv . ee)
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
	 ,res))))

(define-macro (defstruct s . ff)
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
               (map string->keyword ff-without-colons)))
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
               (and (vector? x) (eq? (vector-ref x 0) ',s)))))))))

#|
(define-macro defstruct
  (lambda (s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (unless (>= i n+1)
            (let ((f (car ff)))
              (vector-set! vv i (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f)) ff)))
          `(begin
             (define ,(string->symbol (string-append "MAKE-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (unless (null? fvfv)
                       (vector-set! st (+ (list-index ff (car fvfv)) 1) (cadr fvfv))
                       (loop (cddr fvfv))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol (string-append s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol (string-append "SET!" s-s "." f))
                                  (lambda (x v) (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x) (eq? (vector-ref x 0) ',s))))))))))
|#

;(scmxlate-include "seconds-to-date.scm")

(define set-start-time
  (lambda ()
    (let* ((dt (string-downcase (date)))
           (re (pregexp
                (string-append
                "([a-z]+) ([a-z]+) ([0-9]+) "
                 "([0-9]+):([0-9]+):([0-9]+) ([0-9]+)")))
           (mo (string->symbol (pregexp-replace re dt "\\2")))
           (dy (string->number (pregexp-replace re dt "\\3")))
           (hr (string->number (pregexp-replace re dt "\\4")))
           (mi (string->number (pregexp-replace re dt "\\5")))
           (yr (string->number (pregexp-replace re dt "\\7"))))
      (set! mo
        (cdr (assv mo
                   '((jan . 1) (feb . 2) (mar . 3) (apr . 4)
                     (may . 5) (jun . 6) (jul . 7) (aug . 8)
                     (sep . 9) (oct . 10) (nov . 11) (dec . 12)))))
      (tex-def-count "\\time" (+ (* 60 hr) mi) #t)
      (tex-def-count "\\day" dy #t)
      (tex-def-count "\\month" mo #t)
      (tex-def-count "\\year" yr #t))))
