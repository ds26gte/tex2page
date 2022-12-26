; last change: 2022-12-26

(scmxlate-cond
  ((eqv? *operating-system* 'unix)
   (scmxlate-insert
     "\":\";exec gosh -- $0 \"$@\"\n")))

(define *scheme-version* 
  (string-append "Gauche " (gauche-version)))

(scmxlate-ignoredef
  *tex2page-namespace*
  with-output-to-port
  call-with-input-string
  )

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-rename
  (load disable-load-for-tex2page)
  (remove delete)
  (seconds->date sys-localtime)
  (substring subseq)
  )

(scmxlate-ignoredef-rename
  (strftime sys-strftime) 
  )

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define string-trim
  (lambda (s)
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
            (substring s i n))))))

(define (disable-load-for-tex2page f) #f)

(define file-or-directory-modify-seconds
  (lambda (f)
    (sys-stat->mtime (sys-stat f))))

(define date-day (lambda (tm) (slot-ref tm 'mday)))
(define date-hour (lambda (tm) (slot-ref tm 'hour)))
(define date-minute (lambda (tm) (slot-ref tm 'min)))
(define date-month (lambda (tm) (slot-ref tm 'mon)))
(define date-year (lambda (tm) (slot-ref tm 'year)))
(define date-week-day (lambda (tm) (slot-ref tm 'wday)))
(define date-dst? (lambda (tm) (slot-ref tm 'isdst)))
(define date-time-zone-offset (lambda (tm) #f))

(define (string-upcase s)
  (list->string (map char-upcase (string->list s))))

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

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

(define eval1
  (lambda (e)
    (eval e (interaction-environment))))

(define main
  (lambda (args)
    (tex2page
      (and (>= (length args) 2)
           (list-ref args 1)))))
