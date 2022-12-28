; last change: 2022-12-28

(scmxlate-cond
  ((eqv? *operating-system* 'unix)
   (scmxlate-insert
     "\":\";exec gosh -- $0 \"$@\"\n")))

(define *scheme-version* 
  (string-append "Gauche " (gauche-version)))

(scmxlate-ignoredef
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

(define *tex2page-namespace* (interaction-environment))

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

(define (disable-load-for-tex2page f) #f)

(define (file-or-directory-modify-seconds f)
  (sys-stat->mtime (sys-stat f)))

(define (date-day tm) (slot-ref tm 'mday))
(define (date-hour tm) (slot-ref tm 'hour))
(define (date-minute tm) (slot-ref tm 'min))
(define (date-month tm) (slot-ref tm 'mon))
(define (date-year tm) (slot-ref tm 'year))
(define (date-week-day tm) (slot-ref tm 'wday))
(define (date-dst? tm) (slot-ref tm 'isdst))
(define (date-time-zone-offset tm) #f)

(define (string-upcase s)
  (list->string (map char-upcase (string->list s))))

(define (index-of s x)
  (let loop ((s s) (i 0))
    (cond ((null? s) false)
          ((eq? (car s) x) i)
          (else (loop (cdr s) (+ i 1))))))

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

(define (ormap f s)
  ;Returns true if f is true of some elt in s
  (let loop ((s s))
    (if (null? s) #f
        (or (f (car s)) (loop (cdr s))))))

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

(define (main args)
  (tex2page
    (and (>= (length args) 2)
         (list-ref args 1))))
