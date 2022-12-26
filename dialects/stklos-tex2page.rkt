;last change: 2022-12-26

(scmxlate-cond
  ((eqv? *operating-system* 'unix)
   (scmxlate-insert
     "#!/usr/bin/env stklos-script\n")))

(define *scheme-version*
  (string-append "STklos " (version)))

(scmxlate-ignoredef
  *tex2page-namespace*
  table
  )

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page)

(scmxlate-rename
  (ormap any)
  (remove delete)
  (substring subseq)
  )

(define (eval1 e)
  (eval e (interaction-environment)))

(define (make-table . z)
  (if (null? z)
      (make-hash-table)
      (make-hash-table (cadr z))))

(define (table-get k ht . d)
  (hash-table-ref ht k
                  (lambda ()
                    (if (null? d) #f (car d)))))

(define (table-put! k tbl v)
  (hash-table-set! tbl k v))

(define (table-for-each p tbl)
  (hash-table-for-each tbl p))

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

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

(define (string=split s sep)
  (string-split s (string sep)))

(define (file-or-directory-modify-seconds f)
  (let-values ([(tmpf o) (create-temp-file)])
    (close-port o)
    (system (string-append "stat -c %W " f " > " tmpf))
    (let ([n (call-with-input-file tmpf read)])
      (delete-file tmpf)
      n)))

(scmxlate-include "seconds-to-date.scm")

(scmxlate-postamble)

(let ((cla (argv)))
  (tex2page (and (> (length cla) 0) (car cla))))
