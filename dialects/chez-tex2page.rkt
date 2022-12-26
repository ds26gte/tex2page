; last change: 2022-12-26

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
    " --script\n\n"
    ))

 ((eqv? *operating-system* 'windows)
  (scmxlate-insert
    "\":\";dosify=$(echo $0|sed -e 's,^//\\(.\\)/,\\1:/,')
    \":\";echo \"(define arg-one \\\"$1\\\")(load \\\"$dosify\\\")(exit)\"|exec "
    *chez-name*
    ";exit\n"))
 )

(define *scheme-version* (scheme-version))

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-ignoredef
  *tex2page-namespace*
  )

(scmxlate-rename
  (error chez-error)
  (get-char t2p-get-char)
  (substring subseq)
  (date-time-zone-offset date-zone-offset)
  )

(scmxlate-ignoredef-rename
  (ensure-file-deleted delete-file)
  )

(define (eval1 e)
  (eval e (interaction-environment)))

(define (chez-error . args)
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

(define index-of
  (lambda (s x)
    (let loop ((s s) (i 0))
      (cond ((null? s) false)
            ((eq? (car s) x) i)
            (else (loop (cdr s) (+ i 1)))))))

(define (subseq s i . z)
  (let ((f (if (pair? z) (car z) (string-length s))))
    (substring s i f)))

(define (current-seconds)
  (time-second (current-time)))

(define (file-or-directory-modify-seconds f)
  (time-second (file-modification-time f)))

(define (seconds->date s)
  (time-utc->date (make-time 'time-utc 0 s)))

(scmxlate-cond
 ((eqv? *operating-system* 'windows)
  (define *ghostscript* "d:\\aladdin\\gs6.0\\bin\\gswin32c")
  ))

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

(let ((cla (command-line-arguments)))
  (tex2page
    (and (>= (length cla) 1) (car cla))))
