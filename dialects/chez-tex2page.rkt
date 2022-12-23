; last change: 2022-12-23

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

(define *scheme-version* (scheme-version))

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-rename
  (error petite-error)
  (get-char t2p-get-char)
  (substring subseq)
  )

(define (eval1 e)
  (eval e (interaction-environment)))

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

(scmxlate-ignore-define
  *tex2page-namespace*
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
