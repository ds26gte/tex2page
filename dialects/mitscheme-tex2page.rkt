;last change: 2022-12-26

(scmxlate-cond
  ((eqv? *operating-system* 'unix)
   (scmxlate-insert
     (string-append
       "\":\";"
       "exec mit-scheme "
       "--load $0 "
       "-- $1\n\n"
       ))))

(declare (usual-integrations))

(define *scheme-version*
  (string-append "MIT Scheme " (get-subsystem-version-string "release")))

(scmxlate-ignoredef
  *tex2page-namespace*
  )

(scmxlate-uncall
  define-namespace-anchor
  require
  tex2page
  )

(scmxlate-rename
 (date-day decoded-time/day)
 (date-hour decoded-time/hour)
 (date-minute decoded-time/minute)
 (date-month decoded-time/month)
 (date-year decoded-time/year)
 (seconds->date file-time->local-decoded-time)
 (list* cons*)
 (add1 1+)
 (sub1 -1+)
 (remove delete)
 )

(scmxlate-ignoredef-rename
  (seconds-to-human-time file-time->local-time-string)
  (string-index string-find-next-char)
  (string-reverse-index string-find-previous-char)
  (substring? string-search-forward)
  )

(define ormap
  (lambda (f s)
    (there-exists? s f)))

(define eval1 (lambda (x) (eval x user-initial-environment)))

(define current-seconds
  (lambda ()
    (universal-time->file-time
     (get-universal-time))))

(scmxlate-postamble)

(let ((cla (command-line-arguments)))
  (when (> (length cla) 0)
    (tex2page (car cla))
    (exit 0)))
