(define *scheme-version* "UMB Scheme")

(scmxlate-uncall
 require)


(define getenv
  (lambda (envvar)
    #f))

(defmacro unless (b . ee)
  `(if (not ,b) (begin ,@ee)))

(defmacro when (b . ee)
  `(if ,b (begin ,@ee)))
