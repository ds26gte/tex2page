; last change 2012-04-21

(scmxlate-uncall
    require
    )

(define eval1
  (lambda (e)
    (eval e (scheme-report-environment 5))))

(define *scheme-version*
  "Ikarus Scheme")

    

(define main
  (lambda ()
    (tex2page (car (command-line)))))
