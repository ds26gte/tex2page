;no command-line arg.  Just load and call tex2page proc
(define get-arg1
  (lambda ()
    #f))

(define *scheme-version* 
  (string-append "Pocket Scheme "
                 (let ((v (map number->string (version))))
                   (string-append
                    (list-ref v 0) "."
                    (list-ref v 1) "."
                    (list-ref v 2) "."
                    (list-ref v 3)))))

(scmxlate-uncall
  require
  )
 
(define getenv
  (lambda (env-var)
    #f))
 
(define ormap
  (lambda (f s)
    ;Returns true if f is true of some elt in s
    (let loop ((s s))
      (if (null? s) #f
          (or (f (car s)) (loop (cdr s)))))))
  
 (define andmap
   (lambda (f s)
     (let loop ((s s))
       (if (null? s) #t
           (and (f (car s)) (loop (cdr s)))))))

(define eof
  (let* ((i (open-input-string ""))
         (eof (read i)))
    (close-input-port i)
    eof))
 
(define-macro unless
  (lambda (b . ee)
    `(if (not ,b) (begin ,@ee))))

(define-macro when
  (lambda (b . ee)
    `(if ,b (begin ,@ee))))

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

(define file-or-directory-modify-seconds
  (lambda (f)
    #f))

(define decode-seconds-since-epoch
  (lambda (secs)
    #f))

(define current-seconds
  (lambda () 
    #f))

(define file-exists?
   (lambda (f)
     (member f (directory-list "."))))

(define flush-output force-output)

;(define)

; ex:ft=scheme
