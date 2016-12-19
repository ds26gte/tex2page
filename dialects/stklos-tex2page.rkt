;last change: 2003-06-21

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (scmxlate-insert
   "\":\"; exec stklos -f $0 -- \"$@\""
   "

")))

(define *scheme-version* "STklos")


(scmxlate-ignore
 table
 main
 call-with-input-string
 with-output-to-port)

(scmxlate-uncall
 require)

(scmxlate-rename-define
 (table-put! hash-table-put!)
 (table-for-each hash-table-for-each))

(define make-table
  (lambda z
    (if (null? z)
        (make-hash-table)
        (make-hash-table (cadr z)))))

(define table-get
  (lambda (ht k . d)
    (hash-table-get ht k
                    (if (null? d) #f (car d)))))

(define andmap every)

(scmxlate-rename
 ;STKlos seems to have primitives named emit, main
 (emit em1t) 
 (main ma1n)
 (string-index str1ng-index)
 (ormap any)
 (andmap every)
 ;label as a struct name fails (?)
 (label lab3l)
 (make-label make-lab3l)
 (label.src lab3l.src)
 (label.page lab3l.page)
 (label.name lab3l.name)
 (label.value lab3l.value)
 )

(define get-arg1
  (lambda ()
    (let ((pa (argv)))
      ;argv used to be a vector in older versions
      (cond ((vector? pa)
             (and (> (vector-length pa) 0) (vector-ref pa 0)))
            ((list? pa)
             (and (> (length pa) 0) (list-ref pa 0)))))))

(scmxlate-cond
 ((eqv? *operating-system* 'unix)
  (define delete-file
    (lambda (f)
      (system (string-append "rm " f))))
  )

((eqv? *operating-system* 'windows)
 (define delete-file
   (lambda (f)
     (system (string-append "del " f))))
 ))

;(define delete-file
;  (lambda (f) #t))


(define file-or-directory-modify-seconds
  (lambda (f) #f))

(define eof (call-with-input-string "" read))

;(define reverse!
;  (lambda (s)
;    (let loop ((s s) (r '()))
;      (if (null? s) r
;	  (let ((d (cdr s)))
;            (set-cdr! s r)
;	    (loop d s))))))

;(define ormap
;  (lambda (f s)
;    ;Returns true if f is true of some elt in s
;    (let loop ((s s))
;      (if (null? s) #f
;        (or (f (car s)) (loop (cdr s)))))))


(scmxlate-include "seconds-to-date.scm")

(scmxlate-postamble)

(tex2page (get-arg1))
