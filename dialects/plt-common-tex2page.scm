;last change: 2006-05-22

(scmxlate-rename-define
 (eval1 eval))

(scmxlate-uncall
 require
 trace
 )

(scmxlate-cond
  ((<= *dialect-version* 299)
   (define string-upcase
     (lambda (s)
       (list->string
         (map char-upcase 
              (string->list s)))))))

(scmxlate-cond
((>= *dialect-version* 200)

  (scmxlate-ignore
   table)

  (scmxlate-rename-define
   (table-put! hash-table-put!)
   (table-for-each hash-table-for-each)
   )

  (define make-table
    (lambda z
      (if (null? z)
          (make-hash-table)
          (make-hash-table 'equal))))

  (define table-get
    (lambda (ht k . d)
      (hash-table-get ht k
                      (let ((d (if (null? d) #f (car d))))
                        (lambda () d)))))
  ))

(scmxlate-rename-define
  (*january-number* 1)
  (*sunday-number* 0)
  (*anno-domini-at-0* 0)
  )
