;created < 2014-08-21
;last modified 2022-12-26

(define make-temp-filename
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (let ((f (string-append "Scheme_Temp_file_" 
                              (number->string n))))
        (if (file-exists? f)
            ;try again
            (make-temp-filename)
            f)))))
