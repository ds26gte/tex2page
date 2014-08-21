(define make-temp-filename 
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (let ((f (string-append "Scheme_Temp_file_" 
                              (number->string n))))
        (if (file-exists? f)
            ;try again
            (create-temp-file)
            f)))))
