(define with-output-to-port
  (lambda (o th)
    (let* ((f (make-temp-filename))
           (r (with-output-to-file f th)))
      (call-with-input-file f
        (lambda (i)
          (let loop ()
            (let ((c (read-char i)))
              (if (not (eof-object? c))
                  (begin
                   (write-char c o)
                   (loop)))))))
      (delete-file f)
      r)))
