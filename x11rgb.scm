; x11rgb.scm
; Dorai Sitaram
; Generates x11rgb.tex, which contains color definitions 
; for the names in X11's rgb.txt.

(define capitalized
  (lambda (s)
    (let ((n (string-length s)))
      (if (<= n 0) #f
          (let ((c (string-ref s 0)))
            (if (and (char-alphabetic? c) (char-lower-case? c))
                (string-append (string (char-upcase c))
                               (substring s 1 n))
                #f))))))

(define *predefined-names*
  '("red" "blue" "green"
          "cyan" "magenta" "yellow" "black"
          "white"))

(define *names-not-in-rgb.txt*
  '(
    ("crimson" 220 20 60)
    ("olive" 128 128 0)
    ("MediumGoldenrod" 234 234 173)
    ("SeaShell" 255 245 238)
    ("MediumForestGreen" 107 142 35)
    ("lime" 0 255 0)
    ("aqua" 0 255 255)
    ("teal" 0 128 128)
    ("indigo" 75 0 130)
    ("fuchsia" 255 0 255)
    ("silver" 192 192 192)
    ))

(define display-color-def 
  (lambda (name r g b o)
    (let* ((name (regexp-replace* "[ \t]+" name ""))
           (names (list name))
           (cname (capitalized name)))
      (when cname (set! names (cons cname names)))
      (for-each
       (lambda (name)
         (unless (member name *predefined-names*)
           (fprintf o "\\definecolor{~a}{rgb}{~a, ~a, ~a}%~%"
                    name
                    (/  r 255.)
                    (/  g 255.)
                    (/  b 255.))))
       names))))

(cond ((file-exists? "rgb.txt")
       (call-with-input-file "rgb.txt"
         (lambda (i)
           (call-with-output-file "x11rgb.tex"
             (lambda (o)
               (fprintf o "% x11rgb.tex~%")
               (fprintf o "% Dorai Sitaram~%")
               (fprintf o "% Color definitions for the names in X11's rgb.txt~%~%")
               (fprintf o "\\ifx\\shipout\\UnDeFiNeD~%")
               (fprintf o "% Not needed for HTML, as most color-capable browsers predefine them.~%")
               (fprintf o "\\else~%")
               (let loop ()
                 (let ((x (read-line i)))
                   (unless (eof-object? x)
                     (cond ((regexp-match 
                             "^[ \t]*([0-9]+)[ \t]+([0-9]+)[ \t]+([0-9]+)[ \t]+([a-zA-Z ]+)[ \t]*$" 
                             x) =>
                            (lambda (r)
                              (display-color-def (list-ref r 4)
                                                 (string->number (list-ref r 1))
                                                 (string->number (list-ref r 2))
                                                 (string->number (list-ref r 3))
                                                 o))))
                     (loop))))
               (for-each (lambda (x)
                           (display-color-def
                             (list-ref x 0)
                             (list-ref x 1)
                             (list-ref x 2)
                             (list-ref x 3)
                             o))
                         *names-not-in-rgb.txt*)
               (fprintf o "\\fi~%"))
             'replace))))
      (else
       (printf "Copy X11's rgb.txt to current directory and retry.~%~
                rgb.txt is somewhere under /usr/X11.~%")))
