; Dorai Sitaram
; Generates x11rgb.tex, which contains color definitions
; for the names in X11's rgb.txt.
; Jan 31, 2003 (Scheme ver.)
; 2009-03-30 (CL)
; last change 2009-04-01

(load (merge-pathnames "../pregexp/pregexp.lisp"))

(defun capitalized (s)
  (unless (= (length s) 0)
    (let ((c (char s 0)))
      (when (and (alpha-char-p c) (lower-case-p c))
        (concatenate 'string (string (char-upcase c))
          (subseq s 1))))))

(defvar *predefined-names*
  '("red" "blue" "green"
    "cyan" "magenta" "yellow" "black"
    "white"))

(defvar *names-not-in-rgb.txt*
  '(("crimson" 220 20 60)
    ("olive" 128 128 0)
    ("MediumGoldenrod" 234 234 173)
    ("SeaShell" 255 245 238)
    ("MediumForestGreen" 107 142 35)
    ("lime" 0 255 0)
    ("aqua" 0 255 255)
    ("teal" 0 128 128)
    ("indigo" 75 0 130)
    ("fuchsia" 255 0 255)
    ("silver" 192 192 192)))

(defun display-color-def (name r g b o)
  (let* ((name (remove-if (lambda (c) (char= c #\space))
                          name))
         (names (list name))
         (cname (capitalized name)))
    (when cname (push cname names))
    (dolist (name names)
      (unless (member name *predefined-names* :test #'string=)
        (format o "\\definecolor{~a}{rgb}{~a, ~a, ~a}%~%"
                name
                (/ r 255.0)
                (/ g 255.0)
                (/ b 255.0))))))

(block :head
  (with-open-file (i "rgb.txt" :direction :input :if-does-not-exist nil)
    (unless i
      (format t "Copy X11's rgb.txt to current directory and retry.~%~
                rgb.txt is somewhere under /usr/X11.~%")
      (return-from :head))
    (with-open-file (o "x11rgb.tex" :direction :output :if-exists :supersede)
      (format o "% x11rgb.tex~%")
      (format o "% Dorai Sitaram~%")
      (format o "% Color definitions for the names in X11's rgb.txt~%~%")
      (format o "\\ifx\\shipout\\UnDeFiNeD~%")
      (format o "% Not needed for HTML, as most color-capable browsers~
              predefine them.~%")
      (format o "\\else~%")
      (loop
        (let ((x (read-line i nil)))
          (unless x (return))
          (let ((r (pregexp-match
                    "^ *([0-9]+) +([0-9]+) +([0-9]+)[ \\t]+([a-zA-Z ]+) *$"
                    x)))
            (when r
              (display-color-def
               (elt r 4)
               (read-from-string (elt r 1))
               (read-from-string (elt r 2))
               (read-from-string (elt r 3))
               o)))))
      (dolist (x *names-not-in-rgb.txt*)
        (display-color-def
         (elt x 0)
         (elt x 1)
         (elt x 2)
         (elt x 3)
         o))
      (format o "\\fi~%"))))
