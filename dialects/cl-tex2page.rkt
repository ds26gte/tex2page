; last change: 2022-12-26

;works with CLISP, Clozure CL, CMU CL, ECL, SBCL; but scmxlate
;translation to CL is mostly for verification (of scmxlate?) these days.
;Use handwritten tex2page.lisp in the distribution, which is cleaner
;and faster

#+clisp
(scmxlate-insert
 "\":\"; exec clisp -q -x '"
 "(progn (setq *load-verbose* nil) (defvar arg-one \"'$1'\") (values)) "
 "(progn (load \"'$0'\") (values)) "
 "(progn (tex2page:tex2page arg-one) (values)) "
 "(exit)'"
 "
")

#+clozure
(scmxlate-insert
 "\":\"; exec ccl "
 "-e '(defvar arg-one \"'$1'\")' "
 "-l $0 "
 "-e '(tex2page:tex2page arg-one)' "
 "-e '(quit)'"
 "
")

#+ecl
(scmxlate-insert
  "\":\";exec ecl "
  "-eval '(defvar arg-one \"'$1'\")' "
  "-load $0 "
  "-eval '(tex2page:tex2page arg-one)' "
  "-eval '(quit)'"
  "
  ")

#+mkcl
(scmxlate-insert
  "\":\";exec mkcl "
  "-eval '(defvar arg-one \"'$1'\")' "
  "-load $0 "
  "-eval '(tex2page:tex2page arg-one)' "
  "-eval '(quit)'"
  "
  ")

#+sbcl
(scmxlate-insert
 "\":\"; exec sbcl --noinform --noprint "
 "--eval '(defvar arg-one \"'$1'\")' "
 "--load $0 "
 "--eval '(tex2page:tex2page arg-one)' "
 "--eval '(quit)'"
 "
")

#+allegro
(scmxlate-insert
 "\":\"; exec lisp -batch "
 "-e '(defvar arg-one \"'$1'\")' "
 "-L $0 "
 "-e '(tex2page:tex2page arg-one)' "
 "-e '(exit)'"
 "
")

#+cmu
(scmxlate-insert
 "\":\"; exec lisp -noinit "
 "-eval '(defvar arg-one \"'$1'\")' "
 "-load $0 "
 "-eval '(tex2page:tex2page arg-one)' "
 "-eval '(quit)'"
 "
")

#+abcl
(scmxlate-insert
 "\":\"; echo '(load \"'$0'\")(tex2page:tex2page \"'$1'\")' |abcl"
 "
 ")

#+cmu-obsolete
(scmxlate-insert
 "\":\"; echo '(defvar arg-one \"'$1'\") "
 "(load \"'$0'\") (quit)' | "
 "exec lisp; exit"
 "
")

(scmxlate-insert
 "
;Common Lisp version of
;tex2page
;Dorai Sitaram
;For details, see
;http://www.ccs.neu.edu/~dorai/tex2page/tex2page-doc.html

;The code following this is a computer-generated
;translation of the Scheme file tex2page into
;Common Lisp.

;It may not be very human-readable.

#+sbcl
(declaim (sb-ext:muffle-conditions style-warning))

 ")

(scmxlate-ignoredef
  decode-universal-time
  strftime
  subseq
  write-to-string
  )

(defpackage :tex2page
  (:use :cl
   #+clozure :ccl
   #+cmu :ext
   #+ecl :si
   #+mkcl :mkcl
   #+sbcl :sb-ext
   )
  (:export :tex2page))

(in-package :tex2page)

(defvar *this-package* *package*)

(defvar *operating-system*
  #+win32 'windows
  #-win32 'unix)

#+(and cmu unix)
(defun system (cmd)
  (ext:run-program
   "/bin/sh" (list "-c" cmd)))

#+(and sbcl unix)
(defun system (cmd)
  (sb-ext::run-program "sh" (list "-c" cmd) :search t :output t))

#+(and (or allegro clisp) win32)
(defun system (cmd)
  (shell
   (concatenate 'string "cmd /c " cmd)))

#+cmu
(defun getenv (v)
  (cdr (assoc (intern v :keyword) ext:*environment-list*)))

(scmxlate-ignore
; *operating-system*
; *shell-cmd-prefix*
 *package*
 getenv
 ;list-position
 defstruct
 ;string-index
 ;string-reverse-index
; sort!
 table
 nreverse
 nconc
 with-output-to-port
 main)

;(defun seconds->date (s)
;  (multiple-value-list
;    (decode-universal-time s)))

(defun seconds-to-human-time (s)
  (multiple-value-bind (sec m h d mo y dow dst tz)
      (decode-universal-time s)
      (declare (ignore sec))
    (format nil "~a, ~a ~a, ~a, ~a:~a~a ~am ~a ~a"
            (svref *week-day-names* dow)
            (svref *short-month-names* (1- mo))
            d
            y
            (let ((h (mod h 12)))
              (if (= h 0) 12 h))
            (if (< m 10) "0" "")
            m
            (if (<= 0 h 11) "a" "p")
            (format nil "UTC~a~a"
                    (if (> tz 0) "&minus;" "+")
                    (abs tz))
            (if dst "+1" ""))))

;;(defun date-second (v) (elt v 0))
;(defun date-minute (v) (elt v 1))
;(defun date-hour (v) (elt v 2))
;(defun date-day (v) (elt v 3))
;(defun date-month (v) (elt v 4))
;(defun date-year (v) (elt v 5))
;(defun date-week-day (v) (elt v 6))

(defun set-start-time ()
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time) 5) ;don't care about TZ
    (declare (ignore s mo))
    (tex-def-count "\\time" (+ (* 60 h) m) t)
    (tex-def-count "\\day" d t)
    (tex-def-count "\\month" m t)
    (tex-def-count "\\year" y t)))

(defun !last-modification-time (s &optional (epoch 1900))
  (setq
   *last-modification-time*
   (ecase epoch
     (1900 s)
     (1970 (+ s #.(encode-universal-time 0 0 0 1 1 1970 0))))))

(defun string->number (s &optional (base 10))
  (if (position #\: s :test #'char=) nil
    (let ((n (let ((*read-base* base)) (read-from-string s nil))))
      (if (numberp n) n nil))))

(defun valid-img-file? (f) nil)

(defun string-index (s c)
  (position c s :test #'char=))

(defun string-reverse-index (s c)
  (position c s :test #'char= :from-end t))

(scmxlate-rename
 (current-seconds #'get-universal-time)
 (eof :eof-object)
 (file-or-directory-modify-seconds #'file-write-date)
 (string-index #'string-index)
 (string-reverse-index #'string-reverse-index)
 )

(scmxlate-ignoredef-rename
  (cl-with-output-to-string with-output-to-string)
  (*epoch* 1900)
  (eval1 #'eval)
  (list-position #'position)
  (*return* #\return)
  (string-trim #'string-trim-blanks)
  (substring? #'search)
  (*tab* #\tab)
  (table-for-each #'maphash)
  (table-get gethash)
  )

(scmxlate-uncall
 trace
 require)

(defun make-table (&rest z)
  (if (null z)
      (make-hash-table)
      (make-hash-table :test 'equal)))

(defun table-put! (k ht v)
  (setf (gethash k ht) v))


(setq *print-case* :downcase)

(setq *load-verbose* nil)

#+sbcl
(setq *compile-verbose* nil)

(defvar *common-lisp-version*
  (concatenate 'string
               (lisp-implementation-type) " "
               (lisp-implementation-version)))

(scmxlate-ignoredef-rename
 (*scheme-version* *common-lisp-version*))

(defun number-to-roman (n &optional upcasep)
  (format nil
    (if upcasep "~@r" "~(~@r~)") n))

(defun lassoc (k al equ?)
  (assoc k al :test equ?))

(defun ldelete (y xx equ?)
  (remove y xx :test equ?))

(defun member/string-ci=? (s ss)
  (member s ss :test #'string-equal))

(defun string-trim-blanks (s)
  (string-trim '(#\space #\tab #\newline #\return) s))

#|
(defun terror (where &rest args)
  (mapc #'write-log args)
  (error "~a" where))
|#

(defun no-aux-dir? ()
  (or (probe-file (make-pathname :name ".no-tex2page-aux-dir"))
      (probe-file (make-pathname :name ".no-tex2page-aux"))))

;(scmxlate-include "fake-strftime.lsp")

;(defun strftime (&rest ee) (apply #'strftime-like ee))

;(defvar *tab* #\tab)

;(defun get-arg1 ()
;  (and (boundp 'arg-one)
;       (not (string= arg-one ""))
;       arg-one))

;kludge to let define and set! work inside \eval
;regardless of whether CL or Scheme is used

;(defmacro define (&rest ee)
;  `(defvar ,@ee))
;
;(defmacro set! (&rest ee)
;  `(setq ,@ee))
;
;(defun display (&rest ee)
;  (apply #'princ ee))

(scmxlate-postprocess

 #+(or unix darwin)
 (scmxlate-system "chmod +x my-tex2page")

 #+win32
 (with-open-file (o "tex2page.bat" :direction :output
                    :if-exists :supersede)
   (format o "@echo off ~%~
if exist c:\\_temp.lsp del c:\\_temp.lsp ~%~
echo (defvar arg1 \"%1\") >> c:\\_temp.lsp ~%~
echo (load \"d:/public_html/tex2page/my-tex2page\") >> c:\\_temp.lsp ~%~
echo (exit) >> c:\\_temp.lsp ~%~
~%~
clisp -q -i c:\\_temp.lsp ~%"))

 )
