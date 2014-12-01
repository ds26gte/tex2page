":"; if test -z "$LISP"; then
":";   if test "$USER" = evalwhen; then LISP=ecl
":";   elif test $(hostname | grep TUSMA.*RLKVT); then LISP=clisp
":";   else LISP=sbcl
":";   fi
":"; fi
":"; if test "$USER" = ds26 -a "$HOSTNAME" = fun; then
":";   export LANG=en_US.UTF-8
":"; fi
":"; if test "$LISP" != sbcl; then export T2PARG=$1; fi
":"; if test "$LISP" = abcl; then exec abcl --load $0 --eval '(ext::quit)'
":"; elif test "$LISP" = allegro; then exec alisp -L $0 -kill
":"; elif test "$LISP" = clisp; then exec clisp -q $0
":"; elif test "$LISP" = clozure; then exec ccl -l $0 -e '(ccl:quit)'
":"; elif test "$LISP" = cmucl; then exec lisp -quiet -load $0 -eval '(ext::quit)'
":"; elif test "$LISP" = ecl; then exec ecl -shell $0
":"; else exec sbcl --script $0 "$@"
":"; fi

;Common Lisp version of
;tex2page
;Dorai Sitaram
;For details, see
;http://www.ccs.neu.edu/~dorai/tex2page/index.html

#+sbcl
(declaim (sb-ext:muffle-conditions style-warning))

(defpackage :tex2page
  (:use :cl)
  (:export :tex2page))

(in-package :tex2page)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *print-case* :downcase
        *load-verbose* nil
        *compile-verbose* nil))

(defparameter *tex2page-version* "20141201c") ;last change

(defparameter *tex2page-website*
  ;for details, please see
  "http://www.ccs.neu.edu/~dorai/tex2page/index.html")

(defparameter *tex2page-copyright-notice*
  (format nil "Copyright (C) 1997-~a Dorai Sitaram"
          (subseq *tex2page-version* 0 4)))

(defun retrieve-env (s)
  #+abcl (ext:getenv s)
  #+allegro (sys:getenv s)
  #+clisp (ext:getenv s)
  #+clozure (ccl:getenv s)
  #+cmucl (cdr (assoc (intern s :keyword)
                      ext:*environment-list* :test #'string=))
  #+ecl (si:getenv s)
  #+sbcl (sb-ext:posix-getenv s))

(defparameter *tex2page-file-arg*
  (if (or #+sbcl t)
    (or #+sbcl (nth 1 sb-ext:*posix-argv*))
    (retrieve-env "T2PARG")))

(defparameter *common-lisp-version*
  (concatenate 'string (lisp-implementation-type) " "
               (lisp-implementation-version) " "
               #+linux "Linux"
               #+cygwin "Cygwin"
               #+darwin "Mac OS"
               #+sunos "Solaris"
               #+(and unix (not (or linux cygwin darwin sunos))) "Unix"
               #+windows "Windows"))

(defparameter *short-month-names*
  (vector "Jan" "Feb" "March" "April" "May" "June"
          "July" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defparameter *week-day-names*
  (vector "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))

(defparameter *enable-write-18-p*
  ;true if you want \write18{command} to execute command as an OS
  ;command; false if you think that it is a security risk for your
  ;situation
  t)

(defparameter *output-extension* ".html")

(defparameter *ghostscript*
  #-windows "gs"
  #+windows
  (or
    (some (lambda (f) (and (probe-file f) f))
          '("c:\\cygwin\\bin\\gs.exe" "g:\\cygwin\\bin\\gs.exe"
            "c:\\aladdin\\gs6.01\\bin\\gswin32c.exe"
            "d:\\aladdin\\gs6.01\\bin\\gswin32c.exe"
            "d:\\gs\\gs8.00\\bin\\gswin32.exe"
            "g:\\gs\\gs8.00\\bin\\gswin32.exe"))
    "gswin32.exe"))

(defparameter *use-closing-p-tag-p* t)

(defparameter *metapost* #-windows "mpost" #+windows "mp")

(defparameter *tex-prog-name* "pdftex")

(defparameter *navigation-sentence-begin* "Go to ")
(defparameter *navigation-first-name* "first")
(defparameter *navigation-previous-name* "previous")
(defparameter *navigation-next-name* "next")
(defparameter *navigation-page-name* " page")
(defparameter *navigation-contents-name* "contents")
(defparameter *navigation-index-name* "index")
(defparameter *navigation-sentence-end* "")
(defparameter *last-modified* "Last modified")
(defparameter *html-conversion-by* "HTML conversion by")

(defparameter *doctype* "html")

(defparameter *path-separator* #-windows #\: #+windows #\;)

(defparameter *directory-separator* #-windows "/" #+windows "\\")

(defparameter *int-corresp-to-0* (char-code #\0))

(defparameter *verbatim-visible-space*
  ; for the likes of verb* and {verbatim*}
  ; "<span style=\"color: red\">&middot;</span>"
  "<span style=\"vertical-align: -0.5ex\">&#x2334;</span>" )

(defparameter *aux-file-suffix* "-Z-A")
(defparameter *bib-aux-file-suffix* "-Z-B")
(defparameter *css-file-suffix* "-Z-S.css")
(defparameter *eval-file-suffix* "-Z-E-")
(defparameter *html-node-prefix* "node_")
(defparameter *html-page-suffix* "-Z-H-")
(defparameter *img-file-suffix* "-Z-G-")
(defparameter *imgdef-file-suffix* "D-")
(defparameter *index-file-suffix* "-Z-I")
(defparameter *label-file-suffix* "-Z-L")
(defparameter *mfpic-tex-file-suffix* ".Z-M-tex")
(defparameter *toc-file-suffix* "-Z-C")

(defparameter *ghostscript-options*
  " -q -dBATCH -dNOPAUSE -dNO_PAUSE -sDEVICE=ppmraw")

(defparameter *invisible-space* (list :*invisible-space*))

(defparameter *outer-invisible-space* (list :*outer-invisible-space*))

(defparameter *month-names*
  (vector "January" "February" "March" "April" "May" "June" "July" "August"
          "September" "October" "November" "December"))

(defparameter *if-aware-ctl-seqs*
  '("\\csname" "\\else" "\\end" "\\eval" "\\fi" "\\let"))

(defparameter *tex-logo*
  (concatenate 'string "T" "<span style=\""
    "position: relative; "
    "top: 0.5ex; "
    "margin-left: -0.1667em; "
    "margin-right: -0.125em; "
    "text-transform: uppercase"
    "\">e</span>" "X"))

;above are true globals.  Following are
;per-document globals

(defvar *afterassignment* nil)
(defvar *afterpar* '())
(defvar *afterbye* '())
(defvar *aux-dir* nil)
(defvar *aux-dir/* "")
(defvar *aux-port* nil)

(defvar *bib-aux-port* nil)
(defvar *bibitem-num* 0)

(defvar *color-names* '())
(defvar *comment-char* #\%)
(defvar *css-port* nil)
(defvar *current-source-file* nil)
(defvar *current-tex2page-input* nil)

(defvar *display-justification* nil)
(defvar *dotted-counters* nil)
(defvar *dumping-nontex-p* nil)

(defvar *equation-number* nil)
(defvar *equation-numbered-p* t)
(defvar *equation-position* 0)
(defvar *esc-char* #\\)
(defvar *esc-char-std* #\\)
(defvar *esc-char-verb* #\|)

(defvar *eval-file-count* 0)
(defvar *eval-for-tex-only-p* nil)
(defvar *expand-escape-p* nil)
(defvar *external-label-tables* nil)

(defvar *filename-delims* '())
(defvar *footnote-list* '())
(defvar *footnote-sym* 0)

(defvar *global-texframe* nil)
(defvar *graphics-file-extensions* '())

(defvar *html* nil)
(defvar *html-head* nil)
(defvar *html-only* 0)
(defvar *html-page* nil)
(defvar *html-page-count* nil)

(defvar *ignore-active-space-p* nil)
(defvar *ignore-timestamp-p* nil)
(defvar *img-file-count* 0)
(defvar *img-file-tally* 0)
(defvar *imgdef-file-count* 0)
(defvar *imgpreamble* nil)
(defvar *imgpreamble-inferred* nil)
(defvar *in-alltt-p* nil)
(defvar *in-display-math-p* nil)
(defvar *in-para-p* nil)
(defvar *in-small-caps-p* nil)
(defvar *includeonly-list* nil)
(defvar *index-count* nil)
(defvar *index-page* nil)
(defvar *index-page-mention-alist* nil)
(defvar *index-port* nil)
(defvar *index-table* nil)
(defvar *infructuous-calls-to-tex2page* nil)
(defvar *input-line-no* 0)
(defvar *input-streams* nil)
(defvar *inputting-boilerplate-p* nil)
(defvar *inside-appendix-p* nil)
(defvar *inside-eplain-verbatim-p* nil)
(defvar *it*)

(defvar *jobname* nil)

(defvar *label-port* nil)
(defvar *label-source* nil)
(defvar *label-table* nil)
(defvar *last-modification-time* nil)
(defvar *last-page-number* nil)
(defvar *latex-probability* nil)
(defvar *ligatures-p* nil)
(defvar *loading-external-labels-p* nil)
(defvar *log-file* nil)
(defvar *log-port* nil)

(defvar *main-tex-file* nil)
(defvar *math-delim-left* nil)
(defvar *math-delim-right* nil)
(defvar *math-height* nil)
(defvar *math-mode-p* nil)
(defvar *math-needs-image-p* nil)
(defvar *math-font* nil)
(defvar *math-script-mode-p* nil)
(defvar *mfpic-file-num* nil)
(defvar *mfpic-file-stem* nil)
(defvar *mfpic-port* nil)
(defvar *missing-eps-files* nil)
(defvar *missing-pieces* nil)
(defvar *mp-files* nil)

(defvar *not-processing-p* nil)

(defvar *outer-p* nil)
(defvar *output-streams* nil)
(defvar *outputting-external-title-p* nil)
(defvar *outputting-to-non-html-p* nil)

(defvar *reading-control-sequence-p* nil)
(defvar *recent-node-name* nil)
(defvar *remember-index-number* nil)
(defvar *redirect-delay* nil)
(defvar *redirect-url* nil)

(defvar *scm-builtins* nil)
(defvar *scm-dribbling-p* nil)
(defvar *scm-special-symbols* nil)
(defvar *scm-keywords* nil)
(defvar *scm-variables* nil)
(defvar *section-counters* nil)
(defvar *section-counter-dependencies* nil)
(defvar *slatex-math-escape* nil)
(defvar *source-changed-since-last-run-p* nil)
(defvar *stylesheets* nil)
(defvar *subjobname* *jobname*)

(defvar *tabular-stack* '())
(defvar *temp-string-count* nil)
(defvar *temporarily-use-utf8-for-math-p* nil)
(defvar *tex-env* '())
(defvar *tex-extra-letters* '())
(defvar *tex-format* nil)
(defvar *tex-if-stack* '())
(defvar *tex-like-layout-p* nil)
(defvar *tex-output-format* nil)
(defvar *tex2page-inputs* '())
(defvar *this-package* *package*)
(defvar *title* nil)
(defvar *toc-list* nil)
(defvar *toc-page* nil)

(defvar *unresolved-xrefs* nil)
(defvar *using-bibliography-p* nil)
(defvar *using-chapters-p* nil)
(defvar *using-index-p* nil)

(defvar *verb-display-p* nil)
(defvar *verb-port* nil)
(defvar *verb-visible-space-p* nil)
(defvar *verb-written-files* '())

(defvar *write-log-max* 55)
(defvar *write-log-index* 0)
(defvar *write-log-possible-break-p* nil)

(defvar *scm-token-delims*
  (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

(defstruct counter*  (value 0) (within nil))

(defstruct tocentry*  level number page label header)

#+sbcl
(sb-alien:define-alien-routine system sb-alien:int (command sb-alien:c-string))

#-sbcl
(defun system (cmd)
  #+abcl (ext:run-shell-command cmd)
  #+allegro (excl:shell cmd)
  #+clisp (ext:shell cmd)
  #+clozure (ccl::os-command cmd)
  #+cmucl (ext:run-program "sh" (list "-c" cmd) :output t)
  #+ecl (si:system cmd))

(defun char-tex-alphabetic-p (c)
  (or (alpha-char-p c)
      (member c *tex-extra-letters* :test #'char=)))

(defun gen-temp-string ()
  (incf *temp-string-count*)
  (concatenate 'string "Temp_" (write-to-string *temp-string-count*)))

(defun file-stem-name (f)
  (let ((slash (position #\/ f :test #'char= :from-end t)))
    (when slash (setq f (subseq f (+ slash 1) (length f))))
    (let ((dot (position #\. f :test #'char= :from-end t)))
      (if dot (subseq f 0 dot) f))))

(defun file-extension (f)
  (let ((slash (position #\/ f :test #'char= :from-end t))
        (dot (position #\. f :test #'char= :from-end t)))
    (if (and dot (not (= dot 0)) (or (not slash) (< (+ slash 1) dot)))
        (subseq f dot (length f)) nil)))

(defun ensure-file-deleted (f) (when (probe-file f) (delete-file f)))

(defun write-aux (e)
  (unless *aux-port*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *aux-file-suffix* ".scm")))
      (setq *aux-port* (open f :direction :output :if-exists :supersede))))
  (prin1 e *aux-port*) (terpri *aux-port*))

(defun write-label (e)
  (unless *label-port*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *label-file-suffix* ".scm")))
      (setq *label-port* (open f :direction :output :if-exists :supersede))))
  (prin1 e *label-port*)
  (terpri *label-port*))

(defun write-bib-aux (x)
  (unless *bib-aux-port*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *bib-aux-file-suffix* ".aux")))
      (setq *bib-aux-port* (open f :direction :output :if-exists :supersede))))
  (princ x *bib-aux-port*))

(defun write-log (x)
  (unless *log-port*
    (setq *log-file* (concatenate 'string *aux-dir/* *jobname* ".hlog")
          *log-port* (open *log-file* :direction :output :if-exists :supersede)))
  (when (and *write-log-possible-break-p* (characterp x)
             (member x '(#\) #\] #\} #\,) :test #'char=))
    (setq *write-log-possible-break-p* nil))
  (when (and *write-log-possible-break-p* (> *write-log-index* *write-log-max*))
    (terpri *log-port*)
    (terpri)
    (setq *write-log-possible-break-p* nil
          *write-log-index* 0))
  (unless (and (= *write-log-index* 0)
               (member x '(:separation-newline :separation-space)))
    (case x
      ((#\Newline :separation-newline)
       (when *write-log-possible-break-p*
         (setq *write-log-possible-break-p* nil))
       (terpri *log-port*)
       (terpri)
       (setq *write-log-index* 0))
      (:separation-space (setq *write-log-possible-break-p* t))
      (t (when *write-log-possible-break-p*
           (write-char #\space *log-port*)
           (write-char #\space)
           (setq *write-log-index* (+ *write-log-index* 1))
           (setq *write-log-possible-break-p* nil))
         (princ x *log-port*)
         (princ x)
         (force-output)
         (incf *write-log-index*
               (typecase x
                 (character 1)
                 (number (length (write-to-string x)))
                 (string (length x))
                 (t 1)))))))

(defun display-error-context-lines ()
  (let ((n (or (find-count "\\errorcontextlines") 0)))
    (when (and *current-source-file* (> n 0))
      (let* ((n1 (max 0 (- *input-line-no* (floor (1- n) 2))))
             (nf (+ n1 n -1))
             (ll (with-open-file (ip *current-source-file* :direction :input)
                   (let ((i 1) (ll '()))
                     (loop (let ((L (read-line ip nil :eof-object)))
                             (cond ((eq L :eof-object) (return ll))
                                   ((< i n1) (incf i))
                                   ((<= i nf) (incf i)
                                    (push (cons i L) ll))
                                   (t (return ll)))))))))
        (unless (null ll)
          (let* ((border "__________________________...")
                 (only-1-p (= (length ll) 1))
                 (nf (caar ll))
                 (ll (nreverse ll))
                 (n1 (caar ll)))
            (write-log "Likely error context: ")
            (write-log *current-source-file*)
            (write-log ", line")
            (unless only-1-p (write-log "s"))
            (write-log " ")
            (write-log n1)
            (unless only-1-p
              (write-log "-")
              (write-log nf))
            (write-log ":")
            (write-log #\newline)
            (write-log " /")
            (write-log border)
            (write-log #\newline)
            (mapc
             (lambda (L)
               (write-log " | ")
               (write-log (cdr L))
               (write-log #\newline))
             ll)
            (write-log " |")
            (write-log border)
            (write-log #\newline)
            (write-log "/")))))))

(defun edit-offending-file ()
  (let (bad-texedit-p cmd)
    (cond ((setq *it* (retrieve-env "TEXEDIT"))
           (let ((s *it*))
             (cond ((setq *it* (search "%d" s))
                    (let ((i *it*))
                      (setq s (concatenate (subseq s 0 i)
                                         (write-to-string *input-line-no*)
                                         (subseq s (+ i 2))))))
                   (t (setq bad-texedit-p t)))
             (cond ((and (not bad-texedit-p)
                         (setq *it* (search "%s" s)))
                    (let ((i *it*))
                      (setq s (concatenate (subseq s 0 i)
                                           *current-source-file*
                                           (subseq s (+ i 2))))))
                   (t (setq bad-texedit-p t)))
             (cond (bad-texedit-p
                     (format t "Bad TEXEDIT; using EDITOR~%"))
                   (t (setq cmd s))))))
    (cond ((and (not cmd)
                (setq *it* (or (retrieve-env "EDITOR") "vi")))
           (let ((s *it*))
             (setq cmd
                   (concatenate 'string
                                s
                                " +" (write-to-string *input-line-no*)
                                " " *current-source-file*)))))
    (when cmd (system cmd))))

(defun trace-if (write-p &rest args)
  (when write-p
    (write-log :separation-newline)
    (when (> *input-line-no* 0)
      (write-log "l.")
      (write-log *input-line-no*)
      (write-log #\space))
    (mapc #'write-log args)
    (write-log :separation-newline)))

(defun terror (where &rest args)
  (write-log :separation-newline)
  (write-log "! ")
  (mapc #'write-log args)
  (write-log :separation-newline)
  (write-log "l.")
  (write-log *input-line-no*)
  (write-log #\space)
  (write-log where)
  (write-log " failed.")
  (write-log :separation-newline)
  (display-error-context-lines)
  (close-all-open-ports)
  (output-stats)
  (format t "Type e to edit file at point of error; x to quit.~%")
  (princ "? ")
  (force-output)
  (let ((c (read-char nil nil)))
    (when (and c (char-equal c #\e))
      (edit-offending-file)))
  (error "TeX2page fatal error"))

(defun do-errmessage ()
  (write-log :separation-newline)
  (write-log "! ")
  (write-log (tex-string-to-html-string (get-group)))
  (write-log :separation-newline)
  (terror 'do-errmessage))

(defun do-tracingall ()
  (tex-def-count "\\tracingcommands" 1 nil)
  (tex-def-count "\\tracingmacros" 1 nil))

(defstruct bport* (port nil) (buffer '()))

(defun call-with-input-file/buffered (f th)
  (unless (probe-file f)
    (terror 'call-with-input-file/buffered "I can't find file " f))
  (with-open-file (i f :direction :input)
    (let ((*current-tex2page-input* (make-bport* :port i))
          (*current-source-file* f)
          (*input-line-no* 1))
      (funcall th))))

(defun call-with-input-string/buffered (s th)
  (let ((*current-tex2page-input* (make-bport* :buffer (concatenate 'list s)))
        (*input-line-no* *input-line-no*))
    (funcall th)))

(defun snoop-char ()
  (let ((c (get-char)))
    (toss-back-char c)
    c))

(defun get-char ()
  (let ((b (bport*-buffer *current-tex2page-input*)))
    (if (null b)
        (let ((p (bport*-port *current-tex2page-input*)))
          (if (not p) :eof-object
              (let ((c (read-char p nil :eof-object)))
                (cond ((eq c :eof-object) c)
                      ((char= c #\Newline)
                       (incf *input-line-no*) c)
                      (t c)))))
        (let ((c (car b)))
          (setf (bport*-buffer *current-tex2page-input*) (cdr b))
          c))))

(defun toss-back-string (s)
  (setf (bport*-buffer *current-tex2page-input*)
        (nconc (concatenate 'list s)
               (bport*-buffer *current-tex2page-input*))))

(defun toss-back-char (c)
  (push c (bport*-buffer *current-tex2page-input*)))

(defun emit (s)
  (princ s *html*))

(defun emit-newline ()
  (terpri *html*))

(defun invisible-space-p (x)
  (or (eq x *invisible-space*)
      (eq x *outer-invisible-space*)))

(defun outer-invisible-space-p (x)
  (eq x *outer-invisible-space*))

(defun check-outerness (c)
  (when (eq c *outer-invisible-space*)
    (setq *outer-p* t)))

(defun snoop-actual-char ()
  (let ((c (snoop-char)))
    (cond ((eq c :eof-object) c)
          ((invisible-space-p c) (get-char)
                                 (check-outerness c)
                                 (snoop-actual-char))
          ((char= c #\return) (get-char)
           (let ((c (snoop-actual-char)))
             (if (and (not (eq c :eof-object)) (char= c #\Newline)) c
                 (progn (toss-back-char #\Newline) #\Newline))))
          (t c))))

(defun get-actual-char ()
  (let ((c (get-char)))
    (cond ((eq c :eof-object) c)
          ((invisible-space-p c)
           (check-outerness c)
           (get-actual-char))
          ((char= c #\return)
           (let ((c (snoop-actual-char)))
             (if (and (not (eq c :eof-object)) (char= c #\Newline))
                 (get-actual-char) #\Newline)))
          (t c))))

(defun get-line ()
  (let ((r '()))
    (loop (let ((c (get-actual-char)))
            (cond ((eq c :eof-object)
                   (return (if r (concatenate 'string (nreverse r)) c)))
                  ((char= c #\newline)
                   (return (concatenate 'string (nreverse r))))
                  (t (push c r)))))))

(defun char-whitespace-p (c)
  (or (char= c #\space) (char= c #\tab)
      (not (graphic-char-p c))))

(defun ignorespaces ()
  (unless (and (find-chardef #\space) (not *ignore-active-space-p*))
    (let ((newline-active-p (find-chardef #\Newline))
          (newline-already-read-p nil))
      (loop (let ((c (snoop-char)))
              (when (eql c #\return) (setq c (snoop-actual-char)))
              (cond ((eq c :eof-object) (return))
                    ((invisible-space-p c)
                     (get-char)
                     (when *reading-control-sequence-p* (return)))
                    ((char= c #\newline)
                    (cond (newline-active-p (return))
                          (newline-already-read-p
                            (toss-back-char #\newline)
                            (return))
                          (t (get-actual-char)
                             (setq newline-already-read-p t))))
                    ((char-whitespace-p c)
                     (get-actual-char))
                    (t (return))))))))

(defun ignore-all-whitespace ()
  (loop (let ((c (snoop-actual-char)))
          (when (eq c :eof-object) (return))
          (unless (char-whitespace-p c) (return))
          (get-actual-char))))

(defun munch-newlines ()
  (let ((n 0))
    (loop (let ((c (snoop-actual-char)))
            (cond ((eq c :eof-object) (return n))
                  ((char= c #\newline) (get-actual-char) (incf n))
                  ((char-whitespace-p c) (get-actual-char))
                  (t (return n)))))))

(defun munched-a-newline-p ()
  (loop (let ((c (snoop-actual-char)))
          (cond ((eq c :eof-object) (return nil))
                ((char= c #\newline) (get-actual-char) (return t))
                ((char-whitespace-p c) (get-actual-char))
                (t (return nil))))))

(defun do-xspace ()
  (let ((c (snoop-actual-char)))
    (unless (member c '(#\space #\" #\. #\! #\, #\: #\; #\? #\/ #\' #\) #\-))
      (emit #\space))))

(defun do-relax () t)

(defun get-ctl-seq ()
  (let ((bs (get-actual-char)))
    (unless (char= bs *esc-char*)
      (terror 'get-ctl-seq "Missing control sequence (" bs ")"))
    (let ((c (get-char)))
      (cond ((eq c :eof-object) "\\ ")
            ((invisible-space-p c) "\\ ")
            ((char-tex-alphabetic-p c)
             (concatenate 'string
                          (nreverse
                            (let ((s (list c #\\)))
                              (loop
                                (let ((c (snoop-char)))
                                  (cond ((eq c :eof-object) (return s))
                                        ((invisible-space-p c) (return s))
                                        ((char-tex-alphabetic-p c)
                                         (get-char)
                                         (push c s))
                                        (t (unless (or *math-mode-p*
                                                       *not-processing-p*
                                                       (eq *tex-format* :texinfo))
                                             (let ((*reading-control-sequence-p* t))
                                               (ignorespaces)))
                                           (return s)))))))))
            (t (concatenate 'string (list #\\ c)))))))

(defun ctl-seq-p (z)
  (char= (char z 0) #\\))

(defun if-aware-ctl-seq-p (z)
  (or (member z *if-aware-ctl-seqs* :test #'string=)
      (and (>= (length z) 3) (char= (char z 1) #\i) (char= (char z 2) #\f))
      (let ((z-th (find-corresp-prim-thunk z)))
        (if (stringp z-th) nil
            (some (lambda (y) (eq z-th (find-corresp-prim-thunk y)))
                  *if-aware-ctl-seqs*)))))

(defun get-group-as-reversed-chars ()
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (eq c :eof-object) (terror 'get-group "Runaway argument?"))
    (unless (char= c #\{) (terror 'get-group "Missing {"))
    (let ((s (list c)) (nesting 0) (escape-p nil))
      (loop
        (let ((c (get-actual-char)))
          (when (eq c :eof-object)
            (terror 'get-group "Runaway argument?"))
          (cond (escape-p
                 (push c s) (setq escape-p nil))
                ((char= c *esc-char*)
                 (if *expand-escape-p*
                     (let ((s1 (progn
                             (toss-back-char c)
                             (let ((x (let ((*not-processing-p* t))
                                        (get-ctl-seq))))
                               (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                                 (concatenate 'string
                                              (list (char x 1))))
                                (t (let ((*esc-char* *esc-char-std*))
                                     (tex-string-to-html-string x))))))))
                       (setq s (nconc (nreverse (concatenate 'list s1)) s)
                             escape-p nil))
                   (progn (push c s)
                          (setq escape-p t))))
                ((char= c #\{)
                 (push c s) (incf nesting) (setq escape-p nil))
                ((char= c #\})
                 (push c s)
                 (if (= nesting 0)  (return s)
                   (progn (decf nesting) (setq escape-p nil))))
                (t (push c s)
                   (setq escape-p nil))))))))

(defun get-group ()
  (concatenate 'string (nreverse (get-group-as-reversed-chars))))

(defun string-trim-blanks (s)
  (string-trim '(#\space #\Tab #\Newline #\Return) s))

(defun get-peeled-group () (string-trim-blanks (ungroup (get-group))))

(defun get-token-or-peeled-group () (string-trim-blanks (ungroup (get-token))))

(defun get-grouped-environment-name-if-any ()
  (let ((c (snoop-actual-char)))
    (if (or (eq c :eof-object) (not (char= c #\{))) nil
        (progn
         (get-actual-char)
         (let ((s '()))
           (loop (let ((c (snoop-actual-char)))
                   (cond ((or (alpha-char-p c) (char= c #\*))
                          (get-actual-char) (push c s))
                         ((and (consp s) (char= c #\}))
                          (get-actual-char)
                          (return (concatenate 'string (nreverse s))))
                         (t (mapc #'toss-back-char s)
                            (toss-back-char #\{)
                            (return nil))))))))))

(defun get-bracketed-text-if-any ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (or (eq c :eof-object) (not (char= c #\[))) nil
      (progn
       (get-actual-char)
       (concatenate
        'string
        (nreverse
         (let ((s '()) (nesting 0) (escape-p nil))
           (loop
             (let ((c (get-actual-char)))
               (when (eq c :eof-object)
                 (terror 'get-bracketed-text-if-any
                         "Runaway argument?"))
               (cond (escape-p
                      (push c s) (setq escape-p nil))
                     ((char= c *esc-char*)
                      (push c s) (setq escape-p t))
                     ((char= c #\{)
                      (push c s) (incf nesting))
                     ((char= c #\})
                      (push c s) (decf nesting))
                     ((char= c #\])
                      (when (= nesting 0) (return s))
                      (push c s))
                     (t (push c s))))))))))))

(defun ungroup (s)
  (let* ((n (length s))
         (n-1 (- n 1)))
    (if (and (>= n 2)
             (char= (char s 0) #\{)
             (char= (char s n-1) #\}))
      (subseq s 1 n-1)
      s)))

(defun eat-alphanumeric-string ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (char= c #\")
        (progn (get-actual-char) (eat-till-char #\"))
      (loop (let ((c (snoop-actual-char)))
              (unless (or (alpha-char-p c) (digit-char-p c)) (return))
              (get-actual-char))))))

(defun get-filename (bracedp)
  (ignorespaces)
  (when bracedp
    (let ((c (snoop-actual-char)))
      (if (and (characterp c) (char= c #\{)) (get-actual-char)
        (setq bracedp nil))))
  (concatenate
   'string
   (nreverse
    (let ((s '()))
      (loop
        (let ((c (snoop-actual-char)))
          (cond ((eq c :eof-object) (return s))
                ((and (not bracedp)
                      (or (char-whitespace-p c)
                          (and *comment-char*
                               (char= c *comment-char*))
                          (member c *filename-delims* :test #'char=)))
                 (unless *not-processing-p* (ignorespaces))
                 (return s))
                ((and bracedp (char= c #\}))
                 (get-actual-char) (return s))
                ((and *esc-char* (char= c *esc-char*))
                 (let ((x (get-ctl-seq)))
                   (if (string= x "\\jobname")
                       (setq s (nconc (reverse
                                        (concatenate 'list *jobname*)) s))
                     (progn
                      (toss-back-char *invisible-space*)
                      (toss-back-string x)
                      (return s)))))
                (t (get-actual-char)
                   (push c s)))))))))

(defun get-plain-filename () (get-filename nil))

(defun get-filename-possibly-braced ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (get-filename (and (characterp c) (char= c #\{)))))

(defun get-integer (base)
  (declare (type (member 8 10 16) base))
  (ignorespaces)
  (let ((s '()))
    (loop (let ((c (snoop-actual-char)))
            (when (eq c :eof-object) (return))
            (unless (or (digit-char-p c)
                        (and (= base 16) (alpha-char-p c)))
              (ignorespaces) (return))
            (get-actual-char)
            (push c s)))
    (let ((n (let ((*read-base* base))
               (read-from-string
                (concatenate 'string (nreverse s))))))
      (if (numberp n) n nil))))

(defun get-real ()
  (ignorespaces)
  (let ((minusp nil) (c (snoop-actual-char)))
    (when (char= c #\-) (setq minusp t))
    (when (or minusp (char= c #\+)) (get-actual-char))
    (let ((n (read-from-string
              (concatenate 'string
                           (nreverse
                            (let ((s '()))
                              (loop
                                (let ((c (snoop-actual-char)))
                                  (cond ((eq c :eof-object) (return s))
                                        ((or (digit-char-p c)
                                             (char= c #\.))
                                         (get-actual-char)
                                         (push c s))
                                        (t (ignorespaces)
                                           (return s)))))))))))
      (if minusp (- n) n))))

(defun get-equal-sign ()
  (ignorespaces)
  (when (char= (snoop-actual-char) #\=) (get-actual-char)))

(defun get-by ()
  (ignorespaces)
  (when (char= (snoop-actual-char) #\b)
    (get-actual-char)
    (if (char= (snoop-actual-char) #\y) (get-actual-char)
        (toss-back-char #\b))))

(defun get-to ()
  (ignorespaces)
  (when (char= (snoop-actual-char) #\t)
    (get-actual-char)
    (cond ((char= (snoop-actual-char) #\o) (get-actual-char) (ignorespaces))
          (t (toss-back-char #\t)))))

(defun string-to-number (s &optional (base 10))
  (if (position #\: s :test #'char=) nil
    (let ((n (let ((*read-base* base)) (read-from-string s nil))))
      (if (numberp n) n nil))))

(defun get-number-corresp-to-ctl-seq (x)
  (cond ((string= x "\\the") (get-number-corresp-to-ctl-seq (get-ctl-seq)))
        ((string= x "\\active") 13) ((string= x "\\pageno") *html-page-count*)
        ((string= x "\\inputlineno") *input-line-no*)
        ((string= x "\\footnotenumber") (get-gcount "\\footnotenumber"))
        ((string= x "\\figurenumber")
         (counter*-value (gethash "figure" *dotted-counters*)))
        ((string= x "\\sectiondnumber")
         (gethash (read-from-string (ungroup (get-token)))
                  *section-counters*
                  0))
        ((find-count x)) ((find-dimen x))
        (t (string-to-number (or (resolve-defs x) x)))))

(defun get-number-or-false ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((char= c *esc-char*) (get-number-corresp-to-ctl-seq (get-ctl-seq)))
          ((char= c #\') (get-actual-char) (get-integer 8))
          ((char= c #\") (get-actual-char) (get-integer 16))
          ((char= c #\`) (get-actual-char) (ignorespaces)
           (char-code
            (if (char= (snoop-actual-char) *esc-char*) (char (get-ctl-seq) 1)
                (get-actual-char))))
          ((char= c #\+) (get-actual-char) (get-number-or-false))
          ((char= c #\-) (get-actual-char)
           (let ((n (get-number-or-false)))
             (and n (- n))))
          ((digit-char-p c) (get-integer 10)) (t nil))))

(defun get-number ()
  (or (get-number-or-false) (terror 'get-number "Missing number.")))

(defun get-tex-char-spec ()
  (let ((n (get-number-or-false)))
    (if n (code-char n)
      (terror 'get-tex-char-spec "not a char"))))

(defun get-url ()
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (eq c :eof-object) (not (char= c #\{)))
      (terror 'get-url "Missing {"))
    (string-trim-blanks
     (concatenate 'string
       (nreverse
        (let ((nesting 0) (s '()))
          (loop
            (let ((c (get-actual-char)))
              (when (eq c :eof-object) (terror 'get-url "Missing }"))
              (cond ((and *comment-char* (char= c *comment-char*))
                     (let ((c1 (snoop-actual-char)))
                       (if (and (characterp c1)
                                (char-whitespace-p c1))
                           (ignore-all-whitespace)
                         (push c s))))
                    ((char= c #\{)
                     (incf nesting) (push c s))
                    ((char= c #\})
                     (when (= nesting 0) (return s))
                     (decf nesting) (push c s))
                    (t (push c s)))))))))))

(defun get-csv ()
  ;csv = comma-separated value
  (ignorespaces)
  (let ((rev-lbl
         (let ((s '()) (nesting 0))
           (loop
             (let ((c (get-actual-char)))
               (when (eq c :eof-object)
                 (terror 'get-csv "Runaway argument of \\cite, "
                         "\\nocite, \\expandhtmlindex?"))
               (cond ((and (char= c #\,) (= nesting 0))
                      (return s))
                     ((char= c #\{)
                      (push c s) (incf nesting))
                     ((char= c #\})
                      (when (= nesting 0)
                        (toss-back-char c) (return s))
                      (push c s) (decf nesting))
                     (t (push c s))))))))
    (when rev-lbl
      (concatenate 'string (nreverse rev-lbl)))))

;functions for reading TeX tokens.  Token isn't really the right name.
;It's more like a TeX sexpr (texpr?), i.e. something that is treated as
;a single item by a macro looking for an arg

(defun get-raw-token ()
  (let ((c (snoop-actual-char)))
    (cond ((eq c :eof-object) c)
          ((char= c *esc-char*)
           (let ((*not-processing-p* t))
             (get-ctl-seq)))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun get-raw-token/is ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((eq c :eof-object) c) ((char= c *esc-char*) (get-ctl-seq))
          ((and *comment-char* (char= c *comment-char*)) (eat-till-eol)
           (get-raw-token/is))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun get-token ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((eq c :eof-object) c) ((char= c *esc-char*) (get-ctl-seq))
          ((char= c #\{) (get-group))
          ((and *comment-char* (char= c *comment-char*)) (eat-till-eol)
           (get-token))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun eat-word (word)
  (ignorespaces)
  (let ((r '()))
    (dotimes (i (length word) t)
      (let ((c (snoop-actual-char)))
        (cond ((char= c (char word i)) (get-actual-char) (push c r))
              (t (mapc #'toss-back-char r)
                 (return nil)))))))

(defun eat-skip-fluff (fullp)
  (let ((go-ahead-p t))
    (cond (fullp (get-equal-sign))
          ((some #'eat-word '("plus" "minus")) t)
          (t (setq go-ahead-p nil)))
    (when go-ahead-p
      (let ((*not-processing-p* t))
        (let ((firstp fullp))
          (loop
            (ignorespaces)
            (let ((c (snoop-actual-char)))
              (cond ((eq c :eof-object) (return))
                    ((and (char= c *esc-char*) firstp)
                     (get-ctl-seq) (return))
                    ((or (digit-char-p c) (char= c #\.))
                     (get-real))
                    ((or (char= c #\') (char= c #\"))
                     (get-number))
                    ((some #'eat-word '("+" "-"))
                     t)
                    ((some #'eat-word
                           '("bp" "cc" "cm" "dd" "em" "ex" "filll"
                             "fill" "fil" "in" "minus" "mm" "pc"
                             "plus" "pt" "sp" "true"))
                     (setq firstp nil))
                    (t (return))))))))))

(defun eat-dimen ()
  (eat-skip-fluff t))

(defun eat-integer ()
  (let ((*not-processing-p* t))
    (ignorespaces)
    (get-equal-sign)
    (get-number)))

(defun scm-get-token ()
  (concatenate 'string
    (nreverse
     (let ((s '()) (esc-p nil))
       (loop
         (let ((c (snoop-actual-char)))
           (when (eq c :eof-object) (return s))
           (cond (esc-p (get-actual-char)
                       (push c s) (setq esc-p nil))
                 ((char= c #\\) (get-actual-char)
                  (push c s) (setq esc-p t))
                 ((or (char-whitespace-p c)
                      (member c *scm-token-delims* :test #'char=))
                  (return s))
                 (t (get-actual-char)
                    (push c s)))))))))

(defun emit-html-char (c)
  (unless (eq c :eof-object)
    (cond ((char= c #\Newline) (emit-newline))
          (*outputting-to-non-html-p* (emit c))
          (t (emit (case c
                     ((#\<) "&lt;")
                     ((#\>) "&gt;")
                     ((#\") "&quot;")
                     ((#\&) "&amp;")
                     (t c)))))))

(defun emit-html-string (s)
  (dotimes (i (length s))
    (emit-html-char (char s i))))

(defstruct texframe*
  (definitions (make-hash-table :test #'equal))
  (chardefinitions (make-hash-table))
  (counts (make-hash-table :test #'equal))
  (toks (make-hash-table :test #'equal))
  (dimens (make-hash-table :test #'equal))
  (postludes '())
  (aftergroups '()))

(defvar *primitive-texframe* (make-texframe*))

(defvar *math-primitive-texframe* (make-texframe*))

(defun bgroup ()
  (push (make-texframe*) *tex-env*)
  (when (and *in-display-math-p* (not *math-script-mode-p*))
    (bgroup-math-hook)))

(defun bgroup-math-hook ()
  (let ((old-html *html*)
        (old-math-delim-left *math-delim-left*)
        (old-math-delim-right *math-delim-right*)
        (old-math-height *math-height*))
    (setq *html* (make-string-output-stream)
          *math-delim-left* nil
          *math-delim-right* nil
          *math-height* 0)
    (push :mathbox *tabular-stack*)
    (add-postlude-to-top-frame
     (lambda ()
       (let ((res (get-output-stream-string *html*)))
         (setq res
               (concatenate 'string
                 "<table><tr><td align=center>"
                 res
                 "</td></tr></table>"))
         (when (or *math-delim-left* *math-delim-right*)
           (when (and (or (member *math-delim-left* '(:lbrace :rbrace))
                          (member *math-delim-right* '(:lbrace :rbrace)))
                      (evenp *math-height*))
             (incf *math-height*))
           (setq res
                 (concatenate 'string
                   "<table><tr><td>"
                   (tex-math-delim-string *math-delim-left*)
                   "</td><td>"
                   res
                   "</td><td>"
                   (tex-math-delim-string *math-delim-right*)
                   "</td></tr></table>")))
         (setq *html* old-html
               *math-delim-left* old-math-delim-left
               *math-delim-right* old-math-delim-right
               *math-height* (+ old-math-height *math-height*))
         (pop-tabular-stack :mathbox)
         (emit "</td><td>")
         ;(when (> *math-height* 0) (emit "<small>")) ;not quite
         (emit res)
         ;(when (> *math-height* 0) (emit "</small>"))
         (emit "</td><td>"))))))

(defun do-math-left ()
  (ignorespaces)
  (when (and *in-display-math-p* (not *math-script-mode-p*))
    (let ((s (get-token)))
      (bgroup)
      (cond ((string= s "(") (setq *math-delim-left* :lparen))
            ((string= s "[") (setq *math-delim-left* :lbrack))
            ((string= s "\\{") (setq *math-delim-left* :lbrace))
            ((string= s "|") (setq *math-delim-left* :lvert))
            (t (terror 'do-math-left))))))

(defun do-math-right ()
  (ignorespaces)
  (when (and *in-display-math-p* (not *math-script-mode-p*))
    (let ((s (get-token)))
      (cond ((string= s ")") (setq *math-delim-right* :rparen))
            ((string= s "]") (setq *math-delim-right* :rbrack))
            ((string= s "\\}") (setq *math-delim-right* :rbrace))
            ((string= s "|") (setq *math-delim-right* :rvert))
            (t (terror 'do-math-right)))
      (egroup))))

(defun egroup ()
  (when (null *tex-env*) (terror 'egroup "Too many }'s"))
  (perform-postludes)
  (perform-aftergroups)
  (pop *tex-env*))

(defun perform-postludes ()
  (mapc (lambda (p) (funcall p)) (texframe*-postludes (top-texframe))))

(defun perform-aftergroups ()
  (let ((ags (texframe*-aftergroups (top-texframe))))
    (unless (null ags) (toss-back-char *invisible-space*))
    (mapc (lambda (ag) (funcall ag)) ags)))

(defun perform-afterassignment ()
  (let ((z *afterassignment*))
    (when z (setq *afterassignment* nil) (do-tex-ctl-seq z))))

(defun add-postlude-to-top-frame (p)
  (let ((fr (if (null *tex-env*) *global-texframe* (car *tex-env*))))
    (push p (texframe*-postludes fr))))

(defun add-aftergroup-to-top-frame (ag)
  (let ((fr (if (null *tex-env*) *global-texframe* (car *tex-env*))))
    (push ag (texframe*-aftergroups fr))))

(defun top-texframe ()
  (if (null *tex-env*) *global-texframe* (car *tex-env*)))

(defstruct tdef*
  (argpat '())
  (expansion "")
  (optarg nil)
  (thunk nil)
  (prim nil)
  (defer nil))

(defstruct cdef*
  (argpat nil)
  (expansion nil)
  (optarg nil)
  (active nil))

(defun kopy-tdef (lft rt)
  (setf (tdef*-argpat lft) (tdef*-argpat rt))
  (setf (tdef*-expansion lft) (tdef*-expansion rt))
  (setf (tdef*-optarg lft) (tdef*-optarg rt))
  (setf (tdef*-thunk lft) (tdef*-thunk rt))
  (setf (tdef*-prim lft) (tdef*-prim rt))
  (setf (tdef*-defer lft) (tdef*-defer rt)))

(defun kopy-cdef (lft rt)
  (setf (cdef*-argpat lft) (cdef*-argpat rt))
  (setf (cdef*-expansion lft) (cdef*-expansion rt))
  (setf (cdef*-optarg lft) (cdef*-optarg rt))
  (setf (cdef*-active lft) (cdef*-active rt)))

(defun cleanse-tdef (d)
  (setf (tdef*-argpat d) '())
  (setf (tdef*-expansion d) "")
  (setf (tdef*-optarg d) nil)
  (setf (tdef*-thunk d) nil)
  (setf (tdef*-prim d) nil)
  (setf (tdef*-defer d) nil))

(defun tex-def (name argpat expansion optarg thunk prim defer frame)
  (unless frame (setq frame (top-texframe)))
  (let* ((frame-defs (texframe*-definitions frame))
         (d (or (gethash name frame-defs)
                (let ((d (make-tdef*)))
                  (setf (gethash name frame-defs) d)
                  d))))
    (setf (tdef*-argpat d) argpat
          (tdef*-expansion d) expansion
          (tdef*-optarg d) optarg
          (tdef*-thunk d) thunk
          (tdef*-prim d) prim
          (tdef*-defer d) defer))
  (perform-afterassignment))

(defun tex-def-prim (prim thunk)
  (tex-def prim '() nil nil thunk prim nil *primitive-texframe*))

(defun tex-def-0arg (cs expn)
  (tex-def cs '() expn nil nil nil nil nil))

(defun ctl-seq-no-arg-expand-once (cs)
  (let ((d (find-def cs)))
    (and d (tdef*-expansion d))))

(defun tex-gdef-0arg (cs expn)
  (tex-def cs '() expn nil nil cs nil *global-texframe*))

(defun tex-def-prim-0arg (cs expn)
  (tex-def cs '() expn nil nil cs nil *primitive-texframe*))

(defun get-0arg-expn (cs)
  (let ((d (find-def cs)))
    (if d (tdef*-expansion d) "0")))

(defun tex2page-flag-value (cs)
  (char (get-0arg-expn cs) 0))

(defun tex2page-flag-boolean (cs)
  (not (member (char (get-0arg-expn cs) 0) '(#\0 #\f #\F #\n #\N))))

(defun tex-let (lft rt frame)
  (unless frame (setq frame (top-texframe)))
  (let* ((frame-defs (texframe*-definitions frame))
         (lft-def
           (or (gethash lft frame-defs)
               (let ((lft-def (make-tdef*)))
                 (setf (gethash lft frame-defs) lft-def)
                 lft-def))))
    (cond ((setq *it* (find-def rt))
           (let ((rt-def *it*))
             (kopy-tdef lft-def rt-def)))
          (t (cleanse-tdef lft-def)
             (setf (tdef*-defer lft-def) rt)))))

(defun tex-let-prim (lft rt)
  (tex-let lft rt *primitive-texframe*))

(defun tex-def-thunk (name thunk frame)
  (unless (inside-false-world-p)
    (tex-def name '() nil nil thunk name nil frame)))

(defun tex-def-count (name num globalp)
  (let ((frame (if globalp *global-texframe* (top-texframe))))
    (setf (gethash name (texframe*-counts frame)) num))
  (perform-afterassignment))

(defun tex-def-toks (name tokens globalp)
  (let ((frame (if globalp *global-texframe* (top-texframe))))
    (setf (gethash name (texframe*-toks frame)) tokens)))

(defun tex-def-dimen (name len globalp)
  (let ((frame (if globalp *global-texframe* (top-texframe))))
    (setf (gethash name (texframe*-dimens frame)) len)
    (perform-afterassignment)))

(defun tex-def-char (char argpat expansion frame)
  (unless frame (setq frame (top-texframe)))
  (let ((d (ensure-cdef char frame)))
    (setf (cdef*-argpat d) argpat
          (cdef*-expansion d) expansion))
  (perform-afterassignment))

(defun ensure-cdef (c f)
  (let ((f-chardefs (texframe*-chardefinitions f)))
    (or (gethash c f-chardefs)
        (let ((d (make-cdef*)))
          (setf (gethash c f-chardefs) d)
          d))))

(defun find-chardef (c)
  (let ((x (or (some (lambda (f) (gethash c (texframe*-chardefinitions f)))
                     *tex-env*)
               (gethash c  (texframe*-chardefinitions *global-texframe*))
               (gethash c (texframe*-chardefinitions *primitive-texframe*)))))
    (and x (cdef*-active x) x)))

(defun find-chardef-in-top-frame (c)
  (let ((x (if (null *tex-env*)
               (or (gethash c (texframe*-chardefinitions *global-texframe*))
                   (gethash c  (texframe*-chardefinitions *primitive-texframe*)))
             (gethash c  (texframe*-chardefinitions (car *tex-env*))))))
    (and x (cdef*-active x) x)))

(defun do-defcsactive (globalp)
  (ignorespaces)
  (let* ((cs (get-ctl-seq))
         (c (char cs 1))
         (argpat (get-def-arguments c))
         (rhs (ungroup (get-group)))
         (f (and globalp *global-texframe*)))
    (activate-cdef c)
    (tex-def-char c argpat rhs f)))

(defun activate-cdef (c)
  (let ((y (cond ((setq *it* (find-chardef-in-top-frame c))
                  (let ((y *it*))
                    (setf (cdef*-active y) t)
                    y))
                 (t (let* ((d (find-chardef c))
                           (y (ensure-cdef c (top-texframe))))
                      (when d (kopy-cdef y d))
                      (setf (cdef*-active y) t)
                      y)))))
    (add-postlude-to-top-frame
      (lambda ()
        (setf (cdef*-active y) nil)))))

(defun deactivate-cdef (c)
  (cond ((setq *it* (find-chardef-in-top-frame c))
         ;if c is active in current group, deactivate it
         (let ((y *it*))
           (setf (cdef*-active y) nil)))
        ((setq *it* (find-chardef c))
         ;if c is active in an enclosing group, create a shadowing
         ;unactive def for it in the current group
         (let ((y *it*))
           (let ((d (ensure-cdef c (top-texframe))))
             (kopy-cdef d y)
             (setf (cdef*-active d) nil))))))

(defun do-undefcsactive ()
  (ignorespaces)
  (deactivate-cdef (char (get-ctl-seq) 1)))

(defun do-catcode ()
  (let ((c (get-tex-char-spec)))
    (let ((val (progn (get-equal-sign) (get-number))))
      (set-catcode c val))))

(defun set-catcode (c val)
  (unless (= val 13) (deactivate-cdef c))
  (unless (= val 11)
    (setq *tex-extra-letters* (delete c *tex-extra-letters* :test #'char=)))
  (case val
    (0 (setq *esc-char* 0))
    (11 (push c  *tex-extra-letters*))
    (13 (activate-cdef c))))

(defun do-global ()
  (ignorespaces)
  (let ((next (get-ctl-seq)))
    (cond ((string= next "\\def") (do-def t nil))
          ((string= next "\\edef") (do-def t t))
          ((string= next "\\let") (do-let t))
          ((string= next "\\newcount") (do-newcount t))
          ((string= next "\\newtoks") (do-newtoks t))
          ((string= next "\\newdimen") (do-newdimen t))
          ((string= next "\\advance") (do-advance t))
          ((string= next "\\multiply") (do-multiply t))
          ((string= next "\\divide") (do-divide t))
          ((string= next "\\read") (do-read t))
          ((or (string= next "\\imgdef") (string= next "\\gifdef"))
           (make-reusable-img t))
          ((find-count next) (do-count= next t))
          ((find-toks next) (do-toks= next t)) (t (toss-back-string next)))))

(defun do-externaltitle ()
  (write-aux `(!preferred-title ,(tex-string-to-html-string (get-group)))))

(defun tex2page-string (s)
  (call-with-input-string/buffered s #'generate-html))

(defun make-external-title (title)
  (let ((*outputting-external-title-p* t))
    (bgroup)
    (let ((s (tex-string-to-html-string
              (concatenate 'string "\\let\\\\\\ignorespaces"
                "\\def\\resizebox#1#2#3{}"
                "\\let\\thanks\\TIIPgobblegroup"
                "\\let\\urlh\\TIIPgobblegroup " title))))
      (egroup)
      s)))

(defun output-external-title ()
  (let ((*outputting-external-title-p* t))
    (emit "<title>")
    (emit-newline)
    (emit (or *title* *jobname*))
    (emit-newline)
    (emit "</title>")
    (emit-newline)))

(defun output-title (title)
  (emit "<h1 class=title>")
  (bgroup)
  (tex2page-string (concatenate 'string "\\let\\\\\\break " title))
  (egroup)
  (emit "</h1>")
  (emit-newline))

(defun do-subject ()
  (tex-gdef-0arg "\\TZPtitleused" "1")
  (do-end-para)
  (let ((title (get-group)))
    (unless *title* (flag-missing-piece :document-title))
    (write-aux `(!default-title ,(make-external-title title)))
    (output-title title)))

(defun do-latex-title ()
  (let ((title (get-group)))
    (unless *title* (flag-missing-piece :document-title))
    (write-aux `(!default-title ,(make-external-title title)))
    (toss-back-string title)
    (toss-back-string "\\def\\TIIPtitle")))

(defun do-title ()
  (if (eq *tex-format* :latex)
    (do-latex-title)
    (do-subject)))

(defun do-author () (toss-back-string "\\def\\TIIPauthor"))

(defun do-date () (toss-back-string "\\def\\TIIPdate"))

(defun do-today ()
  (let ((m (get-gcount "\\month")))
    (if (= m 0) (emit "[today]")
        (progn
         (emit (svref *month-names* (- m 1)))
         (emit " ")
         (emit (get-gcount "\\day"))
         (emit ", ")
         (emit (get-gcount "\\year"))))))

(defun add-afterpar (ap)
  (push ap *afterpar*))

(defun do-end-para ()
  (when *in-para-p*
    (when *use-closing-p-tag-p* (emit "</p>"))
    (unless (null *afterpar*)
      (mapc (lambda (ap) (funcall ap)) (nreverse *afterpar*))
      (setq *afterpar* '()))
    (emit-newline)
    (setq *in-para-p* nil)))

(defun do-para ()
  (do-end-para)
  (let ((in-table-p (member (car *tabular-stack*) '(:block))))
    (when in-table-p (emit "</td></tr><tr><td>") (emit-newline))
    (emit "<p>")
    (setq *in-para-p* t)))

(defun do-noindent ()
  (do-end-para)
  (emit-newline)
  (emit "<p class=noindent>")
  (setq *in-para-p* t))

(defun do-para-nopadding ()
  (do-end-para)
  (emit-newline)
  (emit "<p class=nopadding>")
  (setq *in-para-p* t))

(defun do-maketitle ()
  (do-end-para)
  (bgroup)
  (tex2page-string
   (concatenate 'string "\\let\\\\\\break" "\\let\\and\\break"
                "\\let\\thanks\\symfootnote"))
  (output-title "\\TIIPtitle")
  (do-para)
  (do-end-para)
  (emit "<div align=center>")
  (emit-newline)
  (tex2page-string "\\TIIPauthor")
  (do-para)
  (tex2page-string "\\TIIPdate")
  (do-end-para)
  (emit "</div>")
  (emit-newline)
  (egroup)
  (do-para))

(defun do-inputcss ()
  (ignorespaces)
  (let ((f (get-filename-possibly-braced)))
    (when (null *stylesheets*) (flag-missing-piece :stylesheets))
    (write-aux `(!stylesheet ,f))))

(defun do-csname ()
  (ignorespaces)
  (let ((r '()))
    (loop
      (let ((c (snoop-actual-char)))
        (cond ((char= c *esc-char*)
               (let ((x (get-ctl-seq)))
                 (cond ((string= x "\\endcsname")
                        (toss-back-char #\})
                        (mapc #'toss-back-string r)
                        (toss-back-char *esc-char*)
                        (toss-back-char #\{)
                        (toss-back-string "TIIPcsname")
                        (toss-back-char *esc-char*)
                        (return))
                       (t (push (expand-ctl-seq-into-string x) r)))))
              (t (get-actual-char)
                 (push (concatenate 'string (list c)) r)))))))

(defun do-saved-csname ()
  (let ((x (get-peeled-group)))
    (do-tex-ctl-seq x)))

(defun do-cssblock ()
  (let ((*dumping-nontex-p* t))
    (dump-till-end-env "cssblock" *css-port*)))

(defun link-stylesheets ()
  (emit "<link rel=\"stylesheet\" href=\"")
  (emit *jobname*)
  (emit *css-file-suffix*)
  (emit "\" title=default />")
  (emit-newline)
  (mapc
   (lambda (css)
       (emit "<link rel=\"stylesheet\" href=\"")
       (emit css)
       (emit "\" title=default />")
       (emit-newline))
   *stylesheets*))

(defun increment-section-counter (seclvl unnumbered-p)
  (unless unnumbered-p
    ;increment the counter for seclvl.  If counter not set, init it to 1
    (incf (gethash seclvl *section-counters* 0)))
  ;zero the counters of all section levels below current level; except
  ;0, which doesn't change even when -1 does
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (and (> k seclvl) (> k 0))
               (setf (gethash k *section-counters*) 0)))
           *section-counters*)
  ;zero footnote counter if new chapter
  (when (= seclvl 0) (tex-gdef-count "\\footnotenumber" 0))
  ;zero all theorem-counters that hang off this seclvl
  (mapc
   (lambda (counter-name)
     (setf (counter*-value (gethash counter-name *dotted-counters*)) 0))
   (gethash seclvl *section-counter-dependencies* nil)))

(defun number-to-roman (n &optional upcase-p)
  (format nil (if upcase-p "~@r" "~(~@r~)") n))

(defun section-counter-value (seclvl)
  (if (= seclvl -1) (number-to-roman (gethash -1 *section-counters*) t)
      (let ((i (if *using-chapters-p* 0 1)))
        (let ((outermost-secnum
               (let ((n (gethash i *section-counters* 0)))
                 (if *inside-appendix-p*
                     (concatenate 'string
                                  (list (code-char (+ (char-code #\A) -1 n))))
                     (write-to-string n)))))
          (let ((i (1+ i)) (r outermost-secnum))
            (loop
              (when (> i seclvl) (return r))
              (setq r (concatenate 'string r "."
                                   (write-to-string
                                     (gethash i *section-counters* 0))))
              (incf i)))))))

(defun section-ctl-seq-p (s)
  (cond ((string= s "\\sectiond") (string-to-number (ungroup (get-token))))
        ((string= s "\\part") -1)
        ((string= s "\\chapter") (!using-chapters) (write-aux '(!using-chapters))
         (if (and (eq *tex-format* :latex) (< (get-gcount "\\secnumdepth") -1))
             (tex-gdef-count "\\secnumdepth" 2))
         0)
        (t (let ((n (length s)))
             (cond ((< n 8) nil)
                   ((and (>= n 10) (string= (subseq s (- n 9) n) "paragraph"))
                    (let ((n-9 (- n 9)) (i 1) (i+3 4) (k 4))
                      (loop
                        (cond ((> i+3 n-9) (return k))
                              ((string= (subseq s i i+3) "sub")
                               (setq i i+3 i+3 (+ i+3 3) k (1+ k)))
                              (t (return nil))))))
                   ((string= (subseq s (- n 7) n) "section")
                    (let ((n-7 (- n 7)) (i 1) (i+3 4) (k 1))
                      (loop
                        (cond ((> i+3 n-7) (return k))
                              ((string= (subseq s i i+3) "sub")
                               (setq i i+3 i+3 (+ i+3 3) k (1+ k)))
                              (t (return nil))))))
                   (t nil))))))

(defun do-heading (seclvl)
  (let* ((starred-p
          (cond ((char= (snoop-actual-char) #\*) (get-actual-char) t) (t nil)))
         (too-deep-p
          (let ((secnumdepth (get-gcount "\\secnumdepth")))
            (cond ((< secnumdepth -1) nil) ((> seclvl secnumdepth) t)
                  (t nil))))
         (unnumbered-p (or starred-p too-deep-p))
         (header
          (let ((*tabular-stack* (list :header)))
            (tex-string-to-html-string (get-group)))))
    (when (<= seclvl 0) (do-eject))
    (increment-section-counter seclvl unnumbered-p)
    (let ((lbl-val (if unnumbered-p nil (section-counter-value seclvl))))
      (do-heading-aux seclvl starred-p unnumbered-p nil lbl-val
                      header))))

(defun do-heading-aux (seclvl starred-p unnumbered-p chapname lbl-val header)
  (unless lbl-val (setq lbl-val "IGNORE"))
  (let* ((htmlnum (max 1 (min 6 (if *using-chapters-p* (+ seclvl 1) seclvl))))
         (lbl
          (concatenate 'string *html-node-prefix*
            (case seclvl ((-1) "part") ((0) "chap") (t "sec")) "_"
            (if unnumbered-p (gen-temp-string) lbl-val))))
    (unless nil
      (tex-def-0arg "\\TIIPcurrentnodename" lbl)
      (tex-def-0arg "\\@currentlabel" lbl-val))
    (do-end-para)
    (emit-anchor lbl)
    (emit-newline)
    (ignore-all-whitespace)
    (emit "<h")
    (emit htmlnum)
    (case seclvl
      ((-1) (emit " class=part align=center"))
      ((0) (emit " class=chapter"))
      (t (emit " class=section")))
    (emit ">")
    (let ((write-to-toc-p
           (and *toc-page*
                (not
                 (and (eql *tex-format* :latex)
                      (string= header "Contents"))))))
      (case seclvl
        ((-1)
         (emit "<div class=partheading>")
         (if unnumbered-p (emit-nbsp 1)
           (progn
            (when write-to-toc-p
              (emit-page-node-link-start *toc-page*
                                         (concatenate 'string *html-node-prefix* "toc_" lbl)))
            (tex2page-string (or chapname "\\partname"))
            (emit " ")
            (emit lbl-val)
            (when write-to-toc-p (emit-link-stop))))
         (emit "</div><br>")
         (emit-newline))
        ((0)
         (emit-newline)
         (emit "<div class=chapterheading>")
         (if unnumbered-p (emit-nbsp 1)
           (progn
            (when write-to-toc-p
              (emit-page-node-link-start *toc-page*
                                         (concatenate 'string *html-node-prefix* "toc_" lbl)))
            (tex2page-string
             (or chapname
                 (if *inside-appendix-p* "\\appendixname" "\\chaptername")))
            (emit " ")
            (emit lbl-val)
            (when write-to-toc-p (emit-link-stop))))
         (emit "</div><br>")
         (emit-newline)))
      (when write-to-toc-p
        (emit-page-node-link-start *toc-page*
                                   (concatenate 'string *html-node-prefix* "toc_" lbl)))
      (unless (or (<= seclvl 0) unnumbered-p) (emit lbl-val) (emit-nbsp 2))
      (emit header)
      (when write-to-toc-p (emit-link-stop))
      (emit "</h")
      (emit htmlnum)
      (emit ">")
      (emit-newline)
      (do-para)
      (let ((tocdepth (get-gcount "\\tocdepth")))
        (when
            (and write-to-toc-p (not (and (eq *tex-format* :latex) starred-p))
                 (or (< tocdepth -1) (<= seclvl tocdepth)))
          (write-aux
           `(!toc-entry
             ,(if (= seclvl -1) -1
                (if *using-chapters-p* seclvl (- seclvl 1)))
             ,lbl-val ,*html-page-count* ,lbl ,header)))))
    (when *recent-node-name*
      (do-label-aux *recent-node-name*)
      (setq *recent-node-name* nil))))

(defun section-type-to-depth (sectype)
  (cond ((string-to-number sectype))
        ((string= sectype "chapter") 0)
        ((string= sectype "section") 1)
        ((string= sectype "subsection") 2)
        ((string= sectype "subsubsection") 3)
        ((string= sectype "paragraph") 4)
        ((string= sectype "subparagraph") 5)
        (t 3)))

(defun do-write-to-toc-aux (seclvl secnum sectitle)
  (let ((node-name
         (concatenate 'string *html-node-prefix* "sec_"
                      (if (string= secnum "") (gen-temp-string) secnum))))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (tex-def-0arg "\\@currentlabel" secnum)
    (emit-anchor node-name)
    (emit-newline)
    (write-aux
     `(!toc-entry ,seclvl ,secnum ,*html-page-count* ,node-name ,sectitle))))

(defun do-writenumberedcontentsline ()
  (let ((toc (get-peeled-group)))
    (unless (string= toc "toc")
      (terror 'do-writenumberedcontentsline "only #1=toc supported"))
    (do-writenumberedtocline)))

(defun do-writenumberedtocline ()
  (let* ((seclvl (section-type-to-depth (get-peeled-group)))
         (secnum (tex-string-to-html-string (get-group)))
         (sectitle (tex-string-to-html-string (get-group))))
    (do-write-to-toc-aux seclvl secnum sectitle)))

(defun do-addcontentsline ()
  (let ((toc (get-peeled-group)))
    (unless (string= toc "toc")
      (terror 'do-addcontentsline "only #1=toc supported"))
    (let* ((seclvl (section-type-to-depth (get-peeled-group)))
           (sectitle (tex-string-to-html-string (get-group))))
      (write-aux
       `(!toc-entry
         ,(if (= seclvl -1) -1 (if *using-chapters-p* seclvl (- seclvl 1)))
         ,(ctl-seq-no-arg-expand-once "\\@currentlabel") ,*html-page-count*
         ,(ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename") ,sectitle)))))

(defun do-documentclass ()
  (probably-latex)
  (get-bracketed-text-if-any)
  (let ((x (get-peeled-group)))
    (when (member x '("report" "book") :test #'string=)
      (!using-chapters)
      (write-aux '(!using-chapters)))))

(defun get-till-par ()
  (let ((r '()) (newline-p nil))
    (loop
      (let ((c (get-actual-char)))
        (cond ((or (eq c :eof-object)
                   (and newline-p (char= c #\newline)))
               (return (concatenate 'string (nreverse r))))
              (newline-p
                (unless (char-whitespace-p c)
                  (push #\space r)
                  (push c r)
                  (setq newline-p nil)))
              ((char= c #\newline) (setq newline-p t))
              (t (push c r) (setq newline-p nil)))))))

(defun do-beginsection ()
  (do-para)
  (ignorespaces)
  (let ((header (let ((*tabular-stack* (list :header)))
                  (tex-string-to-html-string (get-till-par)))))
    (write-aux `(!default-title ,header))
    (emit-newline)
    (emit "<h1 class=beginsection>")
    (bgroup)
    (if (string= header "") (emit-nbsp 1)
      (emit header))
    (egroup)
    (emit "</h1>")
    (do-noindent)))

(defun do-beginchapter ()
  (if (or (> *html-page-count* 0)
          (tex2page-flag-boolean "\\TZPtitleused"))
      (do-eject)
    (do-para))
  (ignorespaces)
  (let* ((chapno (tex-string-to-html-string
                   (get-till-char #\space)))
         (header (progn (ignorespaces)
                        (let ((*tabular-stack* (list :header)))
                          (tex-string-to-html-string (get-till-par))))))
    (write-aux `(!default-title ,header))
    (emit-newline)
    (tex-def-count "\\footnotenumber" 0 t)
    (do-write-to-toc-aux 1 chapno header)
    (emit "<h1 class=beginchapter>")
    (bgroup)
    (unless (string= chapno "")
      (emit chapno)
      (emit-nbsp 2))
    (if (string= header "") (emit-nbsp 1)
      (emit header))
    (egroup)
    (emit "</h1>")
    (do-noindent)))

(defun do-appendix ()
  (unless *inside-appendix-p*
    (setf *inside-appendix-p* t
          (gethash (if *using-chapters-p* 0 1) *section-counters*) 0)))

(defun do-table-plain () (do-end-para) (emit "<table width=100%><tr><td>"))

(defun do-end-table-plain () (do-end-para) (emit "</td></tr></table>"))

(defun do-table/figure (type)
  (do-end-para)
  (bgroup)
  (when (and (eql type :figure) (char= (snoop-actual-char) #\*))
    (get-actual-char))
  (push type *tabular-stack*)
  (get-bracketed-text-if-any)
  (let ((tbl-tag
         (concatenate 'string *html-node-prefix*
           (if (eql type 'table) "tbl_" "fig_") (gen-temp-string))))
    (tex-def-0arg "\\TIIPcurrentnodename" tbl-tag)
    (emit-anchor tbl-tag)
    (emit-newline)
    (emit "<div class=")
    (emit type)
    (emit " align=")
    (emit *display-justification*)
    (emit "><table width=100%><tr><td align=")
    (emit *display-justification*)
    (emit ">")))

(defun pop-tabular-stack (type)
  (unless (eq (pop *tabular-stack*) type)
    (terror 'pop-tabular-stack "Bad environment closer: " type)))

(defun do-end-table/figure (type)
  (when (and (eql type 'figure) (char= (snoop-actual-char) #\*))
    (get-actual-char))
  (do-end-para)
  (emit "</td></tr>")
  (emit "</table>")
  (emit "</div>")
  (pop-tabular-stack type)
  (egroup)
  (do-para))

(defun bump-dotted-counter (name)
  (let* ((counter (gethash name *dotted-counters*))
         (new-value (1+ (counter*-value counter))))
    (setf (counter*-value counter) new-value)
    (let ((num (concatenate 'string
                            (let ((sec-num (counter*-within counter)))
                              (if sec-num
                                (concatenate 'string
                                             (section-counter-value sec-num)
                                             ".")
                                ""))
                            (format nil "~a" new-value))))
      (tex-def-0arg "\\@currentlabel" num)
      num)))

(defun do-caption ()
  (do-end-para)
  (let* ((i-fig (position :figure *tabular-stack*))
         (i-tbl (position :table *tabular-stack*))
         (type (cond ((and (not i-fig) (not i-tbl))
                      (terror 'do-caption "Mislaid \\caption"))
                     ((not i-fig) :table)
                     ((not i-tbl) :figure)
                     ((< i-fig i-tbl) :figure)
                     ((< i-tbl i-fig) :table)
                     (t (terror 'do-caption "cant happen"))))
         (counter-name (if (eq type :table) "table" "figure"))
         (caption-title (if (eq type :table) "\\tablename" "\\figurename"))
         (num (bump-dotted-counter counter-name)))
    (get-bracketed-text-if-any)
    (emit "</td></tr>")
    (emit-newline)
    (emit "<tr><td align=")
    (emit *display-justification*)
    (emit "><b>")
    (tex2page-string caption-title)
    (emit " ")
    (emit num)
    (emit ":</b>")
    (emit-nbsp 2)
    (tex2page-string (get-group))
    (emit "</td></tr>")
    (emit-newline)
    (emit "<tr><td>")))

(defun do-marginpar ()
  (get-bracketed-text-if-any)
  (emit "<table align=left border=2><tr><td>")
  (tex2page-string (get-group))
  (emit "</td></tr></table>"))

(defun do-minipage ()
  (get-bracketed-text-if-any)
  (get-group)
  (let ((in-table-p
         (and (not (null *tabular-stack*))
              (member (car *tabular-stack*) '(:block :figure :table)))))
    (if in-table-p (emit "</td><td>") (progn (do-para) (do-end-para)))
    (emit "<div align=left>")
    (push :minipage *tabular-stack*)))

(defun do-endminipage ()
  (pop-tabular-stack :minipage)
  (let ((in-table-p (member (car *tabular-stack*) '(:block :figure :table))))
    (emit "</div>")
    (if in-table-p (emit "</td><td>") (do-para))))

(defun do-tabbing ()
  (push :tabbing *tabular-stack*)
  (do-para))

(defun do-end-tabbing ()
  (pop-tabular-stack :tabbing)
  (do-para))

(defun do-equation (type)
  (cond ((and (tex2page-flag-boolean "\\TZPmathimage")
              (not *temporarily-use-utf8-for-math-p*))
         (do-latex-env-as-image
          (case type
            (:equation "equation")
            (:align "align")
            (t "eqnarray"))
          :display))
        (t (do-end-para)
           (bgroup)
           (when (eql type :align) (setq type :eqnarray))
           (when (and (eql type :eqnarray) (eat-star)) (setq type :eqnarray*))
           (push type *tabular-stack*)
           (setq *math-mode-p* t
                 *in-display-math-p* t)
           (let ((eqn-tag
                  (concatenate 'string *html-node-prefix* "eqn_" (gen-temp-string))))
             (tex-def-0arg "\\TIIPcurrentnodename" eqn-tag)
             (emit-anchor eqn-tag)
             (emit-newline)
             (unless (eql type :eqnarray*)
               (setq *equation-number* (bump-dotted-counter "equation")))
             (emit "<div align=")
             (emit *display-justification*)
             (emit "><table width=100%>")
             (emit-newline)
             (emit "<tr><td align=")
             (emit (if (eql type :equation) "center" "right"))
             (emit ">")))))

(defun do-end-equation ()
  (do-end-para)
  (emit "</td>")
  (unless (or (eq (car *tabular-stack*) :eqnarray*)
              (not *equation-numbered-p*))
    (emit "<td>(")
    (emit *equation-number*)
    (emit ")</td>"))
  (emit "</tr>")
  (emit-newline)
  (emit "</table></div>")
  (pop-tabular-stack :equation)
  (setq *math-mode-p* nil
        *in-display-math-p* nil)
  (egroup)
  (setq *equation-numbered-p* t
        *equation-position* 0)
  (do-para))

(defun do-integral ()
  (if (or (not *in-display-math-p*) *math-script-mode-p*)
      (emit "&int;")
    (let ((affixes-already-read '()))
      (emit "<span style=\"font-size: 200%; position: relative; top: .25ex;\">&int;</span>")
      (dotimes (i 2)
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (when (and (member c '(#\_ #\^))
                     (not (member c affixes-already-read)))
            (pushnew c affixes-already-read)
            (get-actual-char)
            (when (= i 0)
              (emit (kern ".16667em")))
            (let* ((*math-script-mode-p* t)
                   (s  (get-token)))
              (emit "<span style=\"font-size: 85%; position: relative; ")
              (emit (ecase c
                      (#\_  "top: 2.5ex; ")
                      (#\^ "bottom: 3ex; ")))
              (emit "\">")
              (tex2page-string s)
              (emit "</span>"))))))))

(defun do-nonumber ()
  (setq *equation-numbered-p* nil))

(defun indent-n-levels (n)
  (dotimes (i (1+ n))
    (emit-nbsp 1)
    (emit " ")
    (emit-nbsp 1)
    (emit " ")))

(defun do-toc ()
  (let ((*subjobname* (concatenate 'string *jobname* *toc-file-suffix*))
        (*img-file-count* 0)
        (*imgdef-file-count* 0))
    (when (eql *tex-format* :latex)
      (tex2page-string
       (if *using-chapters-p* "\\chapter*{\\contentsname}"
         "\\section*{\\contentsname}")))
    (emit-anchor (concatenate 'string *html-node-prefix* "toc_start"))
    (!toc-page *html-page-count*)
    (write-aux `(!toc-page ,*html-page-count*))
    (cond ((null *toc-list*) (flag-missing-piece :toc)
           (non-fatal-error "Table of contents not generated; rerun TeX2page"))
          (t (do-noindent)
             (let ((tocdepth (get-gcount "\\tocdepth")))
               (mapc
                (lambda (x)
                  (let* ((lvl (tocentry*-level x))
                         (secnum (tocentry*-number x))
                         (seclabel (tocentry*-label x))
                         (subentries-p
                          (or (= lvl -1)
                              (and (= lvl 0)
                                   (or (< tocdepth -1)
                                       (and *using-chapters-p* (> tocdepth 0))
                                       (and (not *using-chapters-p*)
                                            (> tocdepth 1)))))))
                    (when subentries-p
                      (if *tex-like-layout-p* (do-bigskip :medskip) (do-para))
                      (do-noindent)
                      (emit "<b>")
                      (emit-newline))
                    (indent-n-levels lvl)
                    (emit-anchor
                     (concatenate 'string *html-node-prefix* "toc_" seclabel))
                    (emit-page-node-link-start (tocentry*-page x) seclabel)
                    (unless (or (string= secnum "") (string= secnum "IGNORE"))
                      (emit secnum)
                      (emit-nbsp 2))
                    (let ((*tabular-stack* (list 'header)))
                      (emit (tocentry*-header x)))
                    (emit-link-stop)
                    (when subentries-p (emit "</b>"))
                    (emit "<br>")
                    (emit-newline)))
                *toc-list*))))
    (emit-anchor (concatenate 'string *html-node-prefix* "toc_end"))))

(defstruct footnotev*  mark text tag caller)

(defun do-numbered-footnote ()
  (do-footnote-aux nil))

(defun do-symfootnote ()
  (incf *footnote-sym*)
  (do-footnote-aux (number-to-footnote-symbol *footnote-sym*)))

(defun tex-string-to-html-string (s)
  (let* ((tmp-port (make-string-output-stream))
         (*html* tmp-port))
    (tex2page-string s)
    (get-output-stream-string tmp-port)))

(defun expand-tex-string (s)
  (let* ((tmp-port (make-string-output-stream))
         (*html* tmp-port)
         (*outputting-to-non-html-p* t))
    (tex2page-string s)
    (get-output-stream-string tmp-port)))

(let ((symlist nil)
      (symlist-len 0))
  (defun number-to-footnote-symbol (n)
    (unless symlist
      (setq symlist
            '("*" "&dagger;" "&Dagger;" "&sect;" "&para;"
              "&#x2225;" ; ||
              "*" "&dagger;&dagger;" "&Dagger;&Dagger;")
            symlist-len (length symlist)))
    (elt symlist (mod (1- n) symlist-len))))

(defun do-plain-footnote ()
  (do-footnote-aux
   (let ((*temporarily-use-utf8-for-math-p* t))
     (tex-string-to-html-string (get-token)))))

(defun do-footnote ()
  (if (eq *tex-format* :latex)
      (do-numbered-footnote)
    (do-plain-footnote)))

(defun do-footnote-aux (fnmark)
  (let* ((fnno nil)
         (fnlabel (gen-temp-string))
         (fntag
          (concatenate 'string *html-node-prefix* "footnote_" fnlabel))
         (fncalltag
          (concatenate 'string *html-node-prefix* "call_footnote_"
            fnlabel)))
    (unless fnmark
      (setq fnno (+ (get-gcount "\\footnotenumber") 1))
      (tex-gdef-count "\\footnotenumber" fnno)
      (setq fnmark (write-to-string fnno)))
    (emit-anchor fncalltag)
    (when fnno (emit "<sup><small>"))
    (emit-page-node-link-start nil fntag)
    (emit fnmark)
    (emit-link-stop)
    (when fnno (emit "</small></sup>"))
    (do-vfootnote-aux fnmark fncalltag fntag)))

(defun do-vfootnote ()
  (do-vfootnote-aux
   (let ((*temporarily-use-utf8-for-math-p* t))
     (tex-string-to-html-string (get-token)))
   nil nil))

(defun do-vfootnote-aux (fnmark fncalltag fntag)
  (ignorespaces)
  (unless (char= (get-actual-char) #\{) (terror 'do-vfootnote-aux "Missing {"))
  (bgroup)
  (let ((old-html *html*) (fn-tmp-port (make-string-output-stream)))
    (setq *html* fn-tmp-port)
    (when fncalltag
      (tex-def-0arg "\\TIIPcurrentnodename" fntag)
      (tex-def-0arg "\\@currentlabel" fnmark))
    (add-aftergroup-to-top-frame
     (lambda ()
       (push (make-footnotev* :mark fnmark :text
                              (get-output-stream-string fn-tmp-port)
                              :tag fntag :caller fncalltag)
             *footnote-list*)
       (setq *html* old-html)))))

(defun output-footnotes ()
  (let ((n (length *footnote-list*)))
    (unless (= n 0)
      (emit "<div class=footnoterule><hr></div>")
      (do-para)
      (do-end-para)
      (emit "<div class=footnote>")
      (let ((i (- n 1)))
        (loop
          (when (< i 0) (return))
          (let* ((fv (elt *footnote-list* i))
                 (fnmark (footnotev*-mark fv))
                 (fnno (read-from-string fnmark))
                 (fncalltag (footnotev*-caller fv)))
            (do-para)
            (when fncalltag
              (emit-anchor (footnotev*-tag fv))
              (when fnno (emit "<sup><small>"))
              (emit-page-node-link-start nil fncalltag))
            (emit fnmark)
            (when fncalltag
              (emit-link-stop)
              (when fnno (emit "</small></sup>")))
            (emit " ")
            (emit (footnotev*-text fv))
            (do-end-para)
            (decf i))))
      (emit "</div>")
      (emit-newline))))

(let ((f (lambda (x)
           (let* ((n  (round (* 1.0 x)))
                  (s (write-to-string n :base 16)))
             (if (< n 16) (concatenate 'string "0" s) s)))))
  (defun rgb-dec-to-hex (r g b)
    (concatenate 'string (funcall f r) (funcall f g) (funcall f b))))

(defun rgb-frac-to-hex (r g b)
  (rgb-dec-to-hex (* r 255) (* g 255) (* b 255)))

(let ((f (lambda (x k) (- 1 (min (max (+ x k) 0) 1)))))
  (defun cmyk-to-rgb (c m y k)
    (rgb-frac-to-hex (funcall f c k) (funcall f m k) (funcall f y k))))

(defun do-color ()
  (let ((model (get-bracketed-text-if-any)))
    (do-switch
     (cond ((not model) :colornamed)
           ((string= model "rgb") :rgb)
           ((string= model "RGB") :rgb255)
           ((string= model "cmyk") :cmyk)
           ((string= model "gray") :gray)
           (t :colornamed)))))

(defun do-definecolor ()
  (let* ((name (get-peeled-group))
         (model (get-peeled-group))
         (spec (get-peeled-group)))
    (bgroup)
    (push (cons name
                (if (string= model "named")
                    (let ((c (assoc name *color-names* :test #'string=)))
                      (if c (cdr c)
                        (terror 'do-definecolor "Color name " name
                                " not defined")))
                  (with-input-from-string
                      (i
                       (tex-string-to-html-string
                        (concatenate 'string "\\defcsactive\\,{ }"
                          spec)))
                    (cond ((string= model "cmyk")
                           (let* ((c (read i nil :eof-object))
                                  (m (read i nil :eof-object))
                                  (y (read i nil :eof-object))
                                  (k (read i nil :eof-object)))
                             (cmyk-to-rgb c m y k)))
                          ((string= model "rgb")
                           (let* ((r (read i nil :eof-object))
                                  (g (read i nil :eof-object))
                                  (b (read i nil :eof-object)))
                             (rgb-frac-to-hex r g b)))
                          ((string= model "RGB")
                           (let* ((r (read i nil :eof-object))
                                  (g (read i nil :eof-object))
                                  (b (read i nil :eof-object)))
                             (rgb-dec-to-hex r g b)))
                          ((string= model "gray")
                           (cmyk-to-rgb 0 0 0 (read i nil :eof-object)))
                          (t
                           (terror 'do-definecolor
                                   "Unknown color model"))))))
          *color-names*)
    (egroup)))

(defun do-switch (sw)
  (unless *outputting-external-title-p*
    (add-postlude-to-top-frame
     (case sw
       (:rm
        (when *math-mode-p*
          (let ((old-math-font *math-font*))
            (setq *math-font* :rm)
            (lambda () (setq *math-font* old-math-font)))))
       (:em (emit "<em>") (lambda () (emit "</em>")))
       (:it (emit "<i>") (lambda () (emit "</i>")))
       (:bf (emit "<strong>") (lambda () (emit "</strong>")))
       (:sl
        (emit "<span style=\"font-style: oblique\">")
        (lambda () (emit "</span>")))
       (:sf
        (emit "<span style=\"font-family: sans-serif\">")
        (lambda () (emit "</span>")))
       (:tt
        (let ((old-ligatures-p *ligatures-p*))
          (setq *ligatures-p* nil)
          (emit "<tt>")
          (lambda () (emit "</tt>") (setq *ligatures-p* old-ligatures-p))))
       (:sc
        (let ((old-in-small-caps-p *in-small-caps-p*))
          (setq *in-small-caps-p* t)
          (lambda () (setq *in-small-caps-p* old-in-small-caps-p))))
       (:span
        (emit "<span ")
        (emit (get-peeled-group))
        (emit ">")
        (lambda () (emit "</span>")))
       (:div
        (emit "<div ")
        (emit (get-peeled-group))
        (emit ">")
        (lambda () (emit "</div>")))
       (:tiny (emit "<span class=tiny>") (lambda () (emit "</span>")))
       (:scriptsize
        (emit "<span class=scriptsize>")
        (lambda () (emit "</span>")))
       ((:footnotesize :fiverm)
        (emit "<span class=footnotesize>")
        (lambda () (emit "</span>")))
       ((:small :sevenrm)
        (emit "<span class=small>")
        (lambda () (emit "</span>")))
       (:normalsize
        (emit "<span class=normalsize>")
        (lambda () (emit "</span>")))
       (:large (emit "<span class=large>") (lambda () (emit "</span>")))
       (:large-cap
        (emit "<span class=largecap>")
        (lambda () (emit "</span>")))
       (:large-up
        (emit "<span class=largeup>")
        (lambda () (emit "</span>")))
       (:huge (emit "<span class=huge>") (lambda () (emit "</span>")))
       (:huge-cap
        (emit "<span class=hugecap>")
        (lambda () (emit "</span>")))
       (:cmyk
        (bgroup)
        (with-input-from-string
            (i
             (tex-string-to-html-string
              (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
          (let* ((c (read i nil :eof-object))
                 (m (read i nil :eof-object))
                 (y (read i nil :eof-object))
                 (k (read i nil :eof-object)))
            (ignorespaces)
            (emit "<span style=\"color: #")
            (emit (cmyk-to-rgb c m y k))
            (emit "\">")))
        (egroup)
        (lambda () (emit "</span>")))
       (:rgb
        (bgroup)
        (with-input-from-string
            (i
             (tex-string-to-html-string
              (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
          (let* ((r (read i nil :eof-object))
                 (g (read i nil :eof-object))
                 (b (read i nil :eof-object)))
            (ignorespaces)
            (emit "<span style=\"color: #")
            (emit (rgb-frac-to-hex r g b))
            (emit "\">")))
        (egroup)
        (lambda () (emit "</span>")))
       (:rgb255
        (bgroup)
        (with-input-from-string
            (i
             (tex-string-to-html-string
              (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
          (let* ((r (read i nil :eof-object))
                 (g (read i nil :eof-object))
                 (b (read i nil :eof-object)))
            (ignorespaces)
            (emit "<span style=\"color: #")
            (emit (rgb-dec-to-hex r g b))
            (emit "\">")))
        (egroup)
        (lambda () (emit "</span>")))
       (:gray
        (with-input-from-string (i (tex-string-to-html-string (get-token)))
          (let ((g (read i nil :eof-object)))
            (ignorespaces)
            (emit "<span style=\"color: #")
            (emit (cmyk-to-rgb 0 0 0 (- 1 g)))
            (emit "\">")))
        (lambda () (emit "</span>")))
       (:colornamed
        (let* ((name (get-peeled-group))
               (c (assoc name *color-names* :test #'string=)))
          (ignorespaces)
          (emit "<span style=\"color: ")
          (emit (if c (progn (emit #\#) (cdr c)) name))
          (emit "\">")
          (lambda () (emit "</span>"))))
       (:bgcolor
        (emit "<span style=\"background-color: ")
        (let ((color (ungroup (get-group))))
          (when (string-to-number color 16)
            (emit "#"))
          (emit color)
          (emit "\">")
          (lambda () (emit "</span>"))))
       (:strike (emit "<strike>") (lambda () (emit "</strike>")))
       (:narrower (emit "<blockquote>") (lambda () (emit "</blockquote>")))
       (:raggedleft
        (do-end-para)
        (emit "<div align=right>")
        (lambda () (do-end-para) (emit "</div>") (do-para)))
       (t
        (emit "<span class=")
        (emit sw)
        (emit ">")
        (lambda () (emit "</span>")))))))

(defun do-obeylines ()
  (when (eql (snoop-actual-char) #\Newline) (get-actual-char))
  ;(do-noindent)
  (activate-cdef #\Newline)
  (tex-def-char #\Newline '() "\\TIIPpar" nil))

(defun do-obeyspaces ()
  (activate-cdef #\space)
  (tex-def-char #\space '() "\\TIIPnbsp" nil))

(defun do-obeywhitespace () (do-obeylines) (do-obeyspaces))

(defun do-block (z)
  (do-end-para)
  (emit "<div ")
  (emit (case z
          (:flushleft "align=left")
          (:flushright "align=right")
          (t "align=center")))
  (emit ">")
  (push :block *tabular-stack*)
  (emit "<table><tr><td>")
  (bgroup)
  (emit-newline))

(defun do-end-block ()
  (do-end-para)
  (egroup)
  (emit "</td></tr></table></div>")
  (pop-tabular-stack :block)
  (emit-newline))

(defun do-function (fn)
  (let ((*math-mode-p* *math-mode-p*))
    (cond (*outputting-external-title-p* nil)
          ((string= fn "\\emph") (emit "<em>"))
          ((string= fn "\\leftline") (do-end-para) (emit "<div align=left>"))
          ((string= fn "\\centerline") (do-end-para)
           (emit "<div align=center>&nbsp;"))
          ((string= fn "\\rightline") (do-end-para)
           (emit "<div align=right>&nbsp;"))
          ((string= fn "\\underline") (emit "<u>"))
          ((string= fn "\\textbf") (setq *math-mode-p* nil) (emit "<b>"))
          ((or (string= fn "\\textit") (string= fn "\\textsl"))
           (setq *math-mode-p* nil) (emit "<i>"))
          ((string= fn "\\textrm") (setq *math-mode-p* nil))
          ((string= fn "\\texttt") (setq *math-mode-p* nil) (emit "<tt>"))
          (t (terror 'do-function "Unknown function " fn)))
    (bgroup)
    (tex2page-string (get-token))
    (egroup)
    (cond (*outputting-external-title-p* nil)
          ((string= fn "\\emph") (emit "</em>"))
          ((string= fn "\\rightline") (emit "</div>") (emit-newline))
          ((or (string= fn "\\leftline") (string= fn "\\centerline"))
           (do-end-para) (emit "&nbsp;</div>") (emit-newline))
          ((string= fn "\\underline") (emit "</u>"))
          ((string= fn "\\textbf") (emit "</b>"))
          ((or (string= fn "\\textsl") (string= fn "\\textit"))
           (emit "</i>"))
          ((string= fn "\\texttt") (emit "</tt>")))))

(defun do-discretionary ()
  (tex2page-string (get-group))
  (get-group)
  (get-group))

(defun do-aftergroup ()
  (ignorespaces)
  (let ((z (get-ctl-seq)))
    (add-aftergroup-to-top-frame (lambda () (toss-back-string z)))))

(defun do-afterassignment ()
  (ignorespaces)
  (let ((z (get-ctl-seq)))
    (setq *afterassignment* z)))

(defun do-space ()
  (emit #\space))

(defun do-tab ()
  (emit-nbsp 8))

(defun emit-nbsp (n)
  (dotimes (i n)
    (emit "&nbsp;")))

(defun scaled-point-equivalent-of (unit)
  (case unit
    (:sp 1)
    (:pt 65536)
    (:bp (* (/ 72) (scaled-point-equivalent-of :in)))
    (:cc (* 12 (scaled-point-equivalent-of :dd)))
    (:dd (* (/ 1238 1157) (scaled-point-equivalent-of :pt)))
    (:em (* 10 (scaled-point-equivalent-of :pt)))
    (:ex (* 4.5 (scaled-point-equivalent-of :pt)))
    (:in (* 72.27 (scaled-point-equivalent-of :pt)))
    (:mm (* 0.1 (scaled-point-equivalent-of :cm)))
    (:cm (* (/ 2.54) (scaled-point-equivalent-of :in)))
    (:pc (* 12 (scaled-point-equivalent-of :pt)))
    (t (terror 'scaled-point-equivalent-of
               "Illegal unit of measure " unit))))

(defun tex-length (num unit)
  (* num (scaled-point-equivalent-of unit)))

(defun sp-to-ems (sp)
  (/ sp 65536 10.0))

(defun sp-to-pixels (sp)
  (floor (/ sp 65536)))

(defun get-scaled-points ()
  (let ((n (or (get-real) 1)))
    (ignorespaces)
    (* n (if (char= (snoop-actual-char) *esc-char*)
             (let ((x (get-ctl-seq)))
               (get-dimen x))
           (progn
            (loop
              (unless (eat-word "true") (return)))
            (cond ((eat-word "bp") (tex-length 1 :bp))
                  ((eat-word "cc") (tex-length 1 :cc))
                  ((eat-word "cm") (tex-length 1 :cm))
                  ((eat-word "dd") (tex-length 1 :dd))
                  ((eat-word "em") (tex-length 1 :em))
                  ((eat-word "ex") (tex-length 1 :ex))
                  ((eat-word "in") (tex-length 1 :in))
                  ((eat-word "mm") (tex-length 1 :mm))
                  ((eat-word "pc") (tex-length 1 :pc))
                  ((eat-word "pt") (tex-length 1 :pt))
                  ((eat-word "sp") 1)
                  (t 1)))))))

(defun get-points () (/ (get-scaled-points) 65536.0))

(defun get-pixels ()
  ;assume 1 HTML pixel == 1 TeX pt
  (floor (get-points)))

(defun do-font ()
  (get-ctl-seq)
  (get-equal-sign)
  (eat-alphanumeric-string)
  (cond ((eat-word "at") (eat-dimen))
        ((eat-word "scaled") (get-number))))

;(defun do-hskip ()
;  ;assume 1 space is 5 pxl wide
;  (emit-nbsp (/ (get-pixels) 5)))

(defun do-hskip ()
  (let ((n (get-pixels)))
    (emit "<span style=\"margin-left: ")
    (emit n)
    ;span needs to contain something that can be moved: use zwnj
    (emit "pt\">&#x200c;</span>")))

(defun do-vskip ()
  (let ((x (get-points)))
    (eat-skip-fluff nil)
    (emit "<div style=\"height: ")
    (emit x)
    (emit "pt\"></div>")
    (emit-newline)
    (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
    (setq *in-para-p* t)))

(defun do-newline () (when (>= (munch-newlines) 1) (do-para)) (emit-newline))

(defun do-br ()
  (if (or (find-chardef #\space) (not (= (the-count "\\TIIPobeylinestrictly") 0)))
      (emit "<br>") (unless (eql (snoop-actual-char) #\Newline) (emit "<br>")))
  (emit-newline))

(defun do-sup ()
  (emit "<sup>")
  (let ((*math-script-mode-p* t))
    (tex2page-string (get-token)))
  (emit "</sup>"))

(defun do-sub ()
  (emit "<sub>")
  (let ((*math-script-mode-p* t))
    (tex2page-string (get-token)))
  (emit "</sub>"))

(defun do-hyphen ()
  (cond (*math-mode-p*
         (emit (if (eq *math-font* :rm) "-" "&minus;")))
        ((not *ligatures-p*) (emit #\-))
        (t
         (let ((c (snoop-actual-char)))
           (if (and (characterp c) (char= c #\-))
               (progn (get-actual-char) (do-ndash)) (emit #\-))))))

(defun do-excl ()
  (if (or *math-mode-p* (not *ligatures-p*)) (emit #\!)
      (let ((c (snoop-actual-char)))
        (if (and (characterp c) (char= c #\`))
            (progn (get-actual-char) (emit "&iexcl;")) (emit #\!)))))

(defun do-quest ()
  (if (or *math-mode-p* (not *ligatures-p*)) (emit #\?)
      (let ((c (snoop-actual-char)))
        (if (and (characterp c) (char= c #\`))
            (progn (get-actual-char) (emit "&iquest;")) (emit #\?)))))

(defun do-ndash ()
  (emit
   (let ((c (snoop-actual-char)))
     (if (and (characterp c) (char= c #\-))
         (progn (get-actual-char) "&mdash;") "&ndash;"))))

(defun do-lsquo ()
  (emit
   (if (not *ligatures-p*) "&lsquo;"
       (let ((c (snoop-actual-char)))
         (if (and (characterp c) (char= c #\`))
             (progn (get-actual-char) "&ldquo;") "&lsquo;")))))

(defun do-rsquo ()
  (emit
   (cond (*math-mode-p*
          (let ((c (snoop-actual-char)))
            (if (and (characterp c) (char= c #\'))
                (progn (get-actual-char) "&Prime;") "&prime;")))
         ((not *ligatures-p*) "&rsquo;")
         (t
          (let ((c (snoop-actual-char)))
            (if (and (characterp c) (char= c #\'))
                (progn (get-actual-char) "&rdquo;") "&rsquo;"))))))

(defstruct label*  (src nil) page name value)

(defun get-label ()
  (let* ((lbl (get-peeled-group))
         (i (or (position #\space lbl :test #'char=)
                (position #\tab lbl :test #'char=)
                (position #\Newline lbl :test #'char=))))
    (if (not i) lbl
      (let ((s (concatenate 'list lbl)) (r '()) (whitep nil))
        (loop
          (when (null s) (return (concatenate 'string (nreverse r))))
          (let ((c (pop s)))
            (cond ((char-whitespace-p c)
                   (unless whitep (push #\space r))
                   (setq whitep t))
                  (t (push c r)
                     (setq whitep nil)))))))))

(defun emit-anchor (lbl) (emit "<a name=\"") (emit lbl) (emit "\"></a>"))

(defun emit-link-start (link) (emit "<a href=\"") (emit link) (emit "\">"))

(defun emit-ext-page-node-link-start (extfile pageno node)
  (emit "<a href=\"")
  (unless (and (not extfile) (or (not pageno) (= *html-page-count* pageno)))
    (emit (or extfile *jobname*))
    (unless (= pageno 0) (emit *html-page-suffix*) (emit pageno))
    (emit *output-extension*))
  (when node (emit "#") (emit node))
  (emit "\">"))

(defun emit-page-node-link-start (pageno node)
  (emit-ext-page-node-link-start nil pageno node))

(defun emit-link-stop () (emit "</a>"))

(defun do-anchor-for-potential-label ()
  (let ((node-name
         (concatenate 'string *html-node-prefix* "anchor_" (gen-temp-string))))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (emit-anchor node-name)))

(defun do-label () (do-label-aux (get-label)))

(defun do-node () (setq *recent-node-name* (get-peeled-group)))

(defun do-label-aux (label)
  (let ((name (ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename"))
        (value (ctl-seq-no-arg-expand-once "\\@currentlabel")))
    (setq value (tex-string-to-html-string value))
    (!label label *html-page-count* name value)
    (write-label `(!label ,label ,*html-page-count* ,name ,value))))

(defun do-inputexternallabels ()
  (let* ((f (get-filename-possibly-braced))
         (fq-f (if (fully-qualified-pathname-p f) f (concatenate 'string *aux-dir/* f)))
         (ext-label-file (concatenate 'string fq-f *label-file-suffix* ".scm"))
         (ext-label-table (gethash f *external-label-tables*)))
    (unless ext-label-table
      (setq ext-label-table (make-hash-table :test #'equal))
      (setf (gethash f *external-label-tables*) ext-label-table))
    (when (probe-file ext-label-file)
      (let ((*label-source* fq-f) (*label-table* ext-label-table))
        (load-tex2page-data-file ext-label-file)))))

(defun do-includeexternallabels ()
  (let* ((jobname (get-filename-possibly-braced))
         (ext-label-file
          (concatenate 'string
            (if (fully-qualified-pathname-p jobname) jobname
              (concatenate 'string *aux-dir/* jobname))
            *label-file-suffix* ".scm")))
    (when (probe-file ext-label-file)
      (let ((*label-source* jobname))
        (load-tex2page-data-file ext-label-file)))))

(defun do-tag ()
  (let ((tag-name (get-peeled-group)))
    (do-tag-aux tag-name (get-group))))

(defun do-definexref ()
  (let* ((tag (get-peeled-group))
         (value (get-group)))
    (get-token)
    (do-tag-aux tag value)))

(defun do-xrdef ()
  (let ((tag (get-peeled-group)))
    (do-tag-aux tag (write-to-string *html-page-count*))))

(defun do-tag-aux (tag-name tag-val)
  (let ((node-name
         (concatenate 'string *html-node-prefix* "tag_" (gen-temp-string))))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (tex-def-0arg "\\@currentlabel" tag-val)
    (emit-anchor node-name)
    (do-label-aux tag-name)))

(defun do-htmlpagelabel ()
  (let ((label (get-peeled-group)))
    (!label label *html-page-count* nil nil)
    (write-label `(!label ,label ,*html-page-count* nil nil))))

(defun do-ref () (do-ref-aux (get-label) nil nil))

(defun do-refexternal ()
  (let ((ext-file (get-peeled-group)))
    (do-ref-aux (get-label) ext-file nil)))

(defun do-ref-aux (label ext-file link-text)
  (let* ((label-ref (label-bound-p label ext-file))
         (label-text
          (cond (link-text (tex-string-to-html-string link-text))
                (label-ref (label*-value label-ref))
                (t label))))
    (cond (label-ref (emit-ext-page-node-link-start
                      (or ext-file (label*-src label-ref))
                      (label*-page label-ref)
                      (label*-name label-ref))
                     (emit label-text)
                     (emit-link-stop))
          (t (non-fatal-error label)))))

(defun maybe-label-page (this-label-src this-label-pageno)
  (if (and (not this-label-src) (= *html-page-count* this-label-pageno)) ""
      (concatenate 'string (or this-label-src *jobname*)
                   (if (= this-label-pageno 0) ""
                       (concatenate 'string *html-page-suffix*
                                    (write-to-string this-label-pageno)))
                   *output-extension*)))

(defun do-htmlref ()
  (let* ((text (get-group))
         (lbl (get-peeled-group)))
    (do-ref-aux lbl nil text)))

(defun do-htmlrefexternal ()
  (let* ((text (get-group))
         (extf (get-peeled-group))
         (lbl (get-peeled-group)))
    (do-ref-aux lbl extf text)))

(defun do-hyperref ()
  (let ((lbl (get-bracketed-text-if-any)))
    (if lbl
        (do-ref-aux lbl nil (get-group))
      ;this is old code -- need to align it with \hyperdef
      (let* ((text (get-group))
             (lbl (progn (get-group) (get-group) (get-peeled-group))))
        (do-ref-aux lbl nil text)))))

(defun do-hypertarget ()
  (let ((lbl (get-peeled-group)))
    (do-tag-aux lbl "hypertarget")))

(defun do-hyperlink ()
  (emit-link-start
   (fully-qualify-url (concatenate 'string "#" (get-peeled-group))))
  (tex2page-string (get-token))
  (emit-link-stop))

(defun label-bound-p (label &optional ext-file)
  (let ((label-table (if ext-file
                         (gethash ext-file *external-label-tables*)
                       *label-table*)))
    (or (and label-table (gethash label label-table))
        (progn
         (flag-unresolved-xref
          (if ext-file (concatenate 'string "{" ext-file " -> " label "}")
            label))
         nil))))

(defun flag-unresolved-xref (xr)
  (pushnew xr *unresolved-xrefs* :test #'equal))

(defun flag-missing-piece (mp)
  ; ;test #'equal ?
  (pushnew mp *missing-pieces*))

(defun show-unresolved-xrefs-and-missing-pieces ()
  (unless (and (null *unresolved-xrefs*) (null *missing-pieces*))
    (show-unresolved-xrefs)
    (show-missing-pieces)
    (write-log :separation-newline)
    (write-log "Rerun: tex2page ")
    (write-log *main-tex-file*)
    (write-log :separation-newline)
    (write-log "If problem persists, check for ")
    (write-log "missing \\label's and \\bibitem's")))

(defun show-unresolved-xrefs ()
  (unless (null *unresolved-xrefs*)
    (write-log :separation-newline)
    (write-log "Unresolved cross-reference")
    (if (> (length *unresolved-xrefs*) 1) (write-log "s"))
    (write-log ": ")
    (setq *unresolved-xrefs* (nreverse *unresolved-xrefs*))
    (write-log (car *unresolved-xrefs*))
    (mapc
     (lambda (x) (write-log #\,) (write-log :separation-space) (write-log x))
     (cdr *unresolved-xrefs*))
    (write-log :separation-newline)))

(defun show-missing-pieces ()
  (unless (null *missing-pieces*)
    (write-log :separation-newline)
    (when (member :document-title *missing-pieces*)
      (write-log "Document title not determined")
      (write-log :separation-newline))
    (when (member :last-page *missing-pieces*)
      (write-log "Last page not determined")
      (write-log :separation-newline))
    (when (member :last-modification-time *missing-pieces*)
      (write-log "Last modification time not determined")
      (write-log :separation-newline))
    (when (member :stylesheets *missing-pieces*)
      (write-log "Style sheets not determined")
      (write-log :separation-newline))
    (when (member :html-head *missing-pieces*)
      (write-log "HTML header info not determined")
      (write-log :separation-newline))
    (when (member :toc *missing-pieces*)
      (write-log "Table of contents not determined")
      (write-log :separation-newline))
    (cond ((member :fresh-index *missing-pieces*) (write-log "Index not refreshed")
           (write-log :separation-newline))
          ((member :index *missing-pieces*) (write-log "Index not included")
           (write-log :separation-newline)))
    (cond ((member :fresh-bibliography *missing-pieces*)
           (write-log "Bibliography not refreshed") (write-log :separation-newline))
          ((member :bibliography *missing-pieces*)
           (write-log "Bibliography not included") (write-log :separation-newline)))
    (when (member :metapost *missing-pieces*)
      (write-log "MetaPost output not included")
      (write-log :separation-newline))))

(defun do-pageref ()
  (let ((label-ref (label-bound-p (get-peeled-group))))
    (if label-ref
        (let ((pageno (label*-page label-ref)))
          (emit-ext-page-node-link-start (label*-src label-ref) pageno nil)
          (emit pageno)
          (emit-link-stop))
        (non-fatal-error "***"))))

(defun do-htmlpageref ()
  (let* ((label (get-peeled-group))
         (label-ref (label-bound-p label)))
    (emit "\"")
    (if label-ref
        (emit
         (maybe-label-page (label*-src label-ref) (label*-page label-ref)))
      (emit *log-file*))
    (emit "\"")))

(defun fully-qualify-url (url)
  (let ((n (length url)))
    (cond ((and (> n 0) (char= (char url 0) #\#))
           (let* ((label (subseq url 1 n))
                  (label-ref (label-bound-p label)))
             (if label-ref
                 (concatenate 'string
                   (maybe-label-page (label*-src label-ref)
                                     (label*-page label-ref))
                   "#" (label*-name label-ref))
               url)))
          ((fully-qualified-url-p url) url) (t (ensure-url-reachable url) url))))

(defun do-url ()
  (let ((url (get-url)))
    (emit-link-start (fully-qualify-url url))
    (emit url)
    (emit-link-stop)))

(defun do-mailto ()
  (let ((addr (get-url)))
    (emit-link-start (concatenate 'string "mailto:" addr))
    (emit addr)
    (emit-link-stop)))

(defun do-urlh ()
  (emit-link-start (fully-qualify-url (get-url)))
  (bgroup)
  (tex2page-string
   (concatenate 'string "\\def\\\\{\\egroup\\endinput}" (get-token)))
  (egroup)
  (emit-link-stop))

(defun do-urlhd ()
  (do-urlh)
  (get-token) ;throw away text meant for PDF
  )

(defun do-urlp ()
  (let ((link-text (get-token)))
    (emit-link-start (fully-qualify-url (get-url)))
    (tex2page-string link-text)
    (emit-link-stop)))

(defun do-hlstart ()
  (let* ((cat (get-peeled-group))
         (url (progn (get-token) ;ignoring options
                     (get-url))))
    (when (string= cat "url")
      (emit-link-start (fully-qualify-url url))
      (bgroup)
      (tex-let "\\hlend" "\\TIIPhlend" nil))
    (ignorespaces)))

(defun do-hlend () (egroup) (emit-link-stop))

(defun do-htmladdimg ()
  (let* ((align-info (get-bracketed-text-if-any))
         (url (fully-qualify-url (get-url))))
    (emit "<img src=\"")
    (emit url)
    (emit "\" border=\"0\" ")
    (when align-info (tex2page-string align-info))
    (emit " alt=\"[")
    (emit url)
    (emit "]\">")))

(defun do-pdfximage ()
  (let ((height nil) (width nil))
    (loop
      (cond ((eat-word "height") (setq height (get-pixels)))
            ((eat-word "width") (setq width (get-pixels)))
            ((eat-word "depth") (get-pixels)) ;ignoring depth
            (t (return))))
    (emit "<img")
    (when height (emit " height=") (emit height))
    (when width (emit " width=") (emit width))
    (emit " src=\"")
    (emit (fully-qualify-url (get-filename-possibly-braced)))
    (emit "\">")
    (ignorespaces)
    (get-ctl-seq)
    (ignorespaces)
    (get-ctl-seq)))

(defun do-cite ()
  (let ((extra-text (get-bracketed-text-if-any)))
    (emit "[")
    (ignorespaces)
    (unless (char= (get-actual-char) #\{)
      (terror 'do-cite "missing {"))
    (let ((first-key-p t))
      (loop
        (let ((key (get-csv)))
          (unless key (return))
          (cond (first-key-p (setq first-key-p nil))
                (t (emit ",") (emit-nbsp 1)))
          (write-bib-aux "\\citation{")
          (write-bib-aux key)
          (write-bib-aux "}")
          (write-bib-aux #\newline)
          (do-ref-aux (concatenate 'string "cite{" key "}") nil nil)))
      (when extra-text
        (emit ",") (emit-nbsp 1)
        (tex2page-string extra-text))
      (unless (char= (get-actual-char) #\})
        (terror 'do-cite "missing }"))
      (when first-key-p
        (terror 'do-cite "empty \\cite")))
    (emit "]")))

(defun do-nocite ()
  (ignorespaces)
  (unless (char= (get-actual-char) #\{)
    (terror 'do-nocite "missing {"))
  (loop
    (let ((key (get-csv)))
      (unless key (return))
      (write-bib-aux "\\citation{")
      (write-bib-aux key)
      (write-bib-aux "}")
      (label-bound-p (concatenate 'string "cite{" key "}"))
      (write-bib-aux #\newline)))
  (unless (char= (get-actual-char) #\})
    (terror 'do-nocite "missing }")))

(defun do-bibliographystyle ()
  (let ((s (ungroup (get-token))))
    (write-bib-aux "\\bibstyle{")
    (write-bib-aux s)
    (write-bib-aux "}")
    (write-bib-aux #\Newline)))

(defun do-bibliography ()
  (setq *using-bibliography-p* t)
  (let ((bibdata (ungroup (get-token)))
        (bbl-file
         (concatenate 'string *aux-dir/* *jobname* *bib-aux-file-suffix*
                      ".bbl")))
    (write-bib-aux "\\bibdata{")
    (write-bib-aux bibdata)
    (write-bib-aux "}")
    (write-bib-aux #\Newline)
    (cond ((probe-file bbl-file) (setq *bibitem-num* 0) (tex2page-file bbl-file)
           (emit-newline))
          (t (flag-missing-piece :bibliography)
             (non-fatal-error "Bibliography not generated; rerun TeX2page")))))

(defun do-thebibliography ()
  (get-group)
  (when (eq *tex-format* :latex)
    (tex2page-string
     (if *using-chapters-p* "\\chapter*{\\bibname}" "\\section*{\\refname}")))
  (bgroup)
  (setq *bibitem-num* 0)
  (tex2page-string "\\let\\em\\it")
  (tex2page-string "\\def\\newblock{ }")
  (do-end-para)
  (emit "<table>")
  (emit-newline))

(defun do-bibitem ()
  (let ((bibmark (get-bracketed-text-if-any)))
    (do-end-para)
    (unless (= *bibitem-num* 0)
      (emit "</td></tr>") (emit-newline))
    (incf *bibitem-num*)
    (emit "<tr><td align=right valign=top>")
    (let* ((bibitem-num-s (write-to-string *bibitem-num*))
           ;wrapping bibitem with cite{...} so label can be used as a regular
           ;non-bib-related label also
           (key (concatenate 'string "cite{" (get-peeled-group) "}"))
           (node-name (concatenate 'string *html-node-prefix* "bib_" bibitem-num-s)))
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (unless bibmark (setq bibmark bibitem-num-s))
      (tex-def-0arg "\\@currentlabel" bibmark)
      (emit-anchor node-name)
      (emit "[")
      (tex2page-string bibmark)
      (emit "]")
      (emit-nbsp 2)
      (do-label-aux key)
      (emit "</td><td>"))))

(defun display-index-entry (s o)
  (mapc (lambda (c) (princ (if (or (char= c #\Newline)) #\space c) o))
        (concatenate 'list s)))

(defun do-index ()
  (let ((idx-entry (ungroup (get-group))))
    (ignorespaces) ;?
    (unless (search "|)" idx-entry)
      (incf *index-count* 2)
      ;
      ;increment by 2 rather than 1, effectively disabling makeindex
      ;from creating ranges, which are meaningless for HTML.  Actually,
      ;makeindex's -r option prevents ranging but who remembers these
      ;things?
      ;
      (!index *index-count* *html-page-count*)
      (write-aux `(!index ,*index-count* ,*html-page-count*))
      (let ((tag (format nil "~aidx_~a" *html-node-prefix* *index-count*)))
        (emit-anchor tag)
        (unless *index-port*
          (let ((idx-file (concatenate 'string *aux-dir/* *jobname*
                                       *index-file-suffix* ".idx")))
            (setq *index-port* (open idx-file :direction :output
                                     :if-exists :supersede))))
        (princ "\\indexentry{" *index-port*)
        (cond ((or (search "|see{" idx-entry)
                   (search "|seealso{" idx-entry))
               (display-index-entry idx-entry *index-port*))
              ((setq *it* (search "|(" idx-entry))
               (let ((i *it*))
                 (display-index-entry (subseq idx-entry 0 i) *index-port*)
                 (princ "|expandhtmlindex" *index-port*)))
              (t (display-index-entry idx-entry *index-port*)
                 (princ "|expandhtmlindex" *index-port*)))
        (princ "}{" *index-port*)
        (princ *index-count* *index-port*)
        (princ "}" *index-port*)
        (terpri *index-port*)))))

(defun do-inputindex (&optional insert-heading-p)
  (setq *using-index-p* t)
  (when insert-heading-p
    (tex2page-string
     (if *using-chapters-p* "\\chapter*{\\indexname}"
         "\\section*{\\indexname}"))
    (emit-newline))
  (emit-anchor (concatenate 'string *html-node-prefix* "index_start"))
  (!index-page *html-page-count*)
  (write-aux `(!index-page ,*html-page-count*))
  (let ((ind-file
         (concatenate 'string *aux-dir/* *jobname* *index-file-suffix* ".ind")))
    (cond ((probe-file ind-file) (tex2page-file ind-file))
          (t (flag-missing-piece :index)
           (non-fatal-error "Index not generated; rerun TeX2page")))))

(defun do-theindex ()
  (bgroup)
  (tex2page-string "\\let\\endtheindex\\egroup")
  (tex2page-string "\\let\\indexspace\\medskip")
  (tex2page-string "\\let\\item\\indexitem")
  (tex2page-string "\\let\\subitem\\indexsubitem")
  (tex2page-string "\\let\\subsubitem\\indexsubsubitem")
  (tex2page-string "\\let\\(\\expandhtmlindex"))

(defun expand-html-index ()
  (let* ((s (get-peeled-group))
         (n (read-from-string s))
         (pageno (gethash n *index-table*)))
    (emit-page-node-link-start
      pageno (concatenate 'string *html-node-prefix* "idx_" s))
    (emit pageno)
    (cond ((setq *it* (gethash pageno *index-page-mention-alist*))
           (let ((n (1+ *it*)))
             (emit (number-to-roman n))
             (setf (gethash pageno *index-page-mention-alist*) n)))
          (t (setf (gethash pageno *index-page-mention-alist*) 1)))
    (emit-link-stop)))

(defun do-see-also ()
  (let ((other-entry (get-group)))
    (get-group) ;ignore
    (emit "<em>see also</em> ")
    (tex2page-string other-entry)))

(defun do-indexitem (indent)
  (setq *index-page-mention-alist* (make-hash-table))
  (emit "<br>")
  (emit-newline)
  (emit-nbsp (* indent 4)))

(defun do-description-item ()
  (do-end-para)
  (emit "</dd><dt>")
  (let ((thing (get-bracketed-text-if-any)))
    (when thing
      (setq thing (string-trim-blanks thing))
      (unless (string= thing "")
        (bgroup)
        (emit "<b>")
        (tex2page-string thing)
        (emit "</b>")
        (egroup))))
  (emit "</dt><dd>"))

(defun do-regular-item ()
  (do-end-para)
  (emit "<li>")
  (do-para)
  (let ((thing (get-bracketed-text-if-any)))
    (when thing
      (emit "<b>")
      (bgroup)
      (tex2page-string thing)
      (egroup)
      (emit "</b>")
      (emit-nbsp 2))))

(defun do-plain-item (n)
  (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
    (do-end-para)
    (emit "<table><tr><td width=")
    (emit parindent)
    (emit " valign=top align=right>")
    (dotimes (i (1- n))
      (emit "</td><td width=")
      (emit parindent)
      (emit " valign=top align=right>"))
    (tex2page-string (get-group))
    (emit-nbsp 2)
    (emit "</td><td>")
    (do-noindent)
    (add-afterpar (lambda () (emit "</td></tr></table>")))))

(defun do-textindent ()
  (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
    (do-noindent)
    (emit "<span style=\"margin-left: ")
    (emit parindent)
    (emit "pt\">&zwnj;</span>")
    (emit "<span style=\"position: relative\">&zwnj;")
    (emit "<span style=\"position: absolute; left: -")
    (emit parindent)
    (emit "pt\">")
    (tex2page-string (get-group))
    (ignorespaces)
    (emit-nbsp 2)
    (emit "</span></span>")))

(defun do-item ()
  (case (car *tabular-stack*)
    (:description (do-description-item))
    ((:itemize :enumerate) (do-regular-item))
    (t (do-plain-item 1))))

(defun do-bigskip (type)
  (do-end-para)
  (emit "<div class=")
  (emit (case type (:medskip "medskip") (:bigskip "bigskip") (t "smallskip")))
  (emit "></div>")
  (emit-newline)
  (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
  (setq *in-para-p* t)
  (emit-newline))

(defun do-hspace ()
  (ignorespaces)
  (if (eql (snoop-actual-char) #\*) (get-actual-char))
  (get-group)
  (emit-nbsp 3))

(defun do-vspace ()
  (ignorespaces)
  (if (eql (snoop-actual-char) #\*) (get-actual-char))
  (get-group)
  (do-bigskip :vspace))

(defun do-htmlmathstyle ()
  (call-with-input-string/buffered (ungroup (get-group))
   (lambda ()
     (loop
       (ignore-all-whitespace)
       (let ((c (snoop-actual-char)))
         (when (eq c :eof-object) (return))
         (case (intern (string-upcase (scm-get-token)) :keyword)
           ((:image :display-image)
            (tex-def-0arg "\\TZPmathimage" "1"))
           ((:no-image :no-display-image)
            (tex-def-0arg "\\TZPmathimage" "0"))))))))

(defun do-htmldoctype ()
  (let ((d (get-peeled-group)))
    (when (string= d "") (setq d 'none))
    (write-aux `(!doctype ,d))))

(defun do-htmlcolophon ()
  (call-with-input-string/buffered
   (ungroup (get-group))
   (lambda ()
     (loop
       (ignore-all-whitespace)
       (let ((c (snoop-actual-char)))
         (when (eq c :eof-object) (return))
         (let ((directive (intern (string-upcase (scm-get-token)) :keyword)))
           (!colophon directive)
           (write-aux `(!colophon ,directive))))))))

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

(defun output-colophon ()
  (let ((colophon-mentions-last-mod-time-p
         (tex2page-flag-boolean "\\TZPcolophontimestamp"))
        (colophon-mentions-tex2page-p
         (tex2page-flag-boolean "\\TZPcolophoncredit"))
        (colophon-links-to-tex2page-website-p
         (tex2page-flag-boolean "\\TZPcolophonweblink")))
    (when (or colophon-mentions-last-mod-time-p colophon-mentions-tex2page-p)
      (do-end-para)
      (emit "<div align=right class=colophon>")
      (when (and colophon-mentions-last-mod-time-p *last-modification-time*
               (> *last-modification-time* 0))
        (tex2page-string *last-modified*)
        (emit ": ")
        (emit (seconds-to-human-time *last-modification-time*))
        (emit "<br>"))
      (when colophon-mentions-tex2page-p
        (emit "<div align=right class=advertisement>")
        (tex2page-string *html-conversion-by*)
        (emit " ")
        (when colophon-links-to-tex2page-website-p
          (emit-link-start
           "http://www.ccs.neu.edu/~dorai/tex2page/index.html"))
        (emit *tex-logo*) (emit "2page ")
        (emit *tex2page-version*)
        (when colophon-links-to-tex2page-website-p (emit-link-stop))
        (emit "</div>"))
      (emit "</div>")
      (emit-newline))))

(defun point-to-adjacent-pages ()
  (let* ((prev-page
          (cond ((= *html-page-count* 0) nil)
                ((= *html-page-count* 1)
                 (concatenate 'string *jobname* *output-extension*))
                (t
                 (concatenate 'string *jobname* *html-page-suffix*
                   (write-to-string (1- *html-page-count*))
                   *output-extension*))))
         (next-page
          (cond ((= *html-page-count* *last-page-number*) nil)
                (t
                 (concatenate 'string *jobname* *html-page-suffix*
                   (write-to-string (1+ *html-page-count*))
                   *output-extension*)))))
    (unless (= *last-page-number* 0)
      (when prev-page (emit-link-start prev-page))
      (emit "&lt;&middot;&middot;&middot;Prev ")
      (when prev-page (emit-link-stop))
      (emit "||")
      (when next-page (emit-link-start next-page))
      (emit " Next&middot;&middot;&middot;&gt;")
      (when next-page (emit-link-stop)))))

(defun output-head-or-foot-line (head-or-foot)
  (emit "<div align=right class=navigation>")
  (cond ((or *tex-like-layout-p*
             (and (eq head-or-foot :foot)
                  (tex2page-flag-boolean "\\TZPtexlayout")))
         (bgroup)
         (tex-let "\\folio" "\\TIIPfolio" nil)
         (tex2page-string
          (if (eq head-or-foot :head) "\\the\\headline" "\\the\\footline"))
         (egroup))
        (t (output-navigation-bar head-or-foot)))
  (emit "</div>")
  (emit-newline))

(defun output-navigation-bar (head-or-foot)
  (let* ((first-page-p (= *html-page-count* 0))
         (last-page-not-determined-p (< *last-page-number* 0))
         (last-page-p (= *html-page-count* *last-page-number*))
         (toc-page-p (and *toc-page* (= *html-page-count* *toc-page*)))
         (index-page-p
          (and *index-page* (= *html-page-count* *index-page*)))
         (first-page
          (concatenate 'string *jobname* *output-extension*))
         (prev-page
          (cond (first-page-p nil)
                ((= *html-page-count* 1) first-page)
                (t (concatenate 'string *jobname* *html-page-suffix*
                     (write-to-string
                      (1- *html-page-count*))
                     *output-extension*))))
         (next-page
          (cond (last-page-p nil)
                (t (concatenate 'string *jobname* *html-page-suffix*
                     (write-to-string (1+ *html-page-count*))
                     *output-extension*)))))
    (unless (and first-page-p
                 (or last-page-p
                     (and (eq head-or-foot :head)
                          last-page-not-determined-p)))
      (emit "[")
      (emit *navigation-sentence-begin*)
      (emit "<span")
      (when first-page-p (emit " class=disable"))
      (emit ">")
      (unless first-page-p (emit-link-start first-page))
      (emit *navigation-first-name*)
      (unless first-page-p (emit-link-stop))
      (emit ", ")
      (unless first-page-p (emit-link-start prev-page))
      (emit *navigation-previous-name*)
      (unless first-page-p (emit-link-stop))
      (emit "</span>")
      (emit "<span")
      (when last-page-p (emit " class=disable"))
      (emit ">")
      (when first-page-p (emit "<span class=disable>"))
      (emit ", ")
      (when first-page-p (emit "</span>"))
      (unless last-page-p (emit-link-start next-page))
      (emit *navigation-next-name*)
      (unless last-page-p (emit-link-stop))
      (emit "</span>")
      (emit *navigation-page-name*)
      (when (or *toc-page* *index-page*)
        (emit "<span")
        (when (or (and toc-page-p (not *index-page*) (not index-page-p))
                  (and index-page-p (not *toc-page*) (not toc-page-p)))
          (emit " class=disable"))
        (emit ">; ")
        (emit-nbsp 2)
        (emit "</span>")
        (when *toc-page*
          (emit "<span")
          (when toc-page-p (emit " class=disable"))
          (emit ">")
          (unless toc-page-p
            (emit-page-node-link-start *toc-page*
                                       (concatenate 'string *html-node-prefix*
                                         "toc_start")))
          (emit *navigation-contents-name*)
          (unless toc-page-p (emit-link-stop))
          (emit "</span>"))
        (when *index-page*
          (emit "<span")
          (when index-page-p (emit " class=disable"))
          (emit ">")
          (emit "<span")
          (unless (and *toc-page* (not toc-page-p))
            (emit " class=disable"))
          (emit ">")
          (when *toc-page* (emit "; ") (emit-nbsp 2))
          (emit "</span>")
          (unless index-page-p
            (emit-page-node-link-start *index-page*
                                       (concatenate 'string *html-node-prefix*
                                         "index_start")))
          (emit *navigation-index-name*)
          (unless index-page-p (emit-link-stop))
          (emit "</span>")))
      (emit *navigation-sentence-end*)
      (emit "]"))))

(defun do-eject ()
  ;kludge: don't start a new page if \eject is the last thing in the
  ;main file.  This is mostly to placate story.tex, which although
  ;horrid as an example file, happens to be viewed as canonical by
  ;everyone looking at TeX
  (unless (and (eq (snoop-actual-char) :eof-object)
               (eql *current-source-file* *main-tex-file*))
    (unless (> *last-page-number* 0)
      (flag-missing-piece :last-modification-time))
    (do-end-page)
    (incf *html-page-count*)
    (setq *html-page*
          (concatenate 'string *aux-dir/* *jobname* *html-page-suffix*
            (write-to-string *html-page-count*)
            *output-extension*))
    (setq *html* (open *html-page* :direction :output
                       :if-exists :supersede))
    (do-start)))

(defun output-html-preamble ()
  (when (stringp *doctype*)
    (emit "<!DOCTYPE ")
    (emit *doctype*)
    (emit ">")
    (emit-newline))
  (emit "<html>")
  (emit-newline)
  (emit "<!--")
  (emit-newline)
  (emit "Generated from ")
  (emit *main-tex-file*)
  (emit " by tex2page, ")
  (emit "v. ")
  (emit *tex2page-version*)
  (emit-newline)
  (emit *tex2page-copyright-notice*)
  (emit-newline)
  (emit "(running on ")
  (emit *common-lisp-version*)
  (emit ")")
  (emit-newline)
  (emit *tex2page-website*)
  (emit-newline)
  (emit "-->")
  (emit-newline)
  (emit "<head>")
  (emit-newline)
  (emit "<meta charset=\"utf-8\">")
  (emit-newline)
  (output-external-title)
  (link-stylesheets)
  (emit "<meta name=robots content=\"index,follow\">")
  (emit-newline)
  (mapc #'emit *html-head*)
  (emit "</head>")
  (emit-newline)
  (emit "<body>")
  (emit-newline)
  (emit "<div id=")
  (emit (if (and (= *html-page-count* 0) *title*) "slidetitle" "slidecontent"))
  (emit ">")
  (emit-newline))

(defun output-html-postamble ()
  (do-end-para)
  (emit "</div>")
  (emit-newline)
  (emit "</body>")
  (emit-newline)
  (emit "</html>")
  (emit-newline))

(defun redirect-if-needed ()
  (when *redirect-url*
      (emit "If not redirected in ")
      (emit *redirect-delay*)
      (emit " sec, go to ")
      (emit-link-start (fully-qualify-url *redirect-url*))
      (emit *redirect-url*)
      (emit-link-stop)))

(defun do-start ()
  (setq *footnote-list* '())
  (output-html-preamble)
  (output-head-or-foot-line :head)
  (do-para)
  (redirect-if-needed))

(defun do-end-page ()
  (do-end-para)
  (output-footnotes)
  (do-bigskip :smallskip)
  (output-head-or-foot-line :foot)
  (do-para)
  (let ((colophon-on-last-page-p
         (tex2page-flag-boolean "\\TZPcolophonlastpage")))
    (when (or (and (not colophon-on-last-page-p) (= *html-page-count* 0))
              (and colophon-on-last-page-p
                   (= *html-page-count* *last-page-number*)))
      (output-colophon)))
  (output-html-postamble)
  (write-log #\[)
  (write-log *html-page-count*)
  (write-log #\])
  (write-log :separation-space)
  (close *html*))

(defun close-all-open-ports ()
  (when *aux-port* (close *aux-port*))
  (when *css-port* (close *css-port*))
  (when *index-port* (close *index-port*))
  (when *label-port* (close *label-port*))
  (when *bib-aux-port* (close *bib-aux-port*))
  (when *verb-port* (close *verb-port*))
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (unless (eq v :free) (close v))) *input-streams*)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (unless (eq v :free) (close v))) *output-streams*))

(defun output-stats ()
  (write-log :separation-newline)
  (cond (*main-tex-file*
         (write-log "Output written on ")
         (write-log *aux-dir/*)
         (write-log *jobname*)
         (write-log *output-extension*)
         (when (> *html-page-count* 0) (write-log ", ..."))
         (write-log " (")
         (write-log (1+ *html-page-count*))
         (write-log " page")
         (unless (= *html-page-count* 0) (write-log #\s))
         (when (> *img-file-tally* 0)
           (write-log ", ")
           (write-log *img-file-tally*)
           (write-log " image")
           (unless (= *img-file-tally* 1) (write-log #\s)))
         (write-log ")."))
        (t (write-log "No pages of output.")))
  (write-log #\Newline)
  (when *log-port* (close *log-port*))
  (format t "Transcript written on ~a.~%" *log-file*))

(defun do-bye ()
  (note-down-tex2page-flags)
  (unless (null *tex-if-stack*)
    (let ((n (length *tex-if-stack*)))
      (trace-if t "(\\end occurred when " n " \\if"
                (if (> n 1) "s were" " was") " incomplete)")))
  (unless (null *tex-env*)
    (trace-if t "\\end occurred inside a group at level "
              (length *tex-env*)))
  (perform-postludes)
  (unless (or (>= *last-page-number* 0) (= *html-page-count* 0))
    (flag-missing-piece :last-page))
  (!last-page-number *html-page-count*)
  (write-aux `(!last-page-number ,*last-page-number*))
  (do-end-page)
  (when *last-modification-time*
    (write-aux `(!last-modification-time ,*last-modification-time* 1900)))
  (mapc (lambda (th) (funcall th)) *afterbye*)
  ;(note-down-tex2page-flags)
  (close-all-open-ports)
  (call-external-programs-if-necessary)
  (show-unresolved-xrefs-and-missing-pieces))

(defun note-down-tex2page-flags ()
  (write-aux `(!head-line ,(get-toks "\\headline")))
  (write-aux `(!foot-line ,(get-toks "\\footline")))
  (when (setq *it* (find-def "\\TZPtitle"))
    (let ((d *it*))
      (write-aux
       `(!preferred-title
         ,(tex-string-to-html-string (tdef*-expansion d))))))
  (when (tex2page-flag-boolean "\\TZPcolophonlastpage")
    (write-aux `(!colophon :last-page)))
  (unless (tex2page-flag-boolean "\\TZPcolophontimestamp")
    (write-aux `(!colophon :no-timestamp)))
  (unless (tex2page-flag-boolean "\\TZPcolophoncredit")
    (write-aux `(!colophon :dont-credit-tex2page)))
  (unless (tex2page-flag-boolean "\\TZPcolophonweblink")
    (write-aux `(!colophon :dont-link-to-tex2page-website)))
  (when (setq *it* (ctl-seq-no-arg-expand-once "\\TZPredirect"))
    (unless *redirect-url*
      (flag-missing-piece :html-head))
    (let ((url *it*)
          (seconds (ctl-seq-no-arg-expand-once "\\TZPredirectseconds")))
      (write-aux
        `(!html-redirect ,url ,seconds))))
  (when (tex2page-flag-boolean "\\TZPslides")
    (tex2page-file (actual-tex-filename "t2pslides")))
  (when (tex2page-flag-boolean "\\TZPtexlayout")
    (write-aux '(!tex-like-layout))
    (terpri *css-port*)
    (princ "body { margin-top: " *css-port*)
    (princ (sp-to-ems (+ (tex-length 0.5 :in) ; 1in is too much!
                         (find-dimen "\\voffset")))
           *css-port*)
    (princ "em; }" *css-port*)
    (terpri *css-port*)
    (princ "body { margin-left: " *css-port*)
    (princ (sp-to-ems (+ (tex-length 0.8 :in) (find-dimen "\\hoffset")))
           *css-port*)
    (princ "em; }" *css-port*)
    (terpri *css-port*)
    (unless (tex2page-flag-boolean "\\TZPraggedright")
      (princ "body { text-align: justify; }" *css-port*)
      (terpri *css-port*))
    (princ "p { margin-bottom: 0pt; }" *css-port*)
    (terpri *css-port*)
    (princ "p { text-indent: " *css-port*)
    (princ (sp-to-pixels (find-dimen "\\parindent")) *css-port*)
    (princ "pt; }" *css-port*)
    (terpri *css-port*)
    (princ "p { margin-top: " *css-port*)
    (princ (sp-to-pixels (find-dimen "\\parskip")) *css-port*)
    (princ "pt; }" *css-port*)
    (terpri *css-port*)
    (princ ".mathdisplay { margin-top: " *css-port*)
    (princ (sp-to-pixels (find-dimen "\\abovedisplayskip")) *css-port*)
    (princ "pt; margin-bottom: " *css-port*)
    (princ (sp-to-pixels (find-dimen "\\belowdisplayskip")) *css-port*)
    (princ "pt; }" *css-port*)
    (terpri *css-port*)
    (princ "body { max-width: " *css-port*)
    (princ (sp-to-pixels (find-dimen "\\hsize")) *css-port*)
    (princ "pt; }" *css-port*)
    (terpri *css-port*)
    (princ ".navigation { color: black; font-style: normal; }" *css-port*)
    (terpri *css-port*))
  (unless (tex2page-flag-boolean "\\TZPtextext")
    (write-aux `(!tex-text 0)))
  )

(defun insert-missing-end ()
  (write-log :separation-newline)
  (write-log "! Missing \\end inserted.")
  (write-log :separation-newline))

(let ((top-diacritics
       '(:grave :acute :circumflex :umlaut
         :tilde :macron :breve :hacek
         :hungarianumlaut :ring)))
  (defun do-diacritic (diac)
    (let ((x (get-token-or-peeled-group)))
      (cond ((and (string= x "\\i") (member diac top-diacritics))
             (emit "i"))
            ((and (string= x "\\j") (member diac top-diacritics))
             (emit "j"))
            (t (tex2page-string x))))
    (emit
     (ecase diac
       (:grave "&#x300;")
       (:acute "&#x301;")
       (:circumflex "&#x302;")
       (:umlaut "&#x308;")
       (:tilde "&#x303;")
       (:macron "&#x304;")
       (:dot "&#x307;")
       (:breve "&#x306;")
       (:hacek "&#x30c;")
       (:hungarianumlaut "&#x30b;")
       (:cedilla "&#x327;")
       (:dotunder "&#x323;")
       (:barunder "&#x331;")
       (:tieafter "&#x361;")
       (:ring "&#x30a;")
       (:ogonek "&#x328;")))))

(defun do-mathdg ()
  (let ((*math-mode-p* t)
        (*in-display-math-p* t)
        (*tabular-stack* '())
        (*ligatures-p* nil))
    (do-end-para)
    (emit "<div align=")
    (emit *display-justification*)
    (emit "><table><tr><td>")
    (tex2page-string (get-group))
    (emit "</td></tr></table></div>")
    (do-para)))

(defun do-mathg ()
  (let ((*math-mode-p* t)
        (*in-display-math-p* nil)
        (*tabular-stack* '())
        (*ligatures-p* nil))
    (tex2page-string (get-group))))

(defun dump-tex-preamble (o)
  (case *tex-format*
    ((:latex)
     (princ "\\documentclass{" o)
     (princ (if *using-chapters-p* "report" "article") o)
     (princ "}" o)
     (terpri o)
     (princ *imgpreamble* o)
     ;(terpri o)
     ;(princ "\\ifx\\bmatrix\\UNDEFINED" o)
     ;(princ "\\usepackage{amsmath}\\fi" o)
     (terpri o)
     (when (member :includegraphics *imgpreamble-inferred*)
       (princ "\\ifx\\includegraphics\\UNDEFINED" o)
       (princ "\\usepackage{graphicx}\\fi" o)
       (terpri o))
     (when (member :epsfbox *imgpreamble-inferred*)
       (princ "\\ifx\\epsfbox\\UNDEFINED" o)
       (princ "\\usepackage{epsfig}\\fi" o)
       (terpri o))
     (princ "\\thispagestyle{empty}" o)
     (terpri o)
     (princ "\\begin{document}" o)
     (terpri o))
    (t
     (princ *imgpreamble* o)
     (terpri o)
     (when (member :includegraphics *imgpreamble-inferred*)
       (princ "\\ifx\\resetatcatcode\\UNDEFINED" o)
       (princ "\\input miniltx \\fi" o)
       (terpri o)
       (princ "\\ifx\\includegraphics\\UNDEFINED" o)
       (princ "\\input graphicx.sty \\fi" o)
       (terpri o))
     (when (member :epsfbox *imgpreamble-inferred*)
       (princ "\\ifx\\epsfbox\\UNDEFINED" o)
       (princ "\\ifx\\pdfoutput\\UNDEFINED\\input epsf \\else" o)
       (princ "\\input supp-pdf " o)
       (princ "\\def\\epsfbox#1{\\convertMPtoPDF{#1}{1}{1}}\\fi" o)
       (terpri o))
     (princ "\\nopagenumbers" o)
     (terpri o))))

(defun dump-tex-postamble (o)
  (case *tex-format*
    ((:latex) (princ "\\end{document}" o) (terpri o))
    (t (princ "\\bye" o) (terpri o))))

(defun skipping-img-file ()
  (incf *img-file-count*))

(defun next-html-image-file-stem ()
  (incf *img-file-count*)
  (concatenate 'string *subjobname* *img-file-suffix*
               (write-to-string *img-file-count*)))

(defun call-with-html-image-port (p &optional alt)
  (let* ((img-file-stem (next-html-image-file-stem))
         (aux-tex-file (concatenate 'string img-file-stem ".tex")))
    (with-open-file (o aux-tex-file :direction :output
                       :if-exists :supersede)
      (dump-tex-preamble o)
      (funcall p o)
      (dump-tex-postamble o))
    (tex-to-img img-file-stem)
    (source-img-file img-file-stem alt)))

;(defun tex2page-math-string (s &optional display-p)
;  (call-with-input-string/buffered s
;    (lambda ()
;      (do-math-fragment s display-p)
;      (generate-html))))

(defun do-math-fragment (s &optional display-p)
  (when display-p
    (emit "<div class=mathdisplay align=")
    (emit *display-justification*)
    (emit ">"))
  (let ((old-math-mode-p *math-mode-p*)
        (old-in-display-math-p *in-display-math-p*)
        (old-tabular-stack *tabular-stack*))
    (setq *math-mode-p* t
          *in-display-math-p* display-p
          *tabular-stack* '())
    (when display-p (emit "<table><tr><td>"))
    (bgroup)
    (toss-back-char #\})
    (toss-back-string s)
    (add-aftergroup-to-top-frame
     (lambda ()
       (setq *math-mode-p* old-math-mode-p
             *in-display-math-p* old-in-display-math-p
             *tabular-stack* old-tabular-stack)
       (when display-p (emit "</td></tr></table>"))
       (when display-p
         (emit "</div>")
         (do-noindent))))))

(defun do-display-math (tex-string)
  (do-end-para)
  (if (and (tex2page-flag-boolean "\\TZPmathimage")
           (not *temporarily-use-utf8-for-math-p*))
      (progn
       (emit "<div class=mathdisplay align=")
       (emit *display-justification*)
       (emit ">")
       (call-with-html-image-port
        (lambda (o) (princ "$$" o) (princ tex-string o) (princ "$$" o))
        tex-string)
       (emit "</div>")
       (do-noindent))
    (do-math-fragment tex-string :display)))

(defun tex-math-delim-string (type)
  (let ((top nil) (mid nil) (bot nil) (ext nil))
    (ecase type
      (:lparen (setq top "&#x239b;" bot "&#x239d;" ext "&#x239c;" mid ext))
      (:lbrack (setq top "&#x23a1;" bot "&#x23a3;" ext "&#x23a2;" mid ext))
      (:lbrace (setq top "&#x23a7;" mid "&#x23a8;" bot "&#x23a9;" ext "&#x23aa;"))
      (:lvert (setq ext "&#x239c;" top ext mid ext bot ext))
      (:rparen (setq top "&#x239e;" bot "&#x23a0;" ext "&#x239f;" mid ext))
      (:rbrack (setq top "&#x23a4;" bot "&#x23a6;" ext "&#x23a5;" mid ext))
      (:rbrace (setq top "&#x23ab;" mid "&#x23ac;" bot "&#x23ad;" ext "&#x23ae;"))
      (:rvert (setq ext "&#x239f;" top ext mid ext bot ext)))
    (concatenate 'string
      "<table cellpadding=0 cellspacing=0><tr><td>" top "</td></tr>"
      (cond ((oddp *math-height*)
             (concatenate 'string
               (let ((r ""))
                 (dotimes (i (/ (1- *math-height*) 2) r)
                   (setq r (concatenate 'string r
                             "<tr><td>"
                             ext
                             "</td></tr>"))))
               "<tr><td>"
               mid
               "</td></tr>"
               (let ((r ""))
                 (dotimes (i (/ (1- *math-height*) 2) r)
                   (setq r (concatenate 'string r
                             "<tr><td>"
                             ext
                             "</td></tr>"))))))
            (t (let ((r ""))
                 (dotimes (i *math-height* r)
                   (setq r (concatenate 'string r
                             "<tr><td>"
                             ext
                             "</td></tr>"))))))
      "<tr><td>" bot "</td></tr></table>")))

(defun tex-math-string-to-html-string (s)
  (let* ((tmp-port (make-string-output-stream))
         (*html* tmp-port))
    (call-with-input-string/buffered ""
      (lambda ()
        (do-math-fragment s nil)
        (generate-html)))
    (get-output-stream-string tmp-port)))

(defun do-intext-math (tex-string)
  (let* ((*math-needs-image-p* nil)
         (html-string (tex-math-string-to-html-string tex-string)))
    (if (and (tex2page-flag-boolean "\\TZPmathimage")
             *math-needs-image-p*
             (not *temporarily-use-utf8-for-math-p*))
      (call-with-html-image-port
        (lambda (o) (princ #\$ o) (princ tex-string o) (princ #\$ o))
        tex-string)
      (emit html-string))))

(defun do-mathp ()
  (call-with-html-image-port
   (lambda (o) (princ #\$ o) (princ (get-group) o) (princ #\$ o))))

(defun do-latex-intext-math ()
  (do-intext-math
   (let ((o (make-string-output-stream)))
     (dump-till-ctl-seq "\\)" o)
     (get-output-stream-string o))))

(defun do-latex-display-math ()
  (do-display-math
   (let ((o (make-string-output-stream)))
     (dump-till-ctl-seq "\\]" o)
     (get-output-stream-string o))))

(defun do-math ()
  (let ((display-p nil))
    (when (eql (snoop-actual-char) #\$) (setq display-p t) (get-actual-char))
    (let ((o (make-string-output-stream)))
      (dump-till-char #\$ o)
      (when display-p
        (let ((c (get-actual-char)))
          (when (or (eq c :eof-object) (not (char= c #\$)))
            (terror 'do-math "Display math should end with $$."))))
      (funcall (if display-p #'do-display-math #'do-intext-math)
               (get-output-stream-string o)))))

(defun dump-till-char (d o)
  (let ((nesting 0) (escape-p nil))
    (loop
      (let ((c (get-actual-char)))
        (when (eq c :eof-object) (terror 'dump-till-char "Missing " d "."))
        (when (and (char= c d) (= nesting 0)) (return))
        (princ c o)
        (cond (escape-p (setq escape-p nil))
              ((char= c #\{) (incf nesting))
              ((char= c #\}) (decf nesting))
              ((char= c #\\) (setq escape-p t)))))))

(defun dump-till-ctl-seq (cs o)
  (let* ((*not-processing-p* t)
         (nesting 0))
    (loop
      (let ((c (snoop-actual-char)))
        (when (eq c :eof-object) (terror 'dump-till-ctl-seq))
        (cond ((char= c *esc-char*)
               (let ((x (get-ctl-seq)))
                 (if (string= x cs) (return)
                   (princ x o))))
              (t (princ (get-actual-char) o)
                 (cond ((char= c #\{) (incf nesting))
                       ((char= c #\}) (decf nesting)))))))))

(defun dump-till-end-env (env o)
  (let* ((endenv (concatenate 'string "\\end" env))
         (endenv-prim (find-corresp-prim endenv))
         (endenv-prim-th (find-corresp-prim-thunk endenv))
         (*not-processing-p* t)
         (brace-nesting 0)
         (env-nesting 0))
    (loop
      (let ((c (snoop-actual-char)))
        (when (eq c :eof-object) (terror 'dump-till-end-env env))
        (cond ((char= c *esc-char*)
               (let ((x (get-ctl-seq)))
                 (cond ((string= (find-corresp-prim x) endenv-prim) (return))
                       ((string= x "\\begin") (princ x o)
                        (let ((g (get-grouped-environment-name-if-any)))
                          (when g
                            (princ #\{ o)
                            (princ g o)
                            (princ #\} o))
                          (when (and g (string= g env)) (incf env-nesting))))
                       ((string= x "\\end")
                        (let ((g (get-grouped-environment-name-if-any)))
                          (when (and g
                                     (or *dumping-nontex-p* (= env-nesting 0))
                                     (let ((endg (concatenate 'string "\\end" g)))
                                       (or (string= (find-corresp-prim endg) endenv-prim)
                                           (eql (find-corresp-prim-thunk endg) endenv-prim-th))))
                            (return))
                          (princ x o)
                          (when g
                            (princ #\{ o)
                            (princ g o)
                            (princ #\} o))
                          (when (and g (string= g env)) (decf env-nesting))))
                       (t (princ x o)))))
              ((and (char= c *comment-char*)
                    (not *dumping-nontex-p*))
               (do-comment) (write-char #\% o) (terpri o))
              (t (write-char (get-actual-char) o)
                 (cond ((char= c #\{)
                        (incf brace-nesting))
                       ((char= c #\})
                        (decf brace-nesting)))))))))

(defun dump-imgdef (f)
  (let ((aux-tex-file (concatenate 'string f ".tex")))
    (with-open-file (o aux-tex-file :direction :output
                       :if-exists :supersede)
      (dump-tex-preamble o)
      (princ (ungroup (get-group)) o)
      (dump-tex-postamble o))))

(defun do-img-preamble ()
  (setq *imgpreamble*
        (let ((*not-processing-p* t)
              (r *imgpreamble*))
          (loop
            (let ((c (snoop-actual-char)))
              (when (eq c :eof-object)
                (terror 'do-img-preamble "Missing \\endimgpreamble"))
              (cond ((char= c *esc-char*)
                     (let ((x (get-ctl-seq)))
                       (when (member x '("\\endimgpreamble" "\\endgifpreamble" "\\endmathpreamble")
                                     :test #'string=)
                         (return r))
                       (setq r (concatenate 'string r x))))
                    (t (get-actual-char)
                       (setq r (concatenate 'string r (list c))))))))))

(defun pick-new-stream-number (stream-list)
  (let ((i 0))
    (loop
      (unless (or (gethash i stream-list)
                  (= i 16) ;reserved for console
                  (= i 18)) ;reserved for shell escape
        (return i))
      (incf i))))

(defun do-new-stream (type)
  (let* ((x (get-ctl-seq))
         (sl (if (eq type :out) *output-streams* *input-streams*))
         (n (pick-new-stream-number sl)))
    (tex-def-count x n t) ;streams always global?
    (setf (gethash n sl) :free)))

(defun do-open-stream (type)
  (let* ((n (get-number))
         (f (get-plain-filename))
         (sl (if (eq type :out) *output-streams* *input-streams*))
         (c (gethash n sl)))
    (unless (eq c :free) (terror 'do-open-stream))
    (setf (gethash n sl)
          (ecase type
            (:out
             (setq f (add-dot-tex-if-no-extension-provided f))
             (open f :direction :output :if-exists :supersede))
            (:in
             (setq f (actual-tex-filename f))
             (make-bport* :port (open f :direction :input)))))))

(defun do-close-stream (type)
  (let* ((sl (if (eql type :out) *output-streams* *input-streams*))
         (o (get-number))
         (c (gethash o sl)))
    (when (eq c :free) (terror 'do-close-stream))
    (close (ecase type
             (:out c)
             (:in (bport*-port c))))
    (setf (gethash o sl) :free)))

(defun tex-write-output-string (s)
  (let* ((o (make-string-output-stream))
         (*outputting-to-non-html-p* t)
         (*html* o))
    (call-with-input-string/buffered s
      (lambda ()
        (loop
          (let ((c (snoop-actual-char)))
            (when (eq c :eof-object) (return))
            (case c
              (#\\ (do-tex-ctl-seq (get-ctl-seq)))
              (t (emit-html-char (get-actual-char))))))))
    (get-output-stream-string o)))

(defun do-write-aux (o)
  (let ((output (tex-write-output-string (get-peeled-group))))
    (cond ((and (= o 18) *enable-write-18-p*)
           (system output))
          ((member o '(16 18) :test #'=)
           (write-log output)
           (write-log :separation-space))
          ((setq *it* (gethash o *output-streams*))
           (let ((p *it*))
             (when (eq p :free) (terror 'do-write-aux))
             (princ output p)
             (princ #\space p)))
          (t (terror 'do-write-aux)))))

(defun do-write ()
  (do-write-aux (get-number)))

(defun do-message ()
  (do-write-aux 16))

(defun read-tex-line (p)
  (let* ((*current-tex2page-input* p)
         (r '()))
    (loop
      (let ((c (snoop-actual-char)))
        (when (eq c :eof-object)
          (return
           (if (null r) c (concatenate 'string (nreverse r)))))
        (when (char= c #\Newline) (get-actual-char)
          (return (concatenate 'string (nreverse r))))
        (when (char= c #\{)
          (return
           (concatenate 'string (concatenate 'string (nreverse r))
             (get-group))))
        (push (get-actual-char) r)))))

(defun do-read (global-p)
  (let* ((i (get-number))
         (x (progn (get-to) (get-ctl-seq)))
         (p nil))
    (cond ((member i '(-1 16) :test #'=)
           (setq p (make-bport* :port *standard-input*))
           (unless (= i -1)
             (write-log x) (write-log #\=)))
          ((setq *it* (gethash i *input-streams*))
           (setq p *it*)
           (unless (eq p :free) (terror 'do-read)))
          (t (terror 'do-read)))
    (funcall (if global-p #'tex-gdef-0arg #'tex-def-0arg)
             x
             (let ((line (read-tex-line p)))
               (if (eq line :eof-object) "" line)))))

(defun do-typein ()
  (let ((ctlseq (get-bracketed-text-if-any))
        (p (make-bport* :port *standard-input*)))
    (write-log :separation-newline)
    (write-log (tex-string-to-html-string (get-group)))
    (write-log :separation-newline)
    (write-log (or ctlseq "\\@typein"))
    (write-log #\=)
    (let ((l (read-tex-line p)))
      (when (eq l :eof-object) (setq l ""))
      (cond (ctlseq (tex-def-0arg ctlseq l))
            (t (tex2page-string l))))))

(defun do-ifeof ()
  (let* ((i (get-number))
         (c (gethash i *input-streams*)))
    (when (eq c :free) (terror 'do-ifeof))
    (if (eq (read-char c nil :eof-object) :eof-object)
        (do-iftrue)
      (do-iffalse))))

(defun do-iffalse ()
  (push nil *tex-if-stack*))

(defun do-iftrue ()
  (push t *tex-if-stack*))

(defun insert-tex-if (test)
  (if test
      (do-iftrue)
    (do-iffalse)))

(defun do-ifx ()
  (let* ((one (get-raw-token/is))
         (two (get-raw-token/is))
         (one2 one)
         (two2 two))
    ;NB: doesn't work like tex's \ifx if one of the args is a
    ;"primitive" ctl seq
    (if (string= one two)
        (do-iftrue)
      (progn
       (when (ctl-seq-p one)
         (setq one2 (cond ((setq *it* (find-def one))
                           (let ((d *it*))
                             (or (tdef*-expansion d)
                                 (tdef*-prim d))))
                          ((setq *it* (find-math-def one))
                           *it*)
                          (t "UnDeFiNeD"))))
       (when (ctl-seq-p two)
         (setq two2 (cond ((setq *it* (find-def two))
                           (let ((d *it*))
                             (or (tdef*-expansion d)
                                 (tdef*-prim d))))
                          ((setq *it* (find-math-def two))
                           *it*)
                          (t "UnDeFiNeD"))))
       (if (or (eql one2 two2)
               (and (stringp one2) (stringp two2)
                    (string= one2 two2)))
           (do-iftrue)
         (do-iffalse))))))

(defun do-ifdefined ()
  (let ((x (get-raw-token/is)))
    (if (or (not (ctl-seq-p x))
            (and (ctl-seq-p x)
                 (or (find-def x) (find-math-def x))))
        (do-iftrue)
      (do-iffalse))))

(defun do-if-get-atomic ()
  (loop
    (let ((x (get-raw-token/is)))
      (if (ctl-seq-p x)
        (cond ((setq *it* (resolve-defs x))
               (let ((z *it*))
                 (toss-back-char *invisible-space*)
                 (toss-back-string z)))
              (t (return x)))
        (return x)))))

(defun do-if ()
  (let* ((one (do-if-get-atomic))
         (two (do-if-get-atomic)))
    (if (or (string= one two)
            (and (ctl-seq-p one) (ctl-seq-p two)))
      (do-iftrue)
      (do-iffalse))))

(defun do-ifmmode ()
  (push *math-mode-p* *tex-if-stack*))

(defun do-ifnum ()
  (let* ((one (get-number))
         (rel (char (get-raw-token/is) 0)))
    (if (funcall
         (case rel
           (#\< #'<)
           (#\= #'=)
           (#\> #'>)
           (t (terror 'do-ifnum "Missing = for \\ifnum.")))
         one (get-number))
        (do-iftrue)
      (do-iffalse))))

(defun read-ifcase-clauses ()
  (let* ((*not-processing-p* t)
         (else-clause nil)
         (or-clauses '())
         (elsep nil))
    (block outer-loop
      (loop
        (let ((clause ""))
          (loop
            (let ((c (snoop-actual-char)))
              (when (eq c :eof-object)
                (terror 'read-ifcase-clauses "Incomplete \\ifcase."))
              (cond ((char= c *esc-char*)
                     (let ((x (get-ctl-seq)))
                       (cond ((string= x "\\or") (ignorespaces)
                              (when elsep
                                (terror 'read-ifcase-clauses
                                                  "Extra \\or."))
                              (push clause or-clauses) (return))
                             ((string= x "\\else") (ignorespaces)
                              (when elsep
                                (terror 'read-ifcase-clauses
                                                  "Extra \\else."))
                              (push clause or-clauses) (setq elsep t) (return))
                             ((string= x "\\fi") (ignorespaces)
                              (cond (elsep (setq else-clause clause))
                                    (t (push clause or-clauses)))
                              (return-from outer-loop))
                             (t (setq clause (concatenate 'string clause x))))))
                    (t (get-actual-char)
                       (setq clause
                             (concatenate 'string clause (list c))))))))))
    (values (nreverse or-clauses) else-clause)))

(defun do-ifcase ()
  (let ((num (get-number)))
    (multiple-value-bind (or-clauses else-clause)
        (read-ifcase-clauses)
      (let ((chosen (or (nth num or-clauses) else-clause)))
        (when chosen
          (tex2page-string chosen))))))

(defun do-ifodd ()
  (if (oddp (get-number))
      (do-iftrue)
    (do-iffalse)))

(defun do-else ()
  (when (null *tex-if-stack*) (terror 'do-else "Extra \\else."))
  (let ((top-if (pop *tex-if-stack*)))
    (push (not top-if) *tex-if-stack*)))

(defun do-fi ()
  (when (null *tex-if-stack*) (terror 'do-fi "Extra \\fi."))
  (pop *tex-if-stack*))

(defun do-newif ()
  (let* ((iffoo (get-ctl-seq))
         (foo (concatenate 'string "\\" (subseq iffoo 3)))
         (foo-register (concatenate 'string foo "BOOLEANREGISTER")))
    (tex-def-count foo-register 0 nil)
    (tex-def-thunk iffoo
                   (lambda ()
                     (push (> (the-count foo-register) 0) *tex-if-stack*))
                   nil)
    (tex-def-thunk (concatenate 'string foo "true")
                   (lambda () (tex-def-count foo-register 1 nil)) nil)
    (tex-def-thunk (concatenate 'string foo "false")
                   (lambda () (tex-def-count foo-register 0 nil)) nil)))

(defun do-htmlimg (env)
  (call-with-html-image-port (lambda (o) (dump-till-end-env env o))))

(defun find-img-file-extn ()
  (case (tex2page-flag-value "\\TZPimageformat")
    ((#\p #\P) ".png")
    ((#\j #\J) ".jpeg")
    (t ".gif")))

(defun do-htmlimageformat ()
  (tex-def-0arg "\\TZPimageformat" (get-peeled-group)))

(defun do-htmlimageconversionprogram ()
  (tex-def-0arg "\\TZPimageconverter" (get-peeled-group)))

(defun do-htmlimgmagnification () t)

(let ((tex-prog-name nil)
      (tex-output-format nil))
  (defun call-tex (f)
    ;run tex on f and return f.ps or f.pdf if successful
    (unless tex-prog-name
      (let ((d (find-def "\\TZPtexprogname")))
        (when d (setq *tex-prog-name* (tdef*-expansion d))))
      (unless *tex-prog-name* (setq *tex-prog-name* "pdftex"))
      (setq tex-prog-name *tex-prog-name*)
      (setq tex-output-format
            (if (or (eql (search "pdf" *tex-prog-name*) 0)
                    (eql (search "xe" *tex-prog-name*) 0)
                    (eql (search "lua" *tex-prog-name*) 0))
              :pdf :dvi))
      (when (eq *tex-format* :latex)
        (setq tex-prog-name
              (concatenate 'string (subseq *tex-prog-name* 0
                                           (- (length *tex-prog-name*) 3))
                           "latex"))))
      (let* ((dvifile (concatenate 'string f
                                   (if (eq tex-output-format :pdf)
                                     ".pdf" ".dvi")))
             (outfile dvifile))
        (system (concatenate 'string
                             tex-prog-name
                             " " f))
        (when (probe-file dvifile)
          (let ((logfile (concatenate 'string f ".log")))
            (when (probe-file logfile)
              ;scan the log file for sign of problems
              (let ((fine-p
                      (with-open-file (i logfile :direction :input)
                        (loop
                          (let ((x (read-line i nil :eof-object)))
                            (when (eq x :eof-object) (return t))
                            (when (search "! I can't find file" x)
                              ;the output can't be good
                              (return nil)))))))
                (when fine-p
                  (unless (eq tex-output-format :pdf)
                    (let ((psfile (concatenate 'string f ".ps")))
                      (system
                        (concatenate 'string "dvips " dvifile " -o " psfile))
                      (setq outfile psfile)))
                  outfile))))))))

(defun ps-to-img/gif/netpbm (psfile f)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" f
                ".ppm.1 " psfile " quit.ps"))
  (system (concatenate 'string "pnmcrop " f ".ppm.1 > " f ".ppm.tmp"))
  (system (concatenate 'string "ppmquant 256 < " f ".ppm.tmp > " f ".ppm"))
  (system
   (concatenate 'string "ppmtogif -transparent rgb:ff/ff/ff < " f ".ppm > "
                *aux-dir/* f ".gif"))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string f e)))
        '(".ppm" ".ppm.tmp" ".ppm.1")))

(defun ps-to-img/png/netpbm (psfile f)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" f
                ".ppm.1 " psfile " quit.ps"))
  (system (concatenate 'string "pnmcrop " f ".ppm.1 > " f ".ppm.tmp"))
  '(system
    (concatenate 'string "ppmquant 256 < " f ".ppm.tmp > " f ".ppm"))
  (system
   (concatenate 'string "pnmtopng -interlace -transparent \"#FFFFFF\" " " < " f
                ".ppm.tmp > " *aux-dir/* f ".png"))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string f e)))
        '(".ppm.1" ".ppm.tmp" ".ppm")))

(defun ps-to-img/jpeg/netpbm (psfile f)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" f
                ".ppm.1 " psfile  " quit.ps"))
  (system (concatenate 'string "pnmcrop " f ".ppm.1 > " f ".ppm.tmp"))
  (system (concatenate 'string "ppmquant 256 < " f ".ppm.tmp > " f ".ppm"))
  (system
   (concatenate 'string "ppmtojpeg --grayscale < " f ".ppm > " *aux-dir/* f
                ".jpeg"))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string f e)))
        '(".ppm.1" ".ppm.tmp" ".ppm")))

(defun ps-to-img (psfile f)
  (case (tex2page-flag-value "\\TZPimageconverter")
    ((#\i #\I)
     (system
      (concatenate 'string "convert -transparent white -trim "
                   psfile  " " f
                   (find-img-file-extn))))
    (t (case (tex2page-flag-value "\\TZPimageformat")
         ((#\p #\P) (ps-to-img/png/netpbm psfile f))
         ((#\j #\J) (ps-to-img/jpeg/netpbm psfile f))
         (t (ps-to-img/gif/netpbm psfile f))))))

(defun tex-to-img (f)
  (incf *img-file-tally*)
  (let ((f.img (concatenate 'string *aux-dir/* f (find-img-file-extn))))
    (unless (probe-file f.img)
      (write-log :separation-space)
      (write-log #\{)
      (write-log (concatenate 'string f ".tex"))
      (write-log :separation-space)
      (write-log "->")
      (write-log :separation-space)
      (cond ((setq *it* (call-tex f))
             (ps-to-img *it* f) (write-log f.img)
             '(mapc
               (lambda (e)
                 (ensure-file-deleted (concatenate 'string f e)))
               '(".aux" ".dvi" ".log" ".pdf" ".ps" ".tex")))
            (t (write-log "failed, try manually")))
      (write-log #\})
      (write-log :separation-space))))

(defun call-with-lazy-image-port (eps-file img-file-stem p)
  (let ((aux-tex-file (concatenate 'string img-file-stem ".tex")))
    (with-open-file (o aux-tex-file :direction :output
                       :if-exists :supersede)
      (dump-tex-preamble o)
      (funcall p o)
      (dump-tex-postamble o))
    (if (probe-file eps-file) (tex-to-img img-file-stem)
      (push (cons eps-file img-file-stem) *missing-eps-files*))))

(defun retry-lazy-image (eps-file img-file-stem)
  (cond ((probe-file eps-file) (tex-to-img img-file-stem))
        (t (write-log "! I can't find EPS file ") (write-log eps-file)
           (write-log :separation-newline))))

(defun lazily-make-epsf-image-file (eps-file img-file-stem)
  (let ((*imgpreamble-inferred* (cons :epsfbox *imgpreamble-inferred*)))
    (call-with-lazy-image-port eps-file img-file-stem
      (lambda (o)
        (princ "\\epsfbox{" o)
        (princ eps-file o)
        (princ #\} o)))))

(defun do-epsfbox ()
  (get-bracketed-text-if-any)
  (let ((f (get-filename-possibly-braced)))
    (unless *eval-for-tex-only-p*
      (let ((epsf-x-size (get-dimen "\\epsfxsize"))
            (epsf-y-size (get-dimen "\\epsfysize")))
        (cond ((and (= epsf-x-size 0) (= epsf-y-size 0))
               (let ((img-file-stem (next-html-image-file-stem)))
                 (lazily-make-epsf-image-file f img-file-stem)
                 (source-img-file img-file-stem)))
              (t (unless (= epsf-x-size 0) (tex2page-string "\\epsfxsize=0pt"))
                 (unless (= epsf-y-size 0) (tex2page-string "\\epsfysize=0pt"))
                 (let ((*imgpreamble-inferred*
                        (cons :epsfbox *imgpreamble-inferred*)))
                   (call-with-html-image-port
                    (lambda (o)
                      (unless (= epsf-x-size 0)
                        (princ "\\epsfxsize=" o)
                        (princ epsf-x-size o)
                        (princ "sp" o)
                        (terpri o))
                      (unless (= epsf-y-size 0)
                        (princ "\\epsfysize=" o)
                        (princ epsf-y-size o)
                        (princ "sp" o)
                        (terpri o))
                      (princ "\\epsfbox{" o)
                      (princ f o)
                      (princ #\} o))))))))))

(defun do-epsfig ()
  (let ((*imgpreamble-inferred* (cons :epsfbox *imgpreamble-inferred*)))
    (call-with-html-image-port
     (lambda (o) (princ "\\epsfig{" o) (dump-groupoid o) (princ #\} o)))))

(defun do-convertmptopdf ()
  (let ((f (get-filename-possibly-braced))
        (img-file-stem (next-html-image-file-stem)))
    (get-token) ;ignore scale args for now
    (get-token)
    (lazily-make-epsf-image-file f img-file-stem)
    (source-img-file img-file-stem)))

(defun do-includegraphics-web (bracketed-text image-file)
  (emit "<img")
  (when bracketed-text
    (let ((height nil) (width nil))
      (toss-back-string " enoughalready ")
      (toss-back-string bracketed-text)
      (loop (cond ((eat-word "height") (get-equal-sign)
                   (setq height (get-pixels)))
                  ((eat-word "width") (get-equal-sign)
                   (setq width (get-pixels)))
                  ((eat-word "enoughalready") (ignorespaces) (return))
                  (t (get-actual-char))))
      (when height (emit " height=") (emit height))
      (when width (emit " width=") (emit width))))
  (emit " src=\"")
  (emit (fully-qualify-url image-file))
  (emit "\">"))

(defun do-includegraphics ()
  (let* ((starred-p (eat-star))
         (b1 (get-bracketed-text-if-any))
         (b2 (and b1 (get-bracketed-text-if-any)))
         (f (get-filename-possibly-braced))
         (img-file-stem (next-html-image-file-stem))
         (ffull (if (probe-file f) f
                  (some (lambda (e)
                          (let ((f2 (concatenate 'string f e)))
                            (and (probe-file f2) f2)))
                        *graphics-file-extensions*)))
         (ffull-ext (and ffull (file-extension ffull))))
    (cond ((and ffull-ext
                (member ffull-ext '(".jpg" ".jpeg" ".png") :test #'string=))
           (do-includegraphics-web b1 ffull))
          (t (let ((*imgpreamble-inferred*
                    (cons :includegraphics *imgpreamble-inferred*)))
               (call-with-lazy-image-port (or ffull f) img-file-stem
                 (lambda (o)
                   (princ "\\includegraphics" o)
                   (when starred-p (princ #\* o))
                   (when b1
                     (princ #\[ o)
                     (princ b1 o)
                     (princ #\] o))
                   (when b2
                     (princ #\[ o)
                     (princ b2 o)
                     (princ #\] o))
                   (princ #\{ o)
                   (princ f o)
                   (princ #\} o))))
             (source-img-file img-file-stem)))))

(defun do-resizebox ()
  (let* ((arg1 (get-group))
         (arg2 (get-group))
         (arg3 (get-group))
         (*imgpreamble-inferred*
          (cons :includegraphics *imgpreamble-inferred*)))
    (call-with-html-image-port
     (lambda (o)
       (princ "\\resizebox" o)
       (princ arg1 o)
       (princ arg2 o)
       (princ arg3 o)))))

(defun do-mfpic-opengraphsfile ()
  (setq *mfpic-file-stem* (get-filename-possibly-braced))
  (when *mfpic-port* (close *mfpic-port*))
  (let ((f (concatenate 'string *mfpic-file-stem* *mfpic-tex-file-suffix*)))
    (setq *mfpic-port* (open f :direction :output
                             :if-exists :supersede)))
  (setq *mfpic-file-num* 0)
  (princ "\\input mfpic \\usemetapost " *mfpic-port*)
  (terpri *mfpic-port*)
  (princ "\\opengraphsfile{" *mfpic-port*)
  (princ *mfpic-file-stem* *mfpic-port*)
  (princ #\} *mfpic-port*)
  (terpri *mfpic-port*)
  (tex-def-prim "\\headshape"
   (lambda ()
       (let* ((g1 (get-group))
              (g2 (get-group))
              (g3 (get-group)))
             (princ "\\headshape" *mfpic-port*)
             (princ g1 *mfpic-port*)
             (princ g2 *mfpic-port*)
             (princ g3 *mfpic-port*)
             (terpri *mfpic-port*))))
  (tex-def-prim "\\mfpframesep" #'eat-dimen)
  (tex-def-prim "\\mftitle" #'get-group))

(defun do-mfpic-closegraphsfile ()
  (princ "\\closegraphsfile" *mfpic-port*)
  (terpri *mfpic-port*)
  (close *mfpic-port*)
  (let ((tex-f (concatenate 'string *mfpic-file-stem* *mfpic-tex-file-suffix*))
        (mp-f (concatenate 'string *mfpic-file-stem* ".mp")))
    (unless (probe-file mp-f)
      (let ((*tex-format* :plain))
        (call-tex tex-f)))
    (when (probe-file mp-f)
      (system (concatenate 'string *metapost* " " *mfpic-file-stem*)))))

(defun do-mfpic ()
  (princ "\\mfpic" *mfpic-port*)
  (dump-till-end-env "mfpic" *mfpic-port*)
  (princ "\\endmfpic" *mfpic-port*)
  (terpri *mfpic-port*)
  (setq *mfpic-file-num* (+ *mfpic-file-num* 1))
  (let ((f (concatenate 'string *mfpic-file-stem* "."
                        (write-to-string *mfpic-file-num*)))
        (img-file-stem (next-html-image-file-stem)))
    (lazily-make-epsf-image-file f img-file-stem)
    (source-img-file img-file-stem)))

(defun do-following-latex-env-as-image ()
  (do-latex-env-as-image (ungroup (get-group)) :display))

(defun do-latex-env-as-image (env display-p)
  (let ((env2 (if (string= env "align") "eqnarray" env)))
    (when (char= (snoop-actual-char) #\*)
      (get-actual-char)
      (setq env (concatenate 'string env "*"))
      (setq env2 (concatenate 'string env2 "*")))
    (egroup)
    (when display-p
      (do-end-para)
      (emit "<div align=")
      (emit *display-justification*)
      (emit ">"))
    (call-with-html-image-port
     (lambda (o)
         (princ "\\begin{" o)
         (princ env2 o)
         (princ "}" o)
         (dump-till-end-env env o)
         (princ "\\end{" o)
         (princ env2 o)
         (princ "}" o)
         (terpri o)))
    (when display-p
      (emit "</div>") (do-para))))

(defun do-box ()
  (let ((*ignore-active-space-p* t))
    (ignorespaces)
    (get-to))
  (eat-dimen)
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (case c
      (#\{ t)
      (#\\ (get-ctl-seq))))
  (get-actual-char)
  (bgroup)
  (add-postlude-to-top-frame
   (let ((old-math-mode-p *math-mode-p*)
         (old-in-display-math-p *in-display-math-p*)
         (old-tabular-stack *tabular-stack*)
         (old-ligatures-p *ligatures-p*))
     (setq *math-mode-p* nil
           *in-display-math-p* nil
           *tabular-stack* '()
           *ligatures-p* t)
     (lambda ()
       (setq *math-mode-p* old-math-mode-p
             *in-display-math-p* old-in-display-math-p
             *tabular-stack* old-tabular-stack
             *ligatures-p* old-ligatures-p)))))

#|
(defun do-latex-frac ()
  (emit "(")
  (tex2page-string (get-token))
  (emit "/")
  (tex2page-string (get-token))
  (emit ")"))
|#

(defun do-latex-frac ()
  (tex2page-string (ungroup (get-token)))
  (emit
    (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
          (t (incf *math-height*)
             "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>")))
  (tex2page-string (ungroup (get-token))))

(defun do-tex-frac ()
  (ignorespaces)
  (let ((inner-level-p (or (not *in-display-math-p*)
                           (not (null *tabular-stack*))))
        (*tabular-stack* (cons :frac *tabular-stack*)))
    (cond (inner-level-p
           (emit "<sup>") (tex2page-string (get-till-char #\/))
           (emit "</sup>/<sub>") (get-actual-char) (ignorespaces)
           (tex2page-string (get-token)) (emit "</sub>"))
          (t (emit "</td><td><table align=left><tr><td align=center>")
             (tex2page-string (get-till-char #\/))
             (get-actual-char) (ignorespaces)
             (emit "<hr noshade>") (tex2page-string (get-token))
             (emit "</td></tr></table></td><td>")))))

(defun do-frac ()
  (if (eq *tex-format* :latex)
      (do-latex-frac)
    (do-tex-frac)))

(defun do-eqno ()
  (unless *in-display-math-p*
    (terror 'do-eqno "You can't use \\eqno in math mode"))
  (emit "</td><td width=10% align=right>"))

(defun do-eqalign (type)
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (eq c :eof-object) (terror 'do-eqalign "Missing {"))
    (unless (char= c #\{) (terror 'do-eqalign "Missing {"))
    (bgroup)
    (push type *tabular-stack*)
    (add-postlude-to-top-frame
     (lambda ()
       (emit "</td></tr>")
       (emit-newline)
       (emit "</table>")
       (emit-newline)
       (when *in-display-math-p* (emit "</td><td>"))
       (pop-tabular-stack type)
       (setq *equation-position* 0)))
    (when *in-display-math-p* (emit "</td><td>"))
    (emit-newline)
    (emit "<table><tr><td>")))

(defun do-noalign ()
  (let* ((type (car *tabular-stack*))
         (split-p (member type '(:eqalignno :displaylines))))
    (when split-p
      (egroup)
      (emit "</td></tr></table></div>")
      (emit-newline)
      (do-para))
    (tex2page-string (get-group))
    (cond (split-p (do-end-para) (emit-newline)
                   (emit "<div align=center><table><tr><td>")
                   (toss-back-char #\{)
                   (do-eqalign type))
          (t (emit "</td></tr>") (emit-newline) (emit "<tr><td>")))))

(defun do-pmatrix ()
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (eq c :eof-object)
              (not (char= c #\{)))
      (terror 'do-pmatrix "Missing {"))
    (bgroup)
    (setq *math-delim-left* :lparen
          *math-delim-right* :rparen)))

(defun do-over ()
  (emit
   (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
         (t (incf *math-height*)
            "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>"))))

(defun eat-till-eol ()
  ;move just past the next newline
  (loop
    (let ((c (get-actual-char)))
      (when (or (eq c :eof-object) (char= c #\newline)) (return)))))

(defun eat-till-char (d)
  (loop
    (let ((c (get-actual-char)))
      (when (or (eq c :eof-object) (char=  c d)) (return)))))

(defun do-comment ()
  (eat-till-eol)
  (when (munched-a-newline-p)
    (toss-back-char #\Newline) (toss-back-char #\Newline)))

(defun latex-style-file-p (f)
  (let ((e (file-extension f)))
    (and e (string-equal e ".sty"))))

(defun path-to-list (p)
  ;convert a Unix path into a Lisp list
  (if (not p) '()
    (let ((p p) (r '()))
      (loop
        (let ((i (position *path-separator* p :test #'char=)))
          (unless i (push p r) (return (nreverse r)))
          (push (subseq p 0 i) r)
          (setq p (subseq p (1+ i))))))))

(defun kpsewhich (f)
  (let ((tmpf (concatenate 'string *aux-dir/* *jobname* "-Z-Z.temp")))
    (system (concatenate 'string "kpsewhich -- " f " > " tmpf))
    (let ((f (and (probe-file tmpf)
                  (with-open-file (i tmpf :direction :input)
                    (read-line i nil :eof-object)))))
      (ensure-file-deleted tmpf)
      (if (or (not f) (eq f :eof-object)) nil
        (let ((f (string-trim-blanks f)))
          (cond ((= (length f) 0) nil)
                ((probe-file f) f)
                (t nil)))))))

(defun find-tex-file (file)
  ;search for file.tex before file.  Search in current directory first.
  ;If TEX2PAGEINPUTS is set search there, else use kpsewhich(1)
  (let ((file.tex (concatenate 'string file ".tex")))
    (or (and (probe-file file.tex) file.tex)
        (and (probe-file file) file)
        (if *tex2page-inputs*
            (dolist (dir *tex2page-inputs*)
              (let ((f (concatenate 'string dir *directory-separator* file.tex)))
                (when (probe-file f) (return f)))
              (let ((f (concatenate 'string dir *directory-separator* file)))
                (when (probe-file f) (return f))))
          (kpsewhich file)))))

(defun initialize-scm-words ()
 (setq *scm-keywords* (make-hash-table :test #'equal)
       *scm-builtins* (make-hash-table :test #'equal)
       *scm-special-symbols* (make-hash-table :test #'equal)
       *scm-variables* (make-hash-table :test #'equal))
  (mapc (lambda (s) (setf (gethash s *scm-keywords*) t))
        '(

          "=>"
          "and"
          "assert"
          "begin"
          "begin0"
          "block"
          "case"
          "cond"
          "decf"
          "define"
          "define-macro"
          "define-syntax"
          "defmacro"
          "defpackage"
          "defparameter"
          "defstruct"
          "defun"
          "defvar"
          "delay"
          "destructuring-bind"
          "do"
          "do-all-symbols"
          "do-external-symbols"
          "do-symbols"
          "dolist"
          "dotimes"
          "ecase"
          "else"
          "etypecase"
          "eval-when"
          "flet"
          "fluid-let"
          "handler-bind"
          "handler-case"
          "if"
          "incf"
          "labels"
          "lambda"
          "let"
          "let*"
          "let-syntax"
          "let-values"
          "letrec"
          "letrec-syntax"
          "loop" ;NB! common variable in Scheme
          "macrolet"
          "multiple-value-bind"
          "multiple-value-setq"
          "or"
          "pop"
          "prog1"
          "progn"
          "push"
          "quasiquote"
          "quote"
          "set!"
          "setf"
          "setq"
          "syntax-case"
          "syntax-rules"
          "typecase"
          "unless"
          "unquote"
          "unquote-splicing"
          "unwind-protect"
          "when"
          "with"
          "with-handlers"
          "with-input-from-string"
          "with-open-file"
          "with-open-socket"
          "with-open-stream"
          "with-output-to-string"
          "with-slots"

          ))
  t)

(defun actual-tex-filename (f &optional check-timestamp-p)
  ;convert filename f to an "actual" filename, just like TeX does. I.e.,
  ;prefer the .tex extension, and search a pathlist.  If doing main TeX
  ;file, also do some setup
  (let ((doing-main-file-p (not *main-tex-file*)) ;i.e., not yet
        (f (and f (find-tex-file f))))
    (when doing-main-file-p
      (when f
        (setq *jobname* (file-stem-name f))
        (make-target-dir)
        (let ((main-html-page
               (concatenate 'string *aux-dir/* *jobname* *output-extension*)))
          (when (string= main-html-page f)
            (let ((f-save (concatenate 'string f ".sav")))
              (write-log :separation-newline)
              (write-log "Copying weirdly named TeX source file ")
              (write-log f)
              (write-log " to ")
              (write-log f-save)
              (write-log :separation-newline)
              (system (concatenate 'string
                                   #-windows "cp -pf "
                                   #+windows "copy/y "
                                   f " " f-save))
              (setq f f-save)))))
      (load-aux-file))
    (when (and f check-timestamp-p)
      (unless (member f *verb-written-files* :test #'string=)
        (update-last-modification-time f)))
    f))

(defun add-dot-tex-if-no-extension-provided (f)
  (let ((e (file-extension f)))
    (if e f (concatenate 'string f ".tex"))))

(defun ignore-tex-specific-text (env)
  (let ((endenv (concatenate 'string "\\end" env)))
    (loop
      (let ((c (snoop-actual-char)))
        (when
            (eq c :eof-object)
          (terror 'ignore-tex-specific-text "Missing \\end" env))
        (cond ((char= c *esc-char*)
               (let ((x (get-ctl-seq)))
                 (cond ((string= x endenv) (return))
                       ((string= x "\\end")
                        (let ((g (get-grouped-environment-name-if-any)))
                          (when (and g (string= g env)) (return)))))))
              (t (get-actual-char)))))))

(defun do-rawhtml ()
  (loop
    (let ((c (snoop-actual-char)))
      (cond ((eq c :eof-object)
             (terror 'do-rawhtml "missing \\endrawhtml"))
            ((char= c *esc-char*)
             (let* ((x (get-ctl-seq))
                    (y (find-corresp-prim x)))
               (cond ((string= y "\\endrawhtml") (return))
                     ((and (string= y "\\end")
                           (setq *it* (get-grouped-environment-name-if-any)))
                      (let* ((g *it*)
                             (y (find-corresp-prim (concatenate 'string x g))))
                        (if (string= y "\\endrawhtml") (return)
                          (progn (emit "\\end{")
                                 (emit g) (emit "}")))))
                     ((string= x "\\\\")
                      (emit c) (toss-back-char c))
                     (t (emit x)))))
            (t (get-actual-char) (emit c))))))

(defun do-htmlheadonly ()
  (when (null *html-head*) (flag-missing-piece :html-head))
  (let ((s '()))
    (loop
      (let ((c (snoop-actual-char)))
        (cond ((eq c :eof-object)
               (write-aux
                `(!html-head ,(concatenate 'string (nreverse s))))
               (return))
              ((char= c *esc-char*)
               (write-aux `(!html-head ,(concatenate 'string (nreverse s))))
               (setq s '())
               (let ((x (get-ctl-seq)))
                 (cond ((string= x "\\endhtmlheadonly") (return))
                       ((string= x "\\input")
                        (let ((f (get-filename-possibly-braced)))
                          (call-with-input-file/buffered f
                                                         #'do-htmlheadonly)))
                       (t (write-aux `(!html-head ,x))))))
              (t (get-actual-char)
                 (push c s)))))))

(defun resolve-chardefs (c)
  (when (setq *it* (find-chardef c))
    (let ((y *it*))
      (get-actual-char)
      (expand-tex-macro (cdef*-optarg y)
                        (cdef*-argpat y)
                        (cdef*-expansion y)))))

(defun resolve-defs (x)
  (when (setq *it* (find-def x))
    (let ((y *it*))
      (cond ((setq *it* (tdef*-defer y)) *it*)
            ((tdef*-thunk y) nil)
            ((and (inside-false-world-p)
                  (not (if-aware-ctl-seq-p x))
                  ;(> (length (tdef*-argpat y)) 0)
                  ) nil)
            (t
              (when *outer-p*
                (setq *outer-p* nil)
                (toss-back-char *outer-invisible-space*))
              (expand-tex-macro (tdef*-optarg y)
                                 (tdef*-argpat y)
                                 (tdef*-expansion y)))))))

(defun do-expandafter ()
  (let* ((first (get-raw-token/is))
         (second (get-raw-token/is)))
    (toss-back-char *invisible-space*)
    (cond ((ctl-seq-p second)
           (toss-back-string (expand-ctl-seq-into-string second)))
          (t (toss-back-string second)))
    (toss-back-char *invisible-space*)
    (toss-back-string first)))

(defun resolve-expandafters ()
  (let ((c (snoop-actual-char)))
    (when (char= c *esc-char*)
      (let ((x (get-ctl-seq)))
        (if (string= x "\\expandafter")
            (do-expandafter)
          (progn
           (toss-back-char *invisible-space*)
           (toss-back-string x)))))))

(defun do-futurelet ()
  (let* ((first (get-raw-token/is))
         (second (get-raw-token/is))
         (third (get-raw-token)))
    (do-futurelet-aux first second third)))

(defun do-futurenonspacelet ()
  (let* ((first (get-raw-token/is))
         (second (get-raw-token/is))
         (third (get-raw-token/is)))
    (do-futurelet-aux first second third)))

(defun do-futurelet-aux (first second third)
  (tex-let first third nil)
  (toss-back-char *invisible-space*)
  (toss-back-string third)
  (toss-back-char *invisible-space*)
  (toss-back-string second))

(defun set-start-time ()
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time) 5) ;don't care about TZ
    (declare (ignore s))
    (tex-def-count "\\time" (+ (* 60 h) m) t)
    (tex-def-count "\\day" d t)
    (tex-def-count "\\month" mo t)
    (tex-def-count "\\year" y t)))

(defun initialize-globals ()
  (setq *global-texframe* (make-texframe*)
        *section-counter-dependencies* (make-hash-table)
        *dotted-counters* (make-hash-table :test #'equal))
  ;
  ;for TeX, 0 <= \language <= 255; for TeX2page, let's make \language =
  ;256
  (tex-def-count "\\language" 256 t)
  ;
  ;the deepest possible section, the part, has depth -1; so -2 is the
  ;closest-to-0 number that is a meaningless depth
  (tex-def-count "\\secnumdepth" -2 t)
  (tex-def-count "\\tocdepth" -2 t)
  ;
  (tex-def-count "\\footnotenumber" 0 t)
  (tex-def-count "\\TIIPtabularborder" 1 t)
  (tex-def-count "\\TIIPnestedtabularborder" 0 t)
  (tex-def-count "\\TIIPobeyspacestrictly" 0 t)
  (tex-def-count "\\TIIPobeylinestrictly" 0 t)
  (tex-def-count "\\errorcontextlines" 5 t)
  (tex-def-count "\\doublehyphendemerits" 10000 t)
  (tex-def-count "\\finalhyphendemerits" 5000 t)
  (tex-def-count "\\hyphenpenalty" 50 t)
  (tex-def-count "\\exhyphenpenalty" 50 t)
  (tex-def-count "\\pretolerance" 100 t)
  (tex-def-count "\\tolerance" 200 t)
  (tex-def-count "\\hbadness" 1000 t)
  (tex-def-count "\\widowpenalty" 150 t)
  (tex-def-count "\\showboxdepth" 3 t)
  (tex-def-count "\\outputpenalty" 0 t)
  (tex-def-count "\\globaldefs" 0 t)
  (tex-def-count "\\mag" 1000 t)
  (tex-def-count "\\tracingcommands" 0 t)
  (tex-def-count "\\tracingmacros" 0 t)
  (tex-def-count "\\tracingonline" 0 t)
  (tex-def-count "\\time" 0 t)
  (tex-def-count "\\day" 0 t)
  (tex-def-count "\\month" 0 t)
  (tex-def-count "\\year" 0 t)
  ;
  (tex-def-dimen "\\hsize" (tex-length 6.5 :in) t)
  (tex-def-dimen "\\vsize" (tex-length 8.9 :in) t)
  (tex-def-dimen "\\maxdepth" (tex-length 4 :pt) t)
  (tex-def-dimen "\\delimitershortfall" (tex-length 5 :pt) t)
  (tex-def-dimen "\\nulldelimiterspace" (tex-length 1.2 :pt) t)
  (tex-def-dimen "\\scriptspace" (tex-length 0.5 :pt) t)
  (tex-def-dimen "\\hoffset" 0 t)
  (tex-def-dimen "\\voffset" 0 t)
  (tex-def-dimen "\\epsfxsize" 0 t)
  (tex-def-dimen "\\epsfysize" 0 t)
  (tex-def-dimen "\\emergencystretch" 0 t)
  (tex-def-dimen "\\hfuzz" (tex-length 0.1 :pt) t)
  (tex-def-dimen "\\vfuzz" (tex-length 0.1 :pt) t)
  (tex-def-dimen "\\textwidth" (tex-length 6.5 :in) t)
  (tex-def-dimen "\\baselineskip" (tex-length 12 :pt) t)
  (tex-def-dimen "\\overfullrule" (tex-length 5 :pt) t)
  (tex-def-dimen "\\parindent" (tex-length 20 :pt) t)
  (tex-def-dimen "\\leftskip" 0 t)
  (tex-def-dimen "\\parfillskip" 0 t)
  (tex-def-dimen "\\parskip" 0 t)
  (tex-def-dimen "\\abovedisplayskip" (tex-length 12 :pt) t)
  (tex-def-dimen "\\belowdisplayskip" (tex-length 12 :pt) t)
  (tex-def-toks "\\everypar" "" t)
  (tex-def-toks "\\headline" "" t)
  (tex-def-toks "\\footline" "\\folio" t)
  (tex-def-dotted-count "figure" nil)
  (tex-def-dotted-count "table" nil)
  (tex-def-dotted-count "equation" nil)
  (tex-gdef-0arg "\\TIIPcurrentnodename" "no value yet")
  (tex-gdef-0arg "\\@currentlabel" "no value yet")
  (tex-gdef-0arg "\\TZPcolophonlastpage" "0")
  (tex-gdef-0arg "\\TZPcolophontimestamp" "1")
  (tex-gdef-0arg "\\TZPcolophoncredit" "1")
  (tex-gdef-0arg "\\TZPcolophonweblink" "1")
  (tex-gdef-0arg "\\TZPmathimage" "1")
  (tex-gdef-0arg "\\TZPimageformat" "PNG")
  (tex-gdef-0arg "\\TZPimageconverter" "NetPBM")
  (tex-gdef-0arg "\\TZPredirectseconds" "5")
  (tex-gdef-0arg "\\TZPslatexcomments" "0")
  (tex-gdef-0arg "\\TZPtexlayout" "0")
  (tex-gdef-0arg "\\TZPtextext" "1")
  (tex-gdef-0arg "\\TZPraggedright" "1")

  (initialize-scm-words)

  )

(defun find-def (ctlseq)
  (or (some (lambda (fr) (gethash ctlseq (texframe*-definitions fr)))
            *tex-env*)
      (and *global-texframe*
           (gethash ctlseq (texframe*-definitions *global-texframe*)))
      (gethash ctlseq (texframe*-definitions *primitive-texframe*))))

(defun find-math-def (ctlseq)
  (gethash ctlseq (texframe*-definitions *math-primitive-texframe*)))

(defun find-count (ctlseq)
  (or (some (lambda (fr) (gethash ctlseq (texframe*-counts fr))) *tex-env*)
      (gethash ctlseq (texframe*-counts *global-texframe*))
      (gethash ctlseq (texframe*-counts *primitive-texframe*))))

(defun find-toks (ctlseq)
  (or (some (lambda (fr) (gethash ctlseq (texframe*-toks fr))) *tex-env*)
      (gethash ctlseq (texframe*-toks *global-texframe*))
      (gethash ctlseq (texframe*-toks *primitive-texframe*))))

(defun find-dimen (ctlseq)
  (or (some (lambda (fr) (gethash ctlseq (texframe*-dimens fr))) *tex-env*)
      (gethash ctlseq (texframe*-dimens *global-texframe*))
      (gethash ctlseq  (texframe*-dimens *primitive-texframe*))))

(defun get-toks (ctlseq)
  (or (find-toks ctlseq) (terror 'get-toks)))

(defun get-dimen (ctlseq)
  (cond ((find-dimen ctlseq))
        (t ;let's just assume the default \hsize
           (tex-length 6.5 :in))))

(defun the-count (dracula)
  (or (find-count dracula) (terror 'the-count)))

(defun do-count= (z globalp)
  (get-equal-sign) (tex-def-count z (get-number) globalp))

(defun do-toks= (z globalp)
  (get-equal-sign) (tex-def-toks z (get-group) globalp))

(defun do-dimen= (z globalp)
  (get-equal-sign)
  (tex-def-dimen z (get-scaled-points) globalp)
  (ignorespaces))

(defun get-gcount (ctlseq)
  (gethash ctlseq (texframe*-counts *global-texframe*)))

(defun tex-gdef-count (ctlseq v)
  (tex-def-count ctlseq v t))

(defun do-number ()
  (emit (get-number)))

(defun do-magnification ()
  (tex-def-count "\\mag" (get-number) nil))

(defun do-magstep ()
  (case (string-to-number (get-token-or-peeled-group))
    (1 "1000")
    (2 "1200")
    (3 "1440")
    (4 "1728")
    (5 "2074")
    (6 "2488")
    (t "")))

(defun scaled-point-to-tex-point (sp)
  (concatenate 'string (write-to-string (/ sp 65536.0)) "pt"))

(defun expand-the ()
  (let ((ctlseq (get-ctl-seq)))
    (cond ((setq *it* (find-dimen ctlseq))
           (scaled-point-to-tex-point *it*))
          ((setq *it* (get-number-corresp-to-ctl-seq ctlseq))
           *it*)
          ((find-toks ctlseq))
          (t (trace-if nil "expand-the failed")))))

(defun do-the ()
  ;almost like do-number
  (let ((ctlseq (get-ctl-seq)))
    (cond ((setq *it* (find-dimen ctlseq))
             (emit (scaled-point-to-tex-point *it*)))
          ((setq *it* (get-number-corresp-to-ctl-seq ctlseq))
           (emit *it*))
          ((setq *it* (find-toks ctlseq))
           (tex2page-string *it*))
          (t (trace-if nil "do-the failed")))))

(defun find-corresp-prim (ctlseq)
  ;is this really necessary?  Why not make resolve-defs take over this
  ;too?
  (let ((y (find-def ctlseq)))
    (or (and y (tdef*-defer y)) ctlseq)))

(defun find-corresp-prim-thunk (ctlseq)
  (let ((y (find-def ctlseq)))
    (if (and y (tdef*-thunk y)) (tdef*-prim y) ctlseq)))

(defun globally-p () (> (get-gcount "\\globaldefs") 0))

(defun do-let (globalp)
  (unless (inside-false-world-p)
    (ignorespaces)
    (let* ((lhs (get-ctl-seq))
           (rhs (progn (get-equal-sign) (get-raw-token/is)))
           (frame (and globalp *global-texframe*)))
      (if (ctl-seq-p rhs) (tex-let lhs rhs frame)
        (tex-def lhs '() rhs nil nil nil nil frame)))))

(defun do-def (globalp e-p)
  (unless (inside-false-world-p)
    (let ((lhs (get-raw-token/is)))
      (when (and (ctl-seq-p lhs) (string= lhs "\\TIIPcsname"))
        (setq lhs (get-peeled-group)))
      (let* ((argpat (get-def-arguments lhs))
             (rhs (ungroup (get-group)))
             (frame (and globalp *global-texframe*)))
        (when e-p (setq rhs (expand-edef-macro rhs)))
        (cond ((ctl-seq-p lhs) (tex-def lhs argpat rhs nil nil nil nil frame))
              (t (tex-def-char (char lhs 0) argpat rhs frame)))))))

(defun do-newcount (globalp) (tex-def-count (get-ctl-seq) 0 globalp))

(defun do-newtoks (globalp) (tex-def-toks (get-ctl-seq) "" globalp))

(defun do-newdimen (globalp) (tex-def-dimen (get-ctl-seq) 0 globalp))

(defun do-advance (globalp)
  (let* ((ctlseq (get-ctl-seq))
         (count (find-count ctlseq)))
    (get-by)
    (if count (tex-def-count ctlseq (+ count (get-number)) globalp) (eat-dimen))))

(defun do-multiply (globalp)
  (let* ((ctlseq (get-ctl-seq))
         (curr-val (find-count ctlseq)))
    (get-by)
    (tex-def-count ctlseq (* curr-val (get-number)) globalp)))

(defun do-divide (globalp)
  (let* ((ctlseq (get-ctl-seq))
         (curr-val (find-count ctlseq)))
    (get-by)
    (tex-def-count ctlseq (floor curr-val (get-number)) globalp)))

(defun do-newcommand (renewp)
  (ignorespaces)
  (let* ((lhs (string-trim-blanks (ungroup (get-token))))
         (optarg nil)
         (argc (cond ((setq *it* (get-bracketed-text-if-any))
                      (let ((s *it*))
                        (cond ((setq *it* (get-bracketed-text-if-any))
                               (setq optarg *it*)))
                        (read-from-string (string-trim-blanks s))))
                     (t 0)))
         (rhs (ungroup (get-token)))
         (ok-to-def-p (or renewp (not (find-def lhs)))))
    (tex-def lhs (latex-argnum-to-plain-argpat argc)
             rhs optarg nil nil nil nil)
    (unless ok-to-def-p
      (trace-if (> (find-count "\\tracingcommands") 0)
                lhs " already defined"))))

(defun do-advancetally (globalp)
  (let* ((ctlseq (get-ctl-seq))
         (increment
          (read-from-string
           (string-trim-blanks (ungroup (get-token))))))
    (tex-def ctlseq '()
             (write-to-string
              (+ (read-from-string
                  (or (resolve-defs ctlseq) ctlseq))
                 increment))
             nil nil nil nil globalp)))

(defun do-newenvironment (renewp)
  (ignorespaces)
  (let* ((envname (string-trim-blanks (ungroup (get-token))))
         (bs-envname (concatenate 'string "\\" envname))
         (optarg nil)
         (argc (cond ((setq *it* (get-bracketed-text-if-any))
                      (let ((s *it*))
                        (cond ((setq *it* (get-bracketed-text-if-any))
                               (setq optarg *it*)))
                        (read-from-string (string-trim-blanks s))))
                     (t 0)))
         (beginning (concatenate 'string "\\begingroup " (ungroup (get-token))))
         (ending (concatenate 'string (ungroup (get-token)) "\\endgroup"))
         (ok-to-def-p (or renewp (not (find-def bs-envname)))))
    (tex-def bs-envname (latex-argnum-to-plain-argpat argc)
             beginning optarg nil nil nil nil)
    (tex-def (concatenate 'string "\\end" envname) '() ending nil
             nil nil nil nil)
    (unless ok-to-def-p
      (trace-if t "{" envname "} already defined"))))

(defun tex-def-dotted-count (counter-name sec-num)
  (when sec-num
    (push counter-name
          (gethash sec-num *section-counter-dependencies* nil)))
  (setf (gethash counter-name *dotted-counters*) (make-counter* :within sec-num)))

(defun do-newtheorem ()
  (let* ((env (ungroup (get-group)))
         (numbered-like (get-bracketed-text-if-any))
         (counter-name (or numbered-like env))
         (caption (ungroup (get-group)))
         (within (if numbered-like nil (get-bracketed-text-if-any)))
         (sec-num (and within
                       (section-ctl-seq-p (concatenate 'string "\\" within)))))
    (unless numbered-like
      (tex-def-dotted-count counter-name sec-num))
    (tex-def (concatenate 'string "\\" env) '()
             (concatenate 'string "\\par\\begingroup\\TIIPtheorem{"
                          counter-name "}{" caption "}")
             nil nil nil nil *global-texframe*)
    (tex-def (concatenate 'string "\\end" env) '() "\\endgroup\\par"
             nil nil nil nil *global-texframe*)))

(defun do-theorem ()
  (let* ((counter-name (ungroup (get-group)))
         (counter (gethash counter-name *dotted-counters*))
         (caption (ungroup (get-group))))
    (unless counter (terror 'do-theorem))
    (let ((new-counter-value (+ 1 (counter*-value counter))))
      (setf (counter*-value counter) new-counter-value)
      (let* ((thm-num
              (let ((sec-num (counter*-within counter)))
                (if sec-num
                    (concatenate 'string (section-counter-value sec-num) "."
                                 (write-to-string new-counter-value))
                  (write-to-string new-counter-value))))
             (lbl (concatenate 'string *html-node-prefix* "thm_" thm-num)))
        (tex-def-0arg "\\TIIPcurrentnodename" lbl)
        (tex-def-0arg "\\@currentlabel" thm-num)
        (emit-anchor lbl)
        (emit-newline)
        (emit "<b>")
        (emit caption)
        (emit " ")
        (emit thm-num)
        (emit ".</b>")
        (emit-nbsp 2)))))

(defun do-begin ()
  (unless (setq *it* (get-grouped-environment-name-if-any))
    (terror 'do-begin "\\begin not followed by environment name"))
  (let ((env *it*))
    (toss-back-char *invisible-space*)
    (toss-back-string (concatenate 'string "\\" env))
    (unless (member env '("htmlonly" "cssblock" "document" "latexonly"
                          "rawhtml" "texonly" "verbatim" "verbatim*") :test #'string=)
      (toss-back-string "\\begingroup")
      (do-end-para))))

(defun do-end ()
  (cond ((setq *it* (get-grouped-environment-name-if-any))
         (let ((env *it*))
           (toss-back-char *invisible-space*)
           (unless (member env '("htmlonly" "document") :test #'string=)
             (do-end-para)
             (toss-back-string "\\endgroup"))
           (toss-back-string (concatenate 'string "\\end" env))))
        (t (toss-back-char *invisible-space*)
           (toss-back-string "\\TIIPbye"))))

(defun latex-argnum-to-plain-argpat (n)
  (let ((n n) (s '()))
    (loop
      (when (<= n 0) (return s))
      (decf n)
      (push (code-char (+ *int-corresp-to-0* n)) s)
      (push #\# s))))

(defun make-reusable-img (globalp)
  (incf *imgdef-file-count*)
  (ignorespaces)
  (let ((lhs (get-ctl-seq))
        (imgdef-file-stem
         (concatenate 'string *subjobname* *img-file-suffix*
                      *imgdef-file-suffix*
                      (write-to-string *imgdef-file-count*))))
    (dump-imgdef imgdef-file-stem)
    (tex-to-img imgdef-file-stem)
    (tex-def lhs '()
     (concatenate 'string "\\TIIPreuseimage{" imgdef-file-stem "}") nil nil nil
     nil (and globalp *global-texframe*))))

(defun valid-img-file-p (f)
  ;disable for now as it generates sbcl ext-format error
  (declare (ignore f))
  nil
  #+disable
  (and (probe-file f)
       (or
        (with-open-file (i f :direction :input)
          (not (eq (read-char i nil :eof-object) :eof-object)))
        (progn (delete-file f) nil))))

(defun source-img-file (img-file-stem &rest alt)
  (let* ((alt (if (null alt) nil (car alt)))
         (img-file (concatenate 'string img-file-stem (find-img-file-extn)))
         (f (concatenate 'string *aux-dir/* img-file)))
    (write-log #\()
    (write-log f)
    (write-log :separation-space)
    (valid-img-file-p f)
    (emit "<img src=\"")
    (emit img-file)
    (emit "\" border=\"0\" alt=\"")
    (cond (alt (emit alt)) (t (emit "[") (emit img-file) (emit "]")))
    (emit "\">")
    (write-log #\))
    (write-log :separation-space)
    t))

(defun reuse-img ()
  (source-img-file (ungroup (get-group))))

(defun get-def-arguments (lhs)
  (labels ((aux ()
                (let ((c (snoop-actual-char)))
                  (when (eq c :eof-object)
                    (terror 'get-def-arguments
                            "EOF found while scanning definition of " lhs))
                  (cond ((char= c *esc-char*)
                         (let ((x (get-ctl-seq)))
                           (if (string= x "\\par")
                               (cons #\Newline (cons #\Newline (aux)))
                             (append (concatenate 'list x) (aux)))))
                        ((char= c #\{) '())
                        (t (cond ((char= c #\Newline)
                                  ;kludge for writing Texinfo-type
                                  ;macros. Should really be equivalent
                                  ;to any white space
                                  (get-actual-char) (ignorespaces))
                                 ((char-whitespace-p c)
                                  (ignorespaces) (setq c #\space))
                                 (t (get-actual-char)))
                           (cons c (aux)))))))
    (aux)))

(defun get-till-char (c0)
  (concatenate
   'string
   (nreverse
    (let ((s '()) (nesting 0) (escape-p nil))
      (loop
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object)
            (terror 'get-till-char "File ended too soon"))
          (cond (escape-p
                 (push (get-actual-char) s)
                 (setq escape-p nil))
                ((char= c c0) (return s))
                ((char= c *esc-char*)
                 (push (get-actual-char) s)
                 (setq escape-p t))
                ((char= c #\{)
                 (push (get-actual-char) s)
                 (incf nesting))
                ((char= c #\})
                 (push (get-actual-char) s)
                 (decf nesting))
                ((> nesting 0)
                 (push (get-actual-char) s))
                ((and (char-whitespace-p c)
                      (not (char= c0 #\Newline))
                      (char-whitespace-p c0))
                 (return s))
                (t (push (get-actual-char) s)))))))))

(defun digit-to-int (d) (- (char-code d) *int-corresp-to-0*))

(defun do-halign ()
  (do-end-para)
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (eq c :eof-object)
              (not (char= c #\{)))
      (terror 'do-halign "Missing {")))
  (let ((*tabular-stack* (cons :halign *tabular-stack*)))
    (bgroup)
    (emit "<table>")
    (let ((tmplt (get-halign-template)))
      (loop
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object)
            (terror 'do-halign "Eof inside \\halign"))
          (cond ((char= c #\}) (get-actual-char) (emit "</table>")
                 (egroup) (do-para) (return))
                (t (expand-halign-line tmplt))))))))

(defun get-halign-template ()
  (let ((s '()))
    (loop
      (let ((x (get-raw-token)))
        (when (eq x :eof-object)
          (terror 'get-halign-template "Eof in \\halign"))
        (cond ((string= x "\\cr")  (push nil s)
               (return (nreverse s)))
              ((string= x "#") (push t s))
              ((string= x "&") (push nil s))
              (t (push x s)))))))

(defun expand-halign-line (tmplt)
  (emit "<tr>")
  (block outer-loop
    (let ((tmplt tmplt) (ins " "))
      (loop
        (let ((x (get-raw-token)))
          (when (eq x :eof-object)
            (terror 'expand-halign-line "Eof in \\halign"))
          (cond ((or (string= x "&") (string= x "\\cr"))
                 (let ((r "{"))
                   (loop
                     (when (null tmplt)
                       (terror 'expand-halign-line "Eof in \\halign"))
                     (let ((y (car tmplt)))
                       (case y
                         ((nil) (emit "<td>")
                          (tex2page-string
                           (concatenate 'string r "}"))
                          (when (and (string= x "\\cr")
                                     (string= ins " "))
                            (emit-nbsp 1))
                          (emit "</td>")
                          (if (string= x "\\cr")
                              (progn (emit "</tr>")
                                     (emit-newline) (return-from outer-loop))
                            (progn (pop tmplt)
                                   (setq ins " ") ; or ""?
                                   (return))))
                         ((t)
                          (pop tmplt)
                          (setq r (concatenate 'string r ins)))
                         (t (pop tmplt)
                            (setq r (concatenate 'string r y))))))))
                (t (setq ins
                         (concatenate 'string ins x)))))))))

(defun read-till-next-sharp (k argpat)
  (let ((n (length argpat)) (ss '()))
    (block outer-loop
      (loop
        (let ((i k) (s '()))
          (loop
            (let ((c (if (< i n) (elt argpat i) #\#)))
              (when (char= c #\#)
                (return-from outer-loop
                             (values i (concatenate 'string (nreverse ss)))))
              (let ((d (snoop-actual-char)))
                (cond ((and (char= c #\space) (char-whitespace-p d))
                       (ignorespaces)
                       (incf i) (push c s))
                      ((and *comment-char*
                            (char= d *comment-char*))
                       (do-comment))
                      ((and (char= c #\Newline)
                            (char-whitespace-p d)
                            (or (munched-a-newline-p)
                                (progn (toss-back-char d) nil)))
                       (incf i)
                       (push c s))
                      ((char= c d)
                       (get-actual-char)
                       (incf i)
                       (push c s))
                      ((= i k)
                       (setq ss
                             (if (and (char= d #\{)
                                      (or (null ss)
                                          (not (char= (car ss) *esc-char*))))
                                 (append (get-group-as-reversed-chars)
                                         ss)
                               (progn
                                (if (and (char-whitespace-p d)
                                         (not (char= d #\Newline)))
                                    (ignorespaces)
                                  (get-actual-char))
                                (cons d ss))))
                       (return))
                      (t (setq ss (append s ss))
                         (return)))))))))))

(defun read-macro-args (argpat k r)
  (let ((n (length argpat)))
    (nreverse
     (let ((k k) (r r))
       (loop
         (when (>= k n) (return r))
         (let ((c (elt argpat k)))
           (cond ((char= c #\#)
                  (cond ((= k (1- n)) (push (get-till-char #\{) r)
                         (return r))
                        ((= k (- n 2)) (push (ungroup (get-token)) r)
                         (return r))
                        (t (let ((c2 (elt argpat (+ k 2))))
                             (if (char= c2 #\#)
                                 (progn (incf k 2)
                                        (push (ungroup (get-token)) r))
                                 (multiple-value-bind (k2 s)
                                   (read-till-next-sharp (+ k 2) argpat)
                                   (setq k k2)
                                   (push s r)))))))
                 (t (let ((d (get-actual-char)))
                      (when (eq d :eof-object)
                        (terror 'read-macro-args "Eof before macro got enough args"))
                      (unless (char= c d)
                        (terror 'read-macro-args "Misformed macro call"))
                      (incf k))))))))))

(defun expand-edef-macro (rhs)
  (let* ((*not-processing-p* t)
         (tmp-port (make-string-output-stream)))
    (call-with-input-string/buffered rhs
      (lambda ()
        (loop
          (let ((c (snoop-actual-char)))
            (when (eq c :eof-object) (return))
            (princ (cond ((char= c *esc-char*)
                          (let ((x (get-ctl-seq)))
                            (toss-back-char *invisible-space*)
                            (cond ((or (string= x "\\the") (string= x "\\number"))
                                   (let ((x2 (get-raw-token/is)))
                                     (toss-back-char *invisible-space*)
                                     (toss-back-string x2)
                                     (cond ((ctl-seq-p x2)
                                            (cond ((string= x "\\the") (expand-the))
                                                  ((string= x "\\number") (get-number))
                                                  (t "DeAdCoDe")))
                                           (t x))))
                                  ((string= x "\\noexpand")
                                   (let ((x2 (get-raw-token/is)))
                                     (toss-back-char *invisible-space*)
                                     x2))
                                  ((setq *it* (find-def x))
                                   (let ((y *it*))
                                     (cond ((and (null (tdef*-argpat y))
                                                 (not (tdef*-optarg y))
                                                 (not (tdef*-thunk y))
                                                 (not (tdef*-prim y))
                                                 (not (tdef*-defer y)))
                                            (toss-back-char *invisible-space*)
                                            (toss-back-string (tdef*-expansion y))
                                            "")
                                           (t x))))
                                  (t x))))
                         (t (get-actual-char) c))
                   tmp-port)))))
    (get-output-stream-string tmp-port)))

(defun expand-tex-macro (optarg argpat rhs)
  (let* ((k 0)
         (r (if (not optarg) '()
              (progn
               (setq k 2)
               (list
                (cond ((setq *it* (get-bracketed-text-if-any)) *it*)
                      (t optarg))))))
         (args (read-macro-args argpat k r))
         (rhs-n (length rhs)))
    (concatenate
     'string
     (labels ((aux
               (k)
               (if (>= k rhs-n) '()
                 (let ((c (char rhs k)))
                   (cond ((char= c #\\)
                          (let ((j (1+ k)) (s (list #\\)))
                            (loop
                              (when (>= j rhs-n) (return (nreverse s)))
                              (let ((c (char rhs j)))
                                (cond ((alpha-char-p c)
                                       (incf j) (push c s))
                                      ((and (char= c #\#) (> (length s) 1))
                                       (return
                                        (append
                                         (nreverse s)
                                         (cons #\ (aux j)))))
                                      ((= (length s) 1)
                                       (return
                                        (append
                                         (nreverse
                                          (cons c s))
                                         (aux (1+ j)))))
                                      (t (return
                                          (append
                                           (nreverse s)
                                           (aux j)))))))))
                         ((char= c #\#)
                          (if (= k (1- rhs-n)) (list #\#)
                            (let ((n (char rhs (1+ k))))
                              (cond ((char= n #\#)
                                     (cons #\# (aux (+ k 2))))
                                    ((and (digit-char-p n)
                                          (<= (digit-to-int n) (length args)))
                                     (append
                                      (concatenate 'list
                                                   (elt args
                                                        (1- (digit-to-int n))))
                                      (aux (+ k 2))))
                                    (t (cons #\# (aux (1+ k))))))))
                         (t (cons c (aux (1+ k)))))))))
       (aux 0)))))

(defun do-verbatimescapechar ()
  (ignorespaces)
  (let* ((c1 (get-actual-char))
         (c2 (get-actual-char)))
    (unless (char= c1 *esc-char*)
      (terror 'do-verbatimescapechar "Arg must be \\<char>"))
    (setq *esc-char-verb* c2)))

(defun do-verb-braced (ignore)
  (declare (ignore ignore))
  (let ((*esc-char* *esc-char-verb*) (*tex-extra-letters* '())
        (nesting 0))
    (loop
      (let ((c (get-actual-char)))
        (when (eq c :eof-object)
          (terror 'do-verb-braced "Eof inside verbatim"))
        (cond ((char= c *esc-char*) (toss-back-char c)
               (let ((x (let ((*not-processing-p* t))
                          (get-ctl-seq))))
                 (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                        (emit (char x 1)))
                       (t (let ((*esc-char* *esc-char-std*))
                            (do-tex-ctl-seq-completely x))))))
              ((char= c #\{)
               (emit #\{) (incf nesting))
              ((char= c #\})
               (when (= nesting 0) (return))
               (emit #\}) (decf nesting))
              ((char= c #\space)
               (emit (if *verb-visible-space-p* *verbatim-visible-space* #\space)))
              ((char= c #\Newline)
               (cond (*verb-display-p* (emit "&nbsp;") (emit-newline))
                     (*verb-visible-space-p* (emit *verbatim-visible-space*))
                     (t (emit-newline))))
              ((and (char= c #\-) (not *verb-display-p*))
               ;in-text verbatim should not break on hyphen
               (emit "&#x2011;"))
              (t (emit-html-char c)))))))

(defun do-verb-delimed (d)
  (loop
    (let ((c (get-actual-char)))
      (when (eq c :eof-object)
        (terror 'do-verb-delimed "Eof inside verbatim"))
      (cond ((char= c d) (return))
            ((char= c #\space)
             (emit
             (if *verb-visible-space-p* *verbatim-visible-space* #\space)))
            ((char= c #\Newline)
             (cond (*verb-display-p* (emit "&nbsp;") (emit-newline))
                   (*verb-visible-space-p* (emit *verbatim-visible-space*))
                   (t (emit-newline))))
            ((and (char= c #\-) (not *verb-display-p*))
             ;in-text verbatim should not break on hyphen
             (emit "&#x2011;"))
            (t (emit-html-char c))))))

(defun do-verb ()
  (ignorespaces)
  (bgroup)
  (let* ((*verb-visible-space-p* (eat-star))
         (*ligatures-p* nil)
         (d (get-actual-char))
         (*verb-display-p* (munched-a-newline-p)))
    (cond (*outputting-external-title-p* nil)
          (*verb-display-p* (do-end-para) (emit "<pre class=verbatim>"))
          (t (emit "<code class=verbatim>")))
    (funcall (if (char= d #\{) #'do-verb-braced #'do-verb-delimed) d)
    (cond (*outputting-external-title-p* nil)
          (*verb-display-p* (emit "</pre>")
                            (do-noindent))
          (t (emit "</code>"))))
  (egroup))

(defun do-verbc ()
  (ignorespaces)
  (bgroup)
  (let ((*ligatures-p* nil))
    (emit "<code class=verbatim>")
    (emit-html-char (get-actual-char))
    (emit "</code>"))
  (egroup))

(defun do-verbatiminput ()
  (ignorespaces)
  (let ((f
         (add-dot-tex-if-no-extension-provided (get-filename-possibly-braced))))
    (cond ((probe-file f) (do-end-para) (bgroup) (emit "<pre class=verbatim>")
           (with-open-file (p f :direction :input)
             (loop
               (let ((c (read-char p nil :eof-object)))
                 (when (eq c :eof-object) (return))
                 (emit-html-char c))))
           (emit "</pre>") (egroup) (do-para))
          (t (non-fatal-error "File " f " not found")))))

(defun do-verbwritefile ()
  (let* ((f (get-filename-possibly-braced))
         (e (file-extension f)))
    (unless e (setq e ".tex") (setq f (concatenate 'string f e)))
    (when *verb-port* (close *verb-port*))
    (push f *verb-written-files*)
    (when (string-equal e ".mp") (push f *mp-files*))
    (setq *verb-port* (open f :direction :output :if-exists :supersede))))

(defun verb-ensure-output-port ()
  (unless *verb-port*
    (let ((output-file (concatenate 'string *jobname* ".txt")))
      (setq *verb-port* (open output-file :direction :output
                              :if-exists :supersede)))))

(defun dump-groupoid (p)
  (ignorespaces)
  (let ((write-char #'write-char) (d (get-actual-char)))
    (unless p
      (setq write-char (lambda (x y)
                         (declare (ignore x y))
                         nil)))
    (case d
      (#\{ (let ((nesting 0))
             (loop
               (let ((c (get-actual-char)))
                 (when (eq c :eof-object)
                   (terror 'dump-groupoid "Eof inside verbatim"))
                 (cond ((char= c *esc-char-verb*)
                        (funcall write-char c p)
                        (funcall write-char (get-actual-char) p))
                       ((char= c #\{)
                        (funcall write-char c p)
                        (incf nesting))
                       ((char= c #\})
                        (when (= nesting 0) (return))
                        (funcall write-char c p)
                        (decf nesting))
                       (t (funcall write-char c p)))))))
      (t (loop
           (let ((c (get-actual-char)))
             (when (eq c :eof-object)
               (terror 'dump-groupoid "Eof inside verbatim"))
             (when (char= c d) (return))
             (funcall write-char c p)))))))

(defun do-makehtmlimage ()
  (ignorespaces)
  (unless (char= (snoop-actual-char) #\{)
    (terror 'do-makehtmlimage "\\makehtmlimage's argument must be a group"))
  (call-with-html-image-port #'dump-groupoid))

(defun do-verbwrite () (verb-ensure-output-port) (dump-groupoid *verb-port*))

(defun do-string ()
  (let ((c (snoop-actual-char)))
    (cond ((eq c :eof-object) nil)
          ((char= c *esc-char*) (get-actual-char)
           (toss-back-char *invisible-space*)
           (toss-back-string "\\TIIPbackslash"))
          ((char= c *comment-char*) (eat-till-eol) (do-string))
          (t (toss-back-char (get-actual-char))))))

(defun do-verbatim ()
  (if (eql *tex-format* :latex) (do-verbatim-latex "verbatim")
      (do-verbatim-eplain)))

(defun do-verbatim-latex (env)
  (do-end-para)
  (bgroup)
  (emit "<pre class=verbatim>")
  (let ((*verb-visible-space-p* (eat-star)))
    (when *verb-visible-space-p* (setq env (concatenate 'string env "*")))
    (munched-a-newline-p)
    (let ((*ligatures-p* nil))
      (loop
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object) (terror 'do-verbatim-latex "Eof inside verbatim"))
          (cond ((char= c #\\)
                 (let ((cs (get-ctl-seq)))
                   (if (string= cs "\\end")
                       (cond ((setq *it* (get-grouped-environment-name-if-any))
                              (let ((e *it*))
                                (when (string= *it* env) (return))
                                (emit-html-string cs)
                                (emit-html-char #\{)
                                (emit-html-string e)
                                (emit-html-char #\})))
                             (t (emit-html-string cs)))
                     (progn (emit-html-string cs)))))
                ((char= c #\space)
                 (get-actual-char)
                 (emit (if *verb-visible-space-p* *verbatim-visible-space*
                    #\space)))
                (t (emit-html-char (get-actual-char))))))))
  (emit "</pre>")
  (egroup)
  (do-para))

(defun do-verbatim-eplain ()
  (let ((*inside-eplain-verbatim-p* t) (*esc-char* *esc-char-verb*))
    (loop
      (unless *inside-eplain-verbatim-p* (return))
      (let ((c (get-actual-char)))
        (when (eq c :eof-object) (terror 'do-verbatim-eplain "Eof inside verbatim"))
        (cond ((char= c *esc-char*) (toss-back-char c)
               (let ((x (let ((*not-processing-p* t))
                          (get-ctl-seq))))
                 (cond ((string= x "\\ ") (emit " "))
                       (t (do-tex-ctl-seq-completely x)))))
              ((char= c #\space) (emit "&nbsp;"))
              ((char= c #\Newline) (emit "<br>") (emit-newline))
              (t (emit-html-char c)))))))

(defun do-endverbatim-eplain ()
  (setq *inside-eplain-verbatim-p* nil))

(defun do-alltt ()
  (do-end-para)
  (bgroup)
  (emit "<pre class=verbatim>")
  (munched-a-newline-p)
  (let ((*in-alltt-p* t))
    (loop
      (let ((c (snoop-actual-char)))
        (when (eq c :eof-object) (terror 'do-alltt "Eof inside alltt"))
        (case c
          (#\\ (do-tex-ctl-seq (get-ctl-seq)))
          (#\{ (get-actual-char) (bgroup))
          (#\} (get-actual-char) (egroup))
          (t (emit-html-char (get-actual-char))))
        (unless *in-alltt-p* (return))))))

(defun do-end-alltt ()
  (emit "</pre>")
  (egroup)
  (do-para)
  (setq *in-alltt-p* nil))

(defun do-scm-set-specialsymbol ()
  (let* ((sym (get-peeled-group))
         (xln (get-group)))
    (setf (gethash sym  *scm-special-symbols*) xln)))

(defun do-scm-unset-specialsymbol ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignore-all-whitespace)
        (when (eq (snoop-actual-char) :eof-object) (return))
        (setf (gethash (scm-get-token) *scm-special-symbols*) nil)))))

(defun do-scm-set-builtins ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignore-all-whitespace)
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-keywords*) nil
                  (gethash s *scm-variables*) nil
                  (gethash s *scm-builtins*) t)))))))

(defun do-scm-set-keywords ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignore-all-whitespace)
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-builtins*) nil
                  (gethash s *scm-variables*) nil
                  (gethash s *scm-keywords*) t)))))))

(defun do-scm-set-variables ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignore-all-whitespace)
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-builtins*) nil
                  (gethash s *scm-keywords*) nil
                  (gethash s *scm-variables*) t)))))))

(defun scm-emit-html-char (c)
  (unless (eq c :eof-object)
    (when *scm-dribbling-p* (write-char c *verb-port*))
    (if (and (char= c #\-) (not *verb-display-p*))
        (emit "&#x2011;")
      (emit-html-char c))))

(defun scm-output-next-chunk ()
  (let ((c (snoop-actual-char)))
    (cond ((and *slatex-math-escape* (char= c *slatex-math-escape*))
           (scm-escape-into-math))
          ((char= c #\;) (scm-output-comment) (do-end-para))
          ((char= c #\") (scm-output-string)) ((char= c #\#) (scm-output-hash))
          ((char= c #\,) (get-actual-char) (emit "<span class=keyword>")
           (scm-emit-html-char c)
           (let ((c (snoop-actual-char)))
             (when (char= c #\@) (get-actual-char) (scm-emit-html-char c)))
           (emit "</span>"))
          ((or (char= c #\') (char= c #\`)) (get-actual-char)
           (emit "<span class=keyword>") (scm-emit-html-char c) (emit "</span>"))
          ((or (char-whitespace-p c) (member c *scm-token-delims* :test #'char=))
           (get-actual-char) (scm-emit-html-char c))
          (t (scm-output-token (scm-get-token))))))

(defun scm-set-mathescape (yes-p)
  (let ((c (let ((*esc-char* (code-char 0)))
             (char (ungroup (get-group)) 0))))
    (cond (yes-p (setq *slatex-math-escape* c)
                 (push *slatex-math-escape* *scm-token-delims*))
          (t (setq *slatex-math-escape* nil)
             (setq *scm-token-delims* (delete c *scm-token-delims* :test #'char=))))))

(defun scm-escape-into-math ()
  (get-actual-char)
  (let ((math-text (get-till-char *slatex-math-escape*)))
    (get-actual-char)
    (unless (string= math-text "")
      (emit "<span class=variable>")
      (let ((*esc-char* *esc-char-std*))
        (tex2page-string (concatenate 'string "$" math-text "$")))
      (emit "</span>"))))

(defun scm-output-slatex-comment ()
  (let ((s (get-line)))
    (emit "<span class=comment>")
    (when *scm-dribbling-p* (princ s *verb-port*) (terpri *verb-port*))
    (let ((*esc-char* *esc-char-std*))
      (tex2page-string s))
    (do-end-para)
    (emit "</span>")
    (toss-back-char #\Newline)))

(defun scm-output-verbatim-comment ()
  (emit "<span class=comment>")
  (loop
    (let ((c (get-actual-char)))
      (when (or (eq c :eof-object) (char= c #\Newline)) (emit "</span>")
        (scm-emit-html-char c) (return))
      (cond ((and (char-whitespace-p c)
                  (let ((c2 (snoop-actual-char)))
                    (or (eq c2 :eof-object) (char= c2 #\Newline))))
             (emit "</span>") (scm-emit-html-char (get-actual-char))
             (return))
            (t (scm-emit-html-char c))))))

(defun scm-output-comment ()
  (funcall
   (if (tex2page-flag-boolean "\\TZPslatexcomments")
       #'scm-output-slatex-comment #'scm-output-verbatim-comment)))

(defun scm-output-extended-comment ()
  (get-actual-char) ;read the |
  (emit "<span class=comment>")
  (scm-emit-html-char #\#)
  (scm-emit-html-char #\|)
  (loop
    (let ((c (get-actual-char)))
      (cond ((eq c :eof-object) (return))
            ((char= c #\|)
             (let ((c2 (snoop-actual-char)))
               (cond ((eq c2 :eof-object) (scm-emit-html-char c)
                      (return))
                     ((char= c2 #\#) (get-actual-char)
                      (return))
                     (t (scm-emit-html-char c)))))
            (t (scm-emit-html-char c)))))
  (scm-emit-html-char #\|)
  (scm-emit-html-char #\#)
  (emit "</span>"))

(defun scm-output-string ()
  (get-actual-char)
  (emit "<span class=selfeval>")
  (scm-emit-html-char #\")
  (let ((esc-p nil))
    (loop
      (let ((c (get-actual-char)))
        (case c
          (#\" (unless esc-p (return))
           (scm-emit-html-char c)
           (setq esc-p nil))
          (#\\ (scm-emit-html-char c)
           (setq esc-p (not esc-p)))
          (t (scm-emit-html-char c)
             (setq esc-p nil))))))
  (scm-emit-html-char #\")
  (emit "</span>"))

(defun scm-output-hash ()
  (get-actual-char) ;read the #
  (let ((c (snoop-actual-char)))
    (cond ((eq c :eof-object) (emit "<span class=selfeval>")
           (scm-emit-html-char #\#) (emit "</span>"))
          ((char= c #\|) (scm-output-extended-comment))
          (t (toss-back-char #\#) (scm-output-token (scm-get-token))))))

(defun scm-output-token (s)
  (case (scm-get-type s)
    (:special-symbol
     (let ((*esc-char* *esc-char-std*))
       (tex2page-string (gethash s *scm-special-symbols*))))
    (:keyword
     (emit "<span class=keyword>")
     (scm-display-token s)
     (emit "</span>"))
    (:global
     (emit "<span class=global>")
     (scm-display-token s)
     (emit "</span>"))
    (:selfeval
     (emit "<span class=selfeval>")
     (scm-display-token s)
     (emit "</span>"))
    (:builtin
     (emit "<span class=builtin>")
     (scm-display-token s)
     (emit "</span>"))
    (:background (scm-display-token s))
    (t (emit "<span class=variable>") (scm-display-token s) (emit "</span>"))))

(defun scm-display-token (s)
  (let ((n (length s)) (k 0))
    (loop
      (unless (< k n) (return))
      (scm-emit-html-char (char s k))
      (incf k))))

(defun do-scm-braced (result-p)
  (get-actual-char)
  (let ((display-p (munched-a-newline-p)))
    (cond ((not display-p) (emit "<code class=scheme")
           (when result-p (emit "response")) (emit ">"))
          (t (do-end-para) (emit "<pre class=scheme>")))
    (bgroup)
    (let ((*esc-char* *esc-char-verb*) (*verb-display-p* display-p) (nesting 0))
      (loop
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object)
            (terror 'do-scm-braced "Eof inside verbatim"))
          (cond ((char= c *esc-char*)
                 (let ((x (let ((*not-processing-p* t))
                            (get-ctl-seq))))
                   (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                          (scm-emit-html-char (char x 1)))
                         (t (let ((*esc-char* *esc-char-std*))
                              (do-tex-ctl-seq-completely x))))))
                ((char= c #\{)
                 (get-actual-char)
                 (scm-emit-html-char c)
                 (incf nesting))
                ((char= c #\})
                 (get-actual-char)
                 (when (= nesting 0) (return))
                 (scm-emit-html-char c)
                 (decf nesting))
                (t (scm-output-next-chunk))))))
    (egroup)
    (if (not display-p)
        (emit "</code>")
      (progn (emit "</pre>")
             (do-noindent)))))

(defun do-scm-delimed (result-p)
  (let* ((d (get-actual-char))
         (display-p (munched-a-newline-p)))
    (cond ((not display-p) (emit "<code class=scheme")
           (when result-p (emit "response")) (emit ">"))
          (t (do-end-para) (emit "<pre class=scheme>")))
    (let ((*verb-display-p* display-p)
          (*scm-token-delims* (cons d *scm-token-delims*)))
      (loop
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object)
            (terror 'do-scm-delimed "Eof inside verbatim"))
          (when (char= c d) (get-actual-char) (return))
          (scm-output-next-chunk))))
    (cond ((not display-p) (emit "</code>"))
          (t (emit "</pre>") ;(do-para)
             (do-noindent)))))

(defun do-scm (&optional result-p)
  (cond (*outputting-external-title-p* (do-verb))
        (t (ignorespaces) (bgroup)
         (let ((*ligatures-p* nil))
           (funcall
            (if (char= (snoop-actual-char) #\{) #'do-scm-braced
                #'do-scm-delimed)
            result-p))
         (egroup))))

(defun do-scminput ()
  (ignorespaces)
  (do-end-para)
  (bgroup)
  (emit "<pre class=scheme>")
  (let ((f (add-dot-tex-if-no-extension-provided
             (get-filename-possibly-braced))))
    (call-with-input-file/buffered f
      (lambda ()
        (loop
          (let ((c (snoop-actual-char)))
            (when (eq c :eof-object) (return))
            (scm-output-next-chunk))))))
  (emit "</pre>")
  (egroup)
  (do-noindent))

(defun do-scmdribble ()
  (verb-ensure-output-port)
  (let ((*scm-dribbling-p* t))
    (do-scm nil))
  (terpri *verb-port*))

(defun do-scm-slatex-lines (env display-p result-p)
  (let ((endenv (concatenate 'string "\\end" env))
        (in-table-p (member (car *tabular-stack*) '(:block :figure :table))))
    (cond (display-p (do-end-para)) (in-table-p (emit "</td><td>")))
    (munched-a-newline-p)
    (bgroup)
    (emit "<div align=left><pre class=scheme")
    (when result-p (emit "response"))
    (emit ">")
    (let ((*ligatures-p* nil) (*verb-display-p* t) (*not-processing-p* t))
      (loop
        (let ((c (snoop-actual-char)))
          (when (eq c :eof-object)
            (terror 'do-scm-slatex-lines "Eof inside " env))
          (cond ((char= c #\Newline) (get-actual-char)
                 (scm-emit-html-char c)
                 (cond ((not (tex2page-flag-boolean "\\TZPslatexcomments")) nil)
                       ((char= (snoop-actual-char) #\;) (get-actual-char)
                        (if (char= (snoop-actual-char) #\;)
                            (toss-back-char #\;)
                          (scm-output-slatex-comment)))))
                ((char= c *esc-char*)
                 (let ((x (get-ctl-seq)))
                   (when (string= x endenv) (return))
                   (cond ((string= x "\\end")
                          (let ((g (get-grouped-environment-name-if-any)))
                            (when (and g (string= g env)) (egroup) (return))
                            (scm-output-token x)
                            (when g
                              (scm-output-token "{")
                              (scm-output-token g)
                              (scm-output-token "}"))))
                         (t (scm-output-token x)))))
                (t (scm-output-next-chunk))))))
    (emit "</pre></div>")
    (egroup)
    (cond (display-p (do-para)) (in-table-p (emit "</td><td>")))))

(defun string-is-all-dots-p (s)
  (dotimes (i (length s) t)
    (unless (char= (char s i) #\.) (return nil))))

(defun string-is-flanked-by-stars-p (s)
  (let ((n (length s)))
    (and (>= n 3)
         (char= (char s 0) (char s (1- n)) #\*))))

(defun scm-get-type (s)
  (cond ((gethash s *scm-special-symbols*) :special-symbol)
        ((gethash s *scm-keywords*) :keyword)
        ((gethash s *scm-builtins*) :builtin)
        ((gethash s *scm-variables*) :variable)
        ((string-is-flanked-by-stars-p s) :global)
        ((setq *it* (position #\: s :test #'char=))
         (if (= *it* 0) :selfeval :variable))
        ((string-is-all-dots-p s) :background) ;for Scheme's ellipsis
        ((char= (char s 0) #\#) :selfeval)
        ((string-to-number s) :selfeval)
        (t :variable)))

(defun eat-star ()
  (let ((c (snoop-actual-char)))
    (if (and (not (eq c :eof-object)) (char= c #\*)) (get-actual-char) nil)))

(defun do-cr (z)
  (ignorespaces)
  (case (car *tabular-stack*)
    (:tabular
     (get-bracketed-text-if-any)
     (egroup)
     (emit "</td></tr>")
     (emit-newline)
     (emit "<tr><td valign=top ")
     (do-tabular-multicolumn))
    (:eqnarray*
     (emit "</td></tr>")
     (emit-newline)
     (setq *equation-position* 0)
     (emit "<tr><td align=right>"))
    (:eqnarray
     (emit "</td>")
     (cond (*equation-numbered-p* (emit "<td>(") (emit *equation-number*)
                                  (bump-dotted-counter "equation") (emit ")</td>"))
           (t (setq *equation-numbered-p* t)))
     (emit "</tr>")
     (emit-newline)
     (setq *equation-position* 0)
     (emit "<tr><td align=right>"))
    (:ruled-table (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))
    ((:minipage :tabbing)
     (get-bracketed-text-if-any)
     (emit "<br>")
     (emit-newline))
    ((:eqalign :eqalignno :displaylines :pmatrix :mathbox)
     (unless (char= (snoop-actual-char) #\})
     (incf *math-height*)
       (emit "</td></tr>")
       (emit-newline)
       (emit "<tr><td align=center>")
       (setq *equation-position* 0)
       (emit-newline)))
    (:header (emit #\space))
    (t (when (and (eql *tex-format* :latex) (string= z "\\\\"))
         (get-bracketed-text-if-any)
         (let ((c (snoop-actual-char)))
           (when (and (not (eq c :eof-object)) (char= c #\*))
             (get-actual-char)))
         (emit "<br>")
         (emit-newline)))))

(defun do-ruledtable ()
  (push :ruled-table *tabular-stack*)
  (emit "<table border=2><tr><td>")
  (emit-newline))

(defun do-endruledtable ()
  (emit-newline)
  (emit "</td></tr></table>")
  (emit-newline)
  (pop-tabular-stack :ruled-table))

(defun do-tabular (&optional mathp)
  (do-end-para)
  (get-bracketed-text-if-any)
  (bgroup)
  (unless mathp
  (add-postlude-to-top-frame
   (let ((old-math-mode-p *math-mode-p*)
         (old-in-display-math-p *in-display-math-p*))
     (setq *math-mode-p* nil)
     (setq *in-display-math-p* nil)
     (lambda ()
       (setq *math-mode-p* old-math-mode-p)
       (setq *in-display-math-p* old-in-display-math-p)))))
  (let ((border-width (if (position #\| (get-group) :test #'char=) 1 0)))
    (push :tabular *tabular-stack*)
    (emit "<table border=")
    (emit border-width)
    (emit "><tr><td valign=top ")
    (do-tabular-multicolumn)))

(defun do-end-tabular ()
  (egroup)
  (do-end-para)
  (emit "</td></tr></table>")
  (pop-tabular-stack :tabular)
  (egroup))

(defun do-tabular-colsep ()
  (egroup)
  (emit "</td><td valign=top ")
  (do-tabular-multicolumn))

(defun do-tabular-multicolumn ()
  (loop
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (unless (and (characterp c) (char= c #\\)) (return))
      (let ((x (get-ctl-seq)))
        (cond ((string= x "\\hline") t)
              ((string= x "\\multicolumn")
               (let ((n (ungroup (get-token))))
                 (get-token)
                 (emit " colspan=")
                 (emit n)
                 (return)))
              (t (toss-back-char *invisible-space*)
                 (toss-back-string x)
                 (return))))))
  (emit ">")
  (bgroup))

(defun do-ruledtable-colsep ()
  (emit-newline)
  (emit "</td><td")
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (char= c #\\)
        (let ((x (get-ctl-seq)))
          (if (string= x "\\multispan")
              (let ((n (ungroup (get-token))))
                (emit " colspan=")
                (emit n))
              (toss-back-string x)))))
  (emit ">")
  (emit-newline))

(defun do-romannumeral (&optional upcase-p)
  (let ((n (get-number-or-false)))
    (when n
      (emit (number-to-roman n upcase-p)))))

(defun do-uppercase ()
  (emit (string-upcase (tex-string-to-html-string (get-token)))))

(defun set-latex-counter (addp)
  (let* ((counter-name (get-peeled-group))
         (new-value (read-from-string (get-token-or-peeled-group))))
    (cond ((setq *it* (gethash counter-name *dotted-counters*))
           (let ((counter *it*))
             (if addp
               (incf (counter*-value counter) new-value)
               (setf (counter*-value counter) new-value))))
          (t
            (let ((count-seq (concatenate 'string "\\" counter-name)))
              (cond ((setq *it* (section-ctl-seq-p count-seq))
                     (let ((n *it*))
                       (if addp
                         (incf (gethash n *section-counters* 0) new-value)
                         (setf (gethash n *section-counters*) new-value))))
                    ((find-count count-seq)
                     ;typically \secnumdepth, \tocdepth
                     (tex-gdef-count count-seq
                                    (if addp
                                      (+ new-value (get-gcount count-seq))
                                      new-value)))
                    (t ;error?
                      nil)))))))

(defun do-tex-prim (z)
  (cond ((setq *it* (find-def z))
         (let ((y *it*))
           (cond ((setq *it* (tdef*-defer y))
                  (let ((s *it*))
                    (toss-back-string s)))
                 ((setq *it* (tdef*-thunk y))
                  (let ((th *it*))
                    (funcall th)))
                 (t (expand-tex-macro
                      (tdef*-optarg y)
                      (tdef*-argpat y)
                      (tdef*-expansion y))))))
        ((setq *it* (section-ctl-seq-p z))
         (let ((n *it*))
           (do-heading n)))
        (*math-mode-p*
          (do-math-ctl-seq z))
        (t (trace-if (> (find-count "\\tracingcommands") 0)
                     "Ignoring " z))))

(defun do-char ()
  ;(emit-html-char (get-tex-char-spec))
  (let ((n (get-number-or-false)))
    (unless n (terror 'do-char "not a char"))
    (cond ((< n 128)
           ;this approach seems to fail in some CLs for higher chars;
           ;nevertheless, we want to keep it for the lower chars because
           ;these may be read back by tex2page
           (emit-html-char (code-char n)))
          (t (emit "&#x")
             (emit (write-to-string n :base 16))
             (emit ";")))))

(defun tex-math-bb (c)
  (case c
    ((#\C) "&#x2102;")
    ((#\H) "&#x210d;")
    ((#\N) "&#x2115;")
    ((#\P) "&#x2119;")
    ((#\Q) "&#x211a;")
    ((#\R) "&#x211d;")
    ((#\Z) "&#x2124;")
    (t (format nil "&#x~x;"
               (+ #x1d538 (- (char-int c) (char-int #\A)))))))

(defun tex-math-cal (c)
  (format nil "&#x~x;"
          (+ #x1d4d0 (- (char-int c) (char-int #\A)))))

(defun tex-math-frak (c)
  (format nil "&#x~x;"
          (+ #x1d56c (- (char-int c) (char-int #\A)))))

(defun emit-math-alpha-char (c)
  (case *math-font*
    ((:rm) (emit c))
    ((:bf) (emit "<b>") (emit c) (emit "</b>"))
    ((:bb) (emit (if (upper-case-p c) (tex-math-bb c) c)))
    ((:cal) (emit (if (upper-case-p c) (tex-math-cal c) c)))
    ((:frak) (emit (if (upper-case-p c) (tex-math-frak c) c)))
    (t (emit "<em>") (emit c) (emit "</em>"))))

(defun do-tex-char (c)
  (cond ((and *comment-char* (char= c *comment-char*)) (do-comment))
        ((inside-false-world-p) t)
        ((char= c #\{) (bgroup))
        ((char= c #\}) (egroup))
        ((char= c #\$) (do-math))
        ((char= c #\-) (do-hyphen))
        ((char= c #\`) (do-lsquo))
        ((char= c #\') (do-rsquo))
        ((char= c #\~) (emit-nbsp 1))
        ((char= c #\!) (do-excl)) ((char= c #\?) (do-quest))
        ((or (char= c #\<) (char= c #\>) (char= c #\")) (emit-html-char c))
        ((char= c #\&)
         (cond (*tabular-stack*
                ;(do-end-para) ;??? ;must check why this is needed
                (case (car *tabular-stack*)
                  ((:pmatrix :eqalign :displaylines :mathbox)
                   (emit "&nbsp;</td><td align=center>&nbsp;"))
                  (:eqalignno
                   (setq *equation-position* (+ *equation-position* 1))
                   (emit "</td><td")
                   (when (= *equation-position* 2) (emit " width=30% align=right"))
                   (emit ">"))
                  ((:eqnarray :eqnarray*)
                   (setq *equation-position* (+ *equation-position* 1))
                   (emit "</td><td")
                   (when (= *equation-position* 1) (emit " align=center width=2%"))
                   (emit ">"))
                  (:tabular (do-tabular-colsep))
                  (:ruled-table (do-ruledtable-colsep))))
               (t (emit-html-char c))))
        ((char= c #\|)
         (if (eq (car *tabular-stack*) :ruled-table)
             (do-ruledtable-colsep) (emit c)))
        ((char= c #\Newline) (do-newline)) ((char= c #\space) (do-space))
        ((char= c #\tab) (do-tab))
        (t
         (cond (*math-mode-p*
                (case c
                  (#\^ (do-sup))
                  (#\_ (do-sub))
                  ((#\+ #\=)
                   (unless *math-script-mode-p* (emit #\space))
                   (emit c)
                   (unless *math-script-mode-p* (emit #\space)))
                  (t (if (alpha-char-p c)
                       (emit-math-alpha-char c)
                       (emit c)))))
               ((and *in-small-caps-p* (lower-case-p c)) (emit "<small>")
                (emit (char-upcase c)) (emit "</small>"))
               (t (emit c))))))

(defun do-tex-ctl-seq-completely (x)
  (cond ((setq *it* (resolve-defs x))
         (tex2page-string *it*))
        ((setq *it* (do-tex-prim (find-corresp-prim x)))
         (when (eq *it* :encountered-undefined-command)
           (emit x)))))

(defun inside-false-world-p ()
  (or (member nil *tex-if-stack*) (member '? *tex-if-stack*)))

(defun do-tex-ctl-seq (z)
  ;process ctl seq z.  Return :encountered-bye if z is \bye;
  ;:encountered-endinput if z is \endinput
  (trace-if (> (find-count "\\tracingcommands") 0) z)
  (cond ((setq *it* (resolve-defs z))
         (let ((s *it*))
           (trace-if (> (find-count "\\tracingmacros") 0)
                     "    --> " s)
           (toss-back-char *invisible-space*)
           (toss-back-string s)))
        ((and (inside-false-world-p)
              (not (if-aware-ctl-seq-p z)))
         nil)
        ((string= z "\\enddocument")
         (probably-latex)
         :encountered-bye)
        ((member z '("\\bye" "\\TIIPbye") :test #'string=)
         :encountered-bye)
        ((string= z "\\endinput")
         (let ((next-token (get-token)))
           (when (and (not (eq next-token :eof-object))
                      (string= next-token "\\fi"))
             (do-fi)))
         :encountered-endinput)
        ((find-count z) (do-count= z nil))
        ((find-toks z) (do-toks= z nil))
        ((find-dimen z) (do-dimen= z nil))
        (t (do-tex-prim z))))

(defun generate-html ()
  (let ((*outer-p* t))
    (loop
      (let ((c (snoop-actual-char)))
        (cond ((eq c :eof-object) (return t))
              ((setq *it* (resolve-chardefs c))
               (let ((s *it*))
                 (toss-back-char *invisible-space*)
                 (toss-back-string s)))
              ((char= c *esc-char*)
               (case (do-tex-ctl-seq (get-ctl-seq))
                 (:encountered-endinput (return t))
                 (:encountered-bye (return :encountered-bye))
                 (t t)))
              (t (get-actual-char)
                 (do-tex-char c)))))))

(defun do-iffileexists ()
  (let* ((file (actual-tex-filename (get-filename-possibly-braced)))
         (then-e (ungroup (get-group)))
         (else-e (ungroup (get-group))))
    (tex2page-string (if file then-e else-e))))

(defun check-input-file-timestamp-p (f)
  (cond ((let ((e (file-extension f)))
           (and e (member e '(".t2p" ".bbl" ".ind") :test 'string=)))
         ;ignore timestamps of .t2p .ind .bbl files
         nil)
        (*inputting-boilerplate-p* nil)
        (*ignore-timestamp-p* nil)
        ((> *html-only* 0) nil)
        ((and (>= (length f) 3) (char= (char f 0) #\.) (char= (char f 1) #\/)) nil)
        ((member f *verb-written-files* :test #'equal) nil)
        (t t)))

(defun do-inputiffileexists ()
  (let* ((f (actual-tex-filename (get-filename-possibly-braced)))
         (then-txt (ungroup (get-group)))
         (else-txt (ungroup (get-group))))
    (cond (f (tex2page-string then-txt) (tex2page-file f))
          (t (tex2page-string else-txt)))))

(defun tex2page-file (f)
  (write-log #\()
  (write-log f)
  (write-log :separation-space)
  (setq f (tex2page-massage-file f))
  (trace-if (> (find-count "\\tracingcommands") 0) "Inputting file " f)
  (prog1
      (call-with-input-file/buffered f #'generate-html)
    (write-log #\))
    (write-log :separation-space)))

(defun tex2page-file-if-exists (f)
  (when (probe-file f) (tex2page-file f)))

(defun do-input ()
  (ignorespaces)
  (let* ((f (get-filename-possibly-braced))
         (boilerplate-index *inputting-boilerplate-p*))
    ;this is ugly code that nobody really needs, so why am I so
    ;invested in keeping it?
    (when (eql *inputting-boilerplate-p* 0)
      (setq *inputting-boilerplate-p* nil))
    (let ((*inputting-boilerplate-p*
           (and boilerplate-index (1+ boilerplate-index))))
      (cond ((or (latex-style-file-p f)
                 (member f
                         '("btxmac" "btxmac.tex" "eplain" "eplain.tex" "epsf" "epsf.tex"
                           "eval4tex" "eval4tex.tex" "supp-pdf" "supp-pdf.tex"
                           "tex2page" "tex2page.tex"
                           ) :test #'string-equal))
             ;dont process .sty files and macro files like btxmac.tex
             nil)
            ((member f '("miniltx" "miniltx.tex") :test #'string-equal)
             ;like above, but miniltx also makes @ a "letter"
             (set-catcode #\@ 11) nil)
            ((member f '("texinfo" "texinfo.tex") :test #'string-equal)
             (let ((txi2p (actual-tex-filename "texi2p")))
               (unless txi2p
                 (terror 'do-input "File texi2p.tex not found"))
               (tex2page-file txi2p)
               (tex2page-file *current-source-file*)
               :encountered-endinput))
            ((setq *it* (actual-tex-filename f (check-input-file-timestamp-p f)))
             (tex2page-file *it*))
            (t (write-log #\() (write-log f)
               (write-log :separation-space) (write-log "not found)")
               (write-log :separation-space))))))

(defun do-includeonly ()
  (ignorespaces)
  (when (eq *includeonly-list* t) (setq *includeonly-list* '()))
  (let ((c (get-actual-char)))
    (when (or (eq c :eof-object) (not (char= c #\{)))
      (terror 'do-includeonly)))
  (let ((*filename-delims* (cons #\} (cons #\, *filename-delims*))))
    (loop
      (ignorespaces)
      (let ((c (snoop-actual-char)))
        (when (eq c :eof-object) (terror 'do-includeonly))
        (cond ((and *comment-char* (char= c *comment-char*))
               (eat-till-eol)  )
              ((char= c #\,) (get-actual-char))
              ((char= c #\}) (get-actual-char) (return))
              ((member c  *filename-delims* :test #'char=)
               (terror 'do-includeonly))
              (t (push (get-plain-filename)
                       *includeonly-list*)))))))

(defun do-include ()
  (let ((f (ungroup (get-group))))
    (when (or (eq *includeonly-list* t)
              (member f *includeonly-list* :test #'string=))
      (let ((*subjobname* (file-stem-name f))
            (*img-file-count* 0)
            (*imgdef-file-count* 0))
        (tex2page-file
         (actual-tex-filename f (check-input-file-timestamp-p f)))))))

(defun do-eval-string (s)
  (with-input-from-string (i s)
    (loop
      (let ((x (read i nil :eof-object)))
        (when (eq x :eof-object) (return))
        (eval x)))))

(defun do-eval (kind)
  (let ((s (if *outer-p*
             (ungroup
              (let ((*esc-char* *esc-char-verb*)
                    (*expand-escape-p* t))
                (get-group)))
             (tex-write-output-string
               (ungroup (get-group)))
             )))
    (unless (inside-false-world-p)
      (when (> *html-only* 0) (setq kind :html))
      (case kind
        (:html
         (let ((o (make-string-output-stream)))
           (let ((*standard-output* o))
             (do-eval-string s))
           (tex2page-string (get-output-stream-string o))))
        (:quiet (do-eval-string s))
        (t (incf *eval-file-count*)
           (let ((eval4tex-file
                  (concatenate 'string *jobname* *eval-file-suffix*
                    (write-to-string *eval-file-count*) ".tex")))
             (with-open-file (o eval4tex-file :direction :output
                                :if-exists :supersede)
               (let ((*standard-output* o))
                 (do-eval-string s)
                 ;following eats the whitespace that creeps in after
                 ;\input file
                 (princ "\\relax")))
             (let ((*ignore-timestamp-p* t))
               (tex2page-file eval4tex-file))))))))

(defun eval-for-tex-only ()
  (setq *eval-for-tex-only-p* t)
  (do-end-page)
  (ensure-file-deleted *html-page*) ;??
  (setq *main-tex-file* nil)
  (setq *html-page* ".eval4texignore")
  (setq *html* (open *html-page* :direction :output
                     :if-exists :supersede)))

(defun expand-ctl-seq-into-string (cs)
  (let ((tmp-port (make-string-output-stream)))
    (let ((*html* tmp-port))
      (do-tex-ctl-seq cs))
    (get-output-stream-string tmp-port)))

(defun call-with-html-output-going-to (p th)
  (let ((*html* p))
    (funcall th)))

(defun call-external-programs-if-necessary ()
  (let ((run-bibtex-p
         (cond ((not *using-bibliography-p*) nil)
               ((not (probe-file
                  (concatenate 'string *aux-dir/* *jobname*
                               *bib-aux-file-suffix* ".aux")))
                nil)
               ((member :bibliography *missing-pieces*) t)
               (*source-changed-since-last-run-p*
                (flag-missing-piece :fresh-bibliography) t)
               (t nil)))
        (run-makeindex-p
         (cond ((not *using-index-p*) nil)
               ((not (probe-file
                  (concatenate 'string *aux-dir/* *jobname* *index-file-suffix*
                               ".idx")))
                nil)
               ((member :index *missing-pieces*) t)
               (*source-changed-since-last-run-p*
                (flag-missing-piece :fresh-index) t)
               (t nil))))
    (when run-bibtex-p
      (write-log :separation-newline)
      (write-log "Running: bibtex ")
      (write-log *aux-dir/*)
      (write-log *jobname*)
      (write-log *bib-aux-file-suffix*)
      (write-log #\space)
      (system
       (concatenate 'string "bibtex " *aux-dir/* *jobname*
                    *bib-aux-file-suffix*))
      (unless
          (probe-file
           (concatenate 'string *jobname* *bib-aux-file-suffix* ".bbl"))
        (write-log " ... failed; try manually"))
      (write-log :separation-newline))
    (when run-makeindex-p
      (write-log :separation-newline)
      (write-log "Running: makeindex ")
      (write-log *aux-dir/*)
      (write-log *jobname*)
      (write-log *index-file-suffix*)
      (write-log #\space)
      (system
       (concatenate 'string "makeindex " *aux-dir/* *jobname*
                    *index-file-suffix*))
      (unless
          (probe-file
           (concatenate 'string *aux-dir/* *jobname* *index-file-suffix*
                        ".ind"))
        (write-log " ... failed; try manually"))
      (write-log :separation-newline))
    (mapc
     (lambda (f)
         (when (probe-file f)
           (write-log :separation-newline)
           (write-log "Running: metapost ")
           (write-log f)
           (write-log :separation-newline)
           (system (concatenate 'string *metapost* " " f))))
     *mp-files*)
    (mapc
     (lambda (eps-file+img-file-stem)
         (retry-lazy-image (car eps-file+img-file-stem)
          (cdr eps-file+img-file-stem)))
     *missing-eps-files*)))

(defun first-file-that-exists (&rest ff)
  (dolist (f ff)
    (when (probe-file f) (return f))))

(defun file-in-home (f)
  (let ((home (retrieve-env "HOME")))
    (and home
         (let ((slash-already-p
                (let ((n (length home)))
                  (and (>= n 0)
                       (let ((c (char home (- n 1))))
                         (or (char= c #\/) (char= c #\\)))))))
           (concatenate 'string home (if slash-already-p "" "/") f)))))

(defun make-target-dir ()
  (let ((hdir-file
         (first-file-that-exists
          (concatenate 'string *jobname* ".hdir")
          ".tex2page.hdir" (file-in-home ".tex2page.hdir"))))
    (when hdir-file
      (let ((hdir
             (call-with-input-file/buffered hdir-file
               (lambda () (get-filename-possibly-braced)))))
        (unless (= (length hdir) 0)
          #-windows
          (progn
            (system (concatenate 'string "mkdir -p " hdir))
            (system (concatenate 'string "touch " hdir "/probe")))
          #+windows
          (progn
            (system (concatenate 'string "mkdir " hdir))
            (system (concatenate 'string "echo probe > " hdir "\\probe")))
          (let ((probe (concatenate 'string hdir "/probe")))
            (when (probe-file probe)
              (ensure-file-deleted probe)
              (setq *aux-dir* hdir
                    *aux-dir/* (concatenate 'string *aux-dir* "/")))))))))

(defun move-aux-files-to-aux-dir (f)
  (when (and *aux-dir*
             (or (probe-file (concatenate 'string f ".tex"))
                 (probe-file (concatenate 'string f ".scm"))
                 (probe-file (concatenate 'string f (find-img-file-extn)))))
    #-windows (system (concatenate 'string "mv " f ".* " *aux-dir*))
    #+windows
    (progn
      (system (concatenate 'string "copy " f ".* " *aux-dir*))
      (when
        (or (probe-file (concatenate 'string f ".tex"))
            (probe-file (concatenate 'string f ".scm")))
        (system (concatenate 'string "del " f ".*"))))))

(defun start-css-file ()
  (let ((css-file (concatenate 'string *aux-dir/* *jobname* *css-file-suffix*)))
    (setq *css-port* (open css-file :direction :output
                           :if-exists :supersede))
    (princ "
               body {
               color: black;
               /*   background-color: #e5e5e5;*/
               background-color: #ffffff;
               /*background-color: beige;*/
               margin-top: 2em;
               margin-bottom: 2em;
               /* margin-left: 8%;
               margin-right: 8%; */
               }

               @media screen {
               body {
               margin-left: 8%;
               margin-right: 8%;
               }
               }

               @media print {
               body {
               text-align: justify;
               }
               }

               @media print {
               a:link, a:visited {
               text-decoration: none;
               color: black;
               }
               }

               /* @media print {
               p {
               text-indent: 2em;
               margin-top: 1ex;
               margin-bottom: 0;
               }

               } */

               h1,h2,h3,h4,h5,h6 {
               margin-top: .8em;
               margin-bottom: .2em;  /* ?? */
               }

               .title {
               font-size: 200%;
               font-weight: normal;
               margin-top: 2.8em;
               text-align: center;
               }

               .partheading {
               font-size: 100%;
               }

               .chapterheading {
               font-size: 100%;
               }

               .beginchapter,.beginsection {
               font-family: sans-serif;
               }

               .beginchapter {
               margin-top: 1.8em;
               font-size: 150%;
               }

               .beginsection {
               margin-top: 1.8em;
               font-size: 110%;
               }

               .tiny {
               font-size: 40%;
               }

               .scriptsize {
               font-size: 60%;
               }

               .footnotesize {
               font-size: 75%;
               }

               .small {
               font-size: 90%;
               }

               .normalsize {
               font-size: 100%;
               }

               .large {
               font-size: 120%;
               }

               .largecap {
               font-size: 150%;
               }

               .largeup {
               font-size: 200%;
               }

               .huge {
               font-size: 300%;
               }

               .hugecap {
               font-size: 350%;
               }

               p.noindent {
               text-indent: 0;
               margin-top: 0;
               }

               p.nopadding {
               margin-top: 0;
               }

               pre {
               overflow: auto;
               margin-left: 2em;
               background-color: #f5f5f5;
               }

               blockquote {
               background-color: #f0e0e0;
               margin-top: 2pt;
               margin-bottom: 2pt;
               margin-left: 2em;
               }

               .smallskip {
               margin-top: 2pt;
               margin-bottom: 2pt;
               min-height: 4pt;
               }

               .medskip {
               margin-top: 3pt;
               margin-bottom: 3pt;
               min-height: 7pt;
               /*margin-top: 1.6em;
               margin-bottom: 2.4em;
               margin-top: 1em;
               margin-bottom: 1.5em; */
               /* top and bottom have to be different so successive \\...skips cause more spacing? */
               }

               .bigskip {
               margin-top: 4pt;
               margin-bottom: 4pt;
               min-height: 13pt;
               /*margin-top: 2.8em;
               margin-bottom: 3.4em;
               margin-top: 2.4em;
               margin-bottom: 1.6em; */
               }

               ol {
               list-style-type: decimal;
               }

               ol ol {
               list-style-type: lower-alpha;
               }

               ol ol ol {
               list-style-type: lower-roman;
               }

               ol ol ol ol {
               list-style-type: upper-alpha;
               }

               /*
               tt i {
               font-family: serif;
               }

               .verbatim em {
               font-family: serif;
               }*/

               /*
               .verbatim {
               color: #4d0000;
               }
               */

               .scheme em {
               color: black;
               font-family: serif;
               }

               .scheme             {color: #553366} /* background punctuation, was 993333 */
               .scheme  .selfeval  {color: #006600}
               .scheme  .keyword   {color: #660000; font-weight: bold}
               .scheme  .builtin   {color: #660000}
               .scheme  .global    {color: #660066}
               .scheme  .variable  {color: #000066}
               .scheme  .comment   {color: #006666; font-style: oblique}

               .schemeresponse {
               color: #006600;
               }

               a:hover {
               text-decoration: none;
               background-color: yellow;
               }

               .navigation {
               color: #993300;
               text-align: right;
               font-size: medium;
               font-style: italic;
               }

               @media print {
               .navigation {
               display: none;
               }
               }

               .disable {
               /* color: #e5e5e5; */
               color: gray;
               }

               .smallcaps {
               font-size: 75%;
               }

               .smallprint {
               color: gray;
               font-size: 75%;
               text-align: right;
               }

               /*
               .smallprint hr {
               text-align: left;
               width: 40%;
               }
               */

               .footnoterule {
               text-align: left;
               width: 40%;
               }

               @media print {
               .footnoterule {
               margin-top: 2em;
               }
               }

               .colophon {
               color: gray;
               font-size: 80%;
               font-style: italic;
               text-align: right;
               margin-top: 1em;
               }

               @media print {
               .colophon .advertisement {
               display: none;
               }
               }

               .colophon a {
               color: gray;
               text-decoration: none;
               }
               "
     *css-port*)))

(defun load-aux-file ()
  (set-start-time)
  (let ((label-file
         (concatenate 'string *aux-dir/* *jobname* *label-file-suffix* ".scm")))
    (when (probe-file label-file)
      (load-tex2page-data-file label-file)
      (delete-file label-file)))
  (unless (string= *jobname* "texput")
    (let ((texput-aux (concatenate 'string "texput" *aux-file-suffix* ".scm")))
      (when (probe-file texput-aux) (delete-file texput-aux))))
  (let ((aux-file
         (concatenate 'string *aux-dir/* *jobname* *aux-file-suffix* ".scm")))
    (when (probe-file aux-file)
      (load-tex2page-data-file aux-file)
      (delete-file aux-file)))
  (start-css-file)
  (unless (null *toc-list*) (setq *toc-list* (nreverse *toc-list*)))
  (unless (null *stylesheets*) (setq *stylesheets* (nreverse *stylesheets*)))
  (unless (null *html-head*) (setq *html-head* (nreverse *html-head*))))

(defun update-last-modification-time (f)
  (let ((s (file-write-date f)))
    (when (and s (or (not *last-modification-time*)
                     (> s *last-modification-time*)))
      (setq *source-changed-since-last-run-p* t
            *last-modification-time* s)
      (when (and (tex2page-flag-boolean "\\TZPcolophontimestamp")
                 (not (tex2page-flag-boolean "\\TZPcolophonlastpage"))
                 (> *html-page-count* 1))
        ;i.e. time to print mod-time is past, and this update is too
        ;late
        (flag-missing-piece :last-modification-time)))))

(defun probably-latex ()
  (when (null *tex-env*)
    (incf *latex-probability* 1)
    (if (>= *latex-probability* 2) (definitely-latex))))

(let ((already-noted-p nil))
  (defun definitely-latex ()
    (unless already-noted-p
      (setq already-noted-p t)
      (!definitely-latex)
      (write-aux '(!definitely-latex)))))

;the following are used to make entries in the aux file.  Their names
;all begin with "!"

(defun !tex-like-layout () (setq *tex-like-layout-p* t))

(defun !head-line (e) (tex-def-toks "\\headline" e t))

(defun !foot-line (e) (tex-def-toks "\\footline" e t))

(defun !toc-page (p) (setq *toc-page* p))

(defun !index-page (p) (setq *index-page* p))

(defun !toc-entry (level number page label header)
  (push
   (make-tocentry* :level level :number number :page page :label label
                   :header header)
   *toc-list*))

(defun !label (label html-page name value)
  (setf (gethash label *label-table*)
   (make-label* :src *label-source* :page html-page :name name :value value)))

(defun !index (index-number html-page-number)
  (setf (gethash index-number *index-table*) html-page-number))

(defun !last-modification-time (s &optional (epoch 1900))
  (declare (type (member 1900 1970) epoch))
  (setq *last-modification-time*
        (ecase epoch
          (1900 s)
          (1970 (+ s #.(encode-universal-time 0 0 0 1 1 1970 0))))))

(defun !last-page-number (n) (setq *last-page-number* n))

(defun !using-chapters () (setq *using-chapters-p* t))

(defun !definitely-latex ()
  (setq *tex-format* :latex)
  (when (< (get-gcount "\\secnumdepth") -1) (tex-gdef-count "\\secnumdepth" 3)))

(defun !using-external-program (x)
  ;obsolete
  (declare (ignore x))
  nil)

(defun !external-labels (f)
  ;obsolete
  (declare (ignore f))
  nil)

(defun !doctype (d) (setq *doctype* d))

(defun !colophon (x)
  (case x
    (:last-page (tex-def-0arg "\\TZPcolophonlastpage" "1"))
    (:no-timestamp (tex-def-0arg "\\TZPcolophontimestamp" "0"))
    ((:dont-credit-tex2page :ingrate) (tex-def-0arg "\\TZPcolophoncredit" "0"))
    (:dont-link-to-tex2page-website
     (tex-def-0arg "\\TZPcolophonweblink" "0"))))

(defun !tex-text (n)
  (when (= n 0)
    (setq *ligatures-p* nil)))

(defun fully-qualified-url-p (u) (or (search "//" u) (char= (char u 0) #\/)))

(defun fully-qualified-pathname-p (f)
  (let ((n (length f)))
    (if (= n 0) t
        (let ((c0 (char f 0)))
          (cond ((char= c0 #\/) t) ((= n 1) nil)
                ((and (alpha-char-p c0) (char= (char f 1) #\:)) t) (t nil))))))

(defun ensure-url-reachable (f)
  (if (and *aux-dir* (not (fully-qualified-url-p f)) (not (search "/" f)))
    (let ((real-f (concatenate 'string *aux-dir/* f)))
      (when (and (probe-file f) (not (probe-file real-f)))
        #-windows
        (system (concatenate 'string "cp -p " f " " real-f))
        #+windows
        (system (concatenate 'string "copy/b " f " " *aux-dir*)))
      real-f)
    f))

(defun !stylesheet (css)
  (if (probe-file (ensure-url-reachable css))
      (push css *stylesheets*)
    (progn
     (write-log "! Can't find stylesheet ")
     (write-log css)
     (write-log :separation-newline))))

(defun !html-head (s)
  (push s *html-head*))

(defun !html-redirect (url seconds)
  (setq *redirect-url* url
        *redirect-delay* seconds)
  (!html-head (concatenate 'string
                           "<meta http-equiv=\"refresh\" content=\""
                           seconds ";"
                           url "\">")))

(defun !default-title (title) (unless *title* (setq *title* title)))

(defun !preferred-title (title) (setq *title* title))

(defun !infructuous-calls-to-tex2page (n)
  (setq *infructuous-calls-to-tex2page* n))

(defun load-tex2page-data-file (f)
  (with-open-file (i f :direction :input :if-does-not-exist nil)
    (when i
      (let ((*current-source-file* f) (*input-line-no* 0))
        (loop
          (let* ((e (read i nil nil))
                 (directive (car e)))
            (unless e (return))
            (incf *input-line-no*)
            (apply
             (case directive
               ((!colophon
                 !default-title
                 !definitely-latex
                 !doctype
                 !external-labels
                 !foot-line
                 !head-line
                 !html-head
                 !html-redirect
                 !index
                 !index-page
                 !infructuous-calls-to-tex2page
                 !label
                 !last-modification-time
                 !last-page-number
                 !preferred-title
                 !stylesheet
                 !tex-like-layout
                 !tex-text
                 !toc-entry
                 !toc-page
                 !using-chapters
                 !using-external-program) (symbol-function directive))
               (t (terror 'load-tex2page-data-file
                          "Fatal aux file error; I'm stymied.")))
             (cdr e))))))))

(defun tex2page-massage-file (f)
  ;can be redefined to process formats like Texinfo that use different
  ;special characters than regular TeX
  f)

(defun tex2page-help (not-a-file)
  (unless not-a-file (setq not-a-file "--missing-arg"))
  (write-aux
   `(!infructuous-calls-to-tex2page ,(+ *infructuous-calls-to-tex2page* 1)))
  (unless (or (string= not-a-file "--help")
              (string= not-a-file "--missing-arg")
              (string= not-a-file "--version"))
    (write-log "! I can't find file `")
    (write-log not-a-file)
    (write-log "'.")
    (write-log :separation-newline))
  (cond ((string= not-a-file "--version")
         (write-log *tex2page-copyright-notice*)
         (write-log "

Permission to distribute and use this work for any
purpose is hereby granted provided this copyright
notice is included in the copy.  This work is provided
as is, with no warranty of any kind.

For information on how to use TeX2page, please see")
         (write-log #\Newline) (write-log *tex2page-website*) (write-log #\.)
         (write-log #\Newline) (write-log #\Newline))
        ((string= not-a-file "--help")
         (write-log "
The command tex2page converts a (La)TeX document into
Web pages.  Call tex2page with the relative or full
pathname of the main TeX file.  The file extension
is optional if it is .tex.

The relative pathnames of the main and any subsidiary
TeX files are resolved against the current working
directory and the list of directories in the
environment variable TEX2PAGEINPUTS, or if that does not
exist, via kpsewhich(1).

The output Web files are generated in the current
directory by default.  An alternate location can be
specified in  <jobname>.hdir, tex2page.hdir, or
~/tex2page.hdir, where <jobname> is the basename of the
main TeX file.

For more information on how to use tex2page, please see")
         (write-log #\Newline) (write-log *tex2page-website*) (write-log #\.)
         (write-log #\Newline) (write-log #\Newline))
        (t (when (string= not-a-file "--missing-arg")
             (write-log "! Missing command-line argument.")
             (write-log :separation-newline))
           (when (> *infructuous-calls-to-tex2page* 0)
             (write-log "You have called TeX2page")
             (write-log #\space)
             (write-log (1+ *infructuous-calls-to-tex2page*))
             (write-log #\space)
             (write-log "times without a valid input document.")
             (write-log :separation-newline))
           (cond ((>= *infructuous-calls-to-tex2page* 4)
                  (write-log "I can't go on meeting you like this.")
                  (write-log :separation-newline) (write-log "Good bye!")
                  (write-log :separation-newline))
                 (t (write-log "Do you need help using TeX2page?
Try the commands
  tex2page --help
  tex2page --version")
                    (write-log :separation-newline)))))
  (close-all-open-ports))

(defun non-fatal-error (&rest ss)
  (emit-link-start (concatenate 'string *jobname* ".hlog"))
  ;x2692 won't print on lynx
  (emit "<span style=\"color: red\">&#x2388;&nbsp;")
  (mapc #'emit-html-string ss)
  (emit-link-stop)
  (emit "</span>"))

(defun do-math-ctl-seq (s)
  (cond ((setq *it* (find-math-def s))
         (funcall (tdef*-thunk *it*)))
        (t (unless *math-needs-image-p*
             (setq *math-needs-image-p* t))
           (emit (subseq s 1)))))

(defun tex-def-math-prim (cs thunk)
  (tex-def cs '() nil nil thunk cs nil *math-primitive-texframe*))

(defun make-reusable-math-image-as-needed (cs &rest expn)
  (let ((expn (if (null expn) cs (car expn))))
    (tex-def-math-prim cs
     (lambda ()
         (tex2page-string
          (concatenate 'string "\\global\\imgdef" cs "{$" expn "$}"))
         (tex2page-string cs)))))

;TeXbook, appendix F, p 434

;1. lowercase Greek

(tex-def-math-prim "\\alpha" (lambda () (emit "&alpha;")))
(tex-def-math-prim "\\beta" (lambda () (emit "&beta;")))
(tex-def-math-prim "\\gamma" (lambda () (emit "&gamma;")))
(tex-def-math-prim "\\delta" (lambda () (emit "&delta;")))
(tex-def-math-prim "\\epsilon" (lambda () (emit "&epsilon;")))
(tex-def-math-prim "\\varepsilon" (lambda () (emit "&epsilon;")))
(tex-def-math-prim "\\zeta" (lambda () (emit "&zeta;")))
(tex-def-math-prim "\\eta" (lambda () (emit "&eta;")))
(tex-def-math-prim "\\theta" (lambda () (emit "&theta;")))
(tex-def-math-prim "\\vartheta" (lambda () (emit "&thetasym;")))
(tex-def-math-prim "\\iota" (lambda () (emit "&iota;")))
(tex-def-math-prim "\\kappa" (lambda () (emit "&kappa;")))
(tex-def-math-prim "\\lambda" (lambda () (emit "&lambda;")))
(tex-def-math-prim "\\mu" (lambda () (emit "&mu;")))
(tex-def-math-prim "\\nu" (lambda () (emit "&nu;")))
(tex-def-math-prim "\\xi" (lambda () (emit "&xi;")))
(tex-def-math-prim "\\omicron" (lambda () (emit "&omicron;")))
(tex-def-math-prim "\\pi" (lambda () (emit "&pi;")))
(tex-def-math-prim "\\varpi" (lambda () (emit "&piv;")))
(tex-def-math-prim "\\rho" (lambda () (emit "&rho;")))
(tex-def-math-prim "\\varrho" (lambda () (emit "&rho;")))
(tex-def-math-prim "\\sigma" (lambda () (emit "&sigma;")))
(tex-def-math-prim "\\varsigma" (lambda () (emit "&sigmaf;")))
(tex-def-math-prim "\\tau" (lambda () (emit "&tau;")))
(tex-def-math-prim "\\upsilon" (lambda () (emit "&upsilon;")))
(tex-def-math-prim "\\phi" (lambda () (emit "&phi;")))
(tex-def-math-prim "\\varphi" (lambda () (emit "&#x3d5;")))
(tex-def-math-prim "\\chi" (lambda () (emit "&chi;")))
(tex-def-math-prim "\\psi" (lambda () (emit "&psi;")))
(tex-def-math-prim "\\omega" (lambda () (emit "&omega;")))

;2. uppercase Greek

(tex-def-math-prim "\\Gamma" (lambda () (emit "&Gamma;")))
(tex-def-math-prim "\\Delta" (lambda () (emit "&Delta;")))
(tex-def-math-prim "\\Theta" (lambda () (emit "&Theta;")))
(tex-def-math-prim "\\Lambda" (lambda () (emit "&Lambda;")))
(tex-def-math-prim "\\Xi" (lambda () (emit "&Xi;")))
(tex-def-math-prim "\\Pi" (lambda () (emit "&Pi;")))
(tex-def-math-prim "\\Sigma" (lambda () (emit "&Sigma;")))
(tex-def-math-prim "\\Upsilon" (lambda () (emit "&Upsilon;")))
(tex-def-math-prim "\\Phi" (lambda () (emit "&Phi;")))
(tex-def-math-prim "\\Psi" (lambda () (emit "&Psi;")))
(tex-def-math-prim "\\Omega" (lambda () (emit "&Omega;")))

;4. misc symbols of type Ord

(tex-def-math-prim "\\aleph" (lambda () (emit "&alefsym;")))
(tex-def-math-prim "\\hbar" (lambda () (emit "&#x210f;")))
(tex-def-math-prim "\\imath" (lambda () (emit "<i>&#x131;</i>"))) ; #x1d6a4 not supported yet
(tex-def-math-prim "\\jmath" (lambda () (emit "<i>&#x237;</i>"))) ; #x1d6a5
(tex-def-math-prim "\\ell" (lambda () (emit "&#x2113;")))
(tex-def-math-prim "\\wp" (lambda () (emit "&weierp;")))
(tex-def-math-prim "\\Re" (lambda () (emit "&real;")))
(tex-def-math-prim "\\Im" (lambda () (emit "&image;")))
(tex-def-math-prim "\\partial" (lambda () (emit "&part;")))
(tex-def-math-prim "\\infty" (lambda () (emit "&infin;")))
(tex-def-math-prim "\\prime" (lambda () (emit "&frasl;")))
(tex-def-math-prim "\\emptyset" (lambda () (emit "&empty;")))
(tex-def-math-prim "\\nabla" (lambda () (emit "&nabla;")))
(tex-def-math-prim "\\surd" (lambda () (emit "&radic;")))
(tex-def-math-prim "\\top" (lambda () (emit "&#x22a4;")))
(tex-def-math-prim "\\bot" (lambda () (emit "&#x22a5;")))
(tex-def-math-prim "\\|" (lambda () (emit "&#x2225;")))
(tex-def-math-prim "\\angle" (lambda () (emit "&ang;")))
(tex-def-math-prim "\\triangle" (lambda () (emit "&Delta;")))
(tex-def-math-prim "\\backslash" (lambda () (emit "\\")))
(tex-def-math-prim "\\forall" (lambda () (emit "&forall;")))
(tex-def-math-prim "\\exists" (lambda () (emit "&exist;")))
(tex-def-math-prim "\\neg" (lambda () (emit "&not;")))
(tex-def-math-prim "\\flat" (lambda () (emit "&#x266d;")))
(tex-def-math-prim "\\natural" (lambda () (emit "&#x266e;")))
(tex-def-math-prim "\\sharp" (lambda () (emit "&#x266f;")))
(tex-def-math-prim "\\clubsuit" (lambda () (emit "&clubs;")))
(tex-def-math-prim "\\diamondsuit" (lambda () (emit "&#x2662;")))
(tex-def-math-prim "\\heartsuit" (lambda () (emit "&#x2661;")))
(tex-def-math-prim "\\spadesuit" (lambda () (emit "&spades;")))

;6. large operators

(tex-def-math-prim "\\sum" (lambda () (emit "&sum;")))
(tex-def-math-prim "\\prod" (lambda () (emit "&prod;")))
(tex-def-math-prim "\\coprod" (lambda () (emit "&#x2210;")))
(tex-def-math-prim "\\int" #'do-integral) ;(lambda () (emit "&int;")))
(tex-def-math-prim "\\oint" (lambda () (emit "&#x222e;")))
(tex-def-math-prim "\\bigcap" (lambda () (emit "&#x2229;")))
(tex-def-math-prim "\\bigcup" (lambda () (emit "&#x222a;")))
(tex-def-math-prim "\\bigsqcup" (lambda () (emit "&#x2294;")))
(tex-def-math-prim "\\bigvee" (lambda () (emit "&#x2228;")))
(tex-def-math-prim "\\bigwedge" (lambda () (emit "&#x2227;")))
(tex-def-math-prim "\\bigodot" (lambda () (emit "&#x2299;")))
(tex-def-math-prim "\\bigotimes" (lambda () (emit "&#x2297;")))
(tex-def-math-prim "\\bigoplus" (lambda () (emit "&#x2295;")))
(tex-def-math-prim "\\biguplus" (lambda () (emit "&#x228e;")))

;7. binary operations

(tex-def-math-prim "\\pm" (lambda () (emit "&plusmn;")))
(tex-def-math-prim "\\mp" (lambda () (emit "&#x2213;")))
(tex-def-math-prim "\\setminus" (lambda () (emit "&#x2216;")))
(tex-def-math-prim "\\cdot" (lambda () (emit " &middot; ")))
(tex-def-math-prim "\\times" (lambda () (emit "&times;")))
(tex-def-math-prim "\\ast" (lambda () (emit "&lowast;")))
(tex-def-math-prim "\\star" (lambda () (emit "&2605;")))
(tex-def-math-prim "\\diamond" (lambda () (emit "&#x25c7;")))
(tex-def-math-prim "\\circ" (lambda () (emit "&#x25cb;")))
(tex-def-math-prim "\\bullet" (lambda () (emit "&bull;")))
(tex-def-math-prim "\\div" (lambda () (emit "&divide;")))
(tex-def-math-prim "\\cap" (lambda () (emit "&cap;")))
(tex-def-math-prim "\\cup" (lambda () (emit "&cup;")))
(tex-def-math-prim "\\uplus" (lambda () (emit "&#x2283;")))
(tex-def-math-prim "\\sqcap" (lambda () (emit "&#x2293;")))
(tex-def-math-prim "\\sqcup" (lambda () (emit "&#x2294;")))
(tex-def-math-prim "\\triangleleft" (lambda () (emit "&#x2282;")))
(tex-def-math-prim "\\triangleright" (lambda () (emit "&#x2283;")))
(tex-def-math-prim "\\wr" (lambda () (emit "&#x2240;")))
(tex-def-math-prim "\\vee" (lambda () (emit "&or;")))
(tex-def-math-prim "\\wedge" (lambda () (emit "&and;")))
(tex-def-math-prim "\\oplus" (lambda () (emit "&oplus;")))
(tex-def-math-prim "\\otimes" (lambda () (emit "&otimes;")))
(tex-def-math-prim "\\oslash" (lambda () (emit "&#x2298;")))
(tex-def-math-prim "\\odot" (lambda () (emit "&#x2299;")))
(tex-def-math-prim "\\dagger" (lambda () (emit "&dagger;")))
(tex-def-math-prim "\\ddagger" (lambda () (emit "&Dagger;")))
(tex-def-math-prim "\\amalg" (lambda () (emit "&#x2210;")))

;8. relations

(tex-def-math-prim "\\leq" (lambda () (emit "&le;")))
(tex-def-math-prim "\\leqslant" (lambda () (emit "&#x2a7d;")))
(tex-def-math-prim "\\prec" (lambda () (emit "&#x227a;")))
(tex-def-math-prim "\\preceq" (lambda () (emit "&#x227c;")))
(tex-def-math-prim "\\ll" (lambda () (emit "&#x226a;")))
(tex-def-math-prim "\\subset" (lambda () (emit "&sub;")))
(tex-def-math-prim "\\subseteq" (lambda () (emit "&sube;")))
(tex-def-math-prim "\\sqsubseteq" (lambda () (emit "&#x2291;")))
(tex-def-math-prim "\\in" (lambda () (emit "&isin;")))
(tex-def-math-prim "\\vdash" (lambda () (emit "&#x22a2;")))
(tex-def-math-prim "\\smile" (lambda () (emit "&#x2323;")))
(tex-def-math-prim "\\frown" (lambda () (emit "&#x2322;")))
(tex-def-math-prim "\\geq" (lambda () (emit "&ge;")))
(tex-def-math-prim "\\geqslant" (lambda () (emit "&#x2a7e;")))
(tex-def-math-prim "\\succ" (lambda () (emit "&#x227b;")))
(tex-def-math-prim "\\succeq" (lambda () (emit "&#x227d;")))
(tex-def-math-prim "\\gg" (lambda () (emit "&#x226b;")))
(tex-def-math-prim "\\supset" (lambda () (emit "&sup;")))
(tex-def-math-prim "\\supseteq" (lambda () (emit "&supe;")))
(tex-def-math-prim "\\sqsupseteq" (lambda () (emit "&#x2292;")))
(tex-def-math-prim "\\ni" (lambda () (emit "&ni;")))
(tex-def-math-prim "\\dashv" (lambda () (emit "&#x22a3;")))
(tex-def-math-prim "\\mid" (lambda () (emit "&#x2223;")))
(tex-def-math-prim "\\parallel" (lambda () (emit "&#x2225;")))
(tex-def-math-prim "\\equiv" (lambda () (emit "&equiv;")))
(tex-def-math-prim "\\sim" (lambda () (emit "&sim;")))
(tex-def-math-prim "\\simeq" (lambda () (emit "&#x2243;")))
(tex-def-math-prim "\\asymp" (lambda () (emit "&#x224d;")))
(tex-def-math-prim "\\approx" (lambda () (emit "&asymp;")))
(tex-def-math-prim "\\cong" (lambda () (emit "&cong;")))
(tex-def-math-prim "\\bowtie" (lambda () (emit "&#x22c8;")))
(tex-def-math-prim "\\propto" (lambda () (emit "&#x221d;")))
(tex-def-math-prim "\\models" (lambda () (emit "&#x22a8;")))
(tex-def-math-prim "\\doteq" (lambda () (emit "&#x2250;")))
(tex-def-math-prim "\\propto" (lambda () (emit "&prop;")))
(tex-def-math-prim "\\perp" (lambda () (emit "&perp;")))

;9. negated relations

(defun do-not ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (char= c *esc-char*)
        (let ((x (get-ctl-seq)))
          (emit (cond ((string= x "\\leq") "&#x2270;")
                      ((string= x "\\le") "&#x2270;")
                      ((string= x "\\prec") "&#x2280;")
                      ((string= x "\\preceq") "&#x22e0;")
                      ((string= x "\\subset") "&#x2284;")
                      ((string= x "\\subseteq") "&#x2288;")
                      ((string= x "\\sqsubseteq") "&#x22e2;")
                      ((string= x "\\geq") "&#x2271;")
                      ((string= x "\\ge") "&#x2271;")
                      ((string= x "\\succ") "&#x2281;")
                      ((string= x "\\succeq") "&#x22e1;")
                      ((string= x "\\supset") "&#x2285;")
                      ((string= x "\\supseteq") "&#x2289;")
                      ((string= x "\\sqsupseteq") "&#x22e3;")
                      ((string= x "\\equiv") "&#x2262;")
                      ((string= x "\\sim") "&#x2241;")
                      ((string= x "\\simeq") "&#x2244;")
                      ((string= x "\\approx") "&#x2249;")
                      ((string= x "\\cong") "&#x2247;")
                      ((string= x "\\asymp") "&#x226d;")
                      (t (toss-back-string x) "/"))))
      (case c
        ((#\< #\> #\=) (get-actual-char)
         (emit (ecase c
                 (#\< "&#x226e;")
                 (#\> "&#x226f;")
                 (#\= "&#x2260;"))))
        (t (emit "/"))))))

(tex-def-math-prim "\\not" #'do-not)
(tex-def-math-prim "\\notin" (lambda () (emit "&notin;")))

;10. arrows

(tex-def-math-prim "\\leftarrow" (lambda () (emit "&larr;")))
(tex-def-math-prim "\\Leftarrow" (lambda () (emit "&lArr;")))
(tex-def-math-prim "\\rightarrow" (lambda () (emit "&rarr;")))
(tex-def-math-prim "\\Rightarrow" (lambda () (emit "&rArr;")))
(tex-def-math-prim "\\leftrightarrow" (lambda () (emit "&harr;")))
(tex-def-math-prim "\\Leftrightarrow" (lambda () (emit "&hArr;")))
(tex-def-math-prim "\\mapsto" (lambda () (emit "&#x21a6;")))
(tex-def-math-prim "\\hookleftarrow" (lambda () (emit "&#x21a9;")))
(tex-def-math-prim "\\leftharpoonup" (lambda () (emit "&#x21bc;")))
(tex-def-math-prim "\\leftharpoondown" (lambda () (emit "&#x21bd;")))
(tex-def-math-prim "\\rightleftharpoons" (lambda () (emit "&#x21cb;")))
(tex-def-math-prim "\\longleftarrow" (lambda () (emit "&larr;&mdash;")))
(tex-def-math-prim "\\Longleftarrow" (lambda () (emit "&lArr;===")))
(tex-def-math-prim "\\longrightarrow" (lambda () (emit "&mdash;&rarr;")))
(tex-def-math-prim "\\Longrightarrow" (lambda () (emit "===&rArr;")))
(tex-def-math-prim "\\longleftrightarrow" (lambda () (emit "&larr;&mdash;&rarr;")))
(tex-def-math-prim "\\Longleftrightarrow" (lambda () (emit "&lArr;===&rArr;")))
(tex-def-math-prim "\\longmapsto" (lambda () (emit "&mdash;&#x21a6;")))
(tex-def-math-prim "\\hookrightarrow" (lambda () (emit "&#x21aa;")))
(tex-def-math-prim "\\rightharpoonup" (lambda () (emit "&#x21c0;")))
(tex-def-math-prim "\\rightharpoondown" (lambda () (emit "&#x21c1;")))
(tex-def-math-prim "\\uparrow" (lambda () (emit "&uarr;")))
(tex-def-math-prim "\\Uparrow" (lambda () (emit "&uArr;")))
(tex-def-math-prim "\\downarrow" (lambda () (emit "&darr;")))
(tex-def-math-prim "\\Downarrow" (lambda () (emit "&dArr;")))
(tex-def-math-prim "\\nearrow" (lambda () (emit "&#x2197;")))
(tex-def-math-prim "\\searrow" (lambda () (emit "&#x2198;")))
(tex-def-math-prim "\\swarrow" (lambda () (emit "&#x2199;")))
(tex-def-math-prim "\\nwarrow" (lambda () (emit "&#x2196;")))

;11. openings

(tex-def-math-prim "\\lbrack" (lambda () (emit "[")))
(tex-def-math-prim "\\lbrace" (lambda () (emit "{")))
(tex-def-math-prim "\\lfloor" (lambda () (emit "&lfloor;")))
(tex-def-math-prim "\\langle" (lambda () (emit "&lang;")))
(tex-def-math-prim "\\lceil" (lambda () (emit "&lceil;")))

;12. closings

(tex-def-math-prim "\\rbrack" (lambda () (emit "]")))
(tex-def-math-prim "\\rbrace" (lambda () (emit "}")))
(tex-def-math-prim "\\rfloor" (lambda () (emit "&rfloor;")))
(tex-def-math-prim "\\rangle" (lambda () (emit "&rang;")))
(tex-def-math-prim "\\rceil" (lambda () (emit "&rceil;")))

;13. punctuation

(tex-def-math-prim "\\colon" (lambda () (emit #\:)))
(tex-def-math-prim "\\ldotp" (lambda () (emit #\.)))
(tex-let-prim "\\cdotp" "\\cdot")

;14. alternate names

(tex-def-math-prim "\\ne" (lambda () (emit "&ne;")))
(tex-let-prim "\\neq" "\\ne")
(tex-let-prim "\\le" "\\leq")
(tex-let-prim "\\ge" "\\geq")
(tex-let-prim "\\{" "\\lbrace")
(tex-let-prim "\\}" "\\rbrace")
(tex-let-prim "\\to" "\\rightarrow")
(tex-let-prim "\\gets" "\\leftarrow")
(tex-let-prim "\\owns" "\\ni")
(tex-let-prim "\\land" "\\wedge")
(tex-let-prim "\\lor" "\\vee")
(tex-let-prim "\\lnot" "\\neg")
(tex-let-prim "\\vert" "\\mid")
(tex-let-prim "\\Vert" "\\parallel")
(tex-let-prim "\\iff" "\\Longleftrightarrow")

;15. non-math symbols

(tex-def-prim "\\S" (lambda () (emit "&sect;")))
(tex-def-prim "\\P" (lambda () (emit "&para;")))
(tex-def-prim "\\dag" (lambda () (emit "&dagger;")))
(tex-def-prim "\\ddag" (lambda () (emit "&Dagger;")))

;end appendix F

(tex-def-math-prim "\\eqalign" (lambda () (do-eqalign :eqalign)))

(tex-def-math-prim "\\eqalignno" (lambda () (do-eqalign :eqalignno)))

(tex-def-math-prim "\\displaylines" (lambda () (do-eqalign :displaylines)))

(tex-def-math-prim "\\noalign" #'do-noalign)

(tex-def-math-prim "\\frac" #'do-frac)

(tex-def-math-prim "\\pmatrix" #'do-pmatrix)

(tex-def-math-prim "\\matrix" #'do-relax) ;??? better?

(tex-def-math-prim "\\eqno" #'do-eqno)

(defun do-math-font (f)
  (lambda ()
    (let ((*math-font* f))
      (tex2page-string (get-token)))))

(tex-def-math-prim "\\mathbf" #'do-relax)

(tex-def-math-prim "\\mathrm" (do-math-font :rm))

(tex-def-math-prim "\\mathbb" (do-math-font :bb))

(tex-def-math-prim "\\mathcal" (do-math-font :cal))

(tex-def-math-prim "\\mathfrak" (do-math-font :frak))

(tex-def-math-prim "\\over" #'do-over); (lambda () (emit "/")))

(tex-def-math-prim "\\sqrt"
 (lambda () (emit "&radic;(") (tex2page-string (get-token)) (emit ")")))

(tex-def-math-prim "\\left" #'do-math-left)

(tex-def-math-prim "\\right" #'do-math-right)

;spaces

(defun kern (len)
  (concatenate 'string "<span style=\"margin-left: "
               len "\"> </span>"))  ;zwnj causes table-row fault

(tex-def-prim "\\enspace" (lambda () (emit (kern ".5em"))))
(tex-def-prim "\\thinspace" (lambda () (emit (kern ".16667em"))))
(tex-def-prim "\\negthinspace" (lambda () (emit (kern "-.16667em"))))
(tex-def-prim "\\quad" (lambda () (emit (kern "1em"))))
(tex-def-prim "\\qquad" (lambda () (emit (kern "2em"))))
(tex-let-prim "\\enskip" "\\enspace")

(tex-def-math-prim "\\," (lambda () (emit (kern ".16667em"))))
(tex-def-math-prim "\\>" (lambda () (emit (kern ".22222em"))))
(tex-def-math-prim "\\;" (lambda () (emit (kern ".27778em"))))
(tex-def-math-prim "\\!" (lambda () (emit (kern "-.16667em"))))

;

(tex-def-prim "\\AA" (lambda () (emit "&Aring;")))
(tex-def-prim "\\aa" (lambda () (emit "&aring;")))

(tex-def-prim "\\abstract"
 (lambda ()
     (tex2page-string "\\quote")
     (tex2page-string "\\centerline{\\bf\\abstractname}\\par")))

(tex-def-prim "\\addcontentsline" #'do-addcontentsline)

(tex-def-prim "\\addtocounter" (lambda () (set-latex-counter t)))

(tex-def-prim "\\advance" (lambda () (do-advance (globally-p))))

(tex-def-prim "\\advancetally" (lambda () (do-advancetally (globally-p))))

(tex-def-prim "\\AE" (lambda () (emit "&AElig;")))

(tex-def-prim "\\ae" (lambda () (emit "&aelig;")))

(tex-def-prim "\\afterassignment" #'do-afterassignment)

(tex-def-prim "\\aftergroup" #'do-aftergroup)

(tex-def-prim "\\align" (lambda () (do-equation :align)))

(tex-def-prim "\\alltt" #'do-alltt)

(tex-def-prim "\\appendix" #'do-appendix)

(tex-def-prim "\\appendixname" (lambda () (emit "Appendix ")))
(tex-def-prim "\\array" (lambda () (do-tabular t)))
(tex-def-prim "\\author" #'do-author)

(tex-def-prim "\\b" (lambda () (do-diacritic :barunder)))
(tex-def-prim "\\begin" #'do-begin)
(tex-def-prim "\\beginsection" #'do-beginsection)
(tex-def-prim "\\beginchapter" #'do-beginchapter)
(tex-def-prim "\\bf" (lambda () (do-switch :bf)))
(tex-def-prim "\\bgcolor" (lambda () (do-switch :bgcolor)))
(tex-def-prim-0arg "\\bgroup" "{")
(tex-def-prim "\\bibitem" #'do-bibitem)
(tex-def-prim "\\bibliography" #'do-bibliography)
(tex-def-prim "\\bibliographystyle" #'do-bibliographystyle)
(tex-def-prim "\\bigbreak" (lambda () (do-bigskip :bigskip)))
(tex-def-prim "\\bigskip" (lambda () (do-bigskip :bigskip)))
(tex-def-prim "\\break" (lambda () (emit "<br>")))
(tex-def-prim "\\bull" (lambda () (emit "&#x25fe;")))

(tex-def-prim "\\c" (lambda () (do-diacritic :cedilla)))
(tex-def-prim "\\caption" #'do-caption)
(tex-def-prim "\\catcode" #'do-catcode)
;(tex-def-math-prim "\\cdots" (lambda () (emit "<tt>&middot;&middot;&middot;</tt>")))
(tex-def-prim "\\cdots" (lambda () (emit "&#x22ef;")))
(tex-def-prim "\\center" (lambda () (do-block :center)))
(tex-def-prim "\\centerline" (lambda () (do-function "\\centerline")))
(tex-def-prim "\\chapter"
 (lambda ()
     (!using-chapters)
     (write-aux '(!using-chapters))
     (when (and (eql *tex-format* :latex) (< (get-gcount "\\secnumdepth") -1))
       (tex-gdef-count "\\secnumdepth" 2))
     (do-heading 0)))
(tex-def-prim "\\chaptername" (lambda () (emit "Chapter ")))

(tex-def-prim "\\char" #'do-char)

(tex-def-prim "\\cite" #'do-cite)

(tex-def-prim "\\closegraphsfile" #'do-mfpic-closegraphsfile)

(tex-def-prim "\\closein" (lambda () (do-close-stream :in)))

(tex-def-prim "\\closeout" (lambda () (do-close-stream :out)))

(tex-def-prim "\\color" #'do-color)

(tex-def-prim "\\convertMPtoPDF" #'do-convertmptopdf)

(tex-def-prim "\\copyright" (lambda () (emit "&copy;")))

(tex-def-prim "\\countdef" (lambda () (do-newcount t) (eat-integer)))
(tex-def-prim "\\CR" (lambda () (do-cr "\\CR")))
(tex-def-prim "\\cr" (lambda () (do-cr "\\cr")))
(tex-def-prim "\\csname" #'do-csname)
(tex-def-prim "\\cssblock" #'do-cssblock)

(tex-def-prim "\\d" (lambda () (do-diacritic :dotunder)))
(tex-def-prim "\\." (lambda () (do-diacritic :dot)))
(tex-def-prim "\\dag" (lambda () (emit "&dagger;")))
(tex-def-prim "\\date" #'do-date)
(tex-def-prim "\\ddag" (lambda () (emit "&Dagger;")))
(tex-def-prim "\\ddots" (lambda () (emit "&#x22f1;")))
(tex-def-prim "\\def" (lambda () (do-def (globally-p) nil)))
(tex-def-prim "\\defcsactive" (lambda () (do-defcsactive (globally-p))))
(tex-def-prim "\\definecolor" #'do-definecolor)
(tex-def-prim "\\DefineNamedColor" (lambda () (get-token) (do-definecolor)))
(tex-def-prim "\\definexref" #'do-definexref)
(tex-def-prim "\\definitelylatex" #'definitely-latex)
(tex-def-prim "\\defschememathescape" (lambda () (scm-set-mathescape t)))
(tex-def-prim "\\degree" (lambda () (emit "&deg;")))
(tex-def-prim "\\description"
  (lambda ()
    (do-end-para)
    (push :description *tabular-stack*)
    (emit "<dl><dt></dt><dd>")))
(tex-def-prim "\\DH" (lambda () (emit "&ETH;")))

(tex-def-prim "\\dh" (lambda () (emit "&eth;")))

(tex-def-prim "\\discretionary" #'do-discretionary)

(tex-def-prim "\\displaymath"
 (lambda () (do-latex-env-as-image "displaymath" :display)))

(tex-def-prim "\\divide" (lambda () (do-divide (globally-p))))

(tex-def-prim "\\document" #'probably-latex)

(tex-def-prim "\\documentclass" #'do-documentclass)

(tex-def-prim "\\dontuseimgforhtmlmath"
 (lambda () (tex-def-0arg "\\TZPmathimage" "0")))

(tex-def-prim "\\dontuseimgforhtmlmathdisplay"
 (lambda () (tex-def-0arg "\\TZPmathimage" "0")))

(tex-def-prim "\\dontuseimgforhtmlmathintext" (lambda () t))
;(tex-def-prim "\\dots" (lambda () (emit "<tt>...</tt>")))
(tex-def-prim "\\dots" (lambda () (emit "&#x2026;")))

(tex-def-prim "\\edef" (lambda () (do-def (globally-p) t)))
(tex-def-prim-0arg "\\egroup" "}")
(tex-def-prim "\\eject" #'do-eject)
(tex-def-prim "\\else" #'do-else)
(tex-def-prim "\\em" (lambda () (do-switch :em)))
(tex-def-prim "\\emph" (lambda () (do-function "\\emph")))
(tex-def-prim-0arg "\\empty"
                   ;for \ifx comparisons
                   "")
(tex-def-prim "\\end" #'do-end)
(tex-def-prim "\\endalign" #'do-end-equation)
(tex-def-prim "\\endalltt" #'do-end-alltt)
(tex-def-prim "\\endarray" #'do-end-tabular)
(tex-def-prim "\\endcenter" #'do-end-block)
(tex-def-prim "\\enddescription"
 (lambda ()
     (pop-tabular-stack :description)
     (do-end-para)
     (emit "</dd></dl>")
     (do-noindent)))
(tex-def-prim "\\endeqnarray" #'do-end-equation)

(tex-def-prim "\\endequation" #'do-end-equation)

(tex-def-prim "\\endenumerate"
 (lambda ()
     (pop-tabular-stack :enumerate)
     (do-end-para)
     (emit "</ol>")
     (do-noindent)))

(tex-def-prim "\\endfigure" (lambda () (do-end-table/figure :figure)))

(tex-def-prim "\\endflushleft" #'do-end-block)

(tex-def-prim "\\endflushright" #'do-end-block)

(tex-def-prim "\\endgraf" #'do-para)

(tex-def-prim "\\endhtmlimg"
 (lambda () (terror 'tex-def-prim "Unmatched \\endhtmlimg")))

(tex-def-prim "\\endhtmlonly"
 (lambda () (decf *html-only*)))

(tex-def-prim "\\enditemize"
 (lambda ()
     (pop-tabular-stack :itemize)
     (do-end-para)
     (emit "</ul>")
     (do-noindent)))

(tex-def-prim "\\endminipage" #'do-endminipage)
(tex-def-prim "\\endquote"
 (lambda () (do-end-para) (egroup) (emit "</blockquote>")))
(tex-def-prim "\\endruledtable" #'do-endruledtable)

(tex-def-prim "\\endtabbing" #'do-end-tabbing)

(tex-def-prim "\\endtable" (lambda () (do-end-table/figure :table)))

(tex-def-prim "\\endtableplain" #'do-end-table-plain)

(tex-def-prim "\\endtabular" #'do-end-tabular)

(tex-def-prim "\\endthebibliography"
 (lambda () (emit "</table>") (egroup) (do-para)))

(tex-def-prim "\\endverbatim" #'do-endverbatim-eplain)

(tex-def-prim "\\enumerate"
  (lambda ()
    (do-end-para)
    (push :enumerate *tabular-stack*)
    (emit "<ol>")))

(tex-def-prim "\\epsfbox" #'do-epsfbox)

(tex-def-prim "\\epsfig" #'do-epsfig)

(tex-def-prim "\\eqnarray" (lambda () (do-equation :eqnarray)))

(tex-def-prim "\\equation" (lambda () (do-equation :equation)))

(tex-def-prim "\\errmessage" #'do-errmessage)

(tex-def-prim "\\eval" (lambda () (do-eval :both)))

;(tex-def-prim "\\TIIPeval" (lambda () (do-eval :inner)))

(tex-def-prim "\\evalh" (lambda () (do-eval :html)))

(tex-def-prim "\\evalq" (lambda () (do-eval :quiet)))

(tex-def-prim "\\expandafter" #'do-expandafter)

(tex-def-prim "\\expandhtmlindex" #'expand-html-index)

(tex-def-prim "\\externaltitle" #'do-externaltitle)

(tex-def-prim "\\fi" (lambda () (do-fi)))

(tex-def-prim "\\figure" (lambda () (do-table/figure :figure)))

(tex-def-prim "\\fiverm" (lambda () (do-switch :fiverm)))

(tex-def-prim "\\flushleft" (lambda () (do-block :flushleft)))

(tex-def-prim "\\flushright" (lambda () (do-block :flushright)))

(tex-def-prim "\\fmtname" (lambda () (emit "TeX2page")))

(tex-def-prim "\\fmtversion" (lambda () (emit *tex2page-version*)))

(tex-def-prim "\\folio" (lambda () (emit *html-page-count*)))

(tex-def-prim "\\font" #'do-font)

(tex-def-prim "\\footnote" #'do-footnote)

(tex-def-prim "\\footnotesize" (lambda () (do-switch :footnotesize)))
(tex-def-prim "\\frac" #'do-frac)
(tex-def-prim "\\futurelet" #'do-futurelet)

(tex-def-prim "\\futurenonspacelet" #'do-futurenonspacelet)

(tex-def-prim "\\gdef" (lambda () (do-def t nil)))

(tex-def-prim "\\global" #'do-global)

(tex-def-prim "\\globaladvancetally" (lambda () (do-advancetally t)))

(tex-def-prim "\\gobblegroup" #'get-group)

(tex-def-prim "\\\"" (lambda () (do-diacritic :umlaut)))

(tex-def-prim "\\H" (lambda () (do-diacritic :hungarianumlaut)))
(tex-def-prim "\\halign" #'do-halign)

(tex-def-prim "\\hbox" #'do-box)

(tex-def-prim "\\hfill" (lambda () (emit-nbsp 5)))

(tex-def-prim "\\hlstart" #'do-hlstart)

(tex-def-prim "\\href" #'do-urlh)

(tex-def-prim "\\hrule"
 (lambda () (do-end-para) (emit "<hr>") (emit-newline) (do-para)))

(tex-def-prim "\\hskip" #'do-hskip)

(tex-def-prim "\\hspace" #'do-hspace)

(tex-def-prim "\\htmladdimg" #'do-htmladdimg)

(tex-def-prim "\\htmlcolophon" #'do-htmlcolophon) ;obs

(tex-def-prim "\\htmldoctype" #'do-htmldoctype)

(tex-def-prim "\\htmlgif" (lambda () (do-htmlimg "htmlgif")))

(tex-def-prim "\\htmlheadonly" #'do-htmlheadonly)

(tex-def-prim "\\htmlimageconversionprogram" #'do-htmlimageconversionprogram)

(tex-def-prim "\\htmlimageformat" #'do-htmlimageformat)

(tex-def-prim "\\htmlimg" (lambda () (do-htmlimg "htmlimg")))

(tex-def-prim "\\htmlimgmagnification" #'do-htmlimgmagnification)

(tex-def-prim "\\htmlmathstyle" #'do-htmlmathstyle)

(tex-def-prim "\\htmlonly" (lambda () (incf *html-only*)))

(tex-def-prim "\\htmlpagelabel" #'do-htmlpagelabel)

(tex-def-prim "\\htmlpageref" #'do-htmlpageref)

(tex-def-prim "\\htmlref" #'do-htmlref)

(tex-def-prim "\\htmlrefexternal" #'do-htmlrefexternal)

(tex-def-prim "\\htmlspan" (lambda () (do-switch :span)))

(tex-def-prim "\\htmldiv" (lambda () (do-switch :div)))

(tex-def-prim "\\huge" (lambda () (do-switch :huge)))

(tex-def-prim "\\Huge" (lambda () (do-switch :huge-cap)))

(tex-def-prim "\\hyperref" #'do-hyperref)
(tex-def-prim "\\hyperlink" #'do-hyperlink)
(tex-def-prim "\\hypertarget" #'do-hypertarget)

(tex-def-prim "\\i" (lambda () (emit "&#x131;")))
(tex-def-prim "\\if" #'do-if)
(tex-def-prim "\\ifcase" #'do-ifcase)
(tex-def-prim "\\ifdefined" #'do-ifdefined)

(tex-def-prim "\\ifeof" #'do-ifeof)

(tex-def-prim "\\ifdim" #'do-iffalse)

(tex-def-prim "\\iffalse" #'do-iffalse)

(tex-def-prim "\\IfFileExists" #'do-iffileexists)

(tex-def-prim "\\ifhmode" #'do-iftrue)

(tex-def-prim "\\ifmmode" #'do-ifmmode)

(tex-def-prim "\\ifnum" #'do-ifnum)

(tex-def-prim "\\iftrue" #'do-iftrue)

(tex-def-prim "\\ifx" #'do-ifx)

(tex-def-prim "\\ifodd" #'do-ifodd)

(tex-def-prim "\\ignorenextinputtimestamp"
  (lambda ()
    (unless *inputting-boilerplate-p* (setq *inputting-boilerplate-p* 0))))

(tex-def-prim "\\ignorespaces" #'ignorespaces)

(tex-def-prim "\\imgdef" (lambda () (make-reusable-img (globally-p))))

(tex-def-prim "\\imgpreamble" #'do-img-preamble)

(tex-def-prim "\\IMGtabbing"
 (lambda () (do-latex-env-as-image "tabbing" :display)))

(tex-def-prim "\\IMGtabular"
 (lambda () (do-latex-env-as-image "tabular" :display)))

(tex-def-prim "\\include" #'do-include)

(tex-def-prim "\\includeexternallabels" #'do-includeexternallabels)

(tex-def-prim "\\includeonly" #'do-includeonly)

(tex-def-prim "\\includegraphics" #'do-includegraphics)

(tex-def-prim "\\index" #'do-index)

(tex-def-prim "\\indexitem" (lambda () (do-indexitem 0)))
(tex-def-prim "\\indexsubitem" (lambda () (do-indexitem 1)))
(tex-def-prim "\\indexsubsubitem" (lambda () (do-indexitem 2)))
(tex-def-prim "\\input" #'do-input)
(tex-def-prim "\\inputcss" #'do-inputcss)
(tex-def-prim "\\inputexternallabels" #'do-inputexternallabels)
(tex-def-prim "\\InputIfFileExists" #'do-inputiffileexists)
(tex-def-prim "\\inputindex" #'do-inputindex)
(tex-def-prim "\\it" (lambda () (do-switch :it)))
(tex-def-prim "\\item" #'do-item)
(tex-def-prim "\\itemitem" (lambda () (do-plain-item 2)))
(tex-def-prim "\\itemize"
  (lambda ()
    (do-end-para)
    (push :itemize *tabular-stack*)
    (emit "<ul>")))
(tex-def-prim "\\itshape" (lambda () (do-switch :it)))

(tex-def-prim "\\j" (lambda () (emit "&#x237;")))
(tex-def-prim "\\jobname" (lambda () (tex2page-string *jobname*)))

(tex-def-prim "\\k" (lambda () (do-diacritic :ogonek)))
(tex-def-prim "\\kern" #'do-hskip)

(tex-def-prim "\\l" (lambda () (emit "&#x142;")))
(tex-def-prim "\\L" (lambda () (emit "&#x141;")))
(tex-def-prim "\\label" #'do-label)
(tex-def-prim "\\large" (lambda () (do-switch :large)))
(tex-def-prim "\\Large" (lambda () (do-switch :large-cap)))
(tex-def-prim "\\LARGE" (lambda () (do-switch :large-up)))
(tex-def-prim "\\latexonly"
 (lambda () (ignore-tex-specific-text "latexonly")))
(tex-def-prim "\\leftdisplays"
 (lambda () (setq *display-justification* "left")))
(tex-def-prim "\\leftline" (lambda () (do-function "\\leftline")))
(tex-def-prim "\\let" (lambda () (do-let (globally-p))))
(tex-def-prim "\\linebreak"
 (lambda () (get-bracketed-text-if-any) (emit "<br>")))
(tex-def-prim "\\listing" #'do-verbatiminput)
(tex-def-prim "\\lstlisting" (lambda () (do-verbatim-latex "lstlisting")))

(tex-def-prim "\\magnification" #'do-magnification)
(tex-def-prim "\\magstep" #'do-magstep)
(tex-def-prim-0arg "\\magstephalf" "1095")
(tex-def-prim "\\mailto" #'do-mailto)
(tex-def-prim "\\makeatletter" (lambda () (set-catcode #\@ 11)))
(tex-def-prim "\\makeatother" (lambda () (set-catcode #\@ 12)))
(tex-def-prim "\\makehtmlimage" #'do-makehtmlimage)
(tex-def-prim "\\maketitle" #'do-maketitle)
(tex-def-prim "\\marginpar" #'do-marginpar)
(tex-def-prim "\\mathg" #'do-mathg)
(tex-def-prim "\\mathdg" #'do-mathdg)
(tex-def-prim "\\mathp" #'do-mathp)
(tex-def-prim "\\medbreak" (lambda () (do-bigskip :medskip)))

(tex-def-prim "\\medskip" (lambda () (do-bigskip :medskip)))

(tex-def-prim "\\message" #'do-message)
(tex-def-prim "\\mfpic" #'do-mfpic)
(tex-def-prim "\\minipage" #'do-minipage)
(tex-def-prim "\\multiply" (lambda () (do-multiply (globally-p))))

(tex-def-prim "\\narrower" (lambda () (do-switch :narrower)))
(tex-def-prim "\\newcommand" (lambda () (do-newcommand nil)))
(tex-def-prim "\\newcount" (lambda () (do-newcount (globally-p))))
(tex-def-prim "\\newdimen" (lambda () (do-newdimen (globally-p))))
(tex-def-prim "\\newenvironment" (lambda () (do-newenvironment nil)))
(tex-def-prim "\\newif" #'do-newif)
(tex-def-prim "\\newread" (lambda () (do-new-stream :in)))
(tex-def-prim "\\newtheorem" #'do-newtheorem)
(tex-def-prim "\\newtoks" (lambda () (do-newtoks (globally-p))))
(tex-def-prim "\\newwrite" (lambda () (do-new-stream :out)))
(tex-def-prim "\\noad" (lambda () (tex-def-0arg "\\TZPcolophoncredit" "0")))
(tex-def-prim "\\nocite" #'do-nocite)
(tex-def-prim "\\node" #'do-node)
(tex-def-prim "\\noindent" #'do-noindent)
(tex-def-prim "\\nonumber" #'do-nonumber)
(tex-def-prim "\\noslatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "0")))
(tex-def-prim "\\notimestamp"
 (lambda () (tex-def-0arg "\\TZPcolophontimestamp" "0")))
(tex-def-prim "\\nr" (lambda () (do-cr "\\nr")))
(tex-def-prim "\\number" #'do-number)
(tex-def-prim "\\numberedfootnote" #'do-numbered-footnote)

(tex-def-prim "\\@ldc@l@r" #'do-color)
(tex-def-prim "\\O" (lambda () (emit "&Oslash;")))
(tex-def-prim "\\o" (lambda () (emit "&oslash;")))
(tex-def-prim "\\obeylines" #'do-obeylines)
(tex-def-prim "\\obeyspaces" #'do-obeyspaces)
(tex-def-prim "\\obeywhitespace" #'do-obeywhitespace)
(tex-def-prim "\\OE" (lambda () (emit "&OElig;")))
(tex-def-prim "\\oe" (lambda () (emit "&oelig;")))
(tex-def-prim "\\opengraphsfile" #'do-mfpic-opengraphsfile)
(tex-def-prim "\\openin" (lambda () (do-open-stream :in)))
(tex-def-prim "\\openout" (lambda () (do-open-stream :out)))

(tex-def-prim "\\pagebreak"
 (lambda () (get-bracketed-text-if-any) (do-eject)))
(tex-def-prim "\\pageno" (lambda () (emit *html-page-count*)))
(tex-def-prim "\\pageref" #'do-pageref)
(tex-def-prim "\\part" (lambda () (do-heading -1)))
(tex-def-prim "\\pdfximage" #'do-pdfximage)
(tex-def-prim "\\picture"
 (lambda () (do-latex-env-as-image "picture" nil)))
(tex-def-prim "\\plainfootnote" #'do-plain-footnote)
(tex-def-prim "\\pounds" (lambda () (emit "&pound;")))
(tex-def-prim "\\printindex" (lambda () (do-inputindex t)))
(tex-def-prim "\\providecommand" (lambda () (do-newcommand nil)))

(tex-def-prim "\\quote"
 (lambda () (do-end-para) (emit "<blockquote>") (bgroup)))

(tex-def-prim "\\r" (lambda () (do-diacritic :ring)))
(tex-def-prim "\\raggedleft" (lambda () (do-switch :raggedleft)))
(tex-def-prim "\\rawhtml" #'do-rawhtml)
(tex-def-prim "\\read" (lambda () (do-read (globally-p))))
(tex-def-prim "\\readtocfile" #'do-toc)
(tex-def-prim "\\ref" #'do-ref)
(tex-def-prim "\\refexternal" #'do-refexternal)
(tex-def-prim "\\refn" #'do-ref)
(tex-def-prim "\\relax" #'do-relax)
(tex-def-prim "\\renewcommand" (lambda () (do-newcommand t)))
(tex-def-prim "\\renewenvironment" (lambda () (do-newenvironment t)))
(tex-def-prim "\\resetatcatcode" (lambda () (set-catcode #\@ 12)))
(tex-def-prim "\\resizebox" #'do-resizebox)
(tex-def-prim "\\rightline" (lambda () (do-function "\\rightline")))
(tex-def-prim "\\rm" (lambda () (if *math-mode-p* (do-switch :rm))))
(tex-def-prim "\\romannumeral" #'do-romannumeral)
(tex-def-prim "\\Romannumeral" (lambda () (do-romannumeral t)))
(tex-def-prim "\\ruledtable" #'do-ruledtable)

(tex-def-prim "\\sc" (lambda () (do-switch :sc)))
(tex-def-prim "\\schemedisplay"
 (lambda () (do-scm-slatex-lines "schemedisplay" t nil)))
(tex-def-prim "\\schemebox"
 (lambda () (do-scm-slatex-lines "schemebox" nil nil)))
(tex-def-prim "\\schemeresponse"
 (lambda () (do-scm-slatex-lines "schemeresponse" t :result)))
(tex-def-prim "\\schemeresponsebox"
 (lambda () (do-scm-slatex-lines "schemeresponsebox" nil :result)))
(tex-def-prim "\\schemeresult" (lambda () (do-scm :result)))
(tex-def-prim "\\scm" #'do-scm)
(tex-def-prim "\\scmbuiltin" #'do-scm-set-builtins)
(tex-def-prim "\\scmdribble" #'do-scmdribble)
(tex-def-prim "\\scminput" #'do-scminput)
(tex-def-prim "\\scmkeyword" #'do-scm-set-keywords)
(tex-def-prim "\\scmspecialsymbol" #'do-scm-set-specialsymbol)
(tex-def-prim "\\scmvariable" #'do-scm-set-variables)
(tex-def-prim "\\scriptsize" (lambda () (do-switch :scriptsize)))
(tex-def-prim "\\section" (lambda () (do-heading 1)))
(tex-def-prim "\\seealso" #'do-see-also)
(tex-def-prim "\\setcounter" (lambda () (set-latex-counter nil)))
(tex-def-prim "\\sevenrm" (lambda () (do-switch :sevenrm)))
(tex-def-prim "\\sf" (lambda () (do-switch :sf)))
(tex-def-prim "\\sidx" #'do-index)
(tex-def-prim "\\sl" (lambda () (do-switch :sl)))
(tex-def-prim "\\slatexdisable" #'get-group)
(tex-def-prim "\\slatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "1")))
(tex-def-prim "\\small" (lambda () (do-switch :small)))
(tex-def-prim "\\smallbreak" (lambda () (do-bigskip :smallskip)))
(tex-def-prim "\\smallskip" (lambda () (do-bigskip :smallskip)))
(tex-def-prim "\\ss" (lambda () (emit "&szlig;")))
(tex-def-prim "\\strike" (lambda () (do-switch :strike)))
(tex-def-prim "\\string" #'do-string)
(tex-def-prim "\\subject" #'do-subject)
(tex-def-prim "\\subsection"
 (lambda () (get-bracketed-text-if-any) (do-heading 2)))
(tex-def-prim "\\subsubsection" (lambda () (do-heading 3)))
(tex-def-prim "\\symfootnote" #'do-symfootnote)

(tex-def-prim "\\t" (lambda () (do-diacritic :tieafter)))
(tex-def-prim "\\tabbing" #'do-tabbing)
(tex-def-prim "\\table" (lambda () (do-table/figure :table)))
(tex-def-prim "\\tableplain" #'do-table-plain)
(tex-def-prim "\\tableofcontents" #'do-toc)
(tex-def-prim "\\tabular" #'do-tabular)
(tex-def-prim "\\tag" #'do-tag)

(tex-def-prim "\\texonly" (lambda () (ignore-tex-specific-text "texonly")))

(tex-def-prim "\\textasciicircum" (lambda () (emit "^")))

(tex-def-prim "\\textbar" (lambda () (emit "|")))

(tex-def-prim "\\textbackslash" (lambda () (emit "\\")))

(tex-def-prim "\\textbf" (lambda () (do-function "\\textbf")))

(tex-def-prim "\\textbullet" (lambda () (emit "&bull;")))

(tex-def-prim "\\textemdash" (lambda () (emit "&mdash;")))

(tex-def-prim "\\textendash" (lambda () (emit "&ndash;")))

(tex-def-prim "\\textexclamdown" (lambda () (emit "&iexcl;")))
(tex-def-prim "\\textgreater" (lambda () (emit "&gt;")))
(tex-def-prim "\\textindent" #'do-textindent)
(tex-def-prim "\\textit" (lambda () (do-function "\\textit")))
(tex-def-prim "\\textless" (lambda () (emit "&lt;")))
(tex-def-prim "\\textperiodcentered" (lambda () (emit "&middot;")))
(tex-def-prim "\\textquestiondown" (lambda () (emit "&iquest;")))
(tex-def-prim "\\textquotedblleft" (lambda () (emit "&ldquo;")))
(tex-def-prim "\\textquotedblright" (lambda () (emit "&rdquo;")))
(tex-def-prim "\\textquoteleft" (lambda () (emit "&lsquo;")))
(tex-def-prim "\\textquoteright" (lambda () (emit "&rsquo;")))

(tex-def-prim "\\textregistered" (lambda () (emit "&reg;")))

(tex-def-prim "\\textrm" (lambda () (do-function "\\textrm")))

(tex-def-prim "\\textsc"
 (lambda ()
     (let ((*in-small-caps-p* t))
       (tex2page-string (get-group)))))

(tex-def-prim "\\textsl" (lambda () (do-function "\\textsl")))

(tex-def-prim "\\textasciitilde" (lambda () (emit "~")))

(tex-def-prim "\\texttt" (lambda () (do-function "\\texttt")))

(tex-def-prim "\\textvisiblespace" (lambda () (emit *verbatim-visible-space*)))

(tex-def-prim "\\TH" (lambda () (emit "&THORN;")))

(tex-def-prim "\\th" (lambda () (emit "&thorn;")))

(tex-def-prim "\\the" #'do-the)

(tex-def-prim "\\thebibliography" #'do-thebibliography)

(tex-def-prim "\\theindex" #'do-theindex)

(tex-def-prim "\\TIIPanchor" #'do-anchor-for-potential-label)

(tex-def-prim "\\TIIPbackslash" (lambda () (emit "\\")))

(tex-def-prim "\\TIIPbr" #'do-br)

(tex-def-prim "\\TIIPcmyk" (lambda () (do-switch :cmyk)))

(tex-def-prim "\\TIIPcsname" #'do-saved-csname)

(tex-def-prim "\\TIIPcomment" #'eat-till-eol)

(tex-def-prim "\\TIIPeatstar" #'eat-star)

(tex-def-prim "\\TIIPendgraf" #'do-end-para)

(tex-def-prim "\\TIIPpar" #'do-para-nopadding)

(tex-def-prim "\\TIIPfolio" #'point-to-adjacent-pages)

(tex-def-prim "\\TIIPgobblegroup" #'get-group)

(tex-def-prim "\\TIIPgray" (lambda () (do-switch :gray)))

(tex-def-prim "\\TIIPhlend" #'do-hlend)

(tex-def-prim "\\TIIPlatexenvasimage" #'do-following-latex-env-as-image)

(tex-def-prim "\\TIIPnbsp" (lambda () (emit-nbsp 1)))

(tex-def-prim "\\TIIPnewline" #'do-newline)

(tex-def-prim "\\TIIPnull" #'get-actual-char)

(tex-def-prim "\\TIIPreuseimage" #'reuse-img)

(tex-def-prim "\\TIIPrgb" (lambda () (do-switch :rgb)))

(tex-def-prim "\\TIIPRGB" (lambda () (do-switch :rgb255)))

(tex-def-prim "\\TIIPtheorem" #'do-theorem)

(tex-def-prim "\\TIIPrelax" #'do-relax)

(tex-def-prim "\\tiny" (lambda () (do-switch :tiny)))

(tex-def-prim "\\title" #'do-title)

(tex-def-prim "\\today" #'do-today)

(tex-def-prim "\\TM" (lambda () (emit "&trade;")))

(tex-def-prim "\\tracingall" #'do-tracingall)

(tex-def-prim "\\tt" (lambda () (do-switch :tt)))

(tex-def-prim "\\typein" #'do-typein)

(tex-def-prim "\\TZPauxdir" (lambda () (emit *aux-dir/*)))
(tex-def-prim "\\TZPlastpageno" (lambda () (emit *last-page-number*)))
(tex-def-prim-0arg "\\TZPcommonlisp" (if 'nil "0" "1"))

(tex-def-prim "\\undefcsactive" #'do-undefcsactive)

(tex-def-prim "\\undefschememathescape" (lambda () (scm-set-mathescape nil)))

(tex-def-prim "\\underline" (lambda () (do-function "\\underline")))

(tex-def-prim "\\unscmspecialsymbol" #'do-scm-unset-specialsymbol)

(tex-def-prim "\\uppercase" #'do-uppercase)

(tex-def-prim "\\url" #'do-url)

(tex-def-prim "\\urlh" #'do-urlh)

(tex-def-prim "\\urlhd" #'do-urlhd)

(tex-def-prim "\\urlp" #'do-urlp)

(tex-def-prim "\\v" (lambda () (do-diacritic :hacek)))

#|
(tex-def-prim "\\vdots"
 (lambda ()
     (emit "<tt><table><tr><td>.</td></tr>")
     (emit "<tr><td>.</td></tr>")
     (emit "<tr><td>.</td></tr></table></tt>")))

(tex-def-prim "\\ddots"
              (lambda ()
     (emit "<tt><table><tr><td>.</td></tr>")
     (emit "<tr><td></td><td>.</td></tr>")
     (emit "<tr><td></td><td></td><td>.</td></tr></table></tt>")))
|#

(tex-def-prim "\\vdots" (lambda () (emit "&#x22ee;")))

(tex-def-prim "\\verb" #'do-verb)

(tex-def-prim "\\verbatim" #'do-verbatim)

(tex-def-prim "\\verbatiminput" #'do-verbatiminput)

(tex-def-prim "\\verbc" #'do-verbc)

(tex-def-prim "\\verbatimescapechar" #'do-verbatimescapechar)

(tex-def-prim "\\verbwrite" #'do-verbwrite)

(tex-def-prim "\\verbwritefile" #'do-verbwritefile)

(tex-def-prim "\\vfootnote" #'do-vfootnote)

(tex-def-prim "\\vskip" #'do-vskip)

(tex-def-prim "\\vspace" #'do-vspace)

(tex-def-prim "\\write" #'do-write)

(tex-def-prim "\\writenumberedcontentsline" #'do-writenumberedcontentsline)

(tex-def-prim "\\writenumberedtocline" #'do-writenumberedtocline)

(tex-def-prim "\\xdef" (lambda () (do-def t t)))

(tex-def-prim "\\xrdef" #'do-xrdef)

(tex-def-prim "\\xrefn" #'do-ref)

(tex-def-prim "\\xrtag" #'do-tag)

(tex-def-prim "\\xspace" #'do-xspace)

(tex-def-prim "\\yen" (lambda () (emit "&yen;")))

(tex-def-prim "\\contentsname" (lambda () (emit "Contents")))

(tex-def-prim "\\listfigurename" (lambda () (emit "List of Figures")))

(tex-def-prim "\\listtablename" (lambda () (emit "List of Tables")))

(tex-def-prim "\\refname" (lambda () (emit "References")))

(tex-def-prim "\\indexname" (lambda () (emit "Index")))

(tex-def-prim "\\figurename" (lambda () (emit "Figure")))

(tex-def-prim "\\tablename" (lambda () (emit "Table")))

(tex-def-prim "\\partname" (lambda () (emit "Part")))

(tex-def-prim "\\appendixname" (lambda () (emit "Appendix")))

(tex-def-prim "\\abstractname" (lambda () (emit "Abstract")))

(tex-def-prim "\\bibname" (lambda () (emit "Bibliography")))

(tex-def-prim "\\chaptername" (lambda () (emit "Chapter")))

(tex-def-prim "\\\\" (lambda () (do-cr "\\\\")))

(tex-def-prim "\\`" (lambda () (do-diacritic :grave)))

(tex-def-prim "\\(" #'do-latex-intext-math)

(tex-def-prim "\\[" #'do-latex-display-math)

(tex-def-prim "\\)" #'egroup)

(tex-def-prim "\\]" #'egroup)

(tex-def-prim "\\{" (lambda () (emit "{")))

(tex-def-prim "\\}" (lambda () (emit "}")))

(tex-let-prim "\\-" "\\TIIPrelax")

(tex-def-prim "\\'" (lambda () (do-diacritic :acute)))

(tex-def-prim "\\="
              (lambda ()
                (unless (eql (car *tabular-stack*) :tabbing))
                (do-diacritic :macron)))

(tex-def-prim "\\>"
              (lambda ()
                (when (eql (car *tabular-stack*) :tabbing)
                  (emit-nbsp 3))))

(tex-def-prim "\\^" (lambda () (do-diacritic :circumflex)))

(tex-def-prim "\\~" (lambda () (do-diacritic :tilde)))

(tex-def-prim "\\#" (lambda () (emit "#")))

(tex-def-prim "\\ " (lambda () (emit #\space)))

(tex-def-prim "\\%" (lambda () (emit "%")))

(tex-def-prim "\\&" (lambda () (emit "&amp;")))

(tex-def-prim "\\@" (lambda () (emit "@")))

(tex-def-prim "\\_" (lambda () (emit "_")))

(tex-def-prim "\\$" (lambda () (emit "$")))

(tex-def-prim (concatenate 'string (list #\\ #\Newline)) #'emit-newline)

;TeX logos

(let* ((TeX *tex-logo*)
       (AmS (concatenate 'string "<span style=\"font-family: cursive;\">"
              "A"
              "<span style=\""
              "position: relative; "
              "top: 0.5ex; "
              "margin-left: -.1667em; "
              "margin-right: -.075em"
              "\">M</span>"
              "S</span>"))
       (Bib (concatenate 'string "B" "<span style=\""
              "text-transform: uppercase"
              "\"><small>ib</small></span>"))
       (ConTeXt (concatenate 'string "Con"
                  TeX
                  "t"))
       (LaTeX (concatenate 'string "L" "<span style=\""
                "position: relative; "
                "bottom: 0.3ex; "
                "margin-left: -0.36em; "
                "margin-right: -0.15em; "
                "text-transform: uppercase"
                "\"><small>a</small></span>"
                TeX))
       (Xe (concatenate 'string "X" "<span style=\""
              "text-transform: uppercase; "
              "position: relative; "
              "top: 0.5ex; "
              "margin-left: -0.125em; "
              "margin-right: -0.1667em"
              "\">&#x18e;</span>"))
       (thinspace (kern ".16667em"))
       (_2e (concatenate 'string "<span style=\""
              "margin-left: .05em"
              "\">2<span>"
              "<span style=\""
              "position: relative; "
              "top: .5ex"
              "\">&epsilon;</span>"))
       (MF (concatenate 'string "<span style=\""
             "font-family: sans-serif"
             "\">METAFONT</span>")))
  (tex-def-prim "\\AmSTeX" (lambda () (emit AmS) (emit #\-) (emit TeX)))
  (tex-def-prim "\\BibTeX" (lambda () (emit Bib) (emit TeX)))
  (tex-def-prim "\\ConTeXt" (lambda () (emit ConTeXt)))
  (tex-def-prim "\\eTeX" (lambda () (emit "&epsilon;-") (emit TeX)))
  (tex-def-prim "\\LaTeX" (lambda () (emit LaTeX)))
  (tex-def-prim "\\LaTeXe" (lambda () (emit LaTeX) (emit _2e)))
  (tex-def-prim "\\MF" (lambda () (emit MF)))
  (tex-def-prim "\\TeX" (lambda () (emit TeX)))
  (tex-def-prim "\\XeLaTeX" (lambda () (emit Xe) (emit thinspace) (emit LaTeX)))
  (tex-def-prim "\\XeTeX" (lambda () (emit Xe) (emit TeX))))

;ignoring these

(tex-let-prim "\\htmladvancedentities" "\\TIIPrelax")

(tex-let-prim "\\displaystyle" "\\TIIPrelax")

(tex-let-prim "\\textstyle" "\\TIIPrelax")

(tex-let-prim "\\endsloppypar" "\\TIIPrelax")

(tex-let-prim "\\frenchspacing" "\\TIIPrelax")

(tex-let-prim "\\oldstyle" "\\TIIPrelax")

(tex-let-prim "\\protect" "\\TIIPrelax")

(tex-let-prim "\\raggedbottom" "\\TIIPrelax")

(tex-let-prim "\\raggedright" "\\TIIPrelax")

(tex-let-prim "\\sloppy" "\\TIIPrelax")

(tex-let-prim "\\sloppypar" "\\TIIPrelax")

(tex-let-prim "\\beginpackages" "\\TIIPrelax")

(tex-let-prim "\\endpackages" "\\TIIPrelax")

(tex-let-prim "\\normalfont" "\\TIIPrelax")

(tex-let-prim "\\textnormal" "\\TIIPrelax")

(tex-let-prim "\\unskip" "\\TIIPrelax")

(tex-def-prim "\\cline" #'get-group)

(tex-def-prim "\\externalref" #'get-group)

(tex-def-prim "\\GOBBLEARG" #'get-group)

(tex-def-prim "\\hyphenation" #'get-group)

(tex-def-prim "\\newcounter" #'get-group)

(tex-def-prim "\\newlength" #'get-group)

(tex-def-prim "\\hphantom" #'get-group)

(tex-def-prim "\\vphantom" #'get-group)

(tex-def-prim "\\phantom" #'get-group)

(tex-def-prim "\\pagenumbering" #'get-group)

(tex-def-prim "\\pagestyle" #'get-group)

(tex-def-prim "\\raisebox" #'get-group)

(tex-def-prim "\\thispagestyle" #'get-group)

(tex-def-prim "\\manpagesection" #'get-group)

(tex-def-prim "\\manpagedescription" #'get-group)

(tex-def-prim "\\lstset" #'get-group)

(tex-def-prim "\\externallabels" (lambda () (get-group) (get-group)))

(tex-let-prim "\\markboth" "\\externallabels")

(tex-def-prim "\\columnsep" #'eat-dimen)

(tex-def-prim "\\columnseprule" #'eat-dimen)

(tex-def-prim "\\evensidemargin" #'eat-dimen)

(tex-def-prim "\\fboxsep" #'eat-dimen)

(tex-def-prim "\\headsep" #'eat-dimen)

(tex-def-prim "\\itemsep" #'eat-dimen)

(tex-def-prim "\\leftcodeskip" #'eat-dimen)

(tex-def-prim "\\lower" #'eat-dimen)

(tex-def-prim "\\oddsidemargin" #'eat-dimen)

(tex-def-prim "\\parsep" #'eat-dimen)

;(tex-def-prim "\\parskip" #'eat-dimen)

(tex-def-prim "\\raise" #'eat-dimen)

(tex-def-prim "\\rightcodeskip" #'eat-dimen)

(tex-def-prim "\\sidemargin" #'eat-dimen)

(tex-def-prim "\\textheight" #'eat-dimen)

(tex-def-prim "\\topmargin" #'eat-dimen)

(tex-def-prim "\\topsep" #'eat-dimen)

(tex-def-prim "\\vertmargin" #'eat-dimen)

(tex-def-prim "\\magstep" #'get-token)

(tex-def-prim "\\textfont" #'get-token)

(tex-def-prim "\\scriptfont" #'get-token)

(tex-def-prim "\\scriptscriptfont" #'get-token)

(tex-def-prim "\\addtolength" (lambda () (get-token) (get-token)))

(tex-let-prim "\\addvspace" "\\vspace")

(tex-let-prim "\\setlength" "\\addtolength")

(tex-let-prim "\\settowidth" "\\addtolength")

(tex-let-prim "\\hookaction" "\\addtolength")

(tex-def-prim "\\enlargethispage" (lambda () (eat-star) (get-group)))

(tex-def-prim "\\parbox" (lambda () (get-bracketed-text-if-any) (get-group)))

(tex-def-prim "\\ProvidesFile"
 (lambda () (get-group) (get-bracketed-text-if-any)))

(tex-def-prim "\\DeclareGraphicsRule"
 (lambda () (get-group) (get-group) (get-group) (get-group)))

(tex-def-prim "\\makebox"
 (lambda () (get-bracketed-text-if-any) (get-bracketed-text-if-any)))

(tex-let-prim "\\framebox" "\\makebox")

(tex-def-prim "\\rule"
 (lambda () (get-bracketed-text-if-any) (get-group) (get-group)))

(tex-def-prim "\\GOBBLEOPTARG" #'get-bracketed-text-if-any)

(tex-def-prim "\\nolinebreak" #'get-bracketed-text-if-any)

(tex-def-prim "\\nopagebreak" #'get-bracketed-text-if-any)

(tex-def-prim "\\hyphenchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\skewchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\usepackage"
 (lambda () (get-bracketed-text-if-any) (get-group) (probably-latex)))

(tex-def-prim "\\readindexfile" (lambda () (get-token) (do-inputindex)))

(tex-let-prim "\\colophon" "\\htmlcolophon")

(tex-let-prim "\\path" "\\verb")

(tex-let-prim "\\par" "\\endgraf")

(tex-let-prim "\\u" "\\`")

(tex-let-prim "\\vbox" "\\hbox")

(tex-let-prim "\\endabstract" "\\endquote")

(tex-let-prim "\\mbox" "\\hbox")

(tex-let-prim "\\supereject" "\\eject")

(tex-let-prim "\\dosupereject" "\\eject")

(tex-let-prim "\\endgroup" "\\egroup")

(tex-let-prim "\\begingroup" "\\bgroup")

(tex-let-prim "\\ldots" "\\dots")

(tex-let-prim "\\documentstyle" "\\documentclass")

(tex-let-prim "\\H" "\\\"")

(tex-let-prim "\\/" "\\TIIPrelax")

(tex-let-prim "\\leavevmode" "\\TIIPrelax")

(tex-let-prim "\\space" "\\ ")

(tex-let-prim "\\quotation" "\\quote")

(tex-let-prim "\\endquotation" "\\endquote")

(tex-let-prim "\\TIIPdate" "\\today")

(tex-let-prim "\\schemeinput" "\\scminput")

(tex-let-prim "\\lispinput" "\\scminput")

(tex-let-prim "\\obeywhitespaces" "\\obeywhitespace")

(tex-let-prim "\\ensuremath" "\\mathg")

(tex-let-prim "\\epsffile" "\\epsfbox")

(tex-let-prim "\\htmlimgformat" "\\htmlimageformat")

(tex-let-prim "\\p" "\\verb")

(tex-let-prim "\\ttraggedright" "\\tt")

(tex-let-prim "\\ttfamily" "\\tt")

(tex-let-prim "\\htmladdnormallink" "\\urlp")

(tex-let-prim "\\htmladdnormallinkfoot" "\\urlp")

(tex-let-prim "\\pagehtmlref" "\\htmlref")

(tex-let-prim "\\circledR" "\\textregistered")

(tex-let-prim "\\registered" "\\textregistered")

(tex-let-prim "\\scmconstant" "\\scmbuiltin")

(tex-let-prim "\\setbuiltin" "\\scmbuiltin")

(tex-let-prim "\\setconstant" "\\scmconstant")

(tex-let-prim "\\setkeyword" "\\scmkeyword")

(tex-let-prim "\\setvariable" "\\scmvariable")

(tex-let-prim "\\unssetspecialsymbol" "\\unscmspecialsymbol")

(tex-let-prim "\\setspecialsymbol" "\\scmspecialsymbol")

(tex-let-prim "\\scmp" "\\scm")

(tex-let-prim "\\q" "\\scm")

(tex-let-prim "\\scheme" "\\scm")

(tex-let-prim "\\tagref" "\\ref")

(tex-let-prim "\\numfootnote" "\\numberedfootnote")

(tex-let-prim "\\f" "\\numberedfootnote")

(tex-let-prim "\\newpage" "\\eject")

(tex-let-prim "\\clearpage" "\\eject")

(tex-let-prim "\\cleardoublepage" "\\eject")

(tex-let-prim "\\htmlpagebreak" "\\eject")

(tex-let-prim "\\typeout" "\\message")

(tex-let-prim "\\unorderedlist" "\\itemize")

(tex-let-prim "\\li" "\\item")

(tex-let-prim "\\htmlstylesheet" "\\inputcss")

(tex-let-prim "\\hr" "\\hrule")

(tex-let-prim "\\htmlrule" "\\hrule")

(tex-let-prim "\\numberedlist" "\\enumerate")

(tex-let-prim "\\orderedlist" "\\enumerate")

(tex-let-prim "\\endunorderedlist" "\\enditemize")

(tex-let-prim "\\endnumberedlist" "\\endenumerate")

(tex-let-prim "\\endorderedlist" "\\endenumerate")

(tex-let-prim "\\newline" "\\break")

(tex-let-prim "\\gifdef" "\\imgdef")

(tex-let-prim "\\schemeeval" "\\eval")

(tex-let-prim "\\gifpreamble" "\\imgpreamble")

(tex-let-prim "\\mathpreamble" "\\imgpreamble")

(tex-let-prim "\\scmverbatim" "\\scm")

(tex-let-prim "\\scmfilename" "\\verbwritefile")

(tex-let-prim "\\scmwritefile" "\\verbwritefile")

(tex-let-prim "\\verbfilename" "\\verbwritefile")

(tex-let-prim "\\scmfileonly" "\\verbwrite")

(tex-let-prim "\\scmverbatimfile" "\\scminput")

(tex-let-prim "\\scmverbatiminput" "\\scminput")

(tex-let-prim "\\scmwrite" "\\verbwrite")

(tex-let-prim "\\scmfile" "\\scmdribble")

(tex-let-prim "\\scmverb" "\\scm")

(tex-let-prim "\\verbinput" "\\verbatiminput")

(tex-let-prim "\\verbatimfile" "\\verbatiminput")

(tex-let-prim "\\verbescapechar" "\\verbatimescapechar")

(tex-let-prim "\\setverbatimescapechar" "\\verbescapechar")

(tex-let-prim "\\nohtmlmathimg" "\\dontuseimgforhtmlmath")

(tex-let-prim "\\nohtmlmathintextimg" "\\dontuseimgforhtmlmathintext")
(tex-let-prim "\\leqalignno" "\\eqalignno")

(tex-let-prim "\\nohtmlmathdisplayimg" "\\dontuseimgforhtmlmathdisplay")
(tex-let-prim "\\writetotoc" "\\writenumberedtocline")

(tex-let-prim "\\textdegree" "\\degree")
(tex-let-prim "\\leqno" "\\eqno")

(tex-let-prim "\\trademark" "\\TM")

(tex-let-prim "\\texttrademark" "\\TM")

(tex-let-prim "\\n" "\\noindent")

;for use within inner \eval

(tex-let-prim "\\Lbackslash" "\\char`\\\\")
(tex-let-prim "\\Ltilde" "\\char`\\~")
(tex-let-prim "\\Llbrace" "\\char`\\{")
(tex-let-prim "\\Lrbrace" "\\char`\\}")
(tex-let-prim "\\Lsup" "\\char`\\^")
(tex-let-prim "\\Lsub" "\\char`\\_")

(defun tex2page (tex-file)
  (unless (= *write-log-index* 0)
    ;nested call to tex2page
    (terpri))
  (when (or (not tex-file) (string= tex-file ""))
    (setq tex-file "--missing-arg"))
  (let (
        (*afterassignment* nil)
        (*afterbye* '())
        (*afterpar* '())
        (*aux-dir* nil)
        (*aux-dir/* "")
        (*aux-port* nil)
        (*bib-aux-port* nil)
        (*bibitem-num* 0)
        (*color-names* '())
        (*comment-char* #\%)
        (*css-port* nil)
        (*current-source-file* nil)
        (*current-tex2page-input* nil)
        (*display-justification* "center")
        (*doctype* *doctype*)
        (*dotted-counters* nil)
        (*dumping-nontex-p* nil)
        (*equation-number* nil)
        (*equation-numbered-p* t)
        (*equation-position* 0)
        (*esc-char* #\\)
        (*esc-char-std* #\\)
        (*esc-char-verb* #\|)
        (*eval-file-count* 0)
        (*eval-for-tex-only-p* nil)
        (*external-label-tables* (make-hash-table :test #'equal))
        (*footnote-list* '())
        (*footnote-sym* 0)
        (*global-texframe* nil)
        ;(*global-texframe* (make-texframe*))
        (*graphics-file-extensions* '(".eps"))
        (*html* nil)
        (*html-head* '())
        (*html-only* 0)
        (*html-page* nil)
        (*html-page-count* 0)
        (*img-file-count* 0)
        (*img-file-tally* 0)
        (*imgdef-file-count* 0)
        (*imgpreamble* "")
        (*imgpreamble-inferred* '())
        (*in-alltt-p* nil)
        (*in-display-math-p* nil)
        (*in-para-p* nil)
        (*in-small-caps-p* nil)
        (*includeonly-list* t)
        (*index-count* 0)
        (*index-page* nil)
        (*index-port* nil)
        (*index-table* (make-hash-table))
        (*infructuous-calls-to-tex2page* 0)
        (*input-line-no* 0)
        (*input-streams* (make-hash-table))
        (*inputting-boilerplate-p* nil)
        (*inside-appendix-p* nil)
        (*jobname* "texput")
        (*label-port* nil)
        (*label-source* nil)
        (*label-table* (make-hash-table :test #'equal))
        (*last-modification-time* nil)
        (*last-page-number* -1)
        (*latex-probability* 0)
        (*ligatures-p* t)
        (*loading-external-labels-p* nil)
        (*log-file* nil)
        (*log-port* nil)
        (*main-tex-file* nil)
        (*math-delim-left* nil)
        (*math-delim-right* nil)
        (*math-height* 0)
        (*math-mode-p* nil)
        (*mfpic-file-num* nil)
        (*mfpic-file-stem* nil)
        (*mfpic-port* nil)
        (*missing-eps-files* '())
        (*missing-pieces* '())
        (*mp-files* '())
        (*not-processing-p* nil)
        (*outer-p* t)
        (*output-streams* (make-hash-table))
        (*outputting-external-title-p* nil)
        (*outputting-to-non-html-p* nil)
        (*package* *this-package*)
        (*reading-control-sequence-p* nil)
        (*recent-node-name* nil)
        (*redirect-delay* nil)
        (*redirect-url* nil)
        (*scm-builtins* nil)
        (*scm-dribbling-p* nil)
        (*scm-keywords* nil)
        (*scm-special-symbols* nil)
        (*scm-variables* nil)
        (*section-counter-dependencies* nil)
        (*section-counters* (make-hash-table))
        (*slatex-math-escape* nil)
        (*source-changed-since-last-run-p* nil)
        (*stylesheets* '())
        (*subjobname* nil)
        (*tabular-stack* '())
        (*temp-string-count* 0)
        (*temporarily-use-utf8-for-math-p* nil)
        (*tex-env* '())
        (*tex-format* :plain)
        (*tex-if-stack* '())
        (*tex-like-layout-p* *tex-like-layout-p*)
        (*tex-output-format* nil)
        (*tex-prog-name* *tex-prog-name*)
        (*tex2page-inputs* (path-to-list (retrieve-env "TEX2PAGEINPUTS")))
        (*title* nil)
        (*toc-list* '())
        (*toc-page* nil)
        (*unresolved-xrefs* '())
        (*using-bibliography-p* nil)
        (*using-chapters-p* nil)
        (*using-index-p* nil)
        (*verb-display-p* nil)
        (*verb-port* nil)
        (*verb-visible-space-p* nil)
        (*verb-written-files* '())
        (*write-log-index* 0)
        (*write-log-possible-break-p* nil)
        )
    (initialize-globals)
    (setq *main-tex-file*
          (actual-tex-filename tex-file
                               (check-input-file-timestamp-p tex-file)))
    ;(load-aux-file)
    (write-log "This is TeX2page, Version ")
    (write-log *tex2page-version*)
    (write-log #\space)
    (write-log #\()
    (write-log *common-lisp-version*)
    (write-log #\))
    (write-log :separation-newline)
    (cond (*main-tex-file*
           (setq *subjobname* *jobname*
                 *html-page*
                 (concatenate 'string *aux-dir/* *jobname* *output-extension*))
           (setq *html* (open *html-page* :direction :output
                              :if-exists :supersede))
           (do-start)
           (let ((*html-only* (1+ *html-only*)))
             (tex2page-file-if-exists (file-in-home ".tex2page.t2p"))
             (tex2page-file-if-exists ".tex2page.t2p")
             (let ((f (actual-tex-filename
                       (concatenate 'string *jobname* ".t2p"))))
               (when f (tex2page-file f))))
           (unless (eql (tex2page-file *main-tex-file*) :encountered-bye)
             (insert-missing-end))
           (do-bye))
          (t (tex2page-help tex-file)))
    (output-stats)))

;(trace do-math do-display-math do-intext-math do-math-fragment do-math-left do-math-right
;       bgroup-math-hook bgroup egroup )

(tex2page *tex2page-file-arg*)
