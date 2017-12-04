":"; export T2PARG=$1
":"; if test -z "$LISP"; then export LISP=ecl; fi
":"; if test "$LISP" = abcl; then exec abcl --load $0 --batch "$@"
":"; elif test "$LISP" = allegro; then exec alisp -L $0 -kill
":"; elif test "$LISP" = clisp; then exec clisp $0 -q "$@"
":"; elif test "$LISP" = clozure; then exec ccl -l $0 -e '(ccl:quit)' -- "$@"
":"; elif test "$LISP" = cmucl; then exec lisp -quiet -load $0 -eval '(ext:quit)' "$@"
":"; elif test "$LISP" = ecl; then exec ecl -shell $0 "$@"
":"; elif test "$LISP" = mkcl; then exec mkcl -shell $0 -- "$@"
":"; elif test "$LISP" = sbcl; then exec sbcl --script $0 "$@"
":"; fi

;tex2page
;Dorai Sitaram
;For details, see
;http://ds26gte.github.io/tex2page/index.html

#+sbcl
(declaim
  (sb-ext:muffle-conditions style-warning))

#+sbcl
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 2))))

(defpackage :tex2page
  (:use :cl)
  (:export :tex2page))

(in-package :tex2page)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *print-case* :downcase
        *load-verbose* nil
        *compile-verbose* nil))

(defparameter *tex2page-version* "20171204") ;last change

(defparameter *tex2page-website*
  ;for details, please see
  "http://ds26gte.github.io/tex2page/index.html")

(defparameter *tex2page-copyright-notice*
  (concatenate 'string "Copyright (C) 1997-"
    (subseq *tex2page-version* 0 4) " Dorai Sitaram"))

(defun retrieve-env (s)
  (declare (string s))
  #+abcl (ext:getenv s)
  #+allegro (sys:getenv s)
  #+clisp (ext:getenv s)
  #+clozure (ccl:getenv s)
  #+cmucl (cdr (assoc (intern s :keyword)
                      ext:*environment-list* :test #'string=))
  #+ecl (ext:getenv s)
  #+mkcl (mkcl:getenv s)
  #+sbcl (sb-ext:posix-getenv s))

#+sbcl
(sb-alien:define-alien-routine system sb-alien:int (command sb-alien:c-string))

#-sbcl
(defun system (cmd)
  (declare (string cmd))
  #+abcl (ext:run-shell-command cmd)
  #+allegro (excl:shell cmd)
  #+clisp (ext:shell cmd)
  #+clozure (ccl::os-command cmd)
  #+cmucl (ext:run-program "sh" (list "-c" cmd) :output t)
  #+ecl (ext:system cmd)
  #+mkcl (mkcl:system cmd))

(defun string=split (p sepc)
  ;convert a Unix path into a Lisp list
  (if (not p) '()
    (let ((p p) (r '()))
      (loop
        (let ((i (position sepc p :test #'char=)))
          (unless i (push p r) (return (nreverse r)))
          (push (subseq p 0 i) r)
          (setq p (subseq p (1+ i))))))))

(defun system-with-visual (cmd)
  (cond ((and (not 'nil) (or #+ecl t #+mkcl t))
         (let ((s (string=split cmd #\space)))
           (#+ecl ext:run-program #+mkcl mkcl:run-program
            (car s) (cdr s) :input t :output t)))
        (t (system cmd))))

(defparameter *tex2page-file-arg*
  (or #+abcl (nth 0 ext:*command-line-argument-list*)
      #+clisp (nth 1 ext:*args*)
      #+clozure (nth 6 ccl:*command-line-argument-list*)
      #+cmucl (nth 6 ext:*command-line-strings*)
      #+ecl (nth 3 (ext:command-args))
      #+mkcl (nth 4 (si:command-args))
      #+sbcl (nth 1 sb-ext:*posix-argv*)
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

(defparameter *month-names*
  (vector "January" "February" "March" "April" "May" "June" "July" "August"
          "September" "October" "November" "December"))

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
   "<span style=\"color: red\">&#xb7;</span>"
  ;"<span style=\"vertical-align: -0.5ex\">&#x2334;</span>"
  )

(defparameter *aux-file-suffix* "-Z-A.lisp")
(defparameter *bib-aux-file-suffix* "-Z-B")
(defparameter *css-file-suffix* "-Z-S.css")
(defparameter *eval4tex-file-suffix* "-Z-E.lisp")
;(defparameter *html-node-prefix* "node_")
(defparameter *html-node-prefix* "TAG:__tex2page_")
(defparameter *html-page-suffix* "-Z-H-")
(defparameter *img-file-suffix* "-Z-G-")
(defparameter *imgdef-file-suffix* "D-")
(defparameter *index-file-suffix* "-Z-I")
(defparameter *label-file-suffix* "-Z-L.lisp")
(defparameter *mfpic-tex-file-suffix* ".Z-M-tex")
(defparameter *toc-file-suffix* "-Z-C")

(defparameter *ghostscript-options*
  " -q -dBATCH -dNOPAUSE -dNO_PAUSE -sDEVICE=ppmraw")

;Not using #. because some Schemes choke on #\nul.
(defparameter *invisible-space* (code-char 0))

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

(defparameter *tex-files-to-ignore*
  '("btxmac" "eplain" "epsf" "lmfonts" "mfpic" "supp-pdf"))

;(defparameter *esc-char-initex* #\\)
;(defparameter *esc-char-universal* #\Ǝ)

(defparameter **escape** 0)
(defparameter **bgroup** 1)
(defparameter **egroup** 2)
(defparameter **math** 3)
(defparameter **alignment** 4)
(defparameter **parameter** 6)
(defparameter **ignore** 9)
(defparameter **space** 10)
(defparameter **letter** 11)
(defparameter **other** 12)
(defparameter **active** 13)
(defparameter **comment** 14)

(defparameter *catcodes*
  (list
    ;Set by tex2page
    ;(cons *esc-char-universal*  0)
    ;
    ;TeXbook, p 343
    ;Set by INITEX
    (cons #\\ **escape**)
    (cons #\space **space**)
    (cons #\% **comment**)
    (cons (code-char 0) **ignore**)
    (cons #\return 5)
    (cons #\newline 5) ;TeX seems to read #\newline as #\return
    ;Set by plain
    (cons #\{ **bgroup**) ;begin group
    (cons #\} **egroup**) ;end group
    (cons #\$ **math**) ;math shift
    (cons #\& **alignment**) ;alignment tab
    (cons #\# **parameter**) ;macro parameter
    (cons #\^ 7) ;superscript
    (cons #\_ 8) ;subscript
    (cons #\tab **space**) ;space
    (cons #\~ **active**) ;tilde is active
    ))

;above are true globals.  Following are
;per-document globals

(defvar *afterassignment* nil)
(defvar *afterpar* '())
(defvar *afterbye* '())
(defvar *aux-dir* nil)
(defvar *aux-dir/* "")
(defvar *aux-stream* nil)

(defvar *basic-style* "")
(defvar *bib-aux-stream* nil)
(defvar *bibitem-num* 0)

(defvar *color-names* '())
(defvar *css-stream* nil)
(defvar *current-source-file* nil)
(defvar *current-tex2page-input* nil)

(defvar *display-justification* nil)
(defvar *dotted-counters* nil)
(defvar *dumping-nontex-p* nil)

(defvar *equation-number* nil)
(defvar *equation-numbered-p* t)
(defvar *equation-position* 0)
(defvar *esc-char-verb* #\|)
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
(defvar *index-stream* nil)
(defvar *index-table* nil)
(defvar *infructuous-calls-to-tex2page* nil)
(defvar *input-line-no* 0)
(defvar *input-streams* nil)
(defvar *inputting-boilerplate-p* nil)
(defvar *inside-appendix-p* nil)
(defvar *inside-eplain-verbatim-p* nil)
(defvar *it*)

(defvar *jobname* nil)

(defvar *label-stream* nil)
(defvar *label-source* nil)
(defvar *label-table* nil)
(defvar *last-modification-time* nil)
(defvar *last-page-number* nil)
(defvar *latex-probability* nil)
(defvar *ligatures-p* nil)
(defvar *loading-external-labels-p* nil)
(defvar *log-file* nil)
(defvar *log-stream* nil)

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
(defvar *mfpic-stream* nil)
(defvar *missing-eps-files* nil)
(defvar *missing-pieces* nil)
(defvar *mp-files* nil)

(defvar *not-processing-p* nil) ;mark for oblivion

(defvar *opmac-active-tt-char* nil)
(defvar *opmac-index-sub-table* nil)
(defvar *opmac-list-style* #\o)
(defvar *opmac-nonum-p* nil)
(defvar *opmac-notoc-p* nil)
(defvar *opmac-verbinput-table* nil)
(defvar *outer-p* t)
(defvar *output-streams* nil)
(defvar *outputting-external-title-p* nil)
(defvar *outputting-to-non-html-p* nil)

(defvar *quote-level* 0)

(defvar *reading-control-sequence-p* nil)
(defvar *recent-node-name* nil)
(defvar *remember-index-number* nil)
(defvar *redirect-delay* nil)
(defvar *redirect-url* nil)
(defvar *reg-num-count* 0)

(defvar *scm-builtins* nil)
(defvar *scm-dribbling-p* nil)
(defvar *scm-special-symbols* nil)
(defvar *scm-keywords* nil)
(defvar *scm-variables* nil)
(defvar *scripts* nil)
(defvar *section-counters* nil)
(defvar *section-counter-dependencies* nil)
(defvar *slatex-math-escape* nil)
(defvar *source-changed-since-last-run-p* nil)
(defvar *start-time* 0)
(defvar *stylesheets* nil)
(defvar *subjobname* *jobname*)

(defvar *tabular-stack* '())
(defvar *temp-string-count* 0)
(defvar *temporarily-use-utf8-for-math-p* nil)
(defvar *tex-env* '())
(defvar *tex-format* nil)
(defvar *tex-if-stack* '())
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
(defvar *verb-stream* nil)
(defvar *verb-visible-space-p* nil)
(defvar *verb-written-files* '())

(defvar *write-log-max* 55)
(defvar *write-log-index* 0)
(defvar *write-log-possible-break-p* nil)

(defvar *scm-token-delims*
  (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

(defstruct counter* (value 0) (within nil))

(defstruct tocentry* level number page label header)

(defstruct istream* (stream nil) (buffer '()))

(defstruct ostream* (stream nil) (hbuffer '()))

(defstruct texframe*
  (definitions (make-hash-table :test #'equal))
  (cdefinitions (make-hash-table))
  (chardefs (make-hash-table :test #'equal))
  (countdefs (make-hash-table :test #'equal))
  (counts (make-hash-table))
  (dimendefs (make-hash-table :test #'equal))
  (dimens (make-hash-table))
  (toksdefs (make-hash-table :test #'equal))
  (tokses (make-hash-table))
  (boxes (make-hash-table :test #'equal))
  (istreams (make-hash-table))
  (ostreams (make-hash-table))
  (postludes '())
  (uccodes (make-hash-table))
  (lccodes (make-hash-table))
  (catcodes '())
  (aftergroups '()))

(defstruct tdef*
  (argpat '())
  (expansion "")
  (optarg nil)
  (thunk nil)
  (prim nil)
  (defer nil)
  (catcodes nil))

(defstruct cdef*
  (argpat nil)
  (expansion nil)
  (optarg nil)
  (active nil)
  (catcodes nil))

(defstruct footnotev* mark text tag caller)

(defstruct label* (src nil) page name value)

(defun gen-temp-string ()
  (incf *temp-string-count*)
  (concatenate 'string "Temp_" (write-to-string *temp-string-count*)))

(defun file-stem-name (f)
  (declare (string f))
  (let ((slash (position #\/ f :test #'char= :from-end t)))
    (when slash (setq f (subseq f (1+ slash))))
    (let ((dot (position #\. f :test #'char= :from-end t)))
      (if dot (subseq f 0 dot) f))))

(defun file-extension (f)
  (declare (string f))
  (let ((slash (position #\/ f :test #'char= :from-end t))
        (dot (position #\. f :test #'char= :from-end t)))
    (if (and dot (not (= dot 0)) (or (not slash) (< (1+ slash) dot)))
        (subseq f dot) nil)))

(defun ensure-file-deleted (f)
  (declare (string f))
  (when (probe-file f) (delete-file f)))

(defun write-aux (e)
  (unless *aux-stream*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *aux-file-suffix*)))
      (setq *aux-stream* (open f :direction :output :if-exists :supersede))))
  (prin1 e *aux-stream*) (terpri *aux-stream*))

(defun write-label (e)
  (unless *label-stream*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *label-file-suffix*)))
      (setq *label-stream* (open f :direction :output :if-exists :supersede))))
  (prin1 e *label-stream*)
  (terpri *label-stream*))

(defun write-bib-aux (x)
  (unless *bib-aux-stream*
    (let ((f (concatenate 'string *aux-dir/* *jobname* *bib-aux-file-suffix* ".aux")))
      (setq *bib-aux-stream* (open f :direction :output :if-exists :supersede))))
  (princ x *bib-aux-stream*))

(defun write-log (x &optional log-file-only-p)
  (unless *log-stream*
    (setq *log-file* (concatenate 'string *aux-dir/* *jobname* ".hlog")
          *log-stream* (open *log-file* :direction :output :if-exists :supersede)))
  (when (and *write-log-possible-break-p* (characterp x)
             (member x '(#\) #\] #\} #\,) :test #'char=))
    (setq *write-log-possible-break-p* nil))
  (when (and *write-log-possible-break-p* (> *write-log-index* *write-log-max*))
    (terpri *log-stream*)
    (unless log-file-only-p (terpri))
    (setq *write-log-possible-break-p* nil
          *write-log-index* 0))
  (unless (and (= *write-log-index* 0)
               (member x '(:separation-newline :separation-space)))
    (case x
      ((#\newline :separation-newline)
       (when *write-log-possible-break-p*
         (setq *write-log-possible-break-p* nil))
       (terpri *log-stream*)
       (unless log-file-only-p (terpri))
       (setq *write-log-index* 0))
      (:separation-space (setq *write-log-possible-break-p* t))
      (t (when *write-log-possible-break-p*
           (write-char #\space *log-stream*)
           (unless log-file-only-p (write-char #\space))
           (setq *write-log-index* (1+ *write-log-index*))
           (setq *write-log-possible-break-p* nil))
         (princ x *log-stream*)
         (unless log-file-only-p (princ x)
           (force-output))
         (incf *write-log-index*
               (typecase x
                 (character 1)
                 (number (length (write-to-string x)))
                 (string (length x))
                 (t 1)))))))

;(trace write-log)

(defun display-error-context-lines ()
  (let ((n (or (find-count "\\errorcontextlines") 0)))
    (when (and *current-source-file* (> n 0))
      (let* ((n1 (max 0 (- *input-line-no* (floor (1- n) 2))))
             (nf (+ n1 n -1))
             (ll (with-open-file (ip *current-source-file* :direction :input)
                   (let ((i 1) (ll '()))
                     (loop (let ((L (read-line ip nil)))
                             (cond ((not L) (return ll))
                                   ((< i n1) (incf i))
                                   ((<= i nf) (incf i)
                                    (push (cons i L) ll))
                                   (t (return ll)))))))))
        (unless (null ll)
          (let* ((border "__________________________...")
                 (only-1-p (= (list-length ll) 1))
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
  (let ((calling-from-text-editor-p (retrieve-env "VIMRUNTIME")))
    (unless calling-from-text-editor-p
      (princ "Type E to edit your file, X to quit.")
      (terpri)
      (princ "? ")
      (force-output)
      (let ((c (read-char nil nil)))
        (when (and c (char-equal c #\e))
          (let ((texedit-string (retrieve-env "TEXEDIT"))
                ill-formed-texedit-p)
            (when texedit-string
              (cond ((setq *it* (search "%d" texedit-string))
                     (let ((i *it*))
                       (setq texedit-string (concatenate 'string (subseq texedit-string 0 i)
                                              (write-to-string *input-line-no*)
                                              (subseq texedit-string (+ i 2))))))
                    (t (setq ill-formed-texedit-p t texedit-string nil))))
            (when texedit-string
              (cond ((setq *it* (search "%s" texedit-string))
                     (let ((i *it*))
                       (setq texedit-string (concatenate 'string (subseq texedit-string 0 i)
                                              *current-source-file*
                                              (subseq texedit-string (+ i 2))))))
                    (t (setq ill-formed-texedit-p t texedit-string nil))))
            (unless texedit-string
              (when ill-formed-texedit-p
                (princ "Ill-formed TEXEDIT; using EDITOR.")
                (terpri))
              (when (setq *it* (or (retrieve-env "EDITOR") "vi"))
                (let ((e *it*))
                  (setq texedit-string
                        (concatenate 'string e
                          " +" (write-to-string *input-line-no*)
                          " " *current-source-file*)))))
            (when texedit-string
              (system-with-visual texedit-string))))))))

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
  (write-log *current-source-file*)
  (write-log #\:)
  (write-log *input-line-no*)
  (write-log ": error")
  (write-log :separation-newline)
  (write-log "l.")
  (write-log *input-line-no*)
  (write-log #\space)
  (write-log where)
  (write-log " failed.")
  (write-log :separation-newline)
  (display-error-context-lines)
  (close-all-open-streams)
  (output-stats)
  (edit-offending-file)
  (error "TeX2page fatal error"))

(defun do-errmessage ()
  (write-log :separation-newline)
  (write-log "! ")
  (write-log (tex-string-to-html-string (get-group)))
  (write-log :separation-newline)
  (terror 'do-errmessage))

(defun do-tracingall ()
  (plain-count "\\tracingcommands" 1 nil)
  (plain-count "\\tracingmacros" 1 nil))

(defun call-with-input-file/buffered (f th)
  (declare (string f) (function th))
  (unless (probe-file f)
    (terror 'call-with-input-file/buffered "I can't find file " f))
  (with-open-file (i f :direction :input)
    (let ((*current-tex2page-input* (make-istream* :stream i))
          (*current-source-file* f)
          (*input-line-no* 1))
      (funcall th))))

(defun call-with-input-string/buffered (s th)
  (declare (string s) (function th))
  (let ((*current-tex2page-input* (make-istream* :buffer (concatenate 'list s)))
        (*input-line-no* *input-line-no*))
    (funcall th)))

(defun snoop-char ()
  (let ((c (get-char)))
    (toss-back-char c)
    c))

(defun get-char ()
  (let ((b (istream*-buffer *current-tex2page-input*)))
    (if (null b)
        (let ((p (istream*-stream *current-tex2page-input*)))
          (if (not p) nil
              (let ((c (read-char p nil)))
                (cond ((not c) c)
                      ((char= c #\newline)
                       (incf *input-line-no*) c)
                      (t c)))))
        (let ((c (car b)))
          (setf (istream*-buffer *current-tex2page-input*) (cdr b))
          c))))

(defun toss-back-string (s)
  (declare (string s))
  (setf (istream*-buffer *current-tex2page-input*)
        (nconc (concatenate 'list s)
               (istream*-buffer *current-tex2page-input*))))

(defun toss-back-char (c)
  (push c (istream*-buffer *current-tex2page-input*)))

(defun snoop-actual-char ()
  (let ((c (snoop-char)))
    (cond ((not c) c)
          ((= (catcode c) **ignore**) (get-char)
                                 ;(check-outerness c)
                                 (snoop-actual-char))
          ((char= c #\return) (get-char)
           (let ((c (snoop-actual-char)))
             (if (and c (char= c #\newline)) c
                 (progn (toss-back-char #\newline) #\newline))))
          (t c))))

(defun get-actual-char ()
  (let ((c (get-char)))
    (cond ((not c) c)
          ((= (catcode c) **ignore**)
           ;(check-outerness c)
           (get-actual-char))
          ((char= c #\return)
           (let ((c (snoop-actual-char)))
             (if (and c (char= c #\newline))
                 (get-actual-char) #\newline)))
          (t c))))

(defun get-line ()
  (let ((r '()))
    (loop (let ((c (get-actual-char)))
            (cond ((not c)
                   (return (if r (concatenate 'string (nreverse r)) c)))
                  ((char= c #\newline)
                   (return (concatenate 'string (nreverse r))))
                  (t (push c r)))))))

(defun char-whitespace-p (c)
  (or (char= c #\space) (char= c #\tab)
      (not (graphic-char-p c))))

(defun ignorespaces (&optional (newlines :stop-before-par))
  ;(format t "ignorespaces ~s~%" newlines)
  ; :stop-before-first-newline
  ; :stop-after-first-newline
  ; :stop-before-par : eat non-newline spaces after 1st newline.
  ;                    if 2nd newline found, add another to it to retain \par
  ; :all : eat all whitespace
  ;
  (unless (/= (catcode #\space) 10)
    (let ((newline-active-p (/= (catcode #\newline) 5))
          (num-newlines-read 0)
          ;(num-spaces-read 0)
          c)
      (loop
        (setq c (snoop-char))
        ;(format t "c = ~s~%" c)
        (when (eql c #\return) (setq c (snoop-actual-char)))
        (cond ((not c) (return))
              ((= (catcode c) **ignore**)
               (get-char)
               (when *reading-control-sequence-p* (return)))
              ((char= c #\newline)
               (if newline-active-p (return)
                   (case newlines
                     (:stop-before-first-newline (return))
                     (:stop-after-first-newline (get-actual-char) (return))
                     (:stop-before-par
                      (case num-newlines-read
                        (0 (get-actual-char) (incf num-newlines-read))
                        (t (toss-back-char #\newline) (return))))
                     (t (get-actual-char)))))
              ((char-whitespace-p c)
               ;(incf num-spaces-read)
               (get-actual-char))
              (t (return))))
      ;(format t "num-spaces-read = ~s~%" num-spaces-read)
      )))

;(trace ignorespaces)

(defun munch-newlines ()
  (let ((n 0))
    (loop (let ((c (snoop-actual-char)))
            (cond ((not c) (return n))
                  ((char= c #\newline) (get-actual-char) (incf n))
                  ((char-whitespace-p c) (get-actual-char))
                  (t (return n)))))))

(defun munched-a-newline-p ()
  (loop (let ((c (snoop-actual-char)))
          (cond ((not c) (return nil))
                ((char= c #\newline) (get-actual-char) (return t))
                ((char-whitespace-p c) (get-actual-char))
                (t (return nil))))))

(defun do-xspace ()
  (let ((c (snoop-actual-char)))
    (unless (member c '(#\space #\" #\. #\! #\, #\: #\; #\? #\/ #\' #\) #\-))
      (emit-space #\space))))

(defun do-relax () t)

(defun get-ctl-seq ()
  (ignorespaces)
  (let ((bs (get-actual-char)))
    (unless (= (catcode bs) **escape**)
      (terror 'get-ctl-seq "Missing control sequence (" bs ")"))
    (let ((c (get-char)))
      (cond ((not c) "\\ ")
            ((= (catcode c) **ignore**) "\\ ")
            ((= (catcode c) **letter**)
             (concatenate 'string
               (nreverse
                 (let ((s (list c #\\)))
                   (loop
                     (let ((c (snoop-char)))
                       (cond ((not c) (return s))
                             ((= (catcode c) **ignore**) (return s))
                             ((= (catcode c) **letter**)
                              (get-char)
                              (push c s))
                             (t (ignorespaces :stop-before-first-newline)
                                (return s)))))))))
            (t (concatenate 'string (list #\\ c)))))))

;(trace get-ctl-seq)

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
    (when (not c) (terror 'get-group "Runaway argument?"))
    (unless (char= c #\{) (terror 'get-group "Missing {"))
    (let ((s (list c)) (nesting 0) (escape-p nil))
      (loop
        (let ((c (get-actual-char)))
          (when (not c)
            (terror 'get-group "Runaway argument?"))
          (cond (escape-p
                 (push c s) (setq escape-p nil))
                ((= (catcode c) **escape**)
                 (if *expand-escape-p*
                     (let ((s1 (progn
                             (toss-back-char c)
                             (let ((x (get-ctl-seq)))
                               (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                                      (concatenate 'string
                                        (list (char x 1))))
                                     (t (tex-string-to-html-string x)))))))
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
  (string-trim '(#\space #\Tab #\newline #\Return) s))

(defun get-peeled-group () (string-trim-blanks (ungroup (get-group))))

(defun get-token-or-peeled-group () (string-trim-blanks (ungroup (get-token))))

(defun get-grouped-environment-name-if-any ()
  (let ((c (snoop-actual-char)))
    (if (or (not c) (not (char= c #\{))) nil
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

;(trace get-grouped-environment-name-if-any)

(defun get-bracketed-text-if-any ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (or (not c) (not (char= c #\[))) nil
      (progn
       (get-actual-char)
       (concatenate
        'string
        (nreverse
         (let ((s '()) (nesting 0) (escape-p nil))
           (loop
             (let ((c (get-actual-char)))
               (when (not c)
                 (terror 'get-bracketed-text-if-any
                         "Runaway argument?"))
               (cond (escape-p
                      (push c s) (setq escape-p nil))
                     ((= (catcode c) **escape**)
                      (push c s) (setq escape-p t))
                     ((char= c #\{)
                      (push c s) (incf nesting))
                     ((char= c #\})
                      (push c s) (decf nesting))
                     ((char= c #\])
                      (if (= nesting 0) (return s)
                          (push c s)))
                     (t (push c s))))))))))))

(defun ungroup (s)
  (let* ((n (length s))
         (n-1 (1- n)))
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
        (loop
          (setq c (snoop-actual-char))
          (unless (or (alpha-char-p c)
                      (digit-char-p c)
                      (member c '(#\:) :test #'char=)) (return))
          (get-actual-char)))))

(defun get-filename (&optional bracedp)
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
            (cond ((not c) (return s))
                  ((and (not bracedp)
                        (or (char-whitespace-p c)
                            (= (catcode c) **comment**)
                            (member c *filename-delims* :test #'char=)))
                   (ignorespaces :stop-before-first-newline)
                   (return s))
                  ((and bracedp (char= c #\}))
                   (get-actual-char) (return s))
                  ((= (catcode c) **escape**)
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

(defun get-filename-possibly-braced ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (get-filename (and (characterp c) (char= c #\{)))))

(defun get-word ()
  (ignorespaces)
  (concatenate 'string
    (nreverse (let ((s '()))
                (loop (let ((c (snoop-actual-char)))
                        (cond ((not c) (return s))
                              ((or (char-whitespace-p c)
                                   (= (catcode c) **comment**)
                                   (= (catcode c)) **escape**)
                               (return s))
                              (t (get-actual-char)
                                 (push c s)))))))))

(defun get-integer (base)
  (declare (type (member 8 10 16) base))
  (ignorespaces)
  (let ((s '()) c)
    (loop
      (setq c (snoop-actual-char))
      (when (not c) (return))
      (unless (or (digit-char-p c)
                  (and (= base 16) (alpha-char-p c)))
        (ignorespaces) (return))
      (get-actual-char)
      (push c s))
    (when (consp s)
      (string-to-number
        (concatenate 'string (nreverse s)) base))))

(defun get-real ()
  (ignorespaces)
  (let ((minusp nil) (c (snoop-actual-char)))
    (when (char= c #\-) (setq minusp t))
    (when (or minusp (char= c #\+)) (get-actual-char))
    (let ((s '()))
      (loop
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return))
                ((or (digit-char-p c)
                     (char= c #\.))
                 (get-actual-char)
                 (push c s))
                (t (ignorespaces)
                   (return)))))
      (if s
          (let ((n (read-from-string (concatenate 'string (nreverse s)))))
            (if minusp (- n) n))
          nil))))

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
    (cond ((char= (snoop-actual-char) #\o)
           (get-actual-char) (ignorespaces :stop-before-first-newline))
          (t (toss-back-char #\t)))))

(defun string-to-number (s &optional (base 10))
  (declare (string s)
           (type (member 8 10 16) base))
  (if (position #\: s :test #'char=) nil
    (let ((n (let ((*read-base* base)) (read-from-string s nil))))
      (if (numberp n) n nil))))

(defun get-number-corresp-to-ctl-seq (x)
  (declare (string x))
  (cond ((string= x "\\the") (get-number-corresp-to-ctl-seq (get-ctl-seq)))
        ((string= x "\\active") 13)
        ;((string= x "\\pageno") *html-page-count*)
        ((string= x "\\inputlineno") *input-line-no*)
        ((string= x "\\footnotenumber") (get-gcount "\\footnotenumber"))
        ((string= x "\\figurenumber")
         (counter*-value (gethash "figure" *dotted-counters*)))
        ((string= x "\\sectiondnumber")
         (gethash (read-from-string (ungroup (get-token)))
                  *section-counters*
                  0))
        ((string= x "\\magstep") (get-number-or-false))
        ((find-chardef x))
        ((setq *it* (find-countdef x)) (find-count *it*))
        ((setq *it* (find-dimendef x)) (find-dimen *it*))
        ((find-count x))
        ((find-dimen x))
        ;not sure about following
        ((setq *it* (resolve-defs x)) (char-code (char *it* 0)))
        ((= (length x) 2) (char-code (char x 1)))
        (t (string-to-number x))))

(defun get-number-or-false ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((= (catcode c) **escape**) (get-number-corresp-to-ctl-seq (get-ctl-seq)))
          ((char= c #\') (get-actual-char) (get-integer 8))
          ((char= c #\") (get-actual-char) (get-integer 16))
          ((char= c #\`) (get-actual-char) (ignorespaces)
           (char-code
            (if (= (catcode (snoop-actual-char)) **escape**) (char (get-ctl-seq) 1)
                (get-actual-char))))
          ((char= c #\+) (get-actual-char) (get-number-or-false))
          ((char= c #\-) (get-actual-char)
           (let ((n (get-number-or-false)))
             (and n (- n))))
          ((digit-char-p c) (get-integer 10))
          (t nil))))

;(trace get-number-or-false get-number-corresp-to-ctl-seq)

(defun get-number ()
  (or (get-number-or-false) (terror 'get-number "Missing number.")))

;(trace get-integer)

(defun get-tex-char-spec ()
  (let ((n (get-number-or-false)))
    (if n (code-char n)
      (terror 'get-tex-char-spec "not a char"))))

(defun get-token-as-tex-char-spec ()
  (let* ((x (get-token))
         (c0 (char x 0)))
    (when (and (= (catcode c0) **escape**) (> (length x) 1))
      (setq c0 (char x 1)))
    c0))

(defun get-url ()
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (not c) (not (char= c #\{)))
      (terror 'get-url "Missing {"))
    (string-trim-blanks
     (concatenate 'string
       (nreverse
        (let ((nesting 0) (s '()))
          (loop
            (let ((c (get-actual-char)))
              (when (not c) (terror 'get-url "Missing }"))
              (cond ((= (catcode c) **comment**)
                     (let ((c1 (snoop-actual-char)))
                       (if (and (characterp c1)
                                (char-whitespace-p c1))
                           (ignorespaces)
                         (push c s))))
                    ((char= c #\{)
                     (incf nesting) (push c s))
                    ((char= c #\})
                     (cond ((= nesting 0) (return s))
                           (t (decf nesting) (push c s))))
                    (t (push c s)))))))))))

(defun get-csv (closing-delim)
  ;csv = comma-separated value
  (declare (character closing-delim))
  (ignorespaces)
  (let ((rev-lbl
          (let ((s '()))
            (loop
              (let ((c (get-actual-char)))
                (when (not c)
                  (terror 'get-csv "Runaway argument of \\cite, "
                          "\\nocite, \\expandhtmlindex?"))
                (cond ((char= c #\,)
                       (return s))
                      ((char= c closing-delim)
                       (toss-back-char c) (return s))
                      (t (push c s))))))))
    (when (not (null rev-lbl))
      (concatenate 'string (nreverse rev-lbl)))))

;functions for reading TeX tokens.  Token isn't really the right name.
;It's more like a TeX sexpr (texpr?), i.e. something that is treated as
;a single item by a macro looking for an arg

(defun get-raw-token ()
  (let ((c (snoop-actual-char)))
    (cond ((not c) c)
          ((= (catcode c) **escape**)
           (get-ctl-seq))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun get-raw-token/is ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((not c) c)
          ((= (catcode c) **escape**) (get-ctl-seq))
          ((= (catcode c) **comment**) (eat-till-eol)
           (get-raw-token/is))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun get-token ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((not c) c)
          ((= (catcode c) **escape**) (get-ctl-seq))
          ((char= c #\{) (get-group))
          ((= (catcode c) **comment**) (eat-till-eol)
           (get-token))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun get-token/ps ()
  ; preserve space
  (let ((c (snoop-actual-char)))
    (cond ((not c) c)
          ((= (catcode c) **escape**) (get-ctl-seq))
          ((char= c #\{) (get-group))
          ((= (catcode c) **comment**) (eat-till-eol) (get-token/ps))
          (t (concatenate 'string (list (get-actual-char)))))))

(defun eat-word (word)
  (declare (string word))
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
              (cond ((not c) (return))
                    ((and (= (catcode c) **escape**) firstp)
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
      (let ((s '()) (esc-p nil) c)
        (loop
          (setq c (snoop-actual-char))
          (when (not c) (return s))
          (cond (esc-p (get-actual-char)
                       (push c s) (setq esc-p nil))
                ((char= c #\\) (get-actual-char)
                               (push c s) (setq esc-p t))
                ((or (char-whitespace-p c)
                     (member c *scm-token-delims* :test #'char=))
                 (return s))
                (t (get-actual-char)
                   (push c s))))))))

(defun make-html-output-stream ()
  (make-ostream* :stream (make-string-output-stream)))

(defun close-html-output-stream (op)
  (mapc #'emit (nreverse (ostream*-hbuffer op)))
  (setf (ostream*-hbuffer op) '())
  (close (ostream*-stream op)))

(defun html-output-stream-to-string (op)
  (mapc #'emit (nreverse (ostream*-hbuffer op)))
  (setf (ostream*-hbuffer op) '())
  (get-output-stream-string (ostream*-stream op)))

(defun emit (s)
  (let ((p (ostream*-stream *html*)))
    (mapc (lambda (x) (princ x p))
          (nreverse (ostream*-hbuffer *html*)))
    (setf (ostream*-hbuffer *html*) '())
    (princ s p)))

(defun emit-space (s)
  (push s (ostream*-hbuffer *html*)))

(defun emit-newline ()
  (push #\newline (ostream*-hbuffer *html*)))

(defun emit-html-char (c)
  (unless (not c)
    (cond ((char= c #\newline) (emit-newline))
          (*outputting-to-non-html-p* (emit c))
          ((char-whitespace-p c) (emit-space c))
          (t (emit (case c
                     ((#\<) "&#x3c;")
                     ((#\>) "&#x3e;")
                     ((#\") "&#x22;")
                     ((#\&) "&#x26;")
                     (t c)))))))

(defun emit-html-string (s)
  (declare (string s))
  (dotimes (i (length s))
    (emit-html-char (char s i))))

(defun do-unskip ()
  (setf (ostream*-hbuffer *html*) '()))

(defun catcode (c &optional n globalp)
  (declare (character c) (ignore globalp))
  (cond ((not n)
         (cond ((setq *it* (assoc c *catcodes*)) (cdr *it*))
               ((alpha-char-p c) 11)
               (t 12)))
        (t ;ignore globalp for now
          (push (cons c n) *catcodes*)
          (funcall (if (= n 13) #'activate-cdef #'deactivate-cdef) c))))

(defun curr-esc-char ()
  (the fixnum (car (rassoc 0 *catcodes*))))

;;

(defun kern (len)
  (concatenate 'string "<span style=\"margin-left: "
    ;in following, tried &#x200c; (= zwnj) instead of space,
    ;but it causes table-row fault
    len "\"> </span>"))

(defun do-kern ()
  (let ((n (get-pixels)))
    (emit-space "<span style=\"margin-left: ")
    (emit-space n)
    ;span needs to contain something that can be moved: use zwnj?
    (emit-space "px\"> </span>")))

(defun read-box ()
  (ignorespaces)
  (let ((c (snoop-actual-char)) s)
    (when (= (catcode c) **escape**)
      (let ((cs (get-ctl-seq)))
        (cond ((member cs '("\\hbox" "\\vbox" "\\vtop") :test #'string=)
               (let* ((box-caller (get-till-char #\{))
                      (box-content (get-group)))
                 (setq s (concatenate 'string cs box-caller box-content))))
              ((string= cs "\\box")
               (setq s (read-box-string (get-number))))
              ((string= cs "\\copy")
               (setq s (read-box-string (get-number) t))))))
    (or s
        (terror 'read-box "A <box> was supposed to be here."))))

;(trace read-box)

(defun do-lower (&optional raisep)
  (let* ((n (get-pixels)) (box (read-box)))
    (emit "<span style=\"position: relative; top: ")
    (when raisep (emit "-"))
    (emit n)
    (emit "px\">")
    (bgroup)
    (add-postlude-to-top-frame
      (lambda ()
        (emit "</span>")))
    (toss-back-char #\})
    (toss-back-string box)
    nil))

;;

(defvar *primitive-texframe* (make-texframe*))

(defvar *math-primitive-texframe* (make-texframe*))

(defun bgroup ()
  (push (make-texframe* :catcodes *catcodes*) *tex-env*)
  (when (and *in-display-math-p* (not *math-script-mode-p*))
    (bgroup-math-hook)))

(defun egroup ()
  (when (null *tex-env*) (terror 'egroup "Too many }'s"))
  ;pop tex-env here?
  (perform-postludes)
  (perform-aftergroups)
  (pop *tex-env*)
  (cond ((setq *it* (top-texframe))
         (setq *catcodes* (texframe*-catcodes *it*)))
        (t (terror 'egroup "This can't happen.")))
  )

(defun bgroup-math-hook ()
  (let ((old-html *html*)
        (old-math-delim-left *math-delim-left*)
        (old-math-delim-right *math-delim-right*)
        (old-math-height *math-height*))
    (setq *html* (make-html-output-stream)
          *math-delim-left* nil
          *math-delim-right* nil
          *math-height* 0)
    (push :mathbox *tabular-stack*)
    (add-postlude-to-top-frame
     (lambda ()
       (let ((res (html-output-stream-to-string *html*)))
         (setq res
               (concatenate 'string
                 "<table><tr><td class=centerline>"
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
            ((string= s ".") (setq *math-delim-left* :nulldelim))
            (t (terror 'do-math-left))))))

(defun do-math-right ()
  (ignorespaces)
  (when (and *in-display-math-p* (not *math-script-mode-p*))
    (let ((s (get-token)))
      (cond ((string= s ")") (setq *math-delim-right* :rparen))
            ((string= s "]") (setq *math-delim-right* :rbrack))
            ((string= s "\\}") (setq *math-delim-right* :rbrace))
            ((string= s "|") (setq *math-delim-right* :rvert))
            ((string= s ".") (setq *math-delim-right* :nulldelim))
            (t (terror 'do-math-right)))
      (egroup))))

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

(defun kopy-tdef (lft rt)
  (declare (tdef* lft rt))
  (setf (tdef*-argpat lft) (tdef*-argpat rt))
  (setf (tdef*-expansion lft) (tdef*-expansion rt))
  (setf (tdef*-optarg lft) (tdef*-optarg rt))
  (setf (tdef*-thunk lft) (tdef*-thunk rt))
  (setf (tdef*-prim lft) (tdef*-prim rt))
  (setf (tdef*-defer lft) (tdef*-defer rt))
  (setf (tdef*-catcodes lft) (tdef*-catcodes rt))
  nil)

(defun kopy-cdef (lft rt)
  (declare (cdef* lft rt))
  (setf (cdef*-argpat lft) (cdef*-argpat rt))
  (setf (cdef*-expansion lft) (cdef*-expansion rt))
  (setf (cdef*-optarg lft) (cdef*-optarg rt))
  (setf (cdef*-active lft) (cdef*-active rt))
  (setf (cdef*-catcodes lft) (cdef*-catcodes rt))
  nil
  )

(defun cleanse-tdef (d)
  (declare (tdef* d))
  (setf (tdef*-argpat d) '())
  (setf (tdef*-expansion d) "")
  (setf (tdef*-optarg d) nil)
  (setf (tdef*-thunk d) nil)
  (setf (tdef*-prim d) nil)
  (setf (tdef*-defer d) nil)
  (setf (tdef*-catcodes d) nil)
  )

(defun ensure-def (name frame)
  (let ((frame-defs (texframe*-definitions frame)))
    (or (gethash name frame-defs)
        (let ((d (make-tdef*)))
          (setf (gethash name frame-defs) d)
          d))))

(defun tex-def (name argpat expansion optarg thunk prim defer frame)
  (unless frame (setq frame (top-texframe)))
  (let ((d (ensure-def name frame)))
    (setf (tdef*-argpat d) argpat
          (tdef*-expansion d) expansion
          (tdef*-optarg d) optarg
          (tdef*-thunk d) thunk
          (tdef*-prim d) prim
          (tdef*-defer d) defer
          (tdef*-catcodes d) *catcodes*))
  (perform-afterassignment))

;(trace tex-def)

(defun tex-def-prim (prim thunk)
  (declare (string prim))
  (tex-def prim '() nil nil thunk prim nil *primitive-texframe*))

(defun tex-def-pat-prim (prim argstr rhs)
  (declare (string prim argstr rhs))
  (tex-def prim (concatenate 'list argstr) rhs nil nil nil nil *primitive-texframe*))

(defun tex-defsym-prim (prim str)
  (declare (string prim str))
  (tex-def prim '() nil nil (lambda () (emit str)) prim nil *primitive-texframe*))

(defun tex-def-0arg (cs expn)
  (declare (string cs expn))
  (tex-def cs '() expn nil nil nil nil nil))

(defun ctl-seq-no-arg-expand-once (cs)
  (declare (string cs))
  (let ((d (find-def cs)))
    (and d (tdef*-expansion d))))

(defun tex-gdef-0arg (cs expn)
  (declare (string cs expn))
  (tex-def cs '() expn nil nil cs nil *global-texframe*))

(defun tex-def-prim-0arg (cs expn)
  (declare (string cs expn))
  (tex-def cs '() expn nil nil cs nil *primitive-texframe*))

(defun get-0arg-expn (cs)
  (declare (string cs))
  (let ((d (find-def cs)))
    (if d (tdef*-expansion d) "0")))

(defun tex2page-flag-value (cs)
  (declare (string cs))
  (char (get-0arg-expn cs) 0))

(defun tex2page-flag-boolean (cs)
  (declare (string cs))
  (not (member (char (get-0arg-expn cs) 0) '(#\0 #\f #\F #\n #\N))))

;(trace tex2page-flag-boolean)

(defun tex-let (lft rt frame)
  (unless frame (setq frame (top-texframe)))
  (let* ((frame-defs (texframe*-definitions frame))
         (lft-def
           (or (gethash lft frame-defs)
               (let ((lft-def (make-tdef*)))
                 (setf (gethash lft frame-defs) lft-def)
                 lft-def))))
    (cond ((setq *it* (or (find-def rt) (find-math-def rt)))
           ;(format t "rhs= ~s~%" *it*)
           (let ((rt-def *it*))
             (kopy-tdef lft-def rt-def)))
          (t
            (cleanse-tdef lft-def)
             ;(setf (tdef*-defer lft-def) rt)
             ))))

(defun tex-let-general (lhs rhs frame)
  ;(format t "tex-let-general ~s ~s~%" lhs rhs)
  (if (ctl-seq-p rhs) (tex-let lhs rhs frame)
      (tex-def lhs '() rhs nil nil nil nil frame)))

(defun tex-let-prim (lft rt)
  (declare (string lft rt))
  (tex-let lft rt *primitive-texframe*)
  )

(defun tex-def-thunk (name thunk frame)
  (declare (string name) (function thunk))
  (unless (inside-false-world-p)
    (tex-def name '() nil nil thunk name nil frame)))

;;

;chardef really should be numdef but TeX prefers this name

(defun tex-def-chardef (ctlseq num &optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash ctlseq (texframe*-chardefs frame)) num))
  (when (and globalp (consp *tex-env*))
    (setf (gethash ctlseq (texframe*-chardefs *global-texframe*)) num)))

(defun do-chardef ()
  (let ((cltseq (get-raw-token/is)))
    (get-equal-sign)
    (let ((num (get-number-or-false)))
      (unless num (terror 'do-chardef))
      (tex-def-chardef cltseq num (globally-p)))))

(defun find-chardef (ctlseq)
  ;finds the num that \ctlseq is bound to
  (or (some (lambda (frame) (gethash ctlseq (texframe*-chardefs frame))) *tex-env*)
      (gethash ctlseq (texframe*-chardefs *global-texframe*))))

;(trace find-chardef)

;

(defun gen-regno (c &optional (avoid '()))
  ; return incremented \count10
  (let ((n (gethash c (texframe*-counts *global-texframe*))))
    (incf n)
    (when (member n avoid) (incf n))
    (setf (gethash c (texframe*-counts *global-texframe*)) n)
    n))

(defun tex-def-countdef (ctlseq &optional regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno
    (setq regno (or (find-countdef ctlseq) (gen-regno 10))))
  (let ((frame (top-texframe)))
    (setf (gethash ctlseq (texframe*-countdefs frame)) regno))
  (when (and globalp (consp *tex-env*))
    (setf (gethash ctlseq (texframe*-countdefs *global-texframe*)) regno))
  (when num
    (tex-def-count regno num globalp)))

(defun plain-count (ctlseq num &optional (globalp t))
  (unless globalp (setq globalp (globally-p)))
  (tex-def-countdef ctlseq nil num globalp))

;(trace tex-def-countdef)

(defun find-countdef (ctlseq)
  (or (some (lambda (frame) (gethash ctlseq (texframe*-countdefs frame))) *tex-env*)
      (gethash ctlseq (texframe*-countdefs *global-texframe*))))

;(trace find-countdef)

(defun do-count= (regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (get-equal-sign)
  (tex-def-count regno (get-number) globalp)
  (perform-afterassignment))

(defun tex-def-count (regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-counts frame)) num)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-counts *global-texframe*)) num))))

(defun do-countdef ()
  (let ((cltseq (get-raw-token/is)))
    (get-equal-sign)
    (let ((n (get-number-or-false)))
      (unless n (terror 'do-countdef "Missing number."))
      (tex-def-countdef cltseq n 0 (globally-p)))))

(defun do-newcount (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-countdef (get-ctl-seq) (gen-regno 10) 0 globalp))

(defun find-count (num)
  (or (some (lambda (frame) (gethash num (texframe*-counts frame))) *tex-env*)
      (gethash num (texframe*-counts *global-texframe*))
      (gethash num (texframe*-counts *primitive-texframe*))))

(defun the-count (dracula)
  (cond ((setq *it* (find-countdef dracula)) (find-count *it*))
        (t (terror 'the-count "Missing count register."))))

(defun get-gcount (ctlseq)
  (gethash (find-countdef ctlseq) (texframe*-counts *global-texframe*) 0))

(defun tex-gdef-count (ctlseq v)
  (tex-def-countdef ctlseq nil v t))

(defun do-count (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((regno (get-number)))
    (get-equal-sign)
    (tex-def-count regno (get-number) globalp)))

;(trace do-count)

;;

(defun tex-def-dimendef (ctlseq &optional regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno
    (setq regno (or (find-dimendef ctlseq) (gen-regno 11))))
  (let ((frame (top-texframe)))
    (setf (gethash ctlseq (texframe*-dimendefs frame)) regno))
  (when (and globalp (consp *tex-env*))
    (setf (gethash ctlseq (texframe*-dimendefs *global-texframe*)) regno))
  (when num
    (tex-def-dimen regno num globalp))
  regno)

(defun plain-dimen (ctlseq num &optional (globalp t))
  (unless globalp (setq globalp (globally-p)))
  (tex-def-dimendef ctlseq nil num globalp))

(defun find-dimendef (ctlseq)
  (or (some (lambda (frame) (gethash ctlseq (texframe*-dimendefs frame))) *tex-env*)
      (gethash ctlseq (texframe*-dimendefs *global-texframe*))))

(defun do-dimen= (regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (get-equal-sign)
  (tex-def-dimen regno (get-scaled-points) globalp)
  (perform-afterassignment))

(defun tex-def-dimen (regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-dimens frame)) num)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-dimens *global-texframe*)) num))))

(defun do-dimendef ()
  (let ((cltseq (get-raw-token/is)))
    (get-equal-sign)
    (let ((n (get-scaled-points)))
      (unless n (terror 'do-dimendef "Missing number."))
      (tex-def-dimendef cltseq n 0 (globally-p)))))

(defun do-newdimen (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-dimendef (get-ctl-seq) (gen-regno 11) 0 globalp))

(defun find-dimen (num)
  (or (some (lambda (frame) (gethash num (texframe*-dimens frame))) *tex-env*)
      (gethash num (texframe*-dimens *global-texframe*))
      (gethash num (texframe*-dimens *primitive-texframe*))))

(defun the-dimen (ctlseq)
  (cond ((setq *it* (find-dimendef ctlseq)) (find-dimen *it*))
        (t (terror 'the-dimen "Missing dimen register."))))

(defun get-dimen (ctlseq)
  (cond ((the-dimen ctlseq))
        (t ;let's just assume the default \hsize
           (tex-length 6.5 :in))))

(defun do-dimen (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((regno (get-number)))
    (get-equal-sign)
    (tex-def-dimen regno (get-scaled-points) globalp)))

;;

(defun do-advance (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((cs (get-ctl-seq)))
    (get-by)
    (cond ((setq *it* (find-countdef cs))
           (let* ((countnum *it*)
                  (countval (find-count countnum)))
             (unless countval (terror 'do-advance "Missing count register."))
             (tex-def-count countnum (+ countval (get-number)) globalp)))
          ((setq *it* (find-dimendef cs))
           (let* ((dimennum *it*)
                  (dimenval (find-dimen dimennum)))
             (unless dimenval (terror 'do-advance "Missing dimen register."))
             (tex-def-dimen dimennum (+ dimenval (get-scaled-points)) globalp)))
          (t (terror 'do-advance "Missing register.")))))

;(trace do-advance)

(defun do-multiply (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((cs (get-ctl-seq)))
    (get-by)
    (cond ((setq *it* (find-countdef cs))
           (let* ((countnum *it*)
                  (countval (find-count countnum)))
             (unless countval (terror 'do-multiply "Missing count register."))
             (tex-def-count countnum (* countval (get-number)) globalp)))
          ((setq *it* (find-dimendef cs))
           (let* ((dimennum *it*)
                  (dimenval (find-dimen dimennum)))
             (unless dimenval (terror 'do-multiply "Missing dimen register."))
             (tex-def-dimen dimennum (* dimenval (get-number)) globalp)))
          (t (terror 'do-multiply "Missing register.")))))

(defun do-divide (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((cs (get-ctl-seq)))
    (get-by)
    (cond ((setq *it* (find-countdef cs))
           (let* ((countnum *it*)
                  (countval (find-count countnum)))
             (unless countval (terror 'do-divide "Missing count register."))
             (tex-def-count countnum (floor countval (get-number)) globalp)))
          ((setq *it* (find-dimendef cs))
           (let* ((dimennum *it*)
                  (dimenval (find-dimen dimennum)))
             (unless dimenval (terror 'do-divide "Missing dimen register."))
             (tex-def-dimen dimennum (floor dimenval (get-number)) globalp)))
          (t (terror 'do-divide "Missing register.")))))

;;
(defun tex-def-newread (ctlseq &optional regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno (setq regno (gen-regno 16 '(16))))
  (tex-def-chardef ctlseq regno globalp)
  (tex-def-istream regno :free globalp))

(defun tex-def-newwrite (ctlseq &optional regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno (setq regno (gen-regno 17 '(16 18))))
  (tex-def-chardef ctlseq regno globalp)
  (tex-def-ostream regno :free globalp))

(defun tex-def-istream (regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-istreams frame)) num)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-istreams *global-texframe*)) num))))

(defun tex-def-ostream (regno num globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-ostreams frame)) num)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-ostreams *global-texframe*)) num))))

(defun find-istream (num)
  (or (some (lambda (frame) (gethash num (texframe*-istreams frame))) *tex-env*)
      (gethash num (texframe*-istreams *global-texframe*))
      (gethash num (texframe*-istreams *primitive-texframe*))))

(defun find-ostream (num)
  (or (some (lambda (frame) (gethash num (texframe*-ostreams frame))) *tex-env*)
      (gethash num (texframe*-ostreams *global-texframe*))
      (gethash num (texframe*-ostreams *primitive-texframe*))))

(defun do-newread (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-newread (get-ctl-seq) (gen-regno 16 '(16)) globalp))

(defun do-newwrite (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-newwrite  (get-ctl-seq) (gen-regno 17 '(16 18)) globalp))

;(trace do-newwrite tex-def-newwrite)

;;

(defun tex-def-newbox (ctlseq &optional regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno (setq regno (gen-regno 14)))
  (tex-def-chardef ctlseq regno globalp)
  (tex-def-box regno "" globalp))

(defun tex-def-box (regno bcontents &optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-boxes frame)) bcontents)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-ostreams *global-texframe*)) bcontents))))

(defun find-box (num)
  (or (some (lambda (frame) (gethash num (texframe*-boxes frame))) *tex-env*)
      (gethash num (texframe*-boxes *global-texframe*))
      (gethash num (texframe*-boxes *primitive-texframe*))))

(defun do-setbox ()
  (let* ((bno (get-number))
         (bcontents (progn (get-equal-sign) (read-box))))
    (tex-def-box bno bcontents (globally-p)))
  (perform-afterassignment))

(defun read-box-string (bno &optional retainp)
  (let ((b (find-box bno)))
    (unless b (terror 'read-box-string))
    (unless retainp (tex-def-box bno "" nil))
    b))

;(trace read-box-string)

(defun do-box (&optional retainp)
  (let* ((bno (get-number))
         (b (read-box-string bno retainp)))
    (toss-back-string b)))

(defun do-newbox (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-newbox (get-ctl-seq) (gen-regno 14) globalp))

;;

(defun tex-def-toksdef (ctlseq &optional regno str globalp)
  (unless globalp (setq globalp (globally-p)))
  (unless regno
    (setq regno (or (find-toksdef ctlseq) (gen-regno 15))))
  (let ((frame (top-texframe)))
    (setf (gethash ctlseq (texframe*-toksdefs frame)) regno))
  (when (and globalp (consp *tex-env*))
    (setf (gethash ctlseq (texframe*-toksdefs *global-texframe*)) regno))
  (when str
    (tex-def-toks regno str globalp)))

(defun find-toksdef (ctlseq)
  (or (some (lambda (frame) (gethash ctlseq (texframe*-toksdefs frame))) *tex-env*)
      (gethash ctlseq (texframe*-toksdefs *global-texframe*))))

(defun do-toks= (regno globalp)
  (unless globalp (setq globalp (globally-p)))
  (get-equal-sign)
  (tex-def-toks regno (get-group) globalp)
  (perform-afterassignment))

(defun do-toksdef ()
  (let ((ctlseq (get-ctl-seq)))
    (get-equal-sign)
    (tex-def-toksdef ctlseq nil (get-group) (globally-p))))

;(trace do-toks=)

;toks is a single item (one string of tokens); hence the awkward plural tokses

(defun tex-def-toks (regno str globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((frame (top-texframe)))
    (setf (gethash regno (texframe*-tokses frame)) str)
    (when (and globalp (consp *tex-env*))
      (setf (gethash regno (texframe*-tokses *global-texframe*)) str))))

(defun do-newtoks (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (tex-def-toksdef (get-ctl-seq) (gen-regno 15) "" globalp))

(defun find-toks (num)
  (or (some (lambda (frame) (gethash num (texframe*-tokses frame))) *tex-env*)
      (gethash num (texframe*-tokses *global-texframe*))
      (gethash num (texframe*-tokses *primitive-texframe*))))

(defun the-toks (ctlseq)
  (cond ((setq *it* (find-toksdef ctlseq)) (find-toks *it*))
        (t (terror 'the-toks "Missing toks register."))))

;(trace the-toks)

(defun plain-toks (ctlseq str &optional (globalp t))
  (unless globalp (setq globalp (globally-p)))
  (tex-def-toksdef ctlseq nil str globalp))

(defun do-toks (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let ((regno (get-number)))
    (get-equal-sign)
    (tex-def-toks regno (get-group) globalp)))
;;

;

;

(defun ensure-cdef (c f)
  (let ((f-cdefs (texframe*-cdefinitions f)))
    (or (gethash c f-cdefs)
        (let ((d (make-cdef*)))
          (setf (gethash c f-cdefs) d)
          d))))

(defun tex-def-char (c argpat expansion frame)
  (unless frame (setq frame (top-texframe)))
  (let ((d (ensure-cdef c frame)))
    (setf (cdef*-argpat d) argpat
          (cdef*-expansion d) expansion
          (cdef*-catcodes d) *catcodes*))
  (perform-afterassignment))

(defun find-cdef (c)
  (let ((x (or (some (lambda (f) (gethash c (texframe*-cdefinitions f)))
                     *tex-env*)
               (gethash c  (texframe*-cdefinitions *global-texframe*))
               (gethash c (texframe*-cdefinitions *primitive-texframe*)))))
    (and x (cdef*-active x) x)))

(defun find-cdef-in-top-frame (c)
  (let ((x (if (null *tex-env*)
               (or (gethash c (texframe*-cdefinitions *global-texframe*))
                   (gethash c  (texframe*-cdefinitions *primitive-texframe*)))
             (gethash c  (texframe*-cdefinitions (car *tex-env*))))))
    (and x (cdef*-active x) x)))

(defun do-defcsactive (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let* ((cs (get-token))
         (c (char cs (if (ctl-seq-p cs) 1 0)))
         (argpat (progn (ignorespaces) (get-def-parameters)))
         (rhs (ungroup (get-group)))
         (f (and globalp *global-texframe*)))
    (catcode c 13)
    (tex-def-char c argpat rhs f)))

(defun activate-cdef (c)
  (let ((y (cond ((setq *it* (find-cdef-in-top-frame c))
                  (let ((y *it*))
                    (setf (cdef*-active y) t)
                    y))
                 (t (let* ((d (find-cdef c))
                           (y (ensure-cdef c (top-texframe))))
                      (when d (kopy-cdef y d))
                      (setf (cdef*-active y) t)
                      y)))))
    (add-postlude-to-top-frame
      (lambda ()
        (setf (cdef*-active y) nil)))))

(defun deactivate-cdef (c)
  (declare (character c))
  (cond ((setq *it* (find-cdef-in-top-frame c))
         ;if c is active in current group, deactivate it
         (let ((y *it*))
           (setf (cdef*-active y) nil)))
        ((setq *it* (find-cdef c))
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
      (catcode c val))))

(defun do-opmac-activettchar ()
  (ignorespaces)
  (let ((c (get-token-as-tex-char-spec)))
    (setq *opmac-active-tt-char* c)
    (activate-cdef c)
    (tex-def-char c '() "\\TIIPopmacverb" nil)))

(defun do-opmac-intext-verb ()
  (bgroup)
  (let ((*ligatures-p* nil))
    (cond (*outputting-external-title-p* nil)
          (t (emit "<code class=verbatim>")))
    (do-verb-delimed *opmac-active-tt-char*)
    (cond (*outputting-external-title-p* nil)
          (t (emit "</code>"))))
  (egroup))

(defun do-global ()
  (ignorespaces)
  (let ((next (get-ctl-seq)))
    (cond ((string= next "\\def") (do-def t nil))
          ((string= next "\\edef") (do-def t t))
          ((string= next "\\let") (do-let t))
          ((string= next "\\newbox") (do-newbox t))
          ((string= next "\\newcount") (do-newcount t))
          ((string= next "\\count") (do-count t))
          ((string= next "\\newtoks") (do-newtoks t))
          ((string= next "\\toks") (do-toks t))
          ((string= next "\\newdimen") (do-newdimen t))
          ((string= next "\\dimen") (do-dimen t))
          ((string= next "\\advance") (do-advance t))
          ((string= next "\\multiply") (do-multiply t))
          ((string= next "\\divide") (do-divide t))
          ((string= next "\\read") (do-read t))
          ((or (string= next "\\imgdef")
               (string= next "\\gifdef")) (make-reusable-img t))
          ((find-count next) (do-count= next t))
          ((find-dimen next) (do-dimen= next t))
          ((find-toks next) (do-toks= next t))
          (t (toss-back-string next)))))

(defun do-externaltitle ()
  (write-aux `(!preferred-title ,(tex-string-to-html-string (get-group)))))

(defun make-external-title (title)
  (declare (string title))
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
  (declare (string title))
  (emit "<h1 class=title>")
  (bgroup)
  (tex2page-string (concatenate 'string "\\let\\\\\\break " title))
  (egroup)
  (emit "</h1>")
  (do-noindent)
  )

(defun do-subject ()
  (tex-gdef-0arg "\\TIIPtitleused" "1")
  (do-end-para)
  (ignorespaces)
  (let ((title
          (let ((c (snoop-actual-char)))
            (if (or (not c) (not (char= c #\{)))
                (get-till-par)
                (get-group)))))
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
         (emit (svref *month-names* (1- m)))
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
  (cond ((and *in-para-p* (consp (ostream*-hbuffer *html*)))
         ;(format t "erasing empty par space~%")
         (setf (ostream*-hbuffer *html*) '()))
        (t
          (do-end-para)
          (let ((in-table-p (and (consp *tabular-stack*)
                                 (eq (car *tabular-stack*) ':block))))
            (when in-table-p (emit "</td></tr><tr><td>") (emit-newline))
            (emit "<p>")
            (emit-newline)
            (setq *in-para-p* t)))))

(defun do-noindent ()
  (do-end-para)
  (emit-newline)
  (emit "<p class=noindent>")
  (setq *in-para-p* t))

(defun do-indent ()
  (let ((parindent (sp-to-pixels (the-dimen "\\parindent"))))
    (emit "<span style=\"margin-left: ")
    (emit parindent)
    (emit "pt\"></span>")))

(defun do-para-nopadding ()
  (do-end-para)
  (emit-newline)
  (emit "<p class=nopadding>")
  (emit-newline)
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
  (emit "<div class=centerline>")
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
        (cond ((= (catcode c) **escape**)
               (let ((x (get-ctl-seq)))
                 (cond ((string= x "\\endcsname")
                        (toss-back-char #\})
                        (mapc #'toss-back-string r)
                        (toss-back-char c)
                        (toss-back-char #\{)
                        (toss-back-string "TIIPcsname")
                        (toss-back-char c)
                        (return))
                       (t (push (expand-ctl-seq-into-string x) r)))))
              (t (get-actual-char)
                 (push (concatenate 'string (list c)) r)))))))

(defun do-saved-csname ()
  (let ((x (get-peeled-group)))
    (do-tex-ctl-seq x)))

(defun do-cssblock ()
  (let ((*dumping-nontex-p* t))
    (dump-till-end-env "cssblock" *css-stream*)))

(defun link-stylesheets ()
  (let ((link-it (lambda (css)
                   (emit "<link rel=\"stylesheet\" href=\"")
                   (emit css)
                   (emit "\" />")
                   (emit-newline))))
    (mapc link-it *stylesheets*)
    (when (or (tex2page-flag-boolean "\\TIIPsinglepage")
              (tex2page-flag-boolean "\\TZPsinglepage"))
      (emit "<style>") (emit-newline)
      (emit *basic-style*)
      (emit "</style>") (emit-newline))
    (funcall link-it (concatenate 'string *jobname* *css-file-suffix*))))

(defun link-scripts ()
  (let ((link-it (lambda (jsf)
                   (emit "<script src=\"")
                   (emit jsf)
                   (emit "\"></script>")
                   (emit-newline))))
    (mapc link-it *scripts*)))

(defun increment-section-counter (seclvl nonum-p)
  (declare (fixnum seclvl))
  (unless nonum-p
    ;increment the counter for seclvl.  If counter not set, init it to 1
    (unless (gethash seclvl *section-counters*)
      (setf (gethash seclvl *section-counters*) 0))
    (incf (gethash seclvl *section-counters*)))
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
   (gethash seclvl *section-counter-dependencies* '())))

(defun number-to-roman (n &optional upcase-p)
  (declare (fixnum n))
  (format nil (if upcase-p "~@r" "~(~@r~)") n))

(defun section-counter-value (seclvl)
  (declare (fixnum seclvl))
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
  (declare (string s))
  (cond ((string= s "\\sectiond") (string-to-number (ungroup (get-token))))
        ((string= s "\\part") -1)
        ((string= s "\\chapter") 0)
        ;optional [...] after section commands?
        (t (let ((n (length s)))
             (cond ((< n 8) nil)
                   ((and (>= n 10) (string= (subseq s (- n 9)) "paragraph"))
                    (let ((n-9 (- n 9)) (i 1) (i+3 4) (k 4))
                      (loop
                        (cond ((> i+3 n-9) (return k))
                              ((string= (subseq s i i+3) "sub")
                               (setq i i+3 i+3 (+ i+3 3) k (1+ k)))
                              (t (return nil))))))
                   ((string= (subseq s (- n 7)) "section")
                    (let ((n-7 (- n 7)) (i 1) (i+3 4) (k 1))
                      (loop
                        (cond ((> i+3 n-7) (return k))
                              ((string= (subseq s i i+3) "sub")
                               (setq i i+3 i+3 (+ i+3 3) k (1+ k)))
                              (t (return nil))))))
                   (t nil))))))

(defun do-heading (seclvl)
  (declare (fixnum seclvl))
  (let* ((starred-p
          (cond ((char= (snoop-actual-char) #\*) (get-actual-char) t) (t nil)))
         (too-deep-p
          (let ((secnumdepth (get-gcount "\\secnumdepth")))
            (cond ((< secnumdepth -1) nil) ((> seclvl secnumdepth) t)
                  (t nil))))
         (nonum-p (or starred-p too-deep-p))
         (header
          (let ((*tabular-stack* (list :header)))
            (get-bracketed-text-if-any) ;FIXME
            (tex-string-to-html-string (get-group)))))
    (do-heading-help seclvl starred-p nonum-p nil nil header)))

(defun do-heading-help (seclvl starred-p nonum-p notoc-p lbl-val header)
  (declare (fixnum seclvl) (string header))
  (write-aux `(!default-title ,header))
  (when (= seclvl 0)
    (!using-chapters) ;(write-aux '(!using-chapters))
    (when (and (eq *tex-format* :latex) (< (get-gcount "\\secnumdepth") -1))
      (tex-gdef-count "\\secnumdepth" 2))
    (cond ((or (> *html-page-count* 0) (tex2page-flag-boolean "\\TIIPtitleused"))
           (do-eject))
          (t (tex-gdef-0arg "\\TIIPtitleused" "1")
             (do-para))))
  (when (and (= seclvl 1)
             (or (tex2page-flag-boolean "\\TIIPslides")
                 (tex2page-flag-boolean "\\TZPslides")))
    (do-eject))
  (increment-section-counter seclvl nonum-p)
  (when lbl-val (setq nonum-p nil))
  (unless lbl-val
    (setq lbl-val
          (if nonum-p "IGNORE" (section-counter-value seclvl))))
  (let* ((htmlnum (max 1 (min 6 (if *using-chapters-p* (1+ seclvl) seclvl))))
         (lbl
          (concatenate 'string *html-node-prefix*
            (case seclvl ((-1) "part") ((0) "chap") (t "sec")) "_"
            (if nonum-p (gen-temp-string) lbl-val))))
    (unless nil
      (tex-def-0arg "\\TIIPcurrentnodename" lbl)
      (tex-def-0arg "\\@currentlabel" lbl-val))
    (do-end-para)
    (emit-anchor lbl)
    (emit-newline)
    (ignorespaces :all)
    (emit "<h")
    (emit htmlnum)
    (case seclvl
      ((-1) (emit " class=part class=centerline"))
      ((0) (emit " class=chapter"))
      (t (emit " class=section")))
    (emit ">")
    (let ((write-to-toc-p
           (and (not notoc-p) *toc-page*
                (not
                 (and (eql *tex-format* :latex)
                      (string= header "Contents"))))))
      (when (eql *tex-format* :latex)
        (case seclvl
          ((-1)
           (emit "<div class=partheading>")
           (if nonum-p (emit-nbsp 1)
               (progn
                 (when write-to-toc-p
                   (emit-page-node-link-start *toc-page*
                                              (concatenate 'string *html-node-prefix* "toc_" lbl)))
                 (tex2page-string "\\partname")
                 (emit " ")
                 (emit lbl-val)
                 (when write-to-toc-p (emit-link-stop))))
           (emit "</div><br>")
           (emit-newline))
          ((0)
           (emit-newline)
           (emit "<div class=chapterheading>")
           (if nonum-p (emit-nbsp 1)
               (progn
                 (when write-to-toc-p
                   (emit-page-node-link-start *toc-page*
                                              (concatenate 'string *html-node-prefix* "toc_" lbl)))
                 (tex2page-string
                   (if *inside-appendix-p* "\\appendixname" "\\chaptername"))
                 (emit " ")
                 (emit lbl-val)
                 (when write-to-toc-p (emit-link-stop))))
           (emit "</div><br>")
           (emit-newline))))
      (when write-to-toc-p
        (emit-page-node-link-start *toc-page*
                                   (concatenate 'string *html-node-prefix* "toc_" lbl)))
      (unless (or (and (eql *tex-format* :latex) (<= seclvl 0)) nonum-p)
        (emit lbl-val) (emit-nbsp 2))
      (emit header)
      (when write-to-toc-p (emit-link-stop))
      (emit "</h")
      (emit htmlnum)
      (emit ">")
      (do-noindent)
      ;(emit-newline)
      ;(do-para)
      (let ((tocdepth (get-gcount "\\tocdepth")))
        (when (and write-to-toc-p (not (and (eq *tex-format* :latex) starred-p))
                   (or (< tocdepth -1) (<= seclvl tocdepth)))
          (write-aux
            `(!toc-entry
               ,(if (= seclvl -1) -1
                    (if *using-chapters-p* seclvl (1- seclvl)))
               ,lbl-val ,*html-page-count* ,lbl ,header)))))
    (when *recent-node-name*
      (do-label-aux *recent-node-name*)
      (setq *recent-node-name* nil))))

(defun section-type-to-depth (sectype)
  (declare (string sectype))
  (cond ((string-to-number sectype))
        ((string= sectype "chapter") 0)
        ((string= sectype "section") 1)
        ((string= sectype "subsection") 2)
        ((string= sectype "subsubsection") 3)
        ((string= sectype "paragraph") 4)
        ((string= sectype "subparagraph") 5)
        (t 3)))

(defun do-write-to-toc-aux (seclvl secnum sectitle)
  (declare (fixnum seclvl) (string secnum sectitle))
  (let ((node-name
         (concatenate 'string *html-node-prefix* "sec_"
                      (if (string= secnum "") (gen-temp-string) secnum))))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (tex-def-0arg "\\@currentlabel" secnum)
    (emit-anchor node-name)
    (emit-newline)
    (write-aux
     `(!toc-entry ,seclvl ,secnum ,*html-page-count* ,node-name ,sectitle))))

(defun do-addcontentsline ()
  (let ((toc (get-peeled-group)))
    (unless (string= toc "toc")
      (terror 'do-addcontentsline "only #1=toc supported"))
    (let* ((seclvl (section-type-to-depth (get-peeled-group)))
           (sectitle (tex-string-to-html-string (get-group))))
      (write-aux
       `(!toc-entry
         ,(if (= seclvl -1) -1 (if *using-chapters-p* seclvl (1- seclvl)))
         ,(ctl-seq-no-arg-expand-once "\\@currentlabel") ,*html-page-count*
         ,(ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename") ,sectitle)))))

(defun do-documentclass ()
  (probably-latex)
  (get-bracketed-text-if-any)
  (let ((x (get-peeled-group)))
    (when (member x '("report" "book") :test #'string=)
      (!using-chapters)
      ;(write-aux '(!using-chapters))
      )))

(defun get-till-par ()
  (let ((r '()) (newline-p nil))
    (loop
      (let ((c (get-actual-char)))
        (cond ((or (not c)
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
  (ignorespaces)
  (let ((header (let ((*tabular-stack* (list :header)))
                  (tex-string-to-html-string (get-till-par)))))
    (do-heading-help 1 nil t t nil header)))

(defun do-beginchapter ()
  (ignorespaces)
  (let* ((chapno (tex-string-to-html-string
                   (get-till-char #\space)))
         (header (progn (ignorespaces)
                        (let ((*tabular-stack* (list :header)))
                          (tex-string-to-html-string (get-till-par))))))
    (tex-gdef-0arg "\\chapno" chapno)
    (tex-gdef-count "\\subsecno" 0)
    (tex-gdef-count "\\footnotenumber" 0)
    (when (string= chapno "") (setq chapno nil))
    (do-heading-help 0 nil t nil chapno header)))

(defun do-appendix ()
  (unless *inside-appendix-p*
    (setf *inside-appendix-p* t
          (gethash (if *using-chapters-p* 0 1) *section-counters*) 0)))

(defun do-table-plain () (do-end-para) (emit "<table width=100%><tr><td>"))

(defun do-end-table-plain () (do-end-para) (emit "</td></tr></table>"))

(defun pop-tabular-stack (type)
  (declare (keyword type))
  (let ((type-in-stack (pop *tabular-stack*)))
    (unless (eq type-in-stack type)
      (terror 'pop-tabular-stack "Bad environment closer: " type " " type-in-stack))))

;(trace pop-tabular-stack)

(defun do-end-table/figure (type)
  (declare (keyword type))
  (when (and (eql type :figure) (char= (snoop-actual-char) #\*))
    (get-actual-char))
  (do-end-para)
  (emit "</td></tr>")
  (emit "</table>")
  (emit "</div>")
  (pop-tabular-stack type)
  (egroup)
  (do-para))

(defun bump-dotted-counter (name)
  (declare (string name))
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
                            (write-to-string new-value))))
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
    (emit "<tr><td class=")
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

(defun do-marginnote ()
  (emit "<span class=marginnote>")
  (tex2page-string (get-group))
  (emit "</span>"))

(defun do-marginpar ()
  (get-bracketed-text-if-any)
  (emit "<table class=leftline border=2><tr><td>")
  (tex2page-string (get-group))
  (emit "</td></tr></table>"))

(defun do-minipage ()
  (get-bracketed-text-if-any)
  (get-group)
  (let ((in-table-p
         (and (not (null *tabular-stack*))
              (member (car *tabular-stack*) '(:block :figure :table)))))
    (if in-table-p (emit "</td><td>") (progn (do-para) (do-end-para)))
    (emit "<div class=leftline>")
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
  (declare (keyword type))
  (cond ((and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                  (tex2page-flag-boolean "\\TZPmathimage"))
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
             (emit "<div class=")
             (emit *display-justification*)
             (emit "><table width=100%>")
             (emit-newline)
             (emit "<tr><td align=")
             (emit (if (eql type :equation) "center" "right"))
             (emit ">")))))

(defun do-end-equation (type)
  (when (and (eql type :eqnarray) (eat-star)) (setq type :eqnarray*))
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
  (pop-tabular-stack type)
  (setq *math-mode-p* nil
        *in-display-math-p* nil)
  (egroup)
  (setq *equation-numbered-p* t
        *equation-position* 0)
  (do-para))

(defun do-integral ()
  (if (or (not *in-display-math-p*) *math-script-mode-p*)
      (emit "&#x222b;")
    (let ((affixes-already-read '()))
      (emit "<span style=\"font-size: 200%; position: relative; top: .25ex;\">&#x222b;</span>")
      (dotimes (i 2)
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (when (and (member c '(#\_ #\^))
                     (not (member c affixes-already-read)))
            (push c affixes-already-read)
            (get-actual-char)
            (when (= i 0)
              (emit-space (kern ".16667em")))
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
    (emit-anchor (concatenate 'string *html-node-prefix* "toc"))
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
                      (if (or (tex2page-flag-boolean "\\TIIPtexlayout")
                              (tex2page-flag-boolean "\\TZPtexlayout"))
                          (do-bigskip :medskip)
                          (do-para))
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

(defun do-numbered-footnote ()
  (do-footnote-aux nil))

(defun do-symfootnote ()
  (incf *footnote-sym*)
  (do-footnote-aux (number-to-footnote-symbol *footnote-sym*)))

(defun tex-string-to-html-string (s)
  (declare (string s))
  (let ((*html* (make-html-output-stream)))
    (tex2page-string s)
    (html-output-stream-to-string *html*)))

(defun expand-tex-string (s)
  (declare (string s))
  (let ((*html* (make-html-output-stream))
        (*outputting-to-non-html-p* t))
    (tex2page-string s)
    (html-output-stream-to-string *html*)))

(let ((symlist nil)
      (symlist-len 0))
  (defun number-to-footnote-symbol (n)
    (declare (fixnum n))
    (unless symlist
      (setq symlist
            '("*" "&#x2020;" "&#x2021;" "&#xa7;" "&#xb6;"
              "&#x2225;" ; ||
              "*" "&#x2020;&#x2020;" "&#x2021;&#x2021;")
            symlist-len (list-length symlist)))
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
      (setq fnno (1+ (get-gcount "\\footnotenumber")))
      (tex-gdef-count "\\footnotenumber" fnno)
      (setq fnmark (write-to-string fnno)))
    (emit-anchor fncalltag)
    (emit "<span class=footnotemark>")
    (when fnno (emit "<sup>"))
    (emit-page-node-link-start nil fntag)
    (emit fnmark)
    (emit-link-stop)
    (when fnno (emit "</sup>"))
    (emit "</span>")
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
  (let ((old-html *html*))
    (setq *html* (make-html-output-stream))
    (when fncalltag
      (tex-def-0arg "\\TIIPcurrentnodename" fntag)
      (tex-def-0arg "\\@currentlabel" fnmark))
    (add-aftergroup-to-top-frame
     (lambda ()
       (push (make-footnotev* :mark fnmark :text
                              (html-output-stream-to-string *html*)
                              :tag fntag :caller fncalltag)
             *footnote-list*)
       (setq *html* old-html)))))

(defun output-footnotes ()
  (let ((n (list-length *footnote-list*)))
    (unless (= n 0)
      (emit "<div class=footnoterule><hr></div>")
      (do-para)
      (do-end-para)
      (emit "<div class=footnote>")
      (let ((i (1- n)))
        (loop
          (when (< i 0) (return))
          (let* ((fv (elt *footnote-list* i))
                 (fnmark (footnotev*-mark fv))
                 (fnno (string-to-number fnmark))
                 (fncalltag (footnotev*-caller fv)))
            (do-para)
            (emit "<span class=footnotemark>")
            (when fncalltag
              (emit-anchor (footnotev*-tag fv))
              (when fnno (emit "<sup>"))
              (emit-page-node-link-start nil fncalltag))
            (emit fnmark)
            (when fncalltag
              (emit-link-stop)
              (when fnno (emit "</sup>"))
              )
            (emit "</span>")
            (emit " ")
            (emit (footnotev*-text fv))
            (do-end-para)
            (decf i))))
      (emit "</div>")
      (emit-newline))))

;color

(let ((f (lambda (x)
           (let* ((n  (round (* 1.0 x)))
                  (s (write-to-string n :base 16)))
             (if (< n 16) (concatenate 'string "0" s) s)))))
  (defun rgb-dec-to-rrggbb (r g b)
    (declare (number r g b))
    (concatenate 'string "#" (funcall f r) (funcall f g) (funcall f b))))

(defun rgb-frac-to-rrggbb (r g b)
  (declare (number r g b))
  (rgb-dec-to-rrggbb (* r 255) (* g 255) (* b 255)))

(let ((f (lambda (x k) (- 1 (min (max (+ x k) 0) 1)))))
  (defun cmyk-to-rrggbb (c m y k)
    (declare (number c m y k))
    (rgb-frac-to-rrggbb (funcall f c k) (funcall f m k) (funcall f y k))))

(defun hsl-in-html (h s L)
  (concatenate 'string "hsl("
    (write-to-string h) ","
    (write-to-string (* s 100)) "%,"
    (write-to-string (* L 100)) "%)"))

(defun hsb360-to-hsl (h s b)
  (let* ((L (* .5 b (- 2 s)))
         (s2 (/ (* b s) (- 1 (abs (1- (* 2 L)))))))
    (hsl-in-html h s2 L)))

(defun wavelength-to-hsl (w)
  (declare (number w))
  (let ((hue (* 1/6
                (cond ((<= w 362.857) 5)
                      ((< w 440) (+ 4 (/ (- w 440) -60)))
                      ((< w 490) (- 4 (/ (- w 440) 50)))
                      ((< w 510) (+ 2 (/ (- w 510) -20)))
                      ((< w 580) (- 2 (/ (- w 510) 70)))
                      ((< w 645) (/ (- w 645) -65))
                      (t 0))))
        (brightness
          (cond ((<= w 362.857) 0)
                ((< w 420) (+ 0.3 (* 0.7 (/ (- w 380) 40))))
                ((<= w 700) 1)
                ((< w 814.285) (+ 0.3 (* 0.7 (/ (- w 780) -80))))
                (t 0))))
    (hsb360-to-hsl (* 360 hue) 1 brightness)))

(defun read-color (model)
  (declare (keyword model))
  (case model
    (:cmy (bgroup)
          (with-input-from-string
            (i (tex-string-to-html-string
                 (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
            (egroup)
            (let* ((c (read i nil))
                   (m (read i nil))
                   (y (read i nil)))
              (ignorespaces)
              (rgb-frac-to-rrggbb (- 1 c) (- 1 m) (- 1 y)))))
    (:cmyk (bgroup)
          (with-input-from-string
            (i (tex-string-to-html-string
                 (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
            (egroup)
            (let* ((c (read i nil))
                   (m (read i nil))
                   (y (read i nil))
                   (k (read i nil)))
              (ignorespaces)
              (cmyk-to-rrggbb c m y k))))
    (:rgb (bgroup)
          (with-input-from-string
            (i (tex-string-to-html-string
                 (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
            (egroup)
            (let* ((r (read i nil))
                   (g (read i nil))
                   (b (read i nil)))
              (ignorespaces)
              (rgb-frac-to-rrggbb r g b))))
    (:rgb255
      (bgroup)
      (with-input-from-string
        (i (tex-string-to-html-string
             (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((r (read i nil))
               (g (read i nil))
               (b (read i nil)))
          (ignorespaces)
          (rgb-dec-to-rrggbb r g b))))
    (:gray
      (with-input-from-string (i (tex-string-to-html-string (get-token)))
        (let ((g (read i nil)))
          (ignorespaces)
          (hsb360-to-hsl 0 0 g))))
    (:gray15
      (with-input-from-string (i (tex-string-to-html-string (get-token)))
        (let ((g (read i nil)))
          (ignorespaces)
          (hsb360-to-hsl 0 0 (/ g 15)))))
    (:html
      (with-input-from-string (i (tex-string-to-html-string (get-token)))
        (let ((rrggbb (read-6hex i)))
          (ignorespaces)
          rrggbb)))
    (:hsb
      (bgroup)
      (with-input-from-string (i (tex-string-to-html-string
                                   (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((h (read i nil))
               (s (read i nil))
               (b (read i nil)))
          (ignorespaces)
          (hsb360-to-hsl (* 360 h) s b))))
    (:hsb360
      (bgroup)
      (with-input-from-string (i (tex-string-to-html-string
                                   (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((h (read i nil))
               (s (read i nil))
               (b (read i nil)))
          (ignorespaces)
          (hsb360-to-hsl h s b))))
    (:hsb240
      (bgroup)
      (with-input-from-string (i (tex-string-to-html-string
                                   (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((h (read i nil))
               (s (read i nil))
               (b (read i nil)))
          (ignorespaces)
          (hsb360-to-hsl (* h #.360/240) (/ s 240) (/ b 240)))))
    (:wave
      (with-input-from-string (i (tex-string-to-html-string (get-token)))
        (let ((w (read i nil)))
          (ignorespaces)
          (wavelength-to-hsl w))))
    (:hsl
      (bgroup)
      (with-input-from-string (i (tex-string-to-html-string
                                   (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((h (read i nil))
               (s (read i nil))
               (L (read i nil)))
          (ignorespaces)
          (hsl-in-html (* 360 h) s L))))
    (:hsl360
      (bgroup)
      (with-input-from-string (i (tex-string-to-html-string
                                   (concatenate 'string "\\defcsactive\\,{ }" (get-token))))
        (egroup)
        (let* ((h (read i nil))
               (s (read i nil))
               (L (read i nil)))
          (ignorespaces)
          (hsl-in-html h s L))))
    (t (let* ((name (get-peeled-group))
              (c (assoc name *color-names* :test #'string=)))
         (ignorespaces)
         (if c (cdr c) name)))))

(defun color-model-to-keyword (model)
  (cond ((not model) :colornamed)
        ((string= model "rgb") :rgb)
        ((string= model "RGB") :rgb255)
        ((string= model "cmyk") :cmyk)
        ((string= model "cmy") :cmy)
        ((string= model "gray") :gray)
        ((string= model "Gray") :gray15)
        ((string= model "HTML") :html)
        ((string= model "hsb") :hsb)
        ((string= model "Hsb") :hsb360)
        ((string= model "HSB") :hsb240)
        ((string= model "wave") :wave)
        ((string= model "hsl") :hsl)
        ((string= model "Hsl") :hsl360)
        (t :colornamed)))

(defun read-6hex (i)
  (declare (string-stream i))
  (format nil "#~6,'0x"
          (let ((*read-base* 16))
            (read i nil))))

(defun do-switch (sw)
  (declare (keyword sw))
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
          (emit "<span style=\"font-family: monospace\">")
          (lambda () (emit "</span>") (setq *ligatures-p* old-ligatures-p))))
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
       ((:cmy :cmyk :rgb :rgb255 :gray :gray15 :html :hsb :hsb360 :hsb240
              :wave :hsl :hsl360 :colornamed)
         (emit "<span style=\"color: ")
         (emit (read-color sw))
         (emit "\">")
         (lambda () (emit "</span>")))
       (:bgcolor
         (emit "<span style=\"background-color: ")
         (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
           (emit (read-color model)))
         (emit "\">")
         (lambda () (emit "</span>")))
       (:strike (emit "<strike>") (lambda () (emit "</strike>")))
       (:narrower
         (do-end-para)
         (emit "<blockquote>")
         (lambda ()
           (do-end-para)
           (emit "</blockquote>")
           (do-para)))
       (:raggedleft
        (do-end-para)
        (emit "<div class=rightline>")
        (lambda () (do-end-para) (emit "</div>") (do-para)))
       (:oldstyle
         (emit "<span class=oldstyle>")
         (lambda () (emit "</span>")))
       (:cal
         (when *math-mode-p*
           (let ((old-math-font *math-font*))
             (setq *math-font* :cal)
             (lambda () (setq *math-font* old-math-font)))))
       (t
        (emit "<span class=")
        (emit sw)
        (emit ">")
        (lambda () (emit "</span>")))))))

(defun do-obeylines ()
  (ignorespaces :stop-after-first-newline)
  ;(when (eql (snoop-actual-char) #\newline) (get-actual-char))
  ;(do-noindent)
  (activate-cdef #\newline)
  ;(tex-def-char #\newline '() "\\TIIPpar" nil)
  (tex-def-char #\newline '() "\\TIIPbr" nil)
  )

(defun do-obeyspaces ()
  (catcode #\space 13)
  (tex-def-char #\space '() "\\TIIPnbsp" nil))

(defun do-block (z)
  (declare (keyword z))
  (do-end-para)
  (emit "<div ")
  (emit (case z
          (:flushleft "class=leftline")
          (:flushright "class=rightline")
          (t "class=centerline")))
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
  (declare (string fn))
  (let ((*math-mode-p* *math-mode-p*))
    (cond (*outputting-external-title-p* nil)
          ((string= fn "\\emph") (emit "<em>"))
          ((string= fn "\\leftline") (do-end-para) (emit "<div class=leftline>"))
          ((string= fn "\\centerline") (do-end-para)
           (emit "<div class=centerline>&#xa0;"))
          ((string= fn "\\rightline") (do-end-para)
           (emit "<div class=rightline>&#xa0;"))
          ((string= fn "\\underline") (emit "<u>"))
          ((string= fn "\\textbf") (setq *math-mode-p* nil) (emit "<b>"))
          ((or (string= fn "\\textit") (string= fn "\\textsl"))
           (setq *math-mode-p* nil) (emit "<i>"))
          ((string= fn "\\textrm") (setq *math-mode-p* nil))
          ((string= fn "\\texttt")
           (setq *math-mode-p* nil)
           (emit "<span style=\"font-family: monospace\">"))
          (t (terror 'do-function "Unknown function " fn)))
    (bgroup)
    (tex2page-string (get-token))
    (egroup)
    (cond (*outputting-external-title-p* nil)
          ((string= fn "\\emph") (emit "</em>"))
          ((string= fn "\\rightline") (emit "</div>") (emit-newline))
          ((or (string= fn "\\leftline") (string= fn "\\centerline"))
           (do-end-para) (emit "&#xa0;</div>") (emit-newline))
          ((string= fn "\\underline") (emit "</u>"))
          ((string= fn "\\textbf") (emit "</b>"))
          ((or (string= fn "\\textsl") (string= fn "\\textit"))
           (emit "</i>"))
          ((string= fn "\\texttt") (emit "</span>")))))

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

(defun do-actual-space ()
  (emit "&#x200b;") (emit #\space) (emit "&#x200b;"))

(defun emit-nbsp (n)
  (declare (fixnum n))
  (dotimes (i n)
    (emit-space "&#xa0;")))

(defun scaled-point-equivalent-of (unit)
  ;TeXbook, chapter 10
  (declare (keyword unit))
  (case unit
    (:pt 65536)
    (:pc (* 12 (scaled-point-equivalent-of :pt)))
    (:in (* 72.27 (scaled-point-equivalent-of :pt)))
    (:bp (* (/ 72) (scaled-point-equivalent-of :in)))
    (:cm (* (/ 2.54) (scaled-point-equivalent-of :in)))
    (:mm (* 0.1 (scaled-point-equivalent-of :cm)))
    (:dd (* (/ 1238 1157) (scaled-point-equivalent-of :pt)))
    (:cc (* 12 (scaled-point-equivalent-of :dd)))
    (:sp 1)
    ;
    ;(:em (* 10 (scaled-point-equivalent-of :pt)))
    ;(:ex (* 4.5 (scaled-point-equivalent-of :pt)))
    ;
    (:em (* 16 (scaled-point-equivalent-of :pt)))
    (:ex (* .45 (scaled-point-equivalent-of :em)))
    ;
    (t (terror 'scaled-point-equivalent-of
               "Illegal unit of measure " unit))))

(defun tex-length (num unit)
  (declare (number num) (keyword unit))
  (* num (scaled-point-equivalent-of unit)))

(defun sp-to-ems (sp)
  (declare (number sp))
  (/ sp #.(* 65536 10.0)))

(defun sp-to-pixels (sp)
  (declare (number sp))
  (floor (/ sp 65536)))

;(trace sp-to-pixels)

(defun get-scaled-points ()
  (let ((n (or (get-real) 1)))
    (ignorespaces)
    (* n (if (= (catcode (snoop-actual-char)) **escape**)
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

(defun do-fontdimen ()
  (get-number)
  (get-ctl-seq)
  (get-equal-sign)
  (eat-dimen))

;(defun do-hskip ()
;  ;assume 1 space is 5 pxl wide
;  (emit-nbsp (/ (get-pixels) 5)))

(defun do-hskip ()
  (let ((n (get-pixels)))
    (emit-space "<span style=\"margin-left: ")
    (emit-space n)
    ;span needs to contain something that can be moved: use zwnj
    (emit-space "pt\">&#x200c;</span>")))

(defun do-vskip ()
  (let ((x (get-points)))
    (eat-skip-fluff nil)
    (emit "<div style=\"height: ")
    (emit x)
    (emit "pt\"></div>")
    (emit-newline)
    (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
    (emit-newline)
    (setq *in-para-p* t)))

(defun do-hrule ()
  (do-end-para) (emit "<hr>") (emit-newline) (do-para))

(defun do-newline ()
  ;(format t "doing do-newline~%")
  (when (>= (munch-newlines) 1) (do-para)) (emit-newline))

(defun do-br ()
  (if (or (find-cdef #\space) (not (= (the-count "\\TIIPobeylinestrictly") 0)))
      (emit "<br>") (unless (eql (snoop-actual-char) #\newline) (emit "<br>")))
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
         (emit (if (eq *math-font* :rm) "-" "&#x2212;")))
        ((not *ligatures-p*) (emit #\-))
        (t
         (let ((c (snoop-actual-char)))
           (if (and (characterp c) (char= c #\-))
               (progn (get-actual-char) (do-ndash)) (emit #\-))))))

(defun do-excl ()
  (if (or *math-mode-p* (not *ligatures-p*)) (emit #\!)
      (let ((c (snoop-actual-char)))
        (if (and (characterp c) (char= c #\`))
            (progn (get-actual-char) (emit "&#xa1;")) (emit #\!)))))

(defun do-quest ()
  (if (or *math-mode-p* (not *ligatures-p*)) (emit #\?)
      (let ((c (snoop-actual-char)))
        (if (and (characterp c) (char= c #\`))
            (progn (get-actual-char) (emit "&#xbf;")) (emit #\?)))))

(defun do-ndash ()
  (emit
   (let ((c (snoop-actual-char)))
     (if (and (characterp c) (char= c #\-))
         (progn (get-actual-char) "&#x2014;") "&#x2013;"))))

(defun do-lsquo ()
  (emit
   (if (not *ligatures-p*) #\`
       (let ((c (snoop-actual-char)))
         (if (and (characterp c) (char= c #\`))
             (progn (get-actual-char) "&#x201c;") "&#x2018;")))))

(defun do-rsquo ()
  (emit
   (cond (*math-mode-p*
          (let ((c (snoop-actual-char)))
            (if (and (characterp c) (char= c #\'))
                (progn (get-actual-char) "&#x2033;") "&#x2032;")))
         ((not *ligatures-p*) #\')
         (t
          (let ((c (snoop-actual-char)))
            (if (and (characterp c) (char= c #\'))
                (progn (get-actual-char) "&#x201d;") "&#x2019;"))))))

(defun do-enquote ()
  (ignorespaces)
  (when (and (char= (snoop-actual-char) #\*))
    (get-actual-char)
    (ignorespaces)
    (when (= *quote-level* 0) (incf *quote-level*)))
  (unless (char= (get-actual-char) #\{)
    (terror 'do-enquote "Missing {"))
  (bgroup)
  (incf *quote-level*)
  (emit (if (oddp *quote-level*) "&#x201c;" "&#x2018;"))
  (add-aftergroup-to-top-frame
    (lambda ()
      (emit (if (oddp *quote-level*) "&#x201d;" "&#x2019;"))
      (decf *quote-level*))))

;cross-references

(defun get-label ()
  (let* ((lbl (get-peeled-group))
         (i (or (position #\space lbl :test #'char=)
                (position #\tab lbl :test #'char=)
                (position #\newline lbl :test #'char=))))
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

(defun emit-anchor (lbl)
  (emit "<a id=\"") (emit lbl) (emit "\"></a>"))

(defun emit-link-start (link)
  (emit "<a href=\"") (emit link) (emit "\">"))

(defun emit-ext-page-node-link-start (extfile pageno node)
  (emit "<a ")
  (unless extfile
    (emit "class=hrefinternal "))
  (emit "href=\"")
  (unless (and (not extfile) (or (not pageno) (= *html-page-count* pageno)))
    (emit (or extfile *jobname*))
    (unless (= pageno 0) (emit *html-page-suffix*) (emit pageno))
    (emit *output-extension*))
  (when node (emit "#") (emit node))
  (emit "\">"))

(defun emit-page-node-link-start (pageno node)
  (emit-ext-page-node-link-start nil pageno node))

(defun emit-page-link-start (pageno)
  (emit-ext-page-node-link-start nil pageno nil))

(defun emit-link-stop () (emit "</a>"))

(defun do-anchor-for-potential-label ()
  (let ((node-name
         (concatenate 'string *html-node-prefix* "anchor_" (gen-temp-string))))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (emit-anchor node-name)))

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
         (ext-label-file (concatenate 'string fq-f *label-file-suffix*))
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
            *label-file-suffix*)))
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

(defun do-tag-aux (tag-name tag-val)
  (declare (string tag-name tag-val))
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
  (declare (keyword mp))
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
    (when (> (list-length *unresolved-xrefs*) 1) (write-log "s"))
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
    (when (member :scripts *missing-pieces*)
      (write-log "Scripts not determined")
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

(defun do-htmlpageref ()
  (let* ((label (get-peeled-group))
         (label-ref (label-bound-p label)))
    (emit "\"")
    (if label-ref
        (emit
         (maybe-label-page (label*-src label-ref) (label*-page label-ref)))
      (emit *log-file*))
    (emit "\"")))

(defun doc-internal-url (url)
  (declare (string url))
  (let ((n (length url)))
    (cond ((and (> n 0) (char= (char url 0) #\#))
           (let* ((label (subseq url 1))
                  (label-ref (label-bound-p label)))
             (if label-ref
                 (if (label*-src label-ref) nil
                     (list (label*-page label-ref) (label*-name label-ref)))
                 nil)))
          (t nil))))

(defun fully-qualify-url (url)
  (declare (string url))
  (let ((n (length url)))
    (cond ((and (> n 0) (char= (char url 0) #\#))
           (let* ((label (subseq url 1))
                  (label-ref (label-bound-p label)))
             (if label-ref
                 (concatenate 'string
                   (maybe-label-page (label*-src label-ref)
                                     (label*-page label-ref))
                   "#" (label*-name label-ref))
               url)))
          ((fully-qualified-url-p url) url)
          (t (ensure-url-reachable url) url))))

(defun do-url ()
  (let ((url (get-url)))
    (let ((durl (doc-internal-url url)))
      (if durl
          (emit-page-node-link-start (car durl) (cadr durl))
          (emit-link-start (fully-qualify-url url))))
    (emit url)
    (emit-link-stop)))

(defun do-mailto ()
  (let ((addr (get-url)))
    (emit-link-start (concatenate 'string "mailto:" addr))
    (emit addr)
    (emit-link-stop)))

(defun do-urlh ()
  (let* ((url (get-url))
         (durl (doc-internal-url url)))
    (if durl
        (emit-page-node-link-start (car durl) (cadr durl))
        (emit-link-start (fully-qualify-url url))))
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
    (let* ((url (get-url))
           (durl (doc-internal-url url)))
      (if durl
          (emit-page-node-link-start (car durl) (cadr durl))
          (emit-link-start (fully-qualify-url url))))
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

(defun do-img-src (f)
  (cond ((or (tex2page-flag-boolean "\\TIIPsinglepage")
             (tex2page-flag-boolean "\\TZPsinglepage"))
         (let ((tmpf (concatenate 'string *aux-dir/* *jobname* "-Z-Z.temp")))
           (system (concatenate 'string "echo -n data: > " tmpf))
           (system (concatenate 'string "file -bN --mime-type " f " >> " tmpf))
           (system (concatenate 'string "echo -n \\;base64, >> " tmpf))
           (system (concatenate 'string "base64 -w0 < " f " >> " tmpf))
           (with-open-file (i tmpf :direction :input)
             (loop
               (let ((c (read-char i nil)))
                 (if c (emit c) (return)))))
           (ensure-file-deleted tmpf)))
        (t (emit f))))

(defun do-htmladdimg ()
  (let* ((align-info (get-bracketed-text-if-any))
         (url (fully-qualify-url (get-url))))
    (emit "<img src=\"")
    (do-img-src url)
    (emit "\"")
    (emit " style=\"border: 0\"")
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
    (do-img-src (fully-qualify-url (get-filename-possibly-braced)))
    (emit "\">")
    (ignorespaces)
    (get-ctl-seq)
    (ignorespaces)
    (get-ctl-seq)))

(defun display-index-entry (s o)
  (mapc (lambda (c) (princ (if (or (char= c #\newline)) #\space c) o))
        (concatenate 'list s)))

(defun escape-opmac-index-entry (x)
  (let ((y '()))
    (dotimes (i (length x))
      (let ((c (char x i)))
        (case c
          ((#\") (push c y) (push c y))
          ((#\! #\@) (push #\" y) (push c y))
          (t (push c y)))))
    (concatenate 'string (nreverse y))))

(defun expand-html-index ()
  (let* ((s (get-peeled-group))
         (n (read-from-string s))
         (pageno (gethash n *index-table*)))
    (emit-page-node-link-start
      pageno (concatenate 'string *html-node-prefix* "index_" s))
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

(defun html-length (s)
  (declare (string s))
  (let ((n (length s))
        (res 0)
        (i 0)
        (skip-tag nil)
        (skip-entity nil))
    (loop
      (when (>= i n) (return res))
      (let ((c (char s i)))
        (incf i)
        (cond (skip-tag (when (char= c #\>) (setq skip-tag nil)))
              (skip-entity (when (char= c #\;) (setq skip-entity nil)))
              ((char= c #\<) (setq skip-tag t))
              ((char= c #\&) (incf res) (setq skip-entity t))
              (t (incf res)))))))

(defun do-llap ()
  (let* ((txt (tex-string-to-html-string (get-group)))
         (html-len (html-length txt))
         (txt-len (sp-to-pixels (tex-length html-len :ex))))
    (emit "<span style=\"position: relative\">")
    (emit "<span style=\"position: absolute; left: -")
    (emit txt-len)
    (emit "pt\">")
    (emit txt)
    (emit "</span></span>")))

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

;(trace do-regular-item)

(defun do-plain-item (n)
  (declare (fixnum n))
  (do-end-para)
  (emit-newline)
  (let ((parindent (sp-to-pixels (the-dimen "\\parindent"))))
    (emit "<p style=\"margin-left: ")
    (emit (* n parindent))
    (emit "pt; text-indent: 0pt\">")
    (emit "<span style=\"margin-left: ")
    (emit parindent)
    (emit "pt\"></span>")
    (emit "<span style=\"position: relative\">")
    (emit "<span class=item style=\"position: absolute; left: -")
    (emit parindent)
    (emit "pt\">")
    (tex2page-string (get-group))
    (ignorespaces)
    (emit-nbsp 2)
    (emit "</span></span>")))

;(trace do-plain-item)

(defun do-textindent ()
  (let ((parindent (sp-to-pixels (the-dimen "\\parindent"))))
    (do-noindent)
    (emit "<span style=\"margin-left: ")
    (emit parindent)
    (emit "pt\"></span>")
    (emit "<span style=\"position: relative\">")
    (emit "<span class=item style=\"position: absolute; left: -")
    (emit parindent)
    (emit "pt\">")
    (tex2page-string (get-group))
    (ignorespaces)
    (emit-nbsp 2)
    (emit "</span></span>")))

(defun do-proclaim ()
  (let* ((head (tex-string-to-html-string (get-till-char #\.)))
         (body (progn (get-actual-char) (ignorespaces)
                      (tex-string-to-html-string (get-till-par)))))
    (do-end-para)
    (emit "<div class=\"proclaim medskip\"><b>")
    (do-noindent)
    (emit head)
    (emit ".</b>")
    (emit-nbsp 2)
    (emit "<i>")
    (emit body)
    (emit "</i>")
    (do-end-para)
    (emit "</div>")
    (do-para)))

(defun do-item ()
  (case (car *tabular-stack*)
    (:description (do-description-item))
    ((:itemize :enumerate) (do-regular-item))
    (t (do-plain-item 1))))

;(trace do-item)

(defun do-itemize ()
  (do-end-para)
  (push :itemize *tabular-stack*)
  (emit "<ul")
  (when (or (tex2page-flag-boolean "\\TIIPslides")
            (tex2page-flag-boolean "\\TZPslides"))
    (emit " class=incremental"))
  (emit ">")
  (emit-newline))

(defun do-enditemize ()
  (do-end-para)
  (pop-tabular-stack :itemize)
  (emit "</ul>")
  (do-noindent))

(defun do-enumerate ()
  (do-end-para)
  (push :enumerate *tabular-stack*)
  (emit "<ol")
  (when (or (tex2page-flag-boolean "\\TIIPslides")
            (tex2page-flag-boolean "\\TZPslides"))
    (emit " class=incremental"))
  (emit ">")
  (emit-newline))

(defun do-endenumerate ()
  (pop-tabular-stack :enumerate)
  (do-end-para)
  (emit "</ol>")
  (do-noindent))

(defun do-opmac-list-style ()
  (ignorespaces)
  (setq *opmac-list-style* (get-actual-char)))

(defun do-bigskip (type)
  (declare (keyword type))
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
  (when (eql (snoop-actual-char) #\*) (get-actual-char))
  (get-group)
  (emit-nbsp 3))

(defun do-vspace ()
  (ignorespaces)
  (when (eql (snoop-actual-char) #\*) (get-actual-char))
  (get-group)
  (do-bigskip :vspace))

(defun do-htmlmathstyle ()
  (call-with-input-string/buffered (ungroup (get-group))
   (lambda ()
     (loop
       (ignorespaces :all)
       (let ((c (snoop-actual-char)))
         (when (not c) (return))
         (case (intern (string-upcase (scm-get-token)) :keyword)
           ((:image :display-image)
            (tex-def-0arg "\\TZPmathtext" "0")
            (tex-def-0arg "\\TZPmathimage" "1"))
           ((:no-image :no-display-image)
            (tex-def-0arg "\\TZPmathtext" "1")
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
       (ignorespaces :all)
       (let ((c (snoop-actual-char)))
         (when (not c) (return))
         (let ((directive (intern (string-upcase (scm-get-token)) :keyword)))
           (!colophon directive)
           (write-aux `(!colophon ,directive))))))))

(defun seconds-to-human-time (s)
  (declare (integer s))
  (multiple-value-bind (sec m h d mo y dow dst tz)
    (decode-universal-time s)
    (declare (ignore sec))
    (concatenate 'string
      (svref *week-day-names* dow)
      ", "
      (svref *short-month-names* (1- mo))
      " "
      (write-to-string d)
      ", "
      (write-to-string y)
      ", "
      (write-to-string
        (let ((h (mod h 12)))
          (if (= h 0) 12 h)))
      ":"
      (if (< m 10) "0" "")
      (write-to-string m)
      " "
      (if (<= 0 h 11) "a" "p")
      "m"
      " "
      "UTC"
      (if (> tz 0) "−" "+")
      (write-to-string (abs tz))
      (if dst "+1" ""))))

(defun output-colophon ()
  (let ((colophon-mentions-last-mod-time-p
          (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
              (tex2page-flag-boolean "\\TZPcolophontimestamp")))
        (colophon-mentions-tex2page-p
          (or (not (tex2page-flag-boolean "\\TZPcolophondisablecredit"))
              (tex2page-flag-boolean "\\TZPcolophoncredit")))
        (colophon-links-to-tex2page-website-p
          (or (not (tex2page-flag-boolean "\\TZPcolophondisableweblink"))
              (tex2page-flag-boolean "\\TZPcolophonweblink"))))
    (when (or colophon-mentions-last-mod-time-p colophon-mentions-tex2page-p)
      (do-end-para)
      (emit "<div class=\"rightline colophon\">")
      (when (and colophon-mentions-last-mod-time-p *last-modification-time*
               (> *last-modification-time* 0))
        (tex2page-string *last-modified*)
        (emit ": ")
        (emit (seconds-to-human-time *last-modification-time*))
        (emit "<br>"))
      (when colophon-mentions-tex2page-p
        (emit "<div class=\"rightline advertisement\">")
        (tex2page-string *html-conversion-by*)
        (emit " ")
        (when colophon-links-to-tex2page-website-p
          (emit-link-start
           "http://ds26gte.github.io/tex2page/index.html"))
        (emit *tex-logo*) (emit "2page ")
        (emit *tex2page-version*)
        (when colophon-links-to-tex2page-website-p (emit-link-stop))
        (emit "</div>"))
      (emit "</div>")
      (emit-newline))))

(defun point-to-adjacent-pages ()
  (let* ((prev-page (if (= *html-page-count* 0) nil (1- *html-page-count*)))
         (next-page (if (= *html-page-count* *last-page-number*) nil (1+ *html-page-count*))))
    (unless (= *last-page-number* 0)
      (when prev-page (emit-page-link-start prev-page))
      (emit "&#x3c;&#xb7;&#xb7;&#xb7;Prev ")
      (when prev-page (emit-link-stop))
      (emit "||")
      (when next-page (emit-page-link-start next-page))
      (emit " Next&#xb7;&#xb7;&#xb7;&#x3e;")
      (when next-page (emit-link-stop)))))

(defun output-head-or-foot-line (head-or-foot)
  (declare (keyword head-or-foot))
  (unless (or (tex2page-flag-boolean "\\TIIPsinglepage")
              (tex2page-flag-boolean "\\TZPsinglepage"))
    (do-end-para)
    ;align=right for lynx
    (emit "<div class=navigation>")
    (cond ((or (tex2page-flag-boolean "\\TIIPtexlayout")
               (tex2page-flag-boolean "\\TZPtexlayout"))
           (bgroup)
           (tex-let "\\folio" "\\TIIPfolio" nil)
           (tex2page-string
             (if (eq head-or-foot :head) "\\the\\headline" "\\the\\footline"))
           (egroup))
          (t (output-navbar head-or-foot)))
    (emit "</div>")
    (emit-newline)))

;(trace output-head-or-foot-line)

(defun output-navbar (head-or-foot)
  (declare (keyword head-or-foot))
  (let* ((first-page-p (= *html-page-count* 0))
         (last-page-not-determined-p (< *last-page-number* 0))
         (last-page-p (= *html-page-count* *last-page-number*))
         (toc-page-p (and *toc-page* (= *html-page-count* *toc-page*)))
         (index-page-p
          (and *index-page* (= *html-page-count* *index-page*)))
         (prev-page (if first-page-p nil (1- *html-page-count*)))
         (next-page (if last-page-p nil (1+ *html-page-count*))))
    (unless (and first-page-p
                 (or last-page-p
                     (and (eq head-or-foot :head)
                          last-page-not-determined-p)))
      (emit "[")
      (emit *navigation-sentence-begin*)
      (emit "<span")
      (when first-page-p (emit " class=disable"))
      (emit ">")
      (unless first-page-p (emit-page-link-start 0))
      (emit *navigation-first-name*)
      (unless first-page-p (emit-link-stop))
      (emit ", ")
      (unless first-page-p (emit-page-link-start prev-page))
      (emit *navigation-previous-name*)
      (unless first-page-p (emit-link-stop))
      (emit "</span>")
      (emit "<span")
      (when last-page-p (emit " class=disable"))
      (emit ">")
      (when first-page-p (emit "<span class=disable>"))
      (emit ", ")
      (when first-page-p (emit "</span>"))
      (unless last-page-p (emit-page-link-start next-page))
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
                                         "toc")))
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

;(trace output-navbar)

(defun do-eject ()
  (ignorespaces)
  (cond ((or (tex2page-flag-boolean "\\TIIPslides")
             (tex2page-flag-boolean "\\TZPslides"))
         (do-end-para)
         (emit "</div>")
         (emit-newline)
         (emit "<div class=slide>")
         (do-para))
        ((or (tex2page-flag-boolean "\\TIIPsinglepage")
             (tex2page-flag-boolean "\\TZPsinglepage")) t)
        (t (unless (and (not (snoop-actual-char))
                        (eql *current-source-file* *main-tex-file*))
            ;kludge: don't start a new page if \eject is the last thing in the
            ;main file.  This is mostly to placate story.tex, which although
            ;horrid as an example file, happens to be viewed as canonical by
            ;everyone looking at TeX
            (unless (> *last-page-number* 0)
              (flag-missing-piece :last-modification-time))
            (do-end-page)
            (tex-def-count 0 (incf *html-page-count*) t)
            (setq *html-page*
                  (concatenate 'string *aux-dir/* *jobname* *html-page-suffix*
                    (write-to-string *html-page-count*)
                    *output-extension*))
            (setq *html*
                  (make-ostream* :stream
                               (open *html-page* :direction :output
                                     :if-exists :supersede)))
            (do-start)))))

;(trace do-eject)

(defun output-html-preamble ()
  (when (stringp *doctype*)
    (emit "<!DOCTYPE ")
    (emit *doctype*)
    (emit ">")
    (emit-newline))
  (emit "<html lang=")
  (emit (resolve-defs "\\TZPlang"))
  ;(emit (tdef*-expansion (find-def "\\TZPlang")))
  (emit ">")
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
  (link-scripts)
  (emit "<meta name=robots content=\"index,follow\">")
  (emit-newline)
  (mapc #'emit *html-head*)
  (emit "</head>")
  (emit-newline)
  (emit "<body>")
  (emit-newline)
  (emit "<div")
  (when (or (tex2page-flag-boolean "\\TIIPslides")
            (tex2page-flag-boolean "\\TZPslides"))
    (emit " class=slide"))
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

(defun check-tex2page-lisp ()
  (let ((cl-p (not 'nil))
        (doc-expects-cl-p (tex2page-flag-boolean "\\TZPcommonlisp")))
    (unless (eql cl-p doc-expects-cl-p)
      (write-log :separation-newline)
      (write-log "! Document ")
      (write-log *main-tex-file*)
      (write-log " appears to require ")
      (write-log (if doc-expects-cl-p "Common Lisp" "Scheme"))
      (write-log " version of TeX2page."))))

(defun do-start ()
  (check-tex2page-lisp)
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
  (close-html-output-stream *html*))

(defun close-all-open-streams ()
  (when *aux-stream* (close *aux-stream*))
  (when *css-stream* (close *css-stream*))
  (when *index-stream* (close *index-stream*))
  (when *label-stream* (close *label-stream*))
  (when *bib-aux-stream* (close *bib-aux-stream*))
  (when *verb-stream* (close *verb-stream*))
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
  (write-log #\newline)
  (when *log-stream* (close *log-stream*))
  (princ "Transcript written on ")
  (princ *log-file*)
  (princ ".")
  (terpri))

(defun do-endinput ()
  (toss-back-char *invisible-space*)
  (toss-back-string "\\TIIPendinput"))

(defun do-bye ()
  (check-tex2page-lisp)
  (note-down-tex2page-flags)
  (unless (null *tex-if-stack*)
    (let ((n (list-length *tex-if-stack*)))
      (trace-if t "(\\end occurred when " n " \\if"
                (if (> n 1) "s were" " was") " incomplete)")))
  (unless (null *tex-env*)
    (trace-if t "\\end occurred inside a group at level "
              (list-length *tex-env*)))
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
  (close-all-open-streams)
  (call-external-programs-if-necessary)
  (show-unresolved-xrefs-and-missing-pieces))

(defun set-text-width ()
  (let ((hsize (cond ((setq *it* (find-def "\\TZPhsize"))
                      (tex2page-string (concatenate 'string "\\TIIPhsize=" (tdef*-expansion *it*)))
                      (the-dimen "\\TIIPhsize"))
                     ((or (tex2page-flag-boolean "\\TIIPtexlayout")
                          (tex2page-flag-boolean "\\TZPtexlayout"))
                      (the-dimen "\\hsize"))
                     (t nil))))
    (when hsize
      (princ "body { max-width: " *css-stream*)
      (princ (sp-to-pixels hsize) *css-stream*)
      (princ "pt; }" *css-stream*)
      (terpri *css-stream*))))

;(trace set-text-width)

(defun note-down-tex2page-flags ()
  (write-aux `(!lang ,(resolve-defs "\\TZPlang")))
  (write-aux `(!head-line ,(the-toks "\\headline")))
  (write-aux `(!foot-line ,(the-toks "\\footline")))
  (when (setq *it* (find-def "\\TZPtitle"))
    (let ((d *it*))
      (write-aux
       `(!preferred-title
         ,(tex-string-to-html-string (tdef*-expansion d))))))
  (when (tex2page-flag-boolean "\\TZPcolophonlastpage")
    (write-aux `(!colophon :last-page)))
  (when (or (tex2page-flag-boolean "\\TZPcolophondisabletimestamp")
            (not (tex2page-flag-boolean "\\TZPcolophontimestamp")))
    (write-aux `(!colophon :no-timestamp)))
  (when (or (tex2page-flag-boolean "\\TZPcolophondisablecredit")
            (not (tex2page-flag-boolean "\\TZPcolophoncredit")))
    (write-aux `(!colophon :dont-credit-tex2page)))
  (when (or (tex2page-flag-boolean "\\TZPcolophondisableweblink")
            (not (tex2page-flag-boolean "\\TZPcolophonweblink")))
    (write-aux `(!colophon :dont-link-to-tex2page-website)))
  (when (setq *it* (ctl-seq-no-arg-expand-once "\\TZPredirect"))
    (unless *redirect-url*
      (flag-missing-piece :html-head))
    (let ((url *it*)
          (seconds (ctl-seq-no-arg-expand-once "\\TZPredirectseconds")))
      (write-aux
        `(!html-redirect ,url ,seconds))))
  (when (tex2page-flag-boolean "\\TZPslides")
    (write-aux '(!slides))
    (write-aux '(!single-page))
    (let ((slidy-css-file "slidy.css"))
      (unless (probe-file slidy-css-file)
        (setq slidy-css-file "http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css"))
      (when (null *stylesheets*) (flag-missing-piece :stylesheets))
      (write-aux `(!stylesheet ,slidy-css-file)))
    (let ((slidy-js-file "slidy.js"))
      (unless (probe-file slidy-js-file)
        (setq slidy-js-file "http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js"))
      (when (null *scripts*) (flag-missing-piece :scripts))
      (write-aux `(!script ,slidy-js-file))))
  (when (tex2page-flag-boolean "\\TZPsinglepage")
    (write-aux '(!single-page)))
  (when (tex2page-flag-boolean "\\TZPtexlayout")
    (write-aux '(!tex-like-layout))
    (terpri *css-stream*)
    (princ "body { margin-top: " *css-stream*)
    (princ (sp-to-ems (+ (tex-length 0.5 :in) ; 1in is too much!
                         (the-dimen "\\voffset")))
           *css-stream*)
    (princ "em; }" *css-stream*)
    (terpri *css-stream*)
    (princ "body { margin-left: " *css-stream*)
    (princ (sp-to-ems (+ (tex-length 0.8 :in) (the-dimen "\\hoffset")))
           *css-stream*)
    (princ "em; }" *css-stream*)
    (terpri *css-stream*)
    (when (or (tex2page-flag-boolean "\\TZPrightjustify")
              (not (tex2page-flag-boolean "\\TZPraggedright")))
      (princ "body { text-align: justify; }" *css-stream*)
      (terpri *css-stream*))
    (princ "p { margin-bottom: 0pt; }" *css-stream*)
    (terpri *css-stream*)
    (princ "p { text-indent: " *css-stream*)
    (princ (sp-to-pixels (the-dimen "\\parindent")) *css-stream*)
    (princ "pt; }" *css-stream*)
    (terpri *css-stream*)
    (princ "p { margin-top: " *css-stream*)
    (princ (sp-to-pixels (the-dimen "\\parskip")) *css-stream*)
    (princ "pt; }" *css-stream*)
    (terpri *css-stream*)
    (princ ".mathdisplay { margin-top: " *css-stream*)
    (princ (sp-to-pixels (the-dimen "\\abovedisplayskip")) *css-stream*)
    (princ "pt; margin-bottom: " *css-stream*)
    (princ (sp-to-pixels (the-dimen "\\belowdisplayskip")) *css-stream*)
    (princ "pt; }" *css-stream*)
    (terpri *css-stream*)
    (princ ".navigation { color: black; font-style: normal; }" *css-stream*)
    (terpri *css-stream*))
  (set-text-width)
  (unless (tex2page-flag-boolean "\\TZPtextext")
    (write-aux `(!tex-text 1)))
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
    (declare (keyword diac))
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

(defun do-backslash-equal ()
  (unless (and (not (null *tabular-stack*))
               (eql (car *tabular-stack*) :tabbing))
    (do-diacritic :macron)))

(defun do-mathdg ()
  (let ((*math-mode-p* t)
        (*in-display-math-p* t)
        (*tabular-stack* '())
        (*ligatures-p* nil))
    (do-end-para)
    (emit "<div class=")
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
  (declare (stream o))
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
    (t (princ *imgpreamble* o)
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
  (declare (stream o))
  (case *tex-format*
    ((:latex) (princ "\\end{document}" o) (terpri o))
    (t (princ "\\bye" o) (terpri o))))

(defun skipping-img-file ()
  (incf *img-file-count*))

(defun next-html-image-file-stem ()
  (incf *img-file-count*)
  (concatenate 'string *subjobname* *img-file-suffix*
               (write-to-string *img-file-count*)))

(defun call-with-html-image-stream (p &optional alt)
  (declare (function p))
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
    (emit "<div class=\"mathdisplay ")
    (emit *display-justification*)
    (emit "\">"))
  (let ((old-math-mode-p *math-mode-p*)
        (old-in-display-math-p *in-display-math-p*)
        (old-tabular-stack *tabular-stack*))
    (setq *math-mode-p* t
          *in-display-math-p* display-p
          *tabular-stack* '())
    (when display-p (emit "<table style=\"margin-left:auto; margin-right:auto\"><tr><td>"))
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
  (if (and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
               (tex2page-flag-boolean "\\TZPmathimage"))
           (not *temporarily-use-utf8-for-math-p*))
      (progn
       (emit "<div class=\"mathdisplay ")
       (emit *display-justification*)
       (emit "\">")
       (call-with-html-image-stream
        (lambda (o) (princ "$$" o) (princ tex-string o) (princ "$$" o))
        tex-string)
       (emit "</div>")
       (do-noindent))
    (do-math-fragment tex-string :display)))

(defun tex-math-delim-string (type)
  (let ((top '()) (mid '()) (bot '()) (ext '()))
    (ecase type
      (:lparen (setq top "&#x239b;" bot "&#x239d;" ext "&#x239c;" mid ext))
      (:lbrack (setq top "&#x23a1;" bot "&#x23a3;" ext "&#x23a2;" mid ext))
      (:lbrace (setq top "&#x23a7;" mid "&#x23a8;" bot "&#x23a9;" ext "&#x23aa;"))
      (:lvert (setq ext "&#x239c;" top ext mid ext bot ext))
      (:nulldelim (setq ext "" top ext mid ext bot ext))
      (:rparen (setq top "&#x239e;" bot "&#x23a0;" ext "&#x239f;" mid ext))
      (:rbrack (setq top "&#x23a4;" bot "&#x23a6;" ext "&#x23a5;" mid ext))
      (:rbrace (setq top "&#x23ab;" mid "&#x23ac;" bot "&#x23ad;" ext "&#x23ae;"))
      (:rvert (setq ext "&#x239f;" top ext mid ext bot ext)))
    (concatenate 'string
      "<table class=mathdelim><tr><td>" top "</td></tr>"
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
  (let ((*html* (make-html-output-stream)))
    (call-with-input-string/buffered ""
      (lambda ()
        (do-math-fragment s nil)
        (generate-html)))
    (html-output-stream-to-string *html*)))

(defun do-intext-math (tex-string)
  (declare (string tex-string))
  (let* ((*math-needs-image-p* nil)
         (html-string (tex-math-string-to-html-string tex-string)))
    (if (and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                 (tex2page-flag-boolean "\\TZPmathimage"))
             *math-needs-image-p*
             (not *temporarily-use-utf8-for-math-p*))
      (call-with-html-image-stream
        (lambda (o) (princ #\$ o) (princ tex-string o) (princ #\$ o))
        tex-string)
      (emit html-string))))

(defun do-mathp ()
  (call-with-html-image-stream
   (lambda (o) (princ #\$ o) (princ (get-group) o) (princ #\$ o))))

(defun do-math ()
  (let ((display-p nil))
    (when (eql (snoop-actual-char) #\$) (setq display-p t) (get-actual-char))
    (let ((o (make-string-output-stream)))
      (loop
        (dump-till-char #\$ o)
        (cond ((not display-p) (return))
              (t (let ((c (get-actual-char)))
                   (cond ((not c) (terror 'do-math "Display math should end with $$."))
                         ((char= c #\$) (return))
                         (t (princ #\$ o) (princ c o)))))))
      (funcall (if display-p #'do-display-math #'do-intext-math)
               (get-output-stream-string o)))))

(defun dump-till-char (d o)
  (declare (character d) (stream o))
  (let ((nesting 0) (escape-p nil) c)
    (loop
      (setq c (get-actual-char))
      (when (not c) (terror 'dump-till-char "Missing " d "."))
      (when (and (char= c d) (= nesting 0)) (return))
      (princ c o)
      (cond (escape-p (setq escape-p nil))
            ((char= c #\{) (incf nesting))
            ((char= c #\}) (decf nesting))
            ((char= c #\\) (setq escape-p t))))))

(defun dump-till-ctl-seq (cs o)
  (let* ((*not-processing-p* t)
         (nesting 0))
    (loop
      (let ((c (snoop-actual-char)))
        (when (not c) (terror 'dump-till-ctl-seq))
        (cond ((= (catcode c) **escape**)
               (let ((x (get-ctl-seq)))
                 (if (string= x cs) (return)
                   (princ x o))))
              (t (princ (get-actual-char) o)
                 (cond ((char= c #\{) (incf nesting))
                       ((char= c #\}) (decf nesting)))))))))

(defun dump-till-end-env (env o)
  (declare (string env) (stream o))
  (let* ((endenv (concatenate 'string "\\end" env))
         (endenv-prim (find-corresp-prim endenv))
         (endenv-prim-th (find-corresp-prim-thunk endenv))
         (*not-processing-p* t)
         (brace-nesting 0)
         (env-nesting 0)
         c)
    (loop
      (setq c (snoop-actual-char))
      (when (not c) (terror 'dump-till-end-env env))
      (cond ((= (catcode c) **escape**)
             (let ((x (get-ctl-seq)))
               (cond ((string= (find-corresp-prim x) endenv-prim) (return))
                     ((string= x "\\begin")
                      (princ x o)
                      (let ((g (get-grouped-environment-name-if-any)))
                        (when g
                          (princ #\{ o)
                          (princ g o)
                          (princ #\} o))
                        (when (and g (string= g env)) (incf env-nesting))))
                     ((string= x "\\end")
                      (let ((g (get-grouped-environment-name-if-any)))
                        (cond ((and g
                                    (or *dumping-nontex-p* (= env-nesting 0))
                                    (let ((endg (concatenate 'string "\\end" g)))
                                      (or (string= (find-corresp-prim endg) endenv-prim)
                                          (eql (find-corresp-prim-thunk endg) endenv-prim-th))))
                               (return))
                              (t (princ x o)
                                 (when g
                                   (princ #\{ o)
                                   (princ g o)
                                   (princ #\} o))
                                 (when (and g (string= g env)) (decf env-nesting))))))
                     (t (princ x o)))))
            ((and (= (catcode c) **comment**) (not *dumping-nontex-p*))
             (do-comment) (write-char #\% o) (terpri o))
            (t (write-char (get-actual-char) o)
               (cond ((char= c #\{)
                      (incf brace-nesting))
                     ((char= c #\})
                      (decf brace-nesting))))))))

(defun dump-imgdef (f)
  (declare (string f))
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
              (when (not c)
                (terror 'do-img-preamble "Missing \\endimgpreamble"))
              (cond ((= (catcode c) **escape**)
                     (let ((x (get-ctl-seq)))
                       (when (member x '("\\endimgpreamble" "\\endgifpreamble" "\\endmathpreamble")
                                     :test #'string=)
                         (return r))
                       (setq r (concatenate 'string r x))))
                    (t (get-actual-char)
                       (setq r (concatenate 'string r (list c))))))))))

(defun do-open-stream (type)
  (declare (keyword type))
  (let* ((n (get-number))
         (f (get-filename))
         (frame (top-texframe))
         (sl (if (eq type :out) (texframe*-ostreams frame) (texframe*-istreams frame)))
         (c (gethash n sl)))
    ;(format t "n = ~s; f = ~s; c = ~s~%" n f c)
    (unless (eq c :free) (terror 'do-open-stream))
    (setf (gethash n sl)
          (ecase type
            (:out
             (setq f (add-dot-tex-if-no-extension-provided f))
             (open f :direction :output :if-exists :supersede))
            (:in
             (setq f (actual-tex-filename f))
             (make-istream* :stream (open f :direction :input)))))))

(defun do-close-stream (type)
  (let* ((frame (top-texframe))
         (sl (if (eql type :out) (texframe*-ostreams frame) (texframe*-istreams frame)))
         (o (get-number))
         (c (gethash o sl)))
    (when (eq c :free) (terror 'do-close-stream))
    (close (ecase type
             (:out c)
             (:in (istream*-stream c))))
    (setf (gethash o sl) :free)))

;(trace do-open-stream do-close-stream)

(defun tex-write-output-string (s)
  (declare (string s))
  (let ((*html* (make-html-output-stream))
         (*outputting-to-non-html-p* t))
    (call-with-input-string/buffered s
      (lambda ()
        (loop
          (let ((c (snoop-actual-char)))
            (when (not c) (return))
            (case c
              (#\\ (do-tex-ctl-seq (get-ctl-seq)))
              (t (emit-html-char (get-actual-char))))))))
    (html-output-stream-to-string *html*)))

(defun do-write-aux (o)
  (declare (fixnum o))
  (let ((output (tex-write-output-string (get-peeled-group))))
    (cond ((and (= o 18) *enable-write-18-p*)
           (system output))
          ((member o '(16 18) :test #'=)
           (write-log output)
           (write-log :separation-space))
          ((setq *it* (find-ostream o))
           (let ((p *it*))
             (when (eq p :free) (terror 'do-write-aux))
             (princ output p)
             (princ #\space p)))
          (t (terror 'do-write-aux)))))

(defun do-wlog ()
  (let ((output (tex-write-output-string (get-peeled-group))))
    (write-log output t)))

(defun do-write ()
  (do-write-aux (get-number)))

(defun do-message ()
  (do-write-aux 16))

(defun read-tex-line (p)
  (let* ((*current-tex2page-input* p)
         (r '()))
    (loop
      (let ((c (snoop-actual-char)))
        (when (not c)
          (return
           (if (null r) c (concatenate 'string (nreverse r)))))
        (when (char= c #\newline) (get-actual-char)
          (return (concatenate 'string (nreverse r))))
        (when (char= c #\{)
          (return
           (concatenate 'string (concatenate 'string (nreverse r))
             (get-group))))
        (push (get-actual-char) r)))))

(defun do-read (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (let* ((i (get-number))
         (x (progn (get-to) (get-ctl-seq)))
         (p nil))
    (cond ((member i '(-1 16) :test #'=)
           (setq p (make-istream* :stream *standard-input*))
           (unless (= i -1)
             (write-log x) (write-log #\=)))
          ((setq *it* (find-istream i))
           (setq p *it*)
           (when (eq p :free) (terror 'do-read)))
          (t (terror 'do-read)))
    (funcall (if globalp #'tex-gdef-0arg #'tex-def-0arg)
             x
             (let ((line (read-tex-line p)))
               (if (not line) "" line)))))

(defun do-typein ()
  (let ((ctlseq (get-bracketed-text-if-any))
        (p (make-istream* :stream *standard-input*)))
    (write-log :separation-newline)
    (write-log (tex-string-to-html-string (get-group)))
    (write-log :separation-newline)
    (write-log (or ctlseq "\\@typein"))
    (write-log #\=)
    (let ((l (read-tex-line p)))
      (when (not l) (setq l ""))
      (cond (ctlseq (tex-def-0arg ctlseq l))
            (t (tex2page-string l))))))

(defun do-ifeof ()
  (let* ((i (get-number))
         (c (gethash i *input-streams*)))
    (when (eq c :free) (terror 'do-ifeof))
    (if (not (read-char c nil))
        (do-iftrue)
      (do-iffalse))))

(defun do-iffalse ()
  (push nil *tex-if-stack*))

(defun do-iftrue ()
  (push t *tex-if-stack*))

;(trace do-iftrue do-iffalse)

(defun insert-tex-if (test)
  (if test
      (do-iftrue)
    (do-iffalse)))

(defun do-ifx ()
  (let* ((one (get-raw-token/is))
         (two (get-raw-token/is))
         (one2 one)
         (two2 two))
    (ignorespaces)
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
         (rel (char (get-raw-token/is) 0))
         (two (get-number)))
    ;(format t "one= ~s; two= ~s~%" one two)
    (if (funcall
         (case rel
           (#\< #'<)
           (#\= #'=)
           (#\> #'>)
           (t (terror 'do-ifnum "Missing = for \\ifnum.")))
         one two)
        (do-iftrue)
      (do-iffalse))))

;(trace do-ifnum)

(defun read-ifcase-clauses ()
  (let* ((*not-processing-p* t)
         (else-clause nil)
         (or-clauses '())
         (elsep nil)
         (outer-loop-done nil))
    (loop
      (when outer-loop-done (return))
      (let ((clause ""))
        (loop
          (let ((c (snoop-actual-char)))
            (when (not c)
              (terror 'read-ifcase-clauses "Incomplete \\ifcase."))
            (cond ((= (catcode c) **escape**)
                   (let ((x (get-ctl-seq)))
                     (cond ((string= x "\\or") (ignorespaces)
                                               (when elsep
                                                 (terror 'read-ifcase-clauses
                                                         "Extra \\or."))
                                               (push clause or-clauses)
                                               (return))
                           ((string= x "\\else") (ignorespaces)
                                                 (when elsep
                                                   (terror 'read-ifcase-clauses
                                                           "Extra \\else."))
                                                 (push clause or-clauses) (setq elsep t) (return))
                           ((string= x "\\fi") (ignorespaces)
                                               (cond (elsep (setq else-clause clause))
                                                     (t (push clause or-clauses)))
                                               (setq outer-loop-done t)
                                               (return))
                           (t (setq clause (concatenate 'string clause x))))))
                  (t (get-actual-char)
                     (setq clause
                           (concatenate 'string clause (list c)))))))))
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
  (ignorespaces)
  (when (null *tex-if-stack*) (terror 'do-else "Extra \\else."))
  (let ((top-if (pop *tex-if-stack*)))
    (push (not top-if) *tex-if-stack*)))

(defun do-fi ()
  ;(format t "doing do-fi~%")
  ;(format t "inside para? ~s~%" *in-para-p*)
  (ignorespaces :stop-before-first-newline)
  ;(format t "done do-fi ignorespaces~%")
  (when (null *tex-if-stack*) (terror 'do-fi "Extra \\fi."))
  (pop *tex-if-stack*))

;(trace do-fi)

(defun do-newif ()
  (let* ((iffoo (get-ctl-seq))
         (foo (concatenate 'string "\\" (subseq iffoo 3)))
         (foo-register (concatenate 'string foo "BOOLEANREGISTER")))
    (plain-count foo-register 0 nil)
    (tex-def-thunk iffoo
                   (lambda ()
                     (push (> (the-count foo-register) 0) *tex-if-stack*))
                   nil)
    (tex-def-thunk (concatenate 'string foo "true")
                   (lambda () (plain-count foo-register 1 nil)) nil)
    (tex-def-thunk (concatenate 'string foo "false")
                   (lambda () (plain-count foo-register 0 nil)) nil)))

;(trace do-newif)

(defun do-htmlimg (env)
  (call-with-html-image-stream (lambda (o) (dump-till-end-env env o))))

(defun find-img-file-extn ()
  (case (tex2page-flag-value "\\TZPimageformat")
    ((#\g #\G) ".gif")
    ((#\j #\J) ".jpeg")
    (t ".png")))

(defun do-htmlimageformat ()
  (tex-def-0arg "\\TZPimageformat" (get-peeled-group)))

(defun do-htmlimageconversionprogram ()
  (tex-def-0arg "\\TZPimageconverter" (get-peeled-group)))

(defun do-htmlimgmagnification () t)

(let ((tex-prog-name nil))
  (defun call-mp (f)
    (unless tex-prog-name
      (setq tex-prog-name "tex")
      #|
      (let ((d (find-def "\\TZPtexprogname")))
        (when d (setq tex-prog-name (tdef*-expansion d))))
      (unless tex-prog-name (setq tex-prog-name "xetex"))
      (when (eq *tex-format* :latex)
        (setq tex-prog-name
              (concatenate 'string (subseq tex-prog-name 0
                                           (- (length tex-prog-name) 3))
                "latex")))
      |#)
    (system (concatenate 'string *metapost* " -tex=" tex-prog-name " " f))))

(let ((tex-prog-name nil)
      (tex-output-format nil))
  (defun call-tex (f)
    ;run tex on f and return f.ps or f.pdf if successful
    (unless tex-prog-name
      (let ((d (find-def "\\TZPtexprogname")))
        (when d (setq tex-prog-name (tdef*-expansion d))))
      (unless tex-prog-name (setq tex-prog-name "xetex"))
      (setq tex-output-format
            (if (or (eql (search "pdf" tex-prog-name) 0)
                    (eql (search "xe" tex-prog-name) 0)
                    (eql (search "lua" tex-prog-name) 0))
              :pdf :dvi))
      (when (eq *tex-format* :latex)
        (setq tex-prog-name
              (concatenate 'string (subseq tex-prog-name 0
                                           (- (length tex-prog-name) 3))
                           "latex"))))
      (let* ((dvi-file (concatenate 'string f
                                   (if (eq tex-output-format :pdf)
                                     ".pdf" ".dvi")))
             (outfile dvi-file))
        (system (concatenate 'string
                             tex-prog-name
                             " " f))
        (when (probe-file dvi-file)
          (let ((logfile (concatenate 'string f ".log")))
            (when (probe-file logfile)
              ;scan the log file for sign of problems
              (let ((fine-p
                      (with-open-file (i logfile :direction :input)
                        (let (x)
                          (loop
                            (setq x (read-line i nil))
                            (when (not x) (return t))
                            (when (search "! I can't find file" x)
                              ;the output can't be good
                              (return nil)))))))
                (when fine-p
                  (unless (eq tex-output-format :pdf)
                    (let ((ps-file (concatenate 'string f ".ps")))
                      (system
                        (concatenate 'string "dvips " dvi-file " -o " ps-file))
                      (setq outfile ps-file)))
                  outfile))))))))

(defun ps-to-img/gif/netpbm (ps-file img-file)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                ".ppm.1 " ps-file " quit.ps"))
  (system (concatenate 'string "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
  (system (concatenate 'string "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
  (system
   (concatenate 'string "ppmtogif -transparent rgb:ff/ff/ff < " img-file ".ppm > "
                *aux-dir/* img-file))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string img-file e)))
        '(".ppm" ".ppm.tmp" ".ppm.1")))

(defun ps-to-img/png/netpbm (ps-file img-file)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                ".ppm.1 " ps-file " quit.ps"))
  (system (concatenate 'string "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
  '(system
    (concatenate 'string "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
  (system
   (concatenate 'string "pnmtopng -interlace -transparent \"#FFFFFF\" " " < " img-file
                ".ppm.tmp > " *aux-dir/* img-file))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string img-file e)))
        '(".ppm.1" ".ppm.tmp" ".ppm")))

(defun ps-to-img/jpeg/netpbm (ps-file img-file)
  (system
   (concatenate 'string *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                ".ppm.1 " ps-file  " quit.ps"))
  (system (concatenate 'string "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
  (system (concatenate 'string "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
  (system
   (concatenate 'string "ppmtojpeg --grayscale < " img-file ".ppm > " *aux-dir/* img-file))
  (mapc (lambda (e) (ensure-file-deleted (concatenate 'string img-file e)))
        '(".ppm.1" ".ppm.tmp" ".ppm")))

(defun ps-to-img (ps-file img-file)
  (case (tex2page-flag-value "\\TZPimageconverter")
    ((#\i #\I)
     (system
      (concatenate 'string "convert -transparent white -trim "
                   ps-file  " " img-file)))
    (t (case (tex2page-flag-value "\\TZPimageformat")
         ((#\p #\P) (ps-to-img/png/netpbm ps-file img-file))
         ((#\j #\J) (ps-to-img/jpeg/netpbm ps-file img-file))
         (t (ps-to-img/gif/netpbm ps-file img-file))))))

(defun tex-to-img (f)
  (incf *img-file-tally*)
  (let ((img-file (concatenate 'string *aux-dir/* f (find-img-file-extn))))
    (unless (probe-file img-file)
      (write-log :separation-space)
      (write-log #\{)
      (write-log (concatenate 'string f ".tex"))
      (write-log :separation-space)
      (write-log "->")
      (write-log :separation-space)
      (cond ((setq *it* (call-tex f))
             (ps-to-img *it* img-file)
             (write-log img-file)
             '(mapc
               (lambda (e)
                 (ensure-file-deleted (concatenate 'string f e)))
               '(".aux" ".dvi" ".log" ".pdf" ".ps" ".tex")))
            (t (write-log "failed, try manually")))
      (write-log #\})
      (write-log :separation-space))))

(defun call-with-lazy-image-stream (eps-file img-file-stem p)
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
    (call-with-lazy-image-stream eps-file img-file-stem
      (lambda (o)
        (princ "\\epsfbox{" o)
        (princ eps-file o)
        (princ #\} o)))))

(defun do-epsfig ()
  (let ((*imgpreamble-inferred* (cons :epsfbox *imgpreamble-inferred*)))
    (call-with-html-image-stream
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
  (do-img-src (fully-qualify-url image-file))
  (emit "\" ")
  (emit " alt=\"")
  (emit image-file)
  (emit "\"")
  (emit ">"))

(defun do-includegraphics ()
  (let* ((starred-p (eat-star))
         (b1 (get-bracketed-text-if-any))
         (b2 (and b1 (get-bracketed-text-if-any)))
         (f (get-filename-possibly-braced))
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
                     (cons :includegraphics *imgpreamble-inferred*))
                   (img-file-stem (next-html-image-file-stem)))
               (call-with-lazy-image-stream (or ffull f) img-file-stem
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
                   (princ #\} o)))
               (source-img-file img-file-stem))))))

(defun do-resizebox ()
  (let* ((arg1 (get-group))
         (arg2 (get-group))
         (arg3 (get-group))
         (*imgpreamble-inferred*
          (cons :includegraphics *imgpreamble-inferred*)))
    (call-with-html-image-stream
     (lambda (o)
       (princ "\\resizebox" o)
       (princ arg1 o)
       (princ arg2 o)
       (princ arg3 o)))))

(defun do-mfpic-opengraphsfile ()
  (setq *mfpic-file-stem* (get-filename-possibly-braced))
  (when *mfpic-stream* (close *mfpic-stream*))
  (let ((f (concatenate 'string *mfpic-file-stem* *mfpic-tex-file-suffix*)))
    (setq *mfpic-stream* (open f :direction :output
                             :if-exists :supersede)))
  (setq *mfpic-file-num* 0)
  (princ "\\input mfpic \\usemetapost " *mfpic-stream*)
  (terpri *mfpic-stream*)
  (princ "\\opengraphsfile{" *mfpic-stream*)
  (princ *mfpic-file-stem* *mfpic-stream*)
  (princ #\} *mfpic-stream*)
  (terpri *mfpic-stream*)
  (tex-def-prim "\\headshape"
   (lambda ()
       (let* ((g1 (get-group))
              (g2 (get-group))
              (g3 (get-group)))
             (princ "\\headshape" *mfpic-stream*)
             (princ g1 *mfpic-stream*)
             (princ g2 *mfpic-stream*)
             (princ g3 *mfpic-stream*)
             (terpri *mfpic-stream*))))
  (tex-def-prim "\\mfpframesep" #'eat-dimen)
  (tex-def-prim "\\mftitle" #'get-group))

(defun do-mfpic-closegraphsfile ()
  (princ "\\closegraphsfile" *mfpic-stream*)
  (terpri *mfpic-stream*)
  (close *mfpic-stream*)
  (let ((tex-f (concatenate 'string *mfpic-file-stem* *mfpic-tex-file-suffix*))
        (mp-f (concatenate 'string *mfpic-file-stem* ".mp")))
    (unless (probe-file mp-f)
      (let ((*tex-format* :plain))
        (call-tex tex-f)))
    (when (probe-file mp-f) (call-mp mp-f))))

(defun do-mfpic ()
  (princ "\\mfpic" *mfpic-stream*)
  (dump-till-end-env "mfpic" *mfpic-stream*)
  (princ "\\endmfpic" *mfpic-stream*)
  (terpri *mfpic-stream*)
  (setq *mfpic-file-num* (1+ *mfpic-file-num*))
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
      (emit "<div class=")
      (emit *display-justification*)
      (emit ">"))
    (call-with-html-image-stream
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

(defun do-hbox ()
  (ignorespaces) ;ignore active space for this and next line?
  (get-to)
  (eat-dimen)
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (case c
      (#\{ (get-actual-char))
      (#\\ (get-ctl-seq)) ;FIXME, may not be \bgroup
      (t (get-actual-char) (toss-back-char #\})
         (toss-back-char c))))
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

(defun do-latex-frac ()
  (tex2page-string (ungroup (get-token)))
  (emit
    (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
          (t (incf *math-height*)
             "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td class=centerline>")))
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
          (t (emit "</td><td><table class=leftline><tr><td class=centerline>")
             (tex2page-string (get-till-char #\/))
             (get-actual-char) (ignorespaces)
             (emit "<hr noshade>") (tex2page-string (get-token))
             (emit "</td></tr></table></td><td>")))))

(defun do-eqno ()
  (unless *in-display-math-p*
    (terror 'do-eqno "You can't use \\eqno in math mode"))
  (emit "</td><td width=10% class=rightline>"))

(defun do-eqalign (type)
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (not c) (terror 'do-eqalign "Missing {"))
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
                   (emit "<div class=centerline><table><tr><td>")
                   (toss-back-char #\{)
                   (do-eqalign type))
          (t (emit "</td></tr>") (emit-newline) (emit "<tr><td>")))))

(defun do-pmatrix ()
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (not c)
              (not (char= c #\{)))
      (terror 'do-pmatrix "Missing {"))
    (bgroup)
    (setq *math-delim-left* :lparen
          *math-delim-right* :rparen)))

;(trace do-pmatrix)

(defun do-over ()
  (emit
   (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
         (t (incf *math-height*)
            "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td class=centerline>"))))

(defun eat-till-eol ()
  ;move just past the next newline
  (loop
    (let ((c (get-actual-char)))
      (when (or (not c) (char= c #\newline)) (return)))))

(defun eat-till-char (d)
  (loop
    (let ((c (get-actual-char)))
      (when (or (not c) (char=  c d)) (return)))))

(defun do-comment ()
  (eat-till-eol)
  (when (munched-a-newline-p)
    (toss-back-char #\newline) (toss-back-char #\newline)))

(defun string=join (ss sepc)
  (let ((res ""))
    (loop
      (when (null ss) (return res))
      (let ((s (pop ss)))
        (setq res
              (cond ((string= res "") s)
                    (t (concatenate 'string res (list sepc) s))))))))

(defun kpsewhich (f)
  (let ((tmpf (concatenate 'string *aux-dir/* *jobname* "-Z-Z.temp")))
    (system (concatenate 'string "kpsewhich -- " f " > " tmpf))
    (let ((f (and (probe-file tmpf)
                  (with-open-file (i tmpf :direction :input)
                    (read-line i nil)))))
      (ensure-file-deleted tmpf)
      (if (not f) nil
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
        (if (not (null *tex2page-inputs*))
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
          "begin"
          "begin0"
          "case"
          "cond"
          "define"
          "define-macro"
          "define-syntax"
          "defmacro"
          "defstruct"
          "delay"
          "do"
          "else"
          "flet"
          "fluid-let"
          "if"
          "labels"
          "lambda"
          "let"
          "let*"
          "let-syntax"
          "let-values"
          "letrec"
          "letrec-syntax"
          "macrolet"
          "or"
          "quasiquote"
          "quote"
          "set!"
          "syntax-case"
          "syntax-rules"
          "unless"
          "unquote"
          "unquote-splicing"
          "when"
          "with"
          "with-handlers"

          "assert"
          "block"
          "decf"
          "defpackage"
          "defparameter"
          "defun"
          "defvar"
          "destructuring-bind"
          "do-all-symbols"
          "do-external-symbols"
          "do-symbols"
          "dolist"
          "dotimes"
          "ecase"
          "etypecase"
          "eval-when"
          "handler-bind"
          "handler-case"
          "incf"
          "loop" ;NB! common variable in Scheme
          "multiple-value-bind"
          "multiple-value-setq"
          "pop"
          "prog1"
          "progn"
          "push"
          "setf"
          "setq"
          "typecase"
          "unwind-protect"
          "with-input-from-string"
          "with-open-file"
          "with-open-socket"
          "with-open-stream"
          "with-output-to-string"
          "with-slots"

          ))
  t)

(defun actual-tex-filename (f &optional check-timestamp-p)
  (declare (string f))
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
      (load-aux-file)
      )
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
            (not c)
          (terror 'ignore-tex-specific-text "Missing \\end" env))
        (cond ((= (catcode c) **escape**)
               (let ((x (get-ctl-seq)))
                 (cond ((string= x endenv) (return))
                       ((string= x "\\end")
                        (let ((g (get-grouped-environment-name-if-any)))
                          (when (and g (string= g env)) (return)))))))
              (t (get-actual-char)))))))

(defun do-rawhtml ()
  (ignorespaces)
  (loop
    (let ((c (snoop-actual-char)))
      (cond ((not c)
             (terror 'do-rawhtml "missing \\endrawhtml"))
            ((= (catcode c) **escape**)
             (let* ((x (get-ctl-seq))
                    (y (find-corresp-prim x)))
               (cond ((string= y "\\endrawhtml")
                      (ignorespaces) (return))
                     ((and (string= y "\\end")
                           (setq *it* (get-grouped-environment-name-if-any)))
                      (let* ((g *it*)
                             (y (find-corresp-prim (concatenate 'string x g))))
                        (cond ((string= y "\\endrawhtml")
                               (ignorespaces) (return))
                              (t (emit "\\end{") (emit g) (emit "}")))))
                     ((string= x "\\\\")
                      (emit c) (toss-back-char c))
                     (t (emit x)))))
            (t (get-actual-char) (emit c))))))

(defun do-htmlheadonly ()
  (when (null *html-head*) (flag-missing-piece :html-head))
  (let ((s '()) s2)
    (loop
      (let ((c (snoop-actual-char)))
        (cond ((not c)
               (setq s2 (concatenate 'string (nreverse s)))
               (write-aux `(!html-head ,s2))
               (return))
              ((= (catcode c) **escape**)
               (setq s2 (concatenate 'string (nreverse s)))
               (write-aux `(!html-head ,s2))
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

(defun resolve-cdefs (c)
  (when (setq *it* (find-cdef c))
    (let ((y *it*))
      (get-actual-char)
      (expand-tex-macro (cdef*-optarg y)
                        (cdef*-argpat y)
                        (cdef*-expansion y)
                        (cdef*-catcodes y)))))

;(trace resolve-cdefs)

(defun resolve-defs (x)
  ;(format t "resolve-defs ~s~%" x)
  (when (setq *it* (find-def x))
    (let ((y *it*))
      (cond ((setq *it* (tdef*-defer y))
             *it*)
            ((tdef*-thunk y)
             nil)
            ((and (null (tdef*-argpat y)) (not (tdef*-optarg y))
                  (setq *it* (tdef*-expansion y)))
             *it*)
            ((and (inside-false-world-p)
                  (not (if-aware-ctl-seq-p x))
                  ;(> (length (tdef*-argpat y)) 0)
                  )
             nil)
            (t
              (when *outer-p*
                (setq *outer-p* nil)
                ;should this "outer" invisible sp be distinguished from regular inv sp?
                (toss-back-char *invisible-space*)
                )
              (progn
                (expand-tex-macro (tdef*-optarg y)
                                  (tdef*-argpat y)
                                  (tdef*-expansion y)
                                  (tdef*-catcodes y))
                ))))))

;(trace resolve-defs)

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
    (when (= (catcode c) **escape**)
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

(defun do-futurelet-aux (first second third)
  (tex-let-general first third nil)
  (toss-back-char *invisible-space*)
  (toss-back-string third)
  (toss-back-char *invisible-space*)
  (toss-back-string second))

(defun set-start-time ()
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time *start-time*)
      ;TeX uses local time zone so we don't worry about reporting what it is
    (declare (ignore s))
    (plain-count "\\time" (+ (* 60 h) m))
    (plain-count "\\day" d)
    (plain-count "\\month" mo)
    (plain-count "\\year" y)))

(defun initialize-globals ()

  (tex-def-countdef "\\pageno" 0 0 t)
  (tex-def-count 10 21 t) ;count register count
  (tex-def-count 11 9 t) ;dimen
  (tex-def-count 14 9 t) ;box
  (tex-def-count 15 9 t) ;toks
  (tex-def-count 16 -1 t) ;input stream
  (tex-def-count 17 -1 t) ;output stream

  (set-start-time)

  ;
  ;for TeX, 0 <= \language <= 255; for TeX2page, let's make \language =
  ;256
  (plain-count "\\language" 256)
  ;
  ;the deepest possible section, the part, has depth -1; so -2 is the
  ;closest-to-0 number that is a meaningless depth
  (plain-count "\\secnumdepth" -2)
  (plain-count "\\tocdepth" -2)

  (plain-count "\\footnotenumber" 0)
  (plain-count "\\TIIPtabularborder" 1)
  (plain-count "\\TIIPnestedtabularborder" 0)
  (plain-count "\\TIIPobeyspacestrictly" 0)
  (plain-count "\\TIIPobeylinestrictly" 0)
  (plain-count "\\TIIPsettabscolumns" 0)
  (plain-count "\\errorcontextlines" 5)
  (plain-count "\\doublehyphendemerits" 10000)
  (plain-count "\\finalhyphendemerits" 5000)
  (plain-count "\\hyphenpenalty" 50)
  (plain-count "\\exhyphenpenalty" 50)
  (plain-count "\\pretolerance" 100)
  (plain-count "\\tolerance" 200)
  (plain-count "\\hbadness" 1000)
  (plain-count "\\widowpenalty" 150)
  (plain-count "\\showboxdepth" 3)
  (plain-count "\\outputpenalty" 0)
  (plain-count "\\globaldefs" 0)
  (plain-count "\\mag" 1000)
  (plain-count "\\tracingcommands" 0)
  (plain-count "\\tracingmacros" 0)
  (plain-count "\\tracingonline" 0)
  (plain-count "\\shellescape" 1)
  (plain-count "\\suppressfontnotfounderror" 1)
  ;
  (plain-dimen "\\TIIPhsize" 0)
  (plain-dimen "\\hsize" (tex-length 6.5 :in))
  (plain-dimen "\\vsize" (tex-length 8.9 :in))
  (plain-dimen "\\maxdepth" (tex-length 4 :pt))
  (plain-dimen "\\delimitershortfall" (tex-length 5 :pt))
  (plain-dimen "\\nulldelimiterspace" (tex-length 1.2 :pt))
  (plain-dimen "\\scriptspace" (tex-length 0.5 :pt))
  (plain-dimen "\\hoffset" 0)
  (plain-dimen "\\voffset" 0)
  (plain-dimen "\\epsfxsize" 0)
  (plain-dimen "\\epsfysize" 0)
  (plain-dimen "\\emergencystretch" 0)
  (plain-dimen "\\hfuzz" (tex-length 0.1 :pt))
  (plain-dimen "\\vfuzz" (tex-length 0.1 :pt))
  (plain-dimen "\\textwidth" (tex-length 6.5 :in))
  (plain-dimen "\\smallskipamount" (tex-length 3 :pt))
  (plain-dimen "\\medskipamount" (tex-length 6 :pt))
  (plain-dimen "\\bigskipamount" (tex-length 12 :pt))
  (plain-dimen "\\lastskip" 0)
  (plain-dimen "\\baselineskip" (tex-length 12 :pt))
  (plain-dimen "\\overfullrule" (tex-length 5 :pt))
  (plain-dimen "\\parindent" (tex-length 20 :pt))
  (plain-dimen "\\leftskip" 0)
  (plain-dimen "\\parfillskip" 0)
  (plain-dimen "\\parskip" 0)
  (plain-dimen "\\abovedisplayskip" (tex-length 12 :pt))
  (plain-dimen "\\belowdisplayskip" (tex-length 12 :pt))

  (plain-toks "\\everypar" "")
  (plain-toks "\\headline" "")
  (plain-toks "\\footline" "\\folio")

  (tex-def-dotted-count "figure" nil)
  (tex-def-dotted-count "table" nil)
  (tex-def-dotted-count "equation" nil)

  (tex-gdef-0arg "\\TIIPcurrentnodename" "no value yet")
  (tex-gdef-0arg "\\@currentlabel" "no value yet")
  (tex-gdef-0arg "\\TZPcolophontimestamp" "1") ;obsolete
  (tex-gdef-0arg "\\TZPcolophoncredit" "1") ;obsolete
  (tex-gdef-0arg "\\TZPcolophonweblink" "1") ;obsolete
  (tex-gdef-0arg "\\TZPimageformat" "PNG")
  (tex-gdef-0arg "\\TZPimageconverter" "NetPBM")
  (tex-gdef-0arg "\\TZPredirectseconds" "0")
  (tex-gdef-0arg "\\TZPtextext" "1")
  (tex-gdef-0arg "\\TZPraggedright" "1")
  (tex-gdef-0arg "\\TZPlang" "en")
  (tex-gdef-0arg "\\TZPcommonlisp" (if 'nil "0" "1"))

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

;;

(defun do-number ()
  (emit (get-number)))

(defun do-magnification ()
  (plain-count "\\mag" (get-number) nil))

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
    (cond ((setq *it* (find-dimendef ctlseq))
           (scaled-point-to-tex-point (find-dimen *it*)))
          ((setq *it* (get-number-corresp-to-ctl-seq ctlseq))
           *it*)
          ((setq *it* (find-toksdef ctlseq)) (find-toks *it*))
          (t (trace-if nil "expand-the failed")))))

(defun do-the ()
  ;almost like do-number
  (let ((ctlseq (get-ctl-seq)))
    (ignorespaces)
    (cond ((string= ctlseq "\\count")
           (emit (find-count (get-number))))
          ((setq *it* (get-number-corresp-to-ctl-seq ctlseq))
           (emit *it*))
          ((setq *it* (find-toksdef ctlseq))
           (tex2page-string (find-toks *it*)))
          (t (trace-if nil "do-the failed")))))

(defun do-arabic ()
  (let* ((counter-name (ungroup (get-group)))
         (counter (gethash counter-name *dotted-counters*))
         it)
    (cond ((setq it (counter*-value counter)) (emit it))
          (t (trace-if nil "do-arabic failed")))))

(defun find-corresp-prim (ctlseq)
  ;is this really necessary?  Why not make resolve-defs take over this
  ;too?
  (let ((y (find-def ctlseq)))
    (or (and y (tdef*-defer y)) ctlseq)))

(defun find-corresp-prim-thunk (ctlseq)
  (let ((y (find-def ctlseq)))
    (if (and y (tdef*-thunk y)) (tdef*-prim y) ctlseq)))

(defun globally-p () (> (get-gcount "\\globaldefs") 0))

(defun do-let (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
  (ignorespaces)
  (let* ((lhs (get-ctl-seq))
         (rhs (progn (get-equal-sign) (get-raw-token/is))))
    (unless (inside-false-world-p)
      (let ((frame (cond (globalp
                           (mapc (lambda (fr)
                                   (remhash lhs (texframe*-definitions fr)))
                                 *tex-env*)
                           *global-texframe*)
                         (t nil))))
        (tex-let-general lhs rhs frame)))))

;(trace do-let)

(defun do-def (&optional globalp expandp)
  (unless globalp (setq globalp (globally-p)))
  (unless (inside-false-world-p)
    (let ((lhs (get-raw-token/is)))
      (when (and (ctl-seq-p lhs) (string= lhs "\\TIIPcsname"))
        (setq lhs (get-peeled-group)))
      (let* ((argpat (get-def-parameters))
             (rhs (ungroup (get-group))))
        (when expandp (setq rhs (expand-edef-macro rhs)))
        (let ((frame (cond (globalp
                             (mapc (lambda (fr)
                                     (remhash lhs
                                              (if (ctl-seq-p lhs)
                                                  (texframe*-definitions fr)
                                                  (texframe*-cdefinitions fr))))
                                   *tex-env*)
                             *global-texframe*)
                           (t nil))))
          (cond ((ctl-seq-p lhs) (tex-def lhs argpat rhs nil nil nil nil frame))
                (t (tex-def-char (char lhs 0) argpat rhs frame))))))))

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
      (trace-if (> (the-count "\\tracingcommands") 0)
                lhs " already defined"))))

(defun do-newcounter ()
  (let* ((counter-name (ungroup (get-group)))
         (within (get-bracketed-text-if-any)))
    (tex-def-dotted-count counter-name within)))

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

;(trace do-newenvironment)

(defun tex-def-dotted-count (counter-name sec-num)
  (declare (string counter-name))
  (when sec-num
    (unless (gethash sec-num *section-counter-dependencies*)
      (setf (gethash sec-num *section-counter-dependencies*) '()))
    (push counter-name
          (gethash sec-num *section-counter-dependencies*)))
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

;(trace do-begin)

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

;(trace do-end)

(defun latex-argnum-to-plain-argpat (n)
  (declare (number n))
  (let ((n n) (s '()))
    (loop
      (when (<= n 0) (return s))
      (decf n)
      (push (code-char (+ *int-corresp-to-0* n)) s)
      (push #\# s))))

(defun make-reusable-img (&optional globalp)
  (unless globalp (setq globalp (globally-p)))
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
          (read-char i nil))
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
    (do-img-src img-file)
    (emit "\"")
    (emit " style=\"border: 0\"")
    (emit " alt=\"")
    (cond (alt (emit alt)) (t (emit "[") (emit img-file) (emit "]")))
    (emit "\">")
    (write-log #\))
    (write-log :separation-space)
    t))

(defun reuse-img ()
  (source-img-file (ungroup (get-group))))

(defun get-def-parameters ()
  (let ((params '()) c)
    (loop
      (setq c (snoop-actual-char))
      (when (not c)
        (terror 'get-def-parameters "Runaway definition?"))
      (cond ((= (catcode c) **escape**)
             (let ((x (get-ctl-seq)))
               (if (string= x "\\par")
                   ;save \par as :par?
                   (progn (push #\newline params)
                          (push #\newline params))
                   (setq params (append (nreverse (concatenate 'list x)) params)))))
            ((= (catcode c) **bgroup**) (return))
            (t (cond ((char= c #\newline)
                      ;kludge for writing Texinfo-type
                      ;macros. Should really be equivalent
                      ;to any white space
                      (get-actual-char) (ignorespaces))
                     ((char-whitespace-p c)
                      (ignorespaces) (setq c #\space))
                     (t (get-actual-char)))
               (push c params))))
    (nreverse params)))

;(trace get-def-parameters)

(defun get-till-char (c0)
  (declare (character c0))
  (concatenate
   'string
   (nreverse
    (let ((s '()) (nesting 0) (escape-p nil))
      (loop
        (let ((c (snoop-actual-char)))
          (when (not c)
            (terror 'get-till-char "File ended too soon"))
          (cond (escape-p
                 (push (get-actual-char) s)
                 (setq escape-p nil))
                ((char= c c0) (return s))
                ((= (catcode c) **escape**)
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
                      (not (char= c0 #\newline))
                      (char-whitespace-p c0))
                 (return s))
                (t (push (get-actual-char) s)))))))))

(defun digit-to-int (d)
  (declare (character d))
  (- (char-code d) *int-corresp-to-0*))

(defun do-halign ()
  (do-end-para)
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (or (not c)
              (not (char= c #\{)))
      (terror 'do-halign "Missing {")))
  (let ((*tabular-stack* (cons :halign *tabular-stack*)))
    (bgroup)
    (emit "<table>")
    (let ((tmplt (get-halign-template)))
      (loop
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (when (not c)
            (terror 'do-halign "Eof inside \\halign"))
          (cond ((char= c #\}) (get-actual-char) (emit "</table>")
                               (emit-newline)
                 (egroup) (do-para) (return))
                (t (expand-halign-line tmplt))))))))

(defun get-halign-template ()
  (let ((s '()))
    (loop
      (let ((x (get-raw-token)))
        (when (not x)
          (terror 'get-halign-template "Eof in \\halign"))
        (cond ((string= x "\\cr")  (push nil s)
               (return (nreverse s)))
              ((string= x "#") (push t s))
              ((string= x "&") (push nil s))
              (t (push x s)))))))

(defun expand-halign-line (tmplt)
  (declare (list tmplt))
  (emit "<tr>")
  (let ((tmplt tmplt) (ins " ") (outer-loop-done nil))
    (loop
      (when outer-loop-done (return))
      (let ((x (get-raw-token)))
        (when (not x)
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
                                         (emit-newline)
                                         (setq outer-loop-done t)
                                         (return))
                                  (progn (pop tmplt)
                                         (setq ins " ") ; or ""?
                                         (return))))
                       ((t)
                        (pop tmplt)
                        (setq r (concatenate 'string r ins)))
                       (t (pop tmplt)
                          (setq r (concatenate 'string r y))))))))
              (t (setq ins
                       (concatenate 'string ins x))))))))

(defun do-settabs ()
  (let ((settabs-spec ""))
    (loop
      (let ((x (get-token/ps)))
        (when (not x)
          (terror 'do-settabs "Eof in \\settabs"))
        (cond ((string= x "\\columns")
               (tex2page-string
                 (concatenate 'string "\\TIIPsettabscolumns=" settabs-spec))
               (return))
              ((string= x "\\cr")
               (plain-count "\\TIIPsettabscolumns" 0 nil)
               (return))
              (t (setq settabs-spec (concatenate 'string settabs-spec x))))))))

(defun do-tabalign ()
  (emit-newline)
  (let ((table-width "")
        (num-cols (the-count "\\TIIPsettabscolumns")))
    (when (> num-cols 0)
      (setq table-width " width=100%"))
    (emit "<table") (emit table-width) (emit ">") (emit-newline)
    (loop
      (do-tabalign-row num-cols)
      (let ((x (get-token/ps)))
        (cond ((not x) (return))
              ((or (string= x "\\+") (string= x "\\tabalign")) t)
              (t (toss-back-string x)
                 (return))))))
  (emit "</table>") (emit-newline))

(defun do-tabalign-row (num-cols)
  (declare (number num-cols))
  (emit "<tr>") (emit-newline)
  (let ((cell-contents "")
        (cell-width ""))
    (when (> num-cols 0)
      (setq cell-width
            (concatenate 'string " width="
              (write-to-string (/ 100.0 num-cols)) "%")))
    (loop
      (let ((x (get-token/ps)))
        (when (not x)
          (terror 'do-tablign "Eof in \\tabalign"))
        (cond ((or (string= x "&") (string= x "\\cr"))
               (emit "<td")
               (emit cell-width)
               (emit ">")
               (bgroup)
               (tex2page-string cell-contents)
               (egroup)
               (setq cell-contents "")
               (emit "</td>") (emit-newline)
               (when (string= x "\\cr") (return)))
              (t (setq cell-contents (concatenate 'string
                                       cell-contents x)))))))
  (emit "</tr>") (emit-newline))

(defun read-till-next-sharp (k argpat)
  ;simplify simplify
  (ignorespaces)
  (let ((n (list-length argpat)) (ss '()) i s c (outer-loop-done nil))
    (loop
      (when outer-loop-done
        (return (values i (concatenate 'string (nreverse ss)))))
      (setq i k
            s '())
      (loop
        (setq c (if (< i n) (elt argpat i) #\#))
        (when (char= c #\#)
          (setq outer-loop-done t)
          (return))
        (let ((d (snoop-actual-char)))
          ;case c = #\,  d = #\space
          (cond ((and (char= c #\space) (char-whitespace-p d))
                 (ignorespaces)
                 (incf i) (push c s))
                ((= (catcode d) **comment**)
                 (do-comment))
                ((and (char= c #\newline)
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
                                    (not (= (catcode (car ss)) **escape**))))
                           (append (get-group-as-reversed-chars)
                                   ss)
                           (progn
                             (get-actual-char)
                             (when (and (char-whitespace-p d)
                                        (not (char= d #\newline)))
                               (ignorespaces))
                             (cons d ss))))
                 (return))
                (t (setq ss (append s ss))
                   (return))))))))

(defun read-macro-args (argpat k r)
  (declare (list argpat r) (fixnum k))
  (let ((n (list-length argpat)))
    (nreverse
     (let ((k k) (r r))
       (loop
         (when (>= k n)
           (when (= k 0) (ignorespaces :stop-after-first-newline))
           (return r))
         (let ((c (elt argpat k)))
           ;eql rather than char= because \par may show up as :par?
           (cond ((= (catcode c) **parameter**)
                  ;should check #n are in order!
                  (cond ((= k (1- n))
                         (ignorespaces)
                         (push (get-till-char #\{) r)
                         (return r))
                        ((= k (- n 2))
                         (push (ungroup (get-token)) r)
                         (return r))
                        (t
                          (let ((c2 (elt argpat (+ k 2))))
                             (if (= (catcode c2) **parameter**)
                                 (progn (incf k 2)
                                        (push (ungroup (get-token)) r))
                                 (multiple-value-bind (k2 s)
                                   (read-till-next-sharp (+ k 2) argpat)
                                   (setq k k2)
                                   (push s r)))))))
                 (t (let ((d (get-actual-char)))
                      (when (not d)
                        (terror 'read-macro-args "Eof before macro got enough args"))
                      (cond ((char= c #\space)
                             ;but if parameter is \ctlseq, argument space
                             ;be ignored. not yet
                             (unless (char-whitespace-p d)
                               (terror 'read-macro-args
                                       "Use of macro doesn't match its definition.")))
                            ((char= c d) t)
                            (t (terror 'read-macro-args
                                       "Use of macro doesn't match its definition.")))
                      (incf k))))))))))

;(trace read-macro-args)

(defun expand-edef-macro (rhs)
  (declare (string rhs))
  (let* ((*not-processing-p* t)
         (tmp-stream (make-string-output-stream)))
    (call-with-input-string/buffered rhs
      (lambda ()
        (loop
          (let ((c (snoop-actual-char)))
            (when (not c) (return))
            (princ (cond ((= (catcode c) **escape**)
                          (let ((x (get-ctl-seq)))
                            (toss-back-char *invisible-space*)
                            (cond ((or (string= x "\\the") (string= x "\\number"))
                                   (let ((x2 (get-raw-token/is)))
                                     (toss-back-char *invisible-space*)
                                     (toss-back-string x2)
                                     (cond ((ctl-seq-p x2)
                                            (cond ((string= x "\\the") (expand-the))
                                                  ((string= x "\\number") (get-number))
                                                  (t #xdeadc0de)))
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
                   tmp-stream)))))
    (get-output-stream-string tmp-stream)))

(defun expand-tex-macro (optarg argpat rhs lexical-catcodes)
  (declare (list argpat) (string rhs))
  (let* ((k 0)
         (r (if (not optarg) '()
              (progn
               (setq k 2)
               (list
                (cond ((setq *it* (get-bracketed-text-if-any)) *it*)
                      (t optarg))))))
         (args (read-macro-args argpat k r))
         (rhs-n (length rhs))
         (*catcodes* lexical-catcodes))
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
                                      ((and (char= c #\#) (> (list-length s) 1))
                                       (return
                                        (append
                                         (nreverse s)
                                         (cons #\ (aux j)))))
                                      ((= (list-length s) 1)
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
                                          (<= (digit-to-int n) (list-length args)))
                                     (append
                                      (concatenate 'list
                                                   (elt args
                                                        (1- (digit-to-int n))))
                                      (aux (+ k 2))))
                                    (t (cons #\# (aux (1+ k))))))))
                         (t (cons c (aux (1+ k)))))))))
       (aux 0)))))

;(trace expand-tex-macro)

(defun do-verbatimescapechar ()
  (ignorespaces)
  (let* ((c1 (get-actual-char))
         (c2 (get-actual-char)))
    (unless (= (catcode c1) **escape**)
      (terror 'do-verbatimescapechar "Arg must be \\<char>"))
    (setq *esc-char-verb* c2)))

(defun do-verb-braced (ign)
  (declare (ignore ign))
  (let ((*catcodes* *catcodes*)
        (nesting 0))
    (catcode #\\ 12) (catcode *esc-char-verb* 0)
    (loop
      (let ((c (get-actual-char)))
        (when (not c)
          (terror 'do-verb-braced "Eof inside verbatim"))
        (cond ((= (catcode c) **escape**) (toss-back-char c)
               (let ((x (let ((*not-processing-p* t))
                          (get-ctl-seq))))
                 (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                        (emit (char x 1)))
                       (t (let ((*catcodes* *catcodes*))
                            (catcode #\\ 0)
                            (do-tex-ctl-seq-completely x))))))
              ((char= c #\{)
               (emit #\{) (incf nesting))
              ((char= c #\})
               (when (= nesting 0) (return))
               (emit #\}) (decf nesting))
              ((char= c #\space)
               (emit (if *verb-visible-space-p* *verbatim-visible-space* #\space)))
              ((char= c #\newline)
               (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                     (*verb-visible-space-p* (emit *verbatim-visible-space*))
                     (t (emit-newline))))
              ((and (char= c #\-) (not *verb-display-p*))
               ;in-text verbatim should not break on hyphen
               (emit "&#x2011;"))
              (t (emit-html-char c)))))))

(defun do-verb-delimed (d)
  (declare (character d))
  (loop
    (let ((c (get-actual-char)))
      (when (not c)
        (terror 'do-verb-delimed "Eof inside verbatim"))
      (cond ((char= c d) (return))
            ((char= c #\space)
             (emit
             (if *verb-visible-space-p* *verbatim-visible-space* #\space)))
            ((char= c #\newline)
             (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
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
  (let* ((f0 (get-filename-possibly-braced))
         (f (find-tex-file f0)))
    (cond ((and f (probe-file f))
           (do-end-para) (bgroup) (emit "<pre class=verbatim>")
           (with-open-file (p f :direction :input)
             (loop
               (let ((c (read-char p nil)))
                 (when (not c) (return))
                 (emit-html-char c))))
           (emit "</pre>") (egroup) (do-para))
          (t (non-fatal-error "File " f0 " not found")))))

(defun get-char-definitely (c0)
  (declare (character c0))
  (ignorespaces)
  (let ((c (get-actual-char)))
    (when (not c) (terror 'get-char-defnitely "Runaway argument"))
    (unless (char= c c0) (terror 'get-char-defnitely "Missing" c0))))

(defun get-char-optionally (cc)
  (declare (list cc))
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((not c) nil)
          ((member c cc :test #'char=) (get-actual-char) c)
          (t nil))))

(defun get-unsigned-number-optionally ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (cond ((not c) nil)
          ((digit-char-p c) (get-integer 10))
          (t nil))))

(defun opmac-verbinput-skip-lines (i n)
  (declare (stream i) (fixnum n))
  (dotimes (_ n)
    (let ((x (read-line i nil)))
      (when (not x)
        (terror 'do-opmac-verbinput "\\verbinput file ended too soon")))))

(defun opmac-verbinput-print-lines (i n)
  (declare (stream i))
  (if (eq n t)
      (loop
        (let ((x (read-line i nil)))
          (when (not x) (return))
          (emit-html-string x)
          (emit-newline)))
      (dotimes (_ n)
        (let ((x (read-line i nil)))
          (when (not x)
            (terror 'do-opmac-verbinput "\\verbinput file ended too soon"))
          (emit-html-string x)
          (emit-newline)))))

(defun do-opmac-verbinput ()
  (let* ((s1 (progn (get-char-definitely #\() (get-char-optionally '(#\+ #\-))))
         (n1 (get-unsigned-number-optionally))
         (s2 (get-char-optionally '(#\+ #\-)))
         (n2 (get-unsigned-number-optionally))
         (f0 (progn (get-char-definitely #\)) (get-filename)))
         (f  (find-tex-file f0))
         (n (and f
                 (let ((n (gethash f *opmac-verbinput-table*)))
                   (or n
                       (progn (setf (gethash f *opmac-verbinput-table*) 0)
                              0))))))
    (cond ((and f (probe-file f))
           (do-end-para)
           (bgroup)
           (emit "<pre class=verbatim>")
           (with-open-file (i f :direction :input)
             (cond ((and s1 n1 s2 n2 (char= s1 #\-) (char= s2 #\+))
                    ;skip n1 after current point, print n2
                    (opmac-verbinput-skip-lines i (+ n n1))
                    (opmac-verbinput-print-lines i n2)
                    (incf n (+ n1 n2)))
                   ;
                   ((and (not s1) n1 s2 n2 (char= s2 #\-))
                    ;print lines n1 thru n2
                    (opmac-verbinput-skip-lines i (1- n1))
                    (opmac-verbinput-print-lines i (1+ (- n2 n1)))
                    (setq n n2))
                   ;
                   ((and (not s1) n1 s2 n2 (char= s2 #\+))
                    ;print n2 lines starting at line n1
                    (opmac-verbinput-skip-lines i (1- n1))
                    (opmac-verbinput-print-lines i n2)
                    (setq n (+ (1- n1) n2)))
                   ;
                   ((and s1 n1 (not s2) (not n2) (char= s1 #\-))
                    ;print lines 1 thru n1
                    (opmac-verbinput-print-lines i n1)
                    (setq n n1))
                   ;
                   ((and s1 n1 (not s2) (not n2) (char= s1 #\+))
                    ;print n1 lines following current point
                    (opmac-verbinput-skip-lines i n)
                    (opmac-verbinput-print-lines i n1)
                    (incf n1))
                   ;
                   ((and (not s1) n1 s2 (not n2) (char= s2 #\-))
                    ;print from line n1 to eof
                    (opmac-verbinput-skip-lines i (1- n1))
                    (opmac-verbinput-print-lines i t)
                    (setq n 0))
                   ;
                   ((and s1 (not n1) (not s2) (not n2) (char= s1 #\+))
                    ;print from current point to eof
                    (opmac-verbinput-skip-lines i n)
                    (opmac-verbinput-print-lines i t)
                    (setq n 0))
                   ;
                   ((and s1 (not n1) (not s2) (not n2) (char= s1 #\-))
                    ;print entire file
                    (opmac-verbinput-print-lines i t)
                    (setq n 0))
                   ;
                   (t (terror 'do-opmac-verbinput "Malformed \\verbinput" s1 n1 s2 n2 f))))
           (setf (gethash f *opmac-verbinput-table*) n)
           (emit "</pre>")
           (egroup)
           (do-para))
          (t (non-fatal-error "File " f0 " not found")))))

(defun do-verbwritefile ()
  (let* ((f (get-filename-possibly-braced))
         (e (file-extension f)))
    (unless e (setq e ".tex") (setq f (concatenate 'string f e)))
    (when *verb-stream* (close *verb-stream*))
    (push f *verb-written-files*)
    (when (string-equal e ".mp") (push f *mp-files*))
    (setq *verb-stream* (open f :direction :output :if-exists :supersede))))

(defun verb-ensure-output-stream ()
  (unless *verb-stream*
    (let ((output-file (concatenate 'string *jobname* ".txt")))
      (setq *verb-stream* (open output-file :direction :output
                              :if-exists :supersede)))))

(defun dump-groupoid (p)
  (declare (stream p))
  (ignorespaces)
  (let ((write-char #'write-char) (d (get-actual-char)))
    (unless p
      (setq write-char (lambda (x y)
                         (declare (ignore x y))
                         nil)))
    (case d
      (#\{ (let ((nesting 0) c)
             (loop
               (setq c (get-actual-char))
               (when (not c)
                 (terror 'dump-groupoid "Eof inside verbatim"))
               (cond ((= (catcode c) **escape**)
                      (funcall write-char c p)
                      (funcall write-char (get-actual-char) p))
                     ((char= c #\{)
                      (funcall write-char c p)
                      (incf nesting))
                     ((char= c #\})
                      (cond ((= nesting 0) (return))
                            (t (funcall write-char c p)
                               (decf nesting))))
                     (t (funcall write-char c p))))))
      (t (loop
           (let ((c (get-actual-char)))
             (when (not c)
               (terror 'dump-groupoid "Eof inside verbatim"))
             (when (char= c d) (return))
             (funcall write-char c p)))))))

(defun do-makehtmlimage ()
  (ignorespaces)
  (unless (char= (snoop-actual-char) #\{)
    (terror 'do-makehtmlimage "\\makehtmlimage's argument must be a group"))
  (call-with-html-image-stream #'dump-groupoid))

(defun do-verbwrite ()
  (verb-ensure-output-stream)
  (dump-groupoid *verb-stream*))

(defun do-string ()
  (let ((c (snoop-actual-char)))
    (cond ((not c) nil)
          ((= (catcode c) **escape**) (get-actual-char)
           (toss-back-char *invisible-space*)
           (toss-back-string "\\TIIPbackslash"))
          ((= (catcode c) **comment**) (eat-till-eol) (do-string))
          (t (toss-back-char (get-actual-char))))))

(defun do-verbatim-latex (env)
  (declare (string env))
  (do-end-para)
  (bgroup)
  (emit "<pre class=verbatim>")
  (let ((*verb-visible-space-p* (eat-star)))
    (when *verb-visible-space-p* (setq env (concatenate 'string env "*")))
    (munched-a-newline-p)
    (let ((*ligatures-p* nil) c)
      (loop
        (setq c (snoop-actual-char))
        (when (not c) (terror 'do-verbatim-latex "Eof inside verbatim"))
        (cond ((char= c #\\)
               (let ((cs (get-ctl-seq)))
                 (if (string= cs "\\end")
                     (cond ((setq *it* (get-grouped-environment-name-if-any))
                            (let ((e *it*))
                              (cond ((string= *it* env) (return))
                                    (t (emit-html-string cs)
                                       (emit-html-char #\{)
                                       (emit-html-string e)
                                       (emit-html-char #\})))))
                           (t (emit-html-string cs)))
                     (progn (emit-html-string cs)))))
              ((char= c #\space)
               (get-actual-char)
               (emit (if *verb-visible-space-p* *verbatim-visible-space*
                         #\space)))
              (t (emit-html-char (get-actual-char)))))))
  (emit "</pre>")
  (egroup)
  (do-para))

(defun do-endverbatim-eplain ()
  (setq *inside-eplain-verbatim-p* nil))

(defun do-alltt ()
  (do-end-para)
  (bgroup)
  (emit "<pre class=verbatim>")
  (munched-a-newline-p)
  (let ((*in-alltt-p* t) c)
    (loop
      (setq c (snoop-actual-char))
      (when (not c) (terror 'do-alltt "Eof inside alltt"))
      (case c
        (#\\ (do-tex-ctl-seq (get-ctl-seq)))
        (#\{ (get-actual-char) (bgroup))
        (#\} (get-actual-char) (egroup))
        (t (emit-html-char (get-actual-char))))
      (unless *in-alltt-p* (return)))))

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
        (ignorespaces :all)
        (when (not (snoop-actual-char)) (return))
        (setf (gethash (scm-get-token) *scm-special-symbols*) nil)))))

(defun scm-emit-html-char (c)
  (unless (not c)
    (when *scm-dribbling-p* (write-char c *verb-stream*))
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
  (let ((c (let ((*catcodes* *catcodes*))
             (catcode #\\ 11) (catcode *esc-char-verb* 11)
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
      (let ((*catcodes* *catcodes*))
        (catcode #\\ 11) (catcode *esc-char-verb* 11)
        (tex2page-string (concatenate 'string "$" math-text "$")))
      (emit "</span>"))))

(defun scm-output-slatex-comment ()
  (let ((s (get-line)))
    (emit "<span class=comment>")
    (when *scm-dribbling-p* (princ s *verb-stream*) (terpri *verb-stream*))
    (let ((*catcodes* *catcodes*))
      (catcode #\\ 0) (catcode *esc-char-verb* 11)
      (tex2page-string s))
    (do-end-para)
    (emit "</span>")
    (toss-back-char #\newline)))

(defun scm-output-verbatim-comment ()
  (emit "<span class=comment>")
  (loop
    (let ((c (get-actual-char)))
      (when (or (not c) (char= c #\newline)) (emit "</span>")
        (scm-emit-html-char c) (return))
      (cond ((and (char-whitespace-p c)
                  (let ((c2 (snoop-actual-char)))
                    (or (not c2) (char= c2 #\newline))))
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
      (cond ((not c) (return))
            ((char= c #\|)
             (let ((c2 (snoop-actual-char)))
               (cond ((not c2) (scm-emit-html-char c)
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
    (cond ((not c) (emit "<span class=selfeval>")
           (scm-emit-html-char #\#) (emit "</span>"))
          ((char= c #\|) (scm-output-extended-comment))
          (t (toss-back-char #\#) (scm-output-token (scm-get-token))))))

(defun scm-output-token (s)
  (declare (string s))
  (case (scm-get-type s)
    (:special-symbol
     (let ((*catcodes* *catcodes*))
       (catcode #\\ 0)
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
  (declare (string s))
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
    (let ((*catcodes* *catcodes*)
          (*verb-display-p* display-p)
          (nesting 0)
          c)
      (catcode #\\ 12) (catcode *esc-char-verb* 0)
      (loop
        (setq c (snoop-actual-char))
        (when (not c)
          (terror 'do-scm-braced "Eof inside verbatim"))
        (cond ((= (catcode c) **escape**)
               (let ((x (get-ctl-seq)))
                 (cond ((member x '("\\ " "\\{" "\\}") :test #'string=)
                        (scm-emit-html-char (char x 1)))
                       (t (let ((*catcodes* *catcodes*))
                            (catcode #\\ 0)
                            (do-tex-ctl-seq-completely x))))))
              ((char= c #\{)
               (get-actual-char)
               (scm-emit-html-char c)
               (incf nesting))
              ((char= c #\})
               (get-actual-char)
               (case nesting
                 (0 (return))
                 (t (scm-emit-html-char c)
                    (decf nesting))))
              (t (scm-output-next-chunk)))))
    (egroup)
    (cond ((not display-p) (emit "</code>"))
          (t (emit "</pre>")
             (do-noindent)))))

(defun do-scm-delimed (result-p)
  (let* ((d (get-actual-char))
         (display-p (munched-a-newline-p)))
    (cond ((not display-p) (emit "<code class=scheme")
           (when result-p (emit "response")) (emit ">"))
          (t (do-end-para) (emit "<pre class=scheme>")))
    (let ((*verb-display-p* display-p)
          (*scm-token-delims* (cons d *scm-token-delims*))
          c)
      (loop
        (setq c (snoop-actual-char))
        (when (not c)
          (terror 'do-scm-delimed "Eof inside verbatim"))
        (when (char= c d) (get-actual-char) (return))
        (scm-output-next-chunk)))
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
             (get-filename-possibly-braced)))
        c)
    (call-with-input-file/buffered f
      (lambda ()
        (loop
          (setq c (snoop-actual-char))
          (when (not c) (return))
          (scm-output-next-chunk)))))
  (emit "</pre>")
  (egroup)
  (do-noindent))

(defun do-scmdribble ()
  (verb-ensure-output-stream)
  (let ((*scm-dribbling-p* t))
    (do-scm nil))
  (terpri *verb-stream*))

(defun string-is-all-dots-p (s)
  (declare (string s))
  (dotimes (i (length s) t)
    (unless (char= (char s i) #\.) (return nil))))

(defun string-is-flanked-by-stars-p (s)
  (declare (string s))
  (let ((n (length s)))
    (and (>= n 3)
         (char= (char s 0) (char s (1- n)) #\*))))

(defun scm-get-type (s)
  (declare (string s))
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
    (if (and (not (not c)) (char= c #\*)) (get-actual-char) nil)))

(defun do-cr (z)
  (declare (string z))
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
     (emit "<tr><td class=rightline>"))
    (:eqnarray
     (emit "</td>")
     (cond (*equation-numbered-p* (emit "<td>(") (emit *equation-number*)
                                  (bump-dotted-counter "equation") (emit ")</td>"))
           (t (setq *equation-numbered-p* t)))
     (emit "</tr>")
     (emit-newline)
     (setq *equation-position* 0)
     (emit "<tr><td class=rightline>"))
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
       (emit "<tr><td class=centerline>")
       (setq *equation-position* 0)
       (emit-newline)))
    (:header (emit #\space))
    (t (when (and (eql *tex-format* :latex) (string= z "\\\\"))
         (get-bracketed-text-if-any)
         (let ((c (snoop-actual-char)))
           (when (and (not (not c)) (char= c #\*))
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
    (when (char= c #\\)
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

(defun do-tex-case-code (kase)
  (declare (keyword kase))
  (unless (inside-false-world-p)
    (let* ((c1 (get-tex-char-spec))
           (c2 (progn (get-equal-sign) (get-tex-char-spec)))
           (fr (top-texframe)))
      (setf (gethash c1 (funcall (case kase
                                   (:uccode #'texframe*-uccodes)
                                   (:lccode #'texframe*-lccodes))
                                 fr))
            c2))))

(defun tex-char-downcase (c)
  (declare (character c))
  (or (some (lambda (fr) (gethash c (texframe*-lccodes fr))) *tex-env*)
      (char-downcase c)))

(defun tex-char-upcase (c)
  (declare (character c))
  (or (some (lambda (fr) (gethash c (texframe*-uccodes fr))) *tex-env*)
      (char-upcase c)))

(defun do-flipcase (kase)
  (declare (keyword kase))
  (emit
    (map 'string (case kase
                   (:uccode #'tex-char-upcase)
                   (:lccode #'tex-char-downcase))
         (get-token))))

(defun do-addtocounter ()
  (let* ((counter-name (get-peeled-group))
         (new-value (string-to-number (get-token-or-peeled-group))))
    (set-latex-counter-aux counter-name t new-value)))

(defun do-setcounter ()
  (let* ((counter-name (get-peeled-group))
         (new-value (string-to-number (get-token-or-peeled-group))))
    (set-latex-counter-aux counter-name nil new-value)))

(defun do-stepcounter ()
  (let* ((counter-name (get-peeled-group)))
    (set-latex-counter-aux counter-name t 1)))

(defun set-latex-counter-aux (counter-name addp new-value)
  (declare (string counter-name) (number new-value))
  (cond ((setq *it* (gethash counter-name *dotted-counters*))
         (let ((counter *it*))
           (if addp
               (incf (counter*-value counter) new-value)
               (setf (counter*-value counter) new-value))))
        (t
          (let ((count-seq (concatenate 'string "\\" counter-name)))
            (cond ((setq *it* (section-ctl-seq-p count-seq))
                   (let ((n *it*))
                     (setf (gethash n *section-counters*)
                           (if addp
                               (+ new-value (gethash n *section-counters* 0))
                               new-value))
                     #|
                     (if addp
                         (incf (gethash n *section-counters* 0) new-value)
                         (setf (gethash n *section-counters*) new-value))
                     |#))
                  ((find-count count-seq)
                   ;typically \secnumdepth, \tocdepth
                   (tex-gdef-count count-seq
                                   (if addp
                                       (+ new-value (get-gcount count-seq))
                                       new-value)))
                  (t ;error?
                    nil))))))

(defun do-tex-prim (z)
  (let (y)
    (cond ((setq *it* (find-def z))
           (setq y *it*)
           (cond ((setq *it* (tdef*-defer y))
                  (setq y *it*)
                  (toss-back-string y))
                 ((setq *it* (tdef*-thunk y))
                  (setq y *it*)
                  (funcall y))
                 (t (expand-tex-macro
                      (tdef*-optarg y)
                      (tdef*-argpat y)
                      (tdef*-expansion y)
                      (tdef*-catcodes y)))))
          (*math-mode-p*
            (do-math-ctl-seq z))
          (t (trace-if (> (the-count "\\tracingcommands") 0)
                       "Ignoring " z)))))

;(trace do-tex-prim)

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
    (t (concatenate 'string "&#x"
                    (write-to-string
                      (+ #x1d538 (- (char-int c) (char-int #\A))) :base 16)
                    ";"))))

(defun tex-math-cal (c)
  (concatenate 'string
               "&#x"
               (write-to-string
                 (+ #x1d4d0 (- (char-int c) (char-int #\A))) :base 16)
               ";"))

(defun tex-math-frak (c)
  (concatenate 'string
               "&#x"
               (write-to-string
                 (if (upper-case-p c)
                   (+ #x1d56c (- (char-int c) (char-int #\A)))
                   (+ #x1d586 (- (char-int c) (char-int #\a)))) :base 16)
               ";"))

(defun emit-math-alpha-char (c)
  (case *math-font*
    ((:rm) (emit c))
    ((:bf) (emit "<b>") (emit c) (emit "</b>"))
    ((:bb) (emit (if (upper-case-p c) (tex-math-bb c) c)))
    ((:cal) (emit (if (upper-case-p c) (tex-math-cal c) c)))
    ((:frak) (emit (if (alpha-char-p c) (tex-math-frak c) c)))
    (t (emit "<em>") (emit c) (emit "</em>"))))

(defun do-tex-char (c)
  (cond ((= (catcode c) **comment**) (do-comment))
        ((inside-false-world-p) t)
        ((= (catcode c) **space**) (emit-space c))
        ((char= c #\{) (bgroup))
        ((char= c #\}) (egroup))
        ((= (catcode c) **math**) (do-math))
        ((char= c #\-) (do-hyphen))
        ((char= c #\`) (do-lsquo))
        ((char= c #\') (do-rsquo))
        ((char= c #\~) (emit-nbsp 1))
        ((char= c #\!) (do-excl)) ((char= c #\?) (do-quest))
        ((or (char= c #\<) (char= c #\>) (char= c #\")) (emit-html-char c))
        ((= (catcode c) **alignment**)
         (cond (*tabular-stack*
                ;(do-end-para) ;??? ;must check why this is needed
                (case (car *tabular-stack*)
                  ((:pmatrix :eqalign :displaylines :mathbox)
                   (emit "&#xa0;</td><td class=centerline>&#xa0;"))
                  (:eqalignno
                   (setq *equation-position* (1+ *equation-position*))
                   (emit "</td><td")
                   (when (= *equation-position* 2) (emit " width=30% class=rightline"))
                   (emit ">"))
                  ((:eqnarray :eqnarray*)
                   (setq *equation-position* (1+ *equation-position*))
                   (emit "</td><td")
                   (when (= *equation-position* 1) (emit " class=centerline width=2%"))
                   (emit ">"))
                  (:tabular (do-tabular-colsep))
                  (:ruled-table (do-ruledtable-colsep))))
               (t (emit-html-char c))))
        ((char= c #\|)
         (if (eq (car *tabular-stack*) :ruled-table)
             (do-ruledtable-colsep) (emit c)))
        ((char= c #\newline) (do-newline))
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

;(trace do-tex-char)

(defun do-tex-ctl-seq-completely (x)
  (declare (string x))
  (cond ((setq *it* (resolve-defs x))
         (tex2page-string *it*))
        ((setq *it* (do-tex-prim (find-corresp-prim x)))
         (when (eq *it* :encountered-undefined-command)
           (emit x)))))

(defun inside-false-world-p ()
  (or (member nil *tex-if-stack*) (member '? *tex-if-stack*)))

(defun do-tex-ctl-seq (z)
  (declare (string z))
  ;(format t "do-tex-ctl-seq ~s~%" z)
  ;process ctl seq z.  Return :encountered-bye if z is \bye;
  ;:encountered-endinput if z is \endinput
  (trace-if (> (the-count "\\tracingmacros") 0) z)
  (cond ((setq *it* (resolve-defs z))
         ;macro body could contain \fi
         (let ((s *it*))
           (trace-if (> (the-count "\\tracingmacros") 0)
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
        ((member z '("\\endinput" "\\TIIPendinput") :test #'string=)
         (let ((next-token (get-token)))
           (when (and next-token
                      (string= next-token "\\fi"))
             (do-fi)))
         :encountered-endinput)
        ((setq *it* (find-countdef z)) (do-count= *it* nil))
        ((setq *it* (find-dimendef z)) (do-dimen= *it* nil))
        ((setq *it* (find-toksdef z)) (do-toks= *it* nil))
        (t (do-tex-prim z))))

;(trace do-tex-ctl-seq)

(defun generate-html ()
  (let ((*outer-p* t))
    (loop
      (let ((c (snoop-actual-char)))
        (cond ((not c) (return t))
              ((setq *it* (resolve-cdefs c))
               (let ((s *it*))
                 (toss-back-char *invisible-space*)
                 (toss-back-string s)))
              ((= (catcode c) **escape**)
               (case (do-tex-ctl-seq (get-ctl-seq))
                 (:encountered-endinput (return t))
                 (:encountered-bye (return :encountered-bye))
                 (t t)))
              (t (get-actual-char)
                 (do-tex-char c)))))))

(defun check-input-file-timestamp-p (f)
  (declare (string f))
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

(defun tex2page-string (s)
  (declare (string s))
  (call-with-input-string/buffered s #'generate-html))

(defun tex2page-file (f)
  (write-log #\()
  (write-log f)
  (write-log :separation-space)
  (trace-if (> (the-count "\\tracingcommands") 0) "Inputting file " f)
  (prog1
      (call-with-input-file/buffered f #'generate-html)
    (write-log #\))
    (write-log :separation-space)))

(defun tex2page-file-if-exists (f)
  (when (probe-file f) (tex2page-file f)))

(defun ignorable-tex-file-p (f)
  (let ((e (or (file-extension f) "")))
    (cond ((string-equal e ".sty") t)
          (t (when (string-equal e ".tex")
               (setq f (subseq f 0 (- (length f) 4))))
             (cond ((string= f "opmac")
                    (tex-gdef-0arg "\\TZPopmac" "1")
                    t)
                   (t (member f *tex-files-to-ignore* :test #'string-equal)))))))

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
      (cond ((ignorable-tex-file-p f)
             ;dont process .sty files and macro files like btxmac.tex
             nil)
            ((member f '("miniltx" "miniltx.tex") :test #'string-equal)
             ;like above, but miniltx also makes @ a "letter"
             (catcode #\@ 11) nil)
            ((member f '("texinfo" "texinfo.tex") :test #'string-equal)
             (cond ((setq *it* (actual-tex-filename "texi2p"))
                    (tex2page-file *it*))
                   (t (terror 'do-input "File texi2p.tex not found"))))
            ((setq *it* (actual-tex-filename f (check-input-file-timestamp-p f)))
             (tex2page-file *it*))
            (t (write-log #\() (write-log f)
               (write-log :separation-space) (write-log "not found)")
               (write-log :separation-space))))))

(defun do-includeonly ()
  (ignorespaces)
  (when (eq *includeonly-list* t) (setq *includeonly-list* '()))
  (let ((c (get-actual-char)))
    (when (or (not c) (not (char= c #\{)))
      (terror 'do-includeonly)))
  (let ((*filename-delims* (cons #\} (cons #\, *filename-delims*))))
    (loop
      (ignorespaces)
      (let ((c (snoop-actual-char)))
        (when (not c) (terror 'do-includeonly))
        (cond ((= (catcode c) **comment**) (eat-till-eol))
              ((char= c #\,) (get-actual-char))
              ((char= c #\}) (get-actual-char) (return))
              ((member c  *filename-delims* :test #'char=)
               (terror 'do-includeonly))
              (t (push (get-filename)
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

;

(defun eval-for-tex-only ()
  (setq *eval-for-tex-only-p* t)
  (do-end-page)
  (ensure-file-deleted *html-page*) ;??
  (setq *main-tex-file* nil)
  (setq *html-page* ".eval4texignore")
  (setq *html*
        (make-ostream* :stream
                     (open *html-page* :direction :output
                           :if-exists :supersede))))

(defun expand-ctl-seq-into-string (cs)
    (let ((*html* (make-html-output-stream)))
      (do-tex-ctl-seq cs))
    (html-output-stream-to-string *html*))

(defun call-with-html-output-going-to (p th)
  (let ((*html* (make-ostream* :stream p)))
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
               ((member :fresh-index *missing-pieces*)
                ;wait to run makeindex
                nil)
               ((member :index *missing-pieces*) t)
               (*source-changed-since-last-run-p*
                (flag-missing-piece :fresh-index) t)
               (t nil))))
    ;bibtex
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
    ;makeindex
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
    ;eval4tex
    (load (concatenate 'string *jobname* *eval4tex-file-suffix*) :if-does-not-exist nil)
    ;metapost
    (mapc
     (lambda (f)
         (when (probe-file f)
           (write-log :separation-newline)
           (write-log "Running: metapost ")
           (write-log f)
           (write-log :separation-newline)
           (call-mp f)))
     *mp-files*)
    ;eps files
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
                       (let ((c (char home (1- n))))
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
  (setq *basic-style* "body {
        color: black;
        background-color: white;
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

        @media amzn-kf8 {
        body {
        text-align: left;
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
        margin-bottom: 0;
        }

        pre {
        overflow: auto;
        margin-left: 2em;
        /* background-color: hsl(0,0%,96%); */ /* Scheme version uncomment? */
        }

        blockquote {
        /* background-color: hsl(0,35%,91%); */
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

        .item {
        font-style: oblique;
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

        ul {
        list-style-type: disc;
        }

        ul ul {
        list-style-type: circle;
        }

        ul ul ul {
        list-style-type: square;
        }

        ul ul ul ul {
        list-style-type: circle;
        }

        .verbatim {
        background-color: hsl(0,0%,96%);
        }

        .scheme em {
        color: black;
        font-family: serif;
        }

        /* scheme background punctuation was hsl(0,50%,40%) */

        .scheme             {color: hsl(280,33%,30%)} /* background punctuation */
        .scheme  .selfeval  {color: hsl(120,100%,20%); font-style: normal}
        .scheme  .keyword   {color: hsl(0,100%,20%);   font-style: normal; font-weight: bold}
        .scheme  .builtin   {color: hsl(0,100%,20%);   font-style: normal}
        .scheme  .global    {color: hsl(300,100%,20%); font-style: normal}
        .scheme  .variable  {color: hsl(240,100%,20%); font-style: normal}
        .scheme  .comment   {color: hsl(180,100%,20%); font-style: oblique}

        .schemeresponse {
        color: hsl(120,100%,20%);
        }

        .navigation {
        color: hsl(20,100%,30%);
        text-align: right;
        font-size: medium;
        font-style: italic;
        }

        @media print {
        .navigation {
        display: none;
        }
        }

        .centerline {
        text-align: center;
        }

        .leftline {
        text-align: left;
        }

        .rightline {
        text-align: right;
        }

        sup {
        font-size: 61%; /* otherwise footnote numbers are horrible */
        }

        .bibitem {
        vertical-align: top;
        }

        table.mathdelim > td,th {
        padding: 0;
        }

        table.mathdelim {
        border-spacing: 0;
        }

        .disable {
        /* color: hsl(0,0%,90%); */
        color: hsl(0,0%,50%);
        }

        .smallcaps {
        font-size: 75%;
        }

        .footnotemark {
        background-color: hsl(60,80%,74%);
        }

        .footnote {
        font-size: 90%;
        }

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
        color: hsl(0,0%,50%);
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
        color: hsl(0,0%,50%);
        text-decoration: none;
        }

        .slide h1.title {
        font-weight: bold;
        text-align: left;
        }

        .slide h2.section {
        margin-left: 0pt;
        }
        ")
          (let ((css-file (concatenate 'string *aux-dir/* *jobname* *css-file-suffix*)))
            (setq *css-stream* (open css-file :direction :output
                                     :if-exists :supersede))
            (unless (or (tex2page-flag-boolean "\\TIIPsinglepage")
                        (tex2page-flag-boolean "\\TZPsinglepage"))
              (princ *basic-style* *css-stream*))))

(defun load-aux-file ()
  (let ((label-file
         (concatenate 'string *aux-dir/* *jobname* *label-file-suffix*)))
    (when (probe-file label-file)
      (load-tex2page-data-file label-file)
      (delete-file label-file)))
  (unless (string= *jobname* "texput")
    (let ((texput-aux (concatenate 'string "texput" *aux-file-suffix*)))
      (when (probe-file texput-aux) (delete-file texput-aux))))
  (let ((aux-file
         (concatenate 'string *aux-dir/* *jobname* *aux-file-suffix*)))
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
      (when (and (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
                     (tex2page-flag-boolean "\\TZPcolophontimestamp"))
                 (not (tex2page-flag-boolean "\\TZPcolophonlastpage"))
                 (> *html-page-count* 1))
        ;i.e. time to print mod-time is past, and this update is too
        ;late
        (flag-missing-piece :last-modification-time)))))

(defun probably-latex ()
  (when (null *tex-env*)
    (incf *latex-probability* 1)
    (when (>= *latex-probability* 2) (definitely-latex))))

(let ((already-noted-p nil))
  (defun definitely-latex ()
    (unless already-noted-p
      (setq already-noted-p t)
      (!definitely-latex)
      (write-aux '(!definitely-latex)))))

;the following are used to make entries in the aux file.  Their names
;all begin with "!"

(defun !tex-like-layout ()
  (tex-def-0arg "\\TIIPtexlayout" "1"))

(defun !head-line (e) (tex-def-toksdef "\\headline" nil e t))

(defun !foot-line (e) (tex-def-toksdef "\\footline" nil e t))

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

(defun !single-page ()
  (tex-def-0arg "\\TIIPsinglepage" "1"))

(defun !slides ()
  (tex-def-0arg "\\TIIPslides" "1"))

(defun !script (jsf)
  (cond ((or (fully-qualified-url-p jsf)
             (probe-file (ensure-url-reachable jsf)))
         (push jsf *scripts*))
        (t (write-log "! Can't find script ")
           (write-log jsf)
           (write-log :separation-newline))))

(defun !using-chapters () (setq *using-chapters-p* t))

(defun !definitely-latex ()
  (setq *tex-format* :latex)
  (when (< (get-gcount "\\secnumdepth") -1)
    (tex-gdef-count "\\secnumdepth" 3)))

(defun !using-external-program (x)
  ;obsolete
  (declare (ignore x))
  nil)

(defun !external-labels (f)
  ;obsolete
  (declare (ignore f))
  nil)

(defun !doctype (d) (setq *doctype* d))

(defun !lang (lang)
  (tex-gdef-0arg "\\TZPlang" lang))

(defun !colophon (x)
  (case x
    (:last-page
      (tex-def-0arg "\\TZPcolophonlastpage" "1"))
    (:no-timestamp
      (tex-def-0arg "\\TZPcolophondisabletimestamp" "1")
      (tex-def-0arg "\\TZPcolophontimestamp" "0"))
    (:dont-credit-tex2page
      (tex-def-0arg "\\TZPcolophondisablecredit" "1")
      (tex-def-0arg "\\TZPcolophoncredit" "0"))
    (:dont-link-to-tex2page-website
      (tex-def-0arg "\\TZPcolophondisableweblink" "1")
      (tex-def-0arg "\\TZPcolophonweblink" "0"))))

(defun !tex-text (n)
  (when (= n 0)
    (setq *ligatures-p* nil)))

(defun !opmac-iis (lhs sub)
  (unless *opmac-index-sub-table*
    (setq *opmac-index-sub-table* (make-hash-table :test #'equal)))
  (setf (gethash lhs *opmac-index-sub-table*) sub))

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
  (cond ((or (fully-qualified-url-p css)
             (probe-file (ensure-url-reachable css)))
         (push css *stylesheets*))
        (t (write-log "! Can't find stylesheet ")
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
      (let ((*current-source-file* f) (*input-line-no* 0)
                                      e directive)
        (loop
          (setq e (read i nil nil))
          (unless e (return))
          (setq directive (car e))
          (incf *input-line-no*)
          (apply
            (case directive
              (!colophon #'!colophon)
              (!default-title #'!default-title)
              (!definitely-latex #'!definitely-latex)
              (!doctype #'!doctype)
              (!external-labels #'!external-labels)
              (!foot-line #'!foot-line)
              (!head-line #'!head-line)
              (!html-head #'!html-head)
              (!html-redirect #'!html-redirect)
              (!index #'!index)
              (!index-page #'!index-page)
              (!infructuous-calls-to-tex2page #'!infructuous-calls-to-tex2page)
              (!label #'!label)
              (!lang #'!lang)
              (!last-modification-time #'!last-modification-time)
              (!last-page-number #'!last-page-number)
              (!opmac-iis #'!opmac-iis)
              (!preferred-title #'!preferred-title)
              (!script #'!script)
              (!single-page #'!single-page)
              (!slides #'!slides)
              (!stylesheet #'!stylesheet)
              (!tex-like-layout #'!tex-like-layout)
              (!tex-text #'!tex-text)
              (!toc-entry #'!toc-entry)
              (!toc-page #'!toc-page)
              (!using-chapters #'!using-chapters)
              (!using-external-program #'!using-external-program)
              (t (terror 'load-tex2page-data-file
                         "Fatal aux file error " directive "; I'm stymied.")))
            (cdr e)))))))

(defun tex2page-help (not-a-file)
  (unless not-a-file (setq not-a-file "--missing-arg"))
  (write-aux
   `(!infructuous-calls-to-tex2page ,(1+ *infructuous-calls-to-tex2page*)))
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
         (write-log #\newline) (write-log *tex2page-website*) (write-log #\.)
         (write-log #\newline) (write-log #\newline))
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
         (write-log #\newline) (write-log *tex2page-website*) (write-log #\.)
         (write-log #\newline) (write-log #\newline))
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
  (close-all-open-streams))

(defun non-fatal-error (&rest ss)
  (emit-link-start (concatenate 'string *jobname* ".hlog"))
  ;x2692 won't print on lynx
  (emit "<span style=\"color: red\">&#x2388;&#xa0;")
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
  (declare (string cs) (function thunk))
  (tex-def cs '() nil nil thunk cs nil *math-primitive-texframe*))

(defun tex-defsym-math-prim (cs str)
  (declare (string cs str))
  (tex-def cs '() nil nil (lambda () (emit str)) cs nil *math-primitive-texframe*))

(defun make-reusable-math-image-as-needed (cs &rest expn)
  ;obsolete?
  (let ((expn (if (null expn) cs (car expn))))
    (tex-def-math-prim cs
     (lambda ()
         (tex2page-string
          (concatenate 'string "\\global\\imgdef" cs "{$" expn "$}"))
         (tex2page-string cs)))))

;;TeX primitives

(tex-def-math-prim "\\eqno" #'do-eqno)
(tex-def-math-prim "\\left" #'do-math-left)
(tex-def-math-prim "\\noalign" #'do-noalign)
(tex-def-math-prim "\\over" #'do-over); (lambda () (emit "/")))
(tex-def-math-prim "\\right" #'do-math-right)
(tex-def-prim "\\ " #'do-actual-space)
(tex-def-prim "\\advance" #'do-advance)
(tex-def-prim "\\afterassignment" #'do-afterassignment)
(tex-def-prim "\\aftergroup" #'do-aftergroup)
(tex-def-prim "\\box" #'do-box)
(tex-def-prim "\\catcode" #'do-catcode)
(tex-def-prim "\\char" #'do-char)
(tex-def-prim "\\chardef" #'do-chardef)
(tex-def-prim "\\closein" (lambda () (do-close-stream :in)))
(tex-def-prim "\\closeout" (lambda () (do-close-stream :out)))
(tex-def-prim "\\copy" (lambda () (do-box t)))
(tex-def-prim "\\count" #'do-count)
(tex-def-prim "\\countdef" #'do-countdef)
(tex-def-prim "\\cr" (lambda () (do-cr "\\cr")))
(tex-def-prim "\\csname" #'do-csname)
(tex-def-prim "\\def" #'do-def)
(tex-def-prim "\\dimen" #'do-dimen)
(tex-def-prim "\\dimendef" #'do-dimendef)
(tex-def-prim "\\discretionary" #'do-discretionary)
(tex-def-prim "\\divide" #'do-divide)
(tex-def-prim "\\edef" (lambda () (do-def nil t)))
(tex-def-prim "\\else" #'do-else)
(tex-def-prim "\\end" #'do-end)
(tex-def-prim "\\endinput" #'do-endinput)
(tex-def-prim "\\errmessage" #'do-errmessage)
(tex-def-prim "\\expandafter" #'do-expandafter)
(tex-def-prim "\\fi" #'do-fi)
(tex-def-prim "\\font" #'do-font)
(tex-def-prim "\\fontdimen" #'do-fontdimen)
(tex-def-prim "\\futurelet" #'do-futurelet)
(tex-def-prim "\\gdef" (lambda () (do-def t nil)))
(tex-def-prim "\\global" #'do-global)
(tex-def-prim "\\halign" #'do-halign)
(tex-def-prim "\\hbox" #'do-hbox)
(tex-def-prim "\\hfill" (lambda () (emit-nbsp 5)))
(tex-def-prim "\\hrule" #'do-hrule)
(tex-def-prim "\\hskip" #'do-hskip)
(tex-def-prim "\\hyphenchar" (lambda () (get-token) (eat-integer)))
(tex-def-prim "\\if" #'do-if)
(tex-def-prim "\\ifcase" #'do-ifcase)
(tex-def-prim "\\ifdim" #'do-iffalse)
(tex-def-prim "\\ifeof" #'do-ifeof)
(tex-def-prim "\\iffalse" #'do-iffalse)
(tex-def-prim "\\ifhmode" #'do-iftrue)
(tex-def-prim "\\ifmmode" #'do-ifmmode)
(tex-def-prim "\\ifnum" #'do-ifnum)
(tex-def-prim "\\ifodd" #'do-ifodd)
(tex-def-prim "\\iftrue" #'do-iftrue)
(tex-def-prim "\\ifx" #'do-ifx)
(tex-def-prim "\\ignorespaces" #'ignorespaces)
(tex-def-prim "\\indent" #'do-indent)
(tex-def-prim "\\input" #'do-input)
(tex-def-prim "\\jobname" (lambda () (tex2page-string *jobname*)))
(tex-def-prim "\\kern" #'do-kern)
(tex-def-prim "\\lccode" (lambda () (do-tex-case-code :lccode)))
(tex-def-prim "\\let" #'do-let)
(tex-def-prim "\\lower" #'do-lower)
(tex-def-prim "\\lowercase" (lambda () (do-flipcase :lccode)))
(tex-def-prim "\\message" #'do-message)
(tex-def-prim "\\multiply" #'do-multiply)
(tex-def-prim "\\noindent" #'do-noindent)
(tex-def-prim "\\number" #'do-number)
(tex-def-prim "\\openin" (lambda () (do-open-stream :in)))
(tex-def-prim "\\openout" (lambda () (do-open-stream :out)))
(tex-def-prim "\\par" #'do-para)
(tex-def-prim "\\raise" (lambda () (do-lower t)))
(tex-def-prim "\\read" #'do-read)
(tex-def-prim "\\relax" #'do-relax)
(tex-def-prim "\\romannumeral" #'do-romannumeral)
(tex-def-prim "\\setbox" #'do-setbox)
(tex-def-prim "\\string" #'do-string)
(tex-def-prim "\\the" #'do-the)
(tex-def-prim "\\toks" #'do-toks)
(tex-def-prim "\\toksdef" #'do-toksdef)
(tex-def-prim "\\uccode" (lambda () (do-tex-case-code :uccode)))
(tex-def-prim "\\underline" (lambda () (do-function "\\underline")))
(tex-def-prim "\\unkern" #'do-unskip)
(tex-def-prim "\\unskip" #'do-unskip)
(tex-def-prim "\\uppercase" (lambda () (do-flipcase :uccode)))
(tex-def-prim "\\vbox" #'do-hbox)
(tex-def-prim "\\vskip" #'do-vskip)
(tex-def-prim "\\vtop" #'do-hbox)
(tex-def-prim "\\write" #'do-write)
(tex-def-prim "\\xdef" (lambda () (do-def t t)))
(tex-def-prim-0arg "\\begingroup" "{")
(tex-def-prim-0arg "\\endgroup" "}")
(tex-let-prim "\\-" "\\TIIPrelax")
(tex-let-prim "\\/" "\\TIIPrelax")
(tex-let-prim "\\displaystyle" "\\TIIPrelax")
(tex-let-prim "\\leqno" "\\eqno")
(tex-let-prim "\\textstyle" "\\TIIPrelax")

;;app B. plain TeX

;sec B.1. code tables

(tex-let-prim "\\@ne" (string (code-char 1)))
(tex-let-prim "\\tw@" (string (code-char 2)))
(tex-let-prim "\\thr@@" (string (code-char 3)))
(tex-let-prim "\\sixt@@n" (string (code-char 16)))
(tex-let-prim "\\@cclv" (string (code-char 255)))
(tex-let-prim "\\@cclvi" (string (code-char 256)))
(tex-let-prim "\\@m" (string (code-char 1000)))
(tex-let-prim "\\@M" (string (code-char 10000)))
(tex-let-prim "\\@MM" (string (code-char 20000)))

;sec B.2, allocation of registers

(tex-def-prim "\\wlog" #'do-wlog)

(tex-def-prim "\\newcount" #'do-newcount)
(tex-def-prim "\\newdimen" #'do-newdimen)
(tex-def-prim "\\newbox" #'do-newbox)
(tex-def-prim "\\newtoks" #'do-newtoks)
(tex-def-prim "\\newread" #'do-newread)
(tex-def-prim "\\newwrite" #'do-newwrite)

(tex-def-prim "\\newif" #'do-newif)

;sec B.4, font info

(tex-def-prim-0arg "\\magstephalf" "1095")
(tex-def-prim "\\magstep" #'do-magstep)

(tex-def-prim "\\sevenrm" (lambda () (do-switch :sevenrm)))
(tex-def-prim "\\fiverm" (lambda () (do-switch :fiverm)))

(tex-def-prim "\\rm" (lambda () (when *math-mode-p* (do-switch :rm))))
(tex-def-prim "\\oldstyle" (lambda () (do-switch :oldstyle)))
(tex-def-prim "\\cal" (lambda () (do-switch :cal)))
(tex-def-prim "\\it" (lambda () (do-switch :it)))
(tex-def-prim "\\sl" (lambda () (do-switch :sl)))
(tex-def-prim "\\bf" (lambda () (do-switch :bf)))
(tex-def-prim "\\tt" (lambda () (do-switch :tt)))

;sec B.5, macros for text

(tex-defsym-math-prim "\\lbrack" "[")
(tex-defsym-math-prim "\\rbrack" "]")
(tex-let-prim "\\endgraf" "\\par")
;(tex-def-prim "\\endgraf" #'do-para)
(tex-let-prim "\\space" "\\ ")
(tex-def-prim-0arg "\\empty" "") ;for \ifx comparisons ""
(tex-def-prim-0arg "\\bgroup" "{")
(tex-def-prim-0arg "\\egroup" "}")

;actually TeX defines \[be]group in terms of \{begin,end}group

(tex-def-prim "\\obeyspaces" #'do-obeyspaces)
(tex-def-prim "\\obeylines" #'do-obeylines)

;loop/repeat

(tex-def-pat-prim "\\loop" "#1\\repeat" "\\def\\body{#1}\\iterate")
(tex-def-pat-prim "\\iterate" "" "\\body\\let\\next\\iterate\\else\\let\\next\\relax\\fi\\next")
(tex-let-prim "\\repeat" "\\fi")

;

(tex-def-prim "\\enskip" (lambda () (emit-space (kern ".5em"))))
(tex-def-prim "\\quad" (lambda () (emit-space (kern "1em"))))
(tex-def-prim "\\qquad" (lambda () (emit-space (kern "2em"))))
(tex-let-prim "\\enspace" "\\enskip")
(tex-def-prim "\\thinspace" (lambda () (emit-space (kern ".16667em"))))
(tex-def-prim "\\negthinspace" (lambda () (emit-space (kern "-.16667em"))))

(tex-def-prim "\\smallskip" (lambda () (do-bigskip :smallskip)))
(tex-def-prim "\\medskip" (lambda () (do-bigskip :medskip)))
(tex-def-prim "\\bigskip" (lambda () (do-bigskip :bigskip)))

(tex-defsym-prim "\\break" "<br>")

(tex-def-prim "\\eject" #'do-eject)
(tex-let-prim "\\supereject" "\\eject")

(tex-def-prim "\\smallbreak" (lambda () (do-bigskip :smallskip)))
(tex-def-prim "\\medbreak" (lambda () (do-bigskip :medskip)))
(tex-def-prim "\\bigbreak" (lambda () (do-bigskip :bigskip)))

(tex-def-prim "\\leftline" (lambda () (do-function "\\leftline")))
(tex-def-prim "\\rightline" (lambda () (do-function "\\rightline")))
(tex-def-prim "\\centerline" (lambda () (do-function "\\centerline")))

(tex-def-prim "\\llap" #'do-llap)

(tex-def-prim "\\settabs" #'do-settabs)
(tex-def-prim "\\tabalign" #'do-tabalign)
(tex-let-prim "\\+" "\\tabalign")

(tex-def-prim "\\item" #'do-item)
(tex-def-prim "\\itemitem" (lambda () (do-plain-item 2)))
(tex-def-prim "\\textindent" #'do-textindent)
(tex-def-prim "\\narrower" (lambda () (do-switch :narrower)))

(tex-def-prim "\\beginsection" #'do-beginsection)

(tex-def-prim "\\proclaim" #'do-proclaim)

(tex-let-prim "\\raggedright" "\\TIIPrelax")
(tex-let-prim "\\ttraggedright" "\\tt")

(tex-defsym-prim "\\%" "%")
(tex-defsym-prim "\\&" "&#x26;")
(tex-defsym-prim "\\#" "#")
(tex-defsym-prim "\\$" "$")
(tex-defsym-prim "\\ss" "&#xdf;")
(tex-defsym-prim "\\ae" "&#xe6;")
(tex-defsym-prim "\\oe" "&#x153;")
(tex-defsym-prim "\\o" "&#xf8;")
(tex-defsym-prim "\\AE" "&#xc6;")
(tex-defsym-prim "\\OE" "&#x152;")
(tex-defsym-prim "\\O" "&#xd8;")
(tex-defsym-prim "\\i" "&#x131;")
(tex-defsym-prim "\\j" "&#x237;")
(tex-defsym-prim "\\aa" "&#xe5;")
(tex-defsym-prim "\\l" "&#x142;")

(tex-let-prim "\\leavevmode" "\\TIIPrelax")
(tex-defsym-prim "\\_" "_")
(tex-defsym-prim "\\L" "&#x141;")
(tex-defsym-prim "\\AA" "&#xc5;")

(tex-defsym-prim "\\dag" "&#x2020;")
(tex-defsym-prim "\\ddag" "&#x2021;")
(tex-defsym-prim "\\S" "&#xa7;")
(tex-defsym-prim "\\P" "&#xb6;")

(tex-def-prim "\\d" (lambda () (do-diacritic :dotunder)))
(tex-def-prim "\\b" (lambda () (do-diacritic :barunder)))
(tex-def-prim "\\c" (lambda () (do-diacritic :cedilla)))
(tex-defsym-prim "\\copyright" "&#xa9;")

(tex-defsym-prim "\\dots" "&#x2026;")
(tex-def-prim "\\TeX" (lambda () (emit *tex-logo*)))

(tex-def-prim "\\`" (lambda () (do-diacritic :grave)))
(tex-def-prim "\\'" (lambda () (do-diacritic :acute)))
(tex-def-prim "\\v" (lambda () (do-diacritic :hacek)))
(tex-def-prim "\\u" (lambda () (do-diacritic :breve)))
(tex-def-prim "\\=" #'do-backslash-equal)
(tex-def-prim "\\^" (lambda () (do-diacritic :circumflex)))
(tex-def-prim "\\." (lambda () (do-diacritic :dot)))
(tex-def-prim "\\H" (lambda () (do-diacritic :hungarianumlaut)))
(tex-def-prim "\\~" (lambda () (do-diacritic :tilde)))
(tex-def-prim "\\\"" (lambda () (do-diacritic :umlaut)))
(tex-def-prim "\\t" (lambda () (do-diacritic :tieafter)))

;sec B.6, macros for math

(tex-def-math-prim "\\," (lambda () (emit-space (kern ".16667em"))))
(tex-def-math-prim "\\!" (lambda () (emit-space (kern "-.16667em"))))
(tex-def-math-prim "\\>" (lambda () (emit-space (kern ".22222em"))))
(tex-def-math-prim "\\;" (lambda () (emit-space (kern ".27778em"))))

;sec F.1. lowercase Greek

(tex-defsym-math-prim "\\alpha" "&#x3b1;")
(tex-defsym-math-prim "\\beta" "&#x3b2;")
(tex-defsym-math-prim "\\gamma" "&#x3b3;")
(tex-defsym-math-prim "\\delta" "&#x3b4;")
(tex-defsym-math-prim "\\epsilon" "&#x3b5;")
(tex-defsym-math-prim "\\varepsilon" "&#x3b5;")
(tex-defsym-math-prim "\\zeta" "&#x3b6;")
(tex-defsym-math-prim "\\eta" "&#x3b7;")
(tex-defsym-math-prim "\\theta" "&#x3b8;")
(tex-defsym-math-prim "\\vartheta" "&#x3d1;")
(tex-defsym-math-prim "\\iota" "&#x3b9;")
(tex-defsym-math-prim "\\kappa" "&#x3ba;")
(tex-defsym-math-prim "\\lambda" "&#x3bb;")
(tex-defsym-math-prim "\\mu" "&#x3bc;")
(tex-defsym-math-prim "\\nu" "&#x3bd;")
(tex-defsym-math-prim "\\xi" "&#x3be;")
(tex-defsym-math-prim "\\omicron" "&#x3bf;")
(tex-defsym-math-prim "\\pi" "&#x3c0;")
(tex-defsym-math-prim "\\varpi" "&#x3d6;")
(tex-defsym-math-prim "\\rho" "&#x3c1;")
(tex-defsym-math-prim "\\varrho" "&#x3f1;")
(tex-defsym-math-prim "\\sigma" "&#x3c3;")
(tex-defsym-math-prim "\\varsigma" "&#x3c2;")
(tex-defsym-math-prim "\\tau" "&#x3c4;")
(tex-defsym-math-prim "\\upsilon" "&#x3c5;")
(tex-defsym-math-prim "\\phi" "&#x3c6;")
(tex-defsym-math-prim "\\varphi" "&#x3d5;")
(tex-defsym-math-prim "\\chi" "&#x3c7;")
(tex-defsym-math-prim "\\psi" "&#x3c8;")
(tex-defsym-math-prim "\\omega" "&#x3c9;")

;sec F.2. uppercase Greek

(tex-defsym-math-prim "\\Gamma" "&#x393;")
(tex-defsym-math-prim "\\Delta" "&#x394;")
(tex-defsym-math-prim "\\Theta" "&#x398;")
(tex-defsym-math-prim "\\Lambda" "&#x39b;")
(tex-defsym-math-prim "\\Xi" "&#x39e;")
(tex-defsym-math-prim "\\Pi" "&#x3a0;")
(tex-defsym-math-prim "\\Sigma" "&#x3a3;")
(tex-defsym-math-prim "\\Upsilon" "&#x3a5;")
(tex-defsym-math-prim "\\Phi" "&#x3a6;")
(tex-defsym-math-prim "\\Psi" "&#x3a8;")
(tex-defsym-math-prim "\\Omega" "&#x3a9;")

;sec F.4. misc symbols of type Ord

(tex-defsym-math-prim "\\aleph" "&#x2135;")
(tex-defsym-math-prim "\\hbar" "&#x210f;")
;(tex-defsym-math-prim "\\imath" "<i>&#x131;</i>")
;(tex-defsym-math-prim "\\jmath" "<i>&#x237;</i>")
(tex-defsym-math-prim "\\imath" "&#x1d6a4;")
(tex-defsym-math-prim "\\jmath" "&#x1d6a5")
(tex-defsym-math-prim "\\ell" "&#x2113;")
(tex-defsym-math-prim "\\wp" "&#x2118;")
(tex-defsym-math-prim "\\Re" "&#x211c;")
(tex-defsym-math-prim "\\Im" "&#x2111;")
(tex-defsym-math-prim "\\partial" "&#x2202;")
(tex-defsym-math-prim "\\infty" "&#x221e;")
(tex-defsym-math-prim "\\prime" "&#x2044;") ;sic, not &#x2032;
(tex-defsym-math-prim "\\emptyset" "&#x2205;")
(tex-defsym-math-prim "\\nabla" "&#x2207;")
(tex-defsym-math-prim "\\surd" "&#x221a;")
(tex-defsym-math-prim "\\top" "&#x22a4;")
(tex-defsym-math-prim "\\bot" "&#x22a5;")
(tex-defsym-math-prim "\\angle" "&#x2220;")
(tex-defsym-math-prim "\\triangle" "&#x394;")
(tex-defsym-math-prim "\\forall" "&#x2200;")
(tex-defsym-math-prim "\\exists" "&#x2203;")
(tex-defsym-math-prim "\\neg" "&#xac;")
(tex-defsym-math-prim "\\flat" "&#x266d;")
(tex-defsym-math-prim "\\natural" "&#x266e;")
(tex-defsym-math-prim "\\sharp" "&#x266f;")
(tex-defsym-math-prim "\\clubsuit" "&#x2663;")
(tex-defsym-math-prim "\\diamondsuit" "&#x2662;")
(tex-defsym-math-prim "\\heartsuit" "&#x2661;")
(tex-defsym-math-prim "\\spadesuit" "&#x2660;")

;sec F.6. large operators

(tex-defsym-math-prim "\\sum" "&#x2211;")
(tex-defsym-math-prim "\\prod" "&#x220f;")
(tex-defsym-math-prim "\\coprod" "&#x2210;")
(tex-def-math-prim "\\int" #'do-integral) ;(lambda () (emit "&#x222b;")))
(tex-defsym-math-prim "\\oint" "&#x222e;")
(tex-defsym-math-prim "\\bigcap" "&#x2229;")
(tex-defsym-math-prim "\\bigcup" "&#x222a;")
(tex-defsym-math-prim "\\bigsqcup" "&#x2294;")
(tex-defsym-math-prim "\\bigvee" "&#x2228;")
(tex-defsym-math-prim "\\bigwedge" "&#x2227;")
(tex-defsym-math-prim "\\bigodot" "&#x2299;")
(tex-defsym-math-prim "\\bigotimes" "&#x2297;")
(tex-defsym-math-prim "\\bigoplus" "&#x2295;")
(tex-defsym-math-prim "\\biguplus" "&#x228e;")

;sec F.7. binary operations

(tex-defsym-math-prim "\\pm" "&#xb1;")
(tex-defsym-math-prim "\\mp" "&#x2213;")
(tex-defsym-math-prim "\\setminus" "&#x2216;")
(tex-defsym-math-prim "\\cdot" " &#xb7; ")
(tex-defsym-math-prim "\\times" "&#xd7;")
(tex-defsym-math-prim "\\ast" "&#x2217;")
(tex-defsym-math-prim "\\star" "&#x2605;")
(tex-defsym-math-prim "\\diamond" "&#x25c7;")
(tex-defsym-math-prim "\\circ" "&#x25cb;")
(tex-defsym-math-prim "\\bullet" "&#x2022;")
(tex-defsym-math-prim "\\div" "&#xf7;")
(tex-defsym-math-prim "\\cap" "&#x2229;")
(tex-defsym-math-prim "\\cup" "&#x222a;")
(tex-defsym-math-prim "\\uplus" "&#x2283;")
(tex-defsym-math-prim "\\sqcap" "&#x2293;")
(tex-defsym-math-prim "\\sqcup" "&#x2294;")
(tex-defsym-math-prim "\\triangleleft" "&#x22b2;")
(tex-defsym-math-prim "\\triangleright" "&#x22b3;")
(tex-defsym-math-prim "\\wr" "&#x2240;")
(tex-defsym-math-prim "\\vee" "&#x2228;")
(tex-defsym-math-prim "\\wedge" "&#x2227;")
(tex-defsym-math-prim "\\oplus" "&#x2295;")
(tex-defsym-math-prim "\\otimes" "&#x2297;")
(tex-defsym-math-prim "\\oslash" "&#x2298;")
(tex-defsym-math-prim "\\odot" "&#x2299;")
(tex-defsym-math-prim "\\dagger" "&#x2020;")
(tex-defsym-math-prim "\\ddagger" "&#x2021;")
(tex-defsym-math-prim "\\amalg" "&#x2210;")

;sec F.8. relations

(tex-defsym-math-prim "\\leq" "&#x2264;")
(tex-defsym-math-prim "\\leqslant" "&#x2a7d;")
(tex-defsym-math-prim "\\prec" "&#x227a;")
(tex-defsym-math-prim "\\preceq" "&#x227c;")
(tex-defsym-math-prim "\\ll" "&#x226a;")
(tex-defsym-math-prim "\\subset" "&#x2282;")
(tex-defsym-math-prim "\\subseteq" "&#x2286;")
(tex-defsym-math-prim "\\sqsubseteq" "&#x2291;")
(tex-defsym-math-prim "\\in" "&#x2208;")
(tex-defsym-math-prim "\\vdash" "&#x22a2;")
(tex-defsym-math-prim "\\smile" "&#x2323;")
(tex-defsym-math-prim "\\frown" "&#x2322;")
(tex-defsym-math-prim "\\geq" "&#x2265;")
(tex-defsym-math-prim "\\geqslant" "&#x2a7e;")
(tex-defsym-math-prim "\\succ" "&#x227b;")
(tex-defsym-math-prim "\\succeq" "&#x227d;")
(tex-defsym-math-prim "\\gg" "&#x226b;")
(tex-defsym-math-prim "\\supset" "&#x2283;")
(tex-defsym-math-prim "\\supseteq" "&#x2287;")
(tex-defsym-math-prim "\\sqsupseteq" "&#x2292;")
(tex-defsym-math-prim "\\ni" "&#x220b;")
(tex-defsym-math-prim "\\dashv" "&#x22a3;")
(tex-defsym-math-prim "\\mid" "&#x2223;")
(tex-defsym-math-prim "\\parallel" "&#x2225;")
(tex-defsym-math-prim "\\equiv" "&#x2261;")
(tex-defsym-math-prim "\\sim" "&#x223c;")
(tex-defsym-math-prim "\\simeq" "&#x2243;")
(tex-defsym-math-prim "\\asymp" "&#x224d;")
(tex-defsym-math-prim "\\approx" "&#x2248;")
(tex-defsym-math-prim "\\bowtie" "&#x22c8;")
(tex-defsym-math-prim "\\propto" "&#x221d;")
(tex-defsym-math-prim "\\models" "&#x22a8;")
(tex-defsym-math-prim "\\propto" "&#x221d;")
(tex-defsym-math-prim "\\perp" "&#x22a5;")

;sec F.9. negated relations

(defun do-not ()
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (= (catcode c) **escape**)
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
                       (emit (cond ((char= c #\<) "&#x226e;")
                                   ((char= c #\>) "&#x226f;")
                                   ((char= c #\=) "&#x2260;"))))
        (t (emit "/"))))))

(tex-def-math-prim "\\not" #'do-not)
(tex-defsym-math-prim "\\neq" "&#x2260;")

;sec F.10. arrows

(tex-defsym-math-prim "\\leftarrow" "&#x2190;")
(tex-defsym-math-prim "\\Leftarrow" "&#x21d0;")
(tex-defsym-math-prim "\\rightarrow" "&#x2192;")
(tex-defsym-math-prim "\\Rightarrow" "&#x21d2;")
(tex-defsym-math-prim "\\leftrightarrow" "&#x2194;")
(tex-defsym-math-prim "\\Leftrightarrow" "&#x21d4;")
(tex-defsym-math-prim "\\mapsto" "&#x21a6;")
(tex-defsym-math-prim "\\hookleftarrow" "&#x21a9;")
(tex-defsym-math-prim "\\leftharpoonup" "&#x21bc;")
(tex-defsym-math-prim "\\leftharpoondown" "&#x21bd;")
(tex-defsym-math-prim "\\longleftarrow" "&#x27f5;")
(tex-defsym-math-prim "\\Longleftarrow" "&#x27f8;")
(tex-defsym-math-prim "\\longrightarrow" "&#x27f6;")
(tex-defsym-math-prim "\\Longrightarrow" "&#x27f9;")
(tex-defsym-math-prim "\\longleftrightarrow" "&#x27f7;")
(tex-defsym-math-prim "\\Longleftrightarrow" "&#x27fa;")
(tex-defsym-math-prim "\\longmapsto" "&#x27fc;")
(tex-defsym-math-prim "\\hookrightarrow" "&#x21aa;")
(tex-defsym-math-prim "\\rightharpoonup" "&#x21c0;")
(tex-defsym-math-prim "\\rightharpoondown" "&#x21c1;")
(tex-defsym-math-prim "\\nearrow" "&#x2197;")
(tex-defsym-math-prim "\\searrow" "&#x2198;")
(tex-defsym-math-prim "\\swarrow" "&#x2199;")
(tex-defsym-math-prim "\\nwarrow" "&#x2196;")

;sec F.13. punctuation

(tex-def-math-prim "\\colon" (lambda () (emit #\:)))
(tex-def-math-prim "\\ldotp" (lambda () (emit #\.)))
(tex-let-prim "\\cdotp" "\\cdot")

;

(tex-let-prim "\\ldots" "\\dots")
(tex-defsym-prim "\\cdots" "&#x22ef;")
(tex-defsym-prim "\\vdots" "&#x22ee;")
(tex-defsym-prim "\\ddots" "&#x22f1;")

;sec F.11 & 12. delimiters

(tex-defsym-math-prim "\\langle" "&#x27e8;")
(tex-defsym-math-prim "\\rangle" "&#x27e9;")
(tex-defsym-math-prim "\\lbrace" "{")
(tex-defsym-math-prim "\\rbrace" "}")
(tex-defsym-math-prim "\\lceil" "&#x2308;")
(tex-defsym-math-prim "\\rceil" "&#x2309;")
(tex-defsym-math-prim "\\lfloor" "&#x230a;")
(tex-defsym-math-prim "\\rfloor" "&#x230b;")
(tex-defsym-math-prim "\\uparrow" "&#x2191;")
(tex-defsym-math-prim "\\Uparrow" "&#x21d1;")
(tex-defsym-math-prim "\\downarrow" "&#x2193;")
(tex-defsym-math-prim "\\Downarrow" "&#x21d3;")
(tex-let-prim "\\vert" "\\mid")
(tex-let-prim "\\Vert" "\\parallel")
(tex-defsym-math-prim "\\backslash" "\\")

(tex-def-math-prim "\\sqrt"
 (lambda () (emit "&#x221a;(") (tex2page-string (get-token)) (emit ")")))

(tex-def-prim "\\vphantom" #'get-group)
(tex-def-prim "\\hphantom" #'get-group)
(tex-def-prim "\\phantom" #'get-group)

(tex-defsym-math-prim "\\cong" "&#x2245;")
(tex-defsym-math-prim "\\notin" "&#x2209;")
(tex-defsym-math-prim "\\rightleftharpoons" "&#x21cb;")
(tex-defsym-math-prim "\\doteq" "&#x2250;")

(tex-let-prim "\\ne" "\\neq")
(tex-let-prim "\\le" "\\leq")
(tex-let-prim "\\ge" "\\geq")
(tex-let-prim "\\{" "\\lbrace")
(tex-let-prim "\\|" "\\Vert")
(tex-let-prim "\\}" "\\rbrace")
(tex-let-prim "\\to" "\\rightarrow")
(tex-let-prim "\\gets" "\\leftarrow")
(tex-let-prim "\\owns" "\\ni")
(tex-let-prim "\\land" "\\wedge")
(tex-let-prim "\\lor" "\\vee")
(tex-let-prim "\\lnot" "\\neg")
(tex-let-prim "\\iff" "\\Longleftrightarrow")

;sec 18.2, non-italic letters in formulas

(defun texbook-18-2 (x)
  (let (lhs rhs)
    (cond ((atom x)
           (setq lhs (concatenate 'string "\\" x)
                 rhs (concatenate 'string x " ")))
          (t (setq lhs (concatenate 'string "\\" (car x))
                   rhs (concatenate 'string (cadr x) " "))))
    (tex-defsym-math-prim lhs rhs)))

(mapc #'texbook-18-2
      '(
        "arccos" "arcsin" "arctan" "arg" "cos" "cosh" "cot" "coth" "csc"
        "deg" "det" "dim" "exp" "gcd" "hom" "inf" "ker" "lg" "lim"
        ("liminf" "lim inf")
        ("limsup" "lim sup")
        "ln" "log" "max" "min" "Pr"
        ("TIIPsec" "sec")
        "sin" "sinh" "sup" "tan" "tanh"
        ))

(tex-def-math-prim "\\matrix" #'do-relax)

(tex-def-math-prim "\\pmatrix" #'do-pmatrix)
(tex-def-math-prim "\\bordermatrix" #'do-pmatrix) ;FIXME

(tex-def-math-prim "\\eqalign" (lambda () (do-eqalign :eqalign)))
(tex-def-math-prim "\\displaylines" (lambda () (do-eqalign :displaylines)))
(tex-def-math-prim "\\eqalignno" (lambda () (do-eqalign :eqalignno)))
(tex-let-prim "\\leqalignno" "\\eqalignno")

;sec B.7. macros for output

;(tex-def-prim "\\pageno" (lambda () (emit *html-page-count*)))
(tex-def-prim "\\folio" (lambda () (emit *html-page-count*)))

(tex-def-prim "\\footnote" #'do-footnote)
(tex-def-prim "\\vfootnote" #'do-vfootnote)

(tex-let-prim "\\dosupereject" "\\eject")

;sec B.8. everything else

(tex-def-prim "\\magnification" #'do-magnification)

(tex-def-prim "\\tracingall" #'do-tracingall)

(tex-defsym-prim "\\fmtname" "TeX2page")
(tex-defsym-prim "\\fmtversion" *tex2page-version*)

;;app E. manmac

(tex-def-prim "\\beginchapter" #'do-beginchapter)

(tex-def-prim "\\MF"
              (let ((MF (concatenate 'string "<span style=\""
                          "font-family: sans-serif"
                          "\">METAFONT</span>")))
                (lambda () (emit MF))))

(tex-def-prim "\\AmSTeX"
              (let ((AmS (concatenate 'string "<span style=\"font-family: cursive;\">"
                           "A"
                           "<span style=\""
                           "position: relative; "
                           "top: 0.5ex; "
                           "margin-left: -.1667em; "
                           "margin-right: -.075em"
                           "\">M</span>"
                           "S</span>")))
                (lambda () (emit AmS) (emit #\-) (emit *tex-logo*))))

(tex-defsym-prim "\\bull" "&#x25fe;")

(defun do-begintt ()
  (do-end-para)
  (let ((*catcodes* *catcodes*))
    (bgroup)
    (do-tex-ctl-seq-completely "\\tthook")
    (catcode #\\ 12)
    (catcode #\space 12)
    (emit "<pre class=verbatim>")
    ;(munched-a-newline-p)
    (let ((*ligatures-p* nil) c)
      (loop
        (setq c (snoop-actual-char))
        (when (not c) (terror 'do-begintt "Eof inside \\begintt"))
        (cond ((char= c #\\)
               (let ((*catcodes* *catcodes*))
                 (catcode #\\ 0)
                 (let ((cs (get-ctl-seq)))
                   (if (string= cs "\\endtt") (return)
                       (emit-html-string cs)))))
              ((= (catcode c) **escape**) (let ((cs (get-ctl-seq)))
                                (let ((*catcodes* *catcodes*))
                                  (catcode #\\ 0)
                                  (do-tex-ctl-seq-completely cs))))
              (t (emit-html-char (get-actual-char))))))
    (emit "</pre>")
    (egroup))
  (do-noindent))

(tex-def-prim "\\begintt" #'do-begintt)

(defun do-frac ()
  (if (eq *tex-format* :latex)
      (do-latex-frac)
    (do-tex-frac)))

(tex-def-prim "\\frac" #'do-frac)

;; XeTeX

(defun do-ifdefined ()
  (let ((x (get-raw-token/is)))
    (if (or (not (ctl-seq-p x))
            (and (ctl-seq-p x)
                 (or (find-def x) (find-math-def x))))
        (do-iftrue)
      (do-iffalse))))

(tex-def-prim "\\ifdefined" #'do-ifdefined)

(defun do-xetexpdffile ()
  (let* ((pdf-file (get-filename))
         height rotated width
         (img-file-stem (next-html-image-file-stem))
         (img-file (concatenate 'string *aux-dir/* img-file-stem (find-img-file-extn))))
    (loop (cond ((eat-word "height") (setq height (get-pixels)))
                ((eat-word "rotated") (setq rotated (get-number)))
                ((eat-word "width") (setq width (get-pixels)))
                (t (return))))
    (unless (probe-file img-file)
      (write-log :separation-space)
      (write-log #\{)
      (write-log pdf-file)
      (write-log :separation-space)
      (write-log "->")
      (write-log :separation-space)
      (write-log img-file)
      (write-log #\})
      (write-log :separation-space)
      (ps-to-img pdf-file img-file))
    (write-log #\()
    (write-log img-file)
    (write-log :separation-space)
    (emit "<img src=\"")
    (do-img-src img-file)
    (emit "\"")
    (when height (emit " height=") (emit height))
    (when rotated
      (setq rotated (- rotated))
      (emit " style=\"transform: rotate(")
      (emit rotated) (emit "deg)\""))
    (when width (emit " width=") (emit width))
    (emit " alt=\"")
    (emit img-file)
    (emit "\"")
    (emit ">")
    (write-log #\))
    (write-log :separation-space)))

(tex-def-prim "\\XeTeXpdffile" #'do-xetexpdffile)

(defun do-xetexpicfile ()
  (let ((img-file (get-filename))
        height rotated width)
    (loop (cond ((eat-word "height") (setq height (get-pixels)))
                ((eat-word "rotated") (setq rotated (get-number)))
                ((eat-word "width") (setq width (get-pixels)))
                (t (return))))
    (emit "<img src=\"")
    (do-img-src img-file)
    (emit "\"")
    (when height (emit " height=") (emit height))
    (when rotated
      (setq rotated (- rotated))
      (emit " style=\"transform: rotate(")
      (emit rotated) (emit "deg)\""))
    (when width (emit " width=") (emit width))
    (emit " alt=\"")
    (emit img-file)
    (emit "\"")
    (emit ">")))

(tex-def-prim "\\XeTeXpicfile" #'do-xetexpicfile)

;;not relevant for TeX

(tex-def-prim "\\cssblock" #'do-cssblock)
(tex-def-prim "\\rawhtml" #'do-rawhtml)

;;tex2page.tex

(tex-def-prim "\\texonly" (lambda () (ignore-tex-specific-text "texonly")))

(tex-def-prim "\\htmlonly" (lambda () (incf *html-only*)))
(tex-def-prim "\\endhtmlonly" (lambda () (decf *html-only*)))

(tex-def-prim "\\verbwritefile" #'do-verbwritefile)
(tex-def-prim "\\verbwrite" #'do-verbwrite)

(tex-def-prim "\\defcsactive" #'do-defcsactive)

(tex-def-prim "\\gobblegroup" #'get-group)

(tex-def-prim "\\verb" #'do-verb)

(tex-def-prim "\\verbatimescapechar" #'do-verbatimescapechar)

(tex-def-prim "\\verbatiminput" #'do-verbatiminput)

(tex-def-prim "\\activettchar" #'do-opmac-activettchar)

(tex-def-prim "\\url" #'do-url)
(tex-def-prim "\\urlh" #'do-urlh)
(tex-def-prim "\\urlp" #'do-urlp)
(tex-def-prim "\\urlhd" #'do-urlhd)
(tex-def-prim "\\href" #'do-urlh)
(tex-let-prim "\\path" "\\verb")

(tex-def-prim "\\scm" #'do-scm)
(tex-def-prim "\\scminput" #'do-scminput)
(tex-let-prim "\\lispinput" "\\scminput")
(tex-def-prim "\\scmdribble" #'do-scmdribble)

(tex-def-prim "\\imgdef" #'make-reusable-img)
(tex-def-prim "\\makehtmlimage" #'do-makehtmlimage)
(tex-def-prim "\\mathg" #'do-mathg)
(tex-def-prim "\\mathdg" #'do-mathdg)

(tex-def-prim "\\xrtag" #'do-tag)

(defun do-iffileexists ()
  (let* ((file (actual-tex-filename (get-filename-possibly-braced)))
         (then-e (ungroup (get-group)))
         (else-e (ungroup (get-group))))
    (tex2page-string (if file then-e else-e))))

(tex-def-prim "\\IfFileExists" #'do-iffileexists)

(defun do-inputiffileexists ()
  (let* ((f (actual-tex-filename (get-filename-possibly-braced)))
         (then-txt (ungroup (get-group)))
         (else-txt (ungroup (get-group))))
    (cond (f (tex2page-string then-txt) (tex2page-file f))
          (t (tex2page-string else-txt)))))

(tex-def-prim "\\InputIfFileExists" #'do-inputiffileexists)

(defun do-futurenonspacelet ()
  (let* ((first (get-raw-token/is))
         (second (get-raw-token/is))
         (third (get-raw-token/is)))
    (do-futurelet-aux first second third)))

(tex-def-prim "\\futurenonspacelet" #'do-futurenonspacelet)

(defun do-scm-set-keywords ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignorespaces :all)
        (let ((c (snoop-actual-char)))
          (when (not c) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-builtins*) nil
                  (gethash s *scm-variables*) nil
                  (gethash s *scm-keywords*) t)))))))

(tex-def-prim "\\scmkeyword" #'do-scm-set-keywords)

(defun do-scm-set-builtins ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignorespaces :all)
        (let ((c (snoop-actual-char)))
          (when (not c) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-keywords*) nil
                  (gethash s *scm-variables*) nil
                  (gethash s *scm-builtins*) t)))))))

(tex-def-prim "\\scmbuiltin" #'do-scm-set-builtins)

(tex-let-prim "\\scmconstant" "\\scmbuiltin")

(defun do-scm-set-variables ()
  (call-with-input-string/buffered (ungroup (get-group))
    (lambda ()
      (loop
        (ignorespaces :all)
        (let ((c (snoop-actual-char)))
          (when (not c) (return))
          (let ((s (scm-get-token)))
            (setf (gethash s *scm-builtins*) nil
                  (gethash s *scm-keywords*) nil
                  (gethash s *scm-variables*) t)))))))

(tex-def-prim "\\scmvariable" #'do-scm-set-variables)

(tex-let-prim "\\setbuiltin" "\\scmbuiltin")
(tex-let-prim "\\setconstant" "\\scmconstant")
(tex-let-prim "\\setkeyword" "\\scmkeyword")
(tex-let-prim "\\setvariable" "\\scmvariable")

(defun do-scm-slatex-lines (env display-p result-p)
  (declare (string env))
  (let ((*catcodes* *catcodes*)
        (endenv (concatenate 'string "\\end" env))
        (in-table-p (and (not (null *tabular-stack*))
                         (member (car *tabular-stack*) '(:block :figure :table)))))
    (cond (display-p (do-end-para)) (in-table-p (emit "</td><td>")))
    ;(munched-a-newline-p)
    (bgroup)
    (when (string= env "tt")
      (do-tex-ctl-seq-completely "\\tthook")
      (catcode #\\ 12))
    (emit "<div class=leftline><pre class=scheme")
    (when result-p (emit "response"))
    (emit ">")
    (let ((*ligatures-p* nil) (*verb-display-p* t) (*not-processing-p* t) c)
      (loop
        (setq c (snoop-actual-char))
        (when (not c)
          (terror 'do-scm-slatex-lines "Eof inside " env))
        (cond ((char= c #\newline)
               (get-actual-char)
               (scm-emit-html-char c)
               (cond ((not (tex2page-flag-boolean "\\TZPslatexcomments")) nil)
                     ((char= (snoop-actual-char) #\;)
                      (get-actual-char)
                      (if (char= (snoop-actual-char) #\;)
                          (toss-back-char #\;)
                          (scm-output-slatex-comment)))))
              ((char= c #\\)
               (let ((x (let ((*catcodes* *catcodes*))
                          (catcode #\\ 0)
                          (get-ctl-seq))))
                 (cond ((string= x endenv) (return))
                       ((string= x "\\end")
                        (let ((g (get-grouped-environment-name-if-any)))
                          (cond ((and g (string= g env)) (egroup) (return))
                                (t (scm-output-token x)
                                   (when g
                                     (scm-output-token "{")
                                     (scm-output-token g)
                                     (scm-output-token "}"))))))
                       (t (scm-output-token x)))))
              ((= (catcode c) **escape**) (let ((cs (get-ctl-seq)))
                                (let ((*catcodes* *catcodes*))
                                  (catcode #\\ 0)
                                  (do-tex-ctl-seq-completely cs))))
              (t (scm-output-next-chunk)))))
    (emit "</pre></div>")
    (egroup)
    (cond (display-p (do-noindent))
          (in-table-p (emit "</td><td>")))))

(tex-def-prim "\\schemedisplay" (lambda () (do-scm-slatex-lines "schemedisplay" t nil)))

(tex-def-prim "\\schemeresponse" (lambda () (do-scm-slatex-lines "schemeresponse" t :result)))

(tex-def-prim "\\begintts" (lambda () (do-scm-slatex-lines "tt" t nil)))

(tex-def-prim "\\title" #'do-title)
(tex-def-prim "\\subject" #'do-subject)

(tex-def-prim "\\em" (lambda () (do-switch :em)))

(tex-def-prim "\\raggedleft" (lambda () (do-switch :raggedleft)))

(defun do-index-help (idx-entry)
  (declare (string idx-entry))
  (incf *index-count* 2)
  ;
  ;increment by 2 rather than 1, effectively disabling makeindex
  ;from creating ranges, which are meaningless for HTML.  Actually,
  ;makeindex's -r option prevents ranging but who remembers these
  ;things?
  ;
  (!index *index-count* *html-page-count*)
  (write-aux `(!index ,*index-count* ,*html-page-count*))
  (let ((tag (concatenate 'string *html-node-prefix* "index_"
               (write-to-string *index-count*))))
    (emit-anchor tag)
    (unless *index-stream*
      (let ((idx-file (concatenate 'string *aux-dir/* *jobname*
                        *index-file-suffix* ".idx")))
        (setq *index-stream* (open idx-file :direction :output
                                 :if-exists :supersede))))
    (princ "\\indexentry{" *index-stream*)
    (cond ((or (search "|see{" idx-entry)
               (search "|seealso{" idx-entry))
           (display-index-entry idx-entry *index-stream*))
          ((setq *it* (search "|(" idx-entry))
           (let ((i *it*))
             (display-index-entry (subseq idx-entry 0 i) *index-stream*)
             (princ "|expandhtmlindex" *index-stream*)))
          (t (display-index-entry idx-entry *index-stream*)
             (princ "|expandhtmlindex" *index-stream*)))
    (princ "}{" *index-stream*)
    (princ *index-count* *index-stream*)
    (princ "}" *index-stream*)
    (terpri *index-stream*)))

(defun do-index ()
  (let ((idx-entry (ungroup (get-group))))
    (ignorespaces) ;?
    (unless (search "|)" idx-entry)
      (do-index-help idx-entry))))

(tex-def-prim "\\index" #'do-index)

(defun do-theindex ()
  (bgroup)
  (tex2page-string "\\let\\endtheindex\\egroup")
  (tex2page-string "\\let\\indexspace\\relax")
  (tex2page-string "\\let\\item\\indexitem")
  (tex2page-string "\\let\\subitem\\indexsubitem")
  (tex2page-string "\\let\\subsubitem\\indexsubsubitem")
  (tex2page-string "\\let\\(\\expandhtmlindex"))

(tex-def-prim "\\theindex" #'do-theindex)

(defun do-indexitem (indent)
  (declare (fixnum indent))
  (setq *index-page-mention-alist* (make-hash-table))
  (emit "<br>")
  (emit-newline)
  (emit-nbsp (* indent 4)))

(tex-def-prim "\\indexitem" (lambda () (do-indexitem 0)))
(tex-def-prim "\\indexsubitem" (lambda () (do-indexitem 1)))
(tex-def-prim "\\indexsubsubitem" (lambda () (do-indexitem 2)))

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

(tex-def-prim "\\inputindex" (lambda () (do-inputindex nil)))

(defun do-xrdef ()
  (let ((tag (get-peeled-group)))
    (do-tag-aux tag (write-to-string *html-page-count*))))

(tex-def-prim "\\xrdef" #'do-xrdef)

(defun do-label () (do-label-aux (get-label)))

(tex-def-prim "\\label" #'do-label)

(defun do-ref () (do-ref-aux (get-label) nil nil))

(tex-def-prim "\\ref" #'do-ref)

(defun do-pageref ()
  (let ((label-ref (label-bound-p (get-peeled-group))))
    (if label-ref
        (let ((pageno (label*-page label-ref)))
          (emit-ext-page-node-link-start (label*-src label-ref) pageno nil)
          (emit pageno)
          (emit-link-stop))
        (non-fatal-error "***"))))

(tex-def-prim "\\pageref" #'do-pageref)

(defun do-writenumberedtocline ()
  (let* ((seclvl (section-type-to-depth (get-peeled-group)))
         (secnum (tex-string-to-html-string (get-group)))
         (sectitle (tex-string-to-html-string (get-group))))
    (do-write-to-toc-aux seclvl secnum sectitle)))

(tex-def-prim "\\writenumberedtocline" #'do-writenumberedtocline)

(defun do-obeywhitespace () (do-obeylines) (do-obeyspaces))

(tex-def-prim "\\obeywhitespace" #'do-obeywhitespace)

(tex-def-prim "\\numberedfootnote" #'do-numbered-footnote)

(tex-def-prim "\\sidemargin" #'eat-dimen)
(tex-def-prim "\\spinemargin" #'eat-dimen)

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
                   (call-with-html-image-stream
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

(tex-def-prim "\\epsfbox" #'do-epsfbox)

;eval

(defun do-eval-string (s)
  (with-input-from-string (i s)
    (let (x)
      (loop
        (setq x (read i nil))
        (when (not x) (return))
        (eval x)))))

(defun do-eval (kind)
  (let ((s (if *outer-p*
             (ungroup
              (let ((*catcodes* *catcodes*)
                    (*expand-escape-p* t))
                (catcode #\\ 12) (catcode *esc-char-verb* 0)
                (get-group)))
             (tex-write-output-string
               (ungroup (get-group))))))
    (unless (inside-false-world-p)
      (when (> *html-only* 0) (setq kind :html))
      (case kind
        (:quiet (do-eval-string s))
        (t (tex2page-string
             (with-output-to-string (*standard-output*)
               (do-eval-string s)))
           #|
          (let ((o (make-string-output-stream)))
             (let ((*standard-output* o))
               (do-eval-string s))
             (tex2page-string (get-output-stream-string o)))
          |#
          )))))

(tex-def-prim "\\eval" (lambda () (do-eval :both)))
(tex-def-prim "\\evalh" (lambda () (do-eval :html)))
(tex-def-prim "\\evalq" (lambda () (do-eval :quiet)))

;; bibtex

(defun do-thebibliography ()
  (do-end-para)
  (get-group)
  (when (eq *tex-format* :latex)
    (tex2page-string
     (if *using-chapters-p* "\\chapter*{\\bibname}" "\\section*{\\refname}")))
  (bgroup)
  (setq *bibitem-num* 0)
  (tex2page-string "\\let\\em\\it")
  (tex2page-string "\\def\\newblock{ }")
  (emit "<table>")
  (munch-newlines)
  (emit-newline))

(tex-def-prim "\\beginthebibliography" #'do-thebibliography)
(tex-def-prim "\\endthebibliography"
 (lambda () (emit "</table>") (egroup) (do-para)))

(defun do-bibitem ()
  (do-end-para)
  (let ((bibmark (get-bracketed-text-if-any)))
    (unless (= *bibitem-num* 0)
      (emit "</td></tr>") (emit-newline))
    (emit "<tr><td class=\"rightline bibitem\">")
    (incf *bibitem-num*)
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

(tex-def-prim "\\bibitem" #'do-bibitem)

(defun do-bibliography-help (bibdata)
  (declare (string bibdata))
  (setq *using-bibliography-p* t)
  (let ((bbl-file
          (concatenate 'string *aux-dir/* *jobname* *bib-aux-file-suffix*
            ".bbl")))
    (write-bib-aux "\\bibdata{")
    (write-bib-aux bibdata)
    (write-bib-aux "}")
    (write-bib-aux #\newline)
    (cond ((probe-file bbl-file) (setq *bibitem-num* 0) (tex2page-file bbl-file)
                                 (emit-newline))
          (t (flag-missing-piece :bibliography)
             (non-fatal-error "Bibliography not generated; rerun TeX2page")))))

(defun do-bibliography ()
  (do-bibliography-help (ungroup (get-token))))

(tex-def-prim "\\bibliography" #'do-bibliography)

(defun do-bibliographystyle-help (s)
  (declare (string s))
  (write-bib-aux "\\bibstyle{")
  (write-bib-aux s)
  (write-bib-aux "}")
  (write-bib-aux #\newline))

(defun do-bibliographystyle ()
  (do-bibliographystyle-help (ungroup (get-token))))

(tex-def-prim "\\bibliographystyle" #'do-bibliographystyle)

(defun do-cite-help (delim &optional extra-text)
  (declare (character delim))
  (let ((closing-delim (cond ((char= delim #\{) #\})
                             ((char= delim #\[) #\])
                             (t (terror 'do-cite-help "faulty delim" delim)))))
    (ignorespaces)
    (unless (char= (get-actual-char) delim)
      (terror 'do-cite "missing" delim))
    (emit "[")
    (let ((first-key-p t) key)
      (loop
        (setq key (get-csv closing-delim))
        (unless key (return))
        (cond (first-key-p (setq first-key-p nil))
              (t (emit ",") (emit-nbsp 1)))
        (write-bib-aux "\\citation{")
        (write-bib-aux key)
        (write-bib-aux "}")
        (write-bib-aux #\newline)
        (do-ref-aux (concatenate 'string "cite{" key "}") nil nil))
      (when extra-text
        (emit ",") (emit-nbsp 1)
        (tex2page-string extra-text))
      (unless (char= (get-actual-char) closing-delim)
        (terror 'do-cite "missing" closing-delim))
      (when first-key-p
        (terror 'do-cite "empty \\cite")))
    (emit "]")))

(defun do-cite ()
  (if (tex2page-flag-boolean "\\TZPopmac")
      (do-cite-help #\[ nil)
      (do-cite-help #\{ (get-bracketed-text-if-any))))

(tex-def-prim "\\cite" #'do-cite)

(defun do-nocite ()
  (ignorespaces)
  (let* ((delim (if (tex2page-flag-boolean "\\TZPopmac") #\[ #\{))
         (closing-delim (if (char= delim #\{) #\} #\])))
    (unless (char= (get-actual-char) delim)
      (terror 'do-nocite "missing" delim))
    (let (key)
      (loop
        (setq key (get-csv closing-delim))
        (unless key (return))
        (write-bib-aux "\\citation{")
        (write-bib-aux key)
        (write-bib-aux "}")
        (label-bound-p (concatenate 'string "cite{" key "}"))
        (write-bib-aux #\newline)))
    (unless (char= (get-actual-char) closing-delim)
      (terror 'do-nocite "missing" closing-delim))))

(tex-def-prim "\\nocite" #'do-nocite)

;; color

(defun do-color ()
  (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
    (do-switch model)))

(tex-def-prim "\\color" #'do-color)

(defun do-colorbox ()
  (let* ((model (get-bracketed-text-if-any))
         (color (get-group))
         (text (get-peeled-group)))
    (toss-back-char #\})
    (toss-back-string text)
    (toss-back-string color)
    (when model
      (toss-back-char #\])
      (toss-back-string model)
      (toss-back-char #\[))
    (toss-back-string "\\bgcolor")
    (toss-back-char #\{)))

(tex-def-prim "\\colorbox" #'do-colorbox)

(tex-def-prim "\\bgcolor" (lambda () (do-switch :bgcolor)))

(defun do-pagecolor ()
  ; Does it for *all* pages instead of just subsequent pages though.
  (let* ((model (color-model-to-keyword (get-bracketed-text-if-any)))
         (color (read-color model)))
    (princ "body { background-color: " *css-stream*)
    (princ color *css-stream*)
    (princ "; }" *css-stream*)
    (terpri *css-stream*)))

(tex-def-prim "\\pagecolor" #'do-pagecolor)

(tex-def-prim "\\setcmykcolor" (lambda () (do-switch :cmyk)))

(defun do-definecolor ()
  (let* ((name (get-peeled-group))
         (model (color-model-to-keyword (get-peeled-group))))
    (push (cons name (read-color model))
          *color-names*)))

(tex-def-prim "\\definecolor" #'do-definecolor)
(tex-def-prim "\\DefineNamedColor" (lambda () (get-token) (do-definecolor)))

;; opmac

(defun do-opmac-ulink ()
  (let* ((url (get-bracketed-text-if-any))
         (link-text (get-group))
         (durl (doc-internal-url url)))
    (if durl
        (emit-page-node-link-start (car durl) (cadr durl))
        (emit-link-start (fully-qualify-url url)))
    (bgroup)
    (tex2page-string link-text)
    (egroup)
    (emit-link-stop)))

(tex-def-prim "\\ulink" #'do-opmac-ulink)

(defun do-opmac-title ()
  (tex-gdef-0arg "\\TIIPtitleused" "1")
  (do-end-para)
  (let ((title (tex-string-to-html-string (get-till-par))))
    (unless *title* (flag-missing-piece :document-title))
    (write-aux `(!default-title ,(make-external-title title)))
    (output-title title)))

(tex-def-prim "\\tit" #'do-opmac-title)

(tex-def-prim "\\notoc" (lambda () (setq *opmac-notoc-p* t)))
(tex-def-prim "\\nonum" (lambda () (setq *opmac-nonum-p* t)))

(defun do-opmac-heading (seclvl)
  (declare (fixnum seclvl))
  (ignorespaces)
  (let ((header (let ((*tabular-stack* (list :header)))
                  (tex-string-to-html-string (get-till-par)))))
    (let ((nonum-p *opmac-nonum-p*)
          (notoc-p *opmac-notoc-p*))
      (setq *opmac-nonum-p* nil *opmac-notoc-p* nil)
      (do-heading-help seclvl nil nonum-p notoc-p nil header))))

(tex-def-prim "\\chap" (lambda () (do-opmac-heading 0)))

(defun do-opmac-sec ()
  (if *math-mode-p*
      (toss-back-string "\\TIIPsec")
      (do-opmac-heading 1)))

(tex-def-prim "\\sec" #'do-opmac-sec)
(tex-def-prim "\\secc" (lambda () (do-opmac-heading 2)))

(defun do-opmac-begitems ()
  (do-end-para)
  (bgroup)
  (plain-count "\\TIIPopmacliststarted" 0 nil)
  (catcode #\* 13)
  (tex-def-char #\* '() "\\TIIPopmacitem" nil))

(tex-def-prim "\\begitems" #'do-opmac-begitems)

(defun do-opmac-item ()
  (when (= (the-count "\\TIIPopmacliststarted") 0)
    (plain-count "\\TIIPopmacliststarted" 1 nil)
    (push :opmac-itemize *tabular-stack*)
    (emit "<")
    (emit (case *opmac-list-style*
            ((#\o #\- #\x #\X) "u")
            (t "o")))
    (emit "l style=\"list-style-type: ")
    (emit (case *opmac-list-style*
            ((#\o) "disc")
            ((#\O) "circle")
            ((#\-) "'-'")
            ((#\n #\N) "decimal")
            ((#\i) "lower-roman")
            ((#\I) "upper-roman")
            ((#\a) "lower-alpha")
            ((#\A) "upper-alpha")
            ((#\x #\X) "square")
            (t "disc")))
    (emit "\">"))
  (do-regular-item))

(defun do-opmac-enditems ()
  (egroup)
  (do-end-para)
  (pop-tabular-stack :opmac-itemize)
  (emit "</")
  (emit (case *opmac-list-style*
          ((#\o #\- #\x #\X) "u")
          (t "o")))
  (emit "l>")
  (do-noindent))

(tex-def-prim "\\enditems" #'do-opmac-enditems)

(defun do-opmac-ii (retainp)
  (let* ((lhs (get-word))
         (sub (and *opmac-index-sub-table* (gethash lhs *opmac-index-sub-table*))))
    (if retainp (toss-back-string lhs)
        (ignorespaces))
    (do-index-help
      (cond (sub sub)
            (t (string=join (mapcar #'escape-opmac-index-entry (string=split lhs #\/))
                            #\!))))))

(tex-def-prim "\\ii" (lambda () (do-opmac-ii nil)))
(tex-def-prim "\\iid" (lambda () (do-opmac-ii t)))

(defun do-opmac-iis ()
  (let* ((lhs (get-word))
         (rhs (get-peeled-group))
         (lhs-list (mapcar #'escape-opmac-index-entry (string=split lhs #\/)))
         (rhs-list (mapcar #'escape-opmac-index-entry (string=split rhs #\/)))
         (sub ""))
    (unless (= (list-length lhs-list) (list-length rhs-list))
      (terror 'do-opmac-iis "Malformed \\iis."))
    (loop
      (when (null lhs-list) (return t))
      (let ((additive (concatenate 'string (pop lhs-list) "@" (pop rhs-list))))
        (setq sub
              (cond ((string= sub "") additive)
                    (t (concatenate 'string sub "!" additive))))))
    (unless *opmac-index-sub-table*
      (flag-missing-piece :fresh-index))
    (!opmac-iis lhs sub)
    (write-aux `(!opmac-iis ,lhs ,sub))))

(tex-def-prim "\\iis" #'do-opmac-iis)

(defun do-opmac-usebibtex ()
  (let* ((bibfile (ungroup (get-token)))
         (bibstyle (ungroup (get-token))))
    (do-bibliographystyle-help bibstyle)
    (do-bibliography-help bibfile)))

(tex-def-prim "\\usebibtex" #'do-opmac-usebibtex)

(defun do-opmac-bib ()
  (do-para)
  (incf *bibitem-num*)
  (let* ((key0 (get-bracketed-text-if-any))
         (bibitem-num-s (write-to-string *bibitem-num*))
         (key (concatenate 'string "cite{" key0 "}"))
         (node-name (concatenate 'string *html-node-prefix* "bib_" bibitem-num-s)))
    (unless key0
      (terror 'do-opmac-bib "Improper \\bib entry"))
    (tex-def-0arg "\\TIIPcurrentnodename" node-name)
    (tex-def-0arg "\\@currentlabel" bibitem-num-s)
    (emit-anchor node-name)
    (emit "[")
    (tex2page-string bibitem-num-s)
    (emit "]")
    (emit-nbsp 2)
    (do-label-aux key)))

(tex-def-prim "\\bib" #'do-opmac-bib)

(defun do-rcite ()
  (do-cite-help #\[ nil))

(tex-def-prim "\\rcite" #'do-rcite)

(tex-def-prim "\\maketoc" #'do-toc)
(tex-let-prim "\\begtt" "\\begintt")
(tex-def-prim "\\makeindex" (lambda () (do-inputindex nil)))

;;amssymb (amsfndoc.pdf)

(tex-defsym-prim "\\checkmark" "&#x2713;")
(tex-defsym-prim "\\circledR" "&#xae;")
(tex-defsym-prim "\\maltese" "&#x2720;")
(tex-defsym-prim "\\yen" "&#xa5;")

;Hebrew letters

(tex-defsym-math-prim "\\beth" "&#x2136;")
(tex-defsym-math-prim "\\daleth" "&#x2138;")
(tex-defsym-math-prim "\\gimel" "&#x2137;")

;misc symbols

(tex-defsym-math-prim "\\Bbbk" "&#x1d55c;")
(tex-defsym-math-prim "\\Finv" "&#x2132;")
(tex-defsym-math-prim "\\Game" "&#x2141;")
(tex-defsym-math-prim "\\lozenge" "&#x2662;")
(tex-defsym-math-prim "\\mho" "&#x2127;")
(tex-defsym-math-prim "\\sphericalangle" "&#x2222;")
(tex-defsym-math-prim "\\square" "&#x25ab;")

(tex-let-prim "\\Box" "\\square") ;LaTeX name (tbl 3.7)

;binary relations

(tex-defsym-math-prim "\\geqslant" "&#x2a7e;")
(tex-defsym-math-prim "\\leqslant" "&#x2a7d;")
(tex-defsym-math-prim "\\sqsubset" "&#x228f;")
(tex-defsym-math-prim "\\therefore" "&#x2234;")
(tex-defsym-math-prim "\\sqsupset" "&#x2290;")
(tex-defsym-math-prim "\\because" "&#x2235;")

;;tx fonts (txfontsdoc.pdf)

;binary operators

(tex-defsym-math-prim "\\lhd" "&#x22b2;")
(tex-defsym-math-prim "\\rhd" "&#x22b3;")
(tex-defsym-math-prim "\\unlhd" "&#x22b4;")
(tex-defsym-math-prim "\\unrhd" "&#x22b5;")

;binary relations

(tex-defsym-math-prim "\\leadsto" "&#x2933;")
(tex-defsym-math-prim "\\Join" "&#x2a1d;")

;ordinary

(tex-defsym-math-prim "\\Diamond" "&#x25c7;")

;;

(defun do-math-font (f)
  (declare (keyword f))
  (lambda ()
    (let ((*math-font* f))
      (tex2page-string (get-token)))))

(tex-def-math-prim "\\mathbf" (do-math-font :bf))
(tex-def-math-prim "\\mathrm" (do-math-font :rm))
(tex-def-math-prim "\\mathbb" (do-math-font :bb))
(tex-def-math-prim "\\mathcal" (do-math-font :cal))
(tex-def-math-prim "\\mathfrak" (do-math-font :frak))

;;

(tex-defsym-prim "\\@" "@")
(tex-defsym-prim "\\DH" "&#xd0;")
(tex-defsym-prim "\\TH" "&#xde;")
(tex-defsym-prim "\\TIIPbackslash" "\\")
(tex-defsym-prim "\\TM" "&#x2122;")
(tex-defsym-prim "\\degree" "&#xb0;")
(tex-defsym-prim "\\dh" "&#xf0;")
(tex-defsym-prim "\\pounds" "&#xa3;")
(tex-defsym-prim "\\textasciicircum" "^")
(tex-defsym-prim "\\textasciitilde" "~")
(tex-defsym-prim "\\textbackslash" "\\")
(tex-defsym-prim "\\textbar" "|")
(tex-defsym-prim "\\textbullet" "&#x2022;")
(tex-defsym-prim "\\textcopyleft" "&#x254;&#x20dd;")
(tex-defsym-prim "\\textemdash" "&#x2014;")
(tex-defsym-prim "\\textendash" "&#x2013;")
(tex-defsym-prim "\\textexclamdown" "&#xa1;")
(tex-defsym-prim "\\textgreater" "&#x3e;")
(tex-defsym-prim "\\textless" "&#x3c;")
(tex-defsym-prim "\\textperiodcentered" "&#xb7;")
(tex-defsym-prim "\\textquestiondown" "&#xbf;")
(tex-defsym-prim "\\textquotedblleft" "&#x201c;")
(tex-defsym-prim "\\textquotedblright" "&#x201d;")
(tex-defsym-prim "\\textquoteleft" "&#x2018;")
(tex-defsym-prim "\\textquoteright" "&#x2019;")
(tex-defsym-prim "\\textregistered" "&#xae;")
(tex-defsym-prim "\\th" "&#xfe;")
;(tex-defsym-prim "\\{" "{")
;(tex-defsym-prim "\\}" "}")

;

(tex-def-prim "\\abstract"
 (lambda ()
     (tex2page-string "\\quote")
     (tex2page-string "\\centerline{\\bf\\abstractname}\\par")))
(tex-def-prim "\\addcontentsline" #'do-addcontentsline)
(tex-def-prim "\\addtocounter" #'do-addtocounter)
(tex-def-prim "\\align" (lambda () (do-equation :align)))
(tex-def-prim "\\alltt" #'do-alltt)
(tex-def-prim "\\appendix" #'do-appendix)
(tex-def-prim "\\arabic" #'do-arabic)
(tex-def-prim "\\array" (lambda () (do-tabular t)))
(tex-def-prim "\\author" #'do-author)

(tex-def-prim "\\begin" #'do-begin)

(tex-def-prim "\\caption" #'do-caption)
(tex-def-prim "\\center" (lambda () (do-block :center)))
(tex-def-prim "\\chapter" (lambda () (do-heading 0)))
(tex-def-prim "\\closegraphsfile" #'do-mfpic-closegraphsfile)
(tex-def-prim "\\convertMPtoPDF" #'do-convertmptopdf)
(tex-def-prim "\\CR" (lambda () (do-cr "\\CR")))

(tex-def-prim "\\date" #'do-date)
(tex-def-prim "\\definexref" #'do-definexref)
(tex-def-prim "\\definitelylatex" #'definitely-latex)
(tex-def-prim "\\defschememathescape" (lambda () (scm-set-mathescape t)))
(tex-def-prim "\\description"
  (lambda ()
    (do-end-para)
    (push :description *tabular-stack*)
    (emit "<dl><dt></dt><dd>")))
(tex-def-prim "\\displaymath"
 (lambda () (do-latex-env-as-image "displaymath" :display)))
(tex-def-prim "\\document" #'probably-latex)
(tex-def-prim "\\documentclass" #'do-documentclass)
(tex-def-prim "\\dontuseimgforhtmlmath" (lambda () (tex-def-0arg "\\TZPmathtext" "1"))) ;obsolete
(tex-def-prim "\\dontuseimgforhtmlmathdisplay" (lambda () (tex-def-0arg "\\TZPmathtext" "1"))) ;obsolete
(tex-def-prim "\\dontuseimgforhtmlmathintext" (lambda () t)) ;obsolete

(tex-def-prim "\\emph" (lambda () (do-function "\\emph")))
(tex-def-prim "\\endalign" (lambda () (do-end-equation :align)))
(tex-def-prim "\\endalltt" #'do-end-alltt)
(tex-def-prim "\\endarray" #'do-end-tabular)
(tex-def-prim "\\endcenter" #'do-end-block)
(tex-def-prim "\\enddescription"
 (lambda ()
     (pop-tabular-stack :description)
     (do-end-para)
     (emit "</dd></dl>")
     (do-noindent)))
(tex-def-prim "\\endeqnarray" (lambda () (do-end-equation :eqnarray)))
(tex-def-prim "\\endequation" (lambda () (do-end-equation :equation)))
(tex-def-prim "\\endenumerate" #'do-endenumerate)
(tex-def-prim "\\endfigure" (lambda () (do-end-table/figure :figure)))
(tex-def-prim "\\endflushleft" #'do-end-block)
(tex-def-prim "\\endflushright" #'do-end-block)
(tex-def-prim "\\endhtmlimg"
 (lambda () (terror 'tex-def-prim "Unmatched \\endhtmlimg")))
(tex-def-prim "\\enditemize" #'do-enditemize)
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
(tex-def-prim "\\enquote" #'do-enquote)
(tex-def-prim "\\enumerate" #'do-enumerate)
(tex-def-prim "\\epsfig" #'do-epsfig)
(tex-def-prim "\\eqnarray" (lambda () (do-equation :eqnarray)))
(tex-def-prim "\\equation" (lambda () (do-equation :equation)))
;(tex-def-prim "\\TIIPeval" (lambda () (do-eval :inner)))
(tex-def-prim "\\expandhtmlindex" #'expand-html-index)
(tex-def-prim "\\externaltitle" #'do-externaltitle)

(defun do-table/figure (type)
  (declare (keyword type))
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
    (emit "<div class=\"")
    (emit type)
    (emit " ")
    (emit *display-justification*)
    (emit "\"><table width=100%><tr><td class=")
    (emit *display-justification*)
    (emit ">")))

(tex-def-prim "\\figure" (lambda () (do-table/figure :figure)))
(tex-def-prim "\\flushleft" (lambda () (do-block :flushleft)))
(tex-def-prim "\\flushright" (lambda () (do-block :flushright)))
(tex-def-prim "\\footnotesize" (lambda () (do-switch :footnotesize)))

(tex-def-prim "\\hlstart" #'do-hlstart)
(tex-def-prim "\\hspace" #'do-hspace)
(tex-def-prim "\\htmladdimg" #'do-htmladdimg)
(tex-def-prim "\\htmlcolophon" #'do-htmlcolophon) ;obsolete
(tex-def-prim "\\htmldoctype" #'do-htmldoctype)
(tex-def-prim "\\htmlgif" (lambda () (do-htmlimg "htmlgif"))) ;obsolete
(tex-def-prim "\\htmlheadonly" #'do-htmlheadonly)
(tex-def-prim "\\htmlimageconversionprogram" #'do-htmlimageconversionprogram) ;obsolete
(tex-def-prim "\\htmlimageformat" #'do-htmlimageformat) ;obsolete
(tex-def-prim "\\htmlimg" (lambda () (do-htmlimg "htmlimg"))) ;obsolete
(tex-def-prim "\\htmlimgmagnification" #'do-htmlimgmagnification) ;obsolete
(tex-def-prim "\\htmlmathstyle" #'do-htmlmathstyle)
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

(tex-def-prim "\\ignorenextinputtimestamp"
  (lambda ()
    (unless *inputting-boilerplate-p* (setq *inputting-boilerplate-p* 0))))
(tex-def-prim "\\imgpreamble" #'do-img-preamble)
(tex-def-prim "\\IMGtabbing"
 (lambda () (do-latex-env-as-image "tabbing" :display)))
(tex-def-prim "\\IMGtabular"
 (lambda () (do-latex-env-as-image "tabular" :display)))
(tex-def-prim "\\include" #'do-include)
(tex-def-prim "\\includeexternallabels" #'do-includeexternallabels)
(tex-def-prim "\\includeonly" #'do-includeonly)
(tex-def-prim "\\includegraphics" #'do-includegraphics)
(tex-def-prim "\\inputcss" #'do-inputcss)
(tex-def-prim "\\inputexternallabels" #'do-inputexternallabels)
(tex-def-prim "\\itemize" #'do-itemize)
(tex-def-prim "\\itshape" (lambda () (do-switch :it)))

(tex-def-prim "\\k" (lambda () (do-diacritic :ogonek)))

(tex-def-prim "\\large" (lambda () (do-switch :large)))
(tex-def-prim "\\Large" (lambda () (do-switch :large-cap)))
(tex-def-prim "\\LARGE" (lambda () (do-switch :large-up)))
(tex-def-prim "\\latexonly"
 (lambda () (ignore-tex-specific-text "latexonly")))
(tex-def-prim "\\leftdisplays"
 (lambda () (setq *display-justification* "leftline")))
(tex-def-prim "\\linebreak"
 (lambda () (get-bracketed-text-if-any) (emit "<br>")))
(tex-def-prim "\\listing" #'do-verbatiminput)
(tex-def-prim "\\lstlisting" (lambda () (do-verbatim-latex "lstlisting")))

(tex-def-prim "\\mailto" #'do-mailto)
(tex-def-prim "\\makeatletter" (lambda () (catcode #\@ 11)))
(tex-def-prim "\\makeatother" (lambda () (catcode #\@ 12)))
(tex-def-prim "\\maketitle" #'do-maketitle)
(tex-def-prim "\\marginnote" #'do-marginnote)
(tex-def-prim "\\marginpar" #'do-marginpar)
(tex-def-prim "\\mathp" #'do-mathp)
(tex-def-prim "\\mfpic" #'do-mfpic)
(tex-def-prim "\\minipage" #'do-minipage)

(tex-def-prim "\\newcommand" (lambda () (do-newcommand nil)))
(tex-def-prim "\\newcounter" #'do-newcounter)
(tex-def-prim "\\newenvironment" (lambda () (do-newenvironment nil)))
(tex-def-prim "\\newtheorem" #'do-newtheorem)
(tex-def-prim "\\noad" (lambda () (tex-def-0arg "\\TZPcolophondisablecredit" "1"))) ;obsolete
(tex-def-prim "\\node" #'do-node)
(tex-def-prim "\\nonumber" #'do-nonumber)
(tex-def-prim "\\noslatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "0")))
(tex-def-prim "\\notimestamp" (lambda () (tex-def-0arg "\\TZPcolophondisabletimestamp" "1"))) ;obsolete
(tex-def-prim "\\nr" (lambda () (do-cr "\\nr")))

(tex-def-prim "\\@ldc@l@r" #'do-color)
(tex-def-prim "\\opengraphsfile" #'do-mfpic-opengraphsfile)

(tex-def-prim "\\pagebreak"
 (lambda () (get-bracketed-text-if-any) (do-eject)))
(tex-def-prim "\\paragraph" (lambda () (do-heading 4)))
(tex-def-prim "\\part" (lambda () (do-heading -1)))
(tex-def-prim "\\pdfximage" #'do-pdfximage)
(tex-def-prim "\\picture"
 (lambda () (do-latex-env-as-image "picture" nil)))
(tex-def-prim "\\plainfootnote" #'do-plain-footnote)
(tex-def-prim "\\printindex" (lambda () (do-inputindex t)))
(tex-def-prim "\\providecommand" (lambda () (do-newcommand nil)))

(tex-def-prim "\\quote"
 (lambda () (do-end-para) (emit "<blockquote>") (bgroup)))

(tex-def-prim "\\r" (lambda () (do-diacritic :ring)))
(tex-def-prim "\\readtocfile" #'do-toc)
(tex-def-prim "\\refexternal" #'do-refexternal)
(tex-def-prim "\\refn" #'do-ref)
(tex-def-prim "\\renewcommand" (lambda () (do-newcommand t)))
(tex-def-prim "\\renewenvironment" (lambda () (do-newenvironment t)))
(tex-def-prim "\\resetatcatcode" (lambda () (catcode #\@ 12)))
(tex-def-prim "\\resizebox" #'do-resizebox)
(tex-def-prim "\\Romannumeral" (lambda () (do-romannumeral t)))
(tex-def-prim "\\ruledtable" #'do-ruledtable)

(tex-def-prim "\\sc" (lambda () (do-switch :sc)))
(tex-def-prim "\\schemebox"
 (lambda () (do-scm-slatex-lines "schemebox" nil nil)))
(tex-def-prim "\\schemeresponsebox"
 (lambda () (do-scm-slatex-lines "schemeresponsebox" nil :result)))
(tex-def-prim "\\schemeresult" (lambda () (do-scm :result)))
(tex-def-prim "\\scmspecialsymbol" #'do-scm-set-specialsymbol)
(tex-def-prim "\\scriptsize" (lambda () (do-switch :scriptsize)))
(tex-def-prim "\\section" (lambda () (do-heading 1)))
(tex-def-prim "\\seealso" #'do-see-also)
(tex-def-prim "\\setcounter" #'do-setcounter)
(tex-def-prim "\\sf" (lambda () (do-switch :sf)))
(tex-def-prim "\\sidx" #'do-index)
(tex-def-prim "\\slatexdisable" #'get-group)
(tex-def-prim "\\slatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "1")))
(tex-def-prim "\\small" (lambda () (do-switch :small)))
(tex-def-prim "\\stepcounter" #'do-stepcounter)
(tex-def-prim "\\strike" (lambda () (do-switch :strike)))
(tex-def-prim "\\style" #'do-opmac-list-style)
(tex-def-prim "\\subparagraph" (lambda () (do-heading 5)))
(tex-def-prim "\\subsection" (lambda () (do-heading 2)))
(tex-def-prim "\\subsubsection" (lambda () (do-heading 3)))
(tex-def-prim "\\symfootnote" #'do-symfootnote)

(tex-def-prim "\\tabbing" #'do-tabbing)
(tex-def-prim "\\table" (lambda () (do-table/figure :table)))
(tex-def-prim "\\tableplain" #'do-table-plain)
(tex-def-prim "\\tableofcontents" #'do-toc)
(tex-def-prim "\\tabular" (lambda () (do-tabular nil)))
(tex-def-prim "\\tag" #'do-tag)

(tex-def-prim "\\textbf" (lambda () (do-function "\\textbf")))
(tex-def-prim "\\textit" (lambda () (do-function "\\textit")))
(tex-def-prim "\\textrm" (lambda () (do-function "\\textrm")))
(tex-def-prim "\\textsc"
 (lambda ()
     (let ((*in-small-caps-p* t))
       (tex2page-string (get-group)))))
(tex-def-prim "\\textsl" (lambda () (do-function "\\textsl")))
(tex-def-prim "\\texttt" (lambda () (do-function "\\texttt")))
(tex-def-prim "\\textvisiblespace" (lambda () (emit *verbatim-visible-space*)))
(tex-def-prim "\\thebibliography" #'do-thebibliography)
(tex-def-prim "\\TIIPanchor" #'do-anchor-for-potential-label)
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
(tex-def-prim "\\TIIPopmacitem" #'do-opmac-item)
(tex-def-prim "\\TIIPopmacverb" #'do-opmac-intext-verb)
(tex-def-prim "\\TIIPreuseimage" #'reuse-img)
(tex-def-prim "\\TIIPrgb" (lambda () (do-switch :rgb)))
(tex-def-prim "\\TIIPRGB" (lambda () (do-switch :rgb255)))
(tex-def-prim "\\TIIPtheorem" #'do-theorem)
(tex-def-prim "\\TIIPrelax" #'do-relax)
(tex-def-prim "\\TIIPauxdir" (lambda () (emit *aux-dir/*)))
(tex-def-prim "\\TIIPlastpageno" (lambda () (emit *last-page-number*)))
(tex-def-prim "\\tiny" (lambda () (do-switch :tiny)))
(tex-def-prim "\\today" #'do-today)
(tex-def-prim "\\typein" #'do-typein)

(tex-def-prim "\\undefcsactive" #'do-undefcsactive)
(tex-def-prim "\\undefschememathescape" (lambda () (scm-set-mathescape nil)))
(tex-def-prim "\\unscmspecialsymbol" #'do-scm-unset-specialsymbol)

(defun do-verbatim-eplain ()
  (let ((*inside-eplain-verbatim-p* t)
        (*catcodes* *catcodes*))
    (catcode #\\ 11) (catcode *esc-char-verb* 0)
    (loop
      (unless *inside-eplain-verbatim-p* (return))
      (let ((c (get-actual-char)))
        (when (not c) (terror 'do-verbatim-eplain "Eof inside verbatim"))
        (cond ((= (catcode c) **escape**) (toss-back-char c)
               (let ((x (get-ctl-seq)))
                 (cond ((string= x "\\ ") (emit " "))
                       (t (do-tex-ctl-seq-completely x)))))
              ((char= c #\space) (emit "&#xa0;"))
              ((char= c #\newline) (emit "<br>") (emit-newline))
              (t (emit-html-char c)))))))

(defun do-verbatim ()
  (if (eql *tex-format* :latex) (do-verbatim-latex "verbatim")
      (do-verbatim-eplain)))

(tex-def-prim "\\verbatim" #'do-verbatim)

(tex-def-prim "\\verbc" #'do-verbc)
(tex-def-prim "\\verbinput" #'do-opmac-verbinput)
(tex-def-prim "\\vspace" #'do-vspace)

(defun do-writenumberedcontentsline ()
  (let ((toc (get-peeled-group)))
    (unless (string= toc "toc")
      (terror 'do-writenumberedcontentsline "only #1=toc supported"))
    (do-writenumberedtocline)))

(tex-def-prim "\\writenumberedcontentsline" #'do-writenumberedcontentsline)

(tex-def-prim "\\xrefn" #'do-ref)
(tex-def-prim "\\xspace" #'do-xspace)

(tex-defsym-prim "\\contentsname" "Contents")
(tex-defsym-prim "\\listfigurename" "List of Figures")
(tex-defsym-prim "\\listtablename" "List of Tables")
(tex-defsym-prim "\\refname" "References")
(tex-defsym-prim "\\indexname" "Index")
(tex-defsym-prim "\\figurename" "Figure")
(tex-defsym-prim "\\tablename" "Table")
(tex-defsym-prim "\\partname" "Part")
(tex-defsym-prim "\\appendixname" "Appendix")
(tex-defsym-prim "\\abstractname" "Abstract")
(tex-defsym-prim "\\bibname" "Bibliography")
(tex-defsym-prim "\\chaptername" "Chapter")

(tex-def-prim "\\\\" (lambda () (do-cr "\\\\")))

(defun do-latex-intext-math ()
  (do-intext-math
   (let ((o (make-string-output-stream)))
     (dump-till-ctl-seq "\\)" o)
     (get-output-stream-string o))))

(tex-def-prim "\\(" #'do-latex-intext-math)

(defun do-latex-display-math ()
  (do-display-math
   (let ((o (make-string-output-stream)))
     (dump-till-ctl-seq "\\]" o)
     (get-output-stream-string o))))

(tex-def-prim "\\[" #'do-latex-display-math)

(tex-def-prim "\\)" #'egroup)
(tex-def-prim "\\]" #'egroup)
(tex-def-prim "\\>"
              (lambda ()
                (when (and (not (null *tabular-stack*))
                           (eql (car *tabular-stack*) :tabbing))
                  (emit-nbsp 3))))
(tex-def-prim (concatenate 'string (list #\\ #\newline)) #'emit-newline)

;TeX logos

(let* ((TeX *tex-logo*)
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
              "\">&#x1dd;</span>"))
       (thinspace (kern ".16667em"))
       (ii/e (concatenate 'string "<span style=\""
              "margin-left: .05em"
              "\">2<span>"
              "<span style=\""
              "position: relative; "
              "top: .5ex"
              "\">&#x3b5;</span>"))
       )
  (tex-def-prim "\\BibTeX" (lambda () (emit Bib) (emit TeX)))
  (tex-def-prim "\\ConTeXt" (lambda () (emit ConTeXt)))
  (tex-def-prim "\\eTeX" (lambda () (emit "&#x3b5;-") (emit TeX)))
  (tex-def-prim "\\LaTeX" (lambda () (emit LaTeX)))
  (tex-def-prim "\\LaTeXe" (lambda () (emit LaTeX) (emit ii/e)))
  (tex-def-prim "\\XeLaTeX" (lambda () (emit Xe) (emit thinspace) (emit LaTeX)))
  (tex-def-prim "\\XeTeX" (lambda () (emit Xe) (emit TeX))))

;ignoring these

(tex-let-prim "\\htmladvancedentities" "\\TIIPrelax")
(tex-let-prim "\\endsloppypar" "\\TIIPrelax")
(tex-let-prim "\\frenchspacing" "\\TIIPrelax")
(tex-let-prim "\\protect" "\\TIIPrelax")
(tex-let-prim "\\raggedbottom" "\\TIIPrelax")
(tex-let-prim "\\sloppy" "\\TIIPrelax")
(tex-let-prim "\\sloppypar" "\\TIIPrelax")
(tex-let-prim "\\beginpackages" "\\TIIPrelax")
(tex-let-prim "\\endpackages" "\\TIIPrelax")
(tex-let-prim "\\normalfont" "\\TIIPrelax")
(tex-let-prim "\\textnormal" "\\TIIPrelax")
;(tex-let-prim "\\unskip" "\\TIIPrelax")
(tex-def-prim "\\cline" #'get-group)
(tex-def-prim "\\externalref" #'get-group)
(tex-def-prim "\\GOBBLEARG" #'get-group)
(tex-def-prim "\\hyphenation" #'get-group)
(tex-def-prim "\\newlength" #'get-group)
(tex-def-prim "\\pagenumbering" #'get-group)
(tex-def-prim "\\pagestyle" #'get-group)
(tex-def-prim "\\raisebox" #'get-group)
(tex-def-prim "\\thispagestyle" #'get-group)
(tex-def-prim "\\manpagesection" #'get-group)
(tex-def-prim "\\manpagedescription" #'get-group)
(tex-def-prim "\\lstset" #'get-group)
(tex-def-prim "\\externallabels" (lambda () (get-group) (get-group)))
(tex-let-prim "\\markboth" "\\externallabels")

(tex-def-prim "\\addtolength" (lambda () (get-token) (get-token)))
(tex-def-prim "\\columnsep" #'eat-dimen)
(tex-def-prim "\\columnseprule" #'eat-dimen)
(tex-def-prim "\\enlargethispage" (lambda () (eat-star) (get-group)))
(tex-def-prim "\\evensidemargin" #'eat-dimen)
(tex-def-prim "\\fboxsep" #'eat-dimen)
(tex-def-prim "\\headsep" #'eat-dimen)
(tex-def-prim "\\itemsep" #'eat-dimen)
(tex-def-prim "\\leftcodeskip" #'eat-dimen)
(tex-def-prim "\\leftmargin" #'eat-dimen)
(tex-def-prim "\\oddsidemargin" #'eat-dimen)
(tex-def-prim "\\pagewidth" #'eat-dimen)
(tex-def-prim "\\parbox" (lambda () (get-bracketed-text-if-any) (get-group)))
(tex-def-prim "\\parsep" #'eat-dimen)
(tex-def-prim "\\rightcodeskip" #'eat-dimen)
(tex-def-prim "\\scriptfont" #'get-token)
(tex-def-prim "\\scriptscriptfont" #'get-token)
(tex-def-prim "\\textfont" #'get-token)
(tex-def-prim "\\textheight" #'eat-dimen)
(tex-def-prim "\\topmargin" #'eat-dimen)
(tex-def-prim "\\topsep" #'eat-dimen)
(tex-let-prim "\\addvspace" "\\vspace")
(tex-let-prim "\\hookaction" "\\addtolength")
(tex-let-prim "\\setlength" "\\addtolength")
(tex-let-prim "\\settowidth" "\\addtolength")

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

(tex-def-prim "\\skewchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\usepackage"
 (lambda () (get-bracketed-text-if-any) (get-group) (probably-latex)))

(tex-def-prim "\\readindexfile" (lambda () (get-token) (do-inputindex nil)))

(tex-let-prim "\\colophon" "\\htmlcolophon")
;(tex-let-prim "\\u" "\\`")
(tex-let-prim "\\endabstract" "\\endquote")
(tex-let-prim "\\mbox" "\\hbox")
(tex-let-prim "\\documentstyle" "\\documentclass")
(tex-let-prim "\\quotation" "\\quote")
(tex-let-prim "\\endquotation" "\\endquote")
(tex-let-prim "\\TIIPdate" "\\today")
(tex-let-prim "\\schemeinput" "\\scminput")

;aliases

(tex-let-prim "\\obeywhitespaces" "\\obeywhitespace")
(tex-let-prim "\\ensuremath" "\\mathg")
(tex-let-prim "\\epsffile" "\\epsfbox")
(tex-let-prim "\\htmlimgformat" "\\htmlimageformat")
(tex-let-prim "\\p" "\\verb")
(tex-let-prim "\\ttfamily" "\\tt")
(tex-let-prim "\\htmladdnormallink" "\\urlp")
(tex-let-prim "\\htmladdnormallinkfoot" "\\urlp")
(tex-let-prim "\\pagehtmlref" "\\htmlref")
(tex-let-prim "\\registered" "\\textregistered")
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

;obsoletisms

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
;(tex-let-prim "\\verbinput" "\\verbatiminput")
(tex-let-prim "\\verbatimfile" "\\verbatiminput")
(tex-let-prim "\\verbescapechar" "\\verbatimescapechar")
(tex-let-prim "\\setverbatimescapechar" "\\verbescapechar")
(tex-let-prim "\\nohtmlmathimg" "\\dontuseimgforhtmlmath")
(tex-let-prim "\\nohtmlmathintextimg" "\\dontuseimgforhtmlmathintext")
(tex-let-prim "\\nohtmlmathdisplayimg" "\\dontuseimgforhtmlmathdisplay")
(tex-let-prim "\\writetotoc" "\\writenumberedtocline") ;obsolete
(tex-let-prim "\\textdegree" "\\degree")
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
        (*aux-stream* nil)
        ;
        (*basic-style* "")
        (*bib-aux-stream* nil)
        (*bibitem-num* 0)
        ;
        (*catcodes* *catcodes*)
        (*color-names* '())
        (*css-stream* nil)
        (*current-source-file* nil)
        (*current-tex2page-input* nil)
        ;
        (*display-justification* "centerline")
        (*doctype* *doctype*)
        (*dotted-counters* (make-hash-table :test #'equal))
        (*dumping-nontex-p* nil)
        ;
        (*equation-number* nil)
        (*equation-numbered-p* t)
        (*equation-position* 0)
        (*esc-char-verb* #\|)
        (*eval-for-tex-only-p* nil)
        (*external-label-tables* (make-hash-table :test #'equal))
        ;
        (*footnote-list* '())
        (*footnote-sym* 0)
        ;
        (*global-texframe* (make-texframe* :catcodes *catcodes*))
        (*graphics-file-extensions* '(".eps"))
        ;
        (*html* nil)
        (*html-head* '())
        (*html-only* 0)
        (*html-page* nil)
        (*html-page-count* 0)
        ;
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
        (*index-stream* nil)
        (*index-table* (make-hash-table))
        (*infructuous-calls-to-tex2page* 0)
        (*input-line-no* 0)
        (*input-streams* (make-hash-table))
        (*inputting-boilerplate-p* nil)
        (*inside-appendix-p* nil)
        ;
        (*jobname* "texput")
        ;
        (*label-stream* nil)
        (*label-source* nil)
        (*label-table* (make-hash-table :test #'equal))
        (*last-modification-time* nil)
        (*last-page-number* -1)
        (*latex-probability* 0)
        (*ligatures-p* t)
        (*loading-external-labels-p* nil)
        (*log-file* nil)
        (*log-stream* nil)
        ;
        (*main-tex-file* nil)
        (*math-delim-left* nil)
        (*math-delim-right* nil)
        (*math-height* 0)
        (*math-mode-p* nil)
        (*mfpic-file-num* nil)
        (*mfpic-file-stem* nil)
        (*mfpic-stream* nil)
        (*missing-eps-files* '())
        (*missing-pieces* '())
        (*mp-files* '())
        ;
        (*not-processing-p* nil)
        ;
        (*opmac-active-tt-char* nil)
        (*opmac-index-sub-table* nil)
        (*opmac-list-style* #\o)
        (*opmac-nonum-p* nil)
        (*opmac-notoc-p* nil)
        (*opmac-verbinput-table* (make-hash-table :test #'equal))
        (*outer-p* t)
        (*output-streams* (make-hash-table))
        (*outputting-external-title-p* nil)
        (*outputting-to-non-html-p* nil)
        ;
        (*package* *this-package*)
        ;
        (*quote-level* 0)
        ;
        (*reading-control-sequence-p* nil)
        (*recent-node-name* nil)
        (*redirect-delay* nil)
        (*redirect-url* nil)
        (*reg-num-count* 255)
        ;
        (*scm-builtins* nil)
        (*scm-dribbling-p* nil)
        (*scm-keywords* nil)
        (*scm-special-symbols* nil)
        (*scm-variables* nil)
        (*scripts* '())
        (*section-counter-dependencies* (make-hash-table))
        (*section-counters* (make-hash-table))
        (*slatex-math-escape* nil)
        (*source-changed-since-last-run-p* nil)
        (*start-time* (get-universal-time))
        (*stylesheets* '())
        (*subjobname* nil)
        ;
        (*tabular-stack* '())
        (*temp-string-count* 0)
        (*temporarily-use-utf8-for-math-p* nil)
        (*tex-env* '())
        (*tex-format* :plain)
        (*tex-if-stack* '())
        (*tex-output-format* nil)
        (*tex2page-inputs* (string=split (retrieve-env "TEX2PAGEINPUTS") *path-separator*))
        (*title* nil)
        (*toc-list* '())
        (*toc-page* nil)
        ;
        (*unresolved-xrefs* '())
        (*using-bibliography-p* nil)
        (*using-chapters-p* nil)
        (*using-index-p* nil)
        ;
        (*verb-display-p* nil)
        (*verb-stream* nil)
        (*verb-visible-space-p* nil)
        (*verb-written-files* '())
        ;
        (*write-log-index* 0)
        (*write-log-possible-break-p* nil)
        )
    (initialize-globals)
    (setq *main-tex-file*
          (actual-tex-filename tex-file
                               (check-input-file-timestamp-p tex-file)))
    (write-log "This is TeX2page, Version ")
    (write-log *tex2page-version*)
    (write-log #\space)
    (write-log #\()
    (write-log *common-lisp-version*)
    (write-log #\))
    (write-log #\space t)
    (write-log (seconds-to-human-time *start-time*) t)
    (write-log :separation-newline)
    (cond (*main-tex-file*
            (setq *subjobname* *jobname*
                  *html-page*
                  (concatenate 'string *aux-dir/* *jobname* *output-extension*))
            (setq *html* (make-ostream* :stream
                                      (open *html-page* :direction :output
                                            :if-exists :supersede)))
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

(tex2page *tex2page-file-arg*)
