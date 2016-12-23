#!/bin/sh
":";exec racket -f $0 -m -- "$@"

;tex2page
;Dorai Sitaram

(require mzlib/process)
(require mzlib/trace)
(require racket/private/more-scheme)

(define *tex2page-version* "20161222") ;last change

(define *tex2page-website*
  ;for details, please see
  "http://ds26gte.github.io/tex2page/index.html")

;some parameters that may hinge on user preference or
;need.  I've tried to make it all automatic, with
;reasonable defaults, but it may need some
;personal tending

(define *operating-system*
  ;change if you need a better OS identifier
  (if (getenv "COMSPEC")
      (let ((term (getenv "TERM")))
        (if (and (string? term) (string=? term "cygwin"))
            ':cygwin ':windows))
      ':unix))

(define *enable-write-18-p*
  ;true if you want \write18{command} to execute command
  ;as an OS command; false if you think that is a
  ;security risk for your situation
  #t)

(define *output-extension*
  ;some may want ".htm" here
  ".html")

(define *ghostscript*
  ;the name of the Ghostscript executable.  You
  ;may need to modify for Windows
  (case *operating-system*
    ((:windows) (or (ormap
                    (lambda (f)
                      (and (file-exists? f) f))
                    '("c:\\cygwin\\bin\\gs.exe"
                      "g:\\cygwin\\bin\\gs.exe"
                      "c:\\aladdin\\gs6.01\\bin\\gswin32c.exe"
                      "d:\\aladdin\\gs6.01\\bin\\gswin32c.exe"
                      "d:\\gs\\gs8.00\\bin\\gswin32.exe"
                      "g:\\gs\\gs8.00\\bin\\gswin32.exe"
                      ))
                   "gswin32.exe"))
    (else "gs")))

(define *use-closing-p-tag-p*
  ;false if you don't want closing </p>
  #t)

(define *metapost*
  ;don't know why, but MetaPost is called mpost in
  ;teTeX (all Unix TeXs?) and mp in MikTeX (all
  ;Windows TeXs?)
  (case *operating-system*
    ((:windows) "mp")
    (else "mpost")))

;change these navbar wordings to suit the natural language of your
;document

(define *navigation-sentence-begin* "Go to ")
(define *navigation-first-name* "first")
(define *navigation-previous-name* "previous")
(define *navigation-next-name* "next")
(define *navigation-page-name* " page")
(define *navigation-contents-name* "contents")
(define *navigation-index-name* "index")
(define *navigation-sentence-end* "")

(define *last-modified* "Last modified")
(define *html-conversion-by* "HTML conversion by")

;
;change stuff below at your peril!  scmxlate will transform them
;appropriately if needed
;

(define *doctype* "html")

(define *scheme-version*
  (string-append "Racket " (version)))

(define *path-separator*
  (if (eqv? *operating-system* ':windows) #\; #\:))

(define *directory-separator*
  (if (eqv? *operating-system* ':windows) "\\" "/"))

;including a \bye from the tex command line.  Unix
;needs to escape the backslash, Windows doesn't

(define *bye-tex*
  (case *operating-system*
    ((:windows) " \\bye")
    (else " \\\\bye")))

(define *int-corresp-to-0* (char->integer #\0))

(define *verbatim-visible-space*
  ; "<span style=\"color: red\">&#xb7;</span>"
  "<span style=\"vertical-align: -0.5ex\">&#x2334;</span>")

(define *aux-file-suffix* "-Z-A")
(define *bib-aux-file-suffix* "-Z-B")
(define *css-file-suffix* "-Z-S.css")
;(define *html-node-prefix* "node_")
(define *html-node-prefix* "TAG:__tex2page_")
(define *html-page-suffix* "-Z-H-")
(define *img-file-suffix* "-Z-G-")
(define *imgdef-file-suffix* "D-")
(define *index-file-suffix* "-Z-I")
(define *label-file-suffix* "-Z-L")
(define *mfpic-tex-file-suffix* ".Z-M-tex")
(define *toc-file-suffix* "-Z-C")

(define *ghostscript-options*
  " -q -dBATCH -dNOPAUSE -dNO_PAUSE -sDEVICE=ppmraw")

(define *invisible-space* (list ':*invisible-space*))

(define *outer-invisible-space* (list ':*outer-invisible-space*))

;January is 1 in PLT, MIT, CL; 0 in others
(define *january-number* 1)

;PLT, MIT, CL give true AD; others give AD - 1900
(define *anno-domini-at-0* 0)

(define *month-names*
  (vector "January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(define *if-aware-ctl-seqs*
  '("\\csname" "\\else" "\\end" "\\eval" "\\fi" "\\let"))

(define *tex-logo*
  (string-append "T" "<span style=\""
    "position: relative; "
    "top: 0.5ex; "
    "margin-left: -0.1667em; "
    "margin-right: -0.125em; "
    "text-transform: uppercase"
    "\">e</span>" "X"))

(define *tex-files-to-ignore*
  '("btxmac" "eplain" "epsf" "lmfonts" "mfpic" "supp-pdf"))

(define *filename-delims* '())

(define *scm-token-delims*
  (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

(define *tex-extra-letters* '()
  ;(list #\@)
  )

(define *return* (integer->char 13))

(define *tab* (integer->char 9))

(define *epoch* 1970)

;

;above are true globals.  Following are
;per-document globals

(define *afterassignment* #f)
(define *afterpar* '())
(define *afterbye* '())
(define *aux-dir* #f)
(define *aux-dir/* "")
(define *aux-port* #f)

(define *bib-aux-port* #f)
(define *bibitem-num* 0)

(define *color-names* '())
(define *comment-char* #\%)
(define *css-port* #f)
(define *current-source-file* #f)
(define *current-tex2page-input* #f)

(define *display-justification* #f)
(define *dotted-counters* #f)
(define *dumping-nontex-p* #f)

(define *equation-number* #f)
(define *equation-numbered-p* #t)
(define *equation-position* 0)
(define *esc-chars* '(#\\))
(define *esc-char-std* #\\)
(define *esc-char-verb* #\|)
(define *eval-for-tex-only-p* #f)
(define *expand-escape-p* #f)
(define *external-label-tables* #f)

(define *footnote-list* '())
(define *footnote-sym* 0)

(define *global-texframe* #f)
(define *graphics-file-extensions* '())

(define *html* (current-output-port))
(define *html-head* #f)
(define *html-only* 0)
(define *html-page* #f)
(define *html-page-count* #f)

(define *ignore-timestamp-p* #f)
(define *ignore-active-space-p* #f)
;(define *img-magnification* 1)
(define *img-file-count* 0)
(define *img-file-tally* 0)
(define *imgdef-file-count* 0)
(define *imgpreamble* #f)
(define *imgpreamble-inferred* #f)
(define *in-alltt-p* #f)
(define *in-display-math-p* #f)
(define *in-para-p* #f)
(define *in-small-caps-p* #f)
(define *includeonly-list* #f)
(define *index-page-mention-alist* #f)
(define *index-table* #f)
(define *index-count* #f)
(define *index-page* #f)
(define *index-port* #f)
(define *infructuous-calls-to-tex2page* #f)
(define *input-line-no* 0)
(define *input-streams* #f)
(define *inputting-boilerplate-p* #f)
(define *inside-appendix-p* #f)
(define *inside-eplain-verbatim-p* #f)

(define *jobname* "texput")

(define *label-port* #f)
;identifier *labels* is reserved in stklos!
(define *label-source* #f)
(define *label-table* #f)
(define *last-modification-time* #f)
(define *last-page-number* #f)
(define *latex-probability* #f)
(define *ligatures-p* #f)
(define *loading-external-labels-p* #f)
(define *log-file* #f)
(define *log-port* #f)

(define *main-tex-file* #f)
(define *math-delim-left* #f)
(define *math-delim-right* #f)
(define *math-height* #f)
(define *math-mode-p* #f)
(define *math-needs-image-p* #f)
(define *math-font* #f)
(define *math-script-mode-p* #f)
(define *mfpic-file-num* #f)
(define *mfpic-file-stem* #f)
(define *mfpic-port* #f)
(define *missing-eps-files* #f)
(define *missing-pieces* #f)
(define *mp-files* #f)

(define *not-processing-p* #f)

(define *opmac-active-tt-char* #f)
(define *opmac-index-sub-table* #f)
(define *opmac-list-style* #\o)
(define *opmac-nonum-p* #f)
(define *opmac-notoc-p* #f)
(define *opmac-verbinput-table* #f)
(define *outer-p* #f)
(define *output-streams* #f)
(define *outputting-external-title-p* #f)
(define *outputting-to-non-html-p* #f)

(define *package* #f)

(define *quote-level* 0)

(define *reading-control-sequence-p* #f)
(define *recent-node-name* #f)
(define *remember-index-number* #f)
(define *redirect-delay* #f)
(define *redirect-url* #f)

(define *scm-builtins* #f)
(define *scm-dribbling-p* #f)

(define *scm-special-symbols* #f)
(define *scm-keywords* #f)

(define *scm-variables* #f)
(define *scripts* #f)
(define *section-counters* #f)
(define *section-counter-dependencies* #f)
;(define *slatex-like-comments-p* #f)
(define *slatex-math-escape* #f)
(define *source-changed-since-last-run-p* #f)
(define *stylesheets* #f)
(define *subjobname* *jobname*)

(define *tabular-stack* '())
(define *temp* #f)
(define *temp-string-count* #f)
(define *temporarily-use-utf8-for-math-p* #f)
(define *tex2page-inputs* '())
;(define *tex2page-flag-set-p* #f)
(define *tex-env* '())
(define *tex-format* #f)
(define *tex-if-stack* '())
(define *tex-like-layout-p* #f)
(define *this-package* #f)
(define *title* #f)
(define *toc-list* #f)
(define *toc-page* #f)

(define *unresolved-xrefs* #f)
(define *using-bibliography-p* #f)
(define *using-chapters-p* #f)
(define *using-index-p* #f)

(define *verb-display-p* #f)
(define *verb-port* #f)
(define *verb-visible-space-p* #f)
(define *verb-written-files* '())

(define *write-log-max* 55)
(define *write-log-index* 0)
(define *write-log-possible-break-p* #f)

;timely stuff

(define *short-month-names*
  (vector "Jan" "Feb" "March" "April" "May" "June"
          "July" "Aug" "Sept" "Oct" "Nov" "Dec"))

(define *week-day-names*
  (vector "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))

(define strftime-like
  (lambda (ignore-format d)
    (let ((m (date-minute d))
          (h (date-hour d))
          (dy (date-day d))
          (mo (date-month d))
          (y (date-year d))
          (dow (date-week-day d))
          (dst (date-dst? d))
          (tz (/ (date-time-zone-offset d) 3600)))
      (format "~a, ~a ~a, ~a, ~a:~a~a ~am ~a"
              (vector-ref *week-day-names* dow)
              (vector-ref *short-month-names* (- mo 1))
              dy
              y
              (let ((h (modulo h 12)))
                (if (= h 0) 12 h))
              (if (< m 10) "0" "")
              m
              (if (<= 0 h 11) "a" "p")
              (format "UTC~a~a"
                      (if (> tz 0) "+" "-")
                      (abs tz))))))

(define seconds-to-human-time
  (lambda (s)
    (strftime-like
     "%a, %b %e, %Y, %l:%M %p %Z"
      (seconds->date s))))

(define nreverse reverse)
(define nconc append)

(define number-to-roman
  ;adapted from CLISP's format impl
  (lambda (n upcase?)
    (unless (and (integer? n) (>= n 0))
      (terror 'number-to-roman "Missing number"))
    ;
    (let ((roman-digits
            ; decimal_value, roman_char, lower_decimal_value_used_to_modify
            '((1000 #\m 100) (500 #\d 100) (100 #\c 10) (50 #\l 10)
              (10 #\x 1) (5 #\v 1) (1 #\i 0)))
          (approp-case (lambda (c)
                         (if upcase? (char-upcase c) c))))
      (let loop ((n n) (dd roman-digits) (s '()))
        (if (null? dd)
            (if (null? s) "0"
                (list->string (nreverse s)))
            (let* ((d (car dd))
                   (val (car d))
                   (char (approp-case (cadr d)))
                   (nextval (caddr d)))
              (let loop2 ((q (quotient n val))
                          (r (remainder n val))
                          (s s))
                (if (= q 0)
                    (if (>= r (- val nextval))
                        (loop (remainder r nextval) (cdr dd)
                          (cons char
                            (cons (approp-case (cadr (assv nextval dd)))
                              s)))
                        (loop r (cdr dd) s))
                    (loop2 (- q 1) r (cons char s))))))))))

(define string-index
  (lambda (s c)
    ;returns the leftmost index of s where c occurs
    ;
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond ((>= i n) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (+ i 1))))))))

(define string-reverse-index
  (lambda (s c)
    ;returns the rightmost index of s where c occurs
    ;
    (let loop ((i (- (string-length s) 1)))
      (cond ((< i 0) #f)
            ((char=? (string-ref s i) c) i)
            (else (loop (- i 1)))))))

(define substring?
  (lambda (s1 s2)
    ;if s1 is a substring of s2, returns the index in
    ;s2 that s1 starts at.  O/w return #f.
    ;
    (let* ((s1-len (string-length s1))
           (s2-len (string-length s2))
           (n-give-up (+ 1 (- s2-len s1-len))))
      (let loop ((i 0))
        (if (< i n-give-up)
            (let loop2 ((j 0) (k i))
              (if (< j s1-len)
                  (if (char=? (string-ref s1 j) (string-ref s2 k))
                      (loop2 (+ j 1) (+ k 1))
                      (loop (+ i 1)))
                  i))
            #f)))))

;(defstruct structname [field | (field default-value)] ...)
;
;creates
;the constructor make-structname
;the predicate structname?
;the accessors structname.field (for each field)
;the setters set!structname.field (for each field)
;
;make-structname can take {field init-value} arguments,
;in which it case it sets field to init-value.  Otherwise,
;it sets field to default-value, if such was provided in
;the defstruct call

(define list-position
  (lambda (x s)
    (let loop ((s s) (i 0))
      (cond ((null? s) #f)
            ((eq? (car s) x) i)
            (else (loop (cdr s) (+ i 1)))))))

(define-syntax defstruct
  (lambda (so)
    (datum->syntax so
      (let ((so-d (syntax->datum so)))
        (let ((s (cadr so-d))
              (ff (cddr so-d)))
          (let ((s-s (symbol->string s)) (n (length ff)))
            (let* ((n+1 (+ n 1))
                   (vv (make-vector n+1)))
              (let loop ((i 1) (ff ff))
                (if (< i n+1)
                    (let ((f (car ff)))
                      (vector-set! vv i (if (pair? f) (cadr f)
                                          ;would like void here
                                          #f))
                      (loop (+ i 1) (cdr ff))) #f))
              (let ((ff (map (lambda (f) (if (pair? f) (car f) f)) ff)))
                `(begin
                   (define ,(string->symbol (string-append "make-" s-s))
                     (lambda fvfv
                       (let ((st (make-vector ,n+1)) (ff ',ff))
                         (vector-set! st 0 ',s)
                         ,@(let loop ((i 1) (r '()))
                             (if (>= i n+1) r
                                 (loop (+ i 1)
                                       (cons `(vector-set! st ,i
                                                          ,(vector-ref vv i))
                                             r))))
                         (let loop ((fvfv fvfv))
                           (unless (null? fvfv)
                             (vector-set! st (+ (list-position (car fvfv) ff) 1)
                                          (cadr fvfv))
                             (loop (cddr fvfv))))
                         st)))
                   ,@(let loop ((i 1) (procs '()))
                       (if (>= i n+1) procs
                           (loop (+ i 1)
                                 (let ((f (symbol->string
                                          (list-ref ff (- i 1)))))
                                   (cons
                                     `(define ,(string->symbol
                                                 (string-append s-s "." f))
                                        (lambda (x) (vector-ref x ,i)))
                                     (cons
                                       `(define ,(string->symbol
                                                   (string-append
                                                "set!" s-s "." f))
                                          (lambda (x v) (vector-set! x ,i v)))
                                       procs))))))
                   (define ,(string->symbol (string-append s-s "?"))
                     (lambda (x)
                       (and (vector? x) (eq? (vector-ref x 0) ',s)))))))))))))

(defstruct table (equ eqv?) (alist '()))

(define table-get
  (lambda (tbl k . d)
    (cond ((lassoc k (table.alist tbl) (table.equ tbl))
           => (lambda (c) (vector-ref (cdr c) 0)))
          ((pair? d) (car d))
          (else #f))))

(define table-put!
  (lambda (tbl k v)
    (let ((al (table.alist tbl)))
      (let ((c (lassoc k al (table.equ tbl))))
        (if c (vector-set! (cdr c) 0 v)
            (set!table.alist tbl (cons (cons k (vector v)) al)))))))

(define table-for-each
  (lambda (tbl p)
    (for-each
     (lambda (c)
       (p (car c) (vector-ref (cdr c) 0)))
     (table.alist tbl))))

(define lassoc
  (lambda (k al equ?)
    (let loop ((al al))
      (if (null? al) #f
          (let ((c (car al)))
            (if (equ? (car c) k) c
                (loop (cdr al))))))))

(define ldelete
  (lambda (y xx equ?)
    (let loop ((xx xx) (r '()))
      (if (null? xx) (nreverse r)
          (let ((x (car xx)))
            (loop (cdr xx)
              (if (equ? x y) r
                  (cons x r))))))))

(defstruct counter (value 0) (within #f))

(defstruct tocentry level number page label header)

(define string-trim-blanks
  (lambda (s)
    (let ((orig-n (string-length s)))
      (let ((i 0) (n orig-n))
        (let loop ((k i))
          (cond ((>= k n) (set! i n))
                ((char-whitespace? (string-ref s k))
                 (loop (+ k 1)))
                (else (set! i k))))
        (let loop ((k (- n 1)))
          (cond ((<= k i) (set! n (+ k 1)))
                ((char-whitespace? (string-ref s k))
                 (loop (- k 1)))
                (else (set! n (+ k 1)))))
        (if (and (= i 0) (= n orig-n)) s
            (substring s i n))))))

(define char-tex-alphabetic?
  (lambda (c)
    (or (char-alphabetic? c)
        (ormap (lambda (d) (char=? c d))
          *tex-extra-letters*))))

(define gen-temp-string
  (lambda ()
    (set! *temp-string-count* (+ *temp-string-count* 1))
    (string-append "Temp_"
                   (number->string *temp-string-count*))))

(define file-stem-name
  (lambda (f)
    ;chop off f's dirname and extension
    (let ((slash (string-reverse-index f #\/)))
      (when slash
        (set! f (substring f (+ slash 1) (string-length f))))
      (let ((dot (string-reverse-index f #\.)))
        (if dot
            (substring f 0 dot)
            f)))))

(define file-extension
  (lambda (f)
    ;returns filename f's extension, if any
    (let ((slash (string-reverse-index f #\/))
          (dot (string-reverse-index f #\.)))
      (if (and dot (not (= dot 0))
               (or (not slash) (< (+ slash 1) dot)))
          (substring f dot (string-length f))
          #f))))

(define ensure-file-deleted
  (lambda (f)
    (when (file-exists? f) (delete-file f))))

(define write-aux
  (lambda (e)
    (unless *aux-port*
      (let ((f (string-append *aux-dir/* *jobname* *aux-file-suffix* ".scm")))
        (ensure-file-deleted f)
        (set! *aux-port* (open-output-file f))))
    (write e *aux-port*)
    (newline *aux-port*)))

(define write-label
  (lambda (e)
    (unless *label-port*
      (let ((f (string-append *aux-dir/* *jobname* *label-file-suffix* ".scm")))
        (ensure-file-deleted f)
        (set! *label-port* (open-output-file f))))
    (write e *label-port*)
    (newline *label-port*)))

(define write-bib-aux
  (lambda (x)
    (unless *bib-aux-port*
      (let ((f (string-append *aux-dir/* *jobname* *bib-aux-file-suffix* ".aux")))
        (ensure-file-deleted f)
        (set! *bib-aux-port* (open-output-file f))))
    (display x *bib-aux-port*)))

(define write-log
  (lambda (x)
    (unless *log-port*
      (set! *log-file* (string-append *aux-dir/* *jobname* ".hlog"))
      (ensure-file-deleted *log-file*)
      (set! *log-port* (open-output-file *log-file*)))
    (when (and *write-log-possible-break-p* (char? x)
               (ormap (lambda (c) (char=? x c))
                      ; #\( #\[ #\{
                      '(#\) #\] #\} #\,)))
      ;disallow breaks before rt-paren-like chars
      (set! *write-log-possible-break-p* #f))
    (when (and *write-log-possible-break-p*
               (> *write-log-index* *write-log-max*))
      ;if current line is too long, start a new one,
      (newline *log-port*)
      (newline)
      (set! *write-log-possible-break-p* #f)
      (set! *write-log-index* 0))
    (unless (and (= *write-log-index* 0)
                 (or (eqv? x ':separation-newline)
                     (eqv? x ':separation-space)))
      (case x
        ((#\newline :separation-newline)
         (when *write-log-possible-break-p*
           (set! *write-log-possible-break-p* #f))
         (newline *log-port*) (newline)
         (set! *write-log-index* 0))
        ((:separation-space)
         (set! *write-log-possible-break-p* #t))
        (else
         (when *write-log-possible-break-p*
           (write-char #\space *log-port*)
           (write-char #\space)
           (set! *write-log-index* (+ *write-log-index* 1))
           (set! *write-log-possible-break-p* #f))
         (display x *log-port*)
         (display x)
         (flush-output)
         (set! *write-log-index*
           (+ *write-log-index*
              (cond ((char? x) 1)
                    ((number? x) (string-length (number->string x)))
                    ((string? x) (string-length x))
                    (else 1)))))))))

(define display-error-context-lines
  (lambda ()
    (let ((n (or (find-count "\\errorcontextlines") 0)))
      (when (and ;*input-line-no*
              *current-source-file* (> n 0))
        (let* ((n1 (max 0 (- *input-line-no* (quotient (- n 1) 2))))
               (nf (+ n1 n -1))
               (ll (call-with-input-file *current-source-file*
                     (lambda (ip)
                       (let loop ((i 1) (ll '()))
                         (let ((L (read-line ip)))
                           (cond ((eof-object? L) ll)
                                 ((< i n1) (loop (+ i 1) ll))
                                 ((<= i nf) (loop (+ i 1)
                                                  (cons (cons i L) ll)))
                                 (else ll))))))))
          (unless (null? ll)
            (let* ((border "__________________________...")
                   (only-1? (= (length ll) 1))
                   (nf (caar ll))
                   (ll (nreverse ll))
                   (n1 (caar ll)))
              (write-log "Likely error context: ")
              (write-log *current-source-file*)
              (write-log ", line")
              (unless only-1? (write-log "s"))
              (write-log " ")
              (write-log n1)
              (unless only-1?
                (write-log "-")
                (write-log nf))
              (write-log ":")
              (write-log #\newline)
              (write-log " /")
              (write-log border)
              (write-log #\newline)
              (for-each
               (lambda (L)
                 (write-log " | ")
                 (write-log (cdr L))
                 (write-log #\newline))
               ll)
              (write-log " |")
              (write-log border)
              (write-log #\newline)
              (write-log "/")
              )))))))

(define terror
  (lambda (where . args)
    (write-log ':separation-newline)
    (write-log "! ")
    (for-each write-log args)
    (write-log ':separation-newline)
    (write-log *current-source-file*)
    (write-log #\:)
    (write-log *input-line-no*)
    (write-log ": error")
    (write-log ':separation-newline)
    (write-log "l.")
    (write-log *input-line-no*)
    (write-log #\space)
    (write-log where)
    (write-log " failed.")
    (write-log ':separation-newline)
    (display-error-context-lines)
    (close-all-open-ports)
    (output-stats)
    (edit-offending-file)
    ;(display "*** TeX2page fatal error! ***")
    ;(newline)
    (error "TeX2page fatal error")))

(define edit-offending-file
  (lambda ()
    (let ((calling-from-text-editor-p (getenv "VIMRUNTIME")))
      (unless calling-from-text-editor-p
        (display "Type e to edit file at point of error; x to quit.")
        (newline)
        (display "? ")
        (flush-output)
        (let ((c (read-char)))
          (when (and (not (eof-object? c))
                     (char-ci=? c #\e))
            (let ((texedit-string (getenv "TEXEDIT")))
              (when texedit-string
                (cond ((substring? "%d" texedit-string)
                       => (lambda (i)
                            (set! texedit-string
                              (string-append (substring texedit-string 0 i)
                                             (number->string *input-line-no*)
                                             (substring texedit-string (+ i 2)
                                                        (string-length texedit-string))))))
                      (else (set! texedit-string #f))))
              (when texedit-string
                (cond ((substring? "%s" texedit-string)
                       => (lambda (i)
                            (set! texedit-string
                              (string-append (substring texedit-string 0 i)
                                             *current-source-file*
                                             (substring texedit-string (+ i 2)
                                                        (string-length texedit-string))))))
                      (else (set! texedit-string #f))))
              (unless texedit-string
                (display "Ill-formed TEXEDIT; using EDITOR.") (newline)
                (cond ((or (getenv "EDITOR") "vi")
                       => (lambda (e)
                            (set! texedit-string
                              (string-append
                                e " +" (number->string *input-line-no*)
                                " " *current-source-file*))))))
              (when texedit-string
                (system texedit-string)))))))))

(define trace-if
  (lambda (write? . args)
    (when write?
      (write-log ':separation-newline)
      (when (> *input-line-no* 0)
        (write-log "l.")
        (write-log *input-line-no*)
        (write-log #\space))
      (for-each write-log args)
      (write-log ':separation-newline))))

(define do-errmessage
  (lambda ()
    (write-log ':separation-newline)
    (write-log "! ")
    (write-log
      (tex-string-to-html-string (get-group)))
    (write-log ':separation-newline)
    (terror "\\errmessage")))

(define do-tracingall
  (lambda ()
    (tex-def-count "\\tracingcommands" 1 #f)
    (tex-def-count "\\tracingmacros" 1 #f)))

;Input port buffers

(defstruct bport (port #f) (buffer '()))

(define call-with-input-file/buffered
  (lambda (f th)
    (unless (file-exists? f)
      (terror 'call-with-input-file/buffered
        "I can't find file " f))
    (call-with-input-file f
      (lambda (i)
        (fluid-let ((*current-tex2page-input* (make-bport 'port i))
                    (*current-source-file* f)
                    (*input-line-no* 1))
          (th))))))

(define call-with-input-string/buffered
  (lambda (s th)
    (fluid-let ((*current-tex2page-input*
                  (make-bport 'buffer (string->list s)))
                (*input-line-no* *input-line-no*))
      (th))))

;  (define call-with-input-string
;  (lambda (s p)
;    (let* ((i (open-input-string s))
;           (r (p i)))
;      (close-input-port i)
;      r)))

(define eval1 eval)

;tex2page's in & out

(define snoop-char
  (lambda ()
    (let ((c (get-char)))
      (toss-back-char c)
      c)))

(define get-char
  (lambda ()
    (let ((b (bport.buffer *current-tex2page-input*)))
      (if (null? b)
          (let ((p (bport.port *current-tex2page-input*)))
            (if (not p) eof
                (let ((c (read-char p)))
                  (cond ((eof-object? c) c)
                        ((char=? c #\newline)
                         (set! *input-line-no* (+ *input-line-no* 1))
                         c)
                        (else c)))))
          (let ((c (car b)))
            (set!bport.buffer *current-tex2page-input* (cdr b))
            c)))))

(define toss-back-string
  (lambda (s)
    (set!bport.buffer *current-tex2page-input*
      (nconc (string->list s) (bport.buffer *current-tex2page-input*)))))

(define toss-back-char
  (lambda (c)
    (set!bport.buffer *current-tex2page-input*
      (cons c (bport.buffer *current-tex2page-input*)))))

(define emit
  (lambda (s)
    (display s *html*)))

(define emit-newline
  (lambda ()
    ;(when *verb-display-p* (emit "&#xa0;"))
    (newline *html*)))

;

(define invisible-space?
  (lambda (x)
    (or (eq? x *invisible-space*)
        (eq? x *outer-invisible-space*))))

(define outer-invisible-space?
  (lambda (c)
    (eq? c *outer-invisible-space*)))

(define check-outerness
  (lambda (c)
    (when (eq? c *outer-invisible-space*)
      (set! *outer-p* #t))))

(define snoop-actual-char
  (lambda ()
    (let ((c (snoop-char)))
      (cond ((eof-object? c) c)
            ((invisible-space? c) (get-char)
                                  (check-outerness c)
             (snoop-actual-char))
            ((char=? c *return*) (get-char)
             (let ((c (snoop-actual-char)))
               (if (and (not (eof-object? c)) (char=? c #\newline))
                   c
                   (begin (toss-back-char #\newline) #\newline))))
            (else c)))))

(define get-actual-char
  (lambda ()
    (let ((c (get-char)))
      (cond ((eof-object? c) c)
            ((invisible-space? c)
             (check-outerness c) (get-actual-char))
            ((char=? c *return*)
             (let ((c (snoop-actual-char)))
               (if (and (not (eof-object? c)) (char=? c #\newline))
                   (get-actual-char)
                   #\newline)))
            (else c)))))

(define get-line
  (lambda ()
    (let loop ((r '()))
      (let ((c (get-actual-char)))
        (cond ((eof-object? c)
               (if (null? r) c (list->string (nreverse r))))
              ((char=? c #\newline)
               (list->string (nreverse r)))
              ;((and (char-whitespace? c)
              ;      (let ((c2 (snoop-actual-char)))
              ;        (or (eof-object? c2) (char=? c2 #\newline))))
              ; (get-actual-char)
              ; (list->string (nreverse r)))
              (else (loop (cons c r))))))))

;tex2page

;the eating of white space

(define ignorespaces
  ;this is like TeX's \ignorespaces, which
  ;goes past at most one newline
  (lambda ()
    (unless (and (find-chardef #\space)
                 (not *ignore-active-space-p*))
      (let ((newline-active? (find-chardef #\newline))
            (newline-already-read? #f))
        (let loop ()
          (let ((c (snoop-char)))
            (when (eqv? c *return*) (set! c (snoop-actual-char)))
            (cond ((eof-object? c) #t)
                  ((invisible-space? c)
                   (get-char)
                   (unless *reading-control-sequence-p*
                     (loop)))
                  ((char=? c #\newline)
                   (cond (newline-active? #t)
                         (newline-already-read?
                           (toss-back-char #\newline))
                         (else (get-actual-char)
                               (set! newline-already-read? #t)
                               (loop))))
                  ((char-whitespace? c) ;space, tab
                   (get-actual-char) (loop))
                  (else #t))))))))

(define ignore-all-whitespace
  (lambda ()
    (let loop ()
      (let ((c (snoop-actual-char)))
        (unless (eof-object? c)
          (when (char-whitespace? c)
            (get-actual-char) (loop)))))))

(define munch-newlines
  (lambda ()
    (let loop ((n 0))
      (let ((c (snoop-actual-char)))
        (cond ((eof-object? c) n)
              ((char=? c #\newline) (get-actual-char) (loop (+ n 1)))
              ((char-whitespace? c) (get-actual-char) (loop n))
              (else n))))))

(define munched-a-newline?
  (lambda ()
    (let loop ()
      (let ((c (snoop-actual-char)))
        (cond ((eof-object? c) #f)
              ((char=? c #\newline)
               (get-actual-char) #t)
              ((char-whitespace? c)
               (get-actual-char) (loop))
              (else #f))))))

(define do-xspace
  (lambda ()
    (let ((c (snoop-actual-char)))
      (unless
       (memv c '(#\space #\" #\. #\! #\, #\:
                          #\; #\? #\/ #\' #\) #\-))
        (emit #\space)))))

(define do-relax
  (lambda () #t))

(define esc-char-p
  (lambda (c)
    (memv c *esc-chars*)))

;

(define get-ctl-seq
  (lambda ()
    ;read a tex \controlsequence
    ;(ignorespaces)
    (let ((bs (get-actual-char)))
      (unless (esc-char-p bs)
        (terror 'get-ctl-seq "Missing control sequence (" bs ")")))
    (let ((c (get-char)))
      (cond ((eof-object? c) "\\ ")
            ((invisible-space? c) "\\ ")
            ((char-tex-alphabetic? c)
             (list->string
              (nreverse
               (let loop ((s (list c #\\ )))
                 (let ((c (snoop-char)))
                   (cond ((eof-object? c) s)
                         ((invisible-space? c) s)
                         ((char-tex-alphabetic? c) (get-char)
                          (loop (cons c s)))
                         (else
                          (unless (or ;*in-verb-p*
                                      *math-mode-p*
                                      *not-processing-p*
                                      (eq? *tex-format* ':texinfo)
                                      )
                            (fluid-let ((*reading-control-sequence-p* #t))
                              (ignorespaces))
                            )
                          s)))))))
            (else (string #\\ c))))))

(define ctl-seq?
  (lambda (z)
    (char=? (string-ref z 0) #\\ )))

(define if-aware-ctl-seq?
  (lambda (z)
    (or (ormap (lambda (y) (string=? z y))
                 *if-aware-ctl-seqs*)
        (and (>= (string-length z) 3)
             (char=? (string-ref z 1) #\i)
             (char=? (string-ref z 2) #\f))
       (let ((z-th (find-corresp-prim-thunk z)))
          (if (string? z-th) #f
              (ormap
               (lambda (y)
                 (eq? z-th (find-corresp-prim-thunk y)))
                 *if-aware-ctl-seqs*))))))

(define get-group-as-reversed-chars
  (lambda ()
    ;read a {TeX group}
    (ignorespaces)
    (let ((c (get-actual-char)))
      (when (eof-object? c) (terror 'get-group "Runaway argument?"))
      (unless (char=? c #\{) (terror 'get-group "Missing {"))
       (let loop ((s (list c)) (nesting 0) (escape? #f))
         (let ((c (get-actual-char)))
           (when (eof-object? c) (terror 'get-group "Runaway argument?"))
           (cond (escape?
                  (loop (cons c s) nesting #f))
                 ((esc-char-p c)
                  (if *expand-escape-p*
                      (let ((s1
                              (begin (toss-back-char c)
                                     (let ((x (fluid-let ((*not-processing-p* #t))
                                                (get-ctl-seq))))
                                       (cond ((ormap (lambda (z) (string=? x z))
                                                     '("\\ " "\\{" "\\}"))
                                              (string (string-ref x 1)))
                                             (else
                                                 (tex-string-to-html-string x)))))))
                        (loop (nconc (nreverse (string->list s1)) s)
                              nesting #f))
                      (loop (cons c s) nesting #t)))
                 ((char=? c #\{)
                  (loop (cons c s) (+ nesting 1) #f))
                 ((char=? c #\})
                  (if (= nesting 0) (cons c s)
                      (loop (cons c s) (- nesting 1) #f)))
                 (else
                  (loop (cons c s) nesting #f))))))))

(define get-group
  (lambda ()
    (list->string (nreverse (get-group-as-reversed-chars)))))

(define get-peeled-group
  (lambda ()
    (string-trim-blanks
     (ungroup (get-group)))))

(define get-token-or-peeled-group
  (lambda ()
    (string-trim-blanks
     (ungroup (get-token)))))

(define get-grouped-environment-name-if-any
  (lambda ()
    ;read {environmentname}, if it's there
    (let ((c (snoop-actual-char)))
      (if (or (eof-object? c) (not (char=? c #\{))) #f
          (begin (get-actual-char)
            (let loop ((s '()))
              (let ((c (snoop-actual-char)))
                (cond ((or (char-alphabetic? c) (char=? c #\*))
                       (get-actual-char)
                       (loop (cons c s)))
                      ((and (pair? s) (char=? c #\})) (get-actual-char)
                       (list->string (nreverse s)))
                      (else (for-each toss-back-char s)
                        (toss-back-char #\{)
                        #f)))))))))

(define get-bracketed-text-if-any
  (lambda ()
    ;read [bracketed text], if it's there
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (if (or (eof-object? c) (not (char=? c #\[))) #f
          (begin
            (get-actual-char)
            (list->string
              (nreverse
                (let loop ((s '()) (nesting 0) (escape? #f))
                  (let ((c (get-actual-char)))
                    (when (eof-object? c)
                      (terror 'get-bracketed-text-if-any
                        "Runaway argument?"))
                    (cond (escape?
                            (loop (cons c s) nesting #f))
                          ((esc-char-p c)
                           (loop (cons c s) nesting #t))
                          ((char=? c #\{)
                           (loop (cons c s) (+ nesting 1) #f))
                          ((char=? c #\})
                           (loop (cons c s) (- nesting 1) #f))
                          ((char=? c #\])
                           (if (= nesting 0) s
                               (loop (cons c s) nesting #f)))
                          (else
                            (loop (cons c s) nesting #f))))))))))))

(define ungroup
  (lambda (s)
    (let* ((n (string-length s))
           (n-1 (- n 1)))
      (if (or (< n 2)
              (not (char=? (string-ref s 0) #\{))
              (not (char=? (string-ref s n-1) #\})))
          s
          (substring s 1 n-1)))))

(define eat-alphanumeric-string
  (lambda ()
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (if (char=? c #\")
        (begin
          (get-actual-char)
          (eat-till-char #\"))
        (let loop ()
          (let ((c (snoop-actual-char)))
            (when (or (char-alphabetic? c)
                      (char-numeric? c)
                      (memv c '(#\:)))
              (get-actual-char)
              (loop))))))))

(define get-filename
  (lambda braced?
    (let ((braced? (if (pair? braced?) (car braced?) #f)))
      ;read a filename
      (ignorespaces)
      (when braced?
        (let ((c (snoop-actual-char)))
          (if (and (char? c) (char=? c #\{))
              (get-actual-char)
              (set! braced? #f))))
      (list->string
        (nreverse
          (let loop ((s '()))
            (let ((c (snoop-actual-char)))
              (cond ((eof-object? c) s)
                    ((and (not braced?)
                          (or (char-whitespace? c)
                              (and *comment-char* (char=? c *comment-char*))
                              (ormap (lambda (d) (char=? c d)) *filename-delims*)))
                     (unless *not-processing-p* (ignorespaces))
                     s)
                    ((and braced? (char=? c #\}))
                     (get-actual-char) s)
                    ((esc-char-p c)
                     (let ((x (get-ctl-seq)))
                       (if (string=? x "\\jobname")
                           (loop (nconc (nreverse (string->list *jobname*)) s))
                           (begin
                             (toss-back-char *invisible-space*)
                             (toss-back-string x)
                             s))))
                    (else (get-actual-char)
                          (loop (cons c s)))))))))))

(define get-filename-possibly-braced
  (lambda ()
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (get-filename
       (and (char? c) (char=? c #\{))))))

(define (get-word)
  (ignorespaces)
  (list->string
    (nreverse (let loop ((s '()))
                (let ((c (snoop-actual-char)))
                  (cond ((eof-object? c) s)
                        ((or (char-whitespace? c)
                             (and *comment-char* (char=? c *comment-char*))
                             (esc-char-p c))
                         s)
                        (else (get-actual-char)
                              (loop (cons c s)))))))))

(define get-integer
  (lambda (base)
    ;base = 8, 10 or 16
    (ignorespaces)
    (string->number
      (list->string
        (nreverse
          (let loop ((s '()))
            (let ((c (snoop-actual-char)))
              (cond ((eof-object? c) s)
                    ((or (char-numeric? c)
                         (and (= base 16) (char-alphabetic? c)))
                     (get-actual-char)
                     (loop (cons c s)))
                    (else (ignorespaces)
                      s)))))) base)))

(define get-real
  (lambda ()
    (ignorespaces)
    (let ((minus? #f)
          (c (snoop-actual-char)))
      (when (char=? c #\-) (set! minus? #t))
      (when (or minus? (char=? c #\+)) (get-actual-char))
      (let ((s (let loop ((s '()))
                 (let ((c (snoop-actual-char)))
                   (cond ((eof-object? c) s)
                         ((or (char-numeric? c) (char=? c #\.))
                          (get-actual-char)
                          (loop (cons c s)))
                         (else (ignorespaces)
                               s))))))
        (and (pair? s)
            (let ((n (string->number (list->string (nreverse s)))))
              (if minus? (- n) n)))))))

(define get-equal-sign
  (lambda ()
    (ignorespaces)
    (when (char=? (snoop-actual-char) #\=)
      (get-actual-char))))

(define get-by
  (lambda ()
    (ignorespaces)
    (when (char=? (snoop-actual-char) #\b)
      (get-actual-char)
      (if (char=? (snoop-actual-char) #\y)
          (get-actual-char)
          (toss-back-char #\b)))))

(define get-to
  (lambda ()
    (ignorespaces)
    (when (char=? (snoop-actual-char) #\t)
      (get-actual-char)
      (cond ((char=? (snoop-actual-char) #\o)
             (get-actual-char)
             (ignorespaces))
            (else
             (toss-back-char #\t))))))

(define get-number-corresp-to-ctl-seq
  (lambda (x)
    (cond ((string=? x "\\the")
           ;(cadr (find-count (get-ctl-seq)))
           (get-number-corresp-to-ctl-seq (get-ctl-seq))
           )
          ((string=? x "\\active") 13)
          ((string=? x "\\pageno") *html-page-count*)
          ((string=? x "\\inputlineno") *input-line-no*)
          ((string=? x "\\footnotenumber")
           (get-gcount "\\footnotenumber"))
          ((string=? x "\\figurenumber")
           (counter.value (table-get *dotted-counters* "figure")))
          ((string=? x "\\sectiondnumber")
           (table-get *section-counters*
                      (string->number
                        (ungroup (get-token))) 0))
          ((string=? x "\\magstep") (get-number-or-false))
          ((find-count x))
          ((find-dimen x))
          (else (let ((it (resolve-defs x)))
                  (if it (char->integer (string-ref it 0))
                      (string->number x)))))))

(define get-number-or-false
  (lambda ()
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (cond ((esc-char-p c)
             (get-number-corresp-to-ctl-seq (get-ctl-seq)))
            ((char=? c #\') (get-actual-char)
             (get-integer 8))
            ((char=? c #\") (get-actual-char)
             (get-integer 16))
            ((char=? c #\`) (get-actual-char) (ignorespaces)
             (char->integer
               (if (esc-char-p (snoop-actual-char))
                   (string-ref (get-ctl-seq) 1)
                   (get-actual-char))))
            ((char=? c #\+) (get-actual-char)
             (get-number-or-false))
            ((char=? c #\-) (get-actual-char)
             (let ((n (get-number-or-false)))
               (and n (- n))))
            ((char-numeric? c) (get-integer 10))
            (else #f)))))

(define get-number
  (lambda ()
    (or (get-number-or-false)
        (terror 'get-number "Missing number."))))

(define get-tex-char-spec
  (lambda ()
    (cond ((get-number-or-false)
           => integer->char)
          (else (terror 'get-tex-char-spec "not a char")))))

(define get-url
  (lambda ()
    ;read a {URL}
    (ignorespaces)
    (let ((c (get-actual-char)))
      (cond ((eof-object? c) (terror 'get-url "Missing {"))
            ((not (char=? c #\{)) (terror 'get-url "Missing {")))
      (string-trim-blanks
       (list->string
        (nreverse
         (let loop ((nesting 0) (s '()))
           (let ((c (get-actual-char)))
             (cond ((eof-object? c)
                    (terror 'get-url "Missing }"))
                   ((and *comment-char* (char=? c *comment-char*))
                    (let ((c1 (snoop-actual-char)))
                      (loop nesting
                            (if (and (char? c1) (char-whitespace? c1))
                                (begin (ignore-all-whitespace) s)
                                (cons c s)))))
                   ((char=? c #\{)
                    (loop (+ nesting 1) (cons c s)))
                   ((char=? c #\})
                    (if (= nesting 0) s
                        (loop (- nesting 1) (cons c s))))
                   (else (loop nesting (cons c s))))))))))))

(define get-csv
  ;csv == comma-separated value
  (lambda (closing-delim)
    (ignorespaces)
    (let ((rev-lbl
            (let loop ((s '()))
              (let ((c (get-actual-char)))
                (when (eof-object? c)
                  (terror 'get-csv
                          "Runaway argument of \\cite, "
                          "\\nocite, \\expandhtmlindex?"))
                (cond ((char=? c #\,)
                       s)
                      ((char=? c closing-delim)
                       (toss-back-char c) s)
                      (else
                        (loop (cons c s))))))))
      (if (null? rev-lbl) #f
          (list->string (nreverse rev-lbl))))))

;procs for reading TeX tokens.  Token isn't really the
;right name.  It's more like a TeX sexpr (texpr?),
;ie, anything that is treated as a single item by
;a macro looking for an arg

(define get-raw-token
  (lambda ()
    ;gets a raw char (incl space, brace, %) or a ctl seq
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) c)
            ((esc-char-p c)
             (fluid-let ((*not-processing-p* #t))
               (get-ctl-seq)))
            ;((and *comment-char* (char=? c *comment-char*))
            ; (eat-till-eol) (get-raw-token))
            (else (string (get-actual-char)))))))

;(trace get-raw-token)

(define get-raw-token/is
  (lambda ()
    ;like above, but ignore space
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) c)
            ((esc-char-p c) (get-ctl-seq))
            ((and *comment-char* (char=? c *comment-char*))
             (eat-till-eol) (get-raw-token/is))
            (else (string (get-actual-char)))))))

;(trace get-raw-token/is)

(define get-token
  (lambda ()
    ;like above, but treats groups as a single entity
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) c)
            ((esc-char-p c) (get-ctl-seq))
            ((char=? c #\{) (get-group))
            ((and *comment-char* (char=? c *comment-char*))
             (eat-till-eol) (get-token))
            (else (string (get-actual-char)))))))

(define eat-word
  (lambda (word)
    (ignorespaces)
    (let ((n (string-length word)))
      (let loop ((i 0) (r '()))
        (if (>= i n) #t
            (let ((c (snoop-actual-char)))
              (cond ((char=? c (string-ref word i))
                     (get-actual-char) (loop (+ i 1) (cons c r)))
                    (else
                     (for-each toss-back-char r)
                     #f))))))))

(define eat-skip-fluff
  (lambda (full?)
    (let ((go-ahead? #t))
      (cond (full? (get-equal-sign))
            ((ormap eat-word '("plus" "minus")) #t)
            (else (set! go-ahead? #f)))
      (when go-ahead?
        (fluid-let ((*not-processing-p* #t))
          (let loop ((first? full?))
            (ignorespaces)
            (let ((c (snoop-actual-char)))
              (cond ((eof-object? c) 'done)
                    ((and (esc-char-p c) first?)
                     (get-ctl-seq))
                    ((or (char-numeric? c) (char=? c #\.))
                     (get-real) (loop first?))
                    ((or (char=? c #\') (char=? c #\" ))
                     (get-number) (loop first?))
                    ((ormap eat-word '("+" "-"))
                     (loop first?))
                    ((ormap eat-word
                            '("bp" "cc" "cm" "dd" "em" "ex" "filll"
                              "fill" "fil" "in" "minus" "mm"
                              "pc" "plus" "pt" "sp" "true"))
                     (loop #f))
                    (else 'done)))))))))

(define eat-dimen
  (lambda ()
    (eat-skip-fluff #t)))

(define eat-integer
  (lambda ()
    (fluid-let ((*not-processing-p* #t))
      (ignorespaces)
      (get-equal-sign)
      (get-number))))

(define scm-get-token
  (lambda ()
    (list->string
      (nreverse
        (let loop ((s '()) (esc? #f))
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c) s)
                  (esc? (get-actual-char)
                    (loop (cons c s) #f))
                  ((char=? c #\\ ) (get-actual-char)
                   (loop (cons c s) #t))
                  ((or (char-whitespace? c) (memv c *scm-token-delims*))
                   s)
                  (else (get-actual-char) (loop (cons c s) #f)))))))))

;

(define emit-html-char
  (lambda (c)
    ;display the html equiv of char c
    (unless (eof-object? c)
      (cond ((char=? c #\newline) (emit-newline))
            (*outputting-to-non-html-p* (emit c))
            (else
             (case c
               ((#\<) (emit "&#x3c;"))
               ((#\>) (emit "&#x3e;"))
               ((#\") (emit "&#x22;"))
               ((#\&) (emit "&#x26;"))
               (else (emit c))))))))

(define emit-html-string
  (lambda (s)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (unless (>= i n)
          (emit-html-char (string-ref s i))
          (loop (+ i 1)))))))

(define member/string-ci=?
  (lambda (s ss)
    (ormap (lambda (x)
             (string-ci=? x s))
      ss)))

;Groups

(defstruct texframe
  (definitions (make-table 'equ string=?))
  (chardefinitions (make-table))
  (counts (make-table 'equ string=?))
  (toks (make-table 'equ string=?))
  (dimens (make-table 'equ string=?))
  (postludes '())
  (uccodes (make-table 'equ char=?))
  (lccodes (make-table 'equ char=?))
  (aftergroups '()))

(define *primitive-texframe* (make-texframe))
(define *math-primitive-texframe* (make-texframe))

(define bgroup
  (lambda ()
    (set! *tex-env* (cons (make-texframe) *tex-env*))
    (when (and *in-display-math-p* (not *math-script-mode-p*))
      (bgroup-math-hook))))

(define bgroup-math-hook
  (lambda ()
    (let ((old-html *html*)
          (old-math-delim-left *math-delim-left*)
          (old-math-delim-right *math-delim-right*)
          (old-math-height *math-height*))
      (set! *html* (open-output-string))
      (set! *math-delim-left* #f)
      (set! *math-delim-right* #f)
      (set! *math-height* 0)
      (set! *tabular-stack* (cons ':mathbox *tabular-stack*))
      (add-postlude-to-top-frame
       (lambda ()
         (let ((res
                (string-append
                 "<table><tr><td align=center>"
                 (get-output-string *html*)
                 "</td></tr></table>")))
           (when (or *math-delim-left* *math-delim-right*)
             (when (and (or (member *math-delim-left* '(:lbrace :rbrace))
                            (member *math-delim-right* '(:lbrace :rbrace)))
                        (even? *math-height*))
               (set! *math-height* (+ *math-height* 1)))
             (set! res
                   (string-append
                    "<table><tr><td>"
                    (tex-math-delim-string *math-delim-left*)
                    "</td><td>"
                    res
                    "</td><td>"
                    (tex-math-delim-string *math-delim-right*)
                    "</td></tr></table>")))
           (set! *html* old-html)
           (set!  *math-delim-left* old-math-delim-left)
           (set! *math-delim-right* old-math-delim-right)
           (set! *math-height* (+ old-math-height *math-height*))
           (pop-tabular-stack ':mathbox)
           (unless *math-script-mode-p*
             (emit "</td><td>"))
           ;(when (> *math-height* 0) (emit "<small>")) ;not quite
           (emit res)
           ;(when (> *math-height* 0) (emit "</small>"))
           (unless *math-script-mode-p*
             (emit "</td><td>"))))))))

(define do-math-left
  (lambda ()
    (ignorespaces)
    (when (and *in-display-math-p* (not *math-script-mode-p*))
      (let ((s (get-token)))
        (bgroup)
        (cond ((string=? s "(") (set! *math-delim-left* ':lparen))
              ((string=? s "[") (set! *math-delim-left* ':lbrack))
              ((string=? s "\\{") (set! *math-delim-left* ':lbrace))
              ((string=? s "|") (set! *math-delim-left* ':lvert))
              (else (terror 'do-math-left)))))))

(define do-math-right
  (lambda ()
    (ignorespaces)
    (when (and *in-display-math-p* (not *math-script-mode-p*))
      (let ((s (get-token)))
        (cond ((string=? s ")") (set! *math-delim-right* ':rparen))
              ((string=? s "]") (set! *math-delim-right* ':rbrack))
              ((string=? s "\\}") (set! *math-delim-right* ':rbrace))
              ((string=? s "|") (set! *math-delim-right* ':rvert))
              (else (terror 'do-math-right)))
        (egroup)))))

(define egroup
  (lambda ()
    (when (null? *tex-env*) (terror 'egroup "Too many }'s"))
    (perform-postludes)
    (perform-aftergroups)
    (set! *tex-env* (cdr *tex-env*))))

(define perform-postludes
  (lambda ()
    (for-each
     (lambda (p) (p))
     (texframe.postludes (top-texframe)))))

(define perform-aftergroups
  (lambda ()
    (let ((ags (texframe.aftergroups (top-texframe))))
      (unless (null? ags)
        (toss-back-char *invisible-space*))
      (for-each
        (lambda (ag) (ag)) ags))))

(define perform-afterassignment
  (lambda ()
    (let ((z *afterassignment*))
      (when z
        (set! *afterassignment* #f)
        (do-tex-ctl-seq z)))))

(define add-postlude-to-top-frame
  (lambda (p)
    (let ((fr (if (null? *tex-env*) *global-texframe* (car *tex-env*))))
      (set!texframe.postludes
       fr
       (cons p (texframe.postludes fr))))))

(define add-aftergroup-to-top-frame
  (lambda (ag)
    (let ((fr (if (null? *tex-env*) *global-texframe* (car *tex-env*))))
      (set!texframe.aftergroups
       fr
       (cons ag (texframe.aftergroups fr))))))

(define top-texframe
  (lambda ()
    (if (null? *tex-env*) *global-texframe* (car *tex-env*))))

;

(defstruct tdef
  (argpat '()) (expansion "") (optarg #f)
  (thunk #f) (prim #f) (defer #f))

(defstruct cdef
  (argpat #f) (expansion #f) (optarg #f) (active #f))

(define kopy-tdef
  (lambda (lft rt)
    (set!tdef.argpat lft (tdef.argpat rt))
    (set!tdef.expansion lft (tdef.expansion rt))
    (set!tdef.optarg lft (tdef.optarg rt))
    (set!tdef.thunk lft (tdef.thunk rt))
    (set!tdef.prim lft (tdef.prim rt))
    (set!tdef.defer lft (tdef.defer rt))))

(define kopy-cdef
  (lambda (lft rt)
    (set!cdef.argpat lft (cdef.argpat rt))
    (set!cdef.expansion lft (cdef.expansion rt))
    (set!cdef.optarg lft (cdef.optarg rt))
    (set!cdef.active lft (cdef.active rt))))

(define cleanse-tdef
  (lambda (d)
    (set!tdef.argpat d '())
    (set!tdef.expansion d "")
    (set!tdef.optarg d #f)
    (set!tdef.thunk d #f)
    (set!tdef.prim d #f)
    (set!tdef.defer d #f)))

(define tex-def
  (lambda (name argpat expansion optarg thunk prim defer frame)
    (unless frame (set! frame (top-texframe)))
    (let* ((frame-defs (texframe.definitions frame))
           (d (or (table-get frame-defs name)
                  (let ((d (make-tdef)))
                    (table-put! frame-defs name
                                d)
                    d))))
      (set!tdef.argpat d argpat)
      (set!tdef.expansion d expansion)
      (set!tdef.optarg d optarg)
      (set!tdef.thunk d thunk)
      (set!tdef.prim d prim)
      (set!tdef.defer d defer))
    (perform-afterassignment)))

(define tex-def-prim
  (lambda (prim thunk)
    (tex-def prim '() #f #f thunk prim #f *primitive-texframe*)))

(define tex-defsym-prim
  (lambda (prim str)
    (tex-def prim '() #f #f (lambda () (emit str)) prim #f *primitive-texframe*)))

(define tex-def-0arg
  (lambda (cs expn)
    (tex-def cs '() expn #f #f #f #f #f)))

(define ctl-seq-no-arg-expand-once
  (lambda (cs)
    (cond ((find-def cs) => tdef.expansion)
          (else #f))))

(define tex-gdef-0arg
  (lambda (cs expn)
    ;is cs in prim field really necessary?
    (tex-def cs '() expn #f #f cs #f *global-texframe*)))

(define tex-def-prim-0arg
  (lambda (cs expn)
    ;is cs in prim field really necessary?
    (tex-def cs '() expn #f #f cs #f *primitive-texframe*)))

(define get-0arg-expn
  (lambda (cs)
    (cond ((find-def cs) => tdef.expansion)
          (else "0"))))

(define tex2page-flag-value
  (lambda (cs)
    (string-ref (get-0arg-expn cs) 0)))

(define tex2page-flag-boolean
  (lambda (cs)
    (not (memv (string-ref (get-0arg-expn cs) 0)
               '(#\0 #\f #\F #\n #\N)))))

(define tex-let
  (lambda (lft rt frame)
    (unless frame (set! frame (top-texframe)))
    (let* ((frame-defs (texframe.definitions frame))
           (lft-def
             (or (table-get frame-defs lft)
                 (let ((lft-def (make-tdef)))
                   (table-put! frame-defs lft lft-def)
                   lft-def))))
      (cond ((find-def rt)
             => (lambda (rt-def)
                  (kopy-tdef lft-def rt-def)))
            (else
              (cleanse-tdef lft-def)
              ;(set!tdef.defer lft-def rt)
              )))))

(define (tex-let-general lhs rhs frame)
  (if (ctl-seq? rhs) (tex-let lhs rhs frame)
      (tex-def lhs '() rhs #f #f #f #f frame)))

(define tex-let-prim
  (lambda (lft rt)
    (tex-let lft rt *primitive-texframe*)))

;(trace tex-let)

(define tex-def-thunk
  (lambda (name thunk frame)
    (unless (inside-false-world?)
      (tex-def name '() #f #f thunk name #f frame))))

(define tex-def-count
  (lambda (name num g?)
    (let ((frame (if g? *global-texframe* (top-texframe))))
      (table-put! (texframe.counts frame) name num))
    (perform-afterassignment)))

(define tex-def-toks
  (lambda (name tokens g?)
    (let ((frame (if g? *global-texframe* (top-texframe))))
      (table-put! (texframe.toks frame) name tokens))))

(define tex-def-dimen
  (lambda (name len g?)
    (let ((frame (if g? *global-texframe* (top-texframe))))
      (table-put! (texframe.dimens frame) name len)
    (perform-afterassignment))))

;char defs

(define tex-def-char
  (lambda (char argpat expansion frame)
    ;are we using argpat?
    (unless frame (set! frame (top-texframe)))
    (let ((d (ensure-cdef char frame)))
      (set!cdef.argpat d argpat)
      (set!cdef.expansion d expansion)
      ;(set!cdef.active d #t)
      )
    (perform-afterassignment)))

(define ensure-cdef
  (lambda (c f)
    (let ((f-chardefs (texframe.chardefinitions f)))
      (or (table-get f-chardefs c)
          (let ((d (make-cdef)))
            (table-put! f-chardefs c d)
            d)))))

(define find-chardef
  (lambda (c)
    (let ((x (or (ormap (lambda (f)
                          (table-get (texframe.chardefinitions f) c))
                        *tex-env*)
                 (table-get (texframe.chardefinitions
                           *global-texframe*) c)
                 (table-get (texframe.chardefinitions
                           *primitive-texframe*) c))))
      (and x (cdef.active x) x))))

(define find-chardef-in-top-frame
  (lambda (c)
    (let ((x (if (null? *tex-env*)
                 (or (table-get (texframe.chardefinitions *global-texframe*) c)
                     (table-get (texframe.chardefinitions *primitive-texframe*) c))
               (table-get (texframe.chardefinitions (car *tex-env*)) c))))
      (and x (cdef.active x) x))))

(define do-defcsactive
  (lambda (g?)
    (let* ((cs (get-token))
           (c (string-ref cs (if (ctl-seq? cs) 1 0)))
           (argpat (begin (ignorespaces) (get-def-arguments c)))
           (rhs (ungroup (get-group)))
           (f (and g? *global-texframe*)))
      (activate-cdef c)
      (tex-def-char c argpat rhs f))))

(define activate-cdef
  (lambda (c)
    (let ((y (cond ((find-chardef-in-top-frame c)
                    => (lambda (y)
                         (set!cdef.active y #t)
                         y))
                   (else
                     (let* ((d (find-chardef c))
                            (y (ensure-cdef c (top-texframe))))
                       (when d (kopy-cdef y d))
                       (set!cdef.active y #t)
                       y)))))
      (add-postlude-to-top-frame
        (lambda ()
          (set!cdef.active y #f))))))

(define deactivate-cdef
  (lambda (c)
    (cond ((find-chardef-in-top-frame c)
           ;if c is active in current group,
           ;deactivate it
           => (lambda (y) (set!cdef.active y #f)))
          ((find-chardef c)
           ;if c is active in an enclosing group,
           ;create a shadowing unactive def for
           ;it in the current group
           => (lambda (y)
                (let ((d (ensure-cdef c (top-texframe))))
                  (kopy-cdef d y)
                  (set!cdef.active d #f)))))))

(define do-undefcsactive
  (lambda ()
    (ignorespaces)
    (deactivate-cdef
     (string-ref (get-ctl-seq) 1))))

(define do-catcode
  (lambda ()
    (let* ((c (get-tex-char-spec))
           (val (begin (get-equal-sign) (get-number))))
      (set-catcode c val))))

(define set-catcode
  (lambda (c val)
    (unless (= val 13) (deactivate-cdef c))
    (unless (= val 11)
      (set! *tex-extra-letters* (ldelete c *tex-extra-letters* char=?)))
    (case val
      ((0) (set! *esc-chars* (cons c *esc-chars*)))
      ((11) (set! *tex-extra-letters* (cons c *tex-extra-letters*)))
      ((13) (activate-cdef c))
      )
    ))

(define do-opmac-activettchar
  (lambda ()
    (ignorespaces)
    (let ((c (get-actual-char)))
      (set! *opmac-active-tt-char* c)
      (activate-cdef c)
      (tex-def-char c '() "\\TIIPopmacverb" #f))))

(define do-opmac-intext-verb
  (lambda ()
    (bgroup)
    (fluid-let ((*ligatures-p* #f))
      (cond (*outputting-external-title-p* #f)
            (else (emit "<code class=verbatim>")))
      (do-verb-delimed *opmac-active-tt-char*)
      (cond (*outputting-external-title-p* #f)
            (else (emit "</code>"))))
    (egroup)))

;

(define do-global
  (lambda ()
    (ignorespaces)
    (let ((next (get-ctl-seq)))
      (cond ((string=? next "\\def")
             (do-def #t #f))
            ((string=? next "\\edef")
             (do-def #t #t))
            ((string=? next "\\let")
             (do-let #t))
            ;tex can't do this, so shouldn't t2p
            ;((string=? next "\\defcsactive")
            ; (do-defcsactive #t))
            ((string=? next "\\newcount")
             (do-newcount #t))
            ((string=? next "\\newtoks")
             (do-newtoks #t))
            ((string=? next "\\newdimen")
             (do-newdimen #t))
            ((string=? next "\\advance")
             (do-advance #t))
            ((string=? next "\\multiply")
             (do-multiply #t))
            ((string=? next "\\divide")
             (do-divide #t))
            ((string=? next "\\read")
             (do-read #t))
            ((ormap (lambda (z) (string=? next z))
               '("\\imgdef" "\\gifdef"))
             (make-reusable-img #t))
            ((find-count next)
             (do-count= next #t))
            ((find-toks next)
             (do-toks= next #t))
            (else
              (toss-back-string next))))))

;Headings

(define do-externaltitle
  (lambda ()
    (write-aux
      `(!preferred-title
         ,(tex-string-to-html-string (get-group))))))

(define tex2page-string
  (lambda (s)
    (call-with-input-string/buffered s
      (lambda ()
          (generate-html)))))

(define make-external-title
  (lambda (title)
    (fluid-let ((*outputting-external-title-p* #t))
      (bgroup)
      (let ((s (tex-string-to-html-string
		 (string-append
		   ;"\\def\\\\{}"
		   "\\let\\\\\\ignorespaces"
		   "\\def\\resizebox#1#2#3{}"
		   "\\let\\thanks\\TIIPgobblegroup"
		   "\\let\\urlh\\TIIPgobblegroup "
		   title))))
	(egroup)
	s))))

(define output-external-title
  (lambda ()
    (fluid-let ((*outputting-external-title-p* #t))
      (emit "<title>") (emit-newline)
      (emit (or *title* *jobname*))
      (emit-newline)
      (emit "</title>") (emit-newline))))

(define output-title
  (lambda (title)
    (emit "<h1 class=title>")
    (bgroup)
    (tex2page-string
     (string-append
      ;"\\def\\\\{\\break\\ignorespaces}"
      "\\let\\\\\\break "
      title))
    (egroup)
    (emit "</h1>")
    (emit-newline)))

(define do-subject
  (lambda ()
    (tex-gdef-0arg "\\TIIPtitleused" "1")
    (do-end-para)
    (ignorespaces)
    (let ((title (get-group)))
      (unless *title* (flag-missing-piece ':document-title))
      (write-aux `(!default-title ,(make-external-title title)))
      (output-title title)
      ;(do-para)
      )))

(define do-opmac-title
  (lambda ()
    (tex-gdef-0arg "\\TIIPtitleused" "1")
    (do-end-para)
    (let ((title (tex-string-to-html-string (get-till-par))))
      (unless *title* (flag-missing-piece ':document-title))
      (write-aux `(!default-title ,(make-external-title title)))
      (output-title title))))

(define do-latex-title
  (lambda ()
    (let ((title (get-group)))
      (unless *title* (flag-missing-piece ':document-title))
      (write-aux `(!default-title ,(make-external-title title)))
      (toss-back-string title)
      (toss-back-string "\\def\\TIIPtitle"))))

(define do-title
  (lambda ()
    ((if (eqv? *tex-format* ':latex)
         do-latex-title
         do-subject))))

(define do-author
  (lambda ()
    (toss-back-string "\\def\\TIIPauthor")))

(define do-date
  (lambda ()
    (toss-back-string "\\def\\TIIPdate")))

(define do-today
  (lambda ()
    (let ((m (get-gcount "\\month")))
      (if (= m 0) (emit "[today]")
          (begin
            (emit (vector-ref *month-names* (- m 1)))
            (emit " ")
            (emit (get-gcount "\\day"))
            (emit ", ")
            (emit (get-gcount "\\year")))))))

;

(define add-afterpar
  (lambda (ap)
    (set! *afterpar*
      (cons ap *afterpar*))))

(define do-end-para
  (lambda ()
    (when *in-para-p*
      (when *use-closing-p-tag-p* (emit "</p>"))
      (unless (null? *afterpar*)
        (for-each (lambda (ap) (ap))
                  (nreverse *afterpar*))
        (set! *afterpar* '()))
      (emit-newline)
      (set! *in-para-p* #f))))

(define do-para
  (lambda ()
    (do-end-para)
    (let ((in-table? (and (not (null? *tabular-stack*))
                          (memv (car *tabular-stack*) '(:block)))))
      ;(when *in-para-p* (when *use-closing-p-tag-p* (emit "</p>")))
      ;(emit-newline)
      (when in-table?
        (emit "</td></tr><tr><td>")
        (emit-newline))
      (emit "<p>")
      (set! *in-para-p* #t)
      ;too many bogus <p>s generated right now for this
      ;to work.  Plus, is it too expensive?
      ;(tex2page-string (get-toks "\\everypar"))
      )))

(define do-noindent
  (lambda ()
    (do-end-para)
    (emit-newline)
    (emit "<p class=noindent>")
    (set! *in-para-p* #t)))

(define (do-indent)
  (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
    (emit "<span style=\"margin-left: ")
    (emit parindent)
    (emit "pt\"></span>")))

(define do-para-nopadding
  (lambda ()
    (do-end-para)
    (emit-newline)
    (emit "<p class=nopadding>")
    (set! *in-para-p* #t)))

(define do-maketitle
  (lambda ()
    (do-end-para)
    (bgroup)
    (tex2page-string
     (string-append
      "\\let\\\\\\break"
      "\\let\\and\\break"
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
    (do-para)))

(define do-inputcss
  (lambda ()
    (ignorespaces)
    (let ((f (get-filename-possibly-braced)))
      (when (null? *stylesheets*)
        (flag-missing-piece ':stylesheets))
      (write-aux `(!stylesheet ,f)))))

(define do-csname
  (lambda ()
    (ignorespaces)
    (let loop ((r '()))
      (let ((c (snoop-actual-char)))
        (cond ((esc-char-p c)
               (let ((x (get-ctl-seq)))
                 (cond ((string=? x "\\endcsname")
                        ;(toss-back-char *invisible-space*)
                        (toss-back-char #\})
                        (for-each toss-back-string r)
                        (toss-back-char c)
                        (toss-back-char #\{)
                        (toss-back-string "TIIPcsname")
                        (toss-back-char c)
                        )
                       (else
                         (loop (cons (expand-ctl-seq-into-string x)
                                     r))
                         ))))
              (else (get-actual-char)
                (loop (cons (string c) r))))))))

(define do-saved-csname
  (lambda ()
    (let ((x (get-peeled-group)))
      (do-tex-ctl-seq x))))

(define do-cssblock
  (lambda ()
    (fluid-let ((*dumping-nontex-p* #t))
      (dump-till-end-env "cssblock" *css-port*))))

(define link-stylesheets
  (lambda ()
    (let ((link-it (lambda (css)
                     (emit "<link rel=\"stylesheet\" href=\"")
                     (emit css)
                     (emit "\" />")
                     (emit-newline))))
      (for-each link-it *stylesheets*)
      (link-it (string-append *jobname* *css-file-suffix*)))))

(define (link-scripts)
  (let ((link-it (lambda (jsf)
                   (emit "<script src=\"")
                   (emit jsf)
                   (emit "\"></script>")
                   (emit-newline))))
    (for-each link-it *scripts*)))

;sections

(define increment-section-counter
  (lambda (seclvl nonum?)
    (unless nonum?
      ;increment the counter for seclvl.
      ;if counter not set, init it to 1
      (table-put! *section-counters* seclvl
        (+ 1 (table-get *section-counters* seclvl 0))))
    ;zero the counters of all section levels below
    ;current level -- except 0, which doesn't change
    ;even when -1 changes
    (table-for-each *section-counters*
      (lambda (k v)
        (when (and (> k seclvl) (> k 0))
            (table-put! *section-counters* k 0))))
    ;zero footnote counter if new chapter
    (when (= seclvl 0)
      (set-gcount! "\\footnotenumber" 0))
    ;zero all the theorem-counters that hang off of
    ;this section level
    (for-each
      (lambda (counter-name)
        (set!counter.value
          (table-get *dotted-counters* counter-name)
          0))
      (table-get *section-counter-dependencies* seclvl '()))))

(define section-counter-value
  (lambda (seclvl)
    (if (= seclvl -1)
        (number-to-roman (table-get *section-counters* -1) #t)
        (let ((i (if *using-chapters-p* 0 1)))
          (let ((outermost-secnum
                  (let ((n (table-get *section-counters* i 0)))
                    (if *inside-appendix-p*
                        (string (integer->char (+ (char->integer #\A) -1
                                                  n)))
                        (number->string n)))))
            (let loop ((i (+ i 1)) (r outermost-secnum))
              (if (> i seclvl) r
                  (loop (+ i 1)
                    (string-append r "."
                      (number->string
                        (table-get *section-counters* i 0)))))))))))

(define section-ctl-seq?
  (lambda (s)
    (cond ((string=? s "\\sectiond")
           (string->number (ungroup (get-token))))
          ((string=? s "\\part") -1)
          ((string=? s "\\chapter") 0)
          (else
            (let ((n (string-length s)))
              (cond ((< n 8) #f)
                    ((and (>= n 10)
                          (string=? (substring s (- n 9) n) "paragraph"))
                     (let ((n-9 (- n 9)))
                       (let loop ((i 1) (i+3 4) (k 4))
                         (cond ((> i+3 n-9) k)
                               ((string=? (substring s i i+3) "sub")
                                (loop i+3 (+ i+3 3) (+ k 1)))
                               (else #f)))))
                    ((string=? (substring s (- n 7) n) "section")
                     (let ((n-7 (- n 7)))
                       (let loop ((i 1) (i+3 4) (k 1))
                         (cond ((> i+3 n-7) k)
                               ((string=? (substring s i i+3) "sub")
                                (loop i+3 (+ i+3 3) (+ k 1)))
                               (else #f)))))
                    (else #f)))))))

(define do-heading
  (lambda (seclvl)
    (let* ((starred? (cond ((char=? (snoop-actual-char) #\*)
                            (get-actual-char) #t)
                           (else #f)))
           (too-deep? (let ((secnumdepth (get-gcount "\\secnumdepth")))
                        (cond ((< secnumdepth -1) #f)
                              ((> seclvl secnumdepth) #t)
                              (else #f))))
           (nonum? (or starred? too-deep?))
           (header (fluid-let ((*tabular-stack* (list ':header)))
                     (tex-string-to-html-string (get-group)))))
      (do-heading-help seclvl starred? nonum? #f #f header))))

(define do-heading-help
  (lambda (seclvl starred? nonum? notoc? lbl-val header)
    (write-aux `(!default-title ,header))
    (when (<= seclvl 0)
      (!using-chapters)
      (when (and (eqv? *tex-format* ':latex) (< (get-gcount "\\secnumdepth") -1))
        (set-gcount! "\\secnumdepth" 2))
      (cond ((or (> *html-page-count* 0) (tex2page-flag-boolean "\\TIIPtitleused"))
             (do-eject))
            (else (tex-gdef-0arg "\\TIIPtitleused" "1")
                  (do-para))))
    (when (and (= seclvl 1) (tex2page-flag-boolean "\\TZPslides"))
      (do-eject))
    (increment-section-counter seclvl nonum?)
    (when lbl-val (set! nonum? #f))
    (unless lbl-val
      (set! lbl-val
        (if nonum? "IGNORE" (section-counter-value seclvl))))
    (let* ((htmlnum (max 1 (min 6 (if *using-chapters-p*
                                      (+ seclvl 1) seclvl))))
           (lbl (string-append *html-node-prefix*
                               (case seclvl
                                 ((-1) "part") ((0) "chap") (else "sec"))
                               "_"
                               (if nonum? (gen-temp-string) lbl-val))))
      (unless #f ;nonum?
        (tex-def-0arg "\\TIIPcurrentnodename" lbl)
        (tex-def-0arg "\\@currentlabel" lbl-val)
        )
      (do-end-para)
      (emit-anchor lbl)
      (emit-newline)
      (ignore-all-whitespace)
      (emit "<h")
      (emit htmlnum)
      (case seclvl
        ((-1) (emit " class=part align=center"))
        ((0) (emit " class=chapter"))
        (else (emit " class=section")))
      (emit ">")
      (let ((write-to-toc? ;kludge?
              (and (not notoc?) *toc-page*
                   (not (and (eqv? *tex-format* ':latex)
                             (string=? header "Contents"))))))
        (when (eqv? *tex-format* ':latex)
          (case seclvl
            ((-1)
             (emit "<div class=partheading>")
             (if nonum? (emit-nbsp 1)
                 (begin
                   (when write-to-toc?
                     (emit-page-node-link-start *toc-page*
                                                (string-append
                                                  *html-node-prefix*
                                                  "toc_" lbl)))
                   (tex2page-string "\\partname")
                   (emit " ")
                   (emit lbl-val)
                   (when write-to-toc? (emit-link-stop))))
             (emit "</div><br>")
             (emit-newline))
            ((0)
             (emit-newline)
             (emit "<div class=chapterheading>")
             (if nonum? (emit-nbsp 1)
                 (begin
                   (when write-to-toc?
                     (emit-page-node-link-start
                       *toc-page* (string-append *html-node-prefix*
                                                 "toc_" lbl)))
                   (tex2page-string
                     (if *inside-appendix-p*
                         "\\appendixname" "\\chaptername"))
                   (emit " ")
                   (emit lbl-val)
                   (when write-to-toc? (emit-link-stop))))
             (emit "</div><br>")
             (emit-newline))))
        (when write-to-toc?
          (emit-page-node-link-start
            *toc-page* (string-append
                         *html-node-prefix*
                         "toc_" lbl)))
        (unless (or (and (eqv? *tex-format* ':latex) (<= seclvl 0)) nonum?)
          (emit lbl-val)
          (emit-nbsp 2))
        ;(fluid-let ((*tabular-stack* (list ':header)))
        ;  (tex2page-string header))
        (emit header)
        (when write-to-toc? (emit-link-stop))
        (emit "</h")
        (emit htmlnum)
        (emit ">")
        (do-noindent)
        ;
        (let ((tocdepth (get-gcount "\\tocdepth")))
          (when (and write-to-toc?
                     (not (and (eqv? *tex-format* ':latex) starred?))
                     (or (< tocdepth -1)
                         (<= seclvl tocdepth)))
            (write-aux
              `(!toc-entry
                 ,(if (= seclvl -1) -1
                      (if *using-chapters-p* seclvl (- seclvl 1)))
                 ,lbl-val
                 ,*html-page-count* ,lbl ,header))))
        )
      (when *recent-node-name*
        (do-label-aux *recent-node-name*)
        (set! *recent-node-name* #f))
      ;(if (= seclvl -1) (do-eject))
      )))

(define section-type-to-depth
  (lambda (sectype)
    (cond ((string->number sectype))
          ((string=? sectype "chapter") 0)
          ((string=? sectype "section") 1)
          ((string=? sectype "subsection") 2)
          ((string=? sectype "subsubsection") 3)
          ((string=? sectype "paragraph") 4)
          ((string=? sectype "subparagraph") 5)
          (else 3) ;?
          )))

(define do-write-to-toc-aux
  (lambda (seclvl secnum sectitle)
    (let ((node-name (string-append *html-node-prefix* "sec_"
                                    (if (string=? secnum "")
                                        (gen-temp-string) secnum))))
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (tex-def-0arg "\\@currentlabel" secnum)
      (emit-anchor node-name) (emit-newline)
      (write-aux
        `(!toc-entry
           ,seclvl
           ,secnum
           ,*html-page-count*
           ,node-name
           ,sectitle)))))

(define do-writenumberedcontentsline
  (lambda ()
    (let ((toc (get-peeled-group)))
      (unless (string=? toc "toc")
        (terror 'do-writenumberedcontentsline "only #1=toc supported"))
      (do-writenumberedtocline))))

(define do-writenumberedtocline
  (lambda ()
    (let* ((seclvl (section-type-to-depth (get-peeled-group)))
           (secnum (tex-string-to-html-string (get-group)))
           (sectitle (tex-string-to-html-string (get-group))))
      (do-write-to-toc-aux seclvl secnum sectitle))))

(define do-addcontentsline
  (lambda ()
    (let* ((toc (get-peeled-group)))
      (unless (string=? toc "toc")
        (terror 'do-addcontentsline "only #1=toc supported"))
      (let* ((seclvl (section-type-to-depth (get-peeled-group)))
             (sectitle (tex-string-to-html-string (get-group))))
        (write-aux
          `(!toc-entry
             ,(if (= seclvl -1) -1
                  (if *using-chapters-p* seclvl (- seclvl 1)))
             ,(ctl-seq-no-arg-expand-once "\\@currentlabel") ;?
             ,*html-page-count*
             ,(ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename")
             ,sectitle))))))

(define do-documentclass
  (lambda ()
    (probably-latex)
    (get-bracketed-text-if-any)
    (let ((x (get-peeled-group)))
      (when (ormap (lambda (z) (string=? x z))
              '("report" "book"))
        (!using-chapters)))))

(define get-till-par
  (lambda ()
    (let loop ((r '()) (newline? #f))
      (let ((c (get-actual-char)))
        (cond ((or (eof-object? c)
                   (and newline? (char=? c #\newline)))
               (list->string (nreverse r)))
              (newline?
                ;c can't be #\newline
                (if (char-whitespace? c) (loop r #t)
                    (loop (cons c (cons #\space r)) #f)))
              ((char=? c #\newline) (loop r #t))
              (else (loop (cons c r) #f)))))))

(define do-beginsection
  (lambda ()
    (ignorespaces)
    (let ((header (let ((*tabular-stack* (list ':header)))
                    (tex-string-to-html-string (get-till-par)))))
      (do-heading-help 1 #f #t #t #f header))))

(define (do-beginchapter)
    (ignorespaces)
    (let* ((chapno (tex-string-to-html-string
                     (get-till-char #\space)))
           (header (begin (ignorespaces)
                          (let ((*tabular-stack* (list ':header)))
                            (tex-string-to-html-string
                             (get-till-par))))))
      (tex-gdef-0arg "\\chapno" chapno)
      (set-gcount! "\\subsecno" 0)
      (set-gcount! "\\footnotenumber" 0)
      (when (string=? chapno "") (set! chapno #f))
      (do-heading-help 0 #f #t #f chapno header)))

(define do-opmac-heading
  (lambda (seclvl)
    (ignorespaces)
    (let ((header (fluid-let ((*tabular-stack* (list ':header)))
                    (tex-string-to-html-string (get-till-par)))))
      (let ((nonum? *opmac-nonum-p*)
            (notoc? *opmac-notoc-p*))
        (set! *opmac-nonum-p* #f)
        (set! *opmac-notoc-p* #f)
        (do-heading-help seclvl #f nonum? notoc? #f header)))))

(define do-opmac-sec
  (lambda ()
    (if *math-mode-p*
        (toss-back-string "\\TIIPsec")
        (do-opmac-heading 1))))

(define do-appendix
  (lambda ()
    (unless *inside-appendix-p*
      (set! *inside-appendix-p* #t)
      (table-put! *section-counters*
        (if *using-chapters-p* 0 1) 0))))

;HTML's approximation of the TeX box

(define do-table-plain
  (lambda ()
    (do-end-para)
    (emit "<table width=100%><tr><td>")))

(define do-end-table-plain
  (lambda ()
    (do-end-para)
    (emit "</td></tr></table>")
    ))

;tables & figures

(define do-table/figure
  (lambda (type)
    (do-end-para)
    (bgroup)
    (when (and (eqv? type ':figure) (char=? (snoop-actual-char) #\*))
      (get-actual-char))
    (set! *tabular-stack* (cons type *tabular-stack*))
    (get-bracketed-text-if-any)
    (let ((tbl-tag (string-append *html-node-prefix*
                                  (if (eqv? type ':table) "tbl_" "fig_")
                                  (gen-temp-string))))
      (tex-def-0arg "\\TIIPcurrentnodename" tbl-tag)
      (emit-anchor tbl-tag) (emit-newline)
      ;(emit "<hr>")
      (emit "<div class=")
      (emit type)
      (emit " align=")
      (emit *display-justification*)
      (emit "><table width=100%><tr><td align=")
      (emit *display-justification*)
      (emit ">")
      ;(do-table-plain)
      )))

(define pop-tabular-stack
  (lambda (type)
    (if (null? *tabular-stack*)
        (terror 'pop-tabular-stack
               "Bad environment closer: "
               type)
        (set! *tabular-stack* (cdr *tabular-stack*)))))

(define do-end-table/figure
  (lambda (type)
    (when (and (eqv? type ':figure)
               (char=? (snoop-actual-char) #\*))
      (get-actual-char))
    (do-end-para)
    (emit "</td></tr>")
    (emit "</table>")
    (emit "</div>")
    ;(emit "<hr>")
    (pop-tabular-stack type)
    (egroup)
    (do-para)))

(define bump-dotted-counter
  (lambda (name)
    (let* ((counter (table-get *dotted-counters* name))
           (new-value (+ 1 (counter.value counter))))
      (set!counter.value counter new-value)
      (let ((num (string-append
                 (cond ((counter.within counter)
                        => (lambda (sec-num)
                             (string-append
                              (section-counter-value sec-num)
                              ".")))
                       (else ""))
                 (number->string new-value))))
       (tex-def-0arg "\\@currentlabel" num)
        num))))

(define do-caption
  (lambda ()
    (do-end-para)
    (let* ((i-fig (list-position ':figure *tabular-stack*))
           (i-tbl (list-position ':table *tabular-stack*))
           (type (cond ((and (not i-fig) (not i-tbl))
                        (terror 'do-caption "Mislaid \\caption"))
                       ((not i-fig) ':table)
                       ((not i-tbl) ':figure)
                       ((< i-fig i-tbl) ':figure)
                       ((< i-tbl i-fig) ':table)
                       (else (terror 'do-caption "cant happen"))))
           (counter-name (if (eqv? type ':table) "table" "figure"))
           (caption-title (if (eqv? type ':table) "\\tablename" "\\figurename"))
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
      (emit "<tr><td>"))))

(define do-marginnote
  (lambda ()
    (emit "<span class=marginnote>")
    (tex2page-string (get-group))
    (emit "</span>")))

(define do-marginpar
  (lambda ()
    (get-bracketed-text-if-any)
    (emit "<table align=left border=2><tr><td>")
    (tex2page-string (get-group))
    (emit "</td></tr></table>")))

(define do-minipage
  (lambda ()
    (get-bracketed-text-if-any)
    (get-group)
    (let ((in-table? (and (not (null? *tabular-stack*))
                          (memv (car *tabular-stack*) '(:block :figure :table)))))
      (if in-table? (emit "</td><td>")
          (begin (do-para) (do-end-para)))
      (emit "<div align=left>")
      (set! *tabular-stack* (cons ':minipage *tabular-stack*)))))

(define do-endminipage
  (lambda ()
    (pop-tabular-stack ':minipage)
    (let ((in-table? (and (not (null? *tabular-stack*))
                          (memv (car *tabular-stack*) '(:block :figure :table)))))
      (emit "</div>")
      (if in-table? (emit "</td><td>") (do-para)))))

(define do-tabbing
  (lambda ()
    (set! *tabular-stack*
      (cons ':tabbing *tabular-stack*))
    (do-para)))

(define do-end-tabbing
  (lambda ()
    (pop-tabular-stack ':tabbing)
    (do-para)))

(define do-equation
  (lambda (type)
    (cond ((and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                    (tex2page-flag-boolean "\\TZPmathimage"))
                (not *temporarily-use-utf8-for-math-p*))
           (do-latex-env-as-image
             (case type
               ((:equation) "equation")
               ((:align) "align")
               (else "eqnarray"))
            ':display))
          (else
           (do-end-para)
           (bgroup)
           (when (eqv? type ':align) (set! type ':eqnarray));?
           (when (and (eqv? type ':eqnarray) (eat-star))
             (set! type ':eqnarray*))
           (set! *tabular-stack* (cons type *tabular-stack*))
           (set! *math-mode-p* #t)
           (set! *in-display-math-p* #t)
           (let ((eqn-tag (string-append *html-node-prefix* "eqn_"
                                         (gen-temp-string))))
             (tex-def-0arg "\\TIIPcurrentnodename" eqn-tag)
             (emit-anchor eqn-tag) (emit-newline)
             (unless (eqv? type ':eqnarray*)
               (set! *equation-number* (bump-dotted-counter "equation")))
             (emit "<div align=")
             (emit *display-justification*)
             (emit "><table width=100%>") (emit-newline)
             (emit "<tr><td align=")
             (emit (if (eqv? type ':equation) "center" "right"))
             (emit ">")
             )))))

(define do-end-equation
  (lambda ()
    (do-end-para)
    (emit "</td>")
    (unless (or (and (not (null? *tabular-stack*))
                     (eqv? (car *tabular-stack*) ':eqnarray*))
                (not *equation-numbered-p*))
      (emit "<td>(")
      (emit *equation-number*)
      (emit ")</td>"))
    (emit "</tr>")
    (emit-newline)
    (emit "</table></div>")
    (pop-tabular-stack ':equation)
    (set! *math-mode-p* #f)
    (set! *in-display-math-p* #f)
    (egroup)
    (set! *equation-numbered-p* #t)
    (set! *equation-position* 0)
    (do-para)))

(define do-integral
  (lambda ()
    (if (or (not *in-display-math-p*) *math-script-mode-p*)
        (emit "&#x222b;")
      (let ((affixes-already-read '()))
        (emit "<span style=\"font-size: 200%; position: relative; top: .25ex;\">&#x222b;</span>")
        (let loop ((i 2))
          (unless (= i 0)
            (ignorespaces)
            (let ((c (snoop-actual-char)))
              (when (and (memv c '(#\_ #\^))
                         (not (memv c affixes-already-read)))
                (set! affixes-already-read (cons c affixes-already-read))
                (get-actual-char)
                (when (= i 0)
                  (emit (kern ".16667em")))
                (fluid-let ((*math-script-mode-p* #t))
                (let ( (s  (get-token)))
                  (emit "<span style=\"font-size: 85%; position: relative; ")
                  (emit (case c
                          ((#\_)  "top: 2.5ex; ")
                          ((#\^) "bottom: 3ex; ")))
                  (emit "\">")
                  (tex2page-string s)
                  (emit "</span>")))))
            (loop (- i 1))))))))

(define do-nonumber
  (lambda ()
    (set! *equation-numbered-p* #f)))

(define indent-n-levels
  (lambda (n)
    (let loop ((i -1))
      (unless (>= i n)
        (emit-nbsp 1)
        (emit " ")
        (emit-nbsp 1)
        (emit " ")
        (loop (+ i 1))))))

(define do-toc
  (lambda ()
    (fluid-let ((*subjobname* (string-append *jobname* *toc-file-suffix*))
                (*img-file-count* 0)
                (*imgdef-file-count* 0))
      (when (eqv? *tex-format* ':latex)
        (tex2page-string
         (if *using-chapters-p*
             "\\chapter*{\\contentsname}" "\\section*{\\contentsname}")))
      (emit-anchor (string-append *html-node-prefix* "toc"))
      (!toc-page *html-page-count*)
      (write-aux `(!toc-page ,*html-page-count*))
      (cond ((null? *toc-list*)
             (flag-missing-piece ':toc)
             (non-fatal-error "Table of contents not generated; rerun TeX2page"))
            (else
              (do-noindent)
             (let ((tocdepth (get-gcount "\\tocdepth")))
               (for-each
                (lambda (x)
                  (let* ((lvl (tocentry.level x))
                         (secnum (tocentry.number x))
                         (seclabel (tocentry.label x))
                         (subentries?
                          (or (= lvl -1) ;always bold parts
                              (and (= lvl 0)
                                   (or (< tocdepth -1)
                                       (and *using-chapters-p*
                                            (> tocdepth 0))
                                       (and (not *using-chapters-p*)
                                            (> tocdepth 1)))))))
                    (when subentries?
		     (if *tex-like-layout-p*
			 (do-bigskip ':medskip)
			 (do-para))
                      (do-noindent)
                      (emit "<b>")
                      (emit-newline))
                    (indent-n-levels lvl)
                    (emit-anchor (string-append *html-node-prefix* "toc_" seclabel))
                    (emit-page-node-link-start
                     (tocentry.page x)
                     seclabel)
                    ;(when (= lvl -1) (emit "Part "))
                    (unless (or (string=? secnum "") (string=? secnum "IGNORE"))
                      (emit secnum)
                      ;(tex2page-string secnum)
                      ;(emit #\.)
                      (emit-nbsp 2))
                    (fluid-let ((*tabular-stack* (list ':header)))
                      ;tex2page-string
                      (emit (tocentry.header x)))
                    (emit-link-stop)
                    (when subentries?
                      (emit "</b>"))
                    (emit "<br>")
                    (emit-newline)))
                *toc-list*))))
      (emit-anchor (string-append *html-node-prefix* "toc_end")))))

(defstruct footnotev mark text tag caller)

(define do-numbered-footnote
  (lambda ()
    (do-footnote-aux #f)))

(define do-symfootnote
  (lambda ()
    (set! *footnote-sym* (+ *footnote-sym* 1))
    (do-footnote-aux
      (number->footnote-symbol *footnote-sym*)
      )))

(define tex-string-to-html-string
  (lambda (ts)
    (let ((tmp-port (open-output-string)))
      (fluid-let ((*html* tmp-port))
        (tex2page-string ts))
      (get-output-string tmp-port))))

(define number->footnote-symbol
  (let ((symlist #f))
    (lambda (n)
      (unless symlist
        (set! symlist
          (fluid-let ((*temporarily-use-utf8-for-math-p* #t ))
            (map tex-string-to-html-string
                 '("*" "\\dag" "\\ddag" "\\S" "\\P"
                   "$\\Vert$" "**" "\\dag\\dag" "\\ddag\\ddag")))))
      (list-ref symlist
                (modulo (- n 1) 9)))))

(define do-plain-footnote
  (lambda ()
    (do-footnote-aux
      (fluid-let ((*temporarily-use-utf8-for-math-p* #t))
        (tex-string-to-html-string
          (get-token))))))

(define do-footnote
  (lambda ()
    ((if (eqv? *tex-format* ':latex)
        do-numbered-footnote
        do-plain-footnote))))

(define do-footnote-aux
  (lambda (fnmark)
    (let* ((fnno #f)
           (fnlabel (gen-temp-string))
           (fntag (string-append *html-node-prefix* "footnote_" fnlabel))
           (fncalltag (string-append *html-node-prefix*
                                     "call_footnote_" fnlabel)))
      (unless fnmark
        (set! fnno (+ (get-gcount "\\footnotenumber") 1))
        (set-gcount! "\\footnotenumber" fnno)
        ;fnmark should really be $^fnno$ but we will use this
        ;kludge because for numbered footnotes it is nicer on
        ;most browsers to push the <a href> inside the <sup>
        (set! fnmark (number->string fnno)))
      (emit-anchor fncalltag)
      (when fnno (emit "<sup><small>"))
      (emit-page-node-link-start #f fntag)
      (emit fnmark)
      (emit-link-stop)
      (when fnno (emit "</small></sup>"))
      (do-vfootnote-aux fnmark fncalltag fntag))))

(define do-vfootnote
  (lambda ()
    (do-vfootnote-aux
      (fluid-let ((*temporarily-use-utf8-for-math-p* #t))
        (tex-string-to-html-string (get-token)))
      #f ;no footnote call tag
      #f ;no footnote tag
      )))

(define do-vfootnote-aux
  (lambda (fnmark fncalltag fntag)
    (ignorespaces) ;needed?
    (unless (char=? (get-actual-char) #\{)
      (terror 'do-vfootnote-aux "Missing {"))
    (bgroup)
    (let ((old-html *html*)
          (fn-tmp-port (open-output-string)))
      (set! *html* fn-tmp-port)
      (when fncalltag
        (tex-def-0arg "\\TIIPcurrentnodename" fntag)
        (tex-def-0arg "\\@currentlabel" fnmark))
      (add-aftergroup-to-top-frame
        (lambda ()
          ;(close-output-port *html*)
          (set! *footnote-list*
            (cons (make-footnotev
                    'mark fnmark
                    'text (get-output-string fn-tmp-port)
                    'tag fntag
                    'caller fncalltag)
                  *footnote-list*))
          (set! *html* old-html))))))

(define output-footnotes
  (lambda ()
    (let ((n (length *footnote-list*)))
      (unless (= n 0)
        ;(do-end-para)
        (emit "<div class=footnoterule><hr></div>")
        (do-para) (do-end-para)
        (emit "<div class=footnote>")
        (let loop ((i (- n 1)))
          (unless (< i 0)
            (let* ((fv (list-ref *footnote-list* i))
                   (fnmark (footnotev.mark fv))
                   (fnno (string->number fnmark))
                   (fncalltag (footnotev.caller fv)))
              (do-para)
              (when fncalltag
                (emit-anchor (footnotev.tag fv))
                (when fnno (emit "<sup><small>"))
                (emit-page-node-link-start #f fncalltag))
              (emit fnmark)
              (when fncalltag
                (emit-link-stop)
                (when fnno (emit "</small></sup>")))
              (emit " ")
              (emit (footnotev.text fv))
              (do-end-para)
              (loop (- i 1)))))
        (emit "</div>")
        (emit-newline)
        ))))

;color

(define rgb-dec-to-rrggbb
  (let ((f (lambda (x)
             (let* ((n (inexact->exact (round (* 1.0 x))))
                    (s (number->string n 16)))
               (if (< n 16) (string-append "0" s) s)))))
    (lambda (r g b)
      (string-append "#" (f r) (f g) (f b)))))

(define (rgb-frac-to-rrggbb r g b)
  (rgb-dec-to-rrggbb (* r 255) (* g 255) (* b 255)))

(define cmyk-to-rrggbb
  ;from Dan Luecking's mfpic/metapost/grafbase.mp
  (let ((f (lambda (x k)
              (- 1 (min (max (+ x k) 0) 1)))))
    (lambda (c m y k)
      (rgb-frac-to-rrggbb (f c k) (f m k) (f y k)))))

(define (hsb-to-rrggbb hue saturation brightness)
  (let* ((red 0) (green 0) (blue 0)
                 (hue6 (* 6 hue))
                 (i (inexact->exact (floor hue6)))
                 (f (- hue6 i)))
    (case i
      ((0) (set! red 0) (set! green (- 1 f)) (set! blue 1))
      ((1) (set! red f) (set! green 0) (set! blue 1))
      ((2) (set! red 1) (set! green 0) (set! blue (- 1 f)))
      ((3) (set! red 1) (set! green f) (set! blue 0))
      ((4) (set! red (- 1 f)) (set! green 1) (set! blue 0))
      ((5) (set! red 0) (set! green 1) (set! blue f))
      ((6) (set! red 0) (set! green 1) (set! blue 1))
      (else (terror 'hsb-to-rrggbb "Can't happen.")))
    (let ((fu (lambda (x) (* brightness
                             (- 1 (* saturation x))))))
      (rgb-frac-to-rrggbb (fu red) (fu green) (fu blue)))))

(define (wavelength-to-rrggbb w)
  (let ((hue (* 1/6
                (cond ((<= w 362.857) 5)
                      ((< w 440) (+ 4 (/ (- w 440) -60)))
                      ((< w 490) (- 4 (/ (- w 440) 50)))
                      ((< w 510) (+ 2 (/ (- w 510) -20)))
                      ((< w 580) (- 2 (/ (- w 510) 70)))
                      ((< w 645) (/ (- w 645) -65))
                      (else 0))))
        (brightness
          (cond ((<= w 362.857) 0)
                ((< w 420) (+ 0.3 (* 0.7 (/ (- w 380) 40))))
                ((<= w 700) 1)
                ((< w 814.285) (+ 0.3 (* 0.7 (/ (- w 780) -80))))
                (else 0))))
    (hsb-to-rrggbb hue 1 brightness)))

(define (read-color-as-rrggbb model)
  (case model
    ((:cmy) (bgroup)
            (call-with-input-string
              (tex-string-to-html-string
                (string-append "\\defcsactive\\,{ }" (get-token)))
              (lambda (i)
                (egroup)
                (let* ((c (read i))
                       (m (read i))
                       (y (read i)))
                  (ignorespaces)
                  (rgb-frac-to-rrggbb (- 1 c) (- 1 m) (- 1 y))))))
    ((:cmyk) (bgroup)
             (call-with-input-string
               (tex-string-to-html-string
                 (string-append "\\defcsactive\\,{ }" (get-token)))
               (lambda (i)
                 (egroup)
                 (let* ((c (read i))
                        (m (read i))
                        (y (read i))
                        (k (read i)))
                   (ignorespaces)
                   (cmyk-to-rrggbb c m y k)))))
    ((:rgb) (bgroup)
            (call-with-input-string
              (tex-string-to-html-string
                (string-append "\\defcsactive\\,{ }" (get-token)))
              (lambda (i)
                (egroup)
                (let* ((r (read i))
                       (g (read i))
                       (b (read i)))
                  (ignorespaces)
                  (rgb-frac-to-rrggbb r g b)))))
    ((:rgb255)
     (bgroup)
     (call-with-input-string
       (tex-string-to-html-string
         (string-append "\\defcsactive\\,{ }" (get-token)))
       (lambda (i)
         (egroup)
         (let* ((r (read i))
                (g (read i))
                (b (read i)))
           (ignorespaces)
           (rgb-dec-to-rrggbb r g b)))))
    ((:gray)
     (call-with-input-string
       (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((g (read i)))
           (ignorespaces)
           (cmyk-to-rrggbb 0 0 0 (- 1 g))))))
    ((:gray15)
     (call-with-input-string
       (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((g (read i)))
           (ignorespaces)
           (cmyk-to-rrggbb 0 0 0 (- 1 (/ g 15)))))))
    ((:html)
     (call-with-input-string
       (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((rrggbb (atom-to-6hex (read i))))
           (ignorespaces)
           rrggbb))))
    ((:hsb)
     (bgroup)
     (call-with-input-string
       (tex-string-to-html-string
         (string-append "\\defcsactive\\,{ }" (get-token)))
       (lambda (i)
         (egroup)
         (let* ((h (read i))
                (s (read i))
                (b (read i)))
           (ignorespaces)
           (hsb-to-rrggbb h s b)))))
    ((:hsb360)
     (bgroup)
     (call-with-input-string
       (tex-string-to-html-string
         (string-append "\\defcsactive\\,{ }" (get-token)))
       (lambda (i)
         (egroup)
         (let* ((h (read i))
                (s (read i))
                (b (read i)))
           (ignorespaces)
           (hsb-to-rrggbb (/ h 360) s b)))))
    ((:hsb240)
     (bgroup)
     (call-with-input-string
       (tex-string-to-html-string
         (string-append "\\defcsactive\\,{ }" (get-token)))
       (lambda (i)
         (egroup)
         (let* ((h (read i))
                (s (read i))
                (b (read i)))
           (ignorespaces)
           (hsb-to-rrggbb (/ h 240) (/ s 240) (/ b 240))))))
    ((:wave)
     (call-with-input-string
       (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((w (read i)))
           (ignorespaces)
           (wavelength-to-rrggbb w)))))
    (else (let* ((name (get-peeled-group))
                 (c (lassoc name *color-names* string=?)))
            (ignorespaces)
            (if c (cdr c) name)))))

(define (color-model-to-keyword model)
  (cond ((not model) ':colornamed)
        ((string=? model "rgb") ':rgb)
        ((string=? model "RGB") ':rgb255)
        ((string=? model "cmyk") ':cmyk)
        ((string=? model "cmy") ':cmy)
        ((string=? model "gray") ':gray)
        ((string=? model "Gray") ':gray15)
        ((string=? model "HTML") ':html)
        ((string=? model "hsb") ':hsb)
        ((string=? model "Hsb") ':hsb360)
        ((string=? model "HSB") ':hsb240)
        ((string=? model "wave") ':wave)
        (else ':colornamed)))

(define (do-color)
  (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
    (do-switch model)))

(define (do-pagecolor)
  (let* ((model (color-model-to-keyword (get-bracketed-text-if-any)))
         (color (read-color-as-rrggbb model)))
    (display "body { background-color: " *css-port*)
    (display color *css-port*)
    (display "; }" *css-port*)
    (newline *css-port*)))

(define (atom-to-6hex x)
  (let ((htmlcolor (string-upcase
                     (cond ((symbol? x) (symbol->string x))
                           ((number? x) (number->string x))
                           (else (terror 'atom-to-6hex "Misformed argument."))))))
    (string-append "#" (case (string-length htmlcolor)
                         ((1) "00000")
                         ((2) "0000")
                         ((3) "000")
                         ((4) "00")
                         ((5) "0")
                         (else "")) htmlcolor)))

(define (do-colorbox)
  (let* ((color (get-group))
         (text (get-peeled-group)))
    (toss-back-char #\})
    (toss-back-string text)
    (toss-back-string color)
    (toss-back-string "\\bgcolor")
    (toss-back-char #\{)))

(define (do-definecolor)
  (let* ((name (get-peeled-group))
         (model (color-model-to-keyword (get-peeled-group))))
    (set! *color-names*
      (cons (cons name (read-color-as-rrggbb model))
            *color-names*))))

;Groups

(define (do-switch sw)
  ;(ignorespaces)
  (unless *outputting-external-title-p*
    (add-postlude-to-top-frame
      (case sw
        ((:rm)
         (when *math-mode-p*
           (let ((old-math-font *math-font*))
             (set! *math-font* ':rm)
             (lambda ()
               (set! *math-font* old-math-font)))))
        ((:em)
         (emit "<em>")
         (lambda () (emit "</em>")))
        ((:it)
         (emit "<i>")
         (lambda () (emit "</i>")))
        ((:bf :strong)
         (emit "<strong>")
         (lambda () (emit "</strong>")))
        ((:sl)
         (emit "<span style=\"font-style: oblique\">")
         (lambda () (emit "</span>")))
        ((:sf)
         (emit "<span style=\"font-family: sans-serif\">")
         (lambda () (emit "</span>")))
        ((:tt)
         (let ((old-ligatures-p *ligatures-p*))
           (set! *ligatures-p* #f)
           (emit "<tt>")
           (lambda () (emit "</tt>")
             (set! *ligatures-p* old-ligatures-p))))
        ((:sc :scshape)
         (let ((old-in-small-caps-p *in-small-caps-p*))
           (set! *in-small-caps-p* #t)
           (lambda ()
             (set! *in-small-caps-p* old-in-small-caps-p))))
        ;((ormap (lambda (z) (string=? sw z))
        ;        '("\\sevenrm" "\\small" "\\scriptsize"))
        ; (emit "<small>")
        ; (lambda () (emit "</small>")))
        ((:span)
         (emit "<span ")
         (emit (get-peeled-group))
         (emit ">")
         (lambda () (emit "</span>")))
        ((:div)
         (emit "<div ")
         (emit (get-peeled-group))
         (emit ">")
         (lambda () (emit "</div>")))
        ((:tiny)
         (emit "<span class=tiny>")
         (lambda () (emit "</span>")))
        ((:scriptsize)
         (emit "<span class=scriptsize>")
         (lambda () (emit "</span>")))
        ((:footnotesize :fiverm)
         (emit "<span class=footnotesize>")
         (lambda () (emit "</span>")))
        ((:small :sevenrm)
         (emit "<span class=small>")
         (lambda () (emit "</span>")))
        ((:normalsize)
         (emit "<span class=normalsize>")
         (lambda () (emit "</span>")))
        ((:large)
         (emit "<span class=large>")
         (lambda () (emit "</span>")))
        ((:large-cap)
         (emit "<span class=largecap>")
         (lambda () (emit "</span>")))
        ((:large-up)
         (emit "<span class=largeup>")
         (lambda () (emit "</span>")))
        ((:huge)
         (emit "<span class=huge>")
         (lambda () (emit "</span>")))
        ((:huge-cap)
         (emit "<span class=hugecap>")
         (lambda () (emit "</span>")))
        ((:cmy :cmyk :rgb :rgb255 :gray :gray15 :html :hsb :hsb360 :hsb240 :wave :colornamed)
         (emit "<span style=\"color: ")
         (emit (read-color-as-rrggbb sw))
         (emit "\">")
         (lambda () (emit "</span>")))
        ((:bgcolor)
         (emit "<span style=\"background-color: ")
         (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
           (emit (read-color-as-rrggbb model)))
         (emit "\">")
         (lambda () (emit "</span>")))
        ((:strike)
         (emit "<strike>")
         (lambda () (emit "</strike>")))
        ((:narrower)
         (emit "<blockquote>")
         (lambda ()
           (emit "</blockquote>")))
        ((:raggedleft)
         (do-end-para)
         (emit "<div align=right>")
         (lambda ()
           (do-end-para)
           (emit "</div>") (do-para)))
        ((:oldstyle)
         (emit "<span class=oldstyle>")
         (lambda () (emit "</span>")))
        (else
          (emit "<span class=")
          (emit sw)
          (emit ">")
          (lambda ()
            (emit "</span>"))
          ;(terror 'do-switch "Unknown switch " sw)
          )))))

(define do-obeylines
  (lambda ()
    ;(do-para)
    (when (eqv? (snoop-actual-char) #\newline) (get-actual-char))
    (activate-cdef #\newline)
    (tex-def-char #\newline '() "\\TIIPpar" #f)
    ;(add-postlude-to-top-frame do-para)
    ))

(define do-obeyspaces
  (lambda ()
    (activate-cdef #\space)
    (tex-def-char #\space '() "\\TIIPnbsp" #f)))

(define do-obeywhitespace
  (lambda ()
    (do-obeylines)
    (do-obeyspaces)))

(define do-block
  (lambda (z)
    (do-end-para)
    (emit "<div ")
    (emit (case z
            ((:flushleft) "align=left")
            ((:flushright) "align=right")
            (else "align=center")))
    (emit ">")
    (set! *tabular-stack* (cons ':block *tabular-stack*))
    (emit "<table><tr><td>")
    (bgroup)
    (emit-newline)))

(define do-end-block
  (lambda ()
    (do-end-para)
    (egroup)
    (emit "</td></tr></table></div>")
    ;(do-para)
    (pop-tabular-stack ':block)
    (emit-newline)))

(define do-function
  (lambda (fn)
    (fluid-let ((*math-mode-p* *math-mode-p*))
      (cond (*outputting-external-title-p* #f)
            ((string=? fn "\\emph") (emit "<em>"))
            ((string=? fn "\\leftline") (do-end-para) (emit "<div align=left>"))
            ((string=? fn "\\centerline") (do-end-para) (emit "<div align=center>&#xa0;"))
            ((string=? fn "\\rightline") (do-end-para) (emit "<div align=right>&#xa0;"))
            ((string=? fn "\\underline") (emit "<u>"))
            ((string=? fn "\\textbf")
             (set! *math-mode-p* #f)
             (emit "<b>"))
            ((ormap (lambda (z) (string=? fn z))
                    '("\\textit" "\\textsl"))
             (set! *math-mode-p* #f)
             (emit "<i>"))
            ((string=? fn "\\textrm")
             (set! *math-mode-p* #f))
            ((string=? fn "\\texttt")
             (set! *math-mode-p* #f)
             (emit "<tt>"))
            (else (terror 'do-function "Unknown function " fn)))
      (bgroup)
      (tex2page-string (get-token))
      (egroup)
      (cond (*outputting-external-title-p* #f)
            ((string=? fn "\\emph") (emit "</em>"))
            ((string=? fn "\\rightline")
             (emit "</div>") (emit-newline)
             )
            ((ormap (lambda (z) (string=? fn z))
                    '("\\leftline" "\\centerline"))
             (do-end-para)
             (emit "&#xa0;</div>")
             (emit-newline)
             )
            ((string=? fn "\\underline") (emit "</u>"))
            ((string=? fn "\\textbf") (emit "</b>"))
            ((ormap (lambda (z) (string=? fn z))
                    '("\\textsl" "\\textit"))
             (emit "</i>"))
            ;\textrm does nothing
            ((string=? fn "\\texttt") (emit "</tt>"))))))

(define do-discretionary
  (lambda ()
    (tex2page-string (get-group))
    (get-group)
    (get-group)))

(define do-aftergroup
  (lambda ()
    (ignorespaces)
    (let ((z (get-ctl-seq)))
      (add-aftergroup-to-top-frame
       (lambda ()
         (toss-back-string z))))))

(define do-afterassignment
  (lambda ()
    (ignorespaces)
    (let ((z (get-ctl-seq)))
      (set! *afterassignment* z))))

(define do-space
  (lambda ()
    (emit #\space)))

(define do-tab
  (lambda ()
    (emit-nbsp 8)))

(define emit-nbsp
  (lambda (n)
    (let loop ((n n))
      (unless (<= n 0)
        (emit "&#xa0;") (loop (- n 1))))))

(define scaled-point-equivalent-of
  (lambda (unit)
    (case unit
      ((:sp) 1)
      ((:pt) 65536)
      ((:bp) (* (/ 72) (scaled-point-equivalent-of ':in)))
      ((:cc) (* 12 (scaled-point-equivalent-of ':dd)))
      ((:dd) (* (/ 1238 1157) (scaled-point-equivalent-of ':pt)))
      ((:em) (* 10 (scaled-point-equivalent-of ':pt))) ;kludge
      ((:ex) (* 4.5 (scaled-point-equivalent-of ':pt))) ; kludge
      ((:in) (* 72.27 (scaled-point-equivalent-of ':pt)))
      ((:mm) (* .1 (scaled-point-equivalent-of ':cm)))
      ((:cm) (* (/ 2.54) (scaled-point-equivalent-of ':in)))
      ((:pc) (* 12 (scaled-point-equivalent-of ':pt))))))

(define tex-length
  (lambda (num unit)
    (* num (scaled-point-equivalent-of unit))))

(define sp-to-ems
  (lambda (sp)
    (/ (/ sp 65536) 10.0)))

(define sp-to-pixels
  (lambda (sp)
    (inexact->exact
     (floor
      (/ sp 65536.0)))))

(define get-scaled-points
  (lambda ()
    (let ((n (or (get-real) 1)))
      (ignorespaces)
      (* n
         (if (esc-char-p (snoop-actual-char))
             (let ((x (get-ctl-seq)))
               (get-dimen x)
               )
             (let loop ()
               (cond ((eat-word "bp") (tex-length 1 ':bp))
                     ((eat-word "cc") (tex-length 1 ':cc))
                     ((eat-word "cm") (tex-length 1 ':cm))
                     ((eat-word "dd") (tex-length 1 ':dd))
                     ((eat-word "em") (tex-length 1 ':em))
                     ((eat-word "ex") (tex-length 1 ':ex))
                     ((eat-word "in") (tex-length 1 ':in))
                     ((eat-word "mm") (tex-length 1 ':mm))
                     ((eat-word "pc") (tex-length 1 ':pc))
                     ((eat-word "pt") (tex-length 1 ':pt))
                     ((eat-word "sp") 1)
                     ((eat-word "true") (loop))
                     (else 1))))))))

(define get-points
  (lambda ()
    (/ (get-scaled-points) 65536.0)))

(define get-pixels
  (lambda ()
    ;assume 1 pxl = 1pt
    (inexact->exact
      (floor (get-points)))))

(define do-font
  (lambda ()
    (get-ctl-seq)
    (get-equal-sign)
    (eat-alphanumeric-string)
    (cond ((eat-word "at") (eat-dimen))
          ((eat-word "scaled") (get-number)))))

(define do-fontdimen
  (lambda ()
    (get-number)
    (get-ctl-seq)
    (get-equal-sign)
    (eat-dimen)))

(define do-hskip
  (lambda ()
    (let ((n (get-pixels)))
      (emit "<span style=\"margin-left: ")
      (emit n)
      (emit "pt\">&#x200c;</span>"))))

(define do-vskip
  (lambda ()
    (let ((x (get-points)))
      (eat-skip-fluff #f)
      ;(emit-newline)
      (emit "<div style=\"height: ")
      (emit x)
      (emit "pt\"></div>")
      (emit-newline)
      (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
      (set! *in-para-p* #t)
      )))

(define do-newline
  (lambda ()
    (when (>= (munch-newlines) 1)
      (do-para))
    (emit-newline)))

(define do-br
  (lambda ()
    (if (or (find-chardef #\space)
            (not (= (the-count "\\TIIPobeylinestrictly") 0)))
        (emit "<br>")
        (unless (eqv? (snoop-actual-char) #\newline)
          (emit "<br>")))
    (emit-newline)))

(define do-sup
  (lambda ()
    (emit "<sup>")
    (fluid-let ((*math-script-mode-p* #t))
      (tex2page-string (get-token)))
    (emit "</sup>")))

(define do-sub
  (lambda ()
    (emit "<sub>")
    (fluid-let ((*math-script-mode-p* #t))
      (tex2page-string (get-token)))
    (emit "</sub>")))

(define do-hyphen
  (lambda ()
    (cond (*math-mode-p*
            (emit (if (eq? *math-font* ':rm) "-" "&#x2212;")))
          ((not *ligatures-p*) (emit #\-))
          (else (let ((c (snoop-actual-char)))
                  (if (and (char? c) (char=? c #\-))
                      (begin (get-actual-char) (do-ndash))
                      (emit #\-)))))))

(define do-excl
  (lambda ()
    (if (or *math-mode-p* (not *ligatures-p*))
        (emit #\!)
        (let ((c (snoop-actual-char)))
          (if (and (char? c) (char=? c #\`))
              (begin (get-actual-char) (emit "&#xa1;"))
             (emit #\!))))))

(define do-quest
  (lambda ()
    (if (or *math-mode-p* (not *ligatures-p*))
        (emit #\?)
        (let ((c (snoop-actual-char)))
          (if (and (char? c) (char=? c #\`))
              (begin (get-actual-char) (emit "&#xbf;"))
              (emit #\?))))))

(define do-ndash
  (lambda ()
    (emit
      (let ((c (snoop-actual-char)))
        (if (and (char? c) (char=? c #\-))
            (begin (get-actual-char) "&#x2014;")
            "&#x2013;")))))

(define do-lsquo
  (lambda ()
    (emit
      (if (not *ligatures-p*) #\`
          (let ((c (snoop-actual-char)))
            (if (and (char? c) (char=? c #\`))
                (begin (get-actual-char) "&#x201c;")
                "&#x2018;"))))))

(define do-rsquo
  (lambda ()
    (emit
      (cond (*math-mode-p*
              (let ((c (snoop-actual-char)))
                (if (and (char? c) (char=? c #\'))
                    (begin (get-actual-char) "&#x2033;")
                    "&#x2032;")))
            ((not *ligatures-p*) #\')
            (else
              (let ((c (snoop-actual-char)))
                (if (and (char? c) (char=? c #\'))
                    (begin (get-actual-char) "&#x201d;")
                    "&#x2019;")))))))

(define do-enquote
  (lambda ()
    (ignorespaces)
    (when (char=? (snoop-actual-char) #\*)
      (get-actual-char)
      (ignorespaces)
      (when (= *quote-level* 0)
        (set! *quote-level* (+ *quote-level* 1))))
    (unless (char=? (get-actual-char) #\{)
      (terror 'do-enquote "Missing {"))
    (bgroup)
    (set! *quote-level* (+ *quote-level* 1))
    (emit (if (odd? *quote-level*) "&#x201c;" "&#x2018;"))
    (add-aftergroup-to-top-frame
      (lambda ()
        (emit (if (odd? *quote-level*) "&#x201d;" "&#x2019;"))
        (set! *quote-level* (- *quote-level* 1))))))

;cross-references

(defstruct label (src #f) page name value)

(define get-label
  (lambda ()
    (let loop ((lbl (get-peeled-group)))
      (let ((i (or (string-index lbl #\space)
                   (string-index lbl *tab*)
                   (string-index lbl #\newline))))
        (if (not i) lbl
            (let loop ((s (string->list lbl)) (r '())
                       (ws? #f))
              (if (null? s) (list->string (nreverse r))
                  (let ((c (car s)))
                    (loop (cdr s)
                          (if (char-whitespace? c)
                              (if ws? r (cons #\space r))
                              (cons c r))
                          (char-whitespace? c))))))))))

(define emit-anchor
  (lambda (lbl)
    (emit "<a name=\"")
    (emit lbl)
    (emit "\"></a>")))

(define emit-link-start
  (lambda (link)
    (emit "<a href=\"")
    (emit link)
    (emit "\">")))

(define emit-ext-page-node-link-start
  (lambda (extfile pageno node)
    (emit "<a ")
    (unless extfile
      (emit "class=hrefinternal "))
    (emit "href=\"")
    (unless (and (not extfile)
                 (or (not pageno) (= *html-page-count* pageno)))
      (emit (or extfile *jobname*))
      (unless (= pageno 0)
        (emit *html-page-suffix*)
        (emit pageno))
      (emit *output-extension*))
    (when node
      (emit "#")
      (emit node))
    (emit "\">")))

(define emit-page-node-link-start
  (lambda (pageno node)
    (emit-ext-page-node-link-start
     #f pageno node)))

(define emit-page-link-start
  (lambda (pageno)
    (emit-ext-page-node-link-start #f pageno #f)))

(define emit-link-stop
  (lambda ()
    (emit "</a>")))

(define do-anchor-for-potential-label
  (lambda ()
    (let ((node-name (string-append *html-node-prefix*
                                    "anchor_"
                                    (gen-temp-string))))
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (emit-anchor node-name))))

(define do-label
  (lambda ()
    (do-label-aux (get-label))))

(define do-node
  (lambda ()
    (set! *recent-node-name* (get-peeled-group))))

(define do-label-aux
  (lambda (label)
    (let ((name (ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename"))
          (value (ctl-seq-no-arg-expand-once "\\@currentlabel")))
      (set! value (tex-string-to-html-string value))
      (!label label *html-page-count* name value)
      (write-label
        `(!label ,label ,*html-page-count*
           ,name ,value)))))

(define do-inputexternallabels
  (lambda ()
    (let* ((f (get-filename-possibly-braced))
           (fq-f (if (fully-qualified-pathname? f) f
                     (string-append *aux-dir/* f)))
           (ext-label-file (string-append fq-f
                             *label-file-suffix* ".scm"))
           (ext-label-table (table-get *external-label-tables* f)))
      (unless ext-label-table
        (set! ext-label-table (make-table 'equ string=?))
        (table-put! *external-label-tables* f ext-label-table))
      (when (file-exists? ext-label-file)
        (fluid-let ((*label-source* fq-f)
                    (*label-table* ext-label-table))
          (load-tex2page-data-file ext-label-file))))))

(define do-includeexternallabels
  (lambda ()
    (let ((jobname (get-filename-possibly-braced)))
      ;(unless (fully-qualified-pathname? jobname)
      ;  (set! jobname (string-append *aux-dir/* jobname)))
      (let ((ext-label-file (string-append
                             (if (fully-qualified-pathname? jobname)
                                 jobname
                                 (string-append *aux-dir/* jobname))
                             *label-file-suffix* ".scm")))
        (when (file-exists? ext-label-file)
          (fluid-let ((*label-source* jobname))
            (load-tex2page-data-file ext-label-file)))))))

(define do-tag
  (lambda ()
    (let ((tag-name (get-peeled-group)))
      (do-tag-aux tag-name
                  (get-group)))))

(define do-definexref
  (lambda ()
    (let* ((tag (get-peeled-group))
           (value (get-group))
           (class (get-token)))
      (do-tag-aux tag value))))

(define do-xrdef
  (lambda ()
    (let ((tag (get-peeled-group)))
      (do-tag-aux tag (number->string *html-page-count*)))))

(define do-tag-aux
  (lambda (tag-name tag-val)
    (let ((node-name (string-append *html-node-prefix* "tag_"
                                    (gen-temp-string))))
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (tex-def-0arg "\\@currentlabel" tag-val)
      (emit-anchor node-name)
      (do-label-aux tag-name))))

(define do-htmlpagelabel
  (lambda ()
    (let ((label (get-peeled-group)))
      (!label label *html-page-count* #f #f)
      (write-label `(!label ,label ,*html-page-count* #f #f)))))

(define do-ref
  (lambda ()
    (do-ref-aux (get-label) #f #f)))

(define do-refexternal
  (lambda ()
    (let ((ext-file (get-peeled-group)))
      (do-ref-aux (get-label) ext-file #f))))

(define do-ref-aux
  (lambda (label ext-file link-text)
    (let* ((label-ref (label-bound? label ext-file))
           (label-text
            (cond (link-text (tex-string-to-html-string link-text))
                  (label-ref (label.value label-ref))
                  (else #f))))
      (cond (label-ref (emit-ext-page-node-link-start
                        (or ext-file (label.src label-ref))
                        (label.page label-ref)
                        (label.name label-ref))
                       (emit label-text)
                       (emit-link-stop))
            (else (non-fatal-error label))))))

(define maybe-label-page
  (lambda (this-label-src this-label-pageno)
    (if (and (not this-label-src) (= *html-page-count* this-label-pageno))
        "" ;"#"
        (string-append
          (or this-label-src *jobname*)
          (if (= this-label-pageno 0) ""
              (string-append *html-page-suffix*
                (number->string this-label-pageno)))
         *output-extension*))))

(define do-htmlref
  (lambda ()
    (let* ((text (get-group))
           (lbl (get-peeled-group)))
      (do-ref-aux lbl #f text))))

(define do-htmlrefexternal
  (lambda ()
    (let* ((text (get-group))
           (extf (get-peeled-group))
           (lbl (get-peeled-group)))
      (do-ref-aux lbl extf text))))

(define do-hyperref
  (lambda ()
    (let ((lbl (get-bracketed-text-if-any)))
      (if lbl
          (do-ref-aux lbl #f (get-group))
        (let* ((text (get-group))
               (lbl (begin (get-group) (get-group) (get-peeled-group))))
          (do-ref-aux lbl #f text))))))

(define do-hypertarget
  (lambda ()
    (let ((lbl (get-peeled-group)))
      (do-tag-aux lbl "hypertarget"))))

(define do-hyperlink
  (lambda ()
    (emit-link-start (fully-qualify-url (string-append "#" (get-peeled-group))))
    (tex2page-string (get-token))
    (emit-link-stop)))

(define label-bound?
  (lambda (label . ext-file)
    (let* ((ext-file (if (pair? ext-file) (car ext-file) #f))
           (label-table (if ext-file
                            (table-get *external-label-tables* ext-file)
                            *label-table*)))
      (or (and label-table (table-get label-table label))
          (begin
           (flag-unresolved-xref
            (if ext-file
                (string-append "{" ext-file " -> " label "}")
                label))
           #f)))))

(define flag-unresolved-xref
  (lambda (xr)
    (unless (member xr *unresolved-xrefs*)
      (set! *unresolved-xrefs* (cons xr *unresolved-xrefs*))
      ;(trace-if #t "Undefined label " xr)
      )))

(define flag-missing-piece
  (lambda (mp)
    (unless (member mp *missing-pieces*)
      (set! *missing-pieces* (cons mp *missing-pieces*)))))

(define show-unresolved-xrefs-and-missing-pieces
  (lambda ()
    (unless (and (null? *unresolved-xrefs*)
                 (null? *missing-pieces*))
      (show-unresolved-xrefs)
      (show-missing-pieces)
      (write-log ':separation-newline)
      (write-log "Rerun: tex2page ")
      (write-log *main-tex-file*)
      (write-log ':separation-newline)
      (write-log "If problem persists, check for ")
      (write-log "missing \\label's and \\bibitem's"))))

(define show-unresolved-xrefs
  (lambda ()
    (unless (null? *unresolved-xrefs*)
      (write-log ':separation-newline)
      (write-log "Unresolved cross-reference")
      (when (> (length *unresolved-xrefs*) 1) (write-log "s"))
      (write-log ": ")
      (set! *unresolved-xrefs* (nreverse *unresolved-xrefs*))
      (write-log (car *unresolved-xrefs*))
      (for-each (lambda (x) (write-log #\,) (write-log ':separation-space)
                  (write-log x))
        (cdr *unresolved-xrefs*))
      (write-log ':separation-newline))))

(define show-missing-pieces
  (lambda ()
    (unless (null? *missing-pieces*)
      (write-log ':separation-newline)
      (when (memv ':document-title *missing-pieces*)
        (write-log "Document title not determined")
        (write-log ':separation-newline))
      (when (memv ':last-page *missing-pieces*)
        (write-log "Last page not determined")
        (write-log ':separation-newline))
      (when (memv ':last-modification-time *missing-pieces*)
        (write-log "Last modification time not determined")
        (write-log ':separation-newline))
      (when (memv ':stylesheets *missing-pieces*)
        (write-log "Style sheets not determined")
        (write-log ':separation-newline))
      (when (member ':scripts *missing-pieces*)
        (write-log "Scripts not determined")
        (write-log ':separation-newline))
      (when (memv ':html-head *missing-pieces*)
        (write-log "HTML header info not determined")
        (write-log ':separation-newline))
      (when (memv ':toc *missing-pieces*)
        (write-log "Table of contents not determined")
        (write-log ':separation-newline))
      (cond ((memv ':fresh-index *missing-pieces*)
             (write-log "Index not refreshed")
             (write-log ':separation-newline))
            ((memv ':index *missing-pieces*)
             (write-log "Index not included")
             (write-log ':separation-newline)))
      (cond ((memv ':fresh-bibliography *missing-pieces*)
             (write-log "Bibliography not refreshed")
             (write-log ':separation-newline))
            ((memv ':bibliography *missing-pieces*)
             (write-log "Bibliography not included")
             (write-log ':separation-newline)))
      (when (memv ':metapost *missing-pieces*)
        (write-log "MetaPost output not included")
        (write-log ':separation-newline))
;      (when (memv 'mfpic *missing-pieces*)
;        (write-log "MFpic output not included")
;        (write-log ':separation-newline))
      )))

(define do-pageref
  (lambda ()
    (let ((label-ref (label-bound? (get-peeled-group))))
      (if label-ref
          (let ((pageno (label.page label-ref)))
            (emit-ext-page-node-link-start
             (label.src label-ref) pageno #f)
            (emit pageno)
            (emit-link-stop))
          (non-fatal-error "***")))))

(define do-htmlpageref
  (lambda ()
    (let ((label (get-peeled-group)))
      (let ((label-ref (label-bound? label)))
        (emit "\"")
        (if label-ref
            (emit (maybe-label-page (label.src label-ref)
                                    (label.page label-ref)))
            ;above needs trailing # ?
            (emit *log-file*))
        (emit "\"")))))

;URLs

(define doc-internal-url
  (lambda (url)
    (let ((n (string-length url)))
      (cond  ((and (> n 0) (char=? (string-ref url 0) #\#))
              (let* ((label (substring url 1 n))
                     (label-ref (label-bound? label)))
                (if label-ref
                    (if (label.src label-ref) #f
                        (list (label.page label-ref) (label.name label-ref)))
                    #f)))
             (else #f)))))

(define fully-qualify-url
  (lambda (url)
    (let ((n (string-length url)))
      (cond  ((and (> n 0) (char=? (string-ref url 0) #\#))
              (let* ((label (substring url 1 n))
                     (label-ref (label-bound? label)))
                (if label-ref
                    (string-append
                      (maybe-label-page (label.src label-ref)
                                       (label.page label-ref))
                     "#"
                     (label.name label-ref))
                    url)))
            ((fully-qualified-url-p url) url)
            (else (ensure-url-reachable url)
              url)))))

(define do-url
  (lambda ()
    (let ((url (get-url)))
      (let ((durl (doc-internal-url url)))
        (if durl
            (emit-page-node-link-start (car durl) (cadr durl))
            (emit-link-start (fully-qualify-url url))))
      (emit url)
      (emit-link-stop))))

(define do-mailto
  (lambda ()
    (let ((addr (get-url)))
      (emit-link-start (string-append "mailto:" addr))
      (emit addr)
      (emit-link-stop))))

(define do-opmac-ulink
  (lambda ()
    (let* ((url (get-bracketed-text-if-any))
           (link-text (get-group))
           (durl (doc-internal-url url)))
      (if durl
          (emit-page-node-link-start (car durl) (cadr durl))
          (emit-link-start (fully-qualify-url url)))
      (bgroup)
      (tex2page-string link-text)
      (egroup)
      (emit-link-stop))))

(define do-urlh
  (lambda ()
    (let* ((url (get-url))
           (durl (doc-internal-url url)))
      (if durl
          (emit-page-node-link-start (car durl) (cadr durl))
          (emit-link-start (fully-qualify-url url))))
    (bgroup)
    (tex2page-string
      (string-append "\\def\\\\{\\egroup\\endinput}"
                     (get-token)))
    (egroup)
    (emit-link-stop)))

(define do-urlhd
  (lambda ()
    (do-urlh)
    (get-token) ;throw away description meant for DVI
    ))

(define do-urlp
  (lambda ()
    (let ((link-text (get-token)))
      (let* ((url (get-url))
            (durl (doc-internal-url url)))
        (if durl
            (emit-page-node-link-start (car durl) (cadr durl))
            (emit-link-start (fully-qualify-url url))))
      (tex2page-string link-text)
      (emit-link-stop))))

(define do-hlstart
  (lambda ()
    (let* ((cat (get-peeled-group))
           (options (get-token))
           (url (get-url)))
      (when (string=? cat "url")
        (emit-link-start (fully-qualify-url url))
        (bgroup)
        (tex-let "\\hlend" "\\TIIPhlend" #f))
      (ignorespaces))))

(define do-hlend
  (lambda ()
    (egroup)
    (emit-link-stop)))

(define do-htmladdimg
  (lambda ()
    (let* ((align-info (get-bracketed-text-if-any))
           (url (fully-qualify-url (get-url))))
      (emit "<img src=\"")
      (emit url)
      (emit "\" border=\"0\" ")
      (when align-info
        (tex2page-string align-info))
      (emit " alt=\"[")
      (emit url)
      (emit "]\">"))))

(define do-pdfximage
  (lambda ()
    (let ((height #f) (width #f) (depth #f))
      (let loop ()
        (cond ((eat-word "height") (set! height (get-pixels)) (loop))
              ((eat-word "width") (set! width (get-pixels)) (loop))
              ((eat-word "depth") (set! depth (get-pixels)) (loop))
              (else #f)))
      (emit "<img")
      (when height
        (emit " height=") (emit height))
      (when width
        (emit " width=") (emit width))
      (emit " src=\"")
      (emit (fully-qualify-url (get-filename-possibly-braced)))
      ;(emit (fully-qualify-url (get-filename-possibly-braced)))
      (emit "\">")
      (ignorespaces) (get-ctl-seq) ; \pdfrefximage
      (ignorespaces) (get-ctl-seq) ; \pdflastximage
      )))

;bibliography

(define do-cite-help
  (lambda (delim extra-text)
    (let ((closing-delim (cond ((char=? delim #\{) #\})
                               ((char=? delim #\[) #\])
                               (else (terror 'do-cite "faulty delim" delim)))))
      (ignorespaces)
      (unless (char=? (get-actual-char) delim)
        (terror 'do-cite "Missing" delim))
      (emit "[")
      (let ((first-key? #t))
        (let loop ()
          (cond ((get-csv closing-delim)
                 => (lambda (key)
                      (cond (first-key? (set! first-key? #f))
                            (else (emit ",") (emit-nbsp 1)))
                      (write-bib-aux "\\citation{")
                      (write-bib-aux key)
                      (write-bib-aux "}")
                      (write-bib-aux #\newline)
                      (do-ref-aux (string-append "cite{" key "}")
                                  #f #f)
                      (loop)))))
        (when extra-text
          (emit ",") (emit-nbsp 1)
          (tex2page-string extra-text))
        (unless (char=? (get-actual-char) closing-delim)
          (terror 'do-cite "Missing" closing-delim))
        (when first-key? (terror 'do-cite "Empty \\cite")))
      (emit "]"))))

(define do-cite
  (lambda ()
    (if (tex2page-flag-boolean "\\TZPopmac")
        (do-cite-help #\[ #f)
        (do-cite-help #\{ (get-bracketed-text-if-any)))))

(define do-rcite
  (lambda ()
    (do-cite-help #\[ #f)))

(define do-nocite
  (lambda ()
    (ignorespaces)
    (let* ((delim (if (tex2page-flag-boolean "\\TZPopmac") #\[ #\{))
           (closing-delim (if (char=? delim #\{) #\} #\])))
    (unless (char=? (get-actual-char) delim)
      (terror 'do-cite "Missing" delim))
    (let loop ()
      (cond ((get-csv closing-delim)
             => (lambda (key)
                  (write-bib-aux "\\citation{")
                  (write-bib-aux key)
                  (write-bib-aux "}")
                  (label-bound? (string-append "cite{" key "}"))
                  (write-bib-aux #\newline)
                  (loop)))))
    (unless (char=? (get-actual-char) closing-delim)
      (terror 'do-nocite "Missing" closing-delim)))))

(define do-bibliographystyle
  (lambda ()
    (do-bibliographystyle-help (ungroup (get-token)))))

(define do-bibliographystyle-help
  (lambda (s)
    (write-bib-aux "\\bibstyle{")
    (write-bib-aux s)
    (write-bib-aux "}")
    (write-bib-aux #\newline)))

(define do-bibliography
  (lambda ()
    (do-bibliography-help (ungroup (get-token)))))

(define do-bibliography-help
  (lambda (bibdata)
    (set! *using-bibliography-p* #t)
    (let ((bbl-file (string-append *aux-dir/* *jobname*
                                   *bib-aux-file-suffix* ".bbl")))
      (write-bib-aux "\\bibdata{")
      (write-bib-aux bibdata)
      (write-bib-aux "}")
      (write-bib-aux #\newline)
      ;(write-aux `(!using-external-program "bibtex"))
      (cond ((file-exists? bbl-file) (set! *bibitem-num* 0)
             (tex2page-file bbl-file)
             (emit-newline))
            (else
             (flag-missing-piece ':bibliography)
             (non-fatal-error
              "Bibliography not generated; rerun TeX2page"))))))

(define do-opmac-usebibtex
  (lambda ()
    (let* ((bibfile (ungroup (get-token)))
           (bibstyle (ungroup (get-token))))
      (do-bibliographystyle-help bibstyle)
      (do-bibliography-help bibfile))))

(define (do-thebibliography)
  (do-end-para)
  (get-group)
  (when (eqv? *tex-format* ':latex)
    (tex2page-string
      (if *using-chapters-p*
          "\\chapter*{\\bibname}" "\\section*{\\refname}")))
  (bgroup)
  (set! *bibitem-num* 0)
  (tex2page-string "\\let\\em\\it")
  (tex2page-string "\\def\\newblock{ }")
  ;(tex2page-string "\\def\\providecommand#1#2{}")
  (emit "<table>")
  (emit-newline))

(define (do-bibitem)
  (do-end-para)
  (let ((bibmark (get-bracketed-text-if-any)))
    (unless (= *bibitem-num* 0)
      (emit "</td></tr>")
      (emit-newline))
    (set! *bibitem-num* (+ *bibitem-num* 1))
    (emit "<tr><td align=right valign=top>")
    (let* ((bibitem-num-s (number->string *bibitem-num*))
           (key (string-append "cite{" (get-peeled-group) "}"))
           (node-name (string-append *html-node-prefix* "bib_"
                                     bibitem-num-s)))
      ;wrap key with cite{...} so key can be used
      ;as a regular label also (for something else)
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (unless bibmark
        (set! bibmark bibitem-num-s))
      (tex-def-0arg "\\@currentlabel" bibmark)
      (emit-anchor node-name)
      (emit "[")
      (tex2page-string bibmark)
      (emit "]")
      (emit-nbsp 2)
      (do-label-aux key)
      (emit "</td><td>")
      )))

(define do-opmac-bib
  (lambda ()
    (do-para)
    (set! *bibitem-num* (+ *bibitem-num* 1))
    (let* ((key0 (get-bracketed-text-if-any))
           (bibitem-num-s (number->string *bibitem-num*))
           (key (string-append "cite{" key0 "}"))
           (node-name (string-append *html-node-prefix* "bib_" bibitem-num-s)))
      (unless key0
        (terror 'do-opmac-bib "Improper \\bib entry"))
      (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (tex-def-0arg "\\@currentlabel" bibitem-num-s)
      (emit-anchor node-name)
      (emit "[")
      (tex2page-string bibitem-num-s)
      (emit "]")
      (emit-nbsp 2)
      (do-label-aux key))))

;index

(define display-index-entry
  ;like display, but sub space for newline
  (lambda (s o)
    (for-each
      (lambda (c)
        (display
          (if (or ;(char=? c *return*)
                (char=? c #\newline))
              #\space
              c)
          o))
      (string->list s))))

(define do-index-help
  (lambda (idx-entry)
    (set! *index-count* (+ *index-count* 2))
    ;
    ;increment by 2 rather than 1, effectively disabling
    ;makeindex from creating ranges, which are meaningless for
    ;HTML.  Actually, makeindex called with the -r option prevents
    ;ranging but who remembers these things?
    ;
    (!index *index-count* *html-page-count*)
    (write-aux
      `(!index ,*index-count* ,*html-page-count*))
    ;(set! *index-table*
    ;      (cons (cons *index-count* *html-page-count*) *index-table*))
    (let ((tag (string-append *html-node-prefix* "index_"
                              (number->string *index-count*))))
      (emit-anchor tag)
      (unless *index-port*
        (let ((idx-file (string-append *aux-dir/* *jobname*
                                       *index-file-suffix* ".idx")))
          (ensure-file-deleted idx-file)
          (set! *index-port* (open-output-file idx-file))))
      (display "\\indexentry{" *index-port*)
      (cond ((substring? "|see{" idx-entry)
             (display-index-entry idx-entry *index-port*))
            ((substring? "|seealso{" idx-entry)
             (display-index-entry idx-entry *index-port*))
            ((substring? "|(" idx-entry)
             => (lambda (i)
                  (display-index-entry (substring idx-entry 0 i)
                                       *index-port*)
                  (display "|expandhtmlindex" *index-port*)))
            (else (display-index-entry idx-entry *index-port*)
                  (display "|expandhtmlindex" *index-port*)))
      (display "}{" *index-port*)
      (display *index-count* *index-port*)
      (display "}" *index-port*)
      (newline *index-port*))))

(define do-index
  (lambda ()
    (let ((idx-entry (ungroup (get-group))))
      (ignorespaces) ;?
      (unless (substring? "|)" idx-entry)
        (do-index-help idx-entry)))))

(define (escape-opmac-index-entry x)
  (let ((n (string-length x)))
    (let loop ((i (- (string-length x) 1)) (y '()))
      (if (< i 0) (list->string y)
          (let ((c (string-ref x i)))
            (loop (- i 1)
                  (case c
                    ((#\") (cons c (cons c y)))
                    ((#\! #\@) (cons #\" (cons c y)))
                    (else (cons c y)))))))))

(define (do-opmac-ii retainp)
  (let* ((lhs (get-word))
         (sub (and *opmac-index-sub-table* (table-get *opmac-index-sub-table* lhs))))
    (if retainp (toss-back-string lhs)
        (ignorespaces))
    (do-index-help
      (cond (sub sub)
            (else (string=join (map escape-opmac-index-entry (string=split lhs #\/))
                               #\!))))))

(define do-opmac-iis
  (lambda ()
    (let* ((lhs (get-word))
           (rhs (get-peeled-group))
           (lhs-list (map escape-opmac-index-entry (string=split lhs #\/)))
           (rhs-list (map escape-opmac-index-entry (string=split rhs #\/)))
           (sub ""))
      (unless (= (length lhs-list) (length rhs-list))
        (terror 'do-opmac-iis "Malformed \\iis."))
      (let loop ((lhs-list lhs-list) (rhs-list rhs-list))
        (cond ((null? lhs-list) #f)
              (else (let ((additive (string-append (car lhs-list) "@" (car rhs-list))))
                      (set! sub
                        (cond ((string=? sub "") additive)
                              (else (string-append sub "!" additive))))
                      (loop (cdr lhs-list) (cdr rhs-list))))))
      (unless *opmac-index-sub-table*
        (flag-missing-piece ':fresh-index))
      (!opmac-iis lhs sub)
      (write-aux `(!opmac-iis ,lhs ,sub)))))

(define do-inputindex
  (lambda (insert-heading?)
    (set! *using-index-p* #t)
    (when insert-heading?
      (tex2page-string
       (if *using-chapters-p*
           "\\chapter*{\\indexname}" "\\section*{\\indexname}"))
      (emit-newline))
    (emit-anchor (string-append *html-node-prefix* "index_start"))
    (!index-page *html-page-count*)
    (write-aux `(!index-page ,*html-page-count*))
    (let ((ind-file (string-append *aux-dir/* *jobname*
                                   *index-file-suffix* ".ind")))
      ;(write-aux `(!using-external-program "makeindex"))
      (cond ((file-exists? ind-file)
             (tex2page-file ind-file))
            (else
             (flag-missing-piece ':index)
             (non-fatal-error "Index not generated; rerun TeX2page"))))))

(define do-theindex
  (lambda ()
    (bgroup)
    (tex2page-string "\\let\\endtheindex\\egroup")
    (tex2page-string "\\let\\indexspace\\relax")
    (tex2page-string "\\let\\item\\indexitem")
    (tex2page-string "\\let\\subitem\\indexsubitem")
    (tex2page-string "\\let\\subsubitem\\indexsubsubitem")
    (tex2page-string "\\let\\(\\expandhtmlindex")
    ))

(define expand-html-index
  (lambda ()
    (let* ((s (get-peeled-group))
           (n (string->number s))
           (pageno (table-get *index-table* n)))
      (emit-page-node-link-start
        pageno (string-append *html-node-prefix* "index_" s))
      (emit pageno)
      ;this shouldn't be needed -- unless \theindex is redefined
      (unless *index-page-mention-alist*
        (set! *index-page-mention-alist* (make-table)))
      (cond ((table-get *index-page-mention-alist* pageno)
             => (lambda (c)
                  (let ((n (+ 1 c)))
                    (emit (number-to-roman n #f))
                    (table-put! *index-page-mention-alist* pageno n))))
            (else
              (table-put! *index-page-mention-alist* pageno 1)))
      (emit-link-stop))))

(define do-see-also
  (lambda ()
    (let* ((other-entry (get-group))
           (discard (get-group)))
      (emit "<em>see also</em> ")
      (tex2page-string other-entry))))

(define do-setbox
  (lambda ()
    (get-raw-token/is)
    (get-equal-sign)
    (let ((cs (get-raw-token/is)))
      (when (ormap (lambda (e) (string=? cs e)) '("\\hbox" "\\vbox"))
        (get-to)
        (eat-dimen)
        (get-token-or-peeled-group)))))

(define (html-length s)
  (let ((n (string-length s)))
    (let loop ((i 0) (skip-tag #f) (skip-entity #f) (res 0))
      (if (>= i n) res
          (let ((c (string-ref s i)))
            (cond (skip-tag (loop (+ i 1) (if (char=? c #\>) #f #t) #f res))
                  (skip-entity (loop (+ i 1) #f (if (char=? c #\;) #f #t) res))
                  ((char=? c #\<) (loop (+ i 1) #t #f res))
                  ((char=? c #\&) (loop (+ i 1) #f #t (+ res 1)))
                  (else (loop (+ i 1) #f #f (+ res 1)))))))))

(define (do-llap)
  (let* ((txt (tex-string-to-html-string (get-group)))
         (html-len (html-length txt))
         (txt-len (sp-to-pixels (tex-length html-len ':ex))))
    (emit "<span style=\"position: relative\">")
    (emit "<span style=\"position: absolute; left: -")
    (emit txt-len)
    (emit "pt\">")
    (emit txt)
    (emit "</span></span>")))

(define do-indexitem
  (lambda (indent)
    (set! *index-page-mention-alist* (make-table))
    (emit "<br>") (emit-newline)
    (emit-nbsp (* indent 4))))

;item

(define do-description-item
  (lambda ()
    (do-end-para)
    (emit "</dd><dt>")
    (let ((thing (get-bracketed-text-if-any)))
      (when thing
        (set! thing (string-trim-blanks thing))
        (unless (string=? thing "")
          (bgroup)
          (emit "<b>")
          (tex2page-string thing)
          (emit "</b>")
          ;(emit-nbsp 2)
          (egroup)
          )))
    (emit "</dt><dd>")))

(define do-regular-item
  (lambda ()
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
        (emit-nbsp 2)))))

(define (do-plain-item n)
  (do-end-para)
  (emit-newline)
  (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
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

(define (do-textindent)
  (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
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

(define (do-proclaim)
  (let* ((head (tex-string-to-html-string (get-till-char #\.)))
         (body (begin (get-actual-char) (ignorespaces)
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

(define do-item
  (lambda ()
    (let ((a #f))
      (unless (null? *tabular-stack*)
        (set! a (car *tabular-stack*)))
      (case a
        ((:description) (do-description-item))
        ((:itemize :enumerate) (do-regular-item))
        (else (do-plain-item 1))))))

(define (do-itemize)
  (do-end-para)
  (set! *tabular-stack* (cons ':itemize *tabular-stack*))
  (emit "<ul")
  (when (tex2page-flag-boolean "\\TZPslides")
    (emit " class=incremental"))
  (emit ">")
  (emit-newline))

(define (do-enditemize)
    (do-end-para)
    (pop-tabular-stack ':itemize)
    (emit "</ul>")
    (do-noindent))

(define (do-enumerate)
  (do-end-para)
  (set! *tabular-stack* (cons ':enumerate *tabular-stack*))
  (emit "<ol")
  (when (tex2page-flag-boolean "\\TZPslides")
    (emit " class=incremental"))
  (emit ">")
  (emit-newline))

(define (do-endenumerate)
  (pop-tabular-stack ':enumerate)
  (do-end-para)
  (emit "</ol>")
  (do-noindent))

(define do-opmac-list-style
  (lambda ()
    (ignorespaces)
    (set! *opmac-list-style* (get-actual-char))))

(define do-opmac-begitems
  (lambda ()
    (do-end-para)
    (bgroup)
    (tex-def-count "\\TIIPopmacliststarted" 0 #f)
    (activate-cdef #\*)
    (tex-def-char #\* '() "\\TIIPopmacitem" #f)))

(define do-opmac-item
  (lambda ()
    (when (= (find-count "\\TIIPopmacliststarted") 0)
      (tex-def-count "\\TIIPopmacliststarted" 1 #f)
      (set! *tabular-stack* (cons ':opmac-itemize *tabular-stack*))
      (emit "<")
      (emit (case *opmac-list-style*
              ((#\o #\- #\x #\X) "u")
              (else "o")))
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
              (else "disc")))
      (emit "\">"))
    (do-regular-item)))

(define do-opmac-enditems
  (lambda ()
    (egroup)
    (do-end-para)
    (pop-tabular-stack ':opmac-itemize)
    (emit "</")
    (emit (case *opmac-list-style*
            ((#\o #\- #\x #\X) "u")
            (else "o")))
    (emit "l>")
    (do-noindent)))

;vert space

(define do-bigskip
  (lambda (type)
    (do-end-para)
    (emit "<div class=")
    (emit (case type
            ((:medskip) "medskip")
            ((:bigskip) "bigskip")
            (else "smallskip")))
    (emit "></div>")
    (emit-newline)
    (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
    (set! *in-para-p* #t)
    (emit-newline)))

(define do-hspace
  (lambda ()
    (ignorespaces)
    (when (eqv? (snoop-actual-char) #\*) (get-actual-char))
    (get-group)
    (emit-nbsp 3)))

(define do-vspace
  (lambda ()
    (ignorespaces)
    (when (eqv? (snoop-actual-char) #\*) (get-actual-char))
    (get-group)
    (do-bigskip ':vspace)))

(define do-htmlmathstyle
  (lambda ()
    (call-with-input-string/buffered
      (ungroup (get-group))
      (lambda ()
        (let loop ()
          (ignore-all-whitespace)
          (let ((c (snoop-actual-char)))
            (unless (eof-object? c)
              (case (string->symbol (scm-get-token))
                ((image display-image)
                 (tex-def-0arg "\\TZPmathtext" "0")
                 (tex-def-0arg "\\TZPmathimage" "1"))
                ((no-image no-display-image)
                 (tex-def-0arg "\\TZPmathtext" "1")
                 (tex-def-0arg "\\TZPmathimage" "0")))
              (loop))))))))

;

(define do-htmldoctype
  (lambda ()
    (let ((d (get-peeled-group)))
      (when (string=? d "")
        (set! d 'none))
      (write-aux `(!doctype ,d)))))

;timestamp

(define do-htmlcolophon
  (lambda ()
    (call-with-input-string/buffered
      (ungroup (get-group))
      (lambda ()
        (let loop ()
          (ignore-all-whitespace)
          (let ((c (snoop-actual-char)))
            (unless (eof-object? c)
              (let ((directive (string->symbol (string-append ":" (scm-get-token)))))
                (!colophon directive)
                (write-aux `(!colophon ,directive))
                (loop)))))))))

(define output-colophon
  (lambda ()
    (let ((colophon-mentions-last-mod-time?
            (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
                (tex2page-flag-boolean "\\TZPcolophontimestamp")))
          (colophon-mentions-tex2page?
            (or (not (tex2page-flag-boolean "\\TZPcolophondisablecredit"))
                (tex2page-flag-boolean "\\TZPcolophoncredit")))
          (colophon-links-to-tex2page-website?
            (or (not (tex2page-flag-boolean "\\TZPcolophondisableweblink"))
                (tex2page-flag-boolean "\\TZPcolophonweblink"))))
      (when (or colophon-mentions-last-mod-time?
                colophon-mentions-tex2page?)
        ;(do-para)
        (do-end-para)
        (emit "<div align=right class=colophon>")
        ;(emit-newline)
        (when (and colophon-mentions-last-mod-time?
                   *last-modification-time*
                   (> *last-modification-time* 0))
          (tex2page-string *last-modified*) (emit ": ")
          (emit (seconds-to-human-time *last-modification-time*))
          (emit "<br>") ;(emit-newline)
          )
        (when colophon-mentions-tex2page?
          (emit "<div align=right class=advertisement>")
          ;(emit-newline)
          (tex2page-string *html-conversion-by*)
          (emit " ")
          (when colophon-links-to-tex2page-website?
            (emit-link-start
              "http://ds26gte.github.io/tex2page/index.html"))
          (emit *tex-logo*) (emit "2page ")
          (emit *tex2page-version*)
          (when colophon-links-to-tex2page-website?
            (emit-link-stop))
          (emit "</div>")
          ;(emit-newline)
          )
        (emit "</div>")
        (emit-newline)))))

; html pages

(define point-to-adjacent-pages
  (lambda ()
    (let* ((last-page-not-determined? (< *last-page-number* 0))
           (prev-page (if (= *html-page-count* 0) #f (- *html-page-count* 1)))
           (next-page (if (= *html-page-count* *last-page-number*) #f (+ *html-page-count* 1))))
      (unless (= *last-page-number* 0)
        (when prev-page (emit-page-link-start prev-page))
        (emit "&#x3c;&#xb7;&#xb7;&#xb7;Prev ")
        (when prev-page (emit-link-stop))
        (emit "||")
        (when next-page (emit-page-link-start next-page))
        (emit " Next&#xb7;&#xb7;&#xb7;&#x3e;")
        (when next-page (emit-link-stop))))))

(define output-head-or-foot-line
  (lambda (head-or-foot)
    (unless (tex2page-flag-boolean "\\TZPsinglepage")
      (emit "<div align=right class=navigation>")
      (cond ((or *tex-like-layout-p*
                 (and (eq? head-or-foot ':foot)
                      (tex2page-flag-boolean "\\TZPtexlayout")))
             (bgroup)
             (tex-let "\\folio" "\\TIIPfolio" #f)
             (tex2page-string
               (if (eq? head-or-foot ':head)
                 "\\the\\headline"
                 "\\the\\footline"))
             (egroup))
            (else (output-navigation-bar head-or-foot)))
      (emit "</div>")
      (emit-newline))))

(define output-navigation-bar
  (lambda (head-or-foot)
    ;(do-end-para)
    (let* ((first-page? (= *html-page-count* 0))
           (last-page-not-determined? (< *last-page-number* 0))
           (last-page? (= *html-page-count* *last-page-number*))
           (toc-page? (and *toc-page* (= *html-page-count* *toc-page*)))
           (index-page? (and *index-page* (= *html-page-count* *index-page*)))
           (prev-page (if first-page? #f (- *html-page-count* 1)))
           (next-page (if last-page? #f (+ *html-page-count* 1))))
      (unless (and first-page?
                   (or last-page?
                       (and (eq? head-or-foot ':head)
                            last-page-not-determined?)))
        (emit "[")
        (emit *navigation-sentence-begin*)
        ;
        (emit "<span")
        (when first-page? (emit " class=disable"))
        (emit ">")
        (unless first-page?  (emit-page-link-start 0))
        (emit *navigation-first-name*)
        (unless first-page? (emit-link-stop))
        (emit ", ")
        ;
        (unless first-page?  (emit-page-link-start prev-page))
        (emit *navigation-previous-name*)
        (unless first-page? (emit-link-stop))
        ;(emit ", ")
        (emit "</span>")
        ;
        (emit "<span")
        (when last-page? (emit " class=disable"))
        (emit ">")
        (when first-page? (emit "<span class=disable>"))
        (emit ", ")
        (when first-page? (emit "</span>"))
        (unless last-page?  (emit-page-link-start next-page))
        (emit *navigation-next-name*)
        (unless last-page? (emit-link-stop))
        (emit "</span>")
        ;
        (emit *navigation-page-name*)
        ;
        (when (or *toc-page* *index-page*)
          (emit "<span")
          (when (or (and toc-page? (not *index-page*) (not index-page?))
                    (and index-page? (not *toc-page*) (not toc-page?)))
            (emit " class=disable"))
          (emit ">; ")
          (emit-nbsp 2)
          (emit "</span>")
          ;
          (when *toc-page*
            (emit "<span")
            (when toc-page? (emit " class=disable"))
            (emit ">")
            (unless toc-page?
              (emit-page-node-link-start
                *toc-page* (string-append *html-node-prefix* "toc")))
            (emit *navigation-contents-name*)
            (unless toc-page? (emit-link-stop))
            (emit "</span>"))
          ;
          (when *index-page*
            (emit "<span")
            (when index-page? (emit " class=disable"))
            (emit ">")
            (emit "<span")
            (unless (and *toc-page* (not toc-page?))
              (emit " class=disable"))
            (emit ">")
            (when *toc-page*
              (emit "; ")
              (emit-nbsp 2))
            (emit "</span>")
            (unless index-page?
              (emit-page-node-link-start
                *index-page*
                (string-append *html-node-prefix* "index_start")))
            (emit *navigation-index-name*)
            (unless index-page? (emit-link-stop))
            (emit "</span>")))
        (emit *navigation-sentence-end*)
        (emit "]")))))

(define (do-eject)
  (cond ((tex2page-flag-boolean "\\TZPslides")
         (do-end-para)
         (emit "</div>")
         (emit-newline)
         (emit "<div class=slide>")
         (do-para))
        ((tex2page-flag-boolean "\\TZPsinglepage") #t)
        (else
          (unless (and (eof-object? (snoop-actual-char))
                       (eqv? *current-source-file* *main-tex-file*))
            ;kludge: don't start a new page if \eject is the
            ;last thing in the main file.  This is mostly
            ;to placate story.tex, which although horrid as an
            ;example file, happens to be viewed as canonical by
            ;everyone looking at TeX
            ;
            (unless (> *last-page-number* 0) ;???
              (flag-missing-piece ':last-modification-time))
            (do-end-page)
            ;
            (set! *html-page-count* (+ *html-page-count* 1))
            (set! *html-page*
              (string-append *aux-dir/*
                             *jobname* *html-page-suffix*
                             (number->string *html-page-count*)
                             *output-extension*))
            (ensure-file-deleted *html-page*)
            (set! *html* (open-output-file *html-page*))
            (do-start)))))

(define (output-html-preamble)
  (when (string? *doctype*)
    (emit "<!DOCTYPE ")
    (emit *doctype*)
    (emit ">")
    (emit-newline))
  (emit "<html>") (emit-newline)
  (emit "<!--") (emit-newline)
  (emit "Generated from ")
  (emit *main-tex-file*)
  (emit " by tex2page, ")
  (emit "v ") (emit *tex2page-version*) (emit-newline)
  (emit "(running on ")
  (emit *scheme-version*) (emit ", ")
  (emit *operating-system*) (emit "), ") (emit-newline)
  (emit "(c) Dorai Sitaram, ") (emit-newline)
  (emit *tex2page-website*) (emit-newline)
  (emit "-->") (emit-newline)
  (emit "<head>") (emit-newline)
  (emit "<meta charset=\"utf-8\">")
  (emit-newline)
  (output-external-title)
  (link-stylesheets)
  (link-scripts)
  (emit "<meta name=robots content=\"index,follow\">")
  (emit-newline)
  (for-each emit *html-head*)
  (emit "</head>") (emit-newline)
  (emit "<body>") (emit-newline)
  (emit "<div")
  (when (tex2page-flag-boolean "\\TZPslides")
    (emit " class=slide"))
  (emit ">")
  (emit-newline))

(define output-html-postamble
  (lambda ()
    (do-end-para)
    (emit "</div>") (emit-newline)
    (emit "</body>") (emit-newline)
    (emit "</html>") (emit-newline)))

(define redirect-if-needed
  (lambda ()
    (when *redirect-url*
      (emit "If not redirected in ")
      (emit *redirect-delay*)
      (emit " sec, go to ")
      (emit-link-start (fully-qualify-url *redirect-url*))
      (emit *redirect-url*)
      (emit-link-stop))))

(define (check-tex2page-lisp)
  (let ((cl-p (not 'nil))
        (doc-expects-cl-p (tex2page-flag-boolean "\\TZPcommonlisp")))
    (unless (eqv? cl-p doc-expects-cl-p)
      (write-log :separation-newline)
      (write-log "! Document ")
      (write-log *main-tex-file*)
      (write-log " appears to require ")
      (write-log (if doc-expects-cl-p "Common Lisp" "Scheme"))
      (write-log " version of TeX2page."))))

(define do-start
  (lambda ()
    (check-tex2page-lisp)
    (set! *footnote-list* '())
    (output-html-preamble)
    (output-head-or-foot-line ':head)
    (do-para)
    (redirect-if-needed)))

(define do-end-page
  (lambda ()
    (do-end-para)
    (output-footnotes)
    (do-bigskip ':smallskip)
    (output-head-or-foot-line ':foot)
    (do-para)
    (let ((colophon-on-last-page? (tex2page-flag-boolean "\\TZPcolophonlastpage")))
      (when (or (and (not colophon-on-last-page?)
                     (= *html-page-count* 0))
                (and colophon-on-last-page?
                     (= *html-page-count* *last-page-number*)))
        (output-colophon)))
    (output-html-postamble)
    (write-log #\[)
    (write-log *html-page-count*)
    (write-log #\])
    (write-log ':separation-space)
    (close-output-port *html*)))

(define close-all-open-ports
  (lambda ()
    (when *aux-port* (close-output-port *aux-port*))
    (when *css-port* (close-output-port *css-port*))
    (when *index-port* (close-output-port *index-port*))
    (when *label-port* (close-output-port *label-port*))
    (when *bib-aux-port* (close-output-port *bib-aux-port*))
    (when *verb-port* (close-output-port *verb-port*))
    (table-for-each *input-streams*
                    (lambda (k v)
                      (unless (eq? v ':free)
                      (close-input-port v))))
    (table-for-each *output-streams*
                    (lambda (k v)
                      (unless (eq? v ':free)
                      (close-output-port v))))))

(define output-stats
  (lambda ()
    (write-log ':separation-newline)
    (cond (*main-tex-file*
           (let ((num-pages (+ *html-page-count* 1)))
             (write-log "Output written on ")
             (write-log *aux-dir/*)
             (write-log *jobname*)
             (write-log *output-extension*)
             ;(write-log (string-append *aux-dir/* *jobname* *output-extension*))
             ;(set! *write-log-index* 0)
             (when (> num-pages 1)
               (write-log ", ..."))
             (write-log " (")
             (write-log num-pages)
             (write-log " page")
             (unless (= num-pages 1)
               (write-log #\s)))
           ;
           (when (> *img-file-tally* 0)
             (write-log ", ")
             (write-log *img-file-tally*)
             (write-log " image")
             (unless (= *img-file-tally* 1)
               (write-log #\s)))
           ;
           (write-log ")."))
          (else
           (write-log "No pages of output.")))
    ;(write-log ':separation-newline)
    (write-log #\newline)
    (when *log-port* (close-output-port *log-port*))
    (display "Transcript written on ")
    (display *log-file*)
    (display ".")
    (newline)))

(define do-endinput
  (lambda ()
    (toss-back-char *invisible-space*)
    (toss-back-string "\\TIIPendinput")))

(define do-bye
  (lambda ()
    (check-tex2page-lisp)
    (note-down-tex2page-flags)
    (unless (null? *tex-if-stack*)
      (let ((n (length *tex-if-stack*)))
        (trace-if #t "(\\end occurred when " n " \\if"
                  (if (> n 1) "s were" " was") " incomplete)")
        ;(trace-if #t "Found " n " incomplete \\if"
        ;          (if (> n 1) "s" ""))
        ))
    (unless (null? *tex-env*)
      (trace-if #t "\\end occurred inside a group at level "
                (length *tex-env*)))
    (perform-postludes)
    ;(perform-aftergroups)
    (unless (or (>= *last-page-number* 0)
                (= *html-page-count* 0))
      (flag-missing-piece ':last-page))
    (!last-page-number *html-page-count*)
    ;(set! *last-page-number* *html-page-count*)
    ;(when (> *last-page-number* 1)
    (write-aux `(!last-page-number ,*last-page-number*))
    ;)
    (do-end-page)
    (when *last-modification-time*
      (write-aux `(!last-modification-time ,*last-modification-time* ,*epoch*)))
    (for-each
      (lambda (th) (th))
      *afterbye*)
    ;(note-down-tex2page-flags)
    (close-all-open-ports)
    (call-external-programs-if-necessary)
    (show-unresolved-xrefs-and-missing-pieces)
    ;(output-stats)
    ))

(define set-text-width
  (lambda ()
    (let ((hsize (cond ((find-def "\\TZPhsize")
                        => (lambda (d)
                             (tex2page-string (string-append "\\TIIPhsize=" (tdef.expansion d)))
                             (find-dimen "\\TIIPhsize")))
                       (*tex-like-layout-p* (find-dimen "\\hsize"))
                       (else #f))))
      (when hsize
        (display "body { max-width: " *css-port*)
        (display (sp-to-pixels hsize) *css-port*)
        (display "pt; }" *css-port*)
        (newline *css-port*)))))

(define note-down-tex2page-flags
  (lambda ()
    (write-aux `(!head-line ,(get-toks "\\headline")))
    (write-aux `(!foot-line ,(get-toks "\\footline")))
    (cond ((find-def "\\TZPtitle")
           => (lambda (d)
                (write-aux
                 `(!preferred-title
                   ,(tex-string-to-html-string (tdef.expansion d)))))))
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
    (cond ((ctl-seq-no-arg-expand-once "\\TZPredirect")
           => (lambda (url)
                (unless *redirect-url*
                  (flag-missing-piece ':html-head))
                (let ((seconds (ctl-seq-no-arg-expand-once "\\TZPredirectseconds")))
                  (write-aux
                    `(!html-redirect ,url ,seconds))))))
    (when (tex2page-flag-boolean "\\TZPslides")
      (write-aux '(!slides))
      (write-aux '(!single-page))
      (let ((slidy-css-file "slidy.css"))
        (unless (file-exists? slidy-css-file)
          (set! slidy-css-file "http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css"))
        (when (null? *stylesheets*) (flag-missing-piece ':stylesheets))
        (write-aux `(!stylesheet ,slidy-css-file)))
      (let ((slidy-js-file "slidy.js"))
        (unless (file-exists? slidy-js-file)
          (set! slidy-js-file "http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js"))
        (when (null? *scripts*) (flag-missing-piece ':scripts))
        (write-aux `(!script ,slidy-js-file))))
    (when (tex2page-flag-boolean "\\TZPsinglepage")
      (write-aux '(!single-page)))
    (when (tex2page-flag-boolean "\\TZPtexlayout")
      (write-aux `(!tex-like-layout))
      (newline *css-port*)
      (display "body { margin-top: " *css-port*)
      (display (sp-to-ems (+ (tex-length .5 ':in) ;1in is too much!
                             (find-dimen "\\voffset"))) *css-port*)
      (display "em; }" *css-port*)
      (newline *css-port*)
      (display "body { margin-left: " *css-port*)
      (display (sp-to-ems (+ (tex-length .8 ':in)
                             (find-dimen "\\hoffset"))) *css-port*)
      (display "em; }" *css-port*)
      (newline *css-port*)
      (when (or (tex2page-flag-boolean "\\TZPrightjustify")
                (not (tex2page-flag-boolean "\\TZPraggedright")))
        (display "body { text-align: justify; }" *css-port*)
        (newline *css-port*))
      (display "p { margin-bottom: 0pt; }" *css-port*)
      (newline *css-port*)
      (display "p { text-indent: " *css-port*)
      (display (sp-to-pixels (find-dimen "\\parindent")) *css-port*)
      (display "pt; }" *css-port*)
      (newline *css-port*)
      (display "p { margin-top: " *css-port*)
      (display (sp-to-pixels (find-dimen "\\parskip")) *css-port*)
      (display "pt; }" *css-port*)
      (newline *css-port*)
      (display ".mathdisplay { margin-top: " *css-port*)
      (display (sp-to-pixels (find-dimen "\\abovedisplayskip")) *css-port*)
      (display "pt; margin-bottom: " *css-port*)
      (display (sp-to-pixels (find-dimen "\\belowdisplayskip")) *css-port*)
      (display "pt; }" *css-port*)
      (newline *css-port*)
      (display ".navigation { color: black; font-style: normal; }" *css-port*)
      (newline *css-port*)
      )
    (set-text-width)
    (unless (tex2page-flag-boolean "\\TZPtextext")
      (write-aux `(!tex-text 1)))
    ))

(define insert-missing-end
  (lambda ()
    (write-log ':separation-newline)
    (write-log "! Missing \\end inserted.")
    (write-log ':separation-newline)))

;diacritics

(define do-diacritic
  (let ((top-diacritics
         '(:grave :acute :circumflex :umlaut :tilde :macron :breve
           :hacek :hungarianumlaut :ring)))
    (lambda (diac)
      (let ((x (get-token-or-peeled-group)))
        (cond ((and (string=? x "\\i") (member diac top-diacritics))
               (emit "i"))
              ((and (string=? x "\\j") (member diac top-diacritics))
               (emit "j"))
              (else (tex2page-string x))))
      (emit
       (case diac
         ((:grave) "&#x300;")
         ((:acute) "&#x301;")
         ((:circumflex) "&#x302;")
         ((:umlaut) "&#x308;")
         ((:tilde) "&#x303;")
         ((:macron) "&#x304;")
         ((:dot) "&#x307;")
         ((:breve) "&#x306;")
         ((:hacek) "&#x30c;")
         ((:hungarianumlaut) "&#x30b;")
         ((:cedilla) "&#x327;")
         ((:dotunder) "&#x323;")
         ((:barunder) "&#x331;")
         ((:tieafter) "&#x361;")
         ((:ring) "&#x30a;")
         ((:ogonek) "&#x328;")
         (else (terror 'do-diacritic
                       diac " is not a valid diacritic")))))))

;Math

(define do-mathdg
  (lambda ()
    (fluid-let ((*math-mode-p* #t) (*in-display-math-p* #t)
                (*tabular-stack* '()) (*ligatures-p* #f))
      (do-end-para)
      (emit "<div align=")
      (emit *display-justification*)
      (emit "><table><tr><td>")
      (tex2page-string (get-group))
      (emit "</td></tr></table></div>")
      (do-para))))

(define do-mathg
  (lambda ()
    (fluid-let ((*math-mode-p* #t) (*in-display-math-p* #f)
                (*tabular-stack* '()) (*ligatures-p* #f))
      (tex2page-string (get-group)))))

(define dump-tex-preamble
  (lambda (o)
    (case *tex-format*
      ((:latex)
       (display "\\documentclass{" o)
       (display (if *using-chapters-p* "report" "article") o)
       (display "}" o) (newline o)
       (display *imgpreamble* o) (newline o)
       ;(display "\\ifx\\bmatrix\\UNDEFINED" o)
       ;(display "\\usepackage{amsmath}\\fi" o) (newline o)
       (when (memv ':includegraphics *imgpreamble-inferred*)
         (display "\\ifx\\includegraphics\\UNDEFINED" o)
         (display "\\usepackage{graphicx}\\fi" o)
         (newline o))
       (when (memv ':epsfbox *imgpreamble-inferred*)
         (display "\\ifx\\epsfbox\\UNDEFINED" o)
         (display "\\usepackage{epsfig}\\fi" o)
         (newline o))
       (display "\\thispagestyle{empty}" o) (newline o)
       (display "\\begin{document}" o) (newline o))
      (else
       (display *imgpreamble* o) (newline o)
       (when (memv ':includegraphics *imgpreamble-inferred*)
         (display "\\ifx\\resetatcatcode\\UNDEFINED" o)
         (display "\\input miniltx \\fi" o)
         (newline o)
         (display "\\ifx\\includegraphics\\UNDEFINED" o)
         (display "\\input graphicx.sty \\fi" o)
         (newline o))
       (when (memv ':epsfbox *imgpreamble-inferred*)
         (display "\\ifx\\epsfbox\\UNDEFINED" o)
         (display "\\ifx\\pdfoutput\\UNDEFINED\\input epsf \\else" o)
         (display "\\input supp-pdf " o)
         (display "\\def\\epsfbox#1{\\convertMPtoPDF{#1}{1}{1}}\\fi" o)
         (newline o))
       (display "\\nopagenumbers" o) (newline o)))))

(define dump-tex-postamble
  (lambda (o)
    (case *tex-format*
      ((:latex)
       (display "\\end{document}" o) (newline o))
      (else
        (display "\\bye" o) (newline o)))))

(define skipping-img-file
  (lambda ()
    (set! *img-file-count* (+ *img-file-count* 1))))

(define next-html-image-file-stem
  (lambda ()
    (set! *img-file-count* (+ *img-file-count* 1))
    (string-append *subjobname* *img-file-suffix*
                   (number->string *img-file-count*))))

(define call-with-html-image-port
  (lambda (p . alt)
    (let* ((alt (if (null? alt) #f (car alt)))
           (img-file-stem (next-html-image-file-stem))
           (aux-tex-file (string-append img-file-stem ".tex")))
      (ensure-file-deleted aux-tex-file)
      (call-with-output-file aux-tex-file
        (lambda (o)
          (dump-tex-preamble o)
          (p o)
          (dump-tex-postamble o)))
      (tex-to-img img-file-stem)
      (source-img-file img-file-stem alt))))

(define do-math-fragment
  (lambda (s display-p)
    (when display-p
      (emit "<div class=mathdisplay align=")
      (emit *display-justification*)
      (emit ">")
      (emit "<table><tr><td>"))
    (let ((old-math-mode-p *math-mode-p*)
          (old-in-display-math-p *in-display-math-p*)
          (old-tabular-stack *tabular-stack*))
      (set! *math-mode-p* #t)
      (set! *in-display-math-p* display-p)
      (set! *tabular-stack* '())
      (bgroup)
      (toss-back-char #\})
      (toss-back-string s)
      (add-aftergroup-to-top-frame
       (lambda ()
         (set! *math-mode-p* old-math-mode-p)
         (set! *in-display-math-p* old-in-display-math-p)
         (set! *tabular-stack* old-tabular-stack)
         (when display-p
           (emit "</td></tr></table>")
           (emit "</div>")
           (do-noindent)))))))

(define do-display-math
  (lambda (tex-string)
    (do-end-para)
    (if (and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                 (tex2page-flag-boolean "\\TZPmathimage"))
             (not *temporarily-use-utf8-for-math-p*))
        (begin
         (emit "<div class=mathdisplay align=")
         (emit *display-justification*)
         (emit ">")
         (call-with-html-image-port
          (lambda (o) (display "$$" o) (display tex-string o) (display "$$" o))
          tex-string)
         (emit "</div>")
         (do-noindent))
      (do-math-fragment tex-string ':display))))

(define tex-math-delim-string
  (lambda (type)
    (let ((top #f) (mid #f) (bot #f) (ext #f))
      (case type
        ((:lparen) (set! top "&#x239b;")
                   (set! bot "&#x239d;")
                   (set! ext "&#x239c;")
                   (set! mid ext))
        ((:lbrack) (set! top "&#x23a1;")
                   (set! bot "&#x23a3;")
                   (set! ext "&#x23a2;")
                   (set! mid ext))
        ((:lbrace) (set! top "&#x23a7;")
                   (set! mid "&#x23a8;")
                   (set! bot "&#x23a9;")
                   (set! ext "&#x23aa;"))
        ((:lvert) (set! ext "&#x239c;")
                  (set! top ext)
                  (set! mid ext)
                  (set! bot ext))
        ((:rparen) (set! top "&#x239e;")
                   (set! bot "&#x23a0;")
                   (set! ext "&#x239f;")
                   (set! mid ext))
        ((:rbrack) (set! top "&#x23a4;")
                   (set! bot "&#x23a6;")
                   (set! ext "&#x23a5;")
                   (set! mid ext))
        ((:rbrace) (set! top "&#x23ab;")
                   (set! mid "&#x23ac;")
                   (set! bot "&#x23ad;")
                   (set! ext "&#x23ae;"))
        ((:rvert) (set! ext "&#x239f;")
                  (set! top ext)
                  (set! mid ext)
                  (set! bot ext))
        (else (terror 'tex-math-delim-string "bad delim" type)))
      (string-append
       "<table cellpadding=0 cellspacing=0><tr><td>" top "</td></tr>"
       (cond ((odd? *math-height*)
              (string-append
               (let loop ((r "") (i (/ (- *math-height* 1) 2)))
                 (if (<= i 0) r
                   (loop (string-append r
                                        "<tr><td>"
                                        ext
                                        "</td></tr>") (- i 1))))
               "<tr><td>"
               mid
               "</td></tr>"
               (let loop ((r "") (i (/ (- *math-height* 1) 2)))
                 (if (<= i 0) r
                   (loop (string-append r
                                        "<tr><td>"
                                        ext
                                        "</td></tr>") (- i 1))))))
             (else
              (let loop ((r "") (i *math-height*))
                (if (<= i 0) r
                  (loop (string-append r
                                       "<tr><td>"
                                       ext
                                       "</td></tr>") (- i 1))))))
       "<tr><td>" bot "</td></tr></table>"))))

(define tex-math-string-to-html-string
  (lambda (s)
    (let ((tmp-port (open-output-string)))
      (fluid-let ((*html* tmp-port))
        (call-with-input-string/buffered ""
          (lambda ()
            (do-math-fragment s #f)
            (generate-html))))
      (get-output-string tmp-port))))

(define do-intext-math
  (lambda (tex-string)
    (fluid-let ((*math-needs-image-p* #f))
      (let ((html-string (tex-math-string-to-html-string tex-string)))
        (if (and (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                     (tex2page-flag-boolean "\\TZPmathimage"))
                 *math-needs-image-p*
                 (not *temporarily-use-utf8-for-math-p*))
            (call-with-html-image-port
             (lambda (o) (display #\$ o) (display tex-string o) (display #\$ o))
             tex-string)
          (emit html-string))))))

(define do-mathp
  (lambda ()
      (call-with-html-image-port
        (lambda (o)
          (display #\$ o)
          (display (get-group) o)
          (display #\$ o)))))

(define do-latex-intext-math
  (lambda ()
    (do-intext-math
      (let ((o (open-output-string)))
        (dump-till-ctl-seq "\\)" o)
        (get-output-string o)))))

(define do-latex-display-math
  (lambda ()
    (do-display-math
      (let ((o (open-output-string)))
        (dump-till-ctl-seq "\\]" o)
        (get-output-string o)))))

(define do-math
  (lambda ()
    (let ((display? #f))
      (when (eqv? (snoop-actual-char) #\$)
        (set! display? #t) (get-actual-char))
      (let ((o (open-output-string)))
        (dump-till-char #\$ o)
        (when display?
          (let ((c (get-actual-char)))
            (when (or (eof-object? c) (not (char=? c #\$)))
              (terror 'do-math "Display math should end with $$."))))
        ((if display?
             do-display-math
             do-intext-math)
         (get-output-string o))))))

(define dump-till-char
  (lambda (d o)
    (let loop ((nesting 0) (escape? #f))
      (let ((c (get-actual-char)))
        (cond ((eof-object? c)
               (terror 'dump-till-char "Missing " d "."))
              ((and (char=? c d) (= nesting 0)) #t)
              (else (display c o)
                (cond (escape? (loop nesting #f))
                      ((char=? c #\{) (loop (+ nesting 1) #f))
                      ((char=? c #\}) (loop (- nesting 1) #f))
                      ((char=? c #\\ ) (loop nesting #t))
                      (else (loop nesting #f)))))))))

(define dump-till-ctl-seq
  (lambda (cs o)
    (fluid-let ((*not-processing-p* #t))
      (let loop ((nesting 0))
        (let ((c (snoop-actual-char)))
          (cond ((eof-object? c) (terror 'dump-till-ctl-seq))
                ((esc-char-p c)
                 (let ((x (get-ctl-seq)))
                   (if (string=? x cs) #t
                       (begin (display x o)
                         (loop nesting)))))
                (else (display (get-actual-char) o)
                  (cond ((char=? c #\{) (loop (+ nesting 1)))
                        ((char=? c #\}) (loop (- nesting 1)))
                        (else (loop nesting))))))))))

(define dump-till-end-env
  (lambda (env o)
    (let* ((endenv (string-append "\\end" env))
           (endenv-prim (find-corresp-prim endenv))
           (endenv-prim-th (find-corresp-prim-thunk endenv)))
      (fluid-let ((*not-processing-p* #t))
        (let loop ((brace-nesting 0) (env-nesting 0))
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c) (terror 'dump-till-end-env env))
                  ((esc-char-p c)
                   (let ((x (get-ctl-seq)))
                     (cond ((string=? (find-corresp-prim x) endenv-prim) #t)
                           ((string=? x "\\begin") (display x o)
                            (let ((g (get-grouped-environment-name-if-any)))
                              (when g
                                (display #\{ o) (display g o) (display #\} o))
                              (loop brace-nesting
                                (if (and g (string=? g env)) (+ env-nesting 1)
                                    env-nesting))))
                           ((string=? x "\\end")
                            (let ((g (get-grouped-environment-name-if-any)))
                              (unless
                                (and g (or *dumping-nontex-p* (= env-nesting 0))
                                     (let ((endg (string-append "\\end" g)))
                                       (or (string=? (find-corresp-prim endg)
                                                     endenv-prim)
                                           (eqv? (find-corresp-prim-thunk endg)
                                                 endenv-prim-th))))
                                (display x o)
                                (when g
                                  (display #\{ o) (display g o)
                                  (display #\} o))
                                (loop brace-nesting
                                      (if (and g (string=? g env))
                                          (- env-nesting 1)
                                          env-nesting)))))
                           (else (display x o)
                             (loop brace-nesting env-nesting)))))
                  ((and (char=? c *comment-char*)
                        (not *dumping-nontex-p*))
                   (do-comment)
                   (write-char #\% o) (newline o)
                   (loop brace-nesting env-nesting))
                  (else (write-char (get-actual-char) o)
                    (cond ((char=? c #\{)
                           (loop (+ brace-nesting 1) env-nesting))
                          ((char=? c #\})
                           (loop (- brace-nesting 1) env-nesting))
                          (else (loop brace-nesting env-nesting)))))))))))

;images

(define dump-imgdef
  (lambda (f)
    (let ((aux-tex-file (string-append f ".tex")))
      (ensure-file-deleted aux-tex-file)
      (call-with-output-file aux-tex-file
        (lambda (o)
          (dump-tex-preamble o)
          (display (ungroup (get-group)) o)
          (dump-tex-postamble o))))))

(define do-img-preamble
  (lambda ()
    ;(if *imgpreamble*
    ;    (terror 'do-img-preamble "Calling \\imgpreamble more than once"))
    (set! *imgpreamble*
      (fluid-let ((*not-processing-p* #t))
        (let loop ((r *imgpreamble*))
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'do-img-preamble "Missing \\endimgpreamble"))
                  ((esc-char-p c)
                   (let ((x (get-ctl-seq)))
                     (cond ((ormap (lambda (z) (string=? x z))
                                   '("\\endimgpreamble"
                                     "\\endgifpreamble"
                                     "\\endmathpreamble"))
                            r)
                           (else (loop (string-append r x))))))
                  (else (get-actual-char)
                    (loop (string-append r (string c)))))))))))

;streams

(define pick-new-stream-number
  (lambda (stream-list)
    (let loop ((i 0))
      (if (or (table-get stream-list i)
              (= i 16)
              (= i 18))
          (loop (+ i 1))
          i))))

(define do-new-stream
  (lambda (type)
    (let* ((x (get-ctl-seq))
           (sl (if (eqv? type ':out) *output-streams*
                 *input-streams*))
           (n (pick-new-stream-number sl)))
      (tex-def-count x n #t) ;streams are global?
      (table-put! sl n ':free))))

(define do-open-stream
  (lambda (type)
    (let* ((n (get-number))
           (f (get-filename))
           (sl (if (eqv? type ':out)
                   *output-streams* *input-streams*))
           (c (table-get sl n)))
      (unless (eq? c ':free)
        (terror 'do-open-stream))
      (table-put! sl n
                  (case type
                    ((:out)
                     (set! f (add-dot-tex-if-no-extension-provided f))
                     (ensure-file-deleted f)
                     (open-output-file f))
                    (else
                     (set! f (actual-tex-filename f #f))
                     (make-bport 'port (open-input-file f))))))))

(define do-close-stream
  (lambda (type)
    (let* ((sl (if (eqv? type ':out)
                   *output-streams*
                   *input-streams*))
           (o (get-number))
           (c (table-get sl o)))
      (when (eq? c ':free)
        (terror 'do-close-stream))
      (case type
        ((:out) (close-output-port  c))
        ((:in) (close-output-port (bport.port c))))
      (table-put! sl o ':free))))

(define tex-write-output-string
  (lambda (s)
    (let ((o (open-output-string)))
      (fluid-let ((*outputting-to-non-html-p* #t)
                  (*html* o))
        (call-with-input-string/buffered
         s (lambda ()
             (let loop ()
               (let ((c (snoop-actual-char)))
                 (unless (eof-object? c)
                   (case c
                     ((#\\ ) (do-tex-ctl-seq (get-ctl-seq)))
                     (else (emit-html-char (get-actual-char))))
                   (loop)))))))
      (get-output-string o))))

(define do-write-aux
  (lambda (o)
    (let ((output (tex-write-output-string (get-peeled-group))))
      (cond ((and (= o 18) *enable-write-18-p*)
             (system output))
            ((or (= o 16) (= o 18))
             (write-log output)
             (write-log ':separation-space))
            ((table-get *output-streams* o)
             => (lambda (p)
                    (cond ((eq? p ':free) (terror 'do-write-aux))
                          (else
                            (display output p)
                            (display #\space p)))))
            (else (terror 'do-write))))))

(define do-write
  (lambda ()
    (do-write-aux (get-number))))

(define do-message
  (lambda ()
    (do-write-aux 16)))

(define read-tex-line
  (lambda (p)
    (fluid-let ((*current-tex2page-input* p))
      (let loop ((r '()))
        (let ((c (snoop-actual-char)))
          (cond ((eof-object? c)
                 (if (null? r) c (list->string (nreverse r))))
                ((char=? c #\newline)
                 (get-actual-char)
                 (list->string (nreverse r)))
                ((char=? c #\{)
                 (string-append (list->string (nreverse r))
                                (get-group)))
                (else
                 (loop (cons (get-actual-char) r)))))))))

(define do-read
  (lambda (g?)
    (let* ((i (get-number))
           (x (begin (get-to) (get-ctl-seq)))
           (p #f))
      (cond ((ormap (lambda (j) (= i j)) '(-1 16))
             (set! p (make-bport 'port (current-input-port)))
             (unless (= i -1)
               (write-log x) (write-log #\=)))
            ((table-get *input-streams* i)
             => (lambda (c)
                  (set! p c)
                  (unless (eq? p ':free) (terror 'do-read))))
            (else (terror 'do-read)))
      ((if g? tex-gdef-0arg tex-def-0arg)
       x (let ((line (read-tex-line p)))
           (if (eof-object? line) "" line))))))

(define do-typein
  (lambda ()
    (let ((ctlseq (get-bracketed-text-if-any))
          (p (make-bport 'port (current-input-port))))
      (write-log ':separation-newline)
      (write-log (tex-string-to-html-string (get-group)))
      (write-log ':separation-newline)
      (write-log (or ctlseq "\\@typein"))
      (write-log #\=)
      (let ((L (read-tex-line p)))
        (when (eof-object? L) (set! L ""))
        (cond (ctlseq
               (tex-def-0arg ctlseq L))
              (else
               (tex2page-string L)))))))

;conditionals

(define do-ifeof
  (lambda ()
    (let* ((i (get-number))
          (c (table-get *input-streams* i)))
      (when (eq? c ':free)
        (terror 'do-ifeof))
      (if (eof-object? (read-char  c))
          (do-iftrue)
          (do-iffalse)))))

(define do-iffalse
  (lambda ()
    (set! *tex-if-stack* (cons #f *tex-if-stack*))))

(define do-iftrue
  (lambda ()
    (set! *tex-if-stack* (cons #t *tex-if-stack*))))

(define insert-tex-if ;???
  (lambda (test)
    ((if test do-iftrue do-iffalse))))

(define do-ifx
  (lambda ()
    (let* ((one (get-raw-token/is))
           (two (get-raw-token/is))
           (one2 one)
           (two2 two))
      ;NB: doesn't work like tex's \ifx if
      ;one of the args is a "primitive" ctl seq
      ((if (string=? one two) do-iftrue
           (begin
             (when (ctl-seq? one)
               (set! one2
                 (cond ((find-def one)
                        => (lambda (d)
                             (or (tdef.expansion d)
                                 (tdef.prim d))))
                       ((find-math-def one) => (lambda (x) x))
                       (else "UnDeFiNeD"))))
             (when (ctl-seq? two)
               (set! two2
                 (cond ((find-def two)
                        => (lambda (d)
                             (or (tdef.expansion d)
                                 (tdef.prim d))))
                       ((find-math-def two) => (lambda (x) x))
                       (else "UnDeFiNeD"))))
             (if (or (eqv? one2 two2)
                     (and (string? one2) (string? two2)
                          (string=? one2 two2)))
                 do-iftrue
                 do-iffalse)))))))

(define do-ifdefined
  (lambda ()
    (let ((x (get-raw-token/is)))
      ((if (or (not (ctl-seq? x))
               (and (ctl-seq? x)
                    (or (find-def x) (find-math-def x))))
           do-iftrue
           do-iffalse)))))

(define do-if-get-atomic
  (lambda ()
    (let loop ()
      (let ((x (get-raw-token/is)))
        (if (ctl-seq? x)
            (cond ((resolve-defs x)
                   => (lambda (z)
                        (toss-back-char *invisible-space*)
                        (toss-back-string z)
                        (loop)))
                  (else x))
            x)))))

(define do-if
  (lambda ()
    (let* ((one (do-if-get-atomic))
           (two (do-if-get-atomic)))
      ((if (or (string=? one two)
               (and (ctl-seq? one) (ctl-seq? two)))
           do-iftrue
           do-iffalse)))))

(define do-ifmmode
  (lambda ()
    (set! *tex-if-stack*
      (cons *math-mode-p* *tex-if-stack*))))

(define do-ifnum
  (lambda ()
    (let* ((one (get-number))
           (rel (string-ref (get-raw-token/is) 0)))
      ((if ((case rel
              ((#\<) <)
              ((#\=) =)
              ((#\>) >)
              (else (terror 'do-ifnum "Missing = for \\ifnum.")))
            one (get-number))
           do-iftrue
           do-iffalse)))))

(define read-ifcase-clauses
  (lambda ()
    (fluid-let ((*not-processing-p* #t))
      (let* ((else-clause #f)
             (or-clauses
              (let loop ((or-clauses '()) (else? #f))
                (let loop2 ((clause ""))
                  (let ((c (snoop-actual-char)))
                    (cond ((eof-object? c)
                           (terror 'read-ifcase-clauses "Incomplete \\ifcase."))
                          ((esc-char-p c)
                           (let ((x (get-ctl-seq)))
                             (cond ((string=? x "\\or")
                                    (ignorespaces)
                                    (if else?
                                        (terror 'read-ifcase-clauses
                                                "\\or after \\else")
                                        (loop (cons clause or-clauses) #f)))
                                   ((string=? x "\\else")
                                    (ignorespaces)
                                    (if else?
                                        (terror 'read-ifcase-clauses
                                                "\\else after \\else")
                                        (loop (cons clause or-clauses) #t)))
                                   ((string=? x "\\fi")
                                    (ignorespaces)
                                    (if else?
                                        (begin
                                         (set! else-clause clause)
                                         or-clauses)
                                        (cons clause or-clauses)))
                                   (else
                                    (loop2 (string-append clause x))))))
                          (else (get-actual-char)
                                (loop2 (string-append clause (string c)))))
                    )))))
        (cons else-clause or-clauses)))))

(define do-ifcase
  (lambda ()
    (let* ((num (get-number))
           (clauses (read-ifcase-clauses))
           (else-clause (car clauses))
           (or-clauses (nreverse (cdr clauses)))
           (num-or-clauses (length or-clauses)))
      (cond ((< num num-or-clauses)
             (tex2page-string
              (list-ref or-clauses num)))
            (else-clause
             (tex2page-string else-clause))))))

(define do-ifodd
  (lambda ()
    ((if (odd? (get-number))
         do-iftrue
         do-iffalse))))

(define do-else
  (lambda ()
    (when (null? *tex-if-stack*) (terror 'do-else "Extra \\else"))
    (let ((top-if (car *tex-if-stack*)))
      (set! *tex-if-stack* (cons (not top-if) (cdr *tex-if-stack*))))))

(define do-fi
  (lambda ()
    (when (null? *tex-if-stack*) (terror 'do-fi "Extra \\fi"))
    (set! *tex-if-stack* (cdr *tex-if-stack*))))

(define do-newif
  (lambda ()
    (let* ((iffoo (get-ctl-seq))
           (init-val #f)
           (foo (string-append "\\" (substring iffoo 3 (string-length iffoo))))
           (foo-register (string-append foo "BOOLEANREGISTER")))
      (tex-def-count foo-register 0 #f)
      (tex-def-thunk iffoo
                     (lambda ()
                       (set! *tex-if-stack*
                         (cons (> (the-count foo-register) 0)
                               *tex-if-stack*))) #f)
      (tex-def-thunk (string-append foo "true")
                     (lambda ()
                       (tex-def-count foo-register 1 #f)) #f)
      (tex-def-thunk (string-append foo "false")
                     (lambda ()
                       (tex-def-count foo-register 0 #f)) #f))))

;

(define do-htmlimg
  (lambda (env)
    ;(emit "<p><center>")
      (call-with-html-image-port
        (lambda (o)
          (dump-till-end-env env o))
      ;(emit "</center><p>")
      )))

(define (find-img-file-extn)
  (case (tex2page-flag-value "\\TZPimageformat")
    ((#\g #\G) ".gif")
    ((#\j #\J) ".jpeg")
    (else ".png")))

(define do-htmlimageformat
  (lambda ()
    (tex-def-0arg "\\TZPimageformat" (get-peeled-group)) ))

(define do-htmlimageconversionprogram
  (lambda ()
    (tex-def-0arg "\\TZPimageconverter" (get-peeled-group))))

(define do-htmlimgmagnification
  (lambda ()
    #t ; obsolete
    ))

(define call-mp
  (let ((tex-prog-name #f))
    (lambda (f)
      (unless tex-prog-name
        (set! tex-prog-name "tex")
        #|
        (let ((d (find-def "\\TZPtexprogname")))
          (when d (set! tex-prog-name (tdef*-expansion d))))
        (unless tex-prog-name (set! tex-prog-name "xetex"))
        (when (eq? *tex-format* ':latex)
          (set! tex-prog-name
            (string-append (substring tex-prog-name 0
                                      (- (string-length tex-prog-name) 3))
                           "latex")))
        |#)
      (system (string-append *metapost* " -tex=" tex-prog-name " " f)))))

(define call-tex
  (let ((tex-prog-name #f)
        (tex-output-format #f))
  (lambda (f)
    ;run tex on f and return f.ps or f.pdf if successful
    (unless tex-prog-name
      (let ((d (find-def "\\TZPtexprogname")))
        (when d (set! tex-prog-name (tdef.expansion d))))
      (unless tex-prog-name (set! tex-prog-name "pdftex"))
      (set! tex-output-format
        (if (or (eqv? (substring? "pdf" tex-prog-name) 0)
                (eqv? (substring? "xe" tex-prog-name) 0)
                (eqv? (substring? "lua" tex-prog-name) 0))
          ':pdf ':dvi))
      (when (eq? *tex-format* ':latex)
        (set! tex-prog-name
          (string-append (substring tex-prog-name
                                    0 (- (string-length tex-prog-name) 3))
                         "latex"))))
    (let* ((dvi-file (string-append
                      f (if (eq? tex-output-format ':pdf)
                          ".pdf" ".dvi")))
           (outfile dvi-file))
      ;[shd we check if dvi-file already exists? no]
      (system (string-append
                tex-prog-name
               " " f))
      (and (file-exists? dvi-file)
           (let ((logfile (string-append f ".log")))
             (and (file-exists? logfile)
                  ;scan the log file for sign of problems
                  (let ((fine?
                         (call-with-input-file logfile
                           (lambda (i)
                             (let loop ()
                               (let ((x (read-line i)))
                                 (cond ((eof-object? x) #t)
                                       ((substring? "! I can't find file" x)
                                        ;the dvi can't be good
                                        #f)
                                       (else (loop)))))))))
                    (and fine?
                         (begin
                          (unless (eq? tex-output-format ':pdf)
                            (let ((ps-file (string-append f ".ps")))
                              (system
                               (string-append "dvips " dvi-file " -o " ps-file))
                              (set! outfile ps-file)))
                          outfile))))))))))

(define ps-to-img/gif/netpbm
  (lambda (ps-file img-file)
    (system
      (string-append *ghostscript* *ghostscript-options* " -sOutputFile=" img-file ".ppm.1 "
                     ps-file " quit.ps"))
    (system
      (string-append "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
;    (unless (= *img-magnification* 1)
;       (system
;         (string-append "pbmpscale " (number->string *img-magnification*)
;                        " " f-ppm-tmp " > " f-ppm))
;       (let ((swp f-ppm))
;         (set! f-ppm f-ppm-tmp) (set! f-ppm-tmp swp)))
    (system
      (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
    (system
      (string-append "ppmtogif -transparent rgb:ff/ff/ff < "
                     img-file ".ppm > " *aux-dir/* img-file))
    (for-each
       (lambda (e)
         (ensure-file-deleted (string-append img-file e)))
       '(".ppm" ".ppm.tmp" ".ppm.1"))))

(define ps-to-img/png/netpbm
  (lambda (ps-file img-file)
    (system
      (string-append *ghostscript* *ghostscript-options* " -sOutputFile=" img-file ".ppm.1 "
                    ps-file " quit.ps"))
    (system
      (string-append "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
    '(system
       (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
    (system
      (string-append "pnmtopng -interlace -transparent \"#FFFFFF\" "
                     " < " img-file ".ppm.tmp > " *aux-dir/* img-file))
    (for-each
      (lambda (e)
        (ensure-file-deleted (string-append img-file e)))
      '(".ppm.1" ".ppm.tmp" ".ppm"))
    ))

(define ps-to-img/jpeg/netpbm
  (lambda (ps-file img-file)
    (system
      (string-append *ghostscript* *ghostscript-options* " -sOutputFile=" img-file ".ppm.1 "
                    ps-file " quit.ps"))
    (system
      (string-append "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
    (system
      (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
    (system
      (string-append "ppmtojpeg --grayscale < " img-file ".ppm > "
        *aux-dir/* img-file))
    (for-each
      (lambda (e)
        (ensure-file-deleted (string-append img-file e)))
      '(".ppm.1" ".ppm.tmp" ".ppm"))
    ))

(define ps-to-img
  (lambda (ps-file img-file)
    (case (tex2page-flag-value "\\TZPimageconverter")
      ((#\i #\I)
       (system
         (string-append "convert -transparent white -trim "
                  ps-file  " " img-file)))
      (else
        ((case (tex2page-flag-value "\\TZPimageformat")
           ((#\p #\P) ps-to-img/png/netpbm)
           ((#\j #\J) ps-to-img/jpeg/netpbm)
           (else ps-to-img/gif/netpbm)) ps-file img-file)))))

(define tex-to-img
  (lambda (f)
    ;converts the TeX subfile 'f' into img.  Follows the same
    ;route as used in LaTeX2HTML
    (set! *img-file-tally* (+ *img-file-tally* 1))
    (let ((img-file (string-append *aux-dir/* f (find-img-file-extn))))
      (unless (file-exists? img-file)
        (write-log ':separation-space)
        (write-log #\{)
        (write-log (string-append f ".tex"))
        (write-log ':separation-space)
        (write-log "->")
        (write-log ':separation-space)
        (cond ((call-tex f)
               => (lambda (ps-file)
               (ps-to-img ps-file img-file)
               (write-log img-file)
               '(for-each
                  (lambda (e)
                    (ensure-file-deleted (string-append f e)))
                  '(".aux" ".dvi" ".log" ".pdf" ".ps" ".tex"))))
              (else
                (write-log "failed, try manually")))
        (write-log #\})
        (write-log ':separation-space)
        )
      ;(move-aux-files-to-aux-dir f)
      )))

(define call-with-lazy-image-port
  (lambda (eps-file img-file-stem p)
    (let ((aux-tex-file (string-append img-file-stem ".tex")))
      (ensure-file-deleted aux-tex-file)
      (call-with-output-file aux-tex-file
        (lambda (o)
          (dump-tex-preamble o)
          (p o)
          (dump-tex-postamble o)))
      (if (file-exists? eps-file) (tex-to-img img-file-stem)
          (set! *missing-eps-files*
            (cons (cons eps-file img-file-stem)
                  *missing-eps-files*))))))

(define retry-lazy-image
  (lambda (eps-file img-file-stem)
    (cond ((file-exists? eps-file) (tex-to-img img-file-stem))
          (else
           (write-log "! I can't find EPS file ")
           (write-log eps-file)
           (write-log ':separation-newline)))))

(define lazily-make-epsf-image-file
  (lambda (eps-file img-file-stem)
    (fluid-let ((*imgpreamble-inferred*
                 (cons ':epsfbox *imgpreamble-inferred*)))
      (call-with-lazy-image-port
       eps-file img-file-stem
       (lambda (o)
         (display "\\epsfbox{" o)
         (display eps-file o)
         (display #\} o))))
    ;(source-img-file img-file-stem)
    ))

(define do-epsfbox
  (lambda ()
    (let* ((b (get-bracketed-text-if-any))
           (f (get-filename-possibly-braced)))
      (unless *eval-for-tex-only-p*
        (let ((epsf-x-size (get-dimen "\\epsfxsize"))
              (epsf-y-size (get-dimen "\\epsfysize")))
          (cond ((and (= epsf-x-size 0) (= epsf-y-size 0))
                 (let ((img-file-stem (next-html-image-file-stem)))
                   (lazily-make-epsf-image-file f img-file-stem)
                   (source-img-file img-file-stem)))
                (else
                  (unless (= epsf-x-size 0)
                    (tex2page-string "\\epsfxsize=0pt"))
                  (unless (= epsf-y-size 0)
                    (tex2page-string "\\epsfysize=0pt"))
                  (fluid-let ((*imgpreamble-inferred*
                                (cons ':epsfbox *imgpreamble-inferred*)))
                    (call-with-html-image-port
                      (lambda (o)
                        (unless (= epsf-x-size 0)
                          (display "\\epsfxsize=" o)
                          (display epsf-x-size o)
                          (display "sp" o) (newline o))
                        (unless (= epsf-y-size 0)
                          (display "\\epsfysize=" o)
                          (display epsf-y-size o)
                          (display "sp" o) (newline o))
                        (display "\\epsfbox{" o)
                        (display f o)
                        (display #\} o)))))))))))

(define do-epsfig
  (lambda ()
    (fluid-let ((*imgpreamble-inferred*
                  (cons ':epsfbox *imgpreamble-inferred*)))
      (call-with-html-image-port
        (lambda (o)
          (display "\\epsfig{" o)
          (dump-groupoid o)
          (display #\} o))))))

(define do-convertmptopdf
  (lambda ()
    (let ((f (get-filename-possibly-braced))
          (img-file-stem (next-html-image-file-stem)))
      (get-token) (get-token) ;ignore scale args 4now
      (lazily-make-epsf-image-file f img-file-stem)
      (source-img-file img-file-stem))))

(define do-includegraphics-web
  (lambda (bracketed-text image-file)
    (emit "<img")
    (when bracketed-text
      (let ((height #f) (width #f))
        (toss-back-string " enoughalready ")
        (toss-back-string bracketed-text)
        (let loop ()
          (cond ((eat-word "height") (get-equal-sign)
                                     (set! height (get-pixels)) (loop))
                ((eat-word "width") (get-equal-sign)
                                    (set! width (get-pixels)) (loop))
                ((eat-word "enoughalready") (ignorespaces))
                (else (get-actual-char) (loop))))
        (when height
          (emit " height=") (emit height))
        (when width
          (emit " width=") (emit width))))
    (emit " src=\"")
    (emit (fully-qualify-url image-file))
    (emit "\">")))

(define do-includegraphics
  (lambda ()
    (let* ((star? (eat-star))
           (b1 (get-bracketed-text-if-any))
           (b2 (and b1 (get-bracketed-text-if-any)))
           (f (get-filename-possibly-braced))
           (ffull (if (file-exists? f) f
                      (ormap (lambda (e)
                               (let ((f2 (string-append f e)))
                                 (and (file-exists? f2) f2)))
                             *graphics-file-extensions*)))
           (ffull-ext (and ffull (file-extension ffull))))
      (cond ((and ffull-ext
                  (ormap (lambda (y) (string=? ffull-ext y))
                         '(".jpg" ".jpeg" ".png")))
             (do-includegraphics-web b1 ffull))
            (else
              (fluid-let ((*imgpreamble-inferred*
                            (cons ':includegraphics *imgpreamble-inferred*)))
                (let ((img-file-stem (next-html-image-file-stem)))
                  (call-with-lazy-image-port
                    (or ffull f) img-file-stem
                    (lambda (o)
                      (display "\\includegraphics" o)
                      (when star? (display #\* o))
                      (when b1
                        (display #\[ o)
                        (display b1 o)
                        (display #\] o))
                      (when b2
                        (display #\[ o)
                        (display b2 o)
                        (display #\] o))
                      (display #\{ o)
                      (display f o)
                      (display #\} o)))
                  (source-img-file img-file-stem))))))))

(define do-xetexpdffile
  (lambda ()
    (let* ((pdf-file (get-filename))
           (height #f) (rotated #f) (width #f)
           (img-file-stem (next-html-image-file-stem))
           (img-file (string-append *aux-dir/* img-file-stem (find-img-file-extn))))
      (let loop ()
        (cond ((eat-word "height") (set! height (get-pixels)) (loop))
              ((eat-word "rotated") (set! rotated (get-number)) (loop))
              ((eat-word "width") (set! width (get-pixels)) (loop))
              (else #f)))
      (unless (file-exists? img-file)
        (write-log ':separation-space)
        (write-log #\{)
        (write-log pdf-file)
        (write-log ':separation-space)
        (write-log "->")
        (write-log ':separation-space)
        (write-log img-file)
        (write-log #\})
        (write-log ':separation-space)
        (ps-to-img pdf-file img-file))
      (write-log #\()
      (write-log img-file)
      (write-log ':separation-space)
      (emit "<img src=\"")
      (emit img-file)
      (emit "\"")
      (when height (emit " height=") (emit height))
      (when rotated
        (set! rotated (- rotated))
        (emit " style=\"transform: rotate(")
        (emit rotated) (emit "deg)\""))
      (when width (emit " width=") (emit width))
      (emit ">")
      (write-log #\))
      (write-log ':separation-space))))

(define do-xetexpicfile
  (lambda ()
    (let ((img-file (get-filename))
          (height #f) (rotated #f) (width #f))
      (let loop ()
        (cond ((eat-word "height") (set! height (get-pixels)) (loop))
              ((eat-word "rotated") (set! rotated (get-number)) (loop))
              ((eat-word "width") (set! width (get-pixels)) (loop))
              (else #f)))
      (emit "<img src=\"")
      (emit img-file)
      (emit "\"")
      (when height (emit " height=") (emit height))
      (when rotated
        (set! rotated (- rotated))
        (emit " style=\"transform: rotate(")
        (emit rotated) (emit "deg)\""))
      (when width (emit " width=") (emit width))
      (emit ">"))))

(define do-resizebox
  (lambda ()
    (let* ((arg1 (get-group))
           (arg2 (get-group))
           (arg3 (get-group)))
      (fluid-let ((*imgpreamble-inferred*
                   (cons ':includegraphics *imgpreamble-inferred*)))
         (call-with-html-image-port
          (lambda (o)
            (display "\\resizebox" o)
            (display arg1 o)
            (display arg2 o)
            (display arg3 o)))))))

;mfpic

(define do-mfpic-opengraphsfile
  (lambda ()
    (set! *mfpic-file-stem* (get-filename-possibly-braced))
    (when *mfpic-port* (close-output-port *mfpic-port*))
    (let ((f (string-append *mfpic-file-stem* *mfpic-tex-file-suffix*)))
      (ensure-file-deleted f)
      (set! *mfpic-port* (open-output-file f)))
    (set! *mfpic-file-num* 0)
    (display "\\input mfpic \\usemetapost " *mfpic-port*) (newline *mfpic-port*)
    (display "\\opengraphsfile{" *mfpic-port*)
    (display *mfpic-file-stem* *mfpic-port*)
    (display #\} *mfpic-port*) (newline *mfpic-port*)
    (tex-def-prim "\\headshape"
                  (lambda ()
                    (let* ((g1 (get-group))
                           (g2 (get-group))
                           (g3 (get-group)))
                      (display "\\headshape" *mfpic-port*)
                      (display g1 *mfpic-port*)
                      (display g2 *mfpic-port*)
                      (display g3 *mfpic-port*)
                      (newline *mfpic-port*))))
    (tex-def-prim "\\mfpframesep" eat-dimen)
    (tex-def-prim "\\mftitle" get-group)
    ))

(define do-mfpic-closegraphsfile
  (lambda ()
    (display "\\closegraphsfile" *mfpic-port*)
    (newline *mfpic-port*)
    (close-output-port *mfpic-port*)
    (let ((tex-f (string-append *mfpic-file-stem* *mfpic-tex-file-suffix*))
          (mp-f (string-append *mfpic-file-stem* ".mp")))
      (unless (file-exists? mp-f)
        (fluid-let ((*tex-format* ':plain))
          (call-tex tex-f)))
      (when (file-exists? mp-f)
        (call-mp mp-f)))))

(define do-mfpic
  (lambda ()
    (display "\\mfpic" *mfpic-port*)
    (dump-till-end-env "mfpic" *mfpic-port*)
    (display "\\endmfpic" *mfpic-port*) (newline *mfpic-port*)
    (set! *mfpic-file-num* (+ *mfpic-file-num* 1))
    (let ((f (string-append *mfpic-file-stem* "."
                            (number->string *mfpic-file-num*)))
          (img-file-stem (next-html-image-file-stem)))
      (lazily-make-epsf-image-file f img-file-stem)
      (source-img-file img-file-stem))))

(define do-following-latex-env-as-image
  (lambda ()
    (do-latex-env-as-image
      (ungroup (get-group)) ':display)))

(define do-latex-env-as-image
  (lambda (env display-p)
    (let ((env2 (if (string=? env "align") "eqnarray" env)))
    (when (char=? (snoop-actual-char) #\*)
      (get-actual-char)
      (set! env (string-append env "*"))
      (set! env2 (string-append env2 "*")))
    (egroup) ;because \begin introduces a group
    (when display-p
      (do-end-para)
      (emit "<div align=")
      (emit *display-justification*)
      (emit ">"))
     (call-with-html-image-port
      (lambda (o)
        (display "\\begin{" o)
        (display env2 o)
        (display "}" o)
        (dump-till-end-env env o)
        (display "\\end{" o)
        (display env2 o)
        (display "}" o) (newline o)
        ))
    (when display-p
      (emit "</div>")
      (do-para)))))

(define do-box
  (lambda ()
    (fluid-let ((*ignore-active-space-p* #t))
      (ignorespaces)
      (get-to))
    (eat-dimen)
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (case c
        ((#\{) #t)
        ((#\\ ) (get-ctl-seq))
        ;(else error) ;well let's not stop the show on this
        ))
    (get-actual-char)
    (bgroup)
    (add-postlude-to-top-frame
      (let ((old-math-mode-p *math-mode-p*)
            (old-in-display-math-p *in-display-math-p*)
            (old-tabular-stack *tabular-stack*)
            (old-ligatures-p *ligatures-p*))
        (set! *math-mode-p* #f)
        (set! *in-display-math-p* #f)
        (set! *tabular-stack* '())
        (set! *ligatures-p* #t)
        (lambda ()
          (set! *math-mode-p* old-math-mode-p)
          (set! *in-display-math-p* old-in-display-math-p)
          (set! *tabular-stack* old-tabular-stack)
          (set! *ligatures-p* old-ligatures-p))))))

(define do-latex-frac
  (lambda ()
    (tex2page-string (ungroup (get-token)))
    (emit
      (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
            (else (set! *math-height* (+ *math-height* 1))
               "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>")))
    (tex2page-string (ungroup (get-token)))))

(define do-tex-frac
  (lambda ()
    (ignorespaces)
    (let ((inner-level? (or (not *in-display-math-p*)
                            (not (null? *tabular-stack*)))))
      (fluid-let ((*tabular-stack* (cons 'frac *tabular-stack*)))
        (cond (inner-level?
               (emit "<sup>")
               (tex2page-string (get-till-char #\/))
               (emit "</sup>/<sub>")
               (get-actual-char) (ignorespaces)
               (tex2page-string (get-token))
               (emit "</sub>"))
              (else
               (emit "</td><td><table align=left><tr><td align=center>")
               (tex2page-string (get-till-char #\/))
               (get-actual-char) (ignorespaces)
               (emit  "<hr noshade>")
               (tex2page-string (get-token))
               (emit  "</td></tr></table></td><td>")))))))

(define do-frac
  (lambda ()
    ((if (eqv? *tex-format* ':latex)
         do-latex-frac
       do-tex-frac))))

(define do-eqno
  (lambda ()
    (unless *in-display-math-p*
      (terror 'do-eqno "You can't use \\eqno in math mode"))
    (emit "</td><td width=10% align=right>")))

(define do-eqalign
  (lambda (type)
    (ignorespaces)
    (let ((c (get-actual-char)))
      (when (eof-object? c) (terror 'do-eqalign "Missing {"))
      (unless (char=? c #\{) (terror 'do-eqalign "Missing {"))
      ;
      (bgroup)
      (set! *tabular-stack* (cons type *tabular-stack*))
      (add-postlude-to-top-frame
       (lambda ()
         (emit "</td></tr>") (emit-newline)
         (emit "</table>") (emit-newline)
         (when *in-display-math-p* (emit "</td><td>"))
         (pop-tabular-stack type)
         (set! *equation-position* 0)))
      (when *in-display-math-p* (emit "</td><td>"))
      (emit-newline)
      (emit "<table><tr><td>"))))

(define do-noalign
  (lambda ()
    (let* ((type (and (not (null? *tabular-stack*))
                      (car *tabular-stack*)))
           (split? (memv type '(:eqalignno :displaylines))))
      (when split? (egroup)
        (emit "</td></tr></table></div>") (emit-newline)
        ;(emit "<tr><td>")
        (do-para))
      (tex2page-string (get-group))
      (cond (split? (do-end-para)
                    ;(emit "</td></tr>")
                    (emit-newline) (emit "<div align=center><table><tr><td>")
                    (toss-back-char #\{) (do-eqalign type))
            (else (emit "</td></tr>") (emit-newline)
                  (emit "<tr><td>"))))))

(define do-pmatrix
  (lambda ()
    (ignorespaces)
    (let ((c (get-actual-char)))
      (when (eof-object? c) (terror 'do-pmatrix "Missing {"))
      (unless (char=? c #\{) (terror 'do-pmatrix "Missing {"))
      (bgroup)
      (set! *tabular-stack* (cons ':pmatrix *tabular-stack*))
      (add-postlude-to-top-frame
       (lambda ()
         (emit "</td></tr></table>")
         (when *in-display-math-p* (emit "</td><td>"))
         (emit-newline)
         (pop-tabular-stack ':pmatrix)))
      (when *in-display-math-p* (emit "</td><td>"))
      (emit "<table border=1><tr><td>")
      (emit-newline))))

(define do-over
  (lambda ()
    (emit
     (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
           (else (set! *math-height* (+ *math-height* 1))
                 "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>")))))

(define eat-till-eol
  (lambda ()
    ;move just past the next newline
    (let loop ()
      (let ((c (get-actual-char)))
        (unless (or (eof-object? c) (char=? c #\newline)) (loop))))))

(define eat-till-char
  (lambda (d)
    (let loop ()
      (let ((c (get-actual-char)))
        (unless (or (eof-object? c) (char=? c d)) (loop))))))

(define do-comment
  (lambda ()
    ;move past tex %comment
    (eat-till-eol)
    (when (munched-a-newline?)
      (toss-back-char #\newline)
      (toss-back-char #\newline))
    ;(emit-newline)
    ))

(define string=split
  (lambda (p sepc)
    ;convert an OS path into a Scheme list
    (if (not p) '()
        (let loop ((p p) (r '()))
          (let ((i (string-index p sepc)))
            (if i
                (loop (substring p (+ i 1) (string-length p))
                  (cons (substring p 0 i) r))
                (nreverse (cons p r))))))))

(define string=join
  (lambda (ss sepc)
    (let ((res ""))
      (let loop ((ss ss))
        (cond ((null? ss) res)
              (else (let ((s (car ss)))
                      (set! res
                        (cond ((string=? res "") s)
                              (else (string-append res (string sepc) s))))
                      (loop (cdr ss)))))))))

(define kpsewhich
  (lambda (f)
    (let ((tmpf (string-append *aux-dir/* *jobname* "-Z-Z.temp")))
      (ensure-file-deleted tmpf)
      (system (string-append "kpsewhich -- " f " > " tmpf))
      (let ((f (and (file-exists? tmpf)
                    (call-with-input-file tmpf
                      (lambda (i) (read-line i))))))
        (ensure-file-deleted tmpf)
        (if (or (not f) (eof-object? f)) #f
            (let ((f (string-trim-blanks f)))
              (when (eq? *operating-system* ':cygwin)
                (cond  ((eqv? (substring? "/cygdrive/" f) 0)
                        (set! f (substring f 11 (string-length f))))
                       ((eqv? (substring? "/usr/" f) 0)
                        (set! f (string-append "/cygwin" f)))))
              (cond ((= (string-length f) 0) #f)
                    ((eq? *operating-system* ':cygwin) f)
                    ((file-exists? f) f)
                    (else #f))))))))

(define find-tex-file
  (lambda (file)
    ;search for file.tex before file;
    ;search in current directory first;
    ;if TEX2PAGEINPUTS is set, search there,
    ;else use kpsewhich
    (let ((files (list (string-append file ".tex")
                       file)))
      (or (ormap (lambda (file)
                   (and (file-exists? file) file))
                 files)
          (if (not (null? *tex2page-inputs*))
              (ormap (lambda (dir)
                       (ormap (lambda (file)
                                (let ((qfile (string-append
                                              dir
                                              *directory-separator*
                                              file)))
                                  (and (file-exists? qfile)
                                       qfile)))
                              files))
                     *tex2page-inputs*)
              (kpsewhich file))))))

(define initialize-scm-words
  (lambda ()
    (set! *scm-keywords* (make-table 'equ string=?))
    (set! *scm-builtins* (make-table 'equ string=?))
    (set! *scm-special-symbols* (make-table 'equ string=?))
    (set! *scm-variables* (make-table 'equ string=?))
    (for-each (lambda (s) (table-put! *scm-keywords* s #t))
              '(
                ;#include scmkeywords.lisp
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
                ;#endinclude scmkeywords.lisp

                ))))

(define actual-tex-filename
  (lambda (f check-timestamp?)
    ;convert filename f to an "actual" filename, just
    ;like TeX does.  Ie, prefer the .tex extension, and
    ;search a pathlist.
    ;If doing main TeX file, do some setup
    (let ((doing-main-file? (not *main-tex-file*))
          (f2 (find-tex-file f)))
      (when doing-main-file?
        (when f2
          (set! *jobname* (file-stem-name f2))
          (make-target-dir)
          (let ((zeroth-html-page
                  (string-append *aux-dir/* *jobname* *output-extension*)))
            (when (string=? zeroth-html-page f2)
              (let ((f2-save (string-append f2 "_save")))
                (write-log ':separation-newline)
                (write-log "Copying weirdly named TeX source file ")
                (write-log f2)
                (write-log " to ")
                (write-log f2-save)
                (write-log ':separation-newline)
                (case *operating-system*
                  ((:cygwin :unix)
                   (system (string-append "cp -pf " f2 " " f2-save)))
                  ((:windows)
                   (system (string-append "copy/y " f2 " " f2-save))))
                (set! f2 f2-save))))
          )
        (load-aux-file)
        )
      (when (and f2 check-timestamp?
                 (ormap (lambda (vwf) (string=? f2 vwf))
                        *verb-written-files*))
        (set! check-timestamp? #f))
      (when (and f2 check-timestamp?)
        (update-last-modification-time f2))
      f2)))

(define add-dot-tex-if-no-extension-provided
  (lambda (f)
    (let ((e (file-extension f)))
      (if e f
          (string-append f ".tex")))))

(define ignore-tex-specific-text
  (lambda (env)
    (let ((endenv (string-append "\\end" env)))
      (let loop ()
        (let ((c (snoop-actual-char)))
          (cond ((eof-object? c)
                 (terror 'ignore-tex-specific-text "Missing \\end" env))
                ((esc-char-p c)
                 (let ((x (get-ctl-seq)))
                   (cond ((string=? x endenv) #t)
                         ((string=? x "\\end")
                          (let ((g (get-grouped-environment-name-if-any)))
                            (unless (and g (string=? g env))
                              (loop))))
                         (else (loop)))))
                (else (get-actual-char) (loop))))))))

(define do-rawhtml
  (lambda ()
    (let loop ()
      (let ((c (snoop-actual-char)))
        (cond ((eof-object? c)
               (terror 'do-rawhtml "Missing \\endrawhtml"))
              ((esc-char-p c)
               (let* ((x (get-ctl-seq))
                      (y (find-corresp-prim x)))
                 (cond ((string=? y "\\endrawhtml") 'done)
                       ((and (string=? x "\\end")
                             (get-grouped-environment-name-if-any))
                        => (lambda (g)
                             (let ((y (find-corresp-prim (string-append x g))))
                               (if (string=? y "\\endrawhtml") 'done
                                   (begin (emit "\\end{")
                                          (emit g) (emit "}")
                                          (loop))))))
                       ((string=? x "\\\\")
                        (emit c) (toss-back-char c) (loop))
                       (else (emit x) (loop)))))
              (else (get-actual-char)
                    (emit c) (loop)))))))

(define do-htmlheadonly
  (lambda ()
    (when (null? *html-head*) (flag-missing-piece ':html-head))
    (let loop ((s '()))
      (let ((c (snoop-actual-char)))
        (cond ((eof-object? c)
               (write-aux `(!html-head ,(list->string (nreverse s)))))
              ((esc-char-p c)
               (write-aux `(!html-head ,(list->string (nreverse s))))
               (let ((x (get-ctl-seq)))
                 (cond ((string=? x "\\endhtmlheadonly") 'done)
                       ((string=? x "\\input")
                        (let ((f (get-filename-possibly-braced)))
                          (call-with-input-file/buffered
                           f do-htmlheadonly)
                          (loop '())))
                       (else (write-aux `(!html-head ,x))
                             (loop '())))))
              ((char=? c #\newline)
               ;kludge: (most?) Schemes print newline within string as \n, which
               ;CL reads as n
               (get-actual-char)
               (loop (cons #\space s)))
              (else (get-actual-char)
                    (loop (cons c s))))))))

;\let, \def, \imgdef

(define resolve-chardefs
  (lambda (c)
    (cond ((find-chardef c)
           => (lambda (y)
                (get-actual-char)
                (expand-tex-macro (cdef.optarg y)
                                  (cdef.argpat y)
                                  (cdef.expansion y))))
          (else #f))))

(define resolve-defs
  (lambda (x)
    (cond ((find-def x)
           => (lambda (y)
                (cond ((tdef.defer y) => (lambda (z) z))
                      ((tdef.thunk y) #f)
                      (else
                        (cond ((and (inside-false-world?)
                                    (not (if-aware-ctl-seq? x))
                                    ;(> (length (tdef.argpat y)) 0)
                                    ) #f)
                              (else
                                (when *outer-p*
                                  (set! *outer-p* #f)
                                  (toss-back-char *outer-invisible-space*))
                                (expand-tex-macro
                                  (tdef.optarg y)
                                  (tdef.argpat y)
                                  (tdef.expansion y))))))))
          (else #f))))

(define do-expandafter
  (lambda ()
    (let* ((first (get-raw-token/is))
           (second (get-raw-token/is)))
      (toss-back-char *invisible-space*)
      (cond ((ctl-seq? second)
             (toss-back-string (expand-ctl-seq-into-string second)))
            (else (toss-back-string second)))
      (toss-back-char *invisible-space*)
      (toss-back-string first))))

(define resolve-expandafters
  (lambda ()
    (let ((c (snoop-actual-char)))
      (when (esc-char-p c)
        (let ((x (get-ctl-seq)))
          (if (string=? x "\\expandafter")
              (do-expandafter)
            (begin (toss-back-char *invisible-space*)
                   (toss-back-string x))))))))

(define do-futurelet
  (lambda ()
    (let* ((first (get-raw-token/is))
           (second (get-raw-token/is))
           (third (get-raw-token)))
      (do-futurelet-aux first second third))))

(define do-futurenonspacelet
  (lambda ()
    (let* ((first (get-raw-token/is))
           (second (get-raw-token/is))
           (third (get-raw-token/is)))
      (do-futurelet-aux first second third))))

(define do-futurelet-aux
  (lambda (first second third)
    (tex-let-general first third #f)
    (toss-back-char *invisible-space*)
    (toss-back-string third)
    (toss-back-char *invisible-space*)
    (toss-back-string second)))

(define set-start-time
  (lambda ()
    (let* ((secs (current-seconds))
           (ht (and secs (seconds->date secs))))
      (when ht
        (tex-def-count "\\time"
                       (+ (* 60 (date-hour ht)) (date-minute ht))
                       #t)
        (tex-def-count "\\day" (date-day ht) #t)
        (tex-def-count "\\month"
                       (+ (date-month ht) (- *january-number*) 1)
                       #t)
        (tex-def-count "\\year"
                       (+ *anno-domini-at-0* (date-year ht))
                       #t)))))

(define initialize-globals
  (lambda ()
    (set! *global-texframe* (make-texframe))
    (set! *section-counter-dependencies* (make-table))
    (set! *dotted-counters* (make-table 'equ string=?))

    ;#include globdefs.scm
  ;
  ;for TeX, 0 <= \language <= 255; for TeX2page, let's make \language =
  ;256
  (tex-def-count "\\language" 256 #t)
  ;
  ;the deepest possible section, the part, has depth -1; so -2 is the
  ;closest-to-0 number that is a meaningless depth
  (tex-def-count "\\secnumdepth" -2 #t)
  (tex-def-count "\\tocdepth" -2 #t)
  ;
  (tex-def-count "\\footnotenumber" 0 #t)
  (tex-def-count "\\TIIPtabularborder" 1 #t)
  (tex-def-count "\\TIIPnestedtabularborder" 0 #t)
  (tex-def-count "\\TIIPobeyspacestrictly" 0 #t)
  (tex-def-count "\\TIIPobeylinestrictly" 0 #t)
  (tex-def-count "\\errorcontextlines" 5 #t)
  (tex-def-count "\\doublehyphendemerits" 10000 #t)
  (tex-def-count "\\finalhyphendemerits" 5000 #t)
  (tex-def-count "\\hyphenpenalty" 50 #t)
  (tex-def-count "\\exhyphenpenalty" 50 #t)
  (tex-def-count "\\pretolerance" 100 #t)
  (tex-def-count "\\tolerance" 200 #t)
  (tex-def-count "\\hbadness" 1000 #t)
  (tex-def-count "\\widowpenalty" 150 #t)
  (tex-def-count "\\showboxdepth" 3 #t)
  (tex-def-count "\\outputpenalty" 0 #t)
  (tex-def-count "\\globaldefs" 0 #t)
  (tex-def-count "\\mag" 1000 #t)
  (tex-def-count "\\tracingcommands" 0 #t)
  (tex-def-count "\\tracingmacros" 0 #t)
  (tex-def-count "\\tracingonline" 0 #t)
  (tex-def-count "\\time" 0 #t)
  (tex-def-count "\\day" 0 #t)
  (tex-def-count "\\month" 0 #t)
  (tex-def-count "\\year" 0 #t)
  (tex-def-count "\\shellescape" 1 #t)
  (tex-def-count "\\suppressfontnotfounderror" 1 #t)
  ;
  (tex-def-dimen "\\TIIPhsize" 0 #t)
  (tex-def-dimen "\\hsize" (tex-length 6.5 ':in) #t)
  (tex-def-dimen "\\vsize" (tex-length 8.9 ':in) #t)
  (tex-def-dimen "\\maxdepth" (tex-length 4 ':pt) #t)
  (tex-def-dimen "\\delimitershortfall" (tex-length 5 ':pt) #t)
  (tex-def-dimen "\\nulldelimiterspace" (tex-length 1.2 ':pt) #t)
  (tex-def-dimen "\\scriptspace" (tex-length 0.5 ':pt) #t)
  (tex-def-dimen "\\hoffset" 0 #t)
  (tex-def-dimen "\\voffset" 0 #t)
  (tex-def-dimen "\\epsfxsize" 0 #t)
  (tex-def-dimen "\\epsfysize" 0 #t)
  (tex-def-dimen "\\emergencystretch" 0 #t)
  (tex-def-dimen "\\hfuzz" (tex-length 0.1 ':pt) #t)
  (tex-def-dimen "\\vfuzz" (tex-length 0.1 ':pt) #t)
  (tex-def-dimen "\\textwidth" (tex-length 6.5 ':in) #t)
  (tex-def-dimen "\\smallskipamount" (tex-length 3 ':pt) #t)
  (tex-def-dimen "\\medskipamount" (tex-length 6 ':pt) #t)
  (tex-def-dimen "\\bigskipamount" (tex-length 12 ':pt) #t)
  (tex-def-dimen "\\lastskip" 0 #t)
  (tex-def-dimen "\\baselineskip" (tex-length 12 ':pt) #t)
  (tex-def-dimen "\\overfullrule" (tex-length 5 ':pt) #t)
  (tex-def-dimen "\\parindent" (tex-length 20 ':pt) #t)
  (tex-def-dimen "\\leftskip" 0 #t)
  (tex-def-dimen "\\parfillskip" 0 #t)
  (tex-def-dimen "\\parskip" 0 #t)
  (tex-def-dimen "\\abovedisplayskip" (tex-length 12 ':pt) #t)
  (tex-def-dimen "\\belowdisplayskip" (tex-length 12 ':pt) #t)
  (tex-def-toks "\\everypar" "" #t)
  (tex-def-toks "\\headline" "" #t)
  (tex-def-toks "\\footline" "\\folio" #t)
  (tex-def-dotted-count "figure" #f)
  (tex-def-dotted-count "table" #f)
  (tex-def-dotted-count "equation" #f)
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
  (tex-gdef-0arg "\\TZPcommonlisp" (if 'nil "0" "1"))
    ;#endinclude globdefs.scm

    (initialize-scm-words)
    ))

;

(define find-def
  (lambda (ctlseq)
    (or (ormap
          (lambda (fr)
            (table-get (texframe.definitions fr) ctlseq))
          *tex-env*)
        (and *global-texframe*
             (table-get (texframe.definitions *global-texframe*) ctlseq))
        (table-get (texframe.definitions *primitive-texframe*) ctlseq))))

;(trace find-def)

(define find-math-def
  (lambda (ctlseq)
    (table-get (texframe.definitions *math-primitive-texframe*)
               ctlseq)))

(define find-count
  (lambda (ctlseq)
    (or (ormap (lambda (fr)
                 (table-get (texframe.counts fr) ctlseq))
                 *tex-env*)
        (table-get (texframe.counts *global-texframe*) ctlseq)
        (table-get (texframe.counts *primitive-texframe*) ctlseq))))

(define find-toks
  (lambda (ctlseq)
    (or (ormap (lambda (fr)
                 (table-get (texframe.toks fr) ctlseq))
               *tex-env*)
        (table-get  (texframe.toks *global-texframe*) ctlseq)
        (table-get (texframe.toks *primitive-texframe*) ctlseq))))

(define find-dimen
  (lambda (ctlseq)
    (or (ormap (lambda (fr)
                 (table-get (texframe.dimens fr) ctlseq))
          *tex-env*)
        (table-get (texframe.dimens *global-texframe*) ctlseq)
        (table-get (texframe.dimens *primitive-texframe*) ctlseq))))

(define get-toks
  (lambda (ctlseq)
    (or (find-toks ctlseq)
        (terror 'get-toks))))

(define get-dimen
  (lambda (ctlseq)
    (cond ((find-dimen ctlseq))
          (else ;let's just assume 6.5in (the default \hsize)
          (tex-length 6.5 ':in)))))

(define the-count
  (lambda (ctlseq)
    (or (find-count ctlseq) (terror 'the-count))))

(define do-count=
  (lambda (z g?)
    (get-equal-sign)
    (tex-def-count z
                   (get-number)
                   g?)))

(define do-toks=
  (lambda (z g?)
    (get-equal-sign)
    (tex-def-toks z
                  (get-group)
                  g?)))

(define do-dimen=
  (lambda (z g?)
    (get-equal-sign)
    (tex-def-dimen z (get-scaled-points) g?)
    (ignorespaces)))

(define (get-gcount ctlseq)
  (or (table-get (texframe.counts *global-texframe*) ctlseq) 0))

(define set-gcount!
  (lambda (ctlseq v)
    (tex-def-count ctlseq v #t)))

(define do-number
  (lambda ()
    (emit (get-number))))

(define do-magnification
  (lambda ()
    (tex-def-count "\\mag" (get-number) #f)))

(define do-magstep
  (lambda ()
    (case (string->number (get-token-or-peeled-group))
      ((1) "1000")
      ((2) "1200")
      ((3) "1440")
      ((4) "1728")
      ((5) "2074")
      ((6) "2488")
      (else ""))))

(define scaled-point-to-tex-point
  (lambda (sp)
    (string-append
      (number->string
        (/ sp 65536.0)) "pt")))

(define expand-the
  (lambda ()
    (let ((ctlseq (get-ctl-seq)))
      (cond ((find-dimen ctlseq)
             => (lambda (x) (scaled-point-to-tex-point x)))
            ((get-number-corresp-to-ctl-seq ctlseq) => (lambda (x) x))
            ((find-toks ctlseq))
            (else (trace-if #f "expand-the failed"))))))

(define do-the
  (lambda ()
    ;almost like do-number
    (let ((ctlseq (get-ctl-seq)))
      (cond ((find-dimen ctlseq)
             => (lambda (x)
                  (emit (scaled-point-to-tex-point  x))))
            ((get-number-corresp-to-ctl-seq ctlseq) => emit)
            ;       ((find-count ctlseq) =>
            ;             (lambda (x) (emit (cadr x))))
            ((find-toks ctlseq)
             => (lambda (x)
                  (tex2page-string x)))
            (else (trace-if #f "do-the failed"))))))

(define do-arabic
  (lambda ()
    (let* ((counter-name (ungroup (get-group)))
           (counter (table-get *dotted-counters* counter-name)))
      (cond ((counter.value counter) => emit)
            (else (trace-if #f "do-arabic failed"))))))

(define find-corresp-prim
  ;is this really necessary? -- why not make resolve-defs
  ;take over this too?
  (lambda (ctlseq)
    (let ((y (find-def ctlseq)))
      (or (and y (tdef.defer y))
          ctlseq))))

(define find-corresp-prim-thunk
  (lambda (ctlseq)
    (let ((y (find-def ctlseq)))
      (if (and y (tdef.thunk y))
          (tdef.prim y)
          ctlseq))))

(define globally-p
  (lambda ()
    (> (get-gcount "\\globaldefs") 0)))

(define do-let
  (lambda (g?)
    (unless (inside-false-world?)
      (ignorespaces)
      (let* ((lhs (get-ctl-seq))
             (rhs (begin (get-equal-sign)
                         (get-raw-token/is)))
             (frame (and g? *global-texframe*)))
        (tex-let-general lhs rhs frame)))))

(define do-def
  (lambda (g? e?)
    (unless (inside-false-world?)
      (let ((lhs (get-raw-token/is)))
        (when (and (ctl-seq? lhs) (string=? lhs "\\TIIPcsname"))
          (set! lhs (get-peeled-group)))
        (let* ((argpat (get-def-arguments lhs))
               (rhs (ungroup (get-group)))
               (frame (and g? *global-texframe*)))
          (when e?
            (set! rhs (expand-edef-macro rhs)))
          (cond ((ctl-seq? lhs)
                 (tex-def lhs argpat rhs #f #f #f #f frame))
                (else
                  (tex-def-char (string-ref lhs 0) argpat rhs frame))))))))

(define do-newcount
  (lambda (g?)
    (tex-def-count (get-ctl-seq) 0
                   g?)))

(define do-newtoks
  (lambda (g?)
    (tex-def-toks (get-ctl-seq) ""
                  g?)))

(define do-newdimen
  (lambda (g?)
    (tex-def-dimen (get-ctl-seq) 0
                   g?)))

(define do-advance
  (lambda (g?)
    (let* ((ctlseq (get-ctl-seq))
           (count (find-count ctlseq)))
      (get-by)
      (if count
          (tex-def-count ctlseq
                         (+ count (get-number))
                         g?)
        (eat-dimen)))))

(define do-multiply
  (lambda (g?)
    (let* ((ctlseq (get-ctl-seq))
           (curr-val (find-count ctlseq)))
      (get-by)
      (tex-def-count ctlseq
                     (* curr-val (get-number))
                     g?))))

(define do-divide
  (lambda (g?)
    (let* ((ctlseq (get-ctl-seq))
           (curr-val (find-count ctlseq)))
      (get-by)
      (tex-def-count ctlseq
        (quotient curr-val (get-number))
                     g?))))

(define do-newcommand
  (lambda (renew?)
    (ignorespaces)
    (let* ((lhs (string-trim-blanks (ungroup (get-token))))
           (optarg #f)
           (argc
             (cond ((get-bracketed-text-if-any)
                    => (lambda (s)
                         (cond ((get-bracketed-text-if-any)
                                => (lambda (s)
                                     (set! optarg s))))
                         (string->number (string-trim-blanks s))))
                   (else 0)))
           (rhs (ungroup (get-token)))
           (ok-to-def? (or renew? (not (find-def lhs)))))
      (tex-def lhs (latex-argnum-to-plain-argpat argc)
               rhs optarg #f #f #f #f)
      (unless ok-to-def?
        (trace-if (> (find-count "\\tracingcommands") 0)
                  lhs " already defined")))))

(define do-newcounter
  (lambda ()
    (let* ((counter-name (ungroup (get-group)))
           (within (get-bracketed-text-if-any)))
      (tex-def-dotted-count counter-name within))))

(define do-newenvironment
  (lambda (renew?)
    (ignorespaces)
    (let* ((envname (string-trim-blanks (ungroup (get-token))))
           (bs-envname (string-append "\\" envname))
           (optarg #f)
           (argc
             (cond ((get-bracketed-text-if-any)
                    => (lambda (s)
                         (cond ((get-bracketed-text-if-any)
                                => (lambda (s)
                                     (set! optarg s))))
                         (string->number (string-trim-blanks s))))
                   (else 0)))
           (beginning (string-append "\\begingroup " (ungroup (get-token))))
           (ending (string-append (ungroup (get-token)) "\\endgroup"))
           (ok-to-def? (or renew? (not (find-def bs-envname)))))
      (tex-def bs-envname
               (latex-argnum-to-plain-argpat argc) beginning optarg
               #f #f #f #f)
      (tex-def (string-append "\\end" envname) '() ending #f
               #f #f #f #f)
      (unless ok-to-def?
        (trace-if #t "{" envname "} already defined")))))

(define tex-def-dotted-count
  (lambda (counter-name sec-num)
    (when sec-num
      (table-put! *section-counter-dependencies* sec-num
        (cons counter-name
          (table-get *section-counter-dependencies* sec-num '()))))
    (table-put! *dotted-counters* counter-name
      (make-counter 'within sec-num))))

(define do-newtheorem
  (lambda ()
    (let* ((env (ungroup (get-group)))
           (numbered-like (get-bracketed-text-if-any))
           (counter-name (or numbered-like env))
           (caption (ungroup (get-group)))
           (within (if numbered-like #f (get-bracketed-text-if-any)))
           (sec-num (and within (section-ctl-seq?
                                 (string-append "\\" within)))))
      (unless numbered-like
        (tex-def-dotted-count counter-name sec-num))
      (tex-def (string-append "\\" env) '()
               (string-append "\\par\\begingroup\\TIIPtheorem{"
                              counter-name "}{" caption "}")
               #f #f #f #f *global-texframe*)
      (tex-def (string-append "\\end" env) '()
               "\\endgroup\\par" #f #f #f #f *global-texframe*))))

(define do-theorem
  (lambda ()
    (let* ((counter-name (ungroup (get-group)))
           (counter (table-get *dotted-counters* counter-name))
           (caption (ungroup (get-group))))
      (unless counter (terror 'do-theorem))
      (let ((new-counter-value (+ 1 (counter.value counter))))
        (set!counter.value counter new-counter-value)
        (let* ((thm-num (let ((sec-num (counter.within counter)))
                          (if sec-num
                              (string-append (section-counter-value sec-num) "."
                                (number->string new-counter-value))
                              (number->string new-counter-value))))
               (lbl (string-append *html-node-prefix* "thm_" thm-num)))
          (tex-def-0arg "\\TIIPcurrentnodename" lbl)
          (tex-def-0arg  "\\@currentlabel" thm-num)
          (emit-anchor lbl)
          (emit-newline)
          (emit "<b>")
          (emit caption)
          (emit " ")
          (emit thm-num)
          (emit ".</b>")
          (emit-nbsp 2))))))

(define do-begin
  (lambda ()
    (cond ((get-grouped-environment-name-if-any)
           => (lambda (env)
                (toss-back-char *invisible-space*)
                (toss-back-string (string-append "\\" env))
                (unless (ormap (lambda (y) (string=? env y))
                               '("htmlonly" "cssblock" "document" "latexonly"
                                 "rawhtml" "texonly" "verbatim" "verbatim*"))
                  (toss-back-string "\\begingroup")
                  (do-end-para))))
                (else
                  (terror 'do-begin "\\begin not followed by environment name")))))

(define do-end
  (lambda ()
    (cond ((get-grouped-environment-name-if-any)
           => (lambda (env)
                (toss-back-char *invisible-space*)
                (unless (ormap (lambda (y) (string=? env y))
                               '("htmlonly" "document"))
                  (do-end-para)
                  (toss-back-string "\\endgroup"))
                (toss-back-string (string-append "\\end" env))))
          (else
            (toss-back-char *invisible-space*)
            (toss-back-string "\\TIIPbye")))))

(define latex-argnum-to-plain-argpat
  (lambda (n)
    (let loop ((n n) (s '()))
      (if (<= n 0) s
          (loop (- n 1)
            (cons #\# (cons (integer->char (+ *int-corresp-to-0* n)) s)))))))

(define make-reusable-img
  (lambda (g?)
    (set! *imgdef-file-count* (+ *imgdef-file-count* 1))
    (ignorespaces)
    (let ((lhs (get-ctl-seq))
          (imgdef-file-stem (string-append
                             *subjobname*
                             *img-file-suffix* *imgdef-file-suffix*
                             (number->string *imgdef-file-count*))))
      (dump-imgdef imgdef-file-stem)
      (tex-to-img imgdef-file-stem)
      (tex-def
       lhs '() (string-append "\\TIIPreuseimage{" imgdef-file-stem "}")
       #f #f #f #f (and g? *global-texframe*)))))

(define valid-img-file?
  (lambda (f)
    (and (file-exists? f)
         (or
          (call-with-input-file f
            (lambda (i)
              (not (eof-object? (read-char i)))))
          (begin (delete-file f)
                 #f)))))

(define source-img-file
  (lambda (img-file-stem . alt)
    (let* ((alt (if (null? alt) #f (car alt)))
           (img-file (string-append img-file-stem (find-img-file-extn)))
           (f (string-append *aux-dir/* img-file)))
      (write-log #\()
      (write-log f)
      (write-log ':separation-space)
      (valid-img-file? f)
      (emit "<img src=\"")
      (emit img-file)
      (emit "\" border=\"0\" alt=\"")
      (cond (alt (emit alt))
            (else
              (emit "[")
              (emit img-file)
              (emit "]")))
      (emit "\">")
      (write-log #\))
      (write-log ':separation-space)
      #t ;?
      )))

(define reuse-img
  (lambda ()
    (source-img-file (ungroup (get-group)))))

(define get-def-arguments
  (lambda (lhs)
    (let aux ()
      (let ((c (snoop-actual-char)))
        (cond ((eof-object? c)
               (terror 'get-def-arguments
                       "EOF found while scanning definition of " lhs))
              ((esc-char-p c)
               (let ((x (get-ctl-seq)))
                 (if (string=? x "\\par")
                     (cons #\newline
                           (cons #\newline (aux)))
                     (append (string->list x) (aux)))))
              ((char=? c #\{) '())
              (else
               (cond ((char=? c #\newline)
                      ;kludge for writing texi-type macros.  Should
                      ;really be equivalent to any whitespace
                      (get-actual-char)
                      (ignorespaces))
                     ((char-whitespace? c) (ignorespaces)
                      (set! c #\space))
                     (else (get-actual-char)))
               (cons c (aux))))))))

(define get-till-char
  (lambda (c0)
    ;(ignorespaces)
    (list->string
      (nreverse
        (let loop ((s '()) (nesting 0) (escape? #f))
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'get-till-char "File ended too soon"))
                  (escape?
                    (loop (cons (get-actual-char) s) nesting #f))
                  ((char=? c c0) s)
                  ((esc-char-p c)
                   (loop (cons (get-actual-char) s) nesting #t))
                  ((char=? c #\{)
                   (loop (cons (get-actual-char) s) (+ nesting 1) #f))
                  ((char=? c #\})
                   (loop (cons (get-actual-char) s) (- nesting 1) #f))
                  ((> nesting 0)
                   (loop (cons (get-actual-char) s) nesting #f))
                  ((and (char-whitespace? c) (not (char=? c0 #\newline))
                        (char-whitespace? c0))
                   s)
                  (else (loop (cons (get-actual-char) s) nesting #f)))))))))

(define digit-to-int
  (lambda (d)
    (- (char->integer d) *int-corresp-to-0*)))

(define do-halign
  (lambda ()
    (do-end-para)
    (ignorespaces)
    (let ((c (get-actual-char)))
      (when (eof-object? c) (terror 'do-halign "Missing {"))
      (unless (char=? c #\{) (terror 'do-halign "Missing {")))
    (fluid-let ((*tabular-stack* (cons 'halign *tabular-stack*)))
      (bgroup)
      (emit "<table>")
      (let ((tmplt (get-halign-template)))
        (let loop ()
          (ignorespaces)
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'do-halign "Eof inside \\halign"))
                  ((char=? c #\}) (get-actual-char)
                   (emit "</table>")
                   (egroup)
                   (do-para))
                  (else
                    (expand-halign-line tmplt)
                    (loop)))))))))

(define get-halign-template
  (lambda ()
    (let loop ((s '()))
      (let ((x (get-raw-token)))
        (cond ((eof-object? x)
               (terror 'get-halign-template "Eof in \\halign"))
              ((string=? x "\\cr") (nreverse (cons #f s)))
              ((string=? x "#") (loop (cons #t s)))
              ((string=? x "&") (loop (cons #f s)))
              (else (loop (cons x s))))))))

(define expand-halign-line
  (lambda (tmplt)
    (emit "<tr>")
    (let loop ((tmplt tmplt) (ins " "))
      (let ((x (get-raw-token)))
        (cond ((eof-object? x)
               (terror 'expand-halign-line "Eof in \\halign"))
              ((or (string=? x "&") (string=? x "\\cr"))
               (let loop2 ((tmplt tmplt) (r "{"))
                 (if (null? tmplt)
                     (terror 'expand-halign-line "Eof in \\halign")
                     (let ((y (car tmplt)))
                       (case y
                         ((#f) (emit "<td>")
                          (tex2page-string (string-append r "}"))
                          (when (and (string=? x "\\cr")
                                     (string=? ins " "))
                            (emit-nbsp 1))
                          (emit "</td>")
                          (if (string=? x "\\cr")
                              (begin (emit "</tr>") (emit-newline))
                              (loop (cdr tmplt) " ")))
                         ((#t) (loop2 (cdr tmplt)
                                 (string-append r ins)))
                         (else (loop2 (cdr tmplt)
                                 (string-append r y))))))))
              (else (loop tmplt (string-append ins x))))))))

(define read-till-next-sharp
  (lambda (k argpat)
    (let ((n (length argpat)))
      (let loop ((ss '()))
        (let loop2 ((i k) (s '()))
          (let ((c (if (< i n) (list-ref argpat i) #\#)))
            (if (char=? c #\#)
                (cons i (list->string (nreverse ss)))
                (let ((d (snoop-actual-char)))
                  (cond ((and (char=? c #\space) (char-whitespace? d))
                         (ignorespaces)
                         '(if (char=? d #\newline) (get-actual-char)
                             (ignorespaces))
                         (loop2 (+ i 1) (cons c s)))
                        ((and *comment-char* (char=? d *comment-char*))
                         (do-comment)
                         (loop2 i s))
                        ((and (char=? c #\newline)
                              (char-whitespace? d)
                              (or (munched-a-newline?)
                                  (begin (toss-back-char d) #f)))
                         (loop2 (+ i 1) (cons c s)))
                        ((char=? c d) (get-actual-char)
                         (loop2 (+ i 1) (cons c s)))
                        ((= i k)
                         (loop
                          (if (and (char=? d #\{)
                                   (or (null? ss)
                                       (not (esc-char-p (car ss)))))
                              (append (get-group-as-reversed-chars) ss)
                              (begin
                               (if (and (char-whitespace? d)
                                        (not (char=? d #\newline)))
                                   (ignorespaces)
                                   (get-actual-char))
                               (cons d ss)))))
                        (else (loop (append s ss))))))))))))

;(trace read-till-next-sharp)

(define read-macro-args
  (lambda (argpat k r)
    (let ((n (length argpat)))
      (nreverse
       (let loop ((k k) (r r))
         (if (>= k n) r
             (let ((c (list-ref argpat k)))
               ;(resolve-expandafters)
               (cond ((eqv? c #\#)
                      (cond ((= k (- n 1))
                             (ignorespaces)
                             (cons (get-till-char #\{) r))
                            ((= k (- n 2))
                             (cons (ungroup (get-token)) r))
                            (else
                             (let ((c2 (list-ref argpat (+ k 2))))
                               (if (eqv? c2 #\#)
                                   (loop (+ k 2)
                                         (cons (ungroup (get-token)) r))
                                   (let ((x (read-till-next-sharp
                                             (+ k 2) argpat)))
                                     (loop (car x) (cons (cdr x) r))))))))
                     (else
                      (let ((d (get-actual-char)))
                        (cond ((eof-object? d)
                               (terror 'read-macro-args
                                       "Eof before macro got enough args"))
                              ((eqv? c #\space)
                               (unless (char-whitespace? d)
                                 (terror 'read-macro-args "Misformed macro call")))
                              ((char=? c d)
                               (loop (+ k 1) r))
                              (else
                               (terror 'read-macro-args
                                       "Misformed macro call")))))))))))))

;(trace read-macro-args)

(define expand-edef-macro
  (lambda (rhs)
    (fluid-let ((*not-processing-p* #t))
      (let ((tmp-port (open-output-string)))
        (call-with-input-string/buffered
          rhs
          (lambda ()
            (let loop ()
              (let ((c (snoop-actual-char)))
                (unless (eof-object? c)
                  (display
                    (cond ((esc-char-p c)
                           (let ((x (get-ctl-seq)))
                             (toss-back-char *invisible-space*)
                             (cond ((or (string=? x "\\the")
                                        (string=? x "\\number"))
                                    (let ((x2 (get-raw-token/is)))
                                      (toss-back-char *invisible-space*)
                                      (toss-back-string x2)
                                      (cond ((ctl-seq? x2)
                                             (cond ((string=? x "\\the")
                                                    (expand-the))
                                                   ((string=? x "\\number")
                                                    (get-number))
                                                   (else "deadcode")))
                                            (else x))))
                                   ((string=? x "\\noexpand")
                                    (let ((x2 (get-raw-token/is)))
                                      (toss-back-char *invisible-space*)
                                      x2))
                                   ((find-def x)
                                    => (lambda (y)
                                         (cond ((and (null? (tdef.argpat y))
                                                     (not (tdef.optarg y))
                                                     (not (tdef.thunk y))
                                                     (not (tdef.prim y))
                                                     (not (tdef.defer y)))
                                                (toss-back-char *invisible-space*)
                                                (toss-back-string
                                                  (tdef.expansion y))
                                                "")
                                               (else x))))
                                   (else x))))
                          (else (get-actual-char) c))
                    tmp-port)
                  (loop))))))
        (get-output-string tmp-port)))))

(define expand-tex-macro
  (lambda (optarg argpat rhs)
    ;(unless (or *in-verb-p* *math-mode-p*)
    ;     (ignorespaces))
    (let* (;(arg-pat-n (length argpat))
           (k 0)
           (r (if (not optarg) '()
                  (begin (set! k 2)
                    (list (cond ((get-bracketed-text-if-any)
                                 => (lambda (s) s))
                                (else optarg))))))
           (args (read-macro-args argpat k r))
           (rhs-n (string-length rhs)))
      (list->string
        (let aux ((k 0))
          (if (>= k rhs-n) '()
              (let ((c (string-ref rhs k)))
                (cond ((char=? c #\\ )
                       ;shall we assume esc char inside def bodies is always \ ?
                       ;when we expand it  \ becomes <curr esc char>
                       (let loop ((j (+ k 1)) (s (list #\\ )))
                         (if (>= j rhs-n)
                             (nreverse s)
                             (let ((c (string-ref rhs j)))
                               (cond ((char-alphabetic? c)
                                      (loop (+ j 1) (cons c s)))
                                     ((and (char=? c #\#)
                                           (> (length s) 1))
                                      ;# preceded by ctlseq -- insert space
                                      (append (nreverse s)
                                        ;(string->list "\\TIIPnull1")
                                        (cons #\space (aux j))))
                                     ((= (length s) 1)
                                      (append (nreverse (cons c s))
                                        (aux (+ j 1))))
                                     (else
                                       (append (nreverse s) (aux j))))))))
                      ((char=? c #\#)
                       (if (= k (- rhs-n 1))
                           (list #\#)
                           (let ((n (string-ref rhs (+ k 1))))
                             (cond ((char=? n #\#)
                                    (cons #\# (aux (+ k 2)))) ;really?
                                   ((and (char-numeric? n)
                                         (<= (digit-to-int n) (length args)))
                                    ;not really such a good test :-<
                                    (append (string->list
                                              (list-ref args
                                                (- (digit-to-int n) 1)))
                                      (aux (+ k 2))))
                                   (else (cons #\# (aux (+ k 1))))))))
                      (else (cons c (aux (+ k 1))))))))))))

;(trace expand-tex-macro)

;verbatim

(define do-verbatimescapechar
  (lambda ()
    (ignorespaces)
    (let* ((c1 (get-actual-char))
           (c2 (get-actual-char)))
      (unless (esc-char-p c1)
        (terror 'do-verbatimescapechar "Arg must be \\<char>"))
      (set! *esc-char-verb* c2))))

(define do-verb-braced
  (lambda (ignore)
    (fluid-let ((*esc-chars* (list *esc-char-verb*))
                (*tex-extra-letters* '()))
      (let loop ((nesting 0))
        (let ((c (get-actual-char)))
          (cond ((eof-object? c)
                 (terror 'do-verb-braced "Eof inside verbatim"))
                ((esc-char-p c)
                 (toss-back-char c)
                 (let ((x (fluid-let ((*not-processing-p* #t))
                            (get-ctl-seq))))
                   (cond ((ormap (lambda (z) (string=? x z))
                                 '("\\ " "\\{" "\\}"))
                          (emit (string-ref x 1)))
                         (else
                           (fluid-let ((*esc-chars* (list *esc-char-std*)))
                             (do-tex-ctl-seq-completely x)))))
                 (loop nesting))
                ((char=? c #\{)
                 (emit #\{)
                 (loop (+ nesting 1)))
                ((char=? c #\})
                 (unless (= nesting 0)
                   (emit #\})
                   (loop (- nesting 1))))
                ((char=? c #\space)
                 (emit (if *verb-visible-space-p* *verbatim-visible-space* #\space))
                 (loop nesting))
                ((char=? c #\newline)
                 (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                       (*verb-visible-space-p* (emit *verbatim-visible-space*))
                       (else (emit-newline)))
                 (loop nesting))
                ((and (char=? c #\-) (not *verb-display-p*))
                 (emit "&#x2011;") (loop nesting))
                (else (emit-html-char c)
                      (loop nesting))))))))

(define do-verb-delimed
  (lambda (d)
    (let loop ()
      (let ((c (get-actual-char)))
        (cond ((eof-object? c)
               (terror 'do-verb-delimed "Eof inside verbatim"))
              ((char=? c d) 'done)
              ((char=? c #\space)
               (emit
               (if *verb-visible-space-p*
                   *verbatim-visible-space*
 #\space))
               (loop))
              ((char=? c #\newline)
               (cond (*verb-display-p* (emit "&#xa0;")
                                      (emit-newline))
                     (*verb-visible-space-p*
                      (emit *verbatim-visible-space*))
                     (else (emit-newline)))
               (loop))
              ((and (char=? c #\-) (not *verb-display-p*))
               (emit "&#x2011;") (loop))
              (else (emit-html-char c)
                    (loop)))))))

(define do-verb
  (lambda ()
    (ignorespaces)
    (bgroup)
    (fluid-let ((*verb-visible-space-p* (eat-star))
                (*ligatures-p* #f))
      (let ((d (get-actual-char)))
        (fluid-let ((*verb-display-p* (munched-a-newline?)))
          (cond (*outputting-external-title-p* #f)
                (*verb-display-p* (do-end-para) (emit "<pre class=verbatim>"))
                (else (emit "<code class=verbatim>")))
          ((if (char=? d #\{)
               do-verb-braced
               do-verb-delimed) d)
          (cond (*outputting-external-title-p* #f)
                (*verb-display-p* (emit "</pre>") ;(do-para)
                                 (do-noindent))
                (else (emit "</code>"))))))
    (egroup)))

(define do-verbc
  (lambda ()
    (ignorespaces)
    (bgroup)
    (fluid-let ((*ligatures-p* #f))
      (emit "<code class=verbatim>")
      (emit-html-char (get-actual-char))
      (emit "</code>"))
    (egroup)))

(define do-verbatiminput
  (lambda ()
    (ignorespaces)
    (let* ((f0 (get-filename-possibly-braced))
           (f (find-tex-file f0)))
      (cond ((and f (file-exists? f))
             (do-end-para)
             (bgroup)
             (emit "<pre class=verbatim>")
             (call-with-input-file f
               (lambda (p)
                 (let loop ()
                   (let ((c (read-char p)))
                     (unless (eof-object? c)
                       (emit-html-char c)
                       (loop))))))
             (emit "</pre>")
             (egroup)
             (do-para))
            (else
              (non-fatal-error
                "File " f0 " not found"))))))

(define get-char-definitely
  (lambda (c0)
    (ignorespaces)
    (let ((c (get-actual-char)))
      (when (eof-object? c) (terror 'get-char-definitely "Runaway argument"))
      (unless (char=? c c0) (terror 'get-char-definitely "Missing" c0)))))

(define get-char-optionally
  (lambda (cc)
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) #f)
            ((memv c cc) (get-actual-char) c)
            (else #f)))))

(define get-unsigned-number-optionally
  (lambda ()
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) #f)
            ((char-numeric? c) (get-integer 10))
            (else #f)))))

(define opmac-verbinput-skip-lines
  (lambda (i n)
    (let loop ((k 0))
      (unless (>= k n)
        (let ((x (read-line i)))
          (when (eof-object? x) (terror 'do-opmac-verbinput "\\verbinput file ended too soon"))
          (loop (+ k 1)))))))

(define opmac-verbinput-print-lines
  (lambda (i n)
    (if (eq? n #t)
        (let loop ()
          (let ((x (read-line i)))
            (unless (eof-object? x)
              (emit-html-string x)
              (emit-newline)
              (loop))))
        (let loop ((k 0))
          (unless (>= k n)
            (let ((x (read-line i)))
              (when (eof-object? x) (terror 'do-opmac-verbinput "\\verbinput file ended too soon"))
              (emit-html-string x)
              (emit-newline)
              (loop (+ k 1))))))))

(define do-opmac-verbinput
  (lambda ()
    (let* ((s1 (begin (get-char-definitely #\() (get-char-optionally '(#\+ #\-))))
           (n1 (get-unsigned-number-optionally))
           (s2 (get-char-optionally '(#\+ #\-)))
           (n2 (get-unsigned-number-optionally))
           (f0 (begin (get-char-definitely #\)) (get-filename)))
           (f (find-tex-file f0))
           (n (and f (let ((n (table-get *opmac-verbinput-table* f)))
                       (or n (begin
                               (table-put! *opmac-verbinput-table* f 0)
                               0))))))
      (cond ((and f (file-exists? f))
             (do-end-para)
             (bgroup)
             (emit "<pre class=verbatim>")
             (call-with-input-file f
               (lambda (i)
                 (cond ((and s1 n1 s2 n2 (char=? s1 #\-) (char=? s2 #\+))
                        ;skip n1 after current point, print n2
                        (opmac-verbinput-skip-lines i (+ n n1))
                        (opmac-verbinput-print-lines i n2)
                        (set! n (+ n n1 n2)))
                       ;
                       ((and (not s1) n1 s2 n2 (char=? s2 #\-))
                        ;print lines n1 thru n2
                        (opmac-verbinput-skip-lines i (- n1 1))
                        (opmac-verbinput-print-lines i (+ (- n2 n1) 1))
                        (set! n n2))
                       ;
                       ((and (not s1) n1 s2 n2 (char=? s2 #\+))
                        ;print n2 lines starting at line n1
                        (opmac-verbinput-skip-lines i (- n1 1))
                        (opmac-verbinput-print-lines i n2)
                        (set! n (+ (- n1 1) n2)))
                       ;
                       ((and s1 n1 (not s2) (not n2) (char=? s1 #\-))
                        ;print lines 1 thru n1
                        (opmac-verbinput-print-lines i n1)
                        (set! n n1))
                       ;
                       ((and s1 n1 (not s2) (not n2) (char=? s1 #\+))
                        ;print n1 lines following current point
                        (opmac-verbinput-skip-lines i n)
                        (opmac-verbinput-print-lines i n1)
                        (set! n (+ n n1)))
                       ;
                       ((and (not s1) n1 s2 (not n2) (char=? s2 #\-))
                        ;print from line n1 to eof
                        (opmac-verbinput-skip-lines i (- n1 1))
                        (opmac-verbinput-print-lines i #t)
                        (set! n 0))
                       ;
                       ((and s1 (not n1) (not s2) (not n2) (char=? s1 #\+))
                        ;print from current point to eof
                        (opmac-verbinput-skip-lines i n)
                        (opmac-verbinput-print-lines i #t)
                        (set! n 0))
                       ;
                       ((and s1 (not n1) (not s2) (not n2) (char=? s1 #\-))
                        ;print entire file
                        (opmac-verbinput-print-lines i #t)
                        (set! n 0))
                       ;
                       (else (terror 'do-opmac-verbinput "Malformed \\verbinput" s1 n1 s2 n2 f)))))
             (table-put! *opmac-verbinput-table* f n)
             (emit "</pre>")
             (egroup)
             (do-para))
            (else (non-fatal-error "File " f0 " not found"))))))

(define do-verbwritefile
  (lambda ()
    (let* ((f (get-filename-possibly-braced))
           (e (file-extension f)))
      (unless e
        (set! e ".tex")
        (set! f (string-append f e)))
      (when *verb-port* (close-output-port *verb-port*))
      (ensure-file-deleted f)
      (set! *verb-written-files* (cons f *verb-written-files*))
      (when (string-ci=? e ".mp")
        (set! *mp-files* (cons f *mp-files*)))
      (set! *verb-port* (open-output-file f)))))

(define verb-ensure-output-port
  (lambda ()
    (unless *verb-port*
      (let ((output-file (string-append *jobname* ".txt")))
        (ensure-file-deleted output-file)
        (set! *verb-port*
          (open-output-file output-file))))))

(define dump-groupoid
  (lambda (p)
    (ignorespaces)
    (let ((write-char write-char)
          (d (get-actual-char)))
      (unless p
        (set! write-char (lambda (x y) #f)))
      (case d
        ((#\{) (let loop ((nesting 0))
                 (let ((c (get-actual-char)))
                   (cond ((eof-object? c)
                          (terror 'dump-groupoid
                                  "Eof inside verbatim"))
                         ((char=? c *esc-char-verb*)
                          (write-char c p)
                          (write-char (get-actual-char) p)
                          (loop nesting))
                         ((char=? c #\{)
                          (write-char c p)
                          (loop (+ nesting 1)))
                         ((char=? c #\})
                          (unless (= nesting 0)
                            (write-char c p)
                            (loop (- nesting 1))))
                         (else (write-char c p)
                               (loop nesting))))))
        (else (let loop ()
                (let ((c (get-actual-char)))
                  (cond ((eof-object? c)
                         (terror 'dump-groupoid
                                 "Eof inside verbatim"))
                        ((char=? c d) 'done)
                        (else (write-char c p)
                              (loop))))))))))

(define do-makehtmlimage
  (lambda ()
    (ignorespaces)
    (unless (char=? (snoop-actual-char) #\{)
      (terror 'do-makehtmlimage "\\makehtmlimage's argument must be a group"))
     (call-with-html-image-port dump-groupoid)))

(define do-verbwrite
  (lambda ()
    (verb-ensure-output-port)
    (dump-groupoid *verb-port*)))

(define do-string
  (lambda ()
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c) #f)
            ((esc-char-p c)
             (get-actual-char) (toss-back-char *invisible-space*)
             (toss-back-string "\\TIIPbackslash"))
            ((char=? c *comment-char*) (eat-till-eol) (do-string))
            (else (toss-back-char (get-actual-char)))))))

;LaTeX and eplain verbatim

(define do-verbatim
  (lambda ()
    (if (eqv? *tex-format* ':latex)
      (do-verbatim-latex "verbatim")
      (do-verbatim-eplain))))

(define do-verbatim-latex
  (lambda (env)
    (do-end-para)
    (bgroup)
    (fluid-let ((*verb-visible-space-p* (eat-star)))
               (when *verb-visible-space-p*
                 (set! env (string-append env "*")))
      (emit "<pre class=verbatim>")
      (munched-a-newline?)
      (fluid-let ((*ligatures-p* #f))
        (let loop ()
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'do-verbatim-latex
                           "Eof inside verbatim"))
                  ((char=? c #\\ )
                   (let ((end? (get-ctl-seq)))
                     (if (string=? end? "\\end")
                         (cond ((get-grouped-environment-name-if-any)
                                => (lambda (e)
                                  (unless (string=? e env)
                                    (emit-html-string end?)
                                    (emit-html-char #\{)
                                    (emit-html-string e)
                                    (emit-html-char #\})
                                    (loop))))
                               (else (emit-html-string end?)
                                     (loop)))
                         (begin
                          (emit-html-string end?)
                          (loop)))))
                  ((char=? c #\space)
                   (get-actual-char)
                   (emit
                   (if *verb-visible-space-p*
                       *verbatim-visible-space*
                       #\space))
                   (loop))
                  (else (emit-html-char (get-actual-char))
                        (loop))))))
      (emit "</pre>"))
    (egroup)
    (do-para)))

(define do-verbatim-eplain
  (lambda ()
    (fluid-let ((*inside-eplain-verbatim-p* #t)
                (*esc-chars* (list *esc-char-verb*)))
      (let loop ()
        (when *inside-eplain-verbatim-p*
          (let ((c (get-actual-char)))
            (cond ((eof-object? c) (terror 'do-verbatim-eplain "Eof inside verbatim"))
                  ((esc-char-p c)
                   (toss-back-char c)
                   (let ((x (fluid-let ((*not-processing-p* #t))
                              (get-ctl-seq))))
                     (cond ((string=? x "\\ ") (emit " "))
                           (else (do-tex-ctl-seq-completely x))))
                   (loop))
                  ((char=? c #\space) (emit "&#xa0;") (loop))
                  ((char=? c #\newline) (emit "<br>") (emit-newline)
                                        (loop))
                  (else (emit-html-char c)
                        (loop)))))))))

(define do-endverbatim-eplain
  (lambda ()
    (set! *inside-eplain-verbatim-p* #f)))

(define do-begintt
  (lambda ()
    (do-end-para)
    (let ((*esc-chars* *esc-chars*))
      (bgroup)
      (do-tex-ctl-seq-completely "\\tthook")
      (set! *esc-chars* (ldelete *esc-char-std* *esc-chars* char=?))
      (emit "<pre class=verbatim>")
      (munched-a-newline?)
      (fluid-let ((*ligatures-p* #f))
        (let loop ()
          (let ((c (snoop-actual-char)))
            (when (eof-object? c)
              (terror 'do-begintt "Eof inside \\begintt"))
            (cond ((char=? c #\\)
                   (fluid-let ((*esc-chars* (list *esc-char-std*)))
                     (let ((cs (fluid-let ((*not-processing-p* #t))
                                 (get-ctl-seq))))
                       (unless (string=? cs "\\endtt")
                         (emit-html-string cs)
                         (loop)))))
                  ((esc-char-p c) (let ((cs (fluid-let ((*not-processing-p* #t))
                                              (get-ctl-seq))))
                                    (fluid-let ((*esc-chars* (list *esc-char-std*)))
                                      (do-tex-ctl-seq-completely cs)))
                                  (loop))
                  (else (emit-html-char (get-actual-char))
                        (loop))))))
      (emit "</pre>")
      (egroup))
    (do-noindent)))

;

(define do-alltt
  (lambda ()
    (do-end-para)
    (bgroup)
    (emit "<pre class=verbatim>")
    (munched-a-newline?)
    (fluid-let ((*in-alltt-p* #t))
      (let loop ()
        (let ((c (snoop-actual-char)))
          (if (eof-object? c)
              (terror 'do-alltt "Eof inside alltt")
              (begin
               (case c
                 ((#\\ ) (do-tex-ctl-seq (get-ctl-seq)))
                 ((#\{) (get-actual-char) (bgroup))
                 ((#\}) (get-actual-char) (egroup))
                 (else (emit-html-char (get-actual-char))))
               (when *in-alltt-p* (loop)))))))))

(define do-end-alltt
  (lambda ()
    (emit "</pre>")
    (egroup)
    (do-para)
    (set! *in-alltt-p* #f)))

;Scheme tokens

(define do-scm-set-specialsymbol
  (lambda ()
    (let* ((sym (get-peeled-group))
           (xln (get-group)))
      (table-put! *scm-special-symbols* sym xln))))

(define do-scm-unset-specialsymbol
  (lambda ()
    (call-with-input-string/buffered
      (ungroup (get-group))
      (lambda ()
        (let loop ()
          (ignore-all-whitespace)
          (unless (eof-object? (snoop-actual-char))
            (table-put! *scm-special-symbols*
                        (scm-get-token) #f)
            (loop)))))))

(define do-scm-set-builtins
  (lambda ()
    (call-with-input-string/buffered
     (ungroup (get-group))
     (lambda ()
       (let loop ()
         (ignore-all-whitespace)
         (let ((c (snoop-actual-char)))
           (unless (eof-object? c)
             (let ((s (scm-get-token)))
               (table-put! *scm-keywords* s #f)
               (table-put! *scm-variables* s #f)
               (table-put! *scm-builtins* s #t))
             (loop))))))))

(define do-scm-set-keywords
  (lambda ()
    (call-with-input-string/buffered
     (ungroup (get-group))
     (lambda ()
       (let loop ()
         (ignore-all-whitespace)
         (let ((c (snoop-actual-char)))
           (unless (eof-object? c)
             (let ((s (scm-get-token)))
               (table-put! *scm-builtins* s #f)
               (table-put! *scm-variables* s #f)
               (table-put! *scm-keywords* s #t))
             (loop))))))))

(define do-scm-set-variables
  (lambda ()
    (call-with-input-string/buffered
     (ungroup (get-group))
     (lambda ()
       (let loop ()
         (ignore-all-whitespace)
         (let ((c (snoop-actual-char)))
           (unless (eof-object? c)
             (let ((s (scm-get-token)))
               (table-put! *scm-builtins* s #f)
               (table-put! *scm-keywords* s #f)
               (table-put! *scm-variables* s #t))
             (loop))))))))

;displaying Scheme programs

(define scm-emit-html-char
  (lambda (c)
    (unless (eof-object? c)
      (when *scm-dribbling-p* (write-char c *verb-port*))
      (if (and (char=? c #\-) (not *verb-display-p*))
          (emit "&#x2011;")
        (emit-html-char c)))))

(define scm-output-next-chunk
  (lambda ()
    (let ((c (snoop-actual-char)))
      (cond ((and *slatex-math-escape*
                  (char=? c *slatex-math-escape*))
             (scm-escape-into-math))
            ((char=? c #\;) (scm-output-comment) (do-end-para))
            ((char=? c #\") (scm-output-string))
            ((char=? c #\#) (scm-output-hash))
            ((char=? c #\,) (get-actual-char)
             (emit "<span class=keyword>")
             (scm-emit-html-char c)
             (let ((c (snoop-actual-char)))
               (when (char=? c #\@)
                 (get-actual-char)
                 (scm-emit-html-char c)))
             (emit "</span>"))
            ((or (char=? c #\') (char=? c #\`))
             (get-actual-char)
             (emit "<span class=keyword>")
             (scm-emit-html-char c)
             (emit "</span>"))
            ((or (char-whitespace? c) (memv c *scm-token-delims*))
             (get-actual-char)
             (scm-emit-html-char c))
            (else (scm-output-token (scm-get-token)))))))

(define scm-set-mathescape
  (lambda (yes?)
    (let ((c (fluid-let ((*esc-chars* '()))
               (string-ref (ungroup (get-group)) 0))))
      (cond (yes?
              (set! *slatex-math-escape* c)
              (set! *scm-token-delims*
                (cons *slatex-math-escape* *scm-token-delims*)))
            (else
              (set! *slatex-math-escape* #f)
              (set! *scm-token-delims*
                (ldelete c *scm-token-delims* char=?)))))))

(define scm-escape-into-math
  (lambda ()
    (get-actual-char)
    (let ((math-text (get-till-char *slatex-math-escape*)))
      (get-actual-char)
      (unless (string=? math-text "")
        (emit "<span class=variable>")
        (fluid-let ((*esc-chars* (list *esc-char-std*)))
          (tex2page-string (string-append "$" math-text "$")))
        (emit "</span>")))))

(define scm-output-slatex-comment
  (lambda ()
    (let ((s (get-line)))
      (emit "<span class=comment>")
      (when *scm-dribbling-p*
        (display s *verb-port*)
        (newline *verb-port*))
      (fluid-let ((*esc-chars* (list *esc-char-std*)))
        (tex2page-string s))
      (do-end-para)
      (emit "</span>")
      (toss-back-char #\newline)
      ;(emit-newline)
      )))

(define scm-output-verbatim-comment
  (lambda ()
    ;(get-actual-char)
    (emit "<span class=comment>")
    ;(scm-emit-html-char #\;)
    (let loop ()
      (let ((c (get-actual-char)))
        (cond ((or (eof-object? c) (char=? c #\newline))
               (emit "</span>")
               (scm-emit-html-char c))
              ((and (char-whitespace? c)
                    (let ((c2 (snoop-actual-char)))
                      (or (eof-object? c2) (char=? c2 #\newline))))
               (emit "</span>")
               (scm-emit-html-char (get-actual-char)))
              (else (scm-emit-html-char c)
                (loop)))))))

(define scm-output-comment
  (lambda ()
    ((if (tex2page-flag-boolean "\\TZPslatexcomments")
         scm-output-slatex-comment
         scm-output-verbatim-comment))))

(define scm-output-extended-comment
  (lambda ()
    (get-actual-char)              ;read the `|'
    (emit "<span class=comment>")
    (scm-emit-html-char #\#)
    (scm-emit-html-char #\|)
    (let loop ()
      (let ((c (get-actual-char)))
        (cond ((eof-object? c) #t)
              ((char=? c #\|)
               (let ((c2 (snoop-actual-char)))
                 (cond ((eof-object? c2) (scm-emit-html-char c))
                       ((char=? c2 #\#) (get-actual-char))
                       (else (scm-emit-html-char c)
                             (loop)))))
              (else (scm-emit-html-char c)
                    (loop)))))
    (scm-emit-html-char #\|)
    (scm-emit-html-char #\#)
    (emit "</span>")))

(define scm-output-string
  (lambda ()
    (get-actual-char)
    (emit "<span class=selfeval>")
    (scm-emit-html-char #\")
    (let loop ((esc? #f))
      (let ((c (get-actual-char)))
        (case c
          ((#\") (when esc?
                   (scm-emit-html-char c) (loop #f)))
          ((#\\ ) (scm-emit-html-char c) (loop (not esc?)))
          (else (scm-emit-html-char c) (loop #f)))))
    (scm-emit-html-char #\")
    (emit "</span>")))

(define scm-output-hash
  (lambda ()
    (get-actual-char) ;read the #\#
    (let ((c (snoop-actual-char)))
      (cond ((eof-object? c)
             (emit "<span class=selfeval>")
             (scm-emit-html-char #\#)
             (emit "</span>"))
            ((char=? c #\|) (scm-output-extended-comment))
            (else (toss-back-char #\#)
                  (scm-output-token (scm-get-token)))))))

(define scm-output-token
  (lambda (s)
    (case (scm-get-type s)
      ((special-symbol)
       (fluid-let ((*esc-chars* (list *esc-char-std*)))
         (tex2page-string
           (table-get *scm-special-symbols* s))))
      ((keyword)
       (emit "<span class=keyword>")
       (scm-display-token s)
       (emit "</span>"))
      ((global)
       (emit "<span class=global>")
       (scm-display-token s)
       (emit "</span>"))
      ((selfeval)
       (emit "<span class=selfeval>")
       (scm-display-token s)
       (emit "</span>"))
      ((builtin)
       (emit "<span class=builtin>")
       (scm-display-token s)
       (emit "</span>"))
      ((background)
       (scm-display-token s))
      (else
        (emit "<span class=variable>")
        (scm-display-token s)
        (emit "</span>")))))

(define scm-display-token
  (lambda (s)
    (let ((n (string-length s)))
      (let loop ((k 0))
        (when (< k n)
          (scm-emit-html-char (string-ref s k))
          (loop (+ k 1)))))))

(define do-scm-braced
  (lambda (result?)
    (get-actual-char)
    (let ((display? (munched-a-newline?)))
      (cond ((not display?)
             (emit "<code class=scheme")
             (when result? (emit "response"))
             (emit ">"))
            (else (do-end-para) (emit "<pre class=scheme>")))
      (bgroup)
      (fluid-let ((*esc-chars* (list *esc-char-verb*))
                  (*verb-display-p* display?))
        (let loop ((nesting 0))
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'do-scm-braced "Eof inside verbatim"))
                  ((esc-char-p c)
                   (let ((x (fluid-let ((*not-processing-p* #t))
                              (get-ctl-seq))))
                     (cond ((ormap (lambda (z) (string=? x z))
                                   '("\\ " "\\{" "\\}"))
                            (scm-emit-html-char (string-ref x 1)))
                           (else
                            (fluid-let ((*esc-chars* (list *esc-char-std*)))
                              (do-tex-ctl-seq-completely x)))))
                   (loop nesting))
                  ((char=? c #\{)
                   (get-actual-char)
                   (scm-emit-html-char c)
                   (loop (+ nesting 1)))
                  ((char=? c #\})
                   (get-actual-char)
                   (unless (= nesting 0)
                     (scm-emit-html-char c)
                     (loop (- nesting 1))))
                  (else (scm-output-next-chunk)
                        (loop nesting))))))
      (egroup)
      (if (not display?) (emit "</code>")
          (begin (emit "</pre>")
                 ;(do-para)
                 (do-noindent)
                 )))))

(define do-scm-delimed
  (lambda (result?)
    (let ((d (get-actual-char)))
      (let ((display? (munched-a-newline?)))
        (cond  ((not display?) (emit "<code class=scheme")
                         (when result? (emit "response"))
                         (emit ">"))
               (else (do-end-para) (emit "<pre class=scheme>")))
        (fluid-let ((*verb-display-p* display?)
                    (*scm-token-delims* (cons d *scm-token-delims*)))
          (let loop ()
            (let ((c (snoop-actual-char)))
              (cond ((eof-object? c)
                     (terror 'do-scm-delimed "Eof inside verbatim"))
                    ((char=? c d) (get-actual-char))
                    (else (scm-output-next-chunk)
                      (loop))))))
        (if (not display?) (emit "</code>")
            (begin (emit "</pre>") (do-para)))))))

(define do-scm
  (lambda (result?)
    (cond (*outputting-external-title-p* (do-verb))
          (else
            (ignorespaces)
            (bgroup)
            (fluid-let ((*ligatures-p* #f))
              ((if (char=? (snoop-actual-char) #\{)
                   do-scm-braced
                   do-scm-delimed) result?))
            (egroup)))))

(define do-scminput
  (lambda ()
    (ignorespaces)
    (do-end-para)
    (bgroup)
    (emit "<pre class=scheme>")
    (let ((f (add-dot-tex-if-no-extension-provided
               (get-filename-possibly-braced))))
      (call-with-input-file/buffered f
        (lambda ()
          (let loop ()
            (let ((c (snoop-actual-char)))
              (unless (eof-object? c)
                (scm-output-next-chunk)
                (loop)))))))
    (emit "</pre>")
    (egroup)
    ;(do-para)
    (do-noindent)))

;Literate programming

(define do-scmdribble
  (lambda ()
    (verb-ensure-output-port)
    (fluid-let ((*scm-dribbling-p* #t))
      (do-scm #f))
    (newline *verb-port*)))

;SLaTeX {schemedisplay}, &c

(define do-scm-slatex-lines
  (lambda (env display? result?)
    (fluid-let ((*esc-chars* *esc-chars*))
    (let ((endenv (string-append "\\end" env))
          (in-table? (and (not (null? *tabular-stack*))
                          (memv (car *tabular-stack*) '(:block :figure :table)))))
      (cond (display? (do-end-para))
            (in-table? (emit "</td><td>")))
      (munched-a-newline?)
      (bgroup)
      (when (string=? env "tt")
        (do-tex-ctl-seq-completely "\\tthook")
        (set! *esc-chars* (ldelete *esc-char-std* *esc-chars* char=?)))
      (emit "<div align=left><pre class=scheme")
      (when result? (emit "response"))
      (emit ">")
      (fluid-let ((*ligatures-p* #f)
                  (*verb-display-p* #t)
                  (*not-processing-p* #t))
        (let loop ()
          (let ((c (snoop-actual-char)))
            (cond ((eof-object? c)
                   (terror 'do-scm-slatex-lines
                           "Eof inside " env))
                  ((char=? c #\newline) (get-actual-char)
                   (scm-emit-html-char c)
                   (cond ((not (tex2page-flag-boolean "\\TZPslatexcomments")) #f)
                         ((char=? (snoop-actual-char) #\;)
                          (get-actual-char)
                          (if (char=? (snoop-actual-char) #\;)
                              (toss-back-char #\;)
                              (scm-output-slatex-comment))))
                   (loop))
                  ((char=? c #\\)
                   (let ((x (fluid-let ((*esc-chars* (list *esc-char-std*))
                                        (*not-processing-p* #f))
                              (get-ctl-seq))))
                     (cond ((string=? x endenv) #t)
                           ((string=? x "\\end")
                            (let ((g (get-grouped-environment-name-if-any)))
                              (if (and g (string=? g env))
                                  (egroup)
                                  (begin
                                   (scm-output-token x)
                                   (when g
                                     (scm-output-token "{")
                                     (scm-output-token g)
                                     (scm-output-token "}"))
                                   (loop)))))
                           (else (scm-output-token x) (loop)))))
                  ((esc-char-p c) (let ((cs (fluid-let ((*not-processing-p* #t))
                                              (get-ctl-seq))))
                                    (fluid-let ((*esc-chars* (list *esc-char-std*)))
                                      (do-tex-ctl-seq-completely cs)))
                                  (loop))
                  (else (scm-output-next-chunk)
                        (loop))))))
      (emit "</pre></div>")
      (egroup)
      (cond (display? (do-noindent))
            (in-table? (emit "</td><td>")))))))

(define string-is-all-dots?
  (lambda (s)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond ((>= i n) #t)
              ((char=? (string-ref s i) #\.) (loop (+ i 1)))
              (else #f))))))

(define string-is-flanked-by-stars?
  (lambda (s)
    (let ((n (string-length s)))
      (and (>= n 3)
           (char=? (string-ref s 0) #\*)
           (char=? (string-ref s (- n 1)) #\*)))))

(define string-starts-with-hash?
  (lambda (s)
    (char=? (string-ref s 0) #\#)))

(define scm-get-type
  (lambda (s)
    (cond ((table-get *scm-special-symbols* s) 'special-symbol)
          ((table-get *scm-keywords* s) 'keyword)
          ((table-get *scm-builtins* s) 'builtin)
          ((table-get *scm-variables* s) 'variable)
          ((string-is-flanked-by-stars? s) 'global)
          (else
           (let ((colon (string-index s #\:)))
             ;should use string->number only after ascertaining
             ;that s has no colon, is not all dots,
             ;and doesn't start with #.  Otherwise
             ;the cl version of this code could choke
             (cond (colon (if (= colon 0) 'selfeval 'variable))
                   ((string-is-all-dots? s) 'background)
                   ((string-starts-with-hash? s) 'selfeval)
                   ((string->number s) 'selfeval)
                   (else 'variable)))))))

(define eat-star
  (lambda ()
    (let ((c (snoop-actual-char)))
      (if (and (not (eof-object? c))
               (char=? c #\*))
          (get-actual-char)
          #f))))

(define do-cr
  (lambda (z)
    (ignorespaces)
    (let ((top-tabular (if (not (null? *tabular-stack*))
                           (car *tabular-stack*) 'nothing)))
      (case top-tabular
        ((:tabular)
         (get-bracketed-text-if-any)
         (egroup)
         (emit "</td></tr>")
         (emit-newline)
         (emit "<tr><td valign=top ")
         (do-tabular-multicolumn)
         )
        ((:eqnarray*)
         (emit "</td></tr>") (emit-newline)
         (set! *equation-position* 0)
         (emit "<tr><td align=right>"))
        ((:eqnarray)
         (emit "</td>")
         (cond (*equation-numbered-p*
                (emit "<td>(")
                (emit *equation-number*)
                (bump-dotted-counter "equation")
                (emit ")</td>"))
               (else (set! *equation-numbered-p* #t)))
         (emit "</tr>") (emit-newline)
         (set! *equation-position* 0)
         (emit "<tr><td align=right>"))
        ((:ruled-table) (emit "</td></tr>")
         (emit-newline)
         (emit "<tr><td>"))
        ((:minipage :tabbing)
         (get-bracketed-text-if-any)
         (emit "<br>") (emit-newline))
        ((:eqalign :eqalignno :displaylines :pmatrix :mathbox)
         (unless (char=? (snoop-actual-char) #\})
           (set! *math-height* (+ *math-height* 1))
           (emit "</td></tr>") (emit-newline)
           (emit "<tr><td align=center>")
           (set! *equation-position* 0)
           (emit-newline)))
        ((:header) (emit #\space))
        (else
          (when (and (eqv? *tex-format* ':latex)
                     (string=? z "\\\\"))
            (get-bracketed-text-if-any)
            (let ((c (snoop-actual-char)))
              (when (and (not (eof-object? c)) (char=? c #\*))
                (get-actual-char)))
            (emit "<br>")
            (emit-newline))
          )))))

(define do-ruledtable
  (lambda ()
    (set! *tabular-stack* (cons ':ruled-table *tabular-stack*))
    (emit "<table border=2><tr><td>") (emit-newline)))

(define do-endruledtable
  (lambda ()
    (emit-newline)
    (emit "</td></tr></table>") (emit-newline)
    (pop-tabular-stack ':ruled-table)))

(define do-tabular
  (lambda (math?)
    (do-end-para)
    (get-bracketed-text-if-any)
    (bgroup)
    (unless math?
      (add-postlude-to-top-frame
        (let ((old-math-mode-p *math-mode-p*)
              (old-in-display-math-p *in-display-math-p*))
          (set! *math-mode-p* #f)
          (set! *in-display-math-p* #f)
          (lambda ()
            (set! *math-mode-p* old-math-mode-p)
            (set! *in-display-math-p* old-in-display-math-p)))))
    (let ((border-width
            (if (string-index (get-group) #\|) 1 0)))
      (set! *tabular-stack* (cons ':tabular *tabular-stack*))
      (emit "<table border=")
      (emit border-width)
      (emit "><tr><td valign=top ")
      (do-tabular-multicolumn))))

(define do-end-tabular
  (lambda ()
    (egroup)
    (do-end-para)
    (emit "</td></tr></table>")
    (pop-tabular-stack ':tabular)
    (egroup)))

(define do-tabular-colsep
  (lambda ()
    (egroup)
    (emit "</td><td valign=top ")
    (do-tabular-multicolumn)))

(define do-tabular-multicolumn
  (lambda ()
    (let loop ()
      (ignorespaces)
      (let ((c (snoop-actual-char)))
        (when (and (char? c) (char=? c #\\ ))
          (let ((x (get-ctl-seq)))
            (cond ((string=? x "\\hline") (loop))
                  ((string=? x "\\multicolumn")
                   (let ((n (ungroup (get-token))))
                     (get-token)
                     (emit " colspan=")
                     (emit n)))
                  (else (toss-back-char *invisible-space*)
                        (toss-back-string x)))))))
    (emit ">")
    (bgroup)))

(define do-ruledtable-colsep
  (lambda ()
    (emit-newline)
    (emit "</td><td")
    (ignorespaces)
    (let ((c (snoop-actual-char)))
      (when (char=? c #\\ )
          (let ((x (get-ctl-seq)))
            (if (string=? x "\\multispan")
                (let ((n (ungroup (get-token))))
                  (emit " colspan=")
                  (emit n))
                (toss-back-string x)))))
    (emit ">") (emit-newline)))

(define do-romannumeral
  (lambda (upcase?)
    (cond ((get-number-or-false)
           => (lambda (n)
                (emit
                  (number-to-roman n upcase?)))))))

(define do-tex-case-code
  (lambda (kase)
    (unless (inside-false-world?)
      (let* ((c1 (get-tex-char-spec))
             (c2 (begin (get-equal-sign) (get-tex-char-spec)))
             (fr (top-texframe)))
        (table-put!
          ((case kase
             ((:lccode) texframe.uccodes)
             ((:uccode) texframe.lccodes)) fr)
          c1 c2)))))

(define tex-char-downcase
  (lambda (c)
    (or (ormap (lambda (f)
                 (table-get (texframe.lccodes f) c)) *tex-env*)
        (char-downcase c))))

(define tex-char-upcase
  (lambda (c)
    (or (ormap (lambda (f)
                 (table-get (texframe.uccodes f) c)) *tex-env*)
        (char-upcase c))))

(define do-flipcase
  (lambda (kase)
    (emit
      (list->string
        (map (case kase
               ((:uccode) tex-char-upcase)
               ((:lccode) tex-char-downcase))
             (string->list (get-token)))))))

(define do-addtocounter
  (lambda ()
    (let* ((counter-name (get-peeled-group))
           (new-value (string->number (get-token-or-peeled-group))))
      (set-latex-counter-aux counter-name #t new-value))))

(define do-setcounter
  (lambda ()
    (let* ((counter-name (get-peeled-group))
           (new-value (string->number (get-token-or-peeled-group))))
      (set-latex-counter-aux counter-name #f new-value))))

(define do-stepcounter
  (lambda ()
    (let ((counter-name (get-peeled-group)))
      (set-latex-counter-aux counter-name #t 1))))

#|
(define set-latex-counter
  (lambda (add?)
    (let* ((counter-name (get-peeled-group))
           (new-value (string->number (get-token-or-peeled-group))))
      (set-latex-counter-aux add? new-value))))
|#

(define set-latex-counter-aux
  (lambda (counter-name add? new-value)
    (cond ((table-get *dotted-counters* counter-name)
           => (lambda (counter)
                (set!counter.value counter
                                   (if add?
                                       (+ new-value (counter.value counter))
                                       new-value))))
          (else
            (let ((count-seq (string-append "\\" counter-name)))
              (cond
                ((section-ctl-seq? count-seq)
                 => (lambda (n)
                      (table-put! *section-counters* n
                                  (if add?
                                      (+ new-value
                                         (table-get *section-counters* n 0))
                                      new-value))))
                ((find-count count-seq)
                 ;typically \secnumdepth, \tocdepth
                 (set-gcount! count-seq
                              (if add?
                                  (+ new-value (get-gcount count-seq))
                                  new-value)))
                (else ;(terror 'set-latex-counter-aux)
                  #f)))))))

(define do-tex-prim
  (lambda (z)
    (cond ((find-def z)
           => (lambda (y)
             (cond ((tdef.defer y) => toss-back-string)
                   ((tdef.thunk y) => (lambda (th) (th)))
                   (else (expand-tex-macro
                          (tdef.optarg y)
                          (tdef.argpat y)
                          (tdef.expansion y))))))
          (*math-mode-p* (do-math-ctl-seq z))
          (else
            (trace-if (> (find-count "\\tracingcommands") 0)
                      "Ignoring " z)))))

(define do-char
  (lambda ()
    (let ((n (get-number-or-false)))
      (unless n (terror 'do-char "not a char"))
      (cond ((< n 128) (emit-html-char (integer->char n)))
            (else (emit "&#x")
                  (emit (number->string n 16))
                  (emit ";"))))))

(define do-chardef
  (lambda ()
    (let ((lhs (get-raw-token/is)))
      (get-equal-sign)
      (let ((n (get-number-or-false)))
        (if n
            (tex-let lhs (string (integer->char n)) #f)
            (terror 'do-chardef "non-number found while scanning definition of " lhs))))))

(define tex-math-bb
  (lambda (c)
    (case c
      ((#\C) "&#x2102;")
      ((#\H) "&#x210d;")
      ((#\N) "&#x2115;")
      ((#\P) "&#x2119;")
      ((#\Q) "&#x211a;")
      ((#\R) "&#x211d;")
      ((#\Z) "&#x2124;")
      (else (string-append "&#x"
                           (number->string
                             (+ #x1d538 (- (char->integer c) (char->integer #\A))) 16)
                           ";")))))

(define tex-math-cal
  (lambda (c)
    (string-append "&#x"
                   (number->string
                     (+ #x1d4d0 (- (char->integer c) (char->integer #\A))) 16)
                   ";")))

(define tex-math-frak
  (lambda (c)
    (string-append "&#x"
                   (number->string
                     (if (char-upper-case? c)
                       (+ #x1d56c (- (char->integer c) (char->integer #\A)))
                       (+ #x1d586 (- (char->integer c) (char->integer #\a))))
                     16)
                   ";")))

(define emit-math-alpha-char
  (lambda (c)
    (case *math-font*
      ((:rm) (emit c))
      ((:bf) (emit "<b>") (emit c) (emit "</b>"))
      ((:bb) (emit (if (char-upper-case? c) (tex-math-bb c) c)))
      ((:cal) (emit (if (char-upper-case? c) (tex-math-cal c) c)))
      ((:frak) (emit (if (char-alphabetic? c) (tex-math-frak c) c)))
      (else (emit "<em>") (emit c) (emit "</em>")))))

(define do-tex-char
  (lambda (c)
    (cond ((and *comment-char* (char=? c *comment-char*))
           (do-comment))
          ((inside-false-world?) #t)
          ((char=? c #\{) (bgroup))
          ((char=? c #\}) (egroup))
          ((char=? c #\$) (do-math))
          ((char=? c #\-) (do-hyphen))
          ((char=? c #\`) (do-lsquo))
          ((char=? c #\') (do-rsquo))
          ((char=? c #\~) (emit-nbsp 1))
          ((char=? c #\!) (do-excl))
          ((char=? c #\?) (do-quest))
          ((or (char=? c #\<)
               (char=? c #\>)
               (char=? c #\")) (emit-html-char c))
          ((char=? c #\&)
           (cond ((not (null? *tabular-stack*))
                  (do-end-para)
                  (case (car *tabular-stack*)
                    ((:pmatrix :eqalign :displaylines :mathbox)
                     (emit "&#xa0;</td><td align=center>&#xa0;"))
                    ((:eqalignno)
                     (set! *equation-position* (+ *equation-position* 1))
                     (emit "</td><td")
                     (when (= *equation-position* 2)
                       (emit " width=30% align=right"))
                     (emit ">"))
                    ((:eqnarray :eqnarray*)
                     (set! *equation-position* (+ *equation-position* 1))
                     (emit "</td><td")
                     (when (= *equation-position* 1)
                       (emit " align=center width=2%"))
                     (emit ">"))
                    ((:tabular) (do-tabular-colsep))
                    ((:ruled-table) (do-ruledtable-colsep))))
                 (else (emit-html-char c))))
          ((char=? c #\|) (if (and (not (null? *tabular-stack*))
                                   (eqv? (car *tabular-stack*) ':ruled-table))
                              (do-ruledtable-colsep)
                              (emit c)))
          ((char=? c #\newline) (do-newline))
          ((char=? c #\space) (do-space))
          ((char=? c *tab*) (do-tab))
          (else
            (cond (*math-mode-p*
                    (case c
                      ((#\^) (do-sup))
                      ((#\_) (do-sub))
                      ((#\+ #\=)
                       (unless *math-script-mode-p* (emit #\space))
                       (emit c)
                       (unless *math-script-mode-p* (emit #\space)))
                      (else (if (char-alphabetic? c)
                              (emit-math-alpha-char c)
                              (emit c)))))
                 ((and *in-small-caps-p* (char-lower-case? c))
                  (emit "<small>")
                  (emit (char-upcase c))
                  (emit "</small>")
                  )
                  (else (emit c)))))))

(define do-tex-ctl-seq-completely
  (lambda (x)
    (cond ((resolve-defs x) => tex2page-string)
          ((do-tex-prim (find-corresp-prim x))
           => (lambda (y)
             (when (eqv? y ':encountered-undefined-command)
                 (emit x)))))))

(define inside-false-world?
  (lambda ()
    (memv #f *tex-if-stack*)))

(define do-tex-ctl-seq
  (lambda (z)
    ;process ctl-seq z.  Return :encountered-bye,
    ;:encountered-endinput if z = \bye, \endinput
    ;respectively
    (trace-if (> (find-count "\\tracingcommands") 0) z)
    (cond ((resolve-defs z)
           => (lambda (s)
             (trace-if (> (find-count "\\tracingmacros") 0)
                       "    --> " s)
             (toss-back-char *invisible-space*)
             (toss-back-string s)))
          ((and (inside-false-world?)
                (not (if-aware-ctl-seq? z)))
           #f)
          ((string=? z "\\enddocument") (probably-latex)
           ':encountered-bye)
          ((or (string=? z "\\bye")
               (string=? z "\\TIIPbye")) ':encountered-bye)
          ((or (string=? z "\\endinput")
               (string=? z "\\TIIPendinput"))
           (let ((next-token (get-token)))
             (when (and (not (eof-object? next-token)) (string=? next-token "\\fi"))
               (do-fi)))
           ':encountered-endinput)
          ((find-count z) (do-count= z #f))
          ((find-toks z) (do-toks= z #f))
          ((find-dimen z) (do-dimen= z #f))
          (else (do-tex-prim z)))))

(define generate-html
  (lambda ()
    (fluid-let ((*outer-p* #t))
      (let loop ()
        (let ((c (snoop-actual-char)))
          (cond ((eof-object? c)
                 #t
                 ;(values)
                 )
                ((resolve-chardefs c)
                 => (lambda (s)
                      (toss-back-char *invisible-space*)
                      (toss-back-string s)
                      (loop)))
                ((esc-char-p c)
                 (let ((r (do-tex-ctl-seq (get-ctl-seq))))
                   (case r
                     ((:encountered-endinput) #t)
                     ((:encountered-bye) ':encountered-bye)
                     (else (loop)))))
                (else (get-actual-char)
                      (do-tex-char c)
                      (loop))))))))

(define do-iffileexists
  (lambda ()
    (let* ((file (actual-tex-filename (get-filename-possibly-braced) #f))
           (thene (ungroup (get-group)))
           (elsee (ungroup (get-group))))
      (tex2page-string
       (if file thene elsee)))))

(define check-input-file-timestamp?
  (lambda (f)
    (cond ((let ((e (file-extension f)))
                 (and e (member/string-ci=?
                         e '(".t2p" ".bbl" ".ind"))))
          ;don't use timestamp of .t2p .ind .bbl files
           #f)
          (*inputting-boilerplate-p* #f)
          (*ignore-timestamp-p* #f)
          ((> *html-only* 0) #f)
          ((and (>= (string-length f) 3)
                (char=? (string-ref f 0) #\.)
                (char=? (string-ref f 1) #\/)) #f)
          ((member f *verb-written-files*) #f)
          (else #t))))

(define do-inputiffileexists
  (lambda ()
    (let* ((f (actual-tex-filename (get-filename-possibly-braced) #f))
           (then-txt (ungroup (get-group)))
           (else-txt (ungroup (get-group))))
      (cond (f (tex2page-string then-txt)
               (tex2page-file f))
            (else (tex2page-string else-txt))))))

(define tex2page-file
  (lambda (f)
    (write-log #\()
    (write-log f)
    (write-log ':separation-space)
    (set! f (tex2page-massage-file f))
    (trace-if (> (find-count "\\tracingcommands") 0)
              "Inputting file " f)
    (let ((r (call-with-input-file/buffered
               f generate-html)))
      (write-log #\))
      (write-log ':separation-space)
      r)))

(define tex2page-file-if-exists
  (lambda (f)
    (when (file-exists? f)
      (tex2page-file f))))

(define ignorable-tex-file?
  (lambda (f)
    (let ((e (or (file-extension f) "")))
      (cond ((string-ci=? e ".sty") #t)
            (else (when (string-ci=? e ".tex")
                    (set! f (substring f 0 (- (string-length f) 4))))
                  (cond ((string=? f "opmac")
                         (tex-gdef-0arg "\\TZPopmac" "1")
                         #t)
                        (else (member/string-ci=? f *tex-files-to-ignore*))))))))

(define do-input
  (lambda ()
    (ignorespaces)
    (let ((f (get-filename-possibly-braced)))
      (let ((boilerplate-index *inputting-boilerplate-p*))
        ;this is ugly code that nobody really needs, so
        ;why am I so invested in keeping it?
        (when (eqv? *inputting-boilerplate-p* 0)
          (set! *inputting-boilerplate-p* #f))
        (fluid-let ((*inputting-boilerplate-p*
                     (and boilerplate-index (+ boilerplate-index 1))))
          (cond ((ignorable-tex-file? f)
                 ;don't process .sty files,
                 ;and macro files like btxmac.tex
                 #f)
                ((member/string-ci=? f '("miniltx" "miniltx.tex"))
                 ;like above, but miniltx also makes @ a "letter"
                 (set-catcode #\@ 11)
                 #f)
                ((ormap (lambda (z) (string=? f z))
                        '("texinfo" "texinfo.tex"))
                 (let ((txi2p (actual-tex-filename "texi2p" #f)))
                   (if txi2p
                       (begin (tex2page-file txi2p)
                              (tex2page-file *current-source-file*)
                              ':encountered-endinput)
                       (terror 'do-input "File texi2p.tex not found"))))
                ((actual-tex-filename
                  f (check-input-file-timestamp? f))
                 => tex2page-file)
                (else (write-log #\() ;)
                      (write-log f)
                      (write-log ':separation-space)
                      (write-log "not found)")
                      (write-log ':separation-space))))))))

(define do-includeonly
  (lambda ()
    (ignorespaces)
    (when (eq? *includeonly-list* #t)
      (set! *includeonly-list* '()))
    (let ((c (get-actual-char)))
      (when (or (eof-object? c) (not (char=? c #\{)))
        (terror 'do-includeonly)))
    (fluid-let ((*filename-delims* (cons #\} (cons #\, *filename-delims*))))
      (let loop ()
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (cond ((eof-object? c) (terror 'do-includeonly))
                ((and *comment-char* (char=? c *comment-char*))
                 (eat-till-eol) (ignorespaces) (loop))
                ((char=? c #\,) (get-actual-char) (loop))
                ((char=? c #\}) (get-actual-char))
                ((ormap (lambda (d) (char=? c d)) *filename-delims*)
                 (terror 'do-includeonly))
                (else
                  (set! *includeonly-list*
                    (cons (get-filename) *includeonly-list*))
                  (loop))))))))

(define do-include
  (lambda ()
    (let ((f (ungroup (get-group))))
      (when (or (eq? *includeonly-list* #t)
                (ormap (lambda (i) (string=? f i)) *includeonly-list*))
        (fluid-let ((*subjobname* (file-stem-name f))
                    (*img-file-count* 0)
                    (*imgdef-file-count* 0))
          (tex2page-file
           (actual-tex-filename f (check-input-file-timestamp? f))))))))

(define do-eval-string
  (lambda (s)
    (call-with-input-string s
      (lambda (i)
        (let loop ()
          (let ((x (read i)))
            (unless (eof-object? x)
              (eval1 x) (loop))))))))

(define with-output-to-port
  (lambda (o th)
    (parameterize ((current-output-port o))
      (th))))

(define (do-eval kind)
  (let ((s (if *outer-p*
               ;Careful: This could break existing docs that rely on
               ;this!
               (ungroup
                 (fluid-let ((*esc-chars* (list *esc-char-verb*))
                             (*expand-escape-p* #t))
                   (get-group)))
               (tex-write-output-string
                 (ungroup (get-group))))))
    (unless (inside-false-world?)
      (when (> *html-only* 0) (set! kind ':html))
      (case kind
        ((:quiet) (do-eval-string s))
        (else
          (let ((o (open-output-string)))
            (with-output-to-port o
              (lambda ()
                (do-eval-string s)
                ;(display "\\relax")
                ))
            (tex2page-string (get-output-string o))))))))

(define eval-for-tex-only
  (lambda ()
    (set! *eval-for-tex-only-p* #t)
    (do-end-page)
    (ensure-file-deleted *html-page*)
    (set! *main-tex-file* #f)
    (set! *html-page* ".eval4texignore")
    (ensure-file-deleted *html-page*)
    (set! *html* (open-output-file *html-page*))))

(define expand-ctl-seq-into-string
  (lambda (cs)
    (let ((tmp-port (open-output-string)))
      (fluid-let ((*html* tmp-port))
        (do-tex-ctl-seq cs)
       ;(do-tex-ctl-seq-completely cs)
        )
      (get-output-string tmp-port))))

(define call-with-html-output-going-to
  ;this allows users of modulized versions of tex2page to
  ;fluidly set the module-global *html* from .t2p file
  (lambda (p th)
    (fluid-let ((*html* p))
      (th))))

;aux file

(define call-external-programs-if-necessary
  (lambda ()
    (let ((run-bibtex?
           (cond ((not *using-bibliography-p*) #f)
                 ((not (file-exists?
                        (string-append *aux-dir/* *jobname*
                                       *bib-aux-file-suffix* ".aux")))
                  #f)
                 ((memv 'bibliography *missing-pieces*) #t)
                 (*source-changed-since-last-run-p*
                  (flag-missing-piece ':fresh-bibliography) #t)
                 (else #f)))
          (run-makeindex?
           (cond ((not *using-index-p*) #f)
                 ((not (file-exists?
                        (string-append *aux-dir/* *jobname*
                                       *index-file-suffix* ".idx")))
                  #f)
                 ((memv ':fresh-index *missing-pieces*) #f)
                 ((memv ':index *missing-pieces*) #t)
                 (*source-changed-since-last-run-p*
                  (flag-missing-piece ':fresh-index) #t)
                 (else #f))))
      ;bibtex
      (when run-bibtex?
        (write-log ':separation-newline)
        (write-log "Running: bibtex ")
        (write-log *aux-dir/*)
        (write-log *jobname*)
        (write-log *bib-aux-file-suffix*)
        (write-log #\space)
        (system
          (string-append "bibtex " *aux-dir/* *jobname*
                         *bib-aux-file-suffix*))
        (unless (file-exists? (string-append *jobname*
                                             *bib-aux-file-suffix* ".bbl"))
          (write-log " ... failed; try manually"))
        (write-log ':separation-newline))
      ;makeindex
      (when run-makeindex?
        (write-log ':separation-newline)
        (write-log "Running: makeindex ")
        (write-log *aux-dir/*)
        (write-log *jobname*)
        (write-log *index-file-suffix*)
        (write-log #\space)
        (system
          (string-append "makeindex " *aux-dir/* *jobname*
                         *index-file-suffix*))
        (unless (file-exists?
                  (string-append *aux-dir/* *jobname*
                                 *index-file-suffix* ".ind"))
          (write-log " ... failed; try manually"))
        (write-log ':separation-newline))
      ;eval4tex
      (let ((eval4tex-aux-file (string-append *jobname* ".eval4tex")))
        (when (file-exists? eval4tex-aux-file)
          (let ((load-it (call-with-input-file eval4tex-aux-file
                           (lambda (i)
                             (let ((x (read i)))
                               (and (not (eof-object? x))
                                    (pair? x)
                                    (eq? (car x) (if 'nil 'define 'defun))))))))
            (when load-it
              (load eval4tex-aux-file)))))
      ;metapost
      (for-each
        (lambda (f)
          (when (file-exists? f)
            (write-log ':separation-newline)
            (write-log "Running: metapost ")
            (write-log f)
            (write-log ':separation-newline)
            (call-mp f)))
        *mp-files*)
      ;eps files
      (for-each
        (lambda (eps-file+img-file-stem)
          (retry-lazy-image
            (car eps-file+img-file-stem)
            (cdr eps-file+img-file-stem)))
        *missing-eps-files*)
      ;
      )))

(define first-file-that-exists
  (lambda ff
    (ormap (lambda (f)
             (and f (file-exists? f) f))
           ff)))

(define file-in-home
  (lambda (f)
    (let ((home (getenv "HOME")))
      (and home
           (let ((slash-already?
                  (let ((n (string-length home)))
                    (and (>= n 0)
                         (let ((c (string-ref home (- n 1))))
                           (or (char=? c #\/)
                               (char=? c #\\ )))))))
             (string-append home
                            (if slash-already? "" "/")
                            f))))))

(define make-target-dir
  (lambda ()
    (let ((hdir-file
           (first-file-that-exists
            (string-append *jobname* ".hdir")
            ".tex2page.hdir"
            (file-in-home ".tex2page.hdir"))))
      (when hdir-file
        (let ((hdir (call-with-input-file/buffered hdir-file
                      (lambda ()
                          (get-filename-possibly-braced)))))
          (unless (= (string-length hdir) 0)
            (case *operating-system*
              ((:cygwin :unix)
               (system (string-append "mkdir -p " hdir))
               (system (string-append "touch " hdir "/probe")))
              ((:windows)
               (system (string-append "mkdir " hdir))
               (system (string-append "echo probe > " hdir "\\probe"))))
            (let ((probe (string-append hdir "/probe")))
              (when (file-exists? probe)
                (ensure-file-deleted probe)
                (set! *aux-dir* hdir)
                (set! *aux-dir/* (string-append *aux-dir* "/"))))))))))

(define move-aux-files-to-aux-dir
  (lambda (f)
    (when (and *aux-dir*
               (or (file-exists? (string-append f ".tex"))
                   (file-exists? (string-append f ".scm"))
                   (file-exists? (string-append f (find-img-file-extn)))))
      (case *operating-system*
        ((:cygwin :unix) (system (string-append "mv " f ".* " *aux-dir*)))
        ((:windows) (system (string-append "copy " f ".* " *aux-dir*))
         (when (or (file-exists? (string-append f ".tex"))
                   (file-exists? (string-append f ".scm")))
           (system (string-append "del " f ".*"))))))))

(define start-css-file
  (lambda ()
    (let ((css-file (string-append
                      *aux-dir/* *jobname* *css-file-suffix*)))
      (ensure-file-deleted css-file)
      (set! *css-port* (open-output-file css-file))
      (display
        ;#include tex2page.css
      "body {
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
      /* background-color: #f5f5f5; */ /* Scheme version uncomment? */
      }

      blockquote {
      /* background-color: #f0e0e0; */
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

      /*
      tt i {
      font-family: serif;
      }

      .verbatim em {
      font-family: serif;
      }
      */

      /*
      .verbatim {
      color: #4d0000;
      }
      */

      .verbatim {
      background-color: #f5f5f5;
      }

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

      .slide h1.title {
      font-weight: bold;
      text-align: left;
      }

      .slide h2.section {
      margin-left: 0pt;
      }
      "
        ;#endinclude tex2page.css
        *css-port*)
      )))

(define load-aux-file
  (lambda ()
    (set-start-time)
    (let ((label-file (string-append *aux-dir/* *jobname*
                                     *label-file-suffix* ".scm")))
      (when (file-exists? label-file)
        (load-tex2page-data-file label-file)
        (delete-file label-file)))
    (unless (string=? *jobname* "texput")
      (let ((jobname-aux
             (string-append "texput" *aux-file-suffix* ".scm")))
        (when (file-exists? jobname-aux)
          (delete-file jobname-aux))))
    (let ((aux-file (string-append *aux-dir/* *jobname*
                                   *aux-file-suffix* ".scm")))
      (when (file-exists? aux-file)
        (load-tex2page-data-file aux-file)
        (delete-file aux-file)))
    (start-css-file)
    (unless (null? *toc-list*)
      (set! *toc-list* (nreverse *toc-list*)))
    (unless (null? *stylesheets*)
      (set! *stylesheets* (nreverse *stylesheets*)))
    (unless (null? *html-head*)
      (set! *html-head* (nreverse *html-head*)))))

(define update-last-modification-time
  (lambda (f)
    (let ((s (file-or-directory-modify-seconds f)))
      (when (and s (or (not *last-modification-time*)
                       (> s *last-modification-time*)))
        (set! *source-changed-since-last-run-p* #t)
        (set! *last-modification-time* s)
        ;(write-aux `(!last-modification-time ,s))
        (when (and (tex2page-flag-boolean "\\TZPcolophontimestamp")
                   (not (tex2page-flag-boolean "\\TZPcolophonlastpage"))
                   (> *html-page-count* 1))
          ;ie, mod-time was already printed
          (flag-missing-piece ':last-modification-time))))))

(define probably-latex
  (lambda ()
    (when (null? *tex-env*)
      (set! *latex-probability* (+ *latex-probability* 1))
      (when (>= *latex-probability* 2)
          (definitely-latex)))))

(define definitely-latex
  (let ((already-noted? #f))
    (lambda ()
      (unless already-noted?
        (set! already-noted? #t)
        (!definitely-latex)
        (write-aux `(!definitely-latex))))))

;the following are used to make entries in the aux file.
;Their names all start with "!"

(define !tex-like-layout
  (lambda ()
    (set! *tex-like-layout-p* #t)))

(define !head-line
  (lambda (e)
    (tex-def-toks "\\headline" e #t)))

(define !foot-line
  (lambda (e)
    (tex-def-toks "\\footline" e #t)))

(define !toc-page
  (lambda (p)
    (set! *toc-page* p)))

(define !index-page
  (lambda (p)
    (set! *index-page* p)))

(define !toc-entry
  (lambda (level number page label header)
    (set! *toc-list*
          (cons
           (make-tocentry 'level level
                          'number number
                          'page page
                          'label label
                          'header header)
           *toc-list*))))

(define !label
  (lambda (label html-page name value)
    (table-put! *label-table* label
                (make-label 'src *label-source*
                            'page html-page
                            'name name
                            'value value))))

(define !index
  (lambda (index-number html-page-number)
    (table-put! *index-table* index-number
                html-page-number)))

(define !last-modification-time
  (let ((seconds-from-1900-to-1970
         ;CL's (encode-universal-time 0 0 0 1 1 1970 0)
         2208988800))
    (lambda (s . epoch)
      (set! *last-modification-time*
            (case (if (pair? epoch) (car epoch) 1970)
              ((1900) (- s seconds-from-1900-to-1970))
              ((1970) s))))))

(define !last-page-number
  (lambda (n)
    (set! *last-page-number* n)))

(define !single-page
  (lambda ()
    (tex-def-0arg "\\TZPsinglepage" "1")))

(define (!slides)
  (tex-def-0arg "\\TZPslides" "1"))

(define (!script jsf)
  (cond ((or (fully-qualified-url-p jsf)
             (file-exists? (ensure-url-reachable jsf)))
         (set! *scripts* (cons jsf *scripts*)))
        (else (write-log "! Can't find script ")
              (write-log jsf)
              (write-log ':separation-newline))))

(define !using-chapters
  (lambda ()
    (set! *using-chapters-p* #t)))

(define !definitely-latex
  (lambda ()
    (set! *tex-format* ':latex)
    (when (< (get-gcount "\\secnumdepth") -1)
      (set-gcount! "\\secnumdepth" 3))))

(define !using-external-program ;obsolete
  (lambda (x)
    ;(set! *external-programs* (cons x *external-programs*))
    #f
    ))

(define !external-labels ;obsolete
  (lambda (f)
    #f))

(define !doctype
  (lambda (d)
    (set! *doctype* d)))

(define !colophon
  (lambda (x)
    (case x
      ((:last-page)
       (tex-def-0arg "\\TZPcolophonlastpage" "1"))
      ((:no-timestamp)
       (tex-def-0arg "\\TZPcolophondisabletimestamp" "1")
       (tex-def-0arg "\\TZPcolophontimestamp" "0"))
      ((:dont-credit-tex2page)
       (tex-def-0arg "\\TZPcolophondisablecredit" "1")
       (tex-def-0arg "\\TZPcolophoncredit" "0"))
      ((:dont-link-to-tex2page-website)
       (tex-def-0arg "\\TZPcolophondisableweblink" "1")
       (tex-def-0arg "\\TZPcolophonweblink" "0")))))

(define !tex-text
  (lambda (n)
    (when (= n 0)
      (set! *ligatures-p* #f))))

(define !opmac-iis
  (lambda (lhs sub)
    (unless *opmac-index-sub-table*
      (set! *opmac-index-sub-table* (make-table 'equ string=?)))
    (table-put! *opmac-index-sub-table* lhs sub)))

;;

(define fully-qualified-url-p
  (lambda (u)
    (or (substring? "//" u) ;URL
        (char=? (string-ref u 0) #\/) ;website-dependent pathname
        )))

(define fully-qualified-pathname?
  (lambda (f)
    (let ((n (string-length f)))
      (if (= n 0) #t
          (let ((c0 (string-ref f 0)))
            (cond ((char=? c0 #\/) #t)
                  ((= n 1) #f)
                  ((and (char-alphabetic? c0)
                        (char=? (string-ref f 1) #\:))
                   #t)
                  (else #f)))))))

(define ensure-url-reachable
  (lambda (f)
    (if (and *aux-dir* (not (fully-qualified-url-p f))
                     ;but we don't want to try too hard here, so
             (not (substring? "/" f)))
        (let ((real-f (string-append *aux-dir/* f)))
          (when (and (file-exists? f)
                     (not (file-exists? real-f)))
            (case *operating-system*
              ((:cygwin :unix)
               (system (string-append "cp -p " f " " real-f)))
              ((:windows)
               (system (string-append "copy/b " f
                         " " *aux-dir*)))))
          real-f)
        f)))

(define !stylesheet
  (lambda (css)
    (cond ((or (fully-qualified-url-p css)
               (file-exists? (ensure-url-reachable css)))
           (set! *stylesheets* (cons css *stylesheets*)))
          (else (write-log "! Can't find stylesheet ")
                (write-log css)
                (write-log ':separation-newline)))))

(define !html-head
  (lambda (s)
    (set! *html-head* (cons s *html-head*))))

(define !html-redirect
  (lambda (url seconds)
    (set! *redirect-url* url)
    (set! *redirect-delay* seconds)
    (!html-head (string-append "<meta http-equiv=\"refresh\" content=\""
                               seconds ";"
                               url "\">"))))

(define !default-title
  (lambda (title)
    (unless *title* ;already set
      (set! *title* title))))

(define !preferred-title
  (lambda (title)
    (set! *title* title)))

(define !infructuous-calls-to-tex2page
  (lambda (n)
    (set! *infructuous-calls-to-tex2page* n)))

(define load-tex2page-data-file
  (lambda (f)
    (when (file-exists? f)
      (fluid-let ((*current-source-file* f)
                  (*input-line-no* 0))
        (call-with-input-file f
          (lambda (i)
            (let loop ()
              (let ((e (read i)))
                (unless (eof-object? e)
                  (set! *input-line-no* (+ *input-line-no* 1))
                  (let ((x (car e)))
                    (apply
                      (case x
                        ((!colophon) !colophon)
                        ((!default-title) !default-title)
                        ((!definitely-latex) !definitely-latex)
                        ((!doctype) !doctype)
                        ((!external-labels) !external-labels)
                        ((!foot-line) !foot-line)
                        ((!head-line) !head-line)
                        ((!html-head) !html-head)
                        ((!html-redirect) !html-redirect)
                        ((!index) !index)
                        ((!index-page) !index-page)
                        ((!infructuous-calls-to-tex2page) !infructuous-calls-to-tex2page)
                        ((!label) !label)
                        ((!last-modification-time) !last-modification-time)
                        ((!last-page-number) !last-page-number)
                        ((!opmac-iis) !opmac-iis)
                        ((!preferred-title) !preferred-title)
                        ((!script) !script)
                        ((!single-page) !single-page)
                        ((!slides) !slides)
                        ((!stylesheet) !stylesheet)
                        ((!tex-like-layout) !tex-like-layout)
                        ((!tex-text) !tex-text)
                        ((!toc-entry) !toc-entry)
                        ((!toc-page) !toc-page)
                        ((!using-chapters) !using-chapters)
                        ((!using-external-program) !using-external-program)
                        (else (terror 'load-tex2page-data-file
                                      "Fatal aux file error; I'm stymied."
                                      )))
                      (cdr e))
                    (loop)))))))))))

;

(define tex2page-massage-file
  (lambda (f) f))

(define tex2page-help
  (lambda (not-a-file)
    (write-aux
      `(!infructuous-calls-to-tex2page
         ,(+ *infructuous-calls-to-tex2page* 1)))
    ;
    (unless (or (string=? not-a-file "--help")
                (string=? not-a-file "--missing-arg")
                (string=? not-a-file "--version"))
      (write-log "! I can't find file `")
      (write-log not-a-file)
      (write-log "'.")
      (write-log ':separation-newline))
    ;
    (cond
      ((string=? not-a-file "--version")
       (write-log "Copyright (c) 1997-")
       (write-log (substring *tex2page-version* 0 4))
       (write-log ", Dorai Sitaram.

Permission to distribute and use this work for any
purpose is hereby granted provided this copyright
notice is included in the copy.  This work is provided
as is, with no warranty of any kind.

For information on how to use TeX2page, please see")
                  (write-log #\newline)
                  (write-log *tex2page-website*)
                  (write-log #\.)
                  (write-log #\newline)
                  (write-log #\newline))
      ((string=? not-a-file "--help")
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

For more information on how to use tex2page, please see"
)
       (write-log #\newline)
       (write-log *tex2page-website*)
       (write-log #\.)
       (write-log #\newline)
       (write-log #\newline))
      (else
        (when (string=? not-a-file "--missing-arg")
          (write-log "! Missing command-line argument.")
          (write-log ':separation-newline))
        ;(write-log "No pages of output.")
        ;(write-log ':separation-newline)
        (when (> *infructuous-calls-to-tex2page* 0)
          (write-log "You have called TeX2page")
          (write-log #\space)
          (write-log (+ *infructuous-calls-to-tex2page* 1))
          (write-log #\space)
          (write-log "times without a valid input document.")
          (write-log ':separation-newline))
        (cond ((>= *infructuous-calls-to-tex2page* 4)
               (write-log "I can't go on meeting you like this.")
               (write-log ':separation-newline)
               (write-log "Good bye!")
               (write-log ':separation-newline))
              (else
                (write-log "Do you need help using TeX2page?
Try the commands
  tex2page --help
  tex2page --version"
  )
                 (write-log ':separation-newline)))))
      (close-all-open-ports)
      ;(close-output-port *aux-port*)
      ;(close-output-port *log-port*)
      ))

(define non-fatal-error
  (lambda ss
    (emit-link-start (string-append *jobname* ".hlog"))
    (emit "<span style=\"color: red\">&#x2388;&#xa0;")
    (for-each emit-html-string ss)
    ;(for-each tex2page-string ss)
    (emit-link-stop)
    (emit "</span>")))

(define do-math-ctl-seq
  (lambda (s)
    (cond ((find-math-def s)
           => (lambda (x) ((tdef.thunk x))))
          (else
            (unless *math-needs-image-p* (set! *math-needs-image-p* #t))
            (emit (substring s 1 (string-length s)))
            ;(emit " ")
            ))))

(define tex-def-math-prim
  (lambda (cs thunk)
    (tex-def cs '() #f #f thunk cs #f *math-primitive-texframe*)))

(define tex-defsym-math-prim
  (lambda (cs str)
    (tex-def cs '() #f #f (lambda () (emit str)) cs #f *math-primitive-texframe*)))

(define make-reusable-math-image-as-needed
  (lambda (cs . expn)
    (let ((expn (if (null? expn) cs (car expn))))
      (tex-def-math-prim cs
                         (lambda ()
                           (tex2page-string
                             (string-append "\\global\\imgdef" cs
                                            "{$" expn "$}"))
                           (tex2page-string cs))))))

;#include primdefs.scm
;TeXbook, sec 18.2, non-italic letters in formulas

(tex-defsym-math-prim "\\Pr" "Pr ")
(tex-defsym-math-prim "\\TIIPsec" "sec ")
(tex-defsym-math-prim "\\arccos" "arccos ")
(tex-defsym-math-prim "\\arcsin" "arcsin ")
(tex-defsym-math-prim "\\arctan" "arctan ")
(tex-defsym-math-prim "\\arg" "arg ")
(tex-defsym-math-prim "\\tan" "tan ")
(tex-defsym-math-prim "\\tanh" "tanh ")
(tex-defsym-math-prim "\\sin" "sin ")
(tex-defsym-math-prim "\\sinh" "sinh ")
(tex-defsym-math-prim "\\cos" "cos ")
(tex-defsym-math-prim "\\cosh" "cosh ")
(tex-defsym-math-prim "\\cot" "cot ")
(tex-defsym-math-prim "\\coth" "coth ")
(tex-defsym-math-prim "\\csc" "csc ")
(tex-defsym-math-prim "\\deg" "deg ")
(tex-defsym-math-prim "\\det" "det ")
(tex-defsym-math-prim "\\dim" "dim ")
(tex-defsym-math-prim "\\exp" "exp ")
(tex-defsym-math-prim "\\gcd" "gcd ")
(tex-defsym-math-prim "\\hom" "hom ")
(tex-defsym-math-prim "\\sup" "sup ")
(tex-defsym-math-prim "\\inf" "inf ")
(tex-defsym-math-prim "\\ker" "ker ")
(tex-defsym-math-prim "\\lg" "lg ")
(tex-defsym-math-prim "\\liminf" "lim inf ")
(tex-defsym-math-prim "\\limsup" "lim sup ")
(tex-defsym-math-prim "\\ln" "ln ")
(tex-defsym-math-prim "\\log" "log ")
(tex-defsym-math-prim "\\max" "max ")
(tex-defsym-math-prim "\\min" "min ")

;TeXbook, appendix F, p 434

;1. lowercase Greek

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

;2. uppercase Greek

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

;4. misc symbols of type Ord

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
(tex-defsym-math-prim "\\|" "&#x2225;")
(tex-defsym-math-prim "\\angle" "&#x2220;")
(tex-defsym-math-prim "\\triangle" "&#x394;")
(tex-defsym-math-prim "\\backslash" "\\")
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

;6. large operators

(tex-defsym-math-prim "\\sum" "&#x2211;")
(tex-defsym-math-prim "\\prod" "&#x220f;")
(tex-defsym-math-prim "\\coprod" "&#x2210;")
(tex-def-math-prim "\\int" do-integral) ;(lambda () (emit "&#x222b;")))
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

;7. binary operations

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

;8. relations

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
(tex-defsym-math-prim "\\cong" "&#x2245;")
(tex-defsym-math-prim "\\bowtie" "&#x22c8;")
(tex-defsym-math-prim "\\propto" "&#x221d;")
(tex-defsym-math-prim "\\models" "&#x22a8;")
(tex-defsym-math-prim "\\doteq" "&#x2250;")
(tex-defsym-math-prim "\\propto" "&#x221d;")
(tex-defsym-math-prim "\\perp" "&#x22a5;")

;9. negated relations

(define (do-not )
  (ignorespaces)
  (let ((c (snoop-actual-char)))
    (if (esc-char-p c)
        (let ((x (get-ctl-seq)))
          (emit (cond ((string=? x "\\leq") "&#x2270;")
                      ((string=? x "\\le") "&#x2270;")
                      ((string=? x "\\prec") "&#x2280;")
                      ((string=? x "\\preceq") "&#x22e0;")
                      ((string=? x "\\subset") "&#x2284;")
                      ((string=? x "\\subseteq") "&#x2288;")
                      ((string=? x "\\sqsubseteq") "&#x22e2;")
                      ((string=? x "\\geq") "&#x2271;")
                      ((string=? x "\\ge") "&#x2271;")
                      ((string=? x "\\succ") "&#x2281;")
                      ((string=? x "\\succeq") "&#x22e1;")
                      ((string=? x "\\supset") "&#x2285;")
                      ((string=? x "\\supseteq") "&#x2289;")
                      ((string=? x "\\sqsupseteq") "&#x22e3;")
                      ((string=? x "\\equiv") "&#x2262;")
                      ((string=? x "\\sim") "&#x2241;")
                      ((string=? x "\\simeq") "&#x2244;")
                      ((string=? x "\\approx") "&#x2249;")
                      ((string=? x "\\cong") "&#x2247;")
                      ((string=? x "\\asymp") "&#x226d;")
                      (else (toss-back-string x) "/"))))
      (case c
        ((#\< #\> #\=) (get-actual-char)
                       (emit (cond ((char=? c #\<) "&#x226e;")
                                   ((char=? c #\>) "&#x226f;")
                                   ((char=? c #\=) "&#x2260;"))))
        (else (emit "/"))))))

(tex-def-math-prim "\\not" do-not)
(tex-defsym-math-prim "\\notin" "&#x2209;")

;10. arrows

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
(tex-defsym-math-prim "\\rightleftharpoons" "&#x21cb;")
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
(tex-defsym-math-prim "\\uparrow" "&#x2191;")
(tex-defsym-math-prim "\\Uparrow" "&#x21d1;")
(tex-defsym-math-prim "\\downarrow" "&#x2193;")
(tex-defsym-math-prim "\\Downarrow" "&#x21d3;")
(tex-defsym-math-prim "\\nearrow" "&#x2197;")
(tex-defsym-math-prim "\\searrow" "&#x2198;")
(tex-defsym-math-prim "\\swarrow" "&#x2199;")
(tex-defsym-math-prim "\\nwarrow" "&#x2196;")

;11. openings

(tex-defsym-math-prim "\\lbrack" "[")
(tex-defsym-math-prim "\\lbrace" "{")
(tex-defsym-math-prim "\\lfloor" "&#x230a;")
(tex-defsym-math-prim "\\langle" "&#x27e8;")
(tex-defsym-math-prim "\\lceil" "&#x2308;")

;12. closings

(tex-defsym-math-prim "\\rbrack" "]")
(tex-defsym-math-prim "\\rbrace" "}")
(tex-defsym-math-prim "\\rfloor" "&#x230b;")
(tex-defsym-math-prim "\\rangle" "&#x27e9;")
(tex-defsym-math-prim "\\rceil" "&#x2309;")

;13. punctuation

(tex-def-math-prim "\\colon" (lambda () (emit #\:)))
(tex-def-math-prim "\\ldotp" (lambda () (emit #\.)))
(tex-let-prim "\\cdotp" "\\cdot")

;14. alternate names

(tex-defsym-math-prim "\\ne" "&#x2260;")
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

(tex-defsym-prim "\\S" "&#xa7;")
(tex-defsym-prim "\\P" "&#xb6;")
(tex-defsym-prim "\\dag" "&#x2020;")
(tex-defsym-prim "\\ddag" "&#x2021;")

;end appendix F

;latexsym (latex sec 3.3.2)

(tex-defsym-math-prim "\\lhd" "&#x22b2;")
(tex-defsym-math-prim "\\rhd" "&#x22b3;")
(tex-defsym-math-prim "\\unlhd" "&#x22b4;")
(tex-defsym-math-prim "\\unrhd" "&#x22b5;")

(tex-defsym-math-prim "\\sqsubset" "&#x228f;")
(tex-defsym-math-prim "\\sqsupset" "&#x2290;")

(tex-defsym-math-prim "\\Join" "&#x2a1d;")

(tex-defsym-math-prim "\\leadsto" "&#x2933;")

(tex-defsym-math-prim "\\mho" "&#x2127;")
(tex-defsym-math-prim "\\Box" "&#x25ab;")
(tex-defsym-math-prim "\\Diamond" "&#x25c7;")

;amssymb

(tex-defsym-prim "\\checkmark" "&#x2713;")
(tex-defsym-prim "\\maltese" "&#x2720;")

(tex-defsym-math-prim "\\because" "&#x2235;")
(tex-defsym-math-prim "\\geqslant" "&#x2a7e;")
(tex-defsym-math-prim "\\leqslant" "&#x2a7d;")
(tex-defsym-math-prim "\\therefore" "&#x2234;")
(tex-defsym-math-prim "\\Game" "&#x2141;")
(tex-defsym-math-prim "\\Finv" "&#x2132;")
(tex-defsym-math-prim "\\beth" "&#x2136;")
(tex-defsym-math-prim "\\gimel" "&#x2137;")
(tex-defsym-math-prim "\\daleth" "&#x2138;")
(tex-defsym-math-prim "\\lozenge" "&#x2662;")
(tex-defsym-math-prim "\\Bbbk" "&#x1d55c;")

;

(tex-def-math-prim "\\eqalign" (lambda () (do-eqalign ':eqalign)))
(tex-def-math-prim "\\eqalignno" (lambda () (do-eqalign ':eqalignno)))
(tex-def-math-prim "\\displaylines" (lambda () (do-eqalign ':displaylines)))
(tex-def-math-prim "\\noalign" do-noalign)
(tex-def-math-prim "\\frac" do-frac)
(tex-def-math-prim "\\pmatrix" do-pmatrix)
(tex-def-math-prim "\\matrix" do-relax) ;??? better?
(tex-def-math-prim "\\eqno" do-eqno)

(define (do-math-font f)
  (lambda ()
    (fluid-let ((*math-font* f))
      (tex2page-string (get-token)))))

(tex-def-math-prim "\\mathbf" (do-math-font ':bf))
(tex-def-math-prim "\\mathrm" (do-math-font ':rm))
(tex-def-math-prim "\\mathbb" (do-math-font ':bb))
(tex-def-math-prim "\\mathcal" (do-math-font ':cal))
(tex-def-math-prim "\\mathfrak" (do-math-font ':frak))

(tex-def-math-prim "\\over" do-over); (lambda () (emit "/")))
(tex-def-math-prim "\\sqrt"
 (lambda () (emit "&#x221a;(") (tex2page-string (get-token)) (emit ")")))
(tex-def-math-prim "\\left" do-math-left)
(tex-def-math-prim "\\right" do-math-right)

;spaces

(define (kern len)
  (string-append "<span style=\"margin-left: "
    ;in following, tried &#x200c; (= zwnj) instead of space,
    ;but it causes table-row fault
    len "\"> </span>"))

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

(tex-defsym-prim "\\AA" "&#xc5;")
(tex-defsym-prim "\\aa" "&#xe5;")
(tex-def-prim "\\abstract"
 (lambda ()
     (tex2page-string "\\quote")
     (tex2page-string "\\centerline{\\bf\\abstractname}\\par")))
(tex-def-prim "\\activettchar" do-opmac-activettchar)
(tex-def-prim "\\addcontentsline" do-addcontentsline)
(tex-def-prim "\\addtocounter" do-addtocounter)
(tex-def-prim "\\advance" (lambda () (do-advance (globally-p))))
(tex-defsym-prim "\\AE" "&#xc6;")
(tex-defsym-prim "\\ae" "&#xe6;")
(tex-def-prim "\\afterassignment" do-afterassignment)
(tex-def-prim "\\aftergroup" do-aftergroup)
(tex-def-prim "\\align" (lambda () (do-equation ':align)))
(tex-def-prim "\\alltt" do-alltt)
(tex-def-prim "\\appendix" do-appendix)
(tex-defsym-prim "\\appendixname" "Appendix ")
(tex-def-prim "\\arabic" do-arabic)
(tex-def-prim "\\array" (lambda () (do-tabular #t)))
(tex-def-prim "\\author" do-author)

(tex-def-prim "\\b" (lambda () (do-diacritic ':barunder)))
(tex-def-prim "\\begin" do-begin)
(tex-def-prim "\\beginchapter" do-beginchapter)
(tex-def-prim "\\beginsection" do-beginsection)
(tex-def-prim "\\beginthebibliography" do-thebibliography)
(tex-def-prim "\\begintt" do-begintt)
(tex-def-prim "\\begintts" (lambda() (do-scm-slatex-lines "tt" #t #f)))
(tex-def-prim "\\begitems" do-opmac-begitems)
(tex-def-prim "\\bf" (lambda () (do-switch ':bf)))
(tex-def-prim "\\bgcolor" (lambda () (do-switch ':bgcolor)))
(tex-def-prim-0arg "\\bgroup" "{")
(tex-def-prim "\\bib" do-opmac-bib)
(tex-def-prim "\\bibitem" do-bibitem)
(tex-def-prim "\\bibliography" do-bibliography)
(tex-def-prim "\\bibliographystyle" do-bibliographystyle)
(tex-def-prim "\\bigbreak" (lambda () (do-bigskip ':bigskip)))
(tex-def-prim "\\bigskip" (lambda () (do-bigskip ':bigskip)))
(tex-defsym-prim "\\break" "<br>")
(tex-defsym-prim "\\bull" "&#x25fe;")

(tex-def-prim "\\c" (lambda () (do-diacritic ':cedilla)))
(tex-def-prim "\\caption" do-caption)
(tex-def-prim "\\catcode" do-catcode)
(tex-defsym-prim "\\cdots" "&#x22ef;")
(tex-def-prim "\\center" (lambda () (do-block ':center)))
(tex-def-prim "\\centerline" (lambda () (do-function "\\centerline")))
(tex-def-prim "\\chap" (lambda () (do-opmac-heading 0)))
(tex-def-prim "\\chapter" (lambda () (do-heading 0)))
(tex-defsym-prim "\\chaptername" "Chapter ")
(tex-def-prim "\\char" do-char)
(tex-def-prim "\\chardef" do-chardef)
(tex-def-prim "\\cite" do-cite)
(tex-def-prim "\\closegraphsfile" do-mfpic-closegraphsfile)
(tex-def-prim "\\closein" (lambda () (do-close-stream ':in)))
(tex-def-prim "\\closeout" (lambda () (do-close-stream ':out)))
(tex-def-prim "\\color" do-color)
(tex-def-prim "\\colorbox" do-colorbox)
(tex-def-prim "\\convertMPtoPDF" do-convertmptopdf)
(tex-defsym-prim "\\copyright" "&#xa9;")
(tex-def-prim "\\countdef" (lambda () (do-newcount #t) (eat-integer)))
(tex-def-prim "\\CR" (lambda () (do-cr "\\CR")))
(tex-def-prim "\\cr" (lambda () (do-cr "\\cr")))
(tex-def-prim "\\csname" do-csname)
(tex-def-prim "\\cssblock" do-cssblock)

(tex-def-prim "\\d" (lambda () (do-diacritic ':dotunder)))
(tex-def-prim "\\." (lambda () (do-diacritic ':dot)))
(tex-defsym-prim "\\dag" "&#x2020;")
(tex-def-prim "\\date" do-date)
(tex-defsym-prim "\\ddag" "&#x2021;")
(tex-defsym-prim "\\ddots" "&#x22f1;")
(tex-def-prim "\\def" (lambda () (do-def (globally-p) #f)))
(tex-def-prim "\\defcsactive" (lambda () (do-defcsactive (globally-p))))
(tex-def-prim "\\definecolor" do-definecolor)
(tex-def-prim "\\DefineNamedColor" (lambda () (get-token) (do-definecolor)))
(tex-def-prim "\\definexref" do-definexref)
(tex-def-prim "\\definitelylatex" definitely-latex)
(tex-def-prim "\\defschememathescape" (lambda () (scm-set-mathescape #t)))
(tex-defsym-prim "\\degree" "&#xb0;")
(tex-def-prim "\\description"
  (lambda ()
    (do-end-para)
    (set! *tabular-stack* (cons ':description *tabular-stack*))
    (emit "<dl><dt></dt><dd>")))
(tex-defsym-prim "\\DH" "&#xd0;")
(tex-defsym-prim "\\dh" "&#xf0;")
(tex-def-prim "\\discretionary" do-discretionary)
(tex-def-prim "\\displaymath"
 (lambda () (do-latex-env-as-image "displaymath" ':display)))
(tex-def-prim "\\divide" (lambda () (do-divide (globally-p))))
(tex-def-prim "\\document" probably-latex)
(tex-def-prim "\\documentclass" do-documentclass)
(tex-def-prim "\\dontuseimgforhtmlmath" (lambda () (tex-def-0arg "\\TZPmathtext" "1"))) ;obsolete
(tex-def-prim "\\dontuseimgforhtmlmathdisplay" (lambda () (tex-def-0arg "\\TZPmathtext" "1"))) ;obsolete
(tex-def-prim "\\dontuseimgforhtmlmathintext" (lambda () #t)) ;obsolete
(tex-defsym-prim "\\dots" "&#x2026;")

(tex-def-prim "\\edef" (lambda () (do-def (globally-p) #t)))
(tex-def-prim-0arg "\\egroup" "}")
(tex-def-prim "\\eject" do-eject)
(tex-def-prim "\\else" do-else)
(tex-def-prim "\\em" (lambda () (do-switch ':em)))
(tex-def-prim "\\emph" (lambda () (do-function "\\emph")))
(tex-def-prim-0arg "\\empty"
                   ;for \ifx comparisons
                   "")
(tex-def-prim "\\end" do-end)
(tex-def-prim "\\endalign" do-end-equation)
(tex-def-prim "\\endalltt" do-end-alltt)
(tex-def-prim "\\endarray" do-end-tabular)
(tex-def-prim "\\endcenter" do-end-block)
(tex-def-prim "\\enddescription"
 (lambda ()
     (pop-tabular-stack ':description)
     (do-end-para)
     (emit "</dd></dl>")
     (do-noindent)))
(tex-def-prim "\\endeqnarray" do-end-equation)
(tex-def-prim "\\endequation" do-end-equation)
(tex-def-prim "\\endenumerate" do-endenumerate)
(tex-def-prim "\\endfigure" (lambda () (do-end-table/figure ':figure)))
(tex-def-prim "\\endflushleft" do-end-block)
(tex-def-prim "\\endflushright" do-end-block)
(tex-def-prim "\\endgraf" do-para)
(tex-def-prim "\\endhtmlimg"
 (lambda () (terror 'tex-def-prim "Unmatched \\endhtmlimg")))
(tex-def-prim "\\endhtmlonly"
 (lambda () (set! *html-only* (- *html-only* 1))))
(tex-def-prim "\\endinput" do-endinput)
(tex-def-prim "\\enditemize" do-enditemize)
(tex-def-prim "\\enditems" do-opmac-enditems)
(tex-def-prim "\\endminipage" do-endminipage)
(tex-def-prim "\\endquote"
 (lambda () (do-end-para) (egroup) (emit "</blockquote>")))
(tex-def-prim "\\endruledtable" do-endruledtable)
(tex-def-prim "\\endtabbing" do-end-tabbing)
(tex-def-prim "\\endtable" (lambda () (do-end-table/figure ':table)))
(tex-def-prim "\\endtableplain" do-end-table-plain)
(tex-def-prim "\\endtabular" do-end-tabular)
(tex-def-prim "\\endthebibliography"
 (lambda () (emit "</table>") (egroup) (do-para)))
(tex-def-prim "\\endverbatim" do-endverbatim-eplain)
(tex-def-prim "\\enquote" do-enquote)
(tex-def-prim "\\enumerate" do-enumerate)
(tex-def-prim "\\epsfbox" do-epsfbox)
(tex-def-prim "\\epsfig" do-epsfig)
(tex-def-prim "\\eqnarray" (lambda () (do-equation ':eqnarray)))
(tex-def-prim "\\equation" (lambda () (do-equation ':equation)))
(tex-def-prim "\\errmessage" do-errmessage)
(tex-def-prim "\\eval" (lambda () (do-eval ':both)))
;(tex-def-prim "\\TIIPeval" (lambda () (do-eval ':inner)))
(tex-def-prim "\\evalh" (lambda () (do-eval ':html)))
(tex-def-prim "\\evalq" (lambda () (do-eval ':quiet)))
(tex-def-prim "\\expandafter" do-expandafter)
(tex-def-prim "\\expandhtmlindex" expand-html-index)
(tex-def-prim "\\externaltitle" do-externaltitle)

(tex-def-prim "\\fi" (lambda () (do-fi)))
(tex-def-prim "\\figure" (lambda () (do-table/figure ':figure)))
(tex-def-prim "\\fiverm" (lambda () (do-switch ':fiverm)))
(tex-def-prim "\\flushleft" (lambda () (do-block ':flushleft)))
(tex-def-prim "\\flushright" (lambda () (do-block ':flushright)))
(tex-defsym-prim "\\fmtname" "TeX2page")
(tex-defsym-prim "\\fmtversion" *tex2page-version*)
(tex-def-prim "\\folio" (lambda () (emit *html-page-count*)))
(tex-def-prim "\\font" do-font)
(tex-def-prim "\\fontdimen" do-fontdimen)
(tex-def-prim "\\footnote" do-footnote)
(tex-def-prim "\\footnotesize" (lambda () (do-switch ':footnotesize)))
(tex-def-prim "\\frac" do-frac)
(tex-def-prim "\\futurelet" do-futurelet)
(tex-def-prim "\\futurenonspacelet" do-futurenonspacelet)

(tex-def-prim "\\gdef" (lambda () (do-def #t #f)))
(tex-def-prim "\\global" do-global)
(tex-def-prim "\\gobblegroup" get-group)

(tex-def-prim "\\H" (lambda () (do-diacritic ':hungarianumlaut)))
(tex-def-prim "\\halign" do-halign)
(tex-def-prim "\\hbox" do-box)
(tex-def-prim "\\hfill" (lambda () (emit-nbsp 5)))
(tex-def-prim "\\hlstart" do-hlstart)
(tex-def-prim "\\href" do-urlh)
(tex-def-prim "\\hrule"
 (lambda () (do-end-para) (emit "<hr>") (emit-newline) (do-para)))
(tex-def-prim "\\hskip" do-hskip)
(tex-def-prim "\\hspace" do-hspace)
(tex-def-prim "\\htmladdimg" do-htmladdimg)
(tex-def-prim "\\htmlcolophon" do-htmlcolophon) ;obsolete
(tex-def-prim "\\htmldoctype" do-htmldoctype)
(tex-def-prim "\\htmlgif" (lambda () (do-htmlimg "htmlgif"))) ;obsolete
(tex-def-prim "\\htmlheadonly" do-htmlheadonly)
(tex-def-prim "\\htmlimageconversionprogram" do-htmlimageconversionprogram) ;obsolete
(tex-def-prim "\\htmlimageformat" do-htmlimageformat) ;obsolete
(tex-def-prim "\\htmlimg" (lambda () (do-htmlimg "htmlimg"))) ;obsolete
(tex-def-prim "\\htmlimgmagnification" do-htmlimgmagnification) ;obsolete
(tex-def-prim "\\htmlmathstyle" do-htmlmathstyle)
(tex-def-prim "\\htmlonly" (lambda () (set! *html-only* (+ *html-only* 1))))
(tex-def-prim "\\htmlpagelabel" do-htmlpagelabel)
(tex-def-prim "\\htmlpageref" do-htmlpageref)
(tex-def-prim "\\htmlref" do-htmlref)
(tex-def-prim "\\htmlrefexternal" do-htmlrefexternal)
(tex-def-prim "\\htmlspan" (lambda () (do-switch ':span)))
(tex-def-prim "\\htmldiv" (lambda () (do-switch ':div)))
(tex-def-prim "\\huge" (lambda () (do-switch ':huge)))
(tex-def-prim "\\Huge" (lambda () (do-switch ':huge-cap)))
(tex-def-prim "\\hyperref" do-hyperref)
(tex-def-prim "\\hyperlink" do-hyperlink)
(tex-def-prim "\\hypertarget" do-hypertarget)

(tex-defsym-prim "\\i" "&#x131;")
(tex-def-prim "\\if" do-if)
(tex-def-prim "\\ifcase" do-ifcase)
(tex-def-prim "\\ifdefined" do-ifdefined)
(tex-def-prim "\\ifeof" do-ifeof)
(tex-def-prim "\\ifdim" do-iffalse)
(tex-def-prim "\\iffalse" do-iffalse)
(tex-def-prim "\\IfFileExists" do-iffileexists)
(tex-def-prim "\\ifhmode" do-iftrue)
(tex-def-prim "\\ifmmode" do-ifmmode)
(tex-def-prim "\\ifnum" do-ifnum)
(tex-def-prim "\\iftrue" do-iftrue)
(tex-def-prim "\\ifx" do-ifx)
(tex-def-prim "\\ifodd" do-ifodd)
(tex-def-prim "\\ignorenextinputtimestamp"
  (lambda ()
    (unless *inputting-boilerplate-p* (set! *inputting-boilerplate-p* 0))))
(tex-def-prim "\\ignorespaces" ignorespaces)
(tex-def-prim "\\ii" (lambda () (do-opmac-ii #f)))
(tex-def-prim "\\iid" (lambda () (do-opmac-ii #t)))
(tex-def-prim "\\iis" do-opmac-iis)
(tex-def-prim "\\imgdef" (lambda () (make-reusable-img (globally-p))))
(tex-def-prim "\\imgpreamble" do-img-preamble)
(tex-def-prim "\\IMGtabbing"
 (lambda () (do-latex-env-as-image "tabbing" ':display)))
(tex-def-prim "\\IMGtabular"
 (lambda () (do-latex-env-as-image "tabular" ':display)))
(tex-def-prim "\\include" do-include)
(tex-def-prim "\\includeexternallabels" do-includeexternallabels)
(tex-def-prim "\\includeonly" do-includeonly)
(tex-def-prim "\\includegraphics" do-includegraphics)
(tex-def-prim "\\indent" do-indent)
(tex-def-prim "\\index" do-index)
(tex-def-prim "\\indexitem" (lambda () (do-indexitem 0)))
(tex-def-prim "\\indexsubitem" (lambda () (do-indexitem 1)))
(tex-def-prim "\\indexsubsubitem" (lambda () (do-indexitem 2)))
(tex-def-prim "\\input" do-input)
(tex-def-prim "\\inputcss" do-inputcss)
(tex-def-prim "\\inputexternallabels" do-inputexternallabels)
(tex-def-prim "\\InputIfFileExists" do-inputiffileexists)
(tex-def-prim "\\inputindex" (lambda () (do-inputindex #f)))
(tex-def-prim "\\it" (lambda () (do-switch ':it)))
(tex-def-prim "\\item" do-item)
(tex-def-prim "\\itemitem" (lambda () (do-plain-item 2)))
(tex-def-prim "\\itemize" do-itemize)
(tex-def-prim "\\itshape" (lambda () (do-switch ':it)))

(tex-defsym-prim "\\j" "&#x237;")
(tex-def-prim "\\jobname" (lambda () (tex2page-string *jobname*)))

(tex-def-prim "\\k" (lambda () (do-diacritic ':ogonek)))
(tex-def-prim "\\kern" do-hskip)

(tex-defsym-prim "\\l" "&#x142;")
(tex-defsym-prim "\\L" "&#x141;")
(tex-def-prim "\\label" do-label)
(tex-def-prim "\\large" (lambda () (do-switch ':large)))
(tex-def-prim "\\Large" (lambda () (do-switch ':large-cap)))
(tex-def-prim "\\LARGE" (lambda () (do-switch ':large-up)))
(tex-def-prim "\\latexonly"
 (lambda () (ignore-tex-specific-text "latexonly")))
(tex-def-prim "\\lccode" (lambda () (do-tex-case-code ':lccode)))
(tex-def-prim "\\leftdisplays"
 (lambda () (set! *display-justification* "left")))
(tex-def-prim "\\leftline" (lambda () (do-function "\\leftline")))
(tex-def-prim "\\let" (lambda () (do-let (globally-p))))
(tex-def-prim "\\linebreak"
 (lambda () (get-bracketed-text-if-any) (emit "<br>")))
(tex-def-prim "\\listing" do-verbatiminput)
(tex-def-prim "\\llap" do-llap)
(tex-def-prim "\\lstlisting" (lambda () (do-verbatim-latex "lstlisting")))
(tex-def-prim "\\lowercase" (lambda () (do-flipcase ':lccode)))

(tex-def-prim "\\magnification" do-magnification)
(tex-def-prim "\\magstep" do-magstep)
(tex-def-prim-0arg "\\magstephalf" "1095")
(tex-def-prim "\\mailto" do-mailto)
(tex-def-prim "\\makeatletter" (lambda () (set-catcode #\@ 11)))
(tex-def-prim "\\makeatother" (lambda () (set-catcode #\@ 12)))
(tex-def-prim "\\makehtmlimage" do-makehtmlimage)
(tex-def-prim "\\makeindex" (lambda () (do-inputindex #f)))
(tex-def-prim "\\maketitle" do-maketitle)
(tex-def-prim "\\maketoc" do-toc)
(tex-def-prim "\\marginnote" do-marginnote)
(tex-def-prim "\\marginpar" do-marginpar)
(tex-def-prim "\\mathg" do-mathg)
(tex-def-prim "\\mathdg" do-mathdg)
(tex-def-prim "\\mathp" do-mathp)
(tex-def-prim "\\medbreak" (lambda () (do-bigskip ':medskip)))
(tex-def-prim "\\medskip" (lambda () (do-bigskip ':medskip)))
(tex-def-prim "\\message" do-message)
(tex-def-prim "\\mfpic" do-mfpic)
(tex-def-prim "\\minipage" do-minipage)
(tex-def-prim "\\multiply" (lambda () (do-multiply (globally-p))))

(tex-def-prim "\\narrower" (lambda () (do-switch ':narrower)))
(tex-def-prim "\\newcommand" (lambda () (do-newcommand #f)))
(tex-def-prim "\\newcount" (lambda () (do-newcount (globally-p))))
(tex-def-prim "\\newcounter" do-newcounter)
(tex-def-prim "\\newdimen" (lambda () (do-newdimen (globally-p))))
(tex-def-prim "\\newenvironment" (lambda () (do-newenvironment #f)))
(tex-def-prim "\\newif" do-newif)
(tex-def-prim "\\newread" (lambda () (do-new-stream ':in)))
(tex-def-prim "\\newtheorem" do-newtheorem)
(tex-def-prim "\\newtoks" (lambda () (do-newtoks (globally-p))))
(tex-def-prim "\\newwrite" (lambda () (do-new-stream ':out)))
(tex-def-prim "\\noad" (lambda () (tex-def-0arg "\\TZPcolophondisablecredit" "1"))) ;obsolete
(tex-def-prim "\\nocite" do-nocite)
(tex-def-prim "\\node" do-node)
(tex-def-prim "\\noindent" do-noindent)
(tex-def-prim "\\nonum" (lambda () (set! *opmac-nonum-p* #t)))
(tex-def-prim "\\nonumber" do-nonumber)
(tex-def-prim "\\noslatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "0")))
(tex-def-prim "\\notoc" (lambda () (set! *opmac-notoc-p* #t)))
(tex-def-prim "\\notimestamp" (lambda () (tex-def-0arg "\\TZPcolophondisabletimestamp" "1"))) ;obsolete
(tex-def-prim "\\nr" (lambda () (do-cr "\\nr")))
(tex-def-prim "\\number" do-number)
(tex-def-prim "\\numberedfootnote" do-numbered-footnote)

(tex-def-prim "\\@ldc@l@r" do-color)
(tex-defsym-prim "\\O" "&#xd8;")
(tex-defsym-prim "\\o" "&#xf8;")
(tex-def-prim "\\obeylines" do-obeylines)
(tex-def-prim "\\obeyspaces" do-obeyspaces)
(tex-def-prim "\\obeywhitespace" do-obeywhitespace)
(tex-defsym-prim "\\OE" "&#x152;")
(tex-defsym-prim "\\oe" "&#x153;")
(tex-def-prim "\\oldstyle" (lambda () (do-switch ':oldstyle)))
(tex-def-prim "\\opengraphsfile" do-mfpic-opengraphsfile)
(tex-def-prim "\\openin" (lambda () (do-open-stream ':in)))
(tex-def-prim "\\openout" (lambda () (do-open-stream ':out)))

(tex-def-prim "\\pagebreak"
 (lambda () (get-bracketed-text-if-any) (do-eject)))
(tex-def-prim "\\pagecolor" do-pagecolor)
(tex-def-prim "\\pageno" (lambda () (emit *html-page-count*)))
(tex-def-prim "\\pageref" do-pageref)
(tex-def-prim "\\paragraph" (lambda () (do-heading 4)))
(tex-def-prim "\\part" (lambda () (do-heading -1)))
(tex-def-prim "\\pdfximage" do-pdfximage)
(tex-def-prim "\\picture"
 (lambda () (do-latex-env-as-image "picture" #f)))
(tex-def-prim "\\plainfootnote" do-plain-footnote)
(tex-defsym-prim "\\pounds" "&#xa3;")
(tex-def-prim "\\printindex" (lambda () (do-inputindex #t)))
(tex-def-prim "\\proclaim" do-proclaim)
(tex-def-prim "\\providecommand" (lambda () (do-newcommand #f)))

(tex-def-prim "\\quote"
 (lambda () (do-end-para) (emit "<blockquote>") (bgroup)))

(tex-def-prim "\\r" (lambda () (do-diacritic ':ring)))
(tex-def-prim "\\raggedleft" (lambda () (do-switch ':raggedleft)))
(tex-def-prim "\\rawhtml" do-rawhtml)
(tex-def-prim "\\rcite" do-rcite)
(tex-def-prim "\\read" (lambda () (do-read (globally-p))))
(tex-def-prim "\\readtocfile" do-toc)
(tex-def-prim "\\ref" do-ref)
(tex-def-prim "\\refexternal" do-refexternal)
(tex-def-prim "\\refn" do-ref)
(tex-def-prim "\\relax" do-relax)
(tex-def-prim "\\renewcommand" (lambda () (do-newcommand #t)))
(tex-def-prim "\\renewenvironment" (lambda () (do-newenvironment #t)))
(tex-def-prim "\\resetatcatcode" (lambda () (set-catcode #\@ 12)))
(tex-def-prim "\\resizebox" do-resizebox)
(tex-def-prim "\\rightline" (lambda () (do-function "\\rightline")))
(tex-def-prim "\\rm" (lambda () (when *math-mode-p* (do-switch ':rm))))
(tex-def-prim "\\romannumeral" (lambda () (do-romannumeral #f)))
(tex-def-prim "\\Romannumeral" (lambda () (do-romannumeral #t)))
(tex-def-prim "\\ruledtable" do-ruledtable)

(tex-def-prim "\\sc" (lambda () (do-switch ':sc)))
(tex-def-prim "\\schemedisplay"
 (lambda () (do-scm-slatex-lines "schemedisplay" #t #f)))
(tex-def-prim "\\schemebox"
 (lambda () (do-scm-slatex-lines "schemebox" #f #f)))
(tex-def-prim "\\schemeresponse"
 (lambda () (do-scm-slatex-lines "schemeresponse" #t ':result)))
(tex-def-prim "\\schemeresponsebox"
 (lambda () (do-scm-slatex-lines "schemeresponsebox" #f ':result)))
(tex-def-prim "\\schemeresult" (lambda () (do-scm ':result)))
(tex-def-prim "\\scm" (lambda () (do-scm #f)))
(tex-def-prim "\\scmbuiltin" do-scm-set-builtins)
(tex-def-prim "\\scmdribble" do-scmdribble)
(tex-def-prim "\\scminput" do-scminput)
(tex-def-prim "\\scmkeyword" do-scm-set-keywords)
(tex-def-prim "\\scmspecialsymbol" do-scm-set-specialsymbol)
(tex-def-prim "\\scmvariable" do-scm-set-variables)
(tex-def-prim "\\scriptsize" (lambda () (do-switch ':scriptsize)))
(tex-def-prim "\\sec" do-opmac-sec)
(tex-def-prim "\\secc" (lambda () (do-opmac-heading 2)))
(tex-def-prim "\\section" (lambda () (do-heading 1)))
(tex-def-prim "\\seealso" do-see-also)
(tex-def-prim "\\setbox" do-setbox)
(tex-def-prim "\\setcmykcolor" (lambda () (do-switch ':cmyk)))
(tex-def-prim "\\setcounter" do-setcounter)
(tex-def-prim "\\sevenrm" (lambda () (do-switch ':sevenrm)))
(tex-def-prim "\\sf" (lambda () (do-switch ':sf)))
(tex-def-prim "\\sidx" do-index)
(tex-def-prim "\\sl" (lambda () (do-switch ':sl)))
(tex-def-prim "\\slatexdisable" get-group)
(tex-def-prim "\\slatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "1")))
(tex-def-prim "\\small" (lambda () (do-switch ':small)))
(tex-def-prim "\\smallbreak" (lambda () (do-bigskip ':smallskip)))
(tex-def-prim "\\smallskip" (lambda () (do-bigskip ':smallskip)))
(tex-defsym-prim "\\ss" "&#xdf;")
(tex-def-prim "\\stepcounter" do-stepcounter)
(tex-def-prim "\\strike" (lambda () (do-switch ':strike)))
(tex-def-prim "\\string" do-string)
(tex-def-prim "\\style" do-opmac-list-style)
(tex-def-prim "\\subject" do-subject)
(tex-def-prim "\\subparagraph" (lambda () (do-heading 5)))
(tex-def-prim "\\subsection" (lambda () (do-heading 2)))
(tex-def-prim "\\subsubsection" (lambda () (do-heading 3)))
(tex-def-prim "\\symfootnote" do-symfootnote)

(tex-def-prim "\\t" (lambda () (do-diacritic ':tieafter)))
(tex-def-prim "\\tabbing" do-tabbing)
(tex-def-prim "\\table" (lambda () (do-table/figure ':table)))
(tex-def-prim "\\tableplain" do-table-plain)
(tex-def-prim "\\tableofcontents" do-toc)
(tex-def-prim "\\tabular" (lambda () (do-tabular #f)))
(tex-def-prim "\\tag" do-tag)

(tex-def-prim "\\texonly" (lambda () (ignore-tex-specific-text "texonly")))

(tex-defsym-prim "\\textasciicircum" "^")
(tex-defsym-prim "\\textbar" "|")
(tex-defsym-prim "\\textbackslash" "\\")
(tex-def-prim "\\textbf" (lambda () (do-function "\\textbf")))
(tex-defsym-prim "\\textbullet" "&#x2022;")
(tex-defsym-prim "\\textcopyleft" "&#x254;&#x20dd;")
(tex-defsym-prim "\\textemdash" "&#x2014;")
(tex-defsym-prim "\\textendash" "&#x2013;")
(tex-defsym-prim "\\textexclamdown" "&#xa1;")
(tex-defsym-prim "\\textgreater" "&#x3e;")
(tex-def-prim "\\textindent" do-textindent)
(tex-def-prim "\\textit" (lambda () (do-function "\\textit")))
(tex-defsym-prim "\\textless" "&#x3c;")
(tex-defsym-prim "\\textperiodcentered" "&#xb7;")
(tex-defsym-prim "\\textquestiondown" "&#xbf;")
(tex-defsym-prim "\\textquotedblleft" "&#x201c;")
(tex-defsym-prim "\\textquotedblright" "&#x201d;")
(tex-defsym-prim "\\textquoteleft" "&#x2018;")
(tex-defsym-prim "\\textquoteright" "&#x2019;")
(tex-defsym-prim "\\textregistered" "&#xae;")
(tex-def-prim "\\textrm" (lambda () (do-function "\\textrm")))
(tex-def-prim "\\textsc"
 (lambda ()
     (fluid-let ((*in-small-caps-p* #t))
       (tex2page-string (get-group)))))
(tex-def-prim "\\textsl" (lambda () (do-function "\\textsl")))
(tex-defsym-prim "\\textasciitilde" "~")
(tex-def-prim "\\texttt" (lambda () (do-function "\\texttt")))
(tex-def-prim "\\textvisiblespace" (lambda () (emit *verbatim-visible-space*)))
(tex-defsym-prim "\\TH" "&#xde;")
(tex-defsym-prim "\\th" "&#xfe;")
(tex-def-prim "\\the" do-the)
(tex-def-prim "\\thebibliography" do-thebibliography)
(tex-def-prim "\\theindex" do-theindex)
(tex-def-prim "\\TIIPanchor" do-anchor-for-potential-label)
(tex-defsym-prim "\\TIIPbackslash" "\\")
(tex-def-prim "\\TIIPbr" do-br)
(tex-def-prim "\\TIIPcmyk" (lambda () (do-switch ':cmyk)))
(tex-def-prim "\\TIIPcsname" do-saved-csname)
(tex-def-prim "\\TIIPcomment" eat-till-eol)
(tex-def-prim "\\TIIPeatstar" eat-star)
(tex-def-prim "\\TIIPendgraf" do-end-para)
(tex-def-prim "\\TIIPpar" do-para-nopadding)
(tex-def-prim "\\TIIPfolio" point-to-adjacent-pages)
(tex-def-prim "\\TIIPgobblegroup" get-group)
(tex-def-prim "\\TIIPgray" (lambda () (do-switch ':gray)))
(tex-def-prim "\\TIIPhlend" do-hlend)
(tex-def-prim "\\TIIPlatexenvasimage" do-following-latex-env-as-image)
(tex-def-prim "\\TIIPnbsp" (lambda () (emit-nbsp 1)))
(tex-def-prim "\\TIIPnewline" do-newline)
(tex-def-prim "\\TIIPnull" get-actual-char)
(tex-def-prim "\\TIIPopmacitem" do-opmac-item)
(tex-def-prim "\\TIIPopmacverb" do-opmac-intext-verb)
(tex-def-prim "\\TIIPreuseimage" reuse-img)
(tex-def-prim "\\TIIPrgb" (lambda () (do-switch ':rgb)))
(tex-def-prim "\\TIIPRGB" (lambda () (do-switch ':rgb255)))
(tex-def-prim "\\TIIPtheorem" do-theorem)
(tex-def-prim "\\TIIPrelax" do-relax)
(tex-def-prim "\\TIIPauxdir" (lambda () (emit *aux-dir/*)))
(tex-def-prim "\\TIIPlastpageno" (lambda () (emit *last-page-number*)))
(tex-def-prim "\\tiny" (lambda () (do-switch ':tiny)))
(tex-def-prim "\\tit" do-opmac-title)
(tex-def-prim "\\title" do-title)
(tex-def-prim "\\today" do-today)
(tex-defsym-prim "\\TM" "&#x2122;")
(tex-def-prim "\\tracingall" do-tracingall)
(tex-def-prim "\\tt" (lambda () (do-switch ':tt)))
(tex-def-prim "\\typein" do-typein)

(tex-def-prim "\\uccode" (lambda () (do-tex-case-code ':uccode)))
(tex-def-prim "\\ulink" do-opmac-ulink)
(tex-def-prim "\\undefcsactive" do-undefcsactive)
(tex-def-prim "\\undefschememathescape" (lambda () (scm-set-mathescape #f)))
(tex-def-prim "\\underline" (lambda () (do-function "\\underline")))
(tex-def-prim "\\unscmspecialsymbol" do-scm-unset-specialsymbol)
(tex-def-prim "\\uppercase" (lambda () (do-flipcase ':uccode)))
(tex-def-prim "\\url" do-url)
(tex-def-prim "\\urlh" do-urlh)
(tex-def-prim "\\urlhd" do-urlhd)
(tex-def-prim "\\urlp" do-urlp)
(tex-def-prim "\\usebibtex" do-opmac-usebibtex)

(tex-def-prim "\\v" (lambda () (do-diacritic ':hacek)))
(tex-defsym-prim "\\vdots" "&#x22ee;")
(tex-def-prim "\\verb" do-verb)
(tex-def-prim "\\verbatim" do-verbatim)
(tex-def-prim "\\verbatiminput" do-verbatiminput)
(tex-def-prim "\\verbc" do-verbc)
(tex-def-prim "\\verbatimescapechar" do-verbatimescapechar)
(tex-def-prim "\\verbinput" do-opmac-verbinput)
(tex-def-prim "\\verbwrite" do-verbwrite)
(tex-def-prim "\\verbwritefile" do-verbwritefile)
(tex-def-prim "\\vfootnote" do-vfootnote)
(tex-def-prim "\\vskip" do-vskip)
(tex-def-prim "\\vspace" do-vspace)

(tex-def-prim "\\write" do-write)
(tex-def-prim "\\writenumberedcontentsline" do-writenumberedcontentsline)
(tex-def-prim "\\writenumberedtocline" do-writenumberedtocline)

(tex-def-prim "\\xdef" (lambda () (do-def #t #t)))
(tex-def-prim "\\XeTeXpdffile" do-xetexpdffile)
(tex-def-prim "\\XeTeXpicfile" do-xetexpicfile)
(tex-def-prim "\\xrdef" do-xrdef)
(tex-def-prim "\\xrefn" do-ref)
(tex-def-prim "\\xrtag" do-tag)
(tex-def-prim "\\xspace" do-xspace)

(tex-defsym-prim "\\yen" "&#xa5;")

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
(tex-def-prim "\\\"" (lambda () (do-diacritic ':umlaut)))
(tex-def-prim "\\`" (lambda () (do-diacritic ':grave)))
(tex-def-prim "\\(" do-latex-intext-math)
(tex-def-prim "\\[" do-latex-display-math)
(tex-def-prim "\\)" egroup)
(tex-def-prim "\\]" egroup)
(tex-defsym-prim "\\{" "{")
(tex-defsym-prim "\\}" "}")
(tex-let-prim "\\-" "\\TIIPrelax")
(tex-def-prim "\\'" (lambda () (do-diacritic ':acute)))
(tex-def-prim "\\="
              (lambda ()
                (unless (and (not (null? *tabular-stack*))
                             (eqv? (car *tabular-stack*) ':tabbing))
                  (do-diacritic ':macron))))
(tex-def-prim "\\>"
              (lambda ()
                (when (and (not (null? *tabular-stack*))
                           (eqv? (car *tabular-stack*) ':tabbing))
                  (emit-nbsp 3))))
(tex-def-prim "\\^" (lambda () (do-diacritic ':circumflex)))
(tex-def-prim "\\~" (lambda () (do-diacritic ':tilde)))
(tex-defsym-prim "\\#" "#")
(tex-def-prim "\\ " (lambda () (emit #\space)))
(tex-defsym-prim "\\%" "%")
(tex-defsym-prim "\\&" "&#x26;")
(tex-defsym-prim "\\@" "@")
(tex-defsym-prim "\\_" "_")
(tex-defsym-prim "\\$" "$")
(tex-def-prim (string #\\ #\Newline) emit-newline)

;TeX logos

(let* ((TeX *tex-logo*)
       (AmS (string-append "<span style=\"font-family: cursive;\">"
              "A"
              "<span style=\""
              "position: relative; "
              "top: 0.5ex; "
              "margin-left: -.1667em; "
              "margin-right: -.075em"
              "\">M</span>"
              "S</span>"))
       (Bib (string-append "B" "<span style=\""
              "text-transform: uppercase"
              "\"><small>ib</small></span>"))
       (ConTeXt (string-append "Con"
                  TeX
                  "t"))
       (LaTeX (string-append "L" "<span style=\""
                "position: relative; "
                "bottom: 0.3ex; "
                "margin-left: -0.36em; "
                "margin-right: -0.15em; "
                "text-transform: uppercase"
                "\"><small>a</small></span>"
                TeX))
       (Xe (string-append "X" "<span style=\""
              "text-transform: uppercase; "
              "position: relative; "
              "top: 0.5ex; "
              "margin-left: -0.125em; "
              "margin-right: -0.1667em"
              "\">&#x1dd;</span>"))
       (thinspace (kern ".16667em"))
       (_2e (string-append "<span style=\""
              "margin-left: .05em"
              "\">2<span>"
              "<span style=\""
              "position: relative; "
              "top: .5ex"
              "\">&#x3b5;</span>"))
       (MF (string-append "<span style=\""
             "font-family: sans-serif"
             "\">METAFONT</span>")))
  (tex-def-prim "\\AmSTeX" (lambda () (emit AmS) (emit #\-) (emit TeX)))
  (tex-def-prim "\\BibTeX" (lambda () (emit Bib) (emit TeX)))
  (tex-def-prim "\\ConTeXt" (lambda () (emit ConTeXt)))
  (tex-def-prim "\\eTeX" (lambda () (emit "&#x3b5;-") (emit TeX)))
  (tex-def-prim "\\LaTeX" (lambda () (emit LaTeX)))
  (tex-def-prim "\\LaTeXe" (lambda () (emit LaTeX) (emit _2e)))
  (tex-def-prim "\\MF" (lambda () (emit MF)))
  (tex-def-prim "\\TeX" (lambda () (emit TeX)))
  (tex-def-prim "\\XeLaTeX" (lambda () (emit Xe) (emit thinspace) (emit LaTeX)))
  (tex-def-prim "\\XeTeX" (lambda () (emit Xe) (emit TeX))))

;plain quicknums

(tex-let-prim "\\@ne" (string (integer->char 1)))
(tex-let-prim "\\tw@" (string (integer->char 2)))
(tex-let-prim "\\thr@@" (string (integer->char 3)))
(tex-let-prim "\\sixt@@n" (string (integer->char 16)))
(tex-let-prim "\\@cclv" (string (integer->char 255)))
(tex-let-prim "\\@cclvi" (string (integer->char 256)))
(tex-let-prim "\\@m" (string (integer->char 1000)))
(tex-let-prim "\\@M" (string (integer->char 10000)))
(tex-let-prim "\\@MM" (string (integer->char 20000)))

;ignoring these

(tex-let-prim "\\htmladvancedentities" "\\TIIPrelax")
(tex-let-prim "\\displaystyle" "\\TIIPrelax")
(tex-let-prim "\\textstyle" "\\TIIPrelax")
(tex-let-prim "\\endsloppypar" "\\TIIPrelax")
(tex-let-prim "\\frenchspacing" "\\TIIPrelax")
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
(tex-def-prim "\\cline" get-group)
(tex-def-prim "\\externalref" get-group)
(tex-def-prim "\\GOBBLEARG" get-group)
(tex-def-prim "\\hyphenation" get-group)
(tex-def-prim "\\newlength" get-group)
(tex-def-prim "\\hphantom" get-group)
(tex-def-prim "\\vphantom" get-group)
(tex-def-prim "\\phantom" get-group)
(tex-def-prim "\\pagenumbering" get-group)
(tex-def-prim "\\pagestyle" get-group)
(tex-def-prim "\\raisebox" get-group)
(tex-def-prim "\\thispagestyle" get-group)
(tex-def-prim "\\manpagesection" get-group)
(tex-def-prim "\\manpagedescription" get-group)
(tex-def-prim "\\lstset" get-group)
(tex-def-prim "\\externallabels" (lambda () (get-group) (get-group)))
(tex-let-prim "\\markboth" "\\externallabels")

(tex-def-prim "\\addtolength" (lambda () (get-token) (get-token)))
(tex-def-prim "\\columnsep" eat-dimen)
(tex-def-prim "\\columnseprule" eat-dimen)
(tex-def-prim "\\enlargethispage" (lambda () (eat-star) (get-group)))
(tex-def-prim "\\evensidemargin" eat-dimen)
(tex-def-prim "\\fboxsep" eat-dimen)
(tex-def-prim "\\headsep" eat-dimen)
(tex-def-prim "\\itemsep" eat-dimen)
(tex-def-prim "\\leftcodeskip" eat-dimen)
(tex-def-prim "\\leftmargin" eat-dimen)
(tex-def-prim "\\lower" eat-dimen)
(tex-def-prim "\\magstep" get-token)
(tex-def-prim "\\oddsidemargin" eat-dimen)
(tex-def-prim "\\pagewidth" eat-dimen)
(tex-def-prim "\\parbox" (lambda () (get-bracketed-text-if-any) (get-group)))
(tex-def-prim "\\parsep" eat-dimen)
(tex-def-prim "\\raise" eat-dimen)
(tex-def-prim "\\rightcodeskip" eat-dimen)
(tex-def-prim "\\scriptfont" get-token)
(tex-def-prim "\\scriptscriptfont" get-token)
(tex-def-prim "\\sidemargin" eat-dimen)
(tex-def-prim "\\spinemargin" eat-dimen)
(tex-def-prim "\\textfont" get-token)
(tex-def-prim "\\textheight" eat-dimen)
(tex-def-prim "\\topmargin" eat-dimen)
(tex-def-prim "\\topsep" eat-dimen)
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

(tex-def-prim "\\GOBBLEOPTARG" get-bracketed-text-if-any)
(tex-def-prim "\\nolinebreak" get-bracketed-text-if-any)
(tex-def-prim "\\nopagebreak" get-bracketed-text-if-any)

(tex-def-prim "\\hyphenchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\skewchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\usepackage"
 (lambda () (get-bracketed-text-if-any) (get-group) (probably-latex)))

(tex-def-prim "\\readindexfile" (lambda () (get-token) (do-inputindex #f)))

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
(tex-let-prim "\\/" "\\TIIPrelax")
(tex-let-prim "\\leavevmode" "\\TIIPrelax")
(tex-let-prim "\\space" "\\ ")
(tex-let-prim "\\quotation" "\\quote")
(tex-let-prim "\\endquotation" "\\endquote")
(tex-let-prim "\\TIIPdate" "\\today")
(tex-let-prim "\\schemeinput" "\\scminput")
(tex-let-prim "\\lispinput" "\\scminput")

;aliases

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
(tex-let-prim "\\begtt" "\\begintt")

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
(tex-let-prim "\\leqalignno" "\\eqalignno")
(tex-let-prim "\\nohtmlmathdisplayimg" "\\dontuseimgforhtmlmathdisplay")
(tex-let-prim "\\writetotoc" "\\writenumberedtocline") ;obsolete
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

;#endinclude primdefs.scm

(define tex2page
  (lambda (tex-file)
    (unless (= *write-log-index* 0)
      ;nested call to tex2page
      (newline))
    (when (or (not tex-file) (string=? tex-file ""))
      (set! tex-file "--missing-arg"))
    (fluid-let ((*afterassignment* #f)
                (*afterpar* '())
                (*afterbye* '())
                (*aux-dir* #f)
                (*aux-dir/* "")
                (*aux-port* #f)
                ;
                (*bib-aux-port* #f)
                (*bibitem-num* 0)
                ;
                (*color-names* '())
                (*comment-char* #\%)
                (*css-port* #f)
                (*current-tex2page-input* #f) ;will be set by tex2page-file
                (*current-source-file* #f) ; ''
                ;
                (*display-justification* "center")
                (*doctype* *doctype*)
                (*dotted-counters* #f)
                (*dumping-nontex-p* #f)
                ;
                (*equation-number* #f)
                (*equation-numbered-p* #t)
                (*equation-position* 0)
                (*esc-chars* (list #\\))
                (*esc-char-std* #\\ )
                (*esc-char-verb* #\|)
                (*eval-for-tex-only-p* #f)
                (*external-label-tables* (make-table 'equ string=?))
                ;
                (*footnote-list* '())
                (*footnote-sym* 0)
                ;
                (*global-texframe* #f)
                (*graphics-file-extensions* '(".eps"))
                ;
                (*html* (current-output-port))
                (*html-head* '())
                (*html-only* 0)
                (*html-page* #f)
                (*html-page-count* 0)
                ;
                (*img-file-count* 0)
                (*img-file-tally* 0)
                (*imgdef-file-count* 0)
                (*imgpreamble* "")
                (*imgpreamble-inferred* '())
                (*in-alltt-p* #f)
                (*in-display-math-p* #f)
                (*in-para-p* #f)
                (*in-small-caps-p* #f)
                (*includeonly-list* #t)
                (*index-table* (make-table))
                (*index-count* 0)
                (*index-page* #f)
                (*index-port* #f)
                (*infructuous-calls-to-tex2page* 0)
                (*input-line-no* 0) ;will be set by tex2page-file
                (*input-streams* (make-table))
                (*inputting-boilerplate-p* #f)
                (*inside-appendix-p* #f)
                ;
                (*jobname* "texput")
                ;
                (*label-port* #f)
                (*label-source* #f)
                (*label-table* (make-table 'equ string=?))
                (*last-modification-time* #f)
                (*last-page-number* -1)
                (*latex-probability* 0)
                (*ligatures-p* #t)
                (*loading-external-labels-p* #f)
                (*log-file* #f)
                (*log-port* #f)
                ;
                (*main-tex-file* #f)
                (*math-delim-left* #f)
                (*math-delim-right* #f)
                (*math-height* 0)
                (*math-mode-p* #f)
                (*mfpic-file-num* #f)
                (*mfpic-file-stem* #f)
                (*mfpic-port* #f)
                (*missing-eps-files* '())
                (*missing-pieces* '())
                (*mp-files* '())
                ;
                (*not-processing-p* #f)
                ;
                (*opmac-active-tt-char* #f)
                (*opmac-index-sub-table* #f)
                (*opmac-list-style* #\o)
                (*opmac-nonum-p* #f)
                (*opmac-notoc-p* #f)
                (*opmac-verbinput-table* (make-table 'equ string=?))
                (*outer-p* #t)
                (*output-streams* (make-table))
                (*outputting-external-title-p* #f)
                (*outputting-to-non-html-p* #f)
                ;
                (*package* *this-package*) ;place-holder for CL
                ;
                (*quote-level* 0)
                ;
                (*reading-control-sequence-p* #f)
                (*recent-node-name* #f)
                (*redirect-delay* #f)
                (*redirect-url* #f)
                ;
                (*scm-builtins* #f)
                (*scm-dribbling-p* #f)
                (*scm-keywords* #f)
                (*scm-special-symbols* #f)
                (*scm-variables* #f)
                (*scripts* '())
                (*section-counter-dependencies* #f)
                (*section-counters* (make-table))
                ;(*slatex-like-comments-p* #f)
                (*slatex-math-escape* #f)
                (*source-changed-since-last-run-p* #f)
                (*stylesheets* '())
                (*subjobname* #f)
                ;
                (*tabular-stack* '())
                (*temp-string-count* 0)
                (*temporarily-use-utf8-for-math-p* #f)
                (*tex2page-inputs* (string=split (or (getenv "TEX2PAGEINPUTS")
                                                     (getenv "TIIPINPUTS")) *path-separator*))
                (*tex-env* '())
                (*tex-format* ':plain)
                (*tex-if-stack* '())
                (*tex-like-layout-p* *tex-like-layout-p*)
                (*title* #f)
                (*toc-list* '())
                (*toc-page* #f)
                ;
                (*unresolved-xrefs* '())
                (*using-bibliography-p* #f)
                (*using-chapters-p* #f)
                (*using-index-p* #f)
                ;
                (*verb-display-p* #f)
                (*verb-port* #f)
                (*verb-visible-space-p* #f)
                (*verb-written-files* '())
                ;
                (*write-log-index* 0)
                (*write-log-possible-break-p* #f))
      ;
      (initialize-globals)
      (set! *main-tex-file* (actual-tex-filename
                              tex-file (check-input-file-timestamp? tex-file)))
      (write-log "This is TeX2page, Version ")
      (write-log *tex2page-version*)
      (write-log #\space) (write-log #\()
      (write-log *scheme-version*)
      (write-log #\,) (write-log #\space)
      (write-log *operating-system*)
      (write-log #\))
      (write-log #\space)
      (write-log (seconds-to-human-time (current-seconds)))
      (write-log ':separation-newline)
      (cond (*main-tex-file*
             (set! *subjobname* *jobname*)
             (set! *html-page* (string-append *aux-dir/* *jobname*
                                              *output-extension*))
             (ensure-file-deleted *html-page*)
             (set! *html* (open-output-file *html-page*))
             (do-start)
             (fluid-let ((*html-only* (+ *html-only* 1)))
               (tex2page-file-if-exists (file-in-home ".tex2page.t2p"))
               (tex2page-file-if-exists ".tex2page.t2p")
               (cond ((actual-tex-filename
                        (string-append *jobname* ".t2p") #f)
                      => tex2page-file)))
             (unless (eqv? (tex2page-file *main-tex-file*)
                           ':encountered-bye)
               (insert-missing-end))
             (do-bye))
            (else (tex2page-help tex-file)))
      (output-stats))))

(define main
  (lambda args
    (tex2page
     (and (>= (length args) 1)
          (list-ref args 0))))) 