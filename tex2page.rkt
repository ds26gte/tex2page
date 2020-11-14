":";exec racket -f $0 -- "$@"
  
(require mzlib/process)

(require racket/private/more-scheme)

(define *operating-system*
 (if (getenv "COMSPEC")
     (let ((term (getenv "TERM")))
       (if (and (string? term) (string=? term "cygwin")) ':cygwin ':windows))
     ':unix))

(define *scheme-version*
 (string-append "Racket " (version) " " (symbol->string *operating-system*)))

(define *path-separator* (if (eqv? *operating-system* ':windows) #\; #\:))

(define *directory-separator* (if (eqv? *operating-system* ':windows) "\\" "/"))

(define *package* false)

(define eval1 eval)

(define (decode-universal-time s)
 (let ((ht (and s (seconds->date s))))
   (cond
    (ht
     (list false
           (date-minute ht)
           (date-hour ht)
           (date-day ht)
           (date-month ht)
           (date-year ht)))
    (else (list false false false false false false false false false)))))

(define (strftime ignore-format d)
 (let ((m (date-minute d))
       (h (date-hour d))
       (dy (date-day d))
       (mo (date-month d))
       (y (date-year d))
       (dow (date-week-day d))
       (dst (date-dst? d))
       (tzsec (date-time-zone-offset d)))
   (let ((tz (and tzsec (/ tzsec 3600))))
     (string-append (vector-ref *week-day-names* dow) ", "
      (vector-ref *short-month-names* (- mo 1)) " " (number->string dy) ", "
      (number->string y)
      (if tz
          (string-append ", "
           (let ((h (modulo h 12)))
             (number->string (if (= h 0) 12 h)))
           ":" (if (< m 10) "0" "") (number->string m) " "
           (if (<= 0 h 11) "a" "p") "m UTC" (if (> tz 0) "+" "−")
           (number->string (abs tz)))
          "")))))

(define (seconds-to-human-time s)
 (strftime "%a, %b %e, %Y, %l:%M %p %Z" (seconds->date s)))

(define list-position
 (lambda (x s)
   (let loop
     ((s s) (i 0))
     (cond ((null? s) false)
           ((eq? (car s) x) i)
           (else (loop (cdr s) (+ i 1)))))))

(define-syntax defstruct
 (lambda (%so)
   (datum->syntax %so
    (let ((%so-d (syntax->datum %so)))
      (apply
       (lambda (s . ff)
         (let ((s-s (symbol->string s)) (n (length ff)))
           (let* ((n+1 (+ n 1)) (vv (make-vector n+1)))
             (let loop
               ((i 1) (ff ff))
               (if (< i n+1)
                   (let ((f (car ff)))
                     (vector-set! vv i (if (pair? f) (cadr f) (not 't)))
                     (loop (+ i 1) (cdr ff)))
                   0))
             (let* ((ff-without-colons
                     (map
                      (lambda (f) (symbol->string (if (pair? f) (car f) f)))
                      ff))
                    (ff-with-colons
                     (map (lambda (f) (string->symbol (string-append ":" f)))
                          ff-without-colons)))
               (quasiquote
                (begin
                 (define (unquote (string->symbol (string-append "make-" s-s)))
                  (lambda fvfv
                    (let ((st (make-vector (unquote n+1)))
                          (ff '(unquote ff-with-colons)))
                      (vector-set! st 0 '(unquote s))
                      (unquote-splicing
                       (let loop
                         ((i 1) (r '()))
                         (if (>= i n+1)
                             r
                             (loop (+ i 1)
                                   (cons
                                    (quasiquote
                                     (vector-set! st (unquote i)
                                      (unquote (vector-ref vv i))))
                                    r)))))
                      (let loop
                        ((fvfv fvfv))
                        (if (null? fvfv)
                            0
                            (begin
                             (vector-set! st
                              (+ (list-position (car fvfv) ff) 1) (cadr fvfv))
                             (loop (cddr fvfv)))))
                      st)))
                 (unquote-splicing
                  (let loop
                    ((i 1) (procs '()))
                    (if (>= i n+1)
                        procs
                        (loop (+ i 1)
                              (let* ((f-s (list-ref ff-without-colons (- i 1))))
                                (cons
                                 (quasiquote
                                  (define
                                   (unquote
                                    (string->symbol
                                     (string-append s-s "-" f-s)))
                                   (lambda (x) (vector-ref x (unquote i)))))
                                 (cons
                                  (quasiquote
                                   (define
                                    (unquote
                                     (string->symbol
                                      (string-append "set!" s-s "-" f-s)))
                                    (lambda (x v)
                                      (vector-set! x (unquote i) v))))
                                  procs)))))))
                 (define (unquote (string->symbol (string-append s-s "?")))
                  (lambda (x)
                    (and (vector? x)
                         (eq? (vector-ref x 0) '(unquote s)))))))))))
       (cdr %so-d))))))

(define-syntax cl-with-output-to-string
 (lambda (%so)
   (datum->syntax %so
    (let ((%so-d (syntax->datum %so)))
      (apply
       (lambda (ignore-wots-arg . body)
         (list 'with-output-to-string (list* 'lambda '() body)))
       (cdr %so-d))))))

(defstruct table (test eqv?) (alist '()))

(define (table-get k tbl . d)
 (cond
  ((lassoc k (table-alist tbl) (table-test tbl)) =>
   (lambda (c) (vector-ref (cdr c) 0)))
  ((pair? d) (car d))
  (else false)))

(define (table-rem k tbl) (table-put! k tbl false))

(define (table-put! k tbl v)
 (let ((al (table-alist tbl)))
   (let ((c (lassoc k al (table-test tbl))))
     (if c
         (vector-set! (cdr c) 0 v)
         (set!table-alist tbl (cons (cons k (vector v)) al))))))

(define (table-for-each p tbl)
 (for-each (lambda (c) (p (car c) (vector-ref (cdr c) 0))) (table-alist tbl)))

(define (substring? s1 s2)
 (let* ((s1-len (string-length s1))
        (s2-len (string-length s2))
        (n-give-up (+ 1 (- s2-len s1-len))))
   (let loop
     ((i 0))
     (if (< i n-give-up)
         (let loop2
           ((j 0) (k i))
           (if (< j s1-len)
               (if (char=? (string-ref s1 j) (string-ref s2 k))
                   (loop2 (+ j 1) (+ k 1))
                   (loop (+ i 1)))
               i))
         false))))

(define (lassoc k al equ?)
 (let loop
   ((al al))
   (if (null? al)
       false
       (let ((c (car al)))
         (if (equ? (car c) k) c (loop (cdr al)))))))

(define-syntax rassoc
 (lambda (%so)
   (datum->syntax %so
    (let ((%so-d (syntax->datum %so)))
      (apply
       (lambda (k al . z)
         (quasiquote
          (scheme-rassoc (unquote k) (unquote al)
           (unquote (if (null? z) 'eqv? (cadr z))))))
       (cdr %so-d))))))

(define (scheme-rassoc k al equ)
 (let loop
   ((al al))
   (if (null? al)
       false
       (let ((c (car al)))
         (if (equ (cdr c) k) c (loop (cdr al)))))))

(define (write-to-string n . z)
 (if (pair? z)
     (number->string (if (inexact? n) (inexact->exact n) n) 16)
     (number->string n)))

(define (number-to-roman n . upcase?)
 (set! upcase? (and (pair? upcase?) (car upcase?)))
 (unless (and (integer? n) (>= n 0))
   (terror 'number-to-roman "Missing number"))
 (let ((roman-digits
        '((1000 #\m 100) (500 #\d 100) (100 #\c 10) (50 #\l 10) (10 #\x 1)
          (5 #\v 1) (1 #\i 0)))
       (approp-case (lambda (c) (if upcase? (char-upcase c) c))))
   (let loop
     ((n n) (dd roman-digits) (s '()))
     (if (null? dd)
         (if (null? s) "0" (list->string (reverse s)))
         (let* ((d (car dd))
                (val (car d))
                (char (approp-case (cadr d)))
                (nextval (caddr d)))
           (let loop2
             ((q (quotient n val)) (r (remainder n val)) (s s))
             (if (= q 0)
                 (if (>= r (- val nextval))
                     (loop (remainder r nextval)
                           (cdr dd)
                           (cons char
                                 (cons (approp-case (cadr (assv nextval dd)))
                                       s)))
                     (loop r (cdr dd) s))
                 (loop2 (- q 1) r (cons char s)))))))))

(define (string-index s c)
 (let ((n (string-length s)))
   (let loop
     ((i 0))
     (cond ((>= i n) false)
           ((char=? (string-ref s i) c) i)
           (else (loop (+ i 1)))))))

(define (string-reverse-index s c)
 (let loop
   ((i (- (string-length s) 1)))
   (cond ((< i 0) false)
         ((char=? (string-ref s i) c) i)
         (else (loop (- i 1))))))

(define (read-6hex i)
 (let* ((x (read i))
        (htmlcolor
         (string-upcase
          (cond ((symbol? x) (symbol->string x))
                ((number? x) (number->string x))
                (else (terror 'atom-to-6hex "Misformed argument."))))))
   (string-append "#"
    (case (string-length htmlcolor)
      ((1) "00000")
      ((2) "0000")
      ((3) "000")
      ((4) "00")
      ((5) "0")
      (else ""))
    htmlcolor)))

;Translated from Common Lisp source tex2page.lisp by CLiiScm v. 20200201, ecl.


(define *tex2page-version* "20201114")

(define *tex2page-website* "http://ds26gte.github.io/tex2page/index.html")

(define *tex2page-copyright-notice*
 (string-append "Copyright (C) 1997-" (substring *tex2page-version* 0 4)
  " Dorai Sitaram"))

(define (string=split p sepc)
 (if (not p)
     null
     (let ((p p) (r null))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (let ((i
                  (let ((%position-v sepc) (%position-s p))
                    (cond
                     ((string? %position-s)
                      (string-index %position-s %position-v))
                     (else (list-position %position-v %position-s))))))
             (cond ((not i) (set! r (cons p r)) (return (reverse r)))
                   (else false))
             (set! r (cons (substring p 0 i) r))
             (set! p (substring p (add1 i)))
             p)
           (if %loop-returned %loop-result (%loop)))))))

(define *month-names*
 (vector "January"
         "February"
         "March"
         "April"
         "May"
         "June"
         "July"
         "August"
         "September"
         "October"
         "November"
         "December"))

(define *short-month-names*
 (vector "Jan"
         "Feb"
         "March"
         "April"
         "May"
         "June"
         "July"
         "Aug"
         "Sept"
         "Oct"
         "Nov"
         "Dec"))

(define *week-day-names* (vector "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))

(define *enable-write-18-p* true)

(define *output-extension* ".html")

(define *ghostscript* "gs")

(define *use-closing-p-tag-p* true)

(define *metapost* "mpost")

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

(define *doctype* "html")

(define *int-corresp-to-0* (char->integer #\0))

(define *verbatim-visible-space* "<span style=\"color: red\">&#xb7;</span>")

(define *aux-file-suffix* "-Z-A.lisp")

(define *bib-aux-file-suffix* "-Z-B")

(define *css-file-suffix* "-Z-S.css")

(define *eval4tex-file-suffix* "-Z-E.lisp")

(define *html-node-prefix* "TAG:__tex2page_")

(define *html-page-suffix* "-Z-H-")

(define *img-file-suffix* "-Z-G-")

(define *imgdef-file-suffix* "D-")

(define *index-file-suffix* "-Z-I")

(define *label-file-suffix* "-Z-L.lisp")

(define *mfpic-tex-file-suffix* ".Z-M-tex")

(define *toc-file-suffix* "-Z-C")

(define *ghostscript-options*
 " -q -dBATCH -dNOPAUSE -dNO_PAUSE -sDEVICE=ppmraw")

(define *invisible-space* (integer->char 0))

(define *if-aware-ctl-seqs*
 '("\\csname" "\\else" "\\end" "\\eval" "\\fi" "\\let"))

(define *tex-logo*
 (string-append "T" "<span style=\"" "position: relative; " "top: 0.5ex; "
  "margin-left: -0.1667em; " "margin-right: -0.125em; "
  "text-transform: uppercase" "\">e</span>" "X"))

(define *tex-files-to-ignore*
 '("btxmac" "eplain" "epsf" "lmfonts" "mfpic" "supp-pdf"))

(define **escape** 0)

(define **bgroup** 1)

(define **egroup** 2)

(define **math** 3)

(define **alignment** 4)

(define **parameter** 6)

(define **ignore** 9)

(define **space** 10)

(define **letter** 11)

(define **other** 12)

(define **active** 13)

(define **comment** 14)

(define *catcodes*
 (list (cons #\\ **escape**)
       (cons #\  **space**)
       (cons #\% **comment**)
       (cons (integer->char 0) **ignore**)
       (cons #\return 5)
       (cons #\newline 5)
       (cons #\{ **bgroup**)
       (cons #\} **egroup**)
       (cons #\$ **math**)
       (cons #\& **alignment**)
       (cons #\# **parameter**)
       (cons #\^ 7)
       (cons #\_ 8)
       (cons #\tab **space**)
       (cons #\~ **active**)))

(define *afterassignment* false)

(define *afterpar* null)

(define *afterbye* null)

(define *aux-dir* false)

(define *aux-dir/* "")

(define *aux-stream* false)

(define *basic-style* "")

(define *bib-aux-stream* false)

(define *bibitem-num* 0)

(define *color-names* null)

(define *css-stream* false)

(define *current-source-file* false)

(define *current-tex2page-input* false)

(define *display-justification* false)

(define *dotted-counters* false)

(define *dumping-nontex-p* false)

(define *emit-enabled-p* true)

(define *equation-number* false)

(define *equation-numbered-p* true)

(define *equation-position* 0)

(define *esc-char-verb* #\|)

(define *eval-for-tex-only-p* false)

(define *expand-escape-p* false)

(define *external-label-tables* false)

(define *filename-delims* null)

(define *footnote-list* null)

(define *footnote-sym* 0)

(define *global-texframe* false)

(define *graphics-file-extensions* null)

(define *html* false)

(define *html-head* false)

(define *html-only* 0)

(define *html-page* false)

(define *html-page-count* false)

(define *ignore-timestamp-p* false)

(define *img-file-count* 0)

(define *img-file-tally* 0)

(define *imgdef-file-count* 0)

(define *imgpreamble* false)

(define *imgpreamble-inferred* false)

(define *in-alltt-p* false)

(define *in-display-math-p* false)

(define *in-para-p* false)

(define *in-small-caps-p* false)

(define *includeonly-list* false)

(define *index-count* false)

(define *index-page* false)

(define *index-page-mention-alist* false)

(define *index-stream* false)

(define *index-table* false)

(define *infructuous-calls-to-tex2page* false)

(define *input-line-no* 0)

(define *input-streams* false)

(define *inputting-boilerplate-p* false)

(define *inside-appendix-p* false)

(define *inside-eplain-verbatim-p* false)

(define *it* false)

(define *jobname* false)

(define *label-stream* false)

(define *label-source* false)

(define *label-table* false)

(define *last-modification-time* false)

(define *last-page-number* false)

(define *latex-probability* false)

(define *ligatures-p* false)

(define *loading-external-labels-p* false)

(define *log-file* false)

(define *log-stream* false)

(define *main-tex-file* false)

(define *math-delim-left* false)

(define *math-delim-right* false)

(define *math-height* false)

(define *math-mode-p* false)

(define *math-needs-image-p* false)

(define *math-font* false)

(define *math-script-mode-p* false)

(define *mfpic-file-num* false)

(define *mfpic-file-stem* false)

(define *mfpic-stream* false)

(define *missing-eps-files* false)

(define *missing-pieces* false)

(define *mp-files* false)

(define *not-processing-p* false)

(define *opmac-active-tt-char* false)

(define *opmac-index-sub-table* false)

(define *opmac-list-style* #\o)

(define *opmac-nonum-p* false)

(define *opmac-notoc-p* false)

(define *opmac-verbinput-table* false)

(define *outer-p* true)

(define *output-streams* false)

(define *outputting-external-title-p* false)

(define *outputting-to-non-html-p* false)

(define *quote-level* 0)

(define *reading-control-sequence-p* false)

(define *recent-node-name* false)

(define *remember-index-number* false)

(define *redirect-delay* false)

(define *redirect-url* false)

(define *reg-num-count* 0)

(define *scm-builtins* false)

(define *scm-dribbling-p* false)

(define *scm-special-symbols* false)

(define *scm-keywords* false)

(define *scm-variables* false)

(define *scripts* false)

(define *section-counters* false)

(define *section-counter-dependencies* false)

(define *slatex-math-escape* false)

(define *source-changed-since-last-run-p* false)

(define *start-time* 0)

(define *stylesheets* false)

(define *subjobname* *jobname*)

(define *tabular-stack* null)

(define *temp-string-count* 0)

(define *temporarily-use-utf8-for-math-p* false)

(define *tex-env* null)

(define *tex-format* false)

(define *tex-if-stack* null)

(define *tex-output-format* false)

(define *tex2page-inputs* null)

(define *this-package* *package*)

(define *title* false)

(define *toc-list* false)

(define *toc-page* false)

(define *unresolved-xrefs* false)

(define *using-bibliography-p* false)

(define *using-chapters-p* false)

(define *using-index-p* false)

(define *verb-display-p* false)

(define *verb-stream* false)

(define *verb-visible-space-p* false)

(define *verb-written-files* null)

(define *write-log-max* 55)

(define *write-log-index* 0)

(define *write-log-possible-break-p* false)

(define *scm-token-delims*
 (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

(defstruct counter* (value 0) (within false))

(defstruct tocentry* level number page label header)

(defstruct istream* (stream false) (buffer null))

(defstruct ostream* (stream false) (hbuffer null))

(defstruct texframe*
  (definitions (make-table ':test equal?))
  (cdefinitions (make-table))
  (chardefs (make-table ':test equal?))
  (countdefs (make-table ':test equal?))
  (counts (make-table))
  (dimendefs (make-table ':test equal?))
  (dimens (make-table))
  (toksdefs (make-table ':test equal?))
  (tokses (make-table))
  (boxes (make-table ':test equal?))
  (istreams (make-table))
  (ostreams (make-table))
  (postludes null)
  (uccodes (make-table))
  (lccodes (make-table))
  (catcodes null)
  (aftergroups null))

(defstruct tdef*
  (argpat null)
  (expansion "")
  (optarg false)
  (thunk false)
  (prim false)
  (defer false)
  (catcodes false))

(defstruct cdef*
  (argpat false)
  (expansion false)
  (optarg false)
  (active false)
  (catcodes false))

(defstruct footnotev* mark text tag caller)

(defstruct label* (src false) page name value)

(define (gen-temp-string) (set! *temp-string-count* (+ *temp-string-count* 1))
 (string-append "Temp_" (write-to-string *temp-string-count*)))

(define (file-stem-name f)
 (let ((slash
        (let ((%position-v #\/)
              (%position-s f)
              (%ee (list ':test char=? ':from-end true)))
          (cond
           ((string? %position-s)
            (string-reverse-index %position-s %position-v))
           (else (list-position %position-v %position-s))))))
   (cond (slash (set! f (substring f (add1 slash))) f) (else false))
   (let ((dot
          (let ((%position-v #\.)
                (%position-s f)
                (%ee (list ':test char=? ':from-end true)))
            (cond
             ((string? %position-s)
              (string-reverse-index %position-s %position-v))
             (else (list-position %position-v %position-s))))))
     (if dot (substring f 0 dot) f))))

(define (file-extension f)
 (let ((slash
        (let ((%position-v #\/)
              (%position-s f)
              (%ee (list ':test char=? ':from-end true)))
          (cond
           ((string? %position-s)
            (string-reverse-index %position-s %position-v))
           (else (list-position %position-v %position-s)))))
       (dot
        (let ((%position-v #\.)
              (%position-s f)
              (%ee (list ':test char=? ':from-end true)))
          (cond
           ((string? %position-s)
            (string-reverse-index %position-s %position-v))
           (else (list-position %position-v %position-s))))))
   (if (and dot (not (= dot 0)) (or (not slash) (< (add1 slash) dot)))
       (substring f dot)
       false)))

(define (ensure-file-deleted f)
 (cond ((file-exists? f) (delete-file f)) (else false)))

(define (write-aux e)
 (cond
  ((not *aux-stream*)
   (let ((f (string-append *aux-dir/* *jobname* *aux-file-suffix*)))
     (set! *aux-stream*
      (let* ((%f f)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *aux-stream*))
  (else false))
 (write e *aux-stream*) (newline *aux-stream*))

(define (write-label e)
 (cond
  ((not *label-stream*)
   (let ((f (string-append *aux-dir/* *jobname* *label-file-suffix*)))
     (set! *label-stream*
      (let* ((%f f)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *label-stream*))
  (else false))
 (write e *label-stream*) (newline *label-stream*))

(define (write-bib-aux x)
 (cond
  ((not *bib-aux-stream*)
   (let ((f (string-append *aux-dir/* *jobname* *bib-aux-file-suffix* ".aux")))
     (set! *bib-aux-stream*
      (let* ((%f f)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *bib-aux-stream*))
  (else false))
 (display x *bib-aux-stream*))

(define (write-log x . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (log-file-only-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! log-file-only-p (list-ref %lambda-rest-arg 0)))
   (cond
    ((not *log-stream*)
     (set! *log-file* (string-append *aux-dir/* *jobname* ".hlog"))
     (set! *log-stream*
      (let* ((%f *log-file*)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *log-stream*)
    (else false))
   (cond
    ((and *write-log-possible-break-p* (char? x) (member x '(#\) #\] #\} #\,)))
     (set! *write-log-possible-break-p* false) *write-log-possible-break-p*)
    (else false))
   (cond
    ((and *write-log-possible-break-p* (> *write-log-index* *write-log-max*))
     (newline *log-stream*)
     (cond ((not log-file-only-p) (newline)) (else false))
     (set! *write-log-possible-break-p* false) (set! *write-log-index* 0)
     *write-log-index*)
    (else false))
   (cond
    ((not
      (and (= *write-log-index* 0)
           (member x '(:separation-newline :separation-space))))
     (case x
       ((#\newline :separation-newline)
        (cond
         (*write-log-possible-break-p*
          (set! *write-log-possible-break-p* false)
          *write-log-possible-break-p*)
         (else false))
        (newline *log-stream*)
        (cond ((not log-file-only-p) (newline)) (else false))
        (set! *write-log-index* 0)
        *write-log-index*)
       ((:separation-space)
        (set! *write-log-possible-break-p* true)
        *write-log-possible-break-p*)
       (else
        (cond
         (*write-log-possible-break-p* (write-char #\  *log-stream*)
          (cond ((not log-file-only-p) (write-char #\ )) (else false))
          (set! *write-log-index* (add1 *write-log-index*))
          (set! *write-log-possible-break-p* false)
          *write-log-possible-break-p*)
         (else false))
        (display x *log-stream*)
        (cond ((not log-file-only-p) (display x) (flush-output)) (else false))
        (set! *write-log-index*
         (+ *write-log-index*
            (let ((%tag x))
              (cond ((char? %tag) 1)
                    ((number? %tag) (string-length (write-to-string x)))
                    ((string? %tag) (string-length x))
                    (else 1)))))
        *write-log-index*)))
    (else false))))

(define (display-error-context-lines)
 (let ((n (or (find-count "\\errorcontextlines") 0)))
   (cond
    ((and *current-source-file* (> n 0))
     (let ((n1 (max 0 (- *input-line-no* (quotient (sub1 n) 2)))))
       (let ((nf (+ n1 n -1)))
         (let ((ll
                (let ((ip (open-input-file *current-source-file*)))
                  (let ((%with-open-file-res
                         (let ((i 1) (ll null))
                           (let* ((%loop-returned false)
                                  (%loop-result 0)
                                  (return
                                   (lambda %args
                                     (set! %loop-returned true)
                                     (set! %loop-result
                                      (and (pair? %args) (car %args))))))
                             (let %loop
                               ()
                               (let ((l
                                      (let ((%read-line-res (read-line ip)))
                                        (when (eof-object? %read-line-res)
                                          (set! %read-line-res false))
                                        %read-line-res)))
                                 (cond ((not l) (return ll))
                                       ((< i n1) (set! i (+ i 1)) i)
                                       ((<= i nf) (set! i (+ i 1))
                                        (set! ll (cons (cons i l) ll)) ll)
                                       (else (return ll))))
                               (if %loop-returned %loop-result (%loop)))))))
                    (cond
                     (ip
                      ((if (input-port? ip) close-input-port close-output-port)
                       ip))
                     (else false))
                    %with-open-file-res))))
           (cond
            ((not (null? ll))
             (let ((border "__________________________..."))
               (let ((only-1-p (= (length ll) 1)))
                 (let ((nf (caar ll)))
                   (let ((ll (reverse ll)))
                     (let ((n1 (caar ll)))
                       (write-log "Likely error context: ")
                       (write-log *current-source-file*)
                       (write-log ", line")
                       (cond ((not only-1-p) (write-log "s")) (else false))
                       (write-log " ")
                       (write-log n1)
                       (cond ((not only-1-p) (write-log "-") (write-log nf))
                             (else false))
                       (write-log ":")
                       (write-log #\newline)
                       (write-log " /")
                       (write-log border)
                       (write-log #\newline)
                       (for-each
                        (lambda (l)
                          (write-log " | ")
                          (write-log (cdr l))
                          (write-log #\newline))
                        ll)
                       (write-log " |")
                       (write-log border)
                       (write-log #\newline)
                       (write-log "/")))))))
            (else false))))))
    (else false))))

(define (edit-offending-file)
 (let ((calling-from-text-editor-p (getenv "VIMRUNTIME")))
   (cond
    ((not calling-from-text-editor-p)
     (display "Type E to edit your file, X to quit.") (newline) (display "? ")
     (flush-output)
     (let ((c
            (let* ((%read-char-port false)
                   (%read-char-res
                    (if %read-char-port
                        (read-char %read-char-port)
                        (read-char))))
              (when (eof-object? %read-char-res) (set! %read-char-res false))
              %read-char-res)))
       (cond
        ((and c (char-ci=? c #\e))
         (let ((texedit-string (getenv "TEXEDIT")) (ill-formed-texedit-p false))
           (cond
            (texedit-string
             (cond
              ((begin (set! *it* (substring? "%d" texedit-string)) *it*)
               (let ((i *it*))
                 (set! texedit-string
                  (string-append (substring texedit-string 0 i)
                   (write-to-string *input-line-no*)
                   (substring texedit-string (+ i 2))))
                 texedit-string))
              (else (set! ill-formed-texedit-p true)
               (set! texedit-string false) texedit-string)))
            (else false))
           (cond
            (texedit-string
             (cond
              ((begin (set! *it* (substring? "%s" texedit-string)) *it*)
               (let ((i *it*))
                 (set! texedit-string
                  (string-append (substring texedit-string 0 i)
                   *current-source-file* (substring texedit-string (+ i 2))))
                 texedit-string))
              (else (set! ill-formed-texedit-p true)
               (set! texedit-string false) texedit-string)))
            (else false))
           (cond
            ((not texedit-string)
             (cond
              (ill-formed-texedit-p
               (display "Ill-formed TEXEDIT; using EDITOR.") (newline))
              (else false))
             (cond
              ((begin (set! *it* (or (getenv "EDITOR") "vi")) *it*)
               (let ((e *it*))
                 (set! texedit-string
                  (string-append e " +" (write-to-string *input-line-no*) " "
                   *current-source-file*))
                 texedit-string))
              (else false)))
            (else false))
           (cond (texedit-string (system texedit-string)) (else false))))
        (else false))))
    (else false))))

(define (trace-if write-p . args)
 (cond
  (write-p (write-log ':separation-newline)
   (cond
    ((> *input-line-no* 0) (write-log "l.") (write-log *input-line-no*)
     (write-log #\ ))
    (else false))
   (for-each write-log args) (write-log ':separation-newline))
  (else false)))

(define (terror where . args) (write-log ':separation-newline) (write-log "! ")
 (for-each write-log args) (write-log ':separation-newline)
 (write-log *current-source-file*) (write-log #\:) (write-log *input-line-no*)
 (write-log ": error") (write-log ':separation-newline) (write-log "l.")
 (write-log *input-line-no*) (write-log #\ ) (write-log where)
 (write-log " failed.") (write-log ':separation-newline)
 (display-error-context-lines) (close-all-open-streams) (output-stats)
 (edit-offending-file) (error "TeX2page fatal error"))

(define (do-errmessage) (write-log ':separation-newline) (write-log "! ")
 (write-log (tex-string-to-html-string (get-group)))
 (write-log ':separation-newline) (terror 'do-errmessage))

(define (do-tracingall) (plain-count "\\tracingcommands" 1 false)
 (plain-count "\\tracingmacros" 1 false))

(define (call-with-input-file/buffered f th)
 (cond
  ((not (file-exists? f))
   (terror 'call-with-input-file/buffered "I can't find file " f))
  (else false))
 (let ((i (open-input-file f)))
   (let ((%with-open-file-res
          (fluid-let
           ((*current-tex2page-input* (make-istream* ':stream i))
            (*current-source-file* f) (*input-line-no* 1))
           (th))))
     (cond (i ((if (input-port? i) close-input-port close-output-port) i))
           (else false))
     %with-open-file-res)))

(define (call-with-input-string/buffered s th)
 (fluid-let
  ((*current-tex2page-input* (make-istream* ':buffer (string->list s)))
   (*input-line-no* *input-line-no*))
  (th)))

(define (snoop-char)
 (let ((c (get-char)))
   (toss-back-char c)
   c))

(define (get-char)
 (let ((b (istream*-buffer *current-tex2page-input*)))
   (if (null? b)
       (let ((p (istream*-stream *current-tex2page-input*)))
         (if (not p)
             false
             (let ((c
                    (let* ((%read-char-port p)
                           (%read-char-res
                            (if %read-char-port
                                (read-char %read-char-port)
                                (read-char))))
                      (when (eof-object? %read-char-res)
                        (set! %read-char-res false))
                      %read-char-res)))
               (cond ((not c) c)
                     ((char=? c #\newline)
                      (set! *input-line-no* (+ *input-line-no* 1)) c)
                     (else c)))))
       (let ((c (car b)))
         (set!istream*-buffer *current-tex2page-input* (cdr b))
         (istream*-buffer *current-tex2page-input*)
         c))))

(define (toss-back-string s)
 (set!istream*-buffer *current-tex2page-input*
  (append (string->list s) (istream*-buffer *current-tex2page-input*)))
 (istream*-buffer *current-tex2page-input*))

(define (toss-back-char c)
 (set!istream*-buffer *current-tex2page-input*
  (cons c (istream*-buffer *current-tex2page-input*)))
 (istream*-buffer *current-tex2page-input*))

(define (snoop-actual-char)
 (let ((c (snoop-char)))
   (cond ((not c) c)
         ((= (catcode c) **ignore**) (get-char) (snoop-actual-char))
         ((char=? c #\return) (get-char)
          (let ((c (snoop-actual-char)))
            (if (and c (char=? c #\newline))
                c
                (begin (toss-back-char #\newline) #\newline))))
         (else c))))

(define (get-actual-char)
 (let ((c (get-char)))
   (cond ((not c) c)
         ((= (catcode c) **ignore**) (get-actual-char))
         ((char=? c #\return)
          (let ((c (snoop-actual-char)))
            (if (and c (char=? c #\newline)) (get-actual-char) #\newline)))
         (else c))))

(define (get-line)
 (let ((r null))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (get-actual-char)))
         (cond ((not c) (return (if r (list->string (reverse r)) c)))
               ((char=? c #\newline) (return (list->string (reverse r))))
               (else (set! r (cons c r)) r)))
       (if %loop-returned %loop-result (%loop))))))

(define (ignorespaces . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (newlines ':stop-before-par))
   (when (< 0 %lambda-rest-arg-len)
     (set! newlines (list-ref %lambda-rest-arg 0)))
   (cond
    ((not (not (= (catcode #\ ) 10)))
     (let ((newline-active-p (not (= (catcode #\newline) 5)))
           (num-newlines-read 0)
           (c false))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (set! c (snoop-char))
           (cond ((eqv? c #\return) (set! c (snoop-actual-char)) c)
                 (else false))
           (cond ((not c) (return))
                 ((= (catcode c) **ignore**) (get-char)
                  (cond (*reading-control-sequence-p* (return)) (else false)))
                 ((char=? c #\newline)
                  (if newline-active-p
                      (return)
                      (case newlines
                        ((:stop-before-first-newline) (return))
                        ((:stop-after-first-newline)
                         (get-actual-char)
                         (return))
                        ((:stop-before-par)
                         (case num-newlines-read
                           ((0)
                            (get-actual-char)
                            (set! num-newlines-read (+ num-newlines-read 1))
                            num-newlines-read)
                           (else (toss-back-char #\newline) (return))))
                        (else (get-actual-char)))))
                 ((char-whitespace? c) (get-actual-char))
                 (else (return)))
           (if %loop-returned %loop-result (%loop))))))
    (else false))))

(define (munch-newlines)
 (let ((n 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (snoop-actual-char)))
         (cond ((not c) (return n))
               ((char=? c #\newline) (get-actual-char) (set! n (+ n 1)) n)
               ((char-whitespace? c) (get-actual-char))
               (else (return n))))
       (if %loop-returned %loop-result (%loop))))))

(define (munched-a-newline-p)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (snoop-actual-char)))
       (cond ((not c) (return false))
             ((char=? c #\newline) (get-actual-char) (return true))
             ((char-whitespace? c) (get-actual-char))
             (else (return false))))
     (if %loop-returned %loop-result (%loop)))))

(define (do-xspace)
 (let ((c (snoop-actual-char)))
   (cond
    ((not (member c '(#\  #\" #\. #\! #\, #\: #\; #\? #\/ #\' #\) #\-)))
     (emit-space #\ ))
    (else false))))

(define (do-relax) true)

(define (get-ctl-seq) (ignorespaces)
 (let ((bs (get-actual-char)))
   (cond
    ((not (= (catcode bs) **escape**))
     (terror 'get-ctl-seq "Missing control sequence (" bs ")"))
    (else false))
   (let ((c (get-char)))
     (cond ((not c) "\\ ")
           ((= (catcode c) **ignore**) "\\ ")
           ((= (catcode c) **letter**)
            (list->string
             (reverse
              (let ((s (list c #\\)))
                (let* ((%loop-returned false)
                       (%loop-result 0)
                       (return
                        (lambda %args
                          (set! %loop-returned true)
                          (set! %loop-result (and (pair? %args) (car %args))))))
                  (let %loop
                    ()
                    (let ((c (snoop-char)))
                      (cond ((not c) (return s))
                            ((= (catcode c) **ignore**) (return s))
                            ((= (catcode c) **letter**) (get-char)
                             (set! s (cons c s)) s)
                            (else (ignorespaces ':stop-before-first-newline)
                             (return s))))
                    (if %loop-returned %loop-result (%loop))))))))
           (else (list->string (list #\\ c)))))))

(define (ctl-seq-p z) (char=? (string-ref z 0) #\\))

(define (if-aware-ctl-seq-p z)
 (or (member z *if-aware-ctl-seqs*)
     (and (>= (string-length z) 3)
          (char=? (string-ref z 1) #\i)
          (char=? (string-ref z 2) #\f))
     (let ((z-th (find-corresp-prim-thunk z)))
       (if (string? z-th)
           false
           (ormap (lambda (y) (eq? z-th (find-corresp-prim-thunk y)))
            *if-aware-ctl-seqs*)))))

(define (get-group-as-reversed-chars) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((not c) (terror 'get-group "Runaway argument?")) (else false))
   (cond ((not (char=? c #\{)) (terror 'get-group "Missing {")) (else false))
   (let ((s (list c)) (nesting 0) (escape-p false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (get-actual-char)))
           (cond ((not c) (terror 'get-group "Runaway argument?"))
                 (else false))
           (cond (escape-p (set! s (cons c s)) (set! escape-p false) escape-p)
                 ((= (catcode c) **escape**)
                  (if *expand-escape-p*
                      (let ((s1
                             (begin (toss-back-char c)
                              (let ((x (get-ctl-seq)))
                                (cond
                                 ((member x '("\\ " "\\{" "\\}"))
                                  (list->string (list (string-ref x 1))))
                                 (else (tex-string-to-html-string x)))))))
                        (set! s (append (reverse (string->list s1)) s))
                        (set! escape-p false)
                        escape-p)
                      (begin (begin (set! s (cons c s)) s)
                       (begin (set! escape-p true) escape-p))))
                 ((char=? c #\{) (set! s (cons c s))
                  (set! nesting (+ nesting 1)) (set! escape-p false) escape-p)
                 ((char=? c #\}) (set! s (cons c s))
                  (if (= nesting 0)
                      (return s)
                      (begin (begin (set! nesting (- nesting 1)) nesting)
                       (begin (set! escape-p false) escape-p))))
                 (else (set! s (cons c s)) (set! escape-p false) escape-p)))
         (if %loop-returned %loop-result (%loop)))))))

(define (get-group) (list->string (reverse (get-group-as-reversed-chars))))

(define (get-peeled-group) (string-trim (ungroup (get-group))))

(define (get-token-or-peeled-group) (string-trim (ungroup (get-token))))

(define (get-grouped-environment-name-if-any)
 (let ((c (snoop-actual-char)))
   (if (or (not c) (not (char=? c #\{)))
       false
       (begin (get-actual-char)
        (let ((s null))
          (let* ((%loop-returned false)
                 (%loop-result 0)
                 (return
                  (lambda %args
                    (set! %loop-returned true)
                    (set! %loop-result (and (pair? %args) (car %args))))))
            (let %loop
              ()
              (let ((c (snoop-actual-char)))
                (cond
                 ((or (char-alphabetic? c) (char=? c #\*)) (get-actual-char)
                  (set! s (cons c s)) s)
                 ((and (pair? s) (char=? c #\})) (get-actual-char)
                  (return (list->string (reverse s))))
                 (else (for-each toss-back-char s) (toss-back-char #\{)
                  (return false))))
              (if %loop-returned %loop-result (%loop)))))))))

(define (get-bracketed-text-if-any) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (if (or (not c) (not (char=? c #\[)))
       false
       (begin (get-actual-char)
        (list->string
         (reverse
          (let ((s null) (nesting 0) (escape-p false))
            (let* ((%loop-returned false)
                   (%loop-result 0)
                   (return
                    (lambda %args
                      (set! %loop-returned true)
                      (set! %loop-result (and (pair? %args) (car %args))))))
              (let %loop
                ()
                (let ((c (get-actual-char)))
                  (cond
                   ((not c)
                    (terror 'get-bracketed-text-if-any "Runaway argument?"))
                   (else false))
                  (cond
                   (escape-p (set! s (cons c s)) (set! escape-p false)
                    escape-p)
                   ((= (catcode c) **escape**) (set! s (cons c s))
                    (set! escape-p true) escape-p)
                   ((char=? c #\{) (set! s (cons c s))
                    (set! nesting (+ nesting 1)) nesting)
                   ((char=? c #\}) (set! s (cons c s))
                    (set! nesting (- nesting 1)) nesting)
                   ((char=? c #\])
                    (if (= nesting 0)
                        (return s)
                        (begin (set! s (cons c s)) s)))
                   (else (set! s (cons c s)) s)))
                (if %loop-returned %loop-result (%loop)))))))))))

(define (ungroup s)
 (let ((n (string-length s)))
   (let ((n-1 (sub1 n)))
     (if
      (and (>= n 2)
           (char=? (string-ref s 0) #\{)
           (char=? (string-ref s n-1) #\}))
      (substring s 1 n-1)
      s))))

(define (eat-alphanumeric-string) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (if (char=? c #\")
       (begin (get-actual-char) (eat-till-char #\"))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (set! c (snoop-actual-char))
           (cond
            ((not
              (or (char-alphabetic? c) (char-numeric? c) (member c '(#\:))))
             (return))
            (else false))
           (unless %loop-returned (get-actual-char))
           (if %loop-returned %loop-result (%loop)))))))

(define (get-filename . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (bracedp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! bracedp (list-ref %lambda-rest-arg 0)))
   (ignorespaces)
   (cond
    (bracedp
     (let ((c (snoop-actual-char)))
       (if (and (char? c) (char=? c #\{))
           (get-actual-char)
           (begin (set! bracedp false) bracedp))))
    (else false))
   (list->string
    (reverse
     (let ((s null))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (let ((c (snoop-actual-char)))
             (cond ((not c) (return s))
                   ((and (not bracedp)
                         (or (char-whitespace? c)
                             (= (catcode c) **comment**)
                             (member c *filename-delims*)))
                    (ignorespaces ':stop-before-first-newline) (return s))
                   ((and bracedp (char=? c #\})) (get-actual-char) (return s))
                   ((= (catcode c) **escape**)
                    (let ((x (get-ctl-seq)))
                      (if (string=? x "\\jobname")
                          (begin
                           (set! s
                            (append (reverse (string->list *jobname*)) s))
                           s)
                          (begin (toss-back-char *invisible-space*)
                           (toss-back-string x) (return s)))))
                   (else (get-actual-char) (set! s (cons c s)) s)))
           (if %loop-returned %loop-result (%loop)))))))))

(define (get-filename-possibly-braced) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (get-filename (and (char? c) (char=? c #\{)))))

(define (get-word) (ignorespaces)
 (list->string
  (reverse
   (let ((s null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (snoop-actual-char)))
           (cond ((not c) (return s))
                 ((or (char-whitespace? c)
                      (= (catcode c) **comment**)
                      (= (catcode c))
                      **escape**)
                  (return s))
                 (else (get-actual-char) (set! s (cons c s)) s)))
         (if %loop-returned %loop-result (%loop))))))))

(define (get-integer base) (ignorespaces)
 (let ((s null) (c false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (set! c (snoop-actual-char))
       (cond ((not c) (return)) (else false))
       (unless %loop-returned
         (cond
          ((not (or (char-numeric? c) (and (= base 16) (char-alphabetic? c))))
           (ignorespaces) (return))
          (else false)))
       (unless %loop-returned (get-actual-char))
       (unless %loop-returned (set! s (cons c s)) s)
       (if %loop-returned %loop-result (%loop))))
   (cond ((pair? s) (string->number (list->string (reverse s)) base))
         (else false))))

(define (get-real) (ignorespaces)
 (let ((negative? false) (c (snoop-actual-char)))
   (cond ((char=? c #\-) (set! negative? true) negative?) (else false))
   (cond ((or negative? (char=? c #\+)) (get-actual-char)) (else false))
   (let ((s null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (snoop-actual-char)))
           (cond ((not c) (return))
                 ((or (char-numeric? c) (char=? c #\.)) (get-actual-char)
                  (set! s (cons c s)) s)
                 (else (ignorespaces) (return))))
         (if %loop-returned %loop-result (%loop))))
     (if s
         (let ((n (string->number (list->string (reverse s)))))
           (if negative? (- n) n))
         false))))

(define (get-equal-sign) (ignorespaces)
 (cond ((char=? (snoop-actual-char) #\=) (get-actual-char)) (else false)))

(define (get-by) (ignorespaces)
 (cond
  ((char=? (snoop-actual-char) #\b) (get-actual-char)
   (if (char=? (snoop-actual-char) #\y)
       (get-actual-char)
       (toss-back-char #\b)))
  (else false)))

(define (get-to) (ignorespaces)
 (cond
  ((char=? (snoop-actual-char) #\t) (get-actual-char)
   (cond
    ((char=? (snoop-actual-char) #\o) (get-actual-char)
     (ignorespaces ':stop-before-first-newline))
    (else (toss-back-char #\t))))
  (else false)))

(define (get-number-corresp-to-ctl-seq x)
 (cond ((string=? x "\\the") (get-number-corresp-to-ctl-seq (get-ctl-seq)))
       ((string=? x "\\active") 13)
       ((string=? x "\\inputlineno") *input-line-no*)
       ((string=? x "\\footnotenumber") (get-gcount "\\footnotenumber"))
       ((string=? x "\\figurenumber")
        (counter*-value (table-get "figure" *dotted-counters*)))
       ((string=? x "\\sectiondnumber")
        (table-get (string->number (ungroup (get-token))) *section-counters*
         0))
       ((string=? x "\\magstep") (get-number-or-false))
       ((find-chardef x))
       ((begin (set! *it* (find-countdef x)) *it*) (find-count *it*))
       ((begin (set! *it* (find-dimendef x)) *it*) (find-dimen *it*))
       ((find-count x))
       ((find-dimen x))
       ((begin (set! *it* (resolve-defs x)) *it*)
        (char->integer (string-ref *it* 0)))
       ((= (string-length x) 2) (char->integer (string-ref x 1)))
       (else (string->number x))))

(define (get-number-or-false) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond
    ((= (catcode c) **escape**) (get-number-corresp-to-ctl-seq (get-ctl-seq)))
    ((char=? c #\') (get-actual-char) (get-integer 8))
    ((char=? c #\") (get-actual-char) (get-integer 16))
    ((char=? c #\`) (get-actual-char) (ignorespaces)
     (char->integer
      (if (= (catcode (snoop-actual-char)) **escape**)
          (string-ref (get-ctl-seq) 1)
          (get-actual-char))))
    ((char=? c #\+) (get-actual-char) (get-number-or-false))
    ((char=? c #\-) (get-actual-char)
     (let ((n (get-number-or-false)))
       (and n (- n))))
    ((char-numeric? c) (get-integer 10))
    (else false))))

(define (get-number)
 (or (get-number-or-false) (terror 'get-number "Missing number.")))

(define (get-tex-char-spec)
 (let ((n (get-number-or-false)))
   (if n (integer->char n) (terror 'get-tex-char-spec "not a char"))))

(define (get-token-as-tex-char-spec)
 (let ((x (get-token-or-peeled-group)))
   (let ((c0 (string-ref x 0)))
     (cond
      ((and (= (catcode c0) **escape**) (> (string-length x) 1))
       (set! c0 (string-ref x 1)) c0)
      (else false))
     c0)))

(define (get-url) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((or (not c) (not (char=? c #\{))) (terror 'get-url "Missing {"))
         (else false))
   (string-trim
    (list->string
     (reverse
      (let ((nesting 0) (s null))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (let ((c (get-actual-char)))
              (cond ((not c) (terror 'get-url "Missing }")) (else false))
              (cond
               ((= (catcode c) **comment**)
                (let ((c1 (snoop-actual-char)))
                  (if (and (char? c1) (char-whitespace? c1))
                      (ignorespaces)
                      (begin (set! s (cons c s)) s))))
               ((char=? c #\{) (set! nesting (+ nesting 1)) (set! s (cons c s))
                s)
               ((char=? c #\})
                (cond ((= nesting 0) (return s))
                      (else (set! nesting (- nesting 1)) (set! s (cons c s))
                       s)))
               (else (set! s (cons c s)) s)))
            (if %loop-returned %loop-result (%loop))))))))))

(define (get-csv closing-delim) (ignorespaces)
 (let ((rev-lbl
        (let ((s null))
          (let* ((%loop-returned false)
                 (%loop-result 0)
                 (return
                  (lambda %args
                    (set! %loop-returned true)
                    (set! %loop-result (and (pair? %args) (car %args))))))
            (let %loop
              ()
              (let ((c (get-actual-char)))
                (cond
                 ((not c)
                  (terror 'get-csv "Runaway argument of \\cite, "
                   "\\nocite, \\expandhtmlindex?"))
                 (else false))
                (cond ((char=? c #\,) (return s))
                      ((char=? c closing-delim) (toss-back-char c) (return s))
                      (else (set! s (cons c s)) s)))
              (if %loop-returned %loop-result (%loop)))))))
   (cond ((not (null? rev-lbl)) (list->string (reverse rev-lbl)))
         (else false))))

(define (get-raw-token)
 (let ((c (snoop-actual-char)))
   (cond ((not c) c)
         ((= (catcode c) **escape**) (get-ctl-seq))
         (else (list->string (list (get-actual-char)))))))

(define (get-raw-token/is) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond ((not c) c)
         ((= (catcode c) **escape**) (get-ctl-seq))
         ((= (catcode c) **comment**) (eat-till-eol) (get-raw-token/is))
         (else (list->string (list (get-actual-char)))))))

(define (get-token) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond ((not c) c)
         ((= (catcode c) **escape**) (get-ctl-seq))
         ((char=? c #\{) (get-group))
         ((= (catcode c) **comment**) (eat-till-eol) (get-token))
         (else (list->string (list (get-actual-char)))))))

(define (get-token/ps)
 (let ((c (snoop-actual-char)))
   (cond ((not c) c)
         ((= (catcode c) **escape**) (get-ctl-seq))
         ((char=? c #\{) (get-group))
         ((= (catcode c) **comment**) (eat-till-eol) (get-token/ps))
         (else (list->string (list (get-actual-char)))))))

(define (eat-word word) (ignorespaces)
 (let ((r null))
   (let ((%dotimes-n (string-length word)) (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (if (>= i %dotimes-n) (return true) false)
         (unless %loop-returned
           (let ((c (snoop-actual-char)))
             (cond
              ((char=? c (string-ref word i)) (get-actual-char)
               (set! r (cons c r)) r)
              (else (for-each toss-back-char r) (return false)))))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned %loop-result (%loop)))))))

(define (eat-skip-fluff fullp)
 (let ((go-ahead-p true))
   (cond (fullp (get-equal-sign))
         ((ormap eat-word '("plus" "minus")) true)
         (else (set! go-ahead-p false) go-ahead-p))
   (cond
    (go-ahead-p
     (fluid-let ((*not-processing-p* true))
      (let ((firstp fullp))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (ignorespaces)
            (let ((c (snoop-actual-char)))
              (cond ((not c) (return))
                    ((and (= (catcode c) **escape**) firstp) (get-ctl-seq)
                     (return))
                    ((or (char-numeric? c) (char=? c #\.)) (get-real))
                    ((or (char=? c #\') (char=? c #\")) (get-number))
                    ((ormap eat-word '("+" "-")) true)
                    ((ormap eat-word
                      '("bp" "cc" "cm" "dd" "em" "ex" "filll" "fill" "fil" "in"
                        "minus" "mm" "pc" "plus" "pt" "sp" "true"))
                     (set! firstp false) firstp)
                    (else (return))))
            (if %loop-returned %loop-result (%loop)))))))
    (else false))))

(define (eat-dimen) (eat-skip-fluff true))

(define (eat-integer)
 (fluid-let ((*not-processing-p* true)) (ignorespaces) (get-equal-sign)
  (get-number)))

(define (scm-get-token)
 (list->string
  (reverse
   (let ((s null) (esc-p false) (c false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (set! c (snoop-actual-char))
         (cond ((not c) (return s)) (else false))
         (unless %loop-returned
           (cond
            (esc-p (get-actual-char) (set! s (cons c s)) (set! esc-p false)
             esc-p)
            ((char=? c #\\) (get-actual-char) (set! s (cons c s))
             (set! esc-p true) esc-p)
            ((or (char-whitespace? c) (member c *scm-token-delims*))
             (return s))
            (else (get-actual-char) (set! s (cons c s)) s)))
         (if %loop-returned %loop-result (%loop))))))))

(define (make-html-output-stream) (make-ostream* ':stream (open-output-string)))

(define (close-html-output-stream op)
 (for-each emit (reverse (ostream*-hbuffer op))) (set!ostream*-hbuffer op null)
 (ostream*-hbuffer op)
 (let ((%close-port-arg (ostream*-stream op)))
   ((if (input-port? %close-port-arg) close-input-port close-output-port)
    %close-port-arg)))

(define (html-output-stream-to-string op)
 (for-each emit (reverse (ostream*-hbuffer op))) (set!ostream*-hbuffer op null)
 (ostream*-hbuffer op) (get-output-string (ostream*-stream op)))

(define (emit s)
 (cond
  (*emit-enabled-p*
   (let ((p (ostream*-stream *html*)))
     (for-each (lambda (x) (display x p)) (reverse (ostream*-hbuffer *html*)))
     (set!ostream*-hbuffer *html* null)
     (ostream*-hbuffer *html*)
     (display s p)))
  (else false)))

(define (emit-space s)
 (cond
  (*emit-enabled-p*
   (set!ostream*-hbuffer *html* (cons s (ostream*-hbuffer *html*)))
   (ostream*-hbuffer *html*))
  (else false)))

(define (emit-newline)
 (cond
  (*emit-enabled-p*
   (set!ostream*-hbuffer *html* (cons #\newline (ostream*-hbuffer *html*)))
   (ostream*-hbuffer *html*))
  (else false)))

(define (emit-html-char c)
 (cond
  ((not (not c))
   (cond ((char=? c #\newline) (emit-newline))
         (*outputting-to-non-html-p* (emit c))
         ((char-whitespace? c) (emit-space c))
         (else
          (emit
           (case c
             ((#\<) "&#x3c;")
             ((#\>) "&#x3e;")
             ((#\") "&#x22;")
             ((#\&) "&#x26;")
             (else c))))))
  (else false)))

(define (emit-html-string s)
 (let ((%dotimes-n (string-length s)) (i 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (>= i %dotimes-n) (return) false)
       (unless %loop-returned (emit-html-char (string-ref s i)))
       (unless %loop-returned (set! i (+ i 1)))
       (if %loop-returned %loop-result (%loop))))))

(define (do-unskip) (set!ostream*-hbuffer *html* null)
 (ostream*-hbuffer *html*))

(define (catcode c . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (n false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! n (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 1)))
   (cond
    ((not n)
     (cond ((begin (set! *it* (assoc c *catcodes*)) *it*) (cdr *it*))
           ((char-alphabetic? c) 11)
           (else 12)))
    (else (set! *catcodes* (cons (cons c n) *catcodes*))
     ((if (= n 13) activate-cdef deactivate-cdef) c)))))

(define (curr-esc-char) (car (rassoc 0 *catcodes*)))

(define (kern len)
 (string-append "<span style=\"margin-left: " len "\"> </span>"))

(define (do-kern)
 (let ((n (get-pixels)))
   (emit-space "<span style=\"margin-left: ")
   (emit-space n)
   (emit-space "px\"> </span>")))

(define (read-box) (ignorespaces)
 (let ((c (snoop-actual-char)) (s false))
   (cond
    ((= (catcode c) **escape**)
     (let ((cs (get-ctl-seq)))
       (cond
        ((member cs '("\\hbox" "\\vbox" "\\vtop"))
         (let ((box-caller (get-till-char #\{)))
           (let ((box-content (get-group)))
             (set! s (string-append cs box-caller box-content))
             s)))
        ((string=? cs "\\box") (set! s (read-box-string (get-number))) s)
        ((string=? cs "\\copy") (set! s (read-box-string (get-number) true))
         s))))
    (else false))
   (or s (terror 'read-box "A <box> was supposed to be here."))))

(define (do-lower . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (raisep false))
   (when (< 0 %lambda-rest-arg-len)
     (set! raisep (list-ref %lambda-rest-arg 0)))
   (let ((n (get-pixels)))
     (let ((box (read-box)))
       (emit "<span style=\"position: relative; top: ")
       (cond (raisep (emit "-")) (else false))
       (emit n)
       (emit "px\">")
       (bgroup)
       (add-postlude-to-top-frame (lambda () (emit "</span>")))
       (toss-back-char #\})
       (toss-back-string box)
       false))))

(define *primitive-texframe* (make-texframe*))

(define *math-primitive-texframe* (make-texframe*))

(define (bgroup)
 (set! *tex-env* (cons (make-texframe* ':catcodes *catcodes*) *tex-env*))
 (cond
  ((and *in-display-math-p* (not *math-script-mode-p*)) (bgroup-math-hook))
  (else false)))

(define (egroup)
 (cond ((null? *tex-env*) (terror 'egroup "Too many }'s")) (else false))
 (perform-postludes) (perform-aftergroups)
 (let* ((%pop-old-stack *tex-env*) (%pop-top-value (car %pop-old-stack)))
   (begin (set! *tex-env* (cdr %pop-old-stack)) *tex-env*)
   %pop-top-value)
 (cond
  ((begin (set! *it* (top-texframe)) *it*)
   (set! *catcodes* (texframe*-catcodes *it*)) *catcodes*)
  (else (terror 'egroup "This can't happen."))))

(define (bgroup-math-hook)
 (let ((old-html *html*)
       (old-math-delim-left *math-delim-left*)
       (old-math-delim-right *math-delim-right*)
       (old-math-height *math-height*))
   (set! *html* (make-html-output-stream))
   (set! *math-delim-left* false)
   (set! *math-delim-right* false)
   (set! *math-height* 0)
   (set! *tabular-stack* (cons ':mathbox *tabular-stack*))
   (add-postlude-to-top-frame
    (lambda ()
      (let ((res (html-output-stream-to-string *html*)))
        (set! res
         (string-append "<table><tr><td class=centerline>" res
          "</td></tr></table>"))
        (cond
         ((or *math-delim-left* *math-delim-right*)
          (cond
           ((and
             (or (member *math-delim-left* '(:lbrace :rbrace))
                 (member *math-delim-right* '(:lbrace :rbrace)))
             (even? *math-height*))
            (set! *math-height* (+ *math-height* 1)) *math-height*)
           (else false))
          (set! res
           (string-append "<table><tr><td>"
            (tex-math-delim-string *math-delim-left*) "</td><td>" res
            "</td><td>" (tex-math-delim-string *math-delim-right*)
            "</td></tr></table>"))
          res)
         (else false))
        (set! *html* old-html)
        (set! *math-delim-left* old-math-delim-left)
        (set! *math-delim-right* old-math-delim-right)
        (set! *math-height* (+ old-math-height *math-height*))
        (pop-tabular-stack ':mathbox)
        (emit "</td><td>")
        (emit res)
        (emit "</td><td>"))))))

(define (do-math-left) (ignorespaces)
 (cond
  ((and *in-display-math-p* (not *math-script-mode-p*))
   (let ((s (get-token)))
     (bgroup)
     (cond
      ((string=? s "(") (set! *math-delim-left* ':lparen) *math-delim-left*)
      ((string=? s "[") (set! *math-delim-left* ':lbrack) *math-delim-left*)
      ((string=? s "\\{") (set! *math-delim-left* ':lbrace) *math-delim-left*)
      ((string=? s "|") (set! *math-delim-left* ':lvert) *math-delim-left*)
      ((string=? s ".") (set! *math-delim-left* ':nulldelim) *math-delim-left*)
      (else (terror 'do-math-left)))))
  (else false)))

(define (do-math-right) (ignorespaces)
 (cond
  ((and *in-display-math-p* (not *math-script-mode-p*))
   (let ((s (get-token)))
     (cond
      ((string=? s ")") (set! *math-delim-right* ':rparen) *math-delim-right*)
      ((string=? s "]") (set! *math-delim-right* ':rbrack) *math-delim-right*)
      ((string=? s "\\}") (set! *math-delim-right* ':rbrace)
       *math-delim-right*)
      ((string=? s "|") (set! *math-delim-right* ':rvert) *math-delim-right*)
      ((string=? s ".") (set! *math-delim-right* ':nulldelim)
       *math-delim-right*)
      (else (terror 'do-math-right)))
     (egroup)))
  (else false)))

(define (perform-postludes)
 (for-each (lambda (p) (p)) (texframe*-postludes (top-texframe))))

(define (perform-aftergroups)
 (let ((ags (texframe*-aftergroups (top-texframe))))
   (cond ((not (null? ags)) (toss-back-char *invisible-space*)) (else false))
   (for-each (lambda (ag) (ag)) ags)))

(define (perform-afterassignment)
 (let ((z *afterassignment*))
   (cond (z (set! *afterassignment* false) (do-tex-ctl-seq z)) (else false))))

(define (add-postlude-to-top-frame p)
 (let ((fr (if (null? *tex-env*) *global-texframe* (car *tex-env*))))
   (set!texframe*-postludes fr (cons p (texframe*-postludes fr)))
   (texframe*-postludes fr)))

(define (add-aftergroup-to-top-frame ag)
 (let ((fr (if (null? *tex-env*) *global-texframe* (car *tex-env*))))
   (set!texframe*-aftergroups fr (cons ag (texframe*-aftergroups fr)))
   (texframe*-aftergroups fr)))

(define (top-texframe) (if (null? *tex-env*) *global-texframe* (car *tex-env*)))

(define (kopy-tdef lft rt) (set!tdef*-argpat lft (tdef*-argpat rt))
 (tdef*-argpat lft) (set!tdef*-expansion lft (tdef*-expansion rt))
 (tdef*-expansion lft) (set!tdef*-optarg lft (tdef*-optarg rt))
 (tdef*-optarg lft) (set!tdef*-thunk lft (tdef*-thunk rt)) (tdef*-thunk lft)
 (set!tdef*-prim lft (tdef*-prim rt)) (tdef*-prim lft)
 (set!tdef*-defer lft (tdef*-defer rt)) (tdef*-defer lft)
 (set!tdef*-catcodes lft (tdef*-catcodes rt)) (tdef*-catcodes lft) false)

(define (kopy-cdef lft rt) (set!cdef*-argpat lft (cdef*-argpat rt))
 (cdef*-argpat lft) (set!cdef*-expansion lft (cdef*-expansion rt))
 (cdef*-expansion lft) (set!cdef*-optarg lft (cdef*-optarg rt))
 (cdef*-optarg lft) (set!cdef*-active lft (cdef*-active rt)) (cdef*-active lft)
 (set!cdef*-catcodes lft (cdef*-catcodes rt)) (cdef*-catcodes lft) false)

(define (cleanse-tdef d) (set!tdef*-argpat d null) (tdef*-argpat d)
 (set!tdef*-expansion d "") (tdef*-expansion d) (set!tdef*-optarg d false)
 (tdef*-optarg d) (set!tdef*-thunk d false) (tdef*-thunk d)
 (set!tdef*-prim d false) (tdef*-prim d) (set!tdef*-defer d false)
 (tdef*-defer d) (set!tdef*-catcodes d false) (tdef*-catcodes d))

(define (ensure-def name frame)
 (let ((frame-defs (texframe*-definitions frame)))
   (or (table-get name frame-defs)
       (let ((d (make-tdef*)))
         (table-put! name frame-defs d)
         (table-get name frame-defs)
         d))))

(define (tex-def name argpat expansion optarg thunk prim defer frame)
 (cond ((not frame) (set! frame (top-texframe)) frame) (else false))
 (let ((d (ensure-def name frame)))
   (set!tdef*-argpat d argpat)
   (set!tdef*-expansion d expansion)
   (set!tdef*-optarg d optarg)
   (set!tdef*-thunk d thunk)
   (set!tdef*-prim d prim)
   (set!tdef*-defer d defer)
   (set!tdef*-catcodes d *catcodes*)
   (tdef*-catcodes d))
 (perform-afterassignment))

(define (tex-def-prim prim thunk)
 (tex-def prim null false false thunk prim false *primitive-texframe*))

(define (tex-def-pat-prim prim argstr rhs)
 (tex-def prim (string->list argstr) rhs false false false false
  *primitive-texframe*))

(define (tex-defsym-prim prim str)
 (tex-def prim null false false (lambda () (emit str)) prim false
  *primitive-texframe*))

(define (tex-def-0arg cs expn)
 (tex-def cs null expn false false false false false))

(define (ctl-seq-no-arg-expand-once cs)
 (let ((d (find-def cs)))
   (and d (tdef*-expansion d))))

(define (tex-gdef-0arg cs expn)
 (tex-def cs null expn false false cs false *global-texframe*))

(define (tex-def-prim-0arg cs expn)
 (tex-def cs null expn false false cs false *primitive-texframe*))

(define (get-0arg-expn cs)
 (let ((d (find-def cs)))
   (if d (tdef*-expansion d) "0")))

(define (tex2page-flag-value cs) (string-ref (get-0arg-expn cs) 0))

(define (tex2page-flag-boolean cs)
 (not (member (string-ref (get-0arg-expn cs) 0) '(#\0 #\f #\F #\n #\N))))

(define (tex-let lft rt frame)
 (cond ((not frame) (set! frame (top-texframe)) frame) (else false))
 (let ((frame-defs (texframe*-definitions frame)))
   (let ((lft-def
          (or (table-get lft frame-defs)
              (let ((lft-def (make-tdef*)))
                (table-put! lft frame-defs lft-def)
                (table-get lft frame-defs)
                lft-def))))
     (cond
      ((begin (set! *it* (or (find-def rt) (find-math-def rt))) *it*)
       (let ((rt-def *it*))
         (kopy-tdef lft-def rt-def)))
      (else (cleanse-tdef lft-def))))))

(define (tex-let-general lhs rhs frame)
 (if (ctl-seq-p rhs)
     (tex-let lhs rhs frame)
     (tex-def lhs null rhs false false false false frame)))

(define (tex-let-prim lft rt) (tex-let lft rt *primitive-texframe*))

(define (tex-def-thunk name thunk frame)
 (cond
  ((not (inside-false-world-p))
   (tex-def name null false false thunk name false frame))
  (else false)))

(define (tex-def-chardef ctlseq num . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((frame (top-texframe)))
     (table-put! ctlseq (texframe*-chardefs frame) num)
     (table-get ctlseq (texframe*-chardefs frame)))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! ctlseq (texframe*-chardefs *global-texframe*) num)
     (table-get ctlseq (texframe*-chardefs *global-texframe*)))
    (else false))))

(define (do-chardef)
 (let ((cltseq (get-raw-token/is)))
   (get-equal-sign)
   (let ((num (get-number-or-false)))
     (cond ((not num) (terror 'do-chardef)) (else false))
     (tex-def-chardef cltseq num (globally-p)))))

(define (find-chardef ctlseq)
 (or
  (ormap (lambda (frame) (table-get ctlseq (texframe*-chardefs frame)))
   *tex-env*)
  (table-get ctlseq (texframe*-chardefs *global-texframe*))))

(define (gen-regno c . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (avoid null))
   (when (< 0 %lambda-rest-arg-len) (set! avoid (list-ref %lambda-rest-arg 0)))
   (let ((n (table-get c (texframe*-counts *global-texframe*))))
     (set! n (+ n 1))
     (cond ((member n avoid) (set! n (+ n 1)) n) (else false))
     (table-put! c (texframe*-counts *global-texframe*) n)
     (table-get c (texframe*-counts *global-texframe*))
     n)))

(define (tex-def-countdef ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (num false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len) (set! num (list-ref %lambda-rest-arg 1)))
   (when (< 2 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 2)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond
    ((not regno) (set! regno (or (find-countdef ctlseq) (gen-regno 10))) regno)
    (else false))
   (let ((frame (top-texframe)))
     (table-put! ctlseq (texframe*-countdefs frame) regno)
     (table-get ctlseq (texframe*-countdefs frame)))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! ctlseq (texframe*-countdefs *global-texframe*) regno)
     (table-get ctlseq (texframe*-countdefs *global-texframe*)))
    (else false))
   (cond (num (tex-def-count regno num globalp)) (else false))))

(define (plain-count ctlseq num . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp true))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-countdef ctlseq false num globalp)))

(define (find-countdef ctlseq)
 (or
  (ormap (lambda (frame) (table-get ctlseq (texframe*-countdefs frame)))
   *tex-env*)
  (table-get ctlseq (texframe*-countdefs *global-texframe*))))

(define (do-count= regno globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (get-equal-sign) (tex-def-count regno (get-number) globalp)
 (perform-afterassignment))

(define (tex-def-count regno num globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (let ((frame (top-texframe)))
   (table-put! regno (texframe*-counts frame) num)
   (table-get regno (texframe*-counts frame))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! regno (texframe*-counts *global-texframe*) num)
     (table-get regno (texframe*-counts *global-texframe*)))
    (else false))))

(define (do-countdef)
 (let ((cltseq (get-raw-token/is)))
   (get-equal-sign)
   (let ((n (get-number-or-false)))
     (cond ((not n) (terror 'do-countdef "Missing number.")) (else false))
     (tex-def-countdef cltseq n 0 (globally-p)))))

(define (do-newcount . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-countdef (get-ctl-seq) (gen-regno 10) 0 globalp)))

(define (find-count num)
 (or
  (ormap (lambda (frame) (table-get num (texframe*-counts frame))) *tex-env*)
  (table-get num (texframe*-counts *global-texframe*))
  (table-get num (texframe*-counts *primitive-texframe*))))

(define (the-count dracula)
 (cond ((begin (set! *it* (find-countdef dracula)) *it*) (find-count *it*))
       (else (terror 'the-count "Missing count register."))))

(define (get-gcount ctlseq)
 (table-get (find-countdef ctlseq) (texframe*-counts *global-texframe*) 0))

(define (tex-gdef-count ctlseq v) (tex-def-countdef ctlseq false v true))

(define (do-count . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((regno (get-number)))
     (get-equal-sign)
     (tex-def-count regno (get-number) globalp))))

(define (tex-def-dimendef ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (num false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len) (set! num (list-ref %lambda-rest-arg 1)))
   (when (< 2 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 2)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond
    ((not regno) (set! regno (or (find-dimendef ctlseq) (gen-regno 11))) regno)
    (else false))
   (let ((frame (top-texframe)))
     (table-put! ctlseq (texframe*-dimendefs frame) regno)
     (table-get ctlseq (texframe*-dimendefs frame)))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! ctlseq (texframe*-dimendefs *global-texframe*) regno)
     (table-get ctlseq (texframe*-dimendefs *global-texframe*)))
    (else false))
   (cond (num (tex-def-dimen regno num globalp)) (else false))
   regno))

(define (plain-dimen ctlseq num . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp true))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-dimendef ctlseq false num globalp)))

(define (find-dimendef ctlseq)
 (or
  (ormap (lambda (frame) (table-get ctlseq (texframe*-dimendefs frame)))
   *tex-env*)
  (table-get ctlseq (texframe*-dimendefs *global-texframe*))))

(define (do-dimen= regno globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (get-equal-sign) (tex-def-dimen regno (get-scaled-points) globalp)
 (perform-afterassignment))

(define (tex-def-dimen regno num globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (let ((frame (top-texframe)))
   (table-put! regno (texframe*-dimens frame) num)
   (table-get regno (texframe*-dimens frame))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! regno (texframe*-dimens *global-texframe*) num)
     (table-get regno (texframe*-dimens *global-texframe*)))
    (else false))))

(define (do-dimendef)
 (let ((cltseq (get-raw-token/is)))
   (get-equal-sign)
   (let ((n (get-scaled-points)))
     (cond ((not n) (terror 'do-dimendef "Missing number.")) (else false))
     (tex-def-dimendef cltseq n 0 (globally-p)))))

(define (do-newdimen . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-dimendef (get-ctl-seq) (gen-regno 11) 0 globalp)))

(define (find-dimen num)
 (or
  (ormap (lambda (frame) (table-get num (texframe*-dimens frame))) *tex-env*)
  (table-get num (texframe*-dimens *global-texframe*))
  (table-get num (texframe*-dimens *primitive-texframe*))))

(define (the-dimen ctlseq)
 (cond ((begin (set! *it* (find-dimendef ctlseq)) *it*) (find-dimen *it*))
       (else (terror 'the-dimen "Missing dimen register."))))

(define (get-dimen ctlseq)
 (cond ((the-dimen ctlseq)) (else (tex-length 6.5 ':in))))

(define (do-dimen . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((regno (get-number)))
     (get-equal-sign)
     (tex-def-dimen regno (get-scaled-points) globalp))))

(define (do-advance . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((cs (get-ctl-seq)))
     (get-by)
     (cond
      ((begin (set! *it* (find-countdef cs)) *it*)
       (let ((countnum *it*))
         (let ((countval (find-count countnum)))
           (cond
            ((not countval) (terror 'do-advance "Missing count register."))
            (else false))
           (tex-def-count countnum (+ countval (get-number)) globalp))))
      ((begin (set! *it* (find-dimendef cs)) *it*)
       (let ((dimennum *it*))
         (let ((dimenval (find-dimen dimennum)))
           (cond
            ((not dimenval) (terror 'do-advance "Missing dimen register."))
            (else false))
           (tex-def-dimen dimennum (+ dimenval (get-scaled-points)) globalp))))
      (else (terror 'do-advance "Missing register."))))))

(define (do-multiply . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((cs (get-ctl-seq)))
     (get-by)
     (cond
      ((begin (set! *it* (find-countdef cs)) *it*)
       (let ((countnum *it*))
         (let ((countval (find-count countnum)))
           (cond
            ((not countval) (terror 'do-multiply "Missing count register."))
            (else false))
           (tex-def-count countnum (* countval (get-number)) globalp))))
      ((begin (set! *it* (find-dimendef cs)) *it*)
       (let ((dimennum *it*))
         (let ((dimenval (find-dimen dimennum)))
           (cond
            ((not dimenval) (terror 'do-multiply "Missing dimen register."))
            (else false))
           (tex-def-dimen dimennum (* dimenval (get-number)) globalp))))
      (else (terror 'do-multiply "Missing register."))))))

(define (do-divide . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((cs (get-ctl-seq)))
     (get-by)
     (cond
      ((begin (set! *it* (find-countdef cs)) *it*)
       (let ((countnum *it*))
         (let ((countval (find-count countnum)))
           (cond ((not countval) (terror 'do-divide "Missing count register."))
                 (else false))
           (tex-def-count countnum (quotient countval (get-number)) globalp))))
      ((begin (set! *it* (find-dimendef cs)) *it*)
       (let ((dimennum *it*))
         (let ((dimenval (find-dimen dimennum)))
           (cond ((not dimenval) (terror 'do-divide "Missing dimen register."))
                 (else false))
           (tex-def-dimen dimennum (quotient dimenval (get-number)) globalp))))
      (else (terror 'do-divide "Missing register."))))))

(define (tex-def-newread ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 1)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond ((not regno) (set! regno (gen-regno 16 '(16))) regno) (else false))
   (tex-def-chardef ctlseq regno globalp)
   (tex-def-istream regno ':free globalp)))

(define (tex-def-newwrite ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 1)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond ((not regno) (set! regno (gen-regno 17 '(16 18))) regno) (else false))
   (tex-def-chardef ctlseq regno globalp)
   (tex-def-ostream regno ':free globalp)))

(define (tex-def-istream regno num globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (let ((frame (top-texframe)))
   (table-put! regno (texframe*-istreams frame) num)
   (table-get regno (texframe*-istreams frame))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! regno (texframe*-istreams *global-texframe*) num)
     (table-get regno (texframe*-istreams *global-texframe*)))
    (else false))))

(define (tex-def-ostream regno num globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (let ((frame (top-texframe)))
   (table-put! regno (texframe*-ostreams frame) num)
   (table-get regno (texframe*-ostreams frame))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! regno (texframe*-ostreams *global-texframe*) num)
     (table-get regno (texframe*-ostreams *global-texframe*)))
    (else false))))

(define (find-istream num)
 (or
  (ormap (lambda (frame) (table-get num (texframe*-istreams frame))) *tex-env*)
  (table-get num (texframe*-istreams *global-texframe*))
  (table-get num (texframe*-istreams *primitive-texframe*))))

(define (find-ostream num)
 (or
  (ormap (lambda (frame) (table-get num (texframe*-ostreams frame))) *tex-env*)
  (table-get num (texframe*-ostreams *global-texframe*))
  (table-get num (texframe*-ostreams *primitive-texframe*))))

(define (do-newread . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-newread (get-ctl-seq) (gen-regno 16 '(16)) globalp)))

(define (do-newwrite . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-newwrite (get-ctl-seq) (gen-regno 17 '(16 18)) globalp)))

(define (tex-def-newbox ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 1)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond ((not regno) (set! regno (gen-regno 14)) regno) (else false))
   (tex-def-chardef ctlseq regno globalp)
   (tex-def-box regno "" globalp)))

(define (tex-def-box regno bcontents . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((frame (top-texframe)))
     (table-put! regno (texframe*-boxes frame) bcontents)
     (table-get regno (texframe*-boxes frame))
     (cond
      ((and globalp (pair? *tex-env*))
       (table-put! regno (texframe*-ostreams *global-texframe*) bcontents)
       (table-get regno (texframe*-ostreams *global-texframe*)))
      (else false)))))

(define (find-box num)
 (or (ormap (lambda (frame) (table-get num (texframe*-boxes frame))) *tex-env*)
     (table-get num (texframe*-boxes *global-texframe*))
     (table-get num (texframe*-boxes *primitive-texframe*))))

(define (do-setbox)
 (let ((bno (get-number)))
   (let ((bcontents (begin (get-equal-sign) (read-box))))
     (tex-def-box bno bcontents (globally-p))))
 (perform-afterassignment))

(define (read-box-string bno . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (retainp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! retainp (list-ref %lambda-rest-arg 0)))
   (let ((b (find-box bno)))
     (cond ((not b) (terror 'read-box-string)) (else false))
     (cond ((not retainp) (tex-def-box bno "" false)) (else false))
     b)))

(define (do-box . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (retainp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! retainp (list-ref %lambda-rest-arg 0)))
   (let ((bno (get-number)))
     (let ((b (read-box-string bno retainp)))
       (toss-back-string b)))))

(define (do-newbox . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-newbox (get-ctl-seq) (gen-regno 14) globalp)))

(define (tex-def-toksdef ctlseq . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (regno false)
       (str false)
       (globalp false))
   (when (< 0 %lambda-rest-arg-len) (set! regno (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len) (set! str (list-ref %lambda-rest-arg 1)))
   (when (< 2 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 2)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond
    ((not regno) (set! regno (or (find-toksdef ctlseq) (gen-regno 15))) regno)
    (else false))
   (let ((frame (top-texframe)))
     (table-put! ctlseq (texframe*-toksdefs frame) regno)
     (table-get ctlseq (texframe*-toksdefs frame)))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! ctlseq (texframe*-toksdefs *global-texframe*) regno)
     (table-get ctlseq (texframe*-toksdefs *global-texframe*)))
    (else false))
   (cond (str (tex-def-toks regno str globalp)) (else false))))

(define (find-toksdef ctlseq)
 (or
  (ormap (lambda (frame) (table-get ctlseq (texframe*-toksdefs frame)))
   *tex-env*)
  (table-get ctlseq (texframe*-toksdefs *global-texframe*))))

(define (do-toks= regno globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (get-equal-sign) (tex-def-toks regno (get-group) globalp)
 (perform-afterassignment))

(define (do-toksdef)
 (let ((ctlseq (get-ctl-seq)))
   (get-equal-sign)
   (tex-def-toksdef ctlseq false (get-group) (globally-p))))

(define (tex-def-toks regno str globalp)
 (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
 (let ((frame (top-texframe)))
   (table-put! regno (texframe*-tokses frame) str)
   (table-get regno (texframe*-tokses frame))
   (cond
    ((and globalp (pair? *tex-env*))
     (table-put! regno (texframe*-tokses *global-texframe*) str)
     (table-get regno (texframe*-tokses *global-texframe*)))
    (else false))))

(define (do-newtoks . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-toksdef (get-ctl-seq) (gen-regno 15) "" globalp)))

(define (find-toks num)
 (or
  (ormap (lambda (frame) (table-get num (texframe*-tokses frame))) *tex-env*)
  (table-get num (texframe*-tokses *global-texframe*))
  (table-get num (texframe*-tokses *primitive-texframe*))))

(define (the-toks ctlseq)
 (cond ((begin (set! *it* (find-toksdef ctlseq)) *it*) (find-toks *it*))
       (else (terror 'the-toks "Missing toks register."))))

(define (plain-toks ctlseq str . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp true))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (tex-def-toksdef ctlseq false str globalp)))

(define (do-toks . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((regno (get-number)))
     (get-equal-sign)
     (tex-def-toks regno (get-group) globalp))))

(define (ensure-cdef c f)
 (let ((f-cdefs (texframe*-cdefinitions f)))
   (or (table-get c f-cdefs)
       (let ((d (make-cdef*)))
         (table-put! c f-cdefs d)
         (table-get c f-cdefs)
         d))))

(define (tex-def-char c argpat expansion frame)
 (cond ((not frame) (set! frame (top-texframe)) frame) (else false))
 (let ((d (ensure-cdef c frame)))
   (set!cdef*-argpat d argpat)
   (set!cdef*-expansion d expansion)
   (set!cdef*-catcodes d *catcodes*)
   (cdef*-catcodes d))
 (perform-afterassignment))

(define (find-cdef c)
 (let ((x
        (or
         (ormap (lambda (f) (table-get c (texframe*-cdefinitions f)))
          *tex-env*)
         (table-get c (texframe*-cdefinitions *global-texframe*))
         (table-get c (texframe*-cdefinitions *primitive-texframe*)))))
   (and x (cdef*-active x) x)))

(define (find-cdef-in-top-frame c)
 (let ((x
        (if (null? *tex-env*)
            (or (table-get c (texframe*-cdefinitions *global-texframe*))
                (table-get c (texframe*-cdefinitions *primitive-texframe*)))
            (table-get c (texframe*-cdefinitions (car *tex-env*))))))
   (and x (cdef*-active x) x)))

(define (do-defcsactive . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((cs (get-token)))
     (let ((c (string-ref cs (if (ctl-seq-p cs) 1 0))))
       (let ((argpat (begin (ignorespaces) (get-def-parameters))))
         (let ((rhs (ungroup (get-group))))
           (let ((f (and globalp *global-texframe*)))
             (catcode c 13)
             (tex-def-char c argpat rhs f))))))))

(define (activate-cdef c)
 (let ((y
        (cond
         ((begin (set! *it* (find-cdef-in-top-frame c)) *it*)
          (let ((y *it*))
            (set!cdef*-active y true)
            (cdef*-active y)
            y))
         (else
          (let ((d (find-cdef c)))
            (let ((y (ensure-cdef c (top-texframe))))
              (cond (d (kopy-cdef y d)) (else false))
              (set!cdef*-active y true)
              (cdef*-active y)
              y))))))
   (add-postlude-to-top-frame
    (lambda () (set!cdef*-active y false) (cdef*-active y)))))

(define (deactivate-cdef c)
 (cond
  ((begin (set! *it* (find-cdef-in-top-frame c)) *it*)
   (let ((y *it*))
     (set!cdef*-active y false)
     (cdef*-active y)))
  ((begin (set! *it* (find-cdef c)) *it*)
   (let ((y *it*))
     (let ((d (ensure-cdef c (top-texframe))))
       (kopy-cdef d y)
       (set!cdef*-active d false)
       (cdef*-active d))))))

(define (do-undefcsactive) (ignorespaces)
 (deactivate-cdef (string-ref (get-ctl-seq) 1)))

(define (do-catcode)
 (let ((c (get-tex-char-spec)))
   (let ((val (begin (get-equal-sign) (get-number))))
     (catcode c val))))

(define (do-opmac-activettchar) (ignorespaces)
 (let ((c (get-token-as-tex-char-spec)))
   (set! *opmac-active-tt-char* c)
   (activate-cdef c)
   (tex-def-char c null "\\TIIPopmacverb" false)))

(define (do-opmac-intext-verb) (bgroup)
 (fluid-let ((*ligatures-p* false))
  (cond (*outputting-external-title-p* false)
        (else (emit "<code class=verbatim>")))
  (do-verb-delimed *opmac-active-tt-char*)
  (cond (*outputting-external-title-p* false) (else (emit "</code>"))))
 (egroup))

(define (do-global) (ignorespaces)
 (let ((next (get-ctl-seq)))
   (cond ((string=? next "\\def") (do-def true false))
         ((string=? next "\\edef") (do-def true true))
         ((string=? next "\\let") (do-let true))
         ((string=? next "\\newbox") (do-newbox true))
         ((string=? next "\\newcount") (do-newcount true))
         ((string=? next "\\count") (do-count true))
         ((string=? next "\\newtoks") (do-newtoks true))
         ((string=? next "\\toks") (do-toks true))
         ((string=? next "\\newdimen") (do-newdimen true))
         ((string=? next "\\dimen") (do-dimen true))
         ((string=? next "\\advance") (do-advance true))
         ((string=? next "\\multiply") (do-multiply true))
         ((string=? next "\\divide") (do-divide true))
         ((string=? next "\\read") (do-read true))
         ((or (string=? next "\\imgdef") (string=? next "\\gifdef"))
          (make-reusable-img true))
         ((find-count next) (do-count= next true))
         ((find-dimen next) (do-dimen= next true))
         ((find-toks next) (do-toks= next true))
         (else (toss-back-string next)))))

(define (do-externaltitle)
 (write-aux
  (quasiquote
   (!preferred-title (unquote (tex-string-to-html-string (get-group)))))))

(define (make-external-title title)
 (fluid-let ((*outputting-external-title-p* true)) (bgroup)
  (let ((s
         (tex-string-to-html-string
          (string-append "\\let\\\\\\ignorespaces" "\\def\\resizebox#1#2#3{}"
           "\\let\\thanks\\TIIPgobblegroup" "\\let\\urlh\\TIIPgobblegroup "
           title))))
    (egroup)
    s)))

(define (output-external-title)
 (fluid-let ((*outputting-external-title-p* true)) (emit "<title>")
  (emit-newline) (emit (or *title* *jobname*)) (emit-newline) (emit "</title>")
  (emit-newline)))

(define (output-title title) (emit "<h1 class=title>") (bgroup)
 (tex2page-string (string-append "\\let\\\\\\break " title)) (egroup)
 (emit "</h1>") (do-noindent))

(define (do-subject) (tex-gdef-0arg "\\TIIPtitleused" "1") (do-end-para)
 (ignorespaces)
 (let ((title
        (let ((c (snoop-actual-char)))
          (if (or (not c) (not (char=? c #\{))) (get-till-par) (get-group)))))
   (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
   (write-aux
    (quasiquote (!default-title (unquote (make-external-title title)))))
   (output-title title)))

(define (do-latex-title)
 (let ((title (get-group)))
   (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
   (write-aux
    (quasiquote (!default-title (unquote (make-external-title title)))))
   (toss-back-string title)
   (toss-back-string "\\def\\TIIPtitle")))

(define (do-title)
 (if (eq? *tex-format* ':latex) (do-latex-title) (do-subject)))

(define (do-author) (toss-back-string "\\def\\TIIPauthor"))

(define (do-date) (toss-back-string "\\def\\TIIPdate"))

(define (do-today)
 (let ((m (get-gcount "\\month")))
   (if (= m 0)
       (emit "[today]")
       (begin (emit (vector-ref *month-names* (sub1 m))) (emit " ")
        (emit (get-gcount "\\day")) (emit ", ") (emit (get-gcount "\\year"))))))

(define (add-afterpar ap) (set! *afterpar* (cons ap *afterpar*)) *afterpar*)

(define (do-end-para)
 (cond
  (*in-para-p* (cond (*use-closing-p-tag-p* (emit "</p>")) (else false))
   (cond
    ((not (null? *afterpar*))
     (for-each (lambda (ap) (ap)) (reverse *afterpar*)) (set! *afterpar* null)
     *afterpar*)
    (else false))
   (emit-newline) (set! *in-para-p* false) *in-para-p*)
  (else false)))

(define (do-para)
 (cond
  ((and *in-para-p* (pair? (ostream*-hbuffer *html*)))
   (set!ostream*-hbuffer *html* null) (ostream*-hbuffer *html*))
  (else (do-end-para)
   (let ((in-table-p
          (and (pair? *tabular-stack*) (eq? (car *tabular-stack*) ':block))))
     (cond (in-table-p (emit "</td></tr><tr><td>") (emit-newline))
           (else false))
     (emit "<p>")
     (emit-newline)
     (set! *in-para-p* true)
     *in-para-p*))))

(define (do-noindent) (do-end-para) (emit-newline) (emit "<p class=noindent>")
 (set! *in-para-p* true) *in-para-p*)

(define (do-indent)
 (let ((parindent (sp-to-pixels (the-dimen "\\parindent"))))
   (emit "<span style=\"margin-left: ")
   (emit parindent)
   (emit "pt\"></span>")))

(define (do-para-nopadding) (do-end-para) (emit-newline)
 (emit "<p class=nopadding>") (emit-newline) (set! *in-para-p* true)
 *in-para-p*)

(define (do-maketitle) (do-end-para) (bgroup)
 (tex2page-string
  (string-append "\\let\\\\\\break" "\\let\\and\\break"
   "\\let\\thanks\\symfootnote"))
 (output-title "\\TIIPtitle") (do-para) (do-end-para)
 (emit "<div class=centerline>") (emit-newline)
 (tex2page-string "\\TIIPauthor") (do-para) (tex2page-string "\\TIIPdate")
 (do-end-para) (emit "</div>") (emit-newline) (egroup) (do-para))

(define (do-inputcss) (ignorespaces)
 (let ((f (get-filename-possibly-braced)))
   (cond ((null? *stylesheets*) (flag-missing-piece ':stylesheets))
         (else false))
   (write-aux (quasiquote (!stylesheet (unquote f))))))

(define (do-csname) (ignorespaces)
 (let ((r null))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (snoop-actual-char)))
         (cond
          ((= (catcode c) **escape**)
           (let ((x (get-ctl-seq)))
             (cond
              ((string=? x "\\endcsname") (toss-back-char #\})
               (for-each toss-back-string r) (toss-back-char c)
               (toss-back-char #\{) (toss-back-string "TIIPcsname")
               (toss-back-char c) (return))
              (else (set! r (cons (expand-ctl-seq-into-string x) r)) r))))
          (else (get-actual-char) (set! r (cons (list->string (list c)) r))
           r)))
       (if %loop-returned %loop-result (%loop))))))

(define (do-saved-csname)
 (let ((x (get-peeled-group)))
   (do-tex-ctl-seq x)))

(define (do-cssblock)
 (fluid-let ((*dumping-nontex-p* true))
  (dump-till-end-env "cssblock" *css-stream*)))

(define (link-stylesheets)
 (let ((link-it
        (lambda (css)
          (emit "<link rel=\"stylesheet\" href=\"")
          (emit css)
          (emit "\" />")
          (emit-newline))))
   (for-each link-it *stylesheets*)
   (cond
    ((or (tex2page-flag-boolean "\\TIIPsinglepage")
         (tex2page-flag-boolean "\\TZPsinglepage"))
     (emit "<style>") (emit-newline) (emit *basic-style*) (emit "</style>")
     (emit-newline))
    (else false))
   (link-it (string-append *jobname* *css-file-suffix*))))

(define (link-scripts)
 (let ((link-it
        (lambda (jsf)
          (emit "<script src=\"")
          (emit jsf)
          (emit "\"></script>")
          (emit-newline))))
   (for-each link-it *scripts*)))

(define (increment-section-counter seclvl nonum-p)
 (cond
  ((not nonum-p)
   (cond
    ((not (table-get seclvl *section-counters*))
     (table-put! seclvl *section-counters* 0)
     (table-get seclvl *section-counters*))
    (else false))
   (table-put! seclvl *section-counters*
    (+ (table-get seclvl *section-counters*) 1))
   (table-get seclvl *section-counters*))
  (else false))
 (table-for-each
  (lambda (k v)
    (cond
     ((and (> k seclvl) (> k 0)) (table-put! k *section-counters* 0)
      (table-get k *section-counters*))
     (else false)))
  *section-counters*)
 (cond
  ((and
    (not
     (or (tex2page-flag-boolean "\\TIIPsinglepage")
         (tex2page-flag-boolean "\\TZPsinglepage")))
    (= seclvl 0))
   (tex-gdef-count "\\footnotenumber" 0))
  (else false))
 (for-each
  (lambda (counter-name)
    (set!counter*-value (table-get counter-name *dotted-counters*) 0)
    (counter*-value (table-get counter-name *dotted-counters*)))
  (table-get seclvl *section-counter-dependencies* null)))

(define (section-counter-value seclvl)
 (if (= seclvl -1)
     (number-to-roman (table-get -1 *section-counters*) true)
     (let ((i (if *using-chapters-p* 0 1)))
       (let ((outermost-secnum
              (let ((n (table-get i *section-counters* 0)))
                (if *inside-appendix-p*
                    (list->string
                     (list (integer->char (+ (char->integer #\A) -1 n))))
                    (write-to-string n)))))
         (let ((i (add1 i)) (r outermost-secnum))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (cond ((> i seclvl) (return r)) (else false))
               (unless %loop-returned
                 (set! r
                  (string-append r "."
                   (write-to-string (table-get i *section-counters* 0))))
                 r)
               (unless %loop-returned (set! i (+ i 1)) i)
               (if %loop-returned %loop-result (%loop)))))))))

(define (section-ctl-seq-p s)
 (cond ((string=? s "\\sectiond") (string->number (ungroup (get-token))))
       ((string=? s "\\part") -1)
       ((string=? s "\\chapter") 0)
       (else
        (let ((n (string-length s)))
          (cond ((< n 8) false)
                ((and (>= n 10) (string=? (substring s (- n 9)) "paragraph"))
                 (let ((n-9 (- n 9)) (i 1) (i+3 4) (k 4))
                   (let* ((%loop-returned false)
                          (%loop-result 0)
                          (return
                           (lambda %args
                             (set! %loop-returned true)
                             (set! %loop-result
                              (and (pair? %args) (car %args))))))
                     (let %loop
                       ()
                       (cond ((> i+3 n-9) (return k))
                             ((string=? (substring s i i+3) "sub") (set! i i+3)
                              (set! i+3 (+ i+3 3)) (set! k (add1 k)) k)
                             (else (return false)))
                       (if %loop-returned %loop-result (%loop))))))
                ((string=? (substring s (- n 7)) "section")
                 (let ((n-7 (- n 7)) (i 1) (i+3 4) (k 1))
                   (let* ((%loop-returned false)
                          (%loop-result 0)
                          (return
                           (lambda %args
                             (set! %loop-returned true)
                             (set! %loop-result
                              (and (pair? %args) (car %args))))))
                     (let %loop
                       ()
                       (cond ((> i+3 n-7) (return k))
                             ((string=? (substring s i i+3) "sub") (set! i i+3)
                              (set! i+3 (+ i+3 3)) (set! k (add1 k)) k)
                             (else (return false)))
                       (if %loop-returned %loop-result (%loop))))))
                (else false))))))

(define (do-heading seclvl)
 (let ((starred-p
        (cond ((char=? (snoop-actual-char) #\*) (get-actual-char) true)
              (else false))))
   (let ((too-deep-p
          (let ((secnumdepth (get-gcount "\\secnumdepth")))
            (cond ((< secnumdepth -1) false)
                  ((> seclvl secnumdepth) true)
                  (else false)))))
     (let ((nonum-p (or starred-p too-deep-p)))
       (let ((header
              (fluid-let ((*tabular-stack* (list ':header)))
               (get-bracketed-text-if-any)
               (tex-string-to-html-string (get-group)))))
         (do-heading-help seclvl starred-p nonum-p false false header))))))

(define (do-heading-help seclvl starred-p nonum-p notoc-p lbl-val header)
 (write-aux (quasiquote (!default-title (unquote header))))
 (cond
  ((= seclvl 0) (!using-chapters)
   (cond
    ((and (eq? *tex-format* ':latex) (< (get-gcount "\\secnumdepth") -1))
     (tex-gdef-count "\\secnumdepth" 2))
    (else false))
   (cond
    ((or (> *html-page-count* 0) (tex2page-flag-boolean "\\TIIPtitleused"))
     (do-eject))
    (else (tex-gdef-0arg "\\TIIPtitleused" "1") (do-para))))
  (else false))
 (cond
  ((and (= seclvl 1)
        (or (tex2page-flag-boolean "\\TIIPslides")
            (tex2page-flag-boolean "\\TZPslides")))
   (do-eject))
  (else false))
 (increment-section-counter seclvl nonum-p)
 (cond (lbl-val (set! nonum-p false) nonum-p) (else false))
 (cond
  ((not lbl-val)
   (set! lbl-val (if nonum-p "IGNORE" (section-counter-value seclvl))) lbl-val)
  (else false))
 (let ((htmlnum (max 1 (min 6 (if *using-chapters-p* (add1 seclvl) seclvl)))))
   (let ((lbl
          (string-append *html-node-prefix*
           (case seclvl ((-1) "part") ((0) "chap") (else "sec")) "_"
           (if nonum-p (gen-temp-string) lbl-val))))
     (cond
      ((not false) (tex-def-0arg "\\TIIPcurrentnodename" lbl)
       (tex-def-0arg "\\@currentlabel" lbl-val))
      (else false))
     (do-end-para)
     (emit-anchor lbl)
     (emit-newline)
     (ignorespaces ':all)
     (emit "<h")
     (emit htmlnum)
     (case seclvl
       ((-1) (emit " class=part class=centerline"))
       ((0) (emit " class=chapter"))
       (else (emit " class=section")))
     (emit ">")
     (let ((write-to-toc-p
            (and (not notoc-p)
                 *toc-page*
                 (not
                  (and (eqv? *tex-format* ':latex)
                       (string=? header "Contents"))))))
       (cond
        ((eqv? *tex-format* ':latex)
         (case seclvl
           ((-1)
            (emit "<div class=partheading>")
            (if nonum-p
                (emit-nbsp 1)
                (begin
                 (cond
                  (write-to-toc-p
                   (emit-page-node-link-start *toc-page*
                    (string-append *html-node-prefix* "toc_" lbl)))
                  (else false))
                 (tex2page-string "\\partname") (emit " ") (emit lbl-val)
                 (cond (write-to-toc-p (emit-link-stop)) (else false))))
            (emit "</div><br>")
            (emit-newline))
           ((0)
            (emit-newline)
            (emit "<div class=chapterheading>")
            (if nonum-p
                (emit-nbsp 1)
                (begin
                 (cond
                  (write-to-toc-p
                   (emit-page-node-link-start *toc-page*
                    (string-append *html-node-prefix* "toc_" lbl)))
                  (else false))
                 (tex2page-string
                  (if *inside-appendix-p* "\\appendixname" "\\chaptername"))
                 (emit " ") (emit lbl-val)
                 (cond (write-to-toc-p (emit-link-stop)) (else false))))
            (emit "</div><br>")
            (emit-newline))))
        (else false))
       (cond
        (write-to-toc-p
         (emit-page-node-link-start *toc-page*
          (string-append *html-node-prefix* "toc_" lbl)))
        (else false))
       (cond
        ((not (or (and (eqv? *tex-format* ':latex) (<= seclvl 0)) nonum-p))
         (emit lbl-val) (emit-nbsp 2))
        (else false))
       (emit header)
       (cond (write-to-toc-p (emit-link-stop)) (else false))
       (emit "</h")
       (emit htmlnum)
       (emit ">")
       (do-noindent)
       (let ((tocdepth (get-gcount "\\tocdepth")))
         (cond
          ((and write-to-toc-p
                (not (and (eq? *tex-format* ':latex) starred-p))
                (or (< tocdepth -1) (<= seclvl tocdepth)))
           (write-aux
            (quasiquote
             (!toc-entry
              (unquote
               (if (= seclvl -1)
                   -1
                   (if *using-chapters-p* seclvl (sub1 seclvl))))
              (unquote lbl-val) (unquote *html-page-count*) (unquote lbl)
              (unquote header)))))
          (else false))))
     (cond
      (*recent-node-name* (do-label-aux *recent-node-name*)
       (set! *recent-node-name* false) *recent-node-name*)
      (else false)))))

(define (section-type-to-depth sectype)
 (cond ((string->number sectype))
       ((string=? sectype "chapter") 0)
       ((string=? sectype "section") 1)
       ((string=? sectype "subsection") 2)
       ((string=? sectype "subsubsection") 3)
       ((string=? sectype "paragraph") 4)
       ((string=? sectype "subparagraph") 5)
       (else 3)))

(define (do-write-to-toc-aux seclvl secnum sectitle)
 (let ((node-name
        (string-append *html-node-prefix* "sec_"
         (if (string=? secnum "") (gen-temp-string) secnum))))
   (tex-def-0arg "\\TIIPcurrentnodename" node-name)
   (tex-def-0arg "\\@currentlabel" secnum)
   (emit-anchor node-name)
   (emit-newline)
   (write-aux
    (quasiquote
     (!toc-entry (unquote seclvl) (unquote secnum) (unquote *html-page-count*)
      (unquote node-name) (unquote sectitle))))))

(define (do-addcontentsline)
 (let ((toc (get-peeled-group)))
   (cond
    ((not (string=? toc "toc"))
     (terror 'do-addcontentsline "only #1=toc supported"))
    (else false))
   (let ((seclvl (section-type-to-depth (get-peeled-group))))
     (let ((sectitle (tex-string-to-html-string (get-group))))
       (write-aux
        (quasiquote
         (!toc-entry
          (unquote
           (if (= seclvl -1) -1 (if *using-chapters-p* seclvl (sub1 seclvl))))
          (unquote (ctl-seq-no-arg-expand-once "\\@currentlabel"))
          (unquote *html-page-count*)
          (unquote (ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename"))
          (unquote sectitle))))))))

(define (do-documentclass) (probably-latex) (get-bracketed-text-if-any)
 (let ((x (get-peeled-group)))
   (cond ((member x '("report" "book")) (!using-chapters)) (else false))))

(define (do-document) (probably-latex)
 (cond
  ((eqv? *tex-format* ':latex) (set! *emit-enabled-p* true) *emit-enabled-p*)
  (else false)))

(define (get-till-par)
 (let ((r null) (newline-p false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (get-actual-char)))
         (cond
          ((or (not c) (and newline-p (char=? c #\newline)))
           (return (list->string (reverse r))))
          (newline-p
           (cond
            ((not (char-whitespace? c)) (set! r (cons #\  r))
             (set! r (cons c r)) (set! newline-p false) newline-p)
            (else false)))
          ((char=? c #\newline) (set! newline-p true) newline-p)
          (else (set! r (cons c r)) (set! newline-p false) newline-p)))
       (if %loop-returned %loop-result (%loop))))))

(define (do-beginsection) (ignorespaces)
 (let ((header
        (fluid-let ((*tabular-stack* (list ':header)))
         (tex-string-to-html-string (get-till-par)))))
   (do-heading-help 1 false true true false header)))

(define (do-beginchapter) (ignorespaces)
 (let ((chapno (tex-string-to-html-string (get-till-char #\ ))))
   (let ((header
          (begin (ignorespaces)
           (fluid-let ((*tabular-stack* (list ':header)))
            (tex-string-to-html-string (get-till-par))))))
     (tex-gdef-0arg "\\chapno" chapno)
     (tex-gdef-count "\\subsecno" 0)
     (tex-gdef-count "\\footnotenumber" 0)
     (cond ((string=? chapno "") (set! chapno false) chapno) (else false))
     (do-heading-help 0 false true false chapno header))))

(define (do-appendix)
 (cond
  ((not *inside-appendix-p*) (set! *inside-appendix-p* true)
   (table-put! (if *using-chapters-p* 0 1) *section-counters* 0)
   (table-get (if *using-chapters-p* 0 1) *section-counters*))
  (else false)))

(define (do-table-plain) (do-end-para) (emit "<table width=100%><tr><td>"))

(define (do-end-table-plain) (do-end-para) (emit "</td></tr></table>"))

(define (pop-tabular-stack type)
 (let ((type-in-stack
        (let* ((%pop-old-stack *tabular-stack*)
               (%pop-top-value (car %pop-old-stack)))
          (begin (set! *tabular-stack* (cdr %pop-old-stack)) *tabular-stack*)
          %pop-top-value)))
   (cond
    ((not (eq? type-in-stack type))
     (terror 'pop-tabular-stack "Bad environment closer: " type " "
      type-in-stack))
    (else false))))

(define (do-end-table/figure type)
 (cond
  ((and (eqv? type ':figure) (char=? (snoop-actual-char) #\*))
   (get-actual-char))
  (else false))
 (do-end-para) (emit "</td></tr>") (emit "</table>") (emit "</div>")
 (pop-tabular-stack type) (egroup) (do-para))

(define (bump-dotted-counter name)
 (let ((counter (table-get name *dotted-counters*)))
   (let ((new-value (add1 (counter*-value counter))))
     (set!counter*-value counter new-value)
     (counter*-value counter)
     (let ((num
            (string-append
             (let ((sec-num (counter*-within counter)))
               (if sec-num
                   (string-append (section-counter-value sec-num) ".")
                   ""))
             (write-to-string new-value))))
       (tex-def-0arg "\\@currentlabel" num)
       num))))

(define (do-caption) (do-end-para)
 (let ((i-fig
        (let ((%position-v ':figure) (%position-s *tabular-stack*))
          (cond ((string? %position-s) (string-index %position-s %position-v))
                (else (list-position %position-v %position-s))))))
   (let ((i-tbl
          (let ((%position-v ':table) (%position-s *tabular-stack*))
            (cond
             ((string? %position-s) (string-index %position-s %position-v))
             (else (list-position %position-v %position-s))))))
     (let ((type
            (cond
             ((and (not i-fig) (not i-tbl))
              (terror 'do-caption "Mislaid \\caption"))
             ((not i-fig) ':table)
             ((not i-tbl) ':figure)
             ((< i-fig i-tbl) ':figure)
             ((< i-tbl i-fig) ':table)
             (else (terror 'do-caption "cant happen")))))
       (let ((counter-name (if (eq? type ':table) "table" "figure")))
         (let ((caption-title
                (if (eq? type ':table) "\\tablename" "\\figurename")))
           (let ((num (bump-dotted-counter counter-name)))
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
             (emit "<tr><td>"))))))))

(define (do-marginnote) (emit "<span class=marginnote>")
 (tex2page-string (get-group)) (emit "</span>"))

(define (do-marginpar) (get-bracketed-text-if-any)
 (emit "<table class=leftline border=2><tr><td>") (tex2page-string (get-group))
 (emit "</td></tr></table>"))

(define (do-minipage) (get-bracketed-text-if-any) (get-group)
 (let ((in-table-p
        (and (not (null? *tabular-stack*))
             (member (car *tabular-stack*) '(:block :figure :table)))))
   (if in-table-p (emit "</td><td>") (begin (do-para) (do-end-para)))
   (emit "<div class=leftline>")
   (set! *tabular-stack* (cons ':minipage *tabular-stack*))
   *tabular-stack*))

(define (do-endminipage) (pop-tabular-stack ':minipage)
 (let ((in-table-p (member (car *tabular-stack*) '(:block :figure :table))))
   (emit "</div>")
   (if in-table-p (emit "</td><td>") (do-para))))

(define (do-tabbing) (set! *tabular-stack* (cons ':tabbing *tabular-stack*))
 (do-para))

(define (do-end-tabbing) (pop-tabular-stack ':tabbing) (do-para))

(define (do-equation type)
 (cond
  ((and
    (or (not (tex2page-flag-boolean "\\TZPmathtext"))
        (tex2page-flag-boolean "\\TZPmathimage"))
    (not *temporarily-use-utf8-for-math-p*))
   (do-latex-env-as-image
    (case type ((:equation) "equation") ((:align) "align") (else "eqnarray"))
    ':display))
  (else (do-end-para) (bgroup)
   (cond ((eqv? type ':align) (set! type ':eqnarray) type) (else false))
   (cond ((and (eqv? type ':eqnarray) (eat-star)) (set! type ':eqnarray*) type)
         (else false))
   (set! *tabular-stack* (cons type *tabular-stack*)) (set! *math-mode-p* true)
   (set! *in-display-math-p* true)
   (let ((eqn-tag (string-append *html-node-prefix* "eqn_" (gen-temp-string))))
     (tex-def-0arg "\\TIIPcurrentnodename" eqn-tag)
     (emit-anchor eqn-tag)
     (emit-newline)
     (cond
      ((not (eqv? type ':eqnarray*))
       (set! *equation-number* (bump-dotted-counter "equation"))
       *equation-number*)
      (else false))
     (emit "<div class=")
     (emit *display-justification*)
     (emit "><table width=100%>")
     (emit-newline)
     (emit "<tr><td align=")
     (emit (if (eqv? type ':equation) "center" "right"))
     (emit ">")))))

(define (do-end-equation type)
 (cond ((and (eqv? type ':eqnarray) (eat-star)) (set! type ':eqnarray*) type)
       (else false))
 (do-end-para) (emit "</td>")
 (cond
  ((not
    (or (eq? (car *tabular-stack*) ':eqnarray*) (not *equation-numbered-p*)))
   (emit "<td>(") (emit *equation-number*) (emit ")</td>"))
  (else false))
 (emit "</tr>") (emit-newline) (emit "</table></div>") (pop-tabular-stack type)
 (set! *math-mode-p* false) (set! *in-display-math-p* false) (egroup)
 (set! *equation-numbered-p* true) (set! *equation-position* 0) (do-para))

(define (do-integral)
 (if (or (not *in-display-math-p*) *math-script-mode-p*)
     (emit "&#x222b;")
     (let ((affixes-already-read null))
       (emit
        "<span style=\"font-size: 200%; position: relative; top: .25ex;\">&#x222b;</span>")
       (let ((%dotimes-n 2) (i 0))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (if (>= i %dotimes-n) (return) false)
             (unless %loop-returned (ignorespaces))
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (cond
                  ((and (member c '(#\_ #\^))
                        (not (member c affixes-already-read)))
                   (set! affixes-already-read (cons c affixes-already-read))
                   (get-actual-char)
                   (cond ((= i 0) (emit-space (kern ".16667em"))) (else false))
                   (fluid-let ((*math-script-mode-p* true))
                    (let ((s (get-token)))
                      (emit
                       "<span style=\"font-size: 85%; position: relative; ")
                      (emit
                       (case c
                         ((#\_) "top: 2.5ex; ")
                         ((#\^) "bottom: 3ex; ")
                         (else (error 'ecase "0xdeadc0de"))))
                      (emit "\">")
                      (tex2page-string s)
                      (emit "</span>"))))
                  (else false))))
             (unless %loop-returned (set! i (+ i 1)))
             (if %loop-returned %loop-result (%loop))))))))

(define (do-nonumber) (set! *equation-numbered-p* false) *equation-numbered-p*)

(define (indent-n-levels n)
 (let ((%dotimes-n (add1 n)) (i 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (>= i %dotimes-n) (return) false)
       (unless %loop-returned (emit-nbsp 1))
       (unless %loop-returned (emit " "))
       (unless %loop-returned (emit-nbsp 1))
       (unless %loop-returned (emit " "))
       (unless %loop-returned (set! i (+ i 1)))
       (if %loop-returned %loop-result (%loop))))))

(define (do-toc)
 (fluid-let
  ((*subjobname* (string-append *jobname* *toc-file-suffix*))
   (*img-file-count* 0) (*imgdef-file-count* 0))
  (cond
   ((eqv? *tex-format* ':latex)
    (tex2page-string
     (if *using-chapters-p*
         "\\chapter*{\\contentsname}"
         "\\section*{\\contentsname}")))
   (else false))
  (emit-anchor (string-append *html-node-prefix* "toc"))
  (!toc-page *html-page-count*)
  (write-aux (quasiquote (!toc-page (unquote *html-page-count*))))
  (cond
   ((null? *toc-list*) (flag-missing-piece ':toc)
    (non-fatal-error "Table of contents not generated; rerun TeX2page"))
   (else (do-noindent)
    (let ((tocdepth (get-gcount "\\tocdepth")))
      (for-each
       (lambda (x)
         (let ((lvl (tocentry*-level x)))
           (let ((secnum (tocentry*-number x)))
             (let ((seclabel (tocentry*-label x)))
               (let ((subentries-p
                      (or (= lvl -1)
                          (and (= lvl 0)
                               (or (< tocdepth -1)
                                   (and *using-chapters-p* (> tocdepth 0))
                                   (and (not *using-chapters-p*)
                                        (> tocdepth 1)))))))
                 (cond
                  (subentries-p
                   (if
                    (or (tex2page-flag-boolean "\\TIIPtexlayout")
                        (tex2page-flag-boolean "\\TZPtexlayout"))
                    (do-bigskip ':medskip)
                    (do-para))
                   (do-noindent) (emit "<b>") (emit-newline))
                  (else false))
                 (indent-n-levels lvl)
                 (emit-anchor
                  (string-append *html-node-prefix* "toc_" seclabel))
                 (emit-page-node-link-start (tocentry*-page x) seclabel)
                 (cond
                  ((not (or (string=? secnum "") (string=? secnum "IGNORE")))
                   (emit secnum) (emit-nbsp 2))
                  (else false))
                 (fluid-let ((*tabular-stack* (list 'header)))
                  (emit (tocentry*-header x)))
                 (emit-link-stop)
                 (cond (subentries-p (emit "</b>")) (else false))
                 (emit "<br>")
                 (emit-newline))))))
       *toc-list*))))
  (emit-anchor (string-append *html-node-prefix* "toc_end"))))

(define (do-numbered-footnote) (do-footnote-aux false))

(define (do-symfootnote) (set! *footnote-sym* (+ *footnote-sym* 1))
 (do-footnote-aux (number-to-footnote-symbol *footnote-sym*)))

(define (tex-string-to-html-string s)
 (fluid-let ((*html* (make-html-output-stream))) (tex2page-string s)
  (html-output-stream-to-string *html*)))

(define (expand-tex-string s)
 (fluid-let
  ((*html* (make-html-output-stream)) (*outputting-to-non-html-p* true))
  (tex2page-string s) (html-output-stream-to-string *html*)))

(define number-to-footnote-symbol
 (let ((symlist false) (symlist-len 0))
   (lambda (n)
     (cond
      ((not symlist)
       (set! symlist
        '("*" "&#x2020;" "&#x2021;" "&#xa7;" "&#xb6;" "&#x2225;" "*"
          "&#x2020;&#x2020;" "&#x2021;&#x2021;"))
       (set! symlist-len (length symlist)) symlist-len)
      (else false))
     (list-ref symlist (modulo (sub1 n) symlist-len)))))

(define (do-plain-footnote)
 (do-footnote-aux
  (fluid-let ((*temporarily-use-utf8-for-math-p* true))
   (tex-string-to-html-string (get-token)))))

(define (do-footnote)
 (if (eq? *tex-format* ':latex) (do-numbered-footnote) (do-plain-footnote)))

(define (do-footnote-aux fnmark)
 (let ((fnno false))
   (let ((fnlabel (gen-temp-string)))
     (let ((fntag (string-append *html-node-prefix* "footnote_" fnlabel)))
       (let ((fncalltag
              (string-append *html-node-prefix* "call_footnote_" fnlabel)))
         (cond
          ((not fnmark) (set! fnno (add1 (get-gcount "\\footnotenumber")))
           (tex-gdef-count "\\footnotenumber" fnno)
           (set! fnmark (write-to-string fnno)) fnmark)
          (else false))
         (emit-anchor fncalltag)
         (emit "<span class=footnotemark>")
         (cond (fnno (emit "<sup>")) (else false))
         (emit-page-node-link-start false fntag)
         (emit fnmark)
         (emit-link-stop)
         (cond (fnno (emit "</sup>")) (else false))
         (emit "</span>")
         (do-vfootnote-aux fnmark fncalltag fntag))))))

(define (do-vfootnote)
 (do-vfootnote-aux
  (fluid-let ((*temporarily-use-utf8-for-math-p* true))
   (tex-string-to-html-string (get-token)))
  false false))

(define (do-vfootnote-aux fnmark fncalltag fntag) (ignorespaces)
 (cond
  ((not (char=? (get-actual-char) #\{)) (terror 'do-vfootnote-aux "Missing {"))
  (else false))
 (bgroup)
 (let ((old-html *html*))
   (set! *html* (make-html-output-stream))
   (cond
    (fncalltag (tex-def-0arg "\\TIIPcurrentnodename" fntag)
     (tex-def-0arg "\\@currentlabel" fnmark))
    (else false))
   (add-aftergroup-to-top-frame
    (lambda ()
      (set! *footnote-list*
       (cons
        (make-footnotev* ':mark fnmark ':text
         (html-output-stream-to-string *html*) ':tag fntag ':caller fncalltag)
        *footnote-list*))
      (set! *html* old-html)
      *html*))))

(define (output-footnotes)
 (let ((n (length *footnote-list*)))
   (cond
    ((not (= n 0)) (emit "<div class=footnoterule><hr></div>") (do-para)
     (do-end-para) (emit "<div class=footnote>")
     (let ((i (sub1 n)))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (cond ((< i 0) (return)) (else false))
           (unless %loop-returned
             (let ((fv (list-ref *footnote-list* i)))
               (let ((fnmark (footnotev*-mark fv)))
                 (let ((fnno (string->number fnmark)))
                   (let ((fncalltag (footnotev*-caller fv)))
                     (do-para)
                     (emit "<span class=footnotemark>")
                     (cond
                      (fncalltag (emit-anchor (footnotev*-tag fv))
                       (cond (fnno (emit "<sup>")) (else false))
                       (emit-page-node-link-start false fncalltag))
                      (else false))
                     (emit fnmark)
                     (cond
                      (fncalltag (emit-link-stop)
                       (cond (fnno (emit "</sup>")) (else false)))
                      (else false))
                     (emit "</span>")
                     (emit " ")
                     (emit (footnotev*-text fv))
                     (do-end-para)
                     (set! i (- i 1))
                     i)))))
           (if %loop-returned %loop-result (%loop)))))
     (emit "</div>") (emit-newline))
    (else false))))

(define rgb-dec-to-rrggbb
 (let ((f
        (lambda (x)
          (let ((n (round (* 1.0 x))))
            (let ((s (write-to-string n ':base 16)))
              (if (< n 16) (string-append "0" s) s))))))
   (lambda (r g b) (string-append "#" (f r) (f g) (f b)))))

(define (rgb-frac-to-rrggbb r g b)
 (rgb-dec-to-rrggbb (* r 255) (* g 255) (* b 255)))

(define cmyk-to-rrggbb
 (let ((f (lambda (x k) (- 1 (min (max (+ x k) 0) 1)))))
   (lambda (c m y k) (rgb-frac-to-rrggbb (f c k) (f m k) (f y k)))))

(define (hsl-in-html h s l)
 (string-append "hsl(" (write-to-string h) "," (write-to-string (* s 100)) "%,"
  (write-to-string (* l 100)) "%)"))

(define (hsb360-to-hsl h s b)
 (let ((l (* 0.5 b (- 2 s))))
   (let ((s2 (/ (* b s) (- 1 (abs (sub1 (* 2 l)))))))
     (hsl-in-html h s2 l))))

(define (wavelength-to-hsl w)
 (let ((hue
        (* 1/6
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
   (hsb360-to-hsl (* 360 hue) 1 brightness)))

(define (read-color model)
 (case model
   ((:cmy)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((c
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((m
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((y
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (rgb-frac-to-rrggbb (- 1 c) (- 1 m) (- 1 y))))))))
   ((:cmyk)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((c
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((m
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((y
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (let ((k
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res) (set! %read-res false))
                      %read-res)))
               (ignorespaces)
               (cmyk-to-rrggbb c m y k))))))))
   ((:rgb)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((r
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((g
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((b
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (rgb-frac-to-rrggbb r g b)))))))
   ((:rgb255)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((r
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((g
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((b
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (rgb-dec-to-rrggbb r g b)))))))
   ((:gray)
    (call-with-input-string (tex-string-to-html-string (get-token))
     (lambda (i)
       (let ((g
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (ignorespaces)
         (hsb360-to-hsl 0 0 g)))))
   ((:gray15)
    (call-with-input-string (tex-string-to-html-string (get-token))
     (lambda (i)
       (let ((g
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (ignorespaces)
         (hsb360-to-hsl 0 0 (/ g 15))))))
   ((:html)
    (call-with-input-string (tex-string-to-html-string (get-token))
     (lambda (i)
       (let ((rrggbb (read-6hex i)))
         (ignorespaces)
         rrggbb))))
   ((:hsb)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((h
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((s
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((b
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (hsb360-to-hsl (* 360 h) s b)))))))
   ((:hsb360)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((h
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((s
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((b
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (hsb360-to-hsl h s b)))))))
   ((:hsb240)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((h
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((s
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((b
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (hsb360-to-hsl (* h 3/2) (/ s 240) (/ b 240))))))))
   ((:wave)
    (call-with-input-string (tex-string-to-html-string (get-token))
     (lambda (i)
       (let ((w
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (ignorespaces)
         (wavelength-to-hsl w)))))
   ((:hsl)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((h
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((s
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((l
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (hsl-in-html (* 360 h) s l)))))))
   ((:hsl360)
    (bgroup)
    (call-with-input-string
     (tex-string-to-html-string
      (string-append "\\defcsactive\\,{ }" (get-token)))
     (lambda (i)
       (egroup)
       (let ((h
              (let ((%read-res (read i)))
                (when (eof-object? %read-res) (set! %read-res false))
                %read-res)))
         (let ((s
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res false))
                  %read-res)))
           (let ((l
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res) (set! %read-res false))
                    %read-res)))
             (ignorespaces)
             (hsl-in-html h s l)))))))
   (else
    (let ((name (get-peeled-group)))
      (let ((c (assoc name *color-names*)))
        (ignorespaces)
        (if c (cdr c) name))))))

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
       ((string=? model "hsl") ':hsl)
       ((string=? model "Hsl") ':hsl360)
       (else ':colornamed)))

(define (do-switch sw)
 (cond
  ((not *outputting-external-title-p*)
   (add-postlude-to-top-frame
    (case sw
      ((:rm)
       (cond
        (*math-mode-p*
         (let ((old-math-font *math-font*))
           (set! *math-font* ':rm)
           (lambda () (set! *math-font* old-math-font) *math-font*)))
        (else false)))
      ((:em) (emit "<em>") (lambda () (emit "</em>")))
      ((:it) (emit "<i>") (lambda () (emit "</i>")))
      ((:bf) (emit "<strong>") (lambda () (emit "</strong>")))
      ((:sl)
       (emit "<span style=\"font-style: oblique\">")
       (lambda () (emit "</span>")))
      ((:sf)
       (emit "<span style=\"font-family: sans-serif\">")
       (lambda () (emit "</span>")))
      ((:tt)
       (let ((old-ligatures-p *ligatures-p*))
         (set! *ligatures-p* false)
         (emit "<span style=\"font-family: monospace\">")
         (lambda ()
           (emit "</span>")
           (set! *ligatures-p* old-ligatures-p)
           *ligatures-p*)))
      ((:sc)
       (let ((old-in-small-caps-p *in-small-caps-p*))
         (set! *in-small-caps-p* true)
         (lambda ()
           (set! *in-small-caps-p* old-in-small-caps-p)
           *in-small-caps-p*)))
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
      ((:tiny) (emit "<span class=tiny>") (lambda () (emit "</span>")))
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
      ((:large) (emit "<span class=large>") (lambda () (emit "</span>")))
      ((:large-cap)
       (emit "<span class=largecap>")
       (lambda () (emit "</span>")))
      ((:large-up) (emit "<span class=largeup>") (lambda () (emit "</span>")))
      ((:huge) (emit "<span class=huge>") (lambda () (emit "</span>")))
      ((:huge-cap) (emit "<span class=hugecap>") (lambda () (emit "</span>")))
      ((:cmy :cmyk :rgb :rgb255 :gray :gray15 :html :hsb :hsb360 :hsb240 :wave
        :hsl :hsl360 :colornamed)
       (emit "<span style=\"color: ")
       (emit (read-color sw))
       (emit "\">")
       (lambda () (emit "</span>")))
      ((:bgcolor)
       (emit "<span style=\"background-color: ")
       (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
         (emit (read-color model)))
       (emit "\">")
       (lambda () (emit "</span>")))
      ((:strike) (emit "<strike>") (lambda () (emit "</strike>")))
      ((:narrower)
       (do-end-para)
       (emit "<blockquote>")
       (lambda () (do-end-para) (emit "</blockquote>") (do-para)))
      ((:raggedleft)
       (do-end-para)
       (emit "<div class=rightline>")
       (lambda () (do-end-para) (emit "</div>") (do-para)))
      ((:oldstyle) (emit "<span class=oldstyle>") (lambda () (emit "</span>")))
      ((:cal)
       (cond
        (*math-mode-p*
         (let ((old-math-font *math-font*))
           (set! *math-font* ':cal)
           (lambda () (set! *math-font* old-math-font) *math-font*)))
        (else false)))
      (else
       (emit "<span class=")
       (emit sw)
       (emit ">")
       (lambda () (emit "</span>"))))))
  (else false)))

(define (do-obeylines) (ignorespaces ':stop-after-first-newline)
 (activate-cdef #\newline) (tex-def-char #\newline null "\\TIIPbr" false))

(define (do-obeyspaces) (catcode #\  13)
 (tex-def-char #\  null "\\TIIPnbsp" false))

(define (do-block z) (do-end-para) (emit "<div ")
 (emit
  (case z
    ((:flushleft) "class=leftline")
    ((:flushright) "class=rightline")
    (else "class=centerline")))
 (emit ">") (set! *tabular-stack* (cons ':block *tabular-stack*))
 (emit "<table><tr><td>") (bgroup) (emit-newline))

(define (do-end-block) (do-end-para) (egroup) (emit "</td></tr></table></div>")
 (pop-tabular-stack ':block) (emit-newline))

(define (do-function fn)
 (fluid-let ((*math-mode-p* *math-mode-p*))
  (cond (*outputting-external-title-p* false)
        ((string=? fn "\\emph") (emit "<em>"))
        ((string=? fn "\\leftline") (do-end-para)
         (emit "<div class=leftline>"))
        ((string=? fn "\\centerline") (do-end-para)
         (emit "<div class=centerline>&#xa0;"))
        ((string=? fn "\\rightline") (do-end-para)
         (emit "<div class=rightline>&#xa0;"))
        ((string=? fn "\\underline") (emit "<u>"))
        ((string=? fn "\\textbf") (set! *math-mode-p* false) (emit "<b>"))
        ((or (string=? fn "\\textit") (string=? fn "\\textsl"))
         (set! *math-mode-p* false) (emit "<i>"))
        ((string=? fn "\\textrm") (set! *math-mode-p* false) *math-mode-p*)
        ((string=? fn "\\texttt") (set! *math-mode-p* false)
         (emit "<span style=\"font-family: monospace\">"))
        (else (terror 'do-function "Unknown function " fn)))
  (bgroup) (tex2page-string (get-token)) (egroup)
  (cond (*outputting-external-title-p* false)
        ((string=? fn "\\emph") (emit "</em>"))
        ((string=? fn "\\rightline") (emit "</div>") (emit-newline))
        ((or (string=? fn "\\leftline") (string=? fn "\\centerline"))
         (do-end-para) (emit "&#xa0;</div>") (emit-newline))
        ((string=? fn "\\underline") (emit "</u>"))
        ((string=? fn "\\textbf") (emit "</b>"))
        ((or (string=? fn "\\textsl") (string=? fn "\\textit")) (emit "</i>"))
        ((string=? fn "\\texttt") (emit "</span>")))))

(define (do-discretionary) (tex2page-string (get-group)) (get-group)
 (get-group))

(define (do-aftergroup) (ignorespaces)
 (let ((z (get-ctl-seq)))
   (add-aftergroup-to-top-frame (lambda () (toss-back-string z)))))

(define (do-afterassignment) (ignorespaces)
 (let ((z (get-ctl-seq)))
   (set! *afterassignment* z)
   *afterassignment*))

(define (do-actual-space) (emit "&#x200b;") (emit #\ ) (emit "&#x200b;"))

(define (emit-nbsp n)
 (let ((%dotimes-n n) (i 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (>= i %dotimes-n) (return) false)
       (unless %loop-returned (emit-space "&#xa0;"))
       (unless %loop-returned (set! i (+ i 1)))
       (if %loop-returned %loop-result (%loop))))))

(define (scaled-point-equivalent-of unit)
 (case unit
   ((:pt) 65536)
   ((:pc) (* 12 (scaled-point-equivalent-of ':pt)))
   ((:in) (* 72.27 (scaled-point-equivalent-of ':pt)))
   ((:bp) (* (/ 72) (scaled-point-equivalent-of ':in)))
   ((:cm) (* (/ 2.54) (scaled-point-equivalent-of ':in)))
   ((:mm) (* 0.1 (scaled-point-equivalent-of ':cm)))
   ((:dd) (* (/ 1238 1157) (scaled-point-equivalent-of ':pt)))
   ((:cc) (* 12 (scaled-point-equivalent-of ':dd)))
   ((:sp) 1)
   ((:em) (* 16 (scaled-point-equivalent-of ':pt)))
   ((:ex) (* 0.45 (scaled-point-equivalent-of ':em)))
   (else (terror 'scaled-point-equivalent-of "Illegal unit of measure " unit))))

(define (tex-length num unit) (* num (scaled-point-equivalent-of unit)))

(define (sp-to-ems sp) (/ sp 655360.0))

(define (sp-to-pixels sp) (inexact->exact (floor (/ sp 65536))))

(define (get-scaled-points)
 (let ((n (or (get-real) 1)))
   (ignorespaces)
   (* n
      (if (= (catcode (snoop-actual-char)) **escape**)
          (let ((x (get-ctl-seq)))
            (get-dimen x))
          (begin
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (cond ((not (eat-word "true")) (return)) (else false))
               (if %loop-returned %loop-result (%loop))))
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
                 (else 1)))))))

(define (get-points) (/ (get-scaled-points) 65536.0))

(define (get-pixels) (inexact->exact (floor (get-points))))

(define (do-font) (get-ctl-seq) (get-equal-sign) (eat-alphanumeric-string)
 (cond ((eat-word "at") (eat-dimen)) ((eat-word "scaled") (get-number))))

(define (do-fontdimen) (get-number) (get-ctl-seq) (get-equal-sign) (eat-dimen))

(define (do-hskip)
 (let ((n (get-pixels)))
   (emit-space "<span style=\"margin-left: ")
   (emit-space n)
   (emit-space "pt\">&#x200c;</span>")))

(define (do-vskip)
 (let ((x (get-points)))
   (eat-skip-fluff false)
   (emit "<div style=\"height: ")
   (emit x)
   (emit "pt\"></div>")
   (emit-newline)
   (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
   (emit-newline)
   (set! *in-para-p* true)
   *in-para-p*))

(define (do-hrule) (do-end-para) (emit "<hr>") (emit-newline) (do-para))

(define (do-newline) (cond ((>= (munch-newlines) 1) (do-para)) (else false))
 (emit-newline))

(define (do-br)
 (if (or (find-cdef #\ ) (not (= (the-count "\\TIIPobeylinestrictly") 0)))
     (emit "<br>")
     (cond ((not (eqv? (snoop-actual-char) #\newline)) (emit "<br>"))
           (else false)))
 (emit-newline))

(define (do-sup) (emit "<sup>")
 (fluid-let ((*math-script-mode-p* true)) (tex2page-string (get-token)))
 (emit "</sup>"))

(define (do-sub) (emit "<sub>")
 (fluid-let ((*math-script-mode-p* true)) (tex2page-string (get-token)))
 (emit "</sub>"))

(define (do-hyphen)
 (cond (*math-mode-p* (emit (if (eq? *math-font* ':rm) "-" "&#x2212;")))
       ((not *ligatures-p*) (emit #\-))
       (else
        (let ((c (snoop-actual-char)))
          (if (and (char? c) (char=? c #\-))
              (begin (get-actual-char) (do-ndash))
              (emit #\-))))))

(define (do-excl)
 (if (or *math-mode-p* (not *ligatures-p*))
     (emit #\!)
     (let ((c (snoop-actual-char)))
       (if (and (char? c) (char=? c #\`))
           (begin (get-actual-char) (emit "&#xa1;"))
           (emit #\!)))))

(define (do-quest)
 (if (or *math-mode-p* (not *ligatures-p*))
     (emit #\?)
     (let ((c (snoop-actual-char)))
       (if (and (char? c) (char=? c #\`))
           (begin (get-actual-char) (emit "&#xbf;"))
           (emit #\?)))))

(define (do-ndash)
 (emit
  (let ((c (snoop-actual-char)))
    (if (and (char? c) (char=? c #\-))
        (begin (get-actual-char) "&#x2014;")
        "&#x2013;"))))

(define (do-lsquo)
 (emit
  (if (not *ligatures-p*)
      #\`
      (let ((c (snoop-actual-char)))
        (if (and (char? c) (char=? c #\`))
            (begin (get-actual-char) "&#x201c;")
            "&#x2018;")))))

(define (do-rsquo)
 (emit
  (cond
   (*math-mode-p*
    (let ((c (snoop-actual-char)))
      (if (and (char? c) (char=? c #\'))
          (begin (get-actual-char) "&#x2033;")
          "&#x2032;")))
   ((not *ligatures-p*) #\')
   (else
    (let ((c (snoop-actual-char)))
      (if (and (char? c) (char=? c #\'))
          (begin (get-actual-char) "&#x201d;")
          "&#x2019;"))))))

(define (do-enquote) (ignorespaces)
 (cond
  ((and (char=? (snoop-actual-char) #\*)) (get-actual-char) (ignorespaces)
   (cond
    ((= *quote-level* 0) (set! *quote-level* (+ *quote-level* 1))
     *quote-level*)
    (else false)))
  (else false))
 (cond ((not (char=? (get-actual-char) #\{)) (terror 'do-enquote "Missing {"))
       (else false))
 (bgroup) (set! *quote-level* (+ *quote-level* 1))
 (emit (if (odd? *quote-level*) "&#x201c;" "&#x2018;"))
 (add-aftergroup-to-top-frame
  (lambda ()
    (emit (if (odd? *quote-level*) "&#x201d;" "&#x2019;"))
    (set! *quote-level* (- *quote-level* 1))
    *quote-level*)))

(define (get-label)
 (let ((lbl (get-peeled-group)))
   (let ((i
          (or
           (let ((%position-v #\ ) (%position-s lbl))
             (cond
              ((string? %position-s) (string-index %position-s %position-v))
              (else (list-position %position-v %position-s))))
           (let ((%position-v #\tab) (%position-s lbl))
             (cond
              ((string? %position-s) (string-index %position-s %position-v))
              (else (list-position %position-v %position-s))))
           (let ((%position-v #\newline) (%position-s lbl))
             (cond
              ((string? %position-s) (string-index %position-s %position-v))
              (else (list-position %position-v %position-s)))))))
     (if (not i)
         lbl
         (let ((s (string->list lbl)) (r null) (whitep false))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (cond ((null? s) (return (list->string (reverse r))))
                     (else false))
               (unless %loop-returned
                 (let ((c
                        (let* ((%pop-old-stack s)
                               (%pop-top-value (car %pop-old-stack)))
                          (begin (set! s (cdr %pop-old-stack)) s)
                          %pop-top-value)))
                   (cond
                    ((char-whitespace? c)
                     (cond ((not whitep) (set! r (cons #\  r)) r) (else false))
                     (set! whitep true) whitep)
                    (else (set! r (cons c r)) (set! whitep false) whitep))))
               (if %loop-returned %loop-result (%loop)))))))))

(define (emit-anchor lbl) (emit "<a id=\"") (emit lbl) (emit "\"></a>"))

(define (emit-link-start link) (emit "<a href=\"") (emit link) (emit "\">"))

(define (emit-ext-page-node-link-start extfile pageno node) (emit "<a ")
 (cond ((not extfile) (emit "class=hrefinternal ")) (else false))
 (emit "href=\"")
 (cond
  ((not (and (not extfile) (or (not pageno) (= *html-page-count* pageno))))
   (emit (or extfile *jobname*))
   (cond ((not (= pageno 0)) (emit *html-page-suffix*) (emit pageno))
         (else false))
   (emit *output-extension*))
  (else false))
 (cond (node (emit "#") (emit node)) (else false)) (emit "\">"))

(define (emit-page-node-link-start pageno node)
 (emit-ext-page-node-link-start false pageno node))

(define (emit-page-link-start pageno)
 (emit-ext-page-node-link-start false pageno false))

(define (emit-link-stop) (emit "</a>"))

(define (do-anchor-for-potential-label)
 (let ((node-name
        (string-append *html-node-prefix* "anchor_" (gen-temp-string))))
   (tex-def-0arg "\\TIIPcurrentnodename" node-name)
   (emit-anchor node-name)))

(define (do-node) (set! *recent-node-name* (get-peeled-group))
 *recent-node-name*)

(define (do-label-aux label)
 (let ((name (ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename"))
       (value (ctl-seq-no-arg-expand-once "\\@currentlabel")))
   (set! value (tex-string-to-html-string value))
   (!label label *html-page-count* name value)
   (write-label
    (quasiquote
     (!label (unquote label) (unquote *html-page-count*) (unquote name)
      (unquote value))))))

(define (do-inputexternallabels)
 (let ((f (get-filename-possibly-braced)))
   (let ((fq-f
          (if (fully-qualified-pathname-p f) f (string-append *aux-dir/* f))))
     (let ((ext-label-file (string-append fq-f *label-file-suffix*)))
       (let ((ext-label-table (table-get f *external-label-tables*)))
         (cond
          ((not ext-label-table)
           (set! ext-label-table (make-table ':test equal?))
           (table-put! f *external-label-tables* ext-label-table)
           (table-get f *external-label-tables*))
          (else false))
         (cond
          ((file-exists? ext-label-file)
           (fluid-let ((*label-source* fq-f) (*label-table* ext-label-table))
            (load-tex2page-data-file ext-label-file)))
          (else false)))))))

(define (do-includeexternallabels)
 (let ((jobname (get-filename-possibly-braced)))
   (let ((ext-label-file
          (string-append
           (if (fully-qualified-pathname-p jobname)
               jobname
               (string-append *aux-dir/* jobname))
           *label-file-suffix*)))
     (cond
      ((file-exists? ext-label-file)
       (fluid-let ((*label-source* jobname))
        (load-tex2page-data-file ext-label-file)))
      (else false)))))

(define (do-tag)
 (let ((tag-name (get-peeled-group)))
   (do-tag-aux tag-name (get-group))))

(define (do-definexref)
 (let ((tag (get-peeled-group)))
   (let ((value (get-group)))
     (get-token)
     (do-tag-aux tag value))))

(define (do-tag-aux tag-name tag-val)
 (let ((node-name (string-append *html-node-prefix* "tag_" (gen-temp-string))))
   (tex-def-0arg "\\TIIPcurrentnodename" node-name)
   (tex-def-0arg "\\@currentlabel" tag-val)
   (emit-anchor node-name)
   (do-label-aux tag-name)))

(define (do-htmlpagelabel)
 (let ((label (get-peeled-group)))
   (!label label *html-page-count* false false)
   (write-label
    (quasiquote
     (!label (unquote label) (unquote *html-page-count*) false false)))))

(define (do-refexternal)
 (let ((ext-file (get-peeled-group)))
   (do-ref-aux (get-label) ext-file false)))

(define (do-ref-aux label ext-file link-text)
 (let ((label-ref (label-bound-p label ext-file)))
   (let ((label-text
          (cond (link-text (tex-string-to-html-string link-text))
                (label-ref (label*-value label-ref))
                (else label))))
     (cond
      (label-ref
       (emit-ext-page-node-link-start (or ext-file (label*-src label-ref))
        (label*-page label-ref) (label*-name label-ref))
       (emit label-text) (emit-link-stop))
      (else (non-fatal-error label))))))

(define (maybe-label-page this-label-src this-label-pageno)
 (if (and (not this-label-src) (= *html-page-count* this-label-pageno))
     ""
     (string-append (or this-label-src *jobname*)
      (if (= this-label-pageno 0)
          ""
          (string-append *html-page-suffix*
           (write-to-string this-label-pageno)))
      *output-extension*)))

(define (do-htmlref)
 (let ((text (get-group)))
   (let ((lbl (get-peeled-group)))
     (do-ref-aux lbl false text))))

(define (do-htmlrefexternal)
 (let ((text (get-group)))
   (let ((extf (get-peeled-group)))
     (let ((lbl (get-peeled-group)))
       (do-ref-aux lbl extf text)))))

(define (do-hyperref)
 (let ((lbl (get-bracketed-text-if-any)))
   (if lbl
       (do-ref-aux lbl false (get-group))
       (let ((text (get-group)))
         (let ((lbl (begin (get-group) (get-group) (get-peeled-group))))
           (do-ref-aux lbl false text))))))

(define (do-hypertarget)
 (let ((lbl (get-peeled-group)))
   (do-tag-aux lbl "hypertarget")))

(define (do-hyperlink)
 (emit-link-start (fully-qualify-url (string-append "#" (get-peeled-group))))
 (tex2page-string (get-token)) (emit-link-stop))

(define (label-bound-p label . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (ext-file false))
   (when (< 0 %lambda-rest-arg-len)
     (set! ext-file (list-ref %lambda-rest-arg 0)))
   (let ((label-table
          (if ext-file
              (table-get ext-file *external-label-tables*)
              *label-table*)))
     (or (and label-table (table-get label label-table))
         (begin
          (flag-unresolved-xref
           (if ext-file (string-append "{" ext-file " -> " label "}") label))
          false)))))

(define (flag-unresolved-xref xr)
 (let ((%push-added-value xr) (%push-old-stack *unresolved-xrefs*))
   (cond ((member %push-added-value %push-old-stack) %push-old-stack)
         (else
          (set! *unresolved-xrefs* (cons %push-added-value %push-old-stack))
          *unresolved-xrefs*))))

(define (flag-missing-piece mp)
 (let ((%push-added-value mp) (%push-old-stack *missing-pieces*))
   (cond ((member %push-added-value %push-old-stack) %push-old-stack)
         (else (set! *missing-pieces* (cons %push-added-value %push-old-stack))
          *missing-pieces*))))

(define (show-unresolved-xrefs-and-missing-pieces)
 (cond
  ((not (and (null? *unresolved-xrefs*) (null? *missing-pieces*)))
   (show-unresolved-xrefs) (show-missing-pieces)
   (write-log ':separation-newline) (write-log "Rerun: tex2page ")
   (write-log *main-tex-file*) (write-log ':separation-newline)
   (write-log "If problem persists, check for ")
   (write-log "missing \\label's and \\bibitem's"))
  (else false)))

(define (show-unresolved-xrefs)
 (cond
  ((not (null? *unresolved-xrefs*)) (write-log ':separation-newline)
   (write-log "Unresolved cross-reference")
   (cond ((> (length *unresolved-xrefs*) 1) (write-log "s")) (else false))
   (write-log ": ") (set! *unresolved-xrefs* (reverse *unresolved-xrefs*))
   (write-log (car *unresolved-xrefs*))
   (for-each
    (lambda (x) (write-log #\,) (write-log ':separation-space) (write-log x))
    (cdr *unresolved-xrefs*))
   (write-log ':separation-newline))
  (else false)))

(define (show-missing-pieces)
 (cond
  ((not (null? *missing-pieces*)) (write-log ':separation-newline)
   (cond
    ((member ':tex-format *missing-pieces*)
     (write-log "TeX format not determined") (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':document-title *missing-pieces*)
     (write-log "Document title not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':last-page *missing-pieces*)
     (write-log "Last page not determined") (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':last-modification-time *missing-pieces*)
     (write-log "Last modification time not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':stylesheets *missing-pieces*)
     (write-log "Style sheets not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':scripts *missing-pieces*) (write-log "Scripts not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':html-head *missing-pieces*)
     (write-log "HTML header info not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':toc *missing-pieces*)
     (write-log "Table of contents not determined")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((member ':fresh-index *missing-pieces*) (write-log "Index not refreshed")
     (write-log ':separation-newline))
    ((member ':index *missing-pieces*) (write-log "Index not included")
     (write-log ':separation-newline)))
   (cond
    ((member ':fresh-bibliography *missing-pieces*)
     (write-log "Bibliography not refreshed") (write-log ':separation-newline))
    ((member ':bibliography *missing-pieces*)
     (write-log "Bibliography not included") (write-log ':separation-newline)))
   (cond
    ((member ':metapost *missing-pieces*)
     (write-log "MetaPost output not included")
     (write-log ':separation-newline))
    (else false)))
  (else false)))

(define (do-htmlpageref)
 (let ((label (get-peeled-group)))
   (let ((label-ref (label-bound-p label)))
     (emit "\"")
     (if label-ref
         (emit
          (maybe-label-page (label*-src label-ref) (label*-page label-ref)))
         (emit *log-file*))
     (emit "\""))))

(define (doc-internal-url url)
 (let ((n (string-length url)))
   (cond
    ((and (> n 0) (char=? (string-ref url 0) #\#))
     (let ((label (substring url 1)))
       (let ((label-ref (label-bound-p label)))
         (if label-ref
             (if (label*-src label-ref)
                 false
                 (list (label*-page label-ref) (label*-name label-ref)))
             false))))
    (else false))))

(define (fully-qualify-url url)
 (let ((n (string-length url)))
   (cond
    ((and (> n 0) (char=? (string-ref url 0) #\#))
     (let ((label (substring url 1)))
       (let ((label-ref (label-bound-p label)))
         (if label-ref
             (string-append
              (maybe-label-page (label*-src label-ref) (label*-page label-ref))
              "#" (label*-name label-ref))
             url))))
    ((fully-qualified-url-p url) url)
    (else (ensure-url-reachable url) url))))

(define (do-url)
 (let ((url (get-url)))
   (let ((durl (doc-internal-url url)))
     (if durl
         (emit-page-node-link-start (car durl) (cadr durl))
         (emit-link-start (fully-qualify-url url))))
   (emit url)
   (emit-link-stop)))

(define (do-mailto)
 (let ((addr (get-url)))
   (emit-link-start (string-append "mailto:" addr))
   (emit addr)
   (emit-link-stop)))

(define (do-urlh)
 (let ((url (get-url)))
   (let ((durl (doc-internal-url url)))
     (if durl
         (emit-page-node-link-start (car durl) (cadr durl))
         (emit-link-start (fully-qualify-url url)))))
 (bgroup)
 (tex2page-string (string-append "\\def\\\\{\\egroup\\endinput}" (get-token)))
 (egroup) (emit-link-stop))

(define (do-urlhd) (do-urlh) (get-token))

(define (do-urlp)
 (let ((link-text (get-token)))
   (let ((url (get-url)))
     (let ((durl (doc-internal-url url)))
       (if durl
           (emit-page-node-link-start (car durl) (cadr durl))
           (emit-link-start (fully-qualify-url url)))))
   (tex2page-string link-text)
   (emit-link-stop)))

(define (do-hlstart)
 (let ((cat (get-peeled-group)))
   (let ((url (begin (get-token) (get-url))))
     (cond
      ((string=? cat "url") (emit-link-start (fully-qualify-url url)) (bgroup)
       (tex-let "\\hlend" "\\TIIPhlend" false))
      (else false))
     (ignorespaces))))

(define (do-hlend) (egroup) (emit-link-stop))

(define (do-img-src f)
 (cond
  ((or (tex2page-flag-boolean "\\TIIPsinglepage")
       (tex2page-flag-boolean "\\TZPsinglepage"))
   (let ((tmpf (string-append *aux-dir/* *jobname* "-Z-Z.temp")))
     (system (string-append "echo -n data: > " tmpf))
     (system (string-append "file -bN --mime-type " f " >> " tmpf))
     (system (string-append "echo -n \\;base64, >> " tmpf))
     (system (string-append "base64 -w0 < " f " >> " tmpf))
     (let ((i (open-input-file tmpf)))
       (let ((%with-open-file-res
              (let* ((%loop-returned false)
                     (%loop-result 0)
                     (return
                      (lambda %args
                        (set! %loop-returned true)
                        (set! %loop-result (and (pair? %args) (car %args))))))
                (let %loop
                  ()
                  (let ((c
                         (let* ((%read-char-port i)
                                (%read-char-res
                                 (if %read-char-port
                                     (read-char %read-char-port)
                                     (read-char))))
                           (when (eof-object? %read-char-res)
                             (set! %read-char-res false))
                           %read-char-res)))
                    (if c (emit c) (return)))
                  (if %loop-returned %loop-result (%loop))))))
         (cond (i ((if (input-port? i) close-input-port close-output-port) i))
               (else false))
         %with-open-file-res))
     (ensure-file-deleted tmpf)))
  (else (emit (ensure-url-reachable f)))))

(define (do-htmladdimg)
 (let ((align-info (get-bracketed-text-if-any)))
   (let ((url (get-url)))
     (emit "<img src=\"")
     (do-img-src (fully-qualify-url url))
     (emit "\"")
     (emit " style=\"border: 0\"")
     (cond (align-info (tex2page-string align-info)) (else false))
     (emit " alt=\"[")
     (emit url)
     (emit "]\">"))))

(define (do-pdfximage)
 (let ((height false) (width false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((eat-word "height") (set! height (get-pixels)) height)
             ((eat-word "width") (set! width (get-pixels)) width)
             ((eat-word "depth") (get-pixels))
             (else (return)))
       (if %loop-returned %loop-result (%loop))))
   (emit "<img")
   (cond (height (emit " height=") (emit height)) (else false))
   (cond (width (emit " width=") (emit width)) (else false))
   (emit " src=\"")
   (do-img-src (fully-qualify-url (get-filename-possibly-braced)))
   (emit "\">")
   (ignorespaces)
   (get-ctl-seq)
   (ignorespaces)
   (get-ctl-seq)))

(define (display-index-entry s o)
 (for-each (lambda (c) (display (if (or (char=? c #\newline)) #\  c) o))
  (string->list s)))

(define (escape-opmac-index-entry x)
 (let ((y null))
   (let ((%dotimes-n (string-length x)) (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (if (>= i %dotimes-n) (return) false)
         (unless %loop-returned
           (let ((c (string-ref x i)))
             (case c
               ((#\") (set! y (cons c y)) (set! y (cons c y)) y)
               ((#\! #\@) (set! y (cons #\" y)) (set! y (cons c y)) y)
               (else (set! y (cons c y)) y))))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned %loop-result (%loop)))))
   (list->string (reverse y))))

(define (expand-html-index)
 (let ((s (get-peeled-group)))
   (let ((n (string->number s)))
     (let ((pageno (table-get n *index-table*)))
       (emit-page-node-link-start pageno
        (string-append *html-node-prefix* "index_" s))
       (emit pageno)
       (cond
        ((begin (set! *it* (table-get pageno *index-page-mention-alist*)) *it*)
         (let ((n (add1 *it*)))
           (emit (number-to-roman n))
           (table-put! pageno *index-page-mention-alist* n)
           (table-get pageno *index-page-mention-alist*)))
        (else (table-put! pageno *index-page-mention-alist* 1)
         (table-get pageno *index-page-mention-alist*)))
       (emit-link-stop)))))

(define (do-see-also)
 (let ((other-entry (get-group)))
   (get-group)
   (emit "<em>see also</em> ")
   (tex2page-string other-entry)))

(define (html-length s)
 (let ((n (string-length s)) (res 0) (i 0) (skip-tag false) (skip-entity false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((>= i n) (return res)) (else false))
       (unless %loop-returned
         (let ((c (string-ref s i)))
           (set! i (+ i 1))
           (cond
            (skip-tag
             (cond ((char=? c #\>) (set! skip-tag false) skip-tag)
                   (else false)))
            (skip-entity
             (cond ((char=? c #\;) (set! skip-entity false) skip-entity)
                   (else false)))
            ((char=? c #\<) (set! skip-tag true) skip-tag)
            ((char=? c #\&) (set! res (+ res 1)) (set! skip-entity true)
             skip-entity)
            (else (set! res (+ res 1)) res))))
       (if %loop-returned %loop-result (%loop))))))

(define (do-llap)
 (let ((txt (tex-string-to-html-string (get-group))))
   (let ((html-len (html-length txt)))
     (let ((txt-len (sp-to-pixels (tex-length html-len ':ex))))
       (emit "<span style=\"position: relative\">")
       (emit "<span style=\"position: absolute; left: -")
       (emit txt-len)
       (emit "pt\">")
       (emit txt)
       (emit "</span></span>")))))

(define (do-description-item) (do-end-para) (emit "</dd><dt>")
 (let ((thing (get-bracketed-text-if-any)))
   (cond
    (thing (set! thing (string-trim thing))
     (cond
      ((not (string=? thing "")) (bgroup) (emit "<b>") (tex2page-string thing)
       (emit "</b>") (egroup))
      (else false)))
    (else false)))
 (emit "</dt><dd>"))

(define (do-regular-item) (do-end-para) (emit "<li>") (do-para)
 (let ((thing (get-bracketed-text-if-any)))
   (cond
    (thing (emit "<b>") (bgroup) (tex2page-string thing) (egroup) (emit "</b>")
     (emit-nbsp 2))
    (else false))))

(define (do-plain-item n) (do-end-para) (emit-newline)
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

(define (do-textindent)
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

(define (do-proclaim)
 (let ((head (tex-string-to-html-string (get-till-char #\.))))
   (let ((body
          (begin (get-actual-char) (ignorespaces)
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
     (do-para))))

(define (do-item)
 (case (car *tabular-stack*)
   ((:description) (do-description-item))
   ((:itemize :enumerate) (do-regular-item))
   (else (do-plain-item 1))))

(define (do-itemize) (do-end-para)
 (set! *tabular-stack* (cons ':itemize *tabular-stack*)) (emit "<ul")
 (cond
  ((or (tex2page-flag-boolean "\\TIIPslides")
       (tex2page-flag-boolean "\\TZPslides"))
   (emit " class=incremental"))
  (else false))
 (emit ">") (emit-newline))

(define (do-enditemize) (do-end-para) (pop-tabular-stack ':itemize)
 (emit "</ul>") (do-noindent))

(define (do-enumerate) (do-end-para)
 (set! *tabular-stack* (cons ':enumerate *tabular-stack*)) (emit "<ol")
 (cond
  ((or (tex2page-flag-boolean "\\TIIPslides")
       (tex2page-flag-boolean "\\TZPslides"))
   (emit " class=incremental"))
  (else false))
 (emit ">") (emit-newline))

(define (do-endenumerate) (pop-tabular-stack ':enumerate) (do-end-para)
 (emit "</ol>") (do-noindent))

(define (do-opmac-list-style) (ignorespaces)
 (set! *opmac-list-style* (get-actual-char)) *opmac-list-style*)

(define (do-bigskip type) (do-end-para) (emit "<div class=")
 (emit
  (case type ((:medskip) "medskip") ((:bigskip) "bigskip") (else "smallskip")))
 (emit "></div>") (emit-newline)
 (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
 (set! *in-para-p* true) (emit-newline))

(define (do-hspace) (ignorespaces)
 (cond ((eqv? (snoop-actual-char) #\*) (get-actual-char)) (else false))
 (get-group) (emit-nbsp 3))

(define (do-vspace) (ignorespaces)
 (cond ((eqv? (snoop-actual-char) #\*) (get-actual-char)) (else false))
 (get-group) (do-bigskip ':vspace))

(define (do-htmlmathstyle)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return)) (else false))
          (case
              (string->symbol
               (string-append ":" (string-upcase (scm-get-token))))
            ((:image :display-image)
             (tex-def-0arg "\\TZPmathtext" "0")
             (tex-def-0arg "\\TZPmathimage" "1"))
            ((:no-image :no-display-image)
             (tex-def-0arg "\\TZPmathtext" "1")
             (tex-def-0arg "\\TZPmathimage" "0"))))
        (if %loop-returned %loop-result (%loop)))))))

(define (do-htmldoctype)
 (let ((d (get-peeled-group)))
   (cond ((string=? d "") (set! d 'none) d) (else false))
   (write-aux (quasiquote (!doctype (unquote d))))))

(define (do-htmlcolophon)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return)) (else false))
          (let ((directive
                 (string->symbol
                  (string-append ":" (string-upcase (scm-get-token))))))
            (!colophon directive)
            (write-aux (quasiquote (!colophon (unquote directive))))))
        (if %loop-returned %loop-result (%loop)))))))

(define (output-colophon)
 (let ((colophon-mentions-last-mod-time-p
        (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
            (tex2page-flag-boolean "\\TZPcolophontimestamp")))
       (colophon-mentions-tex2page-p
        (or (not (tex2page-flag-boolean "\\TZPcolophondisablecredit"))
            (tex2page-flag-boolean "\\TZPcolophoncredit")))
       (colophon-links-to-tex2page-website-p
        (or (not (tex2page-flag-boolean "\\TZPcolophondisableweblink"))
            (tex2page-flag-boolean "\\TZPcolophonweblink"))))
   (cond
    ((or colophon-mentions-last-mod-time-p colophon-mentions-tex2page-p)
     (do-end-para) (emit "<div class=\"rightline colophon\">")
     (cond
      ((and colophon-mentions-last-mod-time-p
            *last-modification-time*
            (> *last-modification-time* 0))
       (tex2page-string *last-modified*) (emit ": ")
       (emit (seconds-to-human-time *last-modification-time*)) (emit "<br>"))
      (else false))
     (cond
      (colophon-mentions-tex2page-p
       (emit "<div class=\"rightline advertisement\">")
       (tex2page-string *html-conversion-by*) (emit " ")
       (cond
        (colophon-links-to-tex2page-website-p
         (emit-link-start "http://ds26gte.github.io/tex2page/index.html"))
        (else false))
       (emit *tex-logo*) (emit "2page ") (emit *tex2page-version*)
       (cond (colophon-links-to-tex2page-website-p (emit-link-stop))
             (else false))
       (emit "</div>"))
      (else false))
     (emit "</div>") (emit-newline))
    (else false))))

(define (point-to-adjacent-pages)
 (let ((prev-page (if (= *html-page-count* 0) false (sub1 *html-page-count*))))
   (let ((next-page
          (if (= *html-page-count* *last-page-number*)
              false
              (add1 *html-page-count*))))
     (cond
      ((not (= *last-page-number* 0))
       (cond (prev-page (emit-page-link-start prev-page)) (else false))
       (emit "&#x3c;&#xb7;&#xb7;&#xb7;Prev ")
       (cond (prev-page (emit-link-stop)) (else false)) (emit "||")
       (cond (next-page (emit-page-link-start next-page)) (else false))
       (emit " Next&#xb7;&#xb7;&#xb7;&#x3e;")
       (cond (next-page (emit-link-stop)) (else false)))
      (else false)))))

(define (output-head-or-foot-line head-or-foot)
 (cond
  ((not
    (or (tex2page-flag-boolean "\\TIIPsinglepage")
        (tex2page-flag-boolean "\\TZPsinglepage")))
   (do-end-para) (emit "<div class=navigation>")
   (cond
    ((or (tex2page-flag-boolean "\\TIIPtexlayout")
         (tex2page-flag-boolean "\\TZPtexlayout"))
     (bgroup) (tex-let "\\folio" "\\TIIPfolio" false)
     (tex2page-string
      (if (eq? head-or-foot ':head) "\\the\\headline" "\\the\\footline"))
     (egroup))
    (else (output-navbar head-or-foot)))
   (emit "</div>") (emit-newline))
  (else false)))

(define (output-navbar head-or-foot)
 (let ((first-page-p (= *html-page-count* 0)))
   (let ((last-page-not-determined-p (< *last-page-number* 0)))
     (let ((last-page-p (= *html-page-count* *last-page-number*)))
       (let ((toc-page-p (and *toc-page* (= *html-page-count* *toc-page*))))
         (let ((index-page-p
                (and *index-page* (= *html-page-count* *index-page*))))
           (let ((prev-page (if first-page-p false (sub1 *html-page-count*))))
             (let ((next-page (if last-page-p false (add1 *html-page-count*))))
               (cond
                ((not
                  (and first-page-p
                       (or last-page-p
                           (and (eq? head-or-foot ':head)
                                last-page-not-determined-p))))
                 (emit "[") (emit *navigation-sentence-begin*) (emit "<span")
                 (cond (first-page-p (emit " class=disable")) (else false))
                 (emit ">")
                 (cond ((not first-page-p) (emit-page-link-start 0))
                       (else false))
                 (emit *navigation-first-name*)
                 (cond ((not first-page-p) (emit-link-stop)) (else false))
                 (emit ", ")
                 (cond ((not first-page-p) (emit-page-link-start prev-page))
                       (else false))
                 (emit *navigation-previous-name*)
                 (cond ((not first-page-p) (emit-link-stop)) (else false))
                 (emit "</span>") (emit "<span")
                 (cond (last-page-p (emit " class=disable")) (else false))
                 (emit ">")
                 (cond (first-page-p (emit "<span class=disable>"))
                       (else false))
                 (emit ", ")
                 (cond (first-page-p (emit "</span>")) (else false))
                 (cond ((not last-page-p) (emit-page-link-start next-page))
                       (else false))
                 (emit *navigation-next-name*)
                 (cond ((not last-page-p) (emit-link-stop)) (else false))
                 (emit "</span>") (emit *navigation-page-name*)
                 (cond
                  ((or *toc-page* *index-page*) (emit "<span")
                   (cond
                    ((or (and toc-page-p (not *index-page*) (not index-page-p))
                         (and index-page-p (not *toc-page*) (not toc-page-p)))
                     (emit " class=disable"))
                    (else false))
                   (emit ">; ") (emit-nbsp 2) (emit "</span>")
                   (cond
                    (*toc-page* (emit "<span")
                     (cond (toc-page-p (emit " class=disable")) (else false))
                     (emit ">")
                     (cond
                      ((not toc-page-p)
                       (emit-page-node-link-start *toc-page*
                        (string-append *html-node-prefix* "toc")))
                      (else false))
                     (emit *navigation-contents-name*)
                     (cond ((not toc-page-p) (emit-link-stop)) (else false))
                     (emit "</span>"))
                    (else false))
                   (cond
                    (*index-page* (emit "<span")
                     (cond (index-page-p (emit " class=disable")) (else false))
                     (emit ">") (emit "<span")
                     (cond
                      ((not (and *toc-page* (not toc-page-p)))
                       (emit " class=disable"))
                      (else false))
                     (emit ">")
                     (cond (*toc-page* (emit "; ") (emit-nbsp 2)) (else false))
                     (emit "</span>")
                     (cond
                      ((not index-page-p)
                       (emit-page-node-link-start *index-page*
                        (string-append *html-node-prefix* "index_start")))
                      (else false))
                     (emit *navigation-index-name*)
                     (cond ((not index-page-p) (emit-link-stop)) (else false))
                     (emit "</span>"))
                    (else false)))
                  (else false))
                 (emit *navigation-sentence-end*) (emit "]"))
                (else false))))))))))

(define (do-eject) (ignorespaces)
 (cond
  ((or (tex2page-flag-boolean "\\TIIPslides")
       (tex2page-flag-boolean "\\TZPslides"))
   (do-end-para) (emit "</div>") (emit-newline) (emit "<div class=slide>")
   (do-para))
  ((or (tex2page-flag-boolean "\\TIIPsinglepage")
       (tex2page-flag-boolean "\\TZPsinglepage"))
   true)
  (else
   (cond
    ((not
      (and (not (snoop-actual-char))
           (eqv? *current-source-file* *main-tex-file*)))
     (cond
      ((not (> *last-page-number* 0))
       (flag-missing-piece ':last-modification-time))
      (else false))
     (do-end-page)
     (tex-def-count 0
      (begin (set! *html-page-count* (+ *html-page-count* 1))
       *html-page-count*)
      true)
     (set! *html-page*
      (string-append *aux-dir/* *jobname* *html-page-suffix*
       (write-to-string *html-page-count*) *output-extension*))
     (set! *html*
      (make-ostream* ':stream
       (let* ((%f *html-page*)
              (%ee (list ':direction ':output ':if-exists ':supersede))
              (%direction (memv ':direction %ee))
              (%if-exists (memv ':if-exists %ee))
              (%if-does-not-exist ':error)
              (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
         (when %direction (set! %direction (cadr %direction)))
         (when %if-exists (set! %if-exists (cadr %if-exists)))
         (when %if-does-not-exist-from-user
           (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
         (cond
          ((eqv? %direction ':output)
           (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
             (delete-file %f))
           (open-output-file %f))
          ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
          (else (open-input-file %f))))))
     (do-start))
    (else false)))))

(define (output-html-preamble)
 (cond
  ((string? *doctype*) (emit "<!DOCTYPE ") (emit *doctype*) (emit ">")
   (emit-newline))
  (else false))
 (emit "<html lang=") (emit (resolve-defs "\\TZPlang")) (emit ">")
 (emit-newline) (emit "<!--") (emit-newline) (emit "Generated from ")
 (emit *main-tex-file*) (emit " by tex2page, ") (emit "v. ")
 (emit *tex2page-version*) (emit-newline) (emit *tex2page-copyright-notice*)
 (emit-newline) (emit "(running on ") (emit *scheme-version*) (emit ")")
 (emit-newline) (emit *tex2page-website*) (emit-newline) (emit "-->")
 (emit-newline) (emit "<head>") (emit-newline)
 (emit "<meta charset=\"utf-8\">") (emit-newline) (output-external-title)
 (link-stylesheets) (link-scripts)
 (emit "<meta name=robots content=\"index,follow\">") (emit-newline)
 (for-each emit *html-head*) (emit "</head>") (emit-newline) (emit "<body>")
 (emit-newline) (emit "<div")
 (cond
  ((or (tex2page-flag-boolean "\\TIIPslides")
       (tex2page-flag-boolean "\\TZPslides"))
   (emit " class=slide"))
  (else false))
 (emit ">") (emit-newline))

(define (output-html-postamble) (do-end-para) (emit "</div>") (emit-newline)
 (emit "</body>") (emit-newline) (emit "</html>") (emit-newline))

(define (redirect-if-needed)
 (cond
  (*redirect-url* (emit "If not redirected in ") (emit *redirect-delay*)
   (emit " sec, go to ") (emit-link-start (fully-qualify-url *redirect-url*))
   (emit *redirect-url*) (emit-link-stop))
  (else false)))

(define (check-tex2page-lisp)
 (let ((cl-p (not null))
       (doc-expects-cl-p (tex2page-flag-boolean "\\TZPcommonlisp")))
   (cond
    ((not (eqv? cl-p doc-expects-cl-p)) (write-log ':separation-newline)
     (write-log "! Document ") (write-log *main-tex-file*)
     (write-log " appears to require ")
     (write-log (if doc-expects-cl-p "Common Lisp" "Scheme"))
     (write-log " version of TeX2page."))
    (else false))))

(define (do-start) (check-tex2page-lisp) (set! *footnote-list* null)
 (output-html-preamble) (output-head-or-foot-line ':head) (do-para)
 (redirect-if-needed))

(define (do-end-page) (do-end-para) (output-footnotes) (do-bigskip ':smallskip)
 (output-head-or-foot-line ':foot) (do-para)
 (let ((colophon-on-last-page-p
        (tex2page-flag-boolean "\\TZPcolophonlastpage")))
   (cond
    ((or (and (not colophon-on-last-page-p) (= *html-page-count* 0))
         (and colophon-on-last-page-p
              (= *html-page-count* *last-page-number*)))
     (output-colophon))
    (else false)))
 (output-html-postamble) (write-log #\[) (write-log *html-page-count*)
 (write-log #\]) (write-log ':separation-space)
 (close-html-output-stream *html*))

(define (close-all-open-streams)
 (cond
  (*aux-stream*
   (let ((%close-port-arg *aux-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (cond
  (*css-stream*
   (let ((%close-port-arg *css-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (cond
  (*index-stream*
   (let ((%close-port-arg *index-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (cond
  (*label-stream*
   (let ((%close-port-arg *label-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (cond
  (*bib-aux-stream*
   (let ((%close-port-arg *bib-aux-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (cond
  (*verb-stream*
   (let ((%close-port-arg *verb-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (table-for-each
  (lambda (k v)
    (cond
     ((not (eq? v ':free))
      (let ((%close-port-arg v))
        ((if (input-port? %close-port-arg) close-input-port close-output-port)
         %close-port-arg)))
     (else false)))
  *input-streams*)
 (table-for-each
  (lambda (k v)
    (cond
     ((not (eq? v ':free))
      (let ((%close-port-arg v))
        ((if (input-port? %close-port-arg) close-input-port close-output-port)
         %close-port-arg)))
     (else false)))
  *output-streams*))

(define (output-stats) (write-log ':separation-newline)
 (cond
  (*main-tex-file* (write-log "Output written on ") (write-log *aux-dir/*)
   (write-log *jobname*) (write-log *output-extension*)
   (cond ((> *html-page-count* 0) (write-log ", ...")) (else false))
   (write-log " (") (write-log (add1 *html-page-count*)) (write-log " page")
   (cond ((not (= *html-page-count* 0)) (write-log #\s)) (else false))
   (cond
    ((> *img-file-tally* 0) (write-log ", ") (write-log *img-file-tally*)
     (write-log " image")
     (cond ((not (= *img-file-tally* 1)) (write-log #\s)) (else false)))
    (else false))
   (write-log ")."))
  (else (write-log "No pages of output.")))
 (write-log #\newline)
 (cond
  (*log-stream*
   (let ((%close-port-arg *log-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (display "Transcript written on ") (display *log-file*) (display ".")
 (newline))

(define (do-endinput) (toss-back-char *invisible-space*)
 (toss-back-string "\\TIIPendinput"))

(define (do-bye) (check-tex2page-lisp) (note-down-tex2page-flags)
 (cond
  ((not (null? *tex-if-stack*))
   (let ((n (length *tex-if-stack*)))
     (trace-if true "(\\end occurred when " n " \\if"
      (if (> n 1) "s were" " was") " incomplete)")))
  (else false))
 (cond
  ((not (null? *tex-env*))
   (trace-if true "\\end occurred inside a group at level "
    (length *tex-env*)))
  (else false))
 (perform-postludes)
 (cond
  ((not (or (>= *last-page-number* 0) (= *html-page-count* 0)))
   (flag-missing-piece ':last-page))
  (else false))
 (!last-page-number *html-page-count*)
 (write-aux (quasiquote (!last-page-number (unquote *last-page-number*))))
 (do-end-page)
 (cond
  (*last-modification-time*
   (write-aux
    (quasiquote
     (!last-modification-time (unquote *last-modification-time*) 1900))))
  (else false))
 (for-each (lambda (th) (th)) *afterbye*) (close-all-open-streams)
 (call-external-programs-if-necessary)
 (show-unresolved-xrefs-and-missing-pieces))

(define (set-text-width)
 (let ((hsize
        (cond
         ((begin (set! *it* (find-def "\\TZPhsize")) *it*)
          (tex2page-string
           (string-append "\\TIIPhsize=" (tdef*-expansion *it*)))
          (the-dimen "\\TIIPhsize"))
         ((or (tex2page-flag-boolean "\\TIIPtexlayout")
              (tex2page-flag-boolean "\\TZPtexlayout"))
          (the-dimen "\\hsize"))
         (else false))))
   (cond
    (hsize (display "body { max-width: " *css-stream*)
     (display (sp-to-pixels hsize) *css-stream*) (display "pt; }" *css-stream*)
     (newline *css-stream*))
    (else false))))

(define (note-down-tex2page-flags)
 (write-aux (quasiquote (!lang (unquote (resolve-defs "\\TZPlang")))))
 (write-aux (quasiquote (!head-line (unquote (the-toks "\\headline")))))
 (write-aux (quasiquote (!foot-line (unquote (the-toks "\\footline")))))
 (cond
  ((begin (set! *it* (find-def "\\TZPtitle")) *it*)
   (let ((d *it*))
     (write-aux
      (quasiquote
       (!preferred-title
        (unquote (tex-string-to-html-string (tdef*-expansion d))))))))
  (else false))
 (cond
  ((tex2page-flag-boolean "\\TZPcolophonlastpage")
   (write-aux (quasiquote (!colophon ':last-page))))
  (else false))
 (cond
  ((or (tex2page-flag-boolean "\\TZPcolophondisabletimestamp")
       (not (tex2page-flag-boolean "\\TZPcolophontimestamp")))
   (write-aux (quasiquote (!colophon ':no-timestamp))))
  (else false))
 (cond
  ((or (tex2page-flag-boolean "\\TZPcolophondisablecredit")
       (not (tex2page-flag-boolean "\\TZPcolophoncredit")))
   (write-aux (quasiquote (!colophon ':dont-credit-tex2page))))
  (else false))
 (cond
  ((or (tex2page-flag-boolean "\\TZPcolophondisableweblink")
       (not (tex2page-flag-boolean "\\TZPcolophonweblink")))
   (write-aux (quasiquote (!colophon ':dont-link-to-tex2page-website))))
  (else false))
 (cond
  ((begin (set! *it* (ctl-seq-no-arg-expand-once "\\TZPredirect")) *it*)
   (cond ((not *redirect-url*) (flag-missing-piece ':html-head)) (else false))
   (let ((url *it*)
         (seconds (ctl-seq-no-arg-expand-once "\\TZPredirectseconds")))
     (write-aux
      (quasiquote (!html-redirect (unquote url) (unquote seconds))))))
  (else false))
 (cond
  ((tex2page-flag-boolean "\\TZPslides") (write-aux '(!slides))
   (write-aux '(!single-page))
   (let ((slidy-css-file "slidy.css"))
     (cond
      ((not (file-exists? slidy-css-file))
       (set! slidy-css-file
        "http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css")
       slidy-css-file)
      (else false))
     (cond ((null? *stylesheets*) (flag-missing-piece ':stylesheets))
           (else false))
     (write-aux (quasiquote (!stylesheet (unquote slidy-css-file)))))
   (let ((slidy-js-file "slidy.js"))
     (cond
      ((not (file-exists? slidy-js-file))
       (set! slidy-js-file
        "http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js")
       slidy-js-file)
      (else false))
     (cond ((null? *scripts*) (flag-missing-piece ':scripts)) (else false))
     (write-aux (quasiquote (!script (unquote slidy-js-file))))))
  (else false))
 (cond ((tex2page-flag-boolean "\\TZPsinglepage") (write-aux '(!single-page)))
       (else false))
 (cond
  ((tex2page-flag-boolean "\\TZPtexlayout") (write-aux '(!tex-like-layout))
   (newline *css-stream*) (display "body { margin-top: " *css-stream*)
   (display (sp-to-ems (+ (tex-length 0.5 ':in) (the-dimen "\\voffset")))
    *css-stream*)
   (display "em; }" *css-stream*) (newline *css-stream*)
   (display "body { margin-left: " *css-stream*)
   (display (sp-to-ems (+ (tex-length 0.8 ':in) (the-dimen "\\hoffset")))
    *css-stream*)
   (display "em; }" *css-stream*) (newline *css-stream*)
   (cond
    ((or (tex2page-flag-boolean "\\TZPrightjustify")
         (not (tex2page-flag-boolean "\\TZPraggedright")))
     (display "body { text-align: justify; }" *css-stream*)
     (newline *css-stream*))
    (else false))
   (display "p { margin-bottom: 0pt; }" *css-stream*) (newline *css-stream*)
   (display "p { text-indent: " *css-stream*)
   (display (sp-to-pixels (the-dimen "\\parindent")) *css-stream*)
   (display "pt; }" *css-stream*) (newline *css-stream*)
   (display "p { margin-top: " *css-stream*)
   (display (sp-to-pixels (the-dimen "\\parskip")) *css-stream*)
   (display "pt; }" *css-stream*) (newline *css-stream*)
   (display ".mathdisplay { margin-top: " *css-stream*)
   (display (sp-to-pixels (the-dimen "\\abovedisplayskip")) *css-stream*)
   (display "pt; margin-bottom: " *css-stream*)
   (display (sp-to-pixels (the-dimen "\\belowdisplayskip")) *css-stream*)
   (display "pt; }" *css-stream*) (newline *css-stream*)
   (display ".navigation { color: black; font-style: normal; }" *css-stream*)
   (newline *css-stream*))
  (else false))
 (set-text-width)
 (cond
  ((not (tex2page-flag-boolean "\\TZPtextext"))
   (write-aux (quasiquote (!tex-text 1))))
  (else false)))

(define (insert-missing-end) (write-log ':separation-newline)
 (write-log "! Missing \\end inserted.") (write-log ':separation-newline))

(define do-diacritic
 (let ((top-diacritics
        '(:grave :acute :circumflex :umlaut :tilde :macron :breve :hacek
          :hungarianumlaut :ring)))
   (lambda (diac)
     (let ((x (get-token-or-peeled-group)))
       (cond ((and (string=? x "\\i") (member diac top-diacritics)) (emit "i"))
             ((and (string=? x "\\j") (member diac top-diacritics)) (emit "j"))
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
        (else (error 'ecase "0xdeadc0de")))))))

(define (do-backslash-equal)
 (cond
  ((not
    (and (not (null? *tabular-stack*)) (eqv? (car *tabular-stack*) ':tabbing)))
   (do-diacritic ':macron))
  (else false)))

(define (do-mathdg)
 (fluid-let
  ((*math-mode-p* true) (*in-display-math-p* true) (*tabular-stack* null)
   (*ligatures-p* false))
  (do-end-para) (emit "<div class=") (emit *display-justification*)
  (emit "><table><tr><td>") (tex2page-string (get-group))
  (emit "</td></tr></table></div>") (do-para)))

(define (do-mathg)
 (fluid-let
  ((*math-mode-p* true) (*in-display-math-p* false) (*tabular-stack* null)
   (*ligatures-p* false))
  (tex2page-string (get-group))))

(define (dump-tex-preamble o)
 (case *tex-format*
   ((:latex)
    (display "\\documentclass{" o)
    (display (if *using-chapters-p* "report" "article") o)
    (display "}" o)
    (newline o)
    (display *imgpreamble* o)
    (newline o)
    (cond
     ((member ':includegraphics *imgpreamble-inferred*)
      (display "\\ifx\\includegraphics\\UNDEFINED" o)
      (display "\\usepackage{graphicx}\\fi" o) (newline o))
     (else false))
    (cond
     ((member ':epsfbox *imgpreamble-inferred*)
      (display "\\ifx\\epsfbox\\UNDEFINED" o)
      (display "\\usepackage{epsfig}\\fi" o) (newline o))
     (else false))
    (display "\\thispagestyle{empty}" o)
    (newline o)
    (display "\\begin{document}" o)
    (newline o))
   (else
    (display *imgpreamble* o)
    (newline o)
    (cond
     ((member ':includegraphics *imgpreamble-inferred*)
      (display "\\ifx\\resetatcatcode\\UNDEFINED" o)
      (display "\\input miniltx \\fi" o) (newline o)
      (display "\\ifx\\includegraphics\\UNDEFINED" o)
      (display "\\input graphicx.sty \\fi" o) (newline o))
     (else false))
    (cond
     ((member ':epsfbox *imgpreamble-inferred*)
      (display "\\ifx\\epsfbox\\UNDEFINED" o)
      (display "\\ifx\\directlua\\UNDEFINED\\input epsf \\else" o)
      (display "\\input miniltx \\input graphicx " o)
      (display "\\def\\epsfbox#1{\\convertMPtoPDF{#1}{1}{1}}" o)
      (display "\\fi\\fi" o) (newline o))
     (else false))
    (display "\\nopagenumbers" o)
    (newline o))))

(define (dump-tex-postamble o)
 (case *tex-format*
   ((:latex) (display "\\end{document}" o) (newline o))
   (else (display "\\bye" o) (newline o))))

(define (skipping-img-file) (set! *img-file-count* (+ *img-file-count* 1))
 *img-file-count*)

(define (next-html-image-file-stem)
 (set! *img-file-count* (+ *img-file-count* 1))
 (string-append *subjobname* *img-file-suffix*
  (write-to-string *img-file-count*)))

(define (call-with-html-image-stream p . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (alt false))
   (when (< 0 %lambda-rest-arg-len) (set! alt (list-ref %lambda-rest-arg 0)))
   (let ((img-file-stem (next-html-image-file-stem)))
     (let ((aux-tex-file (string-append img-file-stem ".tex")))
       (let ((o
              (let* ((%f aux-tex-file)
                     (%ee (list ':direction ':output ':if-exists ':supersede))
                     (%direction (memv ':direction %ee))
                     (%if-exists (memv ':if-exists %ee))
                     (%if-does-not-exist ':error)
                     (%if-does-not-exist-from-user
                      (memv ':if-does-not-exist %ee)))
                (when %direction (set! %direction (cadr %direction)))
                (when %if-exists (set! %if-exists (cadr %if-exists)))
                (when %if-does-not-exist-from-user
                  (set! %if-does-not-exist
                   (cadr %if-does-not-exist-from-user)))
                (cond
                 ((eqv? %direction ':output)
                  (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
                    (delete-file %f))
                  (open-output-file %f))
                 ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
                 (else (open-input-file %f))))))
         (let ((%with-open-file-res
                (begin (dump-tex-preamble o) (p o) (dump-tex-postamble o))))
           (cond
            (o ((if (input-port? o) close-input-port close-output-port) o))
            (else false))
           %with-open-file-res))
       (tex-to-img img-file-stem)
       (source-img-file img-file-stem alt)))))

(define (do-math-fragment s . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (display-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! display-p (list-ref %lambda-rest-arg 0)))
   (cond
    (display-p (emit "<div class=\"mathdisplay ")
     (emit *display-justification*) (emit "\">"))
    (else false))
   (let ((old-math-mode-p *math-mode-p*)
         (old-in-display-math-p *in-display-math-p*)
         (old-tabular-stack *tabular-stack*))
     (set! *math-mode-p* true)
     (set! *in-display-math-p* display-p)
     (set! *tabular-stack* null)
     (cond
      (display-p
       (emit "<table style=\"margin-left:auto; margin-right:auto\"><tr><td>"))
      (else false))
     (bgroup)
     (toss-back-char #\})
     (toss-back-string s)
     (add-aftergroup-to-top-frame
      (lambda ()
        (set! *math-mode-p* old-math-mode-p)
        (set! *in-display-math-p* old-in-display-math-p)
        (set! *tabular-stack* old-tabular-stack)
        (cond (display-p (emit "</td></tr></table>")) (else false))
        (cond (display-p (emit "</div>") (do-noindent)) (else false)))))))

(define (do-display-math tex-string) (do-end-para)
 (if
  (and
   (or (not (tex2page-flag-boolean "\\TZPmathtext"))
       (tex2page-flag-boolean "\\TZPmathimage"))
   (not *temporarily-use-utf8-for-math-p*))
  (begin (emit "<div class=\"mathdisplay ") (emit *display-justification*)
   (emit "\">")
   (call-with-html-image-stream
    (lambda (o) (display "$$" o) (display tex-string o) (display "$$" o))
    tex-string)
   (emit "</div>") (do-noindent))
  (do-math-fragment tex-string ':display)))

(define (tex-math-delim-string type)
 (let ((top null) (mid null) (bot null) (ext null))
   (case type
     ((:lparen)
      (set! top "&#x239b;")
      (set! bot "&#x239d;")
      (set! ext "&#x239c;")
      (set! mid ext)
      mid)
     ((:lbrack)
      (set! top "&#x23a1;")
      (set! bot "&#x23a3;")
      (set! ext "&#x23a2;")
      (set! mid ext)
      mid)
     ((:lbrace)
      (set! top "&#x23a7;")
      (set! mid "&#x23a8;")
      (set! bot "&#x23a9;")
      (set! ext "&#x23aa;")
      ext)
     ((:lvert)
      (set! ext "&#x239c;")
      (set! top ext)
      (set! mid ext)
      (set! bot ext)
      bot)
     ((:nulldelim)
      (set! ext "")
      (set! top ext)
      (set! mid ext)
      (set! bot ext)
      bot)
     ((:rparen)
      (set! top "&#x239e;")
      (set! bot "&#x23a0;")
      (set! ext "&#x239f;")
      (set! mid ext)
      mid)
     ((:rbrack)
      (set! top "&#x23a4;")
      (set! bot "&#x23a6;")
      (set! ext "&#x23a5;")
      (set! mid ext)
      mid)
     ((:rbrace)
      (set! top "&#x23ab;")
      (set! mid "&#x23ac;")
      (set! bot "&#x23ad;")
      (set! ext "&#x23ae;")
      ext)
     ((:rvert)
      (set! ext "&#x239f;")
      (set! top ext)
      (set! mid ext)
      (set! bot ext)
      bot)
     (else (error 'ecase "0xdeadc0de")))
   (string-append "<table class=mathdelim><tr><td>" top "</td></tr>"
    (cond
     ((odd? *math-height*)
      (string-append
       (let ((r ""))
         (let ((%dotimes-n (/ (sub1 *math-height*) 2)) (i 0))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (if (>= i %dotimes-n) (return r) false)
               (unless %loop-returned
                 (set! r (string-append r "<tr><td>" ext "</td></tr>"))
                 r)
               (unless %loop-returned (set! i (+ i 1)))
               (if %loop-returned %loop-result (%loop))))))
       "<tr><td>" mid "</td></tr>"
       (let ((r ""))
         (let ((%dotimes-n (/ (sub1 *math-height*) 2)) (i 0))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (if (>= i %dotimes-n) (return r) false)
               (unless %loop-returned
                 (set! r (string-append r "<tr><td>" ext "</td></tr>"))
                 r)
               (unless %loop-returned (set! i (+ i 1)))
               (if %loop-returned %loop-result (%loop))))))))
     (else
      (let ((r ""))
        (let ((%dotimes-n *math-height*) (i 0))
          (let* ((%loop-returned false)
                 (%loop-result 0)
                 (return
                  (lambda %args
                    (set! %loop-returned true)
                    (set! %loop-result (and (pair? %args) (car %args))))))
            (let %loop
              ()
              (if (>= i %dotimes-n) (return r) false)
              (unless %loop-returned
                (set! r (string-append r "<tr><td>" ext "</td></tr>"))
                r)
              (unless %loop-returned (set! i (+ i 1)))
              (if %loop-returned %loop-result (%loop))))))))
    "<tr><td>" bot "</td></tr></table>")))

(define (tex-math-string-to-html-string s)
 (fluid-let ((*html* (make-html-output-stream)))
  (call-with-input-string/buffered ""
   (lambda () (do-math-fragment s false) (generate-html)))
  (html-output-stream-to-string *html*)))

(define (do-intext-math tex-string)
 (fluid-let ((*math-needs-image-p* false))
  (let ((html-string (tex-math-string-to-html-string tex-string)))
    (if
     (and
      (or (not (tex2page-flag-boolean "\\TZPmathtext"))
          (tex2page-flag-boolean "\\TZPmathimage"))
      *math-needs-image-p*
      (not *temporarily-use-utf8-for-math-p*))
     (call-with-html-image-stream
      (lambda (o) (display #\$ o) (display tex-string o) (display #\$ o))
      tex-string)
     (emit html-string)))))

(define (do-mathp)
 (call-with-html-image-stream
  (lambda (o) (display #\$ o) (display (get-group) o) (display #\$ o))))

(define (do-math)
 (let ((display-p false))
   (cond
    ((eqv? (snoop-actual-char) #\$) (set! display-p true) (get-actual-char))
    (else false))
   (let ((o (open-output-string)))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (dump-till-char #\$ o)
         (cond ((not display-p) (return))
               (else
                (let ((c (get-actual-char)))
                  (cond
                   ((not c)
                    (terror 'do-math "Display math should end with $$."))
                   ((char=? c #\$) (return))
                   (else (display #\$ o) (display c o))))))
         (if %loop-returned %loop-result (%loop))))
     ((if display-p do-display-math do-intext-math) (get-output-string o)))))

(define (dump-till-char d o)
 (let ((nesting 0) (escape-p false) (c false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (set! c (get-actual-char))
       (cond ((not c) (terror 'dump-till-char "Missing " d ".")) (else false))
       (cond ((and (char=? c d) (= nesting 0)) (return)) (else false))
       (unless %loop-returned (display c o))
       (unless %loop-returned
         (cond (escape-p (set! escape-p false) escape-p)
               ((char=? c #\{) (set! nesting (+ nesting 1)) nesting)
               ((char=? c #\}) (set! nesting (- nesting 1)) nesting)
               ((char=? c #\\) (set! escape-p true) escape-p)))
       (if %loop-returned %loop-result (%loop))))))

(define (dump-till-ctl-seq cs o)
 (fluid-let ((*not-processing-p* true))
  (let ((nesting 0))
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (let ((c (snoop-actual-char)))
          (cond ((not c) (terror 'dump-till-ctl-seq)) (else false))
          (cond
           ((= (catcode c) **escape**)
            (let ((x (get-ctl-seq)))
              (if (string=? x cs) (return) (display x o))))
           (else (display (get-actual-char) o)
            (cond ((char=? c #\{) (set! nesting (+ nesting 1)) nesting)
                  ((char=? c #\}) (set! nesting (- nesting 1)) nesting)))))
        (if %loop-returned %loop-result (%loop)))))))

(define (dump-till-end-env env o)
 (let ((endenv (string-append "\\end" env)))
   (let ((endenv-prim (find-corresp-prim endenv)))
     (let ((endenv-prim-th (find-corresp-prim-thunk endenv)))
       (fluid-let ((*not-processing-p* true))
        (let ((brace-nesting 0))
          (let ((env-nesting 0))
            (let ((c false))
              (let* ((%loop-returned false)
                     (%loop-result 0)
                     (return
                      (lambda %args
                        (set! %loop-returned true)
                        (set! %loop-result (and (pair? %args) (car %args))))))
                (let %loop
                  ()
                  (set! c (snoop-actual-char))
                  (cond ((not c) (terror 'dump-till-end-env env)) (else false))
                  (cond
                   ((= (catcode c) **escape**)
                    (let ((x (get-ctl-seq)))
                      (cond
                       ((string=? (find-corresp-prim x) endenv-prim) (return))
                       ((string=? x "\\begin") (display x o)
                        (let ((g (get-grouped-environment-name-if-any)))
                          (cond
                           (g (display #\{ o) (display g o) (display #\} o))
                           (else false))
                          (cond
                           ((and g (string=? g env))
                            (set! env-nesting (+ env-nesting 1)) env-nesting)
                           (else false))))
                       ((string=? x "\\end")
                        (let ((g (get-grouped-environment-name-if-any)))
                          (cond
                           ((and g
                                 (or *dumping-nontex-p* (= env-nesting 0))
                                 (let ((endg (string-append "\\end" g)))
                                   (or
                                    (string=? (find-corresp-prim endg)
                                     endenv-prim)
                                    (eqv? (find-corresp-prim-thunk endg)
                                     endenv-prim-th))))
                            (return))
                           (else (display x o)
                            (cond
                             (g (display #\{ o) (display g o) (display #\} o))
                             (else false))
                            (cond
                             ((and g (string=? g env))
                              (set! env-nesting (- env-nesting 1)) env-nesting)
                             (else false))))))
                       (else (display x o)))))
                   ((and (= (catcode c) **comment**) (not *dumping-nontex-p*))
                    (do-comment) (write-char #\% o) (newline o))
                   (else (write-char (get-actual-char) o)
                    (cond
                     ((char=? c #\{) (set! brace-nesting (+ brace-nesting 1))
                      brace-nesting)
                     ((char=? c #\}) (set! brace-nesting (- brace-nesting 1))
                      brace-nesting))))
                  (if %loop-returned %loop-result (%loop))))))))))))

(define (dump-imgdef f)
 (let ((aux-tex-file (string-append f ".tex")))
   (let ((o
          (let* ((%f aux-tex-file)
                 (%ee (list ':direction ':output ':if-exists ':supersede))
                 (%direction (memv ':direction %ee))
                 (%if-exists (memv ':if-exists %ee))
                 (%if-does-not-exist ':error)
                 (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
            (when %direction (set! %direction (cadr %direction)))
            (when %if-exists (set! %if-exists (cadr %if-exists)))
            (when %if-does-not-exist-from-user
              (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
            (cond
             ((eqv? %direction ':output)
              (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
                (delete-file %f))
              (open-output-file %f))
             ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
             (else (open-input-file %f))))))
     (let ((%with-open-file-res
            (begin (dump-tex-preamble o) (display (ungroup (get-group)) o)
             (dump-tex-postamble o))))
       (cond (o ((if (input-port? o) close-input-port close-output-port) o))
             (else false))
       %with-open-file-res))))

(define (do-img-preamble)
 (set! *imgpreamble*
  (let ((%fluid-var-*not-processing-p* true) (r *imgpreamble*))
    (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (snoop-actual-char)))
           (cond ((not c) (terror 'do-img-preamble "Missing \\endimgpreamble"))
                 (else false))
           (cond
            ((= (catcode c) **escape**)
             (let ((x (get-ctl-seq)))
               (cond
                ((member x
                         '("\\endimgpreamble" "\\endgifpreamble"
                           "\\endmathpreamble"))
                 (return r))
                (else false))
               (set! r (string-append r x))
               r))
            (else (get-actual-char) (set! r (string-append r (string c))) r)))
         (if %loop-returned %loop-result (%loop)))))))
 *imgpreamble*)

(define (do-open-stream type)
 (let ((n (get-number)))
   (let ((f (get-filename)))
     (let ((frame (top-texframe)))
       (let ((sl
              (if (eq? type ':out)
                  (texframe*-ostreams frame)
                  (texframe*-istreams frame))))
         (let ((c (table-get n sl)))
           (cond ((not (eq? c ':free)) (terror 'do-open-stream)) (else false))
           (table-put! n sl
            (case type
              ((:out)
               (set! f (add-dot-tex-if-no-extension-provided f))
               (let* ((%f f)
                      (%ee (list ':direction ':output ':if-exists ':supersede))
                      (%direction (memv ':direction %ee))
                      (%if-exists (memv ':if-exists %ee))
                      (%if-does-not-exist ':error)
                      (%if-does-not-exist-from-user
                       (memv ':if-does-not-exist %ee)))
                 (when %direction (set! %direction (cadr %direction)))
                 (when %if-exists (set! %if-exists (cadr %if-exists)))
                 (when %if-does-not-exist-from-user
                   (set! %if-does-not-exist
                    (cadr %if-does-not-exist-from-user)))
                 (cond
                  ((eqv? %direction ':output)
                   (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
                     (delete-file %f))
                   (open-output-file %f))
                  ((and (not %if-does-not-exist) (not (file-exists? %f)))
                   false)
                  (else (open-input-file %f)))))
              ((:in)
               (set! f (actual-tex-filename f))
               (make-istream* ':stream (open-input-file f)))
              (else (error 'ecase "0xdeadc0de"))))
           (table-get n sl)))))))

(define (do-close-stream type)
 (let ((frame (top-texframe)))
   (let ((sl
          (if (eqv? type ':out)
              (texframe*-ostreams frame)
              (texframe*-istreams frame))))
     (let ((o (get-number)))
       (let ((c (table-get o sl)))
         (cond ((eq? c ':free) (terror 'do-close-stream)) (else false))
         (let ((%close-port-arg
                (case type
                  ((:out) c)
                  ((:in) (istream*-stream c))
                  (else (error 'ecase "0xdeadc0de")))))
           ((if (input-port? %close-port-arg)
                close-input-port
                close-output-port)
            %close-port-arg))
         (table-put! o sl ':free)
         (table-get o sl))))))

(define (tex-write-output-string s)
 (fluid-let
  ((*html* (make-html-output-stream)) (*outputting-to-non-html-p* true))
  (call-with-input-string/buffered s
   (lambda ()
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (snoop-actual-char)))
           (cond ((not c) (return)) (else false))
           (case c
             ((#\\) (do-tex-ctl-seq (get-ctl-seq)))
             (else (emit-html-char (get-actual-char)))))
         (if %loop-returned %loop-result (%loop))))))
  (html-output-stream-to-string *html*)))

(define (do-write-aux o)
 (let ((output (tex-write-output-string (get-peeled-group))))
   (cond ((and (= o 18) *enable-write-18-p*) (system output))
         ((member o '(16 18)) (write-log output)
          (write-log ':separation-space))
         ((begin (set! *it* (find-ostream o)) *it*)
          (let ((p *it*))
            (cond ((eq? p ':free) (terror 'do-write-aux)) (else false))
            (display output p)
            (display #\  p)))
         (else (terror 'do-write-aux)))))

(define (do-wlog)
 (let ((output (tex-write-output-string (get-peeled-group))))
   (write-log output true)))

(define (do-write) (do-write-aux (get-number)))

(define (do-message) (do-write-aux 16))

(define (read-tex-line p)
 (fluid-let ((*current-tex2page-input* p))
  (let ((r null))
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return (if (null? r) c (list->string (reverse r)))))
                (else false))
          (cond
           ((char=? c #\newline) (get-actual-char)
            (return (list->string (reverse r))))
           (else false))
          (cond
           ((char=? c #\{)
            (return (string-append (list->string (reverse r)) (get-group))))
           (else false))
          (set! r (cons (get-actual-char) r))
          r)
        (if %loop-returned %loop-result (%loop)))))))

(define (do-read . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (let ((i (get-number)))
     (let ((x (begin (get-to) (get-ctl-seq))))
       (let ((p false))
         (cond
          ((member i '(-1 16))
           (set! p (make-istream* ':stream (current-input-port)))
           (cond ((not (= i -1)) (write-log x) (write-log #\=)) (else false)))
          ((begin (set! *it* (find-istream i)) *it*) (set! p *it*)
           (cond ((eq? p ':free) (terror 'do-read)) (else false)))
          (else (terror 'do-read)))
         ((if globalp tex-gdef-0arg tex-def-0arg) x
          (let ((line (read-tex-line p)))
            (if (not line) "" line))))))))

(define (do-typein)
 (let ((ctlseq (get-bracketed-text-if-any))
       (p (make-istream* ':stream (current-input-port))))
   (write-log ':separation-newline)
   (write-log (tex-string-to-html-string (get-group)))
   (write-log ':separation-newline)
   (write-log (or ctlseq "\\@typein"))
   (write-log #\=)
   (let ((l (read-tex-line p)))
     (cond ((not l) (set! l "") l) (else false))
     (cond (ctlseq (tex-def-0arg ctlseq l)) (else (tex2page-string l))))))

(define (do-ifeof)
 (let ((i (get-number)))
   (let ((c (table-get i *input-streams*)))
     (cond ((eq? c ':free) (terror 'do-ifeof)) (else false))
     (if
      (not
       (let* ((%read-char-port c)
              (%read-char-res
               (if %read-char-port (read-char %read-char-port) (read-char))))
         (when (eof-object? %read-char-res) (set! %read-char-res false))
         %read-char-res))
      (do-iftrue)
      (do-iffalse)))))

(define (do-iffalse) (set! *tex-if-stack* (cons false *tex-if-stack*))
 *tex-if-stack*)

(define (do-iftrue) (set! *tex-if-stack* (cons true *tex-if-stack*))
 *tex-if-stack*)

(define (do-ifskip) (set! *tex-if-stack* (cons 'skip *tex-if-stack*))
 *tex-if-stack*)

(define (insert-tex-if test) (if test (do-iftrue) (do-iffalse)))

(define (do-ifx)
 (let ((one (get-raw-token/is)))
   (let ((two (get-raw-token/is)))
     (let ((one2 one))
       (let ((two2 two))
         (ignorespaces)
         (if (string=? one two)
             (do-iftrue)
             (begin
              (cond
               ((ctl-seq-p one)
                (set! one2
                 (cond
                  ((begin (set! *it* (find-def one)) *it*)
                   (let ((d *it*))
                     (or (tdef*-expansion d) (tdef*-prim d))))
                  ((begin (set! *it* (find-math-def one)) *it*) *it*)
                  (else "UnDeFiNeD")))
                one2)
               (else false))
              (cond
               ((ctl-seq-p two)
                (set! two2
                 (cond
                  ((begin (set! *it* (find-def two)) *it*)
                   (let ((d *it*))
                     (or (tdef*-expansion d) (tdef*-prim d))))
                  ((begin (set! *it* (find-math-def two)) *it*) *it*)
                  (else "UnDeFiNeD")))
                two2)
               (else false))
              (if
               (or (eqv? one2 two2)
                   (and (string? one2) (string? two2) (string=? one2 two2)))
               (do-iftrue)
               (do-iffalse)))))))))

(define (do-if-get-atomic)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((x (get-raw-token/is)))
       (if (ctl-seq-p x)
           (cond
            ((begin (set! *it* (resolve-defs x)) *it*)
             (let ((z *it*))
               (toss-back-char *invisible-space*)
               (toss-back-string z)))
            (else (return x)))
           (return x)))
     (if %loop-returned %loop-result (%loop)))))

(define (do-if)
 (let ((one (do-if-get-atomic)))
   (let ((two (do-if-get-atomic)))
     (if (or (string=? one two) (and (ctl-seq-p one) (ctl-seq-p two)))
         (do-iftrue)
         (do-iffalse)))))

(define (do-ifmmode) (set! *tex-if-stack* (cons *math-mode-p* *tex-if-stack*))
 *tex-if-stack*)

(define (do-ifnum)
 (if (inside-false-world-p)
     (do-ifskip)
     (let ((one (get-number)))
       (let ((rel (string-ref (get-raw-token/is) 0)))
         (let ((two (get-number)))
           (printf "one= ~s; two= ~s~%" one two)
           (if
            ((case rel
               ((#\<) <)
               ((#\=) =)
               ((#\>) >)
               (else (terror 'do-ifnum "Missing = for \\ifnum.")))
             one two)
            (do-iftrue)
            (do-iffalse)))))))

(define (read-ifcase-clauses)
 (fluid-let ((*not-processing-p* true))
  (let ((else-clause false))
    (let ((or-clauses null))
      (let ((elsep false))
        (let ((outer-loop-done false))
          (let* ((%loop-returned false)
                 (%loop-result 0)
                 (return
                  (lambda %args
                    (set! %loop-returned true)
                    (set! %loop-result (and (pair? %args) (car %args))))))
            (let %loop
              ()
              (cond (outer-loop-done (return)) (else false))
              (unless %loop-returned
                (let ((clause ""))
                  (let* ((%loop-returned false)
                         (%loop-result 0)
                         (return
                          (lambda %args
                            (set! %loop-returned true)
                            (set! %loop-result
                             (and (pair? %args) (car %args))))))
                    (let %loop
                      ()
                      (let ((c (snoop-actual-char)))
                        (cond
                         ((not c)
                          (terror 'read-ifcase-clauses "Incomplete \\ifcase."))
                         (else false))
                        (cond
                         ((= (catcode c) **escape**)
                          (let ((x (get-ctl-seq)))
                            (cond
                             ((string=? x "\\or") (ignorespaces)
                              (cond
                               (elsep
                                (terror 'read-ifcase-clauses "Extra \\or."))
                               (else false))
                              (set! or-clauses (cons clause or-clauses))
                              (return))
                             ((string=? x "\\else") (ignorespaces)
                              (cond
                               (elsep
                                (terror 'read-ifcase-clauses "Extra \\else."))
                               (else false))
                              (set! or-clauses (cons clause or-clauses))
                              (set! elsep true) (return))
                             ((string=? x "\\fi") (ignorespaces)
                              (cond
                               (elsep (set! else-clause clause) else-clause)
                               (else (set! or-clauses (cons clause or-clauses))
                                or-clauses))
                              (set! outer-loop-done true) (return))
                             (else (set! clause (string-append clause x))
                              clause))))
                         (else (get-actual-char)
                          (set! clause (string-append clause (string c)))
                          clause)))
                      (if %loop-returned %loop-result (%loop))))))
              (if %loop-returned %loop-result (%loop))))
          (list (reverse or-clauses) else-clause)))))))

(define (do-ifcase)
 (let ((num (get-number)))
   (apply
    (lambda (or-clauses else-clause . %mvb-rest-arg)
      (let ((chosen (or (list-ref or-clauses num) else-clause)))
        (cond (chosen (tex2page-string chosen)) (else false))))
    (read-ifcase-clauses))))

(define (do-ifodd) (if (odd? (get-number)) (do-iftrue) (do-iffalse)))

(define (do-else) (ignorespaces)
 (cond ((null? *tex-if-stack*) (terror 'do-else "Extra \\else.")) (else false))
 (let ((top-if
        (let* ((%pop-old-stack *tex-if-stack*)
               (%pop-top-value (car %pop-old-stack)))
          (begin (set! *tex-if-stack* (cdr %pop-old-stack)) *tex-if-stack*)
          %pop-top-value)))
   (set! *tex-if-stack*
    (cons (if (eq? top-if 'skip) 'skip (not top-if)) *tex-if-stack*))
   *tex-if-stack*))

(define (do-fi) (ignorespaces ':stop-before-first-newline)
 (cond ((null? *tex-if-stack*) (terror 'do-fi "Extra \\fi.")) (else false))
 (let* ((%pop-old-stack *tex-if-stack*) (%pop-top-value (car %pop-old-stack)))
   (begin (set! *tex-if-stack* (cdr %pop-old-stack)) *tex-if-stack*)
   %pop-top-value))

(define (do-newif)
 (let ((iffoo (get-ctl-seq)))
   (let ((foo (string-append "\\" (substring iffoo 3))))
     (let ((foo-register (string-append foo "BOOLEANREGISTER")))
       (plain-count foo-register 0 false)
       (tex-def-thunk iffoo
        (lambda ()
          (set! *tex-if-stack*
           (cons (> (the-count foo-register) 0) *tex-if-stack*))
          *tex-if-stack*)
        false)
       (tex-def-thunk (string-append foo "true")
        (lambda () (plain-count foo-register 1 false)) false)
       (tex-def-thunk (string-append foo "false")
        (lambda () (plain-count foo-register 0 false)) false)))))

(define (do-htmlimg env)
 (call-with-html-image-stream (lambda (o) (dump-till-end-env env o))))

(define (find-img-file-extn)
 (case (tex2page-flag-value "\\TZPimageformat")
   ((#\g #\G) ".gif")
   ((#\j #\J) ".jpeg")
   (else ".png")))

(define (do-htmlimageformat)
 (tex-def-0arg "\\TZPimageformat" (get-peeled-group)))

(define (do-htmlimageconversionprogram)
 (tex-def-0arg "\\TZPimageconverter" (get-peeled-group)))

(define (do-htmlimgmagnification) true)

(define call-mp
 (let ((tex-prog-name false))
   (lambda (f)
     (cond ((not tex-prog-name) (set! tex-prog-name "tex") tex-prog-name)
           (else false))
     (system (string-append *metapost* " -tex=" tex-prog-name " " f)))))

(define call-tex
 (let ((tex-prog-name false) (tex-output-format false))
   (lambda (f)
     (cond
      ((not tex-prog-name)
       (let ((d (find-def "\\TZPtexprogname")))
         (cond (d (set! tex-prog-name (tdef*-expansion d)) tex-prog-name)
               (else false)))
       (cond ((not tex-prog-name) (set! tex-prog-name "luatex") tex-prog-name)
             (else false))
       (set! tex-output-format
        (if
         (or (eqv? (substring? "pdf" tex-prog-name) 0)
             (eqv? (substring? "xe" tex-prog-name) 0)
             (eqv? (substring? "lua" tex-prog-name) 0))
         ':pdf
         ':dvi))
       (cond
        ((eq? *tex-format* ':latex)
         (set! tex-prog-name
          (string-append
           (substring tex-prog-name 0 (- (string-length tex-prog-name) 3))
           "latex"))
         tex-prog-name)
        (else false)))
      (else false))
     (let ((dvi-file
            (string-append f (if (eq? tex-output-format ':pdf) ".pdf" ".dvi"))))
       (let ((outfile dvi-file))
         (system (string-append tex-prog-name " " f))
         (cond
          ((file-exists? dvi-file)
           (let ((logfile (string-append f ".log")))
             (cond
              ((file-exists? logfile)
               (let ((fine-p
                      (let ((i (open-input-file logfile)))
                        (let ((%with-open-file-res
                               (let ((x false))
                                 (let* ((%loop-returned false)
                                        (%loop-result 0)
                                        (return
                                         (lambda %args
                                           (set! %loop-returned true)
                                           (set! %loop-result
                                            (and (pair? %args) (car %args))))))
                                   (let %loop
                                     ()
                                     (set! x
                                      (let ((%read-line-res (read-line i)))
                                        (when (eof-object? %read-line-res)
                                          (set! %read-line-res false))
                                        %read-line-res))
                                     (cond ((not x) (return true))
                                           (else false))
                                     (unless %loop-returned
                                       (cond
                                        ((substring? "! I can't find file" x)
                                         (return false))
                                        (else false)))
                                     (if %loop-returned
                                         %loop-result
                                         (%loop)))))))
                          (cond
                           (i
                            ((if (input-port? i)
                                 close-input-port
                                 close-output-port)
                             i))
                           (else false))
                          %with-open-file-res))))
                 (cond
                  (fine-p
                   (cond
                    ((not (eq? tex-output-format ':pdf))
                     (let ((ps-file (string-append f ".ps")))
                       (system
                        (string-append "dvips " dvi-file " -o " ps-file))
                       (set! outfile ps-file)
                       outfile))
                    (else false))
                   outfile)
                  (else false))))
              (else false))))
          (else false)))))))

(define (ps-to-img/gif/netpbm img-file)
 (system
  (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
 (system
  (string-append "ppmtogif -transparent rgb:ff/ff/ff < " img-file ".ppm > "
   img-file))
 (for-each (lambda (e) (ensure-file-deleted (string-append img-file e)))
  '(".ppm" ".ppm.tmp" ".ppm.1")))

(define (ps-to-img/png/netpbm img-file)
 '(system
   (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
 (system
  (string-append "pnmtopng -interlace -transparent \"#FFFFFF\" " " < " img-file
   ".ppm.tmp > " img-file))
 (for-each (lambda (e) (ensure-file-deleted (string-append img-file e)))
  '(".ppm.1" ".ppm.tmp" ".ppm")))

(define (ps-to-img/jpeg/netpbm img-file)
 (system
  (string-append "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm"))
 (system
  (string-append "ppmtojpeg --grayscale < " img-file ".ppm > " img-file))
 (for-each (lambda (e) (ensure-file-deleted (string-append img-file e)))
  '(".ppm.1" ".ppm.tmp" ".ppm")))

(define (ps-to-img ps-file img-file)
 (case (tex2page-flag-value "\\TZPimageconverter")
   ((#\i #\I)
    (system
     (string-append "convert -transparent white -trim " ps-file " " img-file)))
   (else
    (system
     (string-append *ghostscript* *ghostscript-options* " -sOutputFile="
      img-file ".ppm.1 " ps-file))
    (system
     (string-append "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp"))
    (case (tex2page-flag-value "\\TZPimageformat")
      ((#\p #\P) (ps-to-img/png/netpbm img-file))
      ((#\j #\J) (ps-to-img/jpeg/netpbm img-file))
      (else (ps-to-img/gif/netpbm img-file))))))

(define (tex-to-img f) (set! *img-file-tally* (+ *img-file-tally* 1))
 (let ((img-file (string-append f (find-img-file-extn))))
   (let ((fq-img-file (string-append *aux-dir/* img-file)))
     (cond
      ((not (file-exists? fq-img-file)) (write-log ':separation-space)
       (write-log #\{) (write-log (string-append f ".tex"))
       (write-log ':separation-space) (write-log "->")
       (write-log ':separation-space)
       (cond
        ((begin (set! *it* (call-tex f)) *it*) (ps-to-img *it* img-file)
         (ensure-url-reachable img-file ':delete) (write-log img-file)
         (for-each (lambda (e) (ensure-file-deleted (string-append f e)))
          '(".log" ".pdf" ".tex")))
        (else (write-log "failed, try manually")))
       (write-log #\}) (write-log ':separation-space))
      (else false)))))

(define (call-with-lazy-image-stream eps-file img-file-stem p)
 (let ((aux-tex-file (string-append img-file-stem ".tex")))
   (let ((o
          (let* ((%f aux-tex-file)
                 (%ee (list ':direction ':output ':if-exists ':supersede))
                 (%direction (memv ':direction %ee))
                 (%if-exists (memv ':if-exists %ee))
                 (%if-does-not-exist ':error)
                 (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
            (when %direction (set! %direction (cadr %direction)))
            (when %if-exists (set! %if-exists (cadr %if-exists)))
            (when %if-does-not-exist-from-user
              (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
            (cond
             ((eqv? %direction ':output)
              (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
                (delete-file %f))
              (open-output-file %f))
             ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
             (else (open-input-file %f))))))
     (let ((%with-open-file-res
            (begin (dump-tex-preamble o) (p o) (dump-tex-postamble o))))
       (cond (o ((if (input-port? o) close-input-port close-output-port) o))
             (else false))
       %with-open-file-res))
   (if (file-exists? eps-file)
       (tex-to-img img-file-stem)
       (begin
        (set! *missing-eps-files*
         (cons (cons eps-file img-file-stem) *missing-eps-files*))
        *missing-eps-files*))))

(define (retry-lazy-image eps-file img-file-stem)
 (cond ((file-exists? eps-file) (tex-to-img img-file-stem))
       (else (write-log "! I can't find EPS file ") (write-log eps-file)
        (write-log ':separation-newline))))

(define (lazily-make-epsf-image-file eps-file img-file-stem)
 (fluid-let ((*imgpreamble-inferred* (cons ':epsfbox *imgpreamble-inferred*)))
  (call-with-lazy-image-stream eps-file img-file-stem
   (lambda (o) (display "\\epsfbox{" o) (display eps-file o) (display #\} o)))))

(define (do-epsfig)
 (fluid-let ((*imgpreamble-inferred* (cons ':epsfbox *imgpreamble-inferred*)))
  (call-with-html-image-stream
   (lambda (o) (display "\\epsfig{" o) (dump-groupoid o) (display #\} o)))))

(define (do-convertmptopdf)
 (let ((f (get-filename-possibly-braced))
       (img-file-stem (next-html-image-file-stem)))
   (get-token)
   (get-token)
   (lazily-make-epsf-image-file f img-file-stem)
   (source-img-file img-file-stem)))

(define (do-includegraphics-web bracketed-text image-file) (emit "<img")
 (cond
  (bracketed-text
   (let ((height false) (width false))
     (toss-back-string " enoughalready ")
     (toss-back-string bracketed-text)
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (cond
          ((eat-word "height") (get-equal-sign) (set! height (get-pixels))
           height)
          ((eat-word "width") (get-equal-sign) (set! width (get-pixels)) width)
          ((eat-word "enoughalready") (ignorespaces) (return))
          (else (get-actual-char)))
         (if %loop-returned %loop-result (%loop))))
     (cond (height (emit " height=") (emit height)) (else false))
     (cond (width (emit " width=") (emit width)) (else false))))
  (else false))
 (emit " src=\"") (do-img-src (fully-qualify-url image-file)) (emit "\" ")
 (emit " alt=\"") (emit image-file) (emit "\"") (emit ">"))

(define (do-includegraphics)
 (let ((starred-p (eat-star)))
   (let ((b1 (get-bracketed-text-if-any)))
     (let ((b2 (and b1 (get-bracketed-text-if-any))))
       (let ((f (get-filename-possibly-braced)))
         (let ((ffull
                (if (file-exists? f)
                    f
                    (ormap
                     (lambda (e)
                       (let ((f2 (string-append f e)))
                         (and (file-exists? f2) f2)))
                     *graphics-file-extensions*))))
           (let ((ffull-ext (and ffull (file-extension ffull))))
             (cond
              ((and ffull-ext (member ffull-ext '(".jpg" ".jpeg" ".png")))
               (do-includegraphics-web b1 ffull))
              (else
               (let ((%fluid-var-*imgpreamble-inferred*
                      (cons ':includegraphics *imgpreamble-inferred*))
                     (img-file-stem (next-html-image-file-stem)))
                 (fluid-let
                  ((*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*))
                  (call-with-lazy-image-stream (or ffull f) img-file-stem
                   (lambda (o)
                     (display "\\includegraphics" o)
                     (cond (starred-p (display #\* o)) (else false))
                     (cond (b1 (display #\[ o) (display b1 o) (display #\] o))
                           (else false))
                     (cond (b2 (display #\[ o) (display b2 o) (display #\] o))
                           (else false))
                     (display #\{ o)
                     (display f o)
                     (display #\} o)))
                  (source-img-file img-file-stem))))))))))))

(define (do-resizebox)
 (let ((arg1 (get-group)))
   (let ((arg2 (get-group)))
     (let ((arg3 (get-group)))
       (fluid-let
        ((*imgpreamble-inferred*
          (cons ':includegraphics *imgpreamble-inferred*)))
        (call-with-html-image-stream
         (lambda (o)
           (display "\\resizebox" o)
           (display arg1 o)
           (display arg2 o)
           (display arg3 o))))))))

(define (do-mplibcode)
 (call-with-html-image-stream
  (lambda (o)
    (display "\\input luamplib.sty" o)
    (newline o)
    (display "\\mplibcode" o)
    (dump-till-end-env "mplibcode" o)
    (display "\\endmplibcode" o)
    (newline o))))

(define (do-mfpic-opengraphsfile)
 (set! *mfpic-file-stem* (get-filename-possibly-braced))
 (cond
  (*mfpic-stream*
   (let ((%close-port-arg *mfpic-stream*))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))
  (else false))
 (let ((f (string-append *mfpic-file-stem* *mfpic-tex-file-suffix*)))
   (set! *mfpic-stream*
    (let* ((%f f)
           (%ee (list ':direction ':output ':if-exists ':supersede))
           (%direction (memv ':direction %ee))
           (%if-exists (memv ':if-exists %ee))
           (%if-does-not-exist ':error)
           (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
      (when %direction (set! %direction (cadr %direction)))
      (when %if-exists (set! %if-exists (cadr %if-exists)))
      (when %if-does-not-exist-from-user
        (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
      (cond
       ((eqv? %direction ':output)
        (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
          (delete-file %f))
        (open-output-file %f))
       ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
       (else (open-input-file %f)))))
   *mfpic-stream*)
 (set! *mfpic-file-num* 0)
 (display "\\input mfpic \\usemetapost " *mfpic-stream*)
 (newline *mfpic-stream*) (display "\\opengraphsfile{" *mfpic-stream*)
 (display *mfpic-file-stem* *mfpic-stream*) (display #\} *mfpic-stream*)
 (newline *mfpic-stream*)
 (tex-def-prim "\\headshape"
  (lambda ()
    (let ((g1 (get-group)))
      (let ((g2 (get-group)))
        (let ((g3 (get-group)))
          (display "\\headshape" *mfpic-stream*)
          (display g1 *mfpic-stream*)
          (display g2 *mfpic-stream*)
          (display g3 *mfpic-stream*)
          (newline *mfpic-stream*))))))
 (tex-def-prim "\\mfpframesep" eat-dimen) (tex-def-prim "\\mftitle" get-group))

(define (do-mfpic-closegraphsfile) (display "\\closegraphsfile" *mfpic-stream*)
 (newline *mfpic-stream*)
 (let ((%close-port-arg *mfpic-stream*))
   ((if (input-port? %close-port-arg) close-input-port close-output-port)
    %close-port-arg))
 (let ((tex-f (string-append *mfpic-file-stem* *mfpic-tex-file-suffix*))
       (mp-f (string-append *mfpic-file-stem* ".mp")))
   (cond
    ((not (file-exists? mp-f))
     (fluid-let ((*tex-format* ':plain)) (call-tex tex-f)))
    (else false))
   (cond ((file-exists? mp-f) (call-mp mp-f)) (else false))))

(define (do-mfpic) (display "\\mfpic" *mfpic-stream*)
 (dump-till-end-env "mfpic" *mfpic-stream*)
 (display "\\endmfpic" *mfpic-stream*) (newline *mfpic-stream*)
 (set! *mfpic-file-num* (add1 *mfpic-file-num*))
 (let ((f
        (string-append *mfpic-file-stem* "."
         (write-to-string *mfpic-file-num*)))
       (img-file-stem (next-html-image-file-stem)))
   (lazily-make-epsf-image-file f img-file-stem)
   (source-img-file img-file-stem)))

(define (do-following-latex-env-as-image)
 (do-latex-env-as-image (ungroup (get-group)) ':display))

(define (do-latex-env-as-image env display-p)
 (let ((env2 (if (string=? env "align") "eqnarray" env)))
   (cond
    ((char=? (snoop-actual-char) #\*) (get-actual-char)
     (set! env (string-append env "*")) (set! env2 (string-append env2 "*"))
     env2)
    (else false))
   (egroup)
   (cond
    (display-p (do-end-para) (emit "<div class=")
     (emit *display-justification*) (emit ">"))
    (else false))
   (call-with-html-image-stream
    (lambda (o)
      (display "\\begin{" o)
      (display env2 o)
      (display "}" o)
      (dump-till-end-env env o)
      (display "\\end{" o)
      (display env2 o)
      (display "}" o)
      (newline o)))
   (cond (display-p (emit "</div>") (do-para)) (else false))))

(define (do-hbox) (ignorespaces) (get-to) (eat-dimen) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (case c
     ((#\{) (get-actual-char))
     ((#\\) (get-ctl-seq))
     (else (get-actual-char) (toss-back-char #\}) (toss-back-char c))))
 (bgroup)
 (add-postlude-to-top-frame
  (let ((old-math-mode-p *math-mode-p*)
        (old-in-display-math-p *in-display-math-p*)
        (old-tabular-stack *tabular-stack*)
        (old-ligatures-p *ligatures-p*))
    (set! *math-mode-p* false)
    (set! *in-display-math-p* false)
    (set! *tabular-stack* null)
    (set! *ligatures-p* true)
    (lambda ()
      (set! *math-mode-p* old-math-mode-p)
      (set! *in-display-math-p* old-in-display-math-p)
      (set! *tabular-stack* old-tabular-stack)
      (set! *ligatures-p* old-ligatures-p)
      *ligatures-p*))))

(define (do-latex-frac) (tex2page-string (ungroup (get-token)))
 (emit
  (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
        (else (set! *math-height* (+ *math-height* 1))
         "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td class=centerline>")))
 (tex2page-string (ungroup (get-token))))

(define (do-tex-frac) (ignorespaces)
 (let ((inner-level-p
        (or (not *in-display-math-p*) (not (null? *tabular-stack*))))
       (%fluid-var-*tabular-stack* (cons ':frac *tabular-stack*)))
   (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
    (cond
     (inner-level-p (emit "<sup>") (tex2page-string (get-till-char #\/))
      (emit "</sup>/<sub>") (get-actual-char) (ignorespaces)
      (tex2page-string (get-token)) (emit "</sub>"))
     (else (emit "</td><td><table class=leftline><tr><td class=centerline>")
      (tex2page-string (get-till-char #\/)) (get-actual-char) (ignorespaces)
      (emit "<hr noshade>") (tex2page-string (get-token))
      (emit "</td></tr></table></td><td>"))))))

(define (do-eqno)
 (cond
  ((not *in-display-math-p*)
   (terror 'do-eqno "You can't use \\eqno in math mode"))
  (else false))
 (emit "</td><td width=10% class=rightline>"))

(define (do-eqalign type) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((not c) (terror 'do-eqalign "Missing {")) (else false))
   (cond ((not (char=? c #\{)) (terror 'do-eqalign "Missing {")) (else false))
   (bgroup)
   (set! *tabular-stack* (cons type *tabular-stack*))
   (add-postlude-to-top-frame
    (lambda ()
      (emit "</td></tr>")
      (emit-newline)
      (emit "</table>")
      (emit-newline)
      (cond (*in-display-math-p* (emit "</td><td>")) (else false))
      (pop-tabular-stack type)
      (set! *equation-position* 0)
      *equation-position*))
   (cond (*in-display-math-p* (emit "</td><td>")) (else false))
   (emit-newline)
   (emit "<table><tr><td>")))

(define (do-noalign)
 (let ((type (car *tabular-stack*)))
   (let ((split-p (member type '(:eqalignno :displaylines))))
     (cond
      (split-p (egroup) (emit "</td></tr></table></div>") (emit-newline)
       (do-para))
      (else false))
     (tex2page-string (get-group))
     (cond
      (split-p (do-end-para) (emit-newline)
       (emit "<div class=centerline><table><tr><td>") (toss-back-char #\{)
       (do-eqalign type))
      (else (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))))))

(define (do-pmatrix) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((or (not c) (not (char=? c #\{))) (terror 'do-pmatrix "Missing {"))
         (else false))
   (bgroup)
   (set! *math-delim-left* ':lparen)
   (set! *math-delim-right* ':rparen)
   *math-delim-right*))

(define (do-over)
 (emit
  (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
        (else (set! *math-height* (+ *math-height* 1))
         "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td class=centerline>"))))

(define (eat-till-eol)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (get-actual-char)))
       (cond ((or (not c) (char=? c #\newline)) (return)) (else false)))
     (if %loop-returned %loop-result (%loop)))))

(define (eat-till-char d)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (get-actual-char)))
       (cond ((or (not c) (char=? c d)) (return)) (else false)))
     (if %loop-returned %loop-result (%loop)))))

(define (do-comment) (eat-till-eol)
 (cond
  ((munched-a-newline-p) (toss-back-char #\newline) (toss-back-char #\newline))
  (else false)))

(define (string=join ss sepc)
 (let ((res ""))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((null? ss) (return res)) (else false))
       (unless %loop-returned
         (let ((s
                (let* ((%pop-old-stack ss)
                       (%pop-top-value (car %pop-old-stack)))
                  (begin (set! ss (cdr %pop-old-stack)) ss)
                  %pop-top-value)))
           (set! res
            (cond ((string=? res "") s)
                  (else (string-append res (list sepc) s))))
           res))
       (if %loop-returned %loop-result (%loop))))))

(define (kpsewhich f)
 (let ((tmpf (string-append *aux-dir/* *jobname* "-Z-Z.temp")))
   (system (string-append "kpsewhich -- " f " > " tmpf))
   (let ((f
          (and (file-exists? tmpf)
               (let ((i (open-input-file tmpf)))
                 (let ((%with-open-file-res
                        (let ((%read-line-res (read-line i)))
                          (when (eof-object? %read-line-res)
                            (set! %read-line-res false))
                          %read-line-res)))
                   (cond
                    (i
                     ((if (input-port? i) close-input-port close-output-port)
                      i))
                    (else false))
                   %with-open-file-res)))))
     (ensure-file-deleted tmpf)
     (if (not f)
         false
         (let ((f (string-trim f)))
           (cond ((= (string-length f) 0) false)
                 ((file-exists? f) f)
                 (else false)))))))

(define (find-tex-file file)
 (let ((file.tex (string-append file ".tex")))
   (or (and (file-exists? file.tex) file.tex)
       (and (file-exists? file) file)
       (if (not (null? *tex2page-inputs*))
           (let ((%dolist-l *tex2page-inputs*) (dir false))
             (let* ((%loop-returned false)
                    (%loop-result 0)
                    (return
                     (lambda %args
                       (set! %loop-returned true)
                       (set! %loop-result (and (pair? %args) (car %args))))))
               (let %loop
                 ()
                 (if (null? %dolist-l) (return) false)
                 (unless %loop-returned (set! dir (car %dolist-l)))
                 (unless %loop-returned (set! %dolist-l (cdr %dolist-l)))
                 (unless %loop-returned
                   (let ((f (string-append dir *directory-separator* file.tex)))
                     (cond ((file-exists? f) (return f)) (else false))))
                 (unless %loop-returned
                   (let ((f (string-append dir *directory-separator* file)))
                     (cond ((file-exists? f) (return f)) (else false))))
                 (if %loop-returned %loop-result (%loop)))))
           (kpsewhich file)))))

(define (initialize-scm-words) (set! *scm-keywords* (make-table ':test equal?))
 (set! *scm-builtins* (make-table ':test equal?))
 (set! *scm-special-symbols* (make-table ':test equal?))
 (set! *scm-variables* (make-table ':test equal?))
 (for-each
  (lambda (s) (table-put! s *scm-keywords* true) (table-get s *scm-keywords*))
  '("=>" "and" "begin" "begin0" "case" "cond" "define" "define-macro"
    "define-syntax" "defmacro" "defstruct" "delay" "do" "else" "flet"
    "fluid-let" "if" "labels" "lambda" "let" "let*" "let-syntax" "let-values"
    "letrec" "letrec-syntax" "macrolet" "or" "quasiquote" "quote" "set!"
    "syntax-case" "syntax-rules" "unless" "unquote" "unquote-splicing" "when"
    "with" "with-handlers" "assert" "block" "decf" "defpackage" "defparameter"
    "defun" "defvar" "destructuring-bind" "do-all-symbols"
    "do-external-symbols" "do-symbols" "dolist" "dotimes" "ecase" "etypecase"
    "eval-when" "handler-bind" "handler-case" "incf" "loop"
    "multiple-value-bind" "multiple-value-setq" "pop" "prog1" "progn" "push"
    "setf" "setq" "typecase" "unwind-protect" "with-input-from-string"
    "with-open-file" "with-open-socket" "with-open-stream"
    "with-output-to-string" "with-slots"))
 true)

(define (actual-tex-filename f . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (check-timestamp-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! check-timestamp-p (list-ref %lambda-rest-arg 0)))
   (let ((doing-main-file-p (not *main-tex-file*))
         (f (and f (find-tex-file f))))
     (cond
      (doing-main-file-p
       (cond
        (f (set! *jobname* (file-stem-name f)) (make-target-dir)
         (let ((main-html-page
                (string-append *aux-dir/* *jobname* *output-extension*)))
           (cond
            ((string=? main-html-page f)
             (let ((f-save (string-append f ".sav")))
               (write-log ':separation-newline)
               (write-log "Copying weirdly named TeX source file ")
               (write-log f)
               (write-log " to ")
               (write-log f-save)
               (write-log ':separation-newline)
               (system (string-append "cp -pf " f " " f-save))
               (set! f f-save)
               f))
            (else false))))
        (else false))
       (load-aux-file))
      (else false))
     (cond
      ((and f check-timestamp-p)
       (cond
        ((not (member f *verb-written-files*))
         (update-last-modification-time f))
        (else false)))
      (else false))
     f)))

(define (add-dot-tex-if-no-extension-provided f)
 (let ((e (file-extension f)))
   (if e f (string-append f ".tex"))))

(define (ignore-tex-specific-text env)
 (let ((endenv (string-append "\\end" env)))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (snoop-actual-char)))
         (cond ((not c) (terror 'ignore-tex-specific-text "Missing \\end" env))
               (else false))
         (cond
          ((= (catcode c) **escape**)
           (let ((x (get-ctl-seq)))
             (cond ((string=? x endenv) (return))
                   ((string=? x "\\end")
                    (let ((g (get-grouped-environment-name-if-any)))
                      (cond ((and g (string=? g env)) (return))
                            (else false)))))))
          (else (get-actual-char))))
       (if %loop-returned %loop-result (%loop))))))

(define (do-rawhtml) (ignorespaces)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (snoop-actual-char)))
       (cond ((not c) (terror 'do-rawhtml "missing \\endrawhtml"))
             ((= (catcode c) **escape**)
              (let ((x (get-ctl-seq)))
                (let ((y (find-corresp-prim x)))
                  (cond ((string=? y "\\endrawhtml") (ignorespaces) (return))
                        ((and (string=? y "\\end")
                              (begin
                               (set! *it*
                                (get-grouped-environment-name-if-any))
                               *it*))
                         (let ((g *it*))
                           (let ((y (find-corresp-prim (string-append x g))))
                             (cond
                              ((string=? y "\\endrawhtml") (ignorespaces)
                               (return))
                              (else (emit "\\end{") (emit g) (emit "}"))))))
                        ((string=? x "\\\\") (emit c) (toss-back-char c))
                        (else (emit x))))))
             (else (get-actual-char) (emit c))))
     (if %loop-returned %loop-result (%loop)))))

(define (do-htmlheadonly)
 (cond ((null? *html-head*) (flag-missing-piece ':html-head)) (else false))
 (let ((s null) (s2 false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (snoop-actual-char)))
         (cond
          ((not c) (set! s2 (list->string (reverse s)))
           (write-aux (quasiquote (!html-head (unquote s2)))) (return))
          ((= (catcode c) **escape**) (set! s2 (list->string (reverse s)))
           (write-aux (quasiquote (!html-head (unquote s2)))) (set! s null)
           (let ((x (get-ctl-seq)))
             (cond ((string=? x "\\endhtmlheadonly") (return))
                   ((string=? x "\\input")
                    (let ((f (get-filename-possibly-braced)))
                      (call-with-input-file/buffered f do-htmlheadonly)))
                   (else (write-aux (quasiquote (!html-head (unquote x))))))))
          (else (get-actual-char) (set! s (cons c s)) s)))
       (if %loop-returned %loop-result (%loop))))))

(define (resolve-cdefs c)
 (cond
  ((begin (set! *it* (find-cdef c)) *it*)
   (let ((y *it*))
     (get-actual-char)
     (expand-tex-macro (cdef*-optarg y) (cdef*-argpat y) (cdef*-expansion y)
      (cdef*-catcodes y))))
  (else false)))

(define (resolve-defs x)
 (cond
  ((begin (set! *it* (find-def x)) *it*)
   (let ((y *it*))
     (cond ((begin (set! *it* (tdef*-defer y)) *it*) *it*)
           ((tdef*-thunk y) false)
           ((and (null? (tdef*-argpat y))
                 (not (tdef*-optarg y))
                 (begin (set! *it* (tdef*-expansion y)) *it*))
            *it*)
           ((and (inside-false-world-p) (not (if-aware-ctl-seq-p x))) false)
           (else
            (cond
             (*outer-p* (set! *outer-p* false)
              (toss-back-char *invisible-space*))
             (else false))
            (expand-tex-macro (tdef*-optarg y) (tdef*-argpat y)
             (tdef*-expansion y) (tdef*-catcodes y))))))
  (else false)))

(define (do-expandafter)
 (let ((first (get-raw-token/is)))
   (let ((second (get-raw-token/is)))
     (toss-back-char *invisible-space*)
     (cond
      ((ctl-seq-p second)
       (toss-back-string (expand-ctl-seq-into-string second)))
      (else (toss-back-string second)))
     (toss-back-char *invisible-space*)
     (toss-back-string first))))

(define (resolve-expandafters)
 (let ((c (snoop-actual-char)))
   (cond
    ((= (catcode c) **escape**)
     (let ((x (get-ctl-seq)))
       (if (string=? x "\\expandafter")
           (do-expandafter)
           (begin (toss-back-char *invisible-space*) (toss-back-string x)))))
    (else false))))

(define (do-futurelet)
 (let ((first (get-raw-token/is)))
   (let ((second (get-raw-token/is)))
     (let ((third (get-raw-token)))
       (do-futurelet-aux first second third)))))

(define (do-futurelet-aux first second third)
 (tex-let-general first third false) (toss-back-char *invisible-space*)
 (toss-back-string third) (toss-back-char *invisible-space*)
 (toss-back-string second))

(define (set-start-time)
 (apply
  (lambda (s m h d mo y . %mvb-rest-arg)
    (plain-count "\\time" (+ (* 60 h) m))
    (plain-count "\\day" d)
    (plain-count "\\month" mo)
    (plain-count "\\year" y))
  (decode-universal-time *start-time*)))

(define (initialize-globals) (tex-def-countdef "\\pageno" 0 0 true)
 (tex-def-count 10 21 true) (tex-def-count 11 9 true) (tex-def-count 14 9 true)
 (tex-def-count 15 9 true) (tex-def-count 16 -1 true)
 (tex-def-count 17 -1 true) (set-start-time) (plain-count "\\language" 256)
 (plain-count "\\secnumdepth" -2) (plain-count "\\tocdepth" -2)
 (plain-count "\\footnotenumber" 0) (plain-count "\\TIIPtabularborder" 1)
 (plain-count "\\TIIPnestedtabularborder" 0)
 (plain-count "\\TIIPobeyspacestrictly" 0)
 (plain-count "\\TIIPobeylinestrictly" 0)
 (plain-count "\\TIIPsettabscolumns" 0) (plain-count "\\errorcontextlines" 5)
 (plain-count "\\doublehyphendemerits" 10000)
 (plain-count "\\finalhyphendemerits" 5000) (plain-count "\\hyphenpenalty" 50)
 (plain-count "\\exhyphenpenalty" 50) (plain-count "\\pretolerance" 100)
 (plain-count "\\tolerance" 200) (plain-count "\\hbadness" 1000)
 (plain-count "\\widowpenalty" 150) (plain-count "\\showboxdepth" 3)
 (plain-count "\\outputpenalty" 0) (plain-count "\\globaldefs" 0)
 (plain-count "\\mag" 1000) (plain-count "\\tracingcommands" 0)
 (plain-count "\\tracingmacros" 0) (plain-count "\\tracingonline" 0)
 (plain-count "\\shellescape" 1) (plain-count "\\suppressfontnotfounderror" 1)
 (plain-dimen "\\TIIPhsize" 0) (plain-dimen "\\hsize" (tex-length 6.5 ':in))
 (plain-dimen "\\vsize" (tex-length 8.9 ':in))
 (plain-dimen "\\maxdepth" (tex-length 4 ':pt))
 (plain-dimen "\\delimitershortfall" (tex-length 5 ':pt))
 (plain-dimen "\\nulldelimiterspace" (tex-length 1.2 ':pt))
 (plain-dimen "\\scriptspace" (tex-length 0.5 ':pt))
 (plain-dimen "\\hoffset" 0) (plain-dimen "\\voffset" 0)
 (plain-dimen "\\epsfxsize" 0) (plain-dimen "\\epsfysize" 0)
 (plain-dimen "\\emergencystretch" 0)
 (plain-dimen "\\hfuzz" (tex-length 0.1 ':pt))
 (plain-dimen "\\vfuzz" (tex-length 0.1 ':pt))
 (plain-dimen "\\textwidth" (tex-length 6.5 ':in))
 (plain-dimen "\\smallskipamount" (tex-length 3 ':pt))
 (plain-dimen "\\medskipamount" (tex-length 6 ':pt))
 (plain-dimen "\\bigskipamount" (tex-length 12 ':pt))
 (plain-dimen "\\lastskip" 0)
 (plain-dimen "\\baselineskip" (tex-length 12 ':pt))
 (plain-dimen "\\overfullrule" (tex-length 5 ':pt))
 (plain-dimen "\\parindent" (tex-length 20 ':pt)) (plain-dimen "\\leftskip" 0)
 (plain-dimen "\\parfillskip" 0) (plain-dimen "\\parskip" 0)
 (plain-dimen "\\abovedisplayskip" (tex-length 12 ':pt))
 (plain-dimen "\\belowdisplayskip" (tex-length 12 ':pt))
 (plain-toks "\\everypar" "") (plain-toks "\\headline" "")
 (plain-toks "\\footline" "\\folio") (tex-def-dotted-count "figure" false)
 (tex-def-dotted-count "table" false) (tex-def-dotted-count "equation" false)
 (tex-gdef-0arg "\\TIIPcurrentnodename" "no value yet")
 (tex-gdef-0arg "\\@currentlabel" "no value yet")
 (tex-gdef-0arg "\\TZPcolophontimestamp" "1")
 (tex-gdef-0arg "\\TZPcolophoncredit" "1")
 (tex-gdef-0arg "\\TZPcolophonweblink" "1")
 (tex-gdef-0arg "\\TZPimageformat" "PNG")
 (tex-gdef-0arg "\\TZPimageconverter" "NetPBM")
 (tex-gdef-0arg "\\TZPredirectseconds" "0") (tex-gdef-0arg "\\TZPtextext" "1")
 (tex-gdef-0arg "\\TZPraggedright" "1") (tex-gdef-0arg "\\TZPlang" "en")
 (tex-gdef-0arg "\\TZPcommonlisp" (if null "0" "1")) (initialize-scm-words))

(define (find-def ctlseq)
 (or
  (ormap (lambda (fr) (table-get ctlseq (texframe*-definitions fr))) *tex-env*)
  (and *global-texframe*
       (table-get ctlseq (texframe*-definitions *global-texframe*)))
  (table-get ctlseq (texframe*-definitions *primitive-texframe*))))

(define (find-math-def ctlseq)
 (table-get ctlseq (texframe*-definitions *math-primitive-texframe*)))

(define (do-number) (emit (get-number)))

(define (do-magnification) (plain-count "\\mag" (get-number) false))

(define (do-magstep)
 (case (string->number (get-token-or-peeled-group))
   ((1) "1000")
   ((2) "1200")
   ((3) "1440")
   ((4) "1728")
   ((5) "2074")
   ((6) "2488")
   (else "")))

(define (scaled-point-to-tex-point sp)
 (string-append (write-to-string (/ sp 65536.0)) "pt"))

(define (expand-the)
 (let ((ctlseq (get-ctl-seq)))
   (cond
    ((begin (set! *it* (find-dimendef ctlseq)) *it*)
     (scaled-point-to-tex-point (find-dimen *it*)))
    ((begin (set! *it* (get-number-corresp-to-ctl-seq ctlseq)) *it*) *it*)
    ((begin (set! *it* (find-toksdef ctlseq)) *it*) (find-toks *it*))
    (else (trace-if false "expand-the failed")))))

(define (do-the)
 (let ((ctlseq (get-ctl-seq)))
   (ignorespaces)
   (cond ((string=? ctlseq "\\count") (emit (find-count (get-number))))
         ((begin (set! *it* (get-number-corresp-to-ctl-seq ctlseq)) *it*)
          (emit *it*))
         ((begin (set! *it* (find-toksdef ctlseq)) *it*)
          (tex2page-string (find-toks *it*)))
         (else (trace-if false "do-the failed")))))

(define (do-arabic)
 (let ((counter-name (ungroup (get-group))))
   (let ((counter (table-get counter-name *dotted-counters*)))
     (let ((it false))
       (cond ((begin (set! it (counter*-value counter)) it) (emit it))
             (else (trace-if false "do-arabic failed")))))))

(define (find-corresp-prim ctlseq)
 (let ((y (find-def ctlseq)))
   (or (and y (tdef*-defer y)) ctlseq)))

(define (find-corresp-prim-thunk ctlseq)
 (let ((y (find-def ctlseq)))
   (if (and y (tdef*-thunk y)) (tdef*-prim y) ctlseq)))

(define (globally-p) (> (get-gcount "\\globaldefs") 0))

(define (do-let . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (ignorespaces)
   (let ((lhs (get-ctl-seq)))
     (let ((rhs (begin (get-equal-sign) (get-raw-token/is))))
       (cond
        ((not (inside-false-world-p))
         (let ((frame
                (cond
                 (globalp
                  (for-each
                   (lambda (fr) (table-rem lhs (texframe*-definitions fr)))
                   *tex-env*)
                  *global-texframe*)
                 (else false))))
           (tex-let-general lhs rhs frame)))
        (else false))))))

(define (do-def . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (globalp false)
       (expandp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (when (< 1 %lambda-rest-arg-len)
     (set! expandp (list-ref %lambda-rest-arg 1)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (cond
    ((not (inside-false-world-p))
     (let ((lhs (get-raw-token/is)))
       (cond
        ((and (ctl-seq-p lhs) (string=? lhs "\\TIIPcsname"))
         (set! lhs (get-peeled-group)) lhs)
        (else false))
       (let ((argpat (get-def-parameters)))
         (let ((rhs (ungroup (get-group))))
           (cond (expandp (set! rhs (expand-edef-macro rhs)) rhs) (else false))
           (let ((frame
                  (cond
                   (globalp
                    (for-each
                     (lambda (fr)
                       (table-rem lhs
                        (if (ctl-seq-p lhs)
                            (texframe*-definitions fr)
                            (texframe*-cdefinitions fr))))
                     *tex-env*)
                    *global-texframe*)
                   (else false))))
             (cond
              ((ctl-seq-p lhs)
               (tex-def lhs argpat rhs false false false false frame))
              (else (tex-def-char (string-ref lhs 0) argpat rhs frame))))))))
    (else false))))

(define (do-newcommand renewp) (ignorespaces)
 (let ((lhs (string-trim (ungroup (get-token)))))
   (let ((optarg false))
     (let ((argc
            (cond
             ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
              (let ((s *it*))
                (cond
                 ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                  (set! optarg *it*) optarg))
                (string->number (string-trim s))))
             (else 0))))
       (let ((rhs (ungroup (get-token))))
         (let ((ok-to-def-p (or renewp (not (find-def lhs)))))
           (tex-def lhs (latex-argnum-to-plain-argpat argc) rhs optarg false
            false false false)
           (cond
            ((not ok-to-def-p)
             (trace-if (> (the-count "\\tracingcommands") 0) lhs
              " already defined"))
            (else false))))))))

(define (do-newcounter)
 (let ((counter-name (ungroup (get-group))))
   (let ((within (get-bracketed-text-if-any)))
     (tex-def-dotted-count counter-name within))))

(define (do-newenvironment renewp) (ignorespaces)
 (let ((envname (string-trim (ungroup (get-token)))))
   (let ((bs-envname (string-append "\\" envname)))
     (let ((optarg false))
       (let ((argc
              (cond
               ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                (let ((s *it*))
                  (cond
                   ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                    (set! optarg *it*) optarg))
                  (string->number (string-trim s))))
               (else 0))))
         (let ((beginning
                (string-append "\\begingroup " (ungroup (get-token)))))
           (let ((ending (string-append (ungroup (get-token)) "\\endgroup")))
             (let ((ok-to-def-p (or renewp (not (find-def bs-envname)))))
               (tex-def bs-envname (latex-argnum-to-plain-argpat argc)
                beginning optarg false false false false)
               (tex-def (string-append "\\end" envname) null ending false false
                false false false)
               (cond
                ((not ok-to-def-p)
                 (trace-if true "{" envname "} already defined"))
                (else false))))))))))

(define (tex-def-dotted-count counter-name sec-num)
 (cond
  (sec-num
   (cond
    ((not (table-get sec-num *section-counter-dependencies*))
     (table-put! sec-num *section-counter-dependencies* null)
     (table-get sec-num *section-counter-dependencies*))
    (else false))
   (table-put! sec-num *section-counter-dependencies*
    (cons counter-name (table-get sec-num *section-counter-dependencies*)))
   (table-get sec-num *section-counter-dependencies*))
  (else false))
 (table-put! counter-name *dotted-counters* (make-counter* ':within sec-num))
 (table-get counter-name *dotted-counters*))

(define (do-newtheorem)
 (let ((env (ungroup (get-group))))
   (let ((numbered-like (get-bracketed-text-if-any)))
     (let ((counter-name (or numbered-like env)))
       (let ((caption (ungroup (get-group))))
         (let ((within (if numbered-like false (get-bracketed-text-if-any))))
           (let ((sec-num
                  (and within (section-ctl-seq-p (string-append "\\" within)))))
             (cond
              ((not numbered-like) (tex-def-dotted-count counter-name sec-num))
              (else false))
             (tex-def (string-append "\\" env) null
              (string-append "\\par\\begingroup\\TIIPtheorem{" counter-name
               "}{" caption "}")
              false false false false *global-texframe*)
             (tex-def (string-append "\\end" env) null "\\endgroup\\par" false
              false false false *global-texframe*))))))))

(define (do-theorem)
 (let ((counter-name (ungroup (get-group))))
   (let ((counter (table-get counter-name *dotted-counters*)))
     (let ((caption (ungroup (get-group))))
       (cond ((not counter) (terror 'do-theorem)) (else false))
       (let ((new-counter-value (+ 1 (counter*-value counter))))
         (set!counter*-value counter new-counter-value)
         (counter*-value counter)
         (let ((thm-num
                (let ((sec-num (counter*-within counter)))
                  (if sec-num
                      (string-append (section-counter-value sec-num) "."
                       (write-to-string new-counter-value))
                      (write-to-string new-counter-value)))))
           (let ((lbl (string-append *html-node-prefix* "thm_" thm-num)))
             (tex-def-0arg "\\TIIPcurrentnodename" lbl)
             (tex-def-0arg "\\@currentlabel" thm-num)
             (emit-anchor lbl)
             (emit-newline)
             (emit "<b>")
             (emit caption)
             (emit " ")
             (emit thm-num)
             (emit ".</b>")
             (emit-nbsp 2))))))))

(define (do-begin)
 (cond
  ((not (begin (set! *it* (get-grouped-environment-name-if-any)) *it*))
   (terror 'do-begin "\\begin not followed by environment name"))
  (else false))
 (let ((env *it*))
   (toss-back-char *invisible-space*)
   (toss-back-string (string-append "\\" env))
   (cond
    ((not
      (member env
              '("htmlonly" "cssblock" "document" "latexonly" "rawhtml"
                "texonly" "verbatim" "verbatim*")))
     (toss-back-string "\\begingroup") (do-end-para))
    (else false))))

(define (do-end)
 (cond
  ((begin (set! *it* (get-grouped-environment-name-if-any)) *it*)
   (let ((env *it*))
     (toss-back-char *invisible-space*)
     (cond
      ((not (member env '("htmlonly" "document"))) (do-end-para)
       (toss-back-string "\\endgroup"))
      (else false))
     (toss-back-string (string-append "\\end" env))))
  (else (toss-back-char *invisible-space*) (toss-back-string "\\TIIPbye"))))

(define (latex-argnum-to-plain-argpat n)
 (let ((n n) (s null))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((<= n 0) (return s)) (else false))
       (unless %loop-returned (set! n (- n 1)) n)
       (unless %loop-returned
         (set! s (cons (integer->char (+ *int-corresp-to-0* n)) s))
         s)
       (unless %loop-returned (set! s (cons #\# s)) s)
       (if %loop-returned %loop-result (%loop))))))

(define (make-reusable-img . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (globalp false))
   (when (< 0 %lambda-rest-arg-len)
     (set! globalp (list-ref %lambda-rest-arg 0)))
   (cond ((not globalp) (set! globalp (globally-p)) globalp) (else false))
   (set! *imgdef-file-count* (+ *imgdef-file-count* 1))
   (ignorespaces)
   (let ((lhs (get-ctl-seq))
         (imgdef-file-stem
          (string-append *subjobname* *img-file-suffix* *imgdef-file-suffix*
           (write-to-string *imgdef-file-count*))))
     (dump-imgdef imgdef-file-stem)
     (tex-to-img imgdef-file-stem)
     (tex-def lhs null (string-append "\\TIIPreuseimage{" imgdef-file-stem "}")
      false false false false (and globalp *global-texframe*)))))

(define (valid-img-file-p f) false)

(define (source-img-file img-file-stem . alt)
 (let ((alt (if (null? alt) false (car alt))))
   (let ((img-file (string-append img-file-stem (find-img-file-extn))))
     (write-log #\()
     (write-log img-file)
     (write-log ':separation-space)
     (valid-img-file-p img-file)
     (emit "<img src=\"")
     (do-img-src (ensure-url-reachable img-file))
     (emit "\"")
     (emit " style=\"border: 0\"")
     (emit " alt=\"")
     (cond (alt (emit alt)) (else (emit "[") (emit img-file) (emit "]")))
     (emit "\">")
     (write-log #\))
     (write-log ':separation-space)
     true)))

(define (reuse-img) (source-img-file (ungroup (get-group))))

(define (get-def-parameters)
 (let ((params null) (c false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (set! c (snoop-actual-char))
       (cond ((not c) (terror 'get-def-parameters "Runaway definition?"))
             (else false))
       (cond
        ((= (catcode c) **escape**)
         (let ((x (get-ctl-seq)))
           (if (string=? x "\\par")
               (begin (begin (set! params (cons #\newline params)) params)
                (begin (set! params (cons #\newline params)) params))
               (begin (set! params (append (reverse (string->list x)) params))
                params))))
        ((= (catcode c) **bgroup**) (return))
        (else
         (cond ((char=? c #\newline) (get-actual-char) (ignorespaces))
               ((char-whitespace? c) (ignorespaces) (set! c #\ ) c)
               (else (get-actual-char)))
         (set! params (cons c params)) params))
       (if %loop-returned %loop-result (%loop))))
   (reverse params)))

(define (get-till-char c0)
 (list->string
  (reverse
   (let ((s null) (nesting 0) (escape-p false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((c (snoop-actual-char)))
           (cond ((not c) (terror 'get-till-char "File ended too soon"))
                 (else false))
           (cond
            (escape-p (set! s (cons (get-actual-char) s)) (set! escape-p false)
             escape-p)
            ((char=? c c0) (return s))
            ((= (catcode c) **escape**) (set! s (cons (get-actual-char) s))
             (set! escape-p true) escape-p)
            ((char=? c #\{) (set! s (cons (get-actual-char) s))
             (set! nesting (+ nesting 1)) nesting)
            ((char=? c #\}) (set! s (cons (get-actual-char) s))
             (set! nesting (- nesting 1)) nesting)
            ((> nesting 0) (set! s (cons (get-actual-char) s)) s)
            ((and (char-whitespace? c)
                  (not (char=? c0 #\newline))
                  (char-whitespace? c0))
             (return s))
            (else (set! s (cons (get-actual-char) s)) s)))
         (if %loop-returned %loop-result (%loop))))))))

(define (digit-to-int d) (- (char->integer d) *int-corresp-to-0*))

(define (do-halign) (do-end-para) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((or (not c) (not (char=? c #\{))) (terror 'do-halign "Missing {"))
         (else false)))
 (fluid-let ((*tabular-stack* (cons ':halign *tabular-stack*))) (bgroup)
  (emit "<table>")
  (let ((tmplt (get-halign-template)))
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (terror 'do-halign "Eof inside \\halign"))
                (else false))
          (cond
           ((char=? c #\}) (get-actual-char) (emit "</table>") (emit-newline)
            (egroup) (do-para) (return))
           (else (expand-halign-line tmplt))))
        (if %loop-returned %loop-result (%loop)))))))

(define (get-halign-template)
 (let ((s null))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((x (get-raw-token)))
         (cond ((not x) (terror 'get-halign-template "Eof in \\halign"))
               (else false))
         (cond
          ((string=? x "\\cr") (set! s (cons false s)) (return (reverse s)))
          ((string=? x "#") (set! s (cons true s)) s)
          ((string=? x "&") (set! s (cons false s)) s)
          (else (set! s (cons x s)) s)))
       (if %loop-returned %loop-result (%loop))))))

(define (expand-halign-line tmplt) (emit "<tr>")
 (let ((tmplt tmplt) (ins " ") (outer-loop-done false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond (outer-loop-done (return)) (else false))
       (unless %loop-returned
         (let ((x (get-raw-token)))
           (cond ((not x) (terror 'expand-halign-line "Eof in \\halign"))
                 (else false))
           (cond
            ((or (string=? x "&") (string=? x "\\cr"))
             (let ((r "{"))
               (let* ((%loop-returned false)
                      (%loop-result 0)
                      (return
                       (lambda %args
                         (set! %loop-returned true)
                         (set! %loop-result (and (pair? %args) (car %args))))))
                 (let %loop
                   ()
                   (cond
                    ((null? tmplt)
                     (terror 'expand-halign-line "Eof in \\halign"))
                    (else false))
                   (let ((y (car tmplt)))
                     (cond
                      ((not y) (emit "<td>")
                       (tex2page-string (string-append r "}"))
                       (cond
                        ((and (string=? x "\\cr") (string=? ins " "))
                         (emit-nbsp 1))
                        (else false))
                       (emit "</td>")
                       (if (string=? x "\\cr")
                           (begin (emit "</tr>") (emit-newline)
                            (begin (set! outer-loop-done true) outer-loop-done)
                            (return))
                           (begin
                            (let* ((%pop-old-stack tmplt)
                                   (%pop-top-value (car %pop-old-stack)))
                              (begin (set! tmplt (cdr %pop-old-stack)) tmplt)
                              %pop-top-value)
                            (begin (set! ins " ") ins) (return))))
                      ((eq? y true)
                       (let* ((%pop-old-stack tmplt)
                              (%pop-top-value (car %pop-old-stack)))
                         (begin (set! tmplt (cdr %pop-old-stack)) tmplt)
                         %pop-top-value)
                       (set! r (string-append r ins)) r)
                      (else
                       (let* ((%pop-old-stack tmplt)
                              (%pop-top-value (car %pop-old-stack)))
                         (begin (set! tmplt (cdr %pop-old-stack)) tmplt)
                         %pop-top-value)
                       (set! r (string-append r y)) r)))
                   (if %loop-returned %loop-result (%loop))))))
            (else (set! ins (string-append ins x)) ins))))
       (if %loop-returned %loop-result (%loop))))))

(define (do-settabs)
 (let ((settabs-spec ""))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((x (get-token/ps)))
         (cond ((not x) (terror 'do-settabs "Eof in \\settabs")) (else false))
         (cond
          ((string=? x "\\columns")
           (tex2page-string
            (string-append "\\TIIPsettabscolumns=" settabs-spec))
           (return))
          ((string=? x "\\cr") (plain-count "\\TIIPsettabscolumns" 0 false)
           (return))
          (else (set! settabs-spec (string-append settabs-spec x))
           settabs-spec)))
       (if %loop-returned %loop-result (%loop))))))

(define (do-tabalign) (emit-newline)
 (let ((table-width "") (num-cols (the-count "\\TIIPsettabscolumns")))
   (cond ((> num-cols 0) (set! table-width " width=100%") table-width)
         (else false))
   (emit "<table")
   (emit table-width)
   (emit ">")
   (emit-newline)
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (do-tabalign-row num-cols)
       (let ((x (get-token/ps)))
         (cond ((not x) (return))
               ((or (string=? x "\\+") (string=? x "\\tabalign")) true)
               (else (toss-back-string x) (return))))
       (if %loop-returned %loop-result (%loop)))))
 (emit "</table>") (emit-newline))

(define (do-tabalign-row num-cols) (emit "<tr>") (emit-newline)
 (let ((cell-contents "") (cell-width ""))
   (cond
    ((> num-cols 0)
     (set! cell-width
      (string-append " width=" (write-to-string (/ 100.0 num-cols)) "%"))
     cell-width)
    (else false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((x (get-token/ps)))
         (cond ((not x) (terror 'do-tablign "Eof in \\tabalign")) (else false))
         (cond
          ((or (string=? x "&") (string=? x "\\cr")) (emit "<td")
           (emit cell-width) (emit ">") (bgroup)
           (tex2page-string cell-contents) (egroup) (set! cell-contents "")
           (emit "</td>") (emit-newline)
           (cond ((string=? x "\\cr") (return)) (else false)))
          (else (set! cell-contents (string-append cell-contents x))
           cell-contents)))
       (if %loop-returned %loop-result (%loop)))))
 (emit "</tr>") (emit-newline))

(define (read-till-next-sharp k argpat) (ignorespaces)
 (let ((n (length argpat))
       (ss null)
       (i false)
       (s false)
       (c false)
       (outer-loop-done false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond (outer-loop-done (return (list i (list->string (reverse ss)))))
             (else false))
       (unless %loop-returned (set! i k) (set! s null) s)
       (unless %loop-returned
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (set! c (if (< i n) (list-ref argpat i) #\#))
             (cond ((char=? c #\#) (set! outer-loop-done true) (return))
                   (else false))
             (unless %loop-returned
               (let ((d (snoop-actual-char)))
                 (cond
                  ((and (char=? c #\ ) (char-whitespace? d)) (ignorespaces)
                   (set! i (+ i 1)) (set! s (cons c s)) s)
                  ((= (catcode d) **comment**) (do-comment))
                  ((and (char=? c #\newline)
                        (char-whitespace? d)
                        (or (munched-a-newline-p)
                            (begin (toss-back-char d) false)))
                   (set! i (+ i 1)) (set! s (cons c s)) s)
                  ((char=? c d) (get-actual-char) (set! i (+ i 1))
                   (set! s (cons c s)) s)
                  ((= i k)
                   (set! ss
                    (if
                     (and (char=? d #\{)
                          (or (null? ss)
                              (not (= (catcode (car ss)) **escape**))))
                     (append (get-group-as-reversed-chars) ss)
                     (begin (get-actual-char)
                      (cond
                       ((and (char-whitespace? d) (not (char=? d #\newline)))
                        (ignorespaces))
                       (else false))
                      (cons d ss))))
                   (return))
                  (else (set! ss (append s ss)) (return)))))
             (if %loop-returned %loop-result (%loop)))))
       (if %loop-returned %loop-result (%loop))))))

(define (read-macro-args argpat k r)
 (let ((n (length argpat)))
   (reverse
    (let ((k k) (r r))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (cond
           ((>= k n)
            (cond ((= k 0) (ignorespaces ':stop-after-first-newline))
                  (else false))
            (return r))
           (else false))
          (unless %loop-returned
            (let ((c (list-ref argpat k)))
              (cond
               ((= (catcode c) **parameter**)
                (cond
                 ((= k (sub1 n)) (ignorespaces)
                  (set! r (cons (get-till-char #\{) r)) (return r))
                 ((= k (- n 2)) (set! r (cons (ungroup (get-token)) r))
                  (return r))
                 (else
                  (let ((c2 (list-ref argpat (+ k 2))))
                    (if (= (catcode c2) **parameter**)
                        (begin (begin (set! k (+ k 2)) k)
                         (begin (set! r (cons (ungroup (get-token)) r)) r))
                        (apply
                         (lambda (k2 s . %mvb-rest-arg)
                           (set! k k2)
                           (set! r (cons s r))
                           r)
                         (read-till-next-sharp (+ k 2) argpat)))))))
               (else
                (let ((d (get-actual-char)))
                  (cond
                   ((not d)
                    (terror 'read-macro-args
                     "Eof before macro got enough args"))
                   (else false))
                  (cond
                   ((char=? c #\ )
                    (cond
                     ((not (char-whitespace? d))
                      (terror 'read-macro-args
                       "Use of macro doesn't match its definition."))
                     (else false)))
                   ((char=? c d) true)
                   (else
                    (terror 'read-macro-args
                     "Use of macro doesn't match its definition.")))
                  (set! k (+ k 1))
                  k)))))
          (if %loop-returned %loop-result (%loop))))))))

(define (expand-edef-macro rhs)
 (fluid-let ((*not-processing-p* true))
  (let ((tmp-stream (open-output-string)))
    (call-with-input-string/buffered rhs
     (lambda ()
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (let ((c (snoop-actual-char)))
             (cond ((not c) (return)) (else false))
             (display
              (cond
               ((= (catcode c) **escape**)
                (let ((x (get-ctl-seq)))
                  (toss-back-char *invisible-space*)
                  (cond
                   ((or (string=? x "\\the") (string=? x "\\number"))
                    (let ((x2 (get-raw-token/is)))
                      (toss-back-char *invisible-space*)
                      (toss-back-string x2)
                      (cond
                       ((ctl-seq-p x2)
                        (cond ((string=? x "\\the") (expand-the))
                              ((string=? x "\\number") (get-number))
                              (else 3735929054)))
                       (else x))))
                   ((string=? x "\\noexpand")
                    (let ((x2 (get-raw-token/is)))
                      (toss-back-char *invisible-space*)
                      x2))
                   ((begin (set! *it* (find-def x)) *it*)
                    (let ((y *it*))
                      (cond
                       ((and (null? (tdef*-argpat y))
                             (not (tdef*-optarg y))
                             (not (tdef*-thunk y))
                             (not (tdef*-prim y))
                             (not (tdef*-defer y)))
                        (toss-back-char *invisible-space*)
                        (toss-back-string (tdef*-expansion y)) "")
                       (else x))))
                   (else x))))
               (else (get-actual-char) c))
              tmp-stream))
           (if %loop-returned %loop-result (%loop))))))
    (get-output-string tmp-stream))))

(define (expand-tex-macro optarg argpat rhs lexical-catcodes)
 (let ((k 0))
   (let ((r
          (if (not optarg)
              null
              (begin (begin (set! k 2) k)
               (list
                (cond
                 ((begin (set! *it* (get-bracketed-text-if-any)) *it*) *it*)
                 (else optarg)))))))
     (let ((args (read-macro-args argpat k r)))
       (let ((rhs-n (string-length rhs)))
         (fluid-let ((*catcodes* lexical-catcodes))
          (list->string
           (letrec
            ((aux
              (lambda (k)
                (if (>= k rhs-n)
                    null
                    (let ((c (string-ref rhs k)))
                      (cond
                       ((char=? c #\\)
                        (let ((j (add1 k)) (s (list #\\)))
                          (let* ((%loop-returned false)
                                 (%loop-result 0)
                                 (return
                                  (lambda %args
                                    (set! %loop-returned true)
                                    (set! %loop-result
                                     (and (pair? %args) (car %args))))))
                            (let %loop
                              ()
                              (cond ((>= j rhs-n) (return (reverse s)))
                                    (else false))
                              (unless %loop-returned
                                (let ((c (string-ref rhs j)))
                                  (cond
                                   ((char-alphabetic? c) (set! j (+ j 1))
                                    (set! s (cons c s)) s)
                                   ((and (char=? c #\#) (> (length s) 1))
                                    (return
                                     (append (reverse s) (cons #\  (aux j)))))
                                   ((= (length s) 1)
                                    (return
                                     (append (reverse (cons c s))
                                             (aux (add1 j)))))
                                   (else
                                    (return (append (reverse s) (aux j)))))))
                              (if %loop-returned %loop-result (%loop))))))
                       ((char=? c #\#)
                        (if (= k (sub1 rhs-n))
                            (list #\#)
                            (let ((n (string-ref rhs (add1 k))))
                              (cond ((char=? n #\#) (cons #\# (aux (+ k 2))))
                                    ((and (char-numeric? n)
                                          (<= (digit-to-int n) (length args)))
                                     (append
                                      (string->list
                                       (list-ref args (sub1 (digit-to-int n))))
                                      (aux (+ k 2))))
                                    (else (cons #\# (aux (add1 k))))))))
                       (else (cons c (aux (add1 k))))))))))
            (aux 0)))))))))

(define (do-verbatimescapechar) (ignorespaces)
 (let ((c1 (get-actual-char)))
   (let ((c2 (get-actual-char)))
     (cond
      ((not (= (catcode c1) **escape**))
       (terror 'do-verbatimescapechar "Arg must be \\<char>"))
      (else false))
     (set! *esc-char-verb* c2)
     *esc-char-verb*)))

(define (do-verb-braced ign)
 (let ((%fluid-var-*catcodes* *catcodes*) (nesting 0))
   (fluid-let ((*catcodes* %fluid-var-*catcodes*)) (catcode #\\ 12)
    (catcode *esc-char-verb* 0)
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (let ((c (get-actual-char)))
          (cond ((not c) (terror 'do-verb-braced "Eof inside verbatim"))
                (else false))
          (cond
           ((= (catcode c) **escape**) (toss-back-char c)
            (let ((x (fluid-let ((*not-processing-p* true)) (get-ctl-seq))))
              (cond ((member x '("\\ " "\\{" "\\}")) (emit (string-ref x 1)))
                    (else
                     (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
                      (do-tex-ctl-seq-completely x))))))
           ((char=? c #\{) (emit #\{) (set! nesting (+ nesting 1)) nesting)
           ((char=? c #\})
            (if (= nesting 0)
                (return)
                (begin (emit #\})
                 (begin (set! nesting (- nesting 1)) nesting))))
           ((char=? c #\ )
            (emit (if *verb-visible-space-p* *verbatim-visible-space* #\ )))
           ((char=? c #\newline)
            (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                  (*verb-visible-space-p* (emit *verbatim-visible-space*))
                  (else (emit-newline))))
           ((and (char=? c #\-) (not *verb-display-p*)) (emit "&#x2011;"))
           (else (emit-html-char c))))
        (if %loop-returned %loop-result (%loop)))))))

(define (do-verb-delimed d)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (get-actual-char)))
       (cond ((not c) (terror 'do-verb-delimed "Eof inside verbatim"))
             (else false))
       (cond ((char=? c d) (return))
             ((char=? c #\ )
              (emit (if *verb-visible-space-p* *verbatim-visible-space* #\ )))
             ((char=? c #\newline)
              (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                    (*verb-visible-space-p* (emit *verbatim-visible-space*))
                    (else (emit-newline))))
             ((and (char=? c #\-) (not *verb-display-p*)) (emit "&#x2011;"))
             (else (emit-html-char c))))
     (if %loop-returned %loop-result (%loop)))))

(define (do-verb) (ignorespaces) (bgroup)
 (fluid-let ((*verb-visible-space-p* (eat-star)))
  (fluid-let ((*ligatures-p* false))
   (let ((d (get-actual-char)))
     (fluid-let ((*verb-display-p* (munched-a-newline-p)))
      (cond (*outputting-external-title-p* false)
            (*verb-display-p* (do-end-para) (emit "<pre class=verbatim>"))
            (else (emit "<code class=verbatim>")))
      ((if (char=? d #\{) do-verb-braced do-verb-delimed) d)
      (cond (*outputting-external-title-p* false)
            (*verb-display-p* (emit "</pre>") (do-noindent))
            (else (emit "</code>")))))))
 (egroup))

(define (do-verbc) (ignorespaces) (bgroup)
 (fluid-let ((*ligatures-p* false)) (emit "<code class=verbatim>")
  (emit-html-char (get-actual-char)) (emit "</code>"))
 (egroup))

(define (do-verbatiminput) (ignorespaces)
 (let ((f0 (get-filename-possibly-braced)))
   (let ((f (find-tex-file f0)))
     (cond
      ((and f (file-exists? f)) (do-end-para) (bgroup)
       (emit "<pre class=verbatim>")
       (let ((p (open-input-file f)))
         (let ((%with-open-file-res
                (let* ((%loop-returned false)
                       (%loop-result 0)
                       (return
                        (lambda %args
                          (set! %loop-returned true)
                          (set! %loop-result (and (pair? %args) (car %args))))))
                  (let %loop
                    ()
                    (let ((c
                           (let* ((%read-char-port p)
                                  (%read-char-res
                                   (if %read-char-port
                                       (read-char %read-char-port)
                                       (read-char))))
                             (when (eof-object? %read-char-res)
                               (set! %read-char-res false))
                             %read-char-res)))
                      (cond ((not c) (return)) (else false))
                      (emit-html-char c))
                    (if %loop-returned %loop-result (%loop))))))
           (cond
            (p ((if (input-port? p) close-input-port close-output-port) p))
            (else false))
           %with-open-file-res))
       (emit "</pre>") (egroup) (do-para))
      (else (non-fatal-error "File " f0 " not found"))))))

(define (get-char-definitely c0) (ignorespaces)
 (let ((c (get-actual-char)))
   (cond ((not c) (terror 'get-char-defnitely "Runaway argument"))
         (else false))
   (cond ((not (char=? c c0)) (terror 'get-char-defnitely "Missing" c0))
         (else false))))

(define (get-char-optionally cc) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond ((not c) false) ((member c cc) (get-actual-char) c) (else false))))

(define (get-unsigned-number-optionally) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond ((not c) false) ((char-numeric? c) (get-integer 10)) (else false))))

(define (opmac-verbinput-skip-lines i n)
 (let ((%dotimes-n n) (_ 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (>= _ %dotimes-n) (return) false)
       (unless %loop-returned
         (let ((x
                (let ((%read-line-res (read-line i)))
                  (when (eof-object? %read-line-res)
                    (set! %read-line-res false))
                  %read-line-res)))
           (cond
            ((not x)
             (terror 'do-opmac-verbinput "\\verbinput file ended too soon"))
            (else false))))
       (unless %loop-returned (set! _ (+ _ 1)))
       (if %loop-returned %loop-result (%loop))))))

(define (opmac-verbinput-print-lines i n)
 (if (eq? n true)
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (let ((x
                (let ((%read-line-res (read-line i)))
                  (when (eof-object? %read-line-res)
                    (set! %read-line-res false))
                  %read-line-res)))
           (cond ((not x) (return)) (else false))
           (emit-html-string x)
           (emit-newline))
         (if %loop-returned %loop-result (%loop))))
     (let ((%dotimes-n n) (_ 0))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (if (>= _ %dotimes-n) (return) false)
           (unless %loop-returned
             (let ((x
                    (let ((%read-line-res (read-line i)))
                      (when (eof-object? %read-line-res)
                        (set! %read-line-res false))
                      %read-line-res)))
               (cond
                ((not x)
                 (terror 'do-opmac-verbinput
                  "\\verbinput file ended too soon"))
                (else false))
               (emit-html-string x)
               (emit-newline)))
           (unless %loop-returned (set! _ (+ _ 1)))
           (if %loop-returned %loop-result (%loop)))))))

(define (do-opmac-verbinput)
 (let ((s1 (begin (get-char-definitely #\() (get-char-optionally '(#\+ #\-)))))
   (let ((n1 (get-unsigned-number-optionally)))
     (let ((s2 (get-char-optionally '(#\+ #\-))))
       (let ((n2 (get-unsigned-number-optionally)))
         (let ((f0 (begin (get-char-definitely #\)) (get-filename))))
           (let ((f (find-tex-file f0)))
             (let ((n
                    (and f
                         (let ((n (table-get f *opmac-verbinput-table*)))
                           (or n
                               (begin
                                (begin (table-put! f *opmac-verbinput-table* 0)
                                 (table-get f *opmac-verbinput-table*))
                                0))))))
               (cond
                ((and f (file-exists? f)) (do-end-para) (bgroup)
                 (emit "<pre class=verbatim>")
                 (let ((i (open-input-file f)))
                   (let ((%with-open-file-res
                          (cond
                           ((and s1 n1 s2 n2 (char=? s1 #\-) (char=? s2 #\+))
                            (opmac-verbinput-skip-lines i (+ n n1))
                            (opmac-verbinput-print-lines i n2)
                            (set! n (+ n (+ n1 n2))) n)
                           ((and (not s1) n1 s2 n2 (char=? s2 #\-))
                            (opmac-verbinput-skip-lines i (sub1 n1))
                            (opmac-verbinput-print-lines i (add1 (- n2 n1)))
                            (set! n n2) n)
                           ((and (not s1) n1 s2 n2 (char=? s2 #\+))
                            (opmac-verbinput-skip-lines i (sub1 n1))
                            (opmac-verbinput-print-lines i n2)
                            (set! n (+ (sub1 n1) n2)) n)
                           ((and s1 n1 (not s2) (not n2) (char=? s1 #\-))
                            (opmac-verbinput-print-lines i n1) (set! n n1) n)
                           ((and s1 n1 (not s2) (not n2) (char=? s1 #\+))
                            (opmac-verbinput-skip-lines i n)
                            (opmac-verbinput-print-lines i n1)
                            (set! n1 (+ n1 1)) n1)
                           ((and (not s1) n1 s2 (not n2) (char=? s2 #\-))
                            (opmac-verbinput-skip-lines i (sub1 n1))
                            (opmac-verbinput-print-lines i true) (set! n 0) n)
                           ((and s1 (not n1) (not s2) (not n2) (char=? s1 #\+))
                            (opmac-verbinput-skip-lines i n)
                            (opmac-verbinput-print-lines i true) (set! n 0) n)
                           ((and s1 (not n1) (not s2) (not n2) (char=? s1 #\-))
                            (opmac-verbinput-print-lines i true) (set! n 0) n)
                           (else
                            (terror 'do-opmac-verbinput "Malformed \\verbinput"
                             s1 n1 s2 n2 f)))))
                     (cond
                      (i
                       ((if (input-port? i) close-input-port close-output-port)
                        i))
                      (else false))
                     %with-open-file-res))
                 (table-put! f *opmac-verbinput-table* n)
                 (table-get f *opmac-verbinput-table*) (emit "</pre>") (egroup)
                 (do-para))
                (else (non-fatal-error "File " f0 " not found")))))))))))

(define (do-verbwritefile)
 (let ((f (get-filename-possibly-braced)))
   (let ((e (file-extension f)))
     (cond ((not e) (set! e ".tex") (set! f (string-append f e)) f)
           (else false))
     (cond
      (*verb-stream*
       (let ((%close-port-arg *verb-stream*))
         ((if (input-port? %close-port-arg) close-input-port close-output-port)
          %close-port-arg)))
      (else false))
     (set! *verb-written-files* (cons f *verb-written-files*))
     (cond
      ((string-ci=? e ".mp") (set! *mp-files* (cons f *mp-files*)) *mp-files*)
      (else false))
     (set! *verb-stream*
      (let* ((%f f)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *verb-stream*)))

(define (verb-ensure-output-stream)
 (cond
  ((not *verb-stream*)
   (let ((output-file (string-append *jobname* ".txt")))
     (set! *verb-stream*
      (let* ((%f output-file)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f)))))
     *verb-stream*))
  (else false)))

(define (dump-groupoid p) (ignorespaces)
 (let ((write-char write-char) (d (get-actual-char)))
   (cond ((not p) (set! write-char (lambda (x y) false)) write-char)
         (else false))
   (case d
     ((#\{)
      (let ((nesting 0) (c false))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (set! c (get-actual-char))
            (cond ((not c) (terror 'dump-groupoid "Eof inside verbatim"))
                  (else false))
            (cond
             ((= (catcode c) **escape**) (write-char c p)
              (write-char (get-actual-char) p))
             ((char=? c #\{) (write-char c p) (set! nesting (+ nesting 1))
              nesting)
             ((char=? c #\})
              (cond ((= nesting 0) (return))
                    (else (write-char c p) (set! nesting (- nesting 1))
                     nesting)))
             (else (write-char c p)))
            (if %loop-returned %loop-result (%loop))))))
     (else
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (let ((c (get-actual-char)))
            (cond ((not c) (terror 'dump-groupoid "Eof inside verbatim"))
                  (else false))
            (cond ((char=? c d) (return)) (else false))
            (write-char c p))
          (if %loop-returned %loop-result (%loop))))))))

(define (do-makehtmlimage) (ignorespaces)
 (cond
  ((not (char=? (snoop-actual-char) #\{))
   (terror 'do-makehtmlimage "\\makehtmlimage's argument must be a group"))
  (else false))
 (call-with-html-image-stream dump-groupoid))

(define (do-verbwrite) (verb-ensure-output-stream)
 (dump-groupoid *verb-stream*))

(define (do-string)
 (let ((c (snoop-actual-char)))
   (cond ((not c) false)
         ((= (catcode c) **escape**) (get-actual-char)
          (toss-back-char *invisible-space*)
          (toss-back-string "\\TIIPbackslash"))
         ((= (catcode c) **comment**) (eat-till-eol) (do-string))
         (else (toss-back-char (get-actual-char))))))

(define (do-verbatim-latex env) (do-end-para) (bgroup)
 (emit "<pre class=verbatim>")
 (fluid-let ((*verb-visible-space-p* (eat-star)))
  (cond ((string=? env "Verbatim") (get-bracketed-text-if-any)) (else false))
  (cond (*verb-visible-space-p* (set! env (string-append env "*")) env)
        (else false))
  (munched-a-newline-p)
  (let ((%fluid-var-*ligatures-p* false) (c false))
    (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (set! c (snoop-actual-char))
         (cond ((not c) (terror 'do-verbatim-latex "Eof inside verbatim"))
               (else false))
         (cond
          ((char=? c #\\)
           (let ((cs (get-ctl-seq)))
             (if (string=? cs "\\end")
                 (cond
                  ((begin (set! *it* (get-grouped-environment-name-if-any))
                    *it*)
                   (let ((e *it*))
                     (cond ((string=? *it* env) (return))
                           (else (emit-html-string cs) (emit-html-char #\{)
                            (emit-html-string e) (emit-html-char #\})))))
                  (else (emit-html-string cs)))
                 (begin (emit-html-string cs)))))
          ((char=? c #\ ) (get-actual-char)
           (emit (if *verb-visible-space-p* *verbatim-visible-space* #\ )))
          (else (emit-html-char (get-actual-char))))
         (if %loop-returned %loop-result (%loop)))))))
 (emit "</pre>") (egroup) (do-para))

(define (do-endverbatim-eplain) (set! *inside-eplain-verbatim-p* false)
 *inside-eplain-verbatim-p*)

(define (do-alltt) (do-end-para) (bgroup) (emit "<pre class=verbatim>")
 (munched-a-newline-p)
 (let ((%fluid-var-*in-alltt-p* true) (c false))
   (fluid-let ((*in-alltt-p* %fluid-var-*in-alltt-p*))
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (set! c (snoop-actual-char))
        (cond ((not c) (terror 'do-alltt "Eof inside alltt")) (else false))
        (case c
          ((#\\) (do-tex-ctl-seq (get-ctl-seq)))
          ((#\{) (get-actual-char) (bgroup))
          ((#\}) (get-actual-char) (egroup))
          (else (emit-html-char (get-actual-char))))
        (cond ((not *in-alltt-p*) (return)) (else false))
        (if %loop-returned %loop-result (%loop)))))))

(define (do-end-alltt) (emit "</pre>") (egroup) (do-para)
 (set! *in-alltt-p* false) *in-alltt-p*)

(define (do-scm-set-specialsymbol)
 (let ((sym (get-peeled-group)))
   (let ((xln (get-group)))
     (table-put! sym *scm-special-symbols* xln)
     (table-get sym *scm-special-symbols*))))

(define (do-scm-unset-specialsymbol)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (cond ((not (snoop-actual-char)) (return)) (else false))
        (unless %loop-returned
          (table-put! (scm-get-token) *scm-special-symbols* false)
          (table-get (scm-get-token) *scm-special-symbols*))
        (if %loop-returned %loop-result (%loop)))))))

(define (scm-emit-html-char c)
 (cond
  ((not (not c))
   (cond (*scm-dribbling-p* (write-char c *verb-stream*)) (else false))
   (if (and (char=? c #\-) (not *verb-display-p*))
       (emit "&#x2011;")
       (emit-html-char c)))
  (else false)))

(define (scm-output-next-chunk)
 (let ((c (snoop-actual-char)))
   (cond
    ((and *slatex-math-escape* (char=? c *slatex-math-escape*))
     (scm-escape-into-math))
    ((char=? c #\;) (scm-output-comment) (do-end-para))
    ((char=? c #\") (scm-output-string))
    ((char=? c #\#) (scm-output-hash))
    ((char=? c #\,) (get-actual-char) (emit "<span class=keyword>")
     (scm-emit-html-char c)
     (let ((c (snoop-actual-char)))
       (cond ((char=? c #\@) (get-actual-char) (scm-emit-html-char c))
             (else false)))
     (emit "</span>"))
    ((or (char=? c #\') (char=? c #\`)) (get-actual-char)
     (emit "<span class=keyword>") (scm-emit-html-char c) (emit "</span>"))
    ((or (char-whitespace? c) (member c *scm-token-delims*)) (get-actual-char)
     (scm-emit-html-char c))
    (else (scm-output-token (scm-get-token))))))

(define (scm-set-mathescape yes-p)
 (let ((c
        (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 11)
         (catcode *esc-char-verb* 11) (string-ref (ungroup (get-group)) 0))))
   (cond
    (yes-p (set! *slatex-math-escape* c)
     (set! *scm-token-delims* (cons *slatex-math-escape* *scm-token-delims*))
     *scm-token-delims*)
    (else (set! *slatex-math-escape* false)
     (set! *scm-token-delims* (remove c *scm-token-delims*))
     *scm-token-delims*))))

(define (scm-escape-into-math) (get-actual-char)
 (let ((math-text (get-till-char *slatex-math-escape*)))
   (get-actual-char)
   (cond
    ((not (string=? math-text "")) (emit "<span class=variable>")
     (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 11)
      (catcode *esc-char-verb* 11)
      (tex2page-string (string-append "$" math-text "$")))
     (emit "</span>"))
    (else false))))

(define (scm-output-slatex-comment)
 (let ((s (get-line)))
   (emit "<span class=comment>")
   (cond (*scm-dribbling-p* (display s *verb-stream*) (newline *verb-stream*))
         (else false))
   (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
    (catcode *esc-char-verb* 11) (tex2page-string s))
   (do-end-para)
   (emit "</span>")
   (toss-back-char #\newline)))

(define (scm-output-verbatim-comment) (emit "<span class=comment>")
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (get-actual-char)))
       (cond
        ((or (not c) (char=? c #\newline)) (emit "</span>")
         (scm-emit-html-char c) (return))
        ((and (char-whitespace? c)
              (let ((c2 (snoop-actual-char)))
                (or (not c2) (char=? c2 #\newline))))
         (emit "</span>") (scm-emit-html-char (get-actual-char)) (return))
        (else (scm-emit-html-char c))))
     (if %loop-returned %loop-result (%loop)))))

(define (scm-output-comment)
 ((if (tex2page-flag-boolean "\\TZPslatexcomments")
      scm-output-slatex-comment
      scm-output-verbatim-comment)))

(define (scm-output-extended-comment) (get-actual-char)
 (emit "<span class=comment>") (scm-emit-html-char #\#)
 (scm-emit-html-char #\|)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (let ((c (get-actual-char)))
       (cond ((not c) (return))
             ((char=? c #\|)
              (let ((c2 (snoop-actual-char)))
                (cond ((not c2) (scm-emit-html-char c) (return))
                      ((char=? c2 #\#) (get-actual-char) (return))
                      (else (scm-emit-html-char c)))))
             (else (scm-emit-html-char c))))
     (if %loop-returned %loop-result (%loop))))
 (scm-emit-html-char #\|) (scm-emit-html-char #\#) (emit "</span>"))

(define (scm-output-string) (get-actual-char) (emit "<span class=selfeval>")
 (scm-emit-html-char #\")
 (let ((esc-p false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (let ((c (get-actual-char)))
         (case c
           ((#\")
            (cond ((not esc-p) (return)) (else false))
            (scm-emit-html-char c)
            (set! esc-p false)
            esc-p)
           ((#\\) (scm-emit-html-char c) (set! esc-p (not esc-p)) esc-p)
           (else (scm-emit-html-char c) (set! esc-p false) esc-p)))
       (if %loop-returned %loop-result (%loop)))))
 (scm-emit-html-char #\") (emit "</span>"))

(define (scm-output-hash) (get-actual-char)
 (let ((c (snoop-actual-char)))
   (cond
    ((not c) (emit "<span class=selfeval>") (scm-emit-html-char #\#)
     (emit "</span>"))
    ((char=? c #\|) (scm-output-extended-comment))
    (else (toss-back-char #\#) (scm-output-token (scm-get-token))))))

(define (scm-output-token s)
 (case (scm-get-type s)
   ((:special-symbol)
    (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
     (tex2page-string (table-get s *scm-special-symbols*))))
   ((:keyword)
    (emit "<span class=keyword>")
    (scm-display-token s)
    (emit "</span>"))
   ((:global)
    (emit "<span class=global>")
    (scm-display-token s)
    (emit "</span>"))
   ((:selfeval)
    (emit "<span class=selfeval>")
    (scm-display-token s)
    (emit "</span>"))
   ((:builtin)
    (emit "<span class=builtin>")
    (scm-display-token s)
    (emit "</span>"))
   ((:background) (scm-display-token s))
   (else
    (emit "<span class=variable>")
    (scm-display-token s)
    (emit "</span>"))))

(define (scm-display-token s)
 (let ((n (string-length s)) (k 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((not (< k n)) (return)) (else false))
       (unless %loop-returned (scm-emit-html-char (string-ref s k)))
       (unless %loop-returned (set! k (+ k 1)) k)
       (if %loop-returned %loop-result (%loop))))))

(define (do-scm-braced result-p) (get-actual-char)
 (let ((display-p (munched-a-newline-p)))
   (cond
    ((not display-p) (emit "<code class=scheme")
     (cond (result-p (emit "response")) (else false)) (emit ">"))
    (else (do-end-para) (emit "<pre class=scheme>")))
   (bgroup)
   (let ((%fluid-var-*catcodes* *catcodes*)
         (%fluid-var-*verb-display-p* display-p)
         (nesting 0)
         (c false))
     (fluid-let
      ((*catcodes* %fluid-var-*catcodes*)
       (*verb-display-p* %fluid-var-*verb-display-p*))
      (catcode #\\ 12) (catcode *esc-char-verb* 0)
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (set! c (snoop-actual-char))
          (cond ((not c) (terror 'do-scm-braced "Eof inside verbatim"))
                (else false))
          (cond
           ((= (catcode c) **escape**)
            (let ((x (get-ctl-seq)))
              (cond
               ((member x '("\\ " "\\{" "\\}"))
                (scm-emit-html-char (string-ref x 1)))
               (else
                (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
                 (do-tex-ctl-seq-completely x))))))
           ((char=? c #\{) (get-actual-char) (scm-emit-html-char c)
            (set! nesting (+ nesting 1)) nesting)
           ((char=? c #\}) (get-actual-char)
            (case nesting
              ((0) (return))
              (else
               (scm-emit-html-char c)
               (set! nesting (- nesting 1))
               nesting)))
           (else (scm-output-next-chunk)))
          (if %loop-returned %loop-result (%loop))))))
   (egroup)
   (cond ((not display-p) (emit "</code>"))
         (else (emit "</pre>") (do-noindent)))))

(define (do-scm-delimed result-p)
 (let ((d (get-actual-char)))
   (let ((display-p (munched-a-newline-p)))
     (cond
      ((not display-p) (emit "<code class=scheme")
       (cond (result-p (emit "response")) (else false)) (emit ">"))
      (else (do-end-para) (emit "<pre class=scheme>")))
     (let ((%fluid-var-*verb-display-p* display-p)
           (%fluid-var-*scm-token-delims* (cons d *scm-token-delims*))
           (c false))
       (fluid-let
        ((*verb-display-p* %fluid-var-*verb-display-p*)
         (*scm-token-delims* %fluid-var-*scm-token-delims*))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (set! c (snoop-actual-char))
            (cond ((not c) (terror 'do-scm-delimed "Eof inside verbatim"))
                  (else false))
            (cond ((char=? c d) (get-actual-char) (return)) (else false))
            (unless %loop-returned (scm-output-next-chunk))
            (if %loop-returned %loop-result (%loop))))))
     (cond ((not display-p) (emit "</code>"))
           (else (emit "</pre>") (do-noindent))))))

(define (do-scm . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (result-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! result-p (list-ref %lambda-rest-arg 0)))
   (cond (*outputting-external-title-p* (do-verb))
         (else (ignorespaces) (bgroup)
          (fluid-let ((*ligatures-p* false))
           ((if (char=? (snoop-actual-char) #\{) do-scm-braced do-scm-delimed)
            result-p))
          (egroup)))))

(define (do-scminput) (ignorespaces) (do-end-para) (bgroup)
 (emit "<pre class=scheme>")
 (let ((f
        (add-dot-tex-if-no-extension-provided (get-filename-possibly-braced)))
       (c false))
   (call-with-input-file/buffered f
    (lambda ()
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (set! c (snoop-actual-char))
          (cond ((not c) (return)) (else false))
          (unless %loop-returned (scm-output-next-chunk))
          (if %loop-returned %loop-result (%loop)))))))
 (emit "</pre>") (egroup) (do-noindent))

(define (do-scmdribble) (verb-ensure-output-stream)
 (fluid-let ((*scm-dribbling-p* true)) (do-scm false)) (newline *verb-stream*))

(define (string-is-all-dots-p s)
 (let ((%dotimes-n (string-length s)) (i 0))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (>= i %dotimes-n) (return true) false)
       (unless %loop-returned
         (cond ((not (char=? (string-ref s i) #\.)) (return false))
               (else false)))
       (unless %loop-returned (set! i (+ i 1)))
       (if %loop-returned %loop-result (%loop))))))

(define (string-is-flanked-by-stars-p s)
 (let ((n (string-length s)))
   (and (>= n 3) (char=? (string-ref s 0) (string-ref s (sub1 n)) #\*))))

(define (scm-get-type s)
 (cond ((table-get s *scm-special-symbols*) ':special-symbol)
       ((table-get s *scm-keywords*) ':keyword)
       ((table-get s *scm-builtins*) ':builtin)
       ((table-get s *scm-variables*) ':variable)
       ((string-is-flanked-by-stars-p s) ':global)
       ((begin
         (set! *it*
          (let ((%position-v #\:) (%position-s s))
            (cond
             ((string? %position-s) (string-index %position-s %position-v))
             (else (list-position %position-v %position-s)))))
         *it*)
        (if (= *it* 0) ':selfeval ':variable))
       ((string-is-all-dots-p s) ':background)
       ((char=? (string-ref s 0) #\#) ':selfeval)
       ((string->number s) ':selfeval)
       (else ':variable)))

(define (eat-star)
 (let ((c (snoop-actual-char)))
   (if (and (not (not c)) (char=? c #\*)) (get-actual-char) false)))

(define (do-cr z) (ignorespaces)
 (case (and (pair? *tabular-stack*) (car *tabular-stack*))
   ((:tabular)
    (get-bracketed-text-if-any)
    (egroup)
    (emit "</td></tr>")
    (emit-newline)
    (emit "<tr><td valign=top ")
    (do-tabular-multicolumn))
   ((:eqnarray*)
    (emit "</td></tr>")
    (emit-newline)
    (set! *equation-position* 0)
    (emit "<tr><td class=rightline>"))
   ((:eqnarray)
    (emit "</td>")
    (cond
     (*equation-numbered-p* (emit "<td>(") (emit *equation-number*)
      (bump-dotted-counter "equation") (emit ")</td>"))
     (else (set! *equation-numbered-p* true) *equation-numbered-p*))
    (emit "</tr>")
    (emit-newline)
    (set! *equation-position* 0)
    (emit "<tr><td class=rightline>"))
   ((:ruled-table) (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))
   ((:minipage :tabbing)
    (get-bracketed-text-if-any)
    (emit "<br>")
    (emit-newline))
   ((:eqalign :eqalignno :displaylines :pmatrix :mathbox)
    (cond
     ((not (char=? (snoop-actual-char) #\}))
      (set! *math-height* (+ *math-height* 1)) (emit "</td></tr>")
      (emit-newline) (emit "<tr><td class=centerline>")
      (set! *equation-position* 0) (emit-newline))
     (else false)))
   ((:header) (emit #\ ))
   (else
    (cond
     ((and (eqv? *tex-format* ':latex) (string=? z "\\\\"))
      (get-bracketed-text-if-any)
      (let ((c (snoop-actual-char)))
        (cond ((and (not (not c)) (char=? c #\*)) (get-actual-char))
              (else false)))
      (emit "<br>") (emit-newline))
     (else false)))))

(define (do-ruledtable)
 (set! *tabular-stack* (cons ':ruled-table *tabular-stack*))
 (emit "<table border=2><tr><td>") (emit-newline))

(define (do-endruledtable) (emit-newline) (emit "</td></tr></table>")
 (emit-newline) (pop-tabular-stack ':ruled-table))

(define (do-tabular . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (mathp false))
   (when (< 0 %lambda-rest-arg-len) (set! mathp (list-ref %lambda-rest-arg 0)))
   (do-end-para)
   (get-bracketed-text-if-any)
   (bgroup)
   (cond
    ((not mathp)
     (add-postlude-to-top-frame
      (let ((old-math-mode-p *math-mode-p*)
            (old-in-display-math-p *in-display-math-p*))
        (set! *math-mode-p* false)
        (set! *in-display-math-p* false)
        (lambda ()
          (set! *math-mode-p* old-math-mode-p)
          (set! *in-display-math-p* old-in-display-math-p)
          *in-display-math-p*))))
    (else false))
   (let ((border-width
          (if
           (let ((%position-v #\|) (%position-s (get-group)))
             (cond
              ((string? %position-s) (string-index %position-s %position-v))
              (else (list-position %position-v %position-s))))
           1
           0)))
     (set! *tabular-stack* (cons ':tabular *tabular-stack*))
     (emit "<table border=")
     (emit border-width)
     (emit "><tr><td valign=top ")
     (do-tabular-multicolumn))))

(define (do-end-tabular) (egroup) (do-end-para) (emit "</td></tr></table>")
 (pop-tabular-stack ':tabular) (egroup))

(define (do-tabular-colsep) (egroup) (emit "</td><td valign=top ")
 (do-tabular-multicolumn))

(define (do-tabular-multicolumn)
 (let* ((%loop-returned false)
        (%loop-result 0)
        (return
         (lambda %args
           (set! %loop-returned true)
           (set! %loop-result (and (pair? %args) (car %args))))))
   (let %loop
     ()
     (ignorespaces)
     (let ((c (snoop-actual-char)))
       (cond ((not (and (char? c) (char=? c #\\))) (return)) (else false))
       (let ((x (get-ctl-seq)))
         (cond ((string=? x "\\hline") true)
               ((string=? x "\\multicolumn")
                (let ((n (ungroup (get-token))))
                  (get-token)
                  (emit " colspan=")
                  (emit n)
                  (return)))
               (else (toss-back-char *invisible-space*) (toss-back-string x)
                (return)))))
     (if %loop-returned %loop-result (%loop))))
 (emit ">") (bgroup))

(define (do-ruledtable-colsep) (emit-newline) (emit "</td><td") (ignorespaces)
 (let ((c (snoop-actual-char)))
   (cond
    ((char=? c #\\)
     (let ((x (get-ctl-seq)))
       (if (string=? x "\\multispan")
           (let ((n (ungroup (get-token))))
             (emit " colspan=")
             (emit n))
           (toss-back-string x))))
    (else false)))
 (emit ">") (emit-newline))

(define (do-romannumeral . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (upcase-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! upcase-p (list-ref %lambda-rest-arg 0)))
   (let ((n (get-number-or-false)))
     (cond (n (emit (number-to-roman n upcase-p))) (else false)))))

(define (do-tex-case-code kase)
 (cond
  ((not (inside-false-world-p))
   (let ((c1 (get-tex-char-spec)))
     (let ((c2 (begin (get-equal-sign) (get-tex-char-spec))))
       (let ((fr (top-texframe)))
         (table-put! c1
          ((case kase
             ((:uccode) texframe*-uccodes)
             ((:lccode) texframe*-lccodes))
           fr)
          c2)
         (table-get c1
          ((case kase
             ((:uccode) texframe*-uccodes)
             ((:lccode) texframe*-lccodes))
           fr))))))
  (else false)))

(define (tex-char-downcase c)
 (or (ormap (lambda (fr) (table-get c (texframe*-lccodes fr))) *tex-env*)
     (char-downcase c)))

(define (tex-char-upcase c)
 (or (ormap (lambda (fr) (table-get c (texframe*-uccodes fr))) *tex-env*)
     (char-upcase c)))

(define (do-flipcase kase)
 (emit
  (map 'string
       (case kase ((:uccode) tex-char-upcase) ((:lccode) tex-char-downcase))
       (get-token))))

(define (do-addtocounter)
 (let ((counter-name (get-peeled-group)))
   (let ((new-value (string->number (get-token-or-peeled-group))))
     (set-latex-counter-aux counter-name true new-value))))

(define (do-setcounter)
 (let ((counter-name (get-peeled-group)))
   (let ((new-value (string->number (get-token-or-peeled-group))))
     (set-latex-counter-aux counter-name false new-value))))

(define (do-stepcounter)
 (let ((counter-name (get-peeled-group)))
   (set-latex-counter-aux counter-name true 1)))

(define (set-latex-counter-aux counter-name addp new-value)
 (cond
  ((begin (set! *it* (table-get counter-name *dotted-counters*)) *it*)
   (let ((counter *it*))
     (if addp
         (begin
          (set!counter*-value counter (+ (counter*-value counter) new-value))
          (counter*-value counter))
         (begin (set!counter*-value counter new-value)
          (counter*-value counter)))))
  (else
   (let ((count-seq (string-append "\\" counter-name)))
     (cond
      ((begin (set! *it* (section-ctl-seq-p count-seq)) *it*)
       (let ((n *it*))
         (table-put! n *section-counters*
          (if addp (+ new-value (table-get n *section-counters* 0)) new-value))
         (table-get n *section-counters*)))
      ((find-count count-seq)
       (tex-gdef-count count-seq
        (if addp (+ new-value (get-gcount count-seq)) new-value)))
      (else false))))))

(define (do-tex-prim z)
 (let ((y false))
   (cond
    ((begin (set! *it* (find-def z)) *it*) (set! y *it*)
     (cond
      ((begin (set! *it* (tdef*-defer y)) *it*) (set! y *it*)
       (toss-back-string y))
      ((begin (set! *it* (tdef*-thunk y)) *it*) (set! y *it*) (y))
      (else
       (expand-tex-macro (tdef*-optarg y) (tdef*-argpat y) (tdef*-expansion y)
        (tdef*-catcodes y)))))
    (*math-mode-p* (do-math-ctl-seq z))
    (else (trace-if (> (the-count "\\tracingcommands") 0) "Ignoring " z)))))

(define (do-char)
 (let ((n (get-number-or-false)))
   (cond ((not n) (terror 'do-char "not a char")) (else false))
   (cond ((< n 128) (emit-html-char (integer->char n)))
         (else (emit "&#x") (emit (write-to-string n ':base 16)) (emit ";")))))

(define (tex-math-bb c)
 (case c
   ((#\C) "&#x2102;")
   ((#\H) "&#x210d;")
   ((#\N) "&#x2115;")
   ((#\P) "&#x2119;")
   ((#\Q) "&#x211a;")
   ((#\R) "&#x211d;")
   ((#\Z) "&#x2124;")
   (else
    (string-append "&#x"
     (write-to-string (+ 120120 (- (char->integer c) (char->integer #\A)))
                      ':base
                      16)
     ";"))))

(define (tex-math-cal c)
 (string-append "&#x"
  (write-to-string (+ 120016 (- (char->integer c) (char->integer #\A)))
                   ':base
                   16)
  ";"))

(define (tex-math-frak c)
 (string-append "&#x"
  (write-to-string
   (if (char-upper-case? c)
       (+ 120172 (- (char->integer c) (char->integer #\A)))
       (+ 120198 (- (char->integer c) (char->integer #\a))))
   ':base
   16)
  ";"))

(define (emit-math-alpha-char c)
 (case *math-font*
   ((:rm) (emit c))
   ((:bf) (emit "<b>") (emit c) (emit "</b>"))
   ((:bb) (emit (if (char-upper-case? c) (tex-math-bb c) c)))
   ((:cal) (emit (if (char-upper-case? c) (tex-math-cal c) c)))
   ((:frak) (emit (if (char-alphabetic? c) (tex-math-frak c) c)))
   (else (emit "<em>") (emit c) (emit "</em>"))))

(define (do-tex-char c)
 (cond ((= (catcode c) **comment**) (do-comment))
       ((inside-false-world-p) true)
       ((= (catcode c) **space**) (emit-space c))
       ((char=? c #\{) (bgroup))
       ((char=? c #\}) (egroup))
       ((= (catcode c) **math**) (do-math))
       ((char=? c #\-) (do-hyphen))
       ((char=? c #\`) (do-lsquo))
       ((char=? c #\') (do-rsquo))
       ((char=? c #\~) (emit-nbsp 1))
       ((char=? c #\!) (do-excl))
       ((char=? c #\?) (do-quest))
       ((or (char=? c #\<) (char=? c #\>) (char=? c #\")) (emit-html-char c))
       ((= (catcode c) **alignment**)
        (cond
         (*tabular-stack*
          (case (car *tabular-stack*)
            ((:pmatrix :eqalign :displaylines :mathbox)
             (emit "&#xa0;</td><td class=centerline>&#xa0;"))
            ((:eqalignno)
             (set! *equation-position* (add1 *equation-position*))
             (emit "</td><td")
             (cond
              ((= *equation-position* 2) (emit " width=30% class=rightline"))
              (else false))
             (emit ">"))
            ((:eqnarray :eqnarray*)
             (set! *equation-position* (add1 *equation-position*))
             (emit "</td><td")
             (cond
              ((= *equation-position* 1) (emit " class=centerline width=2%"))
              (else false))
             (emit ">"))
            ((:tabular) (do-tabular-colsep))
            ((:ruled-table) (do-ruledtable-colsep))))
         (else (emit-html-char c))))
       ((char=? c #\|)
        (if (eq? (car *tabular-stack*) ':ruled-table)
            (do-ruledtable-colsep)
            (emit c)))
       ((char=? c #\newline) (do-newline))
       (else
        (cond
         (*math-mode-p*
          (case c
            ((#\^) (do-sup))
            ((#\_) (do-sub))
            ((#\+ #\=)
             (cond ((not *math-script-mode-p*) (emit #\ )) (else false))
             (emit c)
             (cond ((not *math-script-mode-p*) (emit #\ )) (else false)))
            (else
             (if (char-alphabetic? c) (emit-math-alpha-char c) (emit c)))))
         ((and *in-small-caps-p* (char-lower-case? c)) (emit "<small>")
          (emit (char-upcase c)) (emit "</small>"))
         (else (emit c))))))

(define (do-tex-ctl-seq-completely x)
 (cond ((begin (set! *it* (resolve-defs x)) *it*) (tex2page-string *it*))
       ((begin (set! *it* (do-tex-prim (find-corresp-prim x))) *it*)
        (cond ((eq? *it* ':encountered-undefined-command) (emit x))
              (else false)))))

(define (inside-false-world-p)
 (or (member false *tex-if-stack*) (member 'skip *tex-if-stack*)))

(define (do-tex-ctl-seq z) (trace-if (> (the-count "\\tracingmacros") 0) z)
 (cond
  ((begin (set! *it* (resolve-defs z)) *it*)
   (let ((s *it*))
     (trace-if (> (the-count "\\tracingmacros") 0) "    --> " s)
     (toss-back-char *invisible-space*)
     (toss-back-string s)))
  ((and (inside-false-world-p) (not (if-aware-ctl-seq-p z))) false)
  ((string=? z "\\enddocument") (probably-latex) ':encountered-bye)
  ((member z '("\\bye" "\\TIIPbye")) ':encountered-bye)
  ((member z '("\\endinput" "\\TIIPendinput"))
   (let ((next-token (get-token)))
     (cond ((and next-token (string=? next-token "\\fi")) (do-fi))
           (else false)))
   ':encountered-endinput)
  ((begin (set! *it* (find-countdef z)) *it*) (do-count= *it* false))
  ((begin (set! *it* (find-dimendef z)) *it*) (do-dimen= *it* false))
  ((begin (set! *it* (find-toksdef z)) *it*) (do-toks= *it* false))
  (else (do-tex-prim z))))

(define (generate-html)
 (fluid-let ((*outer-p* true))
  (let* ((%loop-returned false)
         (%loop-result 0)
         (return
          (lambda %args
            (set! %loop-returned true)
            (set! %loop-result (and (pair? %args) (car %args))))))
    (let %loop
      ()
      (let ((c (snoop-actual-char)))
        (cond ((not c) (return true))
              ((begin (set! *it* (resolve-cdefs c)) *it*)
               (let ((s *it*))
                 (toss-back-char *invisible-space*)
                 (toss-back-string s)))
              ((= (catcode c) **escape**)
               (case (do-tex-ctl-seq (get-ctl-seq))
                 ((:encountered-endinput) (return true))
                 ((:encountered-bye) (return ':encountered-bye))
                 (else true)))
              (else (get-actual-char) (do-tex-char c))))
      (if %loop-returned %loop-result (%loop))))))

(define (check-input-file-timestamp-p f)
 (cond
  ((let ((e (file-extension f)))
     (and e (member e '(".t2p" ".bbl" ".ind"))))
   false)
  (*inputting-boilerplate-p* false)
  (*ignore-timestamp-p* false)
  ((> *html-only* 0) false)
  ((and (>= (string-length f) 3)
        (char=? (string-ref f 0) #\.)
        (char=? (string-ref f 1) #\/))
   false)
  ((member f *verb-written-files*) false)
  (else true)))

(define (tex2page-string s) (call-with-input-string/buffered s generate-html))

(define (tex2page-file f) (write-log #\() (write-log f)
 (write-log ':separation-space)
 (trace-if (> (the-count "\\tracingcommands") 0) "Inputting file " f)
 (let ((%prog1-first-value (call-with-input-file/buffered f generate-html)))
   (write-log #\))
   (write-log ':separation-space)
   %prog1-first-value))

(define (tex2page-file-if-exists f)
 (cond ((file-exists? f) (tex2page-file f)) (else false)))

(define (ignorable-tex-file-p f)
 (let ((e (or (file-extension f) "")))
   (cond ((string-ci=? e ".sty") true)
         (else
          (cond
           ((string-ci=? e ".tex")
            (set! f (substring f 0 (- (string-length f) 4))) f)
           (else false))
          (cond ((string=? f "opmac") (tex-gdef-0arg "\\TZPopmac" "1") true)
                (else (member f *tex-files-to-ignore*)))))))

(define (do-input) (ignorespaces)
 (let ((f (get-filename-possibly-braced)))
   (let ((boilerplate-index *inputting-boilerplate-p*))
     (cond
      ((eqv? *inputting-boilerplate-p* 0)
       (set! *inputting-boilerplate-p* false) *inputting-boilerplate-p*)
      (else false))
     (fluid-let
      ((*inputting-boilerplate-p*
        (and boilerplate-index (add1 boilerplate-index))))
      (cond ((ignorable-tex-file-p f) false)
            ((member f '("miniltx" "miniltx.tex")) (catcode #\@ 11) false)
            ((member f '("texinfo" "texinfo.tex"))
             (cond
              ((begin (set! *it* (actual-tex-filename "texi2p")) *it*)
               (tex2page-file *it*))
              (else (terror 'do-input "File texi2p.tex not found"))))
            ((begin
              (set! *it*
               (actual-tex-filename f (check-input-file-timestamp-p f)))
              *it*)
             (tex2page-file *it*))
            (else (write-log #\() (write-log f) (write-log ':separation-space)
             (write-log "not found)") (write-log ':separation-space)))))))

(define (do-includeonly) (ignorespaces)
 (cond
  ((eq? *includeonly-list* true) (set! *includeonly-list* null)
   *includeonly-list*)
  (else false))
 (let ((c (get-actual-char)))
   (cond ((or (not c) (not (char=? c #\{))) (terror 'do-includeonly))
         (else false)))
 (fluid-let ((*filename-delims* (cons #\} (cons #\, *filename-delims*))))
  (let* ((%loop-returned false)
         (%loop-result 0)
         (return
          (lambda %args
            (set! %loop-returned true)
            (set! %loop-result (and (pair? %args) (car %args))))))
    (let %loop
      ()
      (ignorespaces)
      (let ((c (snoop-actual-char)))
        (cond ((not c) (terror 'do-includeonly)) (else false))
        (cond ((= (catcode c) **comment**) (eat-till-eol))
              ((char=? c #\,) (get-actual-char))
              ((char=? c #\}) (get-actual-char) (return))
              ((member c *filename-delims*) (terror 'do-includeonly))
              (else
               (set! *includeonly-list*
                (cons (get-filename) *includeonly-list*))
               *includeonly-list*)))
      (if %loop-returned %loop-result (%loop))))))

(define (do-include)
 (let ((f (ungroup (get-group))))
   (cond
    ((or (eq? *includeonly-list* true) (member f *includeonly-list*))
     (fluid-let
      ((*subjobname* (file-stem-name f)) (*img-file-count* 0)
       (*imgdef-file-count* 0))
      (tex2page-file
       (actual-tex-filename f (check-input-file-timestamp-p f)))))
    (else false))))

(define (eval-for-tex-only) (set! *eval-for-tex-only-p* true) (do-end-page)
 (ensure-file-deleted *html-page*) (set! *main-tex-file* false)
 (set! *html-page* ".eval4texignore")
 (set! *html*
  (make-ostream* ':stream
   (let* ((%f *html-page*)
          (%ee (list ':direction ':output ':if-exists ':supersede))
          (%direction (memv ':direction %ee))
          (%if-exists (memv ':if-exists %ee))
          (%if-does-not-exist ':error)
          (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
     (when %direction (set! %direction (cadr %direction)))
     (when %if-exists (set! %if-exists (cadr %if-exists)))
     (when %if-does-not-exist-from-user
       (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
     (cond
      ((eqv? %direction ':output)
       (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
         (delete-file %f))
       (open-output-file %f))
      ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
      (else (open-input-file %f))))))
 *html*)

(define (expand-ctl-seq-into-string cs)
 (fluid-let ((*html* (make-html-output-stream))) (do-tex-ctl-seq cs))
 (html-output-stream-to-string *html*))

(define (call-with-html-output-going-to p th)
 (fluid-let ((*html* (make-ostream* ':stream p))) (th)))

(define (call-external-programs-if-necessary)
 (let ((run-bibtex-p
        (cond ((not *using-bibliography-p*) false)
              ((not
                (file-exists?
                 (string-append *aux-dir/* *jobname* *bib-aux-file-suffix*
                  ".aux")))
               false)
              ((member ':bibliography *missing-pieces*) true)
              (*source-changed-since-last-run-p*
               (flag-missing-piece ':fresh-bibliography) true)
              (else false)))
       (run-makeindex-p
        (cond ((not *using-index-p*) false)
              ((not
                (file-exists?
                 (string-append *aux-dir/* *jobname* *index-file-suffix*
                  ".idx")))
               false)
              ((member ':fresh-index *missing-pieces*) false)
              ((member ':index *missing-pieces*) true)
              (*source-changed-since-last-run-p*
               (flag-missing-piece ':fresh-index) true)
              (else false))))
   (cond
    (run-bibtex-p (write-log ':separation-newline)
     (write-log "Running: bibtex ") (write-log *aux-dir/*)
     (write-log *jobname*) (write-log *bib-aux-file-suffix*) (write-log #\ )
     (system
      (string-append "bibtex " *aux-dir/* *jobname* *bib-aux-file-suffix*))
     (cond
      ((not
        (file-exists?
         (string-append *aux-dir/* *jobname* *bib-aux-file-suffix* ".bbl")))
       (write-log " ... failed; try manually"))
      (else false))
     (write-log ':separation-newline))
    (else false))
   (cond
    (run-makeindex-p (write-log ':separation-newline)
     (write-log "Running: makeindex ") (write-log *aux-dir/*)
     (write-log *jobname*) (write-log *index-file-suffix*) (write-log #\ )
     (system
      (string-append "makeindex " *aux-dir/* *jobname* *index-file-suffix*))
     (cond
      ((not
        (file-exists?
         (string-append *aux-dir/* *jobname* *index-file-suffix* ".ind")))
       (write-log " ... failed; try manually"))
      (else false))
     (write-log ':separation-newline))
    (else false))
   (let* ((%f (string-append *jobname* *eval4tex-file-suffix*))
          (%ee (list ':if-does-not-exist false))
          (%if-does-not-exist (cadr (memv ':if-does-not-exist %ee))))
     (cond ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
           (else (load %f))))
   (for-each
    (lambda (f)
      (cond
       ((file-exists? f) (write-log ':separation-newline)
        (write-log "Running: metapost ") (write-log f)
        (write-log ':separation-newline) (call-mp f))
       (else false)))
    *mp-files*)
   (for-each
    (lambda (eps-file+img-file-stem)
      (retry-lazy-image (car eps-file+img-file-stem)
       (cdr eps-file+img-file-stem)))
    *missing-eps-files*)))

(define (first-file-that-exists . ff)
 (let ((%dolist-l ff) (f false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (if (null? %dolist-l) (return) false)
       (unless %loop-returned (set! f (car %dolist-l)))
       (unless %loop-returned (set! %dolist-l (cdr %dolist-l)))
       (unless %loop-returned
         (cond ((file-exists? f) (return f)) (else false)))
       (if %loop-returned %loop-result (%loop))))))

(define (file-in-home f)
 (let ((home (getenv "HOME")))
   (and home
        (let ((slash-already-p
               (let ((n (string-length home)))
                 (and (>= n 0)
                      (let ((c (string-ref home (sub1 n))))
                        (or (char=? c #\/) (char=? c #\\)))))))
          (string-append home (if slash-already-p "" "/") f)))))

(define (make-target-dir)
 (let ((hdir-file
        (first-file-that-exists (string-append *jobname* ".hdir")
         ".tex2page.hdir" (file-in-home ".tex2page.hdir"))))
   (cond
    (hdir-file
     (let ((hdir
            (call-with-input-file/buffered hdir-file
             (lambda () (get-filename-possibly-braced)))))
       (cond
        ((not (= (string-length hdir) 0))
         (system (string-append "mkdir -p " hdir))
         (system (string-append "touch " hdir "/probe"))
         (let ((probe (string-append hdir "/probe")))
           (cond
            ((file-exists? probe) (ensure-file-deleted probe)
             (set! *aux-dir* hdir)
             (set! *aux-dir/* (string-append *aux-dir* "/")) *aux-dir/*)
            (else false))))
        (else false))))
    (else false))))

(define (move-aux-files-to-aux-dir f)
 (cond
  ((and *aux-dir*
        (or (file-exists? (string-append f ".tex"))
            (file-exists? (string-append f ".scm"))
            (file-exists? (string-append f (find-img-file-extn)))))
   (system (string-append "mv " f ".* " *aux-dir*)))
  (else false)))

(define (start-css-file)
 (set! *basic-style* "body {
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
 (let ((css-file (string-append *aux-dir/* *jobname* *css-file-suffix*)))
   (set! *css-stream*
    (let* ((%f css-file)
           (%ee (list ':direction ':output ':if-exists ':supersede))
           (%direction (memv ':direction %ee))
           (%if-exists (memv ':if-exists %ee))
           (%if-does-not-exist ':error)
           (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
      (when %direction (set! %direction (cadr %direction)))
      (when %if-exists (set! %if-exists (cadr %if-exists)))
      (when %if-does-not-exist-from-user
        (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
      (cond
       ((eqv? %direction ':output)
        (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
          (delete-file %f))
        (open-output-file %f))
       ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
       (else (open-input-file %f)))))
   (cond
    ((not
      (or (tex2page-flag-boolean "\\TIIPsinglepage")
          (tex2page-flag-boolean "\\TZPsinglepage")))
     (display *basic-style* *css-stream*))
    (else false))))

(define (load-aux-file)
 (let ((label-file (string-append *aux-dir/* *jobname* *label-file-suffix*)))
   (cond
    ((file-exists? label-file) (load-tex2page-data-file label-file)
     (delete-file label-file))
    (else false)))
 (cond
  ((not (string=? *jobname* "texput"))
   (let ((texput-aux (string-append "texput" *aux-file-suffix*)))
     (cond ((file-exists? texput-aux) (delete-file texput-aux)) (else false))))
  (else false))
 (let ((aux-file (string-append *aux-dir/* *jobname* *aux-file-suffix*)))
   (cond
    ((file-exists? aux-file) (load-tex2page-data-file aux-file)
     (delete-file aux-file))
    (else false)))
 (cond
  ((eqv? *tex-format* ':latex) (set! *emit-enabled-p* false) *emit-enabled-p*)
  (else false))
 (start-css-file)
 (cond
  ((not (null? *toc-list*)) (set! *toc-list* (reverse *toc-list*)) *toc-list*)
  (else false))
 (cond
  ((not (null? *stylesheets*)) (set! *stylesheets* (reverse *stylesheets*))
   *stylesheets*)
  (else false))
 (cond
  ((not (null? *html-head*)) (set! *html-head* (reverse *html-head*))
   *html-head*)
  (else false)))

(define (update-last-modification-time f)
 (let ((s (file-or-directory-modify-seconds f)))
   (cond
    ((and s (or (not *last-modification-time*) (> s *last-modification-time*)))
     (set! *source-changed-since-last-run-p* true)
     (set! *last-modification-time* s)
     (cond
      ((and
        (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
            (tex2page-flag-boolean "\\TZPcolophontimestamp"))
        (not (tex2page-flag-boolean "\\TZPcolophonlastpage"))
        (> *html-page-count* 1))
       (flag-missing-piece ':last-modification-time))
      (else false)))
    (else false))))

(define (probably-latex)
 (cond
  ((null? *tex-env*) (set! *latex-probability* (+ *latex-probability* 1))
   (cond
    ((>= *latex-probability* 2)
     (cond
      ((not (eqv? *tex-format* ':latex)) (flag-missing-piece ':tex-format))
      (else false))
     (definitely-latex))
    (else false)))
  (else false)))

(define definitely-latex
 (let ((already-noted-p false))
   (lambda ()
     (cond
      ((not already-noted-p) (set! already-noted-p true) (!definitely-latex)
       (write-aux '(!definitely-latex)))
      (else false)))))

(define (!tex-like-layout) (tex-def-0arg "\\TIIPtexlayout" "1"))

(define (!head-line e) (tex-def-toksdef "\\headline" false e true))

(define (!foot-line e) (tex-def-toksdef "\\footline" false e true))

(define (!toc-page p) (set! *toc-page* p) *toc-page*)

(define (!index-page p) (set! *index-page* p) *index-page*)

(define (!toc-entry level number page label header)
 (set! *toc-list*
  (cons
   (make-tocentry* ':level level ':number number ':page page ':label label
    ':header header)
   *toc-list*))
 *toc-list*)

(define (!label label html-page name value)
 (table-put! label *label-table*
  (make-label* ':src *label-source* ':page html-page ':name name ':value
   value))
 (table-get label *label-table*))

(define (!index index-number html-page-number)
 (table-put! index-number *index-table* html-page-number)
 (table-get index-number *index-table*))

(define (!last-modification-time s . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (epoch 1900))
   (when (< 0 %lambda-rest-arg-len) (set! epoch (list-ref %lambda-rest-arg 0)))
   (set! *last-modification-time*
    (case epoch
      ((1900) s)
      ((1970) (+ s 2208988800))
      (else (error 'ecase "0xdeadc0de"))))
   *last-modification-time*))

(define (!last-page-number n) (set! *last-page-number* n) *last-page-number*)

(define (!single-page) (tex-def-0arg "\\TIIPsinglepage" "1"))

(define (!slides) (tex-def-0arg "\\TIIPslides" "1"))

(define (!script jsf)
 (cond
  ((or (fully-qualified-url-p jsf) (file-exists? (ensure-url-reachable jsf)))
   (set! *scripts* (cons jsf *scripts*)) *scripts*)
  (else (write-log "! Can't find script ") (write-log jsf)
   (write-log ':separation-newline))))

(define (!using-chapters) (set! *using-chapters-p* true) *using-chapters-p*)

(define (!definitely-latex) (set! *tex-format* ':latex)
 (cond ((< (get-gcount "\\secnumdepth") -1) (tex-gdef-count "\\secnumdepth" 3))
       (else false)))

(define (!using-external-program x) false)

(define (!external-labels f) false)

(define (!doctype d) (set! *doctype* d) *doctype*)

(define (!lang lang) (tex-gdef-0arg "\\TZPlang" lang))

(define (!colophon x)
 (case x
   ((:last-page) (tex-def-0arg "\\TZPcolophonlastpage" "1"))
   ((:no-timestamp)
    (tex-def-0arg "\\TZPcolophondisabletimestamp" "1")
    (tex-def-0arg "\\TZPcolophontimestamp" "0"))
   ((:dont-credit-tex2page)
    (tex-def-0arg "\\TZPcolophondisablecredit" "1")
    (tex-def-0arg "\\TZPcolophoncredit" "0"))
   ((:dont-link-to-tex2page-website)
    (tex-def-0arg "\\TZPcolophondisableweblink" "1")
    (tex-def-0arg "\\TZPcolophonweblink" "0"))))

(define (!tex-text n)
 (cond ((= n 0) (set! *ligatures-p* false) *ligatures-p*) (else false)))

(define (!opmac-iis lhs sub)
 (cond
  ((not *opmac-index-sub-table*)
   (set! *opmac-index-sub-table* (make-table ':test equal?))
   *opmac-index-sub-table*)
  (else false))
 (table-put! lhs *opmac-index-sub-table* sub)
 (table-get lhs *opmac-index-sub-table*))

(define (fully-qualified-url-p u)
 (or (substring? "//" u) (char=? (string-ref u 0) #\/)))

(define (fully-qualified-pathname-p f)
 (let ((n (string-length f)))
   (if (= n 0)
       true
       (let ((c0 (string-ref f 0)))
         (cond ((char=? c0 #\/) true)
               ((= n 1) false)
               ((and (char-alphabetic? c0) (char=? (string-ref f 1) #\:)) true)
               (else false))))))

(define (ensure-url-reachable f . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (deletep false))
   (when (< 0 %lambda-rest-arg-len)
     (set! deletep (list-ref %lambda-rest-arg 0)))
   (cond
    ((and *aux-dir* (not (fully-qualified-url-p f)) (not (substring? "/" f)))
     (let ((real-f (string-append *aux-dir/* f)))
       (cond
        ((and (file-exists? f) (not (file-exists? real-f)))
         (system (string-append "cp -p " f " " real-f)))
        (else false)))
     (cond (deletep (ensure-file-deleted f)) (else false)))
    (else false))
   f))

(define (!stylesheet css)
 (cond
  ((or (fully-qualified-url-p css) (file-exists? (ensure-url-reachable css)))
   (set! *stylesheets* (cons css *stylesheets*)) *stylesheets*)
  (else (write-log "! Can't find stylesheet ") (write-log css)
   (write-log ':separation-newline))))

(define (!html-head s) (set! *html-head* (cons s *html-head*)) *html-head*)

(define (!html-redirect url seconds) (set! *redirect-url* url)
 (set! *redirect-delay* seconds)
 (!html-head
  (string-append "<meta http-equiv=\"refresh\" content=\"" seconds ";" url
   "\">")))

(define (!default-title title)
 (cond ((not *title*) (set! *title* title) *title*) (else false)))

(define (!preferred-title title) (set! *title* title) *title*)

(define (!infructuous-calls-to-tex2page n)
 (set! *infructuous-calls-to-tex2page* n) *infructuous-calls-to-tex2page*)

(define (load-tex2page-data-file f)
 (let ((i
        (let* ((%f f)
               (%ee (list ':direction ':input ':if-does-not-exist false))
               (%direction (memv ':direction %ee))
               (%if-exists (memv ':if-exists %ee))
               (%if-does-not-exist ':error)
               (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
          (when %direction (set! %direction (cadr %direction)))
          (when %if-exists (set! %if-exists (cadr %if-exists)))
          (when %if-does-not-exist-from-user
            (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
          (cond
           ((eqv? %direction ':output)
            (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
              (delete-file %f))
            (open-output-file %f))
           ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
           (else (open-input-file %f))))))
   (let ((%with-open-file-res
          (cond
           (i
            (let ((%fluid-var-*current-source-file* f)
                  (%fluid-var-*input-line-no* 0)
                  (e false)
                  (directive false))
              (fluid-let
               ((*current-source-file* %fluid-var-*current-source-file*)
                (*input-line-no* %fluid-var-*input-line-no*))
               (let* ((%loop-returned false)
                      (%loop-result 0)
                      (return
                       (lambda %args
                         (set! %loop-returned true)
                         (set! %loop-result (and (pair? %args) (car %args))))))
                 (let %loop
                   ()
                   (set! e
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res) (set! %read-res false))
                      %read-res))
                   (cond ((not e) (return)) (else false))
                   (unless %loop-returned (set! directive (car e)) directive)
                   (unless %loop-returned
                     (set! *input-line-no* (+ *input-line-no* 1))
                     *input-line-no*)
                   (unless %loop-returned
                     (apply
                      (case directive
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
                        ((!infructuous-calls-to-tex2page)
                         !infructuous-calls-to-tex2page)
                        ((!label) !label)
                        ((!lang) !lang)
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
                        (else
                         (terror 'load-tex2page-data-file
                          "Fatal aux file error " directive "; I'm stymied.")))
                      (cdr e)))
                   (if %loop-returned %loop-result (%loop)))))))
           (else false))))
     (cond (i ((if (input-port? i) close-input-port close-output-port) i))
           (else false))
     %with-open-file-res)))

(define (tex2page-help not-a-file)
 (cond ((not not-a-file) (set! not-a-file "--missing-arg") not-a-file)
       (else false))
 (write-aux
  (quasiquote
   (!infructuous-calls-to-tex2page
    (unquote (add1 *infructuous-calls-to-tex2page*)))))
 (cond
  ((not
    (or (string=? not-a-file "--help")
        (string=? not-a-file "--missing-arg")
        (string=? not-a-file "--version")))
   (write-log "! I can't find file `") (write-log not-a-file) (write-log "'.")
   (write-log ':separation-newline))
  (else false))
 (cond
  ((string=? not-a-file "--version") (write-log *tex2page-copyright-notice*)
   (write-log "

Permission to distribute and use this work for any
purpose is hereby granted provided this copyright
notice is included in the copy.  This work is provided
as is, with no warranty of any kind.

For information on how to use TeX2page, please see")
   (write-log #\newline) (write-log *tex2page-website*) (write-log #\.)
   (write-log #\newline) (write-log #\newline))
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

For more information on how to use tex2page, please see")
   (write-log #\newline) (write-log *tex2page-website*) (write-log #\.)
   (write-log #\newline) (write-log #\newline))
  (else
   (cond
    ((string=? not-a-file "--missing-arg")
     (write-log "! Missing command-line argument.")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((> *infructuous-calls-to-tex2page* 0)
     (write-log "You have called TeX2page") (write-log #\ )
     (write-log (add1 *infructuous-calls-to-tex2page*)) (write-log #\ )
     (write-log "times without a valid input document.")
     (write-log ':separation-newline))
    (else false))
   (cond
    ((>= *infructuous-calls-to-tex2page* 4)
     (write-log "I can't go on meeting you like this.")
     (write-log ':separation-newline) (write-log "Good bye!")
     (write-log ':separation-newline))
    (else
     (write-log "Do you need help using TeX2page?
Try the commands
  tex2page --help
  tex2page --version")
     (write-log ':separation-newline)))))
 (close-all-open-streams))

(define (non-fatal-error . ss)
 (emit-link-start (string-append *jobname* ".hlog"))
 (emit "<span style=\"color: red\">&#x2388;&#xa0;")
 (for-each emit-html-string ss) (emit-link-stop) (emit "</span>"))

(define (do-math-ctl-seq s)
 (cond ((begin (set! *it* (find-math-def s)) *it*) ((tdef*-thunk *it*)))
       (else
        (cond
         ((not *math-needs-image-p*) (set! *math-needs-image-p* true)
          *math-needs-image-p*)
         (else false))
        (emit (substring s 1)))))

(define (tex-def-math-prim cs thunk)
 (tex-def cs null false false thunk cs false *math-primitive-texframe*))

(define (tex-defsym-math-prim cs str)
 (tex-def cs null false false (lambda () (emit str)) cs false
  *math-primitive-texframe*))

(define (make-reusable-math-image-as-needed cs . expn)
 (let ((expn (if (null? expn) cs (car expn))))
   (tex-def-math-prim cs
    (lambda ()
      (tex2page-string (string-append "\\global\\imgdef" cs "{$" expn "$}"))
      (tex2page-string cs)))))

(tex-def-math-prim "\\eqno" do-eqno)

(tex-def-math-prim "\\left" do-math-left)

(tex-def-math-prim "\\noalign" do-noalign)

(tex-def-math-prim "\\over" do-over)

(tex-def-math-prim "\\right" do-math-right)

(tex-def-prim "\\ " do-actual-space)

(tex-def-prim "\\advance" do-advance)

(tex-def-prim "\\afterassignment" do-afterassignment)

(tex-def-prim "\\aftergroup" do-aftergroup)

(tex-def-prim "\\box" do-box)

(tex-def-prim "\\catcode" do-catcode)

(tex-def-prim "\\char" do-char)

(tex-def-prim "\\chardef" do-chardef)

(tex-def-prim "\\closein" (lambda () (do-close-stream ':in)))

(tex-def-prim "\\closeout" (lambda () (do-close-stream ':out)))

(tex-def-prim "\\copy" (lambda () (do-box true)))

(tex-def-prim "\\count" do-count)

(tex-def-prim "\\countdef" do-countdef)

(tex-def-prim "\\cr" (lambda () (do-cr "\\cr")))

(tex-def-prim "\\csname" do-csname)

(tex-def-prim "\\def" do-def)

(tex-def-prim "\\dimen" do-dimen)

(tex-def-prim "\\dimendef" do-dimendef)

(tex-def-prim "\\discretionary" do-discretionary)

(tex-def-prim "\\divide" do-divide)

(tex-def-prim "\\edef" (lambda () (do-def false true)))

(tex-def-prim "\\else" do-else)

(tex-def-prim "\\end" do-end)

(tex-def-prim "\\endinput" do-endinput)

(tex-def-prim "\\errmessage" do-errmessage)

(tex-def-prim "\\expandafter" do-expandafter)

(tex-def-prim "\\fi" do-fi)

(tex-def-prim "\\font" do-font)

(tex-def-prim "\\fontdimen" do-fontdimen)

(tex-def-prim "\\futurelet" do-futurelet)

(tex-def-prim "\\gdef" (lambda () (do-def true false)))

(tex-def-prim "\\global" do-global)

(tex-def-prim "\\halign" do-halign)

(tex-def-prim "\\hbox" do-hbox)

(tex-def-prim "\\hfill" (lambda () (emit-nbsp 5)))

(tex-def-prim "\\hrule" do-hrule)

(tex-def-prim "\\hskip" do-hskip)

(tex-def-prim "\\hyphenchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\if" do-if)

(tex-def-prim "\\ifcase" do-ifcase)

(tex-def-prim "\\ifdim" do-iffalse)

(tex-def-prim "\\ifeof" do-ifeof)

(tex-def-prim "\\iffalse" do-iffalse)

(tex-def-prim "\\ifhmode" do-iftrue)

(tex-def-prim "\\ifmmode" do-ifmmode)

(tex-def-prim "\\ifnum" do-ifnum)

(tex-def-prim "\\ifodd" do-ifodd)

(tex-def-prim "\\iftrue" do-iftrue)

(tex-def-prim "\\ifx" do-ifx)

(tex-def-prim "\\ignorespaces" ignorespaces)

(tex-def-prim "\\indent" do-indent)

(tex-def-prim "\\input" do-input)

(tex-def-prim "\\jobname" (lambda () (tex2page-string *jobname*)))

(tex-def-prim "\\kern" do-kern)

(tex-def-prim "\\lccode" (lambda () (do-tex-case-code ':lccode)))

(tex-def-prim "\\let" do-let)

(tex-def-prim "\\lower" do-lower)

(tex-def-prim "\\lowercase" (lambda () (do-flipcase ':lccode)))

(tex-def-prim "\\message" do-message)

(tex-def-prim "\\multiply" do-multiply)

(tex-def-prim "\\noindent" do-noindent)

(tex-def-prim "\\number" do-number)

(tex-def-prim "\\openin" (lambda () (do-open-stream ':in)))

(tex-def-prim "\\openout" (lambda () (do-open-stream ':out)))

(tex-def-prim "\\par" do-para)

(tex-def-prim "\\raise" (lambda () (do-lower true)))

(tex-def-prim "\\read" do-read)

(tex-def-prim "\\relax" do-relax)

(tex-def-prim "\\romannumeral" do-romannumeral)

(tex-def-prim "\\setbox" do-setbox)

(tex-def-prim "\\string" do-string)

(tex-def-prim "\\the" do-the)

(tex-def-prim "\\toks" do-toks)

(tex-def-prim "\\toksdef" do-toksdef)

(tex-def-prim "\\uccode" (lambda () (do-tex-case-code ':uccode)))

(tex-def-prim "\\underline" (lambda () (do-function "\\underline")))

(tex-def-prim "\\unkern" do-unskip)

(tex-def-prim "\\unskip" do-unskip)

(tex-def-prim "\\uppercase" (lambda () (do-flipcase ':uccode)))

(tex-def-prim "\\vbox" do-hbox)

(tex-def-prim "\\vskip" do-vskip)

(tex-def-prim "\\vtop" do-hbox)

(tex-def-prim "\\write" do-write)

(tex-def-prim "\\xdef" (lambda () (do-def true true)))

(tex-def-prim-0arg "\\begingroup" "{")

(tex-def-prim-0arg "\\endgroup" "}")

(tex-let-prim "\\-" "\\TIIPrelax")

(tex-let-prim "\\/" "\\TIIPrelax")

(tex-let-prim "\\displaystyle" "\\TIIPrelax")

(tex-let-prim "\\leqno" "\\eqno")

(tex-let-prim "\\textstyle" "\\TIIPrelax")

(tex-let-prim "\\@ne" (string (integer->char 1)))

(tex-let-prim "\\tw@" (string (integer->char 2)))

(tex-let-prim "\\thr@@" (string (integer->char 3)))

(tex-let-prim "\\sixt@@n" (string (integer->char 16)))

(tex-let-prim "\\@cclv" (string (integer->char 255)))

(tex-let-prim "\\@cclvi" (string (integer->char 256)))

(tex-let-prim "\\@m" (string (integer->char 1000)))

(tex-let-prim "\\@M" (string (integer->char 10000)))

(tex-let-prim "\\@MM" (string (integer->char 20000)))

(tex-def-prim "\\wlog" do-wlog)

(tex-def-prim "\\newcount" do-newcount)

(tex-def-prim "\\newdimen" do-newdimen)

(tex-def-prim "\\newbox" do-newbox)

(tex-def-prim "\\newtoks" do-newtoks)

(tex-def-prim "\\newread" do-newread)

(tex-def-prim "\\newwrite" do-newwrite)

(tex-def-prim "\\newif" do-newif)

(tex-def-prim-0arg "\\magstephalf" "1095")

(tex-def-prim "\\magstep" do-magstep)

(tex-def-prim "\\sevenrm" (lambda () (do-switch ':sevenrm)))

(tex-def-prim "\\fiverm" (lambda () (do-switch ':fiverm)))

(tex-def-prim "\\rm"
 (lambda () (cond (*math-mode-p* (do-switch ':rm)) (else false))))

(tex-def-prim "\\oldstyle" (lambda () (do-switch ':oldstyle)))

(tex-def-prim "\\cal" (lambda () (do-switch ':cal)))

(tex-def-prim "\\it" (lambda () (do-switch ':it)))

(tex-def-prim "\\sl" (lambda () (do-switch ':sl)))

(tex-def-prim "\\bf" (lambda () (do-switch ':bf)))

(tex-def-prim "\\tt" (lambda () (do-switch ':tt)))

(tex-defsym-math-prim "\\lbrack" "[")

(tex-defsym-math-prim "\\rbrack" "]")

(tex-let-prim "\\endgraf" "\\par")

(tex-let-prim "\\space" "\\ ")

(tex-def-prim-0arg "\\empty" "")

(tex-def-prim-0arg "\\bgroup" "{")

(tex-def-prim-0arg "\\egroup" "}")

(tex-def-prim "\\obeyspaces" do-obeyspaces)

(tex-def-prim "\\obeylines" do-obeylines)

(tex-def-pat-prim "\\loop" "#1\\repeat" "\\def\\body{#1}\\iterate")

(tex-def-pat-prim "\\iterate" ""
 "\\body\\let\\next\\iterate\\else\\let\\next\\relax\\fi\\next")

(tex-let-prim "\\repeat" "\\fi")

(tex-def-prim "\\enskip" (lambda () (emit-space (kern ".5em"))))

(tex-def-prim "\\quad" (lambda () (emit-space (kern "1em"))))

(tex-def-prim "\\qquad" (lambda () (emit-space (kern "2em"))))

(tex-let-prim "\\enspace" "\\enskip")

(tex-def-prim "\\thinspace" (lambda () (emit-space (kern ".16667em"))))

(tex-def-prim "\\negthinspace" (lambda () (emit-space (kern "-.16667em"))))

(tex-def-prim "\\smallskip" (lambda () (do-bigskip ':smallskip)))

(tex-def-prim "\\medskip" (lambda () (do-bigskip ':medskip)))

(tex-def-prim "\\bigskip" (lambda () (do-bigskip ':bigskip)))

(tex-defsym-prim "\\break" "<br>")

(tex-def-prim "\\eject" do-eject)

(tex-let-prim "\\supereject" "\\eject")

(tex-def-prim "\\smallbreak" (lambda () (do-bigskip ':smallskip)))

(tex-def-prim "\\medbreak" (lambda () (do-bigskip ':medskip)))

(tex-def-prim "\\bigbreak" (lambda () (do-bigskip ':bigskip)))

(tex-def-prim "\\leftline" (lambda () (do-function "\\leftline")))

(tex-def-prim "\\rightline" (lambda () (do-function "\\rightline")))

(tex-def-prim "\\centerline" (lambda () (do-function "\\centerline")))

(tex-def-prim "\\llap" do-llap)

(tex-def-prim "\\settabs" do-settabs)

(tex-def-prim "\\tabalign" do-tabalign)

(tex-let-prim "\\+" "\\tabalign")

(tex-def-prim "\\item" do-item)

(tex-def-prim "\\itemitem" (lambda () (do-plain-item 2)))

(tex-def-prim "\\textindent" do-textindent)

(tex-def-prim "\\narrower" (lambda () (do-switch ':narrower)))

(tex-def-prim "\\beginsection" do-beginsection)

(tex-def-prim "\\proclaim" do-proclaim)

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

(tex-def-prim "\\d" (lambda () (do-diacritic ':dotunder)))

(tex-def-prim "\\b" (lambda () (do-diacritic ':barunder)))

(tex-def-prim "\\c" (lambda () (do-diacritic ':cedilla)))

(tex-defsym-prim "\\copyright" "&#xa9;")

(tex-defsym-prim "\\dots" "&#x2026;")

(tex-def-prim "\\TeX" (lambda () (emit *tex-logo*)))

(tex-def-prim "\\`" (lambda () (do-diacritic ':grave)))

(tex-def-prim "\\'" (lambda () (do-diacritic ':acute)))

(tex-def-prim "\\v" (lambda () (do-diacritic ':hacek)))

(tex-def-prim "\\u" (lambda () (do-diacritic ':breve)))

(tex-def-prim "\\=" do-backslash-equal)

(tex-def-prim "\\^" (lambda () (do-diacritic ':circumflex)))

(tex-def-prim "\\." (lambda () (do-diacritic ':dot)))

(tex-def-prim "\\H" (lambda () (do-diacritic ':hungarianumlaut)))

(tex-def-prim "\\~" (lambda () (do-diacritic ':tilde)))

(tex-def-prim "\\\"" (lambda () (do-diacritic ':umlaut)))

(tex-def-prim "\\t" (lambda () (do-diacritic ':tieafter)))

(tex-def-math-prim "\\," (lambda () (emit-space (kern ".16667em"))))

(tex-def-math-prim "\\!" (lambda () (emit-space (kern "-.16667em"))))

(tex-def-math-prim "\\>" (lambda () (emit-space (kern ".22222em"))))

(tex-def-math-prim "\\;" (lambda () (emit-space (kern ".27778em"))))

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

(tex-defsym-math-prim "\\aleph" "&#x2135;")

(tex-defsym-math-prim "\\hbar" "&#x210f;")

(tex-defsym-math-prim "\\imath" "&#x1d6a4;")

(tex-defsym-math-prim "\\jmath" "&#x1d6a5")

(tex-defsym-math-prim "\\ell" "&#x2113;")

(tex-defsym-math-prim "\\wp" "&#x2118;")

(tex-defsym-math-prim "\\Re" "&#x211c;")

(tex-defsym-math-prim "\\Im" "&#x2111;")

(tex-defsym-math-prim "\\partial" "&#x2202;")

(tex-defsym-math-prim "\\infty" "&#x221e;")

(tex-defsym-math-prim "\\prime" "&#x2044;")

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

(tex-defsym-math-prim "\\sum" "&#x2211;")

(tex-defsym-math-prim "\\prod" "&#x220f;")

(tex-defsym-math-prim "\\coprod" "&#x2210;")

(tex-def-math-prim "\\int" do-integral)

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

(define (do-not) (ignorespaces)
 (let ((c (snoop-actual-char)))
   (if (= (catcode c) **escape**)
       (let ((x (get-ctl-seq)))
         (emit
          (cond ((string=? x "\\leq") "&#x2270;")
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
         ((#\< #\> #\=)
          (get-actual-char)
          (emit
           (cond ((char=? c #\<) "&#x226e;")
                 ((char=? c #\>) "&#x226f;")
                 ((char=? c #\=) "&#x2260;"))))
         (else (emit "/"))))))

(tex-def-math-prim "\\not" do-not)

(tex-defsym-math-prim "\\neq" "&#x2260;")

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

(tex-def-math-prim "\\colon" (lambda () (emit #\:)))

(tex-def-math-prim "\\ldotp" (lambda () (emit #\.)))

(tex-let-prim "\\cdotp" "\\cdot")

(tex-let-prim "\\ldots" "\\dots")

(tex-defsym-prim "\\cdots" "&#x22ef;")

(tex-defsym-prim "\\vdots" "&#x22ee;")

(tex-defsym-prim "\\ddots" "&#x22f1;")

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

(tex-def-prim "\\vphantom" get-group)

(tex-def-prim "\\hphantom" get-group)

(tex-def-prim "\\phantom" get-group)

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

(define (texbook-18-2 x)
 (let ((lhs false) (rhs false))
   (cond
    ((not (pair? x)) (set! lhs (string-append "\\" x))
     (set! rhs (string-append x " ")) rhs)
    (else (set! lhs (string-append "\\" (car x)))
     (set! rhs (string-append (cadr x) " ")) rhs))
   (tex-defsym-math-prim lhs rhs)))

(for-each texbook-18-2
 '("arccos" "arcsin" "arctan" "arg" "cos" "cosh" "cot" "coth" "csc" "deg" "det"
   "dim" "exp" "gcd" "hom" "inf" "ker" "lg" "lim" ("liminf" "lim inf")
   ("limsup" "lim sup") "ln" "log" "max" "min" "Pr" ("TIIPsec" "sec") "sin"
   "sinh" "sup" "tan" "tanh"))

(tex-def-math-prim "\\matrix" do-relax)

(tex-def-math-prim "\\pmatrix" do-pmatrix)

(tex-def-math-prim "\\bordermatrix" do-pmatrix)

(tex-def-math-prim "\\eqalign" (lambda () (do-eqalign ':eqalign)))

(tex-def-math-prim "\\displaylines" (lambda () (do-eqalign ':displaylines)))

(tex-def-math-prim "\\eqalignno" (lambda () (do-eqalign ':eqalignno)))

(tex-let-prim "\\leqalignno" "\\eqalignno")

(tex-def-prim "\\folio" (lambda () (emit *html-page-count*)))

(tex-def-prim "\\footnote" do-footnote)

(tex-def-prim "\\vfootnote" do-vfootnote)

(tex-let-prim "\\dosupereject" "\\eject")

(tex-def-prim "\\magnification" do-magnification)

(tex-def-prim "\\tracingall" do-tracingall)

(tex-defsym-prim "\\fmtname" "TeX2page")

(tex-defsym-prim "\\fmtversion" *tex2page-version*)

(tex-def-prim "\\beginchapter" do-beginchapter)

(tex-def-prim "\\MF"
 (let ((mf
        (string-append "<span style=\"" "font-family: sans-serif"
         "\">METAFONT</span>")))
   (lambda () (emit mf))))

(tex-def-prim "\\AmSTeX"
 (let ((ams
        (string-append "<span style=\"font-family: cursive;\">" "A"
         "<span style=\"" "position: relative; " "top: 0.5ex; "
         "margin-left: -.1667em; " "margin-right: -.075em" "\">M</span>"
         "S</span>")))
   (lambda () (emit ams) (emit #\-) (emit *tex-logo*))))

(tex-defsym-prim "\\bull" "&#x25fe;")

(define (do-begintt) (do-end-para)
 (fluid-let ((*catcodes* *catcodes*)) (bgroup)
  (do-tex-ctl-seq-completely "\\tthook") (catcode #\\ 12) (catcode #\  12)
  (emit "<pre class=verbatim>")
  (let ((%fluid-var-*ligatures-p* false) (c false))
    (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (set! c (snoop-actual-char))
         (cond ((not c) (terror 'do-begintt "Eof inside \\begintt"))
               (else false))
         (cond
          ((char=? c #\\)
           (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
            (let ((cs (get-ctl-seq)))
              (if (string=? cs "\\endtt") (return) (emit-html-string cs)))))
          ((= (catcode c) **escape**)
           (let ((cs (get-ctl-seq)))
             (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
              (do-tex-ctl-seq-completely cs))))
          (else (emit-html-char (get-actual-char))))
         (if %loop-returned %loop-result (%loop))))))
  (emit "</pre>") (egroup))
 (do-noindent))

(tex-def-prim "\\begintt" do-begintt)

(define (do-frac) (if (eq? *tex-format* ':latex) (do-latex-frac) (do-tex-frac)))

(tex-def-prim "\\frac" do-frac)

(define (do-ifdefined)
 (let ((x (get-raw-token/is)))
   (if
    (or (not (ctl-seq-p x))
        (and (ctl-seq-p x) (or (find-def x) (find-math-def x))))
    (do-iftrue)
    (do-iffalse))))

(tex-def-prim "\\ifdefined" do-ifdefined)

(define (do-xetexpdffile)
 (let ((pdf-file (get-filename)))
   (let ((height false))
     (let ((rotated false))
       (let ((width false))
         (let ((img-file-stem (next-html-image-file-stem)))
           (let ((img-file (string-append img-file-stem (find-img-file-extn))))
             (let ((fq-img-file (string-append *aux-dir/* img-file)))
               (let* ((%loop-returned false)
                      (%loop-result 0)
                      (return
                       (lambda %args
                         (set! %loop-returned true)
                         (set! %loop-result (and (pair? %args) (car %args))))))
                 (let %loop
                   ()
                   (cond
                    ((eat-word "height") (set! height (get-pixels)) height)
                    ((eat-word "rotated") (set! rotated (get-number)) rotated)
                    ((eat-word "width") (set! width (get-pixels)) width)
                    (else (return)))
                   (if %loop-returned %loop-result (%loop))))
               (cond
                ((not (file-exists? fq-img-file))
                 (write-log ':separation-space) (write-log #\{)
                 (write-log pdf-file) (write-log ':separation-space)
                 (write-log "->") (write-log ':separation-space)
                 (write-log img-file) (write-log #\})
                 (write-log ':separation-space) (ps-to-img pdf-file img-file))
                (else false))
               (write-log #\()
               (write-log img-file)
               (write-log ':separation-space)
               (emit "<img src=\"")
               (do-img-src (ensure-url-reachable img-file ':delete))
               (emit "\"")
               (cond (height (emit " height=") (emit height)) (else false))
               (cond
                (rotated (set! rotated (- rotated))
                 (emit " style=\"transform: rotate(") (emit rotated)
                 (emit "deg)\""))
                (else false))
               (cond (width (emit " width=") (emit width)) (else false))
               (emit " alt=\"")
               (emit img-file)
               (emit "\"")
               (emit ">")
               (write-log #\))
               (write-log ':separation-space)))))))))

(tex-def-prim "\\XeTeXpdffile" do-xetexpdffile)

(define (do-xetexpicfile)
 (let ((img-file (get-filename)) (height false) (rotated false) (width false))
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (cond ((eat-word "height") (set! height (get-pixels)) height)
             ((eat-word "rotated") (set! rotated (get-number)) rotated)
             ((eat-word "width") (set! width (get-pixels)) width)
             (else (return)))
       (if %loop-returned %loop-result (%loop))))
   (emit "<img src=\"")
   (do-img-src (fully-qualify-url img-file))
   (emit "\"")
   (cond (height (emit " height=") (emit height)) (else false))
   (cond
    (rotated (set! rotated (- rotated)) (emit " style=\"transform: rotate(")
     (emit rotated) (emit "deg)\""))
    (else false))
   (cond (width (emit " width=") (emit width)) (else false))
   (emit " alt=\"")
   (emit img-file)
   (emit "\"")
   (emit ">")))

(tex-def-prim "\\XeTeXpicfile" do-xetexpicfile)

(tex-def-prim "\\cssblock" do-cssblock)

(tex-def-prim "\\rawhtml" do-rawhtml)

(tex-def-prim "\\texonly" (lambda () (ignore-tex-specific-text "texonly")))

(tex-def-prim "\\htmlonly"
 (lambda () (set! *html-only* (+ *html-only* 1)) *html-only*))

(tex-def-prim "\\endhtmlonly"
 (lambda () (set! *html-only* (- *html-only* 1)) *html-only*))

(tex-def-prim "\\verbwritefile" do-verbwritefile)

(tex-def-prim "\\verbwrite" do-verbwrite)

(tex-def-prim "\\defcsactive" do-defcsactive)

(tex-def-prim "\\gobblegroup" get-group)

(tex-def-prim "\\verb" do-verb)

(tex-def-prim "\\verbatimescapechar" do-verbatimescapechar)

(tex-def-prim "\\verbatiminput" do-verbatiminput)

(tex-def-prim "\\activettchar" do-opmac-activettchar)

(tex-def-prim "\\DefineShortVerb" do-opmac-activettchar)

(tex-def-prim "\\url" do-url)

(tex-def-prim "\\urlh" do-urlh)

(tex-def-prim "\\urlp" do-urlp)

(tex-def-prim "\\urlhd" do-urlhd)

(tex-def-prim "\\href" do-urlh)

(tex-let-prim "\\path" "\\verb")

(tex-def-prim "\\scm" do-scm)

(tex-def-prim "\\scminput" do-scminput)

(tex-let-prim "\\lispinput" "\\scminput")

(tex-def-prim "\\scmdribble" do-scmdribble)

(tex-def-prim "\\imgdef" make-reusable-img)

(tex-def-prim "\\makehtmlimage" do-makehtmlimage)

(tex-def-prim "\\mathg" do-mathg)

(tex-def-prim "\\mathdg" do-mathdg)

(tex-def-prim "\\xrtag" do-tag)

(define (do-iffileexists)
 (let ((file (actual-tex-filename (get-filename-possibly-braced))))
   (let ((then-e (ungroup (get-group))))
     (let ((else-e (ungroup (get-group))))
       (tex2page-string (if file then-e else-e))))))

(tex-def-prim "\\IfFileExists" do-iffileexists)

(define (do-inputiffileexists)
 (let ((f (actual-tex-filename (get-filename-possibly-braced))))
   (let ((then-txt (ungroup (get-group))))
     (let ((else-txt (ungroup (get-group))))
       (cond (f (tex2page-string then-txt) (tex2page-file f))
             (else (tex2page-string else-txt)))))))

(tex-def-prim "\\InputIfFileExists" do-inputiffileexists)

(define (do-futurenonspacelet)
 (let ((first (get-raw-token/is)))
   (let ((second (get-raw-token/is)))
     (let ((third (get-raw-token/is)))
       (do-futurelet-aux first second third)))))

(tex-def-prim "\\futurenonspacelet" do-futurenonspacelet)

(define (do-scm-set-keywords)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return)) (else false))
          (let ((s (scm-get-token)))
            (table-put! s *scm-builtins* false)
            (table-put! s *scm-variables* false)
            (table-put! s *scm-keywords* true)
            (table-get s *scm-keywords*)))
        (if %loop-returned %loop-result (%loop)))))))

(tex-def-prim "\\scmkeyword" do-scm-set-keywords)

(define (do-scm-set-builtins)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return)) (else false))
          (let ((s (scm-get-token)))
            (table-put! s *scm-keywords* false)
            (table-put! s *scm-variables* false)
            (table-put! s *scm-builtins* true)
            (table-get s *scm-builtins*)))
        (if %loop-returned %loop-result (%loop)))))))

(tex-def-prim "\\scmbuiltin" do-scm-set-builtins)

(tex-let-prim "\\scmconstant" "\\scmbuiltin")

(define (do-scm-set-variables)
 (call-with-input-string/buffered (ungroup (get-group))
  (lambda ()
    (let* ((%loop-returned false)
           (%loop-result 0)
           (return
            (lambda %args
              (set! %loop-returned true)
              (set! %loop-result (and (pair? %args) (car %args))))))
      (let %loop
        ()
        (ignorespaces ':all)
        (let ((c (snoop-actual-char)))
          (cond ((not c) (return)) (else false))
          (let ((s (scm-get-token)))
            (table-put! s *scm-builtins* false)
            (table-put! s *scm-keywords* false)
            (table-put! s *scm-variables* true)
            (table-get s *scm-variables*)))
        (if %loop-returned %loop-result (%loop)))))))

(tex-def-prim "\\scmvariable" do-scm-set-variables)

(tex-let-prim "\\setbuiltin" "\\scmbuiltin")

(tex-let-prim "\\setconstant" "\\scmconstant")

(tex-let-prim "\\setkeyword" "\\scmkeyword")

(tex-let-prim "\\setvariable" "\\scmvariable")

(define (do-scm-slatex-lines env display-p result-p)
 (let ((%fluid-var-*catcodes* *catcodes*)
       (endenv (string-append "\\end" env))
       (in-table-p
        (and (not (null? *tabular-stack*))
             (member (car *tabular-stack*) '(:block :figure :table)))))
   (fluid-let ((*catcodes* %fluid-var-*catcodes*))
    (cond (display-p (do-end-para)) (in-table-p (emit "</td><td>"))) (bgroup)
    (cond
     ((string=? env "tt") (do-tex-ctl-seq-completely "\\tthook")
      (catcode #\\ 12))
     (else false))
    (emit "<div class=leftline><pre class=scheme")
    (cond (result-p (emit "response")) (else false)) (emit ">")
    (let ((%fluid-var-*ligatures-p* false)
          (%fluid-var-*verb-display-p* true)
          (%fluid-var-*not-processing-p* true)
          (c false))
      (fluid-let
       ((*ligatures-p* %fluid-var-*ligatures-p*)
        (*verb-display-p* %fluid-var-*verb-display-p*)
        (*not-processing-p* %fluid-var-*not-processing-p*))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (set! c (snoop-actual-char))
           (cond ((not c) (terror 'do-scm-slatex-lines "Eof inside " env))
                 (else false))
           (cond
            ((char=? c #\newline) (get-actual-char) (scm-emit-html-char c)
             (cond ((not (tex2page-flag-boolean "\\TZPslatexcomments")) false)
                   ((char=? (snoop-actual-char) #\;) (get-actual-char)
                    (if (char=? (snoop-actual-char) #\;)
                        (toss-back-char #\;)
                        (scm-output-slatex-comment)))))
            ((char=? c #\\)
             (let ((x
                    (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
                     (get-ctl-seq))))
               (cond ((string=? x endenv) (return))
                     ((string=? x "\\end")
                      (let ((g (get-grouped-environment-name-if-any)))
                        (cond ((and g (string=? g env)) (egroup) (return))
                              (else (scm-output-token x)
                               (cond
                                (g (scm-output-token "{") (scm-output-token g)
                                 (scm-output-token "}"))
                                (else false))))))
                     (else (scm-output-token x)))))
            ((= (catcode c) **escape**)
             (let ((cs (get-ctl-seq)))
               (fluid-let ((*catcodes* *catcodes*)) (catcode #\\ 0)
                (do-tex-ctl-seq-completely cs))))
            (else (scm-output-next-chunk)))
           (if %loop-returned %loop-result (%loop))))))
    (emit "</pre></div>") (egroup)
    (cond (display-p (do-noindent)) (in-table-p (emit "</td><td>"))))))

(tex-def-prim "\\schemedisplay"
 (lambda () (do-scm-slatex-lines "schemedisplay" true false)))

(tex-def-prim "\\schemeresponse"
 (lambda () (do-scm-slatex-lines "schemeresponse" true ':result)))

(tex-def-prim "\\begintts" (lambda () (do-scm-slatex-lines "tt" true false)))

(tex-def-prim "\\title" do-title)

(tex-def-prim "\\subject" do-subject)

(tex-def-prim "\\em" (lambda () (do-switch ':em)))

(tex-def-prim "\\raggedleft" (lambda () (do-switch ':raggedleft)))

(define (do-index-help idx-entry) (set! *index-count* (+ *index-count* 2))
 (!index *index-count* *html-page-count*)
 (write-aux
  (quasiquote (!index (unquote *index-count*) (unquote *html-page-count*))))
 (let ((tag
        (string-append *html-node-prefix* "index_"
         (write-to-string *index-count*))))
   (emit-anchor tag)
   (cond
    ((not *index-stream*)
     (let ((idx-file
            (string-append *aux-dir/* *jobname* *index-file-suffix* ".idx")))
       (set! *index-stream*
        (let* ((%f idx-file)
               (%ee (list ':direction ':output ':if-exists ':supersede))
               (%direction (memv ':direction %ee))
               (%if-exists (memv ':if-exists %ee))
               (%if-does-not-exist ':error)
               (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
          (when %direction (set! %direction (cadr %direction)))
          (when %if-exists (set! %if-exists (cadr %if-exists)))
          (when %if-does-not-exist-from-user
            (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
          (cond
           ((eqv? %direction ':output)
            (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
              (delete-file %f))
            (open-output-file %f))
           ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
           (else (open-input-file %f)))))
       *index-stream*))
    (else false))
   (display "\\indexentry{" *index-stream*)
   (cond
    ((or (substring? "|see{" idx-entry) (substring? "|seealso{" idx-entry))
     (display-index-entry idx-entry *index-stream*))
    ((begin (set! *it* (substring? "|(" idx-entry)) *it*)
     (let ((i *it*))
       (display-index-entry (substring idx-entry 0 i) *index-stream*)
       (display "|expandhtmlindex" *index-stream*)))
    (else (display-index-entry idx-entry *index-stream*)
     (display "|expandhtmlindex" *index-stream*)))
   (display "}{" *index-stream*)
   (display *index-count* *index-stream*)
   (display "}" *index-stream*)
   (newline *index-stream*)))

(define (do-index)
 (let ((idx-entry (ungroup (get-group))))
   (ignorespaces)
   (cond ((not (substring? "|)" idx-entry)) (do-index-help idx-entry))
         (else false))))

(tex-def-prim "\\index" do-index)

(define (do-theindex) (bgroup) (tex2page-string "\\let\\endtheindex\\egroup")
 (tex2page-string "\\let\\indexspace\\relax")
 (tex2page-string "\\let\\item\\indexitem")
 (tex2page-string "\\let\\subitem\\indexsubitem")
 (tex2page-string "\\let\\subsubitem\\indexsubsubitem")
 (tex2page-string "\\let\\(\\expandhtmlindex"))

(tex-def-prim "\\theindex" do-theindex)

(define (do-indexitem indent) (set! *index-page-mention-alist* (make-table))
 (emit "<br>") (emit-newline) (emit-nbsp (* indent 4)))

(tex-def-prim "\\indexitem" (lambda () (do-indexitem 0)))

(tex-def-prim "\\indexsubitem" (lambda () (do-indexitem 1)))

(tex-def-prim "\\indexsubsubitem" (lambda () (do-indexitem 2)))

(define (do-inputindex . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
       (insert-heading-p false))
   (when (< 0 %lambda-rest-arg-len)
     (set! insert-heading-p (list-ref %lambda-rest-arg 0)))
   (set! *using-index-p* true)
   (cond
    (insert-heading-p
     (tex2page-string
      (if *using-chapters-p*
          "\\chapter*{\\indexname}"
          "\\section*{\\indexname}"))
     (emit-newline))
    (else false))
   (emit-anchor (string-append *html-node-prefix* "index_start"))
   (!index-page *html-page-count*)
   (write-aux (quasiquote (!index-page (unquote *html-page-count*))))
   (let ((ind-file
          (string-append *aux-dir/* *jobname* *index-file-suffix* ".ind")))
     (cond ((file-exists? ind-file) (tex2page-file ind-file))
           (else (flag-missing-piece ':index)
            (non-fatal-error "Index not generated; rerun TeX2page"))))))

(tex-def-prim "\\inputindex" (lambda () (do-inputindex false)))

(define (do-xrdef)
 (let ((tag (get-peeled-group)))
   (do-tag-aux tag (write-to-string *html-page-count*))))

(tex-def-prim "\\xrdef" do-xrdef)

(define (do-label) (do-label-aux (get-label)))

(tex-def-prim "\\label" do-label)

(define (do-ref) (do-ref-aux (get-label) false false))

(tex-def-prim "\\ref" do-ref)

(define (do-pageref)
 (let ((label-ref (label-bound-p (get-peeled-group))))
   (if label-ref
       (let ((pageno (label*-page label-ref)))
         (emit-ext-page-node-link-start (label*-src label-ref) pageno false)
         (emit pageno)
         (emit-link-stop))
       (non-fatal-error "***"))))

(tex-def-prim "\\pageref" do-pageref)

(define (do-writenumberedtocline)
 (let ((seclvl (section-type-to-depth (get-peeled-group))))
   (let ((secnum (tex-string-to-html-string (get-group))))
     (let ((sectitle (tex-string-to-html-string (get-group))))
       (do-write-to-toc-aux seclvl secnum sectitle)))))

(tex-def-prim "\\writenumberedtocline" do-writenumberedtocline)

(define (do-obeywhitespace) (do-obeylines) (do-obeyspaces))

(tex-def-prim "\\obeywhitespace" do-obeywhitespace)

(tex-def-prim "\\numberedfootnote" do-numbered-footnote)

(tex-def-prim "\\sidemargin" eat-dimen)

(tex-def-prim "\\spinemargin" eat-dimen)

(define (do-epsfbox) (get-bracketed-text-if-any)
 (let ((f (get-filename-possibly-braced)))
   (cond
    ((not *eval-for-tex-only-p*)
     (let ((epsf-x-size (get-dimen "\\epsfxsize"))
           (epsf-y-size (get-dimen "\\epsfysize")))
       (cond
        ((and (= epsf-x-size 0) (= epsf-y-size 0))
         (let ((img-file-stem (next-html-image-file-stem)))
           (lazily-make-epsf-image-file f img-file-stem)
           (source-img-file img-file-stem)))
        (else
         (cond ((not (= epsf-x-size 0)) (tex2page-string "\\epsfxsize=0pt"))
               (else false))
         (cond ((not (= epsf-y-size 0)) (tex2page-string "\\epsfysize=0pt"))
               (else false))
         (fluid-let
          ((*imgpreamble-inferred* (cons ':epsfbox *imgpreamble-inferred*)))
          (call-with-html-image-stream
           (lambda (o)
             (cond
              ((not (= epsf-x-size 0)) (display "\\epsfxsize=" o)
               (display epsf-x-size o) (display "sp" o) (newline o))
              (else false))
             (cond
              ((not (= epsf-y-size 0)) (display "\\epsfysize=" o)
               (display epsf-y-size o) (display "sp" o) (newline o))
              (else false))
             (display "\\epsfbox{" o)
             (display f o)
             (display #\} o))))))))
    (else false))))

(tex-def-prim "\\epsfbox" do-epsfbox)

(define (do-eval-string s)
 (call-with-input-string s
  (lambda (i)
    (let ((x false))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (set! x
           (let ((%read-res (read i)))
             (when (eof-object? %read-res) (set! %read-res false))
             %read-res))
          (cond ((not x) (return)) (else false))
          (unless %loop-returned (eval1 x))
          (if %loop-returned %loop-result (%loop))))))))

(define (do-eval kind)
 (let ((s
        (if *outer-p*
            (ungroup
             (fluid-let ((*catcodes* *catcodes*) (*expand-escape-p* true))
              (catcode #\\ 12) (catcode *esc-char-verb* 0) (get-group)))
            (tex-write-output-string (ungroup (get-group))))))
   (cond
    ((not (inside-false-world-p))
     (cond ((> *html-only* 0) (set! kind ':html) kind) (else false))
     (case kind
       ((:quiet) (do-eval-string s))
       (else
        (tex2page-string
         (cl-with-output-to-string ((current-output-port))
          (do-eval-string s))))))
    (else false))))

(tex-def-prim "\\eval" (lambda () (do-eval ':both)))

(tex-def-prim "\\evalh" (lambda () (do-eval ':html)))

(tex-def-prim "\\evalq" (lambda () (do-eval ':quiet)))

(define (do-thebibliography) (do-end-para) (get-group)
 (cond
  ((eq? *tex-format* ':latex)
   (tex2page-string
    (if *using-chapters-p* "\\chapter*{\\bibname}" "\\section*{\\refname}")))
  (else false))
 (bgroup) (set! *bibitem-num* 0) (tex2page-string "\\let\\em\\it")
 (tex2page-string "\\def\\newblock{ }") (emit "<table>") (munch-newlines)
 (emit-newline))

(tex-def-prim "\\beginthebibliography" do-thebibliography)

(tex-def-prim "\\endthebibliography"
 (lambda () (emit "</table>") (egroup) (do-para)))

(define (do-bibitem) (do-end-para)
 (let ((bibmark (get-bracketed-text-if-any)))
   (cond ((not (= *bibitem-num* 0)) (emit "</td></tr>") (emit-newline))
         (else false))
   (emit "<tr><td class=\"rightline bibitem\">")
   (set! *bibitem-num* (+ *bibitem-num* 1))
   (let ((bibitem-num-s (write-to-string *bibitem-num*)))
     (let ((key (string-append "cite{" (get-peeled-group) "}")))
       (let ((node-name
              (string-append *html-node-prefix* "bib_" bibitem-num-s)))
         (tex-def-0arg "\\TIIPcurrentnodename" node-name)
         (cond ((not bibmark) (set! bibmark bibitem-num-s) bibmark)
               (else false))
         (tex-def-0arg "\\@currentlabel" bibmark)
         (emit-anchor node-name)
         (emit "[")
         (tex2page-string bibmark)
         (emit "]")
         (emit-nbsp 2)
         (do-label-aux key)
         (emit "</td><td>"))))))

(tex-def-prim "\\bibitem" do-bibitem)

(define (do-bibliography-help bibdata) (set! *using-bibliography-p* true)
 (let ((bbl-file
        (string-append *aux-dir/* *jobname* *bib-aux-file-suffix* ".bbl")))
   (write-bib-aux "\\bibdata{")
   (write-bib-aux bibdata)
   (write-bib-aux "}")
   (write-bib-aux #\newline)
   (cond
    ((file-exists? bbl-file) (set! *bibitem-num* 0) (tex2page-file bbl-file)
     (emit-newline))
    (else (flag-missing-piece ':bibliography)
     (non-fatal-error "Bibliography not generated; rerun TeX2page")))))

(define (do-bibliography) (do-bibliography-help (ungroup (get-token))))

(tex-def-prim "\\bibliography" do-bibliography)

(define (do-bibliographystyle-help s) (write-bib-aux "\\bibstyle{")
 (write-bib-aux s) (write-bib-aux "}") (write-bib-aux #\newline))

(define (do-bibliographystyle)
 (do-bibliographystyle-help (ungroup (get-token))))

(tex-def-prim "\\bibliographystyle" do-bibliographystyle)

(define (do-cite-help delim . %lambda-rest-arg)
 (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (extra-text false))
   (when (< 0 %lambda-rest-arg-len)
     (set! extra-text (list-ref %lambda-rest-arg 0)))
   (let ((closing-delim
          (cond ((char=? delim #\{) #\})
                ((char=? delim #\[) #\])
                (else (terror 'do-cite-help "faulty delim" delim)))))
     (ignorespaces)
     (cond
      ((not (char=? (get-actual-char) delim))
       (terror 'do-cite "missing" delim))
      (else false))
     (emit "[")
     (let ((first-key-p true) (key false))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (set! key (get-csv closing-delim))
           (cond ((not key) (return)) (else false))
           (unless %loop-returned
             (cond (first-key-p (set! first-key-p false) first-key-p)
                   (else (emit ",") (emit-nbsp 1))))
           (unless %loop-returned (write-bib-aux "\\citation{"))
           (unless %loop-returned (write-bib-aux key))
           (unless %loop-returned (write-bib-aux "}"))
           (unless %loop-returned (write-bib-aux #\newline))
           (unless %loop-returned
             (do-ref-aux (string-append "cite{" key "}") false false))
           (if %loop-returned %loop-result (%loop))))
       (cond (extra-text (emit ",") (emit-nbsp 1) (tex2page-string extra-text))
             (else false))
       (cond
        ((not (char=? (get-actual-char) closing-delim))
         (terror 'do-cite "missing" closing-delim))
        (else false))
       (cond (first-key-p (terror 'do-cite "empty \\cite")) (else false)))
     (emit "]"))))

(define (do-cite)
 (if (tex2page-flag-boolean "\\TZPopmac")
     (do-cite-help #\[ false)
     (do-cite-help #\{ (get-bracketed-text-if-any))))

(tex-def-prim "\\cite" do-cite)

(define (do-nocite) (ignorespaces)
 (let ((delim (if (tex2page-flag-boolean "\\TZPopmac") #\[ #\{)))
   (let ((closing-delim (if (char=? delim #\{) #\} #\])))
     (cond
      ((not (char=? (get-actual-char) delim))
       (terror 'do-nocite "missing" delim))
      (else false))
     (let ((key false))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (set! key (get-csv closing-delim))
           (cond ((not key) (return)) (else false))
           (unless %loop-returned (write-bib-aux "\\citation{"))
           (unless %loop-returned (write-bib-aux key))
           (unless %loop-returned (write-bib-aux "}"))
           (unless %loop-returned
             (label-bound-p (string-append "cite{" key "}")))
           (unless %loop-returned (write-bib-aux #\newline))
           (if %loop-returned %loop-result (%loop)))))
     (cond
      ((not (char=? (get-actual-char) closing-delim))
       (terror 'do-nocite "missing" closing-delim))
      (else false)))))

(tex-def-prim "\\nocite" do-nocite)

(define (do-color)
 (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
   (do-switch model)))

(tex-def-prim "\\color" do-color)

(define (do-colorbox)
 (let ((model (get-bracketed-text-if-any)))
   (let ((color (get-group)))
     (let ((text (get-peeled-group)))
       (toss-back-char #\})
       (toss-back-string text)
       (toss-back-string color)
       (cond
        (model (toss-back-char #\]) (toss-back-string model)
         (toss-back-char #\[))
        (else false))
       (toss-back-string "\\bgcolor")
       (toss-back-char #\{)))))

(tex-def-prim "\\colorbox" do-colorbox)

(tex-def-prim "\\bgcolor" (lambda () (do-switch ':bgcolor)))

(define (do-pagecolor)
 (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
   (let ((color (read-color model)))
     (display "body { background-color: " *css-stream*)
     (display color *css-stream*)
     (display "; }" *css-stream*)
     (newline *css-stream*))))

(tex-def-prim "\\pagecolor" do-pagecolor)

(tex-def-prim "\\setcmykcolor" (lambda () (do-switch ':cmyk)))

(define (do-definecolor)
 (let ((name (get-peeled-group)))
   (let ((model (color-model-to-keyword (get-peeled-group))))
     (set! *color-names* (cons (cons name (read-color model)) *color-names*))
     *color-names*)))

(tex-def-prim "\\definecolor" do-definecolor)

(tex-def-prim "\\DefineNamedColor" (lambda () (get-token) (do-definecolor)))

(define (do-opmac-ulink)
 (let ((url (get-bracketed-text-if-any)))
   (let ((link-text (get-group)))
     (let ((durl (doc-internal-url url)))
       (if durl
           (emit-page-node-link-start (car durl) (cadr durl))
           (emit-link-start (fully-qualify-url url)))
       (bgroup)
       (tex2page-string link-text)
       (egroup)
       (emit-link-stop)))))

(tex-def-prim "\\ulink" do-opmac-ulink)

(define (do-opmac-title) (tex-gdef-0arg "\\TIIPtitleused" "1") (do-end-para)
 (let ((title (tex-string-to-html-string (get-till-par))))
   (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
   (write-aux
    (quasiquote (!default-title (unquote (make-external-title title)))))
   (output-title title)))

(tex-def-prim "\\tit" do-opmac-title)

(tex-def-prim "\\notoc" (lambda () (set! *opmac-notoc-p* true) *opmac-notoc-p*))

(tex-def-prim "\\nonum" (lambda () (set! *opmac-nonum-p* true) *opmac-nonum-p*))

(define (do-opmac-heading seclvl) (ignorespaces)
 (let ((header
        (fluid-let ((*tabular-stack* (list ':header)))
         (tex-string-to-html-string (get-till-par)))))
   (let ((nonum-p *opmac-nonum-p*) (notoc-p *opmac-notoc-p*))
     (set! *opmac-nonum-p* false)
     (set! *opmac-notoc-p* false)
     (do-heading-help seclvl false nonum-p notoc-p false header))))

(tex-def-prim "\\chap" (lambda () (do-opmac-heading 0)))

(define (do-opmac-sec)
 (if *math-mode-p* (toss-back-string "\\TIIPsec") (do-opmac-heading 1)))

(tex-def-prim "\\sec" do-opmac-sec)

(tex-def-prim "\\secc" (lambda () (do-opmac-heading 2)))

(define (do-opmac-begitems) (do-end-para) (bgroup)
 (plain-count "\\TIIPopmacliststarted" 0 false) (catcode #\* 13)
 (tex-def-char #\* null "\\TIIPopmacitem" false))

(tex-def-prim "\\begitems" do-opmac-begitems)

(define (do-opmac-item)
 (cond
  ((= (the-count "\\TIIPopmacliststarted") 0)
   (plain-count "\\TIIPopmacliststarted" 1 false)
   (set! *tabular-stack* (cons ':opmac-itemize *tabular-stack*)) (emit "<")
   (emit (case *opmac-list-style* ((#\o #\- #\x #\X) "u") (else "o")))
   (emit "l style=\"list-style-type: ")
   (emit
    (case *opmac-list-style*
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
  (else false))
 (do-regular-item))

(define (do-opmac-enditems) (egroup) (do-end-para)
 (pop-tabular-stack ':opmac-itemize) (emit "</")
 (emit (case *opmac-list-style* ((#\o #\- #\x #\X) "u") (else "o")))
 (emit "l>") (do-noindent))

(tex-def-prim "\\enditems" do-opmac-enditems)

(define (do-opmac-ii retainp)
 (let ((lhs (get-word)))
   (let ((sub
          (and *opmac-index-sub-table*
               (table-get lhs *opmac-index-sub-table*))))
     (if retainp (toss-back-string lhs) (ignorespaces))
     (do-index-help
      (cond (sub sub)
            (else
             (string=join (map escape-opmac-index-entry (string=split lhs #\/))
              #\!)))))))

(tex-def-prim "\\ii" (lambda () (do-opmac-ii false)))

(tex-def-prim "\\iid" (lambda () (do-opmac-ii true)))

(define (do-opmac-iis)
 (let ((lhs (get-word)))
   (let ((rhs (get-peeled-group)))
     (let ((lhs-list (map escape-opmac-index-entry (string=split lhs #\/))))
       (let ((rhs-list (map escape-opmac-index-entry (string=split rhs #\/))))
         (let ((sub ""))
           (cond
            ((not (= (length lhs-list) (length rhs-list)))
             (terror 'do-opmac-iis "Malformed \\iis."))
            (else false))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (cond ((null? lhs-list) (return true)) (else false))
               (unless %loop-returned
                 (let ((additive
                        (string-append
                         (let* ((%pop-old-stack lhs-list)
                                (%pop-top-value (car %pop-old-stack)))
                           (begin (set! lhs-list (cdr %pop-old-stack))
                            lhs-list)
                           %pop-top-value)
                         "@"
                         (let* ((%pop-old-stack rhs-list)
                                (%pop-top-value (car %pop-old-stack)))
                           (begin (set! rhs-list (cdr %pop-old-stack))
                            rhs-list)
                           %pop-top-value))))
                   (set! sub
                    (cond ((string=? sub "") additive)
                          (else (string-append sub "!" additive))))
                   sub))
               (if %loop-returned %loop-result (%loop))))
           (cond
            ((not *opmac-index-sub-table*) (flag-missing-piece ':fresh-index))
            (else false))
           (!opmac-iis lhs sub)
           (write-aux
            (quasiquote (!opmac-iis (unquote lhs) (unquote sub))))))))))

(tex-def-prim "\\iis" do-opmac-iis)

(define (do-opmac-usebibtex)
 (let ((bibfile (ungroup (get-token))))
   (let ((bibstyle (ungroup (get-token))))
     (do-bibliographystyle-help bibstyle)
     (do-bibliography-help bibfile))))

(tex-def-prim "\\usebibtex" do-opmac-usebibtex)

(define (do-opmac-bib) (do-para) (set! *bibitem-num* (+ *bibitem-num* 1))
 (let ((key0 (get-bracketed-text-if-any)))
   (let ((bibitem-num-s (write-to-string *bibitem-num*)))
     (let ((key (string-append "cite{" key0 "}")))
       (let ((node-name
              (string-append *html-node-prefix* "bib_" bibitem-num-s)))
         (cond ((not key0) (terror 'do-opmac-bib "Improper \\bib entry"))
               (else false))
         (tex-def-0arg "\\TIIPcurrentnodename" node-name)
         (tex-def-0arg "\\@currentlabel" bibitem-num-s)
         (emit-anchor node-name)
         (emit "[")
         (tex2page-string bibitem-num-s)
         (emit "]")
         (emit-nbsp 2)
         (do-label-aux key))))))

(tex-def-prim "\\bib" do-opmac-bib)

(define (do-rcite) (do-cite-help #\[ false))

(tex-def-prim "\\rcite" do-rcite)

(tex-def-prim "\\maketoc" do-toc)

(tex-let-prim "\\begtt" "\\begintt")

(tex-def-prim "\\makeindex" (lambda () (do-inputindex false)))

(tex-defsym-prim "\\checkmark" "&#x2713;")

(tex-defsym-prim "\\circledR" "&#xae;")

(tex-defsym-prim "\\maltese" "&#x2720;")

(tex-defsym-prim "\\yen" "&#xa5;")

(tex-defsym-math-prim "\\beth" "&#x2136;")

(tex-defsym-math-prim "\\daleth" "&#x2138;")

(tex-defsym-math-prim "\\gimel" "&#x2137;")

(tex-defsym-math-prim "\\Bbbk" "&#x1d55c;")

(tex-defsym-math-prim "\\Finv" "&#x2132;")

(tex-defsym-math-prim "\\Game" "&#x2141;")

(tex-defsym-math-prim "\\lozenge" "&#x2662;")

(tex-defsym-math-prim "\\mho" "&#x2127;")

(tex-defsym-math-prim "\\sphericalangle" "&#x2222;")

(tex-defsym-math-prim "\\square" "&#x25ab;")

(tex-let-prim "\\Box" "\\square")

(tex-defsym-math-prim "\\geqslant" "&#x2a7e;")

(tex-defsym-math-prim "\\leqslant" "&#x2a7d;")

(tex-defsym-math-prim "\\sqsubset" "&#x228f;")

(tex-defsym-math-prim "\\therefore" "&#x2234;")

(tex-defsym-math-prim "\\sqsupset" "&#x2290;")

(tex-defsym-math-prim "\\because" "&#x2235;")

(tex-defsym-math-prim "\\lhd" "&#x22b2;")

(tex-defsym-math-prim "\\rhd" "&#x22b3;")

(tex-defsym-math-prim "\\unlhd" "&#x22b4;")

(tex-defsym-math-prim "\\unrhd" "&#x22b5;")

(tex-defsym-math-prim "\\leadsto" "&#x2933;")

(tex-defsym-math-prim "\\Join" "&#x2a1d;")

(tex-defsym-math-prim "\\Diamond" "&#x25c7;")

(define (do-math-font f)
 (lambda () (fluid-let ((*math-font* f)) (tex2page-string (get-token)))))

(tex-def-math-prim "\\mathbf" (do-math-font ':bf))

(tex-def-math-prim "\\mathrm" (do-math-font ':rm))

(tex-def-math-prim "\\mathbb" (do-math-font ':bb))

(tex-def-math-prim "\\mathcal" (do-math-font ':cal))

(tex-def-math-prim "\\mathfrak" (do-math-font ':frak))

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

(tex-def-prim "\\abstract"
 (lambda ()
   (tex2page-string "\\quote")
   (tex2page-string "\\centerline{\\bf\\abstractname}\\par")))

(tex-def-prim "\\addcontentsline" do-addcontentsline)

(tex-def-prim "\\addtocounter" do-addtocounter)

(tex-def-prim "\\align" (lambda () (do-equation ':align)))

(tex-def-prim "\\alltt" do-alltt)

(tex-def-prim "\\appendix" do-appendix)

(tex-def-prim "\\arabic" do-arabic)

(tex-def-prim "\\array" (lambda () (do-tabular true)))

(tex-def-prim "\\author" do-author)

(tex-def-prim "\\begin" do-begin)

(tex-def-prim "\\caption" do-caption)

(tex-def-prim "\\center" (lambda () (do-block ':center)))

(tex-def-prim "\\chapter" (lambda () (do-heading 0)))

(tex-def-prim "\\closegraphsfile" do-mfpic-closegraphsfile)

(tex-def-prim "\\convertMPtoPDF" do-convertmptopdf)

(tex-def-prim "\\CR" (lambda () (do-cr "\\CR")))

(tex-def-prim "\\date" do-date)

(tex-def-prim "\\definexref" do-definexref)

(tex-def-prim "\\definitelylatex" definitely-latex)

(tex-def-prim "\\defschememathescape" (lambda () (scm-set-mathescape true)))

(tex-def-prim "\\description"
 (lambda ()
   (do-end-para)
   (set! *tabular-stack* (cons ':description *tabular-stack*))
   (emit "<dl><dt></dt><dd>")))

(tex-def-prim "\\displaymath"
 (lambda () (do-latex-env-as-image "displaymath" ':display)))

(tex-def-prim "\\document" do-document)

(tex-def-prim "\\documentclass" do-documentclass)

(tex-def-prim "\\dontuseimgforhtmlmath"
 (lambda () (tex-def-0arg "\\TZPmathtext" "1")))

(tex-def-prim "\\dontuseimgforhtmlmathdisplay"
 (lambda () (tex-def-0arg "\\TZPmathtext" "1")))

(tex-def-prim "\\dontuseimgforhtmlmathintext" (lambda () true))

(tex-def-prim "\\emph" (lambda () (do-function "\\emph")))

(tex-def-prim "\\endalign" (lambda () (do-end-equation ':align)))

(tex-def-prim "\\endalltt" do-end-alltt)

(tex-def-prim "\\endarray" do-end-tabular)

(tex-def-prim "\\endcenter" do-end-block)

(tex-def-prim "\\enddescription"
 (lambda ()
   (pop-tabular-stack ':description)
   (do-end-para)
   (emit "</dd></dl>")
   (do-noindent)))

(tex-def-prim "\\endeqnarray" (lambda () (do-end-equation ':eqnarray)))

(tex-def-prim "\\endequation" (lambda () (do-end-equation ':equation)))

(tex-def-prim "\\endenumerate" do-endenumerate)

(tex-def-prim "\\endfigure" (lambda () (do-end-table/figure ':figure)))

(tex-def-prim "\\endflushleft" do-end-block)

(tex-def-prim "\\endflushright" do-end-block)

(tex-def-prim "\\endhtmlimg"
 (lambda () (terror 'tex-def-prim "Unmatched \\endhtmlimg")))

(tex-def-prim "\\enditemize" do-enditemize)

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

(tex-def-prim "\\epsfig" do-epsfig)

(tex-def-prim "\\eqnarray" (lambda () (do-equation ':eqnarray)))

(tex-def-prim "\\equation" (lambda () (do-equation ':equation)))

(tex-def-prim "\\expandhtmlindex" expand-html-index)

(tex-def-prim "\\externaltitle" do-externaltitle)

(define (do-table/figure type) (do-end-para) (bgroup)
 (cond
  ((and (eqv? type ':figure) (char=? (snoop-actual-char) #\*))
   (get-actual-char))
  (else false))
 (set! *tabular-stack* (cons type *tabular-stack*)) (get-bracketed-text-if-any)
 (let ((tbl-tag
        (string-append *html-node-prefix* (if (eqv? type 'table) "tbl_" "fig_")
         (gen-temp-string))))
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

(tex-def-prim "\\figure" (lambda () (do-table/figure ':figure)))

(tex-def-prim "\\flushleft" (lambda () (do-block ':flushleft)))

(tex-def-prim "\\flushright" (lambda () (do-block ':flushright)))

(tex-def-prim "\\footnotesize" (lambda () (do-switch ':footnotesize)))

(tex-def-prim "\\hlstart" do-hlstart)

(tex-def-prim "\\hspace" do-hspace)

(tex-def-prim "\\htmladdimg" do-htmladdimg)

(tex-def-prim "\\htmlcolophon" do-htmlcolophon)

(tex-def-prim "\\htmldoctype" do-htmldoctype)

(tex-def-prim "\\htmlgif" (lambda () (do-htmlimg "htmlgif")))

(tex-def-prim "\\htmlheadonly" do-htmlheadonly)

(tex-def-prim "\\htmlimageconversionprogram" do-htmlimageconversionprogram)

(tex-def-prim "\\htmlimageformat" do-htmlimageformat)

(tex-def-prim "\\htmlimg" (lambda () (do-htmlimg "htmlimg")))

(tex-def-prim "\\htmlimgmagnification" do-htmlimgmagnification)

(tex-def-prim "\\htmlmathstyle" do-htmlmathstyle)

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

(tex-def-prim "\\ignorenextinputtimestamp"
 (lambda ()
   (cond
    ((not *inputting-boilerplate-p*) (set! *inputting-boilerplate-p* 0)
     *inputting-boilerplate-p*)
    (else false))))

(tex-def-prim "\\imgpreamble" do-img-preamble)

(tex-def-prim "\\IMGtabbing"
 (lambda () (do-latex-env-as-image "tabbing" ':display)))

(tex-def-prim "\\IMGtabular"
 (lambda () (do-latex-env-as-image "tabular" ':display)))

(tex-def-prim "\\include" do-include)

(tex-def-prim "\\includeexternallabels" do-includeexternallabels)

(tex-def-prim "\\includeonly" do-includeonly)

(tex-def-prim "\\includegraphics" do-includegraphics)

(tex-def-prim "\\inputcss" do-inputcss)

(tex-def-prim "\\inputexternallabels" do-inputexternallabels)

(tex-def-prim "\\itemize" do-itemize)

(tex-def-prim "\\itshape" (lambda () (do-switch ':it)))

(tex-def-prim "\\k" (lambda () (do-diacritic ':ogonek)))

(tex-def-prim "\\large" (lambda () (do-switch ':large)))

(tex-def-prim "\\Large" (lambda () (do-switch ':large-cap)))

(tex-def-prim "\\LARGE" (lambda () (do-switch ':large-up)))

(tex-def-prim "\\latexonly" (lambda () (ignore-tex-specific-text "latexonly")))

(tex-def-prim "\\leftdisplays"
 (lambda () (set! *display-justification* "leftline") *display-justification*))

(tex-def-prim "\\linebreak"
 (lambda () (get-bracketed-text-if-any) (emit "<br>")))

(tex-def-prim "\\listing" do-verbatiminput)

(tex-def-prim "\\lstlisting" (lambda () (do-verbatim-latex "lstlisting")))

(tex-def-prim "\\mailto" do-mailto)

(tex-def-prim "\\makeatletter" (lambda () (catcode #\@ 11)))

(tex-def-prim "\\makeatother" (lambda () (catcode #\@ 12)))

(tex-def-prim "\\maketitle" do-maketitle)

(tex-def-prim "\\marginnote" do-marginnote)

(tex-def-prim "\\marginpar" do-marginpar)

(tex-def-prim "\\mathp" do-mathp)

(tex-def-prim "\\mfpic" do-mfpic)

(tex-def-prim "\\minipage" do-minipage)

(tex-def-prim "\\mplibcode" do-mplibcode)

(tex-def-prim "\\newcommand" (lambda () (do-newcommand false)))

(tex-def-prim "\\newcounter" do-newcounter)

(tex-def-prim "\\newenvironment" (lambda () (do-newenvironment false)))

(tex-def-prim "\\newtheorem" do-newtheorem)

(tex-def-prim "\\noad"
 (lambda () (tex-def-0arg "\\TZPcolophondisablecredit" "1")))

(tex-def-prim "\\node" do-node)

(tex-def-prim "\\nonumber" do-nonumber)

(tex-def-prim "\\noslatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "0")))

(tex-def-prim "\\notimestamp"
 (lambda () (tex-def-0arg "\\TZPcolophondisabletimestamp" "1")))

(tex-def-prim "\\nr" (lambda () (do-cr "\\nr")))

(tex-def-prim "\\@ldc@l@r" do-color)

(tex-def-prim "\\opengraphsfile" do-mfpic-opengraphsfile)

(tex-def-prim "\\pagebreak" (lambda () (get-bracketed-text-if-any) (do-eject)))

(tex-def-prim "\\paragraph" (lambda () (do-heading 4)))

(tex-def-prim "\\part" (lambda () (do-heading -1)))

(tex-def-prim "\\pdfximage" do-pdfximage)

(tex-def-prim "\\picture" (lambda () (do-latex-env-as-image "picture" false)))

(tex-def-prim "\\plainfootnote" do-plain-footnote)

(tex-def-prim "\\printindex" (lambda () (do-inputindex true)))

(tex-def-prim "\\providecommand" (lambda () (do-newcommand false)))

(tex-def-prim "\\quote"
 (lambda () (do-end-para) (emit "<blockquote>") (bgroup)))

(tex-def-prim "\\r" (lambda () (do-diacritic ':ring)))

(tex-def-prim "\\readtocfile" do-toc)

(tex-def-prim "\\refexternal" do-refexternal)

(tex-def-prim "\\refn" do-ref)

(tex-def-prim "\\renewcommand" (lambda () (do-newcommand true)))

(tex-def-prim "\\renewenvironment" (lambda () (do-newenvironment true)))

(tex-def-prim "\\resetatcatcode" (lambda () (catcode #\@ 12)))

(tex-def-prim "\\resizebox" do-resizebox)

(tex-def-prim "\\Romannumeral" (lambda () (do-romannumeral true)))

(tex-def-prim "\\ruledtable" do-ruledtable)

(tex-def-prim "\\sc" (lambda () (do-switch ':sc)))

(tex-def-prim "\\schemebox"
 (lambda () (do-scm-slatex-lines "schemebox" false false)))

(tex-def-prim "\\schemeresponsebox"
 (lambda () (do-scm-slatex-lines "schemeresponsebox" false ':result)))

(tex-def-prim "\\schemeresult" (lambda () (do-scm ':result)))

(tex-def-prim "\\scmspecialsymbol" do-scm-set-specialsymbol)

(tex-def-prim "\\scriptsize" (lambda () (do-switch ':scriptsize)))

(tex-def-prim "\\section" (lambda () (do-heading 1)))

(tex-def-prim "\\seealso" do-see-also)

(tex-def-prim "\\setcounter" do-setcounter)

(tex-def-prim "\\sf" (lambda () (do-switch ':sf)))

(tex-def-prim "\\sidx" do-index)

(tex-def-prim "\\slatexdisable" get-group)

(tex-def-prim "\\slatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "1")))

(tex-def-prim "\\small" (lambda () (do-switch ':small)))

(tex-def-prim "\\stepcounter" do-stepcounter)

(tex-def-prim "\\strike" (lambda () (do-switch ':strike)))

(tex-def-prim "\\style" do-opmac-list-style)

(tex-def-prim "\\subparagraph" (lambda () (do-heading 5)))

(tex-def-prim "\\subsection" (lambda () (do-heading 2)))

(tex-def-prim "\\subsubsection" (lambda () (do-heading 3)))

(tex-def-prim "\\symfootnote" do-symfootnote)

(tex-def-prim "\\tabbing" do-tabbing)

(tex-def-prim "\\table" (lambda () (do-table/figure ':table)))

(tex-def-prim "\\tableplain" do-table-plain)

(tex-def-prim "\\tableofcontents" do-toc)

(tex-def-prim "\\tabular" (lambda () (do-tabular false)))

(tex-def-prim "\\tag" do-tag)

(tex-def-prim "\\textbf" (lambda () (do-function "\\textbf")))

(tex-def-prim "\\textit" (lambda () (do-function "\\textit")))

(tex-def-prim "\\textrm" (lambda () (do-function "\\textrm")))

(tex-def-prim "\\textsc"
 (lambda ()
   (fluid-let ((*in-small-caps-p* true)) (tex2page-string (get-group)))))

(tex-def-prim "\\textsl" (lambda () (do-function "\\textsl")))

(tex-def-prim "\\texttt" (lambda () (do-function "\\texttt")))

(tex-def-prim "\\textvisiblespace" (lambda () (emit *verbatim-visible-space*)))

(tex-def-prim "\\thebibliography" do-thebibliography)

(tex-def-prim "\\TIIPanchor" do-anchor-for-potential-label)

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

(tex-def-prim "\\today" do-today)

(tex-def-prim "\\typein" do-typein)

(tex-def-prim "\\undefcsactive" do-undefcsactive)

(tex-def-prim "\\undefschememathescape" (lambda () (scm-set-mathescape false)))

(tex-def-prim "\\unscmspecialsymbol" do-scm-unset-specialsymbol)

(define (do-verbatim-eplain)
 (fluid-let ((*inside-eplain-verbatim-p* true) (*catcodes* *catcodes*))
  (catcode #\\ 11) (catcode *esc-char-verb* 0)
  (let* ((%loop-returned false)
         (%loop-result 0)
         (return
          (lambda %args
            (set! %loop-returned true)
            (set! %loop-result (and (pair? %args) (car %args))))))
    (let %loop
      ()
      (cond ((not *inside-eplain-verbatim-p*) (return)) (else false))
      (unless %loop-returned
        (let ((c (get-actual-char)))
          (cond ((not c) (terror 'do-verbatim-eplain "Eof inside verbatim"))
                (else false))
          (cond
           ((= (catcode c) **escape**) (toss-back-char c)
            (let ((x (get-ctl-seq)))
              (cond ((string=? x "\\ ") (emit " "))
                    (else (do-tex-ctl-seq-completely x)))))
           ((char=? c #\ ) (emit "&#xa0;"))
           ((char=? c #\newline) (emit "<br>") (emit-newline))
           (else (emit-html-char c)))))
      (if %loop-returned %loop-result (%loop))))))

(define (do-verbatim)
 (if (eqv? *tex-format* ':latex)
     (do-verbatim-latex "verbatim")
     (do-verbatim-eplain)))

(tex-def-prim "\\verbatim" do-verbatim)

(tex-def-prim "\\Verbatim" (lambda () (do-verbatim-latex "Verbatim")))

(tex-def-prim "\\verbc" do-verbc)

(tex-def-prim "\\verbinput" do-opmac-verbinput)

(tex-def-prim "\\vspace" do-vspace)

(define (do-writenumberedcontentsline)
 (let ((toc (get-peeled-group)))
   (cond
    ((not (string=? toc "toc"))
     (terror 'do-writenumberedcontentsline "only #1=toc supported"))
    (else false))
   (do-writenumberedtocline)))

(tex-def-prim "\\writenumberedcontentsline" do-writenumberedcontentsline)

(tex-def-prim "\\xrefn" do-ref)

(tex-def-prim "\\xspace" do-xspace)

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

(define (do-latex-intext-math)
 (do-intext-math
  (let ((o (open-output-string)))
    (dump-till-ctl-seq "\\)" o)
    (get-output-string o))))

(tex-def-prim "\\(" do-latex-intext-math)

(define (do-latex-display-math)
 (do-display-math
  (let ((o (open-output-string)))
    (dump-till-ctl-seq "\\]" o)
    (get-output-string o))))

(tex-def-prim "\\[" do-latex-display-math)

(tex-def-prim "\\)" egroup)

(tex-def-prim "\\]" egroup)

(tex-def-prim "\\>"
 (lambda ()
   (cond
    ((and (not (null? *tabular-stack*)) (eqv? (car *tabular-stack*) ':tabbing))
     (emit-nbsp 3))
    (else false))))

(tex-def-prim (list->string (list #\\ #\newline)) emit-newline)

(let ((tex *tex-logo*))
  (let ((bib
         (string-append "B" "<span style=\"" "text-transform: uppercase"
          "\"><small>ib</small></span>")))
    (let ((context (string-append "Con" tex "t")))
      (let ((latex
             (string-append "L" "<span style=\"" "position: relative; "
              "bottom: 0.3ex; " "margin-left: -0.36em; "
              "margin-right: -0.15em; " "text-transform: uppercase"
              "\"><small>a</small></span>" tex)))
        (let ((xe
               (string-append "X" "<span style=\""
                "text-transform: uppercase; " "position: relative; "
                "top: 0.5ex; " "margin-left: -0.125em; "
                "margin-right: -0.1667em" "\">&#x1dd;</span>")))
          (let ((thinspace (kern ".16667em")))
            (let ((ii/e
                   (string-append "<span style=\"" "margin-left: .05em"
                    "\">2<span>" "<span style=\"" "position: relative; "
                    "top: .5ex" "\">&#x3b5;</span>")))
              (tex-def-prim "\\BibTeX" (lambda () (emit bib) (emit tex)))
              (tex-def-prim "\\ConTeXt" (lambda () (emit context)))
              (tex-def-prim "\\eTeX" (lambda () (emit "&#x3b5;-") (emit tex)))
              (tex-def-prim "\\LaTeX" (lambda () (emit latex)))
              (tex-def-prim "\\LaTeXe" (lambda () (emit latex) (emit ii/e)))
              (tex-def-prim "\\XeLaTeX"
               (lambda () (emit xe) (emit thinspace) (emit latex)))
              (tex-def-prim "\\XeTeX" (lambda () (emit xe) (emit tex))))))))))

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

(tex-def-prim "\\cline" get-group)

(tex-def-prim "\\externalref" get-group)

(tex-def-prim "\\GOBBLEARG" get-group)

(tex-def-prim "\\hyphenation" get-group)

(tex-def-prim "\\newlength" get-group)

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

(tex-def-prim "\\oddsidemargin" eat-dimen)

(tex-def-prim "\\pagewidth" eat-dimen)

(tex-def-prim "\\parbox" (lambda () (get-bracketed-text-if-any) (get-group)))

(tex-def-prim "\\parsep" eat-dimen)

(tex-def-prim "\\rightcodeskip" eat-dimen)

(tex-def-prim "\\scriptfont" get-token)

(tex-def-prim "\\scriptscriptfont" get-token)

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

(tex-def-prim "\\skewchar" (lambda () (get-token) (eat-integer)))

(tex-def-prim "\\usepackage"
 (lambda () (get-bracketed-text-if-any) (get-group) (probably-latex)))

(tex-def-prim "\\setmainfont"
 (lambda ()
   (get-bracketed-text-if-any)
   (get-group)
   (get-bracketed-text-if-any)))

(tex-def-prim "\\setsansfont"
 (lambda ()
   (get-bracketed-text-if-any)
   (get-group)
   (get-bracketed-text-if-any)))

(tex-def-prim "\\setmonofont"
 (lambda ()
   (get-bracketed-text-if-any)
   (get-group)
   (get-bracketed-text-if-any)))

(tex-def-prim "\\readindexfile" (lambda () (get-token) (do-inputindex false)))

(tex-let-prim "\\colophon" "\\htmlcolophon")

(tex-let-prim "\\endabstract" "\\endquote")

(tex-let-prim "\\mbox" "\\hbox")

(tex-let-prim "\\documentstyle" "\\documentclass")

(tex-let-prim "\\quotation" "\\quote")

(tex-let-prim "\\endquotation" "\\endquote")

(tex-let-prim "\\TIIPdate" "\\today")

(tex-let-prim "\\schemeinput" "\\scminput")

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

(tex-let-prim "\\verbatimfile" "\\verbatiminput")

(tex-let-prim "\\verbescapechar" "\\verbatimescapechar")

(tex-let-prim "\\setverbatimescapechar" "\\verbescapechar")

(tex-let-prim "\\nohtmlmathimg" "\\dontuseimgforhtmlmath")

(tex-let-prim "\\nohtmlmathintextimg" "\\dontuseimgforhtmlmathintext")

(tex-let-prim "\\nohtmlmathdisplayimg" "\\dontuseimgforhtmlmathdisplay")

(tex-let-prim "\\writetotoc" "\\writenumberedtocline")

(tex-let-prim "\\textdegree" "\\degree")

(tex-let-prim "\\trademark" "\\TM")

(tex-let-prim "\\texttrademark" "\\TM")

(tex-let-prim "\\n" "\\noindent")

(tex-let-prim "\\Lbackslash" "\\char`\\\\")

(tex-let-prim "\\Ltilde" "\\char`\\~")

(tex-let-prim "\\Llbrace" "\\char`\\{")

(tex-let-prim "\\Lrbrace" "\\char`\\}")

(tex-let-prim "\\Lsup" "\\char`\\^")

(tex-let-prim "\\Lsub" "\\char`\\_")

(define (tex2page tex-file)
 (cond ((not (= *write-log-index* 0)) (newline)) (else false))
 (cond
  ((or (not tex-file) (string=? tex-file "")) (set! tex-file "--missing-arg")
   tex-file)
  (else false))
 (fluid-let
  ((*afterassignment* false) (*afterbye* null) (*afterpar* null)
   (*aux-dir* false) (*aux-dir/* "") (*aux-stream* false) (*basic-style* "")
   (*bib-aux-stream* false) (*bibitem-num* 0) (*catcodes* *catcodes*)
   (*color-names* null) (*css-stream* false) (*current-source-file* false)
   (*current-tex2page-input* false) (*display-justification* "centerline")
   (*doctype* *doctype*) (*dotted-counters* (make-table ':test equal?))
   (*dumping-nontex-p* false) (*emit-enabled-p* true) (*equation-number* false)
   (*equation-numbered-p* true) (*equation-position* 0) (*esc-char-verb* #\|)
   (*eval-for-tex-only-p* false)
   (*external-label-tables* (make-table ':test equal?)) (*footnote-list* null)
   (*footnote-sym* 0)
   (*global-texframe* (make-texframe* ':catcodes *catcodes*))
   (*graphics-file-extensions* '(".eps")) (*html* false) (*html-head* null)
   (*html-only* 0) (*html-page* false) (*html-page-count* 0)
   (*img-file-count* 0) (*img-file-tally* 0) (*imgdef-file-count* 0)
   (*imgpreamble* "") (*imgpreamble-inferred* null) (*in-alltt-p* false)
   (*in-display-math-p* false) (*in-para-p* false) (*in-small-caps-p* false)
   (*includeonly-list* true) (*index-count* 0) (*index-page* false)
   (*index-stream* false) (*index-table* (make-table))
   (*infructuous-calls-to-tex2page* 0) (*input-line-no* 0)
   (*input-streams* (make-table)) (*inputting-boilerplate-p* false)
   (*inside-appendix-p* false) (*jobname* "texput") (*label-stream* false)
   (*label-source* false) (*label-table* (make-table ':test equal?))
   (*last-modification-time* false) (*last-page-number* -1)
   (*latex-probability* 0) (*ligatures-p* true)
   (*loading-external-labels-p* false) (*log-file* false) (*log-stream* false)
   (*main-tex-file* false) (*math-delim-left* false) (*math-delim-right* false)
   (*math-height* 0) (*math-mode-p* false) (*mfpic-file-num* false)
   (*mfpic-file-stem* false) (*mfpic-stream* false) (*missing-eps-files* null)
   (*missing-pieces* null) (*mp-files* null) (*not-processing-p* false)
   (*opmac-active-tt-char* false) (*opmac-index-sub-table* false)
   (*opmac-list-style* #\o) (*opmac-nonum-p* false) (*opmac-notoc-p* false)
   (*opmac-verbinput-table* (make-table ':test equal?)) (*outer-p* true)
   (*output-streams* (make-table)) (*outputting-external-title-p* false)
   (*outputting-to-non-html-p* false) (*package* *this-package*)
   (*quote-level* 0) (*reading-control-sequence-p* false)
   (*recent-node-name* false) (*redirect-delay* false) (*redirect-url* false)
   (*reg-num-count* 255) (*scm-builtins* false) (*scm-dribbling-p* false)
   (*scm-keywords* false) (*scm-special-symbols* false) (*scm-variables* false)
   (*scripts* null) (*section-counter-dependencies* (make-table))
   (*section-counters* (make-table)) (*slatex-math-escape* false)
   (*source-changed-since-last-run-p* false) (*start-time* (current-seconds))
   (*stylesheets* null) (*subjobname* false) (*tabular-stack* null)
   (*temp-string-count* 0) (*temporarily-use-utf8-for-math-p* false)
   (*tex-env* null) (*tex-format* ':plain) (*tex-if-stack* null)
   (*tex-output-format* false)
   (*tex2page-inputs*
    (string=split (getenv "TEX2PAGEINPUTS") *path-separator*))
   (*title* false) (*toc-list* null) (*toc-page* false)
   (*unresolved-xrefs* null) (*using-bibliography-p* false)
   (*using-chapters-p* false) (*using-index-p* false) (*verb-display-p* false)
   (*verb-stream* false) (*verb-visible-space-p* false)
   (*verb-written-files* null) (*write-log-index* 0)
   (*write-log-possible-break-p* false))
  (initialize-globals)
  (set! *main-tex-file*
   (actual-tex-filename tex-file (check-input-file-timestamp-p tex-file)))
  (write-log "This is TeX2page, Version ") (write-log *tex2page-version*)
  (write-log #\ ) (write-log #\() (write-log *scheme-version*) (write-log #\))
  (write-log #\  true) (write-log (seconds-to-human-time *start-time*) true)
  (write-log ':separation-newline)
  (cond
   (*main-tex-file* (set! *subjobname* *jobname*)
    (set! *html-page* (string-append *aux-dir/* *jobname* *output-extension*))
    (set! *html*
     (make-ostream* ':stream
      (let* ((%f *html-page*)
             (%ee (list ':direction ':output ':if-exists ':supersede))
             (%direction (memv ':direction %ee))
             (%if-exists (memv ':if-exists %ee))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %direction (set! %direction (cadr %direction)))
        (when %if-exists (set! %if-exists (cadr %if-exists)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
        (cond
         ((eqv? %direction ':output)
          (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
            (delete-file %f))
          (open-output-file %f))
         ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
         (else (open-input-file %f))))))
    (do-start)
    (fluid-let ((*html-only* (add1 *html-only*)))
     (tex2page-file-if-exists (file-in-home ".tex2page.t2p"))
     (tex2page-file-if-exists ".tex2page.t2p")
     (let ((f (actual-tex-filename (string-append *jobname* ".t2p"))))
       (cond (f (tex2page-file f)) (else false))))
    (cond
     ((not (eqv? (tex2page-file *main-tex-file*) ':encountered-bye))
      (insert-missing-end))
     (else false))
    (do-bye))
   (else (tex2page-help tex-file)))
  (output-stats)))

(tex2page
 (let ((args (current-command-line-arguments)))
   (and (> (vector-length args) 0) (vector-ref args 0))))
