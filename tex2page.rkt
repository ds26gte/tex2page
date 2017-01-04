":";exec racket -f $0 -m -- "$@"
  
(require mzlib/process)

(require mzlib/trace)

(require racket/private/more-scheme)

(define-syntax subseq
 (lambda (%so)
   (datum->syntax %so
    (let ((%so-d (syntax->datum %so)))
      (apply
       (lambda (s i . z)
         (if (null? z)
             `(substring ,s ,i)
             `(substring ,s ,i ,(car z))))
       (cdr %so-d))))))

(define *operating-system*
 (if (getenv "COMSPEC")
     (let ((term (getenv "TERM")))
       (if (and (string? term) (string=? term "cygwin"))
           ':cygwin
           ':windows))
     ':unix))

(define *scheme-version*
 (string-append "Racket " (version) " " (symbol->string *operating-system*)))

(define *path-separator*
 (if (eqv? *operating-system* ':windows)
     #\;
     #\:))

(define *directory-separator*
 (if (eqv? *operating-system* ':windows)
     "\\"
     "/"))

(define *package* false)

(define eval1 eval)

(define (decode-universal-time s)
 (let ((ht (and s (seconds->date s))))
   (cond
    (ht
     (list false (date-minute ht) (date-hour ht) (date-day ht) (date-month ht)
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
             (number->string
              (if (= h 0)
                  12
                  h)))
           ":"
           (if (< m 10)
               "0"
               "")
           (number->string m) " "
           (if (<= 0 h 11)
               "a"
               "p")
           "m UTC"
           (if (> tz 0)
               "+"
               "−")
           (number->string (abs tz)))
          "")))))

(define (seconds-to-human-time s)
 (strftime "%a, %b %e, %Y, %l:%M %p %Z" (seconds->date s)))

(define list-position
 (lambda (x s)
   (let loop
     ((s s) (i 0))
     (cond ((null? s) false) ((eq? (car s) x) i)
           (else
            (loop (cdr s)
                  (+ i 1)))))))

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
                     (vector-set! vv i
                      (if (pair? f)
                          (cadr f)
                          (not 't)))
                     (loop (+ i 1)
                           (cdr ff)))
                   0))
             (let* ((ff-without-colons
                     (map
                      (lambda (f)
                        (symbol->string
                         (if (pair? f)
                             (car f)
                             f)))
                      ff))
                    (ff-with-colons
                     (map (lambda (f) (string->symbol (string-append ":" f)))
                          ff-without-colons)))
               `(begin
                 (define ,(string->symbol (string-append "make-" s-s))
                  (lambda fvfv
                    (let ((st (make-vector ,n+1)) (ff ',ff-with-colons))
                      (vector-set! st 0 ',s)
                      ,@(let loop
                          ((i 1) (r '()))
                          (if (>= i n+1)
                              r
                              (loop (+ i 1)
                                    (cons
                                     `(vector-set! st ,i ,(vector-ref vv i))
                                     r))))
                      (let loop
                        ((fvfv fvfv))
                        (if (null? fvfv)
                            0
                            (begin
                             (vector-set! st
                              (+ (list-position (car fvfv) ff) 1) (cadr fvfv))
                             (loop (cddr fvfv)))))
                      st)))
                 ,@(let loop
                     ((i 1) (procs '()))
                     (if (>= i n+1)
                         procs
                         (loop (+ i 1)
                               (let* ((f-s
                                       (list-ref ff-without-colons (- i 1))))
                                 (cons
                                  `(define
                                    ,(string->symbol
                                      (string-append s-s "-" f-s))
                                    (lambda (x) (vector-ref x ,i)))
                                  (cons
                                   `(define
                                     ,(string->symbol
                                       (string-append "set!" s-s "-" f-s))
                                     (lambda (x v) (vector-set! x ,i v)))
                                   procs))))))
                 (define ,(string->symbol (string-append s-s "?"))
                  (lambda (x)
                    (and (vector? x) (eq? (vector-ref x 0) ',s)))))))))
       (cdr %so-d))))))

(define-syntax cl-with-output-to-string
 (lambda (%so)
   (datum->syntax %so
    (let ((%so-d (syntax->datum %so)))
      (apply
       (lambda (ignore-wots-arg . body)
         `(with-output-to-string (lambda () ,@body)))
       (cdr %so-d))))))

(defstruct table (test eqv?) (alist '()))

(define (table-get k tbl . d)
 (cond
  ((lassoc k (table-alist tbl) (table-test tbl)) =>
   (lambda (c) (vector-ref (cdr c) 0)))
  ((pair? d) (car d)) (else false)))

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
         (if (equ? (car c) k)
             c
             (loop (cdr al)))))))

(define (write-to-string n . z)
 (if (pair? z)
     (number->string
      (if (inexact? n)
          (inexact->exact n)
          n)
      16)
     (number->string n)))

(define (number-to-roman n . upcase?)
 (set! upcase? (and (pair? upcase?) (car upcase?)))
 (unless (and (integer? n) (>= n 0))
   (terror 'number-to-roman "Missing number"))
 (let ((roman-digits
        '((1000 #\m 100) (500 #\d 100) (100 #\c 10) (50 #\l 10) (10 #\x 1)
          (5 #\v 1) (1 #\i 0)))
       (approp-case
        (lambda (c)
          (if upcase?
              (char-upcase c)
              c))))
   (let loop
     ((n n) (dd roman-digits) (s '()))
     (if (null? dd)
         (if (null? s)
             "0"
             (list->string (reverse s)))
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
     (cond ((>= i n) false) ((char=? (string-ref s i) c) i)
           (else (loop (+ i 1)))))))

(define (string-reverse-index s c)
 (let loop
   ((i (- (string-length s) 1)))
   (cond ((< i 0) false) ((char=? (string-ref s i) c) i)
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

;Translated from Common Lisp source tex2page.lisp by CLiiScm v. 20170103.


(define *tex2page-version* "20170103")

(define *tex2page-website* "http://ds26gte.github.io/tex2page/index.html")

(define *tex2page-copyright-notice*
 (let ((%type 'string)
       (%ee
        (list "Copyright (C) 1997-" (subseq *tex2page-version* 0 4)
              " Dorai Sitaram")))
   (let ((%res
          (if (eq? %type 'string)
              ""
              null)))
     (let %concatenate-loop
       ((%ee %ee))
       (if (null? %ee)
           %res
           (let ((%a (car %ee)))
             (unless (not %a)
               (set! %res
                (if (eq? %type 'string)
                    (string-append %res
                     (if (string? %a)
                         %a
                         (list->string %a)))
                    (append %res
                            (if (string? %a)
                                (string->list %a)
                                %a)))))
             (%concatenate-loop (cdr %ee)))))
     %res)))

(define retrieve-env (lambda (s) (getenv s)))

(define *short-month-names*
 (vector "Jan" "Feb" "March" "April" "May" "June" "July" "Aug" "Sept" "Oct"
         "Nov" "Dec"))

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

(define *aux-file-suffix* "-Z-A")

(define *bib-aux-file-suffix* "-Z-B")

(define *css-file-suffix* "-Z-S.css")

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

(define *month-names*
 (vector "January" "February" "March" "April" "May" "June" "July" "August"
         "September" "October" "November" "December"))

(define *if-aware-ctl-seqs*
 '("\\csname" "\\else" "\\end" "\\eval" "\\fi" "\\let"))

(define *tex-logo*
 (let ((%type 'string)
       (%ee
        (list "T" "<span style=\"" "position: relative; " "top: 0.5ex; "
              "margin-left: -0.1667em; " "margin-right: -0.125em; "
              "text-transform: uppercase" "\">e</span>" "X")))
   (let ((%res
          (if (eq? %type 'string)
              ""
              null)))
     (let %concatenate-loop
       ((%ee %ee))
       (if (null? %ee)
           %res
           (let ((%a (car %ee)))
             (unless (not %a)
               (set! %res
                (if (eq? %type 'string)
                    (string-append %res
                     (if (string? %a)
                         %a
                         (list->string %a)))
                    (append %res
                            (if (string? %a)
                                (string->list %a)
                                %a)))))
             (%concatenate-loop (cdr %ee)))))
     %res)))

(define *tex-files-to-ignore*
 '("btxmac" "eplain" "epsf" "lmfonts" "mfpic" "supp-pdf"))

(define *afterassignment* false)

(define *afterpar* null)

(define *afterbye* null)

(define *aux-dir* false)

(define *aux-dir/* "")

(define *aux-port* false)

(define *bib-aux-port* false)

(define *bibitem-num* 0)

(define *color-names* null)

(define *comment-char* #\%)

(define *css-port* false)

(define *current-source-file* false)

(define *current-tex2page-input* false)

(define *display-justification* false)

(define *dotted-counters* false)

(define *dumping-nontex-p* false)

(define *equation-number* false)

(define *equation-numbered-p* true)

(define *equation-position* 0)

(define *esc-chars* '(#\\))

(define *esc-char-std* #\\)

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

(define *ignore-active-space-p* false)

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

(define *index-port* false)

(define *index-table* false)

(define *infructuous-calls-to-tex2page* false)

(define *input-line-no* 0)

(define *input-streams* false)

(define *inputting-boilerplate-p* false)

(define *inside-appendix-p* false)

(define *inside-eplain-verbatim-p* false)

(define *it* false)

(define *jobname* false)

(define *label-port* false)

(define *label-source* false)

(define *label-table* false)

(define *last-modification-time* false)

(define *last-page-number* false)

(define *latex-probability* false)

(define *ligatures-p* false)

(define *loading-external-labels-p* false)

(define *log-file* false)

(define *log-port* false)

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

(define *mfpic-port* false)

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

(define *outer-p* false)

(define *output-streams* false)

(define *outputting-external-title-p* false)

(define *outputting-to-non-html-p* false)

(define *quote-level* 0)

(define *reading-control-sequence-p* false)

(define *recent-node-name* false)

(define *remember-index-number* false)

(define *redirect-delay* false)

(define *redirect-url* false)

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

(define *stylesheets* false)

(define *subjobname* *jobname*)

(define *tabular-stack* null)

(define *temp-string-count* false)

(define *temporarily-use-utf8-for-math-p* false)

(define *tex-env* null)

(define *tex-extra-letters* null)

(define *tex-format* false)

(define *tex-if-stack* null)

(define *tex-like-layout-p* false)

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

(define *verb-port* false)

(define *verb-visible-space-p* false)

(define *verb-written-files* null)

(define *write-log-max* 55)

(define *write-log-index* 0)

(define *write-log-possible-break-p* false)

(define *scm-token-delims*
 (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

(defstruct counter* (value 0) (within false))

(defstruct tocentry* level number page label header)

(define char-tex-alphabetic-p
 (lambda (c) (or (char-alphabetic? c) (member c *tex-extra-letters*))))

(define gen-temp-string
 (lambda ()
   (let ((%tmp (+ *temp-string-count* 1)))
     (begin (set! *temp-string-count* %tmp) *temp-string-count*))
   (let ((%type 'string)
         (%ee (list "Temp_" (write-to-string *temp-string-count*))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define file-stem-name
 (lambda (f)
   (let ((slash
          (let ((%position-v #\/)
                (%position-s f)
                (%ee (list ':test char=? ':from-end true)))
            (let ((%position-from-end (memv ':from-end %ee)))
              (when %position-from-end
                (set! %position-from-end (cadr %position-from-end)))
              (if (string? %position-s)
                  ((if %position-from-end
                       string-reverse-index
                       string-index)
                   %position-s %position-v)
                  (list-position %position-v %position-s))))))
     (begin
      (cond
       (slash
        (begin
         (set! f
          (subseq f (+ slash 1)
                  (let ((%length-arg f))
                    ((if (string? %length-arg)
                         string-length
                         length)
                     %length-arg))))
         f))
       (else false))
      (let ((dot
             (let ((%position-v #\.)
                   (%position-s f)
                   (%ee (list ':test char=? ':from-end true)))
               (let ((%position-from-end (memv ':from-end %ee)))
                 (when %position-from-end
                   (set! %position-from-end (cadr %position-from-end)))
                 (if (string? %position-s)
                     ((if %position-from-end
                          string-reverse-index
                          string-index)
                      %position-s %position-v)
                     (list-position %position-v %position-s))))))
        (if dot
            (subseq f 0 dot)
            f))))))

(define file-extension
 (lambda (f)
   (let ((slash
          (let ((%position-v #\/)
                (%position-s f)
                (%ee (list ':test char=? ':from-end true)))
            (let ((%position-from-end (memv ':from-end %ee)))
              (when %position-from-end
                (set! %position-from-end (cadr %position-from-end)))
              (if (string? %position-s)
                  ((if %position-from-end
                       string-reverse-index
                       string-index)
                   %position-s %position-v)
                  (list-position %position-v %position-s)))))
         (dot
          (let ((%position-v #\.)
                (%position-s f)
                (%ee (list ':test char=? ':from-end true)))
            (let ((%position-from-end (memv ':from-end %ee)))
              (when %position-from-end
                (set! %position-from-end (cadr %position-from-end)))
              (if (string? %position-s)
                  ((if %position-from-end
                       string-reverse-index
                       string-index)
                   %position-s %position-v)
                  (list-position %position-v %position-s))))))
     (if (and dot (not (= dot 0)) (or (not slash) (< (+ slash 1) dot)))
         (subseq f dot
                 (let ((%length-arg f))
                   ((if (string? %length-arg)
                        string-length
                        length)
                    %length-arg)))
         false))))

(define ensure-file-deleted
 (lambda (f) (cond ((file-exists? f) (delete-file f)) (else false))))

(define write-aux
 (lambda (e)
   (cond
    ((not *aux-port*)
     (let ((f
            (let ((%type 'string)
                  (%ee (list *aux-dir/* *jobname* *aux-file-suffix* ".scm")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin
        (set! *aux-port*
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
        *aux-port*)))
    (else false))
   (write e *aux-port*)
   (newline *aux-port*)))

(define write-label
 (lambda (e)
   (cond
    ((not *label-port*)
     (let ((f
            (let ((%type 'string)
                  (%ee (list *aux-dir/* *jobname* *label-file-suffix* ".scm")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin
        (set! *label-port*
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
        *label-port*)))
    (else false))
   (write e *label-port*)
   (newline *label-port*)))

(define write-bib-aux
 (lambda (x)
   (cond
    ((not *bib-aux-port*)
     (let ((f
            (let ((%type 'string)
                  (%ee
                   (list *aux-dir/* *jobname* *bib-aux-file-suffix* ".aux")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin
        (set! *bib-aux-port*
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
        *bib-aux-port*)))
    (else false))
   (display x *bib-aux-port*)))

(define write-log
 (lambda (x)
   (cond
    ((not *log-port*)
     (begin
      (set! *log-file*
       (let ((%type 'string) (%ee (list *aux-dir/* *jobname* ".hlog")))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (set! *log-port*
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
      *log-port*))
    (else false))
   (cond
    ((and *write-log-possible-break-p* (char? x) (member x '(#\) #\] #\} #\,)))
     (begin (set! *write-log-possible-break-p* false)
      *write-log-possible-break-p*))
    (else false))
   (cond
    ((and *write-log-possible-break-p* (> *write-log-index* *write-log-max*))
     (newline *log-port*) (newline)
     (begin (set! *write-log-possible-break-p* false)
      (set! *write-log-index* 0) *write-log-index*))
    (else false))
   (cond
    ((not
      (and (= *write-log-index* 0)
           (member x '(:separation-newline :separation-space))))
     (case x
       ((#\newline :separation-newline)
        (cond
         (*write-log-possible-break-p*
          (begin (set! *write-log-possible-break-p* false)
           *write-log-possible-break-p*))
         (else false))
        (newline *log-port*)
        (newline)
        (begin (set! *write-log-index* 0) *write-log-index*))
       ((:separation-space)
        (begin (set! *write-log-possible-break-p* true)
         *write-log-possible-break-p*))
       (else
        (cond
         (*write-log-possible-break-p* (write-char #\  *log-port*)
          (write-char #\ )
          (begin (set! *write-log-index* (+ *write-log-index* 1))
           *write-log-index*)
          (begin (set! *write-log-possible-break-p* false)
           *write-log-possible-break-p*))
         (else false))
        (display x *log-port*)
        (display x)
        (flush-output)
        (let ((%tmp
               (+ *write-log-index*
                  (let ((%tag x))
                    (cond ((char? %tag) 1)
                          ((number? %tag)
                           (let ((%length-arg (write-to-string x)))
                             ((if (string? %length-arg)
                                  string-length
                                  length)
                              %length-arg)))
                          ((string? %tag)
                           (let ((%length-arg x))
                             ((if (string? %length-arg)
                                  string-length
                                  length)
                              %length-arg)))
                          (else 1))))))
          (begin (set! *write-log-index* %tmp) *write-log-index*)))))
    (else false))))

(define display-error-context-lines
 (lambda ()
   (let ((n (or (find-count "\\errorcontextlines") 0)))
     (cond
      ((and *current-source-file* (> n 0))
       (let ((n1 (max 0 (- *input-line-no* (quotient (sub1 n) 2)))))
         (let ((nf (+ n1 n -1)))
           (let ((ll
                  (let ((ip
                         (let* ((%f *current-source-file*)
                                (%ee (list ':direction ':input))
                                (%direction (memv ':direction %ee))
                                (%if-exists (memv ':if-exists %ee))
                                (%if-does-not-exist ':error)
                                (%if-does-not-exist-from-user
                                 (memv ':if-does-not-exist %ee)))
                           (when %direction
                             (set! %direction (cadr %direction)))
                           (when %if-exists
                             (set! %if-exists (cadr %if-exists)))
                           (when %if-does-not-exist-from-user
                             (set! %if-does-not-exist
                              (cadr %if-does-not-exist-from-user)))
                           (cond
                            ((eqv? %direction ':output)
                             (when
                                 (and (eqv? %if-exists ':supersede)
                                      (file-exists? %f))
                               (delete-file %f))
                             (open-output-file %f))
                            ((and (not %if-does-not-exist)
                                  (not (file-exists? %f)))
                             false)
                            (else (open-input-file %f))))))
                    (let ((%with-open-file-res
                           (begin
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
                                  (unless %loop-returned
                                    (let ((l
                                           (let ((%read-line-res
                                                  (read-line ip)))
                                             (when (eof-object? %read-line-res)
                                               (set! %read-line-res
                                                ':eof-object))
                                             %read-line-res)))
                                      (cond ((eq? l ':eof-object) (return ll))
                                            ((< i n1)
                                             (let ((%tmp (+ i 1)))
                                               (begin (set! i %tmp) i)))
                                            ((<= i nf)
                                             (let ((%tmp (+ i 1)))
                                               (begin (set! i %tmp) i))
                                             (let ((%push-new-stack
                                                    (cons (cons i l) ll)))
                                               (begin (set! ll %push-new-stack)
                                                ll)))
                                            (true (return ll)))))
                                  (if %loop-returned
                                      %loop-result
                                      (%loop))))))))
                      (begin
                       (cond
                        (ip
                         ((if (input-port? ip)
                              close-input-port
                              close-output-port)
                          ip))
                        (else false))
                       %with-open-file-res)))))
             (cond
              ((not (null? ll))
               (let ((border "__________________________..."))
                 (let ((only-1-p
                        (=
                         (let ((%length-arg ll))
                           ((if (string? %length-arg)
                                string-length
                                length)
                            %length-arg))
                         1)))
                   (let ((nf (caar ll)))
                     (let ((ll (reverse ll)))
                       (let ((n1 (caar ll)))
                         (begin (write-log "Likely error context: ")
                          (write-log *current-source-file*)
                          (write-log ", line")
                          (cond ((not only-1-p) (write-log "s")) (else false))
                          (write-log " ") (write-log n1)
                          (cond ((not only-1-p) (write-log "-") (write-log nf))
                                (else false))
                          (write-log ":") (write-log #\newline)
                          (write-log " /") (write-log border)
                          (write-log #\newline)
                          (for-each
                           (lambda (l)
                             (write-log " | ")
                             (write-log (cdr l))
                             (write-log #\newline))
                           ll)
                          (write-log " |") (write-log border)
                          (write-log #\newline) (write-log "/"))))))))
              (else false))))))
      (else false)))))

(define edit-offending-file
 (lambda ()
   (let ((calling-from-text-editor-p (retrieve-env "VIMRUNTIME")))
     (cond
      ((not calling-from-text-editor-p)
       (display "Type e to edit file at point of error; x to quit.") (newline)
       (display "? ") (flush-output)
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
           (let ((texedit-string (retrieve-env "TEXEDIT")))
             (begin
              (cond
               (texedit-string
                (cond
                 ((begin (set! *it* (substring? "%d" texedit-string)) *it*)
                  (let ((i *it*))
                    (begin
                     (set! texedit-string
                      (let ((%type 'string)
                            (%ee
                             (list (subseq texedit-string 0 i)
                                   (write-to-string *input-line-no*)
                                   (subseq texedit-string (+ i 2)))))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res)))
                     texedit-string)))
                 (true (begin (set! texedit-string false) texedit-string))))
               (else false))
              (cond
               (texedit-string
                (cond
                 ((begin (set! *it* (substring? "%s" texedit-string)) *it*)
                  (let ((i *it*))
                    (begin
                     (set! texedit-string
                      (let ((%type 'string)
                            (%ee
                             (list (subseq texedit-string 0 i)
                                   *current-source-file*
                                   (subseq texedit-string (+ i 2)))))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res)))
                     texedit-string)))
                 (true (begin (set! texedit-string false) texedit-string))))
               (else false))
              (cond
               ((not texedit-string)
                (display "Ill-formed TEXEDIT; using EDITOR.") (newline)
                (cond
                 ((begin (set! *it* (or (retrieve-env "EDITOR") "vi")) *it*)
                  (let ((e *it*))
                    (begin
                     (set! texedit-string
                      (let ((%type 'string)
                            (%ee
                             (list e " +" (write-to-string *input-line-no*) " "
                                   *current-source-file*)))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res)))
                     texedit-string)))))
               (else false))
              (cond (texedit-string (system texedit-string)) (else false)))))
          (else false))))
      (else false)))))

(define trace-if
 (lambda (write-p . args)
   (cond
    (write-p (write-log ':separation-newline)
     (cond
      ((> *input-line-no* 0) (write-log "l.") (write-log *input-line-no*)
       (write-log #\ ))
      (else false))
     (for-each write-log args) (write-log ':separation-newline))
    (else false))))

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
   (write-log #\ )
   (write-log where)
   (write-log " failed.")
   (write-log ':separation-newline)
   (display-error-context-lines)
   (close-all-open-ports)
   (output-stats)
   (edit-offending-file)
   (error "TeX2page fatal error")))

(define do-errmessage
 (lambda ()
   (write-log ':separation-newline)
   (write-log "! ")
   (write-log (tex-string-to-html-string (get-group)))
   (write-log ':separation-newline)
   (terror 'do-errmessage)))

(define do-tracingall
 (lambda ()
   (tex-def-count "\\tracingcommands" 1 false)
   (tex-def-count "\\tracingmacros" 1 false)))

(defstruct bport* (port false) (buffer null))

(define call-with-input-file/buffered
 (lambda (f th)
   (cond
    ((not (file-exists? f))
     (terror 'call-with-input-file/buffered "I can't find file " f))
    (else false))
   (let ((i
          (let* ((%f f)
                 (%ee (list ':direction ':input))
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
            (begin
             (let ((%fluid-var-*current-tex2page-input* (make-bport* ':port i))
                   (%fluid-var-*current-source-file* f)
                   (%fluid-var-*input-line-no* 1))
               (fluid-let
                ((*input-line-no* %fluid-var-*input-line-no*)
                 (*current-source-file* %fluid-var-*current-source-file*)
                 (*current-tex2page-input*
                  %fluid-var-*current-tex2page-input*))
                (th))))))
       (begin
        (cond
         (i
          ((if (input-port? i)
               close-input-port
               close-output-port)
           i))
         (else false))
        %with-open-file-res)))))

(define call-with-input-string/buffered
 (lambda (s th)
   (let ((%fluid-var-*current-tex2page-input*
          (make-bport* ':buffer
           (let ((%type 'list) (%ee (list s)))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))))
         (%fluid-var-*input-line-no* *input-line-no*))
     (fluid-let
      ((*input-line-no* %fluid-var-*input-line-no*)
       (*current-tex2page-input* %fluid-var-*current-tex2page-input*))
      (th)))))

(define snoop-char
 (lambda ()
   (let ((c (get-char)))
     (begin (toss-back-char c) c))))

(define get-char
 (lambda ()
   (let ((b (bport*-buffer *current-tex2page-input*)))
     (if (null? b)
         (let ((p (bport*-port *current-tex2page-input*)))
           (if (not p)
               ':eof-object
               (let ((c
                      (let* ((%read-char-port p)
                             (%read-char-res
                              (if %read-char-port
                                  (read-char %read-char-port)
                                  (read-char))))
                        (when (eof-object? %read-char-res)
                          (set! %read-char-res ':eof-object))
                        %read-char-res)))
                 (cond ((eq? c ':eof-object) c)
                       ((char=? c #\newline)
                        (let ((%tmp (+ *input-line-no* 1)))
                          (begin (set! *input-line-no* %tmp) *input-line-no*))
                        c)
                       (true c)))))
         (let ((c (car b)))
           (begin
            (begin (set!bport*-buffer *current-tex2page-input* (cdr b))
             (bport*-buffer *current-tex2page-input*))
            c))))))

(define toss-back-string
 (lambda (s)
   (begin
    (set!bport*-buffer *current-tex2page-input*
     (append
      (let ((%type 'list) (%ee (list s)))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))
      (bport*-buffer *current-tex2page-input*)))
    (bport*-buffer *current-tex2page-input*))))

(define toss-back-char
 (lambda (c)
   (let ((%push-new-stack (cons c (bport*-buffer *current-tex2page-input*))))
     (begin (set!bport*-buffer *current-tex2page-input* %push-new-stack)
      (bport*-buffer *current-tex2page-input*)))))

(define emit (lambda (s) (display s *html*)))

(define emit-newline (lambda () (newline *html*)))

(define invisible-space-p
 (lambda (x) (or (eq? x *invisible-space*) (eq? x *outer-invisible-space*))))

(define outer-invisible-space-p (lambda (x) (eq? x *outer-invisible-space*)))

(define check-outerness
 (lambda (c)
   (cond
    ((eq? c *outer-invisible-space*) (begin (set! *outer-p* true) *outer-p*))
    (else false))))

(define snoop-actual-char
 (lambda ()
   (let ((c (snoop-char)))
     (cond ((eq? c ':eof-object) c)
           ((invisible-space-p c) (get-char) (check-outerness c)
            (snoop-actual-char))
           ((char=? c #\return) (get-char)
            (let ((c (snoop-actual-char)))
              (if (and (not (eq? c ':eof-object)) (char=? c #\newline))
                  c
                  (begin (toss-back-char #\newline) #\newline))))
           (true c)))))

(define get-actual-char
 (lambda ()
   (let ((c (get-char)))
     (cond ((eq? c ':eof-object) c)
           ((invisible-space-p c) (check-outerness c) (get-actual-char))
           ((char=? c #\return)
            (let ((c (snoop-actual-char)))
              (if (and (not (eq? c ':eof-object)) (char=? c #\newline))
                  (get-actual-char)
                  #\newline)))
           (true c)))))

(define get-line
 (lambda ()
   (let ((r null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((c (get-actual-char)))
             (cond
              ((eq? c ':eof-object)
               (return
                (if r
                    (let ((%type 'string) (%ee (list (reverse r))))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))
                    c)))
              ((char=? c #\newline)
               (return
                (let ((%type 'string) (%ee (list (reverse r))))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res))))
              (true
               (let ((%push-new-stack (cons c r)))
                 (begin (set! r %push-new-stack) r))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define ignorespaces
 (lambda ()
   (cond
    ((not (and (find-chardef #\ ) (not *ignore-active-space-p*)))
     (let ((newline-active-p (find-chardef #\newline))
           (newline-already-read-p false))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (unless %loop-returned
             (let ((c (snoop-char)))
               (begin
                (cond
                 ((eqv? c #\return) (begin (set! c (snoop-actual-char)) c))
                 (else false))
                (cond ((eq? c ':eof-object) (return))
                      ((invisible-space-p c) (get-char)
                       (cond (*reading-control-sequence-p* (return))
                             (else false)))
                      ((char=? c #\newline)
                       (cond (newline-active-p (return))
                             (newline-already-read-p (toss-back-char #\newline)
                              (return))
                             (true (get-actual-char)
                              (begin (set! newline-already-read-p true)
                               newline-already-read-p))))
                      ((char-whitespace? c) (get-actual-char))
                      (true (return))))))
           (if %loop-returned
               %loop-result
               (%loop))))))
    (else false))))

(define ignore-all-whitespace
 (lambda ()
   (let ((c false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (begin (set! c (snoop-actual-char)) c))
         (unless %loop-returned
           (cond ((eq? c ':eof-object) (return)) (else false)))
         (unless %loop-returned
           (cond ((not (char-whitespace? c)) (return)) (else false)))
         (unless %loop-returned (get-actual-char))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define munch-newlines
 (lambda ()
   (let ((n 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((c (snoop-actual-char)))
             (cond ((eq? c ':eof-object) (return n))
                   ((char=? c #\newline) (get-actual-char)
                    (let ((%tmp (+ n 1)))
                      (begin (set! n %tmp) n)))
                   ((char-whitespace? c) (get-actual-char))
                   (true (return n)))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define munched-a-newline-p
 (lambda ()
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (snoop-actual-char)))
           (cond ((eq? c ':eof-object) (return false))
                 ((char=? c #\newline) (get-actual-char) (return true))
                 ((char-whitespace? c) (get-actual-char))
                 (true (return false)))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define do-xspace
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond
      ((not (member c '(#\  #\" #\. #\! #\, #\: #\; #\? #\/ #\' #\) #\-)))
       (emit #\ ))
      (else false)))))

(define do-relax (lambda () true))

(define esc-char-p (lambda (c) (member c *esc-chars*)))

(define get-ctl-seq
 (lambda ()
   (let ((bs (get-actual-char)))
     (begin
      (cond
       ((not (esc-char-p bs))
        (terror 'get-ctl-seq "Missing control sequence (" bs ")"))
       (else false))
      (let ((c (get-char)))
        (cond ((eq? c ':eof-object) "\\ ") ((invisible-space-p c) "\\ ")
              ((char-tex-alphabetic-p c)
               (let ((%type 'string)
                     (%ee
                      (list
                       (reverse
                        (let ((s (list c #\\)))
                          (let* ((%loop-returned false)
                                 (%loop-result 0)
                                 (return
                                  (lambda %args
                                    (set! %loop-returned true)
                                    (set! %loop-result
                                     (and (pair? %args) (car %args))))))
                            (let %loop
                              ()
                              (unless %loop-returned
                                (let ((c (snoop-char)))
                                  (cond ((eq? c ':eof-object) (return s))
                                        ((invisible-space-p c) (return s))
                                        ((char-tex-alphabetic-p c) (get-char)
                                         (let ((%push-new-stack (cons c s)))
                                           (begin (set! s %push-new-stack) s)))
                                        (true
                                         (cond
                                          ((not
                                            (or *math-mode-p*
                                                *not-processing-p*
                                                (eq? *tex-format* ':texinfo)))
                                           (let ((%fluid-var-*reading-control-sequence-p*
                                                  true))
                                             (fluid-let
                                              ((*reading-control-sequence-p*
                                                %fluid-var-*reading-control-sequence-p*))
                                              (ignorespaces))))
                                          (else false))
                                         (return s)))))
                              (if %loop-returned
                                  %loop-result
                                  (%loop)))))))))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res)))
              (true
               (let ((%type 'string) (%ee (list (list #\\ c))))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res)))))))))

(define ctl-seq-p (lambda (z) (char=? (string-ref z 0) #\\)))

(define if-aware-ctl-seq-p
 (lambda (z)
   (or (member z *if-aware-ctl-seqs*)
       (and
        (>=
         (let ((%length-arg z))
           ((if (string? %length-arg)
                string-length
                length)
            %length-arg))
         3)
        (char=? (string-ref z 1) #\i) (char=? (string-ref z 2) #\f))
       (let ((z-th (find-corresp-prim-thunk z)))
         (if (string? z-th)
             false
             (ormap (lambda (y) (eq? z-th (find-corresp-prim-thunk y)))
              *if-aware-ctl-seqs*))))))

(define get-group-as-reversed-chars
 (lambda ()
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin
      (cond ((eq? c ':eof-object) (terror 'get-group "Runaway argument?"))
            (else false))
      (cond ((not (char=? c #\{)) (terror 'get-group "Missing {"))
            (else false))
      (let ((s (list c)) (nesting 0) (escape-p false))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (unless %loop-returned
              (let ((c (get-actual-char)))
                (begin
                 (cond
                  ((eq? c ':eof-object)
                   (terror 'get-group "Runaway argument?"))
                  (else false))
                 (cond
                  (escape-p
                   (let ((%push-new-stack (cons c s)))
                     (begin (set! s %push-new-stack) s))
                   (begin (set! escape-p false) escape-p))
                  ((esc-char-p c)
                   (if *expand-escape-p*
                       (let ((s1
                              (begin (toss-back-char c)
                               (let ((x
                                      (let ((%fluid-var-*not-processing-p*
                                             true))
                                        (fluid-let
                                         ((*not-processing-p*
                                           %fluid-var-*not-processing-p*))
                                         (get-ctl-seq)))))
                                 (cond
                                  ((member x '("\\ " "\\{" "\\}"))
                                   (let ((%type 'string)
                                         (%ee (list (list (string-ref x 1)))))
                                     (let ((%res
                                            (if (eq? %type 'string)
                                                ""
                                                null)))
                                       (let %concatenate-loop
                                         ((%ee %ee))
                                         (if (null? %ee)
                                             %res
                                             (let ((%a (car %ee)))
                                               (unless (not %a)
                                                 (set! %res
                                                  (if (eq? %type 'string)
                                                      (string-append %res
                                                       (if (string? %a)
                                                           %a
                                                           (list->string %a)))
                                                      (append %res
                                                              (if (string? %a)
                                                                  (string->list
                                                                   %a)
                                                                  %a)))))
                                               (%concatenate-loop (cdr %ee)))))
                                       %res)))
                                  (true (tex-string-to-html-string x)))))))
                         (begin
                          (set! s
                           (append
                            (reverse
                             (let ((%type 'list) (%ee (list s1)))
                               (let ((%res
                                      (if (eq? %type 'string)
                                          ""
                                          null)))
                                 (let %concatenate-loop
                                   ((%ee %ee))
                                   (if (null? %ee)
                                       %res
                                       (let ((%a (car %ee)))
                                         (unless (not %a)
                                           (set! %res
                                            (if (eq? %type 'string)
                                                (string-append %res
                                                 (if (string? %a)
                                                     %a
                                                     (list->string %a)))
                                                (append %res
                                                        (if (string? %a)
                                                            (string->list %a)
                                                            %a)))))
                                         (%concatenate-loop (cdr %ee)))))
                                 %res)))
                            s))
                          (set! escape-p false) escape-p))
                       (begin
                        (let ((%push-new-stack (cons c s)))
                          (begin (set! s %push-new-stack) s))
                        (begin (set! escape-p true) escape-p))))
                  ((char=? c #\{)
                   (let ((%push-new-stack (cons c s)))
                     (begin (set! s %push-new-stack) s))
                   (let ((%tmp (+ nesting 1)))
                     (begin (set! nesting %tmp) nesting))
                   (begin (set! escape-p false) escape-p))
                  ((char=? c #\})
                   (let ((%push-new-stack (cons c s)))
                     (begin (set! s %push-new-stack) s))
                   (if (= nesting 0)
                       (return s)
                       (begin
                        (let ((%tmp (- nesting 1)))
                          (begin (set! nesting %tmp) nesting))
                        (begin (set! escape-p false) escape-p))))
                  (true
                   (let ((%push-new-stack (cons c s)))
                     (begin (set! s %push-new-stack) s))
                   (begin (set! escape-p false) escape-p))))))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define get-group
 (lambda ()
   (let ((%type 'string) (%ee (list (reverse (get-group-as-reversed-chars)))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define get-peeled-group (lambda () (string-trim (ungroup (get-group)))))

(define get-token-or-peeled-group
 (lambda () (string-trim (ungroup (get-token)))))

(define get-grouped-environment-name-if-any
 (lambda ()
   (let ((c (snoop-actual-char)))
     (if (or (eq? c ':eof-object) (not (char=? c #\{)))
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
                (unless %loop-returned
                  (let ((c (snoop-actual-char)))
                    (cond
                     ((or (char-alphabetic? c) (char=? c #\*))
                      (get-actual-char)
                      (let ((%push-new-stack (cons c s)))
                        (begin (set! s %push-new-stack) s)))
                     ((and (pair? s) (char=? c #\})) (get-actual-char)
                      (return
                       (let ((%type 'string) (%ee (list (reverse s))))
                         (let ((%res
                                (if (eq? %type 'string)
                                    ""
                                    null)))
                           (let %concatenate-loop
                             ((%ee %ee))
                             (if (null? %ee)
                                 %res
                                 (let ((%a (car %ee)))
                                   (unless (not %a)
                                     (set! %res
                                      (if (eq? %type 'string)
                                          (string-append %res
                                           (if (string? %a)
                                               %a
                                               (list->string %a)))
                                          (append %res
                                                  (if (string? %a)
                                                      (string->list %a)
                                                      %a)))))
                                   (%concatenate-loop (cdr %ee)))))
                           %res))))
                     (true (for-each toss-back-char s) (toss-back-char #\{)
                      (return false)))))
                (if %loop-returned
                    %loop-result
                    (%loop))))))))))

(define get-bracketed-text-if-any
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (if (or (eq? c ':eof-object) (not (char=? c #\[)))
         false
         (begin (get-actual-char)
          (let ((%type 'string)
                (%ee
                 (list
                  (reverse
                   (let ((s null) (nesting 0) (escape-p false))
                     (let* ((%loop-returned false)
                            (%loop-result 0)
                            (return
                             (lambda %args
                               (set! %loop-returned true)
                               (set! %loop-result
                                (and (pair? %args) (car %args))))))
                       (let %loop
                         ()
                         (unless %loop-returned
                           (let ((c (get-actual-char)))
                             (begin
                              (cond
                               ((eq? c ':eof-object)
                                (terror 'get-bracketed-text-if-any
                                 "Runaway argument?"))
                               (else false))
                              (cond
                               (escape-p
                                (let ((%push-new-stack (cons c s)))
                                  (begin (set! s %push-new-stack) s))
                                (begin (set! escape-p false) escape-p))
                               ((esc-char-p c)
                                (let ((%push-new-stack (cons c s)))
                                  (begin (set! s %push-new-stack) s))
                                (begin (set! escape-p true) escape-p))
                               ((char=? c #\{)
                                (let ((%push-new-stack (cons c s)))
                                  (begin (set! s %push-new-stack) s))
                                (let ((%tmp (+ nesting 1)))
                                  (begin (set! nesting %tmp) nesting)))
                               ((char=? c #\})
                                (let ((%push-new-stack (cons c s)))
                                  (begin (set! s %push-new-stack) s))
                                (let ((%tmp (- nesting 1)))
                                  (begin (set! nesting %tmp) nesting)))
                               ((char=? c #\])
                                (if (= nesting 0)
                                    (return s)
                                    (let ((%push-new-stack (cons c s)))
                                      (begin (set! s %push-new-stack) s))))
                               (true
                                (let ((%push-new-stack (cons c s)))
                                  (begin (set! s %push-new-stack) s)))))))
                         (if %loop-returned
                             %loop-result
                             (%loop)))))))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))))))

(define ungroup
 (lambda (s)
   (let ((n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (let ((n-1 (- n 1)))
       (if (and (>= n 2) (char=? (string-ref s 0) #\{)
                (char=? (string-ref s n-1) #\}))
           (subseq s 1 n-1)
           s)))))

(define eat-alphanumeric-string
 (lambda ()
   (ignorespaces)
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
             (unless %loop-returned (begin (set! c (snoop-actual-char)) c))
             (unless %loop-returned
               (cond
                ((not
                  (or (char-alphabetic? c) (char-numeric? c)
                      (member c '(#\:))))
                 (return))
                (else false)))
             (unless %loop-returned (get-actual-char))
             (if %loop-returned
                 %loop-result
                 (%loop))))))))

(define get-filename
 (lambda %lambda-rest-arg
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
     (let ((%type 'string)
           (%ee
            (list
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
                    (unless %loop-returned
                      (let ((c (snoop-actual-char)))
                        (cond ((eq? c ':eof-object) (return s))
                              ((and (not bracedp)
                                    (or (char-whitespace? c)
                                        (and *comment-char*
                                             (char=? c *comment-char*))
                                        (member c *filename-delims*)))
                               (cond ((not *not-processing-p*) (ignorespaces))
                                     (else false))
                               (return s))
                              ((and bracedp (char=? c #\})) (get-actual-char)
                               (return s))
                              ((esc-char-p c)
                               (let ((x (get-ctl-seq)))
                                 (if (string=? x "\\jobname")
                                     (begin
                                      (set! s
                                       (append
                                        (reverse
                                         (let ((%type 'list)
                                               (%ee (list *jobname*)))
                                           (let ((%res
                                                  (if (eq? %type 'string)
                                                      ""
                                                      null)))
                                             (let %concatenate-loop
                                               ((%ee %ee))
                                               (if (null? %ee)
                                                   %res
                                                   (let ((%a (car %ee)))
                                                     (unless (not %a)
                                                       (set! %res
                                                        (if (eq? %type 'string)
                                                            (string-append %res
                                                             (if (string? %a)
                                                                 %a
                                                                 (list->string
                                                                  %a)))
                                                            (append %res
                                                                    (if (string?
                                                                         %a)
                                                                        (string->list
                                                                         %a)
                                                                        %a)))))
                                                     (%concatenate-loop
                                                      (cdr %ee)))))
                                             %res)))
                                        s))
                                      s)
                                     (begin (toss-back-char *invisible-space*)
                                      (toss-back-string x) (return s)))))
                              (true (get-actual-char)
                               (let ((%push-new-stack (cons c s)))
                                 (begin (set! s %push-new-stack) s))))))
                    (if %loop-returned
                        %loop-result
                        (%loop)))))))))
       (let ((%res
              (if (eq? %type 'string)
                  ""
                  null)))
         (let %concatenate-loop
           ((%ee %ee))
           (if (null? %ee)
               %res
               (let ((%a (car %ee)))
                 (unless (not %a)
                   (set! %res
                    (if (eq? %type 'string)
                        (string-append %res
                         (if (string? %a)
                             %a
                             (list->string %a)))
                        (append %res
                                (if (string? %a)
                                    (string->list %a)
                                    %a)))))
                 (%concatenate-loop (cdr %ee)))))
         %res)))))

(define get-filename-possibly-braced
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (get-filename (and (char? c) (char=? c #\{))))))

(define get-word
 (lambda ()
   (ignorespaces)
   (let ((%type 'string)
         (%ee
          (list
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
                  (unless %loop-returned
                    (let ((c (snoop-actual-char)))
                      (cond ((eq? c ':eof-object) (return s))
                            ((or (char-whitespace? c)
                                 (and *comment-char* (char=? c *comment-char*))
                                 (esc-char-p c))
                             (return s))
                            (true (get-actual-char)
                             (let ((%push-new-stack (cons c s)))
                               (begin (set! s %push-new-stack) s))))))
                  (if %loop-returned
                      %loop-result
                      (%loop)))))))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define get-integer
 (lambda (base)
   false
   (ignorespaces)
   (let ((s null) (c false))
     (begin
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned (begin (set! c (snoop-actual-char)) c))
          (unless %loop-returned
            (cond ((eq? c ':eof-object) (return)) (else false)))
          (unless %loop-returned
            (cond
             ((not
               (or (char-numeric? c) (and (= base 16) (char-alphabetic? c))))
              (ignorespaces) (return))
             (else false)))
          (unless %loop-returned (get-actual-char))
          (unless %loop-returned
            (let ((%push-new-stack (cons c s)))
              (begin (set! s %push-new-stack) s)))
          (if %loop-returned
              %loop-result
              (%loop))))
      (string->number
       (let ((%type 'string) (%ee (list (reverse s))))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))
       base)))))

(define get-real
 (lambda ()
   (ignorespaces)
   (let ((negative? false) (c (snoop-actual-char)))
     (begin
      (cond ((char=? c #\-) (begin (set! negative? true) negative?))
            (else false))
      (cond ((or negative? (char=? c #\+)) (get-actual-char)) (else false))
      (let ((s null))
        (begin
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (cond ((eq? c ':eof-object) (return))
                       ((or (char-numeric? c) (char=? c #\.)) (get-actual-char)
                        (let ((%push-new-stack (cons c s)))
                          (begin (set! s %push-new-stack) s)))
                       (true (ignorespaces) (return)))))
             (if %loop-returned
                 %loop-result
                 (%loop))))
         (if s
             (let ((n
                    (string->number
                     (let ((%type 'string) (%ee (list (reverse s))))
                       (let ((%res
                              (if (eq? %type 'string)
                                  ""
                                  null)))
                         (let %concatenate-loop
                           ((%ee %ee))
                           (if (null? %ee)
                               %res
                               (let ((%a (car %ee)))
                                 (unless (not %a)
                                   (set! %res
                                    (if (eq? %type 'string)
                                        (string-append %res
                                         (if (string? %a)
                                             %a
                                             (list->string %a)))
                                        (append %res
                                                (if (string? %a)
                                                    (string->list %a)
                                                    %a)))))
                                 (%concatenate-loop (cdr %ee)))))
                         %res)))))
               (if negative?
                   (- n)
                   n))
             false)))))))

(define get-equal-sign
 (lambda ()
   (ignorespaces)
   (cond ((char=? (snoop-actual-char) #\=) (get-actual-char)) (else false))))

(define get-by
 (lambda ()
   (ignorespaces)
   (cond
    ((char=? (snoop-actual-char) #\b) (get-actual-char)
     (if (char=? (snoop-actual-char) #\y)
         (get-actual-char)
         (toss-back-char #\b)))
    (else false))))

(define get-to
 (lambda ()
   (ignorespaces)
   (cond
    ((char=? (snoop-actual-char) #\t) (get-actual-char)
     (cond ((char=? (snoop-actual-char) #\o) (get-actual-char) (ignorespaces))
           (true (toss-back-char #\t))))
    (else false))))

(define get-number-corresp-to-ctl-seq
 (lambda (x)
   (cond ((string=? x "\\the") (get-number-corresp-to-ctl-seq (get-ctl-seq)))
         ((string=? x "\\active") 13)
         ((string=? x "\\pageno") *html-page-count*)
         ((string=? x "\\inputlineno") *input-line-no*)
         ((string=? x "\\footnotenumber") (get-gcount "\\footnotenumber"))
         ((string=? x "\\figurenumber")
          (counter*-value (table-get "figure" *dotted-counters*)))
         ((string=? x "\\sectiondnumber")
          (table-get (string->number (ungroup (get-token))) *section-counters*
           0))
         ((string=? x "\\magstep") (get-number-or-false)) ((find-count x))
         ((find-dimen x))
         (true
          (let ((it (resolve-defs x)))
            (if it
                (char->integer (string-ref it 0))
                (string->number x)))))))

(define get-number-or-false
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond ((esc-char-p c) (get-number-corresp-to-ctl-seq (get-ctl-seq)))
           ((char=? c #\') (get-actual-char) (get-integer 8))
           ((char=? c #\") (get-actual-char) (get-integer 16))
           ((char=? c #\`) (get-actual-char) (ignorespaces)
            (char->integer
             (if (esc-char-p (snoop-actual-char))
                 (string-ref (get-ctl-seq) 1)
                 (get-actual-char))))
           ((char=? c #\+) (get-actual-char) (get-number-or-false))
           ((char=? c #\-) (get-actual-char)
            (let ((n (get-number-or-false)))
              (and n (- n))))
           ((char-numeric? c) (get-integer 10)) (true false)))))

(define get-number
 (lambda () (or (get-number-or-false) (terror 'get-number "Missing number."))))

(define get-tex-char-spec
 (lambda ()
   (let ((n (get-number-or-false)))
     (if n
         (integer->char n)
         (terror 'get-tex-char-spec "not a char")))))

(define get-url
 (lambda ()
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin
      (cond
       ((or (eq? c ':eof-object) (not (char=? c #\{)))
        (terror 'get-url "Missing {"))
       (else false))
      (string-trim
       (let ((%type 'string)
             (%ee
              (list
               (reverse
                (let ((nesting 0) (s null))
                  (let* ((%loop-returned false)
                         (%loop-result 0)
                         (return
                          (lambda %args
                            (set! %loop-returned true)
                            (set! %loop-result
                             (and (pair? %args) (car %args))))))
                    (let %loop
                      ()
                      (unless %loop-returned
                        (let ((c (get-actual-char)))
                          (begin
                           (cond
                            ((eq? c ':eof-object)
                             (terror 'get-url "Missing }"))
                            (else false))
                           (cond
                            ((and *comment-char* (char=? c *comment-char*))
                             (let ((c1 (snoop-actual-char)))
                               (if (and (char? c1) (char-whitespace? c1))
                                   (ignore-all-whitespace)
                                   (let ((%push-new-stack (cons c s)))
                                     (begin (set! s %push-new-stack) s)))))
                            ((char=? c #\{)
                             (let ((%tmp (+ nesting 1)))
                               (begin (set! nesting %tmp) nesting))
                             (let ((%push-new-stack (cons c s)))
                               (begin (set! s %push-new-stack) s)))
                            ((char=? c #\})
                             (cond ((= nesting 0) (return s))
                                   (true
                                    (let ((%tmp (- nesting 1)))
                                      (begin (set! nesting %tmp) nesting))
                                    (let ((%push-new-stack (cons c s)))
                                      (begin (set! s %push-new-stack) s)))))
                            (true
                             (let ((%push-new-stack (cons c s)))
                               (begin (set! s %push-new-stack) s)))))))
                      (if %loop-returned
                          %loop-result
                          (%loop)))))))))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))))))

(define get-csv
 (lambda (closing-delim)
   (ignorespaces)
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
                (unless %loop-returned
                  (let ((c (get-actual-char)))
                    (begin
                     (cond
                      ((eq? c ':eof-object)
                       (terror 'get-csv "Runaway argument of \\cite, "
                        "\\nocite, \\expandhtmlindex?"))
                      (else false))
                     (cond ((char=? c #\,) (return s))
                           ((char=? c closing-delim) (toss-back-char c)
                            (return s))
                           (true
                            (let ((%push-new-stack (cons c s)))
                              (begin (set! s %push-new-stack) s)))))))
                (if %loop-returned
                    %loop-result
                    (%loop)))))))
     (cond
      ((not (null? rev-lbl))
       (let ((%type 'string) (%ee (list (reverse rev-lbl))))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (else false)))))

(define get-raw-token
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) c)
           ((esc-char-p c)
            (let ((%fluid-var-*not-processing-p* true))
              (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
               (get-ctl-seq))))
           (true
            (let ((%type 'string) (%ee (list (list (get-actual-char)))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))))

(define get-raw-token/is
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) c) ((esc-char-p c) (get-ctl-seq))
           ((and *comment-char* (char=? c *comment-char*)) (eat-till-eol)
            (get-raw-token/is))
           (true
            (let ((%type 'string) (%ee (list (list (get-actual-char)))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))))

(define get-token
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) c) ((esc-char-p c) (get-ctl-seq))
           ((char=? c #\{) (get-group))
           ((and *comment-char* (char=? c *comment-char*)) (eat-till-eol)
            (get-token))
           (true
            (let ((%type 'string) (%ee (list (list (get-actual-char)))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))))

(define get-token/ps
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) c) ((esc-char-p c) (get-ctl-seq))
           ((char=? c #\{) (get-group))
           ((and *comment-char* (char=? c *comment-char*)) (eat-till-eol)
            (get-token/ps))
           (true
            (let ((%type 'string) (%ee (list (list (get-actual-char)))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))))

(define eat-word
 (lambda (word)
   (ignorespaces)
   (let ((r null))
     (let ((%dotimes-n
            (let ((%length-arg word))
              ((if (string? %length-arg)
                   string-length
                   length)
               %length-arg)))
           (i 0))
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (unless %loop-returned
             (cond ((>= i %dotimes-n) (return true)) (else false)))
           (unless %loop-returned
             (let ((c (snoop-actual-char)))
               (cond
                ((char=? c (string-ref word i)) (get-actual-char)
                 (let ((%push-new-stack (cons c r)))
                   (begin (set! r %push-new-stack) r)))
                (true (for-each toss-back-char r) (return false)))))
           (unless %loop-returned (set! i (+ i 1)))
           (if %loop-returned
               %loop-result
               (%loop))))))))

(define eat-skip-fluff
 (lambda (fullp)
   (let ((go-ahead-p true))
     (begin
      (cond (fullp (get-equal-sign)) ((ormap eat-word '("plus" "minus")) true)
            (true (begin (set! go-ahead-p false) go-ahead-p)))
      (cond
       (go-ahead-p
        (let ((%fluid-var-*not-processing-p* true))
          (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
           (let ((firstp fullp))
             (let* ((%loop-returned false)
                    (%loop-result 0)
                    (return
                     (lambda %args
                       (set! %loop-returned true)
                       (set! %loop-result (and (pair? %args) (car %args))))))
               (let %loop
                 ()
                 (unless %loop-returned (ignorespaces))
                 (unless %loop-returned
                   (let ((c (snoop-actual-char)))
                     (cond ((eq? c ':eof-object) (return))
                           ((and (esc-char-p c) firstp) (get-ctl-seq) (return))
                           ((or (char-numeric? c) (char=? c #\.)) (get-real))
                           ((or (char=? c #\') (char=? c #\")) (get-number))
                           ((ormap eat-word '("+" "-")) true)
                           ((ormap eat-word
                             '("bp" "cc" "cm" "dd" "em" "ex" "filll" "fill"
                               "fil" "in" "minus" "mm" "pc" "plus" "pt" "sp"
                               "true"))
                            (begin (set! firstp false) firstp))
                           (true (return)))))
                 (if %loop-returned
                     %loop-result
                     (%loop))))))))
       (else false))))))

(define eat-dimen (lambda () (eat-skip-fluff true)))

(define eat-integer
 (lambda ()
   (let ((%fluid-var-*not-processing-p* true))
     (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
      (ignorespaces) (get-equal-sign) (get-number)))))

(define scm-get-token
 (lambda ()
   (let ((%type 'string)
         (%ee
          (list
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
                  (unless %loop-returned
                    (begin (set! c (snoop-actual-char)) c))
                  (unless %loop-returned
                    (cond ((eq? c ':eof-object) (return s)) (else false)))
                  (unless %loop-returned
                    (cond
                     (esc-p (get-actual-char)
                      (let ((%push-new-stack (cons c s)))
                        (begin (set! s %push-new-stack) s))
                      (begin (set! esc-p false) esc-p))
                     ((char=? c #\\) (get-actual-char)
                      (let ((%push-new-stack (cons c s)))
                        (begin (set! s %push-new-stack) s))
                      (begin (set! esc-p true) esc-p))
                     ((or (char-whitespace? c) (member c *scm-token-delims*))
                      (return s))
                     (true (get-actual-char)
                      (let ((%push-new-stack (cons c s)))
                        (begin (set! s %push-new-stack) s)))))
                  (if %loop-returned
                      %loop-result
                      (%loop)))))))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define emit-html-char
 (lambda (c)
   (cond
    ((not (eq? c ':eof-object))
     (cond ((char=? c #\newline) (emit-newline))
           (*outputting-to-non-html-p* (emit c))
           (true
            (emit
             (case c
               ((#\<) "&#x3c;")
               ((#\>) "&#x3e;")
               ((#\") "&#x22;")
               ((#\&) "&#x26;")
               (else c))))))
    (else false))))

(define emit-html-string
 (lambda (s)
   (let ((%dotimes-n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg)))
         (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((>= i %dotimes-n) (return)) (else false)))
         (unless %loop-returned (emit-html-char (string-ref s i)))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(defstruct texframe*
  (definitions (make-table ':test equal?))
  (chardefinitions (make-table))
  (counts (make-table ':test equal?))
  (toks (make-table ':test equal?))
  (dimens (make-table ':test equal?))
  (postludes null)
  (uccodes (make-table ':test eqv?))
  (lccodes (make-table ':test eqv?))
  (aftergroups null))

(define *primitive-texframe* (make-texframe*))

(define *math-primitive-texframe* (make-texframe*))

(define bgroup
 (lambda ()
   (let ((%push-new-stack (cons (make-texframe*) *tex-env*)))
     (begin (set! *tex-env* %push-new-stack) *tex-env*))
   (cond
    ((and *in-display-math-p* (not *math-script-mode-p*)) (bgroup-math-hook))
    (else false))))

(define bgroup-math-hook
 (lambda ()
   (let ((old-html *html*)
         (old-math-delim-left *math-delim-left*)
         (old-math-delim-right *math-delim-right*)
         (old-math-height *math-height*))
     (begin
      (begin (set! *html* (open-output-string)) (set! *math-delim-left* false)
       (set! *math-delim-right* false) (set! *math-height* 0) *math-height*)
      (let ((%push-new-stack (cons ':mathbox *tabular-stack*)))
        (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
      (add-postlude-to-top-frame
       (lambda ()
         (let ((res (get-output-string *html*)))
           (begin
            (begin
             (set! res
              (let ((%type 'string)
                    (%ee
                     (list "<table><tr><td align=center>" res
                           "</td></tr></table>")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res)))
             res)
            (cond
             ((or *math-delim-left* *math-delim-right*)
              (cond
               ((and
                 (or (member *math-delim-left* '(:lbrace :rbrace))
                     (member *math-delim-right* '(:lbrace :rbrace)))
                 (even? *math-height*))
                (let ((%tmp (+ *math-height* 1)))
                  (begin (set! *math-height* %tmp) *math-height*)))
               (else false))
              (begin
               (set! res
                (let ((%type 'string)
                      (%ee
                       (list "<table><tr><td>"
                             (tex-math-delim-string *math-delim-left*)
                             "</td><td>" res "</td><td>"
                             (tex-math-delim-string *math-delim-right*)
                             "</td></tr></table>")))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res)))
               res))
             (else false))
            (begin (set! *html* old-html)
             (set! *math-delim-left* old-math-delim-left)
             (set! *math-delim-right* old-math-delim-right)
             (set! *math-height* (+ old-math-height *math-height*))
             *math-height*)
            (pop-tabular-stack ':mathbox) (emit "</td><td>") (emit res)
            (emit "</td><td>")))))))))

(define do-math-left
 (lambda ()
   (ignorespaces)
   (cond
    ((and *in-display-math-p* (not *math-script-mode-p*))
     (let ((s (get-token)))
       (begin (bgroup)
        (cond
         ((string=? s "(")
          (begin (set! *math-delim-left* ':lparen) *math-delim-left*))
         ((string=? s "[")
          (begin (set! *math-delim-left* ':lbrack) *math-delim-left*))
         ((string=? s "\\{")
          (begin (set! *math-delim-left* ':lbrace) *math-delim-left*))
         ((string=? s "|")
          (begin (set! *math-delim-left* ':lvert) *math-delim-left*))
         (true (terror 'do-math-left))))))
    (else false))))

(define do-math-right
 (lambda ()
   (ignorespaces)
   (cond
    ((and *in-display-math-p* (not *math-script-mode-p*))
     (let ((s (get-token)))
       (begin
        (cond
         ((string=? s ")")
          (begin (set! *math-delim-right* ':rparen) *math-delim-right*))
         ((string=? s "]")
          (begin (set! *math-delim-right* ':rbrack) *math-delim-right*))
         ((string=? s "\\}")
          (begin (set! *math-delim-right* ':rbrace) *math-delim-right*))
         ((string=? s "|")
          (begin (set! *math-delim-right* ':rvert) *math-delim-right*))
         (true (terror 'do-math-right)))
        (egroup))))
    (else false))))

(define egroup
 (lambda ()
   (cond ((null? *tex-env*) (terror 'egroup "Too many }'s")) (else false))
   (perform-postludes)
   (perform-aftergroups)
   (let* ((%pop-old-stack *tex-env*) (%pop-top-value (car %pop-old-stack)))
     (begin (set! *tex-env* (cdr %pop-old-stack)) *tex-env*)
     %pop-top-value)))

(define perform-postludes
 (lambda () (for-each (lambda (p) (p)) (texframe*-postludes (top-texframe)))))

(define perform-aftergroups
 (lambda ()
   (let ((ags (texframe*-aftergroups (top-texframe))))
     (begin
      (cond ((not (null? ags)) (toss-back-char *invisible-space*))
            (else false))
      (for-each (lambda (ag) (ag)) ags)))))

(define perform-afterassignment
 (lambda ()
   (let ((z *afterassignment*))
     (cond
      (z (begin (set! *afterassignment* false) *afterassignment*)
       (do-tex-ctl-seq z))
      (else false)))))

(define add-postlude-to-top-frame
 (lambda (p)
   (let ((fr
          (if (null? *tex-env*)
              *global-texframe*
              (car *tex-env*))))
     (let ((%push-new-stack (cons p (texframe*-postludes fr))))
       (begin (set!texframe*-postludes fr %push-new-stack)
        (texframe*-postludes fr))))))

(define add-aftergroup-to-top-frame
 (lambda (ag)
   (let ((fr
          (if (null? *tex-env*)
              *global-texframe*
              (car *tex-env*))))
     (let ((%push-new-stack (cons ag (texframe*-aftergroups fr))))
       (begin (set!texframe*-aftergroups fr %push-new-stack)
        (texframe*-aftergroups fr))))))

(define top-texframe
 (lambda ()
   (if (null? *tex-env*)
       *global-texframe*
       (car *tex-env*))))

(defstruct tdef*
  (argpat null)
  (expansion "")
  (optarg false)
  (thunk false)
  (prim false)
  (defer false))

(defstruct cdef* (argpat false) (expansion false) (optarg false) (active false))

(define kopy-tdef
 (lambda (lft rt)
   (begin (set!tdef*-argpat lft (tdef*-argpat rt)) (tdef*-argpat lft))
   (begin (set!tdef*-expansion lft (tdef*-expansion rt)) (tdef*-expansion lft))
   (begin (set!tdef*-optarg lft (tdef*-optarg rt)) (tdef*-optarg lft))
   (begin (set!tdef*-thunk lft (tdef*-thunk rt)) (tdef*-thunk lft))
   (begin (set!tdef*-prim lft (tdef*-prim rt)) (tdef*-prim lft))
   (begin (set!tdef*-defer lft (tdef*-defer rt)) (tdef*-defer lft))))

(define kopy-cdef
 (lambda (lft rt)
   (begin (set!cdef*-argpat lft (cdef*-argpat rt)) (cdef*-argpat lft))
   (begin (set!cdef*-expansion lft (cdef*-expansion rt)) (cdef*-expansion lft))
   (begin (set!cdef*-optarg lft (cdef*-optarg rt)) (cdef*-optarg lft))
   (begin (set!cdef*-active lft (cdef*-active rt)) (cdef*-active lft))))

(define cleanse-tdef
 (lambda (d)
   (begin (set!tdef*-argpat d null) (tdef*-argpat d))
   (begin (set!tdef*-expansion d "") (tdef*-expansion d))
   (begin (set!tdef*-optarg d false) (tdef*-optarg d))
   (begin (set!tdef*-thunk d false) (tdef*-thunk d))
   (begin (set!tdef*-prim d false) (tdef*-prim d))
   (begin (set!tdef*-defer d false) (tdef*-defer d))))

(define tex-def
 (lambda (name argpat expansion optarg thunk prim defer frame)
   (cond ((not frame) (begin (set! frame (top-texframe)) frame)) (else false))
   (let ((frame-defs (texframe*-definitions frame)))
     (let ((d
            (or (table-get name frame-defs)
                (let ((d (make-tdef*)))
                  (begin
                   (begin (table-put! name frame-defs d)
                    (table-get name frame-defs))
                   d)))))
       (begin (set!tdef*-argpat d argpat) (set!tdef*-expansion d expansion)
        (set!tdef*-optarg d optarg) (set!tdef*-thunk d thunk)
        (set!tdef*-prim d prim) (set!tdef*-defer d defer) (tdef*-defer d))))
   (perform-afterassignment)))

(define tex-def-prim
 (lambda (prim thunk)
   (tex-def prim null false false thunk prim false *primitive-texframe*)))

(define tex-defsym-prim
 (lambda (prim str)
   (tex-def prim null false false (lambda () (emit str)) prim false
    *primitive-texframe*)))

(define tex-def-0arg
 (lambda (cs expn) (tex-def cs null expn false false false false false)))

(define ctl-seq-no-arg-expand-once
 (lambda (cs)
   (let ((d (find-def cs)))
     (and d (tdef*-expansion d)))))

(define tex-gdef-0arg
 (lambda (cs expn)
   (tex-def cs null expn false false cs false *global-texframe*)))

(define tex-def-prim-0arg
 (lambda (cs expn)
   (tex-def cs null expn false false cs false *primitive-texframe*)))

(define get-0arg-expn
 (lambda (cs)
   (let ((d (find-def cs)))
     (if d
         (tdef*-expansion d)
         "0"))))

(define tex2page-flag-value (lambda (cs) (string-ref (get-0arg-expn cs) 0)))

(define tex2page-flag-boolean
 (lambda (cs)
   (not (member (string-ref (get-0arg-expn cs) 0) '(#\0 #\f #\F #\n #\N)))))

(define tex-let
 (lambda (lft rt frame)
   (cond ((not frame) (begin (set! frame (top-texframe)) frame)) (else false))
   (let ((frame-defs (texframe*-definitions frame)))
     (let ((lft-def
            (or (table-get lft frame-defs)
                (let ((lft-def (make-tdef*)))
                  (begin
                   (begin (table-put! lft frame-defs lft-def)
                    (table-get lft frame-defs))
                   lft-def)))))
       (cond
        ((begin (set! *it* (find-def rt)) *it*)
         (let ((rt-def *it*))
           (kopy-tdef lft-def rt-def)))
        (true (cleanse-tdef lft-def)))))))

(define tex-let-general
 (lambda (lhs rhs frame)
   (if (ctl-seq-p rhs)
       (tex-let lhs rhs frame)
       (tex-def lhs null rhs false false false false frame))))

(define tex-let-prim (lambda (lft rt) (tex-let lft rt *primitive-texframe*)))

(define tex-def-thunk
 (lambda (name thunk frame)
   (cond
    ((not (inside-false-world-p))
     (tex-def name null false false thunk name false frame))
    (else false))))

(define tex-def-count
 (lambda (name num globalp)
   (let ((frame
          (if globalp
              *global-texframe*
              (top-texframe))))
     (begin (table-put! name (texframe*-counts frame) num)
      (table-get name (texframe*-counts frame))))
   (perform-afterassignment)))

(define tex-def-toks
 (lambda (name tokens globalp)
   (let ((frame
          (if globalp
              *global-texframe*
              (top-texframe))))
     (begin (table-put! name (texframe*-toks frame) tokens)
      (table-get name (texframe*-toks frame))))))

(define tex-def-dimen
 (lambda (name len globalp)
   (let ((frame
          (if globalp
              *global-texframe*
              (top-texframe))))
     (begin
      (begin (table-put! name (texframe*-dimens frame) len)
       (table-get name (texframe*-dimens frame)))
      (perform-afterassignment)))))

(define tex-def-char
 (lambda (c argpat expansion frame)
   (cond ((not frame) (begin (set! frame (top-texframe)) frame)) (else false))
   (let ((d (ensure-cdef c frame)))
     (begin (set!cdef*-argpat d argpat) (set!cdef*-expansion d expansion)
      (cdef*-expansion d)))
   (perform-afterassignment)))

(define ensure-cdef
 (lambda (c f)
   (let ((f-chardefs (texframe*-chardefinitions f)))
     (or (table-get c f-chardefs)
         (let ((d (make-cdef*)))
           (begin (begin (table-put! c f-chardefs d) (table-get c f-chardefs))
            d))))))

(define find-chardef
 (lambda (c)
   (let ((x
          (or
           (ormap (lambda (f) (table-get c (texframe*-chardefinitions f)))
            *tex-env*)
           (table-get c (texframe*-chardefinitions *global-texframe*))
           (table-get c (texframe*-chardefinitions *primitive-texframe*)))))
     (and x (cdef*-active x) x))))

(define find-chardef-in-top-frame
 (lambda (c)
   (let ((x
          (if (null? *tex-env*)
              (or (table-get c (texframe*-chardefinitions *global-texframe*))
                  (table-get c
                   (texframe*-chardefinitions *primitive-texframe*)))
              (table-get c (texframe*-chardefinitions (car *tex-env*))))))
     (and x (cdef*-active x) x))))

(define do-defcsactive
 (lambda (globalp)
   (let ((cs (get-token)))
     (let ((c
            (string-ref cs
             (if (ctl-seq-p cs)
                 1
                 0))))
       (let ((argpat (begin (ignorespaces) (get-def-arguments c))))
         (let ((rhs (ungroup (get-group))))
           (let ((f (and globalp *global-texframe*)))
             (begin (activate-cdef c) (tex-def-char c argpat rhs f)))))))))

(define activate-cdef
 (lambda (c)
   (let ((y
          (cond
           ((begin (set! *it* (find-chardef-in-top-frame c)) *it*)
            (let ((y *it*))
              (begin (begin (set!cdef*-active y true) (cdef*-active y)) y)))
           (true
            (let ((d (find-chardef c)))
              (let ((y (ensure-cdef c (top-texframe))))
                (begin (cond (d (kopy-cdef y d)) (else false))
                 (begin (set!cdef*-active y true) (cdef*-active y)) y)))))))
     (add-postlude-to-top-frame
      (lambda () (begin (set!cdef*-active y false) (cdef*-active y)))))))

(define deactivate-cdef
 (lambda (c)
   (cond
    ((begin (set! *it* (find-chardef-in-top-frame c)) *it*)
     (let ((y *it*))
       (begin (set!cdef*-active y false) (cdef*-active y))))
    ((begin (set! *it* (find-chardef c)) *it*)
     (let ((y *it*))
       (let ((d (ensure-cdef c (top-texframe))))
         (begin (kopy-cdef d y)
          (begin (set!cdef*-active d false) (cdef*-active d)))))))))

(define do-undefcsactive
 (lambda () (ignorespaces) (deactivate-cdef (string-ref (get-ctl-seq) 1))))

(define do-catcode
 (lambda ()
   (let ((c (get-tex-char-spec)))
     (let ((val (begin (get-equal-sign) (get-number))))
       (set-catcode c val)))))

(define set-catcode
 (lambda (c val)
   (cond ((not (= val 13)) (deactivate-cdef c)) (else false))
   (cond
    ((not (= val 11))
     (begin (set! *tex-extra-letters* (remove c *tex-extra-letters*))
      *tex-extra-letters*))
    (else false))
   (case val
     ((0)
      (let ((%push-new-stack (cons c *esc-chars*)))
        (begin (set! *esc-chars* %push-new-stack) *esc-chars*)))
     ((11)
      (let ((%push-new-stack (cons c *tex-extra-letters*)))
        (begin (set! *tex-extra-letters* %push-new-stack)
         *tex-extra-letters*)))
     ((13) (activate-cdef c)))))

(define do-opmac-activettchar
 (lambda ()
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin (begin (set! *opmac-active-tt-char* c) *opmac-active-tt-char*)
      (activate-cdef c) (tex-def-char c null "\\TIIPopmacverb" false)))))

(define do-opmac-intext-verb
 (lambda ()
   (bgroup)
   (let ((%fluid-var-*ligatures-p* false))
     (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
      (cond (*outputting-external-title-p* false)
            (true (emit "<code class=verbatim>")))
      (do-verb-delimed *opmac-active-tt-char*)
      (cond (*outputting-external-title-p* false) (true (emit "</code>")))))
   (egroup)))

(define do-global
 (lambda ()
   (ignorespaces)
   (let ((next (get-ctl-seq)))
     (cond ((string=? next "\\def") (do-def true false))
           ((string=? next "\\edef") (do-def true true))
           ((string=? next "\\let") (do-let true))
           ((string=? next "\\newcount") (do-newcount true))
           ((string=? next "\\newtoks") (do-newtoks true))
           ((string=? next "\\newdimen") (do-newdimen true))
           ((string=? next "\\advance") (do-advance true))
           ((string=? next "\\multiply") (do-multiply true))
           ((string=? next "\\divide") (do-divide true))
           ((string=? next "\\read") (do-read true))
           ((or (string=? next "\\imgdef") (string=? next "\\gifdef"))
            (make-reusable-img true))
           ((find-count next) (do-count= next true))
           ((find-toks next) (do-toks= next true))
           (true (toss-back-string next))))))

(define do-externaltitle
 (lambda ()
   (write-aux `(!preferred-title ,(tex-string-to-html-string (get-group))))))

(define make-external-title
 (lambda (title)
   (let ((%fluid-var-*outputting-external-title-p* true))
     (fluid-let
      ((*outputting-external-title-p*
        %fluid-var-*outputting-external-title-p*))
      (bgroup)
      (let ((s
             (tex-string-to-html-string
              (let ((%type 'string)
                    (%ee
                     (list "\\let\\\\\\ignorespaces" "\\def\\resizebox#1#2#3{}"
                           "\\let\\thanks\\TIIPgobblegroup"
                           "\\let\\urlh\\TIIPgobblegroup " title)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res)))))
        (begin (egroup) s))))))

(define output-external-title
 (lambda ()
   (let ((%fluid-var-*outputting-external-title-p* true))
     (fluid-let
      ((*outputting-external-title-p*
        %fluid-var-*outputting-external-title-p*))
      (emit "<title>") (emit-newline) (emit (or *title* *jobname*))
      (emit-newline) (emit "</title>") (emit-newline)))))

(define output-title
 (lambda (title)
   (emit "<h1 class=title>")
   (bgroup)
   (tex2page-string
    (let ((%type 'string) (%ee (list "\\let\\\\\\break " title)))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (egroup)
   (emit "</h1>")
   (emit-newline)))

(define do-subject
 (lambda ()
   (tex-gdef-0arg "\\TIIPtitleused" "1")
   (do-end-para)
   (ignorespaces)
   (let ((title (get-group)))
     (begin
      (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
      (write-aux `(!default-title ,(make-external-title title)))
      (output-title title)))))

(define do-opmac-title
 (lambda ()
   (tex-gdef-0arg "\\TIIPtitleused" "1")
   (do-end-para)
   (let ((title (tex-string-to-html-string (get-till-par))))
     (begin
      (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
      (write-aux `(!default-title ,(make-external-title title)))
      (output-title title)))))

(define do-latex-title
 (lambda ()
   (let ((title (get-group)))
     (begin
      (cond ((not *title*) (flag-missing-piece ':document-title)) (else false))
      (write-aux `(!default-title ,(make-external-title title)))
      (toss-back-string title) (toss-back-string "\\def\\TIIPtitle")))))

(define do-title
 (lambda ()
   (if (eq? *tex-format* ':latex)
       (do-latex-title)
       (do-subject))))

(define do-author (lambda () (toss-back-string "\\def\\TIIPauthor")))

(define do-date (lambda () (toss-back-string "\\def\\TIIPdate")))

(define do-today
 (lambda ()
   (let ((m (get-gcount "\\month")))
     (if (= m 0)
         (emit "[today]")
         (begin (emit (vector-ref *month-names* (- m 1))) (emit " ")
          (emit (get-gcount "\\day")) (emit ", ")
          (emit (get-gcount "\\year")))))))

(define add-afterpar
 (lambda (ap)
   (let ((%push-new-stack (cons ap *afterpar*)))
     (begin (set! *afterpar* %push-new-stack) *afterpar*))))

(define do-end-para
 (lambda ()
   (cond
    (*in-para-p* (cond (*use-closing-p-tag-p* (emit "</p>")) (else false))
     (cond
      ((not (null? *afterpar*))
       (for-each (lambda (ap) (ap)) (reverse *afterpar*))
       (begin (set! *afterpar* null) *afterpar*))
      (else false))
     (emit-newline) (begin (set! *in-para-p* false) *in-para-p*))
    (else false))))

(define do-para
 (lambda ()
   (do-end-para)
   (let ((in-table-p
          (and (not (null? *tabular-stack*))
               (member (car *tabular-stack*) '(:block)))))
     (begin
      (cond (in-table-p (emit "</td></tr><tr><td>") (emit-newline))
            (else false))
      (emit "<p>") (begin (set! *in-para-p* true) *in-para-p*)))))

(define do-noindent
 (lambda ()
   (do-end-para)
   (emit-newline)
   (emit "<p class=noindent>")
   (begin (set! *in-para-p* true) *in-para-p*)))

(define do-indent
 (lambda ()
   (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
     (begin (emit "<span style=\"margin-left: ") (emit parindent)
      (emit "pt\"></span>")))))

(define do-para-nopadding
 (lambda ()
   (do-end-para)
   (emit-newline)
   (emit "<p class=nopadding>")
   (begin (set! *in-para-p* true) *in-para-p*)))

(define do-maketitle
 (lambda ()
   (do-end-para)
   (bgroup)
   (tex2page-string
    (let ((%type 'string)
          (%ee
           (list "\\let\\\\\\break" "\\let\\and\\break"
                 "\\let\\thanks\\symfootnote")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
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
     (begin
      (cond ((null? *stylesheets*) (flag-missing-piece ':stylesheets))
            (else false))
      (write-aux `(!stylesheet ,f))))))

(define do-csname
 (lambda ()
   (ignorespaces)
   (let ((r null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((c (snoop-actual-char)))
             (cond
              ((esc-char-p c)
               (let ((x (get-ctl-seq)))
                 (cond
                  ((string=? x "\\endcsname") (toss-back-char #\})
                   (for-each toss-back-string r) (toss-back-char c)
                   (toss-back-char #\{) (toss-back-string "TIIPcsname")
                   (toss-back-char c) (return))
                  (true
                   (let ((%push-new-stack
                          (cons (expand-ctl-seq-into-string x) r)))
                     (begin (set! r %push-new-stack) r))))))
              (true (get-actual-char)
               (let ((%push-new-stack
                      (cons
                       (let ((%type 'string) (%ee (list (list c))))
                         (let ((%res
                                (if (eq? %type 'string)
                                    ""
                                    null)))
                           (let %concatenate-loop
                             ((%ee %ee))
                             (if (null? %ee)
                                 %res
                                 (let ((%a (car %ee)))
                                   (unless (not %a)
                                     (set! %res
                                      (if (eq? %type 'string)
                                          (string-append %res
                                           (if (string? %a)
                                               %a
                                               (list->string %a)))
                                          (append %res
                                                  (if (string? %a)
                                                      (string->list %a)
                                                      %a)))))
                                   (%concatenate-loop (cdr %ee)))))
                           %res))
                       r)))
                 (begin (set! r %push-new-stack) r))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-saved-csname
 (lambda ()
   (let ((x (get-peeled-group)))
     (do-tex-ctl-seq x))))

(define do-cssblock
 (lambda ()
   (let ((%fluid-var-*dumping-nontex-p* true))
     (fluid-let ((*dumping-nontex-p* %fluid-var-*dumping-nontex-p*))
      (dump-till-end-env "cssblock" *css-port*)))))

(define link-stylesheets
 (lambda ()
   (let ((link-it
          (lambda (css)
            (emit "<link rel=\"stylesheet\" href=\"")
            (emit css)
            (emit "\" />")
            (emit-newline))))
     (begin (for-each link-it *stylesheets*)
      (link-it
       (let ((%type 'string) (%ee (list *jobname* *css-file-suffix*)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))))))

(define link-scripts
 (lambda ()
   (let ((link-it
          (lambda (jsf)
            (emit "<script src=\"")
            (emit jsf)
            (emit "\"></script>")
            (emit-newline))))
     (for-each link-it *scripts*))))

(define increment-section-counter
 (lambda (seclvl nonum-p)
   (cond
    ((not nonum-p)
     (cond
      ((not (table-get seclvl *section-counters*))
       (begin (table-put! seclvl *section-counters* 0)
        (table-get seclvl *section-counters*)))
      (else false))
     (let ((%tmp (+ (table-get seclvl *section-counters*) 1)))
       (begin (table-put! seclvl *section-counters* %tmp)
        (table-get seclvl *section-counters*))))
    (else false))
   (table-for-each
    (lambda (k v)
      false
      (cond
       ((and (> k seclvl) (> k 0))
        (begin (table-put! k *section-counters* 0)
         (table-get k *section-counters*)))
       (else false)))
    *section-counters*)
   (cond ((= seclvl 0) (tex-gdef-count "\\footnotenumber" 0)) (else false))
   (for-each
    (lambda (counter-name)
      (begin (set!counter*-value (table-get counter-name *dotted-counters*) 0)
       (counter*-value (table-get counter-name *dotted-counters*))))
    (table-get seclvl *section-counter-dependencies* null))))

(define section-counter-value
 (lambda (seclvl)
   (if (= seclvl -1)
       (number-to-roman (table-get -1 *section-counters*) true)
       (let ((i
              (if *using-chapters-p*
                  0
                  1)))
         (let ((outermost-secnum
                (let ((n (table-get i *section-counters* 0)))
                  (if *inside-appendix-p*
                      (let ((%type 'string)
                            (%ee
                             (list
                              (list
                               (integer->char (+ (char->integer #\A) -1 n))))))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res))
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
                 (unless %loop-returned
                   (cond ((> i seclvl) (return r)) (else false)))
                 (unless %loop-returned
                   (begin
                    (set! r
                     (let ((%type 'string)
                           (%ee
                            (list r "."
                                  (write-to-string
                                   (table-get i *section-counters* 0)))))
                       (let ((%res
                              (if (eq? %type 'string)
                                  ""
                                  null)))
                         (let %concatenate-loop
                           ((%ee %ee))
                           (if (null? %ee)
                               %res
                               (let ((%a (car %ee)))
                                 (unless (not %a)
                                   (set! %res
                                    (if (eq? %type 'string)
                                        (string-append %res
                                         (if (string? %a)
                                             %a
                                             (list->string %a)))
                                        (append %res
                                                (if (string? %a)
                                                    (string->list %a)
                                                    %a)))))
                                 (%concatenate-loop (cdr %ee)))))
                         %res)))
                    r))
                 (unless %loop-returned
                   (let ((%tmp (+ i 1)))
                     (begin (set! i %tmp) i)))
                 (if %loop-returned
                     %loop-result
                     (%loop))))))))))

(define section-ctl-seq-p
 (lambda (s)
   (cond ((string=? s "\\sectiond") (string->number (ungroup (get-token))))
         ((string=? s "\\part") -1) ((string=? s "\\chapter") 0)
         (true
          (let ((n
                 (let ((%length-arg s))
                   ((if (string? %length-arg)
                        string-length
                        length)
                    %length-arg))))
            (cond ((< n 8) false)
                  ((and (>= n 10) (string=? (subseq s (- n 9) n) "paragraph"))
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
                         (unless %loop-returned
                           (cond ((> i+3 n-9) (return k))
                                 ((string=? (subseq s i i+3) "sub")
                                  (begin (set! i i+3) (set! i+3 (+ i+3 3))
                                   (set! k (add1 k)) k))
                                 (true (return false))))
                         (if %loop-returned
                             %loop-result
                             (%loop))))))
                  ((string=? (subseq s (- n 7) n) "section")
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
                         (unless %loop-returned
                           (cond ((> i+3 n-7) (return k))
                                 ((string=? (subseq s i i+3) "sub")
                                  (begin (set! i i+3) (set! i+3 (+ i+3 3))
                                   (set! k (add1 k)) k))
                                 (true (return false))))
                         (if %loop-returned
                             %loop-result
                             (%loop))))))
                  (true false)))))))

(define do-heading
 (lambda (seclvl)
   (let ((starred-p
          (cond ((char=? (snoop-actual-char) #\*) (get-actual-char) true)
                (true false))))
     (let ((too-deep-p
            (let ((secnumdepth (get-gcount "\\secnumdepth")))
              (cond ((< secnumdepth -1) false) ((> seclvl secnumdepth) true)
                    (true false)))))
       (let ((nonum-p (or starred-p too-deep-p)))
         (let ((header
                (let ((%fluid-var-*tabular-stack* (list ':header)))
                  (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
                   (tex-string-to-html-string (get-group))))))
           (do-heading-help seclvl starred-p nonum-p false false header)))))))

(define do-heading-help
 (lambda (seclvl starred-p nonum-p notoc-p lbl-val header)
   (write-aux `(!default-title ,header))
   (cond
    ((= seclvl 0) (!using-chapters)
     (cond
      ((and (eq? *tex-format* ':latex) (< (get-gcount "\\secnumdepth") -1))
       (tex-gdef-count "\\secnumdepth" 2))
      (else false))
     (cond
      ((or (> *html-page-count* 0) (tex2page-flag-boolean "\\TIIPtitleused"))
       (do-eject))
      (true (tex-gdef-0arg "\\TIIPtitleused" "1") (do-para))))
    (else false))
   (cond ((and (= seclvl 1) (tex2page-flag-boolean "\\TZPslides")) (do-eject))
         (else false))
   (increment-section-counter seclvl nonum-p)
   (cond (lbl-val (begin (set! nonum-p false) nonum-p)) (else false))
   (cond
    ((not lbl-val)
     (begin
      (set! lbl-val
       (if nonum-p
           "IGNORE"
           (section-counter-value seclvl)))
      lbl-val))
    (else false))
   (let ((htmlnum
          (max 1
               (min 6
                    (if *using-chapters-p*
                        (+ seclvl 1)
                        seclvl)))))
     (let ((lbl
            (let ((%type 'string)
                  (%ee
                   (list *html-node-prefix*
                         (case seclvl ((-1) "part") ((0) "chap") (else "sec"))
                         "_"
                         (if nonum-p
                             (gen-temp-string)
                             lbl-val))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin
        (cond
         ((not false) (tex-def-0arg "\\TIIPcurrentnodename" lbl)
          (tex-def-0arg "\\@currentlabel" lbl-val))
         (else false))
        (do-end-para) (emit-anchor lbl) (emit-newline) (ignore-all-whitespace)
        (emit "<h") (emit htmlnum)
        (case seclvl
          ((-1) (emit " class=part align=center"))
          ((0) (emit " class=chapter"))
          (else (emit " class=section")))
        (emit ">")
        (let ((write-to-toc-p
               (and (not notoc-p) *toc-page*
                    (not
                     (and (eqv? *tex-format* ':latex)
                          (string=? header "Contents"))))))
          (begin
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
                        (let ((%type 'string)
                              (%ee (list *html-node-prefix* "toc_" lbl)))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res))))
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
                        (let ((%type 'string)
                              (%ee (list *html-node-prefix* "toc_" lbl)))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res))))
                      (else false))
                     (tex2page-string
                      (if *inside-appendix-p*
                          "\\appendixname"
                          "\\chaptername"))
                     (emit " ") (emit lbl-val)
                     (cond (write-to-toc-p (emit-link-stop)) (else false))))
                (emit "</div><br>")
                (emit-newline))))
            (else false))
           (cond
            (write-to-toc-p
             (emit-page-node-link-start *toc-page*
              (let ((%type 'string) (%ee (list *html-node-prefix* "toc_" lbl)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
            (else false))
           (cond
            ((not (or (and (eqv? *tex-format* ':latex) (<= seclvl 0)) nonum-p))
             (emit lbl-val) (emit-nbsp 2))
            (else false))
           (emit header) (cond (write-to-toc-p (emit-link-stop)) (else false))
           (emit "</h") (emit htmlnum) (emit ">") (do-noindent)
           (let ((tocdepth (get-gcount "\\tocdepth")))
             (cond
              ((and write-to-toc-p
                    (not (and (eq? *tex-format* ':latex) starred-p))
                    (or (< tocdepth -1) (<= seclvl tocdepth)))
               (write-aux
                `(!toc-entry
                  ,(if (= seclvl -1)
                       -1
                       (if *using-chapters-p*
                           seclvl
                           (- seclvl 1)))
                  ,lbl-val ,*html-page-count* ,lbl ,header)))
              (else false)))))
        (cond
         (*recent-node-name* (do-label-aux *recent-node-name*)
          (begin (set! *recent-node-name* false) *recent-node-name*))
         (else false)))))))

(define section-type-to-depth
 (lambda (sectype)
   (cond ((string->number sectype)) ((string=? sectype "chapter") 0)
         ((string=? sectype "section") 1) ((string=? sectype "subsection") 2)
         ((string=? sectype "subsubsection") 3)
         ((string=? sectype "paragraph") 4)
         ((string=? sectype "subparagraph") 5) (true 3))))

(define do-write-to-toc-aux
 (lambda (seclvl secnum sectitle)
   (let ((node-name
          (let ((%type 'string)
                (%ee
                 (list *html-node-prefix* "sec_"
                       (if (string=? secnum "")
                           (gen-temp-string)
                           secnum))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (tex-def-0arg "\\@currentlabel" secnum) (emit-anchor node-name)
      (emit-newline)
      (write-aux
       `(!toc-entry ,seclvl ,secnum ,*html-page-count* ,node-name
         ,sectitle))))))

(define do-writenumberedcontentsline
 (lambda ()
   (let ((toc (get-peeled-group)))
     (begin
      (cond
       ((not (string=? toc "toc"))
        (terror 'do-writenumberedcontentsline "only #1=toc supported"))
       (else false))
      (do-writenumberedtocline)))))

(define do-writenumberedtocline
 (lambda ()
   (let ((seclvl (section-type-to-depth (get-peeled-group))))
     (let ((secnum (tex-string-to-html-string (get-group))))
       (let ((sectitle (tex-string-to-html-string (get-group))))
         (do-write-to-toc-aux seclvl secnum sectitle))))))

(define do-addcontentsline
 (lambda ()
   (let ((toc (get-peeled-group)))
     (begin
      (cond
       ((not (string=? toc "toc"))
        (terror 'do-addcontentsline "only #1=toc supported"))
       (else false))
      (let ((seclvl (section-type-to-depth (get-peeled-group))))
        (let ((sectitle (tex-string-to-html-string (get-group))))
          (write-aux
           `(!toc-entry
             ,(if (= seclvl -1)
                  -1
                  (if *using-chapters-p*
                      seclvl
                      (- seclvl 1)))
             ,(ctl-seq-no-arg-expand-once "\\@currentlabel") ,*html-page-count*
             ,(ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename")
             ,sectitle))))))))

(define do-documentclass
 (lambda ()
   (probably-latex)
   (get-bracketed-text-if-any)
   (let ((x (get-peeled-group)))
     (cond ((member x '("report" "book")) (!using-chapters)) (else false)))))

(define get-till-par
 (lambda ()
   (let ((r null) (newline-p false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((c (get-actual-char)))
             (cond
              ((or (eq? c ':eof-object) (and newline-p (char=? c #\newline)))
               (return
                (let ((%type 'string) (%ee (list (reverse r))))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res))))
              (newline-p
               (cond
                ((not (char-whitespace? c))
                 (let ((%push-new-stack (cons #\  r)))
                   (begin (set! r %push-new-stack) r))
                 (let ((%push-new-stack (cons c r)))
                   (begin (set! r %push-new-stack) r))
                 (begin (set! newline-p false) newline-p))
                (else false)))
              ((char=? c #\newline) (begin (set! newline-p true) newline-p))
              (true
               (let ((%push-new-stack (cons c r)))
                 (begin (set! r %push-new-stack) r))
               (begin (set! newline-p false) newline-p)))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-beginsection
 (lambda ()
   (ignorespaces)
   (let ((header
          (let ((%fluid-var-*tabular-stack* (list ':header)))
            (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
             (tex-string-to-html-string (get-till-par))))))
     (do-heading-help 1 false true true false header))))

(define do-beginchapter
 (lambda ()
   (ignorespaces)
   (let ((chapno (tex-string-to-html-string (get-till-char #\ ))))
     (let ((header
            (begin (ignorespaces)
             (let ((%fluid-var-*tabular-stack* (list ':header)))
               (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
                (tex-string-to-html-string (get-till-par)))))))
       (begin (tex-gdef-0arg "\\chapno" chapno) (tex-gdef-count "\\subsecno" 0)
        (tex-gdef-count "\\footnotenumber" 0)
        (cond ((string=? chapno "") (begin (set! chapno false) chapno))
              (else false))
        (do-heading-help 0 false true false chapno header))))))

(define do-opmac-heading
 (lambda (seclvl)
   (ignorespaces)
   (let ((header
          (let ((%fluid-var-*tabular-stack* (list ':header)))
            (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
             (tex-string-to-html-string (get-till-par))))))
     (let ((nonum-p *opmac-nonum-p*) (notoc-p *opmac-notoc-p*))
       (begin
        (begin (set! *opmac-nonum-p* false) (set! *opmac-notoc-p* false)
         *opmac-notoc-p*)
        (do-heading-help seclvl false nonum-p notoc-p false header))))))

(define do-opmac-sec
 (lambda ()
   (if *math-mode-p*
       (toss-back-string "\\TIIPsec")
       (do-opmac-heading 1))))

(define do-appendix
 (lambda ()
   (cond
    ((not *inside-appendix-p*)
     (begin (set! *inside-appendix-p* true)
      (table-put!
       (if *using-chapters-p*
           0
           1)
       *section-counters* 0)
      (table-get
       (if *using-chapters-p*
           0
           1)
       *section-counters*)))
    (else false))))

(define do-table-plain
 (lambda () (do-end-para) (emit "<table width=100%><tr><td>")))

(define do-end-table-plain
 (lambda () (do-end-para) (emit "</td></tr></table>")))

(define do-table/figure
 (lambda (type)
   (do-end-para)
   (bgroup)
   (cond
    ((and (eqv? type ':figure) (char=? (snoop-actual-char) #\*))
     (get-actual-char))
    (else false))
   (let ((%push-new-stack (cons type *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (get-bracketed-text-if-any)
   (let ((tbl-tag
          (let ((%type 'string)
                (%ee
                 (list *html-node-prefix*
                       (if (eqv? type 'table)
                           "tbl_"
                           "fig_")
                       (gen-temp-string))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (tex-def-0arg "\\TIIPcurrentnodename" tbl-tag)
      (emit-anchor tbl-tag) (emit-newline) (emit "<div class=") (emit type)
      (emit " align=") (emit *display-justification*)
      (emit "><table width=100%><tr><td align=") (emit *display-justification*)
      (emit ">")))))

(define pop-tabular-stack
 (lambda (type)
   (cond
    ((not
      (eq?
       (let* ((%pop-old-stack *tabular-stack*)
              (%pop-top-value (car %pop-old-stack)))
         (begin (set! *tabular-stack* (cdr %pop-old-stack)) *tabular-stack*)
         %pop-top-value)
       type))
     (terror 'pop-tabular-stack "Bad environment closer: " type))
    (else false))))

(define do-end-table/figure
 (lambda (type)
   (cond
    ((and (eqv? type 'figure) (char=? (snoop-actual-char) #\*))
     (get-actual-char))
    (else false))
   (do-end-para)
   (emit "</td></tr>")
   (emit "</table>")
   (emit "</div>")
   (pop-tabular-stack type)
   (egroup)
   (do-para)))

(define bump-dotted-counter
 (lambda (name)
   (let ((counter (table-get name *dotted-counters*)))
     (let ((new-value (add1 (counter*-value counter))))
       (begin
        (begin (set!counter*-value counter new-value) (counter*-value counter))
        (let ((num
               (let ((%type 'string)
                     (%ee
                      (list
                       (let ((sec-num (counter*-within counter)))
                         (if sec-num
                             (let ((%type 'string)
                                   (%ee
                                    (list (section-counter-value sec-num) ".")))
                               (let ((%res
                                      (if (eq? %type 'string)
                                          ""
                                          null)))
                                 (let %concatenate-loop
                                   ((%ee %ee))
                                   (if (null? %ee)
                                       %res
                                       (let ((%a (car %ee)))
                                         (unless (not %a)
                                           (set! %res
                                            (if (eq? %type 'string)
                                                (string-append %res
                                                 (if (string? %a)
                                                     %a
                                                     (list->string %a)))
                                                (append %res
                                                        (if (string? %a)
                                                            (string->list %a)
                                                            %a)))))
                                         (%concatenate-loop (cdr %ee)))))
                                 %res))
                             ""))
                       (write-to-string new-value))))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res))))
          (begin (tex-def-0arg "\\@currentlabel" num) num)))))))

(define do-caption
 (lambda ()
   (do-end-para)
   (let ((i-fig
          (let ((%position-v ':figure)
                (%position-s *tabular-stack*)
                (%ee (list)))
            (let ((%position-from-end (memv ':from-end %ee)))
              (when %position-from-end
                (set! %position-from-end (cadr %position-from-end)))
              (if (string? %position-s)
                  ((if %position-from-end
                       string-reverse-index
                       string-index)
                   %position-s %position-v)
                  (list-position %position-v %position-s))))))
     (let ((i-tbl
            (let ((%position-v ':table)
                  (%position-s *tabular-stack*)
                  (%ee (list)))
              (let ((%position-from-end (memv ':from-end %ee)))
                (when %position-from-end
                  (set! %position-from-end (cadr %position-from-end)))
                (if (string? %position-s)
                    ((if %position-from-end
                         string-reverse-index
                         string-index)
                     %position-s %position-v)
                    (list-position %position-v %position-s))))))
       (let ((type
              (cond
               ((and (not i-fig) (not i-tbl))
                (terror 'do-caption "Mislaid \\caption"))
               ((not i-fig) ':table) ((not i-tbl) ':figure)
               ((< i-fig i-tbl) ':figure) ((< i-tbl i-fig) ':table)
               (true (terror 'do-caption "cant happen")))))
         (let ((counter-name
                (if (eq? type ':table)
                    "table"
                    "figure")))
           (let ((caption-title
                  (if (eq? type ':table)
                      "\\tablename"
                      "\\figurename")))
             (let ((num (bump-dotted-counter counter-name)))
               (begin (get-bracketed-text-if-any) (emit "</td></tr>")
                (emit-newline) (emit "<tr><td align=")
                (emit *display-justification*) (emit "><b>")
                (tex2page-string caption-title) (emit " ") (emit num)
                (emit ":</b>") (emit-nbsp 2) (tex2page-string (get-group))
                (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))))))))))

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
   (let ((in-table-p
          (and (not (null? *tabular-stack*))
               (member (car *tabular-stack*) '(:block :figure :table)))))
     (begin
      (if in-table-p
          (emit "</td><td>")
          (begin (do-para) (do-end-para)))
      (emit "<div align=left>")
      (let ((%push-new-stack (cons ':minipage *tabular-stack*)))
        (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))))))

(define do-endminipage
 (lambda ()
   (pop-tabular-stack ':minipage)
   (let ((in-table-p (member (car *tabular-stack*) '(:block :figure :table))))
     (begin (emit "</div>")
      (if in-table-p
          (emit "</td><td>")
          (do-para))))))

(define do-tabbing
 (lambda ()
   (let ((%push-new-stack (cons ':tabbing *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (do-para)))

(define do-end-tabbing (lambda () (pop-tabular-stack ':tabbing) (do-para)))

(define do-equation
 (lambda (type)
   (cond
    ((and
      (or (not (tex2page-flag-boolean "\\TZPmathtext"))
          (tex2page-flag-boolean "\\TZPmathimage"))
      (not *temporarily-use-utf8-for-math-p*))
     (do-latex-env-as-image
      (case type ((:equation) "equation") ((:align) "align") (else "eqnarray"))
      ':display))
    (true (do-end-para) (bgroup)
     (cond ((eqv? type ':align) (begin (set! type ':eqnarray) type))
           (else false))
     (cond
      ((and (eqv? type ':eqnarray) (eat-star))
       (begin (set! type ':eqnarray*) type))
      (else false))
     (let ((%push-new-stack (cons type *tabular-stack*)))
       (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
     (begin (set! *math-mode-p* true) (set! *in-display-math-p* true)
      *in-display-math-p*)
     (let ((eqn-tag
            (let ((%type 'string)
                  (%ee (list *html-node-prefix* "eqn_" (gen-temp-string))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin (tex-def-0arg "\\TIIPcurrentnodename" eqn-tag)
        (emit-anchor eqn-tag) (emit-newline)
        (cond
         ((not (eqv? type ':eqnarray*))
          (begin (set! *equation-number* (bump-dotted-counter "equation"))
           *equation-number*))
         (else false))
        (emit "<div align=") (emit *display-justification*)
        (emit "><table width=100%>") (emit-newline) (emit "<tr><td align=")
        (emit
         (if (eqv? type ':equation)
             "center"
             "right"))
        (emit ">")))))))

(define do-end-equation
 (lambda ()
   (do-end-para)
   (emit "</td>")
   (cond
    ((not
      (or (eq? (car *tabular-stack*) ':eqnarray*) (not *equation-numbered-p*)))
     (emit "<td>(") (emit *equation-number*) (emit ")</td>"))
    (else false))
   (emit "</tr>")
   (emit-newline)
   (emit "</table></div>")
   (pop-tabular-stack ':equation)
   (begin (set! *math-mode-p* false) (set! *in-display-math-p* false)
    *in-display-math-p*)
   (egroup)
   (begin (set! *equation-numbered-p* true) (set! *equation-position* 0)
    *equation-position*)
   (do-para)))

(define do-integral
 (lambda ()
   (if (or (not *in-display-math-p*) *math-script-mode-p*)
       (emit "&#x222b;")
       (let ((affixes-already-read null))
         (begin
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
                (unless %loop-returned
                  (cond ((>= i %dotimes-n) (return)) (else false)))
                (unless %loop-returned (ignorespaces))
                (unless %loop-returned
                  (let ((c (snoop-actual-char)))
                    (cond
                     ((and (member c '(#\_ #\^))
                           (not (member c affixes-already-read)))
                      (let ((%push-new-stack (cons c affixes-already-read)))
                        (begin (set! affixes-already-read %push-new-stack)
                         affixes-already-read))
                      (get-actual-char)
                      (cond ((= i 0) (emit (kern ".16667em"))) (else false))
                      (let ((%fluid-var-*math-script-mode-p* true))
                        (fluid-let
                         ((*math-script-mode-p*
                           %fluid-var-*math-script-mode-p*))
                         (let ((s (get-token)))
                           (begin
                            (emit
                             "<span style=\"font-size: 85%; position: relative; ")
                            (emit
                             (case c
                               ((#\_) "top: 2.5ex; ")
                               ((#\^) "bottom: 3ex; ")
                               (else (error 'ecase "0xdeadc0de"))))
                            (emit "\">") (tex2page-string s)
                            (emit "</span>"))))))
                     (else false))))
                (unless %loop-returned (set! i (+ i 1)))
                (if %loop-returned
                    %loop-result
                    (%loop))))))))))

(define do-nonumber
 (lambda () (begin (set! *equation-numbered-p* false) *equation-numbered-p*)))

(define indent-n-levels
 (lambda (n)
   (let ((%dotimes-n (add1 n)) (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((>= i %dotimes-n) (return)) (else false)))
         (unless %loop-returned (emit-nbsp 1))
         (unless %loop-returned (emit " "))
         (unless %loop-returned (emit-nbsp 1))
         (unless %loop-returned (emit " "))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-toc
 (lambda ()
   (let ((%fluid-var-*subjobname*
          (let ((%type 'string) (%ee (list *jobname* *toc-file-suffix*)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         (%fluid-var-*img-file-count* 0)
         (%fluid-var-*imgdef-file-count* 0))
     (fluid-let
      ((*imgdef-file-count* %fluid-var-*imgdef-file-count*)
       (*img-file-count* %fluid-var-*img-file-count*)
       (*subjobname* %fluid-var-*subjobname*))
      (cond
       ((eqv? *tex-format* ':latex)
        (tex2page-string
         (if *using-chapters-p*
             "\\chapter*{\\contentsname}"
             "\\section*{\\contentsname}")))
       (else false))
      (emit-anchor
       (let ((%type 'string) (%ee (list *html-node-prefix* "toc")))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (!toc-page *html-page-count*) (write-aux `(!toc-page ,*html-page-count*))
      (cond
       ((null? *toc-list*) (flag-missing-piece ':toc)
        (non-fatal-error "Table of contents not generated; rerun TeX2page"))
       (true (do-noindent)
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
                     (begin
                      (cond
                       (subentries-p
                        (if *tex-like-layout-p*
                            (do-bigskip ':medskip)
                            (do-para))
                        (do-noindent) (emit "<b>") (emit-newline))
                       (else false))
                      (indent-n-levels lvl)
                      (emit-anchor
                       (let ((%type 'string)
                             (%ee (list *html-node-prefix* "toc_" seclabel)))
                         (let ((%res
                                (if (eq? %type 'string)
                                    ""
                                    null)))
                           (let %concatenate-loop
                             ((%ee %ee))
                             (if (null? %ee)
                                 %res
                                 (let ((%a (car %ee)))
                                   (unless (not %a)
                                     (set! %res
                                      (if (eq? %type 'string)
                                          (string-append %res
                                           (if (string? %a)
                                               %a
                                               (list->string %a)))
                                          (append %res
                                                  (if (string? %a)
                                                      (string->list %a)
                                                      %a)))))
                                   (%concatenate-loop (cdr %ee)))))
                           %res)))
                      (emit-page-node-link-start (tocentry*-page x) seclabel)
                      (cond
                       ((not
                         (or (string=? secnum "") (string=? secnum "IGNORE")))
                        (emit secnum) (emit-nbsp 2))
                       (else false))
                      (let ((%fluid-var-*tabular-stack* (list 'header)))
                        (fluid-let
                         ((*tabular-stack* %fluid-var-*tabular-stack*))
                         (emit (tocentry*-header x))))
                      (emit-link-stop)
                      (cond (subentries-p (emit "</b>")) (else false))
                      (emit "<br>") (emit-newline)))))))
           *toc-list*))))
      (emit-anchor
       (let ((%type 'string) (%ee (list *html-node-prefix* "toc_end")))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))))))

(defstruct footnotev* mark text tag caller)

(define do-numbered-footnote (lambda () (do-footnote-aux false)))

(define do-symfootnote
 (lambda ()
   (let ((%tmp (+ *footnote-sym* 1)))
     (begin (set! *footnote-sym* %tmp) *footnote-sym*))
   (do-footnote-aux (number-to-footnote-symbol *footnote-sym*))))

(define tex-string-to-html-string
 (lambda (s)
   (let ((tmp-port (open-output-string)))
     (let ((%fluid-var-*html* tmp-port))
       (fluid-let ((*html* %fluid-var-*html*))
        (begin (tex2page-string s) (get-output-string tmp-port)))))))

(define expand-tex-string
 (lambda (s)
   (let ((tmp-port (open-output-string)))
     (let ((%fluid-var-*html* tmp-port))
       (fluid-let ((*html* %fluid-var-*html*))
        (let ((%fluid-var-*outputting-to-non-html-p* true))
          (fluid-let
           ((*outputting-to-non-html-p* %fluid-var-*outputting-to-non-html-p*))
           (begin (tex2page-string s) (get-output-string tmp-port)))))))))

(define number-to-footnote-symbol
 (let ((symlist false) (symlist-len 0))
   (lambda (n)
     (cond
      ((not symlist)
       (begin
        (set! symlist
         '("*" "&#x2020;" "&#x2021;" "&#xa7;" "&#xb6;" "&#x2225;" "*"
           "&#x2020;&#x2020;" "&#x2021;&#x2021;"))
        (set! symlist-len
         (let ((%length-arg symlist))
           ((if (string? %length-arg)
                string-length
                length)
            %length-arg)))
        symlist-len))
      (else false))
     (list-ref symlist (modulo (sub1 n) symlist-len)))))

(define do-plain-footnote
 (lambda ()
   (do-footnote-aux
    (let ((%fluid-var-*temporarily-use-utf8-for-math-p* true))
      (fluid-let
       ((*temporarily-use-utf8-for-math-p*
         %fluid-var-*temporarily-use-utf8-for-math-p*))
       (tex-string-to-html-string (get-token)))))))

(define do-footnote
 (lambda ()
   (if (eq? *tex-format* ':latex)
       (do-numbered-footnote)
       (do-plain-footnote))))

(define do-footnote-aux
 (lambda (fnmark)
   (let ((fnno false))
     (let ((fnlabel (gen-temp-string)))
       (let ((fntag
              (let ((%type 'string)
                    (%ee (list *html-node-prefix* "footnote_" fnlabel)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (let ((fncalltag
                (let ((%type 'string)
                      (%ee (list *html-node-prefix* "call_footnote_" fnlabel)))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res))))
           (begin
            (cond
             ((not fnmark)
              (begin (set! fnno (+ (get-gcount "\\footnotenumber") 1)) fnno)
              (tex-gdef-count "\\footnotenumber" fnno)
              (begin (set! fnmark (write-to-string fnno)) fnmark))
             (else false))
            (emit-anchor fncalltag)
            (cond (fnno (emit "<sup><small>")) (else false))
            (emit-page-node-link-start false fntag) (emit fnmark)
            (emit-link-stop) (cond (fnno (emit "</small></sup>")) (else false))
            (do-vfootnote-aux fnmark fncalltag fntag))))))))

(define do-vfootnote
 (lambda ()
   (do-vfootnote-aux
    (let ((%fluid-var-*temporarily-use-utf8-for-math-p* true))
      (fluid-let
       ((*temporarily-use-utf8-for-math-p*
         %fluid-var-*temporarily-use-utf8-for-math-p*))
       (tex-string-to-html-string (get-token))))
    false false)))

(define do-vfootnote-aux
 (lambda (fnmark fncalltag fntag)
   (ignorespaces)
   (cond
    ((not (char=? (get-actual-char) #\{))
     (terror 'do-vfootnote-aux "Missing {"))
    (else false))
   (bgroup)
   (let ((old-html *html*) (fn-tmp-port (open-output-string)))
     (begin (begin (set! *html* fn-tmp-port) *html*)
      (cond
       (fncalltag (tex-def-0arg "\\TIIPcurrentnodename" fntag)
        (tex-def-0arg "\\@currentlabel" fnmark))
       (else false))
      (add-aftergroup-to-top-frame
       (lambda ()
         (let ((%push-new-stack
                (cons
                 (make-footnotev* ':mark fnmark ':text
                  (get-output-string fn-tmp-port) ':tag fntag ':caller
                  fncalltag)
                 *footnote-list*)))
           (begin (set! *footnote-list* %push-new-stack) *footnote-list*))
         (begin (set! *html* old-html) *html*)))))))

(define output-footnotes
 (lambda ()
   (let ((n
          (let ((%length-arg *footnote-list*))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (cond
      ((not (= n 0)) (emit "<div class=footnoterule><hr></div>") (do-para)
       (do-end-para) (emit "<div class=footnote>")
       (let ((i (- n 1)))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned (cond ((< i 0) (return)) (else false)))
             (unless %loop-returned
               (let ((fv (list-ref *footnote-list* i)))
                 (let ((fnmark (footnotev*-mark fv)))
                   (let ((fnno (string->number fnmark)))
                     (let ((fncalltag (footnotev*-caller fv)))
                       (begin (do-para)
                        (cond
                         (fncalltag (emit-anchor (footnotev*-tag fv))
                          (cond (fnno (emit "<sup><small>")) (else false))
                          (emit-page-node-link-start false fncalltag))
                         (else false))
                        (emit fnmark)
                        (cond
                         (fncalltag (emit-link-stop)
                          (cond (fnno (emit "</small></sup>")) (else false)))
                         (else false))
                        (emit " ") (emit (footnotev*-text fv)) (do-end-para)
                        (let ((%tmp (- i 1)))
                          (begin (set! i %tmp) i))))))))
             (if %loop-returned
                 %loop-result
                 (%loop)))))
       (emit "</div>") (emit-newline))
      (else false)))))

(define rgb-dec-to-rrggbb
 (let ((f
        (lambda (x)
          (let ((n (round (* 1.0 x))))
            (let ((s (write-to-string n ':base 16)))
              (if (< n 16)
                  (let ((%type 'string) (%ee (list "0" s)))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res))
                  s))))))
   (lambda (r g b)
     (let ((%type 'string) (%ee (list "#" (f r) (f g) (f b))))
       (let ((%res
              (if (eq? %type 'string)
                  ""
                  null)))
         (let %concatenate-loop
           ((%ee %ee))
           (if (null? %ee)
               %res
               (let ((%a (car %ee)))
                 (unless (not %a)
                   (set! %res
                    (if (eq? %type 'string)
                        (string-append %res
                         (if (string? %a)
                             %a
                             (list->string %a)))
                        (append %res
                                (if (string? %a)
                                    (string->list %a)
                                    %a)))))
                 (%concatenate-loop (cdr %ee)))))
         %res)))))

(define rgb-frac-to-rrggbb
 (lambda (r g b) (rgb-dec-to-rrggbb (* r 255) (* g 255) (* b 255))))

(define cmyk-to-rrggbb
 (let ((f (lambda (x k) (- 1 (min (max (+ x k) 0) 1)))))
   (lambda (c m y k) (rgb-frac-to-rrggbb (f c k) (f m k) (f y k)))))

(define hsb-to-rrggbb
 (lambda (hue saturation brightness)
   (let ((red false))
     (let ((green false))
       (let ((blue false))
         (let ((hue6 (* 6 hue)))
           (let ((i (inexact->exact (floor hue6))))
             (let ((f (- hue6 i)))
               (begin
                (case i
                  ((0)
                   (begin (set! red 0) (set! green (- 1 f)) (set! blue 1)
                    blue))
                  ((1) (begin (set! red f) (set! green 0) (set! blue 1) blue))
                  ((2)
                   (begin (set! red 1) (set! green 0) (set! blue (- 1 f))
                    blue))
                  ((3) (begin (set! red 1) (set! green f) (set! blue 0) blue))
                  ((4)
                   (begin (set! red (- 1 f)) (set! green 1) (set! blue 0)
                    blue))
                  ((5) (begin (set! red 0) (set! green 1) (set! blue f) blue))
                  ((6) (begin (set! red 0) (set! green 1) (set! blue 1) blue))
                  (else (terror 'hsb-to-rrggbb "Can't happen.")))
                (let ((fu (lambda (x) (* brightness (- 1 (* saturation x))))))
                  (rgb-frac-to-rrggbb (fu red) (fu green) (fu blue))))))))))))

(define wavelength-to-rrggbb
 (lambda (w)
   (let ((hue
          (* 1/6
             (cond ((<= w 362.857) 5) ((< w 440) (+ 4 (/ (- w 440) -60)))
                   ((< w 490) (- 4 (/ (- w 440) 50)))
                   ((< w 510) (+ 2 (/ (- w 510) -20)))
                   ((< w 580) (- 2 (/ (- w 510) 70)))
                   ((< w 645) (/ (- w 645) -65)) (true 0))))
         (brightness
          (cond ((<= w 362.857) 0) ((< w 420) (+ 0.3 (* 0.7 (/ (- w 380) 40))))
                ((<= w 700) 1)
                ((< w 814.285) (+ 0.3 (* 0.7 (/ (- w 780) -80)))) (true 0))))
     (hsb-to-rrggbb hue 1 brightness))))

(define read-color-as-rrggbb
 (lambda (model)
   (case model
     ((:cmy)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((c
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((m
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((y
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces)
                (rgb-frac-to-rrggbb (- 1 c) (- 1 m) (- 1 y)))))))))
     ((:cmyk)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((c
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((m
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((y
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (let ((k
                      (let ((%read-res (read i)))
                        (when (eof-object? %read-res)
                          (set! %read-res ':eof-object))
                        %read-res)))
                 (begin (ignorespaces) (cmyk-to-rrggbb c m y k)))))))))
     ((:rgb)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((r
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((g
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((b
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces) (rgb-frac-to-rrggbb r g b))))))))
     ((:rgb255)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((r
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((g
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((b
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces) (rgb-dec-to-rrggbb r g b))))))))
     ((:gray)
      (call-with-input-string (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((g
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (begin (ignorespaces) (cmyk-to-rrggbb 0 0 0 (- 1 g)))))))
     ((:gray15)
      (call-with-input-string (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((g
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (begin (ignorespaces) (cmyk-to-rrggbb 0 0 0 (- 1 (/ g 15))))))))
     ((:html)
      (call-with-input-string (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((rrggbb (read-6hex i)))
           (begin (ignorespaces) rrggbb)))))
     ((:hsb)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((h
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((s
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((b
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces) (hsb-to-rrggbb h s b))))))))
     ((:hsb360)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((h
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((s
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((b
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces) (hsb-to-rrggbb (/ h 360) s b))))))))
     ((:hsb240)
      (bgroup)
      (call-with-input-string
       (tex-string-to-html-string
        (let ((%type 'string) (%ee (list "\\defcsactive\\,{ }" (get-token))))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (lambda (i)
         (egroup)
         (let ((h
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (let ((s
                  (let ((%read-res (read i)))
                    (when (eof-object? %read-res)
                      (set! %read-res ':eof-object))
                    %read-res)))
             (let ((b
                    (let ((%read-res (read i)))
                      (when (eof-object? %read-res)
                        (set! %read-res ':eof-object))
                      %read-res)))
               (begin (ignorespaces)
                (hsb-to-rrggbb (/ h 240) (/ s 240) (/ b 240)))))))))
     ((:wave)
      (call-with-input-string (tex-string-to-html-string (get-token))
       (lambda (i)
         (let ((w
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res)))
           (begin (ignorespaces) (wavelength-to-rrggbb w))))))
     (else
      (let ((name (get-peeled-group)))
        (let ((c (assoc name *color-names*)))
          (begin (ignorespaces)
           (if c
               (cdr c)
               name))))))))

(define color-model-to-keyword
 (lambda (model)
   (cond ((not model) ':colornamed) ((string=? model "rgb") ':rgb)
         ((string=? model "RGB") ':rgb255) ((string=? model "cmyk") ':cmyk)
         ((string=? model "cmy") ':cmy) ((string=? model "gray") ':gray)
         ((string=? model "Gray") ':gray15) ((string=? model "HTML") ':html)
         ((string=? model "hsb") ':hsb) ((string=? model "Hsb") ':hsb360)
         ((string=? model "HSB") ':hsb240) ((string=? model "wave") ':wave)
         (true ':colornamed))))

(define do-color
 (lambda ()
   (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
     (do-switch model))))

(define do-pagecolor
 (lambda ()
   (let ((model (color-model-to-keyword (get-bracketed-text-if-any))))
     (let ((color (read-color-as-rrggbb model)))
       (begin (display "body { background-color: " *css-port*)
        (display color *css-port*) (display "; }" *css-port*)
        (newline *css-port*))))))

(define do-colorbox
 (lambda ()
   (let ((color (get-group)))
     (let ((text (get-peeled-group)))
       (begin (toss-back-char #\}) (toss-back-string text)
        (toss-back-string color) (toss-back-string "\\bgcolor")
        (toss-back-char #\{))))))

(define do-definecolor
 (lambda ()
   (let ((name (get-peeled-group)))
     (let ((model (color-model-to-keyword (get-peeled-group))))
       (let ((%push-new-stack
              (cons (cons name (read-color-as-rrggbb model)) *color-names*)))
         (begin (set! *color-names* %push-new-stack) *color-names*))))))

(define do-switch
 (lambda (sw)
   (cond
    ((not *outputting-external-title-p*)
     (add-postlude-to-top-frame
      (case sw
        ((:rm)
         (cond
          (*math-mode-p*
           (let ((old-math-font *math-font*))
             (begin (begin (set! *math-font* ':rm) *math-font*)
              (lambda ()
                (begin (set! *math-font* old-math-font) *math-font*)))))
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
           (begin (begin (set! *ligatures-p* false) *ligatures-p*)
            (emit "<tt>")
            (lambda ()
              (emit "</tt>")
              (begin (set! *ligatures-p* old-ligatures-p) *ligatures-p*)))))
        ((:sc)
         (let ((old-in-small-caps-p *in-small-caps-p*))
           (begin (begin (set! *in-small-caps-p* true) *in-small-caps-p*)
            (lambda ()
              (begin (set! *in-small-caps-p* old-in-small-caps-p)
               *in-small-caps-p*)))))
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
        ((:large-up)
         (emit "<span class=largeup>")
         (lambda () (emit "</span>")))
        ((:huge) (emit "<span class=huge>") (lambda () (emit "</span>")))
        ((:huge-cap)
         (emit "<span class=hugecap>")
         (lambda () (emit "</span>")))
        ((:cmy :cmyk :rgb :rgb255 :gray :gray15 :html :hsb :hsb360 :hsb240
          :wave :colornamed)
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
        ((:strike) (emit "<strike>") (lambda () (emit "</strike>")))
        ((:narrower) (emit "<blockquote>") (lambda () (emit "</blockquote>")))
        ((:raggedleft)
         (do-end-para)
         (emit "<div align=right>")
         (lambda () (do-end-para) (emit "</div>") (do-para)))
        ((:oldstyle)
         (emit "<span class=oldstyle>")
         (lambda () (emit "</span>")))
        (else
         (emit "<span class=")
         (emit sw)
         (emit ">")
         (lambda () (emit "</span>"))))))
    (else false))))

(define do-obeylines
 (lambda ()
   (cond ((eqv? (snoop-actual-char) #\newline) (get-actual-char)) (else false))
   (activate-cdef #\newline)
   (tex-def-char #\newline null "\\TIIPpar" false)))

(define do-obeyspaces
 (lambda () (activate-cdef #\ ) (tex-def-char #\  null "\\TIIPnbsp" false)))

(define do-obeywhitespace (lambda () (do-obeylines) (do-obeyspaces)))

(define do-block
 (lambda (z)
   (do-end-para)
   (emit "<div ")
   (emit
    (case z
      ((:flushleft) "align=left")
      ((:flushright) "align=right")
      (else "align=center")))
   (emit ">")
   (let ((%push-new-stack (cons ':block *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (emit "<table><tr><td>")
   (bgroup)
   (emit-newline)))

(define do-end-block
 (lambda ()
   (do-end-para)
   (egroup)
   (emit "</td></tr></table></div>")
   (pop-tabular-stack ':block)
   (emit-newline)))

(define do-function
 (lambda (fn)
   (let ((%fluid-var-*math-mode-p* *math-mode-p*))
     (fluid-let ((*math-mode-p* %fluid-var-*math-mode-p*))
      (cond (*outputting-external-title-p* false)
            ((string=? fn "\\emph") (emit "<em>"))
            ((string=? fn "\\leftline") (do-end-para)
             (emit "<div align=left>"))
            ((string=? fn "\\centerline") (do-end-para)
             (emit "<div align=center>&#xa0;"))
            ((string=? fn "\\rightline") (do-end-para)
             (emit "<div align=right>&#xa0;"))
            ((string=? fn "\\underline") (emit "<u>"))
            ((string=? fn "\\textbf")
             (begin (set! *math-mode-p* false) *math-mode-p*) (emit "<b>"))
            ((or (string=? fn "\\textit") (string=? fn "\\textsl"))
             (begin (set! *math-mode-p* false) *math-mode-p*) (emit "<i>"))
            ((string=? fn "\\textrm")
             (begin (set! *math-mode-p* false) *math-mode-p*))
            ((string=? fn "\\texttt")
             (begin (set! *math-mode-p* false) *math-mode-p*) (emit "<tt>"))
            (true (terror 'do-function "Unknown function " fn)))
      (bgroup) (tex2page-string (get-token)) (egroup)
      (cond (*outputting-external-title-p* false)
            ((string=? fn "\\emph") (emit "</em>"))
            ((string=? fn "\\rightline") (emit "</div>") (emit-newline))
            ((or (string=? fn "\\leftline") (string=? fn "\\centerline"))
             (do-end-para) (emit "&#xa0;</div>") (emit-newline))
            ((string=? fn "\\underline") (emit "</u>"))
            ((string=? fn "\\textbf") (emit "</b>"))
            ((or (string=? fn "\\textsl") (string=? fn "\\textit"))
             (emit "</i>"))
            ((string=? fn "\\texttt") (emit "</tt>")))))))

(define do-discretionary
 (lambda () (tex2page-string (get-group)) (get-group) (get-group)))

(define do-aftergroup
 (lambda ()
   (ignorespaces)
   (let ((z (get-ctl-seq)))
     (add-aftergroup-to-top-frame (lambda () (toss-back-string z))))))

(define do-afterassignment
 (lambda ()
   (ignorespaces)
   (let ((z (get-ctl-seq)))
     (begin (set! *afterassignment* z) *afterassignment*))))

(define do-space (lambda () (emit #\ )))

(define do-tab (lambda () (emit-nbsp 8)))

(define emit-nbsp
 (lambda (n)
   (let ((%dotimes-n n) (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((>= i %dotimes-n) (return)) (else false)))
         (unless %loop-returned (emit "&#xa0;"))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define scaled-point-equivalent-of
 (lambda (unit)
   (case unit
     ((:sp) 1)
     ((:pt) 65536)
     ((:bp) (* (/ 72) (scaled-point-equivalent-of ':in)))
     ((:cc) (* 12 (scaled-point-equivalent-of ':dd)))
     ((:dd) (* (/ 1238 1157) (scaled-point-equivalent-of ':pt)))
     ((:em) (* 10 (scaled-point-equivalent-of ':pt)))
     ((:ex) (* 4.5 (scaled-point-equivalent-of ':pt)))
     ((:in) (* 72.27 (scaled-point-equivalent-of ':pt)))
     ((:mm) (* 0.1 (scaled-point-equivalent-of ':cm)))
     ((:cm) (* (/ 2.54) (scaled-point-equivalent-of ':in)))
     ((:pc) (* 12 (scaled-point-equivalent-of ':pt)))
     (else
      (terror 'scaled-point-equivalent-of "Illegal unit of measure " unit)))))

(define tex-length
 (lambda (num unit) (* num (scaled-point-equivalent-of unit))))

(define sp-to-ems (lambda (sp) (/ sp 655360.0)))

(define sp-to-pixels (lambda (sp) (inexact->exact (floor (/ sp 65536)))))

(define get-scaled-points
 (lambda ()
   (let ((n (or (get-real) 1)))
     (begin (ignorespaces)
      (* n
         (if (esc-char-p (snoop-actual-char))
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
                  (unless %loop-returned
                    (cond ((not (eat-word "true")) (return)) (else false)))
                  (if %loop-returned
                      %loop-result
                      (%loop))))
              (cond ((eat-word "bp") (tex-length 1 ':bp))
                    ((eat-word "cc") (tex-length 1 ':cc))
                    ((eat-word "cm") (tex-length 1 ':cm))
                    ((eat-word "dd") (tex-length 1 ':dd))
                    ((eat-word "em") (tex-length 1 ':em))
                    ((eat-word "ex") (tex-length 1 ':ex))
                    ((eat-word "in") (tex-length 1 ':in))
                    ((eat-word "mm") (tex-length 1 ':mm))
                    ((eat-word "pc") (tex-length 1 ':pc))
                    ((eat-word "pt") (tex-length 1 ':pt)) ((eat-word "sp") 1)
                    (true 1)))))))))

(define get-points (lambda () (/ (get-scaled-points) 65536.0)))

(define get-pixels (lambda () (inexact->exact (floor (get-points)))))

(define do-font
 (lambda ()
   (get-ctl-seq)
   (get-equal-sign)
   (eat-alphanumeric-string)
   (cond ((eat-word "at") (eat-dimen)) ((eat-word "scaled") (get-number)))))

(define do-fontdimen
 (lambda () (get-number) (get-ctl-seq) (get-equal-sign) (eat-dimen)))

(define do-hskip
 (lambda ()
   (let ((n (get-pixels)))
     (begin (emit "<span style=\"margin-left: ") (emit n)
      (emit "pt\">&#x200c;</span>")))))

(define do-vskip
 (lambda ()
   (let ((x (get-points)))
     (begin (eat-skip-fluff false) (emit "<div style=\"height: ") (emit x)
      (emit "pt\"></div>") (emit-newline)
      (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
      (begin (set! *in-para-p* true) *in-para-p*)))))

(define do-newline
 (lambda ()
   (cond ((>= (munch-newlines) 1) (do-para)) (else false))
   (emit-newline)))

(define do-br
 (lambda ()
   (if (or (find-chardef #\ ) (not (= (the-count "\\TIIPobeylinestrictly") 0)))
       (emit "<br>")
       (cond ((not (eqv? (snoop-actual-char) #\newline)) (emit "<br>"))
             (else false)))
   (emit-newline)))

(define do-sup
 (lambda ()
   (emit "<sup>")
   (let ((%fluid-var-*math-script-mode-p* true))
     (fluid-let ((*math-script-mode-p* %fluid-var-*math-script-mode-p*))
      (tex2page-string (get-token))))
   (emit "</sup>")))

(define do-sub
 (lambda ()
   (emit "<sub>")
   (let ((%fluid-var-*math-script-mode-p* true))
     (fluid-let ((*math-script-mode-p* %fluid-var-*math-script-mode-p*))
      (tex2page-string (get-token))))
   (emit "</sub>")))

(define do-hyphen
 (lambda ()
   (cond
    (*math-mode-p*
     (emit
      (if (eq? *math-font* ':rm)
          "-"
          "&#x2212;")))
    ((not *ligatures-p*) (emit #\-))
    (true
     (let ((c (snoop-actual-char)))
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
    (if (not *ligatures-p*)
        #\`
        (let ((c (snoop-actual-char)))
          (if (and (char? c) (char=? c #\`))
              (begin (get-actual-char) "&#x201c;")
              "&#x2018;"))))))

(define do-rsquo
 (lambda ()
   (emit
    (cond
     (*math-mode-p*
      (let ((c (snoop-actual-char)))
        (if (and (char? c) (char=? c #\'))
            (begin (get-actual-char) "&#x2033;")
            "&#x2032;")))
     ((not *ligatures-p*) #\')
     (true
      (let ((c (snoop-actual-char)))
        (if (and (char? c) (char=? c #\'))
            (begin (get-actual-char) "&#x201d;")
            "&#x2019;")))))))

(define do-enquote
 (lambda ()
   (ignorespaces)
   (cond
    ((and (char=? (snoop-actual-char) #\*)) (get-actual-char) (ignorespaces)
     (cond
      ((= *quote-level* 0)
       (let ((%tmp (+ *quote-level* 1)))
         (begin (set! *quote-level* %tmp) *quote-level*)))
      (else false)))
    (else false))
   (cond
    ((not (char=? (get-actual-char) #\{)) (terror 'do-enquote "Missing {"))
    (else false))
   (bgroup)
   (let ((%tmp (+ *quote-level* 1)))
     (begin (set! *quote-level* %tmp) *quote-level*))
   (emit
    (if (odd? *quote-level*)
        "&#x201c;"
        "&#x2018;"))
   (add-aftergroup-to-top-frame
    (lambda ()
      (emit
       (if (odd? *quote-level*)
           "&#x201d;"
           "&#x2019;"))
      (let ((%tmp (- *quote-level* 1)))
        (begin (set! *quote-level* %tmp) *quote-level*))))))

(defstruct label* (src false) page name value)

(define get-label
 (lambda ()
   (let ((lbl (get-peeled-group)))
     (let ((i
            (or
             (let ((%position-v #\ )
                   (%position-s lbl)
                   (%ee (list ':test char=?)))
               (let ((%position-from-end (memv ':from-end %ee)))
                 (when %position-from-end
                   (set! %position-from-end (cadr %position-from-end)))
                 (if (string? %position-s)
                     ((if %position-from-end
                          string-reverse-index
                          string-index)
                      %position-s %position-v)
                     (list-position %position-v %position-s))))
             (let ((%position-v #\tab)
                   (%position-s lbl)
                   (%ee (list ':test char=?)))
               (let ((%position-from-end (memv ':from-end %ee)))
                 (when %position-from-end
                   (set! %position-from-end (cadr %position-from-end)))
                 (if (string? %position-s)
                     ((if %position-from-end
                          string-reverse-index
                          string-index)
                      %position-s %position-v)
                     (list-position %position-v %position-s))))
             (let ((%position-v #\newline)
                   (%position-s lbl)
                   (%ee (list ':test char=?)))
               (let ((%position-from-end (memv ':from-end %ee)))
                 (when %position-from-end
                   (set! %position-from-end (cadr %position-from-end)))
                 (if (string? %position-s)
                     ((if %position-from-end
                          string-reverse-index
                          string-index)
                      %position-s %position-v)
                     (list-position %position-v %position-s)))))))
       (if (not i)
           lbl
           (let ((s
                  (let ((%type 'list) (%ee (list lbl)))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res)))
                 (r null)
                 (whitep false))
             (let* ((%loop-returned false)
                    (%loop-result 0)
                    (return
                     (lambda %args
                       (set! %loop-returned true)
                       (set! %loop-result (and (pair? %args) (car %args))))))
               (let %loop
                 ()
                 (unless %loop-returned
                   (cond
                    ((null? s)
                     (return
                      (let ((%type 'string) (%ee (list (reverse r))))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res))))
                    (else false)))
                 (unless %loop-returned
                   (let ((c
                          (let* ((%pop-old-stack s)
                                 (%pop-top-value (car %pop-old-stack)))
                            (begin (set! s (cdr %pop-old-stack)) s)
                            %pop-top-value)))
                     (cond
                      ((char-whitespace? c)
                       (cond
                        ((not whitep)
                         (let ((%push-new-stack (cons #\  r)))
                           (begin (set! r %push-new-stack) r)))
                        (else false))
                       (begin (set! whitep true) whitep))
                      (true
                       (let ((%push-new-stack (cons c r)))
                         (begin (set! r %push-new-stack) r))
                       (begin (set! whitep false) whitep)))))
                 (if %loop-returned
                     %loop-result
                     (%loop))))))))))

(define emit-anchor
 (lambda (lbl) (emit "<a name=\"") (emit lbl) (emit "\"></a>")))

(define emit-link-start
 (lambda (link) (emit "<a href=\"") (emit link) (emit "\">")))

(define emit-ext-page-node-link-start
 (lambda (extfile pageno node)
   (emit "<a ")
   (cond ((not extfile) (emit "class=hrefinternal ")) (else false))
   (emit "href=\"")
   (cond
    ((not (and (not extfile) (or (not pageno) (= *html-page-count* pageno))))
     (emit (or extfile *jobname*))
     (cond ((not (= pageno 0)) (emit *html-page-suffix*) (emit pageno))
           (else false))
     (emit *output-extension*))
    (else false))
   (cond (node (emit "#") (emit node)) (else false))
   (emit "\">")))

(define emit-page-node-link-start
 (lambda (pageno node) (emit-ext-page-node-link-start false pageno node)))

(define emit-page-link-start
 (lambda (pageno) (emit-ext-page-node-link-start false pageno false)))

(define emit-link-stop (lambda () (emit "</a>")))

(define do-anchor-for-potential-label
 (lambda ()
   (let ((node-name
          (let ((%type 'string)
                (%ee (list *html-node-prefix* "anchor_" (gen-temp-string))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (emit-anchor node-name)))))

(define do-label (lambda () (do-label-aux (get-label))))

(define do-node
 (lambda ()
   (begin (set! *recent-node-name* (get-peeled-group)) *recent-node-name*)))

(define do-label-aux
 (lambda (label)
   (let ((name (ctl-seq-no-arg-expand-once "\\TIIPcurrentnodename"))
         (value (ctl-seq-no-arg-expand-once "\\@currentlabel")))
     (begin (begin (set! value (tex-string-to-html-string value)) value)
      (!label label *html-page-count* name value)
      (write-label `(!label ,label ,*html-page-count* ,name ,value))))))

(define do-inputexternallabels
 (lambda ()
   (let ((f (get-filename-possibly-braced)))
     (let ((fq-f
            (if (fully-qualified-pathname-p f)
                f
                (let ((%type 'string) (%ee (list *aux-dir/* f)))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res)))))
       (let ((ext-label-file
              (let ((%type 'string)
                    (%ee (list fq-f *label-file-suffix* ".scm")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (let ((ext-label-table (table-get f *external-label-tables*)))
           (begin
            (cond
             ((not ext-label-table)
              (begin (set! ext-label-table (make-table ':test equal?))
               ext-label-table)
              (begin (table-put! f *external-label-tables* ext-label-table)
               (table-get f *external-label-tables*)))
             (else false))
            (cond
             ((file-exists? ext-label-file)
              (let ((%fluid-var-*label-source* fq-f)
                    (%fluid-var-*label-table* ext-label-table))
                (fluid-let
                 ((*label-table* %fluid-var-*label-table*)
                  (*label-source* %fluid-var-*label-source*))
                 (load-tex2page-data-file ext-label-file))))
             (else false)))))))))

(define do-includeexternallabels
 (lambda ()
   (let ((jobname (get-filename-possibly-braced)))
     (let ((ext-label-file
            (let ((%type 'string)
                  (%ee
                   (list
                    (if (fully-qualified-pathname-p jobname)
                        jobname
                        (let ((%type 'string) (%ee (list *aux-dir/* jobname)))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res)))
                    *label-file-suffix* ".scm")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (cond
        ((file-exists? ext-label-file)
         (let ((%fluid-var-*label-source* jobname))
           (fluid-let ((*label-source* %fluid-var-*label-source*))
            (load-tex2page-data-file ext-label-file))))
        (else false))))))

(define do-tag
 (lambda ()
   (let ((tag-name (get-peeled-group)))
     (do-tag-aux tag-name (get-group)))))

(define do-definexref
 (lambda ()
   (let ((tag (get-peeled-group)))
     (let ((value (get-group)))
       (begin (get-token) (do-tag-aux tag value))))))

(define do-xrdef
 (lambda ()
   (let ((tag (get-peeled-group)))
     (do-tag-aux tag (write-to-string *html-page-count*)))))

(define do-tag-aux
 (lambda (tag-name tag-val)
   (let ((node-name
          (let ((%type 'string)
                (%ee (list *html-node-prefix* "tag_" (gen-temp-string))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (tex-def-0arg "\\TIIPcurrentnodename" node-name)
      (tex-def-0arg "\\@currentlabel" tag-val) (emit-anchor node-name)
      (do-label-aux tag-name)))))

(define do-htmlpagelabel
 (lambda ()
   (let ((label (get-peeled-group)))
     (begin (!label label *html-page-count* false false)
      (write-label `(!label ,label ,*html-page-count* false false))))))

(define do-ref (lambda () (do-ref-aux (get-label) false false)))

(define do-refexternal
 (lambda ()
   (let ((ext-file (get-peeled-group)))
     (do-ref-aux (get-label) ext-file false))))

(define do-ref-aux
 (lambda (label ext-file link-text)
   (let ((label-ref (label-bound-p label ext-file)))
     (let ((label-text
            (cond (link-text (tex-string-to-html-string link-text))
                  (label-ref (label*-value label-ref)) (true label))))
       (cond
        (label-ref
         (emit-ext-page-node-link-start (or ext-file (label*-src label-ref))
          (label*-page label-ref) (label*-name label-ref))
         (emit label-text) (emit-link-stop))
        (true (non-fatal-error label)))))))

(define maybe-label-page
 (lambda (this-label-src this-label-pageno)
   (if (and (not this-label-src) (= *html-page-count* this-label-pageno))
       ""
       (let ((%type 'string)
             (%ee
              (list (or this-label-src *jobname*)
                    (if (= this-label-pageno 0)
                        ""
                        (let ((%type 'string)
                              (%ee
                               (list *html-page-suffix*
                                     (write-to-string this-label-pageno))))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res)))
                    *output-extension*)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))))

(define do-htmlref
 (lambda ()
   (let ((text (get-group)))
     (let ((lbl (get-peeled-group)))
       (do-ref-aux lbl false text)))))

(define do-htmlrefexternal
 (lambda ()
   (let ((text (get-group)))
     (let ((extf (get-peeled-group)))
       (let ((lbl (get-peeled-group)))
         (do-ref-aux lbl extf text))))))

(define do-hyperref
 (lambda ()
   (let ((lbl (get-bracketed-text-if-any)))
     (if lbl
         (do-ref-aux lbl false (get-group))
         (let ((text (get-group)))
           (let ((lbl (begin (get-group) (get-group) (get-peeled-group))))
             (do-ref-aux lbl false text)))))))

(define do-hypertarget
 (lambda ()
   (let ((lbl (get-peeled-group)))
     (do-tag-aux lbl "hypertarget"))))

(define do-hyperlink
 (lambda ()
   (emit-link-start
    (fully-qualify-url
     (let ((%type 'string) (%ee (list "#" (get-peeled-group))))
       (let ((%res
              (if (eq? %type 'string)
                  ""
                  null)))
         (let %concatenate-loop
           ((%ee %ee))
           (if (null? %ee)
               %res
               (let ((%a (car %ee)))
                 (unless (not %a)
                   (set! %res
                    (if (eq? %type 'string)
                        (string-append %res
                         (if (string? %a)
                             %a
                             (list->string %a)))
                        (append %res
                                (if (string? %a)
                                    (string->list %a)
                                    %a)))))
                 (%concatenate-loop (cdr %ee)))))
         %res))))
   (tex2page-string (get-token))
   (emit-link-stop)))

(define label-bound-p
 (lambda (label . %lambda-rest-arg)
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
             (if ext-file
                 (let ((%type 'string)
                       (%ee (list "{" ext-file " -> " label "}")))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))
                 label))
            false))))))

(define flag-unresolved-xref
 (lambda (xr)
   (let ((%push-added-value xr) (%push-old-stack *unresolved-xrefs*))
     (cond ((member %push-added-value %push-old-stack) %push-old-stack)
           (else
            (begin
             (set! *unresolved-xrefs* (cons %push-added-value %push-old-stack))
             *unresolved-xrefs*))))))

(define flag-missing-piece
 (lambda (mp)
   (let ((%push-added-value mp) (%push-old-stack *missing-pieces*))
     (cond ((member %push-added-value %push-old-stack) %push-old-stack)
           (else
            (begin
             (set! *missing-pieces* (cons %push-added-value %push-old-stack))
             *missing-pieces*))))))

(define show-unresolved-xrefs-and-missing-pieces
 (lambda ()
   (cond
    ((not (and (null? *unresolved-xrefs*) (null? *missing-pieces*)))
     (show-unresolved-xrefs) (show-missing-pieces)
     (write-log ':separation-newline) (write-log "Rerun: tex2page ")
     (write-log *main-tex-file*) (write-log ':separation-newline)
     (write-log "If problem persists, check for ")
     (write-log "missing \\label's and \\bibitem's"))
    (else false))))

(define show-unresolved-xrefs
 (lambda ()
   (cond
    ((not (null? *unresolved-xrefs*)) (write-log ':separation-newline)
     (write-log "Unresolved cross-reference")
     (cond
      ((>
        (let ((%length-arg *unresolved-xrefs*))
          ((if (string? %length-arg)
               string-length
               length)
           %length-arg))
        1)
       (write-log "s"))
      (else false))
     (write-log ": ")
     (begin (set! *unresolved-xrefs* (reverse *unresolved-xrefs*))
      *unresolved-xrefs*)
     (write-log (car *unresolved-xrefs*))
     (for-each
      (lambda (x) (write-log #\,) (write-log ':separation-space) (write-log x))
      (cdr *unresolved-xrefs*))
     (write-log ':separation-newline))
    (else false))))

(define show-missing-pieces
 (lambda ()
   (cond
    ((not (null? *missing-pieces*)) (write-log ':separation-newline)
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
      ((member ':fresh-index *missing-pieces*)
       (write-log "Index not refreshed") (write-log ':separation-newline))
      ((member ':index *missing-pieces*) (write-log "Index not included")
       (write-log ':separation-newline)))
     (cond
      ((member ':fresh-bibliography *missing-pieces*)
       (write-log "Bibliography not refreshed")
       (write-log ':separation-newline))
      ((member ':bibliography *missing-pieces*)
       (write-log "Bibliography not included")
       (write-log ':separation-newline)))
     (cond
      ((member ':metapost *missing-pieces*)
       (write-log "MetaPost output not included")
       (write-log ':separation-newline))
      (else false)))
    (else false))))

(define do-pageref
 (lambda ()
   (let ((label-ref (label-bound-p (get-peeled-group))))
     (if label-ref
         (let ((pageno (label*-page label-ref)))
           (begin
            (emit-ext-page-node-link-start (label*-src label-ref) pageno false)
            (emit pageno) (emit-link-stop)))
         (non-fatal-error "***")))))

(define do-htmlpageref
 (lambda ()
   (let ((label (get-peeled-group)))
     (let ((label-ref (label-bound-p label)))
       (begin (emit "\"")
        (if label-ref
            (emit
             (maybe-label-page (label*-src label-ref) (label*-page label-ref)))
            (emit *log-file*))
        (emit "\""))))))

(define doc-internal-url
 (lambda (url)
   (let ((n
          (let ((%length-arg url))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (cond
      ((and (> n 0) (char=? (string-ref url 0) #\#))
       (let ((label (subseq url 1 n)))
         (let ((label-ref (label-bound-p label)))
           (if label-ref
               (if (label*-src label-ref)
                   false
                   (list (label*-page label-ref) (label*-name label-ref)))
               false))))
      (true false)))))

(define fully-qualify-url
 (lambda (url)
   (let ((n
          (let ((%length-arg url))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (cond
      ((and (> n 0) (char=? (string-ref url 0) #\#))
       (let ((label (subseq url 1 n)))
         (let ((label-ref (label-bound-p label)))
           (if label-ref
               (let ((%type 'string)
                     (%ee
                      (list
                       (maybe-label-page (label*-src label-ref)
                        (label*-page label-ref))
                       "#" (label*-name label-ref))))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res))
               url))))
      ((fully-qualified-url-p url) url)
      (true (ensure-url-reachable url) url)))))

(define do-url
 (lambda ()
   (let ((url (get-url)))
     (begin
      (let ((durl (doc-internal-url url)))
        (if durl
            (emit-page-node-link-start (car durl) (cadr durl))
            (emit-link-start (fully-qualify-url url))))
      (emit url) (emit-link-stop)))))

(define do-mailto
 (lambda ()
   (let ((addr (get-url)))
     (begin
      (emit-link-start
       (let ((%type 'string) (%ee (list "mailto:" addr)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (emit addr) (emit-link-stop)))))

(define do-opmac-ulink
 (lambda ()
   (let ((url (get-bracketed-text-if-any)))
     (let ((link-text (get-group)))
       (let ((durl (doc-internal-url url)))
         (begin
          (if durl
              (emit-page-node-link-start (car durl) (cadr durl))
              (emit-link-start (fully-qualify-url url)))
          (bgroup) (tex2page-string link-text) (egroup) (emit-link-stop)))))))

(define do-urlh
 (lambda ()
   (let ((url (get-url)))
     (let ((durl (doc-internal-url url)))
       (if durl
           (emit-page-node-link-start (car durl) (cadr durl))
           (emit-link-start (fully-qualify-url url)))))
   (bgroup)
   (tex2page-string
    (let ((%type 'string)
          (%ee (list "\\def\\\\{\\egroup\\endinput}" (get-token))))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (egroup)
   (emit-link-stop)))

(define do-urlhd (lambda () (do-urlh) (get-token)))

(define do-urlp
 (lambda ()
   (let ((link-text (get-token)))
     (begin
      (let ((url (get-url)))
        (let ((durl (doc-internal-url url)))
          (if durl
              (emit-page-node-link-start (car durl) (cadr durl))
              (emit-link-start (fully-qualify-url url)))))
      (tex2page-string link-text) (emit-link-stop)))))

(define do-hlstart
 (lambda ()
   (let ((cat (get-peeled-group)))
     (let ((url (begin (get-token) (get-url))))
       (begin
        (cond
         ((string=? cat "url") (emit-link-start (fully-qualify-url url))
          (bgroup) (tex-let "\\hlend" "\\TIIPhlend" false))
         (else false))
        (ignorespaces))))))

(define do-hlend (lambda () (egroup) (emit-link-stop)))

(define do-htmladdimg
 (lambda ()
   (let ((align-info (get-bracketed-text-if-any)))
     (let ((url (fully-qualify-url (get-url))))
       (begin (emit "<img src=\"") (emit url) (emit "\" border=\"0\" ")
        (cond (align-info (tex2page-string align-info)) (else false))
        (emit " alt=\"[") (emit url) (emit "]\">"))))))

(define do-pdfximage
 (lambda ()
   (let ((height false) (width false))
     (begin
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (cond
             ((eat-word "height") (begin (set! height (get-pixels)) height))
             ((eat-word "width") (begin (set! width (get-pixels)) width))
             ((eat-word "depth") (get-pixels)) (true (return))))
          (if %loop-returned
              %loop-result
              (%loop))))
      (emit "<img")
      (cond (height (emit " height=") (emit height)) (else false))
      (cond (width (emit " width=") (emit width)) (else false))
      (emit " src=\"")
      (emit (fully-qualify-url (get-filename-possibly-braced))) (emit "\">")
      (ignorespaces) (get-ctl-seq) (ignorespaces) (get-ctl-seq)))))

(define do-cite-help
 (lambda (delim . %lambda-rest-arg)
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (extra-text false))
     (when (< 0 %lambda-rest-arg-len)
       (set! extra-text (list-ref %lambda-rest-arg 0)))
     (let ((closing-delim
            (cond ((char=? delim #\{) #\}) ((char=? delim #\[) #\])
                  (true (terror 'do-cite-help "faulty delim" delim)))))
       (begin (ignorespaces)
        (cond
         ((not (char=? (get-actual-char) delim))
          (terror 'do-cite "missing" delim))
         (else false))
        (emit "[")
        (let ((first-key-p true) (key false))
          (begin
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (unless %loop-returned
                 (begin (set! key (get-csv closing-delim)) key))
               (unless %loop-returned (cond ((not key) (return)) (else false)))
               (unless %loop-returned
                 (cond
                  (first-key-p (begin (set! first-key-p false) first-key-p))
                  (true (emit ",") (emit-nbsp 1))))
               (unless %loop-returned (write-bib-aux "\\citation{"))
               (unless %loop-returned (write-bib-aux key))
               (unless %loop-returned (write-bib-aux "}"))
               (unless %loop-returned (write-bib-aux #\newline))
               (unless %loop-returned
                 (do-ref-aux
                  (let ((%type 'string) (%ee (list "cite{" key "}")))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res))
                  false false))
               (if %loop-returned
                   %loop-result
                   (%loop))))
           (cond
            (extra-text (emit ",") (emit-nbsp 1) (tex2page-string extra-text))
            (else false))
           (cond
            ((not (char=? (get-actual-char) closing-delim))
             (terror 'do-cite "missing" closing-delim))
            (else false))
           (cond (first-key-p (terror 'do-cite "empty \\cite")) (else false))))
        (emit "]"))))))

(define do-cite
 (lambda ()
   (if (tex2page-flag-boolean "\\TZPopmac")
       (do-cite-help #\[ false)
       (do-cite-help #\{ (get-bracketed-text-if-any)))))

(define do-rcite (lambda () (do-cite-help #\[ false)))

(define do-nocite
 (lambda ()
   (ignorespaces)
   (let ((delim
          (if (tex2page-flag-boolean "\\TZPopmac")
              #\[
              #\{)))
     (let ((closing-delim
            (if (char=? delim #\{)
                #\}
                #\])))
       (begin
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
              (unless %loop-returned
                (begin (set! key (get-csv closing-delim)) key))
              (unless %loop-returned (cond ((not key) (return)) (else false)))
              (unless %loop-returned (write-bib-aux "\\citation{"))
              (unless %loop-returned (write-bib-aux key))
              (unless %loop-returned (write-bib-aux "}"))
              (unless %loop-returned
                (label-bound-p
                 (let ((%type 'string) (%ee (list "cite{" key "}")))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))))
              (unless %loop-returned (write-bib-aux #\newline))
              (if %loop-returned
                  %loop-result
                  (%loop)))))
        (cond
         ((not (char=? (get-actual-char) closing-delim))
          (terror 'do-nocite "missing" closing-delim))
         (else false)))))))

(define do-bibliographystyle
 (lambda () (do-bibliographystyle-help (ungroup (get-token)))))

(define do-bibliographystyle-help
 (lambda (s)
   (write-bib-aux "\\bibstyle{")
   (write-bib-aux s)
   (write-bib-aux "}")
   (write-bib-aux #\newline)))

(define do-bibliography
 (lambda () (do-bibliography-help (ungroup (get-token)))))

(define do-bibliography-help
 (lambda (bibdata)
   (begin (set! *using-bibliography-p* true) *using-bibliography-p*)
   (let ((bbl-file
          (let ((%type 'string)
                (%ee (list *aux-dir/* *jobname* *bib-aux-file-suffix* ".bbl")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (write-bib-aux "\\bibdata{") (write-bib-aux bibdata)
      (write-bib-aux "}") (write-bib-aux #\newline)
      (cond
       ((file-exists? bbl-file) (begin (set! *bibitem-num* 0) *bibitem-num*)
        (tex2page-file bbl-file) (emit-newline))
       (true (flag-missing-piece ':bibliography)
        (non-fatal-error "Bibliography not generated; rerun TeX2page")))))))

(define do-opmac-usebibtex
 (lambda ()
   (let ((bibfile (ungroup (get-token))))
     (let ((bibstyle (ungroup (get-token))))
       (begin (do-bibliographystyle-help bibstyle)
        (do-bibliography-help bibfile))))))

(define do-thebibliography
 (lambda ()
   (do-end-para)
   (get-group)
   (cond
    ((eq? *tex-format* ':latex)
     (tex2page-string
      (if *using-chapters-p*
          "\\chapter*{\\bibname}"
          "\\section*{\\refname}")))
    (else false))
   (bgroup)
   (begin (set! *bibitem-num* 0) *bibitem-num*)
   (tex2page-string "\\let\\em\\it")
   (tex2page-string "\\def\\newblock{ }")
   (emit "<table>")
   (emit-newline)))

(define do-bibitem
 (lambda ()
   (do-end-para)
   (let ((bibmark (get-bracketed-text-if-any)))
     (begin
      (cond ((not (= *bibitem-num* 0)) (emit "</td></tr>") (emit-newline))
            (else false))
      (let ((%tmp (+ *bibitem-num* 1)))
        (begin (set! *bibitem-num* %tmp) *bibitem-num*))
      (emit "<tr><td align=right valign=top>")
      (let ((bibitem-num-s (write-to-string *bibitem-num*)))
        (let ((key
               (let ((%type 'string)
                     (%ee (list "cite{" (get-peeled-group) "}")))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res))))
          (let ((node-name
                 (let ((%type 'string)
                       (%ee (list *html-node-prefix* "bib_" bibitem-num-s)))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))))
            (begin (tex-def-0arg "\\TIIPcurrentnodename" node-name)
             (cond ((not bibmark) (begin (set! bibmark bibitem-num-s) bibmark))
                   (else false))
             (tex-def-0arg "\\@currentlabel" bibmark) (emit-anchor node-name)
             (emit "[") (tex2page-string bibmark) (emit "]") (emit-nbsp 2)
             (do-label-aux key) (emit "</td><td>")))))))))

(define do-opmac-bib
 (lambda ()
   (do-para)
   (let ((%tmp (+ *bibitem-num* 1)))
     (begin (set! *bibitem-num* %tmp) *bibitem-num*))
   (let ((key0 (get-bracketed-text-if-any)))
     (let ((bibitem-num-s (write-to-string *bibitem-num*)))
       (let ((key
              (let ((%type 'string) (%ee (list "cite{" key0 "}")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (let ((node-name
                (let ((%type 'string)
                      (%ee (list *html-node-prefix* "bib_" bibitem-num-s)))
                  (let ((%res
                         (if (eq? %type 'string)
                             ""
                             null)))
                    (let %concatenate-loop
                      ((%ee %ee))
                      (if (null? %ee)
                          %res
                          (let ((%a (car %ee)))
                            (unless (not %a)
                              (set! %res
                               (if (eq? %type 'string)
                                   (string-append %res
                                    (if (string? %a)
                                        %a
                                        (list->string %a)))
                                   (append %res
                                           (if (string? %a)
                                               (string->list %a)
                                               %a)))))
                            (%concatenate-loop (cdr %ee)))))
                    %res))))
           (begin
            (cond ((not key0) (terror 'do-opmac-bib "Improper \\bib entry"))
                  (else false))
            (tex-def-0arg "\\TIIPcurrentnodename" node-name)
            (tex-def-0arg "\\@currentlabel" bibitem-num-s)
            (emit-anchor node-name) (emit "[") (tex2page-string bibitem-num-s)
            (emit "]") (emit-nbsp 2) (do-label-aux key))))))))

(define display-index-entry
 (lambda (s o)
   (for-each
    (lambda (c)
      (display
       (if (or (char=? c #\newline))
           #\
           c)
       o))
    (let ((%type 'list) (%ee (list s)))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))))

(define do-index-help
 (lambda (idx-entry)
   (let ((%tmp (+ *index-count* 2)))
     (begin (set! *index-count* %tmp) *index-count*))
   (!index *index-count* *html-page-count*)
   (write-aux `(!index ,*index-count* ,*html-page-count*))
   (let ((tag
          (let ((%type 'string)
                (%ee
                 (list *html-node-prefix* "index_"
                       (write-to-string *index-count*))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (emit-anchor tag)
      (cond
       ((not *index-port*)
        (let ((idx-file
               (let ((%type 'string)
                     (%ee
                      (list *aux-dir/* *jobname* *index-file-suffix* ".idx")))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res))))
          (begin
           (set! *index-port*
            (let* ((%f idx-file)
                   (%ee (list ':direction ':output ':if-exists ':supersede))
                   (%direction (memv ':direction %ee))
                   (%if-exists (memv ':if-exists %ee))
                   (%if-does-not-exist ':error)
                   (%if-does-not-exist-from-user
                    (memv ':if-does-not-exist %ee)))
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
           *index-port*)))
       (else false))
      (display "\\indexentry{" *index-port*)
      (cond
       ((or (substring? "|see{" idx-entry) (substring? "|seealso{" idx-entry))
        (display-index-entry idx-entry *index-port*))
       ((begin (set! *it* (substring? "|(" idx-entry)) *it*)
        (let ((i *it*))
          (begin (display-index-entry (subseq idx-entry 0 i) *index-port*)
           (display "|expandhtmlindex" *index-port*))))
       (true (display-index-entry idx-entry *index-port*)
        (display "|expandhtmlindex" *index-port*)))
      (display "}{" *index-port*) (display *index-count* *index-port*)
      (display "}" *index-port*) (newline *index-port*)))))

(define do-index
 (lambda ()
   (let ((idx-entry (ungroup (get-group))))
     (begin (ignorespaces)
      (cond ((not (substring? "|)" idx-entry)) (do-index-help idx-entry))
            (else false))))))

(define escape-opmac-index-entry
 (lambda (x)
   (let ((y null))
     (begin
      (let ((%dotimes-n
             (let ((%length-arg x))
               ((if (string? %length-arg)
                    string-length
                    length)
                %length-arg)))
            (i 0))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (unless %loop-returned
              (cond ((>= i %dotimes-n) (return)) (else false)))
            (unless %loop-returned
              (let ((c (string-ref x i)))
                (case c
                  ((#\")
                   (let ((%push-new-stack (cons c y)))
                     (begin (set! y %push-new-stack) y))
                   (let ((%push-new-stack (cons c y)))
                     (begin (set! y %push-new-stack) y)))
                  ((#\! #\@)
                   (let ((%push-new-stack (cons #\" y)))
                     (begin (set! y %push-new-stack) y))
                   (let ((%push-new-stack (cons c y)))
                     (begin (set! y %push-new-stack) y)))
                  (else
                   (let ((%push-new-stack (cons c y)))
                     (begin (set! y %push-new-stack) y))))))
            (unless %loop-returned (set! i (+ i 1)))
            (if %loop-returned
                %loop-result
                (%loop)))))
      (let ((%type 'string) (%ee (list (reverse y))))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))))))

(define do-opmac-ii
 (lambda (retainp)
   (let ((lhs (get-word)))
     (let ((sub
            (and *opmac-index-sub-table*
                 (table-get lhs *opmac-index-sub-table*))))
       (begin
        (if retainp
            (toss-back-string lhs)
            (ignorespaces))
        (do-index-help
         (cond (sub sub)
               (true
                (string=join
                 (map escape-opmac-index-entry (string=split lhs #\/))
                 #\!)))))))))

(define do-opmac-iis
 (lambda ()
   (let ((lhs (get-word)))
     (let ((rhs (get-peeled-group)))
       (let ((lhs-list (map escape-opmac-index-entry (string=split lhs #\/))))
         (let ((rhs-list (map escape-opmac-index-entry (string=split rhs #\/))))
           (let ((sub ""))
             (begin
              (cond
               ((not
                 (=
                  (let ((%length-arg lhs-list))
                    ((if (string? %length-arg)
                         string-length
                         length)
                     %length-arg))
                  (let ((%length-arg rhs-list))
                    ((if (string? %length-arg)
                         string-length
                         length)
                     %length-arg))))
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
                  (unless %loop-returned
                    (cond ((null? lhs-list) (return true)) (else false)))
                  (unless %loop-returned
                    (let ((additive
                           (let ((%type 'string)
                                 (%ee
                                  (list
                                   (let* ((%pop-old-stack lhs-list)
                                          (%pop-top-value (car %pop-old-stack)))
                                     (begin
                                      (set! lhs-list (cdr %pop-old-stack))
                                      lhs-list)
                                     %pop-top-value)
                                   "@"
                                   (let* ((%pop-old-stack rhs-list)
                                          (%pop-top-value (car %pop-old-stack)))
                                     (begin
                                      (set! rhs-list (cdr %pop-old-stack))
                                      rhs-list)
                                     %pop-top-value))))
                             (let ((%res
                                    (if (eq? %type 'string)
                                        ""
                                        null)))
                               (let %concatenate-loop
                                 ((%ee %ee))
                                 (if (null? %ee)
                                     %res
                                     (let ((%a (car %ee)))
                                       (unless (not %a)
                                         (set! %res
                                          (if (eq? %type 'string)
                                              (string-append %res
                                               (if (string? %a)
                                                   %a
                                                   (list->string %a)))
                                              (append %res
                                                      (if (string? %a)
                                                          (string->list %a)
                                                          %a)))))
                                       (%concatenate-loop (cdr %ee)))))
                               %res))))
                      (begin
                       (set! sub
                        (cond ((string=? sub "") additive)
                              (true
                               (let ((%type 'string)
                                     (%ee (list sub "!" additive)))
                                 (let ((%res
                                        (if (eq? %type 'string)
                                            ""
                                            null)))
                                   (let %concatenate-loop
                                     ((%ee %ee))
                                     (if (null? %ee)
                                         %res
                                         (let ((%a (car %ee)))
                                           (unless (not %a)
                                             (set! %res
                                              (if (eq? %type 'string)
                                                  (string-append %res
                                                   (if (string? %a)
                                                       %a
                                                       (list->string %a)))
                                                  (append %res
                                                          (if (string? %a)
                                                              (string->list %a)
                                                              %a)))))
                                           (%concatenate-loop (cdr %ee)))))
                                   %res)))))
                       sub)))
                  (if %loop-returned
                      %loop-result
                      (%loop))))
              (cond
               ((not *opmac-index-sub-table*)
                (flag-missing-piece ':fresh-index))
               (else false))
              (!opmac-iis lhs sub) (write-aux `(!opmac-iis ,lhs ,sub))))))))))

(define do-inputindex
 (lambda %lambda-rest-arg
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
         (insert-heading-p false))
     (when (< 0 %lambda-rest-arg-len)
       (set! insert-heading-p (list-ref %lambda-rest-arg 0)))
     (begin (set! *using-index-p* true) *using-index-p*)
     (cond
      (insert-heading-p
       (tex2page-string
        (if *using-chapters-p*
            "\\chapter*{\\indexname}"
            "\\section*{\\indexname}"))
       (emit-newline))
      (else false))
     (emit-anchor
      (let ((%type 'string) (%ee (list *html-node-prefix* "index_start")))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res)))
     (!index-page *html-page-count*)
     (write-aux `(!index-page ,*html-page-count*))
     (let ((ind-file
            (let ((%type 'string)
                  (%ee (list *aux-dir/* *jobname* *index-file-suffix* ".ind")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (cond ((file-exists? ind-file) (tex2page-file ind-file))
             (true (flag-missing-piece ':index)
              (non-fatal-error "Index not generated; rerun TeX2page")))))))

(define do-theindex
 (lambda ()
   (bgroup)
   (tex2page-string "\\let\\endtheindex\\egroup")
   (tex2page-string "\\let\\indexspace\\relax")
   (tex2page-string "\\let\\item\\indexitem")
   (tex2page-string "\\let\\subitem\\indexsubitem")
   (tex2page-string "\\let\\subsubitem\\indexsubsubitem")
   (tex2page-string "\\let\\(\\expandhtmlindex")))

(define expand-html-index
 (lambda ()
   (let ((s (get-peeled-group)))
     (let ((n (string->number s)))
       (let ((pageno (table-get n *index-table*)))
         (begin
          (emit-page-node-link-start pageno
           (let ((%type 'string) (%ee (list *html-node-prefix* "index_" s)))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res)))
          (emit pageno)
          (cond
           ((begin (set! *it* (table-get pageno *index-page-mention-alist*))
             *it*)
            (let ((n (add1 *it*)))
              (begin (emit (number-to-roman n))
               (begin (table-put! pageno *index-page-mention-alist* n)
                (table-get pageno *index-page-mention-alist*)))))
           (true
            (begin (table-put! pageno *index-page-mention-alist* 1)
             (table-get pageno *index-page-mention-alist*))))
          (emit-link-stop)))))))

(define do-see-also
 (lambda ()
   (let ((other-entry (get-group)))
     (begin (get-group) (emit "<em>see also</em> ")
      (tex2page-string other-entry)))))

(define do-setbox
 (lambda ()
   (get-raw-token/is)
   (get-equal-sign)
   (let ((cs (get-raw-token/is)))
     (cond
      ((member cs '("\\hbox" "\\vbox")) (get-to) (eat-dimen)
       (get-token-or-peeled-group))
      (else false)))))

(define html-length
 (lambda (s)
   (let ((n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg)))
         (res 0)
         (i 0)
         (skip-tag false)
         (skip-entity false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (cond ((>= i n) (return res)) (else false)))
         (unless %loop-returned
           (let ((c (string-ref s i)))
             (begin
              (let ((%tmp (+ i 1)))
                (begin (set! i %tmp) i))
              (cond
               (skip-tag
                (cond ((char=? c #\>) (begin (set! skip-tag false) skip-tag))
                      (else false)))
               (skip-entity
                (cond
                 ((char=? c #\;) (begin (set! skip-entity false) skip-entity))
                 (else false)))
               ((char=? c #\<) (begin (set! skip-tag true) skip-tag))
               ((char=? c #\&)
                (let ((%tmp (+ res 1)))
                  (begin (set! res %tmp) res))
                (begin (set! skip-entity true) skip-entity))
               (true
                (let ((%tmp (+ res 1)))
                  (begin (set! res %tmp) res)))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-llap
 (lambda ()
   (let ((txt (tex-string-to-html-string (get-group))))
     (let ((html-len (html-length txt)))
       (let ((txt-len (sp-to-pixels (tex-length html-len ':ex))))
         (begin (emit "<span style=\"position: relative\">")
          (emit "<span style=\"position: absolute; left: -") (emit txt-len)
          (emit "pt\">") (emit txt) (emit "</span></span>")))))))

(define do-indexitem
 (lambda (indent)
   (begin (set! *index-page-mention-alist* (make-table))
    *index-page-mention-alist*)
   (emit "<br>")
   (emit-newline)
   (emit-nbsp (* indent 4))))

(define do-description-item
 (lambda ()
   (do-end-para)
   (emit "</dd><dt>")
   (let ((thing (get-bracketed-text-if-any)))
     (cond
      (thing (begin (set! thing (string-trim thing)) thing)
       (cond
        ((not (string=? thing "")) (bgroup) (emit "<b>")
         (tex2page-string thing) (emit "</b>") (egroup))
        (else false)))
      (else false)))
   (emit "</dt><dd>")))

(define do-regular-item
 (lambda ()
   (do-end-para)
   (emit "<li>")
   (do-para)
   (let ((thing (get-bracketed-text-if-any)))
     (cond
      (thing (emit "<b>") (bgroup) (tex2page-string thing) (egroup)
       (emit "</b>") (emit-nbsp 2))
      (else false)))))

(define do-plain-item
 (lambda (n)
   (do-end-para)
   (emit-newline)
   (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
     (begin (emit "<p style=\"margin-left: ") (emit (* n parindent))
      (emit "pt; text-indent: 0pt\">") (emit "<span style=\"margin-left: ")
      (emit parindent) (emit "pt\"></span>")
      (emit "<span style=\"position: relative\">")
      (emit "<span class=item style=\"position: absolute; left: -")
      (emit parindent) (emit "pt\">") (tex2page-string (get-group))
      (ignorespaces) (emit-nbsp 2) (emit "</span></span>")))))

(define do-textindent
 (lambda ()
   (let ((parindent (sp-to-pixels (find-dimen "\\parindent"))))
     (begin (do-noindent) (emit "<span style=\"margin-left: ") (emit parindent)
      (emit "pt\"></span>") (emit "<span style=\"position: relative\">")
      (emit "<span class=item style=\"position: absolute; left: -")
      (emit parindent) (emit "pt\">") (tex2page-string (get-group))
      (ignorespaces) (emit-nbsp 2) (emit "</span></span>")))))

(define do-proclaim
 (lambda ()
   (let ((head (tex-string-to-html-string (get-till-char #\.))))
     (let ((body
            (begin (get-actual-char) (ignorespaces)
             (tex-string-to-html-string (get-till-par)))))
       (begin (do-end-para) (emit "<div class=\"proclaim medskip\"><b>")
        (do-noindent) (emit head) (emit ".</b>") (emit-nbsp 2) (emit "<i>")
        (emit body) (emit "</i>") (do-end-para) (emit "</div>") (do-para))))))

(define do-item
 (lambda ()
   (case (car *tabular-stack*)
     ((:description) (do-description-item))
     ((:itemize :enumerate) (do-regular-item))
     (else (do-plain-item 1)))))

(define do-itemize
 (lambda ()
   (do-end-para)
   (let ((%push-new-stack (cons ':itemize *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (emit "<ul")
   (cond ((tex2page-flag-boolean "\\TZPslides") (emit " class=incremental"))
         (else false))
   (emit ">")
   (emit-newline)))

(define do-enditemize
 (lambda ()
   (do-end-para)
   (pop-tabular-stack ':itemize)
   (emit "</ul>")
   (do-noindent)))

(define do-enumerate
 (lambda ()
   (do-end-para)
   (let ((%push-new-stack (cons ':enumerate *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (emit "<ol")
   (cond ((tex2page-flag-boolean "\\TZPslides") (emit " class=incremental"))
         (else false))
   (emit ">")
   (emit-newline)))

(define do-endenumerate
 (lambda ()
   (pop-tabular-stack ':enumerate)
   (do-end-para)
   (emit "</ol>")
   (do-noindent)))

(define do-opmac-list-style
 (lambda ()
   (ignorespaces)
   (begin (set! *opmac-list-style* (get-actual-char)) *opmac-list-style*)))

(define do-opmac-begitems
 (lambda ()
   (do-end-para)
   (bgroup)
   (tex-def-count "\\TIIPopmacliststarted" 0 false)
   (activate-cdef #\*)
   (tex-def-char #\* null "\\TIIPopmacitem" false)))

(define do-opmac-item
 (lambda ()
   (cond
    ((= (find-count "\\TIIPopmacliststarted") 0)
     (tex-def-count "\\TIIPopmacliststarted" 1 false)
     (let ((%push-new-stack (cons ':opmac-itemize *tabular-stack*)))
       (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
     (emit "<")
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
   (do-regular-item)))

(define do-opmac-enditems
 (lambda ()
   (egroup)
   (do-end-para)
   (pop-tabular-stack ':opmac-itemize)
   (emit "</")
   (emit (case *opmac-list-style* ((#\o #\- #\x #\X) "u") (else "o")))
   (emit "l>")
   (do-noindent)))

(define do-bigskip
 (lambda (type)
   (do-end-para)
   (emit "<div class=")
   (emit
    (case type
      ((:medskip) "medskip")
      ((:bigskip) "bigskip")
      (else "smallskip")))
   (emit "></div>")
   (emit-newline)
   (emit "<p style=\"margin-top: 0pt; margin-bottom: 0pt\">")
   (begin (set! *in-para-p* true) *in-para-p*)
   (emit-newline)))

(define do-hspace
 (lambda ()
   (ignorespaces)
   (cond ((eqv? (snoop-actual-char) #\*) (get-actual-char)) (else false))
   (get-group)
   (emit-nbsp 3)))

(define do-vspace
 (lambda ()
   (ignorespaces)
   (cond ((eqv? (snoop-actual-char) #\*) (get-actual-char)) (else false))
   (get-group)
   (do-bigskip ':vspace)))

(define do-htmlmathstyle
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin (cond ((eq? c ':eof-object) (return)) (else false))
               (case
                   (string->symbol
                    (string-append ":" (string-upcase (scm-get-token))))
                 ((:image :display-image)
                  (tex-def-0arg "\\TZPmathtext" "0")
                  (tex-def-0arg "\\TZPmathimage" "1"))
                 ((:no-image :no-display-image)
                  (tex-def-0arg "\\TZPmathtext" "1")
                  (tex-def-0arg "\\TZPmathimage" "0"))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-htmldoctype
 (lambda ()
   (let ((d (get-peeled-group)))
     (begin (cond ((string=? d "") (begin (set! d 'none) d)) (else false))
      (write-aux `(!doctype ,d))))))

(define do-htmlcolophon
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin (cond ((eq? c ':eof-object) (return)) (else false))
               (let ((directive
                      (string->symbol
                       (string-append ":" (string-upcase (scm-get-token))))))
                 (begin (!colophon directive)
                  (write-aux `(!colophon ,directive)))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define output-colophon
 (lambda ()
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
       (do-end-para) (emit "<div align=right class=colophon>")
       (cond
        ((and colophon-mentions-last-mod-time-p *last-modification-time*
              (> *last-modification-time* 0))
         (tex2page-string *last-modified*) (emit ": ")
         (emit (seconds-to-human-time *last-modification-time*)) (emit "<br>"))
        (else false))
       (cond
        (colophon-mentions-tex2page-p
         (emit "<div align=right class=advertisement>")
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
      (else false)))))

(define point-to-adjacent-pages
 (lambda ()
   (let ((prev-page
          (if (= *html-page-count* 0)
              false
              (- *html-page-count* 1))))
     (let ((next-page
            (if (= *html-page-count* *last-page-number*)
                false
                (+ *html-page-count* 1))))
       (cond
        ((not (= *last-page-number* 0))
         (cond (prev-page (emit-page-link-start prev-page)) (else false))
         (emit "&#x3c;&#xb7;&#xb7;&#xb7;Prev ")
         (cond (prev-page (emit-link-stop)) (else false)) (emit "||")
         (cond (next-page (emit-page-link-start next-page)) (else false))
         (emit " Next&#xb7;&#xb7;&#xb7;&#x3e;")
         (cond (next-page (emit-link-stop)) (else false)))
        (else false))))))

(define output-head-or-foot-line
 (lambda (head-or-foot)
   (cond
    ((not (tex2page-flag-boolean "\\TZPsinglepage"))
     (emit "<div align=right class=navigation>")
     (cond
      ((or *tex-like-layout-p*
           (and (eq? head-or-foot ':foot)
                (tex2page-flag-boolean "\\TZPtexlayout")))
       (bgroup) (tex-let "\\folio" "\\TIIPfolio" false)
       (tex2page-string
        (if (eq? head-or-foot ':head)
            "\\the\\headline"
            "\\the\\footline"))
       (egroup))
      (true (output-navigation-bar head-or-foot)))
     (emit "</div>") (emit-newline))
    (else false))))

(define output-navigation-bar
 (lambda (head-or-foot)
   (let ((first-page-p (= *html-page-count* 0)))
     (let ((last-page-not-determined-p (< *last-page-number* 0)))
       (let ((last-page-p (= *html-page-count* *last-page-number*)))
         (let ((toc-page-p (and *toc-page* (= *html-page-count* *toc-page*))))
           (let ((index-page-p
                  (and *index-page* (= *html-page-count* *index-page*))))
             (let ((prev-page
                    (if first-page-p
                        false
                        (- *html-page-count* 1))))
               (let ((next-page
                      (if last-page-p
                          false
                          (+ *html-page-count* 1))))
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
                      ((or
                        (and toc-page-p (not *index-page*) (not index-page-p))
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
                          (let ((%type 'string)
                                (%ee (list *html-node-prefix* "toc")))
                            (let ((%res
                                   (if (eq? %type 'string)
                                       ""
                                       null)))
                              (let %concatenate-loop
                                ((%ee %ee))
                                (if (null? %ee)
                                    %res
                                    (let ((%a (car %ee)))
                                      (unless (not %a)
                                        (set! %res
                                         (if (eq? %type 'string)
                                             (string-append %res
                                              (if (string? %a)
                                                  %a
                                                  (list->string %a)))
                                             (append %res
                                                     (if (string? %a)
                                                         (string->list %a)
                                                         %a)))))
                                      (%concatenate-loop (cdr %ee)))))
                              %res))))
                        (else false))
                       (emit *navigation-contents-name*)
                       (cond ((not toc-page-p) (emit-link-stop)) (else false))
                       (emit "</span>"))
                      (else false))
                     (cond
                      (*index-page* (emit "<span")
                       (cond (index-page-p (emit " class=disable"))
                             (else false))
                       (emit ">") (emit "<span")
                       (cond
                        ((not (and *toc-page* (not toc-page-p)))
                         (emit " class=disable"))
                        (else false))
                       (emit ">")
                       (cond (*toc-page* (emit "; ") (emit-nbsp 2))
                             (else false))
                       (emit "</span>")
                       (cond
                        ((not index-page-p)
                         (emit-page-node-link-start *index-page*
                          (let ((%type 'string)
                                (%ee (list *html-node-prefix* "index_start")))
                            (let ((%res
                                   (if (eq? %type 'string)
                                       ""
                                       null)))
                              (let %concatenate-loop
                                ((%ee %ee))
                                (if (null? %ee)
                                    %res
                                    (let ((%a (car %ee)))
                                      (unless (not %a)
                                        (set! %res
                                         (if (eq? %type 'string)
                                             (string-append %res
                                              (if (string? %a)
                                                  %a
                                                  (list->string %a)))
                                             (append %res
                                                     (if (string? %a)
                                                         (string->list %a)
                                                         %a)))))
                                      (%concatenate-loop (cdr %ee)))))
                              %res))))
                        (else false))
                       (emit *navigation-index-name*)
                       (cond ((not index-page-p) (emit-link-stop))
                             (else false))
                       (emit "</span>"))
                      (else false)))
                    (else false))
                   (emit *navigation-sentence-end*) (emit "]"))
                  (else false)))))))))))

(define do-eject
 (lambda ()
   (cond
    ((tex2page-flag-boolean "\\TZPslides") (do-end-para) (emit "</div>")
     (emit-newline) (emit "<div class=slide>") (do-para))
    ((tex2page-flag-boolean "\\TZPsinglepage") true)
    (true
     (cond
      ((not
        (and (eq? (snoop-actual-char) ':eof-object)
             (eqv? *current-source-file* *main-tex-file*)))
       (cond
        ((not (> *last-page-number* 0))
         (flag-missing-piece ':last-modification-time))
        (else false))
       (do-end-page)
       (let ((%tmp (+ *html-page-count* 1)))
         (begin (set! *html-page-count* %tmp) *html-page-count*))
       (begin
        (set! *html-page*
         (let ((%type 'string)
               (%ee
                (list *aux-dir/* *jobname* *html-page-suffix*
                      (write-to-string *html-page-count*) *output-extension*)))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res)))
        *html-page*)
       (begin
        (set! *html*
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
            (else (open-input-file %f)))))
        *html*)
       (do-start))
      (else false))))))

(define output-html-preamble
 (lambda ()
   (cond
    ((string? *doctype*) (emit "<!DOCTYPE ") (emit *doctype*) (emit ">")
     (emit-newline))
    (else false))
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
   (emit *scheme-version*)
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
   (for-each emit *html-head*)
   (emit "</head>")
   (emit-newline)
   (emit "<body>")
   (emit-newline)
   (emit "<div")
   (cond ((tex2page-flag-boolean "\\TZPslides") (emit " class=slide"))
         (else false))
   (emit ">")
   (emit-newline)))

(define output-html-postamble
 (lambda ()
   (do-end-para)
   (emit "</div>")
   (emit-newline)
   (emit "</body>")
   (emit-newline)
   (emit "</html>")
   (emit-newline)))

(define redirect-if-needed
 (lambda ()
   (cond
    (*redirect-url* (emit "If not redirected in ") (emit *redirect-delay*)
     (emit " sec, go to ") (emit-link-start (fully-qualify-url *redirect-url*))
     (emit *redirect-url*) (emit-link-stop))
    (else false))))

(define check-tex2page-lisp
 (lambda ()
   (let ((cl-p (not null))
         (doc-expects-cl-p (tex2page-flag-boolean "\\TZPcommonlisp")))
     (cond
      ((not (eqv? cl-p doc-expects-cl-p)) (write-log ':separation-newline)
       (write-log "! Document ") (write-log *main-tex-file*)
       (write-log " appears to require ")
       (write-log
        (if doc-expects-cl-p
            "Common Lisp"
            "Scheme"))
       (write-log " version of TeX2page."))
      (else false)))))

(define do-start
 (lambda ()
   (check-tex2page-lisp)
   (begin (set! *footnote-list* null) *footnote-list*)
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
   (let ((colophon-on-last-page-p
          (tex2page-flag-boolean "\\TZPcolophonlastpage")))
     (cond
      ((or (and (not colophon-on-last-page-p) (= *html-page-count* 0))
           (and colophon-on-last-page-p
                (= *html-page-count* *last-page-number*)))
       (output-colophon))
      (else false)))
   (output-html-postamble)
   (write-log #\[)
   (write-log *html-page-count*)
   (write-log #\])
   (write-log ':separation-space)
   (let ((%close-port-arg *html*))
     ((if (input-port? %close-port-arg)
          close-input-port
          close-output-port)
      %close-port-arg))))

(define close-all-open-ports
 (lambda ()
   (cond
    (*aux-port*
     (let ((%close-port-arg *aux-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (cond
    (*css-port*
     (let ((%close-port-arg *css-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (cond
    (*index-port*
     (let ((%close-port-arg *index-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (cond
    (*label-port*
     (let ((%close-port-arg *label-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (cond
    (*bib-aux-port*
     (let ((%close-port-arg *bib-aux-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (cond
    (*verb-port*
     (let ((%close-port-arg *verb-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (table-for-each
    (lambda (k v)
      false
      (cond
       ((not (eq? v ':free))
        (let ((%close-port-arg v))
          ((if (input-port? %close-port-arg)
               close-input-port
               close-output-port)
           %close-port-arg)))
       (else false)))
    *input-streams*)
   (table-for-each
    (lambda (k v)
      false
      (cond
       ((not (eq? v ':free))
        (let ((%close-port-arg v))
          ((if (input-port? %close-port-arg)
               close-input-port
               close-output-port)
           %close-port-arg)))
       (else false)))
    *output-streams*)))

(define output-stats
 (lambda ()
   (write-log ':separation-newline)
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
    (true (write-log "No pages of output.")))
   (write-log #\newline)
   (cond
    (*log-port*
     (let ((%close-port-arg *log-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
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
   (cond
    ((not (null? *tex-if-stack*))
     (let ((n
            (let ((%length-arg *tex-if-stack*))
              ((if (string? %length-arg)
                   string-length
                   length)
               %length-arg))))
       (trace-if true "(\\end occurred when " n " \\if"
        (if (> n 1)
            "s were"
            " was")
        " incomplete)")))
    (else false))
   (cond
    ((not (null? *tex-env*))
     (trace-if true "\\end occurred inside a group at level "
      (let ((%length-arg *tex-env*))
        ((if (string? %length-arg)
             string-length
             length)
         %length-arg))))
    (else false))
   (perform-postludes)
   (cond
    ((not (or (>= *last-page-number* 0) (= *html-page-count* 0)))
     (flag-missing-piece ':last-page))
    (else false))
   (!last-page-number *html-page-count*)
   (write-aux `(!last-page-number ,*last-page-number*))
   (do-end-page)
   (cond
    (*last-modification-time*
     (write-aux `(!last-modification-time ,*last-modification-time* 1900)))
    (else false))
   (for-each (lambda (th) (th)) *afterbye*)
   (close-all-open-ports)
   (call-external-programs-if-necessary)
   (show-unresolved-xrefs-and-missing-pieces)))

(define set-text-width
 (lambda ()
   (let ((hsize
          (cond
           ((begin (set! *it* (find-def "\\TZPhsize")) *it*)
            (tex2page-string
             (let ((%type 'string)
                   (%ee (list "\\TIIPhsize=" (tdef*-expansion *it*))))
               (let ((%res
                      (if (eq? %type 'string)
                          ""
                          null)))
                 (let %concatenate-loop
                   ((%ee %ee))
                   (if (null? %ee)
                       %res
                       (let ((%a (car %ee)))
                         (unless (not %a)
                           (set! %res
                            (if (eq? %type 'string)
                                (string-append %res
                                 (if (string? %a)
                                     %a
                                     (list->string %a)))
                                (append %res
                                        (if (string? %a)
                                            (string->list %a)
                                            %a)))))
                         (%concatenate-loop (cdr %ee)))))
                 %res)))
            (find-dimen "\\TIIPhsize"))
           (*tex-like-layout-p* (find-dimen "\\hsize")) (true false))))
     (cond
      (hsize (display "body { max-width: " *css-port*)
       (display (sp-to-pixels hsize) *css-port*) (display "pt; }" *css-port*)
       (newline *css-port*))
      (else false)))))

(define note-down-tex2page-flags
 (lambda ()
   (write-aux `(!head-line ,(get-toks "\\headline")))
   (write-aux `(!foot-line ,(get-toks "\\footline")))
   (cond
    ((begin (set! *it* (find-def "\\TZPtitle")) *it*)
     (let ((d *it*))
       (write-aux
        `(!preferred-title ,(tex-string-to-html-string (tdef*-expansion d))))))
    (else false))
   (cond
    ((tex2page-flag-boolean "\\TZPcolophonlastpage")
     (write-aux `(!colophon ':last-page)))
    (else false))
   (cond
    ((or (tex2page-flag-boolean "\\TZPcolophondisabletimestamp")
         (not (tex2page-flag-boolean "\\TZPcolophontimestamp")))
     (write-aux `(!colophon ':no-timestamp)))
    (else false))
   (cond
    ((or (tex2page-flag-boolean "\\TZPcolophondisablecredit")
         (not (tex2page-flag-boolean "\\TZPcolophoncredit")))
     (write-aux `(!colophon ':dont-credit-tex2page)))
    (else false))
   (cond
    ((or (tex2page-flag-boolean "\\TZPcolophondisableweblink")
         (not (tex2page-flag-boolean "\\TZPcolophonweblink")))
     (write-aux `(!colophon ':dont-link-to-tex2page-website)))
    (else false))
   (cond
    ((begin (set! *it* (ctl-seq-no-arg-expand-once "\\TZPredirect")) *it*)
     (cond ((not *redirect-url*) (flag-missing-piece ':html-head))
           (else false))
     (let ((url *it*)
           (seconds (ctl-seq-no-arg-expand-once "\\TZPredirectseconds")))
       (write-aux `(!html-redirect ,url ,seconds))))
    (else false))
   (cond
    ((tex2page-flag-boolean "\\TZPslides") (write-aux '(!slides))
     (write-aux '(!single-page))
     (let ((slidy-css-file "slidy.css"))
       (begin
        (cond
         ((not (file-exists? slidy-css-file))
          (begin
           (set! slidy-css-file
            "http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css")
           slidy-css-file))
         (else false))
        (cond ((null? *stylesheets*) (flag-missing-piece ':stylesheets))
              (else false))
        (write-aux `(!stylesheet ,slidy-css-file))))
     (let ((slidy-js-file "slidy.js"))
       (begin
        (cond
         ((not (file-exists? slidy-js-file))
          (begin
           (set! slidy-js-file
            "http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js")
           slidy-js-file))
         (else false))
        (cond ((null? *scripts*) (flag-missing-piece ':scripts)) (else false))
        (write-aux `(!script ,slidy-js-file)))))
    (else false))
   (cond
    ((tex2page-flag-boolean "\\TZPsinglepage") (write-aux '(!single-page)))
    (else false))
   (cond
    ((tex2page-flag-boolean "\\TZPtexlayout")
     (begin (set! *tex-like-layout-p* true) *tex-like-layout-p*)
     (write-aux '(!tex-like-layout)) (newline *css-port*)
     (display "body { margin-top: " *css-port*)
     (display (sp-to-ems (+ (tex-length 0.5 ':in) (find-dimen "\\voffset")))
      *css-port*)
     (display "em; }" *css-port*) (newline *css-port*)
     (display "body { margin-left: " *css-port*)
     (display (sp-to-ems (+ (tex-length 0.8 ':in) (find-dimen "\\hoffset")))
      *css-port*)
     (display "em; }" *css-port*) (newline *css-port*)
     (cond
      ((or (tex2page-flag-boolean "\\TZPrightjustify")
           (not (tex2page-flag-boolean "\\TZPraggedright")))
       (display "body { text-align: justify; }" *css-port*)
       (newline *css-port*))
      (else false))
     (display "p { margin-bottom: 0pt; }" *css-port*) (newline *css-port*)
     (display "p { text-indent: " *css-port*)
     (display (sp-to-pixels (find-dimen "\\parindent")) *css-port*)
     (display "pt; }" *css-port*) (newline *css-port*)
     (display "p { margin-top: " *css-port*)
     (display (sp-to-pixels (find-dimen "\\parskip")) *css-port*)
     (display "pt; }" *css-port*) (newline *css-port*)
     (display ".mathdisplay { margin-top: " *css-port*)
     (display (sp-to-pixels (find-dimen "\\abovedisplayskip")) *css-port*)
     (display "pt; margin-bottom: " *css-port*)
     (display (sp-to-pixels (find-dimen "\\belowdisplayskip")) *css-port*)
     (display "pt; }" *css-port*) (newline *css-port*)
     (display ".navigation { color: black; font-style: normal; }" *css-port*)
     (newline *css-port*))
    (else false))
   (set-text-width)
   (cond
    ((not (tex2page-flag-boolean "\\TZPtextext")) (write-aux `(!tex-text 1)))
    (else false))))

(define insert-missing-end
 (lambda ()
   (write-log ':separation-newline)
   (write-log "! Missing \\end inserted.")
   (write-log ':separation-newline)))

(define do-diacritic
 (let ((top-diacritics
        '(:grave :acute :circumflex :umlaut :tilde :macron :breve :hacek
          :hungarianumlaut :ring)))
   (lambda (diac)
     (let ((x (get-token-or-peeled-group)))
       (cond ((and (string=? x "\\i") (member diac top-diacritics)) (emit "i"))
             ((and (string=? x "\\j") (member diac top-diacritics)) (emit "j"))
             (true (tex2page-string x))))
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

(define do-mathdg
 (lambda ()
   (let ((%fluid-var-*math-mode-p* true)
         (%fluid-var-*in-display-math-p* true)
         (%fluid-var-*tabular-stack* null)
         (%fluid-var-*ligatures-p* false))
     (fluid-let
      ((*ligatures-p* %fluid-var-*ligatures-p*)
       (*tabular-stack* %fluid-var-*tabular-stack*)
       (*in-display-math-p* %fluid-var-*in-display-math-p*)
       (*math-mode-p* %fluid-var-*math-mode-p*))
      (do-end-para) (emit "<div align=") (emit *display-justification*)
      (emit "><table><tr><td>") (tex2page-string (get-group))
      (emit "</td></tr></table></div>") (do-para)))))

(define do-mathg
 (lambda ()
   (let ((%fluid-var-*math-mode-p* true)
         (%fluid-var-*in-display-math-p* false)
         (%fluid-var-*tabular-stack* null)
         (%fluid-var-*ligatures-p* false))
     (fluid-let
      ((*ligatures-p* %fluid-var-*ligatures-p*)
       (*tabular-stack* %fluid-var-*tabular-stack*)
       (*in-display-math-p* %fluid-var-*in-display-math-p*)
       (*math-mode-p* %fluid-var-*math-mode-p*))
      (tex2page-string (get-group))))))

(define dump-tex-preamble
 (lambda (o)
   (case *tex-format*
     ((:latex)
      (display "\\documentclass{" o)
      (display
       (if *using-chapters-p*
           "report"
           "article")
       o)
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
        (display "\\ifx\\pdfoutput\\UNDEFINED\\input epsf \\else" o)
        (display "\\input supp-pdf " o)
        (display "\\def\\epsfbox#1{\\convertMPtoPDF{#1}{1}{1}}\\fi" o)
        (newline o))
       (else false))
      (display "\\nopagenumbers" o)
      (newline o)))))

(define dump-tex-postamble
 (lambda (o)
   (case *tex-format*
     ((:latex) (display "\\end{document}" o) (newline o))
     (else (display "\\bye" o) (newline o)))))

(define skipping-img-file
 (lambda ()
   (let ((%tmp (+ *img-file-count* 1)))
     (begin (set! *img-file-count* %tmp) *img-file-count*))))

(define next-html-image-file-stem
 (lambda ()
   (let ((%tmp (+ *img-file-count* 1)))
     (begin (set! *img-file-count* %tmp) *img-file-count*))
   (let ((%type 'string)
         (%ee
          (list *subjobname* *img-file-suffix*
                (write-to-string *img-file-count*))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define call-with-html-image-port
 (lambda (p . %lambda-rest-arg)
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (alt false))
     (when (< 0 %lambda-rest-arg-len) (set! alt (list-ref %lambda-rest-arg 0)))
     (let ((img-file-stem (next-html-image-file-stem)))
       (let ((aux-tex-file
              (let ((%type 'string) (%ee (list img-file-stem ".tex")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (begin
          (let ((o
                 (let* ((%f aux-tex-file)
                        (%ee
                         (list ':direction ':output ':if-exists ':supersede))
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
                     (when
                         (and (eqv? %if-exists ':supersede) (file-exists? %f))
                       (delete-file %f))
                     (open-output-file %f))
                    ((and (not %if-does-not-exist) (not (file-exists? %f)))
                     false)
                    (else (open-input-file %f))))))
            (let ((%with-open-file-res
                   (begin (dump-tex-preamble o) (p o) (dump-tex-postamble o))))
              (begin
               (cond
                (o
                 ((if (input-port? o)
                      close-input-port
                      close-output-port)
                  o))
                (else false))
               %with-open-file-res)))
          (tex-to-img img-file-stem) (source-img-file img-file-stem alt)))))))

(define do-math-fragment
 (lambda (s . %lambda-rest-arg)
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (display-p false))
     (when (< 0 %lambda-rest-arg-len)
       (set! display-p (list-ref %lambda-rest-arg 0)))
     (cond
      (display-p (emit "<div class=mathdisplay align=")
       (emit *display-justification*) (emit ">"))
      (else false))
     (let ((old-math-mode-p *math-mode-p*)
           (old-in-display-math-p *in-display-math-p*)
           (old-tabular-stack *tabular-stack*))
       (begin
        (begin (set! *math-mode-p* true) (set! *in-display-math-p* display-p)
         (set! *tabular-stack* null) *tabular-stack*)
        (cond (display-p (emit "<table><tr><td>")) (else false)) (bgroup)
        (toss-back-char #\}) (toss-back-string s)
        (add-aftergroup-to-top-frame
         (lambda ()
           (begin (set! *math-mode-p* old-math-mode-p)
            (set! *in-display-math-p* old-in-display-math-p)
            (set! *tabular-stack* old-tabular-stack) *tabular-stack*)
           (cond (display-p (emit "</td></tr></table>")) (else false))
           (cond (display-p (emit "</div>") (do-noindent)) (else false)))))))))

(define do-display-math
 (lambda (tex-string)
   (do-end-para)
   (if (and
        (or (not (tex2page-flag-boolean "\\TZPmathtext"))
            (tex2page-flag-boolean "\\TZPmathimage"))
        (not *temporarily-use-utf8-for-math-p*))
       (begin (emit "<div class=mathdisplay align=")
        (emit *display-justification*) (emit ">")
        (call-with-html-image-port
         (lambda (o) (display "$$" o) (display tex-string o) (display "$$" o))
         tex-string)
        (emit "</div>") (do-noindent))
       (do-math-fragment tex-string ':display))))

(define tex-math-delim-string
 (lambda (type)
   (let ((top null) (mid null) (bot null) (ext null))
     (begin
      (case type
        ((:lparen)
         (begin (set! top "&#x239b;") (set! bot "&#x239d;")
          (set! ext "&#x239c;") (set! mid ext) mid))
        ((:lbrack)
         (begin (set! top "&#x23a1;") (set! bot "&#x23a3;")
          (set! ext "&#x23a2;") (set! mid ext) mid))
        ((:lbrace)
         (begin (set! top "&#x23a7;") (set! mid "&#x23a8;")
          (set! bot "&#x23a9;") (set! ext "&#x23aa;") ext))
        ((:lvert)
         (begin (set! ext "&#x239c;") (set! top ext) (set! mid ext)
          (set! bot ext) bot))
        ((:rparen)
         (begin (set! top "&#x239e;") (set! bot "&#x23a0;")
          (set! ext "&#x239f;") (set! mid ext) mid))
        ((:rbrack)
         (begin (set! top "&#x23a4;") (set! bot "&#x23a6;")
          (set! ext "&#x23a5;") (set! mid ext) mid))
        ((:rbrace)
         (begin (set! top "&#x23ab;") (set! mid "&#x23ac;")
          (set! bot "&#x23ad;") (set! ext "&#x23ae;") ext))
        ((:rvert)
         (begin (set! ext "&#x239f;") (set! top ext) (set! mid ext)
          (set! bot ext) bot))
        (else (error 'ecase "0xdeadc0de")))
      (let ((%type 'string)
            (%ee
             (list "<table cellpadding=0 cellspacing=0><tr><td>" top
                   "</td></tr>"
                   (cond
                    ((odd? *math-height*)
                     (let ((%type 'string)
                           (%ee
                            (list
                             (let ((r ""))
                               (let ((%dotimes-n (/ (sub1 *math-height*) 2))
                                     (i 0))
                                 (let* ((%loop-returned false)
                                        (%loop-result 0)
                                        (return
                                         (lambda %args
                                           (set! %loop-returned true)
                                           (set! %loop-result
                                            (and (pair? %args) (car %args))))))
                                   (let %loop
                                     ()
                                     (unless %loop-returned
                                       (cond ((>= i %dotimes-n) (return r))
                                             (else false)))
                                     (unless %loop-returned
                                       (begin
                                        (set! r
                                         (let ((%type 'string)
                                               (%ee
                                                (list r "<tr><td>" ext
                                                      "</td></tr>")))
                                           (let ((%res
                                                  (if (eq? %type 'string)
                                                      ""
                                                      null)))
                                             (let %concatenate-loop
                                               ((%ee %ee))
                                               (if (null? %ee)
                                                   %res
                                                   (let ((%a (car %ee)))
                                                     (unless (not %a)
                                                       (set! %res
                                                        (if (eq? %type 'string)
                                                            (string-append %res
                                                             (if (string? %a)
                                                                 %a
                                                                 (list->string
                                                                  %a)))
                                                            (append %res
                                                                    (if (string?
                                                                         %a)
                                                                        (string->list
                                                                         %a)
                                                                        %a)))))
                                                     (%concatenate-loop
                                                      (cdr %ee)))))
                                             %res)))
                                        r))
                                     (unless %loop-returned (set! i (+ i 1)))
                                     (if %loop-returned
                                         %loop-result
                                         (%loop))))))
                             "<tr><td>" mid "</td></tr>"
                             (let ((r ""))
                               (let ((%dotimes-n (/ (sub1 *math-height*) 2))
                                     (i 0))
                                 (let* ((%loop-returned false)
                                        (%loop-result 0)
                                        (return
                                         (lambda %args
                                           (set! %loop-returned true)
                                           (set! %loop-result
                                            (and (pair? %args) (car %args))))))
                                   (let %loop
                                     ()
                                     (unless %loop-returned
                                       (cond ((>= i %dotimes-n) (return r))
                                             (else false)))
                                     (unless %loop-returned
                                       (begin
                                        (set! r
                                         (let ((%type 'string)
                                               (%ee
                                                (list r "<tr><td>" ext
                                                      "</td></tr>")))
                                           (let ((%res
                                                  (if (eq? %type 'string)
                                                      ""
                                                      null)))
                                             (let %concatenate-loop
                                               ((%ee %ee))
                                               (if (null? %ee)
                                                   %res
                                                   (let ((%a (car %ee)))
                                                     (unless (not %a)
                                                       (set! %res
                                                        (if (eq? %type 'string)
                                                            (string-append %res
                                                             (if (string? %a)
                                                                 %a
                                                                 (list->string
                                                                  %a)))
                                                            (append %res
                                                                    (if (string?
                                                                         %a)
                                                                        (string->list
                                                                         %a)
                                                                        %a)))))
                                                     (%concatenate-loop
                                                      (cdr %ee)))))
                                             %res)))
                                        r))
                                     (unless %loop-returned (set! i (+ i 1)))
                                     (if %loop-returned
                                         %loop-result
                                         (%loop)))))))))
                       (let ((%res
                              (if (eq? %type 'string)
                                  ""
                                  null)))
                         (let %concatenate-loop
                           ((%ee %ee))
                           (if (null? %ee)
                               %res
                               (let ((%a (car %ee)))
                                 (unless (not %a)
                                   (set! %res
                                    (if (eq? %type 'string)
                                        (string-append %res
                                         (if (string? %a)
                                             %a
                                             (list->string %a)))
                                        (append %res
                                                (if (string? %a)
                                                    (string->list %a)
                                                    %a)))))
                                 (%concatenate-loop (cdr %ee)))))
                         %res)))
                    (true
                     (let ((r ""))
                       (let ((%dotimes-n *math-height*) (i 0))
                         (let* ((%loop-returned false)
                                (%loop-result 0)
                                (return
                                 (lambda %args
                                   (set! %loop-returned true)
                                   (set! %loop-result
                                    (and (pair? %args) (car %args))))))
                           (let %loop
                             ()
                             (unless %loop-returned
                               (cond ((>= i %dotimes-n) (return r))
                                     (else false)))
                             (unless %loop-returned
                               (begin
                                (set! r
                                 (let ((%type 'string)
                                       (%ee
                                        (list r "<tr><td>" ext "</td></tr>")))
                                   (let ((%res
                                          (if (eq? %type 'string)
                                              ""
                                              null)))
                                     (let %concatenate-loop
                                       ((%ee %ee))
                                       (if (null? %ee)
                                           %res
                                           (let ((%a (car %ee)))
                                             (unless (not %a)
                                               (set! %res
                                                (if (eq? %type 'string)
                                                    (string-append %res
                                                     (if (string? %a)
                                                         %a
                                                         (list->string %a)))
                                                    (append %res
                                                            (if (string? %a)
                                                                (string->list
                                                                 %a)
                                                                %a)))))
                                             (%concatenate-loop (cdr %ee)))))
                                     %res)))
                                r))
                             (unless %loop-returned (set! i (+ i 1)))
                             (if %loop-returned
                                 %loop-result
                                 (%loop))))))))
                   "<tr><td>" bot "</td></tr></table>")))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))))))

(define tex-math-string-to-html-string
 (lambda (s)
   (let ((tmp-port (open-output-string)))
     (let ((%fluid-var-*html* tmp-port))
       (fluid-let ((*html* %fluid-var-*html*))
        (begin
         (call-with-input-string/buffered ""
          (lambda () (do-math-fragment s false) (generate-html)))
         (get-output-string tmp-port)))))))

(define do-intext-math
 (lambda (tex-string)
   (let ((%fluid-var-*math-needs-image-p* false))
     (fluid-let ((*math-needs-image-p* %fluid-var-*math-needs-image-p*))
      (let ((html-string (tex-math-string-to-html-string tex-string)))
        (if (and
             (or (not (tex2page-flag-boolean "\\TZPmathtext"))
                 (tex2page-flag-boolean "\\TZPmathimage"))
             *math-needs-image-p* (not *temporarily-use-utf8-for-math-p*))
            (call-with-html-image-port
             (lambda (o)
               (display #\$ o)
               (display tex-string o)
               (display #\$ o))
             tex-string)
            (emit html-string)))))))

(define do-mathp
 (lambda ()
   (call-with-html-image-port
    (lambda (o) (display #\$ o) (display (get-group) o) (display #\$ o)))))

(define do-latex-intext-math
 (lambda ()
   (do-intext-math
    (let ((o (open-output-string)))
      (begin (dump-till-ctl-seq "\\)" o) (get-output-string o))))))

(define do-latex-display-math
 (lambda ()
   (do-display-math
    (let ((o (open-output-string)))
      (begin (dump-till-ctl-seq "\\]" o) (get-output-string o))))))

(define do-math
 (lambda ()
   (let ((display-p false))
     (begin
      (cond
       ((eqv? (snoop-actual-char) #\$) (begin (set! display-p true) display-p)
        (get-actual-char))
       (else false))
      (let ((o (open-output-string)))
        (begin (dump-till-char #\$ o)
         (cond
          (display-p
           (let ((c (get-actual-char)))
             (cond
              ((or (eq? c ':eof-object) (not (char=? c #\$)))
               (terror 'do-math "Display math should end with $$."))
              (else false))))
          (else false))
         ((if display-p
              do-display-math
              do-intext-math)
          (get-output-string o))))))))

(define dump-till-char
 (lambda (d o)
   (let ((nesting 0) (escape-p false) (c false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (begin (set! c (get-actual-char)) c))
         (unless %loop-returned
           (cond
            ((eq? c ':eof-object) (terror 'dump-till-char "Missing " d "."))
            (else false)))
         (unless %loop-returned
           (cond ((and (char=? c d) (= nesting 0)) (return)) (else false)))
         (unless %loop-returned (display c o))
         (unless %loop-returned
           (cond (escape-p (begin (set! escape-p false) escape-p))
                 ((char=? c #\{)
                  (let ((%tmp (+ nesting 1)))
                    (begin (set! nesting %tmp) nesting)))
                 ((char=? c #\})
                  (let ((%tmp (- nesting 1)))
                    (begin (set! nesting %tmp) nesting)))
                 ((char=? c #\\) (begin (set! escape-p true) escape-p))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define dump-till-ctl-seq
 (lambda (cs o)
   (let ((%fluid-var-*not-processing-p* true))
     (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
      (let ((nesting 0))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (unless %loop-returned
              (let ((c (snoop-actual-char)))
                (begin
                 (cond ((eq? c ':eof-object) (terror 'dump-till-ctl-seq))
                       (else false))
                 (cond
                  ((esc-char-p c)
                   (let ((x (get-ctl-seq)))
                     (if (string=? x cs)
                         (return)
                         (display x o))))
                  (true (display (get-actual-char) o)
                   (cond
                    ((char=? c #\{)
                     (let ((%tmp (+ nesting 1)))
                       (begin (set! nesting %tmp) nesting)))
                    ((char=? c #\})
                     (let ((%tmp (- nesting 1)))
                       (begin (set! nesting %tmp) nesting)))))))))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define dump-till-end-env
 (lambda (env o)
   (let ((endenv
          (let ((%type 'string) (%ee (list "\\end" env)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (let ((endenv-prim (find-corresp-prim endenv)))
       (let ((endenv-prim-th (find-corresp-prim-thunk endenv)))
         (let ((%fluid-var-*not-processing-p* true))
           (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
            (let ((brace-nesting 0))
              (let ((env-nesting 0))
                (let* ((%loop-returned false)
                       (%loop-result 0)
                       (return
                        (lambda %args
                          (set! %loop-returned true)
                          (set! %loop-result (and (pair? %args) (car %args))))))
                  (let %loop
                    ()
                    (unless %loop-returned
                      (let ((c (snoop-actual-char)))
                        (begin
                         (cond
                          ((eq? c ':eof-object)
                           (terror 'dump-till-end-env env))
                          (else false))
                         (cond
                          ((esc-char-p c)
                           (let ((x (get-ctl-seq)))
                             (cond
                              ((string=? (find-corresp-prim x) endenv-prim)
                               (return))
                              ((string=? x "\\begin") (display x o)
                               (let ((g (get-grouped-environment-name-if-any)))
                                 (begin
                                  (cond
                                   (g (display #\{ o) (display g o)
                                    (display #\} o))
                                   (else false))
                                  (cond
                                   ((and g (string=? g env))
                                    (let ((%tmp (+ env-nesting 1)))
                                      (begin (set! env-nesting %tmp)
                                       env-nesting)))
                                   (else false)))))
                              ((string=? x "\\end")
                               (let ((g (get-grouped-environment-name-if-any)))
                                 (begin
                                  (cond
                                   ((and g
                                         (or *dumping-nontex-p*
                                             (= env-nesting 0))
                                         (let ((endg
                                                (let ((%type 'string)
                                                      (%ee (list "\\end" g)))
                                                  (let ((%res
                                                         (if (eq? %type
                                                              'string)
                                                             ""
                                                             null)))
                                                    (let %concatenate-loop
                                                      ((%ee %ee))
                                                      (if (null? %ee)
                                                          %res
                                                          (let ((%a (car %ee)))
                                                            (unless (not %a)
                                                              (set! %res
                                                               (if (eq? %type
                                                                    'string)
                                                                   (string-append
                                                                    %res
                                                                    (if (string?
                                                                         %a)
                                                                        %a
                                                                        (list->string
                                                                         %a)))
                                                                   (append %res
                                                                           (if (string?
                                                                                %a)
                                                                               (string->list
                                                                                %a)
                                                                               %a)))))
                                                            (%concatenate-loop
                                                             (cdr %ee)))))
                                                    %res))))
                                           (or
                                            (string=? (find-corresp-prim endg)
                                             endenv-prim)
                                            (eqv?
                                             (find-corresp-prim-thunk endg)
                                             endenv-prim-th))))
                                    (return))
                                   (else false))
                                  (display x o)
                                  (cond
                                   (g (display #\{ o) (display g o)
                                    (display #\} o))
                                   (else false))
                                  (cond
                                   ((and g (string=? g env))
                                    (let ((%tmp (- env-nesting 1)))
                                      (begin (set! env-nesting %tmp)
                                       env-nesting)))
                                   (else false)))))
                              (true (display x o)))))
                          ((and (char=? c *comment-char*)
                                (not *dumping-nontex-p*))
                           (do-comment) (write-char #\% o) (newline o))
                          (true (write-char (get-actual-char) o)
                           (cond
                            ((char=? c #\{)
                             (let ((%tmp (+ brace-nesting 1)))
                               (begin (set! brace-nesting %tmp)
                                brace-nesting)))
                            ((char=? c #\})
                             (let ((%tmp (- brace-nesting 1)))
                               (begin (set! brace-nesting %tmp)
                                brace-nesting)))))))))
                    (if %loop-returned
                        %loop-result
                        (%loop)))))))))))))

(define dump-imgdef
 (lambda (f)
   (let ((aux-tex-file
          (let ((%type 'string) (%ee (list f ".tex")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
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
         (begin
          (cond
           (o
            ((if (input-port? o)
                 close-input-port
                 close-output-port)
             o))
           (else false))
          %with-open-file-res))))))

(define do-img-preamble
 (lambda ()
   (begin
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
            (unless %loop-returned
              (let ((c (snoop-actual-char)))
                (begin
                 (cond
                  ((eq? c ':eof-object)
                   (terror 'do-img-preamble "Missing \\endimgpreamble"))
                  (else false))
                 (cond
                  ((esc-char-p c)
                   (let ((x (get-ctl-seq)))
                     (begin
                      (cond
                       ((member x
                                '("\\endimgpreamble" "\\endgifpreamble"
                                  "\\endmathpreamble"))
                        (return r))
                       (else false))
                      (begin
                       (set! r
                        (let ((%type 'string) (%ee (list r x)))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res)))
                       r))))
                  (true (get-actual-char)
                   (begin
                    (set! r
                     (let ((%type 'string) (%ee (list r (list c))))
                       (let ((%res
                              (if (eq? %type 'string)
                                  ""
                                  null)))
                         (let %concatenate-loop
                           ((%ee %ee))
                           (if (null? %ee)
                               %res
                               (let ((%a (car %ee)))
                                 (unless (not %a)
                                   (set! %res
                                    (if (eq? %type 'string)
                                        (string-append %res
                                         (if (string? %a)
                                             %a
                                             (list->string %a)))
                                        (append %res
                                                (if (string? %a)
                                                    (string->list %a)
                                                    %a)))))
                                 (%concatenate-loop (cdr %ee)))))
                         %res)))
                    r))))))
            (if %loop-returned
                %loop-result
                (%loop)))))))
    *imgpreamble*)))

(define pick-new-stream-number
 (lambda (stream-list)
   (let ((i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond
            ((not (or (table-get i stream-list) (= i 16) (= i 18))) (return i))
            (else false)))
         (unless %loop-returned
           (let ((%tmp (+ i 1)))
             (begin (set! i %tmp) i)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-new-stream
 (lambda (type)
   (let ((x (get-ctl-seq)))
     (let ((sl
            (if (eq? type ':out)
                *output-streams*
                *input-streams*)))
       (let ((n (pick-new-stream-number sl)))
         (begin (tex-def-count x n true)
          (begin (table-put! n sl ':free) (table-get n sl))))))))

(define do-open-stream
 (lambda (type)
   (let ((n (get-number)))
     (let ((f (get-filename)))
       (let ((sl
              (if (eq? type ':out)
                  *output-streams*
                  *input-streams*)))
         (let ((c (table-get n sl)))
           (begin
            (cond ((not (eq? c ':free)) (terror 'do-open-stream)) (else false))
            (begin
             (table-put! n sl
              (case type
                ((:out)
                 (begin (set! f (add-dot-tex-if-no-extension-provided f)) f)
                 (let* ((%f f)
                        (%ee
                         (list ':direction ':output ':if-exists ':supersede))
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
                     (when
                         (and (eqv? %if-exists ':supersede) (file-exists? %f))
                       (delete-file %f))
                     (open-output-file %f))
                    ((and (not %if-does-not-exist) (not (file-exists? %f)))
                     false)
                    (else (open-input-file %f)))))
                ((:in)
                 (begin (set! f (actual-tex-filename f)) f)
                 (make-bport* ':port
                  (let* ((%f f)
                         (%ee (list ':direction ':input))
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
                      (when
                          (and (eqv? %if-exists ':supersede) (file-exists? %f))
                        (delete-file %f))
                      (open-output-file %f))
                     ((and (not %if-does-not-exist) (not (file-exists? %f)))
                      false)
                     (else (open-input-file %f))))))
                (else (error 'ecase "0xdeadc0de"))))
             (table-get n sl)))))))))

(define do-close-stream
 (lambda (type)
   (let ((sl
          (if (eqv? type ':out)
              *output-streams*
              *input-streams*)))
     (let ((o (get-number)))
       (let ((c (table-get o sl)))
         (begin (cond ((eq? c ':free) (terror 'do-close-stream)) (else false))
          (let ((%close-port-arg
                 (case type
                   ((:out) c)
                   ((:in) (bport*-port c))
                   (else (error 'ecase "0xdeadc0de")))))
            ((if (input-port? %close-port-arg)
                 close-input-port
                 close-output-port)
             %close-port-arg))
          (begin (table-put! o sl ':free) (table-get o sl))))))))

(define tex-write-output-string
 (lambda (s)
   (let ((o (open-output-string)))
     (let ((%fluid-var-*outputting-to-non-html-p* true))
       (fluid-let
        ((*outputting-to-non-html-p* %fluid-var-*outputting-to-non-html-p*))
        (let ((%fluid-var-*html* o))
          (fluid-let ((*html* %fluid-var-*html*))
           (begin
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
                   (unless %loop-returned
                     (let ((c (snoop-actual-char)))
                       (begin
                        (cond ((eq? c ':eof-object) (return)) (else false))
                        (case c
                          ((#\\) (do-tex-ctl-seq (get-ctl-seq)))
                          (else (emit-html-char (get-actual-char)))))))
                   (if %loop-returned
                       %loop-result
                       (%loop))))))
            (get-output-string o)))))))))

(define do-write-aux
 (lambda (o)
   (let ((output (tex-write-output-string (get-peeled-group))))
     (cond ((and (= o 18) *enable-write-18-p*) (system output))
           ((member o '(16 18)) (write-log output)
            (write-log ':separation-space))
           ((begin (set! *it* (table-get o *output-streams*)) *it*)
            (let ((p *it*))
              (begin
               (cond ((eq? p ':free) (terror 'do-write-aux)) (else false))
               (display output p) (display #\  p))))
           (true (terror 'do-write-aux))))))

(define do-write (lambda () (do-write-aux (get-number))))

(define do-message (lambda () (do-write-aux 16)))

(define read-tex-line
 (lambda (p)
   (let ((%fluid-var-*current-tex2page-input* p))
     (fluid-let
      ((*current-tex2page-input* %fluid-var-*current-tex2page-input*))
      (let ((r null))
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (unless %loop-returned
              (let ((c (snoop-actual-char)))
                (begin
                 (cond
                  ((eq? c ':eof-object)
                   (return
                    (if (null? r)
                        c
                        (let ((%type 'string) (%ee (list (reverse r))))
                          (let ((%res
                                 (if (eq? %type 'string)
                                     ""
                                     null)))
                            (let %concatenate-loop
                              ((%ee %ee))
                              (if (null? %ee)
                                  %res
                                  (let ((%a (car %ee)))
                                    (unless (not %a)
                                      (set! %res
                                       (if (eq? %type 'string)
                                           (string-append %res
                                            (if (string? %a)
                                                %a
                                                (list->string %a)))
                                           (append %res
                                                   (if (string? %a)
                                                       (string->list %a)
                                                       %a)))))
                                    (%concatenate-loop (cdr %ee)))))
                            %res)))))
                  (else false))
                 (cond
                  ((char=? c #\newline) (get-actual-char)
                   (return
                    (let ((%type 'string) (%ee (list (reverse r))))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))))
                  (else false))
                 (cond
                  ((char=? c #\{)
                   (return
                    (let ((%type 'string)
                          (%ee
                           (list
                            (let ((%type 'string) (%ee (list (reverse r))))
                              (let ((%res
                                     (if (eq? %type 'string)
                                         ""
                                         null)))
                                (let %concatenate-loop
                                  ((%ee %ee))
                                  (if (null? %ee)
                                      %res
                                      (let ((%a (car %ee)))
                                        (unless (not %a)
                                          (set! %res
                                           (if (eq? %type 'string)
                                               (string-append %res
                                                (if (string? %a)
                                                    %a
                                                    (list->string %a)))
                                               (append %res
                                                       (if (string? %a)
                                                           (string->list %a)
                                                           %a)))))
                                        (%concatenate-loop (cdr %ee)))))
                                %res))
                            (get-group))))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))))
                  (else false))
                 (let ((%push-new-stack (cons (get-actual-char) r)))
                   (begin (set! r %push-new-stack) r)))))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define do-read
 (lambda (global-p)
   (let ((i (get-number)))
     (let ((x (begin (get-to) (get-ctl-seq))))
       (let ((p false))
         (begin
          (cond
           ((member i '(-1 16))
            (begin (set! p (make-bport* ':port (current-input-port))) p)
            (cond ((not (= i -1)) (write-log x) (write-log #\=)) (else false)))
           ((begin (set! *it* (table-get i *input-streams*)) *it*)
            (begin (set! p *it*) p)
            (cond ((not (eq? p ':free)) (terror 'do-read)) (else false)))
           (true (terror 'do-read)))
          ((if global-p
               tex-gdef-0arg
               tex-def-0arg)
           x
           (let ((line (read-tex-line p)))
             (if (eq? line ':eof-object)
                 ""
                 line)))))))))

(define do-typein
 (lambda ()
   (let ((ctlseq (get-bracketed-text-if-any))
         (p (make-bport* ':port (current-input-port))))
     (begin (write-log ':separation-newline)
      (write-log (tex-string-to-html-string (get-group)))
      (write-log ':separation-newline) (write-log (or ctlseq "\\@typein"))
      (write-log #\=)
      (let ((l (read-tex-line p)))
        (begin (cond ((eq? l ':eof-object) (begin (set! l "") l)) (else false))
         (cond (ctlseq (tex-def-0arg ctlseq l))
               (true (tex2page-string l)))))))))

(define do-ifeof
 (lambda ()
   (let ((i (get-number)))
     (let ((c (table-get i *input-streams*)))
       (begin (cond ((eq? c ':free) (terror 'do-ifeof)) (else false))
        (if (eq?
             (let* ((%read-char-port c)
                    (%read-char-res
                     (if %read-char-port
                         (read-char %read-char-port)
                         (read-char))))
               (when (eof-object? %read-char-res)
                 (set! %read-char-res ':eof-object))
               %read-char-res)
             ':eof-object)
            (do-iftrue)
            (do-iffalse)))))))

(define do-iffalse
 (lambda ()
   (let ((%push-new-stack (cons false *tex-if-stack*)))
     (begin (set! *tex-if-stack* %push-new-stack) *tex-if-stack*))))

(define do-iftrue
 (lambda ()
   (let ((%push-new-stack (cons true *tex-if-stack*)))
     (begin (set! *tex-if-stack* %push-new-stack) *tex-if-stack*))))

(define insert-tex-if
 (lambda (test)
   (if test
       (do-iftrue)
       (do-iffalse))))

(define do-ifx
 (lambda ()
   (let ((one (get-raw-token/is)))
     (let ((two (get-raw-token/is)))
       (let ((one2 one))
         (let ((two2 two))
           (if (string=? one two)
               (do-iftrue)
               (begin
                (cond
                 ((ctl-seq-p one)
                  (begin
                   (set! one2
                    (cond
                     ((begin (set! *it* (find-def one)) *it*)
                      (let ((d *it*))
                        (or (tdef*-expansion d) (tdef*-prim d))))
                     ((begin (set! *it* (find-math-def one)) *it*) *it*)
                     (true "UnDeFiNeD")))
                   one2))
                 (else false))
                (cond
                 ((ctl-seq-p two)
                  (begin
                   (set! two2
                    (cond
                     ((begin (set! *it* (find-def two)) *it*)
                      (let ((d *it*))
                        (or (tdef*-expansion d) (tdef*-prim d))))
                     ((begin (set! *it* (find-math-def two)) *it*) *it*)
                     (true "UnDeFiNeD")))
                   two2))
                 (else false))
                (if (or (eqv? one2 two2)
                        (and (string? one2) (string? two2)
                             (string=? one2 two2)))
                    (do-iftrue)
                    (do-iffalse))))))))))

(define do-ifdefined
 (lambda ()
   (let ((x (get-raw-token/is)))
     (if (or (not (ctl-seq-p x))
             (and (ctl-seq-p x) (or (find-def x) (find-math-def x))))
         (do-iftrue)
         (do-iffalse)))))

(define do-if-get-atomic
 (lambda ()
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((x (get-raw-token/is)))
           (if (ctl-seq-p x)
               (cond
                ((begin (set! *it* (resolve-defs x)) *it*)
                 (let ((z *it*))
                   (begin (toss-back-char *invisible-space*)
                    (toss-back-string z))))
                (true (return x)))
               (return x))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define do-if
 (lambda ()
   (let ((one (do-if-get-atomic)))
     (let ((two (do-if-get-atomic)))
       (if (or (string=? one two) (and (ctl-seq-p one) (ctl-seq-p two)))
           (do-iftrue)
           (do-iffalse))))))

(define do-ifmmode
 (lambda ()
   (let ((%push-new-stack (cons *math-mode-p* *tex-if-stack*)))
     (begin (set! *tex-if-stack* %push-new-stack) *tex-if-stack*))))

(define do-ifnum
 (lambda ()
   (let ((one (get-number)))
     (let ((rel (string-ref (get-raw-token/is) 0)))
       (if ((case rel
              ((#\<) <)
              ((#\=) =)
              ((#\>) >)
              (else (terror 'do-ifnum "Missing = for \\ifnum.")))
            one (get-number))
           (do-iftrue)
           (do-iffalse))))))

(define read-ifcase-clauses
 (lambda ()
   (let ((%fluid-var-*not-processing-p* true))
     (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
      (let ((else-clause false))
        (let ((or-clauses null))
          (let ((elsep false))
            (let ((outer-loop-done false))
              (begin
               (let* ((%loop-returned false)
                      (%loop-result 0)
                      (return
                       (lambda %args
                         (set! %loop-returned true)
                         (set! %loop-result (and (pair? %args) (car %args))))))
                 (let %loop
                   ()
                   (unless %loop-returned
                     (cond (outer-loop-done (return)) (else false)))
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
                           (unless %loop-returned
                             (let ((c (snoop-actual-char)))
                               (begin
                                (cond
                                 ((eq? c ':eof-object)
                                  (terror 'read-ifcase-clauses
                                   "Incomplete \\ifcase."))
                                 (else false))
                                (cond
                                 ((esc-char-p c)
                                  (let ((x (get-ctl-seq)))
                                    (cond
                                     ((string=? x "\\or") (ignorespaces)
                                      (cond
                                       (elsep
                                        (terror 'read-ifcase-clauses
                                         "Extra \\or."))
                                       (else false))
                                      (let ((%push-new-stack
                                             (cons clause or-clauses)))
                                        (begin
                                         (set! or-clauses %push-new-stack)
                                         or-clauses))
                                      (return))
                                     ((string=? x "\\else") (ignorespaces)
                                      (cond
                                       (elsep
                                        (terror 'read-ifcase-clauses
                                         "Extra \\else."))
                                       (else false))
                                      (let ((%push-new-stack
                                             (cons clause or-clauses)))
                                        (begin
                                         (set! or-clauses %push-new-stack)
                                         or-clauses))
                                      (begin (set! elsep true) elsep) (return))
                                     ((string=? x "\\fi") (ignorespaces)
                                      (cond
                                       (elsep
                                        (begin (set! else-clause clause)
                                         else-clause))
                                       (true
                                        (let ((%push-new-stack
                                               (cons clause or-clauses)))
                                          (begin
                                           (set! or-clauses %push-new-stack)
                                           or-clauses))))
                                      (begin (set! outer-loop-done true)
                                       outer-loop-done)
                                      (return))
                                     (true
                                      (begin
                                       (set! clause
                                        (let ((%type 'string)
                                              (%ee (list clause x)))
                                          (let ((%res
                                                 (if (eq? %type 'string)
                                                     ""
                                                     null)))
                                            (let %concatenate-loop
                                              ((%ee %ee))
                                              (if (null? %ee)
                                                  %res
                                                  (let ((%a (car %ee)))
                                                    (unless (not %a)
                                                      (set! %res
                                                       (if (eq? %type 'string)
                                                           (string-append %res
                                                            (if (string? %a)
                                                                %a
                                                                (list->string
                                                                 %a)))
                                                           (append %res
                                                                   (if (string?
                                                                        %a)
                                                                       (string->list
                                                                        %a)
                                                                       %a)))))
                                                    (%concatenate-loop
                                                     (cdr %ee)))))
                                            %res)))
                                       clause)))))
                                 (true (get-actual-char)
                                  (begin
                                   (set! clause
                                    (let ((%type 'string)
                                          (%ee (list clause (list c))))
                                      (let ((%res
                                             (if (eq? %type 'string)
                                                 ""
                                                 null)))
                                        (let %concatenate-loop
                                          ((%ee %ee))
                                          (if (null? %ee)
                                              %res
                                              (let ((%a (car %ee)))
                                                (unless (not %a)
                                                  (set! %res
                                                   (if (eq? %type 'string)
                                                       (string-append %res
                                                        (if (string? %a)
                                                            %a
                                                            (list->string %a)))
                                                       (append %res
                                                               (if (string? %a)
                                                                   (string->list
                                                                    %a)
                                                                   %a)))))
                                                (%concatenate-loop
                                                 (cdr %ee)))))
                                        %res)))
                                   clause))))))
                           (if %loop-returned
                               %loop-result
                               (%loop))))))
                   (if %loop-returned
                       %loop-result
                       (%loop))))
               (list (reverse or-clauses) else-clause))))))))))

(define do-ifcase
 (lambda ()
   (let ((num (get-number)))
     (apply
      (lambda (or-clauses else-clause)
        (let ((chosen (or (list-ref or-clauses num) else-clause)))
          (cond (chosen (tex2page-string chosen)) (else false))))
      (read-ifcase-clauses)))))

(define do-ifodd
 (lambda ()
   (if (odd? (get-number))
       (do-iftrue)
       (do-iffalse))))

(define do-else
 (lambda ()
   (cond ((null? *tex-if-stack*) (terror 'do-else "Extra \\else."))
         (else false))
   (let ((top-if
          (let* ((%pop-old-stack *tex-if-stack*)
                 (%pop-top-value (car %pop-old-stack)))
            (begin (set! *tex-if-stack* (cdr %pop-old-stack)) *tex-if-stack*)
            %pop-top-value)))
     (let ((%push-new-stack (cons (not top-if) *tex-if-stack*)))
       (begin (set! *tex-if-stack* %push-new-stack) *tex-if-stack*)))))

(define do-fi
 (lambda ()
   (cond ((null? *tex-if-stack*) (terror 'do-fi "Extra \\fi.")) (else false))
   (let* ((%pop-old-stack *tex-if-stack*) (%pop-top-value (car %pop-old-stack)))
     (begin (set! *tex-if-stack* (cdr %pop-old-stack)) *tex-if-stack*)
     %pop-top-value)))

(define do-newif
 (lambda ()
   (let ((iffoo (get-ctl-seq)))
     (let ((foo
            (let ((%type 'string) (%ee (list "\\" (subseq iffoo 3))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (let ((foo-register
              (let ((%type 'string) (%ee (list foo "BOOLEANREGISTER")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (begin (tex-def-count foo-register 0 false)
          (tex-def-thunk iffoo
           (lambda ()
             (let ((%push-new-stack
                    (cons (> (the-count foo-register) 0) *tex-if-stack*)))
               (begin (set! *tex-if-stack* %push-new-stack) *tex-if-stack*)))
           false)
          (tex-def-thunk
           (let ((%type 'string) (%ee (list foo "true")))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))
           (lambda () (tex-def-count foo-register 1 false)) false)
          (tex-def-thunk
           (let ((%type 'string) (%ee (list foo "false")))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))
           (lambda () (tex-def-count foo-register 0 false)) false)))))))

(define do-htmlimg
 (lambda (env)
   (call-with-html-image-port (lambda (o) (dump-till-end-env env o)))))

(define find-img-file-extn
 (lambda ()
   (case (tex2page-flag-value "\\TZPimageformat")
     ((#\g #\G) ".gif")
     ((#\j #\J) ".jpeg")
     (else ".png"))))

(define do-htmlimageformat
 (lambda () (tex-def-0arg "\\TZPimageformat" (get-peeled-group))))

(define do-htmlimageconversionprogram
 (lambda () (tex-def-0arg "\\TZPimageconverter" (get-peeled-group))))

(define do-htmlimgmagnification (lambda () true))

(define call-mp
 (let ((tex-prog-name false))
   (lambda (f)
     (cond
      ((not tex-prog-name) (begin (set! tex-prog-name "tex") tex-prog-name))
      (else false))
     (system
      (let ((%type 'string)
            (%ee (list *metapost* " -tex=" tex-prog-name " " f)))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))))))

(define call-tex
 (let ((tex-prog-name false) (tex-output-format false))
   (lambda (f)
     (cond
      ((not tex-prog-name)
       (let ((d (find-def "\\TZPtexprogname")))
         (cond
          (d (begin (set! tex-prog-name (tdef*-expansion d)) tex-prog-name))
          (else false)))
       (cond
        ((not tex-prog-name)
         (begin (set! tex-prog-name "xetex") tex-prog-name))
        (else false))
       (begin
        (set! tex-output-format
         (if (or (eqv? (substring? "pdf" tex-prog-name) 0)
                 (eqv? (substring? "xe" tex-prog-name) 0)
                 (eqv? (substring? "lua" tex-prog-name) 0))
             ':pdf
             ':dvi))
        tex-output-format)
       (cond
        ((eq? *tex-format* ':latex)
         (begin
          (set! tex-prog-name
           (let ((%type 'string)
                 (%ee
                  (list
                   (subseq tex-prog-name 0
                           (-
                            (let ((%length-arg tex-prog-name))
                              ((if (string? %length-arg)
                                   string-length
                                   length)
                               %length-arg))
                            3))
                   "latex")))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res)))
          tex-prog-name))
        (else false)))
      (else false))
     (let ((dvi-file
            (let ((%type 'string)
                  (%ee
                   (list f
                         (if (eq? tex-output-format ':pdf)
                             ".pdf"
                             ".dvi"))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (let ((outfile dvi-file))
         (begin
          (system
           (let ((%type 'string) (%ee (list tex-prog-name " " f)))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res)))
          (cond
           ((file-exists? dvi-file)
            (let ((logfile
                   (let ((%type 'string) (%ee (list f ".log")))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res))))
              (cond
               ((file-exists? logfile)
                (let ((fine-p
                       (let ((i
                              (let* ((%f logfile)
                                     (%ee (list ':direction ':input))
                                     (%direction (memv ':direction %ee))
                                     (%if-exists (memv ':if-exists %ee))
                                     (%if-does-not-exist ':error)
                                     (%if-does-not-exist-from-user
                                      (memv ':if-does-not-exist %ee)))
                                (when %direction
                                  (set! %direction (cadr %direction)))
                                (when %if-exists
                                  (set! %if-exists (cadr %if-exists)))
                                (when %if-does-not-exist-from-user
                                  (set! %if-does-not-exist
                                   (cadr %if-does-not-exist-from-user)))
                                (cond
                                 ((eqv? %direction ':output)
                                  (when
                                      (and (eqv? %if-exists ':supersede)
                                           (file-exists? %f))
                                    (delete-file %f))
                                  (open-output-file %f))
                                 ((and (not %if-does-not-exist)
                                       (not (file-exists? %f)))
                                  false)
                                 (else (open-input-file %f))))))
                         (let ((%with-open-file-res
                                (begin
                                 (let ((x false))
                                   (let* ((%loop-returned false)
                                          (%loop-result 0)
                                          (return
                                           (lambda %args
                                             (set! %loop-returned true)
                                             (set! %loop-result
                                              (and (pair? %args)
                                                   (car %args))))))
                                     (let %loop
                                       ()
                                       (unless %loop-returned
                                         (begin
                                          (set! x
                                           (let ((%read-line-res (read-line i)))
                                             (when (eof-object? %read-line-res)
                                               (set! %read-line-res
                                                ':eof-object))
                                             %read-line-res))
                                          x))
                                       (unless %loop-returned
                                         (cond
                                          ((eq? x ':eof-object) (return true))
                                          (else false)))
                                       (unless %loop-returned
                                         (cond
                                          ((substring? "! I can't find file" x)
                                           (return false))
                                          (else false)))
                                       (if %loop-returned
                                           %loop-result
                                           (%loop))))))))
                           (begin
                            (cond
                             (i
                              ((if (input-port? i)
                                   close-input-port
                                   close-output-port)
                               i))
                             (else false))
                            %with-open-file-res)))))
                  (cond
                   (fine-p
                    (cond
                     ((not (eq? tex-output-format ':pdf))
                      (let ((ps-file
                             (let ((%type 'string) (%ee (list f ".ps")))
                               (let ((%res
                                      (if (eq? %type 'string)
                                          ""
                                          null)))
                                 (let %concatenate-loop
                                   ((%ee %ee))
                                   (if (null? %ee)
                                       %res
                                       (let ((%a (car %ee)))
                                         (unless (not %a)
                                           (set! %res
                                            (if (eq? %type 'string)
                                                (string-append %res
                                                 (if (string? %a)
                                                     %a
                                                     (list->string %a)))
                                                (append %res
                                                        (if (string? %a)
                                                            (string->list %a)
                                                            %a)))))
                                         (%concatenate-loop (cdr %ee)))))
                                 %res))))
                        (begin
                         (system
                          (let ((%type 'string)
                                (%ee (list "dvips " dvi-file " -o " ps-file)))
                            (let ((%res
                                   (if (eq? %type 'string)
                                       ""
                                       null)))
                              (let %concatenate-loop
                                ((%ee %ee))
                                (if (null? %ee)
                                    %res
                                    (let ((%a (car %ee)))
                                      (unless (not %a)
                                        (set! %res
                                         (if (eq? %type 'string)
                                             (string-append %res
                                              (if (string? %a)
                                                  %a
                                                  (list->string %a)))
                                             (append %res
                                                     (if (string? %a)
                                                         (string->list %a)
                                                         %a)))))
                                      (%concatenate-loop (cdr %ee)))))
                              %res)))
                         (begin (set! outfile ps-file) outfile))))
                     (else false))
                    outfile)
                   (else false))))
               (else false))))
           (else false))))))))

(define ps-to-img/gif/netpbm
 (lambda (ps-file img-file)
   (system
    (let ((%type 'string)
          (%ee
           (list *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                 ".ppm.1 " ps-file " quit.ps")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee (list "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee (list "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee
           (list "ppmtogif -transparent rgb:ff/ff/ff < " img-file ".ppm > "
                 *aux-dir/* img-file)))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (for-each
    (lambda (e)
      (ensure-file-deleted
       (let ((%type 'string) (%ee (list img-file e)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))))
    '(".ppm" ".ppm.tmp" ".ppm.1"))))

(define ps-to-img/png/netpbm
 (lambda (ps-file img-file)
   (system
    (let ((%type 'string)
          (%ee
           (list *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                 ".ppm.1 " ps-file " quit.ps")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee (list "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   '(system
     (let ((%type 'string)
           (%ee
            (list "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm")))
       (let ((%res
              (if (eq? %type 'string)
                  ""
                  null)))
         (let %concatenate-loop
           ((%ee %ee))
           (if (null? %ee)
               %res
               (let ((%a (car %ee)))
                 (unless (not %a)
                   (set! %res
                    (if (eq? %type 'string)
                        (string-append %res
                         (if (string? %a)
                             %a
                             (list->string %a)))
                        (append %res
                                (if (string? %a)
                                    (string->list %a)
                                    %a)))))
                 (%concatenate-loop (cdr %ee)))))
         %res)))
   (system
    (let ((%type 'string)
          (%ee
           (list "pnmtopng -interlace -transparent \"#FFFFFF\" " " < " img-file
                 ".ppm.tmp > " *aux-dir/* img-file)))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (for-each
    (lambda (e)
      (ensure-file-deleted
       (let ((%type 'string) (%ee (list img-file e)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))))
    '(".ppm.1" ".ppm.tmp" ".ppm"))))

(define ps-to-img/jpeg/netpbm
 (lambda (ps-file img-file)
   (system
    (let ((%type 'string)
          (%ee
           (list *ghostscript* *ghostscript-options* " -sOutputFile=" img-file
                 ".ppm.1 " ps-file " quit.ps")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee (list "pnmcrop " img-file ".ppm.1 > " img-file ".ppm.tmp")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee (list "ppmquant 256 < " img-file ".ppm.tmp > " img-file ".ppm")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (system
    (let ((%type 'string)
          (%ee
           (list "ppmtojpeg --grayscale < " img-file ".ppm > " *aux-dir/*
                 img-file)))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (for-each
    (lambda (e)
      (ensure-file-deleted
       (let ((%type 'string) (%ee (list img-file e)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))))
    '(".ppm.1" ".ppm.tmp" ".ppm"))))

(define ps-to-img
 (lambda (ps-file img-file)
   (case (tex2page-flag-value "\\TZPimageconverter")
     ((#\i #\I)
      (system
       (let ((%type 'string)
             (%ee
              (list "convert -transparent white -trim " ps-file " " img-file)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))))
     (else
      (case (tex2page-flag-value "\\TZPimageformat")
        ((#\p #\P) (ps-to-img/png/netpbm ps-file img-file))
        ((#\j #\J) (ps-to-img/jpeg/netpbm ps-file img-file))
        (else (ps-to-img/gif/netpbm ps-file img-file)))))))

(define tex-to-img
 (lambda (f)
   (let ((%tmp (+ *img-file-tally* 1)))
     (begin (set! *img-file-tally* %tmp) *img-file-tally*))
   (let ((img-file
          (let ((%type 'string) (%ee (list *aux-dir/* f (find-img-file-extn))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (cond
      ((not (file-exists? img-file)) (write-log ':separation-space)
       (write-log #\{)
       (write-log
        (let ((%type 'string) (%ee (list f ".tex")))
          (let ((%res
                 (if (eq? %type 'string)
                     ""
                     null)))
            (let %concatenate-loop
              ((%ee %ee))
              (if (null? %ee)
                  %res
                  (let ((%a (car %ee)))
                    (unless (not %a)
                      (set! %res
                       (if (eq? %type 'string)
                           (string-append %res
                            (if (string? %a)
                                %a
                                (list->string %a)))
                           (append %res
                                   (if (string? %a)
                                       (string->list %a)
                                       %a)))))
                    (%concatenate-loop (cdr %ee)))))
            %res)))
       (write-log ':separation-space) (write-log "->")
       (write-log ':separation-space)
       (cond
        ((begin (set! *it* (call-tex f)) *it*) (ps-to-img *it* img-file)
         (write-log img-file)
         '(for-each
           (lambda (e)
             (ensure-file-deleted
              (let ((%type 'string) (%ee (list f e)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
           '(".aux" ".dvi" ".log" ".pdf" ".ps" ".tex")))
        (true (write-log "failed, try manually")))
       (write-log #\}) (write-log ':separation-space))
      (else false)))))

(define call-with-lazy-image-port
 (lambda (eps-file img-file-stem p)
   (let ((aux-tex-file
          (let ((%type 'string) (%ee (list img-file-stem ".tex")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin
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
          (begin
           (cond
            (o
             ((if (input-port? o)
                  close-input-port
                  close-output-port)
              o))
            (else false))
           %with-open-file-res)))
      (if (file-exists? eps-file)
          (tex-to-img img-file-stem)
          (let ((%push-new-stack
                 (cons (cons eps-file img-file-stem) *missing-eps-files*)))
            (begin (set! *missing-eps-files* %push-new-stack)
             *missing-eps-files*)))))))

(define retry-lazy-image
 (lambda (eps-file img-file-stem)
   (cond ((file-exists? eps-file) (tex-to-img img-file-stem))
         (true (write-log "! I can't find EPS file ") (write-log eps-file)
          (write-log ':separation-newline)))))

(define lazily-make-epsf-image-file
 (lambda (eps-file img-file-stem)
   (let ((%fluid-var-*imgpreamble-inferred*
          (cons ':epsfbox *imgpreamble-inferred*)))
     (fluid-let ((*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*))
      (call-with-lazy-image-port eps-file img-file-stem
       (lambda (o)
         (display "\\epsfbox{" o)
         (display eps-file o)
         (display #\} o)))))))

(define do-epsfbox
 (lambda ()
   (get-bracketed-text-if-any)
   (let ((f (get-filename-possibly-braced)))
     (cond
      ((not *eval-for-tex-only-p*)
       (let ((epsf-x-size (get-dimen "\\epsfxsize"))
             (epsf-y-size (get-dimen "\\epsfysize")))
         (cond
          ((and (= epsf-x-size 0) (= epsf-y-size 0))
           (let ((img-file-stem (next-html-image-file-stem)))
             (begin (lazily-make-epsf-image-file f img-file-stem)
              (source-img-file img-file-stem))))
          (true
           (cond ((not (= epsf-x-size 0)) (tex2page-string "\\epsfxsize=0pt"))
                 (else false))
           (cond ((not (= epsf-y-size 0)) (tex2page-string "\\epsfysize=0pt"))
                 (else false))
           (let ((%fluid-var-*imgpreamble-inferred*
                  (cons ':epsfbox *imgpreamble-inferred*)))
             (fluid-let
              ((*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*))
              (call-with-html-image-port
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
                 (display #\} o)))))))))
      (else false)))))

(define do-epsfig
 (lambda ()
   (let ((%fluid-var-*imgpreamble-inferred*
          (cons ':epsfbox *imgpreamble-inferred*)))
     (fluid-let ((*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*))
      (call-with-html-image-port
       (lambda (o)
         (display "\\epsfig{" o)
         (dump-groupoid o)
         (display #\} o)))))))

(define do-convertmptopdf
 (lambda ()
   (let ((f (get-filename-possibly-braced))
         (img-file-stem (next-html-image-file-stem)))
     (begin (get-token) (get-token)
      (lazily-make-epsf-image-file f img-file-stem)
      (source-img-file img-file-stem)))))

(define do-includegraphics-web
 (lambda (bracketed-text image-file)
   (emit "<img")
   (cond
    (bracketed-text
     (let ((height false) (width false))
       (begin (toss-back-string " enoughalready ")
        (toss-back-string bracketed-text)
        (let* ((%loop-returned false)
               (%loop-result 0)
               (return
                (lambda %args
                  (set! %loop-returned true)
                  (set! %loop-result (and (pair? %args) (car %args))))))
          (let %loop
            ()
            (unless %loop-returned
              (cond
               ((eat-word "height") (get-equal-sign)
                (begin (set! height (get-pixels)) height))
               ((eat-word "width") (get-equal-sign)
                (begin (set! width (get-pixels)) width))
               ((eat-word "enoughalready") (ignorespaces) (return))
               (true (get-actual-char))))
            (if %loop-returned
                %loop-result
                (%loop))))
        (cond (height (emit " height=") (emit height)) (else false))
        (cond (width (emit " width=") (emit width)) (else false)))))
    (else false))
   (emit " src=\"")
   (emit (fully-qualify-url image-file))
   (emit "\">")))

(define do-includegraphics
 (lambda ()
   (let ((starred-p (eat-star)))
     (let ((b1 (get-bracketed-text-if-any)))
       (let ((b2 (and b1 (get-bracketed-text-if-any))))
         (let ((f (get-filename-possibly-braced)))
           (let ((ffull
                  (if (file-exists? f)
                      f
                      (ormap
                       (lambda (e)
                         (let ((f2
                                (let ((%type 'string) (%ee (list f e)))
                                  (let ((%res
                                         (if (eq? %type 'string)
                                             ""
                                             null)))
                                    (let %concatenate-loop
                                      ((%ee %ee))
                                      (if (null? %ee)
                                          %res
                                          (let ((%a (car %ee)))
                                            (unless (not %a)
                                              (set! %res
                                               (if (eq? %type 'string)
                                                   (string-append %res
                                                    (if (string? %a)
                                                        %a
                                                        (list->string %a)))
                                                   (append %res
                                                           (if (string? %a)
                                                               (string->list
                                                                %a)
                                                               %a)))))
                                            (%concatenate-loop (cdr %ee)))))
                                    %res))))
                           (and (file-exists? f2) f2)))
                       *graphics-file-extensions*))))
             (let ((ffull-ext (and ffull (file-extension ffull))))
               (cond
                ((and ffull-ext (member ffull-ext '(".jpg" ".jpeg" ".png")))
                 (do-includegraphics-web b1 ffull))
                (true
                 (let ((%fluid-var-*imgpreamble-inferred*
                        (cons ':includegraphics *imgpreamble-inferred*))
                       (img-file-stem (next-html-image-file-stem)))
                   (fluid-let
                    ((*imgpreamble-inferred*
                      %fluid-var-*imgpreamble-inferred*))
                    (call-with-lazy-image-port (or ffull f) img-file-stem
                     (lambda (o)
                       (display "\\includegraphics" o)
                       (cond (starred-p (display #\* o)) (else false))
                       (cond
                        (b1 (display #\[ o) (display b1 o) (display #\] o))
                        (else false))
                       (cond
                        (b2 (display #\[ o) (display b2 o) (display #\] o))
                        (else false))
                       (display #\{ o)
                       (display f o)
                       (display #\} o)))
                    (source-img-file img-file-stem)))))))))))))

(define do-xetexpdffile
 (lambda ()
   (let ((pdf-file (get-filename)))
     (let ((height false))
       (let ((rotated false))
         (let ((width false))
           (let ((img-file-stem (next-html-image-file-stem)))
             (let ((img-file
                    (let ((%type 'string)
                          (%ee
                           (list *aux-dir/* img-file-stem
                                 (find-img-file-extn))))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))))
               (begin
                (let* ((%loop-returned false)
                       (%loop-result 0)
                       (return
                        (lambda %args
                          (set! %loop-returned true)
                          (set! %loop-result (and (pair? %args) (car %args))))))
                  (let %loop
                    ()
                    (unless %loop-returned
                      (cond
                       ((eat-word "height")
                        (begin (set! height (get-pixels)) height))
                       ((eat-word "rotated")
                        (begin (set! rotated (get-number)) rotated))
                       ((eat-word "width")
                        (begin (set! width (get-pixels)) width))
                       (true (return))))
                    (if %loop-returned
                        %loop-result
                        (%loop))))
                (cond
                 ((not (file-exists? img-file)) (write-log ':separation-space)
                  (write-log #\{) (write-log pdf-file)
                  (write-log ':separation-space) (write-log "->")
                  (write-log ':separation-space) (write-log img-file)
                  (write-log #\}) (write-log ':separation-space)
                  (ps-to-img pdf-file img-file))
                 (else false))
                (write-log #\() (write-log img-file)
                (write-log ':separation-space) (emit "<img src=\"")
                (emit img-file) (emit "\"")
                (cond (height (emit " height=") (emit height)) (else false))
                (cond
                 (rotated (begin (set! rotated (- rotated)) rotated)
                  (emit " style=\"transform: rotate(") (emit rotated)
                  (emit "deg)\""))
                 (else false))
                (cond (width (emit " width=") (emit width)) (else false))
                (emit ">") (write-log #\))
                (write-log ':separation-space))))))))))

(define do-xetexpicfile
 (lambda ()
   (let ((img-file (get-filename)) (height false) (rotated false) (width false))
     (begin
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (cond
             ((eat-word "height") (begin (set! height (get-pixels)) height))
             ((eat-word "rotated") (begin (set! rotated (get-number)) rotated))
             ((eat-word "width") (begin (set! width (get-pixels)) width))
             (true (return))))
          (if %loop-returned
              %loop-result
              (%loop))))
      (emit "<img src=\"") (emit img-file) (emit "\"")
      (cond (height (emit " height=") (emit height)) (else false))
      (cond
       (rotated (begin (set! rotated (- rotated)) rotated)
        (emit " style=\"transform: rotate(") (emit rotated) (emit "deg)\""))
       (else false))
      (cond (width (emit " width=") (emit width)) (else false)) (emit ">")))))

(define do-resizebox
 (lambda ()
   (let ((arg1 (get-group)))
     (let ((arg2 (get-group)))
       (let ((arg3 (get-group)))
         (let ((%fluid-var-*imgpreamble-inferred*
                (cons ':includegraphics *imgpreamble-inferred*)))
           (fluid-let
            ((*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*))
            (call-with-html-image-port
             (lambda (o)
               (display "\\resizebox" o)
               (display arg1 o)
               (display arg2 o)
               (display arg3 o))))))))))

(define do-mfpic-opengraphsfile
 (lambda ()
   (begin (set! *mfpic-file-stem* (get-filename-possibly-braced))
    *mfpic-file-stem*)
   (cond
    (*mfpic-port*
     (let ((%close-port-arg *mfpic-port*))
       ((if (input-port? %close-port-arg)
            close-input-port
            close-output-port)
        %close-port-arg)))
    (else false))
   (let ((f
          (let ((%type 'string)
                (%ee (list *mfpic-file-stem* *mfpic-tex-file-suffix*)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin
      (set! *mfpic-port*
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
      *mfpic-port*))
   (begin (set! *mfpic-file-num* 0) *mfpic-file-num*)
   (display "\\input mfpic \\usemetapost " *mfpic-port*)
   (newline *mfpic-port*)
   (display "\\opengraphsfile{" *mfpic-port*)
   (display *mfpic-file-stem* *mfpic-port*)
   (display #\} *mfpic-port*)
   (newline *mfpic-port*)
   (tex-def-prim "\\headshape"
    (lambda ()
      (let ((g1 (get-group)))
        (let ((g2 (get-group)))
          (let ((g3 (get-group)))
            (begin (display "\\headshape" *mfpic-port*)
             (display g1 *mfpic-port*) (display g2 *mfpic-port*)
             (display g3 *mfpic-port*) (newline *mfpic-port*)))))))
   (tex-def-prim "\\mfpframesep" eat-dimen)
   (tex-def-prim "\\mftitle" get-group)))

(define do-mfpic-closegraphsfile
 (lambda ()
   (display "\\closegraphsfile" *mfpic-port*)
   (newline *mfpic-port*)
   (let ((%close-port-arg *mfpic-port*))
     ((if (input-port? %close-port-arg)
          close-input-port
          close-output-port)
      %close-port-arg))
   (let ((tex-f
          (let ((%type 'string)
                (%ee (list *mfpic-file-stem* *mfpic-tex-file-suffix*)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         (mp-f
          (let ((%type 'string) (%ee (list *mfpic-file-stem* ".mp")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin
      (cond
       ((not (file-exists? mp-f))
        (let ((%fluid-var-*tex-format* ':plain))
          (fluid-let ((*tex-format* %fluid-var-*tex-format*))
           (call-tex tex-f))))
       (else false))
      (cond ((file-exists? mp-f) (call-mp mp-f)) (else false))))))

(define do-mfpic
 (lambda ()
   (display "\\mfpic" *mfpic-port*)
   (dump-till-end-env "mfpic" *mfpic-port*)
   (display "\\endmfpic" *mfpic-port*)
   (newline *mfpic-port*)
   (begin (set! *mfpic-file-num* (+ *mfpic-file-num* 1)) *mfpic-file-num*)
   (let ((f
          (let ((%type 'string)
                (%ee
                 (list *mfpic-file-stem* "."
                       (write-to-string *mfpic-file-num*))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         (img-file-stem (next-html-image-file-stem)))
     (begin (lazily-make-epsf-image-file f img-file-stem)
      (source-img-file img-file-stem)))))

(define do-following-latex-env-as-image
 (lambda () (do-latex-env-as-image (ungroup (get-group)) ':display)))

(define do-latex-env-as-image
 (lambda (env display-p)
   (let ((env2
          (if (string=? env "align")
              "eqnarray"
              env)))
     (begin
      (cond
       ((char=? (snoop-actual-char) #\*) (get-actual-char)
        (begin
         (set! env
          (let ((%type 'string) (%ee (list env "*")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         env)
        (begin
         (set! env2
          (let ((%type 'string) (%ee (list env2 "*")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         env2))
       (else false))
      (egroup)
      (cond
       (display-p (do-end-para) (emit "<div align=")
        (emit *display-justification*) (emit ">"))
       (else false))
      (call-with-html-image-port
       (lambda (o)
         (display "\\begin{" o)
         (display env2 o)
         (display "}" o)
         (dump-till-end-env env o)
         (display "\\end{" o)
         (display env2 o)
         (display "}" o)
         (newline o)))
      (cond (display-p (emit "</div>") (do-para)) (else false))))))

(define do-box
 (lambda ()
   (let ((%fluid-var-*ignore-active-space-p* true))
     (fluid-let ((*ignore-active-space-p* %fluid-var-*ignore-active-space-p*))
      (ignorespaces) (get-to)))
   (eat-dimen)
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (case c ((#\{) true) ((#\\) (get-ctl-seq))))
   (get-actual-char)
   (bgroup)
   (add-postlude-to-top-frame
    (let ((old-math-mode-p *math-mode-p*)
          (old-in-display-math-p *in-display-math-p*)
          (old-tabular-stack *tabular-stack*)
          (old-ligatures-p *ligatures-p*))
      (begin
       (begin (set! *math-mode-p* false) (set! *in-display-math-p* false)
        (set! *tabular-stack* null) (set! *ligatures-p* true) *ligatures-p*)
       (lambda ()
         (begin (set! *math-mode-p* old-math-mode-p)
          (set! *in-display-math-p* old-in-display-math-p)
          (set! *tabular-stack* old-tabular-stack)
          (set! *ligatures-p* old-ligatures-p) *ligatures-p*)))))))

(define do-latex-frac
 (lambda ()
   (tex2page-string (ungroup (get-token)))
   (emit
    (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
          (true
           (let ((%tmp (+ *math-height* 1)))
             (begin (set! *math-height* %tmp) *math-height*))
           "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>")))
   (tex2page-string (ungroup (get-token)))))

(define do-tex-frac
 (lambda ()
   (ignorespaces)
   (let ((inner-level-p
          (or (not *in-display-math-p*) (not (null? *tabular-stack*))))
         (%fluid-var-*tabular-stack* (cons ':frac *tabular-stack*)))
     (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*))
      (cond
       (inner-level-p (emit "<sup>") (tex2page-string (get-till-char #\/))
        (emit "</sup>/<sub>") (get-actual-char) (ignorespaces)
        (tex2page-string (get-token)) (emit "</sub>"))
       (true (emit "</td><td><table align=left><tr><td align=center>")
        (tex2page-string (get-till-char #\/)) (get-actual-char) (ignorespaces)
        (emit "<hr noshade>") (tex2page-string (get-token))
        (emit "</td></tr></table></td><td>")))))))

(define do-frac
 (lambda ()
   (if (eq? *tex-format* ':latex)
       (do-latex-frac)
       (do-tex-frac))))

(define do-eqno
 (lambda ()
   (cond
    ((not *in-display-math-p*)
     (terror 'do-eqno "You can't use \\eqno in math mode"))
    (else false))
   (emit "</td><td width=10% align=right>")))

(define do-eqalign
 (lambda (type)
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin
      (cond ((eq? c ':eof-object) (terror 'do-eqalign "Missing {"))
            (else false))
      (cond ((not (char=? c #\{)) (terror 'do-eqalign "Missing {"))
            (else false))
      (bgroup)
      (let ((%push-new-stack (cons type *tabular-stack*)))
        (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
      (add-postlude-to-top-frame
       (lambda ()
         (emit "</td></tr>")
         (emit-newline)
         (emit "</table>")
         (emit-newline)
         (cond (*in-display-math-p* (emit "</td><td>")) (else false))
         (pop-tabular-stack type)
         (begin (set! *equation-position* 0) *equation-position*)))
      (cond (*in-display-math-p* (emit "</td><td>")) (else false))
      (emit-newline) (emit "<table><tr><td>")))))

(define do-noalign
 (lambda ()
   (let ((type (car *tabular-stack*)))
     (let ((split-p (member type '(:eqalignno :displaylines))))
       (begin
        (cond
         (split-p (egroup) (emit "</td></tr></table></div>") (emit-newline)
          (do-para))
         (else false))
        (tex2page-string (get-group))
        (cond
         (split-p (do-end-para) (emit-newline)
          (emit "<div align=center><table><tr><td>") (toss-back-char #\{)
          (do-eqalign type))
         (true (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))))))))

(define do-pmatrix
 (lambda ()
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin
      (cond
       ((or (eq? c ':eof-object) (not (char=? c #\{)))
        (terror 'do-pmatrix "Missing {"))
       (else false))
      (bgroup)
      (begin (set! *math-delim-left* ':lparen)
       (set! *math-delim-right* ':rparen) *math-delim-right*)))))

(define do-over
 (lambda ()
   (emit
    (cond ((or (not *in-display-math-p*) *math-script-mode-p*) "/")
          (true
           (let ((%tmp (+ *math-height* 1)))
             (begin (set! *math-height* %tmp) *math-height*))
           "</td></tr><tr><td style=\"height=1pt; background-color: black\"></td></tr><tr><td align=center>")))))

(define eat-till-eol
 (lambda ()
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (get-actual-char)))
           (cond ((or (eq? c ':eof-object) (char=? c #\newline)) (return))
                 (else false))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define eat-till-char
 (lambda (d)
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (get-actual-char)))
           (cond ((or (eq? c ':eof-object) (char=? c d)) (return))
                 (else false))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define do-comment
 (lambda ()
   (eat-till-eol)
   (cond
    ((munched-a-newline-p) (toss-back-char #\newline)
     (toss-back-char #\newline))
    (else false))))

(define string=split
 (lambda (p sepc)
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
             (unless %loop-returned
               (let ((i
                      (let ((%position-v sepc)
                            (%position-s p)
                            (%ee (list ':test char=?)))
                        (let ((%position-from-end (memv ':from-end %ee)))
                          (when %position-from-end
                            (set! %position-from-end
                             (cadr %position-from-end)))
                          (if (string? %position-s)
                              ((if %position-from-end
                                   string-reverse-index
                                   string-index)
                               %position-s %position-v)
                              (list-position %position-v %position-s))))))
                 (begin
                  (cond
                   ((not i)
                    (let ((%push-new-stack (cons p r)))
                      (begin (set! r %push-new-stack) r))
                    (return (reverse r)))
                   (else false))
                  (let ((%push-new-stack (cons (subseq p 0 i) r)))
                    (begin (set! r %push-new-stack) r))
                  (begin (set! p (subseq p (add1 i))) p))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))))

(define string=join
 (lambda (ss sepc)
   (let ((res ""))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (cond ((null? ss) (return res)) (else false)))
         (unless %loop-returned
           (let ((s
                  (let* ((%pop-old-stack ss)
                         (%pop-top-value (car %pop-old-stack)))
                    (begin (set! ss (cdr %pop-old-stack)) ss)
                    %pop-top-value)))
             (begin
              (set! res
               (cond ((string=? res "") s)
                     (true
                      (let ((%type 'string) (%ee (list res (list sepc) s)))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res)))))
              res)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define kpsewhich
 (lambda (f)
   (let ((tmpf
          (let ((%type 'string) (%ee (list *aux-dir/* *jobname* "-Z-Z.temp")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin
      (system
       (let ((%type 'string) (%ee (list "kpsewhich -- " f " > " tmpf)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (let ((f
             (and (file-exists? tmpf)
                  (let ((i
                         (let* ((%f tmpf)
                                (%ee (list ':direction ':input))
                                (%direction (memv ':direction %ee))
                                (%if-exists (memv ':if-exists %ee))
                                (%if-does-not-exist ':error)
                                (%if-does-not-exist-from-user
                                 (memv ':if-does-not-exist %ee)))
                           (when %direction
                             (set! %direction (cadr %direction)))
                           (when %if-exists
                             (set! %if-exists (cadr %if-exists)))
                           (when %if-does-not-exist-from-user
                             (set! %if-does-not-exist
                              (cadr %if-does-not-exist-from-user)))
                           (cond
                            ((eqv? %direction ':output)
                             (when
                                 (and (eqv? %if-exists ':supersede)
                                      (file-exists? %f))
                               (delete-file %f))
                             (open-output-file %f))
                            ((and (not %if-does-not-exist)
                                  (not (file-exists? %f)))
                             false)
                            (else (open-input-file %f))))))
                    (let ((%with-open-file-res
                           (begin
                            (let ((%read-line-res (read-line i)))
                              (when (eof-object? %read-line-res)
                                (set! %read-line-res ':eof-object))
                              %read-line-res))))
                      (begin
                       (cond
                        (i
                         ((if (input-port? i)
                              close-input-port
                              close-output-port)
                          i))
                        (else false))
                       %with-open-file-res))))))
        (begin (ensure-file-deleted tmpf)
         (if (or (not f) (eq? f ':eof-object))
             false
             (let ((f (string-trim f)))
               (cond
                ((=
                  (let ((%length-arg f))
                    ((if (string? %length-arg)
                         string-length
                         length)
                     %length-arg))
                  0)
                 false)
                ((file-exists? f) f) (true false))))))))))

(define find-tex-file
 (lambda (file)
   (let ((file.tex
          (let ((%type 'string) (%ee (list file ".tex")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (or (and (file-exists? file.tex) file.tex) (and (file-exists? file) file)
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
                   (unless %loop-returned
                     (cond ((null? %dolist-l) (return)) (else false)))
                   (unless %loop-returned (set! dir (car %dolist-l)))
                   (unless %loop-returned (set! %dolist-l (cdr %dolist-l)))
                   (unless %loop-returned
                     (let ((f
                            (let ((%type 'string)
                                  (%ee
                                   (list dir *directory-separator* file.tex)))
                              (let ((%res
                                     (if (eq? %type 'string)
                                         ""
                                         null)))
                                (let %concatenate-loop
                                  ((%ee %ee))
                                  (if (null? %ee)
                                      %res
                                      (let ((%a (car %ee)))
                                        (unless (not %a)
                                          (set! %res
                                           (if (eq? %type 'string)
                                               (string-append %res
                                                (if (string? %a)
                                                    %a
                                                    (list->string %a)))
                                               (append %res
                                                       (if (string? %a)
                                                           (string->list %a)
                                                           %a)))))
                                        (%concatenate-loop (cdr %ee)))))
                                %res))))
                       (cond ((file-exists? f) (return f)) (else false))))
                   (unless %loop-returned
                     (let ((f
                            (let ((%type 'string)
                                  (%ee (list dir *directory-separator* file)))
                              (let ((%res
                                     (if (eq? %type 'string)
                                         ""
                                         null)))
                                (let %concatenate-loop
                                  ((%ee %ee))
                                  (if (null? %ee)
                                      %res
                                      (let ((%a (car %ee)))
                                        (unless (not %a)
                                          (set! %res
                                           (if (eq? %type 'string)
                                               (string-append %res
                                                (if (string? %a)
                                                    %a
                                                    (list->string %a)))
                                               (append %res
                                                       (if (string? %a)
                                                           (string->list %a)
                                                           %a)))))
                                        (%concatenate-loop (cdr %ee)))))
                                %res))))
                       (cond ((file-exists? f) (return f)) (else false))))
                   (if %loop-returned
                       %loop-result
                       (%loop)))))
             (kpsewhich file))))))

(define initialize-scm-words
 (lambda ()
   (begin (set! *scm-keywords* (make-table ':test equal?))
    (set! *scm-builtins* (make-table ':test equal?))
    (set! *scm-special-symbols* (make-table ':test equal?))
    (set! *scm-variables* (make-table ':test equal?)) *scm-variables*)
   (for-each
    (lambda (s)
      (begin (table-put! s *scm-keywords* true) (table-get s *scm-keywords*)))
    '("=>" "and" "begin" "begin0" "case" "cond" "define" "define-macro"
      "define-syntax" "defmacro" "defstruct" "delay" "do" "else" "flet"
      "fluid-let" "if" "labels" "lambda" "let" "let*" "let-syntax" "let-values"
      "letrec" "letrec-syntax" "macrolet" "or" "quasiquote" "quote" "set!"
      "syntax-case" "syntax-rules" "unless" "unquote" "unquote-splicing" "when"
      "with" "with-handlers" "assert" "block" "decf" "defpackage"
      "defparameter" "defun" "defvar" "destructuring-bind" "do-all-symbols"
      "do-external-symbols" "do-symbols" "dolist" "dotimes" "ecase" "etypecase"
      "eval-when" "handler-bind" "handler-case" "incf" "loop"
      "multiple-value-bind" "multiple-value-setq" "pop" "prog1" "progn" "push"
      "setf" "setq" "typecase" "unwind-protect" "with-input-from-string"
      "with-open-file" "with-open-socket" "with-open-stream"
      "with-output-to-string" "with-slots"))
   true))

(define actual-tex-filename
 (lambda (f . %lambda-rest-arg)
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
         (check-timestamp-p false))
     (when (< 0 %lambda-rest-arg-len)
       (set! check-timestamp-p (list-ref %lambda-rest-arg 0)))
     (let ((doing-main-file-p (not *main-tex-file*))
           (f (and f (find-tex-file f))))
       (begin
        (cond
         (doing-main-file-p
          (cond
           (f (begin (set! *jobname* (file-stem-name f)) *jobname*)
            (make-target-dir)
            (let ((main-html-page
                   (let ((%type 'string)
                         (%ee (list *aux-dir/* *jobname* *output-extension*)))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res))))
              (cond
               ((string=? main-html-page f)
                (let ((f-save
                       (let ((%type 'string) (%ee (list f ".sav")))
                         (let ((%res
                                (if (eq? %type 'string)
                                    ""
                                    null)))
                           (let %concatenate-loop
                             ((%ee %ee))
                             (if (null? %ee)
                                 %res
                                 (let ((%a (car %ee)))
                                   (unless (not %a)
                                     (set! %res
                                      (if (eq? %type 'string)
                                          (string-append %res
                                           (if (string? %a)
                                               %a
                                               (list->string %a)))
                                          (append %res
                                                  (if (string? %a)
                                                      (string->list %a)
                                                      %a)))))
                                   (%concatenate-loop (cdr %ee)))))
                           %res))))
                  (begin (write-log ':separation-newline)
                   (write-log "Copying weirdly named TeX source file ")
                   (write-log f) (write-log " to ") (write-log f-save)
                   (write-log ':separation-newline)
                   (system
                    (let ((%type 'string) (%ee (list "cp -pf " f " " f-save)))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res)))
                   (begin (set! f f-save) f))))
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
        f)))))

(define add-dot-tex-if-no-extension-provided
 (lambda (f)
   (let ((e (file-extension f)))
     (if e
         f
         (let ((%type 'string) (%ee (list f ".tex")))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res))))))

(define ignore-tex-specific-text
 (lambda (env)
   (let ((endenv
          (let ((%type 'string) (%ee (list "\\end" env)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((c (snoop-actual-char)))
             (begin
              (cond
               ((eq? c ':eof-object)
                (terror 'ignore-tex-specific-text "Missing \\end" env))
               (else false))
              (cond
               ((esc-char-p c)
                (let ((x (get-ctl-seq)))
                  (cond ((string=? x endenv) (return))
                        ((string=? x "\\end")
                         (let ((g (get-grouped-environment-name-if-any)))
                           (cond ((and g (string=? g env)) (return))
                                 (else false)))))))
               (true (get-actual-char))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-rawhtml
 (lambda ()
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (snoop-actual-char)))
           (cond
            ((eq? c ':eof-object) (terror 'do-rawhtml "missing \\endrawhtml"))
            ((esc-char-p c)
             (let ((x (get-ctl-seq)))
               (let ((y (find-corresp-prim x)))
                 (cond ((string=? y "\\endrawhtml") (return))
                       ((and (string=? y "\\end")
                             (begin
                              (set! *it* (get-grouped-environment-name-if-any))
                              *it*))
                        (let ((g *it*))
                          (let ((y
                                 (find-corresp-prim
                                  (let ((%type 'string) (%ee (list x g)))
                                    (let ((%res
                                           (if (eq? %type 'string)
                                               ""
                                               null)))
                                      (let %concatenate-loop
                                        ((%ee %ee))
                                        (if (null? %ee)
                                            %res
                                            (let ((%a (car %ee)))
                                              (unless (not %a)
                                                (set! %res
                                                 (if (eq? %type 'string)
                                                     (string-append %res
                                                      (if (string? %a)
                                                          %a
                                                          (list->string %a)))
                                                     (append %res
                                                             (if (string? %a)
                                                                 (string->list
                                                                  %a)
                                                                 %a)))))
                                              (%concatenate-loop (cdr %ee)))))
                                      %res)))))
                            (if (string=? y "\\endrawhtml")
                                (return)
                                (begin (emit "\\end{") (emit g) (emit "}"))))))
                       ((string=? x "\\\\") (emit c) (toss-back-char c))
                       (true (emit x))))))
            (true (get-actual-char) (emit c)))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define do-htmlheadonly
 (lambda ()
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
         (unless %loop-returned
           (let ((c (snoop-actual-char)))
             (cond
              ((eq? c ':eof-object)
               (begin
                (set! s2
                 (let ((%type 'string) (%ee (list (reverse s))))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res)))
                s2)
               (write-aux `(!html-head ,s2)) (return))
              ((esc-char-p c)
               (begin
                (set! s2
                 (let ((%type 'string) (%ee (list (reverse s))))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res)))
                s2)
               (write-aux `(!html-head ,s2)) (begin (set! s null) s)
               (let ((x (get-ctl-seq)))
                 (cond ((string=? x "\\endhtmlheadonly") (return))
                       ((string=? x "\\input")
                        (let ((f (get-filename-possibly-braced)))
                          (call-with-input-file/buffered f do-htmlheadonly)))
                       (true (write-aux `(!html-head ,x))))))
              (true (get-actual-char)
               (let ((%push-new-stack (cons c s)))
                 (begin (set! s %push-new-stack) s))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define resolve-chardefs
 (lambda (c)
   (cond
    ((begin (set! *it* (find-chardef c)) *it*)
     (let ((y *it*))
       (begin (get-actual-char)
        (expand-tex-macro (cdef*-optarg y) (cdef*-argpat y)
         (cdef*-expansion y)))))
    (else false))))

(define resolve-defs
 (lambda (x)
   (cond
    ((begin (set! *it* (find-def x)) *it*)
     (let ((y *it*))
       (cond ((begin (set! *it* (tdef*-defer y)) *it*) *it*)
             ((tdef*-thunk y) false)
             ((and (inside-false-world-p) (not (if-aware-ctl-seq-p x))) false)
             (true
              (cond
               (*outer-p* (begin (set! *outer-p* false) *outer-p*)
                (toss-back-char *outer-invisible-space*))
               (else false))
              (expand-tex-macro (tdef*-optarg y) (tdef*-argpat y)
               (tdef*-expansion y))))))
    (else false))))

(define do-expandafter
 (lambda ()
   (let ((first (get-raw-token/is)))
     (let ((second (get-raw-token/is)))
       (begin (toss-back-char *invisible-space*)
        (cond
         ((ctl-seq-p second)
          (toss-back-string (expand-ctl-seq-into-string second)))
         (true (toss-back-string second)))
        (toss-back-char *invisible-space*) (toss-back-string first))))))

(define resolve-expandafters
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond
      ((esc-char-p c)
       (let ((x (get-ctl-seq)))
         (if (string=? x "\\expandafter")
             (do-expandafter)
             (begin (toss-back-char *invisible-space*) (toss-back-string x)))))
      (else false)))))

(define do-futurelet
 (lambda ()
   (let ((first (get-raw-token/is)))
     (let ((second (get-raw-token/is)))
       (let ((third (get-raw-token)))
         (do-futurelet-aux first second third))))))

(define do-futurenonspacelet
 (lambda ()
   (let ((first (get-raw-token/is)))
     (let ((second (get-raw-token/is)))
       (let ((third (get-raw-token/is)))
         (do-futurelet-aux first second third))))))

(define do-futurelet-aux
 (lambda (first second third)
   (tex-let-general first third false)
   (toss-back-char *invisible-space*)
   (toss-back-string third)
   (toss-back-char *invisible-space*)
   (toss-back-string second)))

(define set-start-time
 (lambda ()
   (apply
    (lambda (s m h d mo y . ign)
      false
      (tex-def-count "\\time" (+ (* 60 h) m) true)
      (tex-def-count "\\day" d true)
      (tex-def-count "\\month" mo true)
      (tex-def-count "\\year" y true))
    (decode-universal-time (current-seconds)))))

(define initialize-globals
 (lambda ()
   (begin (set! *global-texframe* (make-texframe*))
    (set! *section-counter-dependencies* (make-table))
    (set! *dotted-counters* (make-table ':test equal?)) *dotted-counters*)
   (tex-def-count "\\language" 256 true)
   (tex-def-count "\\secnumdepth" -2 true)
   (tex-def-count "\\tocdepth" -2 true)
   (tex-def-count "\\footnotenumber" 0 true)
   (tex-def-count "\\TIIPtabularborder" 1 true)
   (tex-def-count "\\TIIPnestedtabularborder" 0 true)
   (tex-def-count "\\TIIPobeyspacestrictly" 0 true)
   (tex-def-count "\\TIIPobeylinestrictly" 0 true)
   (tex-def-count "\\TIIPsettabscolumns" 0 true)
   (tex-def-count "\\errorcontextlines" 5 true)
   (tex-def-count "\\doublehyphendemerits" 10000 true)
   (tex-def-count "\\finalhyphendemerits" 5000 true)
   (tex-def-count "\\hyphenpenalty" 50 true)
   (tex-def-count "\\exhyphenpenalty" 50 true)
   (tex-def-count "\\pretolerance" 100 true)
   (tex-def-count "\\tolerance" 200 true)
   (tex-def-count "\\hbadness" 1000 true)
   (tex-def-count "\\widowpenalty" 150 true)
   (tex-def-count "\\showboxdepth" 3 true)
   (tex-def-count "\\outputpenalty" 0 true)
   (tex-def-count "\\globaldefs" 0 true)
   (tex-def-count "\\mag" 1000 true)
   (tex-def-count "\\tracingcommands" 0 true)
   (tex-def-count "\\tracingmacros" 0 true)
   (tex-def-count "\\tracingonline" 0 true)
   (tex-def-count "\\time" 0 true)
   (tex-def-count "\\day" 0 true)
   (tex-def-count "\\month" 0 true)
   (tex-def-count "\\year" 0 true)
   (tex-def-count "\\shellescape" 1 true)
   (tex-def-count "\\suppressfontnotfounderror" 1 true)
   (tex-def-dimen "\\TIIPhsize" 0 true)
   (tex-def-dimen "\\hsize" (tex-length 6.5 ':in) true)
   (tex-def-dimen "\\vsize" (tex-length 8.9 ':in) true)
   (tex-def-dimen "\\maxdepth" (tex-length 4 ':pt) true)
   (tex-def-dimen "\\delimitershortfall" (tex-length 5 ':pt) true)
   (tex-def-dimen "\\nulldelimiterspace" (tex-length 1.2 ':pt) true)
   (tex-def-dimen "\\scriptspace" (tex-length 0.5 ':pt) true)
   (tex-def-dimen "\\hoffset" 0 true)
   (tex-def-dimen "\\voffset" 0 true)
   (tex-def-dimen "\\epsfxsize" 0 true)
   (tex-def-dimen "\\epsfysize" 0 true)
   (tex-def-dimen "\\emergencystretch" 0 true)
   (tex-def-dimen "\\hfuzz" (tex-length 0.1 ':pt) true)
   (tex-def-dimen "\\vfuzz" (tex-length 0.1 ':pt) true)
   (tex-def-dimen "\\textwidth" (tex-length 6.5 ':in) true)
   (tex-def-dimen "\\smallskipamount" (tex-length 3 ':pt) true)
   (tex-def-dimen "\\medskipamount" (tex-length 6 ':pt) true)
   (tex-def-dimen "\\bigskipamount" (tex-length 12 ':pt) true)
   (tex-def-dimen "\\lastskip" 0 true)
   (tex-def-dimen "\\baselineskip" (tex-length 12 ':pt) true)
   (tex-def-dimen "\\overfullrule" (tex-length 5 ':pt) true)
   (tex-def-dimen "\\parindent" (tex-length 20 ':pt) true)
   (tex-def-dimen "\\leftskip" 0 true)
   (tex-def-dimen "\\parfillskip" 0 true)
   (tex-def-dimen "\\parskip" 0 true)
   (tex-def-dimen "\\abovedisplayskip" (tex-length 12 ':pt) true)
   (tex-def-dimen "\\belowdisplayskip" (tex-length 12 ':pt) true)
   (tex-def-toks "\\everypar" "" true)
   (tex-def-toks "\\headline" "" true)
   (tex-def-toks "\\footline" "\\folio" true)
   (tex-def-dotted-count "figure" false)
   (tex-def-dotted-count "table" false)
   (tex-def-dotted-count "equation" false)
   (tex-gdef-0arg "\\TIIPcurrentnodename" "no value yet")
   (tex-gdef-0arg "\\@currentlabel" "no value yet")
   (tex-gdef-0arg "\\TZPcolophontimestamp" "1")
   (tex-gdef-0arg "\\TZPcolophoncredit" "1")
   (tex-gdef-0arg "\\TZPcolophonweblink" "1")
   (tex-gdef-0arg "\\TZPimageformat" "PNG")
   (tex-gdef-0arg "\\TZPimageconverter" "NetPBM")
   (tex-gdef-0arg "\\TZPredirectseconds" "0")
   (tex-gdef-0arg "\\TZPtextext" "1")
   (tex-gdef-0arg "\\TZPraggedright" "1")
   (tex-gdef-0arg "\\TZPcommonlisp"
    (if null
        "0"
        "1"))
   (initialize-scm-words)))

(define find-def
 (lambda (ctlseq)
   (or
    (ormap (lambda (fr) (table-get ctlseq (texframe*-definitions fr)))
     *tex-env*)
    (and *global-texframe*
         (table-get ctlseq (texframe*-definitions *global-texframe*)))
    (table-get ctlseq (texframe*-definitions *primitive-texframe*)))))

(define find-math-def
 (lambda (ctlseq)
   (table-get ctlseq (texframe*-definitions *math-primitive-texframe*))))

(define find-count
 (lambda (ctlseq)
   (or (ormap (lambda (fr) (table-get ctlseq (texframe*-counts fr))) *tex-env*)
       (table-get ctlseq (texframe*-counts *global-texframe*))
       (table-get ctlseq (texframe*-counts *primitive-texframe*)))))

(define find-toks
 (lambda (ctlseq)
   (or (ormap (lambda (fr) (table-get ctlseq (texframe*-toks fr))) *tex-env*)
       (table-get ctlseq (texframe*-toks *global-texframe*))
       (table-get ctlseq (texframe*-toks *primitive-texframe*)))))

(define find-dimen
 (lambda (ctlseq)
   (or (ormap (lambda (fr) (table-get ctlseq (texframe*-dimens fr))) *tex-env*)
       (table-get ctlseq (texframe*-dimens *global-texframe*))
       (table-get ctlseq (texframe*-dimens *primitive-texframe*)))))

(define get-toks (lambda (ctlseq) (or (find-toks ctlseq) (terror 'get-toks))))

(define get-dimen
 (lambda (ctlseq) (cond ((find-dimen ctlseq)) (true (tex-length 6.5 ':in)))))

(define the-count
 (lambda (dracula) (or (find-count dracula) (terror 'the-count))))

(define do-count=
 (lambda (z globalp) (get-equal-sign) (tex-def-count z (get-number) globalp)))

(define do-toks=
 (lambda (z globalp) (get-equal-sign) (tex-def-toks z (get-group) globalp)))

(define do-dimen=
 (lambda (z globalp)
   (get-equal-sign)
   (tex-def-dimen z (get-scaled-points) globalp)
   (ignorespaces)))

(define get-gcount
 (lambda (ctlseq) (table-get ctlseq (texframe*-counts *global-texframe*) 0)))

(define tex-gdef-count (lambda (ctlseq v) (tex-def-count ctlseq v true)))

(define do-number (lambda () (emit (get-number))))

(define do-magnification (lambda () (tex-def-count "\\mag" (get-number) false)))

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
   (let ((%type 'string) (%ee (list (write-to-string (/ sp 65536.0)) "pt")))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define expand-the
 (lambda ()
   (let ((ctlseq (get-ctl-seq)))
     (cond
      ((begin (set! *it* (find-dimen ctlseq)) *it*)
       (scaled-point-to-tex-point *it*))
      ((begin (set! *it* (get-number-corresp-to-ctl-seq ctlseq)) *it*) *it*)
      ((find-toks ctlseq)) (true (trace-if false "expand-the failed"))))))

(define do-the
 (lambda ()
   (let ((ctlseq (get-ctl-seq)))
     (cond
      ((begin (set! *it* (find-dimen ctlseq)) *it*)
       (emit (scaled-point-to-tex-point *it*)))
      ((begin (set! *it* (get-number-corresp-to-ctl-seq ctlseq)) *it*)
       (emit *it*))
      ((begin (set! *it* (find-toks ctlseq)) *it*) (tex2page-string *it*))
      (true (trace-if false "do-the failed"))))))

(define do-arabic
 (lambda ()
   (let ((counter-name (ungroup (get-group))))
     (let ((counter (table-get counter-name *dotted-counters*)))
       (let ((it false))
         (cond ((begin (set! it (counter*-value counter)) it) (emit it))
               (true (trace-if false "do-arabic failed"))))))))

(define find-corresp-prim
 (lambda (ctlseq)
   (let ((y (find-def ctlseq)))
     (or (and y (tdef*-defer y)) ctlseq))))

(define find-corresp-prim-thunk
 (lambda (ctlseq)
   (let ((y (find-def ctlseq)))
     (if (and y (tdef*-thunk y))
         (tdef*-prim y)
         ctlseq))))

(define globally-p (lambda () (> (get-gcount "\\globaldefs") 0)))

(define do-let
 (lambda (globalp)
   (cond
    ((not (inside-false-world-p)) (ignorespaces)
     (let ((lhs (get-ctl-seq)))
       (let ((rhs (begin (get-equal-sign) (get-raw-token/is))))
         (let ((frame (and globalp *global-texframe*)))
           (tex-let-general lhs rhs frame)))))
    (else false))))

(define do-def
 (lambda (globalp e-p)
   (cond
    ((not (inside-false-world-p))
     (let ((lhs (get-raw-token/is)))
       (begin
        (cond
         ((and (ctl-seq-p lhs) (string=? lhs "\\TIIPcsname"))
          (begin (set! lhs (get-peeled-group)) lhs))
         (else false))
        (let ((argpat (get-def-arguments lhs)))
          (let ((rhs (ungroup (get-group))))
            (let ((frame (and globalp *global-texframe*)))
              (begin
               (cond (e-p (begin (set! rhs (expand-edef-macro rhs)) rhs))
                     (else false))
               (cond
                ((ctl-seq-p lhs)
                 (tex-def lhs argpat rhs false false false false frame))
                (true
                 (tex-def-char (string-ref lhs 0) argpat rhs frame))))))))))
    (else false))))

(define do-newcount (lambda (globalp) (tex-def-count (get-ctl-seq) 0 globalp)))

(define do-newtoks (lambda (globalp) (tex-def-toks (get-ctl-seq) "" globalp)))

(define do-newdimen (lambda (globalp) (tex-def-dimen (get-ctl-seq) 0 globalp)))

(define do-advance
 (lambda (globalp)
   (let ((ctlseq (get-ctl-seq)))
     (let ((count (find-count ctlseq)))
       (begin (get-by)
        (if count
            (tex-def-count ctlseq (+ count (get-number)) globalp)
            (eat-dimen)))))))

(define do-multiply
 (lambda (globalp)
   (let ((ctlseq (get-ctl-seq)))
     (let ((curr-val (find-count ctlseq)))
       (begin (get-by)
        (tex-def-count ctlseq (* curr-val (get-number)) globalp))))))

(define do-divide
 (lambda (globalp)
   (let ((ctlseq (get-ctl-seq)))
     (let ((curr-val (find-count ctlseq)))
       (begin (get-by)
        (tex-def-count ctlseq (quotient curr-val (get-number)) globalp))))))

(define do-newcommand
 (lambda (renewp)
   (ignorespaces)
   (let ((lhs (string-trim (ungroup (get-token)))))
     (let ((optarg false))
       (let ((argc
              (cond
               ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                (let ((s *it*))
                  (begin
                   (cond
                    ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                     (begin (set! optarg *it*) optarg)))
                   (string->number (string-trim s)))))
               (true 0))))
         (let ((rhs (ungroup (get-token))))
           (let ((ok-to-def-p (or renewp (not (find-def lhs)))))
             (begin
              (tex-def lhs (latex-argnum-to-plain-argpat argc) rhs optarg false
               false false false)
              (cond
               ((not ok-to-def-p)
                (trace-if (> (find-count "\\tracingcommands") 0) lhs
                 " already defined"))
               (else false))))))))))

(define do-newcounter
 (lambda ()
   (let ((counter-name (ungroup (get-group))))
     (let ((within (get-bracketed-text-if-any)))
       (tex-def-dotted-count counter-name within)))))

(define do-newenvironment
 (lambda (renewp)
   (ignorespaces)
   (let ((envname (string-trim (ungroup (get-token)))))
     (let ((bs-envname
            (let ((%type 'string) (%ee (list "\\" envname)))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (let ((optarg false))
         (let ((argc
                (cond
                 ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                  (let ((s *it*))
                    (begin
                     (cond
                      ((begin (set! *it* (get-bracketed-text-if-any)) *it*)
                       (begin (set! optarg *it*) optarg)))
                     (string->number (string-trim s)))))
                 (true 0))))
           (let ((beginning
                  (let ((%type 'string)
                        (%ee (list "\\begingroup " (ungroup (get-token)))))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res))))
             (let ((ending
                    (let ((%type 'string)
                          (%ee (list (ungroup (get-token)) "\\endgroup")))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))))
               (let ((ok-to-def-p (or renewp (not (find-def bs-envname)))))
                 (begin
                  (tex-def bs-envname (latex-argnum-to-plain-argpat argc)
                   beginning optarg false false false false)
                  (tex-def
                   (let ((%type 'string) (%ee (list "\\end" envname)))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res))
                   null ending false false false false false)
                  (cond
                   ((not ok-to-def-p)
                    (trace-if true "{" envname "} already defined"))
                   (else false))))))))))))

(define tex-def-dotted-count
 (lambda (counter-name sec-num)
   (cond
    (sec-num
     (cond
      ((not (table-get sec-num *section-counter-dependencies*))
       (begin (table-put! sec-num *section-counter-dependencies* null)
        (table-get sec-num *section-counter-dependencies*)))
      (else false))
     (let ((%push-new-stack
            (cons counter-name
                  (table-get sec-num *section-counter-dependencies*))))
       (begin
        (table-put! sec-num *section-counter-dependencies* %push-new-stack)
        (table-get sec-num *section-counter-dependencies*))))
    (else false))
   (begin
    (table-put! counter-name *dotted-counters*
     (make-counter* ':within sec-num))
    (table-get counter-name *dotted-counters*))))

(define do-newtheorem
 (lambda ()
   (let ((env (ungroup (get-group))))
     (let ((numbered-like (get-bracketed-text-if-any)))
       (let ((counter-name (or numbered-like env)))
         (let ((caption (ungroup (get-group))))
           (let ((within
                  (if numbered-like
                      false
                      (get-bracketed-text-if-any))))
             (let ((sec-num
                    (and within
                         (section-ctl-seq-p
                          (let ((%type 'string) (%ee (list "\\" within)))
                            (let ((%res
                                   (if (eq? %type 'string)
                                       ""
                                       null)))
                              (let %concatenate-loop
                                ((%ee %ee))
                                (if (null? %ee)
                                    %res
                                    (let ((%a (car %ee)))
                                      (unless (not %a)
                                        (set! %res
                                         (if (eq? %type 'string)
                                             (string-append %res
                                              (if (string? %a)
                                                  %a
                                                  (list->string %a)))
                                             (append %res
                                                     (if (string? %a)
                                                         (string->list %a)
                                                         %a)))))
                                      (%concatenate-loop (cdr %ee)))))
                              %res))))))
               (begin
                (cond
                 ((not numbered-like)
                  (tex-def-dotted-count counter-name sec-num))
                 (else false))
                (tex-def
                 (let ((%type 'string) (%ee (list "\\" env)))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))
                 null
                 (let ((%type 'string)
                       (%ee
                        (list "\\par\\begingroup\\TIIPtheorem{" counter-name
                              "}{" caption "}")))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))
                 false false false false *global-texframe*)
                (tex-def
                 (let ((%type 'string) (%ee (list "\\end" env)))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))
                 null "\\endgroup\\par" false false false false
                 *global-texframe*))))))))))

(define do-theorem
 (lambda ()
   (let ((counter-name (ungroup (get-group))))
     (let ((counter (table-get counter-name *dotted-counters*)))
       (let ((caption (ungroup (get-group))))
         (begin (cond ((not counter) (terror 'do-theorem)) (else false))
          (let ((new-counter-value (+ 1 (counter*-value counter))))
            (begin
             (begin (set!counter*-value counter new-counter-value)
              (counter*-value counter))
             (let ((thm-num
                    (let ((sec-num (counter*-within counter)))
                      (if sec-num
                          (let ((%type 'string)
                                (%ee
                                 (list (section-counter-value sec-num) "."
                                       (write-to-string new-counter-value))))
                            (let ((%res
                                   (if (eq? %type 'string)
                                       ""
                                       null)))
                              (let %concatenate-loop
                                ((%ee %ee))
                                (if (null? %ee)
                                    %res
                                    (let ((%a (car %ee)))
                                      (unless (not %a)
                                        (set! %res
                                         (if (eq? %type 'string)
                                             (string-append %res
                                              (if (string? %a)
                                                  %a
                                                  (list->string %a)))
                                             (append %res
                                                     (if (string? %a)
                                                         (string->list %a)
                                                         %a)))))
                                      (%concatenate-loop (cdr %ee)))))
                              %res))
                          (write-to-string new-counter-value)))))
               (let ((lbl
                      (let ((%type 'string)
                            (%ee (list *html-node-prefix* "thm_" thm-num)))
                        (let ((%res
                               (if (eq? %type 'string)
                                   ""
                                   null)))
                          (let %concatenate-loop
                            ((%ee %ee))
                            (if (null? %ee)
                                %res
                                (let ((%a (car %ee)))
                                  (unless (not %a)
                                    (set! %res
                                     (if (eq? %type 'string)
                                         (string-append %res
                                          (if (string? %a)
                                              %a
                                              (list->string %a)))
                                         (append %res
                                                 (if (string? %a)
                                                     (string->list %a)
                                                     %a)))))
                                  (%concatenate-loop (cdr %ee)))))
                          %res))))
                 (begin (tex-def-0arg "\\TIIPcurrentnodename" lbl)
                  (tex-def-0arg "\\@currentlabel" thm-num) (emit-anchor lbl)
                  (emit-newline) (emit "<b>") (emit caption) (emit " ")
                  (emit thm-num) (emit ".</b>") (emit-nbsp 2))))))))))))

(define do-begin
 (lambda ()
   (cond
    ((not (begin (set! *it* (get-grouped-environment-name-if-any)) *it*))
     (terror 'do-begin "\\begin not followed by environment name"))
    (else false))
   (let ((env *it*))
     (begin (toss-back-char *invisible-space*)
      (toss-back-string
       (let ((%type 'string) (%ee (list "\\" env)))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res)))
      (cond
       ((not
         (member env
                 '("htmlonly" "cssblock" "document" "latexonly" "rawhtml"
                   "texonly" "verbatim" "verbatim*")))
        (toss-back-string "\\begingroup") (do-end-para))
       (else false))))))

(define do-end
 (lambda ()
   (cond
    ((begin (set! *it* (get-grouped-environment-name-if-any)) *it*)
     (let ((env *it*))
       (begin (toss-back-char *invisible-space*)
        (cond
         ((not (member env '("htmlonly" "document"))) (do-end-para)
          (toss-back-string "\\endgroup"))
         (else false))
        (toss-back-string
         (let ((%type 'string) (%ee (list "\\end" env)))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res))))))
    (true (toss-back-char *invisible-space*) (toss-back-string "\\TIIPbye")))))

(define latex-argnum-to-plain-argpat
 (lambda (n)
   (let ((n n) (s null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (cond ((<= n 0) (return s)) (else false)))
         (unless %loop-returned
           (let ((%tmp (- n 1)))
             (begin (set! n %tmp) n)))
         (unless %loop-returned
           (let ((%push-new-stack
                  (cons (integer->char (+ *int-corresp-to-0* n)) s)))
             (begin (set! s %push-new-stack) s)))
         (unless %loop-returned
           (let ((%push-new-stack (cons #\# s)))
             (begin (set! s %push-new-stack) s)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define make-reusable-img
 (lambda (globalp)
   (let ((%tmp (+ *imgdef-file-count* 1)))
     (begin (set! *imgdef-file-count* %tmp) *imgdef-file-count*))
   (ignorespaces)
   (let ((lhs (get-ctl-seq))
         (imgdef-file-stem
          (let ((%type 'string)
                (%ee
                 (list *subjobname* *img-file-suffix* *imgdef-file-suffix*
                       (write-to-string *imgdef-file-count*))))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin (dump-imgdef imgdef-file-stem) (tex-to-img imgdef-file-stem)
      (tex-def lhs null
       (let ((%type 'string)
             (%ee (list "\\TIIPreuseimage{" imgdef-file-stem "}")))
         (let ((%res
                (if (eq? %type 'string)
                    ""
                    null)))
           (let %concatenate-loop
             ((%ee %ee))
             (if (null? %ee)
                 %res
                 (let ((%a (car %ee)))
                   (unless (not %a)
                     (set! %res
                      (if (eq? %type 'string)
                          (string-append %res
                           (if (string? %a)
                               %a
                               (list->string %a)))
                          (append %res
                                  (if (string? %a)
                                      (string->list %a)
                                      %a)))))
                   (%concatenate-loop (cdr %ee)))))
           %res))
       false false false false (and globalp *global-texframe*))))))

(define valid-img-file-p (lambda (f) false false))

(define source-img-file
 (lambda (img-file-stem . alt)
   (let ((alt
          (if (null? alt)
              false
              (car alt))))
     (let ((img-file
            (let ((%type 'string)
                  (%ee (list img-file-stem (find-img-file-extn))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (let ((f
              (let ((%type 'string) (%ee (list *aux-dir/* img-file)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (begin (write-log #\() (write-log f) (write-log ':separation-space)
          (valid-img-file-p f) (emit "<img src=\"") (emit img-file)
          (emit "\" border=\"0\" alt=\"")
          (cond (alt (emit alt)) (true (emit "[") (emit img-file) (emit "]")))
          (emit "\">") (write-log #\)) (write-log ':separation-space) true))))))

(define reuse-img (lambda () (source-img-file (ungroup (get-group)))))

(define get-def-arguments
 (lambda (lhs)
   (letrec
    ((aux
      (lambda ()
        (let ((c (snoop-actual-char)))
          (begin
           (cond
            ((eq? c ':eof-object)
             (terror 'get-def-arguments
              "EOF found while scanning definition of " lhs))
            (else false))
           (cond
            ((esc-char-p c)
             (let ((x (get-ctl-seq)))
               (if (string=? x "\\par")
                   (cons #\newline (cons #\newline (aux)))
                   (append
                    (let ((%type 'list) (%ee (list x)))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res))
                    (aux)))))
            ((char=? c #\{) null)
            (true
             (cond ((char=? c #\newline) (get-actual-char) (ignorespaces))
                   ((char-whitespace? c) (ignorespaces) (begin (set! c #\ ) c))
                   (true (get-actual-char)))
             (cons c (aux)))))))))
    (aux))))

(define get-till-char
 (lambda (c0)
   (let ((%type 'string)
         (%ee
          (list
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
                  (unless %loop-returned
                    (let ((c (snoop-actual-char)))
                      (begin
                       (cond
                        ((eq? c ':eof-object)
                         (terror 'get-till-char "File ended too soon"))
                        (else false))
                       (cond
                        (escape-p
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s))
                         (begin (set! escape-p false) escape-p))
                        ((char=? c c0) (return s))
                        ((esc-char-p c)
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s))
                         (begin (set! escape-p true) escape-p))
                        ((char=? c #\{)
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s))
                         (let ((%tmp (+ nesting 1)))
                           (begin (set! nesting %tmp) nesting)))
                        ((char=? c #\})
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s))
                         (let ((%tmp (- nesting 1)))
                           (begin (set! nesting %tmp) nesting)))
                        ((> nesting 0)
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s)))
                        ((and (char-whitespace? c) (not (char=? c0 #\newline))
                              (char-whitespace? c0))
                         (return s))
                        (true
                         (let ((%push-new-stack (cons (get-actual-char) s)))
                           (begin (set! s %push-new-stack) s)))))))
                  (if %loop-returned
                      %loop-result
                      (%loop)))))))))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define digit-to-int (lambda (d) (- (char->integer d) *int-corresp-to-0*)))

(define do-halign
 (lambda ()
   (do-end-para)
   (ignorespaces)
   (let ((c (get-actual-char)))
     (cond
      ((or (eq? c ':eof-object) (not (char=? c #\{)))
       (terror 'do-halign "Missing {"))
      (else false)))
   (let ((%fluid-var-*tabular-stack* (cons ':halign *tabular-stack*)))
     (fluid-let ((*tabular-stack* %fluid-var-*tabular-stack*)) (bgroup)
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
            (unless %loop-returned (ignorespaces))
            (unless %loop-returned
              (let ((c (snoop-actual-char)))
                (begin
                 (cond
                  ((eq? c ':eof-object)
                   (terror 'do-halign "Eof inside \\halign"))
                  (else false))
                 (cond
                  ((char=? c #\}) (get-actual-char) (emit "</table>")
                   (emit-newline) (egroup) (do-para) (return))
                  (true (expand-halign-line tmplt))))))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define get-halign-template
 (lambda ()
   (let ((s null))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((x (get-raw-token)))
             (begin
              (cond
               ((eq? x ':eof-object)
                (terror 'get-halign-template "Eof in \\halign"))
               (else false))
              (cond
               ((string=? x "\\cr")
                (let ((%push-new-stack (cons false s)))
                  (begin (set! s %push-new-stack) s))
                (return (reverse s)))
               ((string=? x "#")
                (let ((%push-new-stack (cons true s)))
                  (begin (set! s %push-new-stack) s)))
               ((string=? x "&")
                (let ((%push-new-stack (cons false s)))
                  (begin (set! s %push-new-stack) s)))
               (true
                (let ((%push-new-stack (cons x s)))
                  (begin (set! s %push-new-stack) s)))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define expand-halign-line
 (lambda (tmplt)
   (emit "<tr>")
   (let ((tmplt tmplt) (ins " ") (outer-loop-done false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (cond (outer-loop-done (return)) (else false)))
         (unless %loop-returned
           (let ((x (get-raw-token)))
             (begin
              (cond
               ((eq? x ':eof-object)
                (terror 'expand-halign-line "Eof in \\halign"))
               (else false))
              (cond
               ((or (string=? x "&") (string=? x "\\cr"))
                (let ((r "{"))
                  (let* ((%loop-returned false)
                         (%loop-result 0)
                         (return
                          (lambda %args
                            (set! %loop-returned true)
                            (set! %loop-result
                             (and (pair? %args) (car %args))))))
                    (let %loop
                      ()
                      (unless %loop-returned
                        (cond
                         ((null? tmplt)
                          (terror 'expand-halign-line "Eof in \\halign"))
                         (else false)))
                      (unless %loop-returned
                        (let ((y (car tmplt)))
                          (case y
                            ((())
                             (emit "<td>")
                             (tex2page-string
                              (let ((%type 'string) (%ee (list r "}")))
                                (let ((%res
                                       (if (eq? %type 'string)
                                           ""
                                           null)))
                                  (let %concatenate-loop
                                    ((%ee %ee))
                                    (if (null? %ee)
                                        %res
                                        (let ((%a (car %ee)))
                                          (unless (not %a)
                                            (set! %res
                                             (if (eq? %type 'string)
                                                 (string-append %res
                                                  (if (string? %a)
                                                      %a
                                                      (list->string %a)))
                                                 (append %res
                                                         (if (string? %a)
                                                             (string->list %a)
                                                             %a)))))
                                          (%concatenate-loop (cdr %ee)))))
                                  %res)))
                             (cond
                              ((and (string=? x "\\cr") (string=? ins " "))
                               (emit-nbsp 1))
                              (else false))
                             (emit "</td>")
                             (if (string=? x "\\cr")
                                 (begin (emit "</tr>") (emit-newline)
                                  (begin (set! outer-loop-done true)
                                   outer-loop-done)
                                  (return))
                                 (begin
                                  (let* ((%pop-old-stack tmplt)
                                         (%pop-top-value (car %pop-old-stack)))
                                    (begin (set! tmplt (cdr %pop-old-stack))
                                     tmplt)
                                    %pop-top-value)
                                  (begin (set! ins " ") ins) (return))))
                            ((true)
                             (let* ((%pop-old-stack tmplt)
                                    (%pop-top-value (car %pop-old-stack)))
                               (begin (set! tmplt (cdr %pop-old-stack)) tmplt)
                               %pop-top-value)
                             (begin
                              (set! r
                               (let ((%type 'string) (%ee (list r ins)))
                                 (let ((%res
                                        (if (eq? %type 'string)
                                            ""
                                            null)))
                                   (let %concatenate-loop
                                     ((%ee %ee))
                                     (if (null? %ee)
                                         %res
                                         (let ((%a (car %ee)))
                                           (unless (not %a)
                                             (set! %res
                                              (if (eq? %type 'string)
                                                  (string-append %res
                                                   (if (string? %a)
                                                       %a
                                                       (list->string %a)))
                                                  (append %res
                                                          (if (string? %a)
                                                              (string->list %a)
                                                              %a)))))
                                           (%concatenate-loop (cdr %ee)))))
                                   %res)))
                              r))
                            (else
                             (let* ((%pop-old-stack tmplt)
                                    (%pop-top-value (car %pop-old-stack)))
                               (begin (set! tmplt (cdr %pop-old-stack)) tmplt)
                               %pop-top-value)
                             (begin
                              (set! r
                               (let ((%type 'string) (%ee (list r y)))
                                 (let ((%res
                                        (if (eq? %type 'string)
                                            ""
                                            null)))
                                   (let %concatenate-loop
                                     ((%ee %ee))
                                     (if (null? %ee)
                                         %res
                                         (let ((%a (car %ee)))
                                           (unless (not %a)
                                             (set! %res
                                              (if (eq? %type 'string)
                                                  (string-append %res
                                                   (if (string? %a)
                                                       %a
                                                       (list->string %a)))
                                                  (append %res
                                                          (if (string? %a)
                                                              (string->list %a)
                                                              %a)))))
                                           (%concatenate-loop (cdr %ee)))))
                                   %res)))
                              r)))))
                      (if %loop-returned
                          %loop-result
                          (%loop))))))
               (true
                (begin
                 (set! ins
                  (let ((%type 'string) (%ee (list ins x)))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res)))
                 ins))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-settabs
 (lambda ()
   (let ((settabs-spec ""))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (let ((x (get-token/ps)))
             (begin
              (cond
               ((eq? x ':eof-object) (terror 'do-settabs "Eof in \\settabs"))
               (else false))
              (cond
               ((string=? x "\\columns")
                (tex2page-string
                 (let ((%type 'string)
                       (%ee (list "\\TIIPsettabscolumns=" settabs-spec)))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res)))
                (return))
               ((string=? x "\\cr")
                (tex-def-count "\\TIIPsettabscolumns" 0 false) (return))
               (true
                (begin
                 (set! settabs-spec
                  (let ((%type 'string) (%ee (list settabs-spec x)))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res)))
                 settabs-spec))))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-tabalign
 (lambda ()
   (emit-newline)
   (let ((table-width "") (num-cols (find-count "\\TIIPsettabscolumns")))
     (begin
      (cond
       ((> num-cols 0) (begin (set! table-width " width=100%") table-width))
       (else false))
      (emit "<table") (emit table-width) (emit ">") (emit-newline)
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned (do-tabalign-row num-cols))
          (unless %loop-returned
            (let ((x (get-token/ps)))
              (cond ((eq? x ':eof-object) (return))
                    ((or (string=? x "\\+") (string=? x "\\tabalign")) true)
                    (true (toss-back-string x) (return)))))
          (if %loop-returned
              %loop-result
              (%loop))))))
   (emit "</table>")
   (emit-newline)))

(define do-tabalign-row
 (lambda (num-cols)
   (emit "<tr>")
   (emit-newline)
   (let ((cell-contents "") (cell-width ""))
     (begin
      (cond
       ((> num-cols 0)
        (begin
         (set! cell-width
          (let ((%type 'string)
                (%ee (list " width=" (write-to-string (/ 100.0 num-cols)) "%")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         cell-width))
       (else false))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (let ((x (get-token/ps)))
              (begin
               (cond
                ((eq? x ':eof-object) (terror 'do-tablign "Eof in \\tabalign"))
                (else false))
               (cond
                ((or (string=? x "&") (string=? x "\\cr")) (emit "<td")
                 (emit cell-width) (emit ">") (bgroup)
                 (tex2page-string cell-contents) (egroup)
                 (begin (set! cell-contents "") cell-contents) (emit "</td>")
                 (emit-newline)
                 (cond ((string=? x "\\cr") (return)) (else false)))
                (true
                 (begin
                  (set! cell-contents
                   (let ((%type 'string) (%ee (list cell-contents x)))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res)))
                  cell-contents))))))
          (if %loop-returned
              %loop-result
              (%loop))))))
   (emit "</tr>")
   (emit-newline)))

(define read-till-next-sharp
 (lambda (k argpat)
   (let ((n
          (let ((%length-arg argpat))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg)))
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
         (unless %loop-returned
           (cond
            (outer-loop-done
             (return
              (list i
                    (let ((%type 'string) (%ee (list (reverse ss))))
                      (let ((%res
                             (if (eq? %type 'string)
                                 ""
                                 null)))
                        (let %concatenate-loop
                          ((%ee %ee))
                          (if (null? %ee)
                              %res
                              (let ((%a (car %ee)))
                                (unless (not %a)
                                  (set! %res
                                   (if (eq? %type 'string)
                                       (string-append %res
                                        (if (string? %a)
                                            %a
                                            (list->string %a)))
                                       (append %res
                                               (if (string? %a)
                                                   (string->list %a)
                                                   %a)))))
                                (%concatenate-loop (cdr %ee)))))
                        %res)))))
            (else false)))
         (unless %loop-returned (begin (set! i k) (set! s null) s))
         (unless %loop-returned
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (unless %loop-returned
                 (begin
                  (set! c
                   (if (< i n)
                       (list-ref argpat i)
                       #\#))
                  c))
               (unless %loop-returned
                 (cond
                  ((char=? c #\#)
                   (begin (set! outer-loop-done true) outer-loop-done)
                   (return))
                  (else false)))
               (unless %loop-returned
                 (let ((d (snoop-actual-char)))
                   (cond
                    ((and (char=? c #\ ) (char-whitespace? d)) (ignorespaces)
                     (let ((%tmp (+ i 1)))
                       (begin (set! i %tmp) i))
                     (let ((%push-new-stack (cons c s)))
                       (begin (set! s %push-new-stack) s)))
                    ((and *comment-char* (char=? d *comment-char*))
                     (do-comment))
                    ((and (char=? c #\newline) (char-whitespace? d)
                          (or (munched-a-newline-p)
                              (begin (toss-back-char d) false)))
                     (let ((%tmp (+ i 1)))
                       (begin (set! i %tmp) i))
                     (let ((%push-new-stack (cons c s)))
                       (begin (set! s %push-new-stack) s)))
                    ((char=? c d) (get-actual-char)
                     (let ((%tmp (+ i 1)))
                       (begin (set! i %tmp) i))
                     (let ((%push-new-stack (cons c s)))
                       (begin (set! s %push-new-stack) s)))
                    ((= i k)
                     (begin
                      (set! ss
                       (if (and (char=? d #\{)
                                (or (null? ss) (not (esc-char-p (car ss)))))
                           (append (get-group-as-reversed-chars) ss)
                           (begin
                            (if (and (char-whitespace? d)
                                     (not (char=? d #\newline)))
                                (ignorespaces)
                                (get-actual-char))
                            (cons d ss))))
                      ss)
                     (return))
                    (true (begin (set! ss (append s ss)) ss) (return)))))
               (if %loop-returned
                   %loop-result
                   (%loop)))))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define read-macro-args
 (lambda (argpat k r)
   (let ((n
          (let ((%length-arg argpat))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
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
            (unless %loop-returned (cond ((>= k n) (return r)) (else false)))
            (unless %loop-returned
              (let ((c (list-ref argpat k)))
                (cond
                 ((char=? c #\#)
                  (cond
                   ((= k (sub1 n)) (ignorespaces)
                    (let ((%push-new-stack (cons (get-till-char #\{) r)))
                      (begin (set! r %push-new-stack) r))
                    (return r))
                   ((= k (- n 2))
                    (let ((%push-new-stack (cons (ungroup (get-token)) r)))
                      (begin (set! r %push-new-stack) r))
                    (return r))
                   (true
                    (let ((c2 (list-ref argpat (+ k 2))))
                      (if (char=? c2 #\#)
                          (begin
                           (let ((%tmp (+ k 2)))
                             (begin (set! k %tmp) k))
                           (let ((%push-new-stack
                                  (cons (ungroup (get-token)) r)))
                             (begin (set! r %push-new-stack) r)))
                          (apply
                           (lambda (k2 s)
                             (begin (set! k k2) k)
                             (let ((%push-new-stack (cons s r)))
                               (begin (set! r %push-new-stack) r)))
                           (read-till-next-sharp (+ k 2) argpat)))))))
                 (true
                  (let ((d (get-actual-char)))
                    (begin
                     (cond
                      ((eq? d ':eof-object)
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
                      (true
                       (terror 'read-macro-args
                        "Use of macro doesn't match its definition.")))
                     (let ((%tmp (+ k 1)))
                       (begin (set! k %tmp) k))))))))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define expand-edef-macro
 (lambda (rhs)
   (let ((%fluid-var-*not-processing-p* true))
     (fluid-let ((*not-processing-p* %fluid-var-*not-processing-p*))
      (let ((tmp-port (open-output-string)))
        (begin
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
                (unless %loop-returned
                  (let ((c (snoop-actual-char)))
                    (begin (cond ((eq? c ':eof-object) (return)) (else false))
                     (display
                      (cond
                       ((esc-char-p c)
                        (let ((x (get-ctl-seq)))
                          (begin (toss-back-char *invisible-space*)
                           (cond
                            ((or (string=? x "\\the") (string=? x "\\number"))
                             (let ((x2 (get-raw-token/is)))
                               (begin (toss-back-char *invisible-space*)
                                (toss-back-string x2)
                                (cond
                                 ((ctl-seq-p x2)
                                  (cond ((string=? x "\\the") (expand-the))
                                        ((string=? x "\\number") (get-number))
                                        (true "DeAdCoDe")))
                                 (true x)))))
                            ((string=? x "\\noexpand")
                             (let ((x2 (get-raw-token/is)))
                               (begin (toss-back-char *invisible-space*) x2)))
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
                                (true x))))
                            (true x)))))
                       (true (get-actual-char) c))
                      tmp-port))))
                (if %loop-returned
                    %loop-result
                    (%loop))))))
         (get-output-string tmp-port)))))))

(define expand-tex-macro
 (lambda (optarg argpat rhs)
   (let ((k 0))
     (let ((r
            (if (not optarg)
                null
                (begin (begin (set! k 2) k)
                 (list
                  (cond
                   ((begin (set! *it* (get-bracketed-text-if-any)) *it*) *it*)
                   (true optarg)))))))
       (let ((args (read-macro-args argpat k r)))
         (let ((rhs-n
                (let ((%length-arg rhs))
                  ((if (string? %length-arg)
                       string-length
                       length)
                   %length-arg))))
           (let ((%type 'string)
                 (%ee
                  (list
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
                                      (unless %loop-returned
                                        (cond
                                         ((>= j rhs-n) (return (reverse s)))
                                         (else false)))
                                      (unless %loop-returned
                                        (let ((c (string-ref rhs j)))
                                          (cond
                                           ((char-alphabetic? c)
                                            (let ((%tmp (+ j 1)))
                                              (begin (set! j %tmp) j))
                                            (let ((%push-new-stack (cons c s)))
                                              (begin (set! s %push-new-stack)
                                               s)))
                                           ((and (char=? c #\#)
                                                 (>
                                                  (let ((%length-arg s))
                                                    ((if (string? %length-arg)
                                                         string-length
                                                         length)
                                                     %length-arg))
                                                  1))
                                            (return
                                             (append (reverse s)
                                                     (cons #\  (aux j)))))
                                           ((=
                                             (let ((%length-arg s))
                                               ((if (string? %length-arg)
                                                    string-length
                                                    length)
                                                %length-arg))
                                             1)
                                            (return
                                             (append (reverse (cons c s))
                                                     (aux (add1 j)))))
                                           (true
                                            (return
                                             (append (reverse s) (aux j)))))))
                                      (if %loop-returned
                                          %loop-result
                                          (%loop))))))
                               ((char=? c #\#)
                                (if (= k (sub1 rhs-n))
                                    (list #\#)
                                    (let ((n (string-ref rhs (add1 k))))
                                      (cond
                                       ((char=? n #\#)
                                        (cons #\# (aux (+ k 2))))
                                       ((and (char-numeric? n)
                                             (<= (digit-to-int n)
                                                 (let ((%length-arg args))
                                                   ((if (string? %length-arg)
                                                        string-length
                                                        length)
                                                    %length-arg))))
                                        (append
                                         (let ((%type 'list)
                                               (%ee
                                                (list
                                                 (list-ref args
                                                  (sub1 (digit-to-int n))))))
                                           (let ((%res
                                                  (if (eq? %type 'string)
                                                      ""
                                                      null)))
                                             (let %concatenate-loop
                                               ((%ee %ee))
                                               (if (null? %ee)
                                                   %res
                                                   (let ((%a (car %ee)))
                                                     (unless (not %a)
                                                       (set! %res
                                                        (if (eq? %type 'string)
                                                            (string-append %res
                                                             (if (string? %a)
                                                                 %a
                                                                 (list->string
                                                                  %a)))
                                                            (append %res
                                                                    (if (string?
                                                                         %a)
                                                                        (string->list
                                                                         %a)
                                                                        %a)))))
                                                     (%concatenate-loop
                                                      (cdr %ee)))))
                                             %res))
                                         (aux (+ k 2))))
                                       (true (cons #\# (aux (add1 k))))))))
                               (true (cons c (aux (add1 k))))))))))
                    (aux 0)))))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))))))))

(define do-verbatimescapechar
 (lambda ()
   (ignorespaces)
   (let ((c1 (get-actual-char)))
     (let ((c2 (get-actual-char)))
       (begin
        (cond
         ((not (esc-char-p c1))
          (terror 'do-verbatimescapechar "Arg must be \\<char>"))
         (else false))
        (begin (set! *esc-char-verb* c2) *esc-char-verb*))))))

(define do-verb-braced
 (lambda (ignore)
   false
   (let ((%fluid-var-*esc-chars* (list *esc-char-verb*))
         (%fluid-var-*tex-extra-letters* null)
         (nesting 0))
     (fluid-let
      ((*tex-extra-letters* %fluid-var-*tex-extra-letters*)
       (*esc-chars* %fluid-var-*esc-chars*))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (let ((c (get-actual-char)))
              (begin
               (cond
                ((eq? c ':eof-object)
                 (terror 'do-verb-braced "Eof inside verbatim"))
                (else false))
               (cond
                ((esc-char-p c) (toss-back-char c)
                 (let ((x
                        (let ((%fluid-var-*not-processing-p* true))
                          (fluid-let
                           ((*not-processing-p* %fluid-var-*not-processing-p*))
                           (get-ctl-seq)))))
                   (cond
                    ((member x '("\\ " "\\{" "\\}")) (emit (string-ref x 1)))
                    (true
                     (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
                       (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
                        (do-tex-ctl-seq-completely x)))))))
                ((char=? c #\{) (emit #\{)
                 (let ((%tmp (+ nesting 1)))
                   (begin (set! nesting %tmp) nesting)))
                ((char=? c #\}) (cond ((= nesting 0) (return)) (else false))
                 (emit #\})
                 (let ((%tmp (- nesting 1)))
                   (begin (set! nesting %tmp) nesting)))
                ((char=? c #\ )
                 (emit
                  (if *verb-visible-space-p*
                      *verbatim-visible-space*
                      #\ )))
                ((char=? c #\newline)
                 (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                       (*verb-visible-space-p* (emit *verbatim-visible-space*))
                       (true (emit-newline))))
                ((and (char=? c #\-) (not *verb-display-p*)) (emit "&#x2011;"))
                (true (emit-html-char c))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-verb-delimed
 (lambda (d)
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (get-actual-char)))
           (begin
            (cond
             ((eq? c ':eof-object)
              (terror 'do-verb-delimed "Eof inside verbatim"))
             (else false))
            (cond ((char=? c d) (return))
                  ((char=? c #\ )
                   (emit
                    (if *verb-visible-space-p*
                        *verbatim-visible-space*
                        #\ )))
                  ((char=? c #\newline)
                   (cond (*verb-display-p* (emit "&#xa0;") (emit-newline))
                         (*verb-visible-space-p*
                          (emit *verbatim-visible-space*))
                         (true (emit-newline))))
                  ((and (char=? c #\-) (not *verb-display-p*))
                   (emit "&#x2011;"))
                  (true (emit-html-char c))))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define do-verb
 (lambda ()
   (ignorespaces)
   (bgroup)
   (let ((%fluid-var-*verb-visible-space-p* (eat-star)))
     (fluid-let ((*verb-visible-space-p* %fluid-var-*verb-visible-space-p*))
      (let ((%fluid-var-*ligatures-p* false))
        (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
         (let ((d (get-actual-char)))
           (let ((%fluid-var-*verb-display-p* (munched-a-newline-p)))
             (fluid-let ((*verb-display-p* %fluid-var-*verb-display-p*))
              (begin
               (cond (*outputting-external-title-p* false)
                     (*verb-display-p* (do-end-para)
                      (emit "<pre class=verbatim>"))
                     (true (emit "<code class=verbatim>")))
               ((if (char=? d #\{)
                    do-verb-braced
                    do-verb-delimed)
                d)
               (cond (*outputting-external-title-p* false)
                     (*verb-display-p* (emit "</pre>") (do-noindent))
                     (true (emit "</code>")))))))))))
   (egroup)))

(define do-verbc
 (lambda ()
   (ignorespaces)
   (bgroup)
   (let ((%fluid-var-*ligatures-p* false))
     (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
      (emit "<code class=verbatim>") (emit-html-char (get-actual-char))
      (emit "</code>")))
   (egroup)))

(define do-verbatiminput
 (lambda ()
   (ignorespaces)
   (let ((f0 (get-filename-possibly-braced)))
     (let ((f (find-tex-file f0)))
       (cond
        ((and f (file-exists? f)) (do-end-para) (bgroup)
         (emit "<pre class=verbatim>")
         (let ((p
                (let* ((%f f)
                       (%ee (list ':direction ':input))
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
                   (else (open-input-file %f))))))
           (let ((%with-open-file-res
                  (begin
                   (let* ((%loop-returned false)
                          (%loop-result 0)
                          (return
                           (lambda %args
                             (set! %loop-returned true)
                             (set! %loop-result
                              (and (pair? %args) (car %args))))))
                     (let %loop
                       ()
                       (unless %loop-returned
                         (let ((c
                                (let* ((%read-char-port p)
                                       (%read-char-res
                                        (if %read-char-port
                                            (read-char %read-char-port)
                                            (read-char))))
                                  (when (eof-object? %read-char-res)
                                    (set! %read-char-res ':eof-object))
                                  %read-char-res)))
                           (begin
                            (cond ((eq? c ':eof-object) (return)) (else false))
                            (emit-html-char c))))
                       (if %loop-returned
                           %loop-result
                           (%loop)))))))
             (begin
              (cond
               (p
                ((if (input-port? p)
                     close-input-port
                     close-output-port)
                 p))
               (else false))
              %with-open-file-res)))
         (emit "</pre>") (egroup) (do-para))
        (true (non-fatal-error "File " f0 " not found")))))))

(define get-char-definitely
 (lambda (c0)
   (ignorespaces)
   (let ((c (get-actual-char)))
     (begin
      (cond
       ((eq? c ':eof-object) (terror 'get-char-defnitely "Runaway argument"))
       (else false))
      (cond ((not (char=? c c0)) (terror 'get-char-defnitely "Missing" c0))
            (else false))))))

(define get-char-optionally
 (lambda (cc)
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) false) ((member c cc) (get-actual-char) c)
           (true false)))))

(define get-unsigned-number-optionally
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) false) ((char-numeric? c) (get-integer 10))
           (true false)))))

(define opmac-verbinput-skip-lines
 (lambda (i n)
   (let ((%dotimes-n n) (_ 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((>= _ %dotimes-n) (return)) (else false)))
         (unless %loop-returned
           (let ((x
                  (let ((%read-line-res (read-line i)))
                    (when (eof-object? %read-line-res)
                      (set! %read-line-res ':eof-object))
                    %read-line-res)))
             (cond
              ((eq? x ':eof-object)
               (terror 'do-opmac-verbinput "\\verbinput file ended too soon"))
              (else false))))
         (unless %loop-returned (set! _ (+ _ 1)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define opmac-verbinput-print-lines
 (lambda (i n)
   (if (eq? n true)
       (let* ((%loop-returned false)
              (%loop-result 0)
              (return
               (lambda %args
                 (set! %loop-returned true)
                 (set! %loop-result (and (pair? %args) (car %args))))))
         (let %loop
           ()
           (unless %loop-returned
             (let ((x
                    (let ((%read-line-res (read-line i)))
                      (when (eof-object? %read-line-res)
                        (set! %read-line-res ':eof-object))
                      %read-line-res)))
               (begin (cond ((eq? x ':eof-object) (return)) (else false))
                (emit-html-string x) (emit-newline))))
           (if %loop-returned
               %loop-result
               (%loop))))
       (let ((%dotimes-n n) (_ 0))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (cond ((>= _ %dotimes-n) (return)) (else false)))
             (unless %loop-returned
               (let ((x
                      (let ((%read-line-res (read-line i)))
                        (when (eof-object? %read-line-res)
                          (set! %read-line-res ':eof-object))
                        %read-line-res)))
                 (begin
                  (cond
                   ((eq? x ':eof-object)
                    (terror 'do-opmac-verbinput
                     "\\verbinput file ended too soon"))
                   (else false))
                  (emit-html-string x) (emit-newline))))
             (unless %loop-returned (set! _ (+ _ 1)))
             (if %loop-returned
                 %loop-result
                 (%loop))))))))

(define do-opmac-verbinput
 (lambda ()
   (let ((s1
          (begin (get-char-definitely #\() (get-char-optionally '(#\+ #\-)))))
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
                                  (begin
                                   (table-put! f *opmac-verbinput-table* 0)
                                   (table-get f *opmac-verbinput-table*))
                                  0))))))
                 (cond
                  ((and f (file-exists? f)) (do-end-para) (bgroup)
                   (emit "<pre class=verbatim>")
                   (let ((i
                          (let* ((%f f)
                                 (%ee (list ':direction ':input))
                                 (%direction (memv ':direction %ee))
                                 (%if-exists (memv ':if-exists %ee))
                                 (%if-does-not-exist ':error)
                                 (%if-does-not-exist-from-user
                                  (memv ':if-does-not-exist %ee)))
                            (when %direction
                              (set! %direction (cadr %direction)))
                            (when %if-exists
                              (set! %if-exists (cadr %if-exists)))
                            (when %if-does-not-exist-from-user
                              (set! %if-does-not-exist
                               (cadr %if-does-not-exist-from-user)))
                            (cond
                             ((eqv? %direction ':output)
                              (when
                                  (and (eqv? %if-exists ':supersede)
                                       (file-exists? %f))
                                (delete-file %f))
                              (open-output-file %f))
                             ((and (not %if-does-not-exist)
                                   (not (file-exists? %f)))
                              false)
                             (else (open-input-file %f))))))
                     (let ((%with-open-file-res
                            (begin
                             (cond
                              ((and s1 n1 s2 n2 (char=? s1 #\-)
                                    (char=? s2 #\+))
                               (opmac-verbinput-skip-lines i (+ n n1))
                               (opmac-verbinput-print-lines i n2)
                               (let ((%tmp (+ n (+ n1 n2))))
                                 (begin (set! n %tmp) n)))
                              ((and (not s1) n1 s2 n2 (char=? s2 #\-))
                               (opmac-verbinput-skip-lines i (- n1 1))
                               (opmac-verbinput-print-lines i (+ (- n2 n1) 1))
                               (begin (set! n n2) n))
                              ((and (not s1) n1 s2 n2 (char=? s2 #\+))
                               (opmac-verbinput-skip-lines i (- n1 1))
                               (opmac-verbinput-print-lines i n2)
                               (begin (set! n (+ (- n1 1) n2)) n))
                              ((and s1 n1 (not s2) (not n2) (char=? s1 #\-))
                               (opmac-verbinput-print-lines i n1)
                               (begin (set! n n1) n))
                              ((and s1 n1 (not s2) (not n2) (char=? s1 #\+))
                               (opmac-verbinput-skip-lines i n)
                               (opmac-verbinput-print-lines i n1)
                               (let ((%tmp (+ n1 1)))
                                 (begin (set! n1 %tmp) n1)))
                              ((and (not s1) n1 s2 (not n2) (char=? s2 #\-))
                               (opmac-verbinput-skip-lines i (- n1 1))
                               (opmac-verbinput-print-lines i true)
                               (begin (set! n 0) n))
                              ((and s1 (not n1) (not s2) (not n2)
                                    (char=? s1 #\+))
                               (opmac-verbinput-skip-lines i n)
                               (opmac-verbinput-print-lines i true)
                               (begin (set! n 0) n))
                              ((and s1 (not n1) (not s2) (not n2)
                                    (char=? s1 #\-))
                               (opmac-verbinput-print-lines i true)
                               (begin (set! n 0) n))
                              (true
                               (terror 'do-opmac-verbinput
                                "Malformed \\verbinput" s1 n1 s2 n2 f))))))
                       (begin
                        (cond
                         (i
                          ((if (input-port? i)
                               close-input-port
                               close-output-port)
                           i))
                         (else false))
                        %with-open-file-res)))
                   (begin (table-put! f *opmac-verbinput-table* n)
                    (table-get f *opmac-verbinput-table*))
                   (emit "</pre>") (egroup) (do-para))
                  (true (non-fatal-error "File " f0 " not found"))))))))))))

(define do-verbwritefile
 (lambda ()
   (let ((f (get-filename-possibly-braced)))
     (let ((e (file-extension f)))
       (begin
        (cond
         ((not e) (begin (set! e ".tex") e)
          (begin
           (set! f
            (let ((%type 'string) (%ee (list f e)))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))
           f))
         (else false))
        (cond
         (*verb-port*
          (let ((%close-port-arg *verb-port*))
            ((if (input-port? %close-port-arg)
                 close-input-port
                 close-output-port)
             %close-port-arg)))
         (else false))
        (let ((%push-new-stack (cons f *verb-written-files*)))
          (begin (set! *verb-written-files* %push-new-stack)
           *verb-written-files*))
        (cond
         ((string-ci=? e ".mp")
          (let ((%push-new-stack (cons f *mp-files*)))
            (begin (set! *mp-files* %push-new-stack) *mp-files*)))
         (else false))
        (begin
         (set! *verb-port*
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
         *verb-port*))))))

(define verb-ensure-output-port
 (lambda ()
   (cond
    ((not *verb-port*)
     (let ((output-file
            (let ((%type 'string) (%ee (list *jobname* ".txt")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (begin
        (set! *verb-port*
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
        *verb-port*)))
    (else false))))

(define dump-groupoid
 (lambda (p)
   (ignorespaces)
   (let ((write-char write-char) (d (get-actual-char)))
     (begin
      (cond
       ((not p)
        (begin (set! write-char (lambda (x y) false false)) write-char))
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
               (unless %loop-returned (begin (set! c (get-actual-char)) c))
               (unless %loop-returned
                 (cond
                  ((eq? c ':eof-object)
                   (terror 'dump-groupoid "Eof inside verbatim"))
                  (else false)))
               (unless %loop-returned
                 (cond
                  ((char=? c *esc-char-verb*) (write-char c p)
                   (write-char (get-actual-char) p))
                  ((char=? c #\{) (write-char c p)
                   (let ((%tmp (+ nesting 1)))
                     (begin (set! nesting %tmp) nesting)))
                  ((char=? c #\})
                   (cond ((= nesting 0) (return))
                         (true (write-char c p)
                          (let ((%tmp (- nesting 1)))
                            (begin (set! nesting %tmp) nesting)))))
                  (true (write-char c p))))
               (if %loop-returned
                   %loop-result
                   (%loop))))))
        (else
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (get-actual-char)))
                 (begin
                  (cond
                   ((eq? c ':eof-object)
                    (terror 'dump-groupoid "Eof inside verbatim"))
                   (else false))
                  (cond ((char=? c d) (return)) (else false))
                  (write-char c p))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))))))

(define do-makehtmlimage
 (lambda ()
   (ignorespaces)
   (cond
    ((not (char=? (snoop-actual-char) #\{))
     (terror 'do-makehtmlimage "\\makehtmlimage's argument must be a group"))
    (else false))
   (call-with-html-image-port dump-groupoid)))

(define do-verbwrite
 (lambda () (verb-ensure-output-port) (dump-groupoid *verb-port*)))

(define do-string
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond ((eq? c ':eof-object) false)
           ((esc-char-p c) (get-actual-char) (toss-back-char *invisible-space*)
            (toss-back-string "\\TIIPbackslash"))
           ((char=? c *comment-char*) (eat-till-eol) (do-string))
           (true (toss-back-char (get-actual-char)))))))

(define do-verbatim
 (lambda ()
   (if (eqv? *tex-format* ':latex)
       (do-verbatim-latex "verbatim")
       (do-verbatim-eplain))))

(define do-verbatim-latex
 (lambda (env)
   (do-end-para)
   (bgroup)
   (emit "<pre class=verbatim>")
   (let ((%fluid-var-*verb-visible-space-p* (eat-star)))
     (fluid-let ((*verb-visible-space-p* %fluid-var-*verb-visible-space-p*))
      (cond
       (*verb-visible-space-p*
        (begin
         (set! env
          (let ((%type 'string) (%ee (list env "*")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         env))
       (else false))
      (munched-a-newline-p)
      (let ((%fluid-var-*ligatures-p* false))
        (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (begin
                  (cond
                   ((eq? c ':eof-object)
                    (terror 'do-verbatim-latex "Eof inside verbatim"))
                   (else false))
                  (cond
                   ((char=? c #\\)
                    (let ((cs (get-ctl-seq)))
                      (if (string=? cs "\\end")
                          (cond
                           ((begin
                             (set! *it* (get-grouped-environment-name-if-any))
                             *it*)
                            (let ((e *it*))
                              (begin
                               (cond ((string=? *it* env) (return))
                                     (else false))
                               (emit-html-string cs) (emit-html-char #\{)
                               (emit-html-string e) (emit-html-char #\}))))
                           (true (emit-html-string cs)))
                          (begin (emit-html-string cs)))))
                   ((char=? c #\ ) (get-actual-char)
                    (emit
                     (if *verb-visible-space-p*
                         *verbatim-visible-space*
                         #\ )))
                   (true (emit-html-char (get-actual-char)))))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))))
   (emit "</pre>")
   (egroup)
   (do-para)))

(define do-verbatim-eplain
 (lambda ()
   (let ((%fluid-var-*inside-eplain-verbatim-p* true)
         (%fluid-var-*esc-chars* (list *esc-char-verb*)))
     (fluid-let
      ((*esc-chars* %fluid-var-*esc-chars*)
       (*inside-eplain-verbatim-p* %fluid-var-*inside-eplain-verbatim-p*))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (cond ((not *inside-eplain-verbatim-p*) (return)) (else false)))
          (unless %loop-returned
            (let ((c (get-actual-char)))
              (begin
               (cond
                ((eq? c ':eof-object)
                 (terror 'do-verbatim-eplain "Eof inside verbatim"))
                (else false))
               (cond
                ((esc-char-p c) (toss-back-char c)
                 (let ((x
                        (let ((%fluid-var-*not-processing-p* true))
                          (fluid-let
                           ((*not-processing-p* %fluid-var-*not-processing-p*))
                           (get-ctl-seq)))))
                   (cond ((string=? x "\\ ") (emit " "))
                         (true (do-tex-ctl-seq-completely x)))))
                ((char=? c #\ ) (emit "&#xa0;"))
                ((char=? c #\newline) (emit "<br>") (emit-newline))
                (true (emit-html-char c))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-endverbatim-eplain
 (lambda ()
   (begin (set! *inside-eplain-verbatim-p* false) *inside-eplain-verbatim-p*)))

(define do-begintt
 (lambda ()
   (do-end-para)
   (let ((%fluid-var-*esc-chars* *esc-chars*))
     (fluid-let ((*esc-chars* %fluid-var-*esc-chars*)) (bgroup)
      (do-tex-ctl-seq-completely "\\tthook")
      (begin (set! *esc-chars* (remove *esc-char-std* *esc-chars*))
       *esc-chars*)
      (emit "<pre class=verbatim>") (munched-a-newline-p)
      (let ((%fluid-var-*ligatures-p* false))
        (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (begin
                  (cond
                   ((eq? c ':eof-object)
                    (terror 'do-begintt "Eof inside \\begintt"))
                   (else false))
                  (cond
                   ((char=? c #\\)
                    (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
                      (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
                       (let ((cs
                              (let ((%fluid-var-*not-processing-p* true))
                                (fluid-let
                                 ((*not-processing-p*
                                   %fluid-var-*not-processing-p*))
                                 (get-ctl-seq)))))
                         (begin
                          (cond ((string=? cs "\\endtt") (return))
                                (else false))
                          (emit-html-string cs))))))
                   ((esc-char-p c)
                    (let ((cs
                           (let ((%fluid-var-*not-processing-p* true))
                             (fluid-let
                              ((*not-processing-p*
                                %fluid-var-*not-processing-p*))
                              (get-ctl-seq)))))
                      (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
                        (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
                         (do-tex-ctl-seq-completely cs)))))
                   (true (emit-html-char (get-actual-char)))))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))
      (emit "</pre>") (egroup)))
   (do-noindent)))

(define do-alltt
 (lambda ()
   (do-end-para)
   (bgroup)
   (emit "<pre class=verbatim>")
   (munched-a-newline-p)
   (let ((%fluid-var-*in-alltt-p* true))
     (fluid-let ((*in-alltt-p* %fluid-var-*in-alltt-p*))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin
               (cond
                ((eq? c ':eof-object) (terror 'do-alltt "Eof inside alltt"))
                (else false))
               (case c
                 ((#\\) (do-tex-ctl-seq (get-ctl-seq)))
                 ((#\{) (get-actual-char) (bgroup))
                 ((#\}) (get-actual-char) (egroup))
                 (else (emit-html-char (get-actual-char))))
               (cond ((not *in-alltt-p*) (return)) (else false)))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-end-alltt
 (lambda ()
   (emit "</pre>")
   (egroup)
   (do-para)
   (begin (set! *in-alltt-p* false) *in-alltt-p*)))

(define do-scm-set-specialsymbol
 (lambda ()
   (let ((sym (get-peeled-group)))
     (let ((xln (get-group)))
       (begin (table-put! sym *scm-special-symbols* xln)
        (table-get sym *scm-special-symbols*))))))

(define do-scm-unset-specialsymbol
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (cond ((eq? (snoop-actual-char) ':eof-object) (return))
                  (else false)))
          (unless %loop-returned
            (begin (table-put! (scm-get-token) *scm-special-symbols* false)
             (table-get (scm-get-token) *scm-special-symbols*)))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-scm-set-builtins
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin (cond ((eq? c ':eof-object) (return)) (else false))
               (let ((s (scm-get-token)))
                 (begin (table-put! s *scm-keywords* false)
                  (table-put! s *scm-variables* false)
                  (table-put! s *scm-builtins* true)
                  (table-get s *scm-builtins*))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-scm-set-keywords
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin (cond ((eq? c ':eof-object) (return)) (else false))
               (let ((s (scm-get-token)))
                 (begin (table-put! s *scm-builtins* false)
                  (table-put! s *scm-variables* false)
                  (table-put! s *scm-keywords* true)
                  (table-get s *scm-keywords*))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-scm-set-variables
 (lambda ()
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
          (unless %loop-returned (ignore-all-whitespace))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin (cond ((eq? c ':eof-object) (return)) (else false))
               (let ((s (scm-get-token)))
                 (begin (table-put! s *scm-builtins* false)
                  (table-put! s *scm-keywords* false)
                  (table-put! s *scm-variables* true)
                  (table-get s *scm-variables*))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define scm-emit-html-char
 (lambda (c)
   (cond
    ((not (eq? c ':eof-object))
     (cond (*scm-dribbling-p* (write-char c *verb-port*)) (else false))
     (if (and (char=? c #\-) (not *verb-display-p*))
         (emit "&#x2011;")
         (emit-html-char c)))
    (else false))))

(define scm-output-next-chunk
 (lambda ()
   (let ((c (snoop-actual-char)))
     (cond
      ((and *slatex-math-escape* (char=? c *slatex-math-escape*))
       (scm-escape-into-math))
      ((char=? c #\;) (scm-output-comment) (do-end-para))
      ((char=? c #\") (scm-output-string)) ((char=? c #\#) (scm-output-hash))
      ((char=? c #\,) (get-actual-char) (emit "<span class=keyword>")
       (scm-emit-html-char c)
       (let ((c (snoop-actual-char)))
         (cond ((char=? c #\@) (get-actual-char) (scm-emit-html-char c))
               (else false)))
       (emit "</span>"))
      ((or (char=? c #\') (char=? c #\`)) (get-actual-char)
       (emit "<span class=keyword>") (scm-emit-html-char c) (emit "</span>"))
      ((or (char-whitespace? c) (member c *scm-token-delims*))
       (get-actual-char) (scm-emit-html-char c))
      (true (scm-output-token (scm-get-token)))))))

(define scm-set-mathescape
 (lambda (yes-p)
   (let ((c
          (let ((%fluid-var-*esc-chars* null))
            (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
             (string-ref (ungroup (get-group)) 0)))))
     (cond
      (yes-p (begin (set! *slatex-math-escape* c) *slatex-math-escape*)
       (let ((%push-new-stack (cons *slatex-math-escape* *scm-token-delims*)))
         (begin (set! *scm-token-delims* %push-new-stack) *scm-token-delims*)))
      (true (begin (set! *slatex-math-escape* false) *slatex-math-escape*)
       (begin (set! *scm-token-delims* (remove c *scm-token-delims*))
        *scm-token-delims*))))))

(define scm-escape-into-math
 (lambda ()
   (get-actual-char)
   (let ((math-text (get-till-char *slatex-math-escape*)))
     (begin (get-actual-char)
      (cond
       ((not (string=? math-text "")) (emit "<span class=variable>")
        (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
          (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
           (tex2page-string
            (let ((%type 'string) (%ee (list "$" math-text "$")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))
        (emit "</span>"))
       (else false))))))

(define scm-output-slatex-comment
 (lambda ()
   (let ((s (get-line)))
     (begin (emit "<span class=comment>")
      (cond (*scm-dribbling-p* (display s *verb-port*) (newline *verb-port*))
            (else false))
      (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
        (fluid-let ((*esc-chars* %fluid-var-*esc-chars*)) (tex2page-string s)))
      (do-end-para) (emit "</span>") (toss-back-char #\newline)))))

(define scm-output-verbatim-comment
 (lambda ()
   (emit "<span class=comment>")
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (get-actual-char)))
           (begin
            (cond
             ((or (eq? c ':eof-object) (char=? c #\newline)) (emit "</span>")
              (scm-emit-html-char c) (return))
             (else false))
            (cond
             ((and (char-whitespace? c)
                   (let ((c2 (snoop-actual-char)))
                     (or (eq? c2 ':eof-object) (char=? c2 #\newline))))
              (emit "</span>") (scm-emit-html-char (get-actual-char)) (return))
             (true (scm-emit-html-char c))))))
       (if %loop-returned
           %loop-result
           (%loop))))))

(define scm-output-comment
 (lambda ()
   ((if (tex2page-flag-boolean "\\TZPslatexcomments")
        scm-output-slatex-comment
        scm-output-verbatim-comment))))

(define scm-output-extended-comment
 (lambda ()
   (get-actual-char)
   (emit "<span class=comment>")
   (scm-emit-html-char #\#)
   (scm-emit-html-char #\|)
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned
         (let ((c (get-actual-char)))
           (cond ((eq? c ':eof-object) (return))
                 ((char=? c #\|)
                  (let ((c2 (snoop-actual-char)))
                    (cond
                     ((eq? c2 ':eof-object) (scm-emit-html-char c) (return))
                     ((char=? c2 #\#) (get-actual-char) (return))
                     (true (scm-emit-html-char c)))))
                 (true (scm-emit-html-char c)))))
       (if %loop-returned
           %loop-result
           (%loop))))
   (scm-emit-html-char #\|)
   (scm-emit-html-char #\#)
   (emit "</span>")))

(define scm-output-string
 (lambda ()
   (get-actual-char)
   (emit "<span class=selfeval>")
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
         (unless %loop-returned
           (let ((c (get-actual-char)))
             (case c
               ((#\")
                (cond ((not esc-p) (return)) (else false))
                (scm-emit-html-char c)
                (begin (set! esc-p false) esc-p))
               ((#\\)
                (scm-emit-html-char c)
                (begin (set! esc-p (not esc-p)) esc-p))
               (else
                (scm-emit-html-char c)
                (begin (set! esc-p false) esc-p)))))
         (if %loop-returned
             %loop-result
             (%loop)))))
   (scm-emit-html-char #\")
   (emit "</span>")))

(define scm-output-hash
 (lambda ()
   (get-actual-char)
   (let ((c (snoop-actual-char)))
     (cond
      ((eq? c ':eof-object) (emit "<span class=selfeval>")
       (scm-emit-html-char #\#) (emit "</span>"))
      ((char=? c #\|) (scm-output-extended-comment))
      (true (toss-back-char #\#) (scm-output-token (scm-get-token)))))))

(define scm-output-token
 (lambda (s)
   (case (scm-get-type s)
     ((:special-symbol)
      (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
        (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
         (tex2page-string (table-get s *scm-special-symbols*)))))
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
      (emit "</span>")))))

(define scm-display-token
 (lambda (s)
   (let ((n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg)))
         (k 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned (cond ((not (< k n)) (return)) (else false)))
         (unless %loop-returned (scm-emit-html-char (string-ref s k)))
         (unless %loop-returned
           (let ((%tmp (+ k 1)))
             (begin (set! k %tmp) k)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define do-scm-braced
 (lambda (result-p)
   (get-actual-char)
   (let ((display-p (munched-a-newline-p)))
     (begin
      (cond
       ((not display-p) (emit "<code class=scheme")
        (cond (result-p (emit "response")) (else false)) (emit ">"))
       (true (do-end-para) (emit "<pre class=scheme>")))
      (bgroup)
      (let ((%fluid-var-*esc-chars* (list *esc-char-verb*))
            (%fluid-var-*verb-display-p* display-p)
            (nesting 0))
        (fluid-let
         ((*verb-display-p* %fluid-var-*verb-display-p*)
          (*esc-chars* %fluid-var-*esc-chars*))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (begin
                  (cond
                   ((eq? c ':eof-object)
                    (terror 'do-scm-braced "Eof inside verbatim"))
                   (else false))
                  (cond
                   ((esc-char-p c)
                    (let ((x
                           (let ((%fluid-var-*not-processing-p* true))
                             (fluid-let
                              ((*not-processing-p*
                                %fluid-var-*not-processing-p*))
                              (get-ctl-seq)))))
                      (cond
                       ((member x '("\\ " "\\{" "\\}"))
                        (scm-emit-html-char (string-ref x 1)))
                       (true
                        (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
                          (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
                           (do-tex-ctl-seq-completely x)))))))
                   ((char=? c #\{) (get-actual-char) (scm-emit-html-char c)
                    (let ((%tmp (+ nesting 1)))
                      (begin (set! nesting %tmp) nesting)))
                   ((char=? c #\}) (get-actual-char)
                    (cond ((= nesting 0) (return)) (else false))
                    (scm-emit-html-char c)
                    (let ((%tmp (- nesting 1)))
                      (begin (set! nesting %tmp) nesting)))
                   (true (scm-output-next-chunk))))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))
      (egroup)
      (if (not display-p)
          (emit "</code>")
          (begin (emit "</pre>") (do-noindent)))))))

(define do-scm-delimed
 (lambda (result-p)
   (let ((d (get-actual-char)))
     (let ((display-p (munched-a-newline-p)))
       (begin
        (cond
         ((not display-p) (emit "<code class=scheme")
          (cond (result-p (emit "response")) (else false)) (emit ">"))
         (true (do-end-para) (emit "<pre class=scheme>")))
        (let ((%fluid-var-*verb-display-p* display-p)
              (%fluid-var-*scm-token-delims* (cons d *scm-token-delims*)))
          (fluid-let
           ((*scm-token-delims* %fluid-var-*scm-token-delims*)
            (*verb-display-p* %fluid-var-*verb-display-p*))
           (let* ((%loop-returned false)
                  (%loop-result 0)
                  (return
                   (lambda %args
                     (set! %loop-returned true)
                     (set! %loop-result (and (pair? %args) (car %args))))))
             (let %loop
               ()
               (unless %loop-returned
                 (let ((c (snoop-actual-char)))
                   (begin
                    (cond
                     ((eq? c ':eof-object)
                      (terror 'do-scm-delimed "Eof inside verbatim"))
                     (else false))
                    (cond ((char=? c d) (get-actual-char) (return))
                          (else false))
                    (scm-output-next-chunk))))
               (if %loop-returned
                   %loop-result
                   (%loop))))))
        (cond ((not display-p) (emit "</code>"))
              (true (emit "</pre>") (do-noindent))))))))

(define do-scm
 (lambda %lambda-rest-arg
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (result-p false))
     (when (< 0 %lambda-rest-arg-len)
       (set! result-p (list-ref %lambda-rest-arg 0)))
     (cond (*outputting-external-title-p* (do-verb))
           (true (ignorespaces) (bgroup)
            (let ((%fluid-var-*ligatures-p* false))
              (fluid-let ((*ligatures-p* %fluid-var-*ligatures-p*))
               ((if (char=? (snoop-actual-char) #\{)
                    do-scm-braced
                    do-scm-delimed)
                result-p)))
            (egroup))))))

(define do-scminput
 (lambda ()
   (ignorespaces)
   (do-end-para)
   (bgroup)
   (emit "<pre class=scheme>")
   (let ((f
          (add-dot-tex-if-no-extension-provided
           (get-filename-possibly-braced))))
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
            (unless %loop-returned
              (let ((c (snoop-actual-char)))
                (begin (cond ((eq? c ':eof-object) (return)) (else false))
                 (scm-output-next-chunk))))
            (if %loop-returned
                %loop-result
                (%loop)))))))
   (emit "</pre>")
   (egroup)
   (do-noindent)))

(define do-scmdribble
 (lambda ()
   (verb-ensure-output-port)
   (let ((%fluid-var-*scm-dribbling-p* true))
     (fluid-let ((*scm-dribbling-p* %fluid-var-*scm-dribbling-p*))
      (do-scm false)))
   (newline *verb-port*)))

(define do-scm-slatex-lines
 (lambda (env display-p result-p)
   (let ((%fluid-var-*esc-chars* *esc-chars*)
         (endenv
          (let ((%type 'string) (%ee (list "\\end" env)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         (in-table-p
          (and (not (null? *tabular-stack*))
               (member (car *tabular-stack*) '(:block :figure :table)))))
     (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
      (cond (display-p (do-end-para)) (in-table-p (emit "</td><td>")))
      (munched-a-newline-p) (bgroup)
      (cond
       ((string=? env "tt") (do-tex-ctl-seq-completely "\\tthook")
        (begin (set! *esc-chars* (remove *esc-char-std* *esc-chars*))
         *esc-chars*))
       (else false))
      (emit "<div align=left><pre class=scheme")
      (cond (result-p (emit "response")) (else false)) (emit ">")
      (let ((%fluid-var-*ligatures-p* false)
            (%fluid-var-*verb-display-p* true)
            (%fluid-var-*not-processing-p* true))
        (fluid-let
         ((*not-processing-p* %fluid-var-*not-processing-p*)
          (*verb-display-p* %fluid-var-*verb-display-p*)
          (*ligatures-p* %fluid-var-*ligatures-p*))
         (let* ((%loop-returned false)
                (%loop-result 0)
                (return
                 (lambda %args
                   (set! %loop-returned true)
                   (set! %loop-result (and (pair? %args) (car %args))))))
           (let %loop
             ()
             (unless %loop-returned
               (let ((c (snoop-actual-char)))
                 (begin
                  (cond
                   ((eq? c ':eof-object)
                    (terror 'do-scm-slatex-lines "Eof inside " env))
                   (else false))
                  (cond
                   ((char=? c #\newline) (get-actual-char)
                    (scm-emit-html-char c)
                    (cond
                     ((not (tex2page-flag-boolean "\\TZPslatexcomments"))
                      false)
                     ((char=? (snoop-actual-char) #\;) (get-actual-char)
                      (if (char=? (snoop-actual-char) #\;)
                          (toss-back-char #\;)
                          (scm-output-slatex-comment)))))
                   ((char=? c #\\)
                    (let ((x
                           (let ((%fluid-var-*esc-chars* (list *esc-char-std*))
                                 (%fluid-var-*not-processing-p* true))
                             (fluid-let
                              ((*not-processing-p*
                                %fluid-var-*not-processing-p*)
                               (*esc-chars* %fluid-var-*esc-chars*))
                              (get-ctl-seq)))))
                      (begin (cond ((string=? x endenv) (return)) (else false))
                       (cond
                        ((string=? x "\\end")
                         (let ((g (get-grouped-environment-name-if-any)))
                           (begin
                            (cond ((and g (string=? g env)) (egroup) (return))
                                  (else false))
                            (scm-output-token x)
                            (cond
                             (g (scm-output-token "{") (scm-output-token g)
                              (scm-output-token "}"))
                             (else false)))))
                        (true (scm-output-token x))))))
                   ((esc-char-p c)
                    (let ((cs
                           (let ((%fluid-var-*not-processing-p* true))
                             (fluid-let
                              ((*not-processing-p*
                                %fluid-var-*not-processing-p*))
                              (get-ctl-seq)))))
                      (let ((%fluid-var-*esc-chars* (list *esc-char-std*)))
                        (fluid-let ((*esc-chars* %fluid-var-*esc-chars*))
                         (do-tex-ctl-seq-completely cs)))))
                   (true (scm-output-next-chunk))))))
             (if %loop-returned
                 %loop-result
                 (%loop))))))
      (emit "</pre></div>") (egroup)
      (cond (display-p (do-noindent)) (in-table-p (emit "</td><td>")))))))

(define string-is-all-dots-p
 (lambda (s)
   (let ((%dotimes-n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg)))
         (i 0))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((>= i %dotimes-n) (return true)) (else false)))
         (unless %loop-returned
           (cond ((not (char=? (string-ref s i) #\.)) (return false))
                 (else false)))
         (unless %loop-returned (set! i (+ i 1)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define string-is-flanked-by-stars-p
 (lambda (s)
   (let ((n
          (let ((%length-arg s))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (and (>= n 3) (char=? (string-ref s 0) (string-ref s (sub1 n)) #\*)))))

(define scm-get-type
 (lambda (s)
   (cond ((table-get s *scm-special-symbols*) ':special-symbol)
         ((table-get s *scm-keywords*) ':keyword)
         ((table-get s *scm-builtins*) ':builtin)
         ((table-get s *scm-variables*) ':variable)
         ((string-is-flanked-by-stars-p s) ':global)
         ((begin
           (set! *it*
            (let ((%position-v #\:) (%position-s s) (%ee (list ':test char=?)))
              (let ((%position-from-end (memv ':from-end %ee)))
                (when %position-from-end
                  (set! %position-from-end (cadr %position-from-end)))
                (if (string? %position-s)
                    ((if %position-from-end
                         string-reverse-index
                         string-index)
                     %position-s %position-v)
                    (list-position %position-v %position-s)))))
           *it*)
          (if (= *it* 0)
              ':selfeval
              ':variable))
         ((string-is-all-dots-p s) ':background)
         ((char=? (string-ref s 0) #\#) ':selfeval)
         ((string->number s) ':selfeval) (true ':variable))))

(define eat-star
 (lambda ()
   (let ((c (snoop-actual-char)))
     (if (and (not (eq? c ':eof-object)) (char=? c #\*))
         (get-actual-char)
         false))))

(define do-cr
 (lambda (z)
   (ignorespaces)
   (case (car *tabular-stack*)
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
      (begin (set! *equation-position* 0) *equation-position*)
      (emit "<tr><td align=right>"))
     ((:eqnarray)
      (emit "</td>")
      (cond
       (*equation-numbered-p* (emit "<td>(") (emit *equation-number*)
        (bump-dotted-counter "equation") (emit ")</td>"))
       (true (begin (set! *equation-numbered-p* true) *equation-numbered-p*)))
      (emit "</tr>")
      (emit-newline)
      (begin (set! *equation-position* 0) *equation-position*)
      (emit "<tr><td align=right>"))
     ((:ruled-table) (emit "</td></tr>") (emit-newline) (emit "<tr><td>"))
     ((:minipage :tabbing)
      (get-bracketed-text-if-any)
      (emit "<br>")
      (emit-newline))
     ((:eqalign :eqalignno :displaylines :pmatrix :mathbox)
      (cond
       ((not (char=? (snoop-actual-char) #\}))
        (let ((%tmp (+ *math-height* 1)))
          (begin (set! *math-height* %tmp) *math-height*))
        (emit "</td></tr>") (emit-newline) (emit "<tr><td align=center>")
        (begin (set! *equation-position* 0) *equation-position*)
        (emit-newline))
       (else false)))
     ((:header) (emit #\ ))
     (else
      (cond
       ((and (eqv? *tex-format* ':latex) (string=? z "\\\\"))
        (get-bracketed-text-if-any)
        (let ((c (snoop-actual-char)))
          (cond
           ((and (not (eq? c ':eof-object)) (char=? c #\*)) (get-actual-char))
           (else false)))
        (emit "<br>") (emit-newline))
       (else false))))))

(define do-ruledtable
 (lambda ()
   (let ((%push-new-stack (cons ':ruled-table *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (emit "<table border=2><tr><td>")
   (emit-newline)))

(define do-endruledtable
 (lambda ()
   (emit-newline)
   (emit "</td></tr></table>")
   (emit-newline)
   (pop-tabular-stack ':ruled-table)))

(define do-tabular
 (lambda %lambda-rest-arg
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (mathp false))
     (when (< 0 %lambda-rest-arg-len)
       (set! mathp (list-ref %lambda-rest-arg 0)))
     (do-end-para)
     (get-bracketed-text-if-any)
     (bgroup)
     (cond
      ((not mathp)
       (add-postlude-to-top-frame
        (let ((old-math-mode-p *math-mode-p*)
              (old-in-display-math-p *in-display-math-p*))
          (begin (begin (set! *math-mode-p* false) *math-mode-p*)
           (begin (set! *in-display-math-p* false) *in-display-math-p*)
           (lambda ()
             (begin (set! *math-mode-p* old-math-mode-p) *math-mode-p*)
             (begin (set! *in-display-math-p* old-in-display-math-p)
              *in-display-math-p*))))))
      (else false))
     (let ((border-width
            (if (let ((%position-v #\|)
                      (%position-s (get-group))
                      (%ee (list ':test char=?)))
                  (let ((%position-from-end (memv ':from-end %ee)))
                    (when %position-from-end
                      (set! %position-from-end (cadr %position-from-end)))
                    (if (string? %position-s)
                        ((if %position-from-end
                             string-reverse-index
                             string-index)
                         %position-s %position-v)
                        (list-position %position-v %position-s))))
                1
                0)))
       (begin
        (let ((%push-new-stack (cons ':tabular *tabular-stack*)))
          (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
        (emit "<table border=") (emit border-width)
        (emit "><tr><td valign=top ") (do-tabular-multicolumn))))))

(define do-end-tabular
 (lambda ()
   (egroup)
   (do-end-para)
   (emit "</td></tr></table>")
   (pop-tabular-stack ':tabular)
   (egroup)))

(define do-tabular-colsep
 (lambda () (egroup) (emit "</td><td valign=top ") (do-tabular-multicolumn)))

(define do-tabular-multicolumn
 (lambda ()
   (let* ((%loop-returned false)
          (%loop-result 0)
          (return
           (lambda %args
             (set! %loop-returned true)
             (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop
       ()
       (unless %loop-returned (ignorespaces))
       (unless %loop-returned
         (let ((c (snoop-actual-char)))
           (begin
            (cond ((not (and (char? c) (char=? c #\\))) (return)) (else false))
            (let ((x (get-ctl-seq)))
              (cond ((string=? x "\\hline") true)
                    ((string=? x "\\multicolumn")
                     (let ((n (ungroup (get-token))))
                       (begin (get-token) (emit " colspan=") (emit n)
                        (return))))
                    (true (toss-back-char *invisible-space*)
                     (toss-back-string x) (return)))))))
       (if %loop-returned
           %loop-result
           (%loop))))
   (emit ">")
   (bgroup)))

(define do-ruledtable-colsep
 (lambda ()
   (emit-newline)
   (emit "</td><td")
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (cond
      ((char=? c #\\)
       (let ((x (get-ctl-seq)))
         (if (string=? x "\\multispan")
             (let ((n (ungroup (get-token))))
               (begin (emit " colspan=") (emit n)))
             (toss-back-string x))))
      (else false)))
   (emit ">")
   (emit-newline)))

(define do-romannumeral
 (lambda %lambda-rest-arg
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (upcase-p false))
     (when (< 0 %lambda-rest-arg-len)
       (set! upcase-p (list-ref %lambda-rest-arg 0)))
     (let ((n (get-number-or-false)))
       (cond (n (emit (number-to-roman n upcase-p))) (else false))))))

(define do-tex-case-code
 (lambda (kase)
   (cond
    ((not (inside-false-world-p))
     (let ((c1 (get-tex-char-spec)))
       (let ((c2 (begin (get-equal-sign) (get-tex-char-spec))))
         (let ((fr (top-texframe)))
           (begin
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
              fr)))))))
    (else false))))

(define tex-char-downcase
 (lambda (c)
   (or (ormap (lambda (fr) (table-get c (texframe*-lccodes fr))) *tex-env*)
       (char-downcase c))))

(define tex-char-upcase
 (lambda (c)
   (or (ormap (lambda (fr) (table-get c (texframe*-uccodes fr))) *tex-env*)
       (char-upcase c))))

(define do-flipcase
 (lambda (kase)
   (emit
    (map 'string
         (case kase ((:uccode) tex-char-upcase) ((:lccode) tex-char-downcase))
         (get-token)))))

(define do-addtocounter
 (lambda ()
   (let ((counter-name (get-peeled-group)))
     (let ((new-value (string->number (get-token-or-peeled-group))))
       (set-latex-counter-aux counter-name true new-value)))))

(define do-setcounter
 (lambda ()
   (let ((counter-name (get-peeled-group)))
     (let ((new-value (string->number (get-token-or-peeled-group))))
       (set-latex-counter-aux counter-name false new-value)))))

(define do-stepcounter
 (lambda ()
   (let ((counter-name (get-peeled-group)))
     (set-latex-counter-aux counter-name true 1))))

(define set-latex-counter-aux
 (lambda (counter-name addp new-value)
   (cond
    ((begin (set! *it* (table-get counter-name *dotted-counters*)) *it*)
     (let ((counter *it*))
       (if addp
           (let ((%tmp (+ (counter*-value counter) new-value)))
             (begin (set!counter*-value counter %tmp)
              (counter*-value counter)))
           (begin (set!counter*-value counter new-value)
            (counter*-value counter)))))
    (true
     (let ((count-seq
            (let ((%type 'string) (%ee (list "\\" counter-name)))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (cond
        ((begin (set! *it* (section-ctl-seq-p count-seq)) *it*)
         (let ((n *it*))
           (begin
            (table-put! n *section-counters*
             (if addp
                 (+ new-value (table-get n *section-counters* 0))
                 new-value))
            (table-get n *section-counters*))))
        ((find-count count-seq)
         (tex-gdef-count count-seq
          (if addp
              (+ new-value (get-gcount count-seq))
              new-value)))
        (true false)))))))

(define do-tex-prim
 (lambda (z)
   (let ((y false))
     (cond
      ((begin (set! *it* (find-def z)) *it*) (begin (set! y *it*) y)
       (cond
        ((begin (set! *it* (tdef*-defer y)) *it*) (begin (set! y *it*) y)
         (toss-back-string y))
        ((begin (set! *it* (tdef*-thunk y)) *it*) (begin (set! y *it*) y) (y))
        (true
         (expand-tex-macro (tdef*-optarg y) (tdef*-argpat y)
          (tdef*-expansion y)))))
      (*math-mode-p* (do-math-ctl-seq z))
      (true (trace-if (> (find-count "\\tracingcommands") 0) "Ignoring " z))))))

(define do-char
 (lambda ()
   (let ((n (get-number-or-false)))
     (begin (cond ((not n) (terror 'do-char "not a char")) (else false))
      (cond ((< n 128) (emit-html-char (integer->char n)))
            (true (emit "&#x") (emit (write-to-string n ':base 16))
             (emit ";")))))))

(define do-chardef
 (lambda ()
   (let ((lhs (get-raw-token/is)))
     (begin (get-equal-sign)
      (let ((n (get-number-or-false)))
        (if n
            (tex-let lhs (string (integer->char n)) false)
            (terror 'do-chardef
             "non-number found while scanning definition of " lhs)))))))

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
     (else
      (let ((%type 'string)
            (%ee
             (list "&#x"
                   (write-to-string
                    (+ 120120 (- (char->integer c) (char->integer #\A))) ':base
                    16)
                   ";")))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))))))

(define tex-math-cal
 (lambda (c)
   (let ((%type 'string)
         (%ee
          (list "&#x"
                (write-to-string
                 (+ 120016 (- (char->integer c) (char->integer #\A))) ':base
                 16)
                ";")))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define tex-math-frak
 (lambda (c)
   (let ((%type 'string)
         (%ee
          (list "&#x"
                (write-to-string
                 (if (char-upper-case? c)
                     (+ 120172 (- (char->integer c) (char->integer #\A)))
                     (+ 120198 (- (char->integer c) (char->integer #\a))))
                 ':base 16)
                ";")))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

(define emit-math-alpha-char
 (lambda (c)
   (case *math-font*
     ((:rm) (emit c))
     ((:bf) (emit "<b>") (emit c) (emit "</b>"))
     ((:bb)
      (emit
       (if (char-upper-case? c)
           (tex-math-bb c)
           c)))
     ((:cal)
      (emit
       (if (char-upper-case? c)
           (tex-math-cal c)
           c)))
     ((:frak)
      (emit
       (if (char-alphabetic? c)
           (tex-math-frak c)
           c)))
     (else (emit "<em>") (emit c) (emit "</em>")))))

(define do-tex-char
 (lambda (c)
   (cond ((and *comment-char* (char=? c *comment-char*)) (do-comment))
         ((inside-false-world-p) true) ((char=? c #\{) (bgroup))
         ((char=? c #\}) (egroup)) ((char=? c #\$) (do-math))
         ((char=? c #\-) (do-hyphen)) ((char=? c #\`) (do-lsquo))
         ((char=? c #\') (do-rsquo)) ((char=? c #\~) (emit-nbsp 1))
         ((char=? c #\!) (do-excl)) ((char=? c #\?) (do-quest))
         ((or (char=? c #\<) (char=? c #\>) (char=? c #\")) (emit-html-char c))
         ((char=? c #\&)
          (cond
           (*tabular-stack*
            (case (car *tabular-stack*)
              ((:pmatrix :eqalign :displaylines :mathbox)
               (emit "&#xa0;</td><td align=center>&#xa0;"))
              ((:eqalignno)
               (begin (set! *equation-position* (+ *equation-position* 1))
                *equation-position*)
               (emit "</td><td")
               (cond
                ((= *equation-position* 2) (emit " width=30% align=right"))
                (else false))
               (emit ">"))
              ((:eqnarray :eqnarray*)
               (begin (set! *equation-position* (+ *equation-position* 1))
                *equation-position*)
               (emit "</td><td")
               (cond
                ((= *equation-position* 1) (emit " align=center width=2%"))
                (else false))
               (emit ">"))
              ((:tabular) (do-tabular-colsep))
              ((:ruled-table) (do-ruledtable-colsep))))
           (true (emit-html-char c))))
         ((char=? c #\|)
          (if (eq? (car *tabular-stack*) ':ruled-table)
              (do-ruledtable-colsep)
              (emit c)))
         ((char=? c #\newline) (do-newline)) ((char=? c #\ ) (do-space))
         ((char=? c #\tab) (do-tab))
         (true
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
               (if (char-alphabetic? c)
                   (emit-math-alpha-char c)
                   (emit c)))))
           ((and *in-small-caps-p* (char-lower-case? c)) (emit "<small>")
            (emit (char-upcase c)) (emit "</small>"))
           (true (emit c)))))))

(define do-tex-ctl-seq-completely
 (lambda (x)
   (cond ((begin (set! *it* (resolve-defs x)) *it*) (tex2page-string *it*))
         ((begin (set! *it* (do-tex-prim (find-corresp-prim x))) *it*)
          (cond ((eq? *it* ':encountered-undefined-command) (emit x))
                (else false))))))

(define inside-false-world-p
 (lambda () (or (member false *tex-if-stack*) (member '? *tex-if-stack*))))

(define do-tex-ctl-seq
 (lambda (z)
   (trace-if (> (find-count "\\tracingcommands") 0) z)
   (cond
    ((begin (set! *it* (resolve-defs z)) *it*)
     (let ((s *it*))
       (begin (trace-if (> (find-count "\\tracingmacros") 0) "    --> " s)
        (toss-back-char *invisible-space*) (toss-back-string s))))
    ((and (inside-false-world-p) (not (if-aware-ctl-seq-p z))) false)
    ((string=? z "\\enddocument") (probably-latex) ':encountered-bye)
    ((member z '("\\bye" "\\TIIPbye")) ':encountered-bye)
    ((member z '("\\endinput" "\\TIIPendinput"))
     (let ((next-token (get-token)))
       (cond
        ((and (not (eq? next-token ':eof-object)) (string=? next-token "\\fi"))
         (do-fi))
        (else false)))
     ':encountered-endinput)
    ((find-count z) (do-count= z false)) ((find-toks z) (do-toks= z false))
    ((find-dimen z) (do-dimen= z false)) (true (do-tex-prim z)))))

(define generate-html
 (lambda ()
   (let ((%fluid-var-*outer-p* true))
     (fluid-let ((*outer-p* %fluid-var-*outer-p*))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (cond ((eq? c ':eof-object) (return true))
                    ((begin (set! *it* (resolve-chardefs c)) *it*)
                     (let ((s *it*))
                       (begin (toss-back-char *invisible-space*)
                        (toss-back-string s))))
                    ((esc-char-p c)
                     (case (do-tex-ctl-seq (get-ctl-seq))
                       ((:encountered-endinput) (return true))
                       ((:encountered-bye) (return ':encountered-bye))
                       (else true)))
                    (true (get-actual-char) (do-tex-char c)))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-iffileexists
 (lambda ()
   (let ((file (actual-tex-filename (get-filename-possibly-braced))))
     (let ((then-e (ungroup (get-group))))
       (let ((else-e (ungroup (get-group))))
         (tex2page-string
          (if file
              then-e
              else-e)))))))

(define check-input-file-timestamp-p
 (lambda (f)
   (cond
    ((let ((e (file-extension f)))
       (and e (member e '(".t2p" ".bbl" ".ind"))))
     false)
    (*inputting-boilerplate-p* false) (*ignore-timestamp-p* false)
    ((> *html-only* 0) false)
    ((and
      (>=
       (let ((%length-arg f))
         ((if (string? %length-arg)
              string-length
              length)
          %length-arg))
       3)
      (char=? (string-ref f 0) #\.) (char=? (string-ref f 1) #\/))
     false)
    ((member f *verb-written-files*) false) (true true))))

(define do-inputiffileexists
 (lambda ()
   (let ((f (actual-tex-filename (get-filename-possibly-braced))))
     (let ((then-txt (ungroup (get-group))))
       (let ((else-txt (ungroup (get-group))))
         (cond (f (tex2page-string then-txt) (tex2page-file f))
               (true (tex2page-string else-txt))))))))

(define tex2page-string
 (lambda (s) (call-with-input-string/buffered s generate-html)))

(define tex2page-file
 (lambda (f)
   (write-log #\()
   (write-log f)
   (write-log ':separation-space)
   (begin (set! f (tex2page-massage-file f)) f)
   (trace-if (> (find-count "\\tracingcommands") 0) "Inputting file " f)
   (let ((%prog1-first-value (call-with-input-file/buffered f generate-html)))
     (write-log #\))
     (write-log ':separation-space)
     %prog1-first-value)))

(define tex2page-file-if-exists
 (lambda (f) (cond ((file-exists? f) (tex2page-file f)) (else false))))

(define ignorable-tex-file-p
 (lambda (f)
   (let ((e (or (file-extension f) "")))
     (cond ((string-ci=? e ".sty") true)
           (true
            (cond
             ((string-ci=? e ".tex")
              (begin
               (set! f
                (subseq f 0
                        (-
                         (let ((%length-arg f))
                           ((if (string? %length-arg)
                                string-length
                                length)
                            %length-arg))
                         4)))
               f))
             (else false))
            (cond ((string=? f "opmac") (tex-gdef-0arg "\\TZPopmac" "1") true)
                  (true (member f *tex-files-to-ignore*))))))))

(define do-input
 (lambda ()
   (ignorespaces)
   (let ((f (get-filename-possibly-braced)))
     (let ((boilerplate-index *inputting-boilerplate-p*))
       (begin
        (cond
         ((eqv? *inputting-boilerplate-p* 0)
          (begin (set! *inputting-boilerplate-p* false)
           *inputting-boilerplate-p*))
         (else false))
        (let ((%fluid-var-*inputting-boilerplate-p*
               (and boilerplate-index (add1 boilerplate-index))))
          (fluid-let
           ((*inputting-boilerplate-p* %fluid-var-*inputting-boilerplate-p*))
           (cond ((ignorable-tex-file-p f) false)
                 ((member f '("miniltx" "miniltx.tex")) (set-catcode #\@ 11)
                  false)
                 ((member f '("texinfo" "texinfo.tex"))
                  (let ((txi2p (actual-tex-filename "texi2p")))
                    (begin
                     (cond
                      ((not txi2p)
                       (terror 'do-input "File texi2p.tex not found"))
                      (else false))
                     (tex2page-file txi2p)
                     (tex2page-file *current-source-file*)
                     ':encountered-endinput)))
                 ((begin
                   (set! *it*
                    (actual-tex-filename f (check-input-file-timestamp-p f)))
                   *it*)
                  (tex2page-file *it*))
                 (true (write-log #\() (write-log f)
                  (write-log ':separation-space) (write-log "not found)")
                  (write-log ':separation-space))))))))))

(define do-includeonly
 (lambda ()
   (ignorespaces)
   (cond
    ((eq? *includeonly-list* true)
     (begin (set! *includeonly-list* null) *includeonly-list*))
    (else false))
   (let ((c (get-actual-char)))
     (cond
      ((or (eq? c ':eof-object) (not (char=? c #\{))) (terror 'do-includeonly))
      (else false)))
   (let ((%fluid-var-*filename-delims* (cons #\} (cons #\, *filename-delims*))))
     (fluid-let ((*filename-delims* %fluid-var-*filename-delims*))
      (let* ((%loop-returned false)
             (%loop-result 0)
             (return
              (lambda %args
                (set! %loop-returned true)
                (set! %loop-result (and (pair? %args) (car %args))))))
        (let %loop
          ()
          (unless %loop-returned (ignorespaces))
          (unless %loop-returned
            (let ((c (snoop-actual-char)))
              (begin
               (cond ((eq? c ':eof-object) (terror 'do-includeonly))
                     (else false))
               (cond
                ((and *comment-char* (char=? c *comment-char*)) (eat-till-eol))
                ((char=? c #\,) (get-actual-char))
                ((char=? c #\}) (get-actual-char) (return))
                ((member c *filename-delims*) (terror 'do-includeonly))
                (true
                 (let ((%push-new-stack
                        (cons (get-filename) *includeonly-list*)))
                   (begin (set! *includeonly-list* %push-new-stack)
                    *includeonly-list*)))))))
          (if %loop-returned
              %loop-result
              (%loop))))))))

(define do-include
 (lambda ()
   (let ((f (ungroup (get-group))))
     (cond
      ((or (eq? *includeonly-list* true) (member f *includeonly-list*))
       (let ((%fluid-var-*subjobname* (file-stem-name f))
             (%fluid-var-*img-file-count* 0)
             (%fluid-var-*imgdef-file-count* 0))
         (fluid-let
          ((*imgdef-file-count* %fluid-var-*imgdef-file-count*)
           (*img-file-count* %fluid-var-*img-file-count*)
           (*subjobname* %fluid-var-*subjobname*))
          (tex2page-file
           (actual-tex-filename f (check-input-file-timestamp-p f))))))
      (else false)))))

(define do-eval-string
 (lambda (s)
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
            (unless %loop-returned
              (begin
               (set! x
                (let ((%read-res (read i)))
                  (when (eof-object? %read-res) (set! %read-res ':eof-object))
                  %read-res))
               x))
            (unless %loop-returned
              (cond ((eq? x ':eof-object) (return)) (else false)))
            (unless %loop-returned (eval1 x))
            (if %loop-returned
                %loop-result
                (%loop)))))))))

(define do-eval
 (lambda (kind)
   (let ((s
          (if *outer-p*
              (ungroup
               (let ((%fluid-var-*esc-chars* (list *esc-char-verb*))
                     (%fluid-var-*expand-escape-p* true))
                 (fluid-let
                  ((*expand-escape-p* %fluid-var-*expand-escape-p*)
                   (*esc-chars* %fluid-var-*esc-chars*))
                  (get-group))))
              (tex-write-output-string (ungroup (get-group))))))
     (cond
      ((not (inside-false-world-p))
       (cond ((> *html-only* 0) (begin (set! kind ':html) kind)) (else false))
       (case kind
         ((:quiet) (do-eval-string s))
         (else
          (tex2page-string
           (cl-with-output-to-string ((current-output-port))
            (do-eval-string s))))))
      (else false)))))

(define eval-for-tex-only
 (lambda ()
   (begin (set! *eval-for-tex-only-p* true) *eval-for-tex-only-p*)
   (do-end-page)
   (ensure-file-deleted *html-page*)
   (begin (set! *main-tex-file* false) *main-tex-file*)
   (begin (set! *html-page* ".eval4texignore") *html-page*)
   (begin
    (set! *html*
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
        (else (open-input-file %f)))))
    *html*)))

(define expand-ctl-seq-into-string
 (lambda (cs)
   (let ((tmp-port (open-output-string)))
     (begin
      (let ((%fluid-var-*html* tmp-port))
        (fluid-let ((*html* %fluid-var-*html*)) (do-tex-ctl-seq cs)))
      (get-output-string tmp-port)))))

(define call-with-html-output-going-to
 (lambda (p th)
   (let ((%fluid-var-*html* p))
     (fluid-let ((*html* %fluid-var-*html*)) (th)))))

(define call-external-programs-if-necessary
 (lambda ()
   (let ((run-bibtex-p
          (cond ((not *using-bibliography-p*) false)
                ((not
                  (file-exists?
                   (let ((%type 'string)
                         (%ee
                          (list *aux-dir/* *jobname* *bib-aux-file-suffix*
                                ".aux")))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res))))
                 false)
                ((member ':bibliography *missing-pieces*) true)
                (*source-changed-since-last-run-p*
                 (flag-missing-piece ':fresh-bibliography) true)
                (true false)))
         (run-makeindex-p
          (cond ((not *using-index-p*) false)
                ((not
                  (file-exists?
                   (let ((%type 'string)
                         (%ee
                          (list *aux-dir/* *jobname* *index-file-suffix*
                                ".idx")))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res))))
                 false)
                ((member ':fresh-index *missing-pieces*) false)
                ((member ':index *missing-pieces*) true)
                (*source-changed-since-last-run-p*
                 (flag-missing-piece ':fresh-index) true)
                (true false))))
     (begin
      (cond
       (run-bibtex-p (write-log ':separation-newline)
        (write-log "Running: bibtex ") (write-log *aux-dir/*)
        (write-log *jobname*) (write-log *bib-aux-file-suffix*) (write-log #\ )
        (system
         (let ((%type 'string)
               (%ee
                (list "bibtex " *aux-dir/* *jobname* *bib-aux-file-suffix*)))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res)))
        (cond
         ((not
           (file-exists?
            (let ((%type 'string)
                  (%ee (list *jobname* *bib-aux-file-suffix* ".bbl")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
          (write-log " ... failed; try manually"))
         (else false))
        (write-log ':separation-newline))
       (else false))
      (cond
       (run-makeindex-p (write-log ':separation-newline)
        (write-log "Running: makeindex ") (write-log *aux-dir/*)
        (write-log *jobname*) (write-log *index-file-suffix*) (write-log #\ )
        (system
         (let ((%type 'string)
               (%ee
                (list "makeindex " *aux-dir/* *jobname* *index-file-suffix*)))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res)))
        (cond
         ((not
           (file-exists?
            (let ((%type 'string)
                  (%ee (list *aux-dir/* *jobname* *index-file-suffix* ".ind")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
          (write-log " ... failed; try manually"))
         (else false))
        (write-log ':separation-newline))
       (else false))
      (let* ((%f
              (let ((%type 'string) (%ee (list *jobname* ".eval4tex")))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res)))
             (%ee (list ':if-does-not-exist false))
             (%if-does-not-exist ':error)
             (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
        (when %if-does-not-exist-from-user
          (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
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
       *missing-eps-files*)))))

(define first-file-that-exists
 (lambda ff
   (let ((%dolist-l ff) (f false))
     (let* ((%loop-returned false)
            (%loop-result 0)
            (return
             (lambda %args
               (set! %loop-returned true)
               (set! %loop-result (and (pair? %args) (car %args))))))
       (let %loop
         ()
         (unless %loop-returned
           (cond ((null? %dolist-l) (return)) (else false)))
         (unless %loop-returned (set! f (car %dolist-l)))
         (unless %loop-returned (set! %dolist-l (cdr %dolist-l)))
         (unless %loop-returned
           (cond ((file-exists? f) (return f)) (else false)))
         (if %loop-returned
             %loop-result
             (%loop)))))))

(define file-in-home
 (lambda (f)
   (let ((home (retrieve-env "HOME")))
     (and home
          (let ((slash-already-p
                 (let ((n
                        (let ((%length-arg home))
                          ((if (string? %length-arg)
                               string-length
                               length)
                           %length-arg))))
                   (and (>= n 0)
                        (let ((c (string-ref home (- n 1))))
                          (or (char=? c #\/) (char=? c #\\)))))))
            (let ((%type 'string)
                  (%ee
                   (list home
                         (if slash-already-p
                             ""
                             "/")
                         f)))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))))

(define make-target-dir
 (lambda ()
   (let ((hdir-file
          (first-file-that-exists
           (let ((%type 'string) (%ee (list *jobname* ".hdir")))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))
           ".tex2page.hdir" (file-in-home ".tex2page.hdir"))))
     (cond
      (hdir-file
       (let ((hdir
              (call-with-input-file/buffered hdir-file
               (lambda () (get-filename-possibly-braced)))))
         (cond
          ((not
            (=
             (let ((%length-arg hdir))
               ((if (string? %length-arg)
                    string-length
                    length)
                %length-arg))
             0))
           (begin
            (system
             (let ((%type 'string) (%ee (list "mkdir -p " hdir)))
               (let ((%res
                      (if (eq? %type 'string)
                          ""
                          null)))
                 (let %concatenate-loop
                   ((%ee %ee))
                   (if (null? %ee)
                       %res
                       (let ((%a (car %ee)))
                         (unless (not %a)
                           (set! %res
                            (if (eq? %type 'string)
                                (string-append %res
                                 (if (string? %a)
                                     %a
                                     (list->string %a)))
                                (append %res
                                        (if (string? %a)
                                            (string->list %a)
                                            %a)))))
                         (%concatenate-loop (cdr %ee)))))
                 %res)))
            (system
             (let ((%type 'string) (%ee (list "touch " hdir "/probe")))
               (let ((%res
                      (if (eq? %type 'string)
                          ""
                          null)))
                 (let %concatenate-loop
                   ((%ee %ee))
                   (if (null? %ee)
                       %res
                       (let ((%a (car %ee)))
                         (unless (not %a)
                           (set! %res
                            (if (eq? %type 'string)
                                (string-append %res
                                 (if (string? %a)
                                     %a
                                     (list->string %a)))
                                (append %res
                                        (if (string? %a)
                                            (string->list %a)
                                            %a)))))
                         (%concatenate-loop (cdr %ee)))))
                 %res))))
           (let ((probe
                  (let ((%type 'string) (%ee (list hdir "/probe")))
                    (let ((%res
                           (if (eq? %type 'string)
                               ""
                               null)))
                      (let %concatenate-loop
                        ((%ee %ee))
                        (if (null? %ee)
                            %res
                            (let ((%a (car %ee)))
                              (unless (not %a)
                                (set! %res
                                 (if (eq? %type 'string)
                                     (string-append %res
                                      (if (string? %a)
                                          %a
                                          (list->string %a)))
                                     (append %res
                                             (if (string? %a)
                                                 (string->list %a)
                                                 %a)))))
                              (%concatenate-loop (cdr %ee)))))
                      %res))))
             (cond
              ((file-exists? probe) (ensure-file-deleted probe)
               (begin (set! *aux-dir* hdir)
                (set! *aux-dir/*
                 (let ((%type 'string) (%ee (list *aux-dir* "/")))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res)))
                *aux-dir/*))
              (else false))))
          (else false))))
      (else false)))))

(define move-aux-files-to-aux-dir
 (lambda (f)
   (cond
    ((and *aux-dir*
          (or
           (file-exists?
            (let ((%type 'string) (%ee (list f ".tex")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))
           (file-exists?
            (let ((%type 'string) (%ee (list f ".scm")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))
           (file-exists?
            (let ((%type 'string) (%ee (list f (find-img-file-extn))))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res)))))
     (system
      (let ((%type 'string) (%ee (list "mv " f ".* " *aux-dir*)))
        (let ((%res
               (if (eq? %type 'string)
                   ""
                   null)))
          (let %concatenate-loop
            ((%ee %ee))
            (if (null? %ee)
                %res
                (let ((%a (car %ee)))
                  (unless (not %a)
                    (set! %res
                     (if (eq? %type 'string)
                         (string-append %res
                          (if (string? %a)
                              %a
                              (list->string %a)))
                         (append %res
                                 (if (string? %a)
                                     (string->list %a)
                                     %a)))))
                  (%concatenate-loop (cdr %ee)))))
          %res))))
    (else false))))

(define start-css-file
 (lambda ()
   (let ((css-file
          (let ((%type 'string)
                (%ee (list *aux-dir/* *jobname* *css-file-suffix*)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (begin
      (begin
       (set! *css-port*
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
       *css-port*)
      (display "body {
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
       *css-port*)))))

(define load-aux-file
 (lambda ()
   (set-start-time)
   (let ((label-file
          (let ((%type 'string)
                (%ee (list *aux-dir/* *jobname* *label-file-suffix* ".scm")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (cond
      ((file-exists? label-file) (load-tex2page-data-file label-file)
       (delete-file label-file))
      (else false)))
   (cond
    ((not (string=? *jobname* "texput"))
     (let ((texput-aux
            (let ((%type 'string)
                  (%ee (list "texput" *aux-file-suffix* ".scm")))
              (let ((%res
                     (if (eq? %type 'string)
                         ""
                         null)))
                (let %concatenate-loop
                  ((%ee %ee))
                  (if (null? %ee)
                      %res
                      (let ((%a (car %ee)))
                        (unless (not %a)
                          (set! %res
                           (if (eq? %type 'string)
                               (string-append %res
                                (if (string? %a)
                                    %a
                                    (list->string %a)))
                               (append %res
                                       (if (string? %a)
                                           (string->list %a)
                                           %a)))))
                        (%concatenate-loop (cdr %ee)))))
                %res))))
       (cond ((file-exists? texput-aux) (delete-file texput-aux))
             (else false))))
    (else false))
   (let ((aux-file
          (let ((%type 'string)
                (%ee (list *aux-dir/* *jobname* *aux-file-suffix* ".scm")))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res))))
     (cond
      ((file-exists? aux-file) (load-tex2page-data-file aux-file)
       (delete-file aux-file))
      (else false)))
   (start-css-file)
   (cond
    ((not (null? *toc-list*))
     (begin (set! *toc-list* (reverse *toc-list*)) *toc-list*))
    (else false))
   (cond
    ((not (null? *stylesheets*))
     (begin (set! *stylesheets* (reverse *stylesheets*)) *stylesheets*))
    (else false))
   (cond
    ((not (null? *html-head*))
     (begin (set! *html-head* (reverse *html-head*)) *html-head*))
    (else false))))

(define update-last-modification-time
 (lambda (f)
   (let ((s (file-or-directory-modify-seconds f)))
     (cond
      ((and s
            (or (not *last-modification-time*) (> s *last-modification-time*)))
       (begin (set! *source-changed-since-last-run-p* true)
        (set! *last-modification-time* s) *last-modification-time*)
       (cond
        ((and
          (or (not (tex2page-flag-boolean "\\TZPcolophondisabletimestamp"))
              (tex2page-flag-boolean "\\TZPcolophontimestamp"))
          (not (tex2page-flag-boolean "\\TZPcolophonlastpage"))
          (> *html-page-count* 1))
         (flag-missing-piece ':last-modification-time))
        (else false)))
      (else false)))))

(define probably-latex
 (lambda ()
   (cond
    ((null? *tex-env*)
     (let ((%tmp (+ *latex-probability* 1)))
       (begin (set! *latex-probability* %tmp) *latex-probability*))
     (cond ((>= *latex-probability* 2) (definitely-latex)) (else false)))
    (else false))))

(define definitely-latex
 (let ((already-noted-p false))
   (lambda ()
     (cond
      ((not already-noted-p)
       (begin (set! already-noted-p true) already-noted-p) (!definitely-latex)
       (write-aux '(!definitely-latex)))
      (else false)))))

(define !tex-like-layout
 (lambda () (begin (set! *tex-like-layout-p* true) *tex-like-layout-p*)))

(define !head-line (lambda (e) (tex-def-toks "\\headline" e true)))

(define !foot-line (lambda (e) (tex-def-toks "\\footline" e true)))

(define !toc-page (lambda (p) (begin (set! *toc-page* p) *toc-page*)))

(define !index-page (lambda (p) (begin (set! *index-page* p) *index-page*)))

(define !toc-entry
 (lambda (level number page label header)
   (let ((%push-new-stack
          (cons
           (make-tocentry* ':level level ':number number ':page page ':label
            label ':header header)
           *toc-list*)))
     (begin (set! *toc-list* %push-new-stack) *toc-list*))))

(define !label
 (lambda (label html-page name value)
   (begin
    (table-put! label *label-table*
     (make-label* ':src *label-source* ':page html-page ':name name ':value
      value))
    (table-get label *label-table*))))

(define !index
 (lambda (index-number html-page-number)
   (begin (table-put! index-number *index-table* html-page-number)
    (table-get index-number *index-table*))))

(define !last-modification-time
 (lambda (s . %lambda-rest-arg)
   (let ((%lambda-rest-arg-len (length %lambda-rest-arg)) (epoch 1900))
     (when (< 0 %lambda-rest-arg-len)
       (set! epoch (list-ref %lambda-rest-arg 0)))
     false
     (begin
      (set! *last-modification-time*
       (case epoch
         ((1900) s)
         ((1970) (+ s 2208988800))
         (else (error 'ecase "0xdeadc0de"))))
      *last-modification-time*))))

(define !last-page-number
 (lambda (n) (begin (set! *last-page-number* n) *last-page-number*)))

(define !single-page (lambda () (tex-def-0arg "\\TZPsinglepage" "1")))

(define !slides (lambda () (tex-def-0arg "\\TZPslides" "1")))

(define !script
 (lambda (jsf)
   (cond
    ((or (fully-qualified-url-p jsf) (file-exists? (ensure-url-reachable jsf)))
     (let ((%push-new-stack (cons jsf *scripts*)))
       (begin (set! *scripts* %push-new-stack) *scripts*)))
    (true (write-log "! Can't find script ") (write-log jsf)
     (write-log ':separation-newline)))))

(define !using-chapters
 (lambda () (begin (set! *using-chapters-p* true) *using-chapters-p*)))

(define !definitely-latex
 (lambda ()
   (begin (set! *tex-format* ':latex) *tex-format*)
   (cond
    ((< (get-gcount "\\secnumdepth") -1) (tex-gdef-count "\\secnumdepth" 3))
    (else false))))

(define !using-external-program (lambda (x) false false))

(define !external-labels (lambda (f) false false))

(define !doctype (lambda (d) (begin (set! *doctype* d) *doctype*)))

(define !colophon
 (lambda (x)
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
      (tex-def-0arg "\\TZPcolophonweblink" "0")))))

(define !tex-text
 (lambda (n)
   (cond ((= n 0) (begin (set! *ligatures-p* false) *ligatures-p*))
         (else false))))

(define !opmac-iis
 (lambda (lhs sub)
   (cond
    ((not *opmac-index-sub-table*)
     (begin (set! *opmac-index-sub-table* (make-table ':test equal?))
      *opmac-index-sub-table*))
    (else false))
   (begin (table-put! lhs *opmac-index-sub-table* sub)
    (table-get lhs *opmac-index-sub-table*))))

(define fully-qualified-url-p
 (lambda (u) (or (substring? "//" u) (char=? (string-ref u 0) #\/))))

(define fully-qualified-pathname-p
 (lambda (f)
   (let ((n
          (let ((%length-arg f))
            ((if (string? %length-arg)
                 string-length
                 length)
             %length-arg))))
     (if (= n 0)
         true
         (let ((c0 (string-ref f 0)))
           (cond ((char=? c0 #\/) true) ((= n 1) false)
                 ((and (char-alphabetic? c0) (char=? (string-ref f 1) #\:))
                  true)
                 (true false)))))))

(define ensure-url-reachable
 (lambda (f)
   (if (and *aux-dir* (not (fully-qualified-url-p f)) (not (substring? "/" f)))
       (let ((real-f
              (let ((%type 'string) (%ee (list *aux-dir/* f)))
                (let ((%res
                       (if (eq? %type 'string)
                           ""
                           null)))
                  (let %concatenate-loop
                    ((%ee %ee))
                    (if (null? %ee)
                        %res
                        (let ((%a (car %ee)))
                          (unless (not %a)
                            (set! %res
                             (if (eq? %type 'string)
                                 (string-append %res
                                  (if (string? %a)
                                      %a
                                      (list->string %a)))
                                 (append %res
                                         (if (string? %a)
                                             (string->list %a)
                                             %a)))))
                          (%concatenate-loop (cdr %ee)))))
                  %res))))
         (begin
          (cond
           ((and (file-exists? f) (not (file-exists? real-f)))
            (system
             (let ((%type 'string) (%ee (list "cp -p " f " " real-f)))
               (let ((%res
                      (if (eq? %type 'string)
                          ""
                          null)))
                 (let %concatenate-loop
                   ((%ee %ee))
                   (if (null? %ee)
                       %res
                       (let ((%a (car %ee)))
                         (unless (not %a)
                           (set! %res
                            (if (eq? %type 'string)
                                (string-append %res
                                 (if (string? %a)
                                     %a
                                     (list->string %a)))
                                (append %res
                                        (if (string? %a)
                                            (string->list %a)
                                            %a)))))
                         (%concatenate-loop (cdr %ee)))))
                 %res))))
           (else false))
          real-f))
       f)))

(define !stylesheet
 (lambda (css)
   (cond
    ((or (fully-qualified-url-p css) (file-exists? (ensure-url-reachable css)))
     (let ((%push-new-stack (cons css *stylesheets*)))
       (begin (set! *stylesheets* %push-new-stack) *stylesheets*)))
    (true (write-log "! Can't find stylesheet ") (write-log css)
     (write-log ':separation-newline)))))

(define !html-head
 (lambda (s)
   (let ((%push-new-stack (cons s *html-head*)))
     (begin (set! *html-head* %push-new-stack) *html-head*))))

(define !html-redirect
 (lambda (url seconds)
   (begin (set! *redirect-url* url) (set! *redirect-delay* seconds)
    *redirect-delay*)
   (!html-head
    (let ((%type 'string)
          (%ee
           (list "<meta http-equiv=\"refresh\" content=\"" seconds ";" url
                 "\">")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))))

(define !default-title
 (lambda (title)
   (cond ((not *title*) (begin (set! *title* title) *title*)) (else false))))

(define !preferred-title (lambda (title) (begin (set! *title* title) *title*)))

(define !infructuous-calls-to-tex2page
 (lambda (n)
   (begin (set! *infructuous-calls-to-tex2page* n)
    *infructuous-calls-to-tex2page*)))

(define load-tex2page-data-file
 (lambda (f)
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
            (begin
             (cond
              (i
               (let ((%fluid-var-*current-source-file* f)
                     (%fluid-var-*input-line-no* 0)
                     (e false)
                     (directive false))
                 (fluid-let
                  ((*input-line-no* %fluid-var-*input-line-no*)
                   (*current-source-file* %fluid-var-*current-source-file*))
                  (let* ((%loop-returned false)
                         (%loop-result 0)
                         (return
                          (lambda %args
                            (set! %loop-returned true)
                            (set! %loop-result
                             (and (pair? %args) (car %args))))))
                    (let %loop
                      ()
                      (unless %loop-returned
                        (begin
                         (set! e
                          (let ((%read-res (read i)))
                            (when (eof-object? %read-res)
                              (set! %read-res false))
                            %read-res))
                         e))
                      (unless %loop-returned
                        (cond ((not e) (return)) (else false)))
                      (unless %loop-returned
                        (begin (set! directive (car e)) directive))
                      (unless %loop-returned
                        (let ((%tmp (+ *input-line-no* 1)))
                          (begin (set! *input-line-no* %tmp) *input-line-no*)))
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
                             "Fatal aux file error " directive
                             "; I'm stymied.")))
                         (cdr e)))
                      (if %loop-returned
                          %loop-result
                          (%loop)))))))
              (else false)))))
       (begin
        (cond
         (i
          ((if (input-port? i)
               close-input-port
               close-output-port)
           i))
         (else false))
        %with-open-file-res)))))

(define tex2page-massage-file (lambda (f) f))

(define tex2page-help
 (lambda (not-a-file)
   (cond
    ((not not-a-file) (begin (set! not-a-file "--missing-arg") not-a-file))
    (else false))
   (write-aux
    `(!infructuous-calls-to-tex2page ,(+ *infructuous-calls-to-tex2page* 1)))
   (cond
    ((not
      (or (string=? not-a-file "--help") (string=? not-a-file "--missing-arg")
          (string=? not-a-file "--version")))
     (write-log "! I can't find file `") (write-log not-a-file)
     (write-log "'.") (write-log ':separation-newline))
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
    (true
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
      (true
       (write-log "Do you need help using TeX2page?
Try the commands
  tex2page --help
  tex2page --version")
       (write-log ':separation-newline)))))
   (close-all-open-ports)))

(define non-fatal-error
 (lambda ss
   (emit-link-start
    (let ((%type 'string) (%ee (list *jobname* ".hlog")))
      (let ((%res
             (if (eq? %type 'string)
                 ""
                 null)))
        (let %concatenate-loop
          ((%ee %ee))
          (if (null? %ee)
              %res
              (let ((%a (car %ee)))
                (unless (not %a)
                  (set! %res
                   (if (eq? %type 'string)
                       (string-append %res
                        (if (string? %a)
                            %a
                            (list->string %a)))
                       (append %res
                               (if (string? %a)
                                   (string->list %a)
                                   %a)))))
                (%concatenate-loop (cdr %ee)))))
        %res)))
   (emit "<span style=\"color: red\">&#x2388;&#xa0;")
   (for-each emit-html-string ss)
   (emit-link-stop)
   (emit "</span>")))

(define do-math-ctl-seq
 (lambda (s)
   (cond ((begin (set! *it* (find-math-def s)) *it*) ((tdef*-thunk *it*)))
         (true
          (cond
           ((not *math-needs-image-p*)
            (begin (set! *math-needs-image-p* true) *math-needs-image-p*))
           (else false))
          (emit (subseq s 1))))))

(define tex-def-math-prim
 (lambda (cs thunk)
   (tex-def cs null false false thunk cs false *math-primitive-texframe*)))

(define tex-defsym-math-prim
 (lambda (cs str)
   (tex-def cs null false false (lambda () (emit str)) cs false
    *math-primitive-texframe*)))

(define make-reusable-math-image-as-needed
 (lambda (cs . expn)
   (let ((expn
          (if (null? expn)
              cs
              (car expn))))
     (tex-def-math-prim cs
      (lambda ()
        (tex2page-string
         (let ((%type 'string)
               (%ee (list "\\global\\imgdef" cs "{$" expn "$}")))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res)))
        (tex2page-string cs))))))

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

(tex-defsym-math-prim "\\cong" "&#x2245;")

(tex-defsym-math-prim "\\bowtie" "&#x22c8;")

(tex-defsym-math-prim "\\propto" "&#x221d;")

(tex-defsym-math-prim "\\models" "&#x22a8;")

(tex-defsym-math-prim "\\doteq" "&#x2250;")

(tex-defsym-math-prim "\\propto" "&#x221d;")

(tex-defsym-math-prim "\\perp" "&#x22a5;")

(define do-not
 (lambda ()
   (ignorespaces)
   (let ((c (snoop-actual-char)))
     (if (esc-char-p c)
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
                  (true (toss-back-string x) "/"))))
         (case c
           ((#\< #\> #\=)
            (get-actual-char)
            (emit
             (cond ((char=? c #\<) "&#x226e;") ((char=? c #\>) "&#x226f;")
                   ((char=? c #\=) "&#x2260;"))))
           (else (emit "/")))))))

(tex-def-math-prim "\\not" do-not)

(tex-defsym-math-prim "\\notin" "&#x2209;")

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

(tex-defsym-math-prim "\\lbrack" "[")

(tex-defsym-math-prim "\\lbrace" "{")

(tex-defsym-math-prim "\\lfloor" "&#x230a;")

(tex-defsym-math-prim "\\langle" "&#x27e8;")

(tex-defsym-math-prim "\\lceil" "&#x2308;")

(tex-defsym-math-prim "\\rbrack" "]")

(tex-defsym-math-prim "\\rbrace" "}")

(tex-defsym-math-prim "\\rfloor" "&#x230b;")

(tex-defsym-math-prim "\\rangle" "&#x27e9;")

(tex-defsym-math-prim "\\rceil" "&#x2309;")

(tex-def-math-prim "\\colon" (lambda () (emit #\:)))

(tex-def-math-prim "\\ldotp" (lambda () (emit #\.)))

(tex-let-prim "\\cdotp" "\\cdot")

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

(tex-defsym-prim "\\S" "&#xa7;")

(tex-defsym-prim "\\P" "&#xb6;")

(tex-defsym-prim "\\dag" "&#x2020;")

(tex-defsym-prim "\\ddag" "&#x2021;")

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

(tex-def-math-prim "\\eqalign" (lambda () (do-eqalign ':eqalign)))

(tex-def-math-prim "\\eqalignno" (lambda () (do-eqalign ':eqalignno)))

(tex-def-math-prim "\\displaylines" (lambda () (do-eqalign ':displaylines)))

(tex-def-math-prim "\\noalign" do-noalign)

(tex-def-math-prim "\\frac" do-frac)

(tex-def-math-prim "\\pmatrix" do-pmatrix)

(tex-def-math-prim "\\matrix" do-relax)

(tex-def-math-prim "\\eqno" do-eqno)

(define do-math-font
 (lambda (f)
   (lambda ()
     (let ((%fluid-var-*math-font* f))
       (fluid-let ((*math-font* %fluid-var-*math-font*))
        (tex2page-string (get-token)))))))

(tex-def-math-prim "\\mathbf" (do-math-font ':bf))

(tex-def-math-prim "\\mathrm" (do-math-font ':rm))

(tex-def-math-prim "\\mathbb" (do-math-font ':bb))

(tex-def-math-prim "\\mathcal" (do-math-font ':cal))

(tex-def-math-prim "\\mathfrak" (do-math-font ':frak))

(tex-def-math-prim "\\over" do-over)

(tex-def-math-prim "\\sqrt"
 (lambda () (emit "&#x221a;(") (tex2page-string (get-token)) (emit ")")))

(tex-def-math-prim "\\left" do-math-left)

(tex-def-math-prim "\\right" do-math-right)

(define kern
 (lambda (len)
   (let ((%type 'string)
         (%ee (list "<span style=\"margin-left: " len "\"> </span>")))
     (let ((%res
            (if (eq? %type 'string)
                ""
                null)))
       (let %concatenate-loop
         ((%ee %ee))
         (if (null? %ee)
             %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                  (if (eq? %type 'string)
                      (string-append %res
                       (if (string? %a)
                           %a
                           (list->string %a)))
                      (append %res
                              (if (string? %a)
                                  (string->list %a)
                                  %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res))))

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

(tex-def-prim "\\array" (lambda () (do-tabular true)))

(tex-def-prim "\\author" do-author)

(tex-def-prim "\\b" (lambda () (do-diacritic ':barunder)))

(tex-def-prim "\\begin" do-begin)

(tex-def-prim "\\beginchapter" do-beginchapter)

(tex-def-prim "\\beginsection" do-beginsection)

(tex-def-prim "\\beginthebibliography" do-thebibliography)

(tex-def-prim "\\begintt" do-begintt)

(tex-def-prim "\\begintts" (lambda () (do-scm-slatex-lines "tt" true false)))

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

(tex-def-prim "\\countdef" (lambda () (do-newcount true) (eat-integer)))

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

(tex-def-prim "\\def" (lambda () (do-def (globally-p) false)))

(tex-def-prim "\\defcsactive" (lambda () (do-defcsactive (globally-p))))

(tex-def-prim "\\definecolor" do-definecolor)

(tex-def-prim "\\DefineNamedColor" (lambda () (get-token) (do-definecolor)))

(tex-def-prim "\\definexref" do-definexref)

(tex-def-prim "\\definitelylatex" definitely-latex)

(tex-def-prim "\\defschememathescape" (lambda () (scm-set-mathescape true)))

(tex-defsym-prim "\\degree" "&#xb0;")

(tex-def-prim "\\description"
 (lambda ()
   (do-end-para)
   (let ((%push-new-stack (cons ':description *tabular-stack*)))
     (begin (set! *tabular-stack* %push-new-stack) *tabular-stack*))
   (emit "<dl><dt></dt><dd>")))

(tex-defsym-prim "\\DH" "&#xd0;")

(tex-defsym-prim "\\dh" "&#xf0;")

(tex-def-prim "\\discretionary" do-discretionary)

(tex-def-prim "\\displaymath"
 (lambda () (do-latex-env-as-image "displaymath" ':display)))

(tex-def-prim "\\divide" (lambda () (do-divide (globally-p))))

(tex-def-prim "\\document" probably-latex)

(tex-def-prim "\\documentclass" do-documentclass)

(tex-def-prim "\\dontuseimgforhtmlmath"
 (lambda () (tex-def-0arg "\\TZPmathtext" "1")))

(tex-def-prim "\\dontuseimgforhtmlmathdisplay"
 (lambda () (tex-def-0arg "\\TZPmathtext" "1")))

(tex-def-prim "\\dontuseimgforhtmlmathintext" (lambda () true))

(tex-defsym-prim "\\dots" "&#x2026;")

(tex-def-prim "\\edef" (lambda () (do-def (globally-p) true)))

(tex-def-prim-0arg "\\egroup" "}")

(tex-def-prim "\\eject" do-eject)

(tex-def-prim "\\else" do-else)

(tex-def-prim "\\em" (lambda () (do-switch ':em)))

(tex-def-prim "\\emph" (lambda () (do-function "\\emph")))

(tex-def-prim-0arg "\\empty" "")

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
 (lambda ()
   (let ((%tmp (- *html-only* 1)))
     (begin (set! *html-only* %tmp) *html-only*))))

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

(tex-def-prim "\\gdef" (lambda () (do-def true false)))

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

(tex-def-prim "\\htmlcolophon" do-htmlcolophon)

(tex-def-prim "\\htmldoctype" do-htmldoctype)

(tex-def-prim "\\htmlgif" (lambda () (do-htmlimg "htmlgif")))

(tex-def-prim "\\htmlheadonly" do-htmlheadonly)

(tex-def-prim "\\htmlimageconversionprogram" do-htmlimageconversionprogram)

(tex-def-prim "\\htmlimageformat" do-htmlimageformat)

(tex-def-prim "\\htmlimg" (lambda () (do-htmlimg "htmlimg")))

(tex-def-prim "\\htmlimgmagnification" do-htmlimgmagnification)

(tex-def-prim "\\htmlmathstyle" do-htmlmathstyle)

(tex-def-prim "\\htmlonly"
 (lambda ()
   (let ((%tmp (+ *html-only* 1)))
     (begin (set! *html-only* %tmp) *html-only*))))

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
   (cond
    ((not *inputting-boilerplate-p*)
     (begin (set! *inputting-boilerplate-p* 0) *inputting-boilerplate-p*))
    (else false))))

(tex-def-prim "\\ignorespaces" ignorespaces)

(tex-def-prim "\\ii" (lambda () (do-opmac-ii false)))

(tex-def-prim "\\iid" (lambda () (do-opmac-ii true)))

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

(tex-def-prim "\\inputindex" (lambda () (do-inputindex false)))

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

(tex-def-prim "\\latexonly" (lambda () (ignore-tex-specific-text "latexonly")))

(tex-def-prim "\\lccode" (lambda () (do-tex-case-code ':lccode)))

(tex-def-prim "\\leftdisplays"
 (lambda ()
   (begin (set! *display-justification* "left") *display-justification*)))

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

(tex-def-prim "\\makeindex" (lambda () (do-inputindex false)))

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

(tex-def-prim "\\newcommand" (lambda () (do-newcommand false)))

(tex-def-prim "\\newcount" (lambda () (do-newcount (globally-p))))

(tex-def-prim "\\newcounter" do-newcounter)

(tex-def-prim "\\newdimen" (lambda () (do-newdimen (globally-p))))

(tex-def-prim "\\newenvironment" (lambda () (do-newenvironment false)))

(tex-def-prim "\\newif" do-newif)

(tex-def-prim "\\newread" (lambda () (do-new-stream ':in)))

(tex-def-prim "\\newtheorem" do-newtheorem)

(tex-def-prim "\\newtoks" (lambda () (do-newtoks (globally-p))))

(tex-def-prim "\\newwrite" (lambda () (do-new-stream ':out)))

(tex-def-prim "\\noad"
 (lambda () (tex-def-0arg "\\TZPcolophondisablecredit" "1")))

(tex-def-prim "\\nocite" do-nocite)

(tex-def-prim "\\node" do-node)

(tex-def-prim "\\noindent" do-noindent)

(tex-def-prim "\\nonum"
 (lambda () (begin (set! *opmac-nonum-p* true) *opmac-nonum-p*)))

(tex-def-prim "\\nonumber" do-nonumber)

(tex-def-prim "\\noslatexlikecomments"
 (lambda () (tex-def-0arg "\\TZPslatexcomments" "0")))

(tex-def-prim "\\notoc"
 (lambda () (begin (set! *opmac-notoc-p* true) *opmac-notoc-p*)))

(tex-def-prim "\\notimestamp"
 (lambda () (tex-def-0arg "\\TZPcolophondisabletimestamp" "1")))

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

(tex-def-prim "\\pagebreak" (lambda () (get-bracketed-text-if-any) (do-eject)))

(tex-def-prim "\\pagecolor" do-pagecolor)

(tex-def-prim "\\pageno" (lambda () (emit *html-page-count*)))

(tex-def-prim "\\pageref" do-pageref)

(tex-def-prim "\\paragraph" (lambda () (do-heading 4)))

(tex-def-prim "\\part" (lambda () (do-heading -1)))

(tex-def-prim "\\pdfximage" do-pdfximage)

(tex-def-prim "\\picture" (lambda () (do-latex-env-as-image "picture" false)))

(tex-def-prim "\\plainfootnote" do-plain-footnote)

(tex-defsym-prim "\\pounds" "&#xa3;")

(tex-def-prim "\\printindex" (lambda () (do-inputindex true)))

(tex-def-prim "\\proclaim" do-proclaim)

(tex-def-prim "\\providecommand" (lambda () (do-newcommand false)))

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

(tex-def-prim "\\renewcommand" (lambda () (do-newcommand true)))

(tex-def-prim "\\renewenvironment" (lambda () (do-newenvironment true)))

(tex-def-prim "\\resetatcatcode" (lambda () (set-catcode #\@ 12)))

(tex-def-prim "\\resizebox" do-resizebox)

(tex-def-prim "\\rightline" (lambda () (do-function "\\rightline")))

(tex-def-prim "\\rm"
 (lambda () (cond (*math-mode-p* (do-switch ':rm)) (else false))))

(tex-def-prim "\\romannumeral" (lambda () (do-romannumeral false)))

(tex-def-prim "\\Romannumeral" (lambda () (do-romannumeral true)))

(tex-def-prim "\\ruledtable" do-ruledtable)

(tex-def-prim "\\sc" (lambda () (do-switch ':sc)))

(tex-def-prim "\\schemedisplay"
 (lambda () (do-scm-slatex-lines "schemedisplay" true false)))

(tex-def-prim "\\schemebox"
 (lambda () (do-scm-slatex-lines "schemebox" false false)))

(tex-def-prim "\\schemeresponse"
 (lambda () (do-scm-slatex-lines "schemeresponse" true ':result)))

(tex-def-prim "\\schemeresponsebox"
 (lambda () (do-scm-slatex-lines "schemeresponsebox" false ':result)))

(tex-def-prim "\\schemeresult" (lambda () (do-scm ':result)))

(tex-def-prim "\\scm" (lambda () (do-scm false)))

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

(tex-def-prim "\\settabs" do-settabs)

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

(tex-def-prim "\\tabalign" do-tabalign)

(tex-def-prim "\\tabbing" do-tabbing)

(tex-def-prim "\\table" (lambda () (do-table/figure ':table)))

(tex-def-prim "\\tableplain" do-table-plain)

(tex-def-prim "\\tableofcontents" do-toc)

(tex-def-prim "\\tabular" (lambda () (do-tabular false)))

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
   (let ((%fluid-var-*in-small-caps-p* true))
     (fluid-let ((*in-small-caps-p* %fluid-var-*in-small-caps-p*))
      (tex2page-string (get-group))))))

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

(tex-def-prim "\\undefschememathescape" (lambda () (scm-set-mathescape false)))

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

(tex-def-prim "\\xdef" (lambda () (do-def true true)))

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

(tex-let-prim "\\+" "\\tabalign")

(tex-def-prim "\\="
 (lambda ()
   (cond
    ((not
      (and (not (null? *tabular-stack*))
           (eqv? (car *tabular-stack*) ':tabbing)))
     (do-diacritic ':macron))
    (else false))))

(tex-def-prim "\\>"
 (lambda ()
   (cond
    ((and (not (null? *tabular-stack*)) (eqv? (car *tabular-stack*) ':tabbing))
     (emit-nbsp 3))
    (else false))))

(tex-def-prim "\\^" (lambda () (do-diacritic ':circumflex)))

(tex-def-prim "\\~" (lambda () (do-diacritic ':tilde)))

(tex-defsym-prim "\\#" "#")

(tex-def-prim "\\ " (lambda () (emit #\ )))

(tex-defsym-prim "\\%" "%")

(tex-defsym-prim "\\&" "&#x26;")

(tex-defsym-prim "\\@" "@")

(tex-defsym-prim "\\_" "_")

(tex-defsym-prim "\\$" "$")

(tex-def-prim
 (let ((%type 'string) (%ee (list (list #\\ #\newline))))
   (let ((%res
          (if (eq? %type 'string)
              ""
              null)))
     (let %concatenate-loop
       ((%ee %ee))
       (if (null? %ee)
           %res
           (let ((%a (car %ee)))
             (unless (not %a)
               (set! %res
                (if (eq? %type 'string)
                    (string-append %res
                     (if (string? %a)
                         %a
                         (list->string %a)))
                    (append %res
                            (if (string? %a)
                                (string->list %a)
                                %a)))))
             (%concatenate-loop (cdr %ee)))))
     %res))
 emit-newline)

(let ((tex *tex-logo*))
  (let ((ams
         (let ((%type 'string)
               (%ee
                (list "<span style=\"font-family: cursive;\">" "A"
                      "<span style=\"" "position: relative; " "top: 0.5ex; "
                      "margin-left: -.1667em; " "margin-right: -.075em"
                      "\">M</span>" "S</span>")))
           (let ((%res
                  (if (eq? %type 'string)
                      ""
                      null)))
             (let %concatenate-loop
               ((%ee %ee))
               (if (null? %ee)
                   %res
                   (let ((%a (car %ee)))
                     (unless (not %a)
                       (set! %res
                        (if (eq? %type 'string)
                            (string-append %res
                             (if (string? %a)
                                 %a
                                 (list->string %a)))
                            (append %res
                                    (if (string? %a)
                                        (string->list %a)
                                        %a)))))
                     (%concatenate-loop (cdr %ee)))))
             %res))))
    (let ((bib
           (let ((%type 'string)
                 (%ee
                  (list "B" "<span style=\"" "text-transform: uppercase"
                        "\"><small>ib</small></span>")))
             (let ((%res
                    (if (eq? %type 'string)
                        ""
                        null)))
               (let %concatenate-loop
                 ((%ee %ee))
                 (if (null? %ee)
                     %res
                     (let ((%a (car %ee)))
                       (unless (not %a)
                         (set! %res
                          (if (eq? %type 'string)
                              (string-append %res
                               (if (string? %a)
                                   %a
                                   (list->string %a)))
                              (append %res
                                      (if (string? %a)
                                          (string->list %a)
                                          %a)))))
                       (%concatenate-loop (cdr %ee)))))
               %res))))
      (let ((context
             (let ((%type 'string) (%ee (list "Con" tex "t")))
               (let ((%res
                      (if (eq? %type 'string)
                          ""
                          null)))
                 (let %concatenate-loop
                   ((%ee %ee))
                   (if (null? %ee)
                       %res
                       (let ((%a (car %ee)))
                         (unless (not %a)
                           (set! %res
                            (if (eq? %type 'string)
                                (string-append %res
                                 (if (string? %a)
                                     %a
                                     (list->string %a)))
                                (append %res
                                        (if (string? %a)
                                            (string->list %a)
                                            %a)))))
                         (%concatenate-loop (cdr %ee)))))
                 %res))))
        (let ((latex
               (let ((%type 'string)
                     (%ee
                      (list "L" "<span style=\"" "position: relative; "
                            "bottom: 0.3ex; " "margin-left: -0.36em; "
                            "margin-right: -0.15em; "
                            "text-transform: uppercase"
                            "\"><small>a</small></span>" tex)))
                 (let ((%res
                        (if (eq? %type 'string)
                            ""
                            null)))
                   (let %concatenate-loop
                     ((%ee %ee))
                     (if (null? %ee)
                         %res
                         (let ((%a (car %ee)))
                           (unless (not %a)
                             (set! %res
                              (if (eq? %type 'string)
                                  (string-append %res
                                   (if (string? %a)
                                       %a
                                       (list->string %a)))
                                  (append %res
                                          (if (string? %a)
                                              (string->list %a)
                                              %a)))))
                           (%concatenate-loop (cdr %ee)))))
                   %res))))
          (let ((xe
                 (let ((%type 'string)
                       (%ee
                        (list "X" "<span style=\""
                              "text-transform: uppercase; "
                              "position: relative; " "top: 0.5ex; "
                              "margin-left: -0.125em; "
                              "margin-right: -0.1667em" "\">&#x1dd;</span>")))
                   (let ((%res
                          (if (eq? %type 'string)
                              ""
                              null)))
                     (let %concatenate-loop
                       ((%ee %ee))
                       (if (null? %ee)
                           %res
                           (let ((%a (car %ee)))
                             (unless (not %a)
                               (set! %res
                                (if (eq? %type 'string)
                                    (string-append %res
                                     (if (string? %a)
                                         %a
                                         (list->string %a)))
                                    (append %res
                                            (if (string? %a)
                                                (string->list %a)
                                                %a)))))
                             (%concatenate-loop (cdr %ee)))))
                     %res))))
            (let ((thinspace (kern ".16667em")))
              (let ((ii/e
                     (let ((%type 'string)
                           (%ee
                            (list "<span style=\"" "margin-left: .05em"
                                  "\">2<span>" "<span style=\""
                                  "position: relative; " "top: .5ex"
                                  "\">&#x3b5;</span>")))
                       (let ((%res
                              (if (eq? %type 'string)
                                  ""
                                  null)))
                         (let %concatenate-loop
                           ((%ee %ee))
                           (if (null? %ee)
                               %res
                               (let ((%a (car %ee)))
                                 (unless (not %a)
                                   (set! %res
                                    (if (eq? %type 'string)
                                        (string-append %res
                                         (if (string? %a)
                                             %a
                                             (list->string %a)))
                                        (append %res
                                                (if (string? %a)
                                                    (string->list %a)
                                                    %a)))))
                                 (%concatenate-loop (cdr %ee)))))
                         %res))))
                (let ((mf
                       (let ((%type 'string)
                             (%ee
                              (list "<span style=\"" "font-family: sans-serif"
                                    "\">METAFONT</span>")))
                         (let ((%res
                                (if (eq? %type 'string)
                                    ""
                                    null)))
                           (let %concatenate-loop
                             ((%ee %ee))
                             (if (null? %ee)
                                 %res
                                 (let ((%a (car %ee)))
                                   (unless (not %a)
                                     (set! %res
                                      (if (eq? %type 'string)
                                          (string-append %res
                                           (if (string? %a)
                                               %a
                                               (list->string %a)))
                                          (append %res
                                                  (if (string? %a)
                                                      (string->list %a)
                                                      %a)))))
                                   (%concatenate-loop (cdr %ee)))))
                           %res))))
                  (begin
                   (tex-def-prim "\\AmSTeX"
                    (lambda () (emit ams) (emit #\-) (emit tex)))
                   (tex-def-prim "\\BibTeX" (lambda () (emit bib) (emit tex)))
                   (tex-def-prim "\\ConTeXt" (lambda () (emit context)))
                   (tex-def-prim "\\eTeX"
                    (lambda () (emit "&#x3b5;-") (emit tex)))
                   (tex-def-prim "\\LaTeX" (lambda () (emit latex)))
                   (tex-def-prim "\\LaTeXe"
                    (lambda () (emit latex) (emit ii/e)))
                   (tex-def-prim "\\MF" (lambda () (emit mf)))
                   (tex-def-prim "\\TeX" (lambda () (emit tex)))
                   (tex-def-prim "\\XeLaTeX"
                    (lambda () (emit xe) (emit thinspace) (emit latex)))
                   (tex-def-prim "\\XeTeX"
                    (lambda () (emit xe) (emit tex)))))))))))))

(tex-let-prim "\\@ne" (string (integer->char 1)))

(tex-let-prim "\\tw@" (string (integer->char 2)))

(tex-let-prim "\\thr@@" (string (integer->char 3)))

(tex-let-prim "\\sixt@@n" (string (integer->char 16)))

(tex-let-prim "\\@cclv" (string (integer->char 255)))

(tex-let-prim "\\@cclvi" (string (integer->char 256)))

(tex-let-prim "\\@m" (string (integer->char 1000)))

(tex-let-prim "\\@M" (string (integer->char 10000)))

(tex-let-prim "\\@MM" (string (integer->char 20000)))

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

(tex-def-prim "\\readindexfile" (lambda () (get-token) (do-inputindex false)))

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

(tex-let-prim "\\leqalignno" "\\eqalignno")

(tex-let-prim "\\nohtmlmathdisplayimg" "\\dontuseimgforhtmlmathdisplay")

(tex-let-prim "\\writetotoc" "\\writenumberedtocline")

(tex-let-prim "\\textdegree" "\\degree")

(tex-let-prim "\\leqno" "\\eqno")

(tex-let-prim "\\trademark" "\\TM")

(tex-let-prim "\\texttrademark" "\\TM")

(tex-let-prim "\\n" "\\noindent")

(tex-let-prim "\\Lbackslash" "\\char`\\\\")

(tex-let-prim "\\Ltilde" "\\char`\\~")

(tex-let-prim "\\Llbrace" "\\char`\\{")

(tex-let-prim "\\Lrbrace" "\\char`\\}")

(tex-let-prim "\\Lsup" "\\char`\\^")

(tex-let-prim "\\Lsub" "\\char`\\_")

(define tex2page
 (lambda (tex-file)
   (cond ((not (= *write-log-index* 0)) (newline)) (else false))
   (cond
    ((or (not tex-file) (string=? tex-file ""))
     (begin (set! tex-file "--missing-arg") tex-file))
    (else false))
   (let ((%fluid-var-*afterassignment* false)
         (%fluid-var-*afterbye* null)
         (%fluid-var-*afterpar* null)
         (%fluid-var-*aux-dir* false)
         (%fluid-var-*aux-dir/* "")
         (%fluid-var-*aux-port* false)
         (%fluid-var-*bib-aux-port* false)
         (%fluid-var-*bibitem-num* 0)
         (%fluid-var-*color-names* null)
         (%fluid-var-*comment-char* #\%)
         (%fluid-var-*css-port* false)
         (%fluid-var-*current-source-file* false)
         (%fluid-var-*current-tex2page-input* false)
         (%fluid-var-*display-justification* "center")
         (%fluid-var-*doctype* *doctype*)
         (%fluid-var-*dotted-counters* false)
         (%fluid-var-*dumping-nontex-p* false)
         (%fluid-var-*equation-number* false)
         (%fluid-var-*equation-numbered-p* true)
         (%fluid-var-*equation-position* 0)
         (%fluid-var-*esc-chars* (list #\\))
         (%fluid-var-*esc-char-std* #\\)
         (%fluid-var-*esc-char-verb* #\|)
         (%fluid-var-*eval-for-tex-only-p* false)
         (%fluid-var-*external-label-tables* (make-table ':test equal?))
         (%fluid-var-*footnote-list* null)
         (%fluid-var-*footnote-sym* 0)
         (%fluid-var-*global-texframe* false)
         (%fluid-var-*graphics-file-extensions* '(".eps"))
         (%fluid-var-*html* false)
         (%fluid-var-*html-head* null)
         (%fluid-var-*html-only* 0)
         (%fluid-var-*html-page* false)
         (%fluid-var-*html-page-count* 0)
         (%fluid-var-*img-file-count* 0)
         (%fluid-var-*img-file-tally* 0)
         (%fluid-var-*imgdef-file-count* 0)
         (%fluid-var-*imgpreamble* "")
         (%fluid-var-*imgpreamble-inferred* null)
         (%fluid-var-*in-alltt-p* false)
         (%fluid-var-*in-display-math-p* false)
         (%fluid-var-*in-para-p* false)
         (%fluid-var-*in-small-caps-p* false)
         (%fluid-var-*includeonly-list* true)
         (%fluid-var-*index-count* 0)
         (%fluid-var-*index-page* false)
         (%fluid-var-*index-port* false)
         (%fluid-var-*index-table* (make-table))
         (%fluid-var-*infructuous-calls-to-tex2page* 0)
         (%fluid-var-*input-line-no* 0)
         (%fluid-var-*input-streams* (make-table))
         (%fluid-var-*inputting-boilerplate-p* false)
         (%fluid-var-*inside-appendix-p* false)
         (%fluid-var-*jobname* "texput")
         (%fluid-var-*label-port* false)
         (%fluid-var-*label-source* false)
         (%fluid-var-*label-table* (make-table ':test equal?))
         (%fluid-var-*last-modification-time* false)
         (%fluid-var-*last-page-number* -1)
         (%fluid-var-*latex-probability* 0)
         (%fluid-var-*ligatures-p* true)
         (%fluid-var-*loading-external-labels-p* false)
         (%fluid-var-*log-file* false)
         (%fluid-var-*log-port* false)
         (%fluid-var-*main-tex-file* false)
         (%fluid-var-*math-delim-left* false)
         (%fluid-var-*math-delim-right* false)
         (%fluid-var-*math-height* 0)
         (%fluid-var-*math-mode-p* false)
         (%fluid-var-*mfpic-file-num* false)
         (%fluid-var-*mfpic-file-stem* false)
         (%fluid-var-*mfpic-port* false)
         (%fluid-var-*missing-eps-files* null)
         (%fluid-var-*missing-pieces* null)
         (%fluid-var-*mp-files* null)
         (%fluid-var-*not-processing-p* false)
         (%fluid-var-*opmac-active-tt-char* false)
         (%fluid-var-*opmac-index-sub-table* false)
         (%fluid-var-*opmac-list-style* #\o)
         (%fluid-var-*opmac-nonum-p* false)
         (%fluid-var-*opmac-notoc-p* false)
         (%fluid-var-*opmac-verbinput-table* (make-table ':test equal?))
         (%fluid-var-*outer-p* true)
         (%fluid-var-*output-streams* (make-table))
         (%fluid-var-*outputting-external-title-p* false)
         (%fluid-var-*outputting-to-non-html-p* false)
         (%fluid-var-*package* *this-package*)
         (%fluid-var-*quote-level* 0)
         (%fluid-var-*reading-control-sequence-p* false)
         (%fluid-var-*recent-node-name* false)
         (%fluid-var-*redirect-delay* false)
         (%fluid-var-*redirect-url* false)
         (%fluid-var-*scm-builtins* false)
         (%fluid-var-*scm-dribbling-p* false)
         (%fluid-var-*scm-keywords* false)
         (%fluid-var-*scm-special-symbols* false)
         (%fluid-var-*scm-variables* false)
         (%fluid-var-*scripts* null)
         (%fluid-var-*section-counter-dependencies* false)
         (%fluid-var-*section-counters* (make-table))
         (%fluid-var-*slatex-math-escape* false)
         (%fluid-var-*source-changed-since-last-run-p* false)
         (%fluid-var-*stylesheets* null)
         (%fluid-var-*subjobname* false)
         (%fluid-var-*tabular-stack* null)
         (%fluid-var-*temp-string-count* 0)
         (%fluid-var-*temporarily-use-utf8-for-math-p* false)
         (%fluid-var-*tex-env* null)
         (%fluid-var-*tex-format* ':plain)
         (%fluid-var-*tex-if-stack* null)
         (%fluid-var-*tex-like-layout-p* *tex-like-layout-p*)
         (%fluid-var-*tex-output-format* false)
         (%fluid-var-*tex2page-inputs*
          (string=split (retrieve-env "TEX2PAGEINPUTS") *path-separator*))
         (%fluid-var-*title* false)
         (%fluid-var-*toc-list* null)
         (%fluid-var-*toc-page* false)
         (%fluid-var-*unresolved-xrefs* null)
         (%fluid-var-*using-bibliography-p* false)
         (%fluid-var-*using-chapters-p* false)
         (%fluid-var-*using-index-p* false)
         (%fluid-var-*verb-display-p* false)
         (%fluid-var-*verb-port* false)
         (%fluid-var-*verb-visible-space-p* false)
         (%fluid-var-*verb-written-files* null)
         (%fluid-var-*write-log-index* 0)
         (%fluid-var-*write-log-possible-break-p* false))
     (fluid-let
      ((*write-log-possible-break-p* %fluid-var-*write-log-possible-break-p*)
       (*write-log-index* %fluid-var-*write-log-index*)
       (*verb-written-files* %fluid-var-*verb-written-files*)
       (*verb-visible-space-p* %fluid-var-*verb-visible-space-p*)
       (*verb-port* %fluid-var-*verb-port*)
       (*verb-display-p* %fluid-var-*verb-display-p*)
       (*using-index-p* %fluid-var-*using-index-p*)
       (*using-chapters-p* %fluid-var-*using-chapters-p*)
       (*using-bibliography-p* %fluid-var-*using-bibliography-p*)
       (*unresolved-xrefs* %fluid-var-*unresolved-xrefs*)
       (*toc-page* %fluid-var-*toc-page*) (*toc-list* %fluid-var-*toc-list*)
       (*title* %fluid-var-*title*)
       (*tex2page-inputs* %fluid-var-*tex2page-inputs*)
       (*tex-output-format* %fluid-var-*tex-output-format*)
       (*tex-like-layout-p* %fluid-var-*tex-like-layout-p*)
       (*tex-if-stack* %fluid-var-*tex-if-stack*)
       (*tex-format* %fluid-var-*tex-format*) (*tex-env* %fluid-var-*tex-env*)
       (*temporarily-use-utf8-for-math-p*
        %fluid-var-*temporarily-use-utf8-for-math-p*)
       (*temp-string-count* %fluid-var-*temp-string-count*)
       (*tabular-stack* %fluid-var-*tabular-stack*)
       (*subjobname* %fluid-var-*subjobname*)
       (*stylesheets* %fluid-var-*stylesheets*)
       (*source-changed-since-last-run-p*
        %fluid-var-*source-changed-since-last-run-p*)
       (*slatex-math-escape* %fluid-var-*slatex-math-escape*)
       (*section-counters* %fluid-var-*section-counters*)
       (*section-counter-dependencies*
        %fluid-var-*section-counter-dependencies*)
       (*scripts* %fluid-var-*scripts*)
       (*scm-variables* %fluid-var-*scm-variables*)
       (*scm-special-symbols* %fluid-var-*scm-special-symbols*)
       (*scm-keywords* %fluid-var-*scm-keywords*)
       (*scm-dribbling-p* %fluid-var-*scm-dribbling-p*)
       (*scm-builtins* %fluid-var-*scm-builtins*)
       (*redirect-url* %fluid-var-*redirect-url*)
       (*redirect-delay* %fluid-var-*redirect-delay*)
       (*recent-node-name* %fluid-var-*recent-node-name*)
       (*reading-control-sequence-p* %fluid-var-*reading-control-sequence-p*)
       (*quote-level* %fluid-var-*quote-level*)
       (*package* %fluid-var-*package*)
       (*outputting-to-non-html-p* %fluid-var-*outputting-to-non-html-p*)
       (*outputting-external-title-p* %fluid-var-*outputting-external-title-p*)
       (*output-streams* %fluid-var-*output-streams*)
       (*outer-p* %fluid-var-*outer-p*)
       (*opmac-verbinput-table* %fluid-var-*opmac-verbinput-table*)
       (*opmac-notoc-p* %fluid-var-*opmac-notoc-p*)
       (*opmac-nonum-p* %fluid-var-*opmac-nonum-p*)
       (*opmac-list-style* %fluid-var-*opmac-list-style*)
       (*opmac-index-sub-table* %fluid-var-*opmac-index-sub-table*)
       (*opmac-active-tt-char* %fluid-var-*opmac-active-tt-char*)
       (*not-processing-p* %fluid-var-*not-processing-p*)
       (*mp-files* %fluid-var-*mp-files*)
       (*missing-pieces* %fluid-var-*missing-pieces*)
       (*missing-eps-files* %fluid-var-*missing-eps-files*)
       (*mfpic-port* %fluid-var-*mfpic-port*)
       (*mfpic-file-stem* %fluid-var-*mfpic-file-stem*)
       (*mfpic-file-num* %fluid-var-*mfpic-file-num*)
       (*math-mode-p* %fluid-var-*math-mode-p*)
       (*math-height* %fluid-var-*math-height*)
       (*math-delim-right* %fluid-var-*math-delim-right*)
       (*math-delim-left* %fluid-var-*math-delim-left*)
       (*main-tex-file* %fluid-var-*main-tex-file*)
       (*log-port* %fluid-var-*log-port*) (*log-file* %fluid-var-*log-file*)
       (*loading-external-labels-p* %fluid-var-*loading-external-labels-p*)
       (*ligatures-p* %fluid-var-*ligatures-p*)
       (*latex-probability* %fluid-var-*latex-probability*)
       (*last-page-number* %fluid-var-*last-page-number*)
       (*last-modification-time* %fluid-var-*last-modification-time*)
       (*label-table* %fluid-var-*label-table*)
       (*label-source* %fluid-var-*label-source*)
       (*label-port* %fluid-var-*label-port*) (*jobname* %fluid-var-*jobname*)
       (*inside-appendix-p* %fluid-var-*inside-appendix-p*)
       (*inputting-boilerplate-p* %fluid-var-*inputting-boilerplate-p*)
       (*input-streams* %fluid-var-*input-streams*)
       (*input-line-no* %fluid-var-*input-line-no*)
       (*infructuous-calls-to-tex2page*
        %fluid-var-*infructuous-calls-to-tex2page*)
       (*index-table* %fluid-var-*index-table*)
       (*index-port* %fluid-var-*index-port*)
       (*index-page* %fluid-var-*index-page*)
       (*index-count* %fluid-var-*index-count*)
       (*includeonly-list* %fluid-var-*includeonly-list*)
       (*in-small-caps-p* %fluid-var-*in-small-caps-p*)
       (*in-para-p* %fluid-var-*in-para-p*)
       (*in-display-math-p* %fluid-var-*in-display-math-p*)
       (*in-alltt-p* %fluid-var-*in-alltt-p*)
       (*imgpreamble-inferred* %fluid-var-*imgpreamble-inferred*)
       (*imgpreamble* %fluid-var-*imgpreamble*)
       (*imgdef-file-count* %fluid-var-*imgdef-file-count*)
       (*img-file-tally* %fluid-var-*img-file-tally*)
       (*img-file-count* %fluid-var-*img-file-count*)
       (*html-page-count* %fluid-var-*html-page-count*)
       (*html-page* %fluid-var-*html-page*)
       (*html-only* %fluid-var-*html-only*)
       (*html-head* %fluid-var-*html-head*) (*html* %fluid-var-*html*)
       (*graphics-file-extensions* %fluid-var-*graphics-file-extensions*)
       (*global-texframe* %fluid-var-*global-texframe*)
       (*footnote-sym* %fluid-var-*footnote-sym*)
       (*footnote-list* %fluid-var-*footnote-list*)
       (*external-label-tables* %fluid-var-*external-label-tables*)
       (*eval-for-tex-only-p* %fluid-var-*eval-for-tex-only-p*)
       (*esc-char-verb* %fluid-var-*esc-char-verb*)
       (*esc-char-std* %fluid-var-*esc-char-std*)
       (*esc-chars* %fluid-var-*esc-chars*)
       (*equation-position* %fluid-var-*equation-position*)
       (*equation-numbered-p* %fluid-var-*equation-numbered-p*)
       (*equation-number* %fluid-var-*equation-number*)
       (*dumping-nontex-p* %fluid-var-*dumping-nontex-p*)
       (*dotted-counters* %fluid-var-*dotted-counters*)
       (*doctype* %fluid-var-*doctype*)
       (*display-justification* %fluid-var-*display-justification*)
       (*current-tex2page-input* %fluid-var-*current-tex2page-input*)
       (*current-source-file* %fluid-var-*current-source-file*)
       (*css-port* %fluid-var-*css-port*)
       (*comment-char* %fluid-var-*comment-char*)
       (*color-names* %fluid-var-*color-names*)
       (*bibitem-num* %fluid-var-*bibitem-num*)
       (*bib-aux-port* %fluid-var-*bib-aux-port*)
       (*aux-port* %fluid-var-*aux-port*) (*aux-dir/* %fluid-var-*aux-dir/*)
       (*aux-dir* %fluid-var-*aux-dir*) (*afterpar* %fluid-var-*afterpar*)
       (*afterbye* %fluid-var-*afterbye*)
       (*afterassignment* %fluid-var-*afterassignment*))
      (initialize-globals)
      (begin
       (set! *main-tex-file*
        (actual-tex-filename tex-file (check-input-file-timestamp-p tex-file)))
       *main-tex-file*)
      (write-log "This is TeX2page, Version ") (write-log *tex2page-version*)
      (write-log #\ ) (write-log #\() (write-log *scheme-version*)
      (write-log #\)) (write-log #\ )
      (write-log (seconds-to-human-time (current-seconds)))
      (write-log ':separation-newline)
      (cond
       (*main-tex-file*
        (begin (set! *subjobname* *jobname*)
         (set! *html-page*
          (let ((%type 'string)
                (%ee (list *aux-dir/* *jobname* *output-extension*)))
            (let ((%res
                   (if (eq? %type 'string)
                       ""
                       null)))
              (let %concatenate-loop
                ((%ee %ee))
                (if (null? %ee)
                    %res
                    (let ((%a (car %ee)))
                      (unless (not %a)
                        (set! %res
                         (if (eq? %type 'string)
                             (string-append %res
                              (if (string? %a)
                                  %a
                                  (list->string %a)))
                             (append %res
                                     (if (string? %a)
                                         (string->list %a)
                                         %a)))))
                      (%concatenate-loop (cdr %ee)))))
              %res)))
         *html-page*)
        (begin
         (set! *html*
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
             (else (open-input-file %f)))))
         *html*)
        (do-start)
        (let ((%fluid-var-*html-only* (add1 *html-only*)))
          (fluid-let ((*html-only* %fluid-var-*html-only*))
           (tex2page-file-if-exists (file-in-home ".tex2page.t2p"))
           (tex2page-file-if-exists ".tex2page.t2p")
           (let ((f
                  (actual-tex-filename
                   (let ((%type 'string) (%ee (list *jobname* ".t2p")))
                     (let ((%res
                            (if (eq? %type 'string)
                                ""
                                null)))
                       (let %concatenate-loop
                         ((%ee %ee))
                         (if (null? %ee)
                             %res
                             (let ((%a (car %ee)))
                               (unless (not %a)
                                 (set! %res
                                  (if (eq? %type 'string)
                                      (string-append %res
                                       (if (string? %a)
                                           %a
                                           (list->string %a)))
                                      (append %res
                                              (if (string? %a)
                                                  (string->list %a)
                                                  %a)))))
                               (%concatenate-loop (cdr %ee)))))
                       %res)))))
             (cond (f (tex2page-file f)) (else false)))))
        (cond
         ((not (eqv? (tex2page-file *main-tex-file*) ':encountered-bye))
          (insert-missing-end))
         (else false))
        (do-bye))
       (true (tex2page-help tex-file)))
      (output-stats)))))

(define (main . args) (tex2page (and (>= (length args) 1) (list-ref args 0))))
