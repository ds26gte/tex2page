; last change: 2009-04-09

(scmxlate-insert
 ";tex2page
;(c) Dorai Sitaram, 1997-2002

(module tex2page-aux mzscheme
  (require mzlib/process)
  (require mzlib/date)
  (require scheme/private/more-scheme)
  ;(require (lib \"process.ss\"))
  ;(require (lib \"date.ss\"))
  (provide (all-defined-except ))

")

(scmxlate-uncall 
  require)

(scmxlate-ignore 
 ;get-arg1
 main
)

(scmxlate-rename
 ;(tex2page tex2page-aux)
 )

(scmxlate-include "plt-common-tex2page.scm")

(scmxlate-postamble)

(scmxlate-insert
  (string-append
    ")"
    "
"))

(scmxlate-postprocess

  (define save-file
    (lambda (f1 f2 . replace?)
      (let ((replace? (and (pair? replace?) (car replace?))))
        (when (and (file-exists? f2) replace?)
          (delete-file f2))
        (when (and (file-exists? f1)
                   (not (file-exists? f2)))
          (copy-file f1 f2)))))

  ;(save-file "t2p4plt.tex" "t2p4plt.tex.orig")

  ; (call-with-output-file "t2p4plt.tex"
  ;   (lambda (o)
  ;     (fprintf o "% Delete this file for ~
  ;              non-PLT-specific document~%~%~
  ;              \\let\\inpltdist t~%"))
  ;              'replace)

  (printf "Generated files are tex2page.ss and tex2page-aux.ss.~%~
          Put them in PLTHOME/collects/tex2page.~%")

  (save-file "my-tex2page" "tex2page-aux.ss" #t)

  (call-with-output-file "tex2page.ss"
    (lambda (o)
      (pretty-print
        `(module tex2page mzscheme
           ;(require (lib "etc.ss"))
           (require mzlib/etc)
           (provide tex2page)
           ;
           (define tex2page
             (lambda (f)
               (parameterize ((current-namespace (make-namespace)))
                             (namespace-require `(file
                                                   ,(path->string 
                                                   (build-path (this-expression-source-directory)
                                                                "tex2page-aux.ss"))))
                             ((namespace-variable-value 'tex2page) f)))))
        o))
        #:exists
    'replace)
  )

