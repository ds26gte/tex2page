;last change: 2005-03-30

(scmxlate-cond
  ((eqv? *operating-system* 'unix)
   (scmxlate-insert
     "#!/bin/sh
     \":\";exec mzscheme -q -C $0 \"$@\"
     "))

  ((eqv? *operating-system* 'windows)
   (scmxlate-insert
     ";@echo off
     ;goto :start
     \":\";dosify=$(echo $0|sed -e 's,^//\\(.\\)/,\\1:/,')
     \":\";exec mzscheme -r $dosify \"$@\"
     #|
     :start
     echo. > c:\\_temp.tmp
     echo (load (find-executable-path \"tex2page.bat\" >> c:\\_temp.tmp
                                      echo \"tex2page.bat\")) >> c:\\_temp.tmp
     mzscheme -r c:\\_temp.tmp %1
     goto :eof
     |#
     ")))


;(scmxlate-cond
;((>= *dialect-version* 200)
;  ;MzScheme version 200 and above have pushed 
;  ;system into a lib that must be explicitly
;  ;loaded
;  (require (lib "process.ss"))
;  ))
;
(scmxlate-include "plt-common-tex2page.scm")
                        
(scmxlate-postamble)

(scmxlate-cond
((eqv? *operating-system* 'windows)
  (scmxlate-insert
    "
;:eof
")))

(scmxlate-postprocess
; (call-with-output-file "t2p4plt.tex"
;   (lambda (o)
;     (fprintf o "%uncomment following to create ~
;                plt-specific document~%~%~
;                %\\let\\inpltdist t~%"))
;   'replace)
(let ((it "t2p4plt.tex"))
(when (file-exists? it) 
    (delete-file it)))
 )

(scmxlate-cond
 ((eqv? *operating-system* 'windows)
  (scmxlate-postprocess
   (call-with-output-file "tex2page.bat"
     (lambda (o)
       (fprintf o "@echo off ~%~
d:\\plt\\mzscheme -r d:\\public_html\\tex2page\\my-tex2page %1~%"))
     'replace)
   )))

