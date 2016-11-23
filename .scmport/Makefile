#last modified 2016-11-23

tex2page: ../tex2page primdefs.scm globdefs.scm scmkeywords.lisp tex2page.css
	cp -p ../tex2page .
	includesubfile.sh tex2page primdefs.scm globdefs.scm scmkeywords.lisp tex2page.css

primdefs.scm: primdefs.lisp
	cl2scm.sh primdefs.lisp

globdefs.scm: globdefs.lisp
	cl2scm.sh globdefs.lisp

primdefs.lisp: ../tex2page.lisp
	extractsubfile.sh ../tex2page.lisp primdefs.lisp

globdefs.lisp: ../tex2page.lisp
	extractsubfile.sh ../tex2page.lisp globdefs.lisp

scmkeywords.lisp: ../tex2page.lisp
	extractsubfile.sh ../tex2page.lisp scmkeywords.lisp

tex2page.css: ../tex2page.lisp
	extractsubfile.sh ../tex2page.lisp tex2page.css

clean:
	rm primdefs.* globdefs.* scmkeywords.lisp tex2page.css tex2page
