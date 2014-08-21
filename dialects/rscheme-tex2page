; last change: 2003-06-21

(scmxlate-insert 
  "\":\";echo '"
  "(define arg-one \"'$1'\")"
  "(load \"'$0'\")"
  "(main (list 1 arg-one))"
  "(exit)"
  "'|exec rs; exit"
  "\n\n")

(scmxlate-uncall
  require
  )
  

