;last modified 2017-01-01

(scmxlate-postprocess
  (scmxlate-system "cp -p my-tex2page.rkt my-tex2page")
  (scmxlate-system "chmod +x my-tex2page")
  (scmxlate-system "rm -f tex2page my-tex2page.rkt")
  (scmxlate-system "cp -p my-tex2page tex2page")
  (scmxlate-system "chmod +x tex2page"))
