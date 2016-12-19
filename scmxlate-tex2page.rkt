;last modified 2016-12-19

(scmxlate-postprocess
  (scmxlate-system "cp -p my-tex2page.rkt my-tex2page")
  (scmxlate-system "rm -f tex2page my-tex2page.rkt")
  (scmxlate-system "cp -p my-tex2page tex2page"))
