#last modified 2016-12-18

lf=$1

sf=${lf%.lisp}.scm

cp $lf $sf

sed -i -e 's/\\t"/\\VERBATIMt"/g' $sf
sed -i -e 's/\bt\b/#t/g' $sf
sed -i -e 's/"#t"/"t"/g' $sf
sed -i -e 's/(#t /(else /' $sf
sed -i -e 's/VERBATIMt/t/' $sf
sed -i -e "s/:\b/':/g" $sf
sed -i -e "s/'nil/VERBATIMquotenil/g" $sf
sed -i -e 's/\bnil\b/#f/g' $sf
sed -i -e "s/VERBATIMquotenil/'nil/g" $sf
sed -i -e "s/#'//g" $sf
sed -i -e 's/\beql\b/eqv?/g' $sf
sed -i -e 's/\bnull\b/null?/g' $sf
sed -i -e 's/\(char\|string\)= /\1=? /g' $sf
sed -i -e 's/\bsetq\b/set!/g' $sf
sed -i -e 's/code-char/integer->char/g' $sf
sed -i -e "s/concatenate 'string/string-append/g" $sf
sed -i -e 's/(string-append (list \(.*\)))/(string \1)/' $sf
sed -i -e 's/(push \(\S\+\) \(\S\+\))/(set! \2 (cons \1 \2))/' $sf
sed -i -e 's/(decf \([^)]\+\))/(set! \1 (- \1 1))/' $sf
sed -i -e 's/(incf \([^)]\+\))/(set! \1 (+ \1 1))/' $sf
sed -i -e 's/(let ((\*/(fluid-let ((*/' $sf
sed -i -e 's/(defun \(\S\+\) (\([^)]*\))/(define (\1 \2)/' $sf
