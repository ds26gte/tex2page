#last modified 2016-11-16

mainfile=$1
subfile=$2

sed -ne '/;#include '$subfile'/,/;#endinclude '$subfile'/p' $mainfile > $subfile

sed -i -e '/;#\(end\)\?include '$subfile'/d' $subfile
