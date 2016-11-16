#last modified 2016-11-16

mainfile=$1
shift

for subfile in "$@"; do
  sed -i -e '/;#include '$subfile'/a;#includedelete' $mainfile
  sed -i -e '/;#endinclude '$subfile'/i;#endincludedelete' $mainfile
  sed -i -e '/;#includedelete/,/;#endincludedelete/d' $mainfile
  sed -i -e '/;#include '$subfile'/r '$subfile $mainfile 
done
