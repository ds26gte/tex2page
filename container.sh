#!/bin/sh
#
# Run tex2page inside a container (e.g. Docker).
#
# - Tar archive with TeX files is read from container's stdin.
# - Tar archive with HTML files is written to container's stdout.
# - Log messages are written to container's stderr.
#
set -eu
exec 3>&1  # Save stdout.
exec 1>&2  # Redirect stdout to stderr.
mkdir -p /work
cd /work
tar -xf -
exec </dev/null  # Prevent further use of stdin.
mkdir -p out
echo out >.tex2page.hdir
tex2page "$@" 2>&1 | tee out/tex2page-1.log
echo "End of tex2page run 1/3."
tex2page "$@" 2>&1 | tee out/tex2page-2.log
echo "End of tex2page run 2/3."
tex2page "$@" 2>&1 | tee out/tex2page-3.log
echo "End of tex2page run 3/3."
cd /work/out
exec 1>&3  # Restore stdout.
tar --sort=name -cf - .
