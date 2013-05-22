#!/bin/sh

echo $1
gs -dNOPAUSE -sDEVICE=jpeg -dFirstPage=1 -dLastPage=237 -sOutputFile=image%d.jpg -dJPEGQ=100 -r300x300 -q $1 -c quit
