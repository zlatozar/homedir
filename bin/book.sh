#!/bin/sh

# This script creates a book in pdf format ready to print on a duplex printer
if [ $# -ne 1 ]; then                        # Check the argument
  echo 1>&2 "Usage: $0 PDF_File"
  exit 1                                     # non zero exit if error
fi

file=$1                                      # Assign the filename
fname=${file%.*}                             # Get the name of the file only

# create postscript booklet
pdftops -paper A4 -noshrink $fname.pdf $fname.ps
cat $fname.ps |psbook -s4 |psnup -Pa4 -2 |pstops -b "2:0,1U(21cm,29.7cm)" > $fname.book.ps

# use #a4 and #None on Windows!
ps2pdf13 -sPAPERSIZE=a4 -sAutoRotatePages=None $fname.book.ps $fname.book.pdf
exit 0  
