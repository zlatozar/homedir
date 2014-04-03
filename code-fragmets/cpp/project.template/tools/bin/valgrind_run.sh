#!/bin/bash

if test "$#" "<" "1"; then
    echo "$0: too few arguments"
    exit 1
fi

VLG=`which valgrind`
VLG_LOG=`basename $PWD`
VLG_LOG="log.$VLG_LOG"
VLG_OPTS="$VLG_OPTS --verbose"
VLG_OPTS="$VLG_OPTS --trace-children=yes"
VLG_OPTS="$VLG_OPTS --track-fds=yes"
VLG_OPTS="$VLG_OPTS --run-libc-freeres=yes"
VLG_OPTS="$VLG_OPTS --logfile-fd=1"
VLG_OPTS="$VLG_OPTS --num-callers=100"
VLG_OPTS="$VLG_OPTS --error-limit=no"
VLG_OPTS="$VLG_OPTS --show-below-main=yes"
VLG_OPTS="$VLG_OPTS --pointercheck=yes"
VLG_OPTS="$VLG_OPTS --demangle=yes"
VLG_OPTS="$VLG_OPTS --leak-check=yes"

export VLG VLG_LOG VALGRIND_OPTS

if test -r "$VLG_LOG"; then
    echo "$0: removing $VLG_LOG"
    rm -f -v "$VLG_LOG"
fi
$VLG $VLG_OPTS $* 2>&1 | tee "$VLG_LOG"
