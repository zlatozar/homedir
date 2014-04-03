#!/bin/bash
#------------------------------------------------------------------------------
# script_template - Briefly describe script purpose 
#
# {Date}, Zlatozar Zhelyazkov <zlatozar@gmail.com>
#
# Copyright (c) 2012, 2013 by {Company}, Inc.
# All rights reserved.
#------------------------------------------------------------------------------
#
# Full description of script purpose........
#
# You can include even schemas like this:
# 
#  project-name
#  `-- src
#      |-- main
#      |   |-- java
#      |   `-- resources
#      |
#      `-- test
#          |-- java
#          `-- resources
#

# Debug script
#set -x

# Keep track of program name for reference, removing absolute path
PROGNAME=${0##*/}
ABSPATH=${0%/*}

# Trap to handle signals, we need to clean-up the environment before exiting
trap clean_exit INT HUP QUIT TERM

# create_error_report: Create a file with the necessary information to help
#                      submitting a bug. Include environment, logs, etc.
# @param $1 - Current error code
create_error_report() {
    echo "Error Report: $PORTDIR $TDATE" > $ERRORREPORT
    echo $DIVIDER >> $ERRORREPORT
    echo "$PROGNAME exited with error $1" >> $ERRORREPORT
    echo $DIVIDER >> $ERRORREPORT
    if [ -n "$CMDLINE" ]; then
        echo "Command-line: $CMDLINE" >> $ERRORREPORT
    else
        echo "Command-line: $PROGNAME $@" >> $ERRORREPORT
    fi
    echo $DIVIDER >> $ERRORREPORT
    if [ -n "$ENVIRONMENT" ]; then
        echo "Running environment is:" >> $ERRORREPORT
        echo $ENVIRONMENT >> $ERRORREPORT
    else
        env >> $ERRORREPORT
    fi
    if [ -n "$LOGFILE_INITED" ]; then
        echo $DIVIDER >> $ERRORREPORT
        echo "Logfile: $LOGFILE" >> $ERRORREPORT
        echo $DIVIDER >> $ERRORREPORT
        cat $LOGFILE >> $ERRORREPORT
    fi
}

# clean_exit: Handle cleaning up and exiting
# @param $1 - Exit status (Optional). Use $?, if not set
clean_exit() {
    estatus=${1:-$?}
    if [ $estatus -ne 0 ]; then
        # If error, wrap up necessary information to submit BUG
        create_error_report $estatus
    fi
    exit $estatus
}

# init_globals: Initialize script globals
# @params - NONE
init_globals() {
    TDATE=`date +%Y%m%d-%H%M%z`
    DIVIDER="==============================================================="
    ERRORREPORT="script_template-$TDATE.log"
    LOGFILE=""
    LOGFILE_INITED=0
}

# init_logfile: Initialize the log file, the default is to create the 
#               logfile in the current directory
# @params - NONE
init_logfile() {
    if [ -z "$LOGFILE" ]; then
        LOGFILE="${PROGNAME}-${TDATE}.log"
    fi
    if [ ! -f "$LOGFILE" ]; then
        >$LOGFILE
        if [ $? -ne 0 ]; then
            die "Failed to create log file $LOGFILE"
        fi
    fi
    LOGFILE_INITED=1
}

# log: Write to the log file
# @params $* - Message to be logged
log() {
    if [ $LOGFILE_INITED -eq 1 ]; then
        echo "$PROGNAME: $*" >> $LOGFILE
    else
        echo "$PROGNAME: $*"
    fi
}

# vecho: Function writes to STDOUT and log file if verbose flag is set.
# @params $* - Message to be echo'd and logged
vecho() {
    if [ -n "$VERBOSE" ]; then
        echo "$PROGNAME: $*" >&2
        if [ $LOGFILE_INITED -eq 1 ]; then
            log "$*"
        fi
    fi
}

# lecho: Log and echo - writes to STDOUT and log file
# @params $* - Message to be echo'd and logged
lecho() {
    echo "$PROGNAME: $*" >&2
    if [ $LOGFILE_INITED -eq 1 ]; then
        log "$*"
    fi
}   

# die: Exit with error. Takes string argument for reason die message.
# @params $* - Message reason for exiting with error
die() {
    echo "$PROGNAME: $@" >&2
    log "$@"
    clean_exit 1
}

# usage: Script usage message
# @params - NONE
usage() {
    cat << EOU
Usage: $PROGNAME OPTIONS

OPTIONS:

Example Usage:
EOU
}

#---------------------------------------------------------------------------
# Main Routin
#---------------------------------------------------------------------------

init_globals
CMDLINE="$@"

# Parse the command-line arguments
OPTIONS=`getopt -o v --long verbose -n "$PROGNAME" -- "$@"`

if [ $? != 0 ]; then
    echo "$PROGNAME: Error interpreting options, exiting..." >&2
    exit 1
fi
eval set -- "$OPTIONS"

while :; do
    case "$1" in
        -v|--verbose)
            VERBOSE=0
            shift
            ;;
        --)
             shift
             break
             ;;
         *)
             echo "${PROGNAME}: Error - invalid option!"
             echo
             usage
             exit 1
             ;;
     esac
done
        
# Set our working directory
if [ -z "$PORTDIR" ]; then
    PORTDIR=$PWD
fi

# Create the log file.
init_logfile

lecho "Working with directory ${PORTDIR}"

# 
# Script logic has to be placed here !!!!
#


lecho "DONE: Sctipt completed!"
exit 0

# __END__
