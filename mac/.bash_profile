# MacPorts is used to install most of the tools

# Third-party installation procedure:
#
#   mkdir build
#   cd build
#   ../configure --prefix=/opt/<name>-version
#   make
#   sudo make install
#   sudo ln -s /opt/<name>-version /opt/<name>
#   ln -s /opt/<name>-version/bin/<executable> ~/bin/<name>

#------------------------------#
# Additional system settings   #
#------------------------------#

HISTSIZE=1000
HISTFILESIZE=20000
HISTCONTROL=ignoreboth

# no new mail notifications
shopt -u mailwarn
unset MAILCHECK

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# causes output from background processes to be output
set -b
# notify when jobs running in background terminate
set -o notify
# no noises. ever!
set bell-style none
# ignore case for completion
set completion-ignore-case on
# necessary for bash completion
shopt -s extglob
# bash history is only saved when close terminal, not after each command and this fixes it
shopt -s histappend histreedit histverify
# pathname expansion will be treated as case-insensitive
shopt -s nocaseglob
# no empty completion
shopt -s no_empty_cmd_completion
# (core file size) don't want any coredumps
ulimit -S -c 0

#------------------------------#
# Additional personal settings #
#------------------------------#

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# add console colors
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# runs emacs as server if not
export ALTERNATE_EDITOR=""

export EDITOR="emacs -Q -D"
export VISUAL="emacs -Q -D"
export PAGER="less --quit-if-one-screen -Mg"

# some very specific programs that are not in distribution
PATH=$PATH:~/bin

if [ -f /opt/local/share/git-core/git-prompt.sh ]; then
    . /opt/local/share/git-core/git-prompt.sh
fi

if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

# If id command returns zero, you’ve root access.
if [ $(id -u) -eq 0 ];
then # you are root, set red colour prompt
    PS1="\\[$(tput setaf 1)\\]\\u@\\h:\\w # \\[$(tput sgr0)\\]"
else # normal
    PS1="(\[\033[0;36m\]\t\[\e[0m\]) [\[\033[1;31m\]\w\[\033[0;31m\]\[\e[1;30m\]\$(__git_ps1)\[\e[0m\]]$ "
fi

# for scripts debug
export PS4='+${BASH_SOURCE}:${LINENO}:${FUNCNAME[0]}: '

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31'

alias g='grep -inIEr'

alias L='less --ignore-case --line-numbers --hilite-unread --hilite-search --LONG-PROMPT --no-init --quit-if-one-screen --quit-on-intr --RAW-CONTROL-CHARS'

alias diff=colordiff

alias e="emacs -nw -q -l ~/.emacs-basic"
alias em="emacsclient -c -n"
alias _emacs="nohup /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs --debug-init &"
alias git-root='cd $(git rev-parse --show-cdup)'

alias reload='source ~/.bash_profile'

alias df='df -h'
alias dirsize='du -hs'
alias biggest='BLOCKSIZE=1048576; du -x | sort -nr | head -10'

alias psu='ps -eo pcpu -o pid -o command -o user|sort -nr|head'
alias proc='ps -jfx'
alias ports='netstat -ltn -p tcp'

# ls section
export LC_COLLATE=C

alias ll='ls -laFoh'
alias l='ls -GlhSr'               # quick listing
alias lx='ls -lXB'                # sort by extension
alias lk='ls -lSr'                # sort by size
alias la='ls -Al'                 # show hidden files
alias lr='ls -lR'                 # recursice ls
alias lt='ls -ltr'                # sort by date
alias lc="ls -lcr"                # sort by change time
alias lu="ls -lur"                # sort by access time
alias lsd='ls -l | grep "^d"'     # list only directories
alias lsize='ls --sort=size -lhr' # list by size
alias tree='tree -Csu'            # nice alternative to 'ls'

# autocomplete ssh commands
complete -W "$(echo `cat ~/.bash_history | egrep '^ssh ' | sort | uniq | sed 's/^ssh //'`;)" ssh

#------------------------------#
# Usefull functions            #
#------------------------------#

function netinfo() {
    echo "--------------- Network Information ---------------"
    ifconfig | awk /'inet addr/ {print $2}'
    ifconfig | awk /'Bcast/ {print $3}'
    ifconfig | awk /'inet addr/ {print $4}'
    ifconfig | awk /'HWaddr/ {print $4,$5}'
    myip=`lynx -dump -hiddenlinks=ignore -nolist http://checkip.dyndns.org:8245/ | sed '/^$/d; s/^[ ]*//g; s/[ ]*$//g' `
    echo -e "\n${myip}\n"
    echo "----------- Application using internet ------------"
    lsof -P -i -n | cut -f 1 -d " "| uniq | tail -n +2
}

function bak() {
    cp -R $1 $1_`date +%H:%M:%S_%d-%m-%Y`
}

function ds() {
    echo "Size of directories in MB"
    if [ $# -lt 1 ] || [ $# -gt 2 ]; then
        echo "... you did not specify a directy, using pwd"
        DIR=$(pwd)
        find $DIR -maxdepth 1 -type d -exec du -sm \{\} \; | sort -nr
    else
        find $1 -maxdepth 1 -type d -exec du -sm \{\} \; | sort -nr
    fi
}

# Find a file(s)... with pattern $1 in name and execute $2 on it
function fe() { find . -type f -name $1 -exec "${2:-file}" {} \;  ; }

# Find a file(s)... who is the newest file in a directory
function newest() { find ${1:-\.} -type f |xargs ls -lrt ; }

function fname() {
    find . ! -name '*.svn*' ! -name '*.git*' ! -name '*.hg*' ! -name '*.metadata*' -iname "*$@*";
}

function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }

# Share current folder over HTTP, port 9090
function expose_folder() { python -m SimpleHTTPServer 9090; }

# Creates an archive from directory
function mktar() { tar cvf  "${1%%/}.tar"     "${1%%/}/"; }
function mkbz2() { tar cvjf "${1%%/}.tar.bz2" "${1%%/}/"; }
function mkgz() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }

function extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1    ;;
            *.tar.gz)    tar xzf $1    ;;
            *.bz2)       bunzip2 $1    ;;
            *.rar)       rar x $1      ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1     ;;
            *.tbz2)      tar xjf $1    ;;
            *.tgz)       tar xzf $1    ;;
            *.zip)       unzip $1      ;;
            *.Z)         uncompress $1 ;;
            *.7z)        7z x $1       ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

function up() {
    usage='USAGE: up <number>'
    if [[ $# -ne 1 ]] ; then
        echo $usage && exit 65
    fi

    num=$1
    upstr='.'
    iter=0
    until [ $iter -eq $num ] ; do
        upstr="${upstr}/.."
        let iter=iter+1
    done
    cd $upstr
}

function remindme() {
    let minutes=$1*60
    sleep $minutes && zenity --warning --title="REMINDER" --text="IT'S TIME!" &
}

# Find all duplicate files
function duplics() {
    find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 \
        find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate
}

# Capture DNS calls with Wireshark
function dnscap() {
    sudo tshark -i eth0 -f "udp port 53" -R "dns.qry.type == A and dns.flags.response == 0"
}

function tcp8080cap() {
    sudo tcpdump -s 0 -A -i lo 'tcp port 8080 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'
}

function urlcap() {
    sudo tcpflow -p -c -i eth0 | grep -oE '(GET|POST|HEAD) .* HTTP/1.[01]|Host: .*'
}

function search() {
    find $(pwd) -type d '(' -name .git ')' -prune -o -type f -exec grep -nH -e "$1" {} +
}

#------------------------------#
# My Programming environment   #
#------------------------------#

# Which Java thread cause CPU load
function jthread() {
    PID=$(top -n1 | grep -m1 java | perl -pe 's/\e\[?.*?[\@-~] ?//g' | cut -f1 -d' ')
    NID=$(printf "%x" $(top -n1 -H | grep -m1 java | perl -pe 's/\e\[?.*?[\@-~] ?//g' | cut -f1 -d' '))
    jstack $PID | grep -A500 $NID | grep -m1 "^$" -B 500
}

# Search JARs for a given class
function findInJar() {
    LOOK_FOR=$1

    for i in `find . -name "*jar"`
    do
        echo "Looking in $i ..."
        jar tvf $i | grep $LOOK_FOR > /dev/null
        if [ $? == 0 ]
        then
            echo "==> Found \"$LOOK_FOR\" in $i"
        fi
    done
}

# C++
export BOOST_ROOT=/opt/local/include/boost
export GLOG_ROOT=/opt/local/include/glog

# Java
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_21.jdk/Contents/Home
export CLASSPATH=.:/opt/clojure/clojure-1.5.2-SNAPSHOT.jar

# Ant
export ANT_OPTS="-Xms512m -Xmx1024m"

# Maven
export MAVEN_OPTS="-Xms512m -Xmx1024m"

# Maven bash-completion
# See: https://github.com/juven/maven-bash-completion

#
# PYTHON (port select --list python)
#
# Install: python setup.py install --user --record files.txt
# Remove:  cat files.txt | xargs rm -rf
# User installed packages are in ~/Library/Python/2.7/lib/python/site-packages

# Common Lisp
# sudo port install sbcl +fancy

# Git
export GIT_PS1_SHOWDIRTYSTATE=1

function git-history-clean() {
    echo "--== Will expire reflog and run GC for current branch ==--"
    branch_refs=$(git symbolic-ref HEAD 2>/dev/null)
    if [ $branch_refs ]; then
        echo "Current branch refs: ($branch_refs)"
        git reflog expire --expire=10.minute $branch_refs
        git fsck --unreachable
        git prune
        git gc
    else
        echo "ERROR: Current directory is not under Git version control."
    fi;
}

# MacPorts Installer addition on 2013-04-18_at_17:03:10: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.
