#
# .bashrc
#

# Third-party installation procedure:
#
#   mkdir build
#   cd build
#   ../configure --prefix=/opt/<name>-version
#   make
#   sudo make install
#   sudo ln -s /opt/<name>-version /opt/<name>
#   ln -s /opt/<name>-version/bin/<executable> ~/bin/<name>

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific aliases and functions

# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi

#------------------------------#
# Additional system settings   #
#------------------------------#

HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# no new mail notifications
shopt -u mailwarn
unset MAILCHECK

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
# no empty completion
shopt -s no_empty_cmd_completion
# (core file size) don't want any coredumps
ulimit -S -c 0

#------------------------------#
# Additional personal settings #
#------------------------------#

# Enable all alias as SUDO user
alias sudo='sudo '

# runs emacs as server if not
export ALTERNATE_EDITOR=""

export EDITOR="emacs -Q -D"
export VISUAL="emacs -Q -D"
export PAGER="less --quit-if-one-screen -Mg"

# 'C-c C-c' when you are ready
export GIT_EDITOR=emacsclient

# If id command returns zero, you’ve root access.

GIT_PS1_SHOWDIRTYSTATE=true
if [ $(id -u) -eq 0 ];
then # you are root, set red colour prompt
    PS1="\\[$(tput setaf 1)\\]\\u@\\h:\\w # \\[$(tput sgr0)\\]"
else # normal
    # PS1="(\[\033[0;36m\]\t\[\e[0m\]) [\[\033[1;37m\]\w\[\033[0;37m\]\[\e[1;30m\]\$(__git_ps1)\[\e[0m\]]$ "
    PS1="(\[\033[0;36m\]\t\[\e[0m\]) [\[\033[1;31m\]\w\[\033[0;31m\]\[\e[1;36m\]\$(__git_ps1)\[\e[0m\]]$ "
fi

# for scripts debug
export PS4='+${BASH_SOURCE}:${LINENO}:${FUNCNAME[0]}: '

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

export GREP_COLOR='1;31'

alias g='grep -inIEr'

alias L='less --ignore-case --line-numbers --hilite-unread --hilite-search --LONG-PROMPT --no-init --quit-if-one-screen --quit-on-intr --RAW-CONTROL-CHARS'

alias diff=colordiff

alias e="emacs -nw -q -l ~/.emacs.d/basic-init.el"
alias ec="emacsclient -c"
alias em="emacsclient -c -n"
alias git-root='cd $(git rev-parse --show-cdup)'

alias reload='source ~/.bashrc'

alias df='df -Tah -x tmpfs -x usbfs'
alias dirsize='du -hs'
alias biggest='BLOCKSIZE=1048576; du -x | sort -nr | head -10'

alias psu='ps -eo pcpu -o pid -o command -o user|sort -nr|head'
alias cpu="ps -e -o pcpu,cpu,nice,state,cputime,args --sort pcpu | sed '/^ 0.0 /d'"
alias memu='ps -e -o rss=,args= | sort -b -k1,1n | pr -TW$COLUMNS'
alias pst='pstree -hAcpul'
alias proc='ps jfx'
alias ports='netstat -ltnp |grep LISTEN'
alias connections='sudo lsof -n -P -i +c 15'

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

#------------------------------#
# Usefull functions            #
#------------------------------#

function netinfo() {
    echo "--------------- Network Information ---------------"
    /sbin/ifconfig | awk /'inet addr/ {print $2}'
    /sbin/ifconfig | awk /'Bcast/ {print $3}'
    /sbin/ifconfig | awk /'inet addr/ {print $4}'
    /sbin/ifconfig | awk /'HWaddr/ {print $4,$5}'
    myip=`lynx -dump -hiddenlinks=ignore -nolist http://checkip.dyndns.org:8245/ | sed '/^$/d; s/^[ ]*//g; s/[ ]*$//g' `
    echo -e "\n${myip}\n"
    echo "----------- Application using internet ------------"
    /usr/sbin/lsof -P -i -n | cut -f 1 -d " "| uniq | tail -n +2
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

# Find all duplicate files
function duplics() {
    find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 \
        find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate
}

function encrypt() {
    gpg -ac --no-options "$1"
}

function decrypt() {
    gpg --no-options "$1"
}

# Capture DNS calls with Wireshark
function dnscap() {
    sudo /usr/sbin/tshark -i eth0 -f "udp port 53" -R "dns.qry.type == A and dns.flags.response == 0"
}

function tcp8080cap() {
    sudo /usr/sbin/tcpdump -s 0 -A -i lo 'tcp port 8080 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'
}

function urlcap() {
    sudo tcpflow -p -c -i eth0 | grep -oE '(GET|POST|HEAD) .* HTTP/1.[01]|Host: .*'
}

function pid2command() {
    ps $1 | tail -1 | awk '{i=5; while (i<NF) {printf "%s ", $i; i++}; print $NF}'
}

# Track all config files

# dotfiles config --local status.showUntrackedFiles no
alias dotfiles='git --git-dir=/home/zlatozar/.dotfiles --work-tree=/'

# Show dotfiles repository
dot() {
  if [[ "$#" -eq 0 ]]; then
    (cd /
    for i in $(dotfiles ls-files); do
      echo -n "$(dotfiles -c color.status=always status $i -s | sed "s#$i##")"
      echo -e "¬/$i¬\e[0;33m$(dotfiles -c color.ui=always log -1 --format="%s" -- $i)\e[0m"
    done
    ) | column -t --separator=¬ -T2
  else
    dotfiles $*
  fi
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

#
# HTTP
# https://httpie.org/

#
# C/C++ settings
#
alias objdump='objdump -d -S -hrt'

#
# JAVA settings
#
export JAVA_HOME=$(dirname $(dirname $(readlink $(readlink $(which javac)))))

# Maven and maven-bash-completion could be installed form Linux packages
# See: https://github.com/juven/maven-bash-completion
# export MAVEN_OPTS="-Xms512m -Xmx1024m"

#
# Python settings
#
#export PYTHONPATH="${PYTHONPATH}:    "

# Installed: pyenv and pyenv-virtualenv plugin
# Packages in system python: pip3 install flake8 isort black python-lsp-server

#
# Docker
#
alias d-clean='docker system prune'
alias d-list='docker container ls -a --format "table {{.ID}}\t{{.Image}}\t{{.Names}}\t{{.Status}}"'
alias d-stop-all='docker stop $(docker container ls -q)'

# Docker Compose
alias dc-start='docker-compose up -d'
alias dc-build='docker-compose build'
alias dc-stop='docker-compose down --remove-orphans'
alias dc-restart='docker-compose up -d --force-recreate'
alias dc-rebuild='docker-compose down --remove-orphans && docker-compose build && docker-compose up -d'

#
# Rust
#
alias rcheck='cargo clippy --all -- -W clippy::pedantic -W clippy::nursery -W clippy::unwrap_used -W clippy::cargo -D warnings'

#
# Git
#
# curl -L https://raw.github.com/git/git/master/contrib/completion/git-prompt.sh > ~/.git-prompt.sh
source ~/.git-prompt.sh
