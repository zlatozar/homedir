# Programs that are not in distribution
PATH=$PATH:~/bin

# Python: brew install pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# LISP settings
export SBCL_HOME=/opt/common-lisp/sbcl/lib/sbcl
