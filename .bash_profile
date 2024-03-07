# Programs that are not in distribution
PATH=$PATH:~/bin

# LISP settings
export SBCL_HOME=/opt/common-lisp/sbcl/lib/sbcl

# pyenv setup
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$PYENV_ROOT/shims:${PATH}"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
