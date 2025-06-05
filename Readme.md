Contains configuration files in my home directory.

## Python notes

```shell
brew install pyenv

# Setup .bashrc and .bash_profile

pyenv install 3
pyenv global 3

# Check
python —version

pip install 'python-lsp-server[all]' black isort ipython jupyterlab line_profiler memory_profiler

cd data_science
pipenv install
pipenv install numpy pandas scipy matplotlib scikit-learn "fastapi[standard]" "uvicorn[standard]" pydantic sqlalchemy alembic

```