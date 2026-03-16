Contains configuration files in my home directory.

## Update Spacemacs

```shell
cd ~/.emacs.d/
git remote update
git pull --rebase
```

Update packages after the run: `SPC f e U` and then reload with `SPC f e R`

## Python notes

```shell
brew install uv
```

Be sure that UV bin is in your path
Add in  ~/.bash_profile following line:

# Python UV in path
export PATH=$HOME/.local/bin:$PATH

```shell
# Check
python —version

uv tool install 'python-lsp-server[all]'
uv tool install ruff
uv tool install mypy
uv tool install pre-commit
uv tool install ipython
uv tool install jupyterlab
uv tool install notebook
uv tool install ipykernel
uv tool install line_profiler
uv tool install memory_profiler

mkdir data_science
cd data_science
# Init project (if need to specify Python version)
uv init --python 3.13 data_science

uv add numpy pandas seaborn scipy matplotlib scikit-learn "fastapi[standard]" "uvicorn[standard]" pydantic sqlalchemy alembic
uv --dev add pytest
uv tree
```

## Jupyter Notebook

To work in particular environment
```shell
source .venv/bin/activate
ipython
jupyther notebook
deactivate
```

## SQL notes

```shell
brew services start postgresql@14
createuser --superuser postgres

psql -h localhost -U postgres

# Create a DB

#CREATE USER <db user> WITH PASSWORD ‘<db pass’;
#CREATE DATABASE <db name>;
#GRANT ALL PRIVILEGES ON DATABASE <db_name> TO <db_user>;

psql -d <db user> -U <db pass>

# To stop postgresql server
# brew services stop postgresql@14

# Uninstall proceduer
brew uninstall postgresql@14
rm -rf /opt/homebrew/var/postgresql@14
```

## Emacs SQL mode

```shell
brew install sql-lint sqlfmt golang
go install github.com/sqls-server/sqls@latest
# export PATH=$HOME/go/bin:$PATH
```

## Project file

In pyproject.toml add something like this:

```toml
[tool.ruff]
target-version = "py313"
line-length = 100
[tool.ruff.lint]
select = [
    "E", "W",   # pycodestyle
    "F",        # Pyflakes
    "I",        # isort (imports)
    "B",        # flake8-bugbear (common bugs)
    "C4",       # flake8-comprehensions
    "UP",       # pyupgrade (modern syntax)
    "ARG",      # flake8-unused-arguments
    "PTH",      # flake8-use-pathlib
    "SIM",      # flake8-simplify
    "RUF",      # Ruff-specific rules
    "TID",      # flake8-tidy-imports (ban relative imports)
]
ignore = [
    "E501",   # line too long (handled by formatter)
]
[tool.ruff.lint.isort]
known-first-party = ["your_project"]
[tool.mypy]
python_version = "3.13"
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_any_generics = true
check_untyped_defs = true
no_implicit_reexport = true
warn_redundant_casts = true
warn_unused_ignores = true
```

Details are here: https://simone-carolini.medium.com/modern-python-code-quality-setup-uv-ruff-and-mypy-8038c6549dcc

## Bibliography

_"Calculus Made Easy"_ by Silvanus P. Thompson<br/>
"Calculus: An Intuitive and Physical Approach" by Morris Kline<br/>
"Essence of statistics" 1982 by Geoffrey Loftus<br/>
"Probability and Stochastic Processes" 1987 by Frederick Solomon<br/>
"First Course in Linear Algebra" 1973 by Raymond Beauregard<br/>

"Leveling Up with SQL - Advanced Techniques for Transforming Data into Insights" 2023 by Mark Simon<br/>
"Python Distilled" 2021 by David Beazley<br/>

"Python Data Science Handbook - Essential Tools for Working with Data" 2023 by Jake VanderPlas<br/>
"A First Course in Machine Learning" 2016 by Simon Rogers, Mark Girolami<br/>
