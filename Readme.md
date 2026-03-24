Contains configuration notes for my ML projects

## Python notes

```shell
brew install uv
```

Be sure that UV bin is in your path
Add in  `~/.bash_profile` following line:

# Python UV in path
export PATH=$HOME/.local/bin:$PATH

```shell
# Check
python —version

uv tool install 'python-lsp-server[all]'
uv tool install ipython
uv tool install line_profiler
uv tool install memory_profiler

mkdir data_science
cd data_science

# Init project (if need to specify Python version)
uv init
uv python pin 3.13

uv add numpy pandas pandas-stubs seaborn scipy matplotlib scikit-learn "fastapi[standard]" "uvicorn[standard]" pydantic sqlalchemy alembic
uv add --dev ipykernel ruff mypy pre-commit pytest pytest-cov

uv tree
```

Add following `pre-commit` configuration to skip committing Jupyter meta data.

```yaml
# file name: .pre-commit-config.yaml

# Force the use of 'uv' for all hooks
default_install_hook_types: [pre-commit]
default_stages: [pre-commit]

repos:
  - repo: https://github.com/kynan/nbstripout
    rev: 0.9.1
    hooks:
      - id: nbstripout
```

then run: `uv run pre-commit install`

## Jupyter Notebook

To work in particular environment there is no need to activate or deactivate - `uv` do the job.

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

In `pyproject.toml` add something like this:

```toml
[tool.ruff]
target-version = "py313"
line-length = 88
indent-width = 4

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
    "E501",     # line too long (handled by formatter)
]

[tool.ruff.lint.isort]
known-first-party = ["your project name"]

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

[tool.pylsp]
plugins = {
    "pycodestyle" = { enabled = false },
    "pyflakes" = { enabled = false },
    "pydocstyle" = { enabled = false },
    "pylint" = { enabled = false },
    "ruff" = { enabled = true }  # Let Ruff handle linting
}
```

Details are here: https://simone-carolini.medium.com/modern-python-code-quality-setup-uv-ruff-and-mypy-8038c6549dcc

## Bibliography

_"Calculus Made Easy"_ by Silvanus P. Thompson<br/>
_"Calculus: An Intuitive and Physical Approach"_ by Morris Kline<br/>
_"Essence of statistics"_ 1982 by Geoffrey Loftus<br/>
_"Probability and Stochastic Processes"_ 1987 by Frederick Solomon<br/>
_"First Course in Linear Algebra"_ 1973 by Raymond Beauregard<br/>

_"Leveling Up with SQL - Advanced Techniques for Transforming Data into Insights"_ 2023 by Mark Simon<br/>
_"Python Distilled"_ 2021 by David Beazley<br/>

_"Python Data Science Handbook - Essential Tools for Working with Data"_ 2023 by Jake VanderPlas<br/>
_"A First Course in Machine Learning"_ 2016 by Simon Rogers, Mark Girolami<br/>
