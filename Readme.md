Contains configuration files in my home directory.

## Python notes

```shell
brew install pyenv

# Setup .bashrc and .bash_profile

pyenv install 3
pyenv global 3

# Check
python —version

pip --user install 'python-lsp-server[all]' black isort

cd data_science
pipenv install
pipenv install ipython jupyterlab notebook ipykernel line_profiler memory_profiler numpy pandas seaborn \
scipy matplotlib scikit-learn "fastapi[standard]" "uvicorn[standard]" pydantic sqlalchemy alembic
pipenv graph
```

## Jupyter Notebook

To work in `Pipenv`.
```shell
pipenv shell
ipython kernel install --name=`basename $VIRTUAL_ENV` --user
jupyther notebook
# select .venv kernel
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

## Bibliography

"Calculus Made Easy" by Silvanus P. Thompson<br/>
"Calculus: An Intuitive and Physical Approach" by Morris Kline<br/>
"Essence of statistics" 1982 by Geoffrey Loftus<br/>
"Probability and Stochastic Processes" 1987 by Frederick Solomon<br/>
"First Course in Linear Algebra" 1973 by Raymond Beauregard<br/>

"Leveling Up with SQL - Advanced Techniques for Transforming Data into Insights" 2023 by Mark Simon<br/>
"Python Distilled" 2021 by David Beazley<br/>

"Python Data Science Handbook - Essential Tools for Working with Data" 2023 by Jake VanderPlas<br/>
"A First Course in Machine Learning" 2016 by Simon Rogers, Mark Girolami<br/>
