#_______________________________________________________________________________
#
#                       Makefile for your app
#_______________________________________________________________________________
#
# NOTE: Replace app with your application name
#
# Date Year, your name <your_email@example.com>
#

export PYTHONPATH=$(shell echo $(PWD))

PYTHON=python

all: clean test

test:
	$(PYTHON) -m pytest -v -l app

shell:
	$(PYTHON) manage.py shell

tox-test:
	tox

pep8:
	pep8 -r --show-source --ignore=E501,E221,W291,W391,E302,E251,E203,W293,E231,E303,E201,E225,E261,E241 --exclude=app/tests app

flake:
	$(PYTHON) -m pyflakes app |sort |uniq

db-upgrade:
	$(PYTHON) manage.py db upgrade

clean: clean-pyc clean-dist clean-docs tox-clean

clean-dist:
	@rm -rf dist
	@rm -rf build
	@rm -rf *.egg-info

tox-clean:
	@rm -rf .tox

clean-pyc:
	@find . -name '*.pyc' -exec rm -f {} +
	@find . -name '*.pyo' -exec rm -f {} +
	@find . -name '*~' -exec rm -f {} +
	@find . -name '__pycache__' -exec rm -rf {} +

docs:
	sphinx-build -b html docs/ docs/_build/

clean-docs:
	@rm -rf docs/_build/

.PHONY: clean-pyc test tox-test docs
