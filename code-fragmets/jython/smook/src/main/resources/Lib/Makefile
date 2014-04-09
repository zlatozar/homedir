#_______________________________________________________________________________
#
#                       Makefile for SMOOK
#_______________________________________________________________________________
#
# April 2014, Zlatozar Zhelyazkov <zlatozar@gmail.com>
#

export JYTHONPATH=$(shell echo $(PWD))

all: clean test

test:
	jython -m pytest -v -l smook

flake:
	jython -m pyflakes smook

clean: clean-pyc

clean-pyc:
	@find . -name '*.pyc' -exec rm -f {} +
	@find . -name '*.pyo' -exec rm -f {} +
	@find . -name '*~' -exec rm -f {} +
	@find . -name '__pycache__' -exec rm -rf {} +

upload-docs:
	$(MAKE) -C docs html latex
	$(MAKE) -C docs/_build/latex all-pdf
	cd docs/_build/; mv html smook-docs; zip -r smook-docs.zip smook-docs; mv smook-docs html
#rsync -a docs/_build/html/ [somewhere]

docs:
	$(MAKE) -C docs clean html

.PHONY: clean-pyc test docs upload-docs
