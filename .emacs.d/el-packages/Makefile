#_______________________________________________________________________________
#
#               Makefile for Emacs package installation
#
# NOTE:
#   Do not forget (require '<package>) in .emacs
#_______________________________________________________________________________
#

#_______________________________________________________________________________
#                                                                     VARIABLES

EMACS = emacs -q --no-site-file

EL=$(shell for f in $(PWD); do ls $$f/*.el; done)
ELC=$(EL:.el=.elc)

.SUFFIXES:	.el .elc

NO_COLOR    = \x1b[0m
OK_COLOR    = \x1b[32;01m
ERROR_COLOR = \x1b[31;01m
WARN_COLOR  = \x1b[33;01m

OK_STRING    = $(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING = $(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING  = $(WARN_COLOR)[WARNINGS]$(NO_COLOR)

INSTALL_DIR = /opt
EMACS_DEFAULT = ~/.emacs.d

#_______________________________________________________________________________
#                                                              HELPER FUNCTIONS

CHDIR_SHELL := $(SHELL)
define chdir

	$(eval _D=$(firstword $(1) $(@D)))
	$(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))

endef

#_______________________________________________________________________________
#                                                                         RULES

default: all

# Do not forget to update .PHONY
all: valgrind.elc prolog.elc hyperspec-naggum.elc mit-scheme-swank flymake-python-pyflakes slime ac-slime ac-cider-compliment notes

.el.elc:
	@ echo "\n** Bite compiling ===> " $@
	$(EMACS) -batch -f batch-byte-compile $<
	@ echo " **Finished."

valgrind.el:
	wget -N http://www.suspectclass.com/sgifford/abandoned/emacs/valgrind.el

prolog.el:
	wget -N http://bruda.ca/_media/emacs/prolog.el

hyperspec-naggum.el:
	@ echo "Eddi Weitz version"
	curl -O http://weitz.de/files/hyperspec-naggum.el

mit-scheme-swank:
	@ echo "$(OK_COLOR)===> Checkout or update mit-scheme-swank...(there is one in official distribution but this one is better)$(NO_COLOR)"
	@ if [ ! -d mit-scheme-swank ]; then git clone https://github.com/ecraven/mit-scheme-swank.git; else cd mit-scheme-swank; git pull --rebase; cd ..; fi;

_flymake-easy:
	@ if [ ! -d flymake-easy ]; then git clone https://github.com/purcell/flymake-easy.git; else cd flymake-easy; git pull --rebase; cd ..; fi;

flymake-python-pyflakes: _flymake-easy
	@ echo "Requires 'pyflakes' installed"
	@ if [ ! -d flymake-python-pyflakes ]; then git clone http://github.com/purcell/flymake-python-pyflakes.git; else cd flymake-python-pyflakes; git pull --rebase; cd ..; fi;

slime:
	@ echo "$(OK_COLOR)===> Checkout or update slime...$(NO_COLOR)"
	@ if [ ! -d slime ]; then cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime; else cd slime; cvs up; cd ..; fi;
	@ echo "$(ERROR_COLOR)Do not forget to build it\n$(NO_COLOR)"

ac-slime:
	@ echo "$(OK_COLOR)===> Checkout or update ac-slime...$(NO_COLOR)"
	@ if [ ! -d ac-slime ]; then git clone https://github.com/purcell/ac-slime.git; else cd ac-slime; git pull --rebase; cd ..; fi;

ac-cider-compliment:
	@ echo "$(OK_COLOR)===> Checkout or update ac-cider-compliment...$(NO_COLOR)"
	@ if [ ! -d ac-cider-compliment ]; then git clone http://github.com/alexander-yakushev/ac-cider-compliment.git; else cd ac-cider-compliment; git pull --rebase; cd ..; fi;

notes:
	@ echo
	@ echo "$(ERROR_COLOR)IMPORTANT!\n$(NO_COLOR)"
	@ echo "1. WordNet(wn) installation should be in $(INSTALL_DIR)/wordnet/"
	@ echo "2. valgrind, cdecl, cmake, clang, w3m and doxygen should be in your PATH"
	@ echo "3. Install as Python modules rope, ropemacs, ropemode could be installed with 'el-get'. pyflakes and pep8 using linux package or 'pip'."
	@ echo "       Example: sudo pip install pyflakes\n"
	@ echo "4. Use 'quicklisp' to add Lisp libraries"
	@ echo

#_______________________________________________________________________________
#                                                     ADDITIONAL/OPTIONAL RULES

_v8:
	$(call chdir, $(EMACS_DEFAULT))
	@ echo "$(OK_COLOR)===> Checkout or update Google V8 engine...(it is slow and SCons is required)$(NO_COLOR)"
	@ if [ ! -d v8 ]; then svn checkout http://v8.googlecode.com/svn/trunk/ v8; else cd v8; svn up; cd ..; fi;
	@ cd v8; scons sample=shell; cd ..;
	@ echo "$(ERROR_COLOR)Please migrate build to use GYP and set 'GCC_TREAT_WARNINGS_AS_ERRORS': 'NO' in build/standalone.gypi\n$(NO_COLOR)"

jslint-v8: _v8
	@ echo "$(OK_COLOR)===> Checkout or update jslint-v8...$(NO_COLOR)"
	@ if [ ! -d jslint-v8 ]; then git clone http://github.com/valeryz/jslint-v8.git; else cd jslint-v8; git pull --rebase; cd ..; fi;
	@ echo "$(ERROR_COLOR)Set path in 'flymake-jslint.el'\n$(NO_COLOR)"

_c++-dirs:
	@ echo "$(OK_COLOR)===> Prepare directory structure for C++ libraries$(NO_COLOR)"
	@ if [ ! -d $(INSTALL_DIR)/include/ ]; then sudo mkdir $(INSTALL_DIR)/include/; sudo chown $(shell echo $$USER):$(shell echo $$USER) $(INSTALL_DIR)/include/; fi;
	@ if [ ! -d $(INSTALL_DIR)/include/glog/ ]; then mkdir $(INSTALL_DIR)/include/glog/; fi;
	@ if [ ! -d $(INSTALL_DIR)/include/gtest/ ]; then mkdir $(INSTALL_DIR)/include/gtest/; fi;
	@ if [ ! -d $(INSTALL_DIR)/include/gmock/ ]; then mkdir $(INSTALL_DIR)/include/gmock/; fi;

c++: _c++-dirs

_clojure-dirs:
	@ echo "$(OK_COLOR)===> Prepare directory structure for Clojure$(NO_COLOR)"
	@ if [ ! -d $(INSTALL_DIR)/clojure/ ]; then sudo mkdir $(INSTALL_DIR)/clojure/; sudo chown $(shell echo $$USER):$(shell echo $$USER) $(INSTALL_DIR)/clojure/; fi;
	@ if [ ! -d $(INSTALL_DIR)/clojure/clojure/ ]; then mkdir $(INSTALL_DIR)/clojure/clojure/; fi;
	@ if [ ! -d $(INSTALL_DIR)/clojure/clojure-libs/ ]; then mkdir $(INSTALL_DIR)/clojure/clojure-libs/; fi;
	@ if [ ! -d $(INSTALL_DIR)/clojure/leiningen/ ]; then mkdir $(INSTALL_DIR)/clojure/leiningen/; fi;

clojure: _clojure-dirs
	$(call chdir, $(INSTALL_DIR)/clojure)
	@ echo "$(OK_COLOR)===> Install leiningen script...$(NO_COLOR)"
	@ if [ ! -d leiningen ]; then mkdir leiningen fi;
	@ cd leiningen; curl -O http://raw.github.com/technomancy/leiningen/stable/bin/lein; chmod +x lein;
	@ echo "$(ERROR_COLOR)Do you have symbolic link in ~/bin/lein?\n$(NO_COLOR)"

	@ echo "$(OK_COLOR)===> Checkout or update clojure...$(NO_COLOR)"
	@ if [ ! -d $(INSTALL_DIR)/clojure/clojure/.git ]; then git clone http://github.com/clojure/clojure.git; else cd clojure; git pull --rebase; cd ..; fi;
	@ echo "$(ERROR_COLOR)Choose branch and build it\n$(NO_COLOR)"

	@ echo "$(OK_COLOR)===> Clojure libraries should be in $(INSTALL_DIR)/clojure/clojure-libs/...$(NO_COLOR)"
	@ echo "$(ERROR_COLOR)Choose branch and build one by one\n$(NO_COLOR)"

# RITZ with all nrepl.el dependency

_nrepl:
	@ echo "$(OK_COLOR)===> Checkout or update nrepl.el(old version of CIDER)...$(NO_COLOR)"
	@ if [ ! -d nrepl.el ]; then git clone https://github.com/DerGuteMoritz/nrepl.el.git; else cd nrepl.el; git pull --rebase; cd ..; fi;

_nrepl-inspect:
	@ echo "$(OK_COLOR)===> Checkout or update nrepl-inspect...$(NO_COLOR)"
	@ if [ ! -d nrepl-inspect ]; then git clone https://github.com/vitalreactor/nrepl-inspect.git; else cd nrepl-inspect; git pull --rebase; cd ..; fi;

_fringe-helper:
	@ echo "$(OK_COLOR)===> Checkout or update fringe-helper.el...$(NO_COLOR)"
	@ if [ ! -d fringe-helper.el ]; then git clone https://github.com/nschum/fringe-helper.el.git; else cd fringe-helper.el; git pull --rebase; cd ..; fi;

_ritz:
	@ echo "$(OK_COLOR)===> Checkout or update ritz...$(NO_COLOR)"
	@ if [ ! -d ritz ]; then git clone https://github.com/pallet/ritz.git; else cd ritz; git pull --rebase; cd ..; fi;

# Meta rule
ritz-ide: _nrepl _nrepl-inspect _fringe-helper _ritz

#_______________________________________________________________________________
#                                                                  SYSTEM RULES

.PHONY: clean mit-scheme-swank flymake-python-pyflakes slime ac-slime ac-cider-compliment

clean:
	rm -f $(ELC)

distclean:
	rm -f $(EL) $(ELC) *~ */*~ .\#* */.\#*
usage:
	@ echo
	@ echo
	@ echo "usage: make [foo.el | foo.elc | clojure | slime | quicklisp | usage | clean | distclean]"
	@ echo "       make foo.el     will fetch foo.el from the Internet"
	@ echo "       make foo.elc    will byte-compile foo.el."
	@ echo "                       (fetching foo.el first if necessary)."
	@ echo "       make clojure    checkout or update leiningen, elein, clojure, clojure-mode, clojure-contrib and swank-clojure"
	@ echo
	@ echo "       make slime      checkout or update slime"
	@ echo
	@ echo "       make quicklisp  downloads quicklisp"
	@ echo
	@ echo "       make clean      will remove all .elc files in this dir."
	@ echo "       make distclean  will remove .el and .elc files in this dir."
	@ echo "       make usage      displays this message."
	@ echo
