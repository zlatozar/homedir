#______________________________________________________________________________
#
#          Project Convenience Makefile Wrapper for Maven2
#______________________________________________________________________________
#

# This Makefile is just a convenience wrapper for the Maven2
# program. The actual building rules for this project may
# be found in the Maven2 "pom.xml" file located in this folder.

#______________________________________________________________________________
#                                                                  DEFINITIONS

# Define the commandline invocation of Maven2:
MAVEN  := mvn

# Define the commandline invocation for removing files and folders:
RM     := rm -rf

# Define the commandline invocation for echoing to the console
ECHO   := echo

#______________________________________________________________________________
#                                                                BUILD TARGETS

all:
	@ $(MAVEN) compile

package:
	@ $(MAVEN) package

compile:
	@ $(MAVEN) compile

check:
	@ $(MAVEN) test

check-style:
	@ $(MAVEN) clean install checkstyle:check

findbugs:
	@ $(MAVEN) findbugs:check

test: compile
	@ $(MAVEN) test

doc:
	@ $(MAVEN) javadoc:javadoc

# Helper rule
_mvn_clean:
	@ $(MAVEN) clean

clean:_mvn_clean
	@- $(RM) ./bin/*
	@- $(RM) ./docs/*

distclean: clean ;

docs: doc ;

javadoc: doc ;

documentation: doc ;

help:
	@ $(ECHO) "Usage   :  make <target>"
	@ $(ECHO) "Targets :"
	@ $(ECHO) "   all ........... Compiles the project."
	@ $(ECHO) "   package ....... Builds project and archives all *.class files."
	@ $(ECHO) "   compile ....... Compiles all Java files."
	@ $(ECHO) "   check ......... Builds and runs all unit tests."
	@ $(ECHO) "   check-style.... Builds and run Checkstyle."
	@ $(ECHO) "   findbugs ...... Runs FindBugs."
	@ $(ECHO) "   test .......... Builds and runs all unit tests."
	@ $(ECHO) "   docs .......... Generates project documentation."
	@ $(ECHO) "   clean ......... Removes build products."
	@ $(ECHO) "   help .......... Prints this help message."
