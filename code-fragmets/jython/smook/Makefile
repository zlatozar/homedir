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

build:
	@ $(MAVEN) package

compile:
	@ $(MAVEN) compile

# Helper rule
_jar_clean:
	@ rm -rf target/*.jar

java:_jar_clean
	@ $(MAVEN) compile jar:jar

repl:java
	@ jython -Dpython.path=.:target/smook-1.0-SNAPSHOT.jar:src/main/resources/Lib/

style:
	@ $(MAVEN) checkstyle:check

findbugs:
	@ $(MAVEN) findbugs:check

test:
	@ $(MAVEN) test

doc:
	@ $(MAVEN) javadoc:javadoc

# Helper rule
_mvn_clean:
	@ $(MAVEN) clean

clean:_mvn_clean
	@- $(RM) ./build/*
	@- $(RM) ./docs/*

docs: doc ;

javadoc: doc ;

.PHONY: clean build

help:
	@ $(ECHO) "Usage   :  make <target>"
	@ $(ECHO) "Targets :"
	@ $(ECHO) "   all ........... Compiles the project."
	@ $(ECHO) "   build ......... Builds project and archives all *.class and *.py files. Jar files are in build directory"
	@ $(ECHO) "   java .......... Builds java project and creates a jar file."
	@ $(ECHO) "   repl .......... Builds java project and runs Jython with classes in path."
	@ $(ECHO) "   compile ....... Compiles all Java files."
	@ $(ECHO) "   style ......... Builds and run Checkstyle."
	@ $(ECHO) "   findbugs ...... Runs FindBugs."
	@ $(ECHO) "   test .......... Builds and runs all unit tests."
	@ $(ECHO) "   docs .......... Generates project documentation."
	@ $(ECHO) "   clean ......... Removes build products."
	@ $(ECHO) "   help .......... Prints this help message."
