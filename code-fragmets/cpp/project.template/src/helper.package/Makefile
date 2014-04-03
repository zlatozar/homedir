#_______________________________________________________________________________
#
#                       Makefile for development
#_______________________________________________________________________________
#

#------------------------ CHANGE HERE ------------------------------------------

TARGET =

SRC         = subject_extract.cpp Car.cpp
TESTSRC     = test_main.cpp test_subject_extract.cpp
API_INCLUDE = subject_extract.h


#_______________________________________________________________________________
#                                                                     VARIABLES

# Includes location
INCLUDES = -I.. -I$(BOOST_ROOT)/include \
	   -I$(GLOG_ROOT)/include

# Libs location
LIBS    = -L$(BOOST_ROOT)/lib \
	  -L$(GLOG_ROOT)/lib

DEP     = $(patsubst %.cpp, bin.debug/%.d, ${SRC})
OBJ     = $(patsubst %.cpp, bin.debug/%.o, ${SRC})
TESTOBJ = $(patsubst %.cpp, bin.debug/%.o, ${TESTSRC})

CXXDEP  = g++

# Debug options
CXXFLAGS += -g3 -ggdb -ansi -fno-inline -Wall -Wextra -DDEBUG $(INCLUDES)

# All needed libraries
LDFLAGS  += -lboost_regex -lglog

#_______________________________________________________________________________
#                                                                IMPLICIT RULES

all: bin.debug/$(TARGET) unit_tests

bin.debug/$(TARGET): $(OBJ)
	@mkdir -p $(dir $@)
ifdef TARGET
	@echo -e "\n**Building ===> " $@
	$(CXX) -g -o $@ $^ $(LIBS) $(LDFLAGS)
else
	@echo -e "\nNo target for executable"
endif
ifdef API_INCLUDE
	@echo -e "\nCopy package public API in include directory"
	-cp -uv $(API_INCLUDE) include/
else
	@echo -e "\nNo public API"
endif

bin.debug/%.o: %.cpp
	@mkdir -p $(dir $@)
	@echo -e "\n*Source: " $< " ===>" $@
	$(CXX) -c $(CXXFLAGS) -MD -o $@ $<

bin.debug/%.d: %.cpp
	@mkdir -p $(dir $@)
	@echo -e "Generating dependencies for: " $<
	@set -e; $(CXXDEP) -MM -MP $(INCLUDES) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@;  \
	rm -f $@.$$$$

#_______________________________________________________________________________
#                                                                         TESTS

unit_tests: $(OBJ) $(TESTOBJ)
	@echo -e "\n**Building test ===> " $@
ifdef TESTSRC
	$(CXX) -o $@ $^ $(LIBS) $(LDFLAGS) -L$(GTEST_ROOT)/lib -lgtest
else
	@echo -e "\nNo unit tests to compile"
endif

bin.debug/%.o: test/%.cpp
	@mkdir -p $(dir $@)
	@echo -e "\n*Test: " $< " ===>" $@
	$(CXX) -c $(CXXFLAGS) -I$(GTEST_ROOT)/include -o $@ $<

tests-exists := $(wildcard unit_tests)

tester: unit_tests
ifeq ($(tests-exists), unit_tests)
	./unit_tests
else
	@echo -e "\nNo unit tests to run"
endif

#_______________________________________________________________________________
#                                                                EXPLICIT RULES


#_______________________________________________________________________________
#                                                                       FLYMAKE

.PHONY: check-syntax
check-syntax:
	$(CXX) $(CXXFLAGS) -I$(GTEST_ROOT)/include -fsyntax-only $(SRC)

#_______________________________________________________________________________
#                                                                         DEBUG

# Show variables (for debug use only)
show:
	@echo 'SOURCES       :' $(SRC)
	@echo 'DEP           :' $(DEP)
	@echo 'TESTS         :' $(TESTSRC)
	@echo 'SOURCE OBJ    :' $(OBJ)
	@echo 'TEST OBJ      :' $(TESTOBJ)
	@echo 'TARGET        :' $(TARGET)

#_______________________________________________________________________________
#                                                                         CLEAN

.PHONY: clean
clean:
	@echo -e "\nRemove debug directory and Unit test"
	-rm -rf bin.debug unit_tests

ifneq ($(MAKECMDGOALS),clean)
-include ${DEP}
endif
