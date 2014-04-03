#_______________________________________________________________________________
#                                                         PACKAGE PREREQUISITES

FIND_PACKAGE(BISON)
FIND_PACKAGE(FLEX)

#_______________________________________________________________________________
#                                                                   OUTPUT PATH

SET(LIBRARY_OUTPUT_PATH "${SOURCE_FOLDER}/${parser_package_NAME}/lib")

#_______________________________________________________________________________
#                                                                    BISON/FLEX

MESSAGE("Generating files for a given grammar...")

# .cc to know that are generated
BISON_TARGET(myparser grammar/parser.yy ${CMAKE_CURRENT_SOURCE_DIR}/grammar/parser.tab.cc)
FLEX_TARGET(mylexer grammar/lexer.ll ${CMAKE_CURRENT_SOURCE_DIR}/grammar/lexer.cc)

# ADD_FLEX_BISON_DEPENDENCY (mylexer myparser)

SET(parser_package_LIB ${parser_package_NAME})

# generated files and related
#SET(parser_part_SRC my_parser.cpp my_parser.h)

# files around
#SET(core_part_SRC some.h some.cpp)

#_______________________________________________________________________________
#                                                                  BUILD TARGET

#ADD_LIBRARY(${parser_package_LIB} ${core_part_SRC} ${parser_part_SRC} ${BISON_myparser_OUTPUTS} ${FLEX_mylexer_OUTPUTS})
