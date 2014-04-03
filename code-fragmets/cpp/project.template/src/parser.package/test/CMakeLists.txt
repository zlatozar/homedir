FILE (GLOB_RECURSE parser_test_SRCS *.cpp *.cxx *.cc *.C *.c *.h *.hpp)

SET (parser_test_BIN ${parser_package_NAME}-test)

# what to test
INCLUDE_DIRECTORIES (${PROJECT_SOURCE_DIR}/${parser_package_NAME})
LINK_DIRECTORIES (${PROJECT_SOURCE_DIR}/${parser_package_NAME}/lib)

ADD_EXECUTABLE(${parser_test_BIN} ${parser_test_SRCS})
TARGET_LINK_LIBRARIES(${parser_test_BIN} ${project_LIBS} ${GTEST_LIBRARIES} ${parser_package_NAME})
