# Including sub-directories recursive
FILE(GLOB_RECURSE helper_test_SRCS *.cpp *.cxx *.cc *.C *.c *.h *.hpp)

# All test executables ends with "-test"
SET(helper_test_BIN ${helper_package_NAME}-test)

#_______________________________________________________________________________
#                                                                  BUILD TARGET

INCLUDE_DIRECTORIES(${SOURCE_FOLDER})
LINK_DIRECTORIES(${SOURCE_FOLDER}/${helper_package_NAME}/lib)

ADD_EXECUTABLE(${helper_test_BIN} ${helper_test_SRCS})
TARGET_LINK_LIBRARIES(${helper_test_BIN} ${project_LIBS} ${GTEST_LIBRARIES} ${helper_package_NAME})
