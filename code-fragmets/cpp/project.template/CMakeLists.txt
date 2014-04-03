#_______________________________________________________________________________
#                                                            PROJECT PROPERTIES

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.0)

PROJECT(project.name)

SET(APPLICATION_NAME "Project Name")
SET(APPLICATION_CODENAME "${PROJECT_NAME}")
SET(APPLICATION_COPYRIGHT_YEARS "2010")

# Versioning
SET(APPLICATION_VERSION_MAJOR 1)
SET(APPLICATION_VERSION_MINOR 0)
SET(APPLICATION_VERSION_PATCH 0)
SET(APPLICATION_VERSION_TYPE SNAPSHOT)

SET(APPLICATION_VERSION_STRING 
   "${APPLICATION_VERSION_MAJOR}.${APPLICATION_VERSION_MINOR}.${APPLICATION_VERSION_PATCH}-${APPLICATION_VERSION_TYPE}")

SET(APPLICATION_VENDOR_ID "com.yourcompany")
SET(APPLICATION_VENDOR_NAME "Your Company")
SET(APPLICATION_VENDOR_URL "yourcompany.com")
SET(APPLICATION_ID "${APPLICATION_VENDOR_ID}.${PROJECT_NAME}")

# Build Type
SET(CMAKE_VERBOSE_MAKEFILE 0) # Use 1 for debugging, 0 for release
SET(CMAKE_BUILD_TYPE Release) # Release or Debug
SET(CMAKE_DEBUG_POSTFIX _d)   # For libraries

#_______________________________________________________________________________
#                                                                   OUTPUT PATH

SET(MAINFOLDER ${PROJECT_SOURCE_DIR})
SET(EXECUTABLE_OUTPUT_PATH "${MAINFOLDER}/build")
SET(SOURCE_FOLDER "${MAINFOLDER}/src")

# Project Search Paths
LIST(APPEND CMAKE_PREFIX_PATH "${MAINFOLDER}")


# How CMake finds libraries
SET(CMAKE_MODULE_PATH "${MAINFOLDER}/tools/share/cmake_modules")

#_______________________________________________________________________________
#                                                         PROJECT PREREQUISITES

# Boost
SET(BOOST_ROOT ${SOURCE_FOLDER}/extern/boost)
# if you have problems with finding particular boost version
#SET(Boost_DEBUG t)
SET(Boost_ADDITIONAL_VERSIONS "1.42" "1.42.0" "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0")

# add libs that you need
FIND_PACKAGE(Boost 1.43 COMPONENTS "date_time" "filesystem" "graph" "program_options" 
                                   "regex" "serialization" "signals" "system" "thread"
                                   "wserialization" REQUIRED)
# add all boost includes
INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})

# Qt (whith qmake in PATH)
SET(QT_ROOT ${SOURCE_FOLDER}/extern/Qt)
FIND_PACKAGE(Qt4 REQUIRED)
INCLUDE(UseQt4)

# gtest
ENABLE_TESTING(TRUE)
SET(GTEST_ROOT ${SOURCE_FOLDER}/extern/gtest)
FIND_PACKAGE(GTest REQUIRED)
INCLUDE_DIRECTORIES(${GTEST_INCLUDE_DIRS})

#_______________________________________________________________________________
#                                                    NEEDS CUSTOM CMAKE MODULES

# glog
SET(GLOG_ROOT ${SOURCE_FOLDER}/extern/glog)
FIND_PACKAGE(Glog REQUIRED)
INCLUDE_DIRECTORIES(${GLOG_INCLUDE_DIRS})

# SOCI (optional)
# SET(SOCI_ROOT ${SOURCE_FOLDER}/extern/soci)
# FIND_PACKAGE(Soci REQUIRED)
# INCLUDE_DIRECTORIES (${SOCI_INCLUDE_DIRS})

#_______________________________________________________________________________
#                                                                     G++ FLAGS

SET(CMAKE_CXX_FLAGS "-Wall -Wextra -Winit-self -Woverloaded-virtual -Wnon-virtual-dtor -Wsign-promo -Wpointer-arith")
SET(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG -DQT_NO_DEBUG")
SET(CMAKE_CXX_FLAGS_DEBUG   "-g3 -ggdb3 -fno-inline")

# status maessages
MESSAGE(STATUS "Project main folder is: " ${MAINFOLDER})
MESSAGE(STATUS "Project src folder is: " ${SOURCE_FOLDER})
MESSAGE(STATUS "CMAKE_BUILD_TYPE: " ${CMAKE_BUILD_TYPE})
MESSAGE(STATUS "LD_LIBRARY_PATH: " $ENV{LD_LIBRARY_PATH})
MESSAGE(STATUS "PATH: " $ENV{PATH})


SET(CMAKE_INCLUDE_CURRENT_DIR ON)
SET(CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)
SET(CMAKE_COLOR_MAKEFILE ON)

#_______________________________________________________________________________
#
# PROJECT SPECIFIC
#_______________________________________________________________________________
#                                                                  


#_______________________________________________________________________________
#                                                                  BUILD TARGET

INCLUDE_DIRECTORIES(${SOURCE_FOLDER})

# libraries used in almost all packages
SET(project_LIBS ${Boost_LIBRARIES} ${QT_LIBRARIES} ${GLOG_LIBRARIES})

# generate parsers
SET(parser_package_NAME "parser.package")
ADD_SUBDIRECTORY(src/parser.package)

# helpers packages first
SET(helper_package_NAME "helper.package")
ADD_SUBDIRECTORY(src/helper.package)

# builds the whole project
SET(main_package_NAME "project.template")
ADD_SUBDIRECTORY(src/main.package)

#_______________________________________________________________________________
#                                                                 DOCUMENTATION

INCLUDE(DocumentationTargets)

#_______________________________________________________________________________
#                                                                          TEST

ENABLE_TESTING()
ADD_TEST(${helper_package_NAME} 
         ${SOURCE_FOLDER}/${helper_package_NAME}/bin/${helper_package_NAME}-test 
         ${helper_package_NAME})

#_______________________________________________________________________________
#                                                                  INSTALLATION

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Project description")
SET(CPACK_PACKAGE_VENDOR ${APPLICATION_VENDOR_NAME})

SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_SOURCE_DIR}/COPYING)

SET(CPACK_PACKAGE_VERSION_MAJOR ${APPLICATION_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${APPLICATION_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${APPLICATION_VERSION_PATCH})

# do not include those
SET(COMMON_IGNORE_FILES ".directory$" "CMakeCache.txt" "/.svn/"  "/CVS/" "~$" ".log$" ".gz$")
#SET(PRJ_COMMON_IGNORE_FILES ${COMMON_IGNORE_FILES} <PROJECT_SPECIFIC_IGNORE_FILES_PATTERN> )

SET(CPACK_SOURCE_IGNORE_FILES ${COMMON_IGNORE_FILES} "/bin/" "/extern/" "/build/")
#SET(CPACK_PACKAGE_IGNORE_FILES ${PRJ_COMMON_IGNORE_FILES} <FILES_PATTERN_THAT_YOU_DONT_WANT_TO_PUT_IN_BINARY_TARBALL> )

SET(CPACK_GENERATOR "TGZ")
SET(CPACK_SOURCE_GENERATOR "TGZ")

INCLUDE(CPack)
