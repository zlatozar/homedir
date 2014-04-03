# Including sub-directories recursively
FILE(GLOB_RECURSE project_SRCS *.cpp *.cxx *.cc *.C *.c *.h *.hpp)

#_______________________________________________________________________________
#                                                                   OUTPUT PATH

SET(EXECUTABLE_OUTPUT_PATH "${MAINFOLDER}/build")
SET(LIBRARY_OUTPUT_PATH "${MAINFOLDER}/build")

#_______________________________________________________________________________
#                                                   CONFIGURE FILES (full path)

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/AppConf.h.cmake ${CMAKE_CURRENT_SOURCE_DIR}/AppConf.h)

SET(project_BIN ${main_package_NAME})

IF(CMAKE_BUILD_TYPE STREQUAL Debug)
  SET(project_BIN ${main_package_NAME}.debug)
endif(CMAKE_BUILD_TYPE STREQUAL Debug)

#_______________________________________________________________________________
#                                                                  BUILD TARGET

INCLUDE_DIRECTORIES(${SOURCE_FOLDER})
LINK_DIRECTORIES(${SOURCE_FOLDER}/${helper_package_NAME}/lib)

ADD_EXECUTABLE(${project_BIN} ${project_SRCS})
TARGET_LINK_LIBRARIES(${project_BIN} ${project_LIBS} ${helper_package_NAME})

#_______________________________________________________________________________
#                                                                  SUB-PROJECTS

# Add directory only if there is a CMakeList.txt or CMake worns you
# ADD_SUBDIRECTORY(test)

#_______________________________________________________________________________
#                                                                  INSTALLATION

# INSTALL (TARGETS ${project_LIB}
#   ARCHIVE
#   DESTINATION lib)

INSTALL(TARGETS ${project_BIN}
  RUNTIME
  DESTINATION bin)

# INSTALL (FILES ${project_INCLUDE}
#   DESTINATION include)
