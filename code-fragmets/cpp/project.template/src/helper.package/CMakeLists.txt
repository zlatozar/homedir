#_______________________________________________________________________________
#                                                                   OUTPUT PATH

SET(EXECUTABLE_OUTPUT_PATH "${SOURCE_FOLDER}/${helper_package_NAME}/bin")
SET(LIBRARY_OUTPUT_PATH "${SOURCE_FOLDER}/${helper_package_NAME}/lib")

#_______________________________________________________________________________
#                                                   CONFIGURE FILES (full path)


# Package Search Paths
LIST(APPEND CMAKE_PREFIX_PATH "${SOURCE_FOLDER}/${helper_package_NAME}/thirdparty")

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/PkgConf.h.cmake ${CMAKE_CURRENT_SOURCE_DIR}/PkgConf.h)

#_______________________________________________________________________________
#                                                                  BUILD TARGET

SET(helper_package_LIB ${helper_package_NAME})
# SET (helper_package_BIN ${helper_package_NAME})

# library for "internal" use
ADD_LIBRARY(${helper_package_LIB} SHARED subject_extract.cpp subject_extract.h Car.cpp Car.h)
TARGET_LINK_LIBRARIES(${helper_package_LIB} ${project_LIBS})

# use it to version libraries e.g. 1.0.1 API is compatible with 1.0
# SET_TARGET_PROPERTIES(${helper_package_LIB} PROPERTIES VERSION 1.0.1 SOVERSION 1)

# or executable
# ADD_EXECUTABLE (${helper_package_BIN} ${helper_package_SRCS} ${project_MOC_SRCS_GENERATED})

#_______________________________________________________________________________
#                                                                  SUB-PROJECTS

# Add directory only if there is a CMakeList.txt or CMake worns you
# ADD_SUBDIRECTORY(sub-package)
ADD_SUBDIRECTORY(test)

#_______________________________________________________________________________
#                                                                  INSTALLATION

EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy 
  # all API header files
  ${CMAKE_CURRENT_SOURCE_DIR}/subject_extract.h
  # destination
  ${CMAKE_CURRENT_SOURCE_DIR}/include)
