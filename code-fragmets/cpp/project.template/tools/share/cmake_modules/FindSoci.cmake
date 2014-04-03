#_______________________________________________________________________________
#
#   This CMake package creates a Soci target
#_______________________________________________________________________________
#

#  SOCI_FOUND - system has Soci
#  SOCI_INCLUDE_DIRS - the Soci include directory
#  SOCI_LIBRARIES - Link these to use Soci
#  SOCI_DEFINITIONS - Compiler switches required for using Soci

IF(SOCI_LIBRARIES AND SOCI_INCLUDE_DIRS)
  SET(SOCI_FOUND TRUE)
ELSE(SOCI_LIBRARIES AND SOCI_INCLUDE_DIRS)
  FIND_PATH(SOCI_INCLUDE_DIR
    HINTS
      $ENV{SOCI_ROOT}/include
      ${SOCI_ROOT}/include
    NAMES
      soci/soci.h
    PATHS
      /usr/include
      /usr/local/include
      /opt/include/soci/include
    PATH_SUFFIXES
      soci
  )

  FIND_LIBRARY(SOCI_LIBRARY
    HINTS
      $ENV{SOCI_ROOT}/lib
      ${SOCI_ROOT}/lib
    NAMES
      soci_core-gcc-3_0
    PATHS
      /usr/lib
      /usr/local/lib
      /opt/include/soci/lib
  )

SET(SOCI_INCLUDE_DIRS ${SOCI_INCLUDE_DIR})
SET(SOCI_LIBRARIES ${SOCI_LIBRARY})

IF(SOCI_INCLUDE_DIRS AND SOCI_LIBRARIES)
  SET(SOCI_FOUND TRUE)
ENDIF(SOCI_INCLUDE_DIRS AND SOCI_LIBRARIES)

IF(SOCI_FOUND)
  IF(NOT Soci_FIND_QUIETLY)
    MESSAGE(STATUS "Found Soci: ${SOCI_LIBRARIES}")
  ENDIF(NOT Soci_FIND_QUIETLY)
ELSE(SOCI_FOUND)
  IF(Soci_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR "Could not find Soci")
  ENDIF (Soci_FIND_REQUIRED)
ENDIF(SOCI_FOUND)
  
# show the SOCI_INCLUDE_DIRS and SOCI_LIBRARIES variables only in the advanced view
MARK_AS_ADVANCED(SOCI_INCLUDE_DIRS SOCI_LIBRARIES)
  
ENDIF(SOCI_LIBRARIES AND SOCI_INCLUDE_DIRS)
