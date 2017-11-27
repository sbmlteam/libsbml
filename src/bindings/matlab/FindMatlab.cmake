# - this module looks for Matlab
# Defines:
#  MATLAB_INCLUDE_DIR: include path for mex.h, engine.h
#  MATLAB_LIBRARIES:   required libraries: libmex, etc
#  MATLAB_MEX_LIBRARY: path to libmex.lib
#  MATLAB_MX_LIBRARY:  path to libmx.lib
#  MATLAB_ENG_LIBRARY: path to libeng.lib

# This file is based on the one coming with the CMAKE distro, however it needed adapting!
# I added a new variable: 
#
# MATLAB_ROOT_PATH which is the path to the Matlab Directory it can also be specified by the users!
#

#=============================================================================
# Copyright 2005-2009 Kitware, Inc.
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distributed this file outside of CMake, substitute the full
#  License text for the above reference.)

SET(MATLAB_FOUND 0)
SET(MATLAB_MEXOPTS_FILE)
SET(MATLAB_ROOT_PATH "" CACHE PATH
    "Path to the matlab root path")
SET(MATLAB_MEX_EXT "" CACHE STRING "mex extension to use")
SET(MATLAB_MEXEXT "" CACHE FILEPATH "Path to mexext executable")
SET(MATLAB_MEX_COMMAND "" CACHE FILEPATH "Path to mex compiler")
SET(MATLAB_MATLAB_COMMAND "" CACHE FILEPATH "Path to matlab command")

if (NOT MATLAB_ROOT_PATH OR NOT EXISTS "${MATLAB_ROOT_PATH}")
  if(UNIX)
    if (APPLE)
    set (FOUND OFF)
    foreach(dir 
      "/Applications/MATLAB_R2017b.app/"
      "/Applications/MATLAB_R2017a.app/"
      "/Applications/MATLAB_R2016b.app/"
      "/Applications/MATLAB_R2016a.app/"
      "/Applications/MATLAB_R2015b.app/"
      "/Applications/MATLAB_R2015a.app/"
      "/Applications/MATLAB_R2014b.app/"
      "/Applications/MATLAB_R2014a.app/"
      "/Applications/MATLAB_R2013b.app/"
      "/Applications/MATLAB_R2013a.app/"
      "/Applications/MATLAB_R2012b.app/"
      "/Applications/MATLAB_R2012a.app/"
      "/Applications/MATLAB_R2011b.app/"
      "/Applications/MATLAB_R2011a.app/"
      "/Applications/MATLAB_R2010b.app/"
      "/Applications/MATLAB_R2010a.app/"
      "/Applications/MATLAB_R2009b.app/"
      "/Applications/MATLAB_R2009a.app/"
      "/Applications/MATLAB_R2008b.app/"
      "/Applications/MATLAB_R2008a.app/"
    )
      if (EXISTS "${dir}" AND NOT ${FOUND})
         set(MATLAB_ROOT_PATH ${dir} CACHE PATH
    "Path to the matlab root path" FORCE)
         set(FOUND ON)
      endif()
    endforeach()
    else()
      if (EXISTS "/opt/matlab/")
        set(MATLAB_ROOT_PATH "/opt/matlab/" CACHE PATH
    "Path to the matlab root path" FORCE)
      endif()
    endif()
  else()
    if (CMAKE_SIZEOF_VOID_P EQUAL 4)
    # search 32 bit
    set (FOUND OFF)
    foreach(dir 
      "MATLAB/R2017b"
      "MATLAB/R2017a"
      "MATLAB/R2016b"
      "MATLAB/R2016a"
      "MATLAB/R2015b"
      "MATLAB/R2015a"
      "MATLAB/R2014b"
      "MATLAB/R2014a"
      "MATLAB/R2013b"
      "MATLAB/R2013a"
      "MATLAB/R2012b"
      "MATLAB/R2012a"
      "MATLAB/R2011b"
      "MATLAB/R2011a"
      "MATLAB/R2010b"
      "MATLAB/R2010a"
      "MATLAB/R2009b"
      "MATLAB/R2009a"
      "MATLAB/R2008b"
      "MATLAB/R2008a"
    )
      if(EXISTS "$ENV{ProgramW6432}/${dir}" AND NOT ${FOUND})
         set(MATLAB_ROOT_PATH $ENV{ProgramW6432}/${dir} CACHE PATH
    "Path to the matlab root path" FORCE)
         set(FOUND ON)
      endif()
      if (EXISTS "$ENV{PROGRAMFILES}/${dir}" AND NOT ${FOUND})
         set(MATLAB_ROOT_PATH $ENV{PROGRAMFILES}/${dir} CACHE PATH
    "Path to the matlab root path" FORCE)
         set(FOUND ON)
      endif()
    endforeach()
    
    else()
    
    # search 64 bit
    set (FOUND OFF)
    foreach(dir 
      "MATLAB/R2017b"
      "MATLAB/R2017a"
      "MATLAB/R2016b"
      "MATLAB/R2016a"
      "MATLAB/R2015b"
      "MATLAB/R2015a"
      "MATLAB/R2014b"
      "MATLAB/R2014a"
      "MATLAB/R2013b"
      "MATLAB/R2013a"
      "MATLAB/R2012b"
      "MATLAB/R2012a"
      "MATLAB/R2011b"
      "MATLAB/R2011a"
      "MATLAB/R2010b"
      "MATLAB/R2010a"
      "MATLAB/R2009b"
      "MATLAB/R2009a"
      "MATLAB/R2008b"
      "MATLAB/R2008a"
    )
      if (EXISTS "$ENV{PROGRAMFILES}/${dir}" AND NOT ${FOUND})
         set(MATLAB_ROOT_PATH $ENV{PROGRAMFILES}/${dir} CACHE PATH
    "Path to the matlab root path" FORCE)
         set(FOUND ON)
      endif()
    endforeach()
    endif()
  endif()
endif() 

if (NOT EXISTS "${MATLAB_ROOT_PATH}")
  message(FATAL_ERROR "The Matlab installation could not be found, please 
    specify the MATLAB_ROOT_PATH. Currently:
    
    MATLAB_ROOT_PATH = ${MATLAB_ROOT_PATH}
    ")
else()
  set(MATLAB_ROOT_PATH "${MATLAB_ROOT_PATH}" CACHE PATH "Matlab directory")
endif()

IF(WIN32)
  IF(${CMAKE_GENERATOR} MATCHES "Visual Studio 6")
    SET(MATLAB_ROOT "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MathWorks\\MATLAB\\7.0;MATLABROOT]/extern/lib/win32/microsoft/msvc60")
  if(NOT EXISTS "${MATLAB_ROOT}")
    SET(MATLAB_ROOT "${MATLAB_ROOT_PATH}/extern/lib/win32/microsoft/msvc60")
  endif()
  ELSE(${CMAKE_GENERATOR} MATCHES "Visual Studio 6")
    IF(${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
      # Assume people are generally using 7.1,
      # if using 7.0 need to link to: ../extern/lib/win32/microsoft/msvc70
      SET(MATLAB_ROOT "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MathWorks\\MATLAB\\7.0;MATLABROOT]/extern/lib/win32/microsoft/msvc71")
    if(NOT EXISTS "${MATLAB_ROOT}")
    SET(MATLAB_ROOT "${MATLAB_ROOT_PATH}/extern/lib/win32/microsoft/msvc71")
    endif()
    ELSE(${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
      IF(${CMAKE_GENERATOR} MATCHES "Borland")
        # Same here, there are also: bcc50 and bcc51 directories
        SET(MATLAB_ROOT "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MathWorks\\MATLAB\\7.0;MATLABROOT]/extern/lib/win32/microsoft/bcc54")
    if(NOT EXISTS "${MATLAB_ROOT}")
      SET(MATLAB_ROOT "${MATLAB_ROOT_PATH}/extern/lib/win32/microsoft/bcc54")
    endif()
      ELSE(${CMAKE_GENERATOR} MATCHES "Borland")
        IF(MATLAB_FIND_REQUIRED)
          MESSAGE(FATAL_ERROR "Generator not compatible: ${CMAKE_GENERATOR}")
        ENDIF(MATLAB_FIND_REQUIRED)
      ENDIF(${CMAKE_GENERATOR} MATCHES "Borland")
    ENDIF(${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
  ENDIF(${CMAKE_GENERATOR} MATCHES "Visual Studio 6")
  
  # unfortunately this won't find matlab R2010b on my machine
  if (NOT "${MATLAB_ROOT}")
    if (NOT MATLAB_MEX_COMMAND)
    SET(MATLAB_MEX_COMMAND "${MATLAB_ROOT_PATH}/bin/mex.bat" CACHE FILEPATH "Path to mex compiler" FORCE)
    endif()
    
    if (CMAKE_SIZEOF_VOID_P EQUAL 4)
      SET(MATLAB_ROOT "${MATLAB_ROOT_PATH}/extern/lib/win32/microsoft/")
    else()
      SET(MATLAB_ROOT "${MATLAB_ROOT_PATH}/extern/lib/win64/microsoft/")
    endif()
  endif()
  if (NOT MATLAB_MEXEXT)
  SET(MATLAB_MEXEXT "${MATLAB_ROOT_PATH}/bin/mexext.bat" CACHE FILEPATH "Path to mexext executable" FORCE)
  endif()
  
  # MATLAB 2017 does not have matlab.bat but matlab.exe
  if (NOT MATLAB_MATLAB_COMMAND)
  if (EXISTS "${MATLAB_ROOT_PATH}/bin/matlab.bat")
      SET(MATLAB_MATLAB_COMMAND "${MATLAB_ROOT_PATH}/bin/matlab.bat" CACHE FILEPATH "Path to matlab command" FORCE)
  else()
      SET(MATLAB_MATLAB_COMMAND "${MATLAB_ROOT_PATH}/bin/matlab.exe" CACHE FILEPATH "Path to matlab command" FORCE)
  endif()
  endif()
ELSE (WIN32)
  if (NOT MATLAB_MEX_COMMAND)
  SET(MATLAB_MEX_COMMAND "${MATLAB_ROOT_PATH}/bin/mex"  CACHE FILEPATH "Path to mex compiler" FORCE)
  endif()
  
  if (NOT MATLAB_MEXEXT)
  SET(MATLAB_MEXEXT ${MATLAB_ROOT_PATH}/bin/mexext CACHE FILEPATH "Path to mexext executable" FORCE)
  endif()
  
  if (NOT MATLAB_MATLAB_COMMAND)
  SET(MATLAB_MATLAB_COMMAND "${MATLAB_ROOT_PATH}/bin/matlab" CACHE FILEPATH "Path to matlab command" FORCE)
  endif()
  
  if(APPLE)
    SET(MATLAB_ROOT 
      ${MATLAB_ROOT_PATH}/extern/lib/maci64/
      ${MATLAB_ROOT_PATH}/bin/maci64/
    )
  else()

    IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
    # Regular x86
    SET(MATLAB_ROOT
    /usr/local/matlab-7sp1/bin/glnx86/
    /opt/matlab-7sp1/bin/glnx86/
    $ENV{HOME}/matlab-7sp1/bin/glnx86/
    $ENV{HOME}/redhat-matlab/bin/glnx86/
    ${MATLAB_ROOT_PATH}/bin/glnx86/
    )
    ELSE(CMAKE_SIZEOF_VOID_P EQUAL 4)
    # AMD64:
    SET(MATLAB_ROOT
    /usr/local/matlab-7sp1/bin/glnxa64/
    /opt/matlab-7sp1/bin/glnxa64/
    $ENV{HOME}/matlab7_64/bin/glnxa64/
    $ENV{HOME}/matlab-7sp1/bin/glnxa64/
    $ENV{HOME}/redhat-matlab/bin/glnxa64/
    ${MATLAB_ROOT_PATH}/bin/glnxa64/
    )
    ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)
  endif()
ENDIF(WIN32)
    
  FIND_LIBRARY(MATLAB_MEX_LIBRARY
    NAMES libmex mex  libmex.dylib
    PATHS
      ${MATLAB_ROOT}
      ${MATLAB_ROOT_PATH}/bin/maci64/
    NO_DEFAULT_PATH
    )
  if (NOT MATLAB_MEX_LIBRARY)
    FIND_LIBRARY(MATLAB_MEX_LIBRARY
      NAMES libmex mex  libmex.dylib
      PATHS
        ${MATLAB_ROOT}
        ${MATLAB_ROOT_PATH}/bin/maci64/
      )
  endif()
  FIND_LIBRARY(MATLAB_MX_LIBRARY
    NAMES libmx mx  libmx.dylib
    PATHS
      ${MATLAB_ROOT}
      ${MATLAB_ROOT_PATH}/bin/maci64/
    NO_DEFAULT_PATH
    )
  if (NOT MATLAB_MX_LIBRARY)
    FIND_LIBRARY(MATLAB_MX_LIBRARY
      NAMES libmx mx  libmx.dylib
      PATHS
        ${MATLAB_ROOT}
        ${MATLAB_ROOT_PATH}/bin/maci64/
      )
  endif()

    FIND_LIBRARY(MATLAB_ENG_LIBRARY
    NAMES libeng eng libeng.dylib
    PATHS
      ${MATLAB_ROOT}
      ${MATLAB_ROOT_PATH}/bin/maci64/
      NO_DEFAULT_PATH
    )

    if (NOT MATLAB_ENG_LIBRARY)
    FIND_LIBRARY(MATLAB_ENG_LIBRARY
    NAMES libeng eng libeng.dylib
    PATHS
      ${MATLAB_ROOT}
      ${MATLAB_ROOT_PATH}/bin/maci64/
    )
    endif()

  FIND_PATH(MATLAB_INCLUDE_DIR
    "mex.h"
    PATHS
    "${MATLAB_ROOT_PATH}/extern/include"
    "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MathWorks\\MATLAB\\7.0;MATLABROOT]/extern/include"
    "C:/Program Files (x86)/MATLAB/R2010b/extern/include"
    "C:/Program Files/MATLAB/R2010b/extern/include"
    "/Applications/MATLAB_R2010b.app/extern/include"
    "/usr/local/matlab-7sp1/extern/include/"
    "/opt/matlab-7sp1/extern/include/"
    "$ENV{HOME}/matlab-7sp1/extern/include/"
    "$ENV{HOME}/redhat-matlab/extern/include/"
    )

# This is common to UNIX and Win32:
SET(MATLAB_LIBRARIES
  ${MATLAB_MEX_LIBRARY}
  ${MATLAB_MX_LIBRARY}
  ${MATLAB_ENG_LIBRARY}
)

IF(MATLAB_INCLUDE_DIR AND MATLAB_LIBRARIES)
  SET(MATLAB_FOUND 1)
ENDIF(MATLAB_INCLUDE_DIR AND MATLAB_LIBRARIES)

if (NOT MATLAB_MEX_EXT)
execute_process(COMMAND ${MATLAB_MEXEXT} OUTPUT_VARIABLE MATLAB_MEX_EXT)
STRING(STRIP "${MATLAB_MEX_EXT}" MATLAB_MEX_EXT)
SET(MATLAB_MEX_EXT "${MATLAB_MEX_EXT}" CACHE STRING "mex extension to use" FORCE)
endif()

MARK_AS_ADVANCED(
  MATLAB_LIBRARIES
  MATLAB_MEX_LIBRARY
  MATLAB_MX_LIBRARY
  MATLAB_ENG_LIBRARY
  MATLAB_INCLUDE_DIR
  MATLAB_FOUND
  MATLAB_MATLAB_COMMAND
  MATLAB_ROOT
  MATLAB_ROOT_PATH
)

# handle the QUIETLY and REQUIRED arguments and set LIBSBML_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Matlab DEFAULT_MSG MATLAB_LIBRARIES
  MATLAB_MEX_LIBRARY
  MATLAB_MX_LIBRARY
  MATLAB_ENG_LIBRARY
  MATLAB_INCLUDE_DIR
  MATLAB_FOUND
  MATLAB_MATLAB_COMMAND
  MATLAB_ROOT
  MATLAB_ROOT_PATH)


