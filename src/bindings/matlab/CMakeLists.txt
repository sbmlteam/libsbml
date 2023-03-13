###############################################################################
#
# Description       : CMake build script for libSBML MATLAB bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2018 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA
#
# Copyright (C) 2002-2005 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#
###############################################################################

if(WITH_MATLAB)
SET(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" ${CMAKE_MODULE_PATH})

if (NOT MATLAB_ROOT_PATH)
  # try and find matlab in path
  find_program(MATLAB_COMMAND NAMES matlab matlab.exe)
  if (MATLAB_COMMAND)
    # if we have it in the path, it will be of form <matlab_version>/bin/matlab,
    # so take the DIRECTORY component twice
    get_filename_component(MATLAB_BIN_PATH ${MATLAB_COMMAND} DIRECTORY)
    get_filename_component(MATLAB_ROOT ${MATLAB_BIN_PATH} DIRECTORY)
    set(MATLAB_ROOT_PATH ${MATLAB_ROOT} CACHE STRING "Matlab root directory" FORCE)
  endif()
endif()

find_package(Matlab)

# by default we build matlab bindings using mxArrayToUTF8String this is only 
# available for later versions of matlab, so the following flag allows us to 
# compile for them. 
option(WITH_MATLAB_UTF8STRING
  "Generate Matlab bindings using mxArrayToUTF8String." ON )

# allow using undefined lookup for matlab libraries
if (NOT WIN32)  
  option(WITH_MATLAB_UNDEFINED_LOOKUP
    "Allow undefined lookup for matlab libraries." OFF )
endif()

#
# Determine the matlab installation directory
#
set(MATLAB_PACKAGE_INSTALL_DIR)
if (UNIX OR CYGWIN)
  set(MATLAB_PACKAGE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR})
else()
  set(MATLAB_PACKAGE_INSTALL_DIR ${MISC_PREFIX}bindings/matlab)
endif()


if (MSVC)
###############################################################################
#
# this is a directory level operation!
#
if (WITH_STATIC_RUNTIME)
  foreach(flag_var
    CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
    CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
    CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
    CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)

    if(${flag_var} MATCHES "/MD")
      string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
    endif(${flag_var} MATCHES "/MD")


  endforeach(flag_var)
  add_definitions( -D_MT)
endif(WITH_STATIC_RUNTIME)
endif()

include_directories(${MATLAB_INCLUDE_DIR})
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)
if (EXTRA_INCLUDE_DIRS)
  include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

SET(COMMON_FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/ModelDetails.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/StructureFields.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/CommonFunctions.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/Filenames.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/InputOutput.cpp"  )

foreach(matlab_source_file "TranslateSBML" "OutputSBML")

  add_library(matlab_binding_${matlab_source_file} SHARED "${CMAKE_CURRENT_SOURCE_DIR}/${matlab_source_file}.cpp" ${COMMON_FILES})
  set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES OUTPUT_NAME "${matlab_source_file}")
  set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES SUFFIX ".${MATLAB_MEX_EXT}")
  set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES  PREFIX "")

  if (WITH_MATLAB_UTF8STRING)
    target_compile_options(matlab_binding_${matlab_source_file} PRIVATE -DUSE_UTF8STRING)
  endif()

  if (MSVC)
    set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES LINK_FLAGS "/export:mexFunction")
  endif()

  if (WITH_MATLAB_UNDEFINED_LOOKUP)
    if (APPLE)
      set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES LINK_FLAGS "-undefined dynamic_lookup")
    endif()
    target_link_libraries(matlab_binding_${matlab_source_file} ${LIBSBML_LIBRARY}-static)
  else()
    target_link_libraries(matlab_binding_${matlab_source_file} ${MATLAB_LIBRARIES} ${LIBSBML_LIBRARY}-static)
  endif()
  install(TARGETS matlab_binding_${matlab_source_file} DESTINATION ${MATLAB_PACKAGE_INSTALL_DIR} )
endforeach()


# mark files for installation
file(GLOB matlab_scripts "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/*.m"
                         "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/*.xml")
install(FILES ${matlab_scripts} DESTINATION ${MATLAB_PACKAGE_INSTALL_DIR})

# add test cases
add_subdirectory(test)

endif()

