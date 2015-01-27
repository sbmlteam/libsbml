###############################################################################
#
# Description       : CMake build script for libSBML PHP bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2014 jointly by the following organizations:
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

find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})

find_path(PHP_INCLUDE_DIR main/php.h
  /usr/include/php4
  /usr/include/php5
  /usr/local/include/php4
  /usr/local/include/php5
  /usr/include/php
  /usr/local/include/php
  /usr/local/apache/php  
)

if (WIN32)
find_library(PHP_LIBRARY
  NAMES libphp5ts.lib php5 php4 libphp
  PATHS ${LIBSBML_DEPENDENCY_DIR}/lib
        /usr/lib /usr/local/lib
        /usr/lib/i386-linux-gnu
  DOC "The file name of the PHP library."
)
endif()

find_program(PHP_EXECUTABLE NAMES php4 php5 php )

####################################################################
#
# determine local dependencies, so as to re-swig if one of them changed
# 

file(GLOB SWIG_DEPENDENCIES 
	${CMAKE_CURRENT_SOURCE_DIR}/*.i 
	${CMAKE_CURRENT_SOURCE_DIR}/*.h 
	${CMAKE_CURRENT_SOURCE_DIR}/*.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/../swig/*.i
	${CMAKE_CURRENT_SOURCE_DIR}/../swig/*.h
	)

####################################################################
#
# generate files that include all packages: 
#


# - local-packages.i
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-*.i")
list(REMOVE_ITEM DOWNCAST_EXTENSION "local-packages.i")
# - local-packages.i
update_ifile(
   "local-packages.i" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}"
)

# - local-downcast.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-packages-*.cpp")
update_cfile(
   "local-downcast.cpp" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}"
)

# - local-downcast-extension.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-extension-*.cpp")
update_cfile(
   "local-downcast-extension.cpp" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}" 
)

# - local-downcast-namespaces.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-namespaces-*.cpp")
update_cfile(
   "local-downcast-namespaces.cpp" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}"
)

# - local-downcast-plugins.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-plugins-*.cpp")
update_cfile(
   "local-downcast-plugins.cpp" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}"
)


# - local-downcast-converters.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-converters-*.cpp")
update_cfile(
   "local-downcast-converters.cpp" 
   ${CMAKE_CURRENT_BINARY_DIR} 
   "${DOWNCAST_EXTENSION}"
)

#
# Remove SWIG wrappers if requested
#
if (LIBSBML_REMOVE_WRAPPERS)
  foreach(file 
    ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
  )
    if (EXISTS ${file})
      FILE(REMOVE ${file})
    endif()
  endforeach()
endif(LIBSBML_REMOVE_WRAPPERS)

ADD_CUSTOM_COMMAND(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
    COMMAND "${SWIG_EXECUTABLE}"
    ARGS	-I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
			-I${CMAKE_CURRENT_SOURCE_DIR} 
			-I${LIBSBML_ROOT_BINARY_DIR}/src
			-I${LIBSBML_ROOT_SOURCE_DIR}/src
			-I${LIBSBML_ROOT_SOURCE_DIR}/include
			-c++
			-php		
			${SWIG_EXTRA_ARGS}		 
			-o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
			${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
	DEPENDS ${SWIG_DEPENDENCIES}
    COMMENT "Swig Php source") 

add_custom_target(binding_php_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)

	
####################################################################
#
# Build native library
#

if(WIN32)
	add_definitions(-DNT=1 -DIMPORT)
endif()
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
include_directories(${LIBSBML_ROOT_SOURCE_DIR}/include)
include_directories(${PHP_INCLUDE_DIR})
include_directories(${PHP_INCLUDE_DIR}/main)
include_directories(${PHP_INCLUDE_DIR}/Zend)
include_directories(${PHP_INCLUDE_DIR}/TSRM)
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})

if (EXTRA_INCLUDE_DIRS) 
 include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

if (WIN32)
  add_definitions(-DZEND_WIN32 -DPHP_WIN32 -DZEND_WIN32_FORCE_INLINE -DZEND_DEBUG=0 -DZTS=1 -D_USE_32BIT_TIME_T=1)
endif()

add_library(binding_php_lib SHARED ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)
add_dependencies(binding_php_lib binding_php_swig)

if (NOT UNIX)
set_target_properties (binding_php_lib PROPERTIES OUTPUT_NAME "php_libSBML")
else()
set_target_properties (binding_php_lib PROPERTIES OUTPUT_NAME "SBML")
endif()
target_link_libraries(binding_php_lib ${LIBSBML_LIBRARY}-static)
if (WIN32)
  target_link_libraries(binding_php_lib ${PHP_LIBRARY})
endif()
INSTALL(TARGETS binding_php_lib DESTINATION ${CMAKE_INSTALL_LIBDIR}/php )

