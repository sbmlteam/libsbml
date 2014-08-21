###############################################################################
#
# Description       : CMake build script for libSBML Ruby bindings
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


# generate binding.gyp
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/binding.gyp.in ${CMAKE_CURRENT_BINARY_DIR}/binding.gyp)

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
			-javascript -node
      -module sbml
			${SWIG_EXTRA_ARGS}		 
			-o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
      -xmlout ${CMAKE_CURRENT_BINARY_DIR}/tree.xml
			${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
	DEPENDS ${SWIG_DEPENDENCIES}
    COMMENT "Swig JS source")

add_custom_target(binding_js_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)


ADD_CUSTOM_COMMAND(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/Release/sbml.node
    COMMAND "${NODE_GYP_EXECUTABLE}"
    ARGS    rebuild
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
	DEPENDS ${SWIG_DEPENDENCIES}
    COMMENT "Build sbml.node")

add_custom_target(binding_js_node ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/build/Release/sbml.node)

