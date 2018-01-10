###############################################################################
#
# Description       : CMake build script for libSBML JS bindings
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

find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})
find_package(PythonInterp)

if (NOT NODE_GYP_EXECUTABLE)
  message(FATAL_ERROR 
  "
       node-gyp is required to build the libSBML JS bindings. 
	   Please set the NODE_GYP_EXECUTABLE variable to the 
	   full location of node-gyp.
  "
  )
endif()

if (NOT NODEJS_EXECUTABLE)
  message(FATAL_ERROR 
  "
       nodejs is required to build the libSBML JS bindings. 
	   Please set the NODEJS_EXECUTABLE variable to the 
	   full location of the nodejs interpreter.
  "
  )
endif()


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

# - local-downcast-plugins.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-astplugins-*.cpp")
update_cfile(
   "local-downcast-astplugins.cpp" 
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

SET(NODE_JS_DEFINES)

# construct the define statement in binding.gyp
# from current settings
if (UNIX)
  SET(NODE_JS_DEFINES "'LINUX', ")
  if (APPLE)
    SET(NODE_JS_DEFINES "${NODE_JS_DEFINES} 'MACOSX', ")
  endif()
else()
  SET(NODE_JS_DEFINES "'WIN32', 'LIBSBML_EXPORTS', 'LIBLAX_EXPORTS', ")
endif()

foreach(var ${SWIG_EXTRA_ARGS})
	string(REPLACE "-D" "" current ${var})
	string(FIND "${NODE_JS_DEFINES}" "${current}" pos)
	if (${pos} EQUAL -1)
	set (NODE_JS_DEFINES "${NODE_JS_DEFINES} '${current}', ")
	endif()
endforeach()

# construct the library statement in binding.gyp
# from current settings

SET(NODE_JS_LIBS)

if (WITH_ZLIB AND EXISTS ${LIBZ_LIBRARY})
	SET(NODE_JS_LIBS "${NODE_JS_LIBS}, '${LIBZ_LIBRARY}'")
endif()

if (WITH_BZIP2 AND EXISTS ${LIBBZ_LIBRARY})
	SET(NODE_JS_LIBS "${NODE_JS_LIBS}, '${LIBBZ_LIBRARY}'")
endif()

foreach (var ${LIBSBML_XML_LIBRARY_LIBS})
  SET(NODE_JS_LIBS "${NODE_JS_LIBS}, '${var}'")
endforeach()

foreach (var ${EXTRA_LIBS})
  SET(NODE_JS_LIBS "${NODE_JS_LIBS}, '${var}'")
endforeach()

# generate binding.gyp
configure_file(
   ${CMAKE_CURRENT_SOURCE_DIR}/binding.gyp.in 
   ${CMAKE_CURRENT_BINARY_DIR}/binding.gyp.in
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


# determine V8 version used by nodejs 
execute_process(
  COMMAND "${NODEJS_EXECUTABLE}" -e "console.log(process.versions.v8)"
  OUTPUT_VARIABLE NODEJS_V8_STRING_VERSION
)

if (NOT NODEJS_V8_STRING_VERSION)
  message(FATAL_ERROR "Could not determine Nodejs V8 version. Please verify that the NODEJS_EXECUTABLE has been set.")
endif()

string(REPLACE "." ";" NODEJS_V8_STRING_VERSION "${NODEJS_V8_STRING_VERSION}" )
string(REPLACE "-" ";" NODEJS_V8_STRING_VERSION "${NODEJS_V8_STRING_VERSION}" )
list(LENGTH NODEJS_V8_STRING_VERSION versionLength)
list(GET NODEJS_V8_STRING_VERSION 0 V8_VERSION_MAJOR )
list(GET NODEJS_V8_STRING_VERSION 1 V8_VERSION_MINOR )
list(GET NODEJS_V8_STRING_VERSION 2 V8_VERSION_PATCH )

MATH(EXPR V8_VERSION_NUMERIC "${V8_VERSION_MAJOR} * 10000 + ${V8_VERSION_MINOR} * 100 + ${V8_VERSION_PATCH}" )

add_custom_command(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
    COMMAND "${SWIG_EXECUTABLE}"
    ARGS	-I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
			    -I${CMAKE_CURRENT_SOURCE_DIR} 
			    -I${LIBSBML_ROOT_BINARY_DIR}/src
			    -I${LIBSBML_ROOT_SOURCE_DIR}/src
			    -I${LIBSBML_ROOT_SOURCE_DIR}/include
			    -c++
			    -javascript -node -DV8_VERSION=0x0${V8_VERSION_NUMERIC}
			    -module sbml
			    ${SWIG_EXTRA_ARGS}		 
			    -o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
			    -xmlout ${CMAKE_CURRENT_BINARY_DIR}/tree.xml
			    ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
  MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
	DEPENDS ${SWIG_DEPENDENCIES} ${LIBSBML_HEADER_FILES}
  COMMENT "Swig JS source")

add_custom_target(binding_js_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp )

set(NODE_GYP_ARGS)

# need extra variable on 32 bit build
if (CMAKE_SIZEOF_VOID_P EQUAL 4)
set (NODE_GYP_ARGS "--arch=ia32")
endif()

ADD_CUSTOM_COMMAND(	
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/Release/sbml.node
	COMMAND "${CMAKE_COMMAND}"
	ARGS -DNODE_GYP_EXECUTABLE=\"${NODE_GYP_EXECUTABLE}\"
	     -DNODEJS_EXECUTABLE=\"${NODEJS_EXECUTABLE}\"
		   -DPYTHON_EXECUTABLE=\"${PYTHON_EXECUTABLE}\"
		   -DNODE_GYP_ARGS=\"${NODE_GYP_ARGS}\"
		   -DBIN_DIRECTORY=\"${CMAKE_CURRENT_BINARY_DIR}\"
		   -DSRC_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}\"
		   -DLIBSBML_LIBRARY=\"$<TARGET_FILE:${LIBSBML_LIBRARY}-static>\"
		   -P "${CMAKE_CURRENT_SOURCE_DIR}/compile-native-files.cmake"
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAIN_DEPENDENCY ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
	DEPENDS ${SWIG_DEPENDENCIES}
  COMMENT "Build sbml.node")

add_custom_target(binding_js_node ALL DEPENDS 
  ${CMAKE_CURRENT_BINARY_DIR}/build/Release/sbml.node
  ${LIBSBML_LIBRARY}-static)

# determine install location
set(NODEJS_PACKAGE_INSTALL_DIR)
if (UNIX OR CYGWIN)
  set(NODEJS_PACKAGE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/node)
else()
  set(NODEJS_PACKAGE_INSTALL_DIR ${MISC_PREFIX}bindings/nodejs)
endif()

# install file 
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/build/Release/sbml.node DESTINATION ${NODEJS_PACKAGE_INSTALL_DIR})
