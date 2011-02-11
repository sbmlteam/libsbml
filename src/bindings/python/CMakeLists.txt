###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for libSBML Python bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2009-2011 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
include(FindPythonLibs)
include(FindPythonInterp)

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


ADD_CUSTOM_COMMAND(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
	COMMAND "${PYTHON_EXECUTABLE}"
	ARGS	"${CMAKE_CURRENT_SOURCE_DIR}/../swig/swigdoc.py"
			python
			-I"${CMAKE_CURRENT_SOURCE_DIR}/../.."
			-D"${CMAKE_CURRENT_SOURCE_DIR}/../../../docs/src"
			"${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbml.i"
			"${CMAKE_CURRENT_BINARY_DIR}/pydoc.i"
    COMMAND "${SWIG_EXECUTABLE}"
    ARGS	-I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
			-I${CMAKE_CURRENT_SOURCE_DIR} 
			-I${CMAKE_CURRENT_BINARY_DIR} 
			-I${CMAKE_SOURCE_DIR}/include
			-c++
			-python		
			${SWIG_EXTRA_ARGS}		 
			-o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
			${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
	        ${SWIG_DEPENDENCIES}
    COMMENT "Swig Python source") 

add_custom_target(binding_python_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)

	
####################################################################
#
# Build native library
#

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${PYTHON_INCLUDE_DIRS})

add_library(binding_python_lib SHARED libsbml_wrap.cpp)
add_dependencies( binding_python_lib binding_python_swig) 

set_target_properties (binding_python_lib PROPERTIES OUTPUT_NAME "_libsbml")
if (UNIX)
	set_target_properties (binding_python_lib PROPERTIES PREFIX "")
	set_target_properties (binding_python_lib PROPERTIES SUFFIX ".so")
else()		
	if (CYGWIN)
		set_target_properties (binding_python_lib PROPERTIES PREFIX "")
		set_target_properties (binding_python_lib PROPERTIES SUFFIX ".dll")
	else()
		set_target_properties (binding_python_lib PROPERTIES SUFFIX ".pyd")	
	endif()
endif()

target_link_libraries(binding_python_lib ${LIBSBML_LIBRARY}-static ${PYTHON_LIBRARIES})

INSTALL(TARGETS binding_python_lib DESTINATION ${MISC_PREFIX}bindings/python/libsbml )

file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/libsbml.pth" "libsbml\n")
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbml.pth  DESTINATION ${MISC_PREFIX}bindings/python )
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbml.py  DESTINATION ${MISC_PREFIX}bindings/python/libsbml )


####################################################################
#
# testing
#

if(WITH_CHECK)

	# copy test files to build dir
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test DESTINATION ${CMAKE_CURRENT_BINARY_DIR} PATTERN *.py )
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

	# convert paths
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/test.py test_runner)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/test test_dir)

	# run python tests
	add_test(test_python_binding ${PYTHON_EXECUTABLE} ${test_runner} )

	# set up environment if necessary
	# for now I have changed the test runner to set up the paths it needs

	#get_property(lib_location TARGET binding_python_lib PROPERTY LOCATION)
	#get_filename_component(lib_path "${lib_location}" PATH)
	#file(TO_NATIVE_PATH ${lib_path} lib_path)
	
		
	set(test_path "PYTHONPATH=.${FILE_SEP}${test_dir}${FILE_SEP}RelWithDebInfo${FILE_SEP}Release${FILE_SEP}Debug")
	if (WIN32)
		string(REPLACE ";" "\\;" test_path "${test_path}")
	endif()
	set_tests_properties(test_python_binding PROPERTIES ENVIRONMENT "${test_path}")

endif()
