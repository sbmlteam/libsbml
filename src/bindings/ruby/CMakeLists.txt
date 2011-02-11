###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for libSBML Ruby bindings
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
include(FindRuby)


ADD_CUSTOM_COMMAND(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
    COMMAND "${SWIG_EXECUTABLE}"
    ARGS	-I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
			-I${CMAKE_CURRENT_SOURCE_DIR} 
			-I${CMAKE_SOURCE_DIR}/include
			-c++
			-ruby		
			${SWIG_EXTRA_ARGS}		 
			-o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
			${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    COMMENT "Swig Ruby source") 

add_custom_target(binding_ruby_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)

	
####################################################################
#
# Build native library
#

if(WIN32)
	add_definitions(-DNT=1 -DIMPORT)
endif()
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${RUBY_INCLUDE_DIRS})
include_directories(${RUBY_HDR_DIR})


add_library(binding_ruby_lib SHARED libsbml_wrap.cpp)
add_dependencies(binding_ruby_lib binding_ruby_swig)

set_target_properties (binding_ruby_lib PROPERTIES OUTPUT_NAME "libSBML")
if (UNIX)
	set_target_properties (binding_ruby_lib PROPERTIES PREFIX "")
	if (APPLE)
		set_target_properties (binding_ruby_lib PROPERTIES SUFFIX ".bundle")
	endif()
endif()
target_link_libraries(binding_ruby_lib ${LIBSBML_LIBRARY}-static ${RUBY_LIBRARY})
INSTALL(TARGETS binding_ruby_lib DESTINATION ${MISC_PREFIX}bindings/ruby )

####################################################################
#
# testing
#

if(WITH_CHECK)

	# copy test files to build dir
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test DESTINATION ${CMAKE_CURRENT_BINARY_DIR} PATTERN *.rb )
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.rb DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

	# convert paths
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/test.rb test_runner)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/test test_dir)

	# run ruby tests
	add_test(test_ruby_binding ${RUBY_EXECUTABLE} -I. ${test_runner} )

	# specify that tests have passed if we find only dots	
	set_tests_properties(test_ruby_binding PROPERTIES PASS_REGULAR_EXPRESSION "[.]+" )
	# specify that tests have failed in case there was one of the words below in the output
	set_tests_properties(test_ruby_binding PROPERTIES FAIL_REGULAR_EXPRESSION "Error;ERROR;Failed")	
	
	
endif()

