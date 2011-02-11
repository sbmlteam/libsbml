###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for libSBML C# bindings
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
# SWIG C# bindings
#

set(SWIG_EXTRA_FLAGS)
if(NOT UNIX)
	set(SWIG_EXTRA_FLAGS -DSWIGWIN -DSWIG_CSHARP_NO_WSTRING_HELPER)
endif()


file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/csharp-files LIBSBML_CS_FILES)

MAKE_DIRECTORY(${CMAKE_CURRENT_BINARY_DIR}/csharp-files)
ADD_CUSTOM_COMMAND(	
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E remove_directory "${LIBSBML_CS_FILES}"
	COMMAND "${CMAKE_COMMAND}"
    ARGS  -E make_directory "${LIBSBML_CS_FILES}"
    COMMAND "${SWIG_EXECUTABLE}"
    ARGS -I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
         -I${CMAKE_CURRENT_SOURCE_DIR} 
         -I${CMAKE_SOURCE_DIR}/include
	     -c++
	     -csharp
	     -namespace libsbml
	     -dllimport libsbmlcs ${SWIG_EXTRA_ARGS}
	     ${SWIG_EXTRA_FLAGS}		 
         -outdir ${LIBSBML_CS_FILES}
	     -o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
	     ${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbml.i

    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbml.i
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
	        ${SWIG_DEPENDENCIES}
    COMMENT "Swig C# source") 

add_custom_target(binding_csharp_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp)

	
####################################################################
#
# Build native library
#

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
include_directories(${CMAKE_SOURCE_DIR}/include)
add_library(binding_csharp_native_lib SHARED libsbml_wrap.cpp)
add_dependencies(binding_csharp_native_lib binding_csharp_swig)
if (UNIX)
set_target_properties (binding_csharp_native_lib PROPERTIES OUTPUT_NAME "sbmlcs")
else()
set_target_properties (binding_csharp_native_lib PROPERTIES OUTPUT_NAME "libsbmlcs")
endif()
target_link_libraries(binding_csharp_native_lib ${LIBSBML_LIBRARY}-static)
INSTALL(TARGETS binding_csharp_native_lib 
        DESTINATION ${MISC_PREFIX}bindings/csharp	
  )

####################################################################
#
# build managed library
# 

ADD_CUSTOM_COMMAND(	
	OUTPUT libsbmlcsP.dll TestRunner.exe libsbmlcsP.xml 
	
	COMMAND "${CMAKE_COMMAND}"
	ARGS -DCSHARP_EXTRA_ARGS=\"${CSHARP_EXTRA_ARGS}\"
		 -DCSHARP_COMPILER=\"${CSHARP_COMPILER}\"
		 -DBIN_DIRECTORY=\"${CMAKE_CURRENT_BINARY_DIR}\"
		 -DSRC_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}\"		 
		 -P "${CMAKE_CURRENT_SOURCE_DIR}/compile-native-files.cmake"
		 
	MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbml.i"
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
			${CMAKE_CURRENT_SOURCE_DIR}/compile-native-files.cmake
			${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
	
	WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Build csharp files"
	) 

add_custom_target(binding_csharp_managed_lib ALL DEPENDS libsbmlcsP.dll)
add_dependencies(binding_csharp_managed_lib binding_csharp_swig)

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbmlcsP.dll DESTINATION ${MISC_PREFIX}bindings/csharp) 

####################################################################
#
# testing
# 

if(WITH_CHECK)

file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/libsbmlcsP.dll LIBSBML_LIB)
ADD_CUSTOM_COMMAND(	
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/TestRW.exe
    COMMAND "${CSHARP_COMPILER}"
    ARGS -r:${LIBSBML_LIB}
		-target:exe
		-out:TestRW.exe
		${CSHARP_EXTRA_ARGS}
		${CMAKE_CURRENT_SOURCE_DIR}${PATH_SEP}TestRW.cs
	MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/TestRW.cs
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbmlcsP.dll
		${CMAKE_CURRENT_SOURCE_DIR}/TestRunner.cs
    COMMENT "Build C# TestRuner") 

	
add_custom_target(binding_csharp_test_runner ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/TestRW.exe libsbmlcsP.dll ${LIBSBML_CS_LIBRARY})
add_dependencies(binding_csharp_test_runner binding_csharp_managed_lib )

if(UNIX)
	add_test(test_csharp_bindings mono ${CMAKE_CURRENT_BINARY_DIR}/TestRW.exe ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-2/)

	# to run the compiled tests
	add_test(test_csharp_bindings_full mono ${CMAKE_CURRENT_BINARY_DIR}/TestRunner.exe ${CMAKE_SOURCE_DIR}/src/sbml/test/test-data)
		
	if(APPLE)
		set_tests_properties(test_csharp_bindings PROPERTIES ENVIRONMENT "DYLD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
		set_tests_properties(test_csharp_bindings_full PROPERTIES ENVIRONMENT "DYLD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
	else()
		set_tests_properties(test_csharp_bindings PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH}")
		set_tests_properties(test_csharp_bindings_full PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH}")
	endif()
else()

	include(${CMAKE_SOURCE_DIR}/common.cmake)
	copy_files( ${CMAKE_SOURCE_DIR}/dependencies/bin/ ${CMAKE_CURRENT_BINARY_DIR} *.dll )

	add_test(test_csharp_bindings ${CMAKE_CURRENT_BINARY_DIR}/TestRW.exe ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-2/)

	#  to run the compiled tests
	add_test(test_csharp_bindings_full 
			${CMAKE_CURRENT_BINARY_DIR}/TestRunner.exe 
			${CMAKE_SOURCE_DIR}/src/sbml/test/test-data)
	
endif()

endif()
