## @file    CMakeLists.txt
## @brief   This CMake Package integrates the SBML Comp Extension with libsbml 5
## @author  Frank T. Bergmann
##
## <!--------------------------------------------------------------------------
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##
## Copyright (C) 2013-2018 jointly by the following organizations:
##     1. California Institute of Technology, Pasadena, CA, USA
##     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
##     3. University of Heidelberg, Heidelberg, Germany
##
## Copyright (C) 2009-2013 jointly by the following organizations: 
##     1. California Institute of Technology, Pasadena, CA, USA
##     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
##  
## Copyright (C) 2006-2008 by the California Institute of Technology,
##     Pasadena, CA, USA 
##  
## Copyright (C) 2002-2005 jointly by the following organizations: 
##     1. California Institute of Technology, Pasadena, CA, USA
##     2. Japan Science and Technology Agency, Japan
## 
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation.  A copy of the license agreement is provided
## in the file named "LICENSE.txt" included with this software distribution
## and also available online as http://sbml.org/software/libsbml/license.html
## ------------------------------------------------------------------------ -->

cmake_minimum_required(VERSION 2.8.4...3.19)

# the project name should be the same name as the SBML package
project(comp)

set(LIBSBML_SOURCE_INTEGRATE "c:/Users/Lucian/Desktop/libsbml-comp/" CACHE PATH "Path to the libsbml source distribution for integration.")
set(LIBSBML_SOURCE_BUILD "c:/Users/Lucian/Desktop/libsbml/install/" CACHE PATH "Path to the libsbml source distribution for libraries and include files.")
set(EXTRA_LIBS "xml2" CACHE STRING "List of Libraries to link against" )

# Enable the generation of unit tests. If enabled, all test runners 
# will be created and can be run with "make test" or ctest.
# This won't work in Visual Studio 2003, so we disable this option there.
#
if(NOT ${CMAKE_GENERATOR} MATCHES "Visual Studio 6" AND NOT ${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
    option(WITH_CHECK    "Compile unit tests. Run with 'make test' or 'ctest'." OFF)
endif()

# compile the package and link it against an existing libsbml-5 version
# file sources
file(GLOB sources
          src/sbml/packages/comp/sbml/*h 
          src/sbml/packages/comp/sbml/*.cpp
          src/sbml/packages/comp/common/*.h
          src/sbml/packages/comp/extension/*.h
          src/sbml/packages/comp/extension/*.cpp
          src/sbml/packages/comp/util/*h
          src/sbml/packages/comp/util/*.cpp
          src/sbml/packages/comp/validator/*.h
          src/sbml/packages/comp/validator/*.cpp
          src/sbml/packages/comp/validator/constraints/*.h
          src/sbml/packages/comp/validator/constraints/*.cpp
    )

# add sources 
set(SOURCE_FILES ${sources} )
		
find_library(LIBSBML_LIBS 
             NAMES libsbml.lib sbml
             PATHS ${LIBSBML_SOURCE_BUILD} 
                   ${LIBSBML_SOURCE_BUILD/lib}
                   ${LIBSBML_SOURCE_BUILD/install/lib}
                   ${LIBSBML_SOURCE_BUILD/src/.libs}
                   /usr/lib /usr/local/lib
                   ${LIBSBML_ROOT_SOURCE_DIR}
                   ${LIBSBML_ROOT_SOURCE_DIR}/dependencies/lib
             )

if (NOT UNIX)
	add_definitions(-DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS)
endif()
	
	if (MSVC)
		option(WITH_STATIC_RUNTIME "Use Static MSVC RUNTIME" OFF)
	
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
		add_definitions( -D_MT -DLIBSBML_EXPORTS)
	endif(WITH_STATIC_RUNTIME)
	endif(MSVC)
	

include_directories(${LIBSBML_SOURCE_BUILD}/src/ ${LIBSBML_SOURCE_BUILD}/include/ src/)

add_library(comp STATIC ${SOURCE_FILES} )
target_link_libraries(comp ${LIBSBML_LIBS})
	
option(WITH_EXAMPLES "Compile Example File" ON)
	
if(WITH_EXAMPLES)
	add_executable(comp_example examples/c++/example1.cpp)
	target_link_libraries(comp_example comp ${EXTRA_LIBS})

	add_executable(comp_example2 examples/c++/example2.cpp)
	target_link_libraries(comp_example2 comp ${EXTRA_LIBS})

	add_executable(comp_example3 examples/c++/example3.cpp)
	target_link_libraries(comp_example3 comp ${EXTRA_LIBS})

	add_executable(spec_example1 examples/c++/spec_example1.cpp)
	target_link_libraries(spec_example1 comp ${EXTRA_LIBS})

	add_executable(spec_example2 examples/c++/spec_example2.cpp)
	target_link_libraries(spec_example2 comp ${EXTRA_LIBS})
		
	add_executable(spec_example3 examples/c++/spec_example3.cpp)
	target_link_libraries(spec_example3 comp ${EXTRA_LIBS})

	add_executable(spec_example4 examples/c++/spec_example4.cpp)
	target_link_libraries(spec_example4 comp ${EXTRA_LIBS})

	add_executable(testing1 examples/c++/testing1.cpp)
	target_link_libraries(testing1 comp ${EXTRA_LIBS})

	add_executable(flattenModel examples/c++/flattenModel.cpp)
	target_link_libraries(flattenModel comp ${EXTRA_LIBS})

	add_executable(invalidity_test examples/c++/invalidity_test.cpp)
	target_link_libraries(invalidity_test comp ${EXTRA_LIBS})

endif()

# This CMake file integrates the binding source with the libsbml source tree
#
#

# include common functions (used for copying / removing files)
if(NOT EXISTS ${LIBSBML_SOURCE_INTEGRATE}/common.cmake)
	message(FATAL_ERROR "Invalid libsbml source directory")
endif()

include(${LIBSBML_SOURCE_INTEGRATE}/common.cmake)

add_custom_target(integrate
	COMMENT Copy all package files to the specified libsbml source directory.
        COMMAND ${CMAKE_COMMAND} -E copy "${LIBSBML_ROOT_SOURCE_DIR}/comp-package.cmake" ${LIBSBML_SOURCE_INTEGRATE}/
        COMMAND ${CMAKE_COMMAND} -E copy "${LIBSBML_ROOT_SOURCE_DIR}/src/comp-package.cmake" ${LIBSBML_SOURCE_INTEGRATE}/src/

	# copy language binding files
        COMMAND ${CMAKE_COMMAND}  -E copy_directory "${LIBSBML_ROOT_SOURCE_DIR}/src/bindings" ${LIBSBML_SOURCE_INTEGRATE}/src/bindings

        # copy package files
        COMMAND ${CMAKE_COMMAND} -E copy_directory  "${LIBSBML_ROOT_SOURCE_DIR}/src/sbml/packages/comp" ${LIBSBML_SOURCE_INTEGRATE}/src/sbml/packages/comp
		
	VERBATIM
        )

#There may be some way to convert original-bindings into installed-binding with a string REPLACE directive or
# something, but I couldn't get it to work.  So instead of using something like this:
file(GLOB original-bindings
          ${LIBSBML_ROOT_SOURCE_DIR}/src/bindings/*/*-comp* 
          ${LIBSBML_ROOT_SOURCE_DIR}/src/bindings/*/comp-* 
    )

file(GLOB installed-bindings
          ${LIBSBML_SOURCE_INTEGRATE}/src/bindings/*/*-comp* 
          ${LIBSBML_SOURCE_INTEGRATE}/src/bindings/*/comp-* 
    )

# ...we just list everything explicitly instead and this works.  We can't search for '-comp' 
# files (like 'installed-bindings', above) because when CMake is first run, they won't exist!
set(BINDING_FILES
                # C# bindings
                  "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/csharp/local-downcast-extension-comp.i"
                  "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/csharp/local-downcast-namespaces-comp.i"
                  "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/csharp/local-packages-comp.i"
		
                # java bindings
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/java/local-downcast-extension-comp.i"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/java/local-downcast-namespaces-comp.i"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/java/local-packages-comp.i"
		
                # perl bindings
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/perl/local-comp.i"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/perl/local-downcast-extension-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/perl/local-downcast-packages-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/perl/local-downcast-namespaces-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/perl/local-downcast-plugins-comp.cpp"
		
                # python bindings
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/python/local-comp.i"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/python/local-downcast-extension-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/python/local-downcast-packages-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/python/local-downcast-namespaces-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/python/local-downcast-plugins-comp.cpp"
		
                # ruby bindings
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/ruby/local-comp.i"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/ruby/local-downcast-extension-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/ruby/local-downcast-packages-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/ruby/local-downcast-namespaces-comp.cpp"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/ruby/local-downcast-plugins-comp.cpp"
		
                # generic swig bindings		
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/swig/comp-package.h"
                 "${LIBSBML_SOURCE_INTEGRATE}/src/bindings/swig/comp-package.i"	
	)

add_custom_target (remove
	COMMENT Remove all package files from the specified libsbml source directory.
	COMMAND ${CMAKE_COMMAND} -E remove_directory  ${LIBSBML_SOURCE_INTEGRATE}/src/sbml/packages/comp
	COMMAND ${CMAKE_COMMAND} -E remove            ${LIBSBML_SOURCE_INTEGRATE}/src/comp-package.cmake
	COMMAND ${CMAKE_COMMAND} -E remove            ${BINDING_FILES}

	COMMENT "Finished removing the SBML comp package from the libsbml source tree in:" "${LIBSBML_SOURCE_INTEGRATE}"
        VERBATIM
        )

###############################################################################
#
# Enable support for testing ... can be invoked by running ctest 
# or make test
#

if(WITH_CHECK)

    enable_testing()
    
    find_library(LIBCHECK_LIBRARY
        NAMES check libcheck
        PATHS /usr/lib /usr/local/lib ${LIBSBML_DEPENDENCY_DIR}/lib
        DOC "The file name of the libcheck library."
    )
    
    find_path(LIBCHECK_INCLUDE_DIR
        NAMES check.h
        PATHS /usr/include /usr/local/include  ${LIBSBML_DEPENDENCY_DIR}/include
        DOC "The directory containing the libcheck include files."
              )
    
    if(NOT EXISTS "${LIBCHECK_INCLUDE_DIR}/check.h")
        message(FATAL_ERROR "The 'check' include directory appears to be invalid. It should contain the file check.h, but it does not.")
    endif()

    if(${CMAKE_GENERATOR} MATCHES "Visual Studio 6" OR ${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
        message(WARNING "Libcheck is not compatible with Visual Studio 2003 (or earlier versions).")
    endif()
    
	add_subdirectory(src/sbml/packages/comp/extension/test)
	add_subdirectory(src/sbml/packages/comp/util/test)
	add_subdirectory(src/sbml/packages/comp/sbml/test)

endif(WITH_CHECK)


