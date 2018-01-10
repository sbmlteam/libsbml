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
find_package(Matlab)

# on windows let us build the library using MSVC, on Linux and OS X we choose
# to build with the buildSBML script by default
#set(MATLAB_MEX_DEFAULT)
#set(WITH_MATLAB_BUILDSBML_DEFAULT)
#if (UNIX)
#   set(WITH_MATLAB_BUILDSBML_DEFAULT ON)
#   set(MATLAB_MEX_DEFAULT OFF)
#else()
#   set(WITH_MATLAB_BUILDSBML_DEFAULT OFF)
#   set(MATLAB_MEX_DEFAULT OFF)
#endif()
#
#option(WITH_MATLAB_BUILDSBML   
#      "Generate Matlab bindings using the buildsbml script from matlab."     ${WITH_MATLAB_BUILDSBML_DEFAULT} )
#option(WITH_MATLAB_MEX   
#      "Generate Matlab bindings using MEX compiler "     ${MATLAB_MEX_DEFAULT} )


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
	
	foreach(matlab_source_file "TranslateSBML" "OutputSBML")
	
		add_library(matlab_binding_${matlab_source_file} SHARED "${CMAKE_CURRENT_SOURCE_DIR}/${matlab_source_file}.cpp")
		set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES OUTPUT_NAME "${matlab_source_file}")
		set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES SUFFIX ".${MATLAB_MEX_EXT}")
                set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES  PREFIX "")
		if (MSVC)
		set_target_properties(matlab_binding_${matlab_source_file} PROPERTIES LINK_FLAGS "/export:mexFunction") 
		endif()
		target_link_libraries(matlab_binding_${matlab_source_file} ${MATLAB_LIBRARIES} ${LIBSBML_LIBRARY}-static)				
		install(TARGETS matlab_binding_${matlab_source_file} DESTINATION ${MATLAB_PACKAGE_INSTALL_DIR} )

	endforeach()
	
	
#endif(WITH_MATLAB_BUILDSBML)


# mark files for installation
file(GLOB matlab_scripts "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/*.m"
                         "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/*.xml")
install(FILES ${matlab_scripts} DESTINATION ${MATLAB_PACKAGE_INSTALL_DIR})

# add test cases
if (WITH_CHECK)
	
	add_subdirectory(test)	

endif()


endif()

