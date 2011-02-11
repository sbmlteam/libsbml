###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for libSBML MATLAB bindings
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

if(WITH_MATLAB)
SET(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" ${CMAKE_MODULE_PATH}) 
find_package(Matlab)

if (NOT EXISTS "${MATLAB_MEX_COMMAND}")
	# fail if mex was not found
	message (FATAL_ERROR "The mex command is vital for building matlab bindings. It was not found during configuration. Please manually specify MATLAB_MEX_COMMAND.")
endif()


# get location of library
get_property(sbml_location TARGET ${LIBSBML_LIBRARY} PROPERTY LOCATION)
get_filename_component(sbml_path "${sbml_location}" PATH)
file(TO_NATIVE_PATH ${sbml_path} sbml_path)

message(STATUS "matlab root        ${MATLAB_ROOT}")
message(STATUS "matlab mex command ${MATLAB_MEX_COMMAND}")
message(STATUS "matlab mex library ${MATLAB_MEX_LIBRARY}")
message(STATUS "matlab include dir ${MATLAB_INCLUDE_DIR}")
message(STATUS "matlab libraries   ${MATLAB_LIBRARIES}")
message(STATUS "matlab root path   ${MATLAB_ROOT_PATH}")
message(STATUS "extension is       ${MATLAB_MEX_EXT}")

# break up the libsbml library dependencies into paths and libraries
set(MATLAB_EXTRA_LIBS)
foreach(lib ${LIBSBML_LIBS})
	get_filename_component(name ${lib} NAME_WE)	
	get_filename_component(path ${lib} PATH)	
	string(REGEX REPLACE "lib" "" name "${name}")
	file(TO_NATIVE_PATH "${path}" native_path)
	list(APPEND MATLAB_EXTRA_LIBS -l${name})
	if (WIN32)
		list(APPEND MATLAB_EXTRA_LIBS -L${native_path})
	endif()
endforeach()

set(MATLAB_LIBSBML_LIB)
set(MATLAB_OBJ_FILE)
set(MATLAB_MEX_EXTRA_ARGS)
if(UNIX)
	set(MATLAB_LIBSBML_LIB -l${LIBSBML_LIBRARY} -L${sbml_path})
	set(MATLAB_MEX_EXTRA_ARGS -cxx)
else()
	if(MSVC)
		set(MATLAB_LIBSBML_LIB -l${LIBSBML_LIBRARY}-static -L${sbml_path})
		set(MATLAB_MEX_EXTRA_ARGS -DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS )
	else()
		set(MATLAB_LIBSBML_LIB -l${LIBSBML_LIBRARY} -L${sbml_path})	
	endif()
endif()

macro(build_matlab_file fileName)
	
	if (NOT UNIX)
		# sarah commented that the mex command did not work for her, 
		# so let us try this one on windows
		
		file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${fileName}.c" src)
		file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../" include_path1)
		file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}/include" include_path2)

		get_property(sbml_import TARGET ${LIBSBML_LIBRARY} PROPERTY LOCATION)
		string(REPLACE ".dll" ".lib" sbml_import ${sbml_import})
		file(TO_NATIVE_PATH "${sbml_import}" sbml_import)
		
		ADD_CUSTOM_COMMAND(	
			OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT}
			
			COMMAND "${CMAKE_COMMAND}"
			ARGS  -E remove ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT}
			
			COMMAND "${MATLAB_MEX_COMMAND}"
			ARGS 
				${src}
				-I${include_path1}
				-I${include_path2}
				${sbml_import}
				
			MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${fileName}.c
			COMMENT "compile and link matlab mex file: ${fileName}") 
	
	else()
	

	if (MSVC)
		set(MATLAB_OBJ_FILE ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.obj)
	else()
		set(MATLAB_OBJ_FILE ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.o)
	endif()

	# compile and link mex file
	ADD_CUSTOM_COMMAND(	
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT}
		
		COMMAND "${CMAKE_COMMAND}"
		ARGS  -E remove ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT}
		
		COMMAND "${MATLAB_MEX_COMMAND}"
		ARGS 
			-I"${CMAKE_CURRENT_SOURCE_DIR}/../../"
			-I"${CMAKE_SOURCE_DIR}/include"
			-c
			${MATLAB_MEX_EXTRA_ARGS}
			"${CMAKE_CURRENT_SOURCE_DIR}/${fileName}.c"
		
		COMMAND "${MATLAB_MEX_COMMAND}"
		ARGS 
			${MATLAB_MEX_EXTRA_ARGS}
			-output "${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT}"
			${MATLAB_OBJ_FILE}
			${MATLAB_LIBSBML_LIB}
			${MATLAB_EXTRA_LIBS}
			
		MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${fileName}.c
		COMMENT "compile and link matlab mex file: ${fileName}") 
	
	endif()
		
	add_custom_target(binding_matlab_${fileName} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT} ${LIBSBML_LIBRARY})
	add_dependencies(binding_matlab_${fileName} ${LIBSBML_LIBRARY})

	install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.${MATLAB_MEX_EXT} DESTINATION ${MISC_PREFIX}bindings/matlab )
	install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/${fileName}.m DESTINATION ${MISC_PREFIX}bindings/matlab )

endmacro()

build_matlab_file("TranslateSBML")
build_matlab_file("OutputSBML")

# mark files for installation

foreach(mFile

		CheckAndConvert.m  
		Contents.m
        ConvertFormulaToMathML.m
        isSBML_Model.m
        TranslateSBML.m
        OutputSBML.m
        buildWin.m
        install_Win32.m
        install_Win64.m
        isoctave.m		
        test.xml
		uninstall_Win32.m
        uninstall_Win64.m
		
)

	install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/${mFile} DESTINATION ${MISC_PREFIX}bindings/matlab )

endforeach()


# add test cases
if (WITH_CHECK)
	
	add_subdirectory(test)	

endif()


endif()

