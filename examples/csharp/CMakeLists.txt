## @file    CMakeLists.txt
## @brief   CMake build script for example programs
## @author  Frank T. Bergmann
##
## <!--------------------------------------------------------------------------
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##
## Copyright (C) 2013-2017 jointly by the following organizations:
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

foreach( example
	
	addCustomValidator
	addCVTerms
	addingEvidenceCodes_1
	addingEvidenceCodes_2
	addModelHistory
	appendAnnotation
	callExternalValidator
	convertSBML
	createExampleSBML
	echoSBML
	GetAllElementsWithNotes
	printAnnotation
	printMath
	printNotes
	printRegisteredPackages
	printSBML
	printsupported
	printUnits
	promoteParameters
	readSBML
	stripPackage
	SetIdFromNames
	translateMath
	unsetAnnotation
	unsetNotes
	validateSBML
	evaluateCustomMath
	inlineInitialAssignments
	replaceOneFD
	
)

	ADD_CUSTOM_COMMAND(	
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${example}.exe
		COMMAND "${CSHARP_EXAMPLE_COMPILER}"
		ARGS -r:${CMAKE_CURRENT_BINARY_DIR}/../../src/bindings/csharp/libsbmlcsP.dll
			-target:exe
			-out:${example}.exe
			${CSHARP_EXTRA_ARGS}
			${CMAKE_CURRENT_SOURCE_DIR}${PATH_SEP}${example}.cs
		MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${example}.cs
		COMMENT "Build C# example: ${example}") 
	
	add_custom_target(example_cs_${example} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${example}.exe)
	add_dependencies(example_cs_${example} binding_csharp_managed_lib)
endforeach()



####################################################################
#
# CMake Build Script for C# examples
#

macro(ADJUST_ENVIRONMENT target)

file(TO_NATIVE_PATH $<TARGET_FILE_DIR:binding_csharp_native_lib> CS_PATH)

if (APPLE)
# adjust DYLD_LIBRARY_PATH
set_tests_properties(${target} PROPERTIES ENVIRONMENT DYLD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}/../:$<TARGET_FILE_DIR:binding_csharp_native_lib>:$ENV{DYLD_LIBRARY_PATH})
elseif(UNIX)
# adjust LD_LIBRARY_PATH
set_tests_properties(${target} PROPERTIES ENVIRONMENT LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/src/bindings/csharp/:$<TARGET_FILE_DIR:binding_csharp_native_lib>:$ENV{LD_LIBRARY_PATH})
else()
# adjust path variable
SET (NEW_PATH "$<TARGET_FILE_DIR:binding_csharp_native_lib>\\;${CS_PATH}\\;$ENV{PATH}")
STRING(REPLACE "\\;" ";" NEW_PATH "${NEW_PATH}")
STRING(REPLACE ";" "\\;" NEW_PATH "${NEW_PATH}")
set_tests_properties(${target} PROPERTIES ENVIRONMENT PATH=${NEW_PATH})
endif()

endmacro()


set(CS_INTERPRETER)
if (NOT WIN32)
set(CS_INTERPRETER mono)
endif()


# now add all examples we want to run
add_test(NAME test_cs_addCVTerms
         COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/addCVTerms.exe
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         out.xml
)
ADJUST_ENVIRONMENT(test_cs_addCVTerms)
