## @file    CMakeLists.txt
## @brief   CMake build script for example programs
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

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${LIBSBML_ROOT_SOURCE_DIR}/src)
include_directories(${LIBSBML_ROOT_SOURCE_DIR}/include)
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)

if (EXTRA_INCLUDE_DIRS) 
include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

foreach(example 

	addCVTerms
	addingEvidenceCodes_1
	addingEvidenceCodes_2
	addModelHistory
	appendAnnotation
	convertSBML
	createExampleSBML
	drawMath
	echoSBML
	evaluateMath
	printAnnotation
	printMath
	printNotes
	printRegisteredPackages
	printSBML
	printSupported
	printUnits
	promoteParameters
	readSBML
	stripPackage
	translateMath
	translateL3Math
	unsetAnnotation
	unsetNotes
	validateSBML

)
	add_executable(example_c_${example} ${example}.c util.c)
	set_target_properties(example_c_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_c_${example} ${LIBSBML_LIBRARY}-static)	

endforeach()

# run examples as test 

add_test(NAME test_c_addCVTerms
         COMMAND "$<TARGET_FILE:example_c_addCVTerms>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addCVTerms.out.xml
)

add_test(NAME test_c_addingEvidenceCodes_1
         COMMAND "$<TARGET_FILE:example_c_addingEvidenceCodes_1>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addingEvidenceCodes_1.out.xml
)

add_test(NAME test_c_addingEvidenceCodes_2
         COMMAND "$<TARGET_FILE:example_c_addingEvidenceCodes_2>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addingEvidenceCodes_2.out.xml
)

add_test(NAME test_c_addModelHistory
         COMMAND "$<TARGET_FILE:example_c_addModelHistory>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addModelHistory.out.xml
)

add_test(NAME test_c_appendAnnotation
         COMMAND "$<TARGET_FILE:example_c_appendAnnotation>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         appendAnnotation.out.xml
)

add_test(NAME test_c_convertSBML
         COMMAND "$<TARGET_FILE:example_c_convertSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         convertSBML.out.xml
)

add_test(NAME test_c_createExampleSBML
         COMMAND "$<TARGET_FILE:example_c_createExampleSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         createExampleSBML.out.xml
)

add_test(NAME test_c_drawMath
         COMMAND "$<TARGET_FILE:example_c_drawMath>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         drawMath.out.xml
)

add_test(NAME test_c_echoSBML
         COMMAND "$<TARGET_FILE:example_c_echoSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         echoSBML.out.xml
)

add_test(NAME test_c_printAnnotation
         COMMAND "$<TARGET_FILE:example_c_printAnnotation>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_printMath
         COMMAND "$<TARGET_FILE:example_c_printMath>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_printNotes
         COMMAND "$<TARGET_FILE:example_c_printNotes>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_printRegisteredPackages
         COMMAND "$<TARGET_FILE:example_c_printRegisteredPackages>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_printSBML
         COMMAND "$<TARGET_FILE:example_c_printSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_printSupported
         COMMAND "$<TARGET_FILE:example_c_printSupported>" 
)

add_test(NAME test_c_printUnits
         COMMAND "$<TARGET_FILE:example_c_printUnits>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_c_promoteParameters
         COMMAND "$<TARGET_FILE:example_c_promoteParameters>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         promoteParameters.out.xml
)

add_test(NAME test_c_readSBML
         COMMAND "$<TARGET_FILE:example_c_readSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

 if(ENABLE_FBC) 

add_test(NAME test_c_stripPackage
         COMMAND "$<TARGET_FILE:example_c_stripPackage>" 
		 ${CMAKE_SOURCE_DIR}/examples/sample-models/fbc/fbc_example1.xml
		 fbc
		 stripPackage.out.xml
)

endif(ENABLE_FBC)

add_test(NAME test_c_unsetAnnotation
         COMMAND "$<TARGET_FILE:example_c_unsetAnnotation>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         unsetAnnotation.out.xml
)

add_test(NAME test_c_unsetNotes
		 COMMAND "$<TARGET_FILE:example_c_unsetNotes>" 
		 ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
		 unsetNotes.out.xml
)

add_test(NAME test_c_validateSBML
         COMMAND "$<TARGET_FILE:example_c_validateSBML>" 
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
