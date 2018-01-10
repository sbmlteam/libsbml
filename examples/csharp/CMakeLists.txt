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

# run examples as test 

if (CMAKE_VERSION VERSION_LESS 3.0)
    message(STATUS "C# example tests check version: ${CMAKE_VERSION} < 3.0, skipping.")
else()

    add_test(NAME test_cs_addCVTerms
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/addCVTerms.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        addCVTerms.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_addCVTerms)
    
    add_test(NAME test_cs_addingEvidenceCodes_1
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/addingEvidenceCodes_1.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        addingEvidenceCodes_1.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_addingEvidenceCodes_1)
    
    add_test(NAME test_cs_addingEvidenceCodes_2
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/addingEvidenceCodes_2.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        addingEvidenceCodes_2.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_addingEvidenceCodes_2)
    
    add_test(NAME test_cs_addModelHistory
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/addModelHistory.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        addModelHistory.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_addModelHistory)
    
    add_test(NAME test_cs_appendAnnotation
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/appendAnnotation.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        appendAnnotation.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_appendAnnotation)
    
    add_test(NAME test_cs_convertSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/convertSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        convertSBML.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_convertSBML)
    
    add_test(NAME test_cs_createExampleSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/createExampleSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        createExampleSBML.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_createExampleSBML)
    
    add_test(NAME test_cs_echoSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/echoSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        echoSBML.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_echoSBML)
    
    add_test(NAME test_cs_GetAllElementsWithNotes
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/GetAllElementsWithNotes.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_GetAllElementsWithNotes)
    
    add_test(NAME test_cs_printAnnotation
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printAnnotation.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printAnnotation)
    
    add_test(NAME test_cs_printMath
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printMath.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printMath)
    
    add_test(NAME test_cs_printNotes
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printNotes.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printNotes)
    
    add_test(NAME test_cs_printRegisteredPackages
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printRegisteredPackages.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        printRegisteredPackages.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printRegisteredPackages)
    
    add_test(NAME test_cs_printSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printSBML)
    
    add_test(NAME test_cs_printsupported
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printsupported.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        printsupported.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printsupported)
    
    add_test(NAME test_cs_printUnits
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/printUnits.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_printUnits)
    
    add_test(NAME test_cs_promoteParameters
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/promoteParameters.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        promoteParameters.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_promoteParameters)
    
    add_test(NAME test_cs_readSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/readSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_readSBML)
    
	if(ENABLE_FBC) 

    add_test(NAME test_cs_stripPackage
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/stripPackage.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/fbc/fbc_example1.xml
        fbc
        stripPackage.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_stripPackage)
    
	endif(ENABLE_FBC)

    add_test(NAME test_cs_SetIdFromNames
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/SetIdFromNames.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        SetIdFromNames.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_SetIdFromNames)
    
    add_test(NAME test_cs_unsetAnnotation
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/unsetAnnotation.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        unsetAnnotation.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_unsetAnnotation)
    
    add_test(NAME test_cs_unsetNotes
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/unsetNotes.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        unsetNotes.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_unsetNotes)
    
    add_test(NAME test_cs_validateSBML
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/validateSBML.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_validateSBML)
    
    add_test(NAME test_cs_evaluateCustomMath
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/evaluateCustomMath.exe
        2*3+4
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    )
    ADJUST_ENVIRONMENT(test_cs_evaluateCustomMath)
    
    add_test(NAME test_cs_inlineInitialAssignments
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/inlineInitialAssignments.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        inlineInitialAssignments.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_inlineInitialAssignments)
    
    add_test(NAME test_cs_replaceOneFD
        COMMAND ${CS_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/replaceOneFD.exe
        ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/functiondef.xml
        f
        reaction_1
        replaceOneFD.out.xml
    )
    ADJUST_ENVIRONMENT(test_cs_replaceOneFD)

endif()
