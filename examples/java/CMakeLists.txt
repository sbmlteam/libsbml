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

find_package(Java COMPONENTS Development REQUIRED)

file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR} CURRENT_BIN)
file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/../../src/bindings/java/libsbmlj.jar CURRENT_JAR )
file(TO_NATIVE_PATH $<TARGET_FILE_DIR:binding_java_lib> LIBRARY_PATH)


foreach(file 
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
    evaluateMath
    getAllElementsWithNotes
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
    setIdFromNames
    translateMath
    unsetAnnotation
    unsetNotes
    validateSBML
    evaluateCustomMath
    inlineInitialAssignments
    replaceOneFD
    )

file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java CURRENT_FILE)

add_custom_command(	
    OUTPUT ${file}.class
    COMMAND "${Java_JAVAC_EXECUTABLE}"
    ARGS -cp ".${FILE_SEP}\"${CURRENT_JAR}\"${FILE_SEP}${CMAKE_CURRENT_SOURCE_DIR}"
        ${CURRENT_FILE}
        -source "${JAVA_COMPATIBILITY}"
        -target "${JAVA_COMPATIBILITY}"
        -d ${CMAKE_CURRENT_BINARY_DIR}
    MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/../../src/bindings/java/local.i"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java
    COMMENT "Build java example ${file}"
) 

add_custom_target(example_java_${file} ALL 
    DEPENDS 
    ${CMAKE_CURRENT_BINARY_DIR}/${file}.class 
    SOURCES 
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java 
)

add_dependencies(example_java_${file} binding_java_jar)

endforeach()

# run examples as test 

add_test(NAME test_java_addCVTerms
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    addCVTerms
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    addCVTerms.out.xml
)

add_test(NAME test_java_addingEvidenceCodes_1
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    addingEvidenceCodes_1
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    addingEvidenceCodes_1.out.xml
)

add_test(NAME test_java_addingEvidenceCodes_2
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    addingEvidenceCodes_2
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    addingEvidenceCodes_2.out.xml
)

add_test(NAME test_java_addModelHistory
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    addModelHistory
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    addModelHistory.out.xml
)

add_test(NAME test_java_appendAnnotation
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    appendAnnotation
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    appendAnnotation.out.xml
)

add_test(NAME test_java_createExampleSBML
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    createExampleSBML
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    createExampleSBML.out.xml
)

add_test(NAME test_java_echoSBML
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    echoSBML
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    echoSBML.out.xml
)

add_test(NAME test_java_evaluateCustomMath
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    evaluateCustomMath
    2*3+4
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_getAllElementsWithNotes
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    getAllElementsWithNotes
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_inlineInitialAssignments
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    inlineInitialAssignments
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    inlineInitialAssignments.out.xml
)


add_test(NAME test_java_printAnnotation
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printAnnotation
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_printMath
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printMath
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_printNotes
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printNotes
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_printRegisteredPackages
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printRegisteredPackages
)

add_test(NAME test_java_printSBML
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printSBML
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_printsupported
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printsupported
)

add_test(NAME test_java_printUnits
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    printUnits
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_promoteParameters
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    promoteParameters
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    promoteParameters.out.xml
)

add_test(NAME test_java_readSBML
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    readSBML
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)

add_test(NAME test_java_replaceOneFD
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    replaceOneFD
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/functiondef.xml
    f
    reaction_1    
    replaceOneFD.out.xml
)

add_test(NAME test_java_setIdFromNames
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    setIdFromNames
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    setIdFromNames.out.xml
)

 if(ENABLE_FBC) 

add_test(NAME test_java_stripPackage
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    stripPackage
    ${CMAKE_SOURCE_DIR}/examples/sample-models/fbc/fbc_example1.xml
    fbc
    stripPackage.out.xml
)

endif(ENABLE_FBC)

add_test(NAME test_java_unsetAnnotation
    COMMAND ${Java_JAVA_EXECUTABLE} 
    -cp ".${FILE_SEP}${CURRENT_JAR}${FILE_SEP}"
    -Djava.library.path=$<TARGET_FILE_DIR:binding_java_lib>
    unsetAnnotation
    ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
    unsetAnnotation.out.xml
)
