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
    addCustomValidator
    addingEvidenceCodes_1
    addingEvidenceCodes_2
    addModelHistory
    appendAnnotation
    callExternalValidator
    convertSBML
    convertToL1V1
    createExampleSBML
    echoSBML
    inferUnits
    inlineFunctionDefintions
    convertReactions
    getAllElementsWithNotes
    printAnnotation
    printMath
    printNotes
    printSBML
    printRegisteredPackages
    printSupported
    printUnits
    promoteParameters
    readSBML
    renameSId
    setIdFromNames
    setNamesFromIds
    stripPackage
    translateMath
    translateL3Math
    unsetAnnotation
    unsetNotes
    validateSBML

)
    add_executable(example_cpp_${example} ${example}.cpp util.c)
    set_target_properties(example_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
    target_link_libraries(example_cpp_${example} ${LIBSBML_LIBRARY}-static)

    if (WITH_LIBXML)
        target_link_libraries(example_cpp_${example} ${LIBXML_LIBRARY} ${EXTRA_LIBS})
    endif()

    if (WITH_ZLIB)
        target_link_libraries(example_cpp_${example} ${LIBZ_LIBRARY})
    endif(WITH_ZLIB)
    if (WITH_BZIP2)
        target_link_libraries(example_cpp_${example} ${LIBBZ_LIBRARY})
    endif(WITH_BZIP2)

endforeach()

if (WITH_LIBXML)
  include_directories(${LIBXML_INCLUDE_DIR})
  add_executable(example_cpp_rngvalidator rngvalidator.cpp util.c)
  set_target_properties(example_cpp_rngvalidator PROPERTIES  OUTPUT_NAME rngvalidator)
  target_link_libraries(example_cpp_rngvalidator ${LIBSBML_LIBRARY}-static)
  target_link_libraries(example_cpp_rngvalidator ${LIBXML_LIBRARY} ${EXTRA_LIBS})
  if (WITH_ZLIB)
    target_link_libraries(example_cpp_rngvalidator ${LIBZ_LIBRARY})
  endif(WITH_ZLIB)
  if (WITH_BZIP2)
    target_link_libraries(example_cpp_rngvalidator ${LIBBZ_LIBRARY})
  endif(WITH_BZIP2)
endif()

# run examples as test

add_test(NAME test_cxx_addCVTerms
         COMMAND "$<TARGET_FILE:example_cpp_addCVTerms>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addCVTerms.out.xml
)
add_test(NAME test_cxx_addCustomValidator
         COMMAND "$<TARGET_FILE:example_cpp_addCustomValidator>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml

)
add_test(NAME test_cxx_addingEvidenceCodes_1
         COMMAND "$<TARGET_FILE:example_cpp_addingEvidenceCodes_1>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addingEvidenceCodes_1.out.xml
)
add_test(NAME test_cxx_addingEvidenceCodes_2
         COMMAND "$<TARGET_FILE:example_cpp_addingEvidenceCodes_2>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addingEvidenceCodes_2.out.xml
)
add_test(NAME test_cxx_addModelHistory
         COMMAND "$<TARGET_FILE:example_cpp_addModelHistory>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         addModelHistory.out.xml
)
add_test(NAME test_cxx_appendAnnotation
         COMMAND "$<TARGET_FILE:example_cpp_appendAnnotation>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         appendAnnotation.out.xml
)
add_test(NAME test_cxx_callExternalValidator
         COMMAND "$<TARGET_FILE:example_cpp_callExternalValidator>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         callExternalValidator.out.xml
)
add_test(NAME test_cxx_convertSBML
         COMMAND "$<TARGET_FILE:example_cpp_convertSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         convertSBML.out.xml
)
add_test(NAME test_cxx_convertToL1V1
         COMMAND "$<TARGET_FILE:example_cpp_convertToL1V1>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         convertToL1V1.out.xml
)
add_test(NAME test_cxx_createExampleSBML
         COMMAND "$<TARGET_FILE:example_cpp_createExampleSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         createExampleSBML.out.xml
)
add_test(NAME test_cxx_echoSBML
         COMMAND "$<TARGET_FILE:example_cpp_echoSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         echoSBML.out.xml
)
add_test(NAME test_cxx_inferUnits
         COMMAND "$<TARGET_FILE:example_cpp_inferUnits>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         inferUnits.out.xml
)
add_test(NAME test_cxx_inlineFunctionDefintions
         COMMAND "$<TARGET_FILE:example_cpp_inlineFunctionDefintions>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         inlineFunctionDefintions.out.xml
)
add_test(NAME test_cxx_convertReactions
         COMMAND "$<TARGET_FILE:example_cpp_convertReactions>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         convertReactions.out.xml
)
add_test(NAME test_cxx_getAllElementsWithNotes
         COMMAND "$<TARGET_FILE:example_cpp_getAllElementsWithNotes>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_printAnnotation
         COMMAND "$<TARGET_FILE:example_cpp_printAnnotation>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_printMath
         COMMAND "$<TARGET_FILE:example_cpp_printMath>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_printNotes
         COMMAND "$<TARGET_FILE:example_cpp_printNotes>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_printSBML
         COMMAND "$<TARGET_FILE:example_cpp_printSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_printRegisteredPackages
         COMMAND "$<TARGET_FILE:example_cpp_printRegisteredPackages>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         printRegisteredPackages.out.xml
)
add_test(NAME test_cxx_printSupported
         COMMAND "$<TARGET_FILE:example_cpp_printSupported>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         printSupported.out.xml
)
add_test(NAME test_cxx_printUnits
         COMMAND "$<TARGET_FILE:example_cpp_printUnits>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_promoteParameters
         COMMAND "$<TARGET_FILE:example_cpp_promoteParameters>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         promoteParameters.out.xml
)
add_test(NAME test_cxx_readSBML
         COMMAND "$<TARGET_FILE:example_cpp_readSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
add_test(NAME test_cxx_renameSId
         COMMAND "$<TARGET_FILE:example_cpp_renameSId>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         veq
         veqnew
         renameSId.out.xml
)
add_test(NAME test_cxx_setIdFromNames
         COMMAND "$<TARGET_FILE:example_cpp_setIdFromNames>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         setIdFromNames.out.xml
)
add_test(NAME test_cxx_setNamesFromIds
         COMMAND "$<TARGET_FILE:example_cpp_setNamesFromIds>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         setNamesFromIds.out.xml
)

 if(ENABLE_FBC) 

add_test(NAME test_cxx_stripPackage
         COMMAND "$<TARGET_FILE:example_cpp_stripPackage>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/fbc/fbc_example1.xml
         fbc
         stripPackage.out.xml
)

endif(ENABLE_FBC)

add_test(NAME test_cxx_unsetAnnotation
         COMMAND "$<TARGET_FILE:example_cpp_unsetAnnotation>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         unsetAnnotation.out.xml
)
add_test(NAME test_cxx_unsetNotes
         COMMAND "$<TARGET_FILE:example_cpp_unsetNotes>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         unsetNotes.out.xml
)
add_test(NAME test_cxx_validateSBML
         COMMAND "$<TARGET_FILE:example_cpp_validateSBML>"
         ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
)
