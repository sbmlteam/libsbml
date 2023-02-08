###############################################################################
#
# Description       : CMake build script for libSBML Octave bindings
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

if(WITH_OCTAVE)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}")
find_package(Octave)

if (NOT EXISTS "${MKOCTFILE_EXECUTABLE}")
	# fail if mkoctfile was not found
	message (FATAL_ERROR "The mkoctfile command is vital for building octave bindings. It was not found during configuration. Please manually specify MKOCTFILE_EXECUTABLE.")
endif()


# get location of library
set(sbml_location $<TARGET_LINKER_FILE:${LIBSBML_LIBRARY}>)
set(sbml_path $<TARGET_FILE_DIR:${LIBSBML_LIBRARY}>)

# break up the libsbml library dependencies into paths and libraries
set(OCTAVE_EXTRA_LIBS)
set(OCTAVE_EXTRA_PATHS)
if (UNIX)
foreach(lib ${LIBSBML_LIBS})

  string(FIND "${lib}" "::" index)

  if (index GREATER 0)
    # found dependent library
    set(name $<TARGET_LINKER_FILE_BASE_NAME:${lib}>)
    set(path $<TARGET_FILE_DIR:${lib}>)
  else()
    get_filename_component(name ${lib} NAME_WE)
    get_filename_component(path ${lib} PATH)
    string(REGEX REPLACE "lib" "" name "${name}")
  endif()

  string(FIND "${name}" "::" index)

  if (index EQUAL -1)

    list(APPEND OCTAVE_EXTRA_LIBS -l${name})
    list(APPEND OCTAVE_EXTRA_PATHS -L${path})

  endif()
endforeach()
endif()

# 
# Determine the octave installation directory
#
set(OCTAVE_PACKAGE_INSTALL_DIR)
if (UNIX OR CYGWIN) 
execute_process(COMMAND "${OCTAVE_CONFIG_EXECUTABLE}" -p CANONICAL_HOST_TYPE
    OUTPUT_VARIABLE OCTAVE_PLATFORM OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(OCTAVE_PACKAGE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/octave/site/oct/${OCTAVE_PLATFORM})
else()
  set(OCTAVE_PACKAGE_INSTALL_DIR ${MISC_PREFIX}bindings/octave)
endif()

SET(COMMON_FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/ModelDetails.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/StructureFields.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/CommonFunctions.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/Filenames.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/../matlab/InputOutput.cpp"	)	

macro(build_octave_file fileName)
	# compile and link mex file
	ADD_CUSTOM_COMMAND(	
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.mex
		COMMAND "${CMAKE_COMMAND}"
		ARGS  -E remove ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.mex
		COMMAND "${MKOCTFILE_EXECUTABLE}"
		ARGS 
			-o ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.mex
			--mex
			-DUSE_OCTAVE		 
			-I${CMAKE_CURRENT_BINARY_DIR}/../../
			-I${CMAKE_CURRENT_SOURCE_DIR}/../../
			-I${LIBSBML_ROOT_SOURCE_DIR}/include
			${CMAKE_CURRENT_SOURCE_DIR}/../matlab/${fileName}.cpp
			${COMMON_FILES}
			-l${LIBSBML_LIBRARY}
			${OCTAVE_EXTRA_LIBS}
			${OCTAVE_EXTRA_PATHS}
			-L${sbml_path}
	
		MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/${fileName}.cpp
		COMMENT "compile and link octave mex file: ${fileName}") 
	
	add_custom_target(binding_octave_${fileName} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.mex ${LIBSBML_LIBRARY})
	add_dependencies(binding_octave_${fileName} ${LIBSBML_LIBRARY})
	
	# mark files
	install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${fileName}.mex DESTINATION ${OCTAVE_PACKAGE_INSTALL_DIR} )

endmacro()

build_octave_file("TranslateSBML")
build_octave_file("OutputSBML")


foreach(mFile
  test.xml
  addLevelVersion.m
  applyUserValidation.m
  CheckAndConvert.m
  Contents.m
  ConvertFormulaToMathML.m
  getDefaultValues.m
  getSBMLDefaultStruct.m
  getStructure.m
  getStructureEnum.m
  getStructureFieldnames.m
  getValueType.m
  installSBML.m
  isEnabled.m
  isFbcEnabled.m
  isoctave.m
  isSBML_Model.m
  OutputSBML.m
  TranslateSBML.m
)

	install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/${mFile} DESTINATION ${OCTAVE_PACKAGE_INSTALL_DIR} )

endforeach()

# add test files
foreach(testfiles
  compareFiles.m
  runTests.m
  testBinding.m
  testCVTerms.m
  testGetSBMLDefaultStruct.m
  testIsSBMLModel.m
  testMissingOutput.m
  testOutput.m
  testReadFlags.m
  testReadFromFile1.m
  testReadFromFile10.m
  testReadFromFile11.m
  testReadFromFile12.m
  testReadFromFile13.m
  testReadFromFile14.m
  testReadFromFile15.m
  testReadFromFile16.m
  testReadFromFile2.m
  testReadFromFile3.m
  testReadFromFile4.m
  testReadFromFile5.m
  testReadFromFile6.m
  testReadFromFile7.m
  testReadFromFile8.m
  testReadFromFile9.m
  testReadFromFileFbc1.m
  testReadFromFileFbc2.m
  testReadFromFileFbc3.m
  testReadFromFileFbc4.m
  testReadWriteGeneProductIds.m
  testReadWriteGeneProducts.m
  testReadFbcKeyValuePair.m
  testAddKeyValuePairs.m
  testVersionInformation.m)

	install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/test/${testfiles} DESTINATION ${OCTAVE_PACKAGE_INSTALL_DIR}/test )

endforeach()

foreach(testxmlfiles
algebraicRules.xml
both.xml
convertedFormulas.xml
convertedFormulas2.xml
convertedFormulasL2.xml
createdAnnotation.xml
createdAnnotationL2.xml
csymbolAvogadro.xml
csymbolDelay.xml
csymbolTime-reaction-l2.xml
csymbolTime.xml
errors.xml
fatal.xml
fbc.xml
fbcL3V2V1.xml
fbcL3V2V2.xml
fbcV2.xml
fbcV2Labels.xml
fbc_groups.xml
fbc_qual.xml
fbc_qual_groups.xml
fbcV3.xml
fbcV3_1.xml
fbc_kvp.xml
funcDefsWithInitialAssignments.xml
functionDefinition.xml
groups-example1.xml
groups_qual.xml
initialAssignments.xml
l1v1-branch.xml
l1v1-minimal.xml
l1v1-rules.xml
l1v1-units.xml
l1v1.xml
l1v2-all.xml
l2v1-all.xml
l2v1-allelements.xml
l2v1-assignment.xml
l2v2-all.xml
l2v2-newComponents.xml
l2v2-newelements.xml
l2v3-all.xml
l2v3-newelements.xml
l2v3-newMath.xml
l2v4-all.xml
l2v5-all.xml
l3v1core.xml
l3v2-empty-event.xml
l3v2-empty-math.xml
l3v2-newmath.xml
l3v2-no-model.xml
l3v2core.xml
math_no_arguments.xml
missing_out.xml
nestedPiecewise.xml
none.xml
nonLogicals.xml
notes_annotations.xml
outGP00.xml
outGP01.xml
outGP10.xml
outGP11.xml
piecewise.xml
qual.xml
rateRules.xml
readerror.xml
sparseStoichiometry.xml
species.xml
test-greek.xml
warn.xml
)

	install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../matlab/test/test-data/${testxmlfiles} DESTINATION ${OCTAVE_PACKAGE_INSTALL_DIR}/test/test-data )

endforeach()


endif()


