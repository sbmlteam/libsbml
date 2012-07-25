####################################################################
#
# CMake Build Script for libsbml c++ examples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${CMAKE_SOURCE_DIR}/include)

foreach(example 

	addCVTerms
	addCustomValidator
	addingEvidenceCodes_1
	addingEvidenceCodes_2
	addModelHistory
	appendAnnotation
	callExternalValidator
	convertSBML
	createExampleSBML
	echoSBML
	printAnnotation
	printMath
	printNotes
	printSBML
	printRegisteredPackages
	printSupported
	printUnits
	readSBML
	translateMath
	translateL3Math
	unsetAnnotation
	unsetNotes
	validateSBML

)
	add_executable(example_cpp_${example} ${example}.cpp util.c)
	set_target_properties(example_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_cpp_${example} ${LIBSBML_LIBRARY}-static)

endforeach()
