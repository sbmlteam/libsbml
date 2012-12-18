####################################################################
#
# CMake Build Script for c examples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${CMAKE_SOURCE_DIR}/include)


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
	install(TARGETS example_c_${example} DESTINATION bin)

endforeach()
