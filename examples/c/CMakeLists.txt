####################################################################
#
# CMake Build Script for c examples
#
# $Author$
# $Id$
# $HeadURL$
#


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/include)


foreach(example 

	convertSBML
	drawMath
	echoSBML
	evaluateMath
	printMath
	printSBML
	readSBML
	translateMath
	validateSBML

)
	add_executable(example_c_${example} ${example}.c util.c)
	set_target_properties(example_c_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_c_${example} ${LIBSBML_LIBRARY}-static)
	install(TARGETS example_c_${example} DESTINATION bin)

endforeach()
