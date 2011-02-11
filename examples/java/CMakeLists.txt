####################################################################
#
# CMake Build Script for java examples
#
# $Author$
# $Id$
# $HeadURL$
#

find_package(Java COMPONENTS Development REQUIRED)

foreach(file 
		addCVTerms
		addModelHistory
		appendAnnotation
		convertSBML
		echoSBML
		evaluateMath
		printAnnotation
		printMath
		printNotes
		printSBML
		printUnits
		readSBML
		translateMath
		unsetAnnotation
		unsetNotes
		validateSBML
		)
	
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR} CURRENT_BIN)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/../../src/bindings/java/libsbmlj.jar CURRENT_JAR )
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java CURRENT_FILE)
		
	ADD_CUSTOM_COMMAND(	
		OUTPUT ${file}.class
		COMMAND "${Java_JAVAC_EXECUTABLE}"
		ARGS -cp ".${FILE_SEP}\"${CURRENT_JAR}\"${FILE_SEP}${CMAKE_CURRENT_SOURCE_DIR}"
			 ${CURRENT_FILE}
			 -d ${CMAKE_CURRENT_BINARY_DIR}
		MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/../../src/bindings/java/local.i"
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
		COMMENT "Build java example ${file}"
	) 
	add_custom_target(example_java_${file} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.class )
	add_dependencies(example_java_${file} binding_java_jar)
	
endforeach()
