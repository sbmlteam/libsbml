####################################################################
#
# CMake Build Script for java examples
#
# 

find_package(Java COMPONENTS Development REQUIRED)

if (JAVA_COMPATIBILITY)
SET(COMPAT_ARGS "-source ${JAVA_COMPATIBILITY} -target ${JAVA_COMPATIBILITY}")
endif()

foreach(file 
		groups_example1
		)
	
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR} CURRENT_BIN)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/../../../src/bindings/java/libsbmlj.jar CURRENT_JAR )
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java CURRENT_FILE)
		
	add_custom_command(	
		OUTPUT ${file}.class
		COMMAND "${Java_JAVAC_EXECUTABLE}"
		ARGS -cp ".${FILE_SEP}\"${CURRENT_JAR}\"${FILE_SEP}${CMAKE_CURRENT_SOURCE_DIR}"
			 ${CURRENT_FILE}
			 ${COMPAT_ARGS}
			 -d ${CMAKE_CURRENT_BINARY_DIR}
		MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/../../../src/bindings/java/local.i"
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.java
		COMMENT "Build java example ${file}"
	) 
	
	add_custom_target(example_groups_java_${file} ALL 
		DEPENDS 
		${CMAKE_CURRENT_BINARY_DIR}/${file}.class 
		SOURCES 
		${CMAKE_CURRENT_SOURCE_DIR}/${file}.java 
	)
	
	add_dependencies(example_groups_java_${file} binding_java_jar)
	
endforeach()
