####################################################################
#
# CMake Build Script for libsbml C# examples
#
# 

foreach( example
	
	groups_example1
	
)

	ADD_CUSTOM_COMMAND(	
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${example}.exe
		COMMAND "${CSHARP_COMPILER}"
		ARGS -r:${CMAKE_CURRENT_BINARY_DIR}/../../../src/bindings/csharp/libsbmlcsP.dll
			-target:exe
			-out:${example}.exe
			${CSHARP_EXTRA_ARGS}
			${CMAKE_CURRENT_SOURCE_DIR}${PATH_SEP}${example}.cs
		MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${example}.cs
		COMMENT "Build C# example: ${example}") 
	
	add_custom_target(example_groups_cs_${example} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${example}.exe)
	add_dependencies(example_groups_cs_${example} binding_csharp_managed_lib)
endforeach()
