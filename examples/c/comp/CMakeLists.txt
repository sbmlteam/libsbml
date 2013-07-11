####################################################################
#
# CMake Build Script for c examples
#
# 

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${CMAKE_SOURCE_DIR}/include)

if (EXTRA_INCLUDE_DIRS) 
include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

foreach(example 

	flattenModel
	spec_example1

)
	add_executable(example_comp_c_${example} ${example}.c ../util.c)
	set_target_properties(example_comp_c_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_comp_c_${example} ${LIBSBML_LIBRARY}-static)
	install(TARGETS example_comp_c_${example} DESTINATION bin)

endforeach()
