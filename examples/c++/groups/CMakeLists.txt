####################################################################
#
# CMake Build Script for libsbml c++ examples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${LIBSBML_ROOT_SOURCE_DIR}/src)
include_directories(${LIBSBML_ROOT_SOURCE_DIR}/include)
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)

if (EXTRA_INCLUDE_DIRS) 
include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

foreach(example 

	groups_example1
	groups_example2
	
)
	add_executable(example_groups_cpp_${example} ${example}.cpp ../util.c)
	set_target_properties(example_groups_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_groups_cpp_${example} ${LIBSBML_LIBRARY}-static)

endforeach()
