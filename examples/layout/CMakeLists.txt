###################################################################
#
# CMake Build Script for libsbml layoutexamples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/src)

foreach(example 

	addLayout
	example1-L3
	example1
	example2-L3
	example2
	example3-L3
	example3

)
	add_executable(example_layout_${example} ${example}.cpp)
	set_target_properties(example_layout_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_layout_${example} ${LIBSBML_LIBRARY}-static)

endforeach()


add_executable(example_layout_convertLayout convertLayout.cpp)
set_target_properties(example_layout_convertLayout PROPERTIES  OUTPUT_NAME convertLayout)

if(ENABLE_RENDER)
  # if we have the render extension available have the example also 
  # convert the render information
  add_definitions(-DCONVERT_RENDER)
endif(USE_RENDER)

target_link_libraries(example_layout_convertLayout convertLayout-static)

