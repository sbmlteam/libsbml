####################################################################
#
# CMake Build Script for libsbml c++ examples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${CMAKE_SOURCE_DIR}/include)

if (EXTRA_INCLUDE_DIRS) 
include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

foreach(example 

	addLayout
	convertLayout
	example1
	example2
	example2-L3
	example3
	example3-L3
	
)
	add_executable(example_layout_cpp_${example} ${example}.cpp ../util.c)
	set_target_properties(example_layout_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_layout_cpp_${example} ${LIBSBML_LIBRARY}-static)
	
	if (WITH_LIBXML)
		target_link_libraries(example_layout_cpp_${example} ${LIBXML_LIBRARY} ${EXTRA_LIBS})
	endif()

	if (WITH_ZLIB)
		target_link_libraries(example_layout_cpp_${example} ${LIBZ_LIBRARY})
	endif(WITH_ZLIB)
	if (WITH_BZIP2)
		target_link_libraries(example_layout_cpp_${example} ${LIBBZ_LIBRARY})
	endif(WITH_BZIP2)

endforeach()


