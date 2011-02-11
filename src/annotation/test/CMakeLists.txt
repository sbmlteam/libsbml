####################################################################
#
# CMake Build Script for libsbml annotation tests
#
# $Author$
# $Id$
# $HeadURL$
#
include(${CMAKE_SOURCE_DIR}/common.cmake)
copy_files( ${CMAKE_CURRENT_SOURCE_DIR}/test-data/ 
				${CMAKE_CURRENT_BINARY_DIR}/test-data * )

file(GLOB C_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

foreach(c_file ${C_FILES})
	set_source_files_properties(${c_file} PROPERTIES LANGUAGE CXX)
endforeach()

file(GLOB CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )

set(TEST_FILES ${CPP_FILES} ${C_FILES})
include_directories(${LIBCHECK_INCLUDE_DIR} ${CMAKE_SOURCE_DIR}/include/sbml)
include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_sbml_annotation ${TEST_FILES})
target_link_libraries(test_sbml_annotation ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_sbml_annotation_run ${CMAKE_CURRENT_BINARY_DIR}/test_sbml_annotation )
