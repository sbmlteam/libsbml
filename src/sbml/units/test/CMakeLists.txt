####################################################################
#
# CMake Build Script for libsbml units tests
#
# 
include(${CMAKE_SOURCE_DIR}/common.cmake)

file(GLOB C_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

foreach(c_file ${C_FILES})
	set_source_files_properties(${c_file} PROPERTIES LANGUAGE CXX)
endforeach()

file(GLOB CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )

set(TEST_FILES ${CPP_FILES} ${C_FILES})

include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_sbml_units ${TEST_FILES})
target_link_libraries(test_sbml_units ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_sbml_units_run ${CMAKE_CURRENT_BINARY_DIR}/test_sbml_units )
set_tests_properties(test_sbml_units_run PROPERTIES ENVIRONMENT 
	"srcdir=${CMAKE_CURRENT_SOURCE_DIR}")
	
