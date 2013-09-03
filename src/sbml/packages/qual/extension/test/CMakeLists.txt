####################################################################
#
# CMake Build Script for qual extension
#
# $Author$
# $Id$
# $HeadURL$
#
include(${CMAKE_SOURCE_DIR}/common.cmake)

file(GLOB C_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

foreach(c_file ${C_FILES})
	set_source_files_properties(${c_file} PROPERTIES LANGUAGE CXX)
endforeach()

file(GLOB CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )

set(TEST_FILES ${CPP_FILES} ${C_FILES})

include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src )
include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_extension_package_qual ${TEST_FILES})
target_link_libraries(test_extension_package_qual ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_extension_package_qual_run ${CMAKE_CURRENT_BINARY_DIR}/test_extension_package_qual )
set_tests_properties(test_extension_package_qual_run  PROPERTIES ENVIRONMENT 
	"srcdir=${CMAKE_CURRENT_SOURCE_DIR}")
