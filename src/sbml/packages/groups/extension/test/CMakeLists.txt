####################################################################
#
# CMake Build Script for groups extension
#
# $Author$
# $Id$
# $HeadURL$
#
include(${LIBSBML_ROOT_SOURCE_DIR}/common.cmake)

file(GLOB C_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

foreach(c_file ${C_FILES})
	set_source_files_properties(${c_file} PROPERTIES LANGUAGE CXX)
endforeach()

file(GLOB CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )

set(TEST_FILES ${CPP_FILES} ${C_FILES})

include_directories(BEFORE ${LIBSBML_ROOT_SOURCE_DIR}/src )
include_directories(${LIBCHECK_INCLUDE_DIR})
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)
add_executable(test_extension_package_groups ${TEST_FILES})
target_link_libraries(test_extension_package_groups ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_extension_package_groups_run ${CMAKE_CURRENT_BINARY_DIR}/test_extension_package_groups )
set_tests_properties(test_extension_package_groups_run  PROPERTIES ENVIRONMENT 
	"srcdir=${CMAKE_CURRENT_SOURCE_DIR}")
