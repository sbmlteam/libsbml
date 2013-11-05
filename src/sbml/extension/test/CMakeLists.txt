####################################################################
#
# CMake Build Script for libsbml extension tests
#
# 
include(${CMAKE_SOURCE_DIR}/common.cmake)

file(GLOB TEST_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
if (EXTRA_INCLUDE_DIRS) 
 include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_sbml_extension ${TEST_FILES})
target_link_libraries(test_sbml_extension ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_sbml_extension_run ${CMAKE_CURRENT_BINARY_DIR}/test_sbml_extension )
set_tests_properties(test_sbml_extension_run PROPERTIES ENVIRONMENT 
	"srcdir=${CMAKE_CURRENT_SOURCE_DIR}")


