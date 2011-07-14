####################################################################
#
# CMake Build Script for libsbml util tests
#
# 

file(GLOB TEST_FILES_UTIL ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_sbml_util ${TEST_FILES_UTIL})

if (NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/TestUtil.c)
# the test checks for the present of a file so lets create it if necessary
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/TestUtil.c "// test")

endif()

target_link_libraries(test_sbml_util ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_sbml_util_run ${CMAKE_CURRENT_BINARY_DIR}/test_sbml_util )

