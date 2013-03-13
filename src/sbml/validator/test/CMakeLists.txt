####################################################################
#
# CMake Build Script for libsbml validator tests
#
# 

if(UNIX)
    if(APPLE)
		if(WITH_LIBXML)
			# this is an odd one
			add_definitions(-DBUGGY_APPLE_LIBXML)
		endif()
	endif()
endif()

include(${CMAKE_SOURCE_DIR}/common.cmake)

file(GLOB C_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

foreach(c_file ${C_FILES})
	set_source_files_properties(${c_file} PROPERTIES LANGUAGE CXX)
endforeach()

file(GLOB CPP_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )
file(GLOB H_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.h )

set(TEST_FILES ${CPP_FILES} ${C_FILES} ${H_FILES})


include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${LIBCHECK_INCLUDE_DIR})
add_executable(test_sbml_validator ${TEST_FILES})
target_link_libraries(test_sbml_validator ${LIBSBML_LIBRARY}-static ${LIBCHECK_LIBRARY})
add_test(test_sbml_validator_run ${CMAKE_CURRENT_BINARY_DIR}/test_sbml_validator 
		${CMAKE_CURRENT_SOURCE_DIR})


