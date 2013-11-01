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

	flattenModel
	spec_example1
	spec_example2
	spec_example3
	spec_example4
	
)
	add_executable(example_comp_cpp_${example} ${example}.cpp ../util.c)
	set_target_properties(example_comp_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_comp_cpp_${example} ${LIBSBML_LIBRARY}-static)
	
	if (WITH_LIBXML)
		target_link_libraries(example_comp_cpp_${example} ${LIBXML_LIBRARY} ${EXTRA_LIBS})
	endif()

	if (WITH_ZLIB)
		target_link_libraries(example_comp_cpp_${example} ${LIBZ_LIBRARY})
	endif(WITH_ZLIB)
	if (WITH_BZIP2)
		target_link_libraries(example_comp_cpp_${example} ${LIBBZ_LIBRARY})
	endif(WITH_BZIP2)

endforeach()


# build web resolver example
# for which we need libcurl

find_library(LIBCURL_LIBRARY
  NAMES libcurl.lib curl
  PATHS /usr/lib /usr/local/lib
        ${LIBSBML_DEPENDENCY_DIR}/lib
  DOC "The file name of the curl library."
)
find_path(LIBCURL_INCLUDE_DIR
  NAMES curl/curl.h
  PATHS /usr/include /usr/local/include
        ${LIBSBML_DEPENDENCY_DIR}/include
  DOC "The directory containing the curl include files."
)

if (${LIBCURL_LIBRARY} AND ${LIBCURL_INCLUDE_DIR})
  include_directories(${CMAKE_CURRENT_SOURCE_DIR})
  include_directories(${LIBCURL_INCLUDE_DIR})
  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)
  add_definitions(-DCURL_STATICLIB)
  add_executable(example_comp_cpp_SBMLHttpResolverExample SBMLHttpResolverExample.cpp ../util.c)
  set_target_properties(example_comp_cpp_SBMLHttpResolverExample PROPERTIES  OUTPUT_NAME SBMLHttpResolverExample)
  target_link_libraries(example_comp_cpp_SBMLHttpResolverExample ${LIBSBML_LIBRARY}-static ${LIBCURL_LIBRARY})
  if (WITH_LIBXML)
  	target_link_libraries(example_comp_cpp_SBMLHttpResolverExample ${LIBXML_LIBRARY} ${EXTRA_LIBS})
  endif()
else()
  message(WARNING "comp example: The SBML HTTP Resolver example cannot be built as libcurl was not found.")
endif (${LIBCURL_LIBRARY} AND ${LIBCURL_INCLUDE_DIR})
