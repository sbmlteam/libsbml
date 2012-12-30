####################################################################
#
# CMake Build Script for libsbml c++ examples
#
# 


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(BEFORE ${CMAKE_SOURCE_DIR}/src)
include_directories(${CMAKE_SOURCE_DIR}/include)

foreach(example 

	addCVTerms
	addCustomValidator
	addingEvidenceCodes_1
	addingEvidenceCodes_2
	addModelHistory
	appendAnnotation
	callExternalValidator
	convertSBML
	createExampleSBML
	echoSBML
	printAnnotation
	printMath
	printNotes
	printSBML
	printRegisteredPackages
	printSupported
	printUnits
	readSBML
	stripPackage
	translateMath
	translateL3Math
	unsetAnnotation
	unsetNotes
	validateSBML
	
)
	add_executable(example_cpp_${example} ${example}.cpp util.c)
	set_target_properties(example_cpp_${example} PROPERTIES  OUTPUT_NAME ${example})
	target_link_libraries(example_cpp_${example} ${LIBSBML_LIBRARY}-static)
	
	if (WITH_LIBXML)
		target_link_libraries(example_cpp_${example} ${LIBXML_LIBRARY} ${EXTRA_LIBS})
	endif()

	if (WITH_ZLIB)
		target_link_libraries(example_cpp_${example} ${LIBZ_LIBRARY})
	endif(WITH_ZLIB)
	if (WITH_BZIP2)
		target_link_libraries(example_cpp_${example} ${LIBBZ_LIBRARY})
	endif(WITH_BZIP2)

endforeach()

if (WITH_LIBXML)
  include_directories(${LIBXML_INCLUDE_DIR})
  add_executable(example_cpp_rngvalidator rngvalidator.cpp util.c)
  set_target_properties(example_cpp_rngvalidator PROPERTIES  OUTPUT_NAME rngvalidator)
  target_link_libraries(example_cpp_rngvalidator ${LIBSBML_LIBRARY}-static)
  target_link_libraries(example_cpp_rngvalidator ${LIBXML_LIBRARY} ${EXTRA_LIBS})
  if (WITH_ZLIB)
    target_link_libraries(example_cpp_rngvalidator ${LIBZ_LIBRARY})
  endif(WITH_ZLIB)
  if (WITH_BZIP2)
    target_link_libraries(example_cpp_rngvalidator ${LIBBZ_LIBRARY})
  endif(WITH_BZIP2)
endif()
