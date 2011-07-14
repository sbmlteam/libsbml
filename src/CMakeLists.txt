###############################################################################
#
# 
#
# Description       : CMake build script for libSBML 
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2009-2011 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#
###############################################################################


###############################################################################
#
# create libsbml-config-common.h
# 
include(CheckIncludeFiles)
check_include_files (check.h HAVE_CHECK_H)
check_include_files (expat.h HAVE_EXPAT_H)
check_include_files (errno.h HAVE_ERRNO_H)
check_include_files (ieeefp.h HAVE_IEEEFP_H)
check_include_files (math.h HAVE_MATH_H)
check_include_files (sys/types.h HAVE_SYS_TYPES_H)
check_include_files (float.h STDC_HEADERS)
check_include_files (stdarg.h STDC_HEADERS)
check_include_files (stdlib.h STDC_HEADERS)
check_include_files (string.h STDC_HEADERS)

set(WORDS_BIGENDIAN)
if (UNIX)
  include (TestBigEndian)
  test_big_endian(WORDS_BIGENDIAN)
else()
  # test_big_endian seems to fail with nmake (VS 2010) on windows
  # however, windows is always little endian, so catch this here
  set(WORDS_BIGENDIAN FALSE)
endif()

# for whatever reason some of the values are not correctly picked up
# so we encode these values here
if (MSVC)
  set(HAVE_ERRNO_H TRUE)
  set(HAVE_IEEEFP_H TRUE)
  set(STDC_HEADERS TRUE)
  set(HAVE_MATH_H TRUE)
  set(HAVE_SYS_TYPES_H TRUE)
endif()


include(CheckLibraryExists)
check_library_exists(m sqrt "" HAVE_LIBM)

# generate configuration for this system
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/sbml/common/libsbml-config-common.h.cmake
	${CMAKE_CURRENT_SOURCE_DIR}/sbml/common/libsbml-config-common.h
	)

# Replace: @PACKAGE_VERSION@ and @LIBSBML_VERSION_NUMERIC@
configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/sbml/common/libsbml-version.h.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/sbml/common/libsbml-version.h"
  )

	
###############################################################################
#
# mark header files for installation
# 
foreach(dir annotation common compress extension math units util validator xml  )

	file(GLOB header_files "${CMAKE_CURRENT_SOURCE_DIR}/sbml/${dir}/*.h")
	install(FILES ${header_files} DESTINATION include/sbml/${dir})

endforeach(dir)

file(GLOB files "${CMAKE_CURRENT_SOURCE_DIR}/sbml/*.h")
install(FILES ${files} DESTINATION include/sbml)

###############################################################################
#
# specify include directories
# 
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
include_directories(${CMAKE_SOURCE_DIR}/src/sbml)
include_directories(${CMAKE_SOURCE_DIR}/src)


###############################################################################
#
# Find all sources 
#
set(LIBSBML_SOURCES)

foreach (directory annotation common extension math sbml 
					units util 
					validator validator/constraints)
	
	set(prefix "sbml/")
	
	if ("${directory}" STREQUAL "sbml")
		set(prefix "")
	endif()
	
	
	file(GLOB temp 
			${prefix}${directory}/*.cpp 
			${prefix}${directory}/*.c 
			${prefix}${directory}/*.h)

	# remove the constraint files as they don't have to be compiled
	# since they don't contain symbols but rather are included directly
	if ("${directory}" STREQUAL "validator/constraints")
	  foreach(tempFile ${temp})
		if ("${tempFile}" MATCHES ".*Constraints.cpp")
		  list(REMOVE_ITEM temp "${tempFile}")
		endif()
	  endforeach()
	endif()

	# create source group for IDEs
	source_group(${directory} FILES ${temp})

	# add to libsbml sources
	set(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${temp})
	
endforeach()



###############################################################################
#
# Include all packages
#
file(GLOB PACKAGE_OPTIONS "*package.cmake")
foreach(package ${PACKAGE_OPTIONS})

	include(${package})

endforeach()

###############################################################################
#
# Find compression sources and adjust include and lib directory
#	
set (COMPRESS_SOURCES)	
set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
	sbml/compress/CompressCommon.h 
	sbml/compress/CompressCommon.cpp 
	sbml/compress/InputDecompressor.cpp 
	sbml/compress/InputDecompressor.h 
	sbml/compress/OutputCompressor.cpp
	sbml/compress/OutputCompressor.h
	)

if(WITH_BZIP2)

  set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
		sbml/compress/bzfstream.h
		sbml/compress/bzfstream.cpp
		)	
  include_directories(${LIBBZ_INCLUDE_DIR})
  set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBBZ_LIBRARY})

endif()

if(WITH_ZLIB)

set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
		sbml/compress/zip.c 
		sbml/compress/zip.h 
		sbml/compress/unzip.h 
		sbml/compress/unzip.c 
		sbml/compress/ioapi.h 		
		sbml/compress/ioapi.c 		
		sbml/compress/zfstream.h
		sbml/compress/zfstream.cpp
		sbml/compress/zipfstream.cpp		
		sbml/compress/zipfstream.h		
	)	

	if (WIN32)
		#
		# compress/iowin32.c contains only symbols on WIN32 if we include
		# it on other platforms a warning is issued by ranlib about 
		# no symbols. So it will only be included on windows. 
		#
		set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
			sbml/compress/iowin32.c 
			sbml/compress/iowin32.h 
		)
	endif()
		
	include_directories(${LIBZ_INCLUDE_DIR})
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBZ_LIBRARY})

endif()

source_group(compress FILES ${COMPRESS_SOURCES})
set(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${COMPRESS_SOURCES})

###############################################################################
#
# Find xml sources and adjust include and lib directory
#	

set(XML_SOURCES)
set(XML_SOURCES ${XML_SOURCES} 
  sbml/xml/XMLAttributes.cpp    
  sbml/xml/XMLBuffer.cpp        
  sbml/xml/XMLError.cpp         
  sbml/xml/XMLErrorLog.cpp      
  sbml/xml/XMLFileBuffer.cpp    
  sbml/xml/XMLHandler.cpp       
  sbml/xml/XMLInputStream.cpp   
  sbml/xml/XMLMemoryBuffer.cpp  
  sbml/xml/XMLNamespaces.cpp    
  sbml/xml/XMLNode.cpp          
  sbml/xml/XMLOutputStream.cpp  
  sbml/xml/XMLParser.cpp        
  sbml/xml/XMLToken.cpp         
  sbml/xml/XMLTokenizer.cpp     
  sbml/xml/XMLTriple.cpp
  sbml/xml/XMLAttributes.h    
  sbml/xml/XMLBuffer.h        
  sbml/xml/XMLError.h         
  sbml/xml/XMLErrorLog.h      
  sbml/xml/XMLFileBuffer.h    
  sbml/xml/XMLHandler.h       
  sbml/xml/XMLInputStream.h   
  sbml/xml/XMLMemoryBuffer.h  
  sbml/xml/XMLNamespaces.h    
  sbml/xml/XMLNode.h          
  sbml/xml/XMLOutputStream.h  
  sbml/xml/XMLParser.h        
  sbml/xml/XMLToken.h         
  sbml/xml/XMLTokenizer.h     
  sbml/xml/XMLTriple.h
)

if(WITH_EXPAT)

	set(XML_SOURCES ${XML_SOURCES} 
		sbml/xml/ExpatAttributes.cpp 
		sbml/xml/ExpatHandler.cpp    
		sbml/xml/ExpatParser.cpp	
		sbml/xml/ExpatAttributes.h 
		sbml/xml/ExpatHandler.h    
		sbml/xml/ExpatParser.h	
	)
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBEXPAT_LIBRARY})
	include_directories(${LIBEXPAT_INCLUDE_DIR})

endif(WITH_EXPAT)

if(WITH_LIBXML)

	set(XML_SOURCES ${XML_SOURCES} 
		sbml/xml/LibXMLAttributes.cpp 
		sbml/xml/LibXMLHandler.cpp   
		sbml/xml/LibXMLNamespaces.cpp
		sbml/xml/LibXMLParser.cpp
		sbml/xml/LibXMLTranscode.cpp	
		sbml/xml/LibXMLAttributes.h 
		sbml/xml/LibXMLHandler.h   
		sbml/xml/LibXMLNamespaces.h
		sbml/xml/LibXMLParser.h
		sbml/xml/LibXMLTranscode.h	
	)
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBXML_LIBRARY})
	include_directories(${LIBXML_INCLUDE_DIR})

endif(WITH_LIBXML)

if(WITH_XERCES)
	
	set(XML_SOURCES ${XML_SOURCES} 
		sbml/xml/XercesAttributes.cpp 
		sbml/xml/XercesHandler.cpp    
		sbml/xml/XercesNamespaces.cpp 
		sbml/xml/XercesParser.cpp 
		sbml/xml/XercesTranscode.cpp
		sbml/xml/XercesAttributes.h 
		sbml/xml/XercesHandler.h    
		sbml/xml/XercesNamespaces.h 
		sbml/xml/XercesParser.h 
		sbml/xml/XercesTranscode.h
	)

	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${XERCES_LIBRARY})
	include_directories(${XERCES_INCLUDE_DIR})

	
endif(WITH_XERCES)

source_group(xml FILES ${XML_SOURCES})
set(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${XML_SOURCES})

###############################################################################
#
# this is a directory level operation!
#
if (MSVC)
if (WITH_STATIC_RUNTIME)
	foreach(flag_var
		CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
		CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
		CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
		CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)

		if(${flag_var} MATCHES "/MD")
			string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
		endif(${flag_var} MATCHES "/MD")
		
		
	endforeach(flag_var)
	add_definitions( -D_MT)
endif(WITH_STATIC_RUNTIME)
endif(MSVC)

###############################################################################
#
# Build library
#
add_library (${LIBSBML_LIBRARY} SHARED ${LIBSBML_SOURCES} )
target_link_libraries(${LIBSBML_LIBRARY} ${LIBSBML_LIBS} ${EXTRA_LIBS})

INSTALL(TARGETS ${LIBSBML_LIBRARY} 
	RUNTIME DESTINATION bin
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
  )

add_library (${LIBSBML_LIBRARY}-static STATIC ${LIBSBML_SOURCES} )
target_link_libraries(${LIBSBML_LIBRARY}-static ${LIBSBML_LIBS} ${EXTRA_LIBS})

INSTALL(TARGETS ${LIBSBML_LIBRARY}-static 
	RUNTIME DESTINATION bin
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
  )

##############################################################################
#
# add native tests
# 
add_subdirectory(sbml)


##############################################################################
#
# create language bindings
# 
add_subdirectory(bindings)
