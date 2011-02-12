###############################################################################
#
# $URL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/CMakeLists.txt $
# $Id: CMakeLists.txt 12544 2011-01-07 22:59:56Z luciansmith $
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

#
# Update version information from files
# 

# Replace: @PACKAGE_VERSION@ and @LIBSBML_VERSION_NUMERIC@
configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/common/libsbml-version.h.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/common/libsbml-version.h"
  )
include_directories(${CMAKE_CURRENT_BINARY_DIR}/common)

###############################################################################
#
# on UNIX systems create libsbml-config-unix.h
# 
if(UNIX)
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

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/common/libsbml-config-unix.h.cmake ${CMAKE_CURRENT_SOURCE_DIR}/common/libsbml-config-unix.h)

endif()

###############################################################################
#
# Begin by copying all header files to the include directory
#
include(${CMAKE_SOURCE_DIR}/common.cmake)

foreach(dir annotation common compress math units util validator xml  )

	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/${dir})
	copy_files( ${CMAKE_CURRENT_SOURCE_DIR}/${dir}/ 
				${CMAKE_CURRENT_SOURCE_DIR}/../include/sbml/${dir} *.h )
	FILE(GLOB header_files "${CMAKE_CURRENT_SOURCE_DIR}/${dir}/*.h")
	INSTALL(FILES ${header_files} DESTINATION include/sbml/${dir})

endforeach(dir)

copy_files( ${CMAKE_CURRENT_SOURCE_DIR}/sbml/ 
				${CMAKE_CURRENT_SOURCE_DIR}/../include/sbml *.h )

copy_files( ${CMAKE_CURRENT_SOURCE_DIR}/sbml/layout 
				${CMAKE_CURRENT_SOURCE_DIR}/../include/sbml/layout *.h )

				
FILE(GLOB files "${CMAKE_CURRENT_SOURCE_DIR}/sbml/*.h")
INSTALL(FILES ${files} DESTINATION include/sbml)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/validator/constraints)

###############################################################################
#
# Find all sources and add compression or xml parsers as needed
#
set(LIBSBML_SOURCES)
foreach (directory annotation common extension math sbml sbml/layout 
					units util 
					validator validator/constraints)
	
	file(GLOB temp ${directory}/*.cpp ${directory}/*.c ${directory}/*.h)

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
	
set (COMPRESS_SOURCES)	
# add compression sources
set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
	compress/CompressCommon.h 
	compress/CompressCommon.cpp 
	compress/InputDecompressor.cpp 
	compress/InputDecompressor.h 
	compress/OutputCompressor.cpp
	compress/OutputCompressor.h
	)

if(WITH_BZIP2)

  set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
		compress/bzfstream.h
		compress/bzfstream.cpp
		)	
  include_directories(${LIBBZ_INCLUDE_DIR})
  set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBBZ_LIBRARY})

endif()

if(WITH_ZLIB)

	set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
		compress/zip.c 
		compress/zip.h 
		compress/unzip.h 
		compress/unzip.c 
		compress/ioapi.h 		
		compress/ioapi.c 		
		compress/zfstream.h
		compress/zfstream.cpp
		compress/zipfstream.cpp		
		compress/zipfstream.h		
	)	

	if (WIN32)
		#
		# compress/iowin32.c contains only symbols on WIN32 if we include
		# it on other platforms a warning is issued by ranlib about 
		# no symbols. So it will only be included on windows. 
		#
		set(COMPRESS_SOURCES ${COMPRESS_SOURCES} 
			compress/iowin32.c 
			compress/iowin32.h 
		)
	endif()
		
	include_directories(${LIBZ_INCLUDE_DIR})
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBZ_LIBRARY})

endif()

source_group(compress FILES ${COMPRESS_SOURCES})
set(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${COMPRESS_SOURCES})

# add xml sources
set(XML_SOURCES)
set(XML_SOURCES ${XML_SOURCES} 
  xml/XMLAttributes.cpp    
  xml/XMLBuffer.cpp        
  xml/XMLError.cpp         
  xml/XMLErrorLog.cpp      
  xml/XMLFileBuffer.cpp    
  xml/XMLHandler.cpp       
  xml/XMLInputStream.cpp   
  xml/XMLMemoryBuffer.cpp  
  xml/XMLNamespaces.cpp    
  xml/XMLNode.cpp          
  xml/XMLOutputStream.cpp  
  xml/XMLParser.cpp        
  xml/XMLToken.cpp         
  xml/XMLTokenizer.cpp     
  xml/XMLTriple.cpp
  xml/XMLAttributes.h    
  xml/XMLBuffer.h        
  xml/XMLError.h         
  xml/XMLErrorLog.h      
  xml/XMLFileBuffer.h    
  xml/XMLHandler.h       
  xml/XMLInputStream.h   
  xml/XMLMemoryBuffer.h  
  xml/XMLNamespaces.h    
  xml/XMLNode.h          
  xml/XMLOutputStream.h  
  xml/XMLParser.h        
  xml/XMLToken.h         
  xml/XMLTokenizer.h     
  xml/XMLTriple.h
)

if(WITH_EXPAT)

	set(XML_SOURCES ${XML_SOURCES} 
		xml/ExpatAttributes.cpp 
		xml/ExpatHandler.cpp    
		xml/ExpatParser.cpp	
		xml/ExpatAttributes.h 
		xml/ExpatHandler.h    
		xml/ExpatParser.h	
	)
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBEXPAT_LIBRARY})
	include_directories(${LIBEXPAT_INCLUDE_DIR})

endif(WITH_EXPAT)

if(WITH_LIBXML)

	set(XML_SOURCES ${XML_SOURCES} 
		xml/LibXMLAttributes.cpp 
		xml/LibXMLHandler.cpp   
		xml/LibXMLNamespaces.cpp
		xml/LibXMLParser.cpp
		xml/LibXMLTranscode.cpp	
		xml/LibXMLAttributes.h 
		xml/LibXMLHandler.h   
		xml/LibXMLNamespaces.h
		xml/LibXMLParser.h
		xml/LibXMLTranscode.h	
	)
	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${LIBXML_LIBRARY})
	include_directories(${LIBXML_INCLUDE_DIR})

endif(WITH_LIBXML)

if(WITH_XERCES)
	
	set(XML_SOURCES ${XML_SOURCES} 
		xml/XercesAttributes.cpp 
		xml/XercesHandler.cpp    
		xml/XercesNamespaces.cpp 
		xml/XercesParser.cpp 
		xml/XercesTranscode.cpp
		xml/XercesAttributes.h 
		xml/XercesHandler.h    
		xml/XercesNamespaces.h 
		xml/XercesParser.h 
		xml/XercesTranscode.h
	)

	set(LIBSBML_LIBS ${LIBSBML_LIBS} ${XERCES_LIBRARY})
	include_directories(${XERCES_INCLUDE_DIR})

	
endif(WITH_XERCES)

source_group(xml FILES ${XML_SOURCES})
set(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${XML_SOURCES})


###############################################################################
#
# Build library
#
add_library (${LIBSBML_LIBRARY} SHARED ${LIBSBML_SOURCES} )
target_link_libraries(${LIBSBML_LIBRARY} ${LIBSBML_LIBS})

INSTALL(TARGETS ${LIBSBML_LIBRARY} 
	RUNTIME DESTINATION bin
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
  )

add_library (${LIBSBML_LIBRARY}-static STATIC ${LIBSBML_SOURCES} )
target_link_libraries(${LIBSBML_LIBRARY}-static ${LIBSBML_LIBS})

INSTALL(TARGETS ${LIBSBML_LIBRARY}-static 
	RUNTIME DESTINATION bin
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
  )

  
###############################################################################
#
# Build test files and add tests
#
if(WITH_CHECK)

	add_subdirectory(annotation/test)	
	add_subdirectory(math/test)
	add_subdirectory(sbml/test)
	add_subdirectory(units/test)
	add_subdirectory(util/test)
	add_subdirectory(validator/test)
	add_subdirectory(xml/test)

endif()

# binding libraries in folders below
add_subdirectory(bindings)
