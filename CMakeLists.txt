###############################################################################
#
# $URL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/CMakeLists.txt $
# $Id: CMakeLists.txt 12493 2010-12-30 20:09:41Z fbergmann $
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

cmake_minimum_required(VERSION 2.8)
project(libsbml)

include (CMakeTestCCompiler)
include (CheckCSourceCompiles)
include (CheckCXXSourceCompiles)
include (CheckStructHasMember)
include (CheckLibraryExists)
include (CheckFunctionExists)
include (CheckCCompilerFlag)
include (CheckCSourceRuns)
include (CheckSymbolExists)
include (CheckTypeSize)


###############################################################################
#
# Parse VERSION.txt to determine the package version
#
SET(LIBSBML_VERSION_MAJOR)
SET(LIBSBML_VERSION_MINOR)
SET(LIBSBML_VERSION_PATCH)
SET(LIBSBML_VERSION_RELEASE)

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/VERSION.txt")

	file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/VERSION.txt" VersionString NEWLINE_CONSUME)
	string(STRIP "${VersionString}" VersionString)
	string(REPLACE "." ";" VersionString "${VersionString}" )
	string(REPLACE "-" ";" VersionString "${VersionString}" )
	list(LENGTH VersionString versionLength)
	list(GET VersionString 0 LIBSBML_VERSION_MAJOR )
	list(GET VersionString 1 LIBSBML_VERSION_MINOR )
	list(GET VersionString 2 LIBSBML_VERSION_PATCH )
	
	if (${versionLength} GREATER 3)
		list(GET VersionString 3 LIBSBML_VERSION_RELEASE )
	endif()	
	
endif()


SET(LIBSBML_VERSION_NUMERIC 50000)
SET(PACKAGE_VERSION "${LIBSBML_VERSION_MAJOR}.${LIBSBML_VERSION_MINOR}.${LIBSBML_VERSION_PATCH}${LIBSBML_VERSION_RELEASE}")
SET(PACKAGE_NAME "libSBML")


# configure the version for documentation / bindings
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/docs/src/common-text/libsbml-version.html.in
	${CMAKE_CURRENT_SOURCE_DIR}/docs/src/common-text/libsbml-version.html
)

###############################################################################
#
# the next lines configure the parameters for packaging the binaries
# they can be invoked with: make package / nmake package or by using 
# cpack -G zip|deb|rpm|dmg|nsis
#

INCLUDE(InstallRequiredSystemLibraries)

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "An API library for reading/writing/manipulating SBML")
SET(CPACK_PACKAGE_NAME "${PACKAGE_NAME}")
SET(CPACK_PACKAGE_VENDOR "The SBML Team")
SET(CPACK_PACKAGE_CONTACT "LibSBML Team <libsbml-team@caltech.edu>")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.txt")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.txt")
SET(CPACK_PACKAGE_VERSION_MAJOR "${LIBSBML_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${LIBSBML_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${LIBSBML_VERSION_PATCH}")
SET(CPACK_RPM_PACKAGE_LICENSE "LGPL")
SET(CPACK_RPM_PACKAGE_GROUP "Libraries/Development")
SET(CPACK_DEBIAN_PACKAGE_SECTION "Libraries")

if (UNIX)
 SET(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
 SET(CPACK_SET_DESTDIR "ON") 
endif()

if (CMAKE_SIZEOF_VOID_P EQUAL 4)
  SET (CPACK_DEBIAN_PACKAGE_ARCHITECTURE "i386")
  SET (CPACK_RPM_PACKAGE_ARCHITECTURE "i386")
else()
  SET (CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
  SET (CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")
endif()

INCLUDE(CPack)




###############################################################################
#
# Here we have the main configuration options for libsbml
#

# build static / shared library
# option(BUILD_SHARED_LIBS  "Build shared library (Set to OFF to build static libraries)" OFF)


# should we compile examples
option(WITH_EXAMPLES "Compile Examples"         OFF )

option(WITH_LAYOUT   "include layout support" 	ON  )
option(WITH_GROUPS   "include groups support" 	OFF )


# which language bindings should be build 
option(WITH_CSHARP   "Generate C# bindings"     OFF )
option(WITH_JAVA     "Generate Java bindings"   OFF )
option(WITH_PYTHON   "Generate python bindings" OFF )
option(WITH_PERL     "Generate perl bindings"   OFF )
option(WITH_RUBY     "Generate ruby bindings"   OFF )
option(WITH_OCTAVE   "Generate octave bindings" OFF )
option(WITH_MATLAB   "Generate matlab bindings" OFF )

# enable the generation of unit tests. If enabled all test runners 
# will be created and can be run with make test, or ctest
# this won't work in Visual Studio 2003 so we disable this option there
#
if (NOT ${CMAKE_GENERATOR} MATCHES "Visual Studio 6" AND NOT ${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
	option(WITH_CHECK    "Unit tests"               OFF )
endif()

# choose the xml parsing library to be used
option(WITH_EXPAT    "Use Expat XML parser"     OFF )
option(WITH_LIBXML   "Use libxml XML parser"    ON  )
option(WITH_XERCES   "Use xerces XML parser"    OFF )

# use C++ namespace
option(WITH_CPP_NAMESPACE "Use C++ namespace for libsbml" OFF)

# generate documentation
option(WITH_DOXYGEN   "Generate Documentation"    OFF )

# re-generate the swig bindings? This really should be on by default
# otherwise one might have the wrong wrapper code without support for
# the libsbml packages one wants. 
option(WITH_SWIG     "Regenerate SWIG bindings"  ON )

# set build type default
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build, options are: None (CMAKE_CXX_FLAGS or CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel" FORCE )

###############################################################################
#
# If with SWIG is selected we need to find swig
#


if(WITH_SWIG)

	find_program(SWIG_EXECUTABLE
		NAMES swig
		PATHS c:/swigwin-2.0.1 c:/swigwin-2.0.0 c:/swigwin-1.3.40 /usr/bin /usr/local/bin
		)
set(SWIG_EXTRA_ARGS)
if(WITH_LAYOUT)
	add_definitions(-DUSE_LAYOUT)
	set(SWIG_EXTRA_ARGS ${SWIG_EXTRA_ARGS} -DUSE_LAYOUT)
endif()

if(WITH_GROUPS)
	add_definitions(-DUSE_GROUPS)
	set(SWIG_EXTRA_ARGS ${SWIG_EXTRA_ARGS} -DUSE_GROUPS)
endif()

if(WITH_CPP_NAMESPACE)
	add_definitions(-DLIBSBML_USE_CPP_NAMESPACE)
endif()

endif(WITH_SWIG)


###############################################################################
#
# Locate expat if needed
#
set(LIBSBML_XML_LIBRARY)
set(LIBSBML_XML_LIBRARY_INCLUDE)
set(LIBSBML_XML_LIBRARY_LIBS)
if(WITH_EXPAT)

	find_library(LIBEXPAT_LIBRARY 
		NAMES libexpat.lib expat
		PATHS /usr/lib /usr/local/lib 
			  ${CMAKE_SOURCE_DIR} 
			  ${CMAKE_SOURCE_DIR}/dependencies/lib
			    )
	
	find_path(LIBEXPAT_INCLUDE_DIR
		NAMES expat.h
		PATHS /usr/include /usr/local/include 
			  ${CMAKE_SOURCE_DIR}/include 
			  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )		

    add_definitions( -DUSE_EXPAT )

	set(LIBSBML_XML_LIBRARY "expat")
	set(LIBSBML_XML_LIBRARY_INCLUDE ${LIBEXPAT_INCLUDE_DIR})
	set(LIBSBML_XML_LIBRARY_LIBS ${LIBEXPAT_LIBRARY})
	
	if (NOT EXISTS "${LIBEXPAT_INCLUDE_DIR}/expat.h")
		message(FATAL_ERROR "The expat include directory you specified is invalid it has to contain: expat.h")
	endif()
	
endif(WITH_EXPAT)

###############################################################################
#
# Locate Libxml2 if needed
#

if(WITH_LIBXML)

	find_library(LIBXML_LIBRARY 
		NAMES libxml2.lib xml2
		PATHS /usr/lib /usr/local/lib 
			  ${CMAKE_SOURCE_DIR} 
			  ${CMAKE_SOURCE_DIR}/dependencies/lib
			    )
	
	find_path(LIBXML_INCLUDE_DIR
		NAMES libxml/parser.h 
		PATHS /usr/include /usr/local/include 
			  /usr/include/libxml2
			  /usr/local/include/libxml2
			  ${CMAKE_SOURCE_DIR}/include 
			  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )		

  add_definitions( -DUSE_LIBXML )

  set(LIBSBML_XML_LIBRARY "libxml2")
  set(LIBSBML_XML_LIBRARY_INCLUDE ${LIBXML_INCLUDE_DIR})
  set(LIBSBML_XML_LIBRARY_LIBS ${LIBXML_LIBRARY})

  if (NOT EXISTS "${LIBXML_INCLUDE_DIR}/libxml/parser.h")
	message(FATAL_ERROR "The libxml include directory you specified is invalid it has to contain: libxml/parser.h")
  endif()
  
endif(WITH_LIBXML)


###############################################################################
#
# Locate xerces

if(WITH_XERCES)
	add_definitions( -DUSE_XERCES )
	find_library(XERCES_LIBRARY 
		NAMES xerces-c_3.lib xerces-c
		PATHS /usr/lib /usr/local/lib 
			  ${CMAKE_SOURCE_DIR} 
			  ${CMAKE_SOURCE_DIR}/dependencies/lib
			    )
	
	find_path(XERCES_INCLUDE_DIR
		NAMES xercesc/parsers/XercesDOMParser.hpp 
		PATHS /usr/include /usr/local/include 
			  /usr/include/xercesc
			  /usr/local/include/xercesc
			  ${CMAKE_SOURCE_DIR}/include 
			  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )		

	set(LIBSBML_XML_LIBRARY "xerces-c")
	set(LIBSBML_XML_LIBRARY_INCLUDE ${XERCES_INCLUDE_DIR})
	set(LIBSBML_XML_LIBRARY_LIBS ${XERCES_LIBRARY})

	if (NOT EXISTS "${XERCES_INCLUDE_DIR}/xercesc/parsers/XercesDOMParser.hpp")
		message(FATAL_ERROR "The xerces include directory you specified is invalid it has to contain: xercesc/parsers/XercesDOMParser.hpp")
	endif()
			  
endif(WITH_XERCES)

###############################################################################
#
# Locate bz2
#

set(BZIP_INITIAL_VALUE)
find_library(LIBBZ_LIBRARY 
	NAMES bzip2.lib bz2 libbz2.lib
	PATHS /usr/lib /usr/local/lib 
		  ${CMAKE_SOURCE_DIR} 
		  ${CMAKE_SOURCE_DIR}/dependencies/lib
)

if(EXISTS ${LIBBZ_LIBRARY})
	set(BZIP_INITIAL_VALUE ON)
else()
	set(BZIP_INITIAL_VALUE OFF)
endif()	

option(WITH_BZIP2    "Enable Bzip2 compression"  ${BZIP_INITIAL_VALUE} )
if(WITH_BZIP2)

	find_path(LIBBZ_INCLUDE_DIR
		NAMES bzlib.h bzip2/bzlib.h
		PATHS /usr/include /usr/local/include 
			  ${CMAKE_SOURCE_DIR}/include 
			  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )		

	add_definitions( -DUSE_BZ2 )	
	
	# make sure that we have a valid bzip2 library	
	check_library_exists("${LIBBZ_LIBRARY}" "BZ2_bzCompressInit" "" LIBBZ_FOUND_SYMBOL)
	if (NOT LIBBZ_FOUND_SYMBOL)
		# this is odd, but on windows this check always fails! must be a bug in the current cmake version
		# so for now only issue this warning on linux
		if (UNIX)
			message(WARNING "The bz2 library does not seem to be valid, as it is missing required symbols. Please chack that ${LIBBZ_LIBRARY} is a bzip2 library. For details see: ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log")		
		endif()
	endif()
	if (NOT EXISTS "${LIBBZ_INCLUDE_DIR}/bzlib.h")
		message(FATAL_ERROR "The bzip2 include directory you specified is invalid it has to contain: bzlib.h")
	endif()

	
endif(WITH_BZIP2)

###############################################################################
#
# list of additional files to link against. 
#
set(EXTRA_LIBS "" CACHE STRING "List of Libraries to link against" )


###############################################################################
#
# Locate zlib
#
set(ZLIB_INITIAL_VALUE)
find_library(LIBZ_LIBRARY 
	NAMES zdll.lib z zlib.lib
	PATHS /usr/lib /usr/local/lib 
		  ${CMAKE_SOURCE_DIR} 
		  ${CMAKE_SOURCE_DIR}/dependencies/lib
    )
if(EXISTS ${LIBZ_LIBRARY})
	set(ZLIB_INITIAL_VALUE ON)
else()
	set(ZLIB_INITIAL_VALUE OFF)
endif()
option(WITH_ZLIB     "Enable zip compression"    ${ZLIB_INITIAL_VALUE} )

if(WITH_ZLIB)

	
	find_path(LIBZ_INCLUDE_DIR
		NAMES zlib.h zlib/zlib.h
		PATHS /usr/include /usr/local/include 
			  ${CMAKE_SOURCE_DIR}/include 
			  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )		

	add_definitions( -DUSE_ZLIB )
	
	# make sure that we have a valid zip library	
	check_library_exists("${LIBZ_LIBRARY}" "gzopen" "" LIBZ_FOUND_SYMBOL)
	if (NOT LIBZ_FOUND_SYMBOL)
		# this is odd, but on windows this check always fails! must be a bug in the current cmake version
		# so for now only issue this warning on linux
		if (UNIX)
			message(WARNING "The zlib library does not seem to be valid, as it is missing required symbols. Please chack that ${LIBZ_LIBRARY} is a zlib library. For details see: ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log")		
		endif()
	endif()

	if (NOT EXISTS "${LIBZ_INCLUDE_DIR}/zlib.h")
		message(FATAL_ERROR "The zlib include directory you specified is invalid it has to contain: zlib.h")
	endif()
	
endif(WITH_ZLIB)

###############################################################################
#
# Find the C# compiler to use and set name for resulting library
#

if(WITH_CSHARP)
	find_program(CSHARP_COMPILER
		NAMES gmcs csc
		PATHS C:/Windows/Microsoft.NET/Framework/v2.0.50727/ /usr/bin /usr/local/bin
		)
	if(UNIX)
	else()
		if (CMAKE_SIZEOF_VOID_P EQUAL 4)
			# mark libsbml library as x86 
			set(CSHARP_EXTRA_ARGS -platform:x86 )
		elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
			# mark libsbml library as x64
			set(CSHARP_EXTRA_ARGS -platform:x64 )		
		endif()
	endif()

endif(WITH_CSHARP)

###############################################################################
#
# Enable support for testing ... can be invoked by running ctest 
# or make test
#

if(WITH_CHECK)

	ENABLE_TESTING()
	
	find_library(LIBCHECK_LIBRARY
		NAMES check libcheck
		PATHS /usr/lib /usr/local/lib ${CMAKE_SOURCE_DIR}/dependencies/lib
	)
	
	find_path(LIBCHECK_INCLUDE_DIR
		NAMES check.h
		PATHS /usr/include /usr/local/include  ${CMAKE_SOURCE_DIR}/dependencies/include
			  )
	
	if (NOT EXISTS "${LIBCHECK_INCLUDE_DIR}/check.h")
		message(FATAL_ERROR "The check include directory you specified is invalid it has to contain: check.h")
	endif()

	if (${CMAKE_GENERATOR} MATCHES "Visual Studio 6" OR ${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
		message(WARNING "LibCheck is not compatible with Visual Studio 2003 (or earlier versions).")
	endif()
	
			  
endif(WITH_CHECK)


###############################################################################
#
# Need some variables set up, such as the name for the libSBML 
# library and the Path and file separator characters. The 
# MISC_PREFIX variable will cause libsbml bindings, examples and 
# documentation to be installed in PREFIX/${MISC_PREFIX}.
#

set( MISC_PREFIX )
if(UNIX)
	set(PATH_SEP "/")
	set(FILE_SEP ":")
	set( MISC_PREFIX "share/libsbml/" )
	set(LIBSBML_LIBRARY sbml)
else()
	set( MISC_PREFIX "" )
	set(PATH_SEP "\\")
	set(FILE_SEP ";")
	if (MINGW)
		set(LIBSBML_LIBRARY sbml)
	else()
		set(LIBSBML_LIBRARY libsbml)
	endif()
endif()

###############################################################################
#
# Set up remaining variables, add option for universal binaries
#
set(BUILD_DEFINITIONS)
if(UNIX)
	if(APPLE)
		add_definitions(-DMACOSX)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DMACOSX")
		
		# On OSX it is common to build universal binaries to support multiple
		# processor architectures. The default behavior is not to build 
		# multiple architectures, as most users might not need that. 
		option(ENABLE_UNIVERSAL "Create Universal Binaries" OFF)
		
		set(CMAKE_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}" CACHE STRING "A semicolon separated list of build architectures to be used")
		if(ENABLE_UNIVERSAL)
			# if universal binaries are requested and none defined so far
			# overwrite them with all three common architectures. If the user 
			# specified their own list of architectures do not touch!
			if (CMAKE_OSX_ARCHITECTURES STREQUAL "")						
				set(CMAKE_OSX_ARCHITECTURES "i386;ppc;x86_64" CACHE STRING "A semicolon separated list of build architectures to be used" FORCE)				
			endif()
		endif(ENABLE_UNIVERSAL)
	else(APPLE)
		add_definitions(-DLINUX)
		
		if (NOT CYGWIN)
			# on cygwin all code is position independent so -fPIC is not needed
			set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing -fPIC")
			set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing -fPIC")
		endif()
		
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DLINUX")
	endif(APPLE)

	add_definitions( -DPACKAGE_VERSION=\"${PACKAGE_VERSION}\"  -DPACKAGE_NAME=\"${PROJECT_NAME}\")
	set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DPACKAGE_VERSION=\"${PACKAGE_VERSION}\"  -DPACKAGE_NAME=\"${PROJECT_NAME}\"")

else(UNIX)
	add_definitions(-DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS)
	set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS")
	if(MSVC)
		add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -D_CRT_SECURE_NO_WARNINGS")
		option(WITH_STATIC_RUNTIME "Compile using the static MSVC Runtime" OFF)
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
	
		file(GLOB WIN32_BINARIES ${CMAKE_SOURCE_DIR}/dependencies/bin/*.dll)
		install(FILES ${WIN32_BINARIES} DESTINATION bin)
		file(GLOB WIN32_DEPENDENCIES ${CMAKE_SOURCE_DIR}/dependencies/lib/*.lib)
		install(FILES ${WIN32_DEPENDENCIES} DESTINATION lib)
		
	elseif(CYGWIN)
		add_definitions(-DCYGWIN)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DCYGWIN")
	elseif(MINGW)
		if(WITH_LIBXML)
			# this is necessary to build with libxml2 on mingw
			add_definitions(-DLIBXML_STATIC)
		endif(WITH_LIBXML)
	endif(MSVC)
	
endif(UNIX)

###############################################################################
#
# Disable in-source build
#

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}" )
	message(FATAL_ERROR "In-source builds of libsbml have been disabled. Please create a separate build directory.")
endif()

###############################################################################
#
# Add check that only one XML library was selection
#
SET(LIBRARY_COUNT 0)
if (WITH_EXPAT)
	math(EXPR LIBRARY_COUNT "${LIBRARY_COUNT} + 1")
endif()
if (WITH_LIBXML)
	math(EXPR LIBRARY_COUNT "${LIBRARY_COUNT} + 1")
endif()
if (WITH_XERCES)
	math(EXPR LIBRARY_COUNT "${LIBRARY_COUNT} + 1")
endif()

if(LIBRARY_COUNT GREATER 1)
	message(FATAL_ERROR "Only one XML library should be chosen. Please select only one of: WITH_LIBXML, WITH_EXPAT or WITH_XERCES")
endif()

###############################################################################
#
# Add check that at least one XML library was selection
#
if(LIBRARY_COUNT EQUAL 0)
	message(FATAL_ERROR "A XML library has to be chosen. Please select one of: WITH_LIBXML, WITH_EXPAT or WITH_XERCES")
endif()


###############################################################################
#
# Build the actual libSBML library
#
add_subdirectory(src)


###############################################################################
#
# Build examples if specified
#

# code that builds the examples
add_subdirectory(examples)


###############################################################################
#
# Include options for all libSBML packages
#
file(GLOB PACKAGE_OPTIONS "*package.cmake")
foreach(package ${PACKAGE_OPTIONS})
	include(${package})
endforeach()

if (WITH_DOXYGEN)
	add_subdirectory(docs)
endif()


###############################################################################
#
# Install documentation
#

set(DOCUMENTATION_FILES
		COPYING.txt
		FUNDING.txt
		LICENSE.txt
		NEWS.txt
		README.txt
		VERSION.txt
)

install(FILES ${DOCUMENTATION_FILES} DESTINATION ${MISC_PREFIX}.)

###############################################################################
#
# Print Summary
#

message(STATUS "
----------------------------------------------------------------------
libSBML version ${PACKAGE_VERSION}
----------------------------------------------------------------------

More information and the latest version are available online at
http://sbml.org/Software/libSBML

Please report problems using the issue tracker at
http://sbml.org/software/libsbml/issue-tracker

To contact the developers directly, email libsbml-team@caltech.edu

Configured on host "$ENV{COMPUTERNAME}" ${HOSTNAME}
  host type                     = ${CMAKE_SYSTEM_NAME}
  host operating system         = ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_VERSION}
  host CPU architecture         = ${CMAKE_SYSTEM_PROCESSOR}

General build flags:
  CC                            = ${CMAKE_C_COMPILER}
  CXX                           = ${CMAKE_CXX_COMPILER}
  CPPFLAGS                      = ${BUILD_DEFINITIONS}
  CFLAGS                        = ${CMAKE_C_FLAGS}
  CXXFLAGS                      = ${CMAKE_CXX_FLAGS}
  LDFLAGS                       = ${CMAKE_EXE_LINKER_FLAGS}

XML parser library configuration:
  XML library                   = ${LIBSBML_XML_LIBRARY}
  CPPFLAGS for XML              = -I${LIBSBML_XML_LIBRARY_INCLUDE}
  LIBS for XML                  = ${LIBSBML_XML_LIBRARY_LIBS}

Other libSBML configuration settings:
  Installation $prefix          = ${CMAKE_INSTALL_PREFIX}")



if (UNIX)
	if (APPLE)
		message(STATUS "  Value of $DYLD_LIBRARY_PATH   = $ENV{DYLD_LIBRARY_PATH}")
	else()
		message(STATUS "  Value of $LD_LIBRARY_PATH     = $ENV{LD_LIBRARY_PATH}")
	endif()
endif()

if (WITH_SWIG)
	message(STATUS "  Using SWIG                    = ${SWIG_EXECUTABLE}")
endif()


if (WITH_CSHARP)
	message(STATUS "  Using C#                      = ${CSHARP_COMPILER}")
endif()

if (WITH_JAVA)
	message(STATUS "  Using Java                    = ${Java_JAVA_EXECUTABLE}")
endif()

if (WITH_PYTHON)
	message(STATUS "  Using Python                  = ${PYTHON_EXECUTABLE}")
endif()

if (WITH_PERL)
	message(STATUS "  Using Perl                    = ${PERL_EXECUTABLE}")
endif()

if (WITH_RUBY)
	message(STATUS "  Using Ruby                    = ${RUBY_EXECUTABLE}")
endif()

if (WITH_OCTAVE)
	message(STATUS "  Using Octave                  = ${MKOCTFILE_EXECUTABLE}")
endif()

if (WITH_MATLAB)
	message(STATUS "  Using Matlab                  = ${MATLAB_ROOT_PATH}")
endif()

if (WITH_CHECK)
	message(STATUS "  Using LibCheck                = ${LIBCHECK_LIBRARY}")
endif()
message(STATUS "
")

if (WITH_CPP_NAMESPACE)
	message(STATUS "  Using C++ namespace for libSBML (libsbml) = yes")
else()
	message(STATUS "  Using C++ namespace for libSBML (libsbml) = no")
endif()

if (APPLE)
	if (CMAKE_OSX_ARCHITECTURES STREQUAL "")
		message(STATUS "  Building universal binaries               = no (using native arch)")
	else()
		list(REMOVE_DUPLICATES CMAKE_OSX_ARCHITECTURES)	
		list(REMOVE_ITEM CMAKE_OSX_ARCHITECTURES "")
		list(SORT CMAKE_OSX_ARCHITECTURES)
		list(LENGTH CMAKE_OSX_ARCHITECTURES NUM_ARCHS)
		if (NUMARCHS EQUAL 1)
			message(STATUS "  Building universal binaries               = no (using ${CMAKE_OSX_ARCHITECTURES})")
		else()
			message(STATUS "  Building universal binaries               = yes (using ${CMAKE_OSX_ARCHITECTURES})")
		endif()
	endif()
endif()


# print selected packages if configured previously
if (EXISTS ${CMAKE_BINARY_DIR}/CMakeCache.txt)
file(STRINGS  ${CMAKE_BINARY_DIR}/CMakeCache.txt cache)

set(last_line)
foreach(line ${cache})
	
	string(LENGTH "${line}" length)
	if (${length} GREATER 7)
		string(SUBSTRING "${line}" 0 7 start)
		if ("${start}" STREQUAL  "ENABLE_")
			if ("${line}" MATCHES "^.*ON")
				message(STATUS "  ${last_line} = yes")
			else()
				message(STATUS "  ${last_line} = no")
			endif()
		endif()
	
		math(EXPR new_length "${length} - 2")
		if (${new_length} GREATER 3)
		string(SUBSTRING "${line}" 2 ${new_length} last_line)
		endif()	
	else()
		set(last_line "")
		message(${line})
	endif()	
endforeach()
endif()

message(STATUS "
")
message(STATUS "Other potentially important settings:")
if(WITH_ZLIB)
	message(STATUS "  Compression support is enabled for .zip and .gz files")
else()
	message(STATUS "
Warning: reading/writing compressed SBML in .zip or .gz format in
this copy of libSBML is not supported or has been disabled.
If this was not your intention, please check the WITH_ZLIB
option.")
endif()

if(WITH_BZIP2)
	message(STATUS "  Compression support is enabled for .bz2 files")
else()
	message(STATUS "
Warning: reading/writing compressed SBML in .bz2 format in
this copy of libSBML is not supported or has been disabled.
If this was not your intention, please check the WITH_BZIP2
option.")
endif()

message(STATUS "
----------------------------------------------------------------------")


# # To compile a test program simply comment out the lines below ... 
# 
# OPTION(WITH_TEST_PROGRAM " COMPILE SMALL TEST PROGRAM " ON)
# 
# if (WITH_TEST_PROGRAM)
# 
# include_directories(${CMAKE_CURRENT_SOURCE_DIR})
# include_directories(${CMAKE_SOURCE_DIR}/include)
# 
# add_executable(libsbml_test test.cpp)
# target_link_libraries(libsbml_test ${LIBSBML_LIBRARY})
# 
# endif()
