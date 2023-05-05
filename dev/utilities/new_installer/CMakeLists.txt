###############################################################################
#
# Description       : build script to configure the windows installer
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2018 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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

cmake_minimum_required(VERSION 2.8...3.19)
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

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../../../VERSION.txt")

	file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/../../../VERSION.txt" VersionString NEWLINE_CONSUME)
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

file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR} CURRENT_DIR)

# configure the version and current directory for installers
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/install_libsbml_script_xml_win32.iss.in
	${CMAKE_CURRENT_SOURCE_DIR}/install_libsbml_script_xml_win32.iss
)
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/install_libsbml_script_xml_win64.iss.in
	${CMAKE_CURRENT_SOURCE_DIR}/install_libsbml_script_xml_win64.iss
)

configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/matlab/install_matlab_libsbml_script_xml_win32.in
	${CMAKE_CURRENT_SOURCE_DIR}/matlab/install_matlab_libsbml_script_xml_win32.iss
)
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/matlab/install_matlab_libsbml_script_xml_win64.in
	${CMAKE_CURRENT_SOURCE_DIR}/matlab/install_matlab_libsbml_script_xml_win64.iss
)

configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/python/setup32.in
	${CMAKE_CURRENT_SOURCE_DIR}/python/setup32.py
)

configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/python/setup64.in
	${CMAKE_CURRENT_SOURCE_DIR}/python/setup64.py
)
