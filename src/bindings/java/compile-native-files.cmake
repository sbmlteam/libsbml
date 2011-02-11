###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for native java files
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

message("Creating: libsbml.jar")

# find all sources
file(GLOB_RECURSE SOURCE_FILES RELATIVE ${BIN_DIRECTORY} ${BIN_DIRECTORY}/java-files/org/sbml/libsbml/*.java)

# convert paths
set(NATIVE_FILES)
foreach(javaFile ${SOURCE_FILES})
	file(TO_NATIVE_PATH ${javaFile} temp)
	set(NATIVE_FILES ${NATIVE_FILES} ${temp})
endforeach()

# delete file if it exists
if (EXISTS ${BIN_DIRECTORY}/libsbml.jar)
	file(REMOVE ${BIN_DIRECTORY}/libsbml.jar)	
endif()

# compile files
execute_process(
	COMMAND "${Java_JAVAC_EXECUTABLE}"
		 -d java-files
		 ${NATIVE_FILES}	
	WORKING_DIRECTORY "${BIN_DIRECTORY}"
)

# enumerate class files
file(GLOB_RECURSE CLASS_FILES RELATIVE ${BIN_DIRECTORY}/java-files ${BIN_DIRECTORY}/java-files/org/sbml/libsbml/*.class)
set(NATIVE_CLASS_FILES)
foreach(classFile ${CLASS_FILES})
	file(TO_NATIVE_PATH ${classFile} temp)
	set(NATIVE_CLASS_FILES ${NATIVE_CLASS_FILES} ${temp})
endforeach()

set (PATH_SEP)
if (UNIX OR CYGWIN)
	set (PATH_SEP "/")
else()
	set (PATH_SEP "\\")
endif()

# create jar
execute_process(
	COMMAND "${Java_JAR_EXECUTABLE}"
		 -cvf ..${PATH_SEP}libsbmlj.jar
		 ${NATIVE_CLASS_FILES}	
	WORKING_DIRECTORY "${BIN_DIRECTORY}/java-files"
)


# # print variables for debug purposes 
# message("BIN_DIRECTORY         : ${BIN_DIRECTORY}")
# message("Java_JAVAC_EXECUTABLE : ${Java_JAVAC_EXECUTABLE}")
# message("Java_JAR_EXECUTABLE   : ${Java_JAR_EXECUTABLE}")


