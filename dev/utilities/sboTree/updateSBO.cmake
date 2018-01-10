###############################################################################
#
# Description       : CMake script for updating the list of SBO Terms
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

# checking arguments 
if ("${SRC_DIR}" STREQUAL "" OR 
    "${PYTHON_EXECUTABLE}" STREQUAL "")
	
	message ( FATAL_ERROR 
"
Need two arguments: 
  SRC_DIR: libsbml root source directory
  PYTHON_EXECUTABLE: the python interpreter to be used
" 
)
endif()


# download file 
file(DOWNLOAD "http://www.ebi.ac.uk/sbo/exports/Main/SBO_OBO_lite.obo" "${SRC_DIR}/dev/utilities/sboTree/sbo-obo-flat.txt")

if (NOT EXISTS "${SRC_DIR}/dev/utilities/sboTree/sbo-obo-flat.txt")
message ( FATAL_ERROR 
"
Couldn't download SBO OBO flat file, please check that the URL is still current: 
  http://www.ebi.ac.uk/sbo/exports/Main/SBO_OBO_lite.obo  
" 
)
endif()

# convert file 
execute_process(
	COMMAND "${PYTHON_EXECUTABLE}"
    "${SRC_DIR}/dev/utilities/sboTree/sbo2cpp.py" 
    "${SRC_DIR}/dev/utilities/sboTree/sbo-obo-flat.txt"
    "${SRC_DIR}/dev/utilities/sboTree/output.txt"
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )

if (NOT EXISTS "${SRC_DIR}/dev/utilities/sboTree/output.txt")
message ( FATAL_ERROR 
"
Conversion from OBO flat file to the libSBML tree didn't work. Please 
check the python script: 

  ${SRC_DIR}/dev/utilities/sboTree/sbo2cpp.py
" 
)
endif()
  
  
# read file 
file(READ "${SRC_DIR}/dev/utilities/sboTree/output.txt" NEW_TERMS)

# get current date 
string(TIMESTAMP DATE "%m/%d/%Y" )

# replace content
configure_file("${SRC_DIR}/dev/utilities/sboTree/SBO.cpp.cmake"
               "${SRC_DIR}/src/sbml/SBO.cpp")

# clean up 
file(REMOVE "${SRC_DIR}/dev/utilities/sboTree/output.txt")
file(REMOVE "${SRC_DIR}/dev/utilities/sboTree/sbo-obo-flat.txt")

# done
message(STATUS "The file '${SRC_DIR}/src/sbml/SBO.cpp' has been updated, don't forget to commit if needed.")
