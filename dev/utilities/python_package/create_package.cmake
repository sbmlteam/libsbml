###############################################################################
#
# Description       : CMake script for constructing a libSBML python source package
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
if ("${OUT_DIR}" STREQUAL ""  OR 
    "${SRC_DIR}" STREQUAL ""  OR
	"${BIN_DIR}" STREQUAL "" )
	
	message ( FATAL_ERROR 
"
Need three arguments: 
  OUT_DIR: directory where to create the package  
  SRC_DIR: libsbml/src directory
  BIN_DIR: build directory, that has been configured with python & swigged
" 

)
	
endif()

SET(CURRENT_DIR ${SRC_DIR}/../dev/utilities/python_package)


# delete prior builds 
if (EXISTS ${OUT_DIR}/build)
  file (REMOVE_RECURSE ${OUT_DIR}/build )
endif()

# delete prior distributions 
if (EXISTS ${OUT_DIR}/dist)
  file (REMOVE_RECURSE ${OUT_DIR}/dist )
endif()

# delete prior base files 
if (EXISTS ${OUT_DIR}/base)
  file (REMOVE_RECURSE ${OUT_DIR}/base )
endif()

# create base dir
file (MAKE_DIRECTORY ${OUT_DIR}/base)

  # # list directories to copy files from (old way)
  # set(DIRECTORIES

  #   sbml
  #   sbml/annotation
  #   sbml/common
  #   sbml/compress
  #   sbml/conversion
  #   sbml/extension
  #   sbml/math
  #   sbml/packages
    
  #   sbml/packages/comp/common
  #   sbml/packages/comp/extension
  #   sbml/packages/comp/sbml
  #   sbml/packages/comp/util
  #   sbml/packages/comp/validator
  #   sbml/packages/comp/validator/constraints
    
  #   sbml/packages/fbc/common
  #   sbml/packages/fbc/extension
  #   sbml/packages/fbc/sbml
  #   sbml/packages/fbc/util
  #   sbml/packages/fbc/validator
  #   sbml/packages/fbc/validator/constraints
    
  #   sbml/packages/layout/common
  #   sbml/packages/layout/extension
  #   sbml/packages/layout/sbml
  #   sbml/packages/layout/util
  #   sbml/packages/layout/validator
  #   sbml/packages/layout/validator/constraints
    
  #   sbml/packages/render/common
  #   sbml/packages/render/extension
  #   sbml/packages/render/sbml
  #   sbml/packages/render/util
  #   sbml/packages/render/validator
  #   sbml/packages/render/validator/constraints
    
  #   sbml/packages/qual/common
  #   sbml/packages/qual/extension
  #   sbml/packages/qual/sbml
  #   sbml/packages/qual/util
  #   sbml/packages/qual/validator
  #   sbml/packages/qual/validator/constraints
    
  #   sbml/packages/multi/common
  #   sbml/packages/multi/extension
  #   sbml/packages/multi/sbml
  #   sbml/packages/multi/validator
  #   sbml/packages/multi/validator/constraints  

  #   sbml/packages/groups/common
  #   sbml/packages/groups/extension
  #   sbml/packages/groups/sbml
  #   sbml/packages/groups/util
  #   sbml/packages/groups/validator
  #   sbml/packages/groups/validator/constraints  

  #   sbml/packages/l3v2extendedmath/common
  #   sbml/packages/l3v2extendedmath/extension
  #   sbml/packages/l3v2extendedmath/validator
  #   sbml/packages/l3v2extendedmath/validator/constraints
    
  #   sbml/units
  #   sbml/util
  #   sbml/validator
  #   sbml/validator/constraints
  #   sbml/xml
  # )

# new way to list directories
IF (${CMAKE_VERSION} VERSION_GREATER 3.2.3)
  message("\nCMake ${CMAKE_VERSION} detected using sane directory tree strategy.")
  MACRO(GET_SBML_SRC_TREE result curdir)
    FILE(GLOB_RECURSE children LIST_DIRECTORIES true RELATIVE ${curdir} ${curdir}/*)
    SET(dirlist "sbml")
    FOREACH(subdir ${children})
      IF(IS_DIRECTORY ${curdir}/${subdir})
        IF(subdir MATCHES "test$" OR subdir MATCHES "test-data$"
          OR subdir MATCHES "/test/" OR  subdir  MATCHES "/test-data/")
          #message("${subdir}")
        ELSE()
          #message("sbml/${subdir}")
          LIST(APPEND dirlist "sbml/${subdir}")
        ENDIF()
      ENDIF()
    ENDFOREACH()
    SET(${result} ${dirlist})
  ENDMACRO()

  GET_SBML_SRC_TREE(DIRECTORIES "${SRC_DIR}/sbml")

ELSE ()
  message("\nCMake ${CMAKE_VERSION} detected using directory tree hack.")
  MACRO(SUBDIRLIST result curdir)
    FILE(GLOB children ABSOLUTE ${curdir} ${curdir}/*)
    SET(dirlist "")
    FOREACH(subdir ${children})
      IF(IS_DIRECTORY ${subdir})
        IF(subdir MATCHES "test$" OR subdir MATCHES "test-data$"
          OR subdir MATCHES "/test/" OR  subdir  MATCHES "/test-data/")
          #message("${subdir}")
        ELSE()
          LIST(APPEND dirlist ${subdir})
        ENDIF()
      ENDIF()
    ENDFOREACH()
    LIST(APPEND "${result}" ${dirlist})
  ENDMACRO()

  MACRO(ADD_SUBDIR outlist inlist)
    FOREACH(subdir ${inlist})
      SUBDIRLIST(temp_dirs "${subdir}")
      LIST(APPEND "${outlist}" ${temp_dirs})
    ENDFOREACH()
    list(REMOVE_DUPLICATES ${outlist})
  ENDMACRO()

  # get the actual list
  SUBDIRLIST(DIRECTORIES "${SRC_DIR}/sbml")
  # get 3 subdirectory levels 
  ADD_SUBDIR(DIRECTORIES "${DIRECTORIES}")
  ADD_SUBDIR(DIRECTORIES "${DIRECTORIES}")
  ADD_SUBDIR(DIRECTORIES "${DIRECTORIES}")

  # postprocess
  SET(ALL_DIRS)
  STRING(LENGTH "${SRC_DIR}/sbml" SRC_DIR_LEN)
  MATH(EXPR SRC_DIR_LEN_FIXED "${SRC_DIR_LEN} - 4")
  FOREACH(dir ${DIRECTORIES})
    STRING(SUBSTRING ${dir} ${SRC_DIR_LEN_FIXED} -1 dirfix)
    LIST(APPEND ALL_DIRS ${dirfix})
  ENDFOREACH()
  SET(DIRECTORIES ${ALL_DIRS})

ENDIF(${CMAKE_VERSION} VERSION_GREATER 3.2.3)

list(SORT DIRECTORIES)

# # DEBUG
# message("${SRC_DIR}/sbml")
# FOREACH(sdir ${DIRECTORIES})
#   message("${sdir}")  
# ENDFOREACH()

# copy files 
foreach( directory ${DIRECTORIES} )
  file (MAKE_DIRECTORY ${OUT_DIR}/base/${directory})
  
  file (GLOB SOURCE_FILES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}  
        ${SRC_DIR}/${directory}/*.cpp 
        ${SRC_DIR}/${directory}/*.c 
		${SRC_DIR}/${directory}/*.h 
		${SRC_DIR}/${directory}/*.in 
		${SRC_DIR}/${directory}/*.cxx)

  file(
    COPY ${SOURCE_FILES} 
    DESTINATION ${OUT_DIR}/base/${directory}
  ) 
      
endforeach()

# remove expat / xerces files 
file (GLOB SOURCE_FILES
        ${OUT_DIR}/base/sbml/xml/Expat*.* 
        ${OUT_DIR}/base/sbml/xml/Xerces*.* 
)

file(REMOVE ${SOURCE_FILES})

# copy swigable files
file (GLOB SOURCE_FILES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
        ${SRC_DIR}/bindings/python/*.cpp 
    	${SRC_DIR}/bindings/python/*.h 
    	${SRC_DIR}/bindings/python/*.i 
    	${SRC_DIR}/bindings/python/*.cxx
        ${SRC_DIR}/bindings/swig/*.cpp 
    	${SRC_DIR}/bindings/swig/*.h 
    	${SRC_DIR}/bindings/swig/*.i 
    	${SRC_DIR}/bindings/swig/*.cxx
)


file(
    COPY ${SOURCE_FILES} 
    DESTINATION ${OUT_DIR}/base/
) 

file (GLOB HEADER_FILES
        ${BIN_DIR}/src/sbml/common/*.h
)		

file(
    COPY ${HEADER_FILES} 
    DESTINATION ${OUT_DIR}/base/sbml/common
) 


file (GLOB BIN_BASE_FILES 
  ${BIN_DIR}/src/bindings/python/*.cpp
  ${BIN_DIR}/src/bindings/python/*.h
  )

# copy swigged files 
file(
    COPY ${BIN_BASE_FILES}	  
    DESTINATION ${OUT_DIR}/base/
) 


if (EXISTS ${OUT_DIR}/swig)
  file (REMOVE_RECURSE ${OUT_DIR}/swig )
endif()


file (MAKE_DIRECTORY ${OUT_DIR}/swig)

# copy swig directory (needed as it is referenced directly)
file (GLOB SOURCE_FILES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}  
        ${SRC_DIR}/bindings/swig/*.cpp 
    	${SRC_DIR}/bindings/swig/*.h 
    	${SRC_DIR}/bindings/swig/*.i 
    	${SRC_DIR}/bindings/swig/*.cxx
)
file(
    COPY ${SOURCE_FILES} 
    DESTINATION ${OUT_DIR}/swig/
) 


# copy files generated in the swig dir
file (GLOB EXTENSION_FILES 
  ${BIN_DIR}/src/sbml/extension/*.cxx
  ${BIN_DIR}/src/sbml/extension/*.cpp
  ${BIN_DIR}/src/sbml/extension/*.h
  )

# copy swigged files 
file(
    COPY ${EXTENSION_FILES}  
    DESTINATION ${OUT_DIR}/base/sbml/extension/
) 


# copy files generated in the swig dir
file (GLOB BIN_SWIG_FILES 
  ${BIN_DIR}/src/bindings/swig/*.cpp
  ${BIN_DIR}/src/bindings/swig/*.h
  )

# copy swigged files 
file(
    COPY ${BIN_SWIG_FILES}  
    DESTINATION ${OUT_DIR}/swig/
) 



# remove previous python scripts  
if (EXISTS ${OUT_DIR}/libsbml)
  file (REMOVE_RECURSE ${OUT_DIR}/libsbml )
endif()
file (MAKE_DIRECTORY ${OUT_DIR}/libsbml)
file (MAKE_DIRECTORY ${OUT_DIR}/script)
# copy new python script
file(
    COPY 
	  ${BIN_DIR}/src/bindings/python/libsbml.py	  
    DESTINATION ${OUT_DIR}/script
) 


# rename python script
file(
    RENAME  
    ${OUT_DIR}/script/libsbml.py
    ${OUT_DIR}/script/libsbml2.py
)

# generate a version compatible with python 3.x
file(READ ${OUT_DIR}/script/libsbml2.py init_script)
string(REPLACE 
  "class SBase(_object):"
  "class SBase(_object, metaclass=AutoProperty):"
  init3_script ${init_script}
)

string(REPLACE 
  "class SBase(object):"
  "class SBase(object, metaclass=AutoProperty):"
  init3_script ${init3_script}
)

string(REPLACE 
  "class SBasePlugin(_object):"
  "class SBasePlugin(_object, metaclass=AutoProperty):"
  init3_script ${init3_script}
)

string(REPLACE 
  "class SBasePlugin(object):"
  "class SBasePlugin(object, metaclass=AutoProperty):"
  init3_script ${init3_script}
)


file(WRITE ${OUT_DIR}/script/libsbml3.py ${init3_script})


# discover current version (default to 5.9.0)
set(LIBSBML_VERSION "5.9.0")
if(EXISTS "${SRC_DIR}/../VERSION.txt")

    file(STRINGS "${SRC_DIR}/../VERSION.txt" VersionString NEWLINE_CONSUME)
    string(STRIP "${VersionString}" VersionString)
    string(REPLACE "." ";" VersionString "${VersionString}" )
    string(REPLACE "-" ";" VersionString "${VersionString}" )
    list(LENGTH VersionString versionLength)
    list(GET VersionString 0 LIBSBML_VERSION_MAJOR )
    list(GET VersionString 1 LIBSBML_VERSION_MINOR )
    list(GET VersionString 2 LIBSBML_VERSION_PATCH )

    if(${versionLength} GREATER 3)
        list(GET VersionString 3 LIBSBML_VERSION_RELEASE )
    endif()
	set(LIBSBML_VERSION "${LIBSBML_VERSION_MAJOR}.${LIBSBML_VERSION_MINOR}.${LIBSBML_VERSION_PATCH}")

endif()

# copy manifest template
file(
    COPY 
	  ${CURRENT_DIR}/MANIFEST.in
    DESTINATION ${OUT_DIR}/
) 

#configure version in setup.py
configure_file(${CURRENT_DIR}/setup.py.in ${OUT_DIR}/setup.py)
