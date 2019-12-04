#################################################
 # @file l3v2extendedmath-package.cmake
 # @brief Src CMake file for l3v2extendedmath package
 # @author SBMLTeam
 #
 # <!--------------------------------------------------------------------------
 # This file is part of libSBML. Please visit http://sbml.org for more
 # information about SBML, and the latest version of libSBML.
 #
 # Copyright (C) 2013-2018 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 # 3. University of Heidelberg, Heidelberg, Germany
 #
 # Copyright (C) 2009-2013 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 #
 # Copyright (C) 2006-2008 by the California Institute of Technology,
 # Pasadena, CA, USA
 #
 # Copyright (C) 2002-2005 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. Japan Science and Technology Agency, Japan
 #
 # This library is free software; you can redistribute it and/or modify it
 # under the terms of the GNU Lesser General Public License as published by the
 # Free Software Foundation. A copy of the license agreement is provided in the
 # file named "LICENSE.txt" included with this software distribution and also
 # available online as http://sbml.org/software/libsbml/license.html
 # ------------------------------------------------------------------------ -->
 #

if (ENABLE_L3V2EXTENDEDMATH)

include(${LIBSBML_ROOT_SOURCE_DIR}/l3v2extendedmath-package.cmake)

 # build up sources
set(L3V2EXTENDEDMATH_SOURCES)

 # go through all directories
foreach(dir common extension sbml validator validator/constraints)

   # add to include directory
  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/l3v2extendedmath/${dir})

   # file sources
  file(GLOB current ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/l3v2extendedmath/${dir}/*.cpp
                    ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/l3v2extendedmath/${dir}/*.c
                    ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/l3v2extendedmath/${dir}/*.h)

   # add sources
  set(L3V2EXTENDEDMATH_SOURCES ${L3V2EXTENDEDMATH_SOURCES} ${current})

   # mark headers for installation
  file(GLOB l3v2extendedmath_headers ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/l3v2extendedmath/${dir}/*.h)

  install(FILES ${l3v2extendedmath_headers} DESTINATION include/sbml/packages/l3v2extendedmath/${dir} )

endforeach()

 # create source group for IDEs
source_group(l3v2extendedmath_package FILES ${L3V2EXTENDEDMATH_SOURCES})

 # add l3v2extendedmath sources to SBML sources
SET(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${L3V2EXTENDEDMATH_SOURCES})

#################################################
 #
 # add test scripts
 #
if (WITH_CHECK)

  add_subdirectory(sbml/packages/l3v2extendedmath/validator/test)
  add_subdirectory(sbml/packages/l3v2extendedmath/extension/test)

endif()

endif()

