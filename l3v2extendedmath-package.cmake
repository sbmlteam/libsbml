#################################################
 # @file l3v2extendedmath-package.cmake
 # @brief Top-level CMake file for l3v2extendedmath package
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

option(ENABLE_L3V2EXTENDEDMATH     "Enable libSBML support for the SBML Level 3 L3v2extendedmath ('l3v2extendedmath') package."      ON)

 # provide summary status
list(APPEND LIBSBML_PACKAGE_SUMMARY "SBML 'l3v2extendedmath' package  = ${ENABLE_L3V2EXTENDEDMATH}")

if (ENABLE_L3V2EXTENDEDMATH)
  add_definitions(-DUSE_L3V2EXTENDEDMATH)
  set(LIBSBML_PACKAGE_INCLUDES ${LIBSBML_PACKAGE_INCLUDES} "LIBSBML_HAS_PACKAGE_L3V2EXTENDEDMATH")
  list(APPEND SWIG_EXTRA_ARGS -DUSE_L3V2EXTENDEDMATH)
  list(APPEND SWIG_SWIGDOCDEFINES --define USE_L3V2EXTENDEDMATH)

  if (!LIBSBML_USE_LEGACY_MATH)
  
    message(FATAL_ERROR 
    "\nThe l3v2extendedmath-package does not work with new math"
    "Please set LIBSBML_USE_LEGACY_MATH to ON.\n\n"
    )
 
  endif(!LIBSBML_USE_LEGACY_MATH)

else ()

    message(FATAL_ERROR 
    "\nThe l3v2extendedmath-package is necessary to use the math constructs "
    "introduced in L3V2.\n\n"
    )

endif(ENABLE_L3V2EXTENDEDMATH)

