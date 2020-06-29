#################################################
 # @file arrays-package.cmake
 # @brief Top-level CMake file for arrays package
 # @author SBMLTeam
 #
 # <!--------------------------------------------------------------------------
 # This file is part of libSBML. Please visit http://sbml.org for more
 # information about SBML, and the latest version of libSBML.
 #
 # Copyright (C) 2013-2017 jointly by the following organizations:
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

option(ENABLE_ARRAYS     "Enable libSBML support for the SBML Level 3 Arrays ('arrays') package."      OFF)

 # provide summary status
list(APPEND LIBSBML_PACKAGE_SUMMARY "SBML 'arrays' package  = ${ENABLE_ARRAYS}")

if(ENABLE_ARRAYS)

    set(USE_ARRAYS TRUE)
  add_definitions(-DUSE_ARRAYS)
  set(LIBSBML_PACKAGE_INCLUDES ${LIBSBML_PACKAGE_INCLUDES} "LIBSBML_HAS_PACKAGE_ARRAYS")
  list(APPEND SWIG_EXTRA_ARGS -DUSE_ARRAYS)
  list(APPEND SWIG_SWIGDOCDEFINES --define USE_ARRAYS)

  if (LIBSBML_USE_LEGACY_MATH)
  
    message(FATAL_ERROR 
	  "\nThe arrays-package extends ASTNodes and thus cannot be used with the old "
	  "AST classes. Please set LIBSBML_USE_LEGACY_MATH to OFF or disable the arrays "
	  "package.\n\n"
	)
  
  endif(LIBSBML_USE_LEGACY_MATH)
	
endif(ENABLE_ARRAYS)
