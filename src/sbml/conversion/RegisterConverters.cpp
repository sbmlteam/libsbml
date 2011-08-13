/**
* @file    RegisterConverters.cpp
* @brief   Registers all available converters. 
* @author  Frank Bergmann
* 
* <!--------------------------------------------------------------------------
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*
* Copyright (C) 2009-2011 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
*  
* Copyright (C) 2006-2008 by the California Institute of Technology,
*     Pasadena, CA, USA 
*  
* Copyright (C) 2002-2005 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. Japan Science and Technology Agency, Japan
* 
* This library is free software; you can redistribute it and/or modify it
* under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation.  A copy of the license agreement is provided
* in the file named "LICENSE.txt" included with this software distribution
* and also available online as http://sbml.org/software/libsbml/license.html
* ------------------------------------------------------------------------ -->
*/

#ifndef RegisterConverters_h
#define RegisterConverters_h

#include <sbml/conversion/SBMLConverterRegister.h>

#include <sbml/conversion/SBMLFunctionDefinitionConverter.h>
#include <sbml/conversion/SBMLInitialAssignmentConverter.h>
#include <sbml/conversion/SBMLLevelVersionConverter.h>
#include <sbml/conversion/SBMLStripPackageConverter.h>

  // TO DO - SK Comment

// Presumably this means that converters added are hardcoded into a release
// some one adding their own would need to add to this file manually ??
  
static SBMLConverterRegister<SBMLFunctionDefinitionConverter> registerFDConverter;
static SBMLConverterRegister<SBMLInitialAssignmentConverter> registerIAConverter;
static SBMLConverterRegister<SBMLLevelVersionConverter> registerLVConverter;
static SBMLConverterRegister<SBMLStripPackageConverter> registerStripConverter;



#endif

