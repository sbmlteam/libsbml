/**
 * @file    SBMLConverterRegistry.h
 * @brief   Definition of SBMLConverterRegistry, a registry of available converters.
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

#ifndef SBMLConverterRegistry_h
#define SBMLConverterRegistry_h


#include <sbml/common/extern.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/ConversionProperties.h>
#include <map>
#include <vector>


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/** 
 * Registry of all converters. 
 */
class SBMLConverterRegistry
{
public:
  /** 
   * @return the singleton for the converter registry. 
   */
  static SBMLConverterRegistry& getInstance();

  /** 
   * Adds the given converter to the registry
   * 
   * @param converter the converter to add to the registry
   * 
   * @return status code
   */
  int addConverter (const SBMLConverter* converter);
  
  /** 
   * Return the converter with the given index
   * 
   * @param index the zero-based index of the converter to return 
   * 
   * @return the converter with the given index, or NULL if not found
   */
  SBMLConverter* getConverterByIndex(int index) const;

  /** 
   * Return the converter that best matches the given configuration properties
   * 
   * @param props the properties to match
   * 
   * @return the converter matching the proeprties or NULL if not found. 
   */
  SBMLConverter* getConverterFor(const ConversionProperties& props) const;

  /** 
   * @return the number of registered converters. 
   */
  int getNumConverters() const;
  
protected:
  /** 
   * protected constructor, use the getInstance() method to access the registry.
   */ 
  SBMLConverterRegistry();
  /** 
   * Destructor
   */
  virtual ~SBMLConverterRegistry();

protected: 
  std::vector<const SBMLConverter*>  mConverters;
   
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* !SBMLConverterRegistry_h */

