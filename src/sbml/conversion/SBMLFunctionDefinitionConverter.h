/**
 * @file    SBMLFunctionDefinitionConverter.h
 * @brief   Definition of SBMLFunctionDefinitionConverter, a converter replacing function definitions
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


#ifndef SBMLFunctionDefinitionConverter_h
#define SBMLFunctionDefinitionConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/SBMLTypes.h>


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


class SBMLFunctionDefinitionConverter : public SBMLConverter
{
public:

  /** @cond doxygen-libsbml-internal */
  
  /* register with the ConversionRegistry */
  static void init();  

  /** @endcond doxygen-libsbml-internal */


  /**
   * Constructor.
   */
  SBMLFunctionDefinitionConverter();


  /**
   * Copy constructor.
   */
  SBMLFunctionDefinitionConverter(const SBMLFunctionDefinitionConverter&);

  /**
   * Creates and returns a deep copy of this SBMLLevelVersionConverter.
   * 
   * @return a (deep) copy of this SBMLLevelVersionConverter.
   */
  virtual SBMLConverter* clone() const;


  /**
   * This function determines whether a given converter matches the 
   * configuration properties given. 
   * 
   * @param props the properties to match
   * 
   * @return <c>true</c> if this covnerter is a match, <c>false</c> otherwise.
   */
  virtual bool matchesProperties(const ConversionProperties &props) const;

  
  /** 
   * the actual conversion 
   * 
   * @return status code represeting success/failure/conversion impossible
   */
  virtual int convert();


  /**
   * @return default properties for the converter
   */
  virtual ConversionProperties getDefaultProperties() const;


private:
  /** @cond doxygen-libsbml-internal */

  bool expandFD_errors(unsigned int errors);

  /** @endcond doxygen-libsbml-internal */


};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

  
#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* SBMLFunctionDefinitionConverter_h */

