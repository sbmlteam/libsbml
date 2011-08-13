/**
 * @file    SBMLLevelVersionConverter.h
 * @brief   Definition of SBMLLevelVersionConverter, the base class for SBML conversion.
 * @author  Sarah Keating
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

#ifndef SBMLLevelVersionConverter_h
#define SBMLLevelVersionConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/SBMLTypes.h>
#include <sbml/validator/ConsistencyValidator.h>
#include <sbml/validator/IdentifierConsistencyValidator.h>
#include <sbml/validator/MathMLConsistencyValidator.h>
#include <sbml/validator/SBOConsistencyValidator.h>
#include <sbml/validator/UnitConsistencyValidator.h>
#include <sbml/validator/OverdeterminedValidator.h>
#include <sbml/validator/ModelingPracticeValidator.h>
#include <sbml/validator/L1CompatibilityValidator.h>
#include <sbml/validator/L2v1CompatibilityValidator.h>
#include <sbml/validator/L2v2CompatibilityValidator.h>
#include <sbml/validator/L2v3CompatibilityValidator.h>
#include <sbml/validator/L2v4CompatibilityValidator.h>
#include <sbml/validator/L3v1CompatibilityValidator.h>
#include <sbml/validator/InternalConsistencyValidator.h>


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


class SBMLLevelVersionConverter : public SBMLConverter
{
public:

  /** @cond doxygen-libsbml-internal */
  
  /* register with the ConversionRegistry */
  static void init();  

  /** @endcond doxygen-libsbml-internal */


  /**
   * Constructor.
   */
  SBMLLevelVersionConverter ();


  /**
   * Copy constructor.
   */
  SBMLLevelVersionConverter(const SBMLLevelVersionConverter&);


  
  /**
   * Destroy this object.
   */
  virtual ~SBMLLevelVersionConverter ();


  /**
   * Assignment operator for SBMLLevelVersionConverter.
   */
  SBMLLevelVersionConverter& operator=(const SBMLLevelVersionConverter&);


  /**
   * Creates and returns a deep copy of this SBMLLevelVersionConverter.
   * 
   * @return a (deep) copy of this SBMLLevelVersionConverter.
   */
  virtual SBMLLevelVersionConverter* clone() const;


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


  /* Convenience functions for this converter */

  /**
   * Returns the target SBML Level for the conversion.
   */
  unsigned int getTargetLevel();


  /**
   * Returns the target SBML version for the conversion.
   */
  unsigned int getTargetVersion();

 
  /**
   * Returns the flag indicating whether the conversion is strict.
   */
  bool getValidityFlag();


#ifndef SWIG

#endif // SWIG



private:
  /** @cond doxygen-libsbml-internal */

  bool conversion_errors(unsigned int errors, bool strictUnits = true);

  /*
   * Predicate returning true if model has strict unit consistency.
   */
  bool hasStrictUnits();

  /*
   * Predicate returning true if model has strict sbo consistency.
   */
  bool hasStrictSBO();



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
#endif  /* SBMLLevelVersionConverter_h */

