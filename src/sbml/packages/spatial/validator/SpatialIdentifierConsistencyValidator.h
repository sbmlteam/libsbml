
/** @cond doxygenLibsbmlInternal */

/**
 * @file SpatialIdentifierConsistencyValidator.h
 * @brief Definition of SpatialIdentifierConsistencyValidator.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef SpatialIdentifierConsistencyValidator_H__
#define SpatialIdentifierConsistencyValidator_H__




#ifdef __cplusplus


#include <sbml/packages/spatial/validator/SpatialValidator.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class SpatialIdentifierConsistencyValidator : public SpatialValidator
{
public:

  SpatialIdentifierConsistencyValidator()
    : SpatialValidator( LIBSBML_CAT_IDENTIFIER_CONSISTENCY )
  {
  }


  virtual ~SpatialIdentifierConsistencyValidator()
  {
  }


  /**
   * Initializes this Validator with a set of Constraints
   */
  virtual void init();


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#endif /* !SpatialIdentifierConsistencyValidator_H__ */



/** @endcond */

