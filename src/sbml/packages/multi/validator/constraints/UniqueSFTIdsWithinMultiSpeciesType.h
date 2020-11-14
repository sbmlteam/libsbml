/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueSFTIdsWithinMultiSpeciesType.h
 * @brief   Ensures the SpeciesFeatureType ids within a MultiSpeciesType are unique
 * @author  Fengkai Zhang
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 */

#ifndef UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType_h
#define UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType_h


#ifdef __cplusplus

#include <string>

#include "UniqueMultiIdBase.h"

LIBSBML_CPP_NAMESPACE_BEGIN

class UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType: public UniqueMultiIdBase
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType (unsigned int id, MultiValidator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType ();


protected:

  /**
   * Checks that all the SpeciesFeatureType ids under the direct parent MultiSpeciesType objects are unique.
   */
  virtual void doCheck (const Model& st);
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType_h */

/** @endcond */
