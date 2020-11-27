/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueSLOSFIdsWithinSpecies.cpp
 * @brief   Ensures the SpeciesFeature ids within a Species are unique
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/Model.h>
#include <sbml/packages/multi/sbml/SubListOfSpeciesFeatures.h>
#include "UniqueSLOSFIdsWithinSpecies.h"

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Constraint with the given constraint id.
 */
UniqueSubListOfSpeciesFeaturesIdsWithinSpecies::UniqueSubListOfSpeciesFeaturesIdsWithinSpecies (unsigned int id, MultiValidator& v) :
  UniqueMultiIdBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueSubListOfSpeciesFeaturesIdsWithinSpecies::~UniqueSubListOfSpeciesFeaturesIdsWithinSpecies ()
{
}


/*
 * Checks that all the SpeciesFeature ids within a Species are unique.
 */
void
UniqueSubListOfSpeciesFeaturesIdsWithinSpecies::doCheck (const Model& m)
{
  for (unsigned int n = 0; n < m.getNumSpecies(); n++)
  {
    const Species* sp = m.getSpecies(n);
    if (sp == NULL) continue;

    const MultiSpeciesPlugin * plug =
      dynamic_cast <const MultiSpeciesPlugin*>(sp->getPlugin("multi"));
    if (plug == NULL)
    {
      continue;
    }

    for (unsigned int i = 0; i < plug->getNumSubListOfSpeciesFeatures(); i++)
    {
        const SubListOfSpeciesFeatures * sf = plug->getSubListOfSpeciesFeatures(i);
        checkId( *sf );
    }

    reset();
  }

}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END

/** @endcond */
