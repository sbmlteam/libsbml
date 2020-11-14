/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueISTBIdsWithinMultiSpeciesType.cpp
 * @brief   Ensures the InSpeciesTypeBond ids within a MultiSpeciesType are unique
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
#include "UniqueISTBIdsWithinMultiSpeciesType.h"

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Constraint with the given constraint id.
 */
UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType::UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType (unsigned int id, MultiValidator& v) :
  UniqueMultiIdBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType::~UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType ()
{
}


/*
 * Checks that all the InSpeciesTypeBond ids under the direct parent MultiSpeciesType objects are unique.
 */
void
UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType::doCheck (const Model& m)
{
  const MultiModelPlugin * plug =
    dynamic_cast <const MultiModelPlugin*>(m.getPlugin("multi"));
  if (plug == NULL)
  {
    return;
  }

  for (unsigned int n = 0; n < plug->getNumMultiSpeciesTypes(); n++)
  {
    const MultiSpeciesType* spt = plug->getMultiSpeciesType(n);
    if (spt == NULL) continue;

    for (unsigned int i = 0; i < spt->getNumInSpeciesTypeBonds(); i++)
    {
        const InSpeciesTypeBond * isb = spt->getInSpeciesTypeBond(i);
        checkId( *isb );
    }

    reset();
  }

}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END

/** @endcond */
