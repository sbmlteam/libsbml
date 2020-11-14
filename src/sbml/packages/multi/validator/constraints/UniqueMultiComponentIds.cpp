/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueMultiComponentIds.cpp
 * @brief   Ensures the appropriate ids within a Model are unique
 * @author  Fengkai Zhang
 * @author  Sarah Keating
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
#include "UniqueMultiComponentIds.h"

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Constraint with the given constraint id.
 */
UniqueMultiComponentIds::UniqueMultiComponentIds (unsigned int id, MultiValidator& v) :
  UniqueMultiIdBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueMultiComponentIds::~UniqueMultiComponentIds ()
{
}


/*
 * Checks that all ids on the following Model objects are unique:
 * FunctionDefinitions, Species, Compartments, global Parameters,
 * Reactions, Events, and MultiSpeciesTypes.
 */
void
UniqueMultiComponentIds::doCheck (const Model& m)
{
  unsigned int n, size, sr, sr_size, i, i_size, j, j_size;

  checkId( m );

  size = m.getNumFunctionDefinitions();
  for (n = 0; n < size; ++n) checkId( *m.getFunctionDefinition(n) );

  size = m.getNumCompartments();
  for (n = 0; n < size; ++n) checkId( *m.getCompartment(n) );

  size = m.getNumSpecies();
  for (n = 0; n < size; ++n) checkId( *m.getSpecies(n) );

  size = m.getNumParameters();
  for (n = 0; n < size; ++n) checkId( *m.getParameter(n) );

  size = m.getNumReactions();
  for (n = 0; n < size; ++n) 
  {
    checkId( *m.getReaction(n) );

    sr_size = m.getReaction(n)->getNumReactants();
    for (sr = 0; sr < sr_size; sr++)
    {
      checkId(*m.getReaction(n)->getReactant(sr));
    }

    sr_size = m.getReaction(n)->getNumProducts();
    for (sr = 0; sr < sr_size; sr++)
    {
      checkId(*m.getReaction(n)->getProduct(sr));
    }

    sr_size = m.getReaction(n)->getNumModifiers();
    for (sr = 0; sr < sr_size; sr++)
    {
      checkId(*m.getReaction(n)->getModifier(sr));
    }

  }

  size = m.getNumEvents();
  for (n = 0; n < size; ++n) checkId( *m.getEvent(n) );


  const MultiModelPlugin * plug =
    dynamic_cast <const MultiModelPlugin*>(m.getPlugin("multi"));
  if (plug == NULL)
  {
    return;
  }

  size = plug->getNumMultiSpeciesTypes();
  for (n = 0; n < size; ++n)
  {
	const MultiSpeciesType * multiSpeciesType = plug->getMultiSpeciesType(n);
    checkId( *(multiSpeciesType) );

    i_size = multiSpeciesType->getNumSpeciesFeatureTypes();
    for (i = 0; i < i_size; ++i)
    {
    	const SpeciesFeatureType * speciesFeatureType
    		= multiSpeciesType->getSpeciesFeatureType(i);
    	j_size = speciesFeatureType->getNumPossibleSpeciesFeatureValues();
    	for (j = 0; j < j_size; ++j) {
    		const PossibleSpeciesFeatureValue * possibleSpeciesFeatureValue
    			= speciesFeatureType->getPossibleSpeciesFeatureValue(j);
    		checkId ( *(possibleSpeciesFeatureValue) );
    	}
    }
  }

  reset();
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END

/** @endcond */
