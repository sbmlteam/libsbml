/**
 * @file    UniqueIdsInModel.cpp
 * @brief   Ensures the appropriate ids within a Model are unique
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/Model.h>
#include "UniqueIdsInModel.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


static const char* PREAMBLE =
    "The value of the 'id' field on every instance of the following type of "
    "object in a model must be unique: <model>, <functionDefinition>, "
    "<compartmentType>, <compartment>, <speciesType>, <species>, <reaction>, "
    "<speciesReference>, <modifierSpeciesReference>, <event>, and model-wide "
    "<parameter>s. Note that <unitDefinition> and parameters defined inside "
    "a reaction are treated separately. (References: L2V1 Section 3.5; L2V2 "
    "Section 3.4)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueIdsInModel::UniqueIdsInModel (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueIdsInModel::~UniqueIdsInModel ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueIdsInModel::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on the following Model objects are unique:
 * FunctionDefinitions, Species, Compartments, global Parameters,
 * Reactions, and Events.
 */
void
UniqueIdsInModel::doCheck (const Model& m)
{
  unsigned int n, size, sr, sr_size;

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

  size = m.getNumCompartmentTypes();
  for (n = 0; n < size; ++n) checkId( *m.getCompartmentType(n) );

  size = m.getNumSpeciesTypes();
  for (n = 0; n < size; ++n) checkId( *m.getSpeciesType(n) );

  reset();
}
