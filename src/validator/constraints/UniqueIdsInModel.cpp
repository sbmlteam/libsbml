/**
 * \file    UniqueIdsInModel.cpp
 * \brief   Ensures the appropriate ids within a Model are unique
 * \author  Ben Bornstein
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


#include <string>

#include <sbml/Model.h>
#include "UniqueIdsInModel.h"


using namespace std;


static const char* PREAMBLE =
  "Ids must be unique among FunctionDefinitions, Compartments, Species, "
  "global Parameters, Reactions, and Events (L2v1 Section 3.5).";


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
  unsigned int n, size;


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
  for (n = 0; n < size; ++n) checkId( *m.getReaction(n) );

  size = m.getNumEvents();
  for (n = 0; n < size; ++n) checkId( *m.getEvent(n) );

  reset();
}
