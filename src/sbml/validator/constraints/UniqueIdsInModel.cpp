/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueIdsInModel.cpp
 * @brief   Ensures the appropriate ids within a Model are unique
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/Model.h>
#include <sbml/ModifierSpeciesReference.h>
#include "UniqueIdsInModel.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

static const char* PREAMBLE =
    "The value of the 'id' field on every instance of the following type of "
    "object in a model must be unique: <model>, <functionDefinition>, "
    "<compartmentType>, <compartment>, <speciesType>, <species>, <reaction>, "
    "<speciesReference>, <modifierSpeciesReference>, <event>, and model-wide "
    "<parameter>s. Note that <unitDefinition> and parameters defined inside "
    "a reaction are treated separately. (References: L2V1 Section 3.5; L2V2 "
    "Section 3.4; L2V3 Section 3.3.)";


/*
 * Creates a new Constraint with the given constraint id.
 */
UniqueIdsInModel::UniqueIdsInModel (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueIdsInModel::~UniqueIdsInModel ()
{
}


/*
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueIdsInModel::getPreamble ()
{
  return PREAMBLE;
}


/*
 * Checks that all ids on the following Model objects are unique:
 * FunctionDefinitions, Species, Compartments, global Parameters,
 * Reactions, and Events.
 */
void
UniqueIdsInModel::doCheck (const Model& m)
{
  // from l3v2 all sbase objects may have an id
  if (m.getLevel() == 3 && m.getVersion() > 1)
  {
    doAllIdCheck(m);
  }
  else
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

  }
  reset();
}

void
UniqueIdsInModel::doAllIdCheck (const Model& m)
{
  unsigned int n, size, j, num;

  /* check any id on the sbml container */
  doCheckId((SBase&)(*m.getSBMLDocument()));

  doCheckId( m );

  size = m.getNumFunctionDefinitions();
  doCheckId(*m.getListOfFunctionDefinitions());
  for (n = 0; n < size; ++n) doCheckId( *m.getFunctionDefinition(n) );

  size = m.getNumUnitDefinitions();
  doCheckId(*m.getListOfUnitDefinitions());
  for (n = 0; n < size; ++n) 
  {
    // unitDefinitions have their own rule
    const UnitDefinition *ud = m.getUnitDefinition(n);
    //doCheckId( *ud );
    num = ud->getNumUnits();
    doCheckId(*ud->getListOfUnits());
    for (j = 0; j < num; j++)
    {
      doCheckId(*ud->getUnit(j));
    }
  }

  size = m.getNumCompartments();
  doCheckId(*m.getListOfCompartments());
  for (n = 0; n < size; ++n) doCheckId( *m.getCompartment(n) );

  size = m.getNumSpecies();
  doCheckId(*m.getListOfSpecies());
  for (n = 0; n < size; ++n) doCheckId( *m.getSpecies(n) );

  size = m.getNumParameters();
  doCheckId(*m.getListOfParameters()); 
  for (n = 0; n < size; ++n) doCheckId( *m.getParameter(n) );

  size = m.getNumInitialAssignments();
  doCheckId(*m.getListOfInitialAssignments()); 
  for (n = 0; n < size; ++n) doCheckId( *m.getInitialAssignment(n) );

  size = m.getNumRules();
  doCheckId(*m.getListOfRules()); 
  for (n = 0; n < size; ++n) doCheckId( *m.getRule(n) );

  size = m.getNumConstraints();
  doCheckId(*m.getListOfConstraints()); 
  for (n = 0; n < size; ++n) doCheckId( *m.getConstraint(n) );

  size = m.getNumReactions();
  doCheckId(*m.getListOfReactions()); 
  for (n = 0; n < size; ++n) 
  {
    const Reaction *r = m.getReaction(n);
    doCheckId( *r );

    if (r->isSetKineticLaw())
    {
      doCheckId(*r->getKineticLaw());
      // local parameters do not apply
      num = r->getKineticLaw()->getNumParameters();
      doCheckId(*r->getKineticLaw()->getListOfParameters());
      //for (j = 0; j < num; j++)
      //{
      //  doCheckId(*r->getKineticLaw()->getParameter(j));
      //}
    }

    num = r->getNumReactants();
    doCheckId(*r->getListOfReactants());
    for (j = 0; j < num; j++)
    {
      doCheckId(*r->getReactant(j));
    }

    num = r->getNumProducts();
    doCheckId(*r->getListOfProducts());
    for (j = 0; j < num; j++)
    {
      doCheckId(*r->getProduct(j));
    }

    num = r->getNumModifiers();
    doCheckId(*r->getListOfModifiers());
    for (j = 0; j < num; j++)
    {
      doCheckId(*r->getModifier(j));
    }
  }
  size = m.getNumEvents();
  doCheckId(*m.getListOfEvents()); 
  for (n = 0; n < size; ++n) 
  {
    const Event *e = m.getEvent(n);
    doCheckId( *e );
 
    if (e->isSetTrigger())
    {
      doCheckId( *e->getTrigger());
    }

    if (e->isSetDelay())
    {
      doCheckId( *e->getDelay());
    }

    if (e->isSetPriority())
    {
      doCheckId( *e->getPriority());
    }

    num = e->getNumEventAssignments();
    doCheckId(*e->getListOfEventAssignments());
    for (j = 0; j < num; j++)
    {
      doCheckId(*e->getEventAssignment(j));
    }
  }

}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
