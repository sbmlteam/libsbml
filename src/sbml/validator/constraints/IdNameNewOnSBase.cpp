/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    IdNameNewOnSBase.cpp
 * @brief   Reports id and name used on SBase but not before
 * @author  Sarah Keating
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

#include <cstring>

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Reaction.h>
#include <sbml/InitialAssignment.h>
#include <sbml/util/List.h>
#include <sbml/util/memory.h>

#include "IdNameNewOnSBase.h"
#include <sbml/util/IdList.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Constraint with the given constraint id.
 */
IdNameNewOnSBase::IdNameNewOnSBase (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
IdNameNewOnSBase::~IdNameNewOnSBase ()
{
}


/*
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
IdNameNewOnSBase::check_ (const Model& m, const Model& object)
{
  // only applies to an l3v2 model
  if (m.getLevel() != 3) return;
  else if (m.getVersion() == 1) return;

  unsigned int i, j;

  /* check all things that did not have id/name before l3v2 */

  /* all listOfs on model*/
  checkObject(m.getListOfFunctionDefinitions());
  checkObject(m.getListOfUnitDefinitions());
  checkObject(m.getListOfCompartments());
  checkObject(m.getListOfSpecies());
  checkObject(m.getListOfParameters());
  checkObject(m.getListOfRules());
  checkObject(m.getListOfInitialAssignments());
  checkObject(m.getListOfConstraints());
  checkObject(m.getListOfReactions());
  checkObject(m.getListOfEvents());

  /* all child listOfs */
  for (i = 0; i < m.getNumUnitDefinitions(); i++)
  {
    checkObject(m.getUnitDefinition(i)->getListOfUnits());
  }
  for (i = 0; i < m.getNumReactions(); i++)
  {
    const Reaction *r = m.getReaction(i);
    checkObject(r->getListOfModifiers());
    checkObject(r->getListOfProducts());
    checkObject(r->getListOfReactants());
    if (r->isSetKineticLaw())
    {
      checkObject(r->getKineticLaw()->getListOfLocalParameters());
    }
  }
  for (i = 0; i < m.getNumEvents(); i++)
  {
    checkObject(m.getEvent(i)->getListOfEventAssignments());
  }

  /* individual object which did not previously have id/name */
  for (i = 0; i < m.getNumInitialAssignments(); i++)
  {
    checkObject(m.getInitialAssignment(i));
  }
  for (i = 0; i < m.getNumRules(); i++)
  {
    checkObject(m.getRule(i));
  }
  for (i = 0; i < m.getNumConstraints(); i++)
  {
    checkObject(m.getConstraint(i));
  }
  for (i = 0; i < m.getNumUnitDefinitions(); i++)
  {
    for (j = 0; j < m.getUnitDefinition(i)->getNumUnits(); j++)
    {
      checkObject(m.getUnitDefinition(i)->getUnit(j));
    }
  }
  for (i = 0; i < m.getNumReactions(); i++)
  {
    if (m.getReaction(i)->isSetKineticLaw())
    {
      checkObject(m.getReaction(i)->getKineticLaw());
    }
  }
  for (i = 0; i < m.getNumEvents(); i++)
  {
    if (m.getEvent(i)->isSetTrigger())
    {
      checkObject(m.getEvent(i)->getTrigger());
    }
    if (m.getEvent(i)->isSetDelay())
    {
      checkObject(m.getEvent(i)->getDelay());
    }
    if (m.getEvent(i)->isSetPriority())
    {
      checkObject(m.getEvent(i)->getPriority());
    }
    for (j = 0; j < m.getEvent(i)->getNumEventAssignments(); j++)
    {
      checkObject(m.getEvent(i)->getEventAssignment(j));
    }
  }
}

void
IdNameNewOnSBase::checkObject(const SBase* object)
{
  if (object->isSetIdAttribute())
    logIdNameUsed(object, "id");
  else if (object->isSetName())
    logIdNameUsed(object, "name");
}

void
IdNameNewOnSBase::logIdNameUsed(const SBase* parent, const std::string& attribute)
{
  msg = "The ";
  if (parent->getTypeCode() == SBML_LIST_OF)
  {
    msg += "listOf";
    msg += SBMLTypeCode_toString(static_cast<const ListOf*>(parent)->getItemTypeCode(), parent->getPackageName().c_str());
    msg += "s";
  }
  else
  {
    msg += SBMLTypeCode_toString(parent->getTypeCode(), parent->getPackageName().c_str());
  }
  msg += " object has the attribute '";
  msg += attribute;
  msg += "' which was not present in earlier levels and versions.";
  
  logFailure(*parent);
}


#endif /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END
/** @endcond */

