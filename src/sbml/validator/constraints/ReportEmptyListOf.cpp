/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ReportEmptyListOf.cpp
 * @brief   Reports an listOf that has been declared but no children
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

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>

#include "ReportEmptyListOf.h"
#include <sbml/util/IdList.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
 * Creates a new Constraint with the given constraint id.
 */
ReportEmptyListOf::ReportEmptyListOf (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
ReportEmptyListOf::~ReportEmptyListOf ()
{
}


/*
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
ReportEmptyListOf::check_ (const Model& m, const Model&)
{
  // only applies to an l3v2 model
  if (m.getLevel() != 3) return;
  else if (m.getVersion() == 1) return;

  if (isEmptyListOf(m.getListOfFunctionDefinitions()))
  {
    logEmptyList(m.getListOfFunctionDefinitions(), m);
  }

  if (isEmptyListOf(m.getListOfCompartments()))
  {
    logEmptyList(m.getListOfCompartments(), m);
  }

  if (isEmptyListOf(m.getListOfSpecies()))
  {
    logEmptyList(m.getListOfSpecies(), m);
  }

  if (isEmptyListOf(m.getListOfParameters()))
  {
    logEmptyList(m.getListOfParameters(), m);
  }

  if (isEmptyListOf(m.getListOfRules()))
  {
    logEmptyList(m.getListOfRules(), m);
  }

  if (isEmptyListOf(m.getListOfInitialAssignments()))
  {
    logEmptyList(m.getListOfInitialAssignments(), m);
  }

  if (isEmptyListOf(m.getListOfConstraints()))
  {
    logEmptyList(m.getListOfConstraints(), m);
  }

  if (isEmptyListOf(m.getListOfReactions()))
  {
    logEmptyList(m.getListOfReactions(), m);
  }

  if (isEmptyListOf(m.getListOfUnitDefinitions()))
  {
    logEmptyList(m.getListOfUnitDefinitions(), m);
  }

  if (isEmptyListOf(m.getListOfEvents()))
  {
    logEmptyList(m.getListOfEvents(), m);
  }

  unsigned int i;
  for (i = 0; i < m.getNumUnitDefinitions(); i++)
  {
    if (isEmptyListOf(m.getUnitDefinition(i)->getListOfUnits()))
    {
      logEmptyList(m.getUnitDefinition(i)->getListOfUnits(), *(m.getUnitDefinition(i)));
    }
  }

  for (i = 0; i < m.getNumEvents(); i++)
  {
    if (isEmptyListOf(m.getEvent(i)->getListOfEventAssignments()))
    {
      logEmptyList(m.getEvent(i)->getListOfEventAssignments(), *(m.getEvent(i)));
    }
  }

  for (i = 0; i < m.getNumReactions(); i++)
  {
    if (isEmptyListOf(m.getReaction(i)->getListOfReactants()))
    {
      logEmptyList(m.getReaction(i)->getListOfReactants(), *(m.getReaction(i)));
    }
    if (isEmptyListOf(m.getReaction(i)->getListOfProducts()))
    {
      logEmptyList(m.getReaction(i)->getListOfProducts(), *(m.getReaction(i)));
    }
    if (isEmptyListOf(m.getReaction(i)->getListOfModifiers()))
    {
      logEmptyList(m.getReaction(i)->getListOfModifiers(), *(m.getReaction(i)));
    }

    if (m.getReaction(i)->isSetKineticLaw())
    {
      const KineticLaw * kl = m.getReaction(i)->getKineticLaw();
      if (isEmptyListOf(kl->getListOfLocalParameters()))
      {
        logEmptyList(kl->getListOfLocalParameters(), *(m.getReaction(i)));
      }
    }
  }

}

bool
ReportEmptyListOf::isEmptyListOf(const ListOf* list)
{
  if (list->size() > 0)
    return false;
  // this will only be set if we have read in a file
  if (list->isExplicitlyListed())
    return true;
  else if (list->hasOptionalAttributes() || list->hasOptionalElements())
    return true;

  return false;
}

void 
ReportEmptyListOf::logEmptyList(const ListOf* list, const SBase& parent)
{
  msg = "The ListOf";
  msg += SBMLTypeCode_toString( list->getItemTypeCode(), list->getPackageName().c_str());
  msg += "s in the ";
  msg += SBMLTypeCode_toString(parent.getTypeCode(), parent.getPackageName().c_str());
  msg += " with id '";
  msg += parent.getId();
  msg += "' has no child ";
  msg += SBMLTypeCode_toString( list->getItemTypeCode(), list->getPackageName().c_str());
  msg += " elements.";
  
  logFailure(*list);
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
