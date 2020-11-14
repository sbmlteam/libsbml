/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    LOMembersConsistentReferences.cpp
 * @brief   Checks sboTerm for consistency
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
#include <sbml/util/IdList.h>
#include <sbml/packages/groups/extension/GroupsModelPlugin.h>

#include "LOMembersConsistentReferences.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new Constraint with the given constraint id.
 */
LOMembersConsistentReferences::LOMembersConsistentReferences (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
  reported.clear();
}


/**
 * Destroys this Constraint.
 */
LOMembersConsistentReferences::~LOMembersConsistentReferences ()
{
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
LOMembersConsistentReferences::check_ (const Model& m, const Model& /*object*/)
{
  const GroupsModelPlugin * plug = 
                 static_cast<const GroupsModelPlugin*>(m.getPlugin("groups"));
  
  if (plug == NULL) 
  {
    return;
  }

  unsigned int numGroups = plug->getNumGroups();
  if (numGroups < 2)
  {
    return;
  }

  for (unsigned int i = 0; i < numGroups; i++)
  {
    const Group * g = plug->getGroup(i);
    if (g->getListOfMembers()->isSetSBOTerm())
    {
      checkForReferences(plug, i, g->getListOfMembers()->getSBOTerm());
    }
  }

}
 
void
LOMembersConsistentReferences::checkForReferences(const GroupsModelPlugin* plug, 
                                          unsigned int index, int sbo)
{
  List* referenced = new List();

  // populate the members in the target list
  const Model* m = static_cast<const Model*>(plug->getParentSBMLObject());
  const Group* g = plug->getGroup(index);
  referenced->add((void*)(g->getListOfMembers()));
  for (unsigned int i = 0; i < g->getNumMembers(); i++)
  {
    const Member * mem = g->getMember(i);
    if (mem->isSetIdRef())
    {
      referenced->add(const_cast<Model*>(m)->getElementBySId(mem->getIdRef()));
    }
    else if (mem->isSetMetaIdRef())
    {
      referenced->add(const_cast<Model*>(m)->getElementByMetaId(mem->getMetaIdRef()));
    }
  }

  for (unsigned int i = 0; i < plug->getNumGroups(); i++)
  {
    if (i == index) continue;

    unsigned int sboTerm = plug->getGroup(i)->getListOfMembers()->getSBOTerm();

    if (!sboTermsConsistent(sboTerm, sbo) && 
         matchesReferences(referenced, plug->getGroup(i)))
    {
      if (!matchAlreadyReported(index, i))
      {
        reported.push_back(make_pair(index, i));
        logInconsistentReference(*(plug->getGroup(i)), *g);
      }
    }
  }

  delete referenced;

}

bool
LOMembersConsistentReferences::matchAlreadyReported(unsigned int parent, unsigned int match)
{
  bool matchReported = false;
  for (std::vector<std::pair<unsigned int, unsigned int> >::iterator it = reported.begin();
    it != reported.end(); ++it)
  {
    std::pair<unsigned int, unsigned int> value = *it;
    if (value.first == parent && value.second == match)
    {
      matchReported = true;
    }
    else if (value.first == match && value.second == parent)
    {
      matchReported = true;
    }
  }
  return matchReported;
}

bool
LOMembersConsistentReferences::matchesReferences(List *referenced, const Group* g)
{
  bool match = false;

  for (unsigned int i = 0; i < g->getNumMembers(); i++)
  {
    SBase * referent = const_cast<Member*>(g->getMember(i))->getReferencedElement();

    for (unsigned int n = 0; n < referenced->getSize(); n++)
    {
      if (referent == referenced->get(n))
      {
        match = true;
        break;
      }
    }
  }

  return match;
}

bool 
LOMembersConsistentReferences::sboTermsConsistent(unsigned int parent, 
                                                  unsigned int match)
{
  bool consistent = false;

  if (parent == match)
  {
    consistent = true;
  }
  else if (SBO::getParentBranch(parent) == SBO::getParentBranch(match))
  {
    consistent = true;
  }

  return consistent;
}


void
LOMembersConsistentReferences::logInconsistentReference (const Group& g,
                          const Group& object)
{
  msg =
    "The <group> with whose <listOfMembers> has sboTerm = '";
  msg += SBO::intToString(g.getListOfMembers()->getSBOTerm());
  msg += "' includes the same member as the <group> whose";
  msg += " <listOfMembers> has sboTerm = '";
  msg += SBO::intToString(object.getListOfMembers()->getSBOTerm());
  msg += "' which are not consistent.";
  
  logFailure(object);

}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
