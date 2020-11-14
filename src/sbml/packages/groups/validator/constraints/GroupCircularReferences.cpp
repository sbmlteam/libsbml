/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    GroupCircularReferences.cpp
 * @brief   Checks cycles in groups
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
#include <sbml/packages/groups/extension/GroupsModelPlugin.h>
#include <sbml/util/IdList.h>

#include "GroupCircularReferences.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new Constraint with the given constraint id.
 */
GroupCircularReferences::GroupCircularReferences (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
GroupCircularReferences::~GroupCircularReferences ()
{
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
GroupCircularReferences::check_ (const Model& m, const Model& /*object*/)
{
  mIdMap.clear();

  const GroupsModelPlugin * plug = 
                 static_cast<const GroupsModelPlugin*>(m.getPlugin("groups"));
  
  if (plug == NULL) 
  {
    return;
  }

  unsigned int numGroups = plug->getNumGroups();

  for (unsigned int i = 0; i < numGroups; i++)
  {
    checkForSelfReference(plug->getGroup(i), i);
    addReferenced(m, plug->getGroup(i));
  }

  determineAllDependencies();
  determineCycles(m);

  return;

}

void
GroupCircularReferences::addReferenced(const Model& /*model*/, const Group * group)
{
  for (unsigned int i = 0; i < group->getNumMembers(); i++)
  {
    const Member* mem = group->getMember(i);
    // if the member has no id or metaid it can never be referenced
    if (!mem->isSetId() && !mem->isSetMetaId())
      continue;
    else
      addAllReferences(mem);
  }
}
 

void 
GroupCircularReferences::addAllReferences(const Member* mem)
{
  std::string ref;
  if (mem->isSetIdRef())
  {
    ref = mem->getIdRef();
  }
  else
  {
    ref = mem->getMetaIdRef();
  }

  if (mem->isSetId())
  { 
    const std::string id = mem->getId();
    mIdMap.insert(pair<const std::string, std::string>(id, ref));
    addChildReferences(mem, id);
  }
  if (mem->isSetMetaId())
  { 
    const std::string id = mem->getMetaId();
    mIdMap.insert(pair<const std::string, std::string>(id, ref));
    addChildReferences(mem, id);
  }

}

void 
GroupCircularReferences::addChildReferences(const Member* mem, const std::string& id)
{
    SBase* element = const_cast<Member*>(mem)->getReferencedElement();
    
    if (element == NULL)
      return;

    if (element->getTypeCode() == SBML_LIST_OF)
    {
      if (static_cast<ListOf*>(element)->getItemTypeCode() == SBML_GROUPS_MEMBER)
      {
        for (unsigned int i = 0; i < static_cast<ListOf*>(element)->size(); i++)
        {
          Member* child = static_cast<Member*>(static_cast<ListOf*>(element)->get(i));
          if (child->isSetId())
          {
            mIdMap.insert(pair<const std::string, std::string>(id, child->getId()));
          }
          if (child->isSetMetaId())
          {
            mIdMap.insert(pair<const std::string, std::string>(id, child->getMetaId()));
          }
        }
      }
    }
    else if (element->getTypeCode() == SBML_GROUPS_GROUP)
    {
      for (unsigned int i = 0; i < static_cast<Group*>(element)->getNumMembers(); i++)
      {
        Member* child = static_cast<Group*>(element)->getMember(i);
        if (child->isSetId())
        {
          mIdMap.insert(pair<const std::string, std::string>(id, child->getId()));
        }
        if (child->isSetMetaId())
        {
          mIdMap.insert(pair<const std::string, std::string>(id, child->getMetaId()));
        }
      }
    }
}

void 
GroupCircularReferences::determineAllDependencies()
{
  IdIter iterator;
  IdIter inner_it;
  IdRange range;

  /* for each pair in the map (x, y)
   * retrieve all other pairs where y is first (e.g. (y, s))
   * and create pairs showing that x depends on these e.g. (x, s)
   * check whether the pair already exists in the map
   * and add it if not
   */
  for (iterator = mIdMap.begin(); iterator != mIdMap.end(); iterator++)
  {
    range = mIdMap.equal_range((*iterator).second);
    for (inner_it = range.first; inner_it != range.second; inner_it++)
    {
      const pair<const std::string, std::string> &depend = 
            pair<const std::string, std::string>((*iterator).first, (*inner_it).second);
      if (!alreadyExistsInMap(mIdMap, depend))
        mIdMap.insert(depend);
    }
  }
}


bool 
GroupCircularReferences::alreadyExistsInMap(IdMap map, 
                                     pair<const std::string, std::string> dependency)
{
  bool exists = false;

  IdIter it;
  
  for (it = map.begin(); it != map.end(); it++)
  {
    if (((*it).first == dependency.first)
      && ((*it).second == dependency.second))
      exists = true;
  }

  return exists;
}

  
void 
GroupCircularReferences::determineCycles(const Model& m)
{
  IdIter it;
  IdRange range;
  IdList variables;
  IdMap logged;
  std::string id;
  variables.clear();

  /* create a list of variables that are cycles ie (x, x) */
  for (it = mIdMap.begin(); it != mIdMap.end(); it++)
  {
    if ((*it).first == (*it).second)
    {
      id = (*it).first;
      if (!variables.contains(id))
      {
        variables.append(id);
      }
    }
  }

  /* loop thru other dependencies for each; if the dependent is also
   * in the list then this is the cycle
   * keep a record of logged dependencies to avoid logging twice
   */
   
  for (unsigned int n = 0; n < variables.size(); n++)
  {
    id = variables.at((int)n);
    range = mIdMap.equal_range(id);
    for (it = range.first; it != range.second; it++)
    {
      if (((*it).second != id)
        && (variables.contains((*it).second))
        && !alreadyExistsInMap(logged, pair<const std::string, std::string>(id, (*it).second))
        && !alreadyExistsInMap(logged, pair<const std::string, std::string>((*it).second, id)))
      {
        logCycle(m, id, (*it).second);
        logged.insert(pair<const std::string, std::string>(id, (*it).second));
      }
    }
  }
}
 



void
GroupCircularReferences::checkForSelfReference(const Group* g, unsigned int index)
{
  IdList ids;

  //create a list that members cannot refer to
  if (g->isSetId())
  {
    ids.append(g->getId());
  }
  if (g->isSetMetaId())
  {
    ids.append(g->getMetaId());
  }
  if (g->getListOfMembers()->isSetId())
  {
    ids.append(g->getListOfMembers()->getId());
  }
  if (g->getListOfMembers()->isSetMetaId())
  {
    ids.append(g->getListOfMembers()->getMetaId());
  }

  for (unsigned int i = 0; i < g->getNumMembers(); i++)
  {
    const Member * mem = g->getMember(i);
    if (mem->isSetIdRef())
    {
      std::string idRef = mem->getIdRef();
      if (mem->isSetId() && mem->getId() == idRef)
      {
        logSelfReference(*mem, *g, index);
      }
      else
      {
        if (ids.contains(idRef))
        {
          logParentReference(*mem, *g, index);
        }
      }
    }
    else if (mem->isSetMetaIdRef())
    {
      std::string idRef = mem->getMetaIdRef();
      if (mem->isSetMetaId() && mem->getMetaId() == idRef)
      {
        logSelfReference(*mem, *g, index);
      }
      else
      {
        if (ids.contains(idRef))
        {
          logParentReference(*mem, *g, index);
        }
      }
    }
  }


}

void
GroupCircularReferences::logSelfReference (const Member& member,
                          const Group& group, unsigned int index)
{
  ostringstream oss;

  if (group.isSetId())
  {
    oss << "In the <group> with id '" <<  group.getId() << "' ";
  }
  else
  {
    oss << "In <group> listed in place " << index << " ";
  }
  if (member.isSetIdRef())
  {
    oss << "the <member> with id '" << member.getId() << "' refers to itself.";
  }
  else
  {
    oss << "the <member> with metaid '" << member.getMetaId() 
      << "' refers to itself.";
  }
  msg = oss.str();
 
  logFailure(member);

}

void
GroupCircularReferences::logParentReference (const Member& member,
                          const Group& group, unsigned int index)
{
  ostringstream oss;

  if (group.isSetId())
  {
    oss << "In the <group> with id '" << group.getId() << "' ";
  }
  else
  {
    oss << "In <group> listed in place " << index << " ";
  }

  std::string ref;
  std::string ref_type;

  if (member.isSetIdRef())
  {
    ref = member.getIdRef();
    if (group.getId() == ref)
    {
      ref_type = "<group>.";
    }
    else
    {
      ref_type = "<listOfMembers>.";
    }
  }
  else
  {
    ref = member.getMetaIdRef();
    if (group.getMetaId() == ref)
    {
      ref_type = "<group>.";
    }
    else
    {
      ref_type = "<listOfMembers>.";
    }
  }


  oss << "the <member> referencing '" << ref << "' refers to its parent " 
    << ref_type;
  msg = oss.str();
 
  logFailure(member);

}

void 
GroupCircularReferences::logCycle (const Model& m, std::string id, std::string id1)
{
  Member * mem = static_cast<Member *>(const_cast<Model&>(m).getElementBySId(id));
  std::string ref_type = "id"; 
  if (mem == NULL)
  {
    mem = static_cast<Member *>(const_cast<Model&>(m).getElementByMetaId(id));
    ref_type = "metaid";
  }
  Member * mem2 = static_cast<Member *>(const_cast<Model&>(m).getElementBySId(id1));
  std::string ref_type1 = "id"; 
  if (mem2 == NULL)
  {
    mem2 = static_cast<Member *>(const_cast<Model&>(m).getElementByMetaId(id1));
    ref_type = "metaid";
  }

  msg = "The <member> with ";
  msg += ref_type;
  msg += " attribute '";
  msg += id;
  msg += "' creates a circular reference with the <member> with ";
  msg += ref_type1;
  msg += " attribute '";
  msg += id1;
  msg += "'.";


  logFailure(m);
}


LIBSBML_CPP_NAMESPACE_END
/** @endcond */
