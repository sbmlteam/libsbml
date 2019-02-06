/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    RateOfCycles.cpp
 * @brief   Ensures unique variables assigned by rules and events
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
#include <sbml/AssignmentRule.h>
#include <sbml/Reaction.h>
#include <sbml/InitialAssignment.h>
#include <sbml/util/List.h>
#include <sbml/util/memory.h>

#include "RateOfCycles.h"
#include <sbml/util/IdList.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Constraint with the given constraint id.
 */
RateOfCycles::RateOfCycles (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
RateOfCycles::~RateOfCycles ()
{
}


/*
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
RateOfCycles::check_ (const Model& m, const Model& object)
{
  // this rule ony applies in l3v2 and beyond
  if (object.getLevel() < 3 
    || (object.getLevel() == 3 && object.getVersion() == 1))
    return;

  unsigned int n;

  mIdMap.clear();
  mRnSpeciesMap.clear();

  for (n = 0; n < m.getNumRules(); ++n)
  { 
    if (m.getRule(n)->isSetMath())
    {
      if (m.getRule(n)->isRate())
      {
        addRuleDependencies(m, *m.getRule(n));
      }
      else if (m.getRule(n)->isAssignment())
      {
        addAssignmentRuleDependencies(m, *m.getRule(n));
      }
    }
  }

  for (n = 0; n < m.getNumInitialAssignments(); ++n)
  { 
    if (m.getInitialAssignment(n)->isSetMath())
    {
      addInitialAssignmentDependencies(m, *m.getInitialAssignment(n));
    }
  }

  for (n = 0; n < m.getNumReactions(); ++n)
  { 
    if (m.getReaction(n)->isSetKineticLaw()){
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        addReactionDependencies(m, *m.getReaction(n));
      }
    }
  }
  
  // check for self assignment
  checkForSelfAssignment(m);

  determineAllDependencies();
  determineCycles(m);
}
 
void 
RateOfCycles::addReactionDependencies(const Model& m, const Reaction& object)
{
  unsigned int ns;

  /* loop thru the list of any rateOf in the Math
    * if they refer to a variable assigned by rate rule OR
    * a species assigned by reaction
    * add to the map
    * with the variable as key
    */
  List* functions = object.getKineticLaw()->getMath()->getListOfNodes( ASTNode_isFunction );
  for (ns = 0; ns < functions->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( functions->get(ns) );
    if (node->getType() != AST_FUNCTION_RATE_OF)
    {
      continue;
    }
    else
    {
      ASTNode * child = node->getChild(0);
      string   name = child->getName() ? child->getName() : "";
      
      if (m.getRule(name) && m.getRule(name)->isRate())
      {
        addRnSpeciesDependencies(name, object);
      }
      else if (assignedByReaction(m, name))
      {
        addRnSpeciesDependencies(name, object);
      }
    }
  }

  delete functions;

  /* now look  for the edge case where the math uses a <ci>
   * that is the subject of an assignment rule/InitialAssignment that
   * uses a rateOf expression
    */
  List* variables = object.getKineticLaw()->getMath()->getListOfNodes( ASTNode_isName );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";
    if (isEdgeCaseAssignment(m, name))
    {
      addRnSpeciesDependencies(name, object);
    }
  }

  delete variables;


}


void 
RateOfCycles::addRuleDependencies(const Model& m, const Rule& object)
{
  unsigned int ns;
  std::string thisId = object.getVariable();

  /* loop thru the list of any rateOf in the Math
    * if they refer to a variable assigned by rate rule OR
    * a species assigned by reaction
    * add to the map
    * with the variable as key
    */
  List* functions = object.getMath()->getListOfNodes( ASTNode_isFunction );
  for (ns = 0; ns < functions->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( functions->get(ns) );
    if (node->getType() != AST_FUNCTION_RATE_OF)
    {
      continue;
    }
    else
    {
      ASTNode * child = node->getChild(0);
      string   name = child->getName() ? child->getName() : "";
      
      if (m.getRule(name) && m.getRule(name)->isRate())
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
      else if (assignedByReaction(m, name))
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
    }
  }

  delete functions;

  /* now look  for the edge case where the math uses a <ci>
   * that is the subject of an assignment rule that
   * uses a rateOf expression
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";
    if (isEdgeCaseAssignment(m, name))
    {
      mIdMap.insert(pair<const std::string, std::string>(thisId, name));
    }
  }

  delete variables;

}


void 
RateOfCycles::addAssignmentRuleDependencies(const Model& m, const Rule& object)
{
  unsigned int ns;
  std::string thisId = object.getVariable();

  /* loop thru the list of any rateOf in the Math
    * if they refer to a variable assigned by rate rule OR
    * a species assigned by reaction
    * add to the map
    * with the variable as key
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isFunction );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    if (node->getType() != AST_FUNCTION_RATE_OF)
    {
      continue;
    }
    else
    {
      ASTNode * child = node->getChild(0);
      string   name = child->getName() ? child->getName() : "";
      
      if (m.getRule(name) && m.getRule(name)->isRate())
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
      else if (assignedByReaction(m, name))
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
    }
  }

  delete variables;
}


void 
RateOfCycles::addInitialAssignmentDependencies(const Model& m, const InitialAssignment& object)
{
  unsigned int ns;
  std::string thisId = object.getSymbol();

  /* loop thru the list of any rateOf in the Math
    * if they refer to a variable assigned by rate rule OR
    * a species assigned by reaction
    * add to the map
    * with the variable as key
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isFunction );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    if (node->getType() != AST_FUNCTION_RATE_OF)
    {
      continue;
    }
    else
    {
      ASTNode * child = node->getChild(0);
      string   name = child->getName() ? child->getName() : "";
      
      if (m.getRule(name) && m.getRule(name)->isRate())
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
      else if (assignedByReaction(m, name))
      {
        mIdMap.insert(pair<const std::string, std::string>(thisId, name));
      }
    }
  }

  delete variables;
}


void 
RateOfCycles::addRnSpeciesDependencies(const std::string& name, const Reaction &r)
{
  for (unsigned int i = 0; i < r.getNumReactants(); i++)
  {
    mIdMap.insert(pair<const std::string, std::string>
      (r.getReactant(i)->getSpecies(), name));
    mRnSpeciesMap.insert(pair<const std::string, const std::string>
      (r.getId(), r.getReactant(i)->getSpecies()));
  }
  for (unsigned int i = 0; i < r.getNumProducts(); i++)
  {
    mIdMap.insert(pair<const std::string, std::string>
      (r.getProduct(i)->getSpecies(), name));
    mRnSpeciesMap.insert(pair<const std::string, const std::string>
      (r.getId(), r.getProduct(i)->getSpecies()));
  }
}

bool 
RateOfCycles::assignedByReaction(const Model& m, const std::string& id)
{
  bool assigned = false;
  if (m.getSpecies(id) == NULL)
  {
    return false;
  }

  unsigned int n = 0;
  while (!assigned && n < m.getNumReactions())
  {
    const Reaction *r = m.getReaction(n);
    if (r->getReactant(id) != NULL)
    {
      assigned = true;
    }
    else if (r->getProduct(id) != NULL)
    {
      assigned = true;
    }

    n++;
  }

  return assigned;
}


bool 
RateOfCycles::isEdgeCaseAssignment(const Model& m, const std::string& id)
{
  bool isEdgeCase = false;

  const AssignmentRule* ar = m.getAssignmentRuleByVariable(id);
  const InitialAssignment *ia = m.getInitialAssignmentBySymbol(id);

  if (ar != NULL)
  {
    if (ar->isSetMath())
    {
      List* variables = ar->getMath()->getListOfNodes( ASTNode_isFunction );
      for (size_t ns = 0; ns < variables->getSize(); ns++)
      {
        ASTNode* node = static_cast<ASTNode*>( variables->get((unsigned int)ns) );
        if (node->getType() != AST_FUNCTION_RATE_OF)
        {
          continue;
        }
        else
        {
          isEdgeCase = true;
        }
      }
      delete variables;
    }
  }
  else if (ia != NULL)
  {
    if (ia->isSetMath())
    {
      List* variables = ia->getMath()->getListOfNodes( ASTNode_isFunction );
      for (size_t ns = 0; ns < variables->getSize(); ns++)
      {
        ASTNode* node = static_cast<ASTNode*>( variables->get((unsigned int)ns) );
        if (node->getType() != AST_FUNCTION_RATE_OF)
        {
          continue;
        }
        else
        {
          isEdgeCase = true;
        }
      }
      delete variables;
    }
  }

  return isEdgeCase;
}

void 
RateOfCycles::determineAllDependencies()
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
RateOfCycles::alreadyExistsInMap(IdMap map, 
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
RateOfCycles::checkForSelfAssignment(const Model& m)
{
  IdIter the_iterator;

  for (the_iterator = mIdMap.begin();
    the_iterator != mIdMap.end(); the_iterator++)
  {
    if ((*the_iterator).first == (*the_iterator).second)
    {
      logMathRefersToSelf(m, (*the_iterator).first);
    }
  }
}


void 
RateOfCycles::determineCycles(const Model& m)
{
  IdIter it;
  IdRange range;
  IdList variables;
  IdMap logged;
  std::vector<IdList> cycle; 
  std::string id;
  variables.clear();
  cycle.clear();

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
  IdList temp, temp2;
  unsigned int n = 0;
  while (n < variables.size())
  {
    temp.clear();
    id = variables.at((int)n);
    temp.append(id);
    range = mIdMap.equal_range(id);
    for (it = range.first; it != range.second; it++)
    {
      //if (((*it).second != id)
      //  && (variables.contains((*it).second))
      //  && !alreadyExistsInMap(logged, pair<const std::string, std::string>(id, (*it).second))
      //  && !alreadyExistsInMap(logged, pair<const std::string, std::string>((*it).second, id)))
      //{
      //  logCycle(m, id, (*it).second);
      //  logged.insert(pair<const std::string, std::string>(id, (*it).second));
      //}

      if ((*it).second != id)
      {
        temp.append((*it).second);  
      }
    }
    if (temp.size() > 1 && !alreadyExistsInCycle(cycle, temp))
    {
      cycle.push_back(temp);

      logCycle(m, temp);
    }
    n++;

  }
}
 

bool 
RateOfCycles::alreadyExistsInCycle(std::vector<IdList> cycle, IdList list)
{
  bool exists = false;

  unsigned int n = 0;
  while (!exists && n < cycle.size())
  {
    if (containSameElements(list, cycle.at(n)))
    {
      exists = true;
    }
    n++;
  }
  return exists;
}

bool 
RateOfCycles::containSameElements(IdList listA, IdList listB)
{
  bool same = true;

  if (listA.size() != listB.size())
    return false;

  unsigned int n = 0;
  while(same && n < listA.size())
  {
    if (!listB.contains(listA.at(n)))
    {
      same = false;
    }
    n++;
  }

  return same;

}

const SBase* getObject(const Model&m, const std::string& id)
{
  const SBase* object = NULL;
  
  object = static_cast<const SBase*>(m.getSpecies(id));
  if (object == NULL)
  {
    object = static_cast<const SBase*>(m.getRuleByVariable(id));
  }
  if (object == NULL)
  {
    object = static_cast<const SBase*>(m.getInitialAssignmentBySymbol(id));
  }

  return object;
}


/*
  * Logs a message about an undefined &lt;ci&gt; element in the given
  * FunctionDefinition.
  */
void
RateOfCycles::logCycle (const Model& m, IdList cycle)
{
  std::string objRef;
  std::string message;
  const SBase *cycleObj = NULL;

  // just in case
  if (cycle.size() == 0)
    return;

  std::string id = cycle.at(0);

  const SBase* object = getObject(m, id);

  // again just in case if we get here there is a problem
  if (object == NULL)
    return;


  for (unsigned int i = 1; i < cycle.size(); i++)
  {
    if (i == 1)
      message += " ";
    else
      message += ", ";
    cycleObj = getObject(m, cycle.at(i));
    getReference(cycleObj, message);
  }

  message += ".";
  logCycle(object, message);

 
}  


void
RateOfCycles::logCycle ( const SBase* object, std::string& message)
{
  std::string objectEl = object->getElementName();
  std::string objRef;

  getReference(object, objRef);

  msg = "The ";
  msg += objRef;
  msg += "creates a cycle with the following: ";
  msg += message;

  
  logFailure(*object);
}


void 
RateOfCycles::getReference(const SBase* object, std::string& ref)
{
  if (object == NULL)
  {
    ref += "invalid object";
  }
  else
  {
    int objectTC = object->getTypeCode();
    ref += "<";
    ref += object->getElementName();
    ref += "> with ";
    switch (objectTC)
    {
    case SBML_ASSIGNMENT_RULE:
    case SBML_RATE_RULE:
      ref += "variable '";
      ref += static_cast<const Rule*>(object)->getVariable();
      ref += "'";
      break;
    case SBML_INITIAL_ASSIGNMENT:
      ref += "symbol '";
      ref += static_cast<const InitialAssignment*>(object)->getSymbol();
      ref += "'";
      break;
    case SBML_SPECIES:
      ref += "id '";
      ref += static_cast<const Species*>(object)->getId();
      ref += "' (as it occurs as reactant/product in a reaction)";
      break;
    default:
      ref = "unexpected object";
      break;
    }
  }
}

void
RateOfCycles::logMathRefersToSelf (const Model& m, std::string id)
{
  if (m.getSpecies(id))
  {
    std::string rnId;
    IdIter it;

    // find the reaction that this species is in
    /// not totally fail safe but gets there most of the time
    for (it = mRnSpeciesMap.begin(); it != mRnSpeciesMap.end(); it++)
    {
      if ((*it).first == id)
      {
        rnId = (*it).second;
        break;
      }
    }
    if (!rnId.empty())
    {
      logMathRefersToSelf(m.getReaction(rnId)->getKineticLaw()->getMath(), 
                static_cast <const SBase * > (m.getSpecies(id)));
    }
    else if (m.getNumReactions() > 0)
    {
      logMathRefersToSelf(m.getReaction(0)->getKineticLaw()->getMath(), 
          static_cast <const SBase * > (m.getSpecies(id)));
    }
  }
  else if (m.getRule(id))
  {
    logMathRefersToSelf(m.getRule(id)->getMath(), 
              static_cast <const SBase * > (m.getRule(id)));
  }

}  
  
  
  
void
RateOfCycles::logMathRefersToSelf (const ASTNode * node,
                                             const SBase* object)
{
  char * formula = SBML_formulaToString(node);   
  std::string ref;
  getReference(object, ref);
  msg = "The ";

  msg += ref;
  msg += " refers to itself within the math formula '";
  msg += formula;
  msg += "'.";
  safe_free(formula);
  
  logFailure(*object);

}

  

#endif /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END
/** @endcond */

