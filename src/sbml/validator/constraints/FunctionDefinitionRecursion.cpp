/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    FunctionDefinitionRecursion.cpp
 * @brief   Checks for recursion in functionDefinitions
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

#include "FunctionDefinitionRecursion.h"
#include <sbml/util/IdList.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
 * Creates a new Constraint with the given constraint id.
 */
FunctionDefinitionRecursion::FunctionDefinitionRecursion (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
FunctionDefinitionRecursion::~FunctionDefinitionRecursion ()
{
}


/*
 * Checks that a function does not refer to itself.
 */
void
FunctionDefinitionRecursion::check_ (const Model& m, const Model&)
{
  mIdMap.clear();

  for (unsigned n = 0; n < m.getNumFunctionDefinitions(); ++n)
  { 
    if (m.getFunctionDefinition(n)->isSetMath())
    {
      addDependencies(m, *m.getFunctionDefinition(n));
    }
  }

  // check for self assignment
  checkForSelfAssignment(m);

  determineAllDependencies();
  determineCycles(m);

}

void 
FunctionDefinitionRecursion::addDependencies(const Model& m, 
                                         const FunctionDefinition& object)
{
  unsigned int ns;
  std::string thisId = object.getId();

  /* loop thru the list of names in the Math
    * if they refer to a FunctionDefinition add to the map
    * with the variable as key
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isFunction );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";

    if (m.getFunctionDefinition(name))
    {
      mIdMap.insert(pair<const std::string, std::string>(thisId, name));
    }
  }

  delete variables;
}

void 
FunctionDefinitionRecursion::determineAllDependencies()
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
FunctionDefinitionRecursion::alreadyExistsInMap(IdMap map, 
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
FunctionDefinitionRecursion::checkForSelfAssignment(const Model& m)
{
  IdIter the_iterator;

  for (the_iterator = mIdMap.begin();
    the_iterator != mIdMap.end(); the_iterator++)
  {
    if ((*the_iterator).first == (*the_iterator).second)
    {
      logSelfRecursion(*(m.getFunctionDefinition((*the_iterator).first)), 
        (*the_iterator).first);
    }
  }
}


void 
FunctionDefinitionRecursion::determineCycles(const Model& m)
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
        && !alreadyExistsInMap(logged, 
                   pair<const std::string, std::string>(id, (*it).second))
        && !alreadyExistsInMap(logged, 
                   pair<const std::string, std::string>((*it).second, id)))
      {
        logCycle(m.getFunctionDefinition(id), m.getFunctionDefinition((*it).second));
        logged.insert(pair<const std::string, std::string>(id, (*it).second));
      }
    }
  }
}
 



/*
  * Logs a message about recursion in the given
  * FunctionDefinition.
  */

void
FunctionDefinitionRecursion::logSelfRecursion ( const FunctionDefinition& fd,
                                       const string& varname )
{
  char * formula = SBML_formulaToString(fd.getMath());   
  msg = "The functionDefinition with id '";
  msg += varname;
  msg += "' refers to itself within the math formula ";
  msg += formula;
  msg += "'.";
  safe_free(formula);

  
  logFailure(fd);
}

void
FunctionDefinitionRecursion::logCycle ( const FunctionDefinition* object,
                                       const FunctionDefinition* conflict )
{
  msg = "The FunctionDefinition with id '";
  msg += object->getId();
  msg += "' creates a cycle with the FunctionDefinition";
  msg += " with id '";
  msg += conflict->getId();
  msg += "'.";

  
  logFailure(*object);
}



#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
