/**
 * @file    AssignmentCycles.cpp
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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

#include <cstring>

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Reaction.h>
#include <sbml/InitialAssignment.h>
#include <sbml/util/List.h>

#include "AssignmentCycles.h"
#include "IdList.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



/**
 * Creates a new Constraint with the given constraint id.
 */
AssignmentCycles::AssignmentCycles (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
AssignmentCycles::~AssignmentCycles ()
{
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
AssignmentCycles::check_ (const Model& m, const Model& object)
{
  // this rule ony applies in l2v2 and beyond
  if (object.getLevel() == 1 
    || (object.getLevel() == 2 && object.getVersion() == 1))
    return;

  unsigned int n;

  mCheckedList.clear();

  for (n = 0; n < m.getNumInitialAssignments(); ++n)
  { 
    if (m.getInitialAssignment(n)->isSetMath())
    {
      mCheckedList.clear();
      mCheckedList.append(m.getInitialAssignment(n)->getId());
      checkInitialAssignmentForSymbol(m, *m.getInitialAssignment(n));
      checkInitialAssignment(m, *m.getInitialAssignment(n));
      checkInitialAssignmentForCompartment(m, *m.getInitialAssignment(n));
    }
  }
  
  for (n = 0; n < m.getNumReactions(); ++n)
  { 
    if (m.getReaction(n)->isSetKineticLaw()){
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        mCheckedList.clear();
        mCheckedList.append(m.getReaction(n)->getId());
        checkReactionForId(m, *m.getReaction(n));
        checkReaction(m, *m.getReaction(n));
      }
    }
  }
  
  for (n = 0; n < m.getNumRules(); ++n)
  { 
    if (m.getRule(n)->isAssignment() && m.getRule(n)->isSetMath())
    {
      mCheckedList.clear();
      mCheckedList.append(m.getRule(n)->getId());
      checkRuleForVariable(m, *m.getRule(n));
      checkRule(m, *m.getRule(n));
      checkRuleForCompartment(m, *m.getRule(n));
    }
  }
}
 
void 
AssignmentCycles::checkInitialAssignment(const Model& m, const InitialAssignment& object)
{
  unsigned int ns;

  /* loop thru the list of names in the Math
    * if they refer to a Reaction, an Assignment Rule
    * or an Initial Assignment add to the list
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";

    if (!mVariables.contains(name))
    {
      if (m.getReaction(name))
        mVariables.append(name);
      else if (m.getRule(name) && m.getRule(name)->isAssignment())
        mVariables.append(name);
      else if (m.getInitialAssignment(name))
        mVariables.append(name);
    }
  }

  delete variables;

  while(mVariables.size() != 0)
  {
    IdList::const_iterator the_iterator;

    // create temporary list
    for (the_iterator = mVariables.begin();
      the_iterator != mVariables.end(); the_iterator++)
    {
      mTempList.append(*the_iterator);
    }
    mVariables.clear();

    // loop thru list and find other reactions/assignments
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      if (m.getInitialAssignment(*the_iterator))
      {
        const InitialAssignment * ia = m.getInitialAssignment(*the_iterator);

        /* if this is the initial assignment being checked skip */
        if (strcmp(ia->getId().c_str(), object.getId().c_str()))
        {
          if (ia->isSetMath())
          {
            List* variables = ia->getMath()->getListOfNodes( ASTNode_isName );
            for (ns = 0; ns < variables->getSize(); ns++)
            {
              ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
              string   name = node->getName() ? node->getName() : "";

              if (mCheckedList.contains(name))
              {
                logUndefined(object, *m.getInitialAssignment(*the_iterator));
              }
              else if (!mVariables.contains(name))
              {
                if (m.getReaction(name))
                  mVariables.append(name);
                else if (m.getRule(name) && m.getRule(name)->isAssignment())
                  mVariables.append(name);
                else if (m.getInitialAssignment(name))
                  mVariables.append(name);
              }
            }
            delete variables;
          }
        }
      }
      else if (m.getReaction(*the_iterator))
      {
        const Reaction * r = m.getReaction(*the_iterator);

        //check that the reaction has a Kinetic Law
        if (r->isSetKineticLaw())
        {
          if (r->getKineticLaw()->isSetMath())
          {
            List* variables = r->getKineticLaw()
                               ->getMath()->getListOfNodes( ASTNode_isName );
            for (ns = 0; ns < variables->getSize(); ns++)
            {
              ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
              string   name = node->getName() ? node->getName() : "";

              if (mCheckedList.contains(name))
              {
                logUndefined(object, *m.getReaction(*the_iterator));
              }
              else if (!mVariables.contains(name))
              {
                if (m.getReaction(name))
                  mVariables.append(name);
                else if (m.getRule(name) && m.getRule(name)->isAssignment())
                  mVariables.append(name);
                else if (m.getInitialAssignment(name))
                  mVariables.append(name);
              }
            }
            delete variables;
          }
        }
      }
      else if (m.getRule(*the_iterator) 
        && m.getRule(*the_iterator)->isAssignment())
      {
        const Rule * ar = m.getRule(*the_iterator);

        if (ar->isSetMath())
        {
          List* variables = ar->getMath()->getListOfNodes( ASTNode_isName );
          for (ns = 0; ns < variables->getSize(); ns++)
          {
            ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
            string   name = node->getName() ? node->getName() : "";

            if (mCheckedList.contains(name))
            {
              logUndefined(object, *m.getRule(*the_iterator));
            }
            else if (!mVariables.contains(name))
            {
              if (m.getReaction(name))
                mVariables.append(name);
              else if (m.getRule(name) && m.getRule(name)->isAssignment())
                mVariables.append(name);
              else if (m.getInitialAssignment(name))
                mVariables.append(name);
            }
          }
          delete variables;
        }
      }
    }

    // create list of checked items
    //for (the_iterator = mTempList.begin();
    //  the_iterator != mTempList.end(); the_iterator++)
    //{
    //  mCheckedList.append(*the_iterator);
    //}
    mTempList.clear();

  } // end of while

}

void 
AssignmentCycles::checkReaction(const Model& m, const Reaction& object)
{
  unsigned int ns;

  /* loop thru the list of names in the Math
    * if they refer to a Reaction, an Assignment Rule
    * or an Initial Assignment add to the list
    */
  List* variables = object.getKineticLaw()->getMath()
                                      ->getListOfNodes( ASTNode_isName );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";

    if (!mVariables.contains(name))
    {
      if (m.getReaction(name))
        mVariables.append(name);
      else if (m.getRule(name) && m.getRule(name)->isAssignment())
        mVariables.append(name);
      else if (m.getInitialAssignment(name))
        mVariables.append(name);
    }
  }
  delete variables;


  while(mVariables.size() != 0)
  {
    IdList::const_iterator the_iterator;

    // create temporary list
    for (the_iterator = mVariables.begin();
      the_iterator != mVariables.end(); the_iterator++)
    {
      mTempList.append(*the_iterator);
    }
    mVariables.clear();

    // loop thru list and find other reactions/assignments
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      if (m.getInitialAssignment(*the_iterator))
      {
        const InitialAssignment * ia = m.getInitialAssignment(*the_iterator);

        if (ia->isSetMath())
        {
          List* variables = ia->getMath()->getListOfNodes( ASTNode_isName );
          for (ns = 0; ns < variables->getSize(); ns++)
          {
            ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
            string   name = node->getName() ? node->getName() : "";

            if (mCheckedList.contains(name))
            {
              logUndefined(object, *m.getInitialAssignment(*the_iterator));
            }
            else if (!mVariables.contains(name))
            {
              if (m.getReaction(name))
                mVariables.append(name);
              else if (m.getRule(name) && m.getRule(name)->isAssignment())
                mVariables.append(name);
              else if (m.getInitialAssignment(name))
                mVariables.append(name);
            }
          }
          delete variables;
        }
      }
      else if (m.getReaction(*the_iterator))
      {
        const Reaction * r = m.getReaction(*the_iterator);

         /* if this is the reaction being checked skip */
        if (strcmp(r->getId().c_str(), object.getId().c_str()))
        {
        //check that the reaction has a Kinetic Law
          if (r->isSetKineticLaw())
          {
            if (r->getKineticLaw()->isSetMath())
            {
              List* variables = r->getKineticLaw()
                                ->getMath()->getListOfNodes( ASTNode_isName );
              for (ns = 0; ns < variables->getSize(); ns++)
              {
                ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
                string   name = node->getName() ? node->getName() : "";

                if (mCheckedList.contains(name))
                {
                  logUndefined(object, *m.getReaction(*the_iterator));
                }
                else if (!mVariables.contains(name))
                {
                  if (m.getReaction(name))
                    mVariables.append(name);
                  else if (m.getRule(name) && m.getRule(name)->isAssignment())
                    mVariables.append(name);
                  else if (m.getInitialAssignment(name))
                    mVariables.append(name);
                }
              }
              delete variables;
            }
          }
        }
      }
      else if (m.getRule(*the_iterator) 
        && m.getRule(*the_iterator)->isAssignment())
      {
        const Rule * ar = m.getRule(*the_iterator);

        if (ar->isSetMath())
        {
          List* variables = ar->getMath()->getListOfNodes( ASTNode_isName );
          for (ns = 0; ns < variables->getSize(); ns++)
          {
            ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
            string   name = node->getName() ? node->getName() : "";

            if (mCheckedList.contains(name))
            {
              logUndefined(object, *m.getRule(*the_iterator));
            }
            else if (!mVariables.contains(name))
            {
              if (m.getReaction(name))
                mVariables.append(name);
              else if (m.getRule(name) && m.getRule(name)->isAssignment())
                mVariables.append(name);
              else if (m.getInitialAssignment(name))
                mVariables.append(name);
            }
          }
          delete variables;
        }
      }
    }

    // create list of checked items
    //for (the_iterator = mTempList.begin();
    //  the_iterator != mTempList.end(); the_iterator++)
    //{
    //  mCheckedList.append(*the_iterator);
    //}
    mTempList.clear();

  } // end of while

}
void 
AssignmentCycles::checkRule(const Model& m, const Rule& object)
{
  unsigned int ns;

  /* loop thru the list of names in the Math
    * if they refer to a Reaction, an Assignment Rule
    * or an Initial Assignment add to the list
    */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );
  for (ns = 0; ns < variables->getSize(); ns++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
    string   name = node->getName() ? node->getName() : "";

    if (!mVariables.contains(name))
    {
      if (m.getReaction(name))
        mVariables.append(name);
      else if (m.getRule(name) && m.getRule(name)->isAssignment())
        mVariables.append(name);
      else if (m.getInitialAssignment(name))
        mVariables.append(name);
    }
  }
  delete variables;


  while(mVariables.size() != 0)
  {
    IdList::const_iterator the_iterator;

    // create temporary list
    for (the_iterator = mVariables.begin();
      the_iterator != mVariables.end(); the_iterator++)
    {
      mTempList.append(*the_iterator);
    }
    mVariables.clear();

    // loop thru list and find other reactions/assignments
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      if (m.getInitialAssignment(*the_iterator))
      {
        const InitialAssignment * ia = m.getInitialAssignment(*the_iterator);

        if (ia->isSetMath())
        {
          List* variables = ia->getMath()->getListOfNodes( ASTNode_isName );
          for (ns = 0; ns < variables->getSize(); ns++)
          {
            ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
            string   name = node->getName() ? node->getName() : "";

            if (mCheckedList.contains(name))
            {
              logUndefined(object, *m.getInitialAssignment(*the_iterator));
            }
            else if (!mVariables.contains(name))
            {
              if (m.getReaction(name))
                mVariables.append(name);
              else if (m.getRule(name) && m.getRule(name)->isAssignment())
                mVariables.append(name);
              else if (m.getInitialAssignment(name))
                mVariables.append(name);
            }
          }
          delete variables;
        }
      }
      else if (m.getReaction(*the_iterator))
      {
        const Reaction * r = m.getReaction(*the_iterator);

         /* if this is the reaction being checked skip */
        if (strcmp(r->getId().c_str(), object.getId().c_str()))
        {
        //check that the reaction has a Kinetic Law
          if (r->isSetKineticLaw())
          {
            if (r->getKineticLaw()->isSetMath())
            {
              List* variables = r->getKineticLaw()
                                ->getMath()->getListOfNodes( ASTNode_isName );
              for (ns = 0; ns < variables->getSize(); ns++)
              {
                ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
                string   name = node->getName() ? node->getName() : "";

                if (mCheckedList.contains(name))
                {
                  logUndefined(object, *m.getReaction(*the_iterator));
                }
                else if (!mVariables.contains(name))
                {
                  if (m.getReaction(name))
                    mVariables.append(name);
                  else if (m.getRule(name) && m.getRule(name)->isAssignment())
                    mVariables.append(name);
                  else if (m.getInitialAssignment(name))
                    mVariables.append(name);
                }
              }
              delete variables;
            }
          }
        }
      }
      else if (m.getRule(*the_iterator) 
        && m.getRule(*the_iterator)->isAssignment())
      {
        const Rule * ar = m.getRule(*the_iterator);
         /* if this is the reaction being checked skip */
        if (strcmp(ar->getId().c_str(), object.getId().c_str()))
        {
          if (ar->isSetMath())
          {
            List* variables = ar->getMath()->getListOfNodes( ASTNode_isName );
            for (ns = 0; ns < variables->getSize(); ns++)
            {
              ASTNode* node = static_cast<ASTNode*>( variables->get(ns) );
              string   name = node->getName() ? node->getName() : "";

              if (mCheckedList.contains(name))
              {
                logUndefined(object, *m.getRule(*the_iterator));
              }
              else if (!mVariables.contains(name))
              {
                if (m.getReaction(name))
                  mVariables.append(name);
                else if (m.getRule(name) && m.getRule(name)->isAssignment())
                  mVariables.append(name);
                else if (m.getInitialAssignment(name))
                  mVariables.append(name);
              }
            }
            delete variables;
          }
        }
      }
    }

    // create list of checked items
    //for (the_iterator = mTempList.begin();
    //  the_iterator != mTempList.end(); the_iterator++)
    //{
    //  mCheckedList.append(*the_iterator);
    //}
    mTempList.clear();

  } // end of while

}

void 
AssignmentCycles::checkInitialAssignmentForSymbol(const Model& m, 
                                  const InitialAssignment& object)
{
  /* list the <ci> elements */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );
  std::string variable = object.getSymbol();

  for (unsigned int i = 0; i < variables->getSize(); i++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(i) );
    const char *   name = node->getName() ? node->getName() : "";
    if (!(strcmp(variable.c_str(), name)))
      logMathRefersToSelf(*(object.getMath()), object);
  }

  delete variables;
}

void 
AssignmentCycles::checkReactionForId(const Model& m, const Reaction& object)
{
  /* list the <ci> elements */
  if (!(object.isSetKineticLaw()))
    return;
  else if (!(object.getKineticLaw()->isSetMath()))
    return;
  else
  {
    List* variables = 
      object.getKineticLaw()->getMath()->getListOfNodes( ASTNode_isName );
    std::string variable = object.getId();

    for (unsigned int i = 0; i < variables->getSize(); i++)
    {
      ASTNode* node = static_cast<ASTNode*>( variables->get(i) );
      const char *   name = node->getName() ? node->getName() : "";
      if (!(strcmp(variable.c_str(), name)))
        logMathRefersToSelf(*(object.getKineticLaw()->getMath()), object);
    }
    
    delete variables;
  }
}



void 
AssignmentCycles::checkRuleForVariable(const Model& m, const Rule& object)
{
  /* list the <ci> elements */
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );
  std::string variable = object.getVariable();

  for (unsigned int i = 0; i < variables->getSize(); i++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(i) );
    const char *   name = node->getName() ? node->getName() : "";
    if (!(strcmp(variable.c_str(), name)))
      logMathRefersToSelf(*(object.getMath()), object);
  }
  
  delete variables;
}

  
void 
AssignmentCycles::checkInitialAssignmentForCompartment(const Model &m, 
                                       const InitialAssignment &object)
{
  /* only need to check if the variable refers to a compartment
   * with dimensions greater than 0
   */
  std::string id = object.getSymbol();
  const Compartment *c = m.getCompartment(id);
  if (c == NULL)
    return;
  else if (c->getSpatialDimensions() == 0)
    return;

  /* list the <ci> elements of the rule*/
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );

  for (unsigned int i = 0; i < variables->getSize(); i++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(i) );
    const char *   name = node->getName() ? node->getName() : "";
    const Species * s = m.getSpecies(name);
    if (s && 
        s->getCompartment() == id &&
        s->getHasOnlySubstanceUnits() == false)
    {
      logImplicitReference(object, *(s));
    }
  }
  
  delete variables;
}
 

void 
AssignmentCycles::checkRuleForCompartment(const Model& m, 
                                          const Rule& object)
{
  /* only need to check if the variable refers to a compartment
   * with dimensions greater than 0
   */
  std::string id = object.getVariable();
  const Compartment *c = m.getCompartment(id);
  if (c == NULL)
    return;
  else if (c->getSpatialDimensions() == 0)
    return;

  /* list the <ci> elements of the rule*/
  List* variables = object.getMath()->getListOfNodes( ASTNode_isName );

  for (unsigned int i = 0; i < variables->getSize(); i++)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(i) );
    const char *   name = node->getName() ? node->getName() : "";
    const Species * s = m.getSpecies(name);
    if (s && 
        s->getCompartment() == id &&
        s->getHasOnlySubstanceUnits() == false)
    {
      logImplicitReference(object, *(s));
    }
  }
  
  delete variables;
}

/**
  * Logs a message about an undefined <ci> element in the given
  * FunctionDefinition.
  */
void
AssignmentCycles::logUndefined ( const SBase& object,
                                       const SBase& conflict )
{
  msg = "The ";
  msg += SBMLTypeCode_toString( object.getTypeCode());
  msg += " with id '";
  msg += object.getId();
  msg += "' creates a cycle with the ";
  msg += SBMLTypeCode_toString( conflict.getTypeCode());
  msg += " with id '";
  msg += conflict.getId();
  msg += "'.";

  
  logFailure(object);
}

void
AssignmentCycles::logMathRefersToSelf (const ASTNode & node,
                                             const SBase& object)
{
  msg = "The ";

  msg += SBMLTypeCode_toString( object.getTypeCode());
  msg += " with id '";
  msg += object.getId();
  msg += "' refers to that variable within the math formula '";
  msg += SBML_formulaToString(&node);
  msg += "'.";

  
  logFailure(object);

}

  
void 
AssignmentCycles::logImplicitReference (const SBase& object, 
                                        const Species& conflict)
{
  msg = "The ";
  msg += SBMLTypeCode_toString( object.getTypeCode());
  msg += " assigning value to compartment '";
  msg += object.getId();
  msg += "' refers to species '";
  msg += conflict.getId();
  msg += "'.  Since the use of the species id in this context ";
  msg += "refers to a concentration, this is an implicit ";
  msg += "reference to compartment '";
  msg += object.getId();
  msg += "'.";

  
  logFailure(object);
}

