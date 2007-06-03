/**
 * @file    AssignmentCycles.cpp
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
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
  unsigned int n;

  mCheckedList.clear();

  for (n = 0; n < m.getNumInitialAssignments(); ++n)
  { 
    if (m.getInitialAssignment(n)->isSetMath())
    {
      checkInitialAssignment(m, *m.getInitialAssignment(n));
    }
  }
  
  for (n = 0; n < m.getNumReactions(); ++n)
  { 
    if (m.getReaction(n)->isSetKineticLaw()){
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        checkReaction(m, *m.getReaction(n));
      }
    }
  }
  
  for (n = 0; n < m.getNumRules(); ++n)
  { 
    if (m.getRule(n)->isAssignment() && m.getRule(n)->isSetMath())
    {
      checkRule(m, *m.getRule(n));
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
        }
      }
    }

    // create list of checked items
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      mCheckedList.append(*the_iterator);
    }
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
        }
      }
    }

    // create list of checked items
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      mCheckedList.append(*the_iterator);
    }
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
          }
        }
      }
    }

    // create list of checked items
    for (the_iterator = mTempList.begin();
      the_iterator != mTempList.end(); the_iterator++)
    {
      mCheckedList.append(*the_iterator);
    }
    mTempList.clear();

  } // end of while

}

/**
  * Logs a message about an undefined <ci> element in the given
  * FunctionDefinition.
  */
void
AssignmentCycles::logUndefined ( const SBase& object,
                                       const SBase& conflict )
{
  msg =
    "There must not be circular dependencies in the combined set of "
    "<initialAssignment>, <assignmentRule> and <kineticLaw> definitions in a "
    "model. Each of these constructs has the effect of assigning a value to "
    "an identifier (i.e. the identifier given in the field 'symbol' in "
    "<initialAssignment>, the field 'variable' in <assignmentRule>, and the "
    "field 'id' on the <kineticLaw>'s enclosing <reaction>). Each of these "
    "constructs computes the value using a mathematical formula. The formula "
    "for a given identifier cannot make reference to a second identifier "
    "whose own definition depends directly or indirectly on the first "
    "identifier. (References: L2V2 Section 4.11.5.) The ";

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
