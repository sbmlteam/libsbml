/**
 * @file    UnitsBase.cpp
 * @brief   Ensures math units are consistent.
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
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/Parameter.h>
#include <sbml/UnitDefinition.h>
#include <sbml/Event.h>
#include <sbml/Reaction.h>
#include <sbml/EventAssignment.h>
#include <sbml/SpeciesReference.h>
#include <sbml/Rule.h>
#include <sbml/math/FormulaFormatter.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include "UnitsBase.h"
#include "MathMLBase.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
UnitsBase::UnitsBase (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UnitsBase::~UnitsBase ()
{
}

/**
 * @return the fieldname to use logging constraint violations.  If not
 * overridden, "id" is returned.
 */
const char*
UnitsBase::getFieldname ()
{
  return "math";
}

/**
 * @return the preamble to use when logging constraint violations.  The
 * preamble will be prepended to each log message.  If not overriden,
 * returns an empty string.
 */
const char*
UnitsBase::getPreamble ()
{
  return "";
}




/**
  * Checks that the units of the result of the assignment rule
  * are consistent with variable being assigned
  *
  * @return true if units are consistent, false otherwise.
  */
void
UnitsBase::check_ (const Model& m, const Model& object)
{
  unsigned int n, ea, sr;
  
  /* check all math within a model */

  for (n = 0; n < m.getNumRules(); n++)
  {
    if (m.getRule(n)->isSetMath())
    {
      checkUnits(m, *m.getRule(n)->getMath(), *m.getRule(n));
    }
  }

  for (n = 0; n < m.getNumReactions(); n++)
  {
    if (m.getReaction(n)->isSetKineticLaw())
    {
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        checkUnits(m, *m.getReaction(n)->getKineticLaw()->getMath(), 
          *m.getReaction(n)->getKineticLaw());
      }
    }
    for (sr = 0; sr < m.getReaction(n)->getNumProducts(); sr++)
    {
      if (m.getReaction(n)->getProduct(sr)->isSetStoichiometryMath())
      {
        checkUnits(m, 
          *m.getReaction(n)->getProduct(sr)->getStoichiometryMath()->getMath(), 
          *m.getReaction(n)->getProduct(sr));
      }
    }
    for (sr = 0; sr < m.getReaction(n)->getNumReactants(); sr++)
    {
      if (m.getReaction(n)->getReactant(sr)->isSetStoichiometryMath())
      {
        checkUnits(m, 
          *m.getReaction(n)->getReactant(sr)->getStoichiometryMath()->getMath(), 
          *m.getReaction(n)->getReactant(sr));
      }
    }
  }

  for (n = 0; n < m.getNumEvents(); n++)
  {
    if (m.getEvent(n)->isSetTrigger())
    {
      checkUnits(m, *m.getEvent(n)->getTrigger()->getMath(), *m.getEvent(n));
    }
    if (m.getEvent(n)->isSetDelay())
    {
      checkUnits(m, *m.getEvent(n)->getDelay()->getMath(), *m.getEvent(n));
    }
    for (ea = 0; ea < m.getEvent(n)->getNumEventAssignments(); ea++)
    {
      if (m.getEvent(n)->getEventAssignment(ea)->isSetMath())
      {
        checkUnits(m, *m.getEvent(n)->getEventAssignment(ea)->getMath(), 
          *m.getEvent(n)->getEventAssignment(ea));
      }
    }
  }

  for (n = 0; n < m.getNumInitialAssignments(); n++)
  {
    if (m.getInitialAssignment(n)->isSetMath())
    {
      checkUnits(m, *m.getInitialAssignment(n)->getMath(), *m.getInitialAssignment(n));
    }
  }

  for (n = 0; n < m.getNumConstraints(); n++)
  {
    if (m.getConstraint(n)->isSetMath())
    {
      checkUnits(m, *m.getConstraint(n)->getMath(), *m.getConstraint(n));
    }
  }
}
/**
  * Checks that the units of the children of ASTnode 
  * are appropriate for the function being performed
  * forces recursion through the AST tree
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
UnitsBase::checkChildren (const Model& m, 
                                  const ASTNode& node, 
                                  const SBase & sb)
{
  unsigned int n;

  for(n = 0; n < node.getNumChildren(); n++)
  {
    checkUnits(m, *node.getChild(n), sb);
  }
}

/**
  * Checks that the units of function definition 
  * are appropriate for the function being performed
  * forces recursion through the AST tree
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
UnitsBase::checkFunction (const Model& m, 
                                  const ASTNode& node, 
                                  const SBase & sb)
{
  unsigned int i, nodeCount;
  unsigned int noBvars;
  ASTNode * fdMath;
  const FunctionDefinition *fd = m.getFunctionDefinition(node.getName());

  if (fd)
  {
    noBvars = fd->getNumArguments();
    if (noBvars == 0)
    {
      fdMath = fd->getMath()->getLeftChild()->deepCopy();
    }
    else
    {
      fdMath = fd->getMath()->getRightChild()->deepCopy();
    }

    for (i = 0, nodeCount = 0; i < noBvars; i++, nodeCount++)
    {
      ReplaceArgument(fdMath, fd->getArgument(i), 
                                          node.getChild(nodeCount));
    }
    /* check the math of the new function */
    checkUnits(m, *fdMath, sb);

    delete fdMath;
  }
}

/**
 * @return the typename of the given SBase object.
 */
const char*
UnitsBase::getTypename (const SBase& object)
{
  return SBMLTypeCode_toString( object.getTypeCode() );
}


/**
 * Logs a message that the given id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
UnitsBase::logUnitConflict (const ASTNode& node, const SBase& object)
{
  logFailure(object, getMessage(node, object));
}
