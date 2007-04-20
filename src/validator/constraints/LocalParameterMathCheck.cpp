/**
 * @file    LocalParameterMathCheck.cpp
 * @brief   checks <ci> element of local parameter not used elsewhere
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
#include <sbml/SBMLTypeCodes.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include "LocalParameterMathCheck.h"

static const char* PREAMBLE =
    "The 'id' value of a <parameter> defined within a <kineticLaw> can only "
    "be used in 'ci' elements within the MathML content of that same "
    "<kineticLaw>; the identifier is not visible to other parts of the "
    "model. (References: L2V2 Section 3.5.3.)";

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
LocalParameterMathCheck::LocalParameterMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
LocalParameterMathCheck::~LocalParameterMathCheck ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
LocalParameterMathCheck::getPreamble ()
{
  return PREAMBLE;
}


/**
  * Checks the MathML of the ASTnode 
  * is appropriate for the function being performed
  *
  * If an inconsistency is found, an error message is logged.
  */
void
LocalParameterMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();
    
  /* if the node is a <ci> element it will have type AST_NAME
   * check that this name is an appropriate component of the model */
    
  switch (type) 
  {
    case AST_NAME:

      checkCiElement(m, node, sb);
      break;

    default:

      checkChildren(m, node, sb);
      break;

  }
}

  
/**
  * Checks any <ci> elements in the MathML of the ASTnode 
  * contain the id of an appropriate component of the model
  *
  * If an inconsistency is found, an error message is logged.
  */
void 
LocalParameterMathCheck::checkCiElement (const Model& m, 
                                        const ASTNode& node, 
                                        const SBase & sb)
{
  std::string name = node.getName();
  const KineticLaw * kl;

  if (!m.getCompartment(name) &&
      !m.getSpecies(name)     &&
      !m.getParameter(name)   &&
      !m.getReaction(name))
  {

    /* check whether we are in a kinetic law since there
     * may be local parameters to this law that are allowed
     */

    if (sb.getTypeCode() == SBML_KINETIC_LAW)
    {
      kl = m.getReaction(mKLCount)->getKineticLaw();

      if (!kl->getParameter(name) && mLocalParameters.contains(name))
      {
        logMathConflict(node, sb);
      }
    }
    else if (mLocalParameters.contains(name))
    {
        logMathConflict(node, sb);
    }
  }
    
}


/**
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
LocalParameterMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  msg << getPreamble();

  msg << "\nThe formula '";
  msg << "' in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " uses '" << node.getName() << "' that is the id of a local parameter.";

  return msg.str();
}
