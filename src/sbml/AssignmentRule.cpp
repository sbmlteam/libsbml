/**
 * \file    AssignmentRule.cpp
 * \brief   SBML AssignmentRule
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "SBMLVisitor.h"
#include "AssignmentRule.h"


/**
 * Creates a new AssignmentRule.
 */
LIBSBML_EXTERN
AssignmentRule::AssignmentRule () : Rule()
{
  init(SBML_ASSIGNMENT_RULE);
  initDefaults();
}


/**
 * Creates a new AssignmentRule with its variable, formula, and
 * (optionally) type attributes set.
 */
LIBSBML_EXTERN
AssignmentRule::AssignmentRule (   const std::string&  variable
                                 , const std::string&  formula
                                 , RuleType_t          type     ) :
    Rule    ( formula  )
  , type    ( type     )
  , variable( variable )
{
  init(SBML_ASSIGNMENT_RULE);
}


/**
 * Creates a new AssignmentRule with its variable, math and (optionally)
 * type attributes set.
 */
LIBSBML_EXTERN
AssignmentRule::AssignmentRule (   const std::string&  variable
                                 , ASTNode*            math
                                 , RuleType_t          type     ) :
    Rule    ( math     )
  , type    ( type     )
  , variable( variable )
{
  init(SBML_ASSIGNMENT_RULE);
}


/**
 * Destroys this AssignmentRule.
 */
LIBSBML_EXTERN
AssignmentRule::~AssignmentRule ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Rule
 * (if available).
 */
LIBSBML_EXTERN
bool
AssignmentRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * The function is kept for backward compatibility with the SBML L1 API.
 *
 * Initializes the fields of this AssignmentRule to their defaults:
 *
 *   - type = RULE_TYPE_SCALAR
 */
LIBSBML_EXTERN
void
AssignmentRule::initDefaults ()
{
  setType(RULE_TYPE_SCALAR);
}


/**
 * @return the type for this AssignmentRule.
 */
LIBSBML_EXTERN
RuleType_t
AssignmentRule::getType () const
{
  return type;
}


/**
 * @return the variable for this AssignmentRule.
 */
LIBSBML_EXTERN
const std::string&
AssignmentRule::getVariable () const
{
  return variable;
}


/**
 * @return true if the variable of this AssignmentRule has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
AssignmentRule::isSetVariable () const
{
  return ! variable.empty();
}


/**
 * Sets the type of this Rule to the given RuleType.
 */
LIBSBML_EXTERN
void
AssignmentRule::setType (RuleType_t rt)
{
  type = rt;
}


/**
 * Sets the variable of this AssignmentRule to a copy of sid.
 */
LIBSBML_EXTERN
void
AssignmentRule::setVariable (const std::string& sid)
{
  variable = sid;
}




/**
 * Creates a new AssignmentRule and returns a pointer to it.
 *
 * In L1 AssignmentRule is an abstract class.  It exists soley to provide
 * fields to its subclasess: CompartmentVolumeRule, ParameterRule and
 * SpeciesConcentrationRule.
 *
 * In L2 the three subclasses are gone and AssigmentRule is concrete;
 * i.e. it may be created, used and destroyed directly.
 */
LIBSBML_EXTERN
AssignmentRule_t *
AssignmentRule_create (void)
{
  return new(std::nothrow) AssignmentRule;
}


/**
 * Creates a new AssignmentRule with the given variable and math and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ar = AssignmentRule_create();
 *   AssignmentRule_setVariable(ar, variable);
 *   Rule_setMath((Rule_t *) ar, math);
 */
LIBSBML_EXTERN
AssignmentRule_t *
AssignmentRule_createWith (const char *variable, ASTNode_t *math)
{
  ASTNode* x = static_cast<ASTNode*>(math);
  return new(std::nothrow) AssignmentRule(variable ? variable : "", x);
}


/**
 * Frees the given AssignmentRule.
 */
LIBSBML_EXTERN
void
AssignmentRule_free (AssignmentRule_t *ar)
{
  delete static_cast<AssignmentRule*>(ar);
}


/**
 * The function is kept for backward compatibility with the SBML L1 API.
 *
 * Initializes the fields of this AssignmentRule to their defaults:
 *
 *   - type = RULE_TYPE_SCALAR
 */
LIBSBML_EXTERN
void
AssignmentRule_initDefaults (AssignmentRule_t *ar)
{
  static_cast<AssignmentRule*>(ar)->initDefaults();
}


/**
 * @return the type for this AssignmentRule.
 */
LIBSBML_EXTERN
RuleType_t
AssignmentRule_getType (const AssignmentRule_t *ar)
{
  return static_cast<const AssignmentRule*>(ar)->getType();
}


/**
 * @return the variable for this AssignmentRule.
 */
LIBSBML_EXTERN
const char *
AssignmentRule_getVariable (const AssignmentRule_t *ar)
{
  const AssignmentRule* x = static_cast<const AssignmentRule*>(ar);


  return x->isSetVariable() ? x->getVariable().c_str() : NULL;
}


/**
 * @return 1 if the variable of this AssignmentRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
AssignmentRule_isSetVariable (const AssignmentRule_t *ar)
{
  return (int) static_cast<const AssignmentRule*>(ar)->isSetVariable();
}


/**
 * Sets the type of this Rule to the given RuleType.
 */
LIBSBML_EXTERN
void
AssignmentRule_setType (AssignmentRule_t *ar, RuleType_t rt)
{
  static_cast<AssignmentRule*>(ar)->setType(rt);
}


/**
 * Sets the variable of this AssignmentRule to a copy of sid.
 */
LIBSBML_EXTERN
void
AssignmentRule_setVariable (AssignmentRule_t *ar, const char *sid)
{
  static_cast<AssignmentRule*>(ar)->setVariable(sid ? sid : "");
}
