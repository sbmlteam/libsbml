/**
 * Filename    : RateRule.cpp
 * Description : SBML RateRule
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-40-29
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include "sbml/RateRule.h"
#include "sbml/RateRule.hpp"


/**
 * Creates a new RateRule, optionally with its variable and math (via
 * an infix formula string) attributes set.
 */
LIBSBML_EXTERN
RateRule::RateRule (   const std::string&  variable
                     , const std::string&  formula  ) :
    Rule    ( formula  )
  , variable( variable )
{
  init(SBML_RATE_RULE);
}


/**
 * Creates a new RateRule with its variable and math attributes set.
 */
LIBSBML_EXTERN
RateRule::RateRule (   const std::string&  variable
                     , ASTNode*            math     ) :
    Rule    ( math     )
  , variable( variable )
{
  init(SBML_RATE_RULE);
}


/**
 * Destroys this RateRule.
 */
LIBSBML_EXTERN
RateRule::~RateRule ()
{
}


/**
 * @return the variable for this RateRule.
 */
LIBSBML_EXTERN
const std::string&
RateRule::getVariable () const
{
  return variable;
}


/**
 * @return true if the variable of this RateRule has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
RateRule::isSetVariable () const
{
  return ! variable.empty();
}


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
LIBSBML_EXTERN
void
RateRule::setVariable (const std::string& sid)
{
  variable = sid;
}




/**
 * Creates a new RateRule and returns a pointer to it.
 */
LIBSBML_EXTERN
RateRule_t *
RateRule_create (void)
{
  return new(std::nothrow) RateRule;
}


/**
 * Creates a new RateRule with the given variable and math and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   rr = RateRule_create();
 *   RateRule_setVariable(rr, variable);
 *   Rule_setMath((Rule_t *) rr, math);
 */
LIBSBML_EXTERN
RateRule_t *
RateRule_createWith (const char *variable, ASTNode_t *math)
{
  ASTNode* x = static_cast<ASTNode*>(math);


  return new(std::nothrow) RateRule( variable ? variable : "", x);
}


/**
 * Frees the given RateRule.
 */
LIBSBML_EXTERN
void
RateRule_free (RateRule_t *rr)
{
  delete static_cast<RateRule*>(rr);
}


/**
 * @return the variable for this RateRule.
 */
LIBSBML_EXTERN
const char *
RateRule_getVariable (const RateRule_t *rr)
{
  const RateRule* x = static_cast<const RateRule*>(rr);


  return x->isSetVariable() ? x->getVariable().c_str() : NULL;
}


/**
 * @return 1 if the variable of this RateRule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
RateRule_isSetVariable (const RateRule_t *rr)
{
  return (int) static_cast<const RateRule*>(rr)->isSetVariable();
}


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
LIBSBML_EXTERN
void
RateRule_setVariable (RateRule_t *rr, const char *sid)
{
  static_cast<RateRule*>(rr)->setVariable(sid ? sid : "");
}
