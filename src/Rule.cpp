/**
 * Filename    : Rule.cpp
 * Description : SBML Rule
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-26
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/FormulaFormatter.h"
#include "sbml/FormulaParser.h"

#include "sbml/Rule.h"
#include "sbml/Rule.hpp"


/**
 * Creates a new Rule, optionally with its formula attribute set.
 */
LIBSBML_EXTERN
Rule::Rule (const std::string& formula) : formula(formula), math(NULL)
{
}


/**
 * Creates a new Rule with its math attribute set.
 */
LIBSBML_EXTERN
Rule::Rule (ASTNode_t* math) : math(math)
{
}


/**
 * Destroys this Rule.
 */
LIBSBML_EXTERN
Rule::~Rule ()
{
  ASTNode_free(math);
}


/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const std::string&
Rule::getFormula () const
{
  return formula;
}


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule::getMath () const
{
  return math;
}


/**
 * @return true if the formula for this Rule has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Rule::isSetFormula () const
{
  return ! formula.empty();
}


/**
 * @return true if the math for this Rule has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Rule::isSetMath () const
{
  return (math != NULL);
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule::setFormula (const std::string& string)
{
  formula = string;
}


/**
 * Sets the formula of this Rule based on the current value of its math
 * field.  This convenience method is equivalent to:
 *
 *   setFormula( SBML_formulaToString( getMath() ))
 *
 * except you do not need to track and free the value returned by
 * SBML_formulaToString().
 *
 * If !isSetMath(r), this method has no effect.
 */
LIBSBML_EXTERN
void
Rule::setFormulaFromMath ()
{
  if ( !isSetMath() ) return;


  char* s = SBML_formulaToString(math);
  formula = s;

  safe_free(s);
}


/**
 * Sets the math of this Rule to the given ASTNode.
 *
 * The node <b>is not copied</b> and this Rule <b>takes ownership</b> of
 * it; i.e. subsequent calls to this method or deleting this Rule will
 * delete the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
Rule::setMath (ASTNode_t *math)
{
  if (this->math == math) return;


  ASTNode_free(this->math);
  this->math = math;
}


/**
 * Sets the math of this Rule from its current formula string.  This
 * convenience method is equivalent to:
 *
 *   setMath( SBML_parseFormula( getFormula() ))
 *
 * If !isSetFormula(), this method has no effect.
 */
LIBSBML_EXTERN
void
Rule::setMathFromFormula ()
{
  if ( !isSetFormula() ) return;


  ASTNode_free(math);
  math = SBML_parseFormula( formula.c_str() );
}




/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r)
{
  const Rule* x = static_cast<const Rule*>(r);


  return x->isSetFormula() ? x->getFormula().c_str() : NULL;
}


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r)
{
  return static_cast<const Rule*>(r)->getMath();
}


/**
 * @return 1 if the formula for this Rule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r)
{
  return (int) static_cast<const Rule*>(r)->isSetFormula();
}


/**
 * @return 1 if the math for this Rule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r)
{
  return (int) static_cast<const Rule*>(r)->isSetMath();
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *string)
{
  static_cast<Rule*>(r)->setFormula(string ? string : "");
}


/**
 * Sets the formula of this Rule based on the current value of its math
 * field.  This convenience function is functionally equivalent to:
 *
 *   Rule_setFormula(r, SBML_formulaToString( Rule_getMath(r) ))
 *
 * except you do not need to track and free the value returned by
 * SBML_formulaToString().
 *
 * If !Rule_isSetMath(r), this function has no effect.
 */
LIBSBML_EXTERN
void
Rule_setFormulaFromMath (Rule_t *r)
{
  static_cast<Rule*>(r)->setFormulaFromMath();
}


/**
 * Sets the math of this Rule to the given ASTNode.
 *
 * The node <b>is not copied</b> and this Rule <b>takes ownership</b> of
 * it; i.e. subsequent calls to this function or a call to Rule_free() will
 * free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
Rule_setMath (Rule_t *r, ASTNode_t *math)
{
  static_cast<Rule*>(r)->setMath(math);
}


/**
 * Sets the math of this Rule from its current formula string.  This
 * convenience function is functionally equivalent to:
 *
 *   Rule_setMath(r, SBML_parseFormula( Rule_getFormula(r) ))
 *
 * If !Rule_isSetFormula(r), this function has no effect.
 */
LIBSBML_EXTERN
void
Rule_setMathFromFormula (Rule_t *r)
{
  static_cast<Rule*>(r)->setMathFromFormula();
}
