/**
 * \file    Rule.cpp
 * \brief   SBML Rule
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


#include "common/common.h"

#include "math/FormulaFormatter.h"
#include "math/FormulaParser.h"
#include "math/ASTNode.h"

#include "SBMLVisitor.h"
#include "Rule.h"


/**
 * Creates a new Rule, optionally with its formula attribute set.
 */
LIBSBML_EXTERN
Rule::Rule (const std::string& formula) : formula(formula), math(0)
{
}


/**
 * Creates a new Rule with its math attribute set.
 */
LIBSBML_EXTERN
Rule::Rule (ASTNode* math) : math(math)
{
}


/**
 * Destroys this Rule.
 */
LIBSBML_EXTERN
Rule::~Rule ()
{
  delete math;
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
Rule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const std::string&
Rule::getFormula () const
{
  if (formula.empty() && math) setFormulaFromMath();
  return formula;
}


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode*
Rule::getMath () const
{
  if (!math && !formula.empty()) setMathFromFormula();
  return math;
}


/**
 * @return true if the formula (or equivalently the math) for this Rule has
 * been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Rule::isSetFormula () const
{
  return !formula.empty() || math;
}


/**
 * @return true if the math (or equivalently the formula) for this Rule has
 * been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Rule::isSetMath () const
{
  return isSetFormula();
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule::setFormula (const std::string& string)
{
  formula = string;

  if (math)
  {
    delete math;
    math = 0;
  }
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
Rule::setMath (ASTNode* math)
{
  if (this->math == math) return;


  delete this->math;
  this->math = math;

  formula.erase();
}


/**
 * This method is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The method is kept around for
 * backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
Rule::setFormulaFromMath () const
{
  if (math)
  {
    char* s = SBML_formulaToString(math);
    formula = s;

    safe_free(s);
  }
  else
  {
    formula.erase();
  }
}


/**
 * This method is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The method is kept around for
 * backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
Rule::setMathFromFormula () const
{
  delete math;
  math = formula.empty() ? 0 : SBML_parseFormula( formula.c_str() );
}




/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r)
{
  return r->isSetFormula() ? r->getFormula().c_str() : NULL;
}


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r)
{
  return r->getMath();
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r)
{
  return static_cast<int>( r->isSetFormula() );
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r)
{
  return static_cast<int>( r->isSetMath() );
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *string)
{
  r->setFormula(string ? string : "");
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
  r->setMath(math);
}


/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
Rule_setFormulaFromMath (const Rule_t *r)
{
  r->setFormulaFromMath();
}


/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
Rule_setMathFromFormula (const Rule_t *r)
{
  r->setMathFromFormula();
}
