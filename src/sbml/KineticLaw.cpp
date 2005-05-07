/**
 * \file    KineticLaw.cpp
 * \brief   SBML KineticLaw
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
#include "Parameter.h"
#include "KineticLaw.h"


/**
 * Creates a new KineticLaw, optionally with its formula, timeUnits and/or
 * substanceUnits set.
 */
LIBSBML_EXTERN
KineticLaw::KineticLaw (   const std::string& formula
                         , const std::string& timeUnits
                         , const std::string& substanceUnits ) :
    SBase()
  , formula        ( formula        )
  , math           ( 0              )
  , timeUnits      ( timeUnits      )
  , substanceUnits ( substanceUnits )
{
  init(SBML_KINETIC_LAW);
}


/**
 * Destroys this KineticLaw.
 */
LIBSBML_EXTERN
KineticLaw::~KineticLaw ()
{
  delete math;
}


/**
 * Accepts the given SBMLVisitor.
 */
LIBSBML_EXTERN
void
KineticLaw::accept (SBMLVisitor& v) const
{
  unsigned int n;
  bool next;


  v.visit(*this);

  //
  // Parameter
  //

  getListOfParameters().accept(v, SBML_PARAMETER);

  for (n = 0, next = true; n < getNumParameters() && next; n++)
  {
    next = getParameter(n)->accept(v);
  }

  v.leave(getListOfParameters(), SBML_PARAMETER);

  v.leave(*this);
}


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const std::string&
KineticLaw::getFormula () const
{
  if (formula.empty() && math) setFormulaFromMath();
  return formula;
}


/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode*
KineticLaw::getMath () const
{
  if (!math && !formula.empty()) setMathFromFormula();
  return math;
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf&
KineticLaw::getListOfParameters ()
{
  return parameter;
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
const ListOf&
KineticLaw::getListOfParameters () const
{
  return parameter;
}


/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const std::string&
KineticLaw::getTimeUnits () const
{
  return timeUnits;
}


/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const std::string&
KineticLaw::getSubstanceUnits () const
{
  return substanceUnits;
}


/**
 * @return true if the formula (or equivalently the math) of this
 * KineticLaw has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
KineticLaw::isSetFormula () const
{
  return !formula.empty() || math;
}


/**
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
KineticLaw::isSetMath () const
{
  return isSetFormula();
}


/**
 * @return true if the timeUnits of this KineticLaw has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
KineticLaw::isSetTimeUnits () const
{
  return ! timeUnits.empty();
}


/**
 * @return true if the substanceUnits of this KineticLaw has been set,
 * false otherwise.
 */
LIBSBML_EXTERN
bool
KineticLaw::isSetSubstanceUnits () const
{
  return ! substanceUnits.empty();
}


/**
 * Sets the formula of this KineticLaw to a copy of string.
 */
LIBSBML_EXTERN
void
KineticLaw::setFormula (const std::string& string)
{
  formula = string;

  if (math)
  {
    delete math;
    math = 0;
  }
}


/**
 * Sets the math of this KineticLaw to the given ASTNode.
 *
 * The node <b>is not copied</b> and this KineticLaw <b>takes ownership</b>
 * of it; i.e. subsequent calls to this method or deleting this
 * KineticLaw will delete the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
KineticLaw::setMath (ASTNode* math)
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
KineticLaw::setFormulaFromMath () const
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
KineticLaw::setMathFromFormula () const
{
  delete math;
  math = formula.empty() ? 0 : SBML_parseFormula( formula.c_str() );
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw::setTimeUnits (const std::string& sname)
{
  timeUnits = sname;
}


/**
 * Sets the substanceUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw::setSubstanceUnits (const std::string& sname)
{
  substanceUnits = sname;
}


/**
 * Adds the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw::addParameter (Parameter& p)
{
  parameter.append(&p);
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter*
KineticLaw::getParameter (unsigned int n) const
{
  return static_cast<Parameter*>( parameter.get(n) );
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw::getNumParameters () const
{
  return parameter.getNumItems();
}


/**
 * Unsets the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw::unsetTimeUnits ()
{
  timeUnits.erase();
}


/**
 * Unsets the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw::unsetSubstanceUnits ()
{
  substanceUnits.erase();
}




/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void)
{
  return new(std::nothrow) KineticLaw;
}


/**
 * Creates a new KineticLaw with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setFormula(kl, formula);
 *   KineticLaw_setTimeUnits(kl, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWith ( const char *formula,
                        const char *timeUnits,
                        const char *substanceUnits )
{
  std::string f  = formula        ? formula        : "";
  std::string tu = timeUnits      ? timeUnits      : "";
  std::string su = substanceUnits ? substanceUnits : "";

  return new(std::nothrow) KineticLaw(f, tu, su);
}


/**
 * Frees the given KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl)
{
  delete kl;
}


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl)
{
  return kl->isSetFormula() ? kl->getFormula().c_str() : NULL;
}


/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl)
{
  return kl->getMath();
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl)
{
  return & kl->getListOfParameters();
}


/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl)
{
  return kl->isSetTimeUnits() ? kl->getTimeUnits().c_str() : NULL;
}


/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl)
{
  return kl->isSetSubstanceUnits() ? kl->getSubstanceUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetFormula() );
}


/**
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetMath() );
}


/**
 * @return 1 if the timeUnits of this KineticLaw has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetTimeUnits() );
}


/**
 * @return 1 if the substanceUnits of this KineticLaw has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetSubstanceUnits() );
}


/**
 * Sets the formula of this KineticLaw to a copy of string.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *string)
{
  kl->setFormula(string ? string : "");
}


/**
 * Sets the math of this KineticLaw to the given ASTNode.
 *
 * The node <b>is not copied</b> and this KineticLaw <b>takes ownership</b>
 * of it; i.e. subsequent calls to this function or a call to
 * KineticLaw_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, ASTNode_t *math)
{
  kl->setMath(math);
}


/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
KineticLaw_setFormulaFromMath (const KineticLaw_t *kl)
{
  kl->setFormulaFromMath();
}


/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
KineticLaw_setMathFromFormula (const KineticLaw_t *kl)
{
  kl->setMathFromFormula();
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sname)
{
  if (sname == NULL) kl->unsetTimeUnits();
  else kl->setTimeUnits(sname);
}


/**
 * Sets the substanceUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sname)
{
  if (sname == NULL) kl->unsetSubstanceUnits();
  else kl->setSubstanceUnits(sname);
}


/**
 * Adds the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, Parameter_t *p)
{
  if (p != NULL) kl->addParameter(*p);
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (const KineticLaw_t *kl, unsigned int n)
{
  return kl->getParameter(n);
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return kl->getNumParameters();
}


/**
 * Unsets the timeUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->timeUnits); kl->timeUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl)
{
  kl->unsetTimeUnits();
}


/**
 * Unsets the substanceUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->substanceUnits); kl->substanceUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl)
{
  kl->unsetSubstanceUnits();
}
