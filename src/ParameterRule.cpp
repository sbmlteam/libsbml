/**
 * Filename    : ParameterRule.cpp
 * Description : SBML ParameterRule
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/ParameterRule.h"
#include "sbml/ParameterRule.hpp"


/**
 * Creates a new ParameterRule.
 */
LIBSBML_EXTERN
ParameterRule::ParameterRule () : AssignmentRule()
{
  init(SBML_PARAMETER_RULE);
}


/**
 * Creates a new ParameterRule with its parameter, name and (optionally)
 * type attributes set.
 */
LIBSBML_EXTERN
ParameterRule::ParameterRule
(
   const std::string&  name
 , const std::string&  formula 
 , RuleType_t          type
) :
  AssignmentRule(name, formula, type)
{
  init(SBML_PARAMETER_RULE);
}


/**
 * Destroys this ParameterRule.
 */
LIBSBML_EXTERN
ParameterRule::~ParameterRule ()
{
}


/**
 * @return the (Parameter) name for this ParameterRule.
 */
LIBSBML_EXTERN
const std::string&
ParameterRule::getName () const
{
  return getVariable();
}


/**
 * @return the units for this ParameterRule.
 */
LIBSBML_EXTERN
const std::string&
ParameterRule::getUnits () const
{
  return units;
}


/**
 * @return true if the (Parameter) name for this ParameterRule has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
ParameterRule::isSetName () const
{
  return isSetVariable();
}


/**
 * @return true if the units for this ParameterRule has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
ParameterRule::isSetUnits () const
{
  return ! units.empty();
}


/**
 * Sets the (Parameter) name for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule::setName (const std::string& sname)
{
  setVariable(sname);
}


/**
 * Sets the units for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule::setUnits (const std::string& sname)
{
  units = sname;
}


/**
 * Unsets the units for this ParameterRule.
 */
LIBSBML_EXTERN
void
ParameterRule::unsetUnits ()
{
  units.erase();
}




/**
 * Creates a new ParameterRule and returns a pointer to it.
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_create (void)
{
  return new(std::nothrow) ParameterRule;
}


/**
 * Creates a new ParameterRule with the given formula, type, and name and
 * and returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ParameterRule_t *pr = ParameterRule_create();
 *   Rule_setFormula((Rule_t *) pr, formula); scr->type = type; ...;
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_createWith ( const char *formula,
                           RuleType_t type,
                           const char *name )
{

  const char* n = name    ? name    : "";
  const char* f = formula ? formula : "";


  return new(std::nothrow) ParameterRule(n, f, type);
}


/**
 * Frees the given ParameterRule.
 */
LIBSBML_EXTERN
void
ParameterRule_free (ParameterRule_t *pr)
{
  delete static_cast<ParameterRule*>(pr);
}


/**
 * @return the (Parameter) name for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getName (const ParameterRule_t *pr)
{
  const ParameterRule* x = static_cast<const ParameterRule*>(pr);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the units for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getUnits (const ParameterRule_t *pr)
{
  const ParameterRule* x = static_cast<const ParameterRule*>(pr);


  return x->isSetUnits() ? x->getUnits().c_str() : NULL;
}


/**
 * @return 1 if the (Parameter) name for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetName (const ParameterRule_t *pr)
{
  return (int) static_cast<const ParameterRule*>(pr)->isSetName();
}


/**
 * @return 1 if the units for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetUnits (const ParameterRule_t *pr)
{
  return (int) static_cast<const ParameterRule*>(pr)->isSetUnits();
}


/**
 * Sets the (Parameter) name for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setName (ParameterRule_t *pr, const char *sname)
{
  static_cast<ParameterRule*>(pr)->setName(sname ? sname : "");
}


/**
 * Sets the units for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setUnits (ParameterRule_t *pr, const char *sname)
{
  if (sname == NULL)
  {
    static_cast<ParameterRule*>(pr)->unsetUnits();
  }
  else
  {
    static_cast<ParameterRule*>(pr)->setUnits(sname);
  }
}


/**
 * Unsets the units for this ParameterRule.  This is equivalent to:
 * safe_free(pr->units); pr->units = NULL;
 */
LIBSBML_EXTERN
void
ParameterRule_unsetUnits (ParameterRule_t *pr)
{
  static_cast<ParameterRule*>(pr)->unsetUnits();
}
