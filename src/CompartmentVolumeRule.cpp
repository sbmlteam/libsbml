/**
 * Filename    : CompartmentVolumeRule.cpp
 * Description : SBML CompartmentVolumeRule
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


#include "sbml/SBMLVisitor.hpp"

#include "sbml/CompartmentVolumeRule.h"
#include "sbml/CompartmentVolumeRule.hpp"


/**
 * Creates a new CompartmentVolumeRule.
 */
LIBSBML_EXTERN
CompartmentVolumeRule::CompartmentVolumeRule () : AssignmentRule()
{
  init(SBML_COMPARTMENT_VOLUME_RULE);
}


/**
 * Creates a new CompartmentVolumeRule with its compartment, formula and
 * (optionally) type attributes set.
 */
LIBSBML_EXTERN
CompartmentVolumeRule::CompartmentVolumeRule
(
   const std::string&  compartment
 , const std::string&  formula 
 , RuleType_t          type
) :
  AssignmentRule(compartment, formula, type)
{
  init(SBML_COMPARTMENT_VOLUME_RULE);
}


/**
 * Destroys this CompartmentVolumeRule.
 */
LIBSBML_EXTERN
CompartmentVolumeRule::~CompartmentVolumeRule ()
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
CompartmentVolumeRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return the compartment of this CompartmentVolumeRule.
 */
LIBSBML_EXTERN
const std::string&
CompartmentVolumeRule::getCompartment () const
{
  return getVariable();
}


/**
 * @return true if the compartment of this CompartmentVolumeRule has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
CompartmentVolumeRule::isSetCompartment () const
{
  return isSetVariable();
}


/**
 * Sets the compartment of this CompartmentVolumeRule to a copy of sname.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule::setCompartment (const std::string& sname)
{
  setVariable(sname);
}




/**
 * Creates a new CompartmentVolumeRule and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_create (void)
{
  return new(std::nothrow) CompartmentVolumeRule;
}


/**
 * Creates a new CompartmentVolumeRule with the given formula, type and
 * compartment and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();
 *   Rule_setFormula((Rule_t *) cvr, formula);
 *   AssignmentRule_setType((AssignmentRule_t *) cvr, type);
 *   ...;
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_createWith ( const char *formula,
                                   RuleType_t type,
                                   const char *compartment )
{
  const char* c = compartment ? compartment : "";
  const char* f = formula     ? formula     : "";


  return new(std::nothrow) CompartmentVolumeRule(c, f, type);
}


/**
 * Frees the given CompartmentVolumeRule.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_free (CompartmentVolumeRule_t *cvr)
{
  delete static_cast<CompartmentVolumeRule*>(cvr);
}


/**
 * @return the compartment of this CompartmentVolumeRule.
 */
LIBSBML_EXTERN
const char *
CompartmentVolumeRule_getCompartment (const CompartmentVolumeRule_t *cvr)
{
  const CompartmentVolumeRule* x =
    static_cast<const CompartmentVolumeRule*>(cvr);


  return x->isSetCompartment() ? x->getCompartment().c_str() : NULL;
}


/**
 * @return 1 if the compartment of this CompartmentVolumeRule has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentVolumeRule_isSetCompartment (const CompartmentVolumeRule_t *cvr)
{
  const CompartmentVolumeRule* x =
    static_cast<const CompartmentVolumeRule*>(cvr);


  return (int) x->isSetCompartment();
}


/**
 * Sets the compartment of this CompartmentVolumeRule to a copy of sname.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_setCompartment ( CompartmentVolumeRule_t *cvr,
                                       const char *sname )
{
  CompartmentVolumeRule* x =
    static_cast<CompartmentVolumeRule*>(cvr);

  x->setCompartment(sname ? sname : "");
}
