/**
 * \file    SpeciesConcentrationRule.cpp
 * \brief   SBML SpeciesConcentrationRule
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
#include "SpeciesConcentrationRule.h"


/**
 * Creates a new SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
SpeciesConcentrationRule::SpeciesConcentrationRule () : AssignmentRule()
{
  init(SBML_SPECIES_CONCENTRATION_RULE);
}


/**
 * Creates a new SpeciesConcentrationRule with its species, formula
 * and (optionally) type attributes set.
 */
LIBSBML_EXTERN
SpeciesConcentrationRule::SpeciesConcentrationRule
(
   const std::string&  species
 , const std::string&  formula 
 , RuleType_t          type
) :
  AssignmentRule(species, formula, type)
{
  init(SBML_SPECIES_CONCENTRATION_RULE);
}


/**
 * Destroys this SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
SpeciesConcentrationRule::~SpeciesConcentrationRule ()
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
SpeciesConcentrationRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return the species of this SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
const std::string&
SpeciesConcentrationRule::getSpecies () const
{
  return getVariable();
}


/**
 * @return true if the species of this SpeciesConcentrationRule has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
SpeciesConcentrationRule::isSetSpecies () const
{
  return isSetVariable();
}


/**
 * Sets the species of this SpeciesConcentrationRule to a copy of sname.
 */
LIBSBML_EXTERN
void
SpeciesConcentrationRule::setSpecies (const std::string& sname)
{
  setVariable(sname);
}




/**
 * Creates a new SpeciesConcentrationRule and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
SpeciesConcentrationRule_create (void)
{
  return new(std::nothrow) SpeciesConcentrationRule;
}


/**
 * Creates a new SpeciesConcentrationRule with the given formula, type and
 * species and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();
 *   Rule_setFormula((Rule_t *) scr, formula);
 *   AssignmentRule_setType((AssignmentRule_t *) scr, type);
 *   ...;
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
SpeciesConcentrationRule_createWith ( const char *formula,
                                      RuleType_t type,
                                      const char *species )
{
  const char* s = species ? species : "";
  const char* f = formula ? formula : "";


  return new(std::nothrow) SpeciesConcentrationRule(s, f, type);
}


/**
 * Frees the given SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
void
SpeciesConcentrationRule_free (SpeciesConcentrationRule_t *scr)
{
  delete static_cast<SpeciesConcentrationRule*>(scr);
}


/**
 * @return the species of this SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
const char *
SpeciesConcentrationRule_getSpecies (const SpeciesConcentrationRule_t *scr)
{
  const SpeciesConcentrationRule* x =
    static_cast<const SpeciesConcentrationRule*>(scr);


  return x->isSetSpecies() ? x->getSpecies().c_str() : NULL;
}


/**
 * @return 1 if the species of this SpeciesConcentrationRule has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesConcentrationRule_isSetSpecies (const SpeciesConcentrationRule_t *scr)
{
  const SpeciesConcentrationRule* x =
    static_cast<const SpeciesConcentrationRule*>(scr);


  return (int) x->isSetSpecies();
}


/**
 * Sets the species of this SpeciesConcentrationRule to a copy of sname.
 */
LIBSBML_EXTERN
void
SpeciesConcentrationRule_setSpecies ( SpeciesConcentrationRule_t *scr,
                                      const char *sname )
{
  SpeciesConcentrationRule* x =
    static_cast<SpeciesConcentrationRule*>(scr);

  x->setSpecies(sname ? sname : "");
}
