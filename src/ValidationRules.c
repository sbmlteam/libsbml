/**
 * Filename    : ValidationRules.c
 * Description : The default set of rules for validating an SBML document
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-03-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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
#include "sbml/Validator.h"


/**
 * This convenience macro is a quick and easy way to declare a new
 * validation rule.
 */
#define RULE(name) \
  static           \
  unsigned int     \
  name (const SBase_t *obj, const SBMLDocument_t *d, List_t *messages)


/**
 * This convenience macro, when coupled with the RULE macro, makes it easy
 * to log a validation error message.
 */
#define LOG_MESSAGE(msg)                                           \
  if (messages != NULL)                                            \
  {                                                                \
    unsigned int line = SBase_getLine(obj);                        \
    unsigned int col  = SBase_getColumn(obj);                      \
    List_add( messages, ParseMessage_createWith(msg, line, col) ); \
  }


RULE (compartment_size_dimensions)
{
  unsigned int   passed = 1;
  Compartment_t *c      = (Compartment_t *) obj;

  const char *msg =
    "Compartment size must not be set if spatialDimensions is zero.";


  if (Compartment_getSpatialDimensions(c) == 0)
  {
    if (Compartment_isSetSize(c))
    {
      passed = 0;
      LOG_MESSAGE(msg);
    }
  }

  return passed;
}


RULE (kineticLaw_substanceUnits)
{
  const char *units;

  Reaction_t   *r      = (Reaction_t *) obj;
  KineticLaw_t *kl     = Reaction_getKineticLaw(r);
  unsigned int  passed = 1;

  const char *msg =
    "substanceUnits must be 'substance', 'items', or 'moles' or the "
    "values of id attributes of unitDefinition elements that define "
    "variants (i.e. have only arbitrary scale, multiplier and "
    "offset values) of 'items' or 'moles";


  if (kl != NULL)
  {
    units = KineticLaw_getSubstanceUnits(kl);

    if (units != NULL)
    {
      if (! ( !strcmp(units, "substance") ||
              !strcmp(units, "items")     ||
              !strcmp(units, "moles") ) )
      {
        passed = 0;
        LOG_MESSAGE(msg);
      }
    }
  }

  return passed;
}


RULE (unitDefinition_idCantBePredefinedUnit)
{
  unsigned int passed = 1;

  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  const char *id = UnitDefinition_getId(ud);

  static const char msg[] =
    "The id of a unitDefinition must not be a predefined kind of unit.";

  if (UnitKind_isValidUnitKindString(id))
  {
    passed = 0;
    LOG_MESSAGE(msg);
  }

  return passed;
}


/**
 * Adds the default ValidationRule set to this Validator.
 */
void
Validator_addDefaultRules (Validator_t *v)
{
  Validator_addRule( v, compartment_size_dimensions, SBML_COMPARTMENT    );
  Validator_addRule( v, kineticLaw_substanceUnits  , SBML_REACTION       );
  Validator_addRule( v, unitDefinition_idCantBePredefinedUnit,
                                                     SBML_UNIT_DEFINITION );
}
