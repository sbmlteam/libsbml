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
#include "sbml/Model.h"
#include "sbml/UnitKind.h"


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


typedef struct {
  int passed;
  const char *msg;
} RuleResult_t;


static
void
initializeRuleResult(RuleResult_t *result)
{
  result->passed = 1;
  result->msg = NULL;
}

typedef int (*PFI)();


static
int
isMeter(UnitKind_t uk)
{
  return uk == UNIT_KIND_METRE || uk == UNIT_KIND_METER;
}


static
int
isLiter(UnitKind_t uk)
{
  return uk == UNIT_KIND_LITRE || uk == UNIT_KIND_LITER;
}


static
int
isSubstanceKind(UnitKind_t uk)
{
  return uk == UNIT_KIND_MOLE || uk == UNIT_KIND_ITEM;
}


static
int
isSecond(UnitKind_t uk)
{
  return uk == UNIT_KIND_SECOND;
}


static
int
isOneOfTheseKinds(UnitDefinition_t *ud, PFI *kindTests)
{
  ListOf_t *kinds = UnitDefinition_getListOfUnits(ud);
  Unit_t *u = (Unit_t *) ListOf_get(kinds, 0);
  UnitKind_t unitKind = Unit_getKind(u);
  PFI *kindTest;

  for (kindTest = kindTests; *kindTest; kindTest++)
  {
    if ((*kindTest)(unitKind))
    {
      return 1;
    }
  }

  return 0;
}


static
void
hasSingleKind(RuleResult_t *result, UnitDefinition_t *ud)
{
  ListOf_t *kinds;
  if (!result->passed) return;

  kinds = UnitDefinition_getListOfUnits(ud);
  if (ListOf_getNumItems(kinds) != 1)
  {
    result->passed = 0;
    result->msg = "must have exactly one unit kind.";
  }
}


static
void
hasAcceptableKinds(
  RuleResult_t *result,
  UnitDefinition_t *ud,
  PFI *acceptableKinds,
  const char *acceptableKindsMsg)
{
  if (!result->passed) return;

  {
    if (!isOneOfTheseKinds(ud, acceptableKinds))
    {
      result->passed = 0;
      result->msg = acceptableKindsMsg;
      return;
    }
  }
}


static
void
hasExponent(RuleResult_t *result, UnitDefinition_t *ud, int requiredExponent)
{
  if (!result->passed) return;

  {
    ListOf_t *kinds = UnitDefinition_getListOfUnits(ud);
    Unit_t *u = (Unit_t *) ListOf_get(kinds, 0);

    int exponent = Unit_getExponent(u);
    if (exponent != requiredExponent)
    {
      char buf[256];

      sprintf(buf, "must have exponent %d.");
      result->passed = 0;
      result->msg = safe_strdup(buf);
    }
  }
}


static
void
logFullMessage(
  const char *baseMsg,
  RuleResult_t *result,
  const SBase_t *obj,
  List_t *messages)
{
    char *message = safe_strcat(baseMsg, result->msg);
    LOG_MESSAGE(message);
    safe_free(message);
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


RULE (unitDefinition_idsMustBeUnique)
{
  static const char msg[] =
    "No two unitDefinitions may have the same id.";

  unsigned int passed = 1;

  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  const char *id = UnitDefinition_getId(ud);

  UnitDefinition_t *got = Model_getUnitDefinitionById(d->model, id);


  if (got != ud)
  {
    passed = 0;
    LOG_MESSAGE(msg);
  }

  return passed;
}


RULE (unitDefinition_idCantBePredefinedUnit)
{
  static const char msg[] =
    "The id of a unitDefinition must not be a predefined kind of unit.";

  unsigned int passed = 1;

  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  const char *id = UnitDefinition_getId(ud);


  if (UnitKind_isValidUnitKindString(id))
  {
    passed = 0;
    LOG_MESSAGE(msg);
  }

  return passed;
}


RULE (unitDefinition_volumeKinds)
{
  RuleResult_t result;
  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  PFI acceptableKinds[] = { isMeter, isLiter, NULL };
  PFI kindsThatNeedExponent3[] = { isMeter, NULL };
  static const char baseMsg[] = "A 'volume' unitDefinition ";
  static const char acceptableKindsMsg[] =
    "may only have units of kind 'liter' or 'metre'.";


  initializeRuleResult(&result);

  if (!strcmp("volume", UnitDefinition_getId(ud)))
  {
    hasSingleKind(&result, ud);
    hasAcceptableKinds(&result, ud, acceptableKinds, acceptableKindsMsg);
    if (isOneOfTheseKinds(ud, kindsThatNeedExponent3))
    {
      hasExponent(&result, ud, 3);
    }
  }

  if (!result.passed)
  {
    logFullMessage(baseMsg, &result, obj, messages);
  }
  return result.passed;
}


RULE (unitDefinition_substanceKinds)
{
  RuleResult_t result;
  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  PFI acceptableKinds[] = { isSubstanceKind, NULL };
  static const char baseMsg[] = "A 'substance' unitDefinition ";
  static const char acceptableKindsMsg[] =
    "may only have units of kind 'mole' or 'item'.";


  initializeRuleResult(&result);

  if (!strcmp("substance", UnitDefinition_getId(ud)))
  {
    hasSingleKind(&result, ud);
    hasAcceptableKinds(&result, ud, acceptableKinds, acceptableKindsMsg);
    hasExponent(&result, ud, 1);
  }

  if (!result.passed)
  {
    logFullMessage(baseMsg, &result, obj, messages);
  }
  return result.passed;
}


RULE (unitDefinition_areaKinds)
{
  RuleResult_t result;
  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  PFI acceptableKinds[] = { isMeter, NULL };
  static const char baseMsg[] = "An 'area' unitDefinition ";
  static const char acceptableKindsMsg[] =
    "may only have units of kind 'metre'.";


  initializeRuleResult(&result);

  if (!strcmp("area", UnitDefinition_getId(ud)))
  {
    hasSingleKind(&result, ud);
    hasAcceptableKinds(&result, ud, acceptableKinds, acceptableKindsMsg);
    hasExponent(&result, ud, 2);
  }

  if (!result.passed)
  {
    logFullMessage(baseMsg, &result, obj, messages);
  }
  return result.passed;
}


RULE (unitDefinition_lengthKinds)
{
  RuleResult_t result;
  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  PFI acceptableKinds[] = { isMeter, NULL };
  static const char baseMsg[] = "A 'length' unitDefinition ";
  static const char acceptableKindsMsg[] =
    "may only have units of kind 'metre'.";


  initializeRuleResult(&result);

  if (!strcmp("length", UnitDefinition_getId(ud)))
  {
    hasSingleKind(&result, ud);
    hasAcceptableKinds(&result, ud, acceptableKinds, acceptableKindsMsg);
    hasExponent(&result, ud, 1);
  }

  if (!result.passed)
  {
    logFullMessage(baseMsg, &result, obj, messages);
  }
  return result.passed;
}


RULE (unitDefinition_timeKinds)
{
  RuleResult_t result;
  UnitDefinition_t *ud = (UnitDefinition_t *) obj;
  PFI acceptableKinds[] = { isSecond, NULL };
  static const char baseMsg[] = "A 'time' unitDefinition ";
  static const char acceptableKindsMsg[] =
    "may only have units of kind 'second'.";


  initializeRuleResult(&result);

  if (!strcmp("time", UnitDefinition_getId(ud)))
  {
    hasSingleKind(&result, ud);
    hasAcceptableKinds(&result, ud, acceptableKinds, acceptableKindsMsg);
    hasExponent(&result, ud, 1);
  }

  if (!result.passed)
  {
    logFullMessage(baseMsg, &result, obj, messages);
  }
  return result.passed;
}


/**
 * The compartment attribute on a species element must contain the value of an
 * id attribute on a compartment element. 
 */
RULE (species_compartmentIsDefined)
{
  static const char msg[] =
    "Compartment '%s' is undefined.";

  const char* compartmentId;
  unsigned int passed = 1;


  Species_t *s = (Species_t *) obj;
  compartmentId = Species_getCompartment(s);
  if (compartmentId
      &&
      Model_getCompartmentById(d->model, compartmentId) == NULL
  ) {
    char buf[512];

    sprintf(buf, msg, compartmentId);
    LOG_MESSAGE(buf);
    passed = 0;
  }

  return passed;
}

/**
 * The spatialSizeUnits attribute must not be present if the
 * hasOnlySubstanceUnits attribute is true.
 */
RULE (species_hasOnlySubstanceUnits)
{
  static const char msg[] =
    "A species with hasOnlySubstanceUnits=true must not have spatialSizeUnits.";
  unsigned int passed = 1;


  Species_t *s = (Species_t *) obj;
  if (Species_getHasOnlySubstanceUnits(s))
  {
    if (Species_isSetSpatialSizeUnits(s))
    {
      LOG_MESSAGE(msg);
      passed = 0;
    }
  }

  return passed;
}

/**
 * The spatialSizeUnits attribute must not be present if the spatial
 * dimensions is zero.
 */
RULE (species_zeroSpatialDimensions)
{
  static const char msg[] =
    "A species must not have spatialSizeUnits if its compartment has "
    "spatialDimensions=0.";
  const char *compartmentId;
  unsigned int passed = 1;


  Species_t *s = (Species_t *) obj;
  compartmentId = Species_getCompartment(s);
  if (compartmentId)
  {
    Compartment_t *c = Model_getCompartmentById(d->model, compartmentId);
    if (
      c
      &&
      Compartment_getSpatialDimensions(c) == 0
      &&
      Species_isSetSpatialSizeUnits(s)
    ) {
      LOG_MESSAGE(msg);
      passed = 0;
    }
  }

  return passed;
}


/**
 * Adds the default ValidationRule set to this Validator.
 */
void
Validator_addDefaultRules (Validator_t *v)
{
  Validator_addRule( v, compartment_size_dimensions,   SBML_COMPARTMENT    );
  Validator_addRule( v, kineticLaw_substanceUnits  ,   SBML_REACTION       );
  Validator_addRule( v, unitDefinition_idsMustBeUnique,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_idCantBePredefinedUnit,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_substanceKinds,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_volumeKinds,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_areaKinds,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_lengthKinds,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, unitDefinition_timeKinds,
                                                       SBML_UNIT_DEFINITION );
  Validator_addRule( v, species_compartmentIsDefined,  SBML_SPECIES        );
  Validator_addRule( v, species_hasOnlySubstanceUnits, SBML_SPECIES        );
  Validator_addRule( v, species_zeroSpatialDimensions, SBML_SPECIES        );
}
