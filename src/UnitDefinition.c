/**
 * Filename    : UnitDefinition.c
 * Description : SBML UnitDefinition
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-22
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/UnitDefinition.h"


/**
 * Creates a new UnitDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void)
{
  UnitDefinition_t *ud;


  ud = (UnitDefinition_t *) safe_calloc(1, sizeof(UnitDefinition_t));
  SBase_init((SBase_t *) ud, SBML_UNIT_DEFINITION);

  ud->unit = (ListOf_t *) ListOf_create();

  return ud;
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setId(UnitDefinition_create(), sid);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *sid)
{
  UnitDefinition_t *ud = UnitDefinition_create();


  UnitDefinition_setId(ud, sid);

  return ud;
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setName(UnitDefinition_create(), string);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *string)
{
  UnitDefinition_t *ud = UnitDefinition_create();


  UnitDefinition_setName(ud, string);

  return ud;
}


/**
 * Frees the given UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  if (ud == NULL) return;


  SBase_clear((SBase_t *) ud);

  ListOf_free(ud->unit);

  safe_free(ud->id);
  safe_free(ud->name);
  safe_free(ud);
}


/**
 * @return the id of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  return ud->id;
}


/**
 * @return the name of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  return ud->name;
}


/**
 * @return 1 if the id of this UnitDefinition has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return (ud->id != NULL);
}


/**
 * @return 1 if the name of this UnitDefinition has been set, 0 otherwise.
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return (ud->name != NULL);
}


/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *sid)
{
  if (ud->id == sid) return;


  if (ud->id != NULL)
  {
    safe_free(ud->id);
  }

  ud->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name of this UnitDefinition to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *string)
{
  if (ud->name == string) return;


  if (ud->name != NULL)
  {
    safe_free(ud->name);
  }

  ud->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Unsets the name of this UnitDefinition.  This is equivalent to:
 * safe_free(ud->name); ud->name = NULL;
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  safe_free(ud->name);
  ud->name = NULL;
}


/**
 * Adds the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, Unit_t *u)
{
  ListOf_append(ud->unit, u);
}


/**
 * @return the nth Unit of this UnitDefinition
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (const UnitDefinition_t *ud, unsigned int n)
{
  return (Unit_t *) ListOf_get(ud->unit, n);
}


/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return ListOf_getNumItems(ud->unit);
}
