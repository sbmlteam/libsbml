/**
 * Filename    : Parameter.c
 * Description : SBML Parameter
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-21
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
#include "sbml/Parameter.h"


/**
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void)
{
  Parameter_t *p;


  p = (Parameter_t *) safe_calloc(1, sizeof(Parameter_t));

  SBase_init((SBase_t *) p, SBML_PARAMETER);
  Parameter_initDefaults(p);

  return p;
}


/**
 * Creates a new Parameter with the given id, value and units and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setName(p, id); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *sid, double value, const char *units)
{
  Parameter_t *p = Parameter_create();


  Parameter_setId   ( p, sid   );
  Parameter_setValue( p, value );
  Parameter_setUnits( p, units );

  return p;
}


/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p)
{
  if (p == NULL) return;

  SBase_clear((SBase_t *) p);

  safe_free(p->id);
  safe_free(p->name);
  safe_free(p->units);
  safe_free(p);
}


/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = 1  (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p)
{
  Parameter_setConstant(p, 1);
}


/**
 * @return the id of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p)
{
  return p->id;
}


/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p)
{
  return p->name;
}


/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p)
{
  return p->value;
}


/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p)
{
  return p->units;
}


/**
 * @return true (non-zero) if this Parameter is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p)
{
  return p->constant;
}


/**
 * @return 1 if the id of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p)
{
  return (p->id != NULL);
}


/**
 * @return 1 if the name of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p)
{
  return (p->name != NULL);
}


/**
 * @return 1 if the value of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p)
{
  return p->isSet.value;
}


/**
 * @return 1 if the units of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p)
{
  return (p->units != NULL);
}


/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid)
{
  if (p->id == sid) return;


  if (p->id != NULL)
  {
    safe_free(p->id);
  }

  p->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name field of this Parameter to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *string)
{
  if (p->name == string) return;


  if (p->name != NULL)
  {
    safe_free(p->name);
  }

  p->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value)
{
  p->value       = value;
  p->isSet.value = 1;
}


/**
 * Sets the units field of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sid)
{
  if (p->units == sid) return;


  if (p->units != NULL)
  {
    safe_free(p->units);
  }

  p->units = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the constant of this Parameter to value (boolean).
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value)
{
  p->constant = value;
}


/**
 * Unsets the name of this Parameter.  This is equivalent to:
 * safe_free(p->name); p->name = NULL;
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p)
{
  safe_free(p->name);
  p->name = NULL;
}


/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p)
{
  p->value       = util_NaN();
  p->isSet.value = 0;
}


/**
 * Unsets the units of this Parameter.  This is equivalent to:
 * safe_free(p->units); p->units = NULL;
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p)
{
  safe_free(p->units);
  p->units = NULL;
}


/**
 * The ParameterIdCmp function compares the string sid to p->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than p->id.
 * Returns -1 if either sid or p->id is NULL.
 */
LIBSBML_EXTERN
int
ParameterIdCmp (const char *sid, const Parameter_t *p)
{
  int result = -1;


  if (sid != NULL && p->id != NULL)
  {
    result = strcmp(sid, p->id);
  }

  return result;
}
