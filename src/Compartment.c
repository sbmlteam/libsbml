/**
 * Filename    : Compartment.c
 * Description : SBML Compartment
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-13
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


#include "sbml/Compartment.h"


/**
 * Creates a new Compartment and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void)
{
  Compartment_t *c;


  c = (Compartment_t *) safe_calloc(1, sizeof(Compartment_t));  

  SBase_init((SBase_t *) c, SBML_COMPARTMENT);
  Compartment_initDefaults(c);

  return c;
}


/**
 * Creates a new Compartment with the  given id, size (volume in L1), units
 * and outside and  returns a pointer to it.   This convenience function is
 * functionally equivalent to:
 *
 *   Compartment_t *c = Compartment_create();
 *   Compartment_setId(c, sid); Compartment_setSize(c, size); ... ;
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith ( const char *sid  , double     size,
                         const char *units, const char *outside )
{
  Compartment_t *c = Compartment_create();


  Compartment_setId     ( c, sid     );
  Compartment_setSize   ( c, size    );
  Compartment_setUnits  ( c, units   );
  Compartment_setOutside( c, outside );

  return c;
}


/**
 * Frees the given Compartment.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c)
{
  if (c == NULL) return;

  SBase_clear((SBase_t *) c);

  safe_free(c->id);
  safe_free(c->name);
  safe_free(c->units);
  safe_free(c->outside);
  safe_free(c);
}


/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c)
{
  Compartment_setVolume           ( c, 1.0 );
  Compartment_setSpatialDimensions( c, 3   );
  Compartment_setConstant         ( c, 1   );
}


/**
 * @return the id of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c)
{
  return c->id;
}


/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c)
{
  return c->name;
}


/**
 * @return the spatialDimensions of this Compartment.
 */
LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c)
{
  return c->spatialDimensions;
}


/**
 * @return the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c)
{
  return c->size;
}


/**
 * @return the volume (size in L2) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c)
{
  return Compartment_getSize(c);
}


/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c)
{
  return c->units;
}


/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c)
{
  return c->outside;
}


/**
 * @return true (non-zero) if this Compartment is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c)
{
  return c->constant;
}


/**
 * @return 1 if the id of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c)
{
  return (c->id != NULL);
}


/**
 * @return 1 if the name of this Compartment has been set, 0 otherwise.
 *
 * In SBML L1, a Compartment name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c)
{
  return (c->name != NULL);
}


/**
 * @return 1 if the size (volume in L1) of this Compartment has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c)
{
  return c->isSet.size;
}


/**
 * @return 1 if the volume (size in L2) of this Compartment has been set, 0
 * otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c)
{
  return c->isSet.volume;
}


/**
 * @return 1 if the units of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c)
{
  return (c->units != NULL);
}


/**
 * @return 1 if the outside of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c)
{
  return (c->outside != NULL);
}


/**
 * Sets the id of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setId (Compartment_t *c, const char *sid)
{
  if (c->id == sid) return;


  if (c->id != NULL)
  {
    safe_free(c->id);
  }

  c->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name of this Compartment to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *string)
{
  if (c->name == string) return;


  if (c->name != NULL)
  {
    safe_free(c->name);
  }

  c->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
LIBSBML_EXTERN
void
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value)
{
  if (value >= 0 && value <= 3)
  {
    c->spatialDimensions = value;
  }
}


/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setSize (Compartment_t *c, double value)
{
  c->size         = value;
  c->isSet.size   = 1;
  c->isSet.volume = 1;
}


/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value)
{
  c->size         = value;
  c->isSet.volume = 1;
}


/**
 * Sets the units of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sid)
{
  if (c->units == sid) return;


  if (c->units != NULL)
  {
    safe_free(c->units);
  }

  c->units = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the outside of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sid)
{
  if (c->outside == sid) return;


  if (c->outside != NULL)
  {
    safe_free(c->outside);
  }

  c->outside = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the constant field of this Compartment to value (boolean).
 */
LIBSBML_EXTERN
void
Compartment_setConstant (Compartment_t *c, int value)
{
  c->constant = value;
}


/**
 * Unsets the name of this Compartment.  This is equivalent to:
 * safe_free(c->name); c->name = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetName (Compartment_t *c)
{
  safe_free(c->name);
  c->name = NULL;
}


/**
 * Unsets the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetSize (Compartment_t *c)
{
  c->size         = util_NaN();
  c->isSet.size   = 0;
  c->isSet.volume = 0;
}


/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume is optional with no default
 * value and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c)
{
  Compartment_unsetSize(c);
}


/**
 * Unsets the units of this Compartment.  This is equivalent to:
 * safe_free(c->units); c->units = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetUnits (Compartment_t *c)
{
  safe_free(c->units);
  c->units = NULL;
}


/**
 * Unsets the outside of this Compartment.  This is equivalent to:
 * safe_free(c->outside); c->outside = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetOutside (Compartment_t *c)
{
  safe_free(c->outside);
  c->outside = NULL;
}
