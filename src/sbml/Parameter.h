/**
 * Filename    : Parameter.h
 * Description : SBML Parameter
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Parameter_h
#define Parameter_h


#include "extern.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef void Parameter_t;


/**
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void);

/**
 * Creates a new Parameter with the given id, value and units and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setId(p, id); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *sid, double value, const char *units);

/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p);

/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = 1  (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p);


/**
 * @return the id of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p);

/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p);

/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p);

/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p);

/**
 * @return true (non-zero) if this Parameter is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p);


/**
 * @return 1 if the id of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p);

/**
 * @return 1 if the name of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p);

/**
 * @return 1 if the value of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p);

/**
 * @return 1 if the units of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p);


/**
 * Moves the id field of this Parameter to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Parameter_moveIdToName (Parameter_t *p);

/**
 * Moves the id field of this Parameter to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Parameter_moveNameToId (Parameter_t *p);


/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid);

/**
 * Sets the name of this Parameter to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *string);

/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value);

/**
 * Sets the units of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sid);

/**
 * Sets the constant of this Parameter to value (boolean).
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value);


/**
 * Unsets the name of this Parameter.  This is equivalent to:
 * safe_free(p->name); p->name = NULL;
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p);

/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p);


/**
 * Unsets the units of this Parameter.  This is equivalent to:
 * safe_free(p->units); p->units = NULL;
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p);


/**
 * The ParameterIdCmp function compares the string sid to p->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than p->id.
 * Returns -1 if either sid or p->id is NULL.
 */
LIBSBML_EXTERN
int
ParameterIdCmp (const char *sid, const Parameter_t *p);


END_C_DECLS


#endif  /** Parameter_h **/
