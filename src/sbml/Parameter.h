/**
 * Filename    : Parameter.h
 * Description : SBML Parameter
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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


#ifndef Parameter_h
#define Parameter_h


#include "common.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;

  char   *name;
  double  value;
  char   *units;

  struct
  {
    unsigned int value:1;
  } isSet;

} Parameter_t;


/**
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void);

/**
 * Creates a new Parameter with the given name, value and units and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setName(p, name); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *name, double value, const char *units);

/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p);


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
 * @return 1 if the name of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p);

/**
 * @return 1 if the value of this Parameter has been set, 0 otherwise.
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
 * Sets the name of this Parameter to a copy of sname.
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *sname);

/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value);

/**
 * Sets the units of this Parameter to a copy of sname.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sname);


/**
 * Marks the value of this Parameter as unset.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p);


END_C_DECLS


#endif  /** Parameter_h **/
