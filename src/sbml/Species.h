/**
 * Filename    : Species.h
 * Description : SBML Species
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


#ifndef Species_h
#define Species_h


#include "common.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;

  char   *name;
  char   *compartment;
  double  initialAmount;
  char   *units;
  int     boundaryCondition;
  int     charge;

  struct
  {
    unsigned int initialAmount    :1;
    unsigned int boundaryCondition:1;
    unsigned int charge           :1;
  } isSet;

} Species_t;


/**
 * Creates a new Species and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_create (void);

/**
 * Creates a new Species with the given name, compartment, initialAmount,
 * units, boundaryCondition and charge and returns a pointer to it.  This
 * convenience function is functionally equivalent to:
 *
 *   Species_t *s = Species_create();
 *   Species_setName(s, name); Species_setCompartment(s, compartment); ...;
 */
LIBSBML_EXTERN
Species_t *
Species_createWith( const char *sname,
                    const char *compartment,
                    double     initialAmount,
                    const char *units,
                    int        boundaryCondition,
                    int        charge );

/**
 * Frees the given Species.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s);

/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = 0 (false)
 */
LIBSBML_EXTERN
void
Species_initDefaults (Species_t *s);


/**
 * @return the name of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getName (const Species_t *s);

/**
 * @return the compartment of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getCompartment (const Species_t *s);

/**
 * @return the initialAmount of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialAmount (const Species_t *s);

/**
 * @return the units of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s);

/**
 * @return the boundaryCondition of this Species.
 */
LIBSBML_EXTERN
int
Species_getBoundaryCondition (const Species_t *s);

/**
 * @return the charge of this Species.
 */
LIBSBML_EXTERN
int
Species_getCharge (const Species_t *s);


/**
 * @return 1 if the name of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetName (const Species_t *s);

/**
 * @return 1 if the compartment of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCompartment (const Species_t *s);

/**
 * @return 1 if the initialAmount of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s);

/**
 * @return 1 if the units of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s);

/**
 * @return 1 if the boundaryCondition of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetBoundaryCondition (const Species_t *s);

/**
 * @return 1 if the charge of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s);


/**
 * Sets the name of this Species to a copy of sname.
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *sname);

/**
 * Sets the compartment of this Species to a copy of sname.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sname);

/**
 * Sets the initialAmount of this Species to value and marks the
 * field as set.
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value);

/**
 * Sets the units of this Species to a copy of sname.
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname);

/**
 * Sets the boundaryCondition of this Species to value (boolean) and marks
 * the field as set.
 */
LIBSBML_EXTERN
void
Species_setBoundaryCondition (Species_t *s, int value);

/**
 * Sets the charge of this Species to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Species_setCharge (Species_t *s, int value);


/**
 * Marks the initialAmount of this Species as unset.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s);

/**
 * Marks the boundaryCondition of this Species as unset.
 */
LIBSBML_EXTERN
void
Species_unsetBoundaryCondition (Species_t *s);

/**
 * Marks the charge of this Species as unset.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s);


END_C_DECLS


#endif  /** Species_h **/
