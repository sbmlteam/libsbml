/**
 * Filename    : Species.h
 * Description : SBML Species
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


#ifndef Species_h
#define Species_h


#include "extern.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;

  char   *id;
  char   *name;
  char   *compartment;

  union
  {
    double Amount;
    double Concentration;
  } initial;


  char   *substanceUnits;
  char   *spatialSizeUnits;
  int     hasOnlySubstanceUnits;
  int     boundaryCondition;
  int     charge;
  int     constant;

  struct
  {
    unsigned int initialAmount       :1;
    unsigned int initialConcentration:1;
    unsigned int charge              :1;
  } isSet;

} Species_t;


/**
 * Creates a new Species and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_create (void);

/**
 * Creates a new Species with the given id, compartment, initialAmount,
 * substanceUnits, boundaryCondition and charge and returns a pointer to
 * it.  This convenience function is functionally equivalent to:
 *
 *   Species_t *s = Species_create();
 *   Species_setId(s, sid); Species_setCompartment(s, compartment); ...;
 */
LIBSBML_EXTERN
Species_t *
Species_createWith( const char *sid,
                    const char *compartment,
                    double      initialAmount,
                    const char *substanceUnits,
                    int         boundaryCondition,
                    int         charge );

/**
 * Frees the given Species.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s);

/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = 0  (false)
 *   - constant          = 0  (false)  (L2 only)
 */
LIBSBML_EXTERN
void
Species_initDefaults (Species_t *s);


/**
 * @return the id of this Species
 */
LIBSBML_EXTERN
const char *
Species_getId (const Species_t *s);

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
 * @return the initialConcentration of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialConcentration (const Species_t *s);

/**
 * @return the substanceUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSubstanceUnits (const Species_t *s);

/**
 * @return the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpatialSizeUnits (const Species_t *s);

/**
 * @return the units of this Species (L1 only).
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s);

/**
 * @return true (non-zero) if this Species hasOnlySubstanceUnits, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getHasOnlySubstanceUnits (const Species_t *s);

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
 * @return true (non-zero) if this Species is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getConstant (const Species_t *s);


/**
 * @return 1 if the id of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetId (const Species_t *s);

/**
 * @return 1 if the name of this Species has been set, 0 otherwise.
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
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
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s);

/**
 * @return 1 if the initialConcentration of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialConcentration (const Species_t *s);

/**
 * @return 1 if the substanceUnits of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSubstanceUnits (const Species_t *s);

/**
 * @return 1 if the spatialSizeUnits of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpatialSizeUnits (const Species_t *s);

/**
 * @return 1 if the units of this Species has been set, 0 otherwise
 * (L1 only).
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s);

/**
 * @return 1 if the charge of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s);


/**
 * Sets the id of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setId (Species_t *s, const char *sid);

/**
 * Sets the name of this Species to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *string);

/**
 * Sets the compartment of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sid);

/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value);

/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
LIBSBML_EXTERN
void
Species_setInitialConcentration (Species_t *s, double value);

/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSubstanceUnits (Species_t *s, const char *sid);

/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpatialSizeUnits (Species_t *s, const char *sid);

/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname);

/**
 * Sets the hasOnlySubstanceUnits field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setHasOnlySubstanceUnits (Species_t *s, int value);

/**
 * Sets the boundaryCondition of this Species to value (boolean).
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
 * Sets the constant field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setConstant (Species_t *s, int value);


/**
 * Unsets the name of this Species.  This is equivalent to:
 * safe_free(s->name); s->name = NULL;
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Species_unsetName (Species_t *s);

/**
 * Unsets the initialAmount of this Species.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s);

/**
 * Unsets the initialConcentration of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetInitialConcentration (Species_t *s);

/**
 * Unsets the substanceUnits of this Species.  This is equivalent to:
 * safe_free(s->substanceUnits); s->substanceUnits = NULL;
 */
LIBSBML_EXTERN
void
Species_unsetSubstanceUnits (Species_t *s);

/**
 * Unsets the spatialSizeUnits of this Species.  This is equivalent to:
 * safe_free(s->spatialSizeUnits); s->spatialSizeUnits = NULL;
 */
LIBSBML_EXTERN
void
Species_unsetSpatialSizeUnits (Species_t *s);

/**
 * Unsets the units of this Species (L1 only).
 */
LIBSBML_EXTERN
void
Species_unsetUnits (Species_t *s);

/**
 * Unsets the charge of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s);


/**
 * The SpeciesIdCmp function compares the string sid to species->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match, or be greater than
 * species->id.  Returns -1 if either sid or species->id is NULL.
 */
LIBSBML_EXTERN
int
SpeciesIdCmp (const char *sid, const Species_t *s);


END_C_DECLS


#endif  /** Species_h **/
