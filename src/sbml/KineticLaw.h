/**
 * Filename    : KineticLaw.h
 * Description : SBML KineticLaw
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-25
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


#ifndef KineticLaw_h
#define KineticLaw_h


#include "List.h"
#include "SBase.h"
#include "Parameter.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  char   *formula;
  List_t *parameter;
  char   *timeUnits;
  char   *substanceUnits;
} KineticLaw_t;


/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
KineticLaw_t *
KineticLaw_create (void);

/**
 * Creates a new KineticLaw with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setFormula(kl, formula);
 *   KineticLaw_setTimeUnits(kl, timeUnits);
 *   ...;
 */
KineticLaw_t *
KineticLaw_createWith ( const char *formula,
                        const char *timeUnits,
                        const char *substanceUnits );

/**
 * Frees the given KineticLaw.
 */
void
KineticLaw_free (KineticLaw_t *kl);

/**
 * Sets the formula field of this KineticLaw to a copy of string.
 */
void
KineticLaw_setFormula(KineticLaw_t *kl, const char *string);

/**
 * Sets the timeUnits field of this KineticLaw to a copy of sname.
 */
void
KineticLaw_setTimeUnits(KineticLaw_t *kl, const char *sname);

/**
 * Sets the substanceUnits field of this KineticLaw to a copy of sname.
 */
void
KineticLaw_setSubstanceUnits(KineticLaw_t *kl, const char *sname);

/**
 * Adds the given Parameter to this KineticLaw.
 */
void
KineticLaw_addParameter(KineticLaw_t *kl, Parameter_t *p);

/**
 * @return the nth Parameter of this KineticLaw.
 */
Parameter_t *
KineticLaw_getParameter(KineticLaw_t *kl, unsigned int n);

/**
 * @return the number of Parameters in this KineticLaw.
 */
unsigned int
KineticLaw_getNumParameters(KineticLaw_t *kl);


#ifdef __cplusplus
}
#endif


#endif  /** KineticLaw_h **/
