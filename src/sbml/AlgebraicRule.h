/**
 * Filename    : AlgebraicRule.h
 * Description : SBML AlgebraicRule
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-26
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


#ifndef AlgebraicRule_h
#define AlgebraicRule_h


#include "SBase.h"
#include "Rule.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  RULE_FIELDS;
} AlgebraicRule_t;


/**
 * Creates a new AlgebraicRule and returns a pointer to it.
 */
AlgebraicRule_t *
AlgebraicRule_create (void);

/**
 * Creates a new AlgebraicRule with the given formula and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   AlgebraicRule_t *ar = AlgebraicRule_create();
 *   Rule_setFormula((Rule_t *) ar, formula);
 */
AlgebraicRule_t *
AlgebraicRule_createWith (const char *formula);

/**
 * Frees the given AlgebraicRule.
 */
void
AlgebraicRule_free (AlgebraicRule_t *ar);


#ifdef __cplusplus
}
#endif


#endif  /** AlgebraicRule_h **/
