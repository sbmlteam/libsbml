/**
 * Filename    : Rule.h
 * Description : SBML Rule
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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


#ifndef Rule_h
#define Rule_h


#include "extern.h"

#include "ASTNode.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef void Rule_t;


/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r);

/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r);


/**
 * @return 1 if the formula for this Rule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r);

/**
 * @return 1 if the math for this Rule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r);


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *string);

/**
 * Sets the formula of this Rule based on the current value of its math
 * field.  This convenience function is functionally equivalent to:
 *
 *   Rule_setFormula(r, SBML_formulaToString( Rule_getMath(r) ))
 *
 * except you do not need to track and free the value returned by
 * SBML_formulaToString().
 *
 * If !Rule_isSetMath(r), this function has no effect.
 */
LIBSBML_EXTERN
void
Rule_setFormulaFromMath (Rule_t *r);

/**
 * Sets the math of this Rule to the given ASTNode.
 *
 * The node <b>is not copied</b> and this Rule <b>takes ownership</b> of
 * it; i.e. subsequent calls to this function or a call to Rule_free() will
 * free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
Rule_setMath (Rule_t *r, ASTNode_t *math);

/**
 * Sets the math of this Rule from its current formula string.  This
 * convenience function is functionally equivalent to:
 *
 *   Rule_setMath(r, SBML_parseFormula( Rule_getFormula(r) ))
 *
 * If !Rule_isSetFormula(r), this function has no effect.
 */
LIBSBML_EXTERN
void
Rule_setMathFromFormula (Rule_t *r);


END_C_DECLS


#endif  /** Rule_h **/
