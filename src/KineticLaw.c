/**
 * Filename    : KineticLaw.c
 * Description : SBML KineticLaw
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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

#include "sbml/FormulaFormatter.h"
#include "sbml/FormulaParser.h"

#include "sbml/KineticLaw.h"


/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void)
{
  KineticLaw_t *kl;


  kl = (KineticLaw_t *) safe_calloc(1, sizeof(KineticLaw_t));
  SBase_init((SBase_t *) kl, SBML_KINETIC_LAW);

  kl->parameter = (ListOf_t *) ListOf_create();

  return kl;
}


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
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWith ( const char *formula,
                        const char *timeUnits,
                        const char *substanceUnits )
{
  KineticLaw_t *kl = KineticLaw_create();


  KineticLaw_setFormula       ( kl, formula        );
  KineticLaw_setTimeUnits     ( kl, timeUnits      );
  KineticLaw_setSubstanceUnits( kl, substanceUnits );

  return kl;
}


/**
 * Frees the given KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl)
{
  if (kl == NULL) return;


  SBase_clear((SBase_t *) kl);

  ListOf_free(kl->parameter);

  safe_free( kl->formula        );
  safe_free( kl->timeUnits      );
  safe_free( kl->substanceUnits );

  ASTNode_free( kl->math );

  safe_free(kl);
}


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl)
{
  return kl->formula;
}


/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl)
{
  return kl->math;
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (const KineticLaw_t *kl)
{
  return kl->parameter;
}


/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl)
{
  return kl->timeUnits;
}


/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl)
{
  return kl->substanceUnits;
}


/**
 * @return 1 if the formula of this KineticLaw has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl)
{
  return (kl->formula != NULL);
}


/**
 * @return 1 if the math of this KineticLaw has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl)
{
  return (kl->math != NULL);
}


/**
 * @return 1 if the timeUnits of this KineticLaw has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl)
{
  return (kl->timeUnits != NULL);
}


/**
 * @return 1 if the substanceUnits of this KineticLaw has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl)
{
  return (kl->substanceUnits != NULL);
}


/**
 * Sets the formula of this KineticLaw to a copy of string.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *string)
{
  if (kl->formula == string) return;


  if (kl->formula != NULL)
  {
    safe_free(kl->formula);
  }

  kl->formula = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the formula of this KineticLaw based on the current value of its
 * math field.  This convenience function is functionally equivalent to:
 *
 *   KineticLaw_setFormula(kl, SBML_formulaToString( KineticLaw_getMath(kl) ))
 *
 * except you do not need to track and free the value returned by
 * SBML_formulaToString().
 *
 * If !KineticLaw_isSetMath(kl), this function has no effect.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormulaFromMath (KineticLaw_t *kl)
{
  if (!KineticLaw_isSetMath(kl)) return;


  if (kl->formula != NULL)
  {
    safe_free(kl->formula);
  }

  kl->formula = SBML_formulaToString(kl->math);
}


/**
 * Sets the math of this KineticLaw to the given ASTNode.
 *
 * The node <b>is not copied</b> and this KineticLaw <b>takes ownership</b>
 * of it; i.e. subsequent calls to this function or a call to
 * KineticLaw_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, ASTNode_t *math)
{
  if (kl->math == math) return;


  if (kl->math != NULL)
  {
    ASTNode_free(kl->math);
  }

  kl->math = math;
}


/**
 * Sets the math of this KineticLaw from its current formula string.  This
 * convenience function is functionally equivalent to:
 *
 *   KineticLaw_setMath(kl, SBML_parseFormula( KineticLaw_getFormula(kl) ))
 *
 * If !KineticLaw_isSetFormula(kl), this function has no effect.
 */
LIBSBML_EXTERN
void
KineticLaw_setMathFromFormula (KineticLaw_t *kl)
{
  if (!KineticLaw_isSetFormula(kl)) return;


  if (kl->math != NULL)
  {
    ASTNode_free(kl->math);
  }

  kl->math = SBML_parseFormula(kl->formula);
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sname)
{
  if (kl->timeUnits == sname) return;


  if (kl->timeUnits != NULL)
  {
    safe_free(kl->timeUnits);
  }

  kl->timeUnits = (sname == NULL) ? NULL : safe_strdup(sname);
}


/**
 * Sets the substanceUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sname)
{
  if (kl->substanceUnits == sname) return;


  if (kl->substanceUnits != NULL)
  {
    safe_free(kl->substanceUnits);
  }

  kl->substanceUnits = (sname == NULL) ? NULL : safe_strdup(sname);
}


/**
 * Adds the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, Parameter_t *p)
{
  ListOf_append(kl->parameter, p);
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (const KineticLaw_t *kl, unsigned int n)
{
  return (Parameter_t *) ListOf_get(kl->parameter, n);
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return ListOf_getNumItems(kl->parameter);
}


/**
 * Unsets the timeUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->timeUnits); kl->timeUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl)
{
  safe_free(kl->timeUnits);
  kl->timeUnits = NULL;
}


/**
 * Unsets the substanceUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->substanceUnits); kl->substanceUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl)
{
  safe_free(kl->substanceUnits);
  kl->substanceUnits = NULL;
}
