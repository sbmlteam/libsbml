/**
 * Filename    : KineticLaw.c
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

  kl->parameter = (List_t *) List_create();

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

  List_freeItems(kl->parameter, Parameter_free, Parameter_t);
  List_free(kl->parameter);

  safe_free(kl->formula);
  safe_free(kl->timeUnits);
  safe_free(kl->substanceUnits);
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
  if (kl->formula != NULL)
  {
    safe_free(kl->formula);
  }

  kl->formula = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sname)
{
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
  List_add(kl->parameter, p);
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (const KineticLaw_t *kl, unsigned int n)
{
  return (Parameter_t *) List_get(kl->parameter, n);
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return List_size(kl->parameter);
}


/**
 * Unsets the formula of this KineticLaw.  This is equivalent to:
 * safe_free(kl->formula); kl->formula = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetFormula (KineticLaw_t *kl)
{
  safe_free(kl->formula);
  kl->formula = NULL;
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
