/**
 * Filename    : TestKineticLaw.c
 * Description : SBML KineticLaw unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "sbml/common.h"

#include "sbml/FormulaFormatter.h"
#include "sbml/FormulaParser.h"

#include "sbml/KineticLaw.h"


static KineticLaw_t *KL;


void
KineticLawTest_setup (void)
{
  KL = KineticLaw_create();

  if (KL == NULL)
  {
    fail("KineticLaw_create() returned a NULL pointer.");
  }
}


void
KineticLawTest_teardown (void)
{
  KineticLaw_free(KL);
}


START_TEST (test_KineticLaw_create)
{
  fail_unless( SBase_getTypeCode  (KL) == SBML_KINETIC_LAW, NULL );
  fail_unless( SBase_getMetaId    (KL) == NULL, NULL );
  fail_unless( SBase_getNotes     (KL) == NULL, NULL );
  fail_unless( SBase_getAnnotation(KL) == NULL, NULL );

  fail_unless( KineticLaw_getFormula       (KL) == NULL, NULL );
  fail_unless( KineticLaw_getMath          (KL) == NULL, NULL );
  fail_unless( KineticLaw_getTimeUnits     (KL) == NULL, NULL );
  fail_unless( KineticLaw_getSubstanceUnits(KL) == NULL, NULL );

  fail_unless( !KineticLaw_isSetFormula       (KL), NULL );
  fail_unless( !KineticLaw_isSetMath          (KL), NULL );
  fail_unless( !KineticLaw_isSetTimeUnits     (KL), NULL );
  fail_unless( !KineticLaw_isSetSubstanceUnits(KL), NULL );

  fail_unless(KineticLaw_getNumParameters(KL) == 0, NULL);
}
END_TEST


START_TEST (test_KineticLaw_createWith)
{
  KineticLaw_t *kl = KineticLaw_createWith("k1*X0", "seconds", "ug");


  fail_unless( SBase_getTypeCode  (kl) == SBML_KINETIC_LAW, NULL );
  fail_unless( SBase_getMetaId    (kl) == NULL, NULL );
  fail_unless( SBase_getNotes     (kl) == NULL, NULL );
  fail_unless( SBase_getAnnotation(kl) == NULL, NULL );

  fail_unless( KineticLaw_getMath(kl) == NULL, NULL );

  fail_unless( !strcmp( KineticLaw_getFormula       (kl), "k1*X0"  ), NULL );
  fail_unless( !strcmp( KineticLaw_getTimeUnits     (kl), "seconds"), NULL );
  fail_unless( !strcmp( KineticLaw_getSubstanceUnits(kl), "ug"     ), NULL );

  fail_unless( !KineticLaw_isSetMath         (kl), NULL );
  fail_unless( KineticLaw_isSetFormula       (kl), NULL );
  fail_unless( KineticLaw_isSetTimeUnits     (kl), NULL );
  fail_unless( KineticLaw_isSetSubstanceUnits(kl), NULL );

  fail_unless(KineticLaw_getNumParameters(kl) == 0, NULL);

  KineticLaw_free(kl);
}
END_TEST


START_TEST (test_KineticLaw_free_NULL)
{
  KineticLaw_free(NULL);
}
END_TEST


START_TEST (test_KineticLaw_setFormula)
{
  char *formula = "k1*X0";


  KineticLaw_setFormula(KL, formula);

  fail_unless( !strcmp(KineticLaw_getFormula(KL), formula), NULL );
  fail_unless( KineticLaw_isSetFormula(KL)  , NULL );

  if (KineticLaw_getFormula(KL) == formula)
  {
    fail("KineticLaw_setFormula(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setFormula(KL, KineticLaw_getFormula(KL));
  fail_unless( !strcmp(KineticLaw_getFormula(KL), formula), NULL );

  KineticLaw_setFormula(KL, NULL);
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  if (KineticLaw_getFormula(KL) != NULL)
  {
    fail("KineticLaw_setFormula(KL, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_KineticLaw_setFormulaFromMath)
{
  ASTNode_t *math = SBML_parseFormula("k1 * X0");


  fail_unless( !KineticLaw_isSetMath   (KL), NULL );
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setFormulaFromMath(KL);
  fail_unless( !KineticLaw_isSetMath   (KL), NULL );
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setMath(KL, math);
  fail_unless(  KineticLaw_isSetMath   (KL), NULL );
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setFormulaFromMath(KL);
  fail_unless( KineticLaw_isSetMath   (KL), NULL );
  fail_unless( KineticLaw_isSetFormula(KL), NULL );

  fail_unless( !strcmp(KineticLaw_getFormula(KL), "k1 * X0"), NULL );
}
END_TEST


START_TEST (test_KineticLaw_setMath)
{
  ASTNode_t *math = SBML_parseFormula("k3 / k2");


  KineticLaw_setMath(KL, math);

  fail_unless( KineticLaw_getMath(KL) == math, NULL );
  fail_unless( KineticLaw_isSetMath(KL), NULL );

  /* Reflexive case (pathological) */
  KineticLaw_setMath(KL, (ASTNode_t *) KineticLaw_getMath(KL));
  fail_unless( KineticLaw_getMath(KL) == math, NULL );

  KineticLaw_setMath(KL, NULL);
  fail_unless( !KineticLaw_isSetMath(KL), NULL );

  if (KineticLaw_getMath(KL) != NULL)
  {
    fail( "KineticLaw_setMath(KL, NULL) did not clear ASTNode." );
  }
}
END_TEST


START_TEST (test_KineticLaw_setMathFromFormula)
{
  char *formula = "k3 / k2";


  fail_unless( !KineticLaw_isSetMath   (KL), NULL );
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setMathFromFormula(KL);
  fail_unless( !KineticLaw_isSetMath   (KL), NULL );
  fail_unless( !KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setFormula(KL, formula);
  fail_unless( !KineticLaw_isSetMath   (KL), NULL );
  fail_unless(  KineticLaw_isSetFormula(KL), NULL );

  KineticLaw_setMathFromFormula(KL);
  fail_unless( KineticLaw_isSetMath   (KL), NULL );
  fail_unless( KineticLaw_isSetFormula(KL), NULL );

  formula = SBML_formulaToString( KineticLaw_getMath(KL) );

  fail_unless( !strcmp(formula, "k3 / k2"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_KineticLaw_setTimeUnits)
{
  char *units = "time";


  KineticLaw_setTimeUnits(KL, units);

  fail_unless( !strcmp(KineticLaw_getTimeUnits(KL), units), NULL );
  fail_unless( KineticLaw_isSetTimeUnits(KL), NULL );

  if (KineticLaw_getTimeUnits(KL) == units)
  {
    fail("KineticLaw_setTimeUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setTimeUnits(KL, KineticLaw_getTimeUnits(KL));
  fail_unless( !strcmp(KineticLaw_getTimeUnits(KL), units), NULL );

  KineticLaw_setTimeUnits(KL, NULL);
  fail_unless( !KineticLaw_isSetTimeUnits(KL), NULL );

  if (KineticLaw_getTimeUnits(KL) != NULL)
  {
    fail("KineticLaw_setTimeUnits(KL, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_KineticLaw_setSubstanceUnits)
{
  char *units = "substance";


  KineticLaw_setSubstanceUnits(KL, units);

  fail_unless( !strcmp(KineticLaw_getSubstanceUnits(KL), units), NULL );
  fail_unless( KineticLaw_isSetSubstanceUnits(KL), NULL );

  if (KineticLaw_getSubstanceUnits(KL) == units)
  {
    fail("KineticLaw_setSubstanceUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setSubstanceUnits(KL, KineticLaw_getSubstanceUnits(KL));
  fail_unless( !strcmp(KineticLaw_getSubstanceUnits(KL), units), NULL );

  KineticLaw_setSubstanceUnits(KL, NULL);
  fail_unless( !KineticLaw_isSetSubstanceUnits(KL), NULL );

  if (KineticLaw_getSubstanceUnits(KL) != NULL)
  {
    fail("KineticLaw_setSubstanceUnits(KL, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_KineticLaw_addParameter)
{
  KineticLaw_addParameter(KL, Parameter_create());

  fail_unless( KineticLaw_getNumParameters(KL) == 1, NULL );
}
END_TEST


START_TEST (test_KineticLaw_getParameter)
{
  Parameter_t *k1 = Parameter_create();
  Parameter_t *k2 = Parameter_create();

  Parameter_setName(k1, "k1");
  Parameter_setName(k2, "k2");

  Parameter_setValue(k1, 3.14);
  Parameter_setValue(k2, 2.72);

  KineticLaw_addParameter(KL, k1);
  KineticLaw_addParameter(KL, k2);

  fail_unless( KineticLaw_getNumParameters(KL) == 2, NULL );

  k1 = KineticLaw_getParameter(KL, 0);
  k2 = KineticLaw_getParameter(KL, 1);

  fail_unless( !strcmp(Parameter_getName(k1), "k1"), NULL );
  fail_unless( !strcmp(Parameter_getName(k2), "k2"), NULL );
  fail_unless( Parameter_getValue(k1) == 3.14, NULL );
  fail_unless( Parameter_getValue(k2) == 2.72, NULL );
}
END_TEST


Suite *
create_suite_KineticLaw (void)
{
  Suite *suite = suite_create("KineticLaw");
  TCase *tcase = tcase_create("KineticLaw");


  tcase_add_checked_fixture( tcase,
                             KineticLawTest_setup,
                             KineticLawTest_teardown );

  tcase_add_test( tcase, test_KineticLaw_create             );
  tcase_add_test( tcase, test_KineticLaw_createWith         );
  tcase_add_test( tcase, test_KineticLaw_free_NULL          );
  tcase_add_test( tcase, test_KineticLaw_setFormula         );
  tcase_add_test( tcase, test_KineticLaw_setFormulaFromMath );
  tcase_add_test( tcase, test_KineticLaw_setMath            );
  tcase_add_test( tcase, test_KineticLaw_setMathFromFormula );
  tcase_add_test( tcase, test_KineticLaw_setTimeUnits       );
  tcase_add_test( tcase, test_KineticLaw_setSubstanceUnits  );
  tcase_add_test( tcase, test_KineticLaw_addParameter       );
  tcase_add_test( tcase, test_KineticLaw_getParameter       );

  suite_add_tcase(suite, tcase);

  return suite;
}
