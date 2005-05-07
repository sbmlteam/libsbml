/**
 * \file    TestKineticLaw.c
 * \brief   SBML KineticLaw unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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

#include "common/common.h"

#include "math/FormulaFormatter.h"
#include "math/FormulaParser.h"

#include "SBase.h"
#include "Parameter.h"
#include "KineticLaw.h"


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
  fail_unless( SBase_getTypeCode  ((SBase_t *) KL) == SBML_KINETIC_LAW );
  fail_unless( SBase_getMetaId    ((SBase_t *) KL) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) KL) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) KL) == NULL );

  fail_unless( KineticLaw_getFormula       (KL) == NULL );
  fail_unless( KineticLaw_getMath          (KL) == NULL );
  fail_unless( KineticLaw_getTimeUnits     (KL) == NULL );
  fail_unless( KineticLaw_getSubstanceUnits(KL) == NULL );

  fail_unless( !KineticLaw_isSetFormula       (KL) );
  fail_unless( !KineticLaw_isSetMath          (KL) );
  fail_unless( !KineticLaw_isSetTimeUnits     (KL) );
  fail_unless( !KineticLaw_isSetSubstanceUnits(KL) );

  fail_unless(KineticLaw_getNumParameters(KL) == 0);
}
END_TEST


START_TEST (test_KineticLaw_createWith)
{
  const ASTNode_t *math;
  char *formula;

  KineticLaw_t *kl = KineticLaw_createWith("k1 * X0", "seconds", "ug");


  fail_unless( SBase_getTypeCode  ((SBase_t *) kl) == SBML_KINETIC_LAW );
  fail_unless( SBase_getMetaId    ((SBase_t *) kl) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) kl) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) kl) == NULL );

  math = KineticLaw_getMath(kl);
  fail_unless( math != NULL );

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "k1 * X0") );

  fail_unless( !strcmp( KineticLaw_getFormula       (kl), formula  ) );
  fail_unless( !strcmp( KineticLaw_getTimeUnits     (kl), "seconds") );
  fail_unless( !strcmp( KineticLaw_getSubstanceUnits(kl), "ug"     ) );

  fail_unless( KineticLaw_isSetMath          (kl) );
  fail_unless( KineticLaw_isSetFormula       (kl) );
  fail_unless( KineticLaw_isSetTimeUnits     (kl) );
  fail_unless( KineticLaw_isSetSubstanceUnits(kl) );

  fail_unless(KineticLaw_getNumParameters(kl) == 0);

  KineticLaw_free(kl);
  safe_free(formula);
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

  fail_unless( !strcmp(KineticLaw_getFormula(KL), formula) );
  fail_unless( KineticLaw_isSetFormula(KL)   );

  if (KineticLaw_getFormula(KL) == formula)
  {
    fail("KineticLaw_setFormula(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setFormula(KL, KineticLaw_getFormula(KL));
  fail_unless( !strcmp(KineticLaw_getFormula(KL), formula) );

  KineticLaw_setFormula(KL, NULL);
  fail_unless( !KineticLaw_isSetFormula(KL) );

  if (KineticLaw_getFormula(KL) != NULL)
  {
    fail("KineticLaw_setFormula(KL, NULL) did not clear string.");
  }
}
END_TEST


/**
 * setFormulaFromMath() is no longer necessary.  LibSBML now keeps formula
 * strings and math ASTs synchronized automatically.  This (now modified)
 * test is kept around to demonstrate the behavioral change.
 */
START_TEST (test_KineticLaw_setFormulaFromMath)
{
  ASTNode_t *math = SBML_parseFormula("k1 * X0");


  fail_unless( !KineticLaw_isSetMath   (KL) );
  fail_unless( !KineticLaw_isSetFormula(KL) );

  KineticLaw_setFormulaFromMath(KL);
  fail_unless( !KineticLaw_isSetMath   (KL) );
  fail_unless( !KineticLaw_isSetFormula(KL) );

  KineticLaw_setMath(KL, math);
  fail_unless(  KineticLaw_isSetMath(KL) );
  fail_unless( /* ! */ KineticLaw_isSetFormula(KL) );

  KineticLaw_setFormulaFromMath(KL);
  fail_unless( KineticLaw_isSetMath   (KL) );
  fail_unless( KineticLaw_isSetFormula(KL) );

  fail_unless( !strcmp(KineticLaw_getFormula(KL), "k1 * X0") );
}
END_TEST


START_TEST (test_KineticLaw_setMath)
{
  ASTNode_t *math = SBML_parseFormula("k3 / k2");


  KineticLaw_setMath(KL, math);

  fail_unless( KineticLaw_getMath(KL) == math );
  fail_unless( KineticLaw_isSetMath(KL) );

  /* Reflexive case (pathological) */
  KineticLaw_setMath(KL, (ASTNode_t *) KineticLaw_getMath(KL));
  fail_unless( KineticLaw_getMath(KL) == math );

  KineticLaw_setMath(KL, NULL);
  fail_unless( !KineticLaw_isSetMath(KL) );

  if (KineticLaw_getMath(KL) != NULL)
  {
    fail( "KineticLaw_setMath(KL, NULL) did not clear ASTNode." );
  }
}
END_TEST


/**
 * setMathFromFormula() is no longer necessary.  LibSBML now keeps formula
 * strings and math ASTs synchronized automatically.  This (now modified)
 * test is kept around to demonstrate the behavioral change.
 */
START_TEST (test_KineticLaw_setMathFromFormula)
{
  char *formula = "k3 / k2";


  fail_unless( !KineticLaw_isSetMath   (KL) );
  fail_unless( !KineticLaw_isSetFormula(KL) );

  KineticLaw_setMathFromFormula(KL);
  fail_unless( !KineticLaw_isSetMath   (KL) );
  fail_unless( !KineticLaw_isSetFormula(KL) );

  KineticLaw_setFormula(KL, formula);
  fail_unless( /* ! */ KineticLaw_isSetMath(KL) );
  fail_unless(  KineticLaw_isSetFormula(KL) );

  KineticLaw_setMathFromFormula(KL);
  fail_unless( KineticLaw_isSetMath   (KL) );
  fail_unless( KineticLaw_isSetFormula(KL) );

  formula = SBML_formulaToString( KineticLaw_getMath(KL) );

  fail_unless( !strcmp(formula, "k3 / k2") );

  safe_free(formula);
}
END_TEST


START_TEST (test_KineticLaw_setTimeUnits)
{
  char *units = "time";


  KineticLaw_setTimeUnits(KL, units);

  fail_unless( !strcmp(KineticLaw_getTimeUnits(KL), units) );
  fail_unless( KineticLaw_isSetTimeUnits(KL) );

  if (KineticLaw_getTimeUnits(KL) == units)
  {
    fail("KineticLaw_setTimeUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setTimeUnits(KL, KineticLaw_getTimeUnits(KL));
  fail_unless( !strcmp(KineticLaw_getTimeUnits(KL), units) );

  KineticLaw_setTimeUnits(KL, NULL);
  fail_unless( !KineticLaw_isSetTimeUnits(KL) );

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

  fail_unless( !strcmp(KineticLaw_getSubstanceUnits(KL), units) );
  fail_unless( KineticLaw_isSetSubstanceUnits(KL) );

  if (KineticLaw_getSubstanceUnits(KL) == units)
  {
    fail("KineticLaw_setSubstanceUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  KineticLaw_setSubstanceUnits(KL, KineticLaw_getSubstanceUnits(KL));
  fail_unless( !strcmp(KineticLaw_getSubstanceUnits(KL), units) );

  KineticLaw_setSubstanceUnits(KL, NULL);
  fail_unless( !KineticLaw_isSetSubstanceUnits(KL) );

  if (KineticLaw_getSubstanceUnits(KL) != NULL)
  {
    fail("KineticLaw_setSubstanceUnits(KL, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_KineticLaw_addParameter)
{
  KineticLaw_addParameter(KL, Parameter_create());

  fail_unless( KineticLaw_getNumParameters(KL) == 1 );
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

  fail_unless( KineticLaw_getNumParameters(KL) == 2 );

  k1 = KineticLaw_getParameter(KL, 0);
  k2 = KineticLaw_getParameter(KL, 1);

  fail_unless( !strcmp(Parameter_getName(k1), "k1") );
  fail_unless( !strcmp(Parameter_getName(k2), "k2") );
  fail_unless( Parameter_getValue(k1) == 3.14 );
  fail_unless( Parameter_getValue(k2) == 2.72 );
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
