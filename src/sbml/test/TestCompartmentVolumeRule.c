/**
 * \file    TestCompartmentVolumeRule.c
 * \brief   CompartmentVolumeRule unit tests
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


#include "common/common.h"

#include "SBase.h"
#include "Rule.h"

#include <check.h>

static Rule_t *CVR;


void
CompartmentVolumeRuleTest_setup (void)
{
  CVR = Rule_createAssignment();
  Rule_setL1TypeCode(CVR, SBML_COMPARTMENT_VOLUME_RULE);

  if (CVR == NULL)
  {
    fail("Rule_create() returned a NULL pointer.");
  }
}


void
CompartmentVolumeRuleTest_teardown (void)
{
  Rule_free(CVR);
}


START_TEST (test_CompartmentVolumeRule_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) CVR) ==
               SBML_ASSIGNMENT_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) CVR) ==
               SBML_COMPARTMENT_VOLUME_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) CVR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) CVR) == NULL );

  fail_unless( Rule_getFormula((Rule_t *) CVR) == NULL );
  fail_unless( Rule_getType((Rule_t *) CVR) ==  RULE_TYPE_SCALAR );

  fail_unless( Rule_getVariable(CVR) == NULL );
  fail_unless( !Rule_isSetVariable(CVR) );
}
END_TEST


START_TEST (test_CompartmentVolumeRule_createWith)
{
  Rule_t *cvr;


  cvr = Rule_createRateWithVariableAndFormula("c", "v + 1");
  Rule_setL1TypeCode(cvr, SBML_COMPARTMENT_VOLUME_RULE);

  fail_unless( SBase_getTypeCode((SBase_t *) cvr) ==
               SBML_RATE_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) cvr) ==
               SBML_COMPARTMENT_VOLUME_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) cvr) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) cvr) == NULL );


  fail_unless( !strcmp(Rule_getFormula(cvr), "v + 1") );
  fail_unless( !strcmp(Rule_getVariable(cvr), "c") );

  fail_unless( Rule_getType(cvr) ==   RULE_TYPE_RATE );

  fail_unless( Rule_isSetVariable(cvr) );

  Rule_free(cvr);
}
END_TEST


START_TEST (test_CompartmentVolumeRule_free_NULL)
{
  Rule_free(NULL);
}
END_TEST


START_TEST (test_CompartmentVolumeRule_setCompartment)
{
  const char *c;
  char *compartment = "cell";


  Rule_setVariable(CVR, compartment);

  fail_unless( !strcmp(Rule_getVariable(CVR), compartment),
               NULL );
  fail_unless( Rule_isSetVariable(CVR) );

  if (Rule_getVariable(CVR) == compartment)
  {
    fail( "Rule_setVariable(...)"
          " did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  c = Rule_getVariable(CVR);
  Rule_setVariable(CVR, c);
  fail_unless( !strcmp(Rule_getVariable(CVR), compartment),
               NULL );

  Rule_setVariable(CVR, NULL);
  fail_unless( !Rule_isSetVariable(CVR) );

  if (Rule_getVariable(CVR) != NULL)
  {
    fail( "Rule_setVariable(CVR, NULL)"
          " did not clear string." );
  }
}
END_TEST


Suite *
create_suite_CompartmentVolumeRule (void)
{
  Suite *suite = suite_create("CompartmentVolumeRule");
  TCase *tcase = tcase_create("CompartmentVolumeRule");


  tcase_add_checked_fixture( tcase,
                             CompartmentVolumeRuleTest_setup,
                             CompartmentVolumeRuleTest_teardown );

  tcase_add_test( tcase, test_CompartmentVolumeRule_create         );
  tcase_add_test( tcase, test_CompartmentVolumeRule_createWith     );
  tcase_add_test( tcase, test_CompartmentVolumeRule_free_NULL      );
  tcase_add_test( tcase, test_CompartmentVolumeRule_setCompartment );

  suite_add_tcase(suite, tcase);

  return suite;
}
