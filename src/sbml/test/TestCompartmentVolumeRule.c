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


#include <check.h>
#include "common/common.h"

#include "SBase.h"
#include "Rule.h"
#include "AssignmentRule.h"
#include "CompartmentVolumeRule.h"


static CompartmentVolumeRule_t *CVR;


void
CompartmentVolumeRuleTest_setup (void)
{
  CVR = CompartmentVolumeRule_create();

  if (CVR == NULL)
  {
    fail("CompartmentVolumeRule_create() returned a NULL pointer.");
  }
}


void
CompartmentVolumeRuleTest_teardown (void)
{
  CompartmentVolumeRule_free(CVR);
}


START_TEST (test_CompartmentVolumeRule_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) CVR) ==
               SBML_COMPARTMENT_VOLUME_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) CVR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) CVR) == NULL );

  fail_unless( Rule_getFormula((Rule_t *) CVR) == NULL );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) CVR) ==
               RULE_TYPE_SCALAR );

  fail_unless( CompartmentVolumeRule_getCompartment(CVR) == NULL );
  fail_unless( !CompartmentVolumeRule_isSetCompartment(CVR) );
}
END_TEST


START_TEST (test_CompartmentVolumeRule_createWith)
{
  CompartmentVolumeRule_t *cvr;


  cvr = CompartmentVolumeRule_createWith("v + 1", RULE_TYPE_RATE, "c");

  fail_unless( SBase_getTypeCode  ((SBase_t *) cvr) ==
               SBML_COMPARTMENT_VOLUME_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) cvr) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) cvr) == NULL );


  fail_unless( !strcmp(Rule_getFormula((Rule_t *) cvr), "v + 1") );
  fail_unless( !strcmp(CompartmentVolumeRule_getCompartment(cvr), "c") );

  fail_unless( AssignmentRule_getType((AssignmentRule_t *) cvr) ==
               RULE_TYPE_RATE );

  fail_unless( CompartmentVolumeRule_isSetCompartment(cvr) );

  CompartmentVolumeRule_free(cvr);
}
END_TEST


START_TEST (test_CompartmentVolumeRule_free_NULL)
{
  CompartmentVolumeRule_free(NULL);
}
END_TEST


START_TEST (test_CompartmentVolumeRule_setCompartment)
{
  const char *c;
  char *compartment = "cell";


  CompartmentVolumeRule_setCompartment(CVR, compartment);

  fail_unless( !strcmp(CompartmentVolumeRule_getCompartment(CVR), compartment),
               NULL );
  fail_unless( CompartmentVolumeRule_isSetCompartment(CVR) );

  if (CompartmentVolumeRule_getCompartment(CVR) == compartment)
  {
    fail( "CompartmentVolumeRule_setCompartment(...)"
          " did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  c = CompartmentVolumeRule_getCompartment(CVR);
  CompartmentVolumeRule_setCompartment(CVR, c);
  fail_unless( !strcmp(CompartmentVolumeRule_getCompartment(CVR), compartment),
               NULL );

  CompartmentVolumeRule_setCompartment(CVR, NULL);
  fail_unless( !CompartmentVolumeRule_isSetCompartment(CVR) );

  if (CompartmentVolumeRule_getCompartment(CVR) != NULL)
  {
    fail( "CompartmentVolumeRule_setCompartment(CVR, NULL)"
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
