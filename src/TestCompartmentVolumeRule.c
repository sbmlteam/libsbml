/**
 * Filename    : TestCompartmentVolumeRule.c
 * Description : CompartmentVolumeRule unit tests
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


#include "sbml/common.h"
#include "sbml/CompartmentVolumeRule.h"


CompartmentVolumeRule_t *CVR;


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
  fail_unless( CVR->typecode   == SBML_COMPARTMENT_VOLUME_RULE, NULL );
  fail_unless( CVR->notes      == NULL, NULL );
  fail_unless( CVR->annotation == NULL, NULL );
  fail_unless( CVR->formula    == NULL, NULL );

  fail_unless( CVR->type        == RULE_TYPE_SCALAR, NULL );
  fail_unless( CVR->compartment == NULL, NULL );
}
END_TEST


START_TEST (test_CompartmentVolumeRule_createWith)
{
  CompartmentVolumeRule_t *cvr;


  cvr = CompartmentVolumeRule_createWith("v + 1", RULE_TYPE_RATE, "c");

  fail_unless( cvr->typecode   == SBML_COMPARTMENT_VOLUME_RULE, NULL );
  fail_unless( cvr->notes      == NULL, NULL );
  fail_unless( cvr->annotation == NULL, NULL );


  fail_unless( !strcmp(cvr->formula    , "v + 1"), NULL );
  fail_unless( !strcmp(cvr->compartment, "c"    ), NULL );

  fail_unless( cvr->type == RULE_TYPE_RATE, NULL );

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
  char *compartment = "cell";


  CompartmentVolumeRule_setCompartment(CVR, compartment);

  fail_unless( !strcmp(CVR->compartment, compartment), NULL );

  if (CVR->compartment == compartment)
  {
    fail( "CompartmentVolumeRule_setCompartment(...)"
          " did not make a copy of string." );
  }

  CompartmentVolumeRule_setCompartment(CVR, NULL);

  if (CVR->compartment != NULL)
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
