/**
 * \file    TestSBMLDocument.c
 * \brief   SBMLDocument unit tests
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

#include "common.h"

#include "SBase.h"
#include "Model.h"
#include "SBMLDocument.h"


START_TEST (test_SBMLDocument_create)
{
  SBMLDocument_t *d = SBMLDocument_create();


  fail_unless( SBase_getTypeCode  ((SBase_t *) d) == SBML_DOCUMENT );
  fail_unless( SBase_getNotes     ((SBase_t *) d) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) d) == NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2);
  fail_unless( SBMLDocument_getVersion(d) == 1);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0 );
  fail_unless( SBMLDocument_getNumErrors  (d) == 0 );
  fail_unless( SBMLDocument_getNumFatals  (d) == 0 );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_createWith)
{
  SBMLDocument_t *d = SBMLDocument_createWith(1, 2);


  fail_unless( SBase_getTypeCode  ((SBase_t *) d) == SBML_DOCUMENT );
  fail_unless( SBase_getNotes     ((SBase_t *) d) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) d) == NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 1);
  fail_unless( SBMLDocument_getVersion(d) == 2);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0 );
  fail_unless( SBMLDocument_getNumErrors  (d) == 0 );
  fail_unless( SBMLDocument_getNumFatals  (d) == 0 );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_free_NULL)
{
  SBMLDocument_free(NULL);
}
END_TEST


START_TEST (test_SBMLDocument_setModel)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  Model_t        *m1 = Model_create();
  Model_t        *m2 = Model_create();


  fail_unless(SBMLDocument_getModel(d) == NULL);

  SBMLDocument_setModel(d, m1);
  fail_unless(SBMLDocument_getModel(d) == m1);

  /* Reflexive case (pathological) */
  SBMLDocument_setModel(d, SBMLDocument_getModel(d));
  fail_unless(SBMLDocument_getModel(d) == m1);

  SBMLDocument_setModel(d, m2);
  fail_unless(SBMLDocument_getModel(d) == m2);

  SBMLDocument_free(d);
  /* m1 is freed by SBMLDocument_setModel(d, m2); */
}
END_TEST


Suite *
create_suite_SBMLDocument (void) 
{ 
  Suite *suite = suite_create("SBMLDocument");
  TCase *tcase = tcase_create("SBMLDocument");
 

  tcase_add_test(tcase, test_SBMLDocument_create     );
  tcase_add_test(tcase, test_SBMLDocument_createWith );
  tcase_add_test(tcase, test_SBMLDocument_free_NULL  );
  tcase_add_test(tcase, test_SBMLDocument_setModel   );

  suite_add_tcase(suite, tcase);

  return suite;
}
