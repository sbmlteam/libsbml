/**
 * Filename    : TestSBMLDocument.c
 * Description : SBMLDocument unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-14
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


#include <check.h>

#include "sbml/common.h"
#include "sbml/SBMLDocument.h"
#include "sbml/SBMLReader.h"


#define XML_HEADER    "<?xml version='1.0' encoding='UTF-8'?>\n"
#define SBML_HEADER   "<sbml level='1' version='1'> <model name='testModel'>\n"
#define SBML_HEADER2  "<sbml level='2' version='1'> <model name='testModel'>\n"
#define SBML_FOOTER   "</model> </sbml>"

/**
 * Wraps the string s in the appropriate XML or SBML boilerplate.
 */
#define wrapXML(s)    XML_HEADER s
#define wrapSBML(s)   XML_HEADER SBML_HEADER  s SBML_FOOTER
#define wrapSBML2(s)  XML_HEADER SBML_HEADER2 s SBML_FOOTER


START_TEST (test_SBMLDocument_create)
{
  SBMLDocument_t *d = SBMLDocument_create();


  fail_unless( d->typecode   == SBML_DOCUMENT, NULL );
  fail_unless( d->notes      == NULL, NULL );
  fail_unless( d->annotation == NULL, NULL );

  fail_unless(d->level   == 2, NULL);
  fail_unless(d->version == 1, NULL);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 0, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_createWith)
{
  SBMLDocument_t *d = SBMLDocument_createWith(1, 2);


  fail_unless( d->typecode   == SBML_DOCUMENT, NULL );
  fail_unless( d->notes      == NULL, NULL );
  fail_unless( d->annotation == NULL, NULL );

  fail_unless(d->level   == 1, NULL);
  fail_unless(d->version == 2, NULL);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 0, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

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


  fail_unless(d->model == NULL, NULL);

  SBMLDocument_setModel(d, m1);
  fail_unless(d->model == m1, NULL);

  /* Reflexive case (pathological) */
  SBMLDocument_setModel(d, d->model);
  fail_unless(d->model == m1, NULL);

  SBMLDocument_setModel(d, m2);
  fail_unless(d->model == m2, NULL);

  SBMLDocument_free(d);
  /* m1 is freed by SBMLDocument_setModel(d, m2); */
}
END_TEST


START_TEST (test_SBMLDocument_validate)
{
  SBMLDocument_t *d;
  unsigned int errors;

  const char* s = wrapSBML
  (
    "<listOfCompartments>"
    "  <compartment name='c' spatialDimensions='0' size='1'>"
    "</listOfCompartments>"
  );

  d = readSBMLFromString(s);

  SBMLDocument_validate(d); 

  errors = SBMLDocument_getNumErrors(d);

  fail_unless(errors == 1, NULL);

  SBMLDocument_free(d);
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
  tcase_add_test(tcase, test_SBMLDocument_validate   );

  suite_add_tcase(suite, tcase);

  return suite;
}
