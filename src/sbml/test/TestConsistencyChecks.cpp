/**
 * \file    TestConsistencyChecks.cpp
 * \brief   Reads test-data/inconsistent.xml into memory and tests it.
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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


#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>


BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_consistency_checks)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  unsigned int errors;
  std::string filename(TestDataDirectory);
  filename += "inconsistent.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent.xml\") returned a NULL pointer.");
  }

  errors = d->checkConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10301);

  d->getErrorLog()->clearLog();
  d->setConsistencyChecks(LIBSBML_CAT_IDENTIFIER_CONSISTENCY, false);
  errors = d->checkConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 20612);

  d->getErrorLog()->clearLog();
  d->setConsistencyChecks(LIBSBML_CAT_GENERAL_CONSISTENCY, false);
  errors = d->checkConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10701);

  d->getErrorLog()->clearLog();
  d->setConsistencyChecks(LIBSBML_CAT_SBO_CONSISTENCY, false);
  errors = d->checkConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10214);

  d->getErrorLog()->clearLog();
  d->setConsistencyChecks(LIBSBML_CAT_MATHML_CONSISTENCY, false);
  errors = d->checkConsistency();

  fail_unless(errors == 2);
  fail_unless(d->getError(0)->getErrorId() == 10523);
  fail_unless(d->getError(1)->getErrorId() == 99505);

  d->getErrorLog()->clearLog();
  d->setConsistencyChecks(LIBSBML_CAT_UNITS_CONSISTENCY, false);
  errors = d->checkConsistency();

  fail_unless(errors == 0);
 /* fail_unless(d->getError(0)->getErrorId() == 80701);*/

  delete d;
}
END_TEST


Suite *
create_suite_TestConsistencyChecks (void)
{ 
  Suite *suite = suite_create("ConsistencyChecks");
  TCase *tcase = tcase_create("ConsistencyChecks");


  tcase_add_test(tcase, test_consistency_checks);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
