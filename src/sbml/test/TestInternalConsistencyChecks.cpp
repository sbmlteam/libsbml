/**
 * \file    TestInternalConsistencyChecks.cpp
 * \brief   Tests the internal consistency validation.
 * \author  Sarah Keating
 *
 * $Id: TestConsistencyChecks.cpp 7249 2008-06-26 22:48:40Z mhucka $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/sbml/test/TestConsistencyChecks.cpp $
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



START_TEST (test_internal_consistency_check_99901)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setSpatialDimensions(2);
  m->addCompartment(c);

  errors = d->checkInternalConsistency();
  /* FIX_ME the const error occurs here because by default constant is
   * true - which it actually shouldnt be for l1 
   */
  fail_unless(errors == 2);
  fail_unless(d->getError(0)->getErrorId() == 99901);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99902)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setCompartmentType("hh");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();
  /* FIX_ME the const error occurs here because by default constant is
   * true - which it actually shouldnt be for l1 
   */
  fail_unless(errors == 2);
  fail_unless(d->getError(0)->getErrorId() == 99902);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99903)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setConstant(true);
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99903);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setMetaId("mmm");
  c->setConstant(false);
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  c->setConstant(false);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setSBOTerm(2);
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_ct)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  CompartmentType *ct = new CompartmentType();
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  ct->setSBOTerm(5);
  m->addCompartmentType(ct);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_delay)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Delay *delay = new Delay();
  Event *e = new Event();
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  delay->setSBOTerm(5);
  e->setDelay(delay);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99906)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  c->setConstant(false);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setUnits("mole");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99906);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99907)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment();
  c->setConstant(false);
  d->setLevelAndVersion(1, 1);
  Model *m = d->createModel();

  /* note - it is impossible to create the situation where a l1 model
   * has no volume set as the code doesnt let you !!!
   */
  c->unsetVolume();

  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99908)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  CompartmentType *ct = new CompartmentType();
  Model *m = d->createModel();

  d->setLevelAndVersion(2, 1);
  m->addCompartmentType(ct);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99908);


  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99909)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Constraint *ct = new Constraint();
  Model *m = d->createModel();

  d->setLevelAndVersion(2, 1);
  m->addConstraint(ct);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99909);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99910)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Event *e = new Event();
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Compartment *c = m->getCompartment(0);
  c->setConstant(false);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99910);

  delete d;
}
END_TEST


Suite *
create_suite_TestInternalConsistencyChecks (void)
{ 
  Suite *suite = suite_create("InternalConsistencyChecks");
  TCase *tcase = tcase_create("InternalConsistencyChecks");


  tcase_add_test(tcase, test_internal_consistency_check_99901);
  tcase_add_test(tcase, test_internal_consistency_check_99902);
  tcase_add_test(tcase, test_internal_consistency_check_99903);
  tcase_add_test(tcase, test_internal_consistency_check_99904);
  tcase_add_test(tcase, test_internal_consistency_check_99905);
  tcase_add_test(tcase, test_internal_consistency_check_99905_ct);
  tcase_add_test(tcase, test_internal_consistency_check_99905_delay);
  tcase_add_test(tcase, test_internal_consistency_check_99906);
  tcase_add_test(tcase, test_internal_consistency_check_99907);
  tcase_add_test(tcase, test_internal_consistency_check_99908);
  tcase_add_test(tcase, test_internal_consistency_check_99909);
  tcase_add_test(tcase, test_internal_consistency_check_99910);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
