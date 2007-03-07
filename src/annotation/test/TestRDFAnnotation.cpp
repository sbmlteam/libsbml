/**
 * \file    TestRDFAnnotation.cpp
 * \brief   fomula units data unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include "sbml/common/common.h"
#include "sbml/common/extern.h"

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"
#include "sbml/SBMLTypeCodes.h"

#include "RDFAnnotation.h"
#include "ModelHistory.h"

#include <check.h>

extern char *TestDataDirectory;

static UnitFormulaFormatter *uff;
static Model *m;
static SBMLDocument* d;

/* 
 * tests the results from rdf annotations
 */


void
RDFAnnotation_setup (void)
{
  d = new SBMLDocument();
 
  char *filename = safe_strcat(TestDataDirectory, "annotation.xml");


  d = readSBML(filename);
  m = d->getModel();


}


void
RDFAnnotation_teardown (void)
{
  delete d;
}
CK_CPPSTART

START_TEST (test_RDFAnnotation_getModelHistory)
{
  ModelHistory * history = m->getModelHistory();

  fail_unless(history != NULL);

  ModelCreator * mc = (ModelCreator * )(history->getCreator()->get(0));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Le Novere"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Nicolas"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "lenov@ebi.ac.uk"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "EMBL-EBI"));

  Date * date = history->getCreatedDate();
  fail_unless(Date_getYear(date) == 2006);
  fail_unless(Date_getMonth(date) == 2);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2005-02-02T14:56:11Z"));
}
END_TEST


Suite *
create_suite_RDFAnnotation (void)
{
  Suite *suite = suite_create("RDFAnnotation");
  TCase *tcase = tcase_create("RDFAnnotation");

  tcase_add_checked_fixture(tcase,
                            RDFAnnotation_setup,
                            RDFAnnotation_teardown);

  tcase_add_test(tcase, test_RDFAnnotation_getModelHistory );
  //tcase_add_test(tcase, test_RDFAnnotation_getcompartment );
  //tcase_add_test(tcase, test_RDFAnnotation_getspecies );
  //tcase_add_test(tcase, test_RDFAnnotation_getparameter );
  //tcase_add_test(tcase, test_RDFAnnotation_getinitialassignment );
  //tcase_add_test(tcase, test_RDFAnnotation_getrule );
  //tcase_add_test(tcase, test_RDFAnnotation_getreaction );
  //tcase_add_test(tcase, test_RDFAnnotation_getevent );
  //tcase_add_test(tcase, test_RDFAnnotation_getById );
  //tcase_add_test(tcase, test_RDFAnnotation_setters );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
