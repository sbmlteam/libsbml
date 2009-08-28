/**
 * \file    TestValidation.cpp
 * \brief   Validation of Date ModelCreator and ModelHistory unit tests
 * \author  Sarah Keating
 *
 * $Id: TestValidation.c 7758 2008-07-26 12:41:26Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/annotation/test/TestValidation.c $
 */
/* Copyright 2007 California Institute of Technology.
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


#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/annotation/RDFAnnotation.h>
#include <sbml/annotation/ModelHistory.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

CK_CPPSTART

START_TEST (test_Validation_Date1)
{
  Date * date = new Date(200, 12, 30, 12, 15, 45, 1, 2, 0);
  fail_unless(date != NULL);

  fail_unless (!(date->representsValidDate()));

  delete date;
}
END_TEST


START_TEST (test_Validation_Date2)
{
  Date * date = new Date(2007, 14, 30, 12, 15, 45, 1, 2, 0);
  fail_unless(date != NULL);

  fail_unless (!(date->representsValidDate()));

  delete date;
}
END_TEST


START_TEST (test_Validation_Date3)
{
  Date * date = new Date("Jan 12");
  fail_unless(date != NULL);

  fail_unless (!(date->representsValidDate()));

  delete date;
}
END_TEST


START_TEST (test_Validation_Date4)
{
  Date * date = new Date(2007, 12, 30, 12, 15, 45, 1, 2, 0);
  fail_unless(date != NULL);

  fail_unless (date->representsValidDate());

  delete date;
}
END_TEST


START_TEST (test_Validation_ModelCreator)
{
  ModelCreator * mc = new ModelCreator();
  fail_unless(mc != NULL);

  fail_unless (!(mc->hasRequiredAttributes()));

  mc->setEmail("k123");

  fail_unless (!(mc->hasRequiredAttributes()));

  mc->setFamilyName("Keating");

  fail_unless (!(mc->hasRequiredAttributes()));

  mc->setGivenName("Sarah");

  fail_unless (mc->hasRequiredAttributes());
  
  delete mc;
}
END_TEST


START_TEST (test_Validation_ModelHistory1)
{
  ModelHistory * mh = new ModelHistory();
  fail_unless(mh != NULL);

  fail_unless (!(mh->hasRequiredAttributes()));

  Date * date = new Date(2007, 12, 30, 12, 15, 45, 1, 2, 0);
  mh->setCreatedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  mh->setModifiedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setGivenName("Sarah");

  mh->addCreator(mc);

  fail_unless (mh->hasRequiredAttributes());
  
  delete mh;
}
END_TEST


START_TEST (test_Validation_ModelHistory2)
{
  ModelHistory * mh = new ModelHistory();
  fail_unless(mh != NULL);

  fail_unless (!(mh->hasRequiredAttributes()));

  Date * date = new Date(200, 12, 30, 12, 15, 45, 1, 2, 0);
  mh->setCreatedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  mh->setModifiedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setGivenName("Sarah");

  mh->addCreator(mc);

  fail_unless (!(mh->hasRequiredAttributes()));
  
  delete mh;
}
END_TEST


START_TEST (test_Validation_ModelHistory3)
{
  ModelHistory * mh = new ModelHistory();
  fail_unless(mh != NULL);

  fail_unless (!(mh->hasRequiredAttributes()));

  Date * date = new Date(2007, 12, 30, 12, 15, 45, 1, 2, 0);
  mh->setCreatedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  mh->setModifiedDate(date);

  fail_unless (!(mh->hasRequiredAttributes()));

  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");

  mh->addCreator(mc);

  fail_unless (!(mh->hasRequiredAttributes()));
  
  delete mh;
}
END_TEST


START_TEST (test_Validation_CVTerm1)
{
  CVTerm * cv = new CVTerm();
  fail_unless(cv != NULL);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->setQualifierType(MODEL_QUALIFIER);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->setModelQualifierType(BQM_IS);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->addResource("ggg");

  fail_unless ((cv->hasRequiredAttributes()));
  
  delete cv;
}
END_TEST


START_TEST (test_Validation_CVTerm2)
{
  CVTerm * cv = new CVTerm();
  fail_unless(cv != NULL);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->setQualifierType(BIOLOGICAL_QUALIFIER);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->setBiologicalQualifierType(BQB_IS);

  fail_unless (!(cv->hasRequiredAttributes()));

  cv->addResource("ggg");

  fail_unless ((cv->hasRequiredAttributes()));
  
  delete cv;
}
END_TEST


Suite *
create_suite_Validation (void)
{
  Suite *suite = suite_create("Validation");
  TCase *tcase = tcase_create("Validation");

  tcase_add_test(tcase, test_Validation_Date1 );
  tcase_add_test(tcase, test_Validation_Date2 );
  tcase_add_test(tcase, test_Validation_Date3 );
  tcase_add_test(tcase, test_Validation_Date4 );
  tcase_add_test(tcase, test_Validation_ModelCreator );
  tcase_add_test(tcase, test_Validation_ModelHistory1 );
  tcase_add_test(tcase, test_Validation_ModelHistory2 );
  tcase_add_test(tcase, test_Validation_ModelHistory3 );
  tcase_add_test(tcase, test_Validation_CVTerm1 );
  tcase_add_test(tcase, test_Validation_CVTerm2 );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
