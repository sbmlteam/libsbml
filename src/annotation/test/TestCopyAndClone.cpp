/**
 * \file    TestReadSBML.cpp
 * \brief   Read SBML unit tests
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


#include "ModelHistory.h"

#include <check.h>


using namespace std;

CK_CPPSTART
START_TEST ( test_Date_copyConstructor )
{
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
    
  fail_unless(date->getMonth() == 12);
  fail_unless(date->getSecond() == 45);

  Date* date2=new Date(*date);

  fail_unless(date2->getMonth() == 12);
  fail_unless(date2->getSecond() == 45);

  delete date2;
  delete date;
}
END_TEST

START_TEST ( test_Date_assignmentOperator )
{
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
    
  fail_unless(date->getMonth() == 12);
  fail_unless(date->getSecond() == 45);

  Date* date2=new Date();
  (*date2) = *date;

  fail_unless(date2->getMonth() == 12);
  fail_unless(date2->getSecond() == 45);

  delete date2;
  delete date;
    
}
END_TEST


START_TEST ( test_Date_clone )
{
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
    
  fail_unless(date->getMonth() == 12);
  fail_unless(date->getSecond() == 45);

  Date* date2 = static_cast<Date*>(date->clone());

  fail_unless(date2->getMonth() == 12);
  fail_unless(date2->getSecond() == 45);

  delete date2;
  delete date;

}
END_TEST


START_TEST ( test_ModelCreator_copyConstructor )
{
  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");
    
  fail_unless(mc->getFamilyName() == "Keating");
  fail_unless(mc->getEmail() == "sbml-team@caltech.edu");

  ModelCreator* mc2=new ModelCreator(*mc);

  fail_unless(mc2->getFamilyName() == "Keating");
  fail_unless(mc2->getEmail() == "sbml-team@caltech.edu");

  delete mc2;
  delete mc;
}
END_TEST

START_TEST ( test_ModelCreator_assignmentOperator )
{
  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");
    
  fail_unless(mc->getFamilyName() == "Keating");
  fail_unless(mc->getEmail() == "sbml-team@caltech.edu");

  ModelCreator* mc2=new ModelCreator();
  (*mc2) = *mc;

  fail_unless(mc2->getFamilyName() == "Keating");
  fail_unless(mc2->getEmail() == "sbml-team@caltech.edu");

  delete mc2;
  delete mc;
    
}
END_TEST


START_TEST ( test_ModelCreator_clone )
{
  ModelCreator * mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");
    
  fail_unless(mc->getFamilyName() == "Keating");
  fail_unless(mc->getEmail() == "sbml-team@caltech.edu");

  ModelCreator* mc2 = static_cast<ModelCreator*>(mc->clone());

  fail_unless(mc2->getFamilyName() == "Keating");
  fail_unless(mc2->getEmail() == "sbml-team@caltech.edu");

  delete mc2;
  delete mc;

}
END_TEST

START_TEST ( test_ModelHistory_copyConstructor )
{
  ModelHistory * mh = new ModelHistory();
  
  ModelCreator *mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");

  mh->addCreator(mc);
  delete mc;
    
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  
  mh->setCreatedDate(date);
  delete date;

  fail_unless(mh->getCreatedDate()->getMonth() == 12);
  fail_unless(mh->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh->getCreator()->get(0))->getFamilyName() == "Keating");

  ModelHistory* mh2=new ModelHistory(*mh);

  fail_unless(mh2->getCreatedDate()->getMonth() == 12);
  fail_unless(mh2->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh2->getCreator()->get(0))->getFamilyName() == "Keating");

  delete mh2;
  delete mh;
}
END_TEST

START_TEST ( test_ModelHistory_assignmentOperator )
{
  ModelHistory * mh = new ModelHistory();
  
  ModelCreator *mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");

  mh->addCreator(mc);
  delete mc;
    
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  
  mh->setCreatedDate(date);
  delete date;

  fail_unless(mh->getCreatedDate()->getMonth() == 12);
  fail_unless(mh->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh->getCreator()->get(0))->getFamilyName() == "Keating");

  ModelHistory* mh2=new ModelHistory();
  (*mh2) = *mh;

  fail_unless(mh2->getCreatedDate()->getMonth() == 12);
  fail_unless(mh2->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh2->getCreator()->get(0))->getFamilyName() == "Keating");

  delete mh2;
  delete mh;
    
}
END_TEST


START_TEST ( test_ModelHistory_clone )
{
  ModelHistory * mh = new ModelHistory();
  
  ModelCreator *mc = new ModelCreator();
  mc->setFamilyName("Keating");
  mc->setEmail("sbml-team@caltech.edu");

  mh->addCreator(mc);
  delete mc;
    
  Date * date = new Date(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  
  mh->setCreatedDate(date);
  delete date;

  fail_unless(mh->getCreatedDate()->getMonth() == 12);
  fail_unless(mh->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh->getCreator()->get(0))->getFamilyName() == "Keating");

  ModelHistory* mh2 = static_cast<ModelHistory*>(mh->clone());

  fail_unless(mh2->getCreatedDate()->getMonth() == 12);
  fail_unless(mh2->getCreatedDate()->getSecond() == 45);

  fail_unless(static_cast <ModelCreator*>(mh2->getCreator()->get(0))->getFamilyName() == "Keating");

  delete mh2;
  delete mh;

}
END_TEST


Suite *
create_suite_CopyAndClone (void)
{
  Suite *suite = suite_create("CopyAndClone");
  TCase *tcase = tcase_create("CopyAndClone");

  tcase_add_test( tcase, test_Date_copyConstructor );
  tcase_add_test( tcase, test_Date_assignmentOperator );
  tcase_add_test( tcase, test_Date_clone );
  tcase_add_test( tcase, test_ModelCreator_copyConstructor );
  tcase_add_test( tcase, test_ModelCreator_assignmentOperator );
  tcase_add_test( tcase, test_ModelCreator_clone );
  tcase_add_test( tcase, test_ModelHistory_copyConstructor );
  tcase_add_test( tcase, test_ModelHistory_assignmentOperator );
  tcase_add_test( tcase, test_ModelHistory_clone );
  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND
