/**
 * @file    TestXMLAttributesC.c
 * @brief   XMLAttributes unit tests, C version
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <stdio.h>
#include <check.h>
#include <XMLAttributes.h>


START_TEST (test_XMLAttributes_create_C)
{
  XMLAttributes_t *xa = XMLAttributes_create();

  XMLAttributes_add(xa, "double", "3.12");
  XMLAttributes_add(xa, "bool", "1");
  XMLAttributes_add(xa, "long", "23456543");
  XMLAttributes_add(xa, "int", "-12");
  XMLAttributes_add(xa, "uint", "33");

  double value = 0.0;
  int bint = 2;
  long longv = 278787878;
  int intv = -646464;
  unsigned int uintv = 99;

  fail_unless(value == 0.0);
  fail_unless(bint == 2);
  fail_unless(longv == 278787878);
  fail_unless(intv == -646464);
  fail_unless(uintv == 99);

  XMLAttributes_readIntoDouble(xa, "double", &value, NULL, 0);
  XMLAttributes_readIntoBoolean(xa, "bool", &bint, NULL, 0);
  XMLAttributes_readIntoLong(xa, "long", &longv, NULL, 0);
  XMLAttributes_readIntoInt(xa, "int", &intv, NULL, 0);
  XMLAttributes_readIntoUnsignedInt(xa, "uint", &uintv, NULL, 0);

  fail_unless(value == 3.12);
  fail_unless(bint == 1);
  fail_unless(longv == 23456543);
  fail_unless(intv == -12);
  fail_unless(uintv == 33);

  XMLAttributes_free(xa);
}
END_TEST

Suite *
create_suite_XMLAttributes_C (void)
{
  Suite *suite = suite_create("XMLAttributesC");
  TCase *tcase = tcase_create("XMLAttributesC");

  tcase_add_test( tcase, test_XMLAttributes_create_C  );
  suite_add_tcase(suite, tcase);

  return suite;
}
