/**
 * \file    TestAttributeFunctions.cpp
 * \brief   Attribute function tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE


/*
 * We create a lot of strings in this file, for testing, and we don't 
 * do what this warning tries to help with, so we shut it up just
 * for this file.
 */
#ifdef __GNUC__ 
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

BEGIN_C_DECLS

static SBase *S;


void
AttributeTest_setup (void)
{
  S = new(std::nothrow) Model(2, 4);

  if (S == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

}


void
AttributeTest_teardown (void)
{
  delete S;
}


START_TEST (test_Attributes_MetaId)
{
  const std::string& metaid = "x12345";
  std::string value;
  int result;

  result = S->setAttribute("metaid", metaid);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getMetaId() == metaid);

  fail_unless(S->isSetMetaId() == true);
  fail_unless(S->isSetAttribute("metaid") == true);

  result = S->getAttribute("metaid", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == metaid);

  result = S->unsetAttribute("metaid");

  fail_unless(S->isSetMetaId() == false);
  fail_unless(S->isSetAttribute("metaid") == false);

  result = S->getAttribute("metaid", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == "");

}
END_TEST


START_TEST (test_Attributes_Id)
{
  const std::string& id = "x12345";
  std::string value;
  int result;

  result = S->setAttribute("id", id);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getIdAttribute() == id);

  fail_unless(S->isSetIdAttribute() == true);
  fail_unless(S->isSetAttribute("id") == true);

  result = S->getAttribute("id", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == id);

  result = S->unsetAttribute("id");

  fail_unless(S->isSetIdAttribute() == false);
  fail_unless(S->isSetAttribute("id") == false);

  result = S->getAttribute("id", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == "");
}
END_TEST


START_TEST (test_Attributes_SBOTerm)
{
  int sboTerm = 5;
  int value;
  int result;

  result = S->setAttribute("sboTerm", sboTerm);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getSBOTerm() == sboTerm);

  fail_unless(S->isSetSBOTerm() == true);
  fail_unless(S->isSetAttribute("sboTerm") == true);

  result = S->getAttribute("sboTerm", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == sboTerm);

  result = S->unsetAttribute("sboTerm");

  fail_unless(S->isSetSBOTerm() == false);
  fail_unless(S->isSetAttribute("sboTerm") == false);

  result = S->getAttribute("sboTerm", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value ==-1);
}
END_TEST


Suite *
create_suite_Attributes (void)
{
  Suite *suite = suite_create("SBase");
  TCase *tcase = tcase_create("SBase");


  tcase_add_checked_fixture(tcase, AttributeTest_setup, AttributeTest_teardown);

  tcase_add_test(tcase, test_Attributes_MetaId     );
  tcase_add_test(tcase, test_Attributes_Id     );
  tcase_add_test(tcase, test_Attributes_SBOTerm     );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
