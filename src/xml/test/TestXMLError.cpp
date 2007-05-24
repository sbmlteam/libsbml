/**
 * @file    TestXMLError.cpp
 * @brief   XMLError unit tests, C++ version
 * @author  Michael Hucka
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

#include <limits>

#include <check.h>
#include <XMLError.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


CK_CPPSTART



START_TEST (test_XMLError_create)
{
  XMLError* error = new XMLError;
  fail_unless( error != 0 );
  delete error;

  error = new XMLError(XMLError::DuplicateAttribute);
  fail_unless( error->getId() == XMLError::DuplicateAttribute );
  fail_unless( error->getSeverity() == XMLError::Error );
  fail_unless( error->getCategory() == XMLError::XML );
  fail_unless( error->getMessage() == "Duplicate attribute" );
  delete error;

  error = new XMLError(12345, 0, 0, "My message");
  fail_unless( error->getId() == 12345 );
  fail_unless( error->getMessage() == "My message" );
  fail_unless( error->getSeverity() == XMLError::Fatal );
  fail_unless( error->getCategory() == XMLError::Internal );
  delete error;

  error = new XMLError(12345, 0, 0, "My message", XMLError::Info, XMLError::System);
  fail_unless( error->getId() == 12345 );
  fail_unless( error->getMessage() == "My message" );
  fail_unless( error->getSeverity() == XMLError::Info );
  fail_unless( error->getCategory() == XMLError::System );
  fail_unless( error->isInfo() );
  fail_unless( error->isSystem() );
  delete error;

  error = new XMLError(10000, 0, 0, "Another message", XMLError::Fatal, XMLError::SBML);
  fail_unless( error->getId() == 10000 );
  fail_unless( error->getMessage() == "Another message" );
  fail_unless( error->getSeverity() == XMLError::Fatal );
  fail_unless( error->getCategory() == XMLError::SBML );
  fail_unless( error->isFatal() );
  fail_unless( error->isSBML() );
  delete error;
}
END_TEST


Suite *
create_suite_XMLError (void)
{
  Suite *suite = suite_create("XMLError");
  TCase *tcase = tcase_create("XMLError");

  tcase_add_test( tcase, test_XMLError_create  );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
