/**
 * \file    TestXMLAttributes.cpp
 * \brief   TestXMLAttributes unit tests
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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


#include <limits>

#include <iostream>
#include <check.h>
#include <XMLAttributes.h>
#include <XMLError.h>
#include <XMLNamespaces.h>
#include <XMLNode.h>
#include <XMLToken.h>
#include <XMLInputStream.h>
#include <sbml/xml/XMLConstructorException.h>

#include <string>


/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */
static const string errMsg = "NULL reference in XML constructor";
static const string errMsg1 = "Null argument to copy constructor";
static const string errMsg2 = "Null argument to assignment operator";
static const string errMsg3 = "Null argument given to constructor";


CK_CPPSTART


START_TEST ( test_XMLInputStream )
{
  const char *stream = NULL;
  
  // XMLInputStream no longer throws an exception when invoked with 
  // NULL stream, instead it produces an invald stream;

  XMLInputStream opstream2(stream);
  fail_unless(opstream2.isError());

}
END_TEST


Suite *
create_suite_XMLExceptions (void)
{
  Suite *suite = suite_create("XMLExceptions");
  TCase *tcase = tcase_create("XMLExceptions");

  tcase_add_test( tcase, test_XMLInputStream);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
