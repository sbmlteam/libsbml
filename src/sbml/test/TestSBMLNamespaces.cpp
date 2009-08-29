/**
 * @file    TestSBMLnamespaces.cpp
 * @brief   SBMLNamespaces unit tests
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
*/


#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNamespaces.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE


BEGIN_C_DECLS


START_TEST (test_SBMLNamespaces_L1V1)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(1, 1);

  fail_unless( sbml->getLevel() == 1 );
  fail_unless( sbml->getVersion() == 1 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level1");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_L1V2)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(1, 2);

  fail_unless( sbml->getLevel() == 1 );
  fail_unless( sbml->getVersion() == 2 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level1");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_L2V1)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(2, 1);

  fail_unless( sbml->getLevel() == 2 );
  fail_unless( sbml->getVersion() == 1 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level2");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_L2V2)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(2, 2);

  fail_unless( sbml->getLevel() == 2 );
  fail_unless( sbml->getVersion() == 2 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level2/version2");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_L2V3)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(2, 3);

  fail_unless( sbml->getLevel() == 2 );
  fail_unless( sbml->getVersion() == 3 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level2/version3");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_L2V4)
{
  SBMLNamespaces *sbml = new SBMLNamespaces(2, 4);

  fail_unless( sbml->getLevel() == 2 );
  fail_unless( sbml->getVersion() == 4 );

  XMLNamespaces * ns = sbml->getNamespaces();

  fail_unless(ns->getLength() == 1);
  fail_unless(ns->getURI(0) == "http://www.sbml.org/sbml/level2/version4");
  fail_unless(ns->getPrefix(0) == "sbml");

  delete sbml;
}
END_TEST


START_TEST (test_SBMLNamespaces_getURI)
{
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(1, 1) == 
                            "http://www.sbml.org/sbml/level1");
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(1, 2) == 
                            "http://www.sbml.org/sbml/level1");
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(2, 1) == 
                            "http://www.sbml.org/sbml/level2");
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(2, 2) == 
                            "http://www.sbml.org/sbml/level2/version2");
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(2, 3) == 
                            "http://www.sbml.org/sbml/level2/version3");
  fail_unless( SBMLNamespaces::getSBMLNamespaceURI(2, 4) == 
                            "http://www.sbml.org/sbml/level2/version4");
}
END_TEST


Suite *
create_suite_SBMLNamespaces (void)
{
  Suite *suite = suite_create("SBMLNamespaces");
  TCase *tcase = tcase_create("SBMLNamespaces");


  tcase_add_test(tcase, test_SBMLNamespaces_L1V1);
  tcase_add_test(tcase, test_SBMLNamespaces_L1V2);
  tcase_add_test(tcase, test_SBMLNamespaces_L2V1);
  tcase_add_test(tcase, test_SBMLNamespaces_L2V2);
  tcase_add_test(tcase, test_SBMLNamespaces_L2V3);
  tcase_add_test(tcase, test_SBMLNamespaces_L2V4);
  tcase_add_test(tcase, test_SBMLNamespaces_getURI);


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
