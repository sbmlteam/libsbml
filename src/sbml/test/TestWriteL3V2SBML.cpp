/**
 * \file    TestWriteL3V2SBML.cpp
 * \brief   Write SBML unit tests
 * \author  Sarah Keating
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

#include <iostream>
#include <sstream>

#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/util/util.h>

#include <sbml/SBMLTypes.h>
#include <sbml/SBMLWriter.h>

#include <check.h>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


#include <sbml/common/extern.h>

BEGIN_C_DECLS

extern char *TestDataDirectory;

/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L3v2     "xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
#define LV_L3v2     "level=\"3\" version=\"2\">\n"
#define SBML_END    "</sbml>\n"
#define NS_EXTRA    "xmlns:extra=\"http://www.sbml.org/sbml/level3/version1/extra/version1\" "
#define REQD_EXTRA  " extra:required=\"false\">\n"
#define LV_L3v2_noEnd     "level=\"3\" version=\"2\""

#define wrapXML(s)        XML_START s
#define wrapSBML_L3v2(s)  XML_START SBML_START NS_L3v2 LV_L3v2 s SBML_END
#define wrapSBML_extra(s)  XML_START SBML_START NS_L3v2 NS_EXTRA LV_L3v2_noEnd REQD_EXTRA s SBML_END


static SBMLDocument* D;


static void
WriteL3V2SBML_setup ()
{
  D = new SBMLDocument();
  D->setLevelAndVersion(3, 2, false);
}


static void
WriteL3V2SBML_teardown ()
{
  delete D;
}


static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


START_TEST (test_WriteL3V2SBML_SBMLDocument_L3v1)
{
  const char *expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
    "level=\"3\" version=\"2\"/>\n"
  );


  string S = writeSBMLToStdString(D);

  fail_unless( equals(expected, S.c_str()) );
}
END_TEST


START_TEST (test_WriteL3V2SBML_Model)
{
  const char* expected = wrapSBML_L3v2(
    "  <model/>\n"
    );

  Model * m = D->createModel("");
  (void) m;

  string S = writeSBMLToStdString(D);

  fail_unless( equals(expected, S.c_str()) );
}
END_TEST


START_TEST (test_WriteL3V2SBML_LO_ann)
{
  const char* expected = wrapSBML_L3v2(
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <annotation>test</annotation>\n"
    "    </listOfCompartments>\n"
    "  </model>\n"
    );

  Model * m = D->createModel("");
  XMLNode * ann = XMLNode::convertStringToXMLNode("test");
  ((SBase*)(m->getListOfCompartments()))->setAnnotation(ann);
  delete ann;

  string S = writeSBMLToStdString(D);

  fail_unless( equals(expected, S.c_str()) );
}
END_TEST


START_TEST (test_WriteL3V2SBML_emptyMath)
{
  const char* expected = wrapSBML_L3v2(
    "  <model>\n"
    "    <listOfInitialAssignments>\n"
    "      <initialAssignment/>\n"
    "    </listOfInitialAssignments>\n"
    "  </model>\n"
    );

  Model * m = D->createModel("");
  /*InitialAssignment *ia =*/ m->createInitialAssignment();

  string S = writeSBMLToStdString(D);

  fail_unless( equals(expected, S.c_str()) );
}
END_TEST


START_TEST (test_WriteL3V2SBML_roundtrip)
{
  const char* expected = wrapSBML_extra(
    "  <model>\n"
    "    <listOfInitialAssignments extra:foo=\"bar\"/>\n"
    "  </model>\n"
    );

  char *filename = safe_strcat(TestDataDirectory, "l3v2-extra.xml");


  SBMLDocument *d = readSBML(filename);
  string S = writeSBMLToStdString(d);
  delete d;
  safe_free(filename);

  fail_unless( equals(expected, S.c_str()) );
}
END_TEST


Suite *
create_suite_WriteL3V2SBML ()
{
  Suite *suite = suite_create("WriteL3V2SBML");
  TCase *tcase = tcase_create("WriteL3V2SBML");


  tcase_add_checked_fixture(tcase, WriteL3V2SBML_setup, WriteL3V2SBML_teardown);

  // SBMLDocument
  tcase_add_test( tcase, test_WriteL3V2SBML_SBMLDocument_L3v1 );
  // Model
  tcase_add_test( tcase, test_WriteL3V2SBML_Model                   );
  // empty lo with annotation
  tcase_add_test( tcase, test_WriteL3V2SBML_LO_ann                   );

  tcase_add_test( tcase, test_WriteL3V2SBML_emptyMath                );
  tcase_add_test( tcase, test_WriteL3V2SBML_roundtrip                );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
