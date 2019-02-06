/**
 * \file    TestReadFromFileL3V2_1.cpp
 * \brief   Reads test-data/l3v2-no-model.xml into memory and tests it.
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

#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}




START_TEST (test_read_l3v2_no_model)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  std::string filename(TestDataDirectory);
  filename += "l3v2-no-model.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-no-model.xml\") returned a NULL pointer.");
  }



  //
  // <sbml level="3" version="2" ...>
  //
  fail_unless( d->getLevel  () == 3, NULL );
  fail_unless( d->getVersion() == 2, NULL );


  m = d->getModel();
  fail_unless( m == NULL, NULL );

  delete d;
}
END_TEST


START_TEST(test_echo_l3v2_no_model)
{
  const char *expected = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
    "level=\"3\" version=\"2\"/>\n"; 
  
  std::string filename(TestDataDirectory);
  filename += "l3v2-no-model.xml";

  std::string fileout(TestDataDirectory);
  fileout += "tempOut.xml";

  SBMLDocument* d = readSBML(filename.c_str());

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-no-model.xml\") returned a NULL pointer.");
  }

  writeSBML(d, fileout.c_str());

  SBMLDocument* outD = readSBML(fileout.c_str());

  fail_unless(d->getModel() == NULL);
  fail_unless(outD->getModel() == NULL);

  char * S = writeSBMLToString(outD);

  fail_unless( equals(expected, S) );

  safe_free(S);

  delete d;
  delete outD;


}
END_TEST

Suite *
create_suite_TestReadFromFileL3V2_1 (void)
{ 
  Suite *suite = suite_create("test-data/l3v2-no-model.xml");
  TCase *tcase = tcase_create("test-data/l3v2-no-model.xml");


  tcase_add_test(tcase, test_read_l3v2_no_model);
  tcase_add_test(tcase, test_echo_l3v2_no_model);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
