/**
 * \file    TestReadFromFileL3V2_2.cpp
 * \brief   Reads test-data/l3v2-empty-lo-2.xml into memory and tests it.
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



START_TEST (test_read_l3v2_empty_math)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  std::string filename(TestDataDirectory);
  filename += "l3v2-empty-math.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-empty-lo-3.xml\") returned a NULL pointer.");
  }



  //
  // <sbml level="3" version="2" ...>
  //
  fail_unless( d->getLevel  () == 3, NULL );
  fail_unless( d->getVersion() == 2, NULL );


  m = d->getModel();
  fail_unless( m != NULL, NULL );

  fail_unless(m->getNumUnitDefinitions() == 0);
  fail_unless(m->getNumFunctionDefinitions() == 0);
  fail_unless(m->getNumCompartments() == 1);
  fail_unless(m->getNumSpecies() == 1);
  fail_unless(m->getNumParameters() == 1);
  fail_unless(m->getNumRules() == 1);
  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(m->getNumConstraints() == 0);
  fail_unless(m->getNumReactions() == 1);
  fail_unless(m->getNumEvents() == 0);

  InitialAssignment * ia = m->getInitialAssignment(0);

  fail_unless( ia != NULL);
  fail_unless( ia->isSetMath() == false);



  Reaction * r = m->getReaction(0);

  fail_unless( r != NULL);
  
  fail_unless( r->getNumReactants() == 1);

  KineticLaw * kl = r->getKineticLaw();

  fail_unless( kl != NULL);
  fail_unless( kl->isSetMath() == false);


  delete d;
}
END_TEST

START_TEST(test_echo_l3v2_empty_math)
{
  const char *expected = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
    "level=\"3\" version=\"2\">\n" 
    "  <model substanceUnits=\"mole\" timeUnits=\"second\" volumeUnits=\"litre\" "
        "areaUnits=\"metre\" lengthUnits=\"metre\" extentUnits=\"mole\" "
        "conversionFactor=\"p\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"c\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"s\" compartment=\"c\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" units=\"dimensionless\" constant=\"true\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfInitialAssignments>\n"
    "      <initialAssignment symbol=\"p\"/>\n"
    "    </listOfInitialAssignments>\n"
    "    <listOfRules>\n"
    "      <algebraicRule/>\n"
    "    </listOfRules>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"r\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"s\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <kineticLaw/>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n";

  std::string filename(TestDataDirectory);
  filename += "l3v2-empty-math.xml";

  std::string fileout(TestDataDirectory);
  fileout += "tempOut.xml";

  SBMLDocument* d = readSBML(filename.c_str());

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-empty-math.xml\") returned a NULL pointer.");
  }

  writeSBML(d, fileout.c_str());

  SBMLDocument* outD = readSBML(fileout.c_str());

  Model * m = outD->getModel();
  fail_unless( m != NULL, NULL );

  fail_unless(m->getNumUnitDefinitions() == 0);
  fail_unless(m->getNumFunctionDefinitions() == 0);
  fail_unless(m->getNumCompartments() == 1);
  fail_unless(m->getNumSpecies() == 1);
  fail_unless(m->getNumParameters() == 1);
  fail_unless(m->getNumRules() == 1);
  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(m->getNumConstraints() == 0);
  fail_unless(m->getNumReactions() == 1);
  fail_unless(m->getNumEvents() == 0);

  InitialAssignment *ia = m->getInitialAssignment(0);

  fail_unless( ia != NULL);
  fail_unless( ia->isSetMath() == false);

  Reaction * r = m->getReaction(0);

  fail_unless( r != NULL);
  
  KineticLaw * kl = r->getKineticLaw();

  fail_unless( kl != NULL);
  fail_unless( kl->isSetMath() == false);

  char * S = writeSBMLToString(outD);

  fail_unless( equals(expected, S) );

  safe_free(S);

  delete d;
  delete outD;
}
END_TEST


Suite *
create_suite_TestReadFromFileL3V2_4 (void)
{ 
  Suite *suite = suite_create("test-data/l3v2-empty-math.xml");
  TCase *tcase = tcase_create("test-data/l3v2-empty-math.xml");


  tcase_add_test(tcase, test_read_l3v2_empty_math);
  tcase_add_test(tcase, test_echo_l3v2_empty_math);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
