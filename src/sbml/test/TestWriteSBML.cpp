/**
 * \file    TestWriteSBML.cpp
 * \brief   Write SBML unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
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


#include <iostream>
#include <sstream>

#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/util/util.h>

#include <sbml/SBMLTypes.h>
#include <sbml/SBMLWriter.h>

#include <check.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L1       "xmlns=\"http://www.sbml.org/sbml/level1\" "
#define NS_L2v1     "xmlns=\"http://www.sbml.org/sbml/level2\" "
#define NS_L2v2     "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
#define NS_L2v3     "xmlns=\"http://www.sbml.org/sbml/level2/version3\" "
#define LV_L1v1     "level=\"1\" version=\"1\">\n"
#define LV_L1v2     "level=\"1\" version=\"2\">\n"
#define LV_L2v1     "level=\"2\" version=\"1\">\n"
#define LV_L2v2     "level=\"2\" version=\"2\">\n"
#define LV_L2v3     "level=\"2\" version=\"3\">\n"
#define SBML_END    "</sbml>\n"

#define wrapXML(s)        XML_START s
#define wrapSBML_L1v1(s)  XML_START SBML_START NS_L1   LV_L1v1 s SBML_END
#define wrapSBML_L1v2(s)  XML_START SBML_START NS_L1   LV_L1v2 s SBML_END
#define wrapSBML_L2v1(s)  XML_START SBML_START NS_L2v1 LV_L2v1 s SBML_END
#define wrapSBML_L2v2(s)  XML_START SBML_START NS_L2v2 LV_L2v2 s SBML_END
#define wrapSBML_L2v3(s)  XML_START SBML_START NS_L2v3 LV_L2v3 s SBML_END


static SBMLDocument* D;
static char*         S;

ostringstream*   OSS;
XMLOutputStream* XOS;


static void
WriteSBML_setup ()
{
  D = new SBMLDocument;
  S = 0;

  OSS = new ostringstream;
  XOS = new XMLOutputStream(*OSS);
}


static void
WriteSBML_teardown ()
{
  delete D;
  free(S);

  delete OSS;
  delete XOS;
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


static bool
equals (const char* expected)
{
  return equals(expected, OSS->str().c_str());
}


CK_CPPSTART


START_TEST (test_WriteSBML_error)
{
  SBMLDocument *d = new SBMLDocument();
  SBMLWriter   *w = new SBMLWriter();

  fail_unless( ! w->writeSBML(d, "/tmp/impossible/path/should/fail") );
  fail_unless( d->getNumErrors() == 1 );
  fail_unless( d->getError(0)->getErrorId() == XMLFileUnwritable );

  delete d;
  delete w;
}
END_TEST



START_TEST (test_WriteSBML_SBMLDocument_L1v1)
{
  D->setLevelAndVersion(1, 1);

  const char *expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" "
    "level=\"1\" version=\"1\"/>\n"
  );


  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_SBMLDocument_L1v2)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" "
    "level=\"1\" version=\"2\"/>\n"
  );


  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_SBMLDocument_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\"/>\n"
  );


  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_SBMLDocument_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
    "level=\"2\" version=\"2\"/>\n"
  );


  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST



START_TEST (test_WriteSBML_Model)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapSBML_L1v1("  <model name=\"Branch\"/>\n");


  D->createModel("Branch");
  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_Model_skipOptional)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapSBML_L1v2("  <model/>\n");


  D->createModel();
  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_Model_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapSBML_L2v1("  <model id=\"Branch\"/>\n");


  D->createModel("Branch");
  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_Model_L2v1_skipOptional)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapSBML_L2v1("  <model/>\n");


  D->createModel();
  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_Model_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapSBML_L2v2("  <model id=\"Branch\" sboTerm=\"SBO:0000004\"/>\n");

  Model * m = D->createModel("Branch");
  m->setSBOTerm(4);

  S = writeSBMLToString(D);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_WriteSBML_FunctionDefinition)
{
  const char* expected = wrapXML
  (
    "<functionDefinition id=\"pow3\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <lambda>\n"
    "      <bvar>\n"
    "        <ci> x </ci>\n"
    "      </bvar>\n"
    "      <apply>\n"
    "        <power/>\n"
    "        <ci> x </ci>\n"
    "        <cn type=\"integer\"> 3 </cn>\n"
    "      </apply>\n"
    "    </lambda>\n"
    "  </math>\n"
    "</functionDefinition>"
  );

  FunctionDefinition fd("pow3", "lambda(x, x^3)");
  fd.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_FunctionDefinition_withSBO)
{
  const char* expected = wrapXML
  (
  "<functionDefinition id=\"pow3\" sboTerm=\"SBO:0000064\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <lambda>\n"
    "      <bvar>\n"
    "        <ci> x </ci>\n"
    "      </bvar>\n"
    "      <apply>\n"
    "        <power/>\n"
    "        <ci> x </ci>\n"
    "        <cn type=\"integer\"> 3 </cn>\n"
    "      </apply>\n"
    "    </lambda>\n"
    "  </math>\n"
    "</functionDefinition>"
  );

  FunctionDefinition fd("pow3", "lambda(x, x^3)");
  fd.setSBOTerm(64);
  fd.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Unit)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>"
  );



  Unit u("kilogram", 2, -3);

  u.setSBMLDocument(D);
  u.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Unit_l2v3)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML
  (
    "<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>"
  );



  Unit u("kilogram", 2, -3);
  u.setOffset(32);

  u.setSBMLDocument(D);
  u.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Unit_defaults)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<unit kind=\"kilogram\"/>");


  Unit u("kilogram", 1, 0);

  u.setSBMLDocument(D);
  u.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Unit_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>"
  );



  Unit u("Celsius", 1, 0, 1.8);
  u.setOffset(32);

  u.setSBMLDocument(D);
  u.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_UnitDefinition)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<unitDefinition name=\"mmls\"/>");



  UnitDefinition ud("mmls");

  ud.setSBMLDocument(D);
  ud.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_UnitDefinition_full)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<unitDefinition name=\"mmls\">\n"
    "  <listOfUnits>\n"
    "    <unit kind=\"mole\" scale=\"-3\"/>\n"
    "    <unit kind=\"liter\" exponent=\"-1\"/>\n"
    "    <unit kind=\"second\" exponent=\"-1\"/>\n"
    "  </listOfUnits>\n"
    "</unitDefinition>"
  );



  UnitDefinition ud("mmls");

  Unit u1("mole"  ,  1, -3);
  Unit u2("liter" , -1);
  Unit u3("second", -1);

  ud.addUnit( &u1 );
  ud.addUnit( &u2 );
  ud.addUnit( &u3 );

  ud.setSBMLDocument(D);
  ud.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_UnitDefinition_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML("<unitDefinition id=\"mmls\"/>");


  UnitDefinition ud("mmls");

  ud.setSBMLDocument(D);
  ud.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST



START_TEST (test_WriteSBML_UnitDefinition_L2v1_full)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<unitDefinition id=\"Fahrenheit\">\n"
    "  <listOfUnits>\n"
    "    <unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n"
    "  </listOfUnits>\n"
    "</unitDefinition>"
  );

  Unit           u1("Celsius", 1, 0, 1.8);
  u1.setOffset(32);
  UnitDefinition ud("Fahrenheit");

  ud.addUnit(&u1);

  ud.setSBMLDocument(D);
  ud.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>"
  );


  Compartment c("A");

  c.setSize(2.1);
  c.setOutside("B");

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_unsetVolume)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<compartment name=\"A\"/>");


  Compartment c;


  c.setId("A");
  c.unsetVolume();

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<compartment id=\"M\" spatialDimensions=\"2\" size=\"2.5\"/>"
  );


  Compartment c("M");


  c.setSize(2.5);
  c.setSpatialDimensions(2);

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_L2v1_constant)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<compartment id=\"cell\" size=\"1.2\" constant=\"false\"/>"
  );

  Compartment c("cell");

  c.setSize(1.2);
  c.setConstant(false);

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_L2v1_unsetSize)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML("<compartment id=\"A\"/>");


  Compartment c;

  c.setId("A");
  c.unsetSize();

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_L2v2_compartmentType)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
    "<compartment id=\"cell\" compartmentType=\"ct\"/>"
  );

  Compartment c("cell");

  c.setCompartmentType("ct");

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Compartment_L2v3_SBO)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML
  (
  "<compartment id=\"cell\" sboTerm=\"SBO:0000005\"/>"
  );

  Compartment c("cell");

  c.setSBOTerm(5);

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>"
  );


  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);
  s.setUnits("mole");
  s.setBoundaryCondition(true);
  s.setCharge(2);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_L1v1)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<specie name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>"
  );


  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);
  s.setUnits("mole");
  s.setBoundaryCondition(true);
  s.setCharge(2);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_defaults)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" charge=\"2\"/>"
  );


  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);
  s.setUnits("mole");
  s.setCharge(2);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_skipOptional)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"/>"
  );


  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<species id=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\" "
    "substanceUnits=\"mole\" constant=\"true\"/>"
  );

  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);
  s.setSubstanceUnits("mole");
  s.setConstant(true);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_L2v1_skipOptional)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<species id=\"Ca2\" compartment=\"cell\"/>"
  );

  Species s("Ca2");
  s.setCompartment("cell");

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
    "<species id=\"Ca2\" speciesType=\"st\" compartment=\"cell\" initialAmount=\"0.7\" "
    "substanceUnits=\"mole\" constant=\"true\"/>"
  );

  Species s("Ca2");

  s.setCompartment("cell");
  s.setInitialAmount(0.7);
  s.setSubstanceUnits("mole");
  s.setConstant(true);
  s.setSpeciesType("st");

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Species_L2v3)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML
  (
  "<species id=\"Ca2\" compartment=\"cell\" sboTerm=\"SBO:0000007\"/>"
  );

  Species s("Ca2");

  s.setCompartment("cell");
  s.setSBOTerm(7);

  s.setSBMLDocument(D);
  s.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>"
  );


  Parameter p("Km1", 2.3, "second");

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L1v1_required)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<parameter name=\"Km1\" value=\"NaN\"/>"
  );

  Parameter p;

  p.setId("Km1");
  p.unsetValue();

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L1v2_skipOptional)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<parameter name=\"Km1\"/>");


  Parameter p;

  p.setId("Km1");
  p.unsetValue();

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<parameter id=\"Km1\" value=\"2.3\" units=\"second\"/>"
  );


  Parameter p("Km1", 2.3, "second");

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L2v1_skipOptional)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML("<parameter id=\"Km1\"/>");


  Parameter p("Km1");

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L2v1_constant)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML("<parameter id=\"x\" constant=\"false\"/>");


  Parameter p("x");

  p.setConstant(false);

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Parameter_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<parameter id=\"Km1\" value=\"2.3\" units=\"second\" sboTerm=\"SBO:0000002\"/>"
  );


  Parameter p("Km1", 2.3, "second");
  p.setSBOTerm(2);

  p.setSBMLDocument(D);
  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_AlgebraicRule)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML("<algebraicRule formula=\"x + 1\"/>");


  AlgebraicRule r("x + 1");

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_AlgebraicRule_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<algebraicRule>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> x </ci>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "    </apply>\n"
    "  </math>\n"
    "</algebraicRule>"
  );


  AlgebraicRule r("x + 1");

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_AlgebraicRule_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
    "<algebraicRule sboTerm=\"SBO:0000004\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> x </ci>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "    </apply>\n"
    "  </math>\n"
    "</algebraicRule>"
  );


  AlgebraicRule r("x + 1");
  r.setSBOTerm(4);

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesConcentrationRule)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<speciesConcentrationRule "
    "formula=\"t * s\" type=\"rate\" species=\"s\"/>"
  );


  D->createModel();
  D->getModel()->createSpecies()->setId("s");

  Rule* r = D->getModel()->createRateRule();

  r->setVariable("s");
  r->setFormula("t * s");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesConcentrationRule_defaults)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<speciesConcentrationRule formula=\"t * s\" species=\"s\"/>"
    
  );


  D->createModel();
  D->getModel()->createSpecies()->setId("s");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("s");
  r->setFormula("t * s");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesConcentrationRule_L1v1)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<specieConcentrationRule formula=\"t * s\" specie=\"s\"/>"
  );


  D->createModel();
  D->getModel()->createSpecies()->setId("s");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("s");
  r->setFormula("t * s");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesConcentrationRule_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<assignmentRule variable=\"s\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> t </ci>\n"
    "      <ci> s </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>"
  );


  D->createModel();
  D->getModel()->createSpecies()->setId("s");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("s");
  r->setFormula("t * s");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesConcentrationRule_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<assignmentRule variable=\"s\" sboTerm=\"SBO:0000006\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> t </ci>\n"
    "      <ci> s </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>"
  );


  D->createModel();
  D->getModel()->createSpecies()->setId("s");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("s");
  r->setFormula("t * s");
  r->setSBOTerm(6);
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentVolumeRule)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<compartmentVolumeRule "
    "formula=\"v + c\" type=\"rate\" compartment=\"c\"/>"
  );


  D->createModel();
  D->getModel()->createCompartment()->setId("c");

  Rule* r = D->getModel()->createRateRule();

  r->setVariable("c");
  r->setFormula("v + c");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentVolumeRule_defaults)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<compartmentVolumeRule formula=\"v + c\" compartment=\"c\"/>"
  );


  D->createModel();
  D->getModel()->createCompartment()->setId("c");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("c");
  r->setFormula("v + c");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentVolumeRule_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<assignmentRule variable=\"c\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> v </ci>\n"
    "      <ci> c </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>"
  );


  D->createModel();
  D->getModel()->createCompartment()->setId("c");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("c");
  r->setFormula("v + c");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentVolumeRule_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<assignmentRule variable=\"c\" sboTerm=\"SBO:0000005\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> v </ci>\n"
    "      <ci> c </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>"
  );


  D->createModel();
  D->getModel()->createCompartment()->setId("c");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("c");
  r->setFormula("v + c");
  r->setSBOTerm(5);
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_ParameterRule)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<parameterRule "
    "formula=\"p * t\" type=\"rate\" name=\"p\"/>"
  );


  D->createModel();
  D->getModel()->createParameter()->setId("p");

  Rule* r = D->getModel()->createRateRule();

  r->setVariable("p");
  r->setFormula("p * t");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_ParameterRule_defaults)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<parameterRule formula=\"p * t\" name=\"p\"/>"
  );


  D->createModel();
  D->getModel()->createParameter()->setId("p");

  Rule* r = D->getModel()->createAssignmentRule();

  r->setVariable("p");
  r->setFormula("p * t");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_ParameterRule_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<rateRule variable=\"p\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> p </ci>\n"
    "      <ci> t </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</rateRule>"
  );


  D->createModel();
  D->getModel()->createParameter()->setId("p");

  Rule* r = D->getModel()->createRateRule();

  r->setVariable("p");
  r->setFormula("p * t");
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_ParameterRule_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<rateRule variable=\"p\" sboTerm=\"SBO:0000007\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> p </ci>\n"
    "      <ci> t </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</rateRule>"
  );


  D->createModel();
  D->getModel()->createParameter()->setId("p");

  Rule* r = D->getModel()->createRateRule();

  r->setVariable("p");
  r->setFormula("p * t");
  r->setSBOTerm(7);
  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>"
  );


  Reaction r("r", "", NULL, false);
  r.setFast(true);

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction_defaults)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<reaction name=\"r\"/>");


  Reaction r;
  r.setId("r");

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction_full)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<reaction name=\"v1\">\n"
    "  <listOfReactants>\n"
    "    <speciesReference species=\"x0\"/>\n"
    "  </listOfReactants>\n"
    "  <listOfProducts>\n"
    "    <speciesReference species=\"s1\"/>\n"
    "  </listOfProducts>\n"
    "  <kineticLaw formula=\"(vm * s1)/(km + s1)\"/>\n"
    "</reaction>"
  );


  D->createModel();

  Reaction* r = D->getModel()->createReaction();

  r->setId("v1");
  r->setReversible(true);

  r->createReactant()->setSpecies("x0");
  r->createProduct ()->setSpecies("s1");

  r->createKineticLaw()->setFormula("(vm * s1)/(km + s1)");

  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<reaction id=\"r\" reversible=\"false\"/>"
  );


  Reaction r("r", "", NULL, false);

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction_L2v1_full)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<reaction id=\"v1\">\n"
    "  <listOfReactants>\n"
    "    <speciesReference species=\"x0\"/>\n"
    "  </listOfReactants>\n"
    "  <listOfProducts>\n"
    "    <speciesReference species=\"s1\"/>\n"
    "  </listOfProducts>\n"
    "  <listOfModifiers>\n"
    "    <modifierSpeciesReference species=\"m1\"/>\n"
    "  </listOfModifiers>\n"
    "  <kineticLaw>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <divide/>\n"
    "        <apply>\n"
    "          <times/>\n"
    "          <ci> vm </ci>\n"
    "          <ci> s1 </ci>\n"
    "        </apply>\n"
    "        <apply>\n"
    "          <plus/>\n"
    "          <ci> km </ci>\n"
    "          <ci> s1 </ci>\n"
    "        </apply>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </kineticLaw>\n"
    "</reaction>"
  );


  D->createModel();

  Reaction* r = D->getModel()->createReaction();

  r->setId("v1");
  r->setReversible(true);

  r->createReactant()->setSpecies("x0");
  r->createProduct ()->setSpecies("s1");
  r->createModifier()->setSpecies("m1");

  r->createKineticLaw()->setFormula("(vm * s1)/(km + s1)");

  r->write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Reaction_L2v2)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<reaction id=\"r\" name=\"r1\" reversible=\"false\" fast=\"true\" sboTerm=\"SBO:0000064\"/>"
  );


  Reaction r("r", "r1", NULL, false);
  r.setFast(true);
  r.setSBOTerm(64);

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<speciesReference species=\"s\" stoichiometry=\"3\" denominator=\"2\"/>"
  );


  SpeciesReference sr("s", 3, 2);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L1v1)
{
  D->setLevelAndVersion(1, 1);

  const char* expected = wrapXML
  (
    "<specieReference specie=\"s\" stoichiometry=\"3\" denominator=\"2\"/>"
  );


  SpeciesReference sr("s", 3, 2);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_defaults)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<speciesReference species=\"s\"/>");


  SpeciesReference sr("s");

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L2v1_1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<speciesReference species=\"s\">\n"
    "  <stoichiometryMath>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n"
    "    </math>\n"
    "  </stoichiometryMath>\n"
    "</speciesReference>"
  );


  SpeciesReference sr("s", 3, 2);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L2v1_2)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<speciesReference species=\"s\" stoichiometry=\"3.2\"/>"
  );


  SpeciesReference sr("s", 3.2);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L2v1_3)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<speciesReference species=\"s\">\n"
    "  <stoichiometryMath>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <divide/>\n"
    "        <cn type=\"integer\"> 1 </cn>\n"
    "        <ci> d </ci>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </stoichiometryMath>\n"
    "</speciesReference>"
  );


  SpeciesReference sr("s");
  ASTNode *math = SBML_parseFormula("1/d");
  StoichiometryMath *stoich = new StoichiometryMath();
  stoich->setMath(math);
  sr.setStoichiometryMath(stoich);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L2v2_1)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<speciesReference id=\"ss\" name=\"odd\" sboTerm=\"SBO:0000009\" species=\"s\">\n"
    "  <stoichiometryMath>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n"
    "    </math>\n"
    "  </stoichiometryMath>\n"
    "</speciesReference>"
  );


  SpeciesReference sr("s", 3, 2);
  sr.setId("ss");
  sr.setName("odd");
  sr.setSBOTerm(9);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_WriteSBML_SpeciesReference_L2v3_1)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML
  (
    "<speciesReference id=\"ss\" name=\"odd\" sboTerm=\"SBO:0000009\" species=\"s\" stoichiometry=\"3.2\"/>"
  );


  SpeciesReference sr("s", 3.2);
  sr.setId("ss");
  sr.setName("odd");
  sr.setSBOTerm(9);

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_WriteSBML_StoichiometryMath)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<stoichiometryMath>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <divide/>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "      <ci> d </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</stoichiometryMath>"
  );

  ASTNode *math = SBML_parseFormula("1/d");
  StoichiometryMath stoich = StoichiometryMath(math);

  stoich.setSBMLDocument(D);
  stoich.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_StoichiometryMath_withSBO)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML
  (
  "<stoichiometryMath sboTerm=\"SBO:0000333\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <divide/>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "      <ci> d </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</stoichiometryMath>"
  );

  ASTNode *math = SBML_parseFormula("1/d");
  StoichiometryMath stoich = StoichiometryMath(math);
  stoich.setSBOTerm(333);

  stoich.setSBMLDocument(D);
  stoich.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_KineticLaw)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<kineticLaw formula=\"k * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\"/>"
  );


  KineticLaw kl("k * e", "second", "item");

  kl.setSBMLDocument(D);
  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_KineticLaw_skipOptional)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML("<kineticLaw formula=\"k * e\"/>");


  KineticLaw kl("k * e");

  kl.setSBMLDocument(D);
  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_KineticLaw_ListOfParameters)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <listOfParameters>\n"
    "    <parameter name=\"n\" value=\"1.2\"/>\n"
    "  </listOfParameters>\n"
    "</kineticLaw>"
  );


  KineticLaw kl("nk * e", "second", "item");
  kl.setSBMLDocument(D);

  Parameter p("n", 1.2);
  kl.addParameter( &p );

  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_KineticLaw_l2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<kineticLaw timeUnits=\"second\" substanceUnits=\"item\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <divide/>\n"
    "      <apply>\n"
    "        <times/>\n"
    "        <ci> vm </ci>\n"
    "        <ci> s1 </ci>\n"
    "      </apply>\n"
    "      <apply>\n"
    "        <plus/>\n"
    "        <ci> km </ci>\n"
    "        <ci> s1 </ci>\n"
    "      </apply>\n"
    "    </apply>\n"
    "  </math>\n"
    "</kineticLaw>"
  );


  KineticLaw kl;
  kl.setTimeUnits("second");
  kl.setSubstanceUnits("item");
  kl.setFormula("(vm * s1)/(km + s1)");

  kl.setSBMLDocument(D);
  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_KineticLaw_withSBO)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML
  (
  "<kineticLaw sboTerm=\"SBO:0000001\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <divide/>\n"
    "      <apply>\n"
    "        <times/>\n"
    "        <ci> vm </ci>\n"
    "        <ci> s1 </ci>\n"
    "      </apply>\n"
    "      <apply>\n"
    "        <plus/>\n"
    "        <ci> km </ci>\n"
    "        <ci> s1 </ci>\n"
    "      </apply>\n"
    "    </apply>\n"
    "  </math>\n"
    "</kineticLaw>"
  );


  KineticLaw kl;
  kl.setFormula("(vm * s1)/(km + s1)");
  kl.setSBOTerm(1);

  kl.setSBMLDocument(D);
  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event)
{
  const char* expected = wrapXML("<event id=\"e\"/>");


  Event e;
  e.setId("e");
  
  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_WithSBO)
{
  const char* expected = wrapXML("<event id=\"e\" sboTerm=\"SBO:0000076\"/>");


  Event e;
  e.setId("e");
  e.setSBOTerm(76);
  
  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_WithNewAttributePlaceHolder)
{
  const char* expected = wrapXML("<event id=\"e\" newAttributePlaceHolder=\"false\"/>");
  D->setLevelAndVersion(2, 4);


  Event e;
  e.setId("e");
  e.setNewAttributePlaceHolder(false);
  
  e.setSBMLDocument(D);

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_trigger)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <trigger>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <leq/>\n"
    "        <ci> P1 </ci>\n"
    "        <ci> t </ci>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </trigger>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node = SBML_parseFormula("leq(P1,t)");
  Trigger t(node);
  e.setTrigger(&t);
  e.setTimeUnits("second");

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_delay)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <delay>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"integer\"> 5 </cn>\n"
    "    </math>\n"
    "  </delay>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node = SBML_parseFormula("5");
  Delay d(node);
  e.setDelay(&d);

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_delayWithSBO)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <delay sboTerm=\"SBO:0000064\">\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"integer\"> 5 </cn>\n"
    "    </math>\n"
    "  </delay>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node = SBML_parseFormula("5");
  Delay d(node);
  d.setSBOTerm(64);
  e.setDelay(&d);

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_trigger_withSBO)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <trigger sboTerm=\"SBO:0000064\">\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <leq/>\n"
    "        <ci> P1 </ci>\n"
    "        <ci> t </ci>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </trigger>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node = SBML_parseFormula("leq(P1,t)");
  Trigger t(node);
  t.setSBOTerm(64);

  e.setTrigger(&t);
  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_both)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <trigger>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <leq/>\n"
    "        <ci> P1 </ci>\n"
    "        <ci> t </ci>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </trigger>\n"
    "  <delay>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"integer\"> 5 </cn>\n"
    "    </math>\n"
    "  </delay>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node1 = SBML_parseFormula("leq(P1,t)");
  Trigger t(node1);
  ASTNode *node = SBML_parseFormula("5");
  Delay d(node);
  e.setDelay(&d);
  e.setTrigger(&t);
  e.setTimeUnits("second");

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_full)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\">\n"
    "  <trigger>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <apply>\n"
    "        <leq/>\n"
    "        <ci> P1 </ci>\n"
    "        <ci> t </ci>\n"
    "      </apply>\n"
    "    </math>\n"
    "  </trigger>\n"
    "  <listOfEventAssignments>\n"
    "    <eventAssignment variable=\"k2\" sboTerm=\"SBO:0000064\">\n"
    "      <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </math>\n"
    "    </eventAssignment>\n"
    "  </listOfEventAssignments>\n"
    "</event>"
  );

  Event e("e");
  ASTNode *node = SBML_parseFormula("leq(P1,t)");
  Trigger t(node);
  ASTNode *math = SBML_parseFormula("0");
  EventAssignment ea("k2", math);
  ea.setSBOTerm(64);

  e.setTrigger(&t);
  e.addEventAssignment( &ea );

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentType)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML("<compartmentType id=\"ct\"/>");


  CompartmentType ct;
  ct.setId("ct");
  ct.setSBOTerm(4);
  ct.setSBMLDocument(D);
  
  ct.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_CompartmentType_withSBO)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML("<compartmentType id=\"ct\" sboTerm=\"SBO:0000004\"/>");


  CompartmentType ct;
  ct.setId("ct");
  ct.setSBOTerm(4);
  ct.setSBMLDocument(D);
  
  ct.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesType)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML("<speciesType id=\"st\"/>");


  SpeciesType st;
  st.setId("st");
  st.setSBOTerm(4);
  st.setSBMLDocument(D);
  
  st.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_SpeciesType_withSBO)
{
  D->setLevelAndVersion(2, 3);

  const char* expected = wrapXML("<speciesType id=\"st\" sboTerm=\"SBO:0000004\"/>");


  SpeciesType st;
  st.setId("st");
  st.setSBOTerm(4);
  st.setSBMLDocument(D);
  
  st.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Constraint)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML("<constraint sboTerm=\"SBO:0000064\"/>");


  Constraint ct;
  ct.setSBOTerm(64);
  ct.setSBMLDocument(D);
  
  ct.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Constraint_math)
{
  const char* expected = wrapXML
  (
    "<constraint>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <leq/>\n"
    "      <ci> P1 </ci>\n"
    "      <ci> t </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</constraint>"
  );

  Constraint c;
  ASTNode *node = SBML_parseFormula("leq(P1,t)");
  c.setMath(node);

  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Constraint_full)
{
  const char* expected = wrapXML
  (
  "<constraint sboTerm=\"SBO:0000064\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <leq/>\n"
    "      <ci> P1 </ci>\n"
    "      <ci> t </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "  <message>\n"
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\"> Species P1 is out of range </p>\n"
    "  </message>\n"
    "</constraint>"
  );

  Constraint c;
  ASTNode *node = SBML_parseFormula("leq(P1,t)");
  c.setMath(node);
  c.setSBOTerm(64);

  const XMLNode *text = XMLNode::convertStringToXMLNode(" Species P1 is out of range ");
  XMLTriple triple = XMLTriple("p", "http://www.w3.org/1999/xhtml", "");
  XMLAttributes att = XMLAttributes();
  att.add("xmlns", "http://www.w3.org/1999/xhtml");
  
  XMLNode *p = new XMLNode(triple, att);
  p->addChild(*(text));
  
  XMLTriple triple1 = XMLTriple("message", "", "");
  XMLAttributes att1 = XMLAttributes();
  XMLNode *message = new XMLNode(triple1, att1);

  message->addChild(*(p));

  c.setMessage(message);

  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_InitialAssignment)
{
  D->setLevelAndVersion(2, 2);

  const char* expected = wrapXML("<initialAssignment symbol=\"c\" sboTerm=\"SBO:0000064\"/>");


  InitialAssignment ia;
  ia.setSBOTerm(64);
  ia.setSymbol("c");
  ia.setSBMLDocument(D);
  
  ia.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_InitialAssignment_math)
{
  const char* expected = wrapXML
  (
    "<initialAssignment symbol=\"c\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> a </ci>\n"
    "      <ci> b </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</initialAssignment>"
  );

  InitialAssignment ia;
  ASTNode *node = SBML_parseFormula("a + b");
  ia.setMath(node);
  ia.setSymbol("c");

  ia.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_NaN)
{
  const char* expected = wrapXML("<parameter id=\"p\" value=\"NaN\"/>");

  Parameter p("p", util_NaN());


  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_INF)
{
  const char* expected = wrapXML("<parameter id=\"p\" value=\"INF\"/>");

  Parameter p ("p", util_PosInf());

  p.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_WriteSBML_NegINF)
{
  const char* expected = wrapXML("<parameter id=\"p\" value=\"-INF\"/>");


  Parameter p("p", util_NegInf());

  p.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_locale)
{
  const char* expected = wrapXML("<parameter id=\"p\" value=\"3.31\"/>");

  Parameter p = Parameter("p", 3.31);


  setlocale(LC_NUMERIC, "de_DE");

  p.write(*XOS);

  fail_unless( equals(expected) );

  setlocale(LC_NUMERIC, "C");
}
END_TEST


START_TEST (test_WriteSBML_gzip)
{
  const unsigned int filenum = 12;
  const char* file[filenum] = {
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
                        };
  char* gzfile = "test.xml.gz";

  for(unsigned int i=0; i < filenum; i++)
  {
    SBMLDocument* d = readSBML(file[i]);
    fail_unless( d != NULL);

    if ( ! SBMLWriter::hasZlib() )
    {
      fail_unless( writeSBML(d, gzfile) == false);
      delete d;
      continue;
    }

    bool result = writeSBML(d, gzfile);
    fail_unless( result );

    SBMLDocument* dg = readSBML(gzfile);
    fail_unless( dg != NULL);

    fail_unless( strcmp(d->toSBML(), dg->toSBML()) == 0 );

    delete d;
    delete dg;
  }

}
END_TEST

START_TEST (test_WriteSBML_bzip2)
{
  const unsigned int filenum = 12;
  const char* file[filenum] = {
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
                        };

  char* bz2file = "test.xml.bz2";

  for(unsigned int i=0; i < filenum; i++)
  {
    SBMLDocument* d = readSBML(file[i]);
    fail_unless( d != NULL);

    if ( ! SBMLWriter::hasBzip2() )
    {
      fail_unless( writeSBML(d, bz2file) == false );
      delete d;
      continue;
    }

    bool result = writeSBML(d, bz2file);
    fail_unless( result );

    SBMLDocument* dg = readSBML(bz2file);
    fail_unless( dg != NULL);

    fail_unless( strcmp(d->toSBML(), dg->toSBML()) == 0 );

    delete d;
    delete dg;
  }
}
END_TEST

START_TEST (test_WriteSBML_zip)
{
  const unsigned int filenum = 12;
  const char* file[filenum] = {
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
                        };

  char* zipfile = "test.xml.zip";

  for(unsigned int i=0; i < filenum; i++)
  {
    SBMLDocument* d = readSBML(file[i]);
    fail_unless( d != NULL);

    if ( ! SBMLWriter::hasZlib() )
    {
      fail_unless( writeSBML(d, zipfile) == false );
      delete d;
      continue;
    }

    bool result = writeSBML (d, zipfile);
    fail_unless( result );

    SBMLDocument* dg = readSBML(zipfile);
    fail_unless( dg != NULL);

    fail_unless( strcmp(d->toSBML(), dg->toSBML()) == 0 );

    delete d;
    delete dg;
  }
}
END_TEST

Suite *
create_suite_WriteSBML ()
{
  Suite *suite = suite_create("WriteSBML");
  TCase *tcase = tcase_create("WriteSBML");


  tcase_add_checked_fixture(tcase, WriteSBML_setup, WriteSBML_teardown);
 
  // Basic writing capability
  tcase_add_test( tcase, test_WriteSBML_error );  

  // SBMLDocument
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_L1v1 );
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_L1v2 );
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_L2v1 );
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_L2v2 );


  // Model
  tcase_add_test( tcase, test_WriteSBML_Model                   );
  tcase_add_test( tcase, test_WriteSBML_Model_skipOptional      );
  tcase_add_test( tcase, test_WriteSBML_Model_L2v1              );
  tcase_add_test( tcase, test_WriteSBML_Model_L2v1_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Model_L2v2              );

  // FunctionDefinition
  tcase_add_test( tcase, test_WriteSBML_FunctionDefinition );
  tcase_add_test( tcase, test_WriteSBML_FunctionDefinition_withSBO );

  // Unit
  tcase_add_test( tcase, test_WriteSBML_Unit          );
  tcase_add_test( tcase, test_WriteSBML_Unit_defaults );
  tcase_add_test( tcase, test_WriteSBML_Unit_L2v1     );
  tcase_add_test( tcase, test_WriteSBML_Unit_l2v3     );

  // UnitDefinition
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition           );
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_full      );  
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_L2v1      );
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_L2v1_full );

  // Compartment
  tcase_add_test( tcase, test_WriteSBML_Compartment                );
  tcase_add_test( tcase, test_WriteSBML_Compartment_unsetVolume    );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1           );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1_constant  );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1_unsetSize );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v2_compartmentType  );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v3_SBO  );

  // Species
  tcase_add_test( tcase, test_WriteSBML_Species                   );
  tcase_add_test( tcase, test_WriteSBML_Species_L1v1              );
  tcase_add_test( tcase, test_WriteSBML_Species_defaults          );
  tcase_add_test( tcase, test_WriteSBML_Species_skipOptional      );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v1              );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v1_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v2              );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v3              );

  // Parameter
  tcase_add_test( tcase, test_WriteSBML_Parameter                   );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L1v1_required     );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L1v2_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1              );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1_constant     );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v2              );

  // AlgebraicRule
  tcase_add_test( tcase, test_WriteSBML_AlgebraicRule      );
  tcase_add_test( tcase, test_WriteSBML_AlgebraicRule_L2v1 );
  tcase_add_test( tcase, test_WriteSBML_AlgebraicRule_L2v2 );

  // SpeciesConcentrationRule
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule          );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_L1v1     );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_L2v1     );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_L2v2     );

  // CompartmentVolumeRule
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule          );
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule_L2v1     );
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule_L2v2     );

  // ParameterRule
  tcase_add_test( tcase, test_WriteSBML_ParameterRule          );
  tcase_add_test( tcase, test_WriteSBML_ParameterRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_ParameterRule_L2v1     );
  tcase_add_test( tcase, test_WriteSBML_ParameterRule_L2v2     );

  // Reaction
  tcase_add_test( tcase, test_WriteSBML_Reaction           );
  tcase_add_test( tcase, test_WriteSBML_Reaction_defaults  );
  tcase_add_test( tcase, test_WriteSBML_Reaction_full      );
  tcase_add_test( tcase, test_WriteSBML_Reaction_L2v1      );
  tcase_add_test( tcase, test_WriteSBML_Reaction_L2v1_full );
  tcase_add_test( tcase, test_WriteSBML_Reaction_L2v2      );

  // SpeciesReference

  tcase_add_test( tcase, test_WriteSBML_SpeciesReference          );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L1v1     );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_defaults );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_1   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_2   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_3   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v2_1   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v3_1   );

  // StoichiometryMath
  tcase_add_test( tcase, test_WriteSBML_StoichiometryMath   );
  tcase_add_test( tcase, test_WriteSBML_StoichiometryMath_withSBO   );

  // KineticLaw
  tcase_add_test( tcase, test_WriteSBML_KineticLaw                  );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_skipOptional     );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_ListOfParameters );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_l2v1                  );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_withSBO                  );

  // Event
  tcase_add_test( tcase, test_WriteSBML_Event         );
  tcase_add_test( tcase, test_WriteSBML_Event_WithSBO         );
  tcase_add_test( tcase, test_WriteSBML_Event_WithNewAttributePlaceHolder         );
  tcase_add_test( tcase, test_WriteSBML_Event_trigger );
  tcase_add_test( tcase, test_WriteSBML_Event_trigger_withSBO );
  tcase_add_test( tcase, test_WriteSBML_Event_delay   );
  tcase_add_test( tcase, test_WriteSBML_Event_delayWithSBO   );
  tcase_add_test( tcase, test_WriteSBML_Event_both    );
  tcase_add_test( tcase, test_WriteSBML_Event_full    );

  //CompartmentType
  tcase_add_test( tcase, test_WriteSBML_CompartmentType    );
  tcase_add_test( tcase, test_WriteSBML_CompartmentType_withSBO    );

  //SpeciesType
  tcase_add_test( tcase, test_WriteSBML_SpeciesType    );
  tcase_add_test( tcase, test_WriteSBML_SpeciesType_withSBO    );

  //Constraint
  tcase_add_test( tcase, test_WriteSBML_Constraint    );
  tcase_add_test( tcase, test_WriteSBML_Constraint_math    );
  tcase_add_test( tcase, test_WriteSBML_Constraint_full    );

  //InitialAssignment
  tcase_add_test( tcase, test_WriteSBML_InitialAssignment    );
  tcase_add_test( tcase, test_WriteSBML_InitialAssignment_math    );

  // Miscellaneous
  tcase_add_test( tcase, test_WriteSBML_NaN     );
  tcase_add_test( tcase, test_WriteSBML_INF     );
  tcase_add_test( tcase, test_WriteSBML_NegINF  );
  tcase_add_test( tcase, test_WriteSBML_locale  );

  // Compressed SBML
  tcase_add_test( tcase, test_WriteSBML_gzip  );
  tcase_add_test( tcase, test_WriteSBML_bzip2  );
  tcase_add_test( tcase, test_WriteSBML_zip  );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
