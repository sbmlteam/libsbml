/**
 * \file    TestWriteSBML.cpp
 * \brief   Write SBML unit tests
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


#include <iostream>
#include <sstream>
#include <check.h>

#include "xml/XMLOutputStream.h"
#include "util/util.h"

#include "SBMLTypes.h"
#include "SBMLWriter.h"


using namespace std;


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L1       "xmlns=\"http://www.sbml.org/sbml/level1\" "
#define NS_L2v1     "xmlns=\"http://www.sbml.org/sbml/level2\" "
#define NS_L2v2     "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
#define LV_L1v1     "level=\"1\" version=\"1\">\n"
#define LV_L1v2     "level=\"1\" version=\"2\">\n"
#define LV_L2v1     "level=\"2\" version=\"1\">\n"
#define LV_L2v2     "level=\"2\" version=\"2\">\n"
#define SBML_END    "</sbml>\n"

#define wrapXML(s)        XML_START s
#define wrapSBML_L1v1(s)  XML_START SBML_START NS_L1   LV_L1v1 s SBML_END
#define wrapSBML_L1v2(s)  XML_START SBML_START NS_L1   LV_L1v2 s SBML_END
#define wrapSBML_L2v1(s)  XML_START SBML_START NS_L2v1 LV_L2v1 s SBML_END
#define wrapSBML_L2v2(s)  XML_START SBML_START NS_L2v2 LV_L2v2 s SBML_END


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



  Unit u("Celsius", 1, 0, 1.8, 32);

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

  Unit           u1("Celsius", 1, 0, 1.8, 32);
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

/*
START_TEST (test_WriteSBML_Compartment_annotation)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<compartment name=\"A\" volume=\"2.1\" outside=\"B\">\n"
    "  <annotation xmlns:mysim=\"http://www.mysim.org/ns\">\n"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\"/>\n"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>\n"
    "</annotation>\n"
    "</compartment>"
  );

  const char* a =
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">\n"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\"/>\n"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>\n"
    "</annotation>";


  Compartment c("A");

  c.setVolume(2.1);
  c.setOutside("B");
  c.setAnnotation(a);

  c.setSBMLDocument(D);
  c.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST
*/

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


START_TEST (test_WriteSBML_Reaction)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>"
  );


  Reaction r("r", NULL, false);
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


  Reaction r("r", NULL, false);

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
  sr.setStoichiometryMath("1/d");

  sr.setSBMLDocument(D);
  sr.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST

/*
START_TEST (test_WriteSBML_ModifierSpeciesReference_notes)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<reaction id=\"r\">\n"
    "  <listOfModifiers>\n"
    "    <modifierSpeciesReference species=\"s\">\n"
    "      <notes>\n"
    "        This is a note.\n"
    "      </notes>\n"
    "    </modifierSpeciesReference>\n"
    "  </listOfModifiers>\n"
    "</reaction>"
  );


  Reaction r("r");
  r.createModifier()->setSpecies("s");
  r.getModifier(0)->setNotes("This is a note.");

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_ModifierSpeciesReference_annotation)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<reaction id=\"r\">\n"
    "  <listOfModifiers>\n"
    "    <modifierSpeciesReference species=\"s\">\n"
    "      <annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "        <ls:this-is-a-test/>\n"
    "      </annotation>\n"
    "    </modifierSpeciesReference>\n"
    "  </listOfModifiers>\n"
    "</reaction>"
  );

  const char* a =
    "<annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "        <ls:this-is-a-test/>\n"
    "      </annotation>";


  Reaction r("r");
  r.createModifier()->setSpecies("s");
  r.getModifier(0)->setAnnotation(a);

  r.setSBMLDocument(D);
  r.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST
*/

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

/*
START_TEST (test_WriteSBML_KineticLaw_notes)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <notes>\n"
    "    This is a note.\n"
    "  </notes>\n"
    "</kineticLaw>"
  );


  KineticLaw kl("nk * e", "second", "item");
  kl.setNotes("This is a note.");

  kl.setSBMLDocument(D);
  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST
*/

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

/*
START_TEST (test_WriteSBML_KineticLaw_ListOfParameters_notes_L1v2)
{
  D->setLevelAndVersion(1, 2);

  const char* expected = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <listOfParameters>\n"
    "    <parameter name=\"n\" value=\"1.2\"/>\n"
    "  </listOfParameters>\n"
    "</kineticLaw>\n"
  );

  KineticLaw kl("nk * e", "second", "item");
  kl.setSBMLDocument(D);

  Parameter p("n", 1.2);
  kl.addParameter( &p );

  kl.getListOfParameters()->setMetaId("lop");
  kl.getListOfParameters()->setNotes("This is a note.");

  kl.write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST
*/
/*
START_TEST (test_WriteSBML_KineticLaw_ListOfParameters_notes_L2v1)
{
  D->setLevelAndVersion(2, 1);

  const char* expected = wrapXML
  (
    "<kineticLaw timeUnits=\"second\" substanceUnits=\"item\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> nk </ci>\n"
    "      <ci> e </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "  <listOfParameters metaid=\"lop\">\n"
    "    <notes>\n"
    "      This is a note.\n"
    "    </notes>\n"
    "    <parameter id=\"n\" value=\"1.2\"/>\n"
    "  </listOfParameters>\n"
    "</kineticLaw>\n"
  );

  KineticLaw kl("nk * e", "second", "item");
  kl.setSBMLDocument(D);

  Parameter p("n", 1.2);
  kl.addParameter( &p );

  kl.getListOfParameters()->setMetaId("lop");
  kl.getListOfParameters()->setNotes("This is a note.");

  kl.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST
*/

START_TEST (test_WriteSBML_Event)
{
  const char* expected = wrapXML("<event id=\"e\"/>");


  Event e("e");

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_trigger)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\" timeUnits=\"second\">\n"
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
  Trigger t("leq(P1, t)");
  e.setTrigger(*t);
  e.setTimeUnits("second");

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_delay)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\" timeUnits=\"second\">\n"
    "  <delay>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"integer\"> 5 </cn>\n"
    "    </math>\n"
    "  </delay>\n"
    "</event>"
  );

  Event e("e");
  Delay d("5");
  e.setDelay(*d);
  e.setTimeUnits("second");

  e.write(*XOS);

  fail_unless( equals(expected) );
}
END_TEST


START_TEST (test_WriteSBML_Event_both)
{
  const char* expected = wrapXML
  (
    "<event id=\"e\" timeUnits=\"second\">\n"
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
  Trigger t("leq(P1, t)");
  Delay d("5");
  e.setDelay(*d);
  e.setTrigger(*t);
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
    "    <eventAssignment variable=\"k2\">\n"
    "      <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </math>\n"
    "    </eventAssignment>\n"
    "  </listOfEventAssignments>\n"
    "</event>"
  );

  Event e("e");
  Trigger t("leq(P1, t)");
  EventAssignment ea("k2", "0");

  e.setTrigger(*t);
  e.addEventAssignment( &ea );

  e.write(*XOS);

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

/*
START_TEST (test_WriteSBML_xmlns)
{
  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "xmlns:jd=\"http://www.sbml.org/2001/ns/jdesigner\" "
    "level=\"2\" version=\"1\"/>\n"
  );


  SBMLDocument d(2, 1);

  d.getNamespaces().add("jd", "http://www.sbml.org/2001/ns/jdesigner");

  fail_unless( equals(expected, S) );
}
END_TEST
*/

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


//
// A <rateRule> with a <notes> and/or <annotation> is not formatted with a
// closing angle bracket, e.g.:
//
//   <rateRule variable="x"  <notes>
//      This is a note.
//     </notes>
//   </rateRule>
//
// Reported by Konstantin Kozlov <kozlov@spbcas.ru>
//
/*
START_TEST (test_WriteSBML_nonempty_RateRule_bug)
{
  const char* expected = wrapXML
  (
    "<rateRule variable=\"x\">\n"
    "  <notes>\n"
    "    This is a note.\n"
    "  </notes>\n"
    "</rateRule>"
  );


  RateRule r("x");
  r.setNotes("This is a note.");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST
*/

//
// An annotation on the top-level <sbml> element is not written-out for L2
// documents.
//
// Reported by Damon Hachmeister <damon.hachmeister@mathworks.com>.
//
/*
START_TEST (test_WriteSBML_SBMLDocument_no_annotation_bug_L2)
{
  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\">\n"
    "  <annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "  <ls:this-is-a-test/>\n"
    "</annotation>\n"
    "</sbml>"
  );

  const char* a =
    "<annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "  <ls:this-is-a-test/>\n"
    "</annotation>";


  SBMLDocument d(2, 1);

  d.setAnnotation(a);

  fail_unless( equals(expected, S) );
}
END_TEST
*/

//
// Annotations are not allowed on the top-level <sbml> element in L1.  I
// wanted to make sure I didn't introduce a bug when I fixed the one above.
//
/*
START_TEST (test_WriteSBML_SBMLDocument_no_annotation_bug_L1)
{
  const char* expected = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" "
    "level=\"1\" version=\"2\"/>\n"
  );

  const char* a =
    "<annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "  <ls:this-is-a-test/>\n"
    "</annotation>";


  SBMLDocument d(1, 2);

  d.setAnnotation(a);

  fail_unless( equals(expected, S) );
}
END_TEST
*/


Suite *
create_suite_WriteSBML ()
{
  Suite *suite = suite_create("WriteSBML");
  TCase *tcase = tcase_create("WriteSBML");


  tcase_add_checked_fixture(tcase, WriteSBML_setup, WriteSBML_teardown);
 
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

  // FunctionDefinition
  tcase_add_test( tcase, test_WriteSBML_FunctionDefinition );

  // Unit
  tcase_add_test( tcase, test_WriteSBML_Unit          );
  tcase_add_test( tcase, test_WriteSBML_Unit_defaults );
  tcase_add_test( tcase, test_WriteSBML_Unit_L2v1     );

  // UnitDefinition
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition           );
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_full      );  
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_L2v1      );
  tcase_add_test( tcase, test_WriteSBML_UnitDefinition_L2v1_full );

  // Compartment
  tcase_add_test( tcase, test_WriteSBML_Compartment                );
  tcase_add_test( tcase, test_WriteSBML_Compartment_unsetVolume    );
  //tcase_add_test( tcase, test_WriteSBML_Compartment_annotation     );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1           );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1_constant  );
  tcase_add_test( tcase, test_WriteSBML_Compartment_L2v1_unsetSize );

  // Species
  tcase_add_test( tcase, test_WriteSBML_Species                   );
  tcase_add_test( tcase, test_WriteSBML_Species_L1v1              );
  tcase_add_test( tcase, test_WriteSBML_Species_defaults          );
  tcase_add_test( tcase, test_WriteSBML_Species_skipOptional      );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v1              );
  tcase_add_test( tcase, test_WriteSBML_Species_L2v1_skipOptional );

  // Parameter
  tcase_add_test( tcase, test_WriteSBML_Parameter                   );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L1v1_required     );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L1v2_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1              );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1_skipOptional );
  tcase_add_test( tcase, test_WriteSBML_Parameter_L2v1_constant     );

  // AlgebraicRule
  tcase_add_test( tcase, test_WriteSBML_AlgebraicRule      );
  tcase_add_test( tcase, test_WriteSBML_AlgebraicRule_L2v1 );

  // SpeciesConcentrationRule
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule          );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_L1v1     );
  tcase_add_test( tcase, test_WriteSBML_SpeciesConcentrationRule_L2v1     );

  // CompartmentVolumeRule
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule          );
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_CompartmentVolumeRule_L2v1     );

  // ParameterRule
  tcase_add_test( tcase, test_WriteSBML_ParameterRule          );
  tcase_add_test( tcase, test_WriteSBML_ParameterRule_defaults );
  tcase_add_test( tcase, test_WriteSBML_ParameterRule_L2v1     );

  // Reaction
  tcase_add_test( tcase, test_WriteSBML_Reaction           );
  tcase_add_test( tcase, test_WriteSBML_Reaction_defaults  );
  tcase_add_test( tcase, test_WriteSBML_Reaction_full      );
  tcase_add_test( tcase, test_WriteSBML_Reaction_L2v1      );
  tcase_add_test( tcase, test_WriteSBML_Reaction_L2v1_full );

  // SpeciesReference

  tcase_add_test( tcase, test_WriteSBML_SpeciesReference          );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L1v1     );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_defaults );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_1   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_2   );
  tcase_add_test( tcase, test_WriteSBML_SpeciesReference_L2v1_3   );

  // ModifierSpeciesReference
  //tcase_add_test( tcase, test_WriteSBML_ModifierSpeciesReference_notes );
  //tcase_add_test( tcase, test_WriteSBML_ModifierSpeciesReference_annotation);

  // KineticLaw
  tcase_add_test( tcase, test_WriteSBML_KineticLaw                  );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_skipOptional     );
  //tcase_add_test( tcase, test_WriteSBML_KineticLaw_notes            );
  tcase_add_test( tcase, test_WriteSBML_KineticLaw_ListOfParameters );
  //tcase_add_test( tcase,
  //                test_WriteSBML_KineticLaw_ListOfParameters_notes_L1v2 );
  //tcase_add_test( tcase,
  //                test_WriteSBML_KineticLaw_ListOfParameters_notes_L2v1 );

  // Event
  tcase_add_test( tcase, test_WriteSBML_Event         );
  tcase_add_test( tcase, test_WriteSBML_Event_trigger );
  tcase_add_test( tcase, test_WriteSBML_Event_delay   );
  tcase_add_test( tcase, test_WriteSBML_Event_both    );
  tcase_add_test( tcase, test_WriteSBML_Event_full    );

  // Miscellaneous
  tcase_add_test( tcase, test_WriteSBML_NaN     );
  tcase_add_test( tcase, test_WriteSBML_INF     );
  tcase_add_test( tcase, test_WriteSBML_NegINF  );
  //tcase_add_test( tcase, test_WriteSBML_xmlns );
  tcase_add_test( tcase, test_WriteSBML_locale  );

  // Bug
  /*
  tcase_add_test( tcase, test_WriteSBML_nonempty_RateRule_bug             );
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_no_annotation_bug_L2 );
  tcase_add_test( tcase, test_WriteSBML_SBMLDocument_no_annotation_bug_L1 );
  */
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
