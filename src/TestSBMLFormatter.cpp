/**
 * Filename    : TestSBMLFormatter.cpp
 * Description : SBMLFormatter unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-03-07
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stephan Hoops
 */


#include <iostream>
#include <check.h>

#include "sbml/common.h"
#include "sbml/SBMLFormatter.hpp"


BEGIN_C_DECLS


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_HEADER   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define wrapXML(s)   XML_HEADER s


static MemBufFormatTarget *target;
static SBMLFormatter      *formatter;


void
TestSBMLFormatter_setup (void)
{
#ifndef USE_EXPAT
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
    fail("XMLPlatformUtils::Initialize() threw an Exception.");
  }
#endif // !USE_EXPAT

  target    = new MemBufFormatTarget();
  formatter = new SBMLFormatter("UTF-8", target);
}


void
TestSBMLFormatter_teardown (void)
{
  delete formatter;
}


START_TEST (test_SBMLFormatter_SBMLDocument_L1v1)
{
  const char *s = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" "
    "level=\"1\" version=\"1\"/>\n"
  );

  SBMLDocument d(1, 1);


  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_SBMLDocument_L1v2)
{
  const char *s = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" "
    "level=\"1\" version=\"2\"/>\n"
  );

  SBMLDocument d(1, 2);


  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_SBMLDocument_L2v1)
{
  const char *s = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\"/>\n"
  );

  SBMLDocument d(2, 1);


  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Model)
{
  const char* s = wrapXML("<model name=\"Branch\"/>\n");
  Model m("", "Branch");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << m;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Model_skipOptional)
{
  const char *s = wrapXML("<model/>\n");
  Model m;


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << m;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Model_L2v1)
{
  const char* s = wrapXML("<model id=\"Branch\"/>\n");
  Model m("Branch");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << m;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Model_L2v1_skipOptional)
{
  const char* s = wrapXML("<model/>\n");
  Model m;


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << m;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_FunctionDefinition)
{
  const char* s = wrapXML
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
    "</functionDefinition>\n"
  );

  FunctionDefinition fd("pow3", "lambda(x, x^3)");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << fd;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Unit)
{
  const char* s = wrapXML
  (
    "<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>\n"
  );

  Unit u("kilogram", 2, -3);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << u;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Unit_defaults)
{
  const char* s = wrapXML("<unit kind=\"kilogram\"/>\n");
  Unit u("kilogram", 1, 0);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << u;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Unit_L2v1)
{
  const char* s = wrapXML
  (
    "<unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n"
  );

  Unit u("Celsius", 1, 0, 1.8, 32);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << u;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_UnitDefinition)
{
  const char* s = wrapXML("<unitDefinition name=\"mmls\"/>\n");
  UnitDefinition ud("mmls");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << ud;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_UnitDefinition_full)
{
  const char* s = wrapXML
  (
    "<unitDefinition name=\"mmls\">\n"
    "  <listOfUnits>\n"
    "    <unit kind=\"mole\" scale=\"-3\"/>\n"
    "    <unit kind=\"liter\" exponent=\"-1\"/>\n"
    "    <unit kind=\"second\" exponent=\"-1\"/>\n"
    "  </listOfUnits>\n"
    "</unitDefinition>\n"
  );

  UnitDefinition ud("mmls");

  ud.addUnit( * new Unit("mole"  ,  1, -3) );
  ud.addUnit( * new Unit("liter" , -1)     );
  ud.addUnit( * new Unit("second", -1)     );


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << ud;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_UnitDefinition_L2v1)
{
  const char* s = wrapXML("<unitDefinition id=\"mmls\"/>\n");
  UnitDefinition ud("mmls");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << ud;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_UnitDefinition_L2v1_full)
{
  const char *s = wrapXML
  (
    "<unitDefinition id=\"Fahrenheit\">\n"
    "  <listOfUnits>\n"
    "    <unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n"
    "  </listOfUnits>\n"
    "</unitDefinition>\n"
  );

  UnitDefinition ud("Fahrenheit");
  ud.addUnit( * new Unit("Celsius", 1, 0, 1.8, 32) );


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << ud;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment)
{
  const char* s = wrapXML
  (
    "<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>\n"
  );

  Compartment c("A");

  c.setSize(2.1);
  c.setOutside("B");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


/**
 * Although compartment has a default volume of 1 in SBML L1, IEEE 754
 * doubles (or floats) cannot be reliably compared for equality.  To be
 * safe, output the compartment volume even if equal to the default.
 */
START_TEST (test_SBMLFormatter_Compartment_skipOptional)
{
  const char* s = wrapXML("<compartment name=\"A\" volume=\"1\"/>\n");
  Compartment c;


  c.setName("A");

  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


/**
 * However, do not output volume if unset.
 */
START_TEST (test_SBMLFormatter_Compartment_unsetVolume)
{
  const char* s = wrapXML("<compartment name=\"A\"/>\n");
  Compartment c;

  c.setName("A");
  c.unsetVolume();

  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment_annotation)
{
  const char* s = wrapXML
  (
    "<compartment name=\"A\" volume=\"2.1\" outside=\"B\">\n"
    "  <annotation xmlns:mysim=\"http://www.mysim.org/ns\">\n"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\"/>\n"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>\n"
    "</annotation>\n"
    "</compartment>\n"
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


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment_L2v1)
{
  const char* s = wrapXML
  (
    "<compartment id=\"M\" spatialDimensions=\"2\" size=\"2.5\"/>\n"
  );

  Compartment c("M");

  c.setSize(2.5);
  c.setSpatialDimensions(2);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment_L2v1_constant)
{
  const char* s = wrapXML
  (
    "<compartment id=\"cell\" size=\"1.2\" constant=\"false\"/>\n"
  );

  Compartment c("cell");

  c.setSize(1.2);
  c.setConstant(false);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment_L2v1_unsetSize)
{
  const char* s = wrapXML("<compartment id=\"A\"/>\n");
  Compartment c;

  c.setId("A");
  c.unsetSize();

  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST



START_TEST (test_SBMLFormatter_Species)
{
  const char* s = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>\n"
  );

  Species sp("Ca2");

  sp.setCompartment("cell");
  sp.setInitialAmount(0.7);
  sp.setUnits("mole");
  sp.setBoundaryCondition(true);
  sp.setCharge(2);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Species_L1v1)
{
  const char* s = wrapXML
  (
    "<specie name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>\n"
  );

  Species sp("Ca2");

  sp.setCompartment("cell");
  sp.setInitialAmount(0.7);
  sp.setUnits("mole");
  sp.setBoundaryCondition(true);
  sp.setCharge(2);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version1;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Species_defaults)
{
  const char* s = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" charge=\"2\"/>\n"
  );

  Species sp("Ca2");

  sp.setCompartment("cell");
  sp.setInitialAmount(0.7);
  sp.setUnits("mole");
  sp.setCharge(2);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Species_skipOptional)
{
  const char* s = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"/>\n"
  );

  Species sp("Ca2");

  sp.setCompartment("cell");
  sp.setInitialAmount(0.7);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Species_L2v1)
{
  const char* s = wrapXML
  (
    "<species id=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\" "
    "substanceUnits=\"mole\" constant=\"true\"/>\n"
  );

  Species sp("Ca2");

  sp.setCompartment("cell");
  sp.setInitialAmount(0.7);
  sp.setSubstanceUnits("mole");
  sp.setConstant(true);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Species_L2v1_skipOptional)
{
  const char* s = wrapXML("<species id=\"Ca2\" compartment=\"cell\"/>\n");

  Species sp("Ca2");
  sp.setCompartment("cell");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter)
{
  const char* s = wrapXML
  (
    "<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>\n"
  );

  Parameter p("Km1", 2.3, "second");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter_L1v1_required)
{
  const char* s = wrapXML("<parameter name=\"Km1\" value=\"NaN\"/>\n");
  Parameter   p;

  p.setName("Km1");
  p.unsetValue();


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version1;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter_L1v2_skipOptional)
{
  const char* s = wrapXML("<parameter name=\"Km1\"/>\n");
  Parameter   p;


  p.setName("Km1");
  p.unsetValue();

  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter_L2v1)
{
  const char* s = wrapXML
  (
    "<parameter id=\"Km1\" value=\"2.3\" units=\"second\"/>\n"
  );

  Parameter p("Km1", 2.3, "second");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter_L2v1_skipOptional)
{
  const char* s = wrapXML("<parameter id=\"Km1\"/>\n");
  Parameter   p("Km1");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter_L2v1_constant)
{
  const char* s = wrapXML("<parameter id=\"x\" constant=\"false\"/>\n");
  Parameter   p("x");

  p.setConstant(false);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_AlgebraicRule)
{
  const char*   s = wrapXML("<algebraicRule formula=\"x + 1\"/>\n");
  AlgebraicRule ar("x + 1");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << ar;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_AlgebraicRule_L2v1)
{
  const char* s = wrapXML
  (
    "<algebraicRule>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> x </ci>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "    </apply>\n"
    "  </math>\n"
    "</algebraicRule>\n"
  );

  AlgebraicRule ar("x + 1");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << ar;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesConcentrationRule)
{
  const char* s = wrapXML
  (
    "<speciesConcentrationRule "
    "formula=\"t * s\" type=\"rate\" species=\"s\"/>\n"
  );

  SpeciesConcentrationRule scr("s", "t * s", RULE_TYPE_RATE);;


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << scr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesConcentrationRule_defaults)
{
  const char* s = wrapXML
  (
    "<speciesConcentrationRule formula=\"t * s\" species=\"s\"/>\n"
    
  );

  SpeciesConcentrationRule scr("s", "t * s");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << scr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesConcentrationRule_L1v1)
{
  const char* s = wrapXML
  (
    "<specieConcentrationRule formula=\"t * s\" specie=\"s\"/>\n"
  );

  SpeciesConcentrationRule scr("s", "t * s", RULE_TYPE_SCALAR);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version1;
  *formatter << scr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesConcentrationRule_L2v1)
{
  const char* s = wrapXML
  (
    "<assignmentRule variable=\"s\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> t </ci>\n"
    "      <ci> s </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>\n"
  );

  SpeciesConcentrationRule scr("s", "t * s");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << scr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_CompartmentVolumeRule)
{
  const char* s = wrapXML
  (
    "<compartmentVolumeRule "
    "formula=\"v + s\" type=\"rate\" compartment=\"c\"/>\n"
  );

  CompartmentVolumeRule cvr("c", "v + s", RULE_TYPE_RATE);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << cvr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_CompartmentVolumeRule_defaults)
{
  const char* s = wrapXML
  (
    "<compartmentVolumeRule formula=\"v + s\" compartment=\"c\"/>\n"
  );

  CompartmentVolumeRule cvr("c", "v + s");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << cvr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_CompartmentVolumeRule_L2v1)
{
  const char* s = wrapXML
  (
    "<assignmentRule variable=\"c\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> v </ci>\n"
    "      <ci> s </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</assignmentRule>\n"
  );

  CompartmentVolumeRule cvr("c", "v + s");

  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << cvr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_ParameterRule)
{
  const char* s = wrapXML
  (
    "<parameterRule "
    "formula=\"p * t\" type=\"rate\" name=\"p\"/>\n"
  );

  ParameterRule pr("p", "p * t", RULE_TYPE_RATE);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << pr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_ParameterRule_defaults)
{
  const char* s = wrapXML
  (
    "<parameterRule formula=\"p * t\" name=\"p\"/>\n"
  );


  ParameterRule pr("p", "p * t");

  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << pr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_ParameterRule_L2v1)
{
  const char* s = wrapXML
  (
    "<rateRule variable=\"p\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> p </ci>\n"
    "      <ci> t </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</rateRule>\n"    
  );


  ParameterRule pr("p", "p * t", RULE_TYPE_RATE);

  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << pr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Reaction)
{
  const char* s = wrapXML
  (
    "<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>\n"
  );

  Reaction r("r", NULL, false);
  r.setFast(true);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Reaction_defaults)
{
  const char* s = wrapXML("<reaction name=\"r\"/>\n");
  Reaction    r;



  r.setName("r");

  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Reaction_full)
{
  const char* s = wrapXML
  (
    "<reaction name=\"v1\">\n"
    "  <listOfReactants>\n"
    "    <speciesReference species=\"x0\"/>\n"
    "  </listOfReactants>\n"
    "  <listOfProducts>\n"
    "    <speciesReference species=\"s1\"/>\n"
    "  </listOfProducts>\n"
    "  <kineticLaw formula=\"(vm * s1)/(km + s1)\"/>\n"
    "</reaction>\n"
  );

  Reaction r("v1", new KineticLaw("(vm * s1)/(km + s1)"), true);

  r.addReactant( * new SpeciesReference("x0") );
  r.addProduct ( * new SpeciesReference("s1") );


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Reaction_L2v1)
{
  const char* s = wrapXML("<reaction id=\"r\" reversible=\"false\"/>\n");
  Reaction    r("r", NULL, false);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Reaction_L2v1_full)
{
  const char* s = wrapXML
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
    "</reaction>\n"
  );

  Reaction r("v1", new KineticLaw("(vm * s1)/(km + s1)"), true);

  r.addReactant( * new SpeciesReference("x0")         );
  r.addProduct ( * new SpeciesReference("s1")         );
  r.addModifier( * new ModifierSpeciesReference("m1") );


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference)
{
  const char* s = wrapXML
  (
    "<speciesReference species=\"s\" stoichiometry=\"3\" denominator=\"2\"/>\n"
  );

  SpeciesReference sr("s", 3, 2);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference_L1v1)
{
  const char* s = wrapXML
  (
    "<specieReference specie=\"s\" stoichiometry=\"3\" denominator=\"2\"/>\n"
  );

  SpeciesReference sr("s", 3, 2);


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version1;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference_defaults)
{
  const char*      s = wrapXML("<speciesReference species=\"s\"/>\n");
  SpeciesReference sr("s");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference_L2v1_1)
{
  const char* s = wrapXML
  (
    "<speciesReference species=\"s\">\n"
    "  <stoichiometryMath>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n"
    "    </math>\n"
    "  </stoichiometryMath>\n"
    "</speciesReference>\n"
  );

  SpeciesReference sr("s", 3, 2);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference_L2v1_2)
{
  const char* s = wrapXML
  (
    "<speciesReference species=\"s\" stoichiometry=\"3.2\"/>\n"
  );

  SpeciesReference sr("s", 3.2);


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_SpeciesReference_L2v1_3)
{
  const char* s = wrapXML
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
    "</speciesReference>\n"
  );

  SpeciesReference sr("s");
  sr.setStoichiometryMath("1/d");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw)
{
  const char* s = wrapXML
  (
    "<kineticLaw formula=\"k * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\"/>\n"
  );

  KineticLaw kl("k * e", "second", "item");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw_skipOptional)
{
  const char* s = wrapXML("<kineticLaw formula=\"k * e\"/>\n");
  KineticLaw  kl("k * e");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw_notes)
{
  const char* s = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <notes>\n"
    "    This is a note.\n"
    "  </notes>\n"
    "</kineticLaw>\n"
  );

  KineticLaw kl("nk * e", "second", "item");
  kl.setNotes("This is a note.");


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw_ListOfParameters)
{
  const char* s = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <listOfParameters>\n"
    "    <parameter name=\"n\" value=\"1.2\"/>\n"
    "  </listOfParameters>\n"
    "</kineticLaw>\n"
  );

  KineticLaw kl("nk * e", "second", "item");
  kl.addParameter( * new Parameter("n", 1.2) );


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw_ListOfParameters_notes_L1v2)
{
  const char* s = wrapXML
  (
    "<kineticLaw formula=\"nk * e\" timeUnits=\"second\" "
    "substanceUnits=\"item\">\n"
    "  <listOfParameters>\n"
    "    <parameter name=\"n\" value=\"1.2\"/>\n"
    "  </listOfParameters>\n"
    "</kineticLaw>\n"
  );

  KineticLaw kl("nk * e", "second", "item");

  kl.addParameter( * new Parameter("n", 1.2) );

  ListOf& lo = kl.getListOfParameters();
  lo.setMetaId( "lop" );
  lo.setNotes ( "This is a note." );


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_KineticLaw_ListOfParameters_notes_L2v1)
{
  const char* s = wrapXML
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

  kl.addParameter( * new Parameter("n", 1.2) );

  ListOf& lo = kl.getListOfParameters();

  lo.setMetaId( "lop" );
  lo.setNotes ( "This is a note." );


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << kl;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Event)
{
  const char* s = wrapXML("<event id=\"e\"/>\n");
  Event       e("e");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << e;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Event_trigger)
{
  const char* s = wrapXML
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
    "</event>\n"
  );

  Event e("e", "leq(P1, t)");
  e.setTimeUnits("second");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << e;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Event_delay)
{
  const char* s = wrapXML
  (
    "<event id=\"e\" timeUnits=\"second\">\n"
    "  <delay>\n"
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "      <cn type=\"integer\"> 5 </cn>\n"
    "    </math>\n"
    "  </delay>\n"
    "</event>\n"
  );

  Event e("e", "", "5");
  e.setTimeUnits("second");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << e;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Event_both)
{
  const char* s = wrapXML
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
    "</event>\n"
  );

  Event e("e", "leq(P1, t)", "5");
  e.setTimeUnits("second");

  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << e;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_Event_full)
{
  const char* s = wrapXML
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
    "</event>\n"
  );

  Event e( "e" , "leq(P1, t)" );
  

  e.addEventAssignment( * new EventAssignment("k2", "0") );


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << e;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


/**
 * In the case of an unset required attribute, there's not much that can be
 * done at this point, except to make it explicit.
 */
START_TEST (test_SBMLFormatter_unset_required)
{
  const char*      s = wrapXML("<speciesReference species=\"\"/>\n");
  SpeciesReference sr;


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << sr;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_NaN)
{
  const char* s = wrapXML("<parameter name=\"p\" value=\"NaN\"/>\n");
  Parameter   p("p", util_NaN());


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_INF)
{
  const char* s = wrapXML("<parameter name=\"p\" value=\"INF\"/>\n");
  Parameter   p ("p", util_PosInf());


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_NegINF)
{
  const char* s = wrapXML("<parameter name=\"p\" value=\"-INF\"/>\n");
  Parameter   p("p", util_NegInf());


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_NegZero)
{
  const char* s = wrapXML("<parameter name=\"p\" value=\"-0\"/>\n");
  Parameter   p = Parameter("p", util_NegZero());


  *formatter << SBMLFormatter::Level1 << SBMLFormatter::Version2;
  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


START_TEST (test_SBMLFormatter_xmlns)
{
  const char *s = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "xmlns:jd=\"http://www.sbml.org/2001/ns/jdesigner\" "
    "level=\"2\" version=\"1\"/>\n"
  );


  SBMLDocument d(2, 1);

  d.getNamespaces().add("jd", "http://www.sbml.org/2001/ns/jdesigner");

  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


/**
 * A <rateRule> with a <notes> and/or <annotation> is not formatted with a
 * closing angle bracket, e.g.:
 *
 *   <rateRule variable="x"  <notes>
 *      This is a note.
 *     </notes>
 *   </rateRule>
 *
 * Reported by Konstantin Kozlov <kozlov@spbcas.ru>
 */
START_TEST (test_SBMLFormatter_nonempty_RateRule_bug)
{
  const char* s = wrapXML
  (
    "<rateRule variable=\"x\">\n"
    "  <notes>\n"
    "    This is a note.\n"
    "  </notes>\n"
    "</rateRule>\n"    
  );


  RateRule r("x");
  r.setNotes("This is a note.");


  *formatter << SBMLFormatter::Level2 << SBMLFormatter::Version1;
  *formatter << r;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );
}
END_TEST


/**
 * An annotation on the top-level <sbml> element is not written-out for L2
 * documents.
 *
 * Reported by Damon Hachmeister <damon.hachmeister@mathworks.com>.
 */
START_TEST (test_SBMLFormatter_SBMLDocument_no_annotation_bug_L2)
{
  const char* s = wrapXML
  (
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\">\n"
    "  <annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "  <ls:this-is-a-test/>\n"
    "</annotation>\n"
    "</sbml>\n"
  );

  const char* a =
    "<annotation xmlns:ls=\"http://www.sbml.org/2001/ns/libsbml\">\n"
    "  <ls:this-is-a-test/>\n"
    "</annotation>";


  SBMLDocument d(2, 1);

  d.setAnnotation(a);

  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


/**
 * Annotations are not allowed on the top-level <sbml> element in L1.  I
 * wanted to make sure I didn't introduce a bug when I fixed the one above.
 */
START_TEST (test_SBMLFormatter_SBMLDocument_no_annotation_bug_L1)
{
  const char* s = wrapXML
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

  *formatter << d;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


Suite *
create_suite_SBMLFormatter (void)
{
  Suite *suite = suite_create("SBMLFormatter");
  TCase *tcase = tcase_create("SBMLFormatter");


  tcase_add_checked_fixture(tcase,
                            TestSBMLFormatter_setup,
                            TestSBMLFormatter_teardown);
 

  /** SBMLDocument **/
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument_L1v1 );
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument_L1v2 );
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument_L2v1 );


  /** Model **/
  tcase_add_test( tcase, test_SBMLFormatter_Model                   );
  tcase_add_test( tcase, test_SBMLFormatter_Model_skipOptional      );
  tcase_add_test( tcase, test_SBMLFormatter_Model_L2v1              );
  tcase_add_test( tcase, test_SBMLFormatter_Model_L2v1_skipOptional );

  /** FunctionDefinition **/
  tcase_add_test( tcase, test_SBMLFormatter_FunctionDefinition );

  /** Unit **/
  tcase_add_test( tcase, test_SBMLFormatter_Unit          );
  tcase_add_test( tcase, test_SBMLFormatter_Unit_defaults );
  tcase_add_test( tcase, test_SBMLFormatter_Unit_L2v1     );

  /** UnitDefinition **/
  tcase_add_test( tcase, test_SBMLFormatter_UnitDefinition           );
  tcase_add_test( tcase, test_SBMLFormatter_UnitDefinition_full      );  
  tcase_add_test( tcase, test_SBMLFormatter_UnitDefinition_L2v1      );
  tcase_add_test( tcase, test_SBMLFormatter_UnitDefinition_L2v1_full );

  /** Compartment **/
  tcase_add_test( tcase, test_SBMLFormatter_Compartment                );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_skipOptional   );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_unsetVolume    );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_annotation     );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_L2v1           );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_L2v1_constant  );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment_L2v1_unsetSize );

  /** Species **/
  tcase_add_test( tcase, test_SBMLFormatter_Species                   );
  tcase_add_test( tcase, test_SBMLFormatter_Species_L1v1              );
  tcase_add_test( tcase, test_SBMLFormatter_Species_defaults          );
  tcase_add_test( tcase, test_SBMLFormatter_Species_skipOptional      );
  tcase_add_test( tcase, test_SBMLFormatter_Species_L2v1              );
  tcase_add_test( tcase, test_SBMLFormatter_Species_L2v1_skipOptional );

  /** Parameter **/
  tcase_add_test( tcase, test_SBMLFormatter_Parameter                   );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter_L1v1_required     );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter_L1v2_skipOptional );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter_L2v1              );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter_L2v1_skipOptional );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter_L2v1_constant     );

  /** AlgebraicRule **/
  tcase_add_test( tcase, test_SBMLFormatter_AlgebraicRule      );
  tcase_add_test( tcase, test_SBMLFormatter_AlgebraicRule_L2v1 );

  /** SpeciesConcentrationRule **/
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesConcentrationRule          );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesConcentrationRule_defaults );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesConcentrationRule_L1v1     );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesConcentrationRule_L2v1     );

  /** CompartmentVolumeRule **/
  tcase_add_test( tcase, test_SBMLFormatter_CompartmentVolumeRule          );
  tcase_add_test( tcase, test_SBMLFormatter_CompartmentVolumeRule_defaults );
  tcase_add_test( tcase, test_SBMLFormatter_CompartmentVolumeRule_L2v1     );

  /** ParameterRule **/
  tcase_add_test( tcase, test_SBMLFormatter_ParameterRule          );
  tcase_add_test( tcase, test_SBMLFormatter_ParameterRule_defaults );
  tcase_add_test( tcase, test_SBMLFormatter_ParameterRule_L2v1     );

  /** Reaction **/
  tcase_add_test( tcase, test_SBMLFormatter_Reaction           );
  tcase_add_test( tcase, test_SBMLFormatter_Reaction_defaults  );
  tcase_add_test( tcase, test_SBMLFormatter_Reaction_full      );
  tcase_add_test( tcase, test_SBMLFormatter_Reaction_L2v1      );
  tcase_add_test( tcase, test_SBMLFormatter_Reaction_L2v1_full );

  /** SpeciesReference **/
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference          );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference_L1v1     );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference_defaults );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference_L2v1_1   );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference_L2v1_2   );
  tcase_add_test( tcase, test_SBMLFormatter_SpeciesReference_L2v1_3   );

  /** KineticLaw **/
  tcase_add_test( tcase, test_SBMLFormatter_KineticLaw                  );
  tcase_add_test( tcase, test_SBMLFormatter_KineticLaw_skipOptional     );
  tcase_add_test( tcase, test_SBMLFormatter_KineticLaw_notes            );
  tcase_add_test( tcase, test_SBMLFormatter_KineticLaw_ListOfParameters );
  tcase_add_test( tcase,
                  test_SBMLFormatter_KineticLaw_ListOfParameters_notes_L1v2 );
  tcase_add_test( tcase,
                  test_SBMLFormatter_KineticLaw_ListOfParameters_notes_L2v1 );

  /** Event **/
  tcase_add_test( tcase, test_SBMLFormatter_Event         );
  tcase_add_test( tcase, test_SBMLFormatter_Event_trigger );
  tcase_add_test( tcase, test_SBMLFormatter_Event_delay   );
  tcase_add_test( tcase, test_SBMLFormatter_Event_both    );
  tcase_add_test( tcase, test_SBMLFormatter_Event_full    );

  /** Miscellaneous **/
  tcase_add_test( tcase, test_SBMLFormatter_unset_required        );
  tcase_add_test( tcase, test_SBMLFormatter_NaN                   );
  tcase_add_test( tcase, test_SBMLFormatter_INF                   );
  tcase_add_test( tcase, test_SBMLFormatter_NegINF                );
  tcase_add_test( tcase, test_SBMLFormatter_NegZero               );
  tcase_add_test( tcase, test_SBMLFormatter_xmlns                 );

  /* Bugs */
  tcase_add_test( tcase, test_SBMLFormatter_nonempty_RateRule_bug             );
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument_no_annotation_bug_L2 );
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument_no_annotation_bug_L1 );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
