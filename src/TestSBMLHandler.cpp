/**
 * Filename    : TestSBMLHandler.cpp
 * Description : SBMLHandler unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-30
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <iostream>
#include <check.h>

#include "sbml/common.h"

#include "sbml/ASTNode.h"
#include "sbml/FormulaFormatter.h"

#include "sbml/SBMLTypes.h"
#include "sbml/SBMLHandler.hpp"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLUnicodeConstants.hpp"


BEGIN_C_DECLS


#define XML_HEADER    "<?xml version='1.0' encoding='UTF-8'?>\n"
#define SBML_HEADER   "<sbml level='1' version='1'> <model name='testModel'>\n"
#define SBML_HEADER2  "<sbml level='2' version='1'> <model name='testModel'>\n"
#define SBML_FOOTER   "</model> </sbml>"

/**
 * Wraps the string s in the appropriate XML or SBML boilerplate.
 */
#define wrapXML(s)    XML_HEADER s
#define wrapSBML(s)   XML_HEADER SBML_HEADER  s SBML_FOOTER
#define wrapSBML2(s)  XML_HEADER SBML_HEADER2 s SBML_FOOTER


static SBMLDocument_t *D;
static Model_t        *M;


void
SBMLHandlerTest_setup (void)
{
  D = NULL;
}


void
SBMLHandlerTest_teardown (void)
{
  SBMLDocument_free(D);
}


START_TEST (test_element_SBML)
{
  const char* s = wrapXML("<sbml level='1' version='1'> </sbml>");
  

  D = readSBMLFromString(s);

  fail_unless(SBMLDocument_getLevel  (D) == 1, NULL);
  fail_unless(SBMLDocument_getVersion(D) == 1, NULL);
}
END_TEST


START_TEST (test_element_Model)
{
  const char* s = wrapXML
  (
    "<sbml level='1' version='1'>"
    "  <model name='testModel'> </model>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(Model_getName(M), "testModel"), NULL );
}
END_TEST


START_TEST (test_element_Model_L2)
{
  const char* s = wrapXML
  (
    "<sbml level='2' version='1'>"
    "  <model id='testModel'> </model>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless(  Model_isSetId  (M), NULL );
  fail_unless( !Model_isSetName(M), NULL );

  fail_unless( !strcmp(Model_getId(M), "testModel"), NULL );
}
END_TEST


START_TEST (test_element_FunctionDefinition)
{
  FunctionDefinition_t* fd;
  const ASTNode_t*      math;
  char*                 formula;

  const char* s = wrapSBML2
  (
    "<functionDefinition id='pow3' name='cubed'>"
    "  <math>"
    "    <lambda>"
    "      <bvar><ci> x </ci></bvar>"
    "      <apply>"
    "        <power/>"
    "        <ci> x </ci>"
    "        <cn> 3 </cn>"
    "      </apply>"
    "    </lambda>"
    "  </math>"
    "</functionDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumFunctionDefinitions(M) == 1, NULL );

  fd = Model_getFunctionDefinition(M, 0);
  fail_unless( fd != NULL, NULL );

  fail_unless( FunctionDefinition_isSetId  (fd), NULL );
  fail_unless( FunctionDefinition_isSetName(fd), NULL );

  fail_unless( !strcmp( FunctionDefinition_getId  (fd), "pow3"  ), NULL );
  fail_unless( !strcmp( FunctionDefinition_getName(fd), "cubed" ), NULL );

  fail_unless( FunctionDefinition_isSetMath(fd), NULL );
  math = FunctionDefinition_getMath(fd);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "lambda(x, pow(x, 3))"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_element_UnitDefinition)
{
  UnitDefinition_t* ud;

  const char* s = wrapSBML("<unitDefinition name='mmls'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumUnitDefinitions(M) == 1, NULL );

  ud = Model_getUnitDefinition(M, 0);
  fail_unless( !strcmp(UnitDefinition_getName(ud), "mmls"), NULL );
}
END_TEST


START_TEST (test_element_UnitDefinition_L2)
{
  UnitDefinition_t* ud;

  const char* s = wrapSBML("<unitDefinition id='mmls' name='mmol/ls'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumUnitDefinitions(M) == 1, NULL );

  ud = Model_getUnitDefinition(M, 0);

  fail_unless( UnitDefinition_isSetId  (ud), NULL );
  fail_unless( UnitDefinition_isSetName(ud), NULL );

  fail_unless( !strcmp(UnitDefinition_getId  (ud), "mmls")   , NULL );
  fail_unless( !strcmp(UnitDefinition_getName(ud), "mmol/ls"), NULL );
}
END_TEST


START_TEST (test_element_Unit)
{
  Unit_t*           u;
  UnitDefinition_t* ud;


  const char* s = wrapSBML
  (
    "<unitDefinition name='substance'>"
    "  <listOfUnits> <unit kind='mole' scale='-3'/> </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumUnitDefinitions(M) == 1, NULL );

  ud = Model_getUnitDefinition(M, 0);

  fail_unless( !strcmp(UnitDefinition_getName(ud), "substance"), NULL );
  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);

  fail_unless( Unit_getKind    (u) == UNIT_KIND_MOLE, NULL );
  fail_unless( Unit_getExponent(u) ==  1, NULL );
  fail_unless( Unit_getScale   (u) == -3, NULL );
}
END_TEST


START_TEST (test_element_Unit_L2)
{
  Unit_t*           u;
  UnitDefinition_t* ud;


  const char* s = wrapSBML
  (
    "<unitDefinition id='Fahrenheit'>"
    "  <listOfUnits>"
    "    <unit kind='Celsius' multiplier='1.8' offset='32'/>"
    "  </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumUnitDefinitions(M) == 1, NULL );

  ud = Model_getUnitDefinition(M, 0);

  fail_unless( UnitDefinition_isSetId(ud), NULL );
  fail_unless( !strcmp(UnitDefinition_getId(ud), "Fahrenheit"), NULL );

  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);

  fail_unless( Unit_getKind      (u) == UNIT_KIND_CELSIUS, NULL );
  fail_unless( Unit_getExponent  (u) ==  1  , NULL );
  fail_unless( Unit_getScale     (u) ==  0  , NULL );
  fail_unless( Unit_getMultiplier(u) ==  1.8, NULL );
  fail_unless( Unit_getOffset    (u) == 32  , NULL );
}
END_TEST


START_TEST (test_element_Unit_defaults_L1_L2)
{
  Unit_t*           u;
  UnitDefinition_t* ud;


  const char* s = wrapSBML
  (
    "<unitDefinition name='bogomips'>"
    "  <listOfUnits> <unit kind='second'/> </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumUnitDefinitions(M) == 1, NULL );

  ud = Model_getUnitDefinition(M, 0);

  fail_unless( !strcmp(UnitDefinition_getName(ud), "bogomips"), NULL );
  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);

  fail_unless( Unit_getKind      (u) == UNIT_KIND_SECOND, NULL );
  fail_unless( Unit_getExponent  (u) ==  1  , NULL );
  fail_unless( Unit_getScale     (u) ==  0  , NULL );
  fail_unless( Unit_getMultiplier(u) ==  1.0, NULL );
  fail_unless( Unit_getOffset    (u) ==  0.0, NULL );
}
END_TEST


START_TEST (test_element_Compartment)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
    "<listOfCompartments>"
    "  <compartment name='mitochondria' volume='.0001' units='milliliters'"
    "               outside='cell'/>"
    "</listOfCompartments>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumCompartments(M) == 1, NULL );

  c = Model_getCompartment(M, 0);

  fail_unless( !strcmp( Compartment_getName   (c), "mitochondria" ), NULL );
  fail_unless( !strcmp( Compartment_getUnits  (c), "milliliters"  ), NULL );
  fail_unless( !strcmp( Compartment_getOutside(c), "cell"         ), NULL );
  fail_unless( Compartment_getVolume(c) == .0001, NULL );

  fail_unless( Compartment_isSetVolume(c), NULL );
  fail_unless( Compartment_isSetSize  (c), NULL );
}
END_TEST


START_TEST (test_element_Compartment_L2)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
    "<listOfCompartments>"
    "  <compartment id='membrane' size='.3' spatialDimensions='2'"
    "               units='area' outside='tissue' constant='false'/>"
    "</listOfCompartments>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumCompartments(M) == 1, NULL );

  c = Model_getCompartment(M, 0);

  fail_unless(  Compartment_isSetId     (c), NULL );
  fail_unless( !Compartment_isSetName   (c), NULL );
  fail_unless(  Compartment_isSetVolume (c), NULL );
  fail_unless(  Compartment_isSetSize   (c), NULL );
  fail_unless(  Compartment_isSetUnits  (c), NULL );
  fail_unless(  Compartment_isSetOutside(c), NULL );

  fail_unless( !strcmp( Compartment_getId     (c), "membrane" ), NULL );
  fail_unless( !strcmp( Compartment_getUnits  (c), "area"     ), NULL );
  fail_unless( !strcmp( Compartment_getOutside(c), "tissue"   ), NULL );

  fail_unless( Compartment_getSpatialDimensions(c) == 2, NULL );
  fail_unless( Compartment_getSize(c) == .3, NULL );
}
END_TEST


START_TEST (test_element_Compartment_defaults)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
     "<listOfCompartments> <compartment name='cell'/> </listOfCompartments>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumCompartments(M) == 1, NULL );

  c = Model_getCompartment(M, 0);

  fail_unless(  Compartment_isSetName   (c), NULL );
  fail_unless(  Compartment_isSetVolume (c), NULL );
  fail_unless( !Compartment_isSetSize   (c), NULL );
  fail_unless( !Compartment_isSetUnits  (c), NULL );
  fail_unless( !Compartment_isSetOutside(c), NULL );

  fail_unless( !strcmp( Compartment_getName(c), "cell" ), NULL );
  fail_unless( Compartment_getVolume(c) == 1.0 , NULL );
}
END_TEST


START_TEST (test_element_Compartment_defaults_L2)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
     "<listOfCompartments> <compartment id='cell'/> </listOfCompartments>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumCompartments(M) == 1, NULL );

  c = Model_getCompartment(M, 0);

  fail_unless(  Compartment_isSetId     (c), NULL );
  fail_unless( !Compartment_isSetName   (c), NULL );
  fail_unless( !Compartment_isSetSize   (c), NULL );
  fail_unless( !Compartment_isSetUnits  (c), NULL );
  fail_unless( !Compartment_isSetOutside(c), NULL );

  fail_unless( !strcmp( Compartment_getId(c), "cell" ), NULL );

  fail_unless( Compartment_getSpatialDimensions(c) == 3, NULL );
  fail_unless( Compartment_getConstant(c)          == 1, NULL );
}
END_TEST


START_TEST (test_element_Specie)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<specie name='Glucose' compartment='cell' initialAmount='4.1'"
    "        units='volume' boundaryCondition='false' charge='6'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless( !strcmp( Species_getName       (sp), "Glucose" ), NULL );
  fail_unless( !strcmp( Species_getCompartment(sp), "cell"    ), NULL );
  fail_unless( !strcmp( Species_getUnits      (sp), "volume"  ), NULL );

  fail_unless( Species_getInitialAmount    (sp) == 4.1, NULL );
  fail_unless( Species_getBoundaryCondition(sp) == 0  , NULL );
  fail_unless( Species_getCharge           (sp) == 6  , NULL );

  fail_unless( Species_isSetInitialAmount(sp) == 1, NULL );
  fail_unless( Species_isSetCharge       (sp) == 1, NULL );
}
END_TEST


START_TEST (test_element_Specie_defaults)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<specie name='Glucose' compartment='cell' initialAmount='1.0'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless( !strcmp( Species_getName       (sp), "Glucose" ), NULL );
  fail_unless( !strcmp( Species_getCompartment(sp), "cell"    ), NULL );

  fail_unless( Species_getInitialAmount    (sp) == 1.0, NULL );
  fail_unless( Species_getBoundaryCondition(sp) == 0  , NULL );

  fail_unless( Species_isSetInitialAmount(sp) == 1, NULL );
  fail_unless( Species_isSetCharge       (sp) == 0, NULL );
}
END_TEST


START_TEST (test_element_Species)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<species name='Glucose' compartment='cell' initialAmount='4.1'"
    "         units='volume' boundaryCondition='false' charge='6'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless( !strcmp( Species_getName       (sp), "Glucose" ), NULL );
  fail_unless( !strcmp( Species_getCompartment(sp), "cell"    ), NULL );
  fail_unless( !strcmp( Species_getUnits      (sp), "volume"  ), NULL );

  fail_unless( Species_getInitialAmount    (sp) == 4.1, NULL );
  fail_unless( Species_getBoundaryCondition(sp) == 0  , NULL );
  fail_unless( Species_getCharge           (sp) == 6  , NULL );

  fail_unless( Species_isSetInitialAmount(sp) == 1, NULL );
  fail_unless( Species_isSetCharge       (sp) == 1, NULL );
}
END_TEST


START_TEST (test_element_Species_L2_1)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<species id='Glucose' compartment='cell' initialConcentration='4.1'"
    "         substanceUnits='item' spatialSizeUnits='volume'"
    "         boundaryCondition='true' charge='6' constant='true'/>"

  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless(  Species_isSetId                  (sp), NULL );
  fail_unless( !Species_isSetName                (sp), NULL );
  fail_unless(  Species_isSetCompartment         (sp), NULL );
  fail_unless( !Species_isSetInitialAmount       (sp), NULL );
  fail_unless(  Species_isSetInitialConcentration(sp), NULL );
  fail_unless(  Species_isSetSubstanceUnits      (sp), NULL );
  fail_unless(  Species_isSetSpatialSizeUnits    (sp), NULL );
  fail_unless(  Species_isSetCharge              (sp), NULL );

  fail_unless( !strcmp( Species_getId              (sp), "Glucose" ), NULL );
  fail_unless( !strcmp( Species_getCompartment     (sp), "cell"    ), NULL );
  fail_unless( !strcmp( Species_getSubstanceUnits  (sp), "item"    ), NULL );
  fail_unless( !strcmp( Species_getSpatialSizeUnits(sp), "volume"  ), NULL );

  fail_unless( Species_getInitialConcentration (sp) == 4.1, NULL );
  fail_unless( Species_getHasOnlySubstanceUnits(sp) == 0  , NULL );
  fail_unless( Species_getBoundaryCondition    (sp) == 1  , NULL );
  fail_unless( Species_getCharge               (sp) == 6  , NULL );
  fail_unless( Species_getConstant             (sp) == 1  , NULL );
}
END_TEST


START_TEST (test_element_Species_L2_2)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<species id='s' compartment='c' hasOnlySubstanceUnits='true'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless(  Species_isSetId                  (sp), NULL );
  fail_unless( !Species_isSetName                (sp), NULL );
  fail_unless(  Species_isSetCompartment         (sp), NULL );
  fail_unless( !Species_isSetInitialAmount       (sp), NULL );
  fail_unless( !Species_isSetInitialConcentration(sp), NULL );
  fail_unless( !Species_isSetSubstanceUnits      (sp), NULL );
  fail_unless( !Species_isSetSpatialSizeUnits    (sp), NULL );
  fail_unless( !Species_isSetCharge              (sp), NULL );

  fail_unless( !strcmp( Species_getId         (sp), "s" ), NULL );
  fail_unless( !strcmp( Species_getCompartment(sp), "c" ), NULL );

  fail_unless( Species_getHasOnlySubstanceUnits(sp) == 1, NULL );
  fail_unless( Species_getBoundaryCondition    (sp) == 0, NULL );
  fail_unless( Species_getConstant             (sp) == 0, NULL );
}
END_TEST


START_TEST (test_element_Species_L2_defaults)
{
  Species_t* sp;

  const char* s = wrapSBML("<species id='Glucose_6_P' compartment='cell'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumSpecies(M) == 1, NULL );

  sp = Model_getSpecies(M, 0);

  fail_unless(  Species_isSetId                  (sp), NULL );
  fail_unless( !Species_isSetName                (sp), NULL );
  fail_unless(  Species_isSetCompartment         (sp), NULL );
  fail_unless( !Species_isSetInitialAmount       (sp), NULL );
  fail_unless( !Species_isSetInitialConcentration(sp), NULL );
  fail_unless( !Species_isSetSubstanceUnits      (sp), NULL );
  fail_unless( !Species_isSetSpatialSizeUnits    (sp), NULL );
  fail_unless( !Species_isSetCharge              (sp), NULL );

  fail_unless( !strcmp( Species_getId         (sp), "Glucose_6_P" ), NULL );
  fail_unless( !strcmp( Species_getCompartment(sp), "cell"        ), NULL );

  fail_unless( Species_getHasOnlySubstanceUnits(sp) == 0, NULL );
  fail_unless( Species_getBoundaryCondition    (sp) == 0, NULL );
  fail_unless( Species_getConstant             (sp) == 0, NULL );
}
END_TEST


START_TEST (test_element_Parameter)
{
  Parameter_t* p;

  const char* s = wrapSBML
  (
    "<parameter name='Km1' value='2.3' units='second'/>"
  );

    
  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumParameters(M) == 1, NULL );

  p = Model_getParameter(M, 0);

  fail_unless( !strcmp( Parameter_getName (p), "Km1"    ), NULL );
  fail_unless( !strcmp( Parameter_getUnits(p), "second" ), NULL );
  fail_unless( Parameter_getValue(p) == 2.3, NULL );

  fail_unless( Parameter_isSetValue(p) == 1, NULL );
}
END_TEST


START_TEST (test_element_Parameter_L2)
{
  Parameter_t* p;

  const char* s = wrapSBML
  (
    "<parameter id='T' value='4.6' units='Celsius' constant='false'/>"
  );

    
  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumParameters(M) == 1, NULL );

  p = Model_getParameter(M, 0);

  fail_unless(  Parameter_isSetId   (p), NULL );
  fail_unless( !Parameter_isSetName (p), NULL );
  fail_unless(  Parameter_isSetValue(p), NULL );
  fail_unless(  Parameter_isSetUnits(p), NULL );

  fail_unless( !strcmp( Parameter_getId   (p), "T"       ), NULL );
  fail_unless( !strcmp( Parameter_getUnits(p), "Celsius" ), NULL );

  fail_unless( Parameter_getValue   (p) == 4.6, NULL );
  fail_unless( Parameter_getConstant(p) == 0  , NULL );
}
END_TEST


START_TEST (test_element_Parameter_L2_defaults)
{
  Parameter_t* p;

  const char* s = wrapSBML("<parameter id='x'/>");

    
  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumParameters(M) == 1, NULL );

  p = Model_getParameter(M, 0);

  fail_unless(  Parameter_isSetId   (p), NULL );
  fail_unless( !Parameter_isSetName (p), NULL );
  fail_unless( !Parameter_isSetValue(p), NULL );
  fail_unless( !Parameter_isSetUnits(p), NULL );

  fail_unless( !strcmp(Parameter_getId(p), "x"), NULL );
  fail_unless( Parameter_getConstant(p) == 1, NULL );
}
END_TEST


START_TEST (test_element_Reaction)
{
  Reaction_t* r;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);
  
  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible(r) == 0, NULL );
  fail_unless( Reaction_getFast      (r) == 0, NULL );
}
END_TEST


START_TEST (test_element_Reaction_defaults)
{
  Reaction_t* r;
  const char* s = wrapSBML("<reaction name='reaction_1'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);
  
  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible(r) != 0, NULL );
  fail_unless( Reaction_getFast      (r) == 0, NULL );
}
END_TEST


START_TEST (test_element_Reaction_L2)
{
  Reaction_t* r;

  const char* s = wrapSBML
  (
    "<reaction id='r1' reversible='false' fast='false'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);
  
  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless(  Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );
  fail_unless(  Reaction_isSetFast(r), NULL );

  fail_unless( !strcmp( Reaction_getId(r), "r1"), NULL );
  fail_unless( Reaction_getReversible(r) == 0, NULL );
  fail_unless( Reaction_getFast(r)       == 0, NULL );
}
END_TEST


START_TEST (test_element_Reaction_L2_defaults)
{
  Reaction_t* r;
  const char* s = wrapSBML("<reaction id='r1'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);
  
  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless(  Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );
  fail_unless( !Reaction_isSetFast(r), NULL );

  fail_unless( !strcmp( Reaction_getId(r), "r1"), NULL );
  fail_unless( Reaction_getReversible(r) == 1, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_Reactant)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <specieReference specie='X0' stoichiometry='1'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible  (r) == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X0"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_Product)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfProducts>"
    "    <specieReference specie='S1' stoichiometry='1'/>"
    "  </listOfProducts>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible (r) == 0, NULL );
  fail_unless( Reaction_getNumProducts(r) == 1, NULL );

  sr = Reaction_getProduct(r, 0);

  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "S1"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_defaults)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <specieReference specie='X0'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible  (r) == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X0"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );
}
END_TEST


START_TEST (test_element_SpeciesReference_defaults)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <speciesReference species='X0'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );
  
  r = Model_getReaction(M, 0);

  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible  (r) == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X0"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );
}
END_TEST


START_TEST (test_element_SpeciesReference_StoichiometryMath_1)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;
  const ASTNode_t*    math;
  char*               formula;

  const char* s = wrapSBML
  (
    "<reaction name='r1'>"
    "  <listOfReactants>"
    "    <speciesReference species='X0'>"
    "      <stoichiometryMath>"
    "        <math> <ci> x </ci> </math>"
    "      </stoichiometryMath>"
    "    </speciesReference>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );

  r = Model_getReaction(M, 0);
  fail_unless( r != NULL, NULL );

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( sr != NULL, NULL );

  fail_unless( SpeciesReference_isSetStoichiometryMath(sr), NULL );
  math = SpeciesReference_getStoichiometryMath(sr);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "x"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_element_SpeciesReference_StoichiometryMath_2)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='r1'>"
    "  <listOfReactants>"
    "    <speciesReference species='X0'>"
    "      <stoichiometryMath>"
    "        <math> <cn type='rational'> 3 <sep/> 2 </cn> </math>"
    "      </stoichiometryMath>"
    "    </speciesReference>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );

  r = Model_getReaction(M, 0);
  fail_unless( r != NULL, NULL );

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( sr != NULL, NULL );

  fail_unless( !SpeciesReference_isSetStoichiometryMath(sr), NULL );

  fail_unless( SpeciesReference_getStoichiometry(sr) == 3, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 2, NULL );
}
END_TEST


START_TEST (test_element_KineticLaw)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'/>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );

  r  = Model_getReaction(M, 0);
  kl = Reaction_getKineticLaw(r);

  fail_unless( !strcmp(KineticLaw_getFormula(kl), "k1*X0"), NULL );
}
END_TEST


START_TEST (test_element_KineticLaw_L2)
{
  Reaction_t*      r;
  KineticLaw_t*    kl;
  const ASTNode_t* math;
  const char*      formula;

  const char* s = wrapSBML2
  (
    "<reaction id='J1'>"
    "  <kineticLaw>"
    "    <math>"
    "      <apply>"
    "        <times/>"
    "        <ci> k  </ci>"
    "        <ci> S2 </ci>"
    "        <ci> X0 </ci>"
    "      </apply>"
    "    </math>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );

  r = Model_getReaction(M, 0);
  fail_unless( r != NULL, NULL );

  kl = Reaction_getKineticLaw(r);
  fail_unless( kl != NULL, NULL );

  fail_unless( KineticLaw_isSetMath(kl), NULL );
  math = KineticLaw_getMath(kl);

  formula = KineticLaw_getFormula(kl);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "k * S2 * X0"), NULL );
}
END_TEST


START_TEST (test_element_KineticLaw_Parameter)
{
  Reaction_t*   r;
  KineticLaw_t* kl;
  Parameter_t*  p;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumReactions(M) == 1, NULL );

  r  = Model_getReaction(M, 0);
  kl = Reaction_getKineticLaw(r);

  fail_unless( !strcmp(KineticLaw_getFormula(kl), "k1*X0"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);

  fail_unless( !strcmp(Parameter_getName(p), "k1"), NULL );
  fail_unless( Parameter_getValue(p) == 0, NULL );
}
END_TEST


START_TEST (test_element_AssignmentRule)
{
  AssignmentRule_t* ar;
  const ASTNode_t*  math;
  const char*       formula;

  const char *s = wrapSBML2
  (
    "<assignmentRule variable='k'>"
    "  <math>"
    "    <apply>"
    "      <divide/>"
    "      <ci> k3 </ci>"
    "      <ci> k2 </ci>"
    "    </apply>"
    "  </math>"
    "</assignmentRule>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  ar = (AssignmentRule_t *) Model_getRule(M, 0);
  fail_unless( ar != NULL, NULL );

  fail_unless( Rule_isSetMath((Rule_t *) ar), NULL );
  math = Rule_getMath((Rule_t *) ar);

  formula = Rule_getFormula((Rule_t *) ar);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "k3 / k2"), NULL );
}
END_TEST


START_TEST (test_element_RateRule)
{
  RateRule_t*      rr;
  const ASTNode_t* math;
  const char*      formula;

  const char *s = wrapSBML2
  (
    "<rateRule variable='x'>"
    "  <math>"
    "    <apply>"
    "      <times/>"
    "      <apply>"
    "        <minus/>"
    "        <cn> 1 </cn>"
    "        <ci> x </ci>"
    "      </apply>"
    "      <apply>"
    "        <ln/>"
    "        <ci> x </ci>"
    "      </apply>"
    "    </apply>"
    "  </math>"
    "</rateRule>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  rr = (RateRule_t *) Model_getRule(M, 0);
  fail_unless( rr != NULL, NULL );

  fail_unless( Rule_isSetMath((Rule_t *) rr), NULL );
  math = Rule_getMath((Rule_t *) rr);

  formula = Rule_getFormula((Rule_t *) rr);
  fail_unless( formula != NULL, NULL );

  /**
   * In L1 formula syntax, the natural log (ln) is defined to be log.
   */
  fail_unless( !strcmp(formula, "(1 - x) * log(x)"), NULL );
}
END_TEST


START_TEST (test_element_AlgebraicRule)
{
  AlgebraicRule_t *ar;

  const char *s = wrapSBML("<algebraicRule formula='x + 1'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  ar = (AlgebraicRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp(Rule_getFormula(ar), "x + 1"), NULL );
}
END_TEST


START_TEST (test_element_AlgebraicRule_L2)
{
  AlgebraicRule_t* ar;
  const ASTNode_t* math;
  const char*      formula;

  const char *s = wrapSBML2
  (
    "<algebraicRule>"
    "  <math>"
    "    <apply>"
    "      <minus/>"
    "      <apply>"
    "        <plus/>"
    "          <ci> S1 </ci>"
    "          <ci> S2 </ci>"
    "      </apply>"
    "      <ci> T </ci>"
    "    </apply>"
    "  </math>"
    "</algebraicRule>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  ar = (AlgebraicRule_t *) Model_getRule(M, 0);
  fail_unless( ar != NULL, NULL );

  fail_unless( Rule_isSetMath((Rule_t *) ar), NULL );
  math = Rule_getMath((Rule_t *) ar);

  formula = Rule_getFormula((Rule_t *) ar);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "S1 + S2 - T"), NULL );
}
END_TEST


START_TEST (test_element_CompartmentVolumeRule)
{
  CompartmentVolumeRule_t *cvr;

  const char *s = wrapSBML
  (
    "<compartmentVolumeRule compartment='A' formula='0.10 * t'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  cvr = (CompartmentVolumeRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp( CompartmentVolumeRule_getCompartment(cvr), "A"), NULL );
  fail_unless( !strcmp( Rule_getFormula(cvr), "0.10 * t" ), NULL );

  fail_unless( AssignmentRule_getType(cvr) == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_ParameterRule)
{
  ParameterRule_t *pr;

  const char *s = wrapSBML
  (
    "<parameterRule name='k' formula='k3/k2'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  pr = (ParameterRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp( ParameterRule_getName(pr), "k"), NULL );
  fail_unless( !strcmp( Rule_getFormula(pr), "k3/k2" ), NULL );
  fail_unless( AssignmentRule_getType(pr) == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_SpecieConcentrationRule)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<specieConcentrationRule specie='s2' formula='k * t/(1 + k)'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp( SpeciesConcentrationRule_getSpecies(scr), "s2"), NULL );
  fail_unless( !strcmp( Rule_getFormula(scr), "k * t/(1 + k)" ), NULL );
  fail_unless( AssignmentRule_getType(scr) == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_SpecieConcentrationRule_rate)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<specieConcentrationRule specie='s2' formula='k * t/(1 + k)' type='rate'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp( SpeciesConcentrationRule_getSpecies(scr), "s2"), NULL );
  fail_unless( !strcmp( Rule_getFormula(scr), "k * t/(1 + k)" ), NULL );
  fail_unless( AssignmentRule_getType(scr) == RULE_TYPE_RATE, NULL );
}
END_TEST


START_TEST (test_element_SpeciesConcentrationRule)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<speciesConcentrationRule species='s2' formula='k * t/(1 + k)'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(M, 0);

  fail_unless( !strcmp( SpeciesConcentrationRule_getSpecies(scr), "s2"), NULL );
  fail_unless( !strcmp( Rule_getFormula(scr), "k * t/(1 + k)" ), NULL );
  fail_unless( AssignmentRule_getType(scr) == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_Event)
{
  Event_t* e;

  const char* s = wrapSBML2("<event id='e1' name='MyEvent' timeUnits='time'/>");


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumEvents(M) == 1, NULL );

  e = Model_getEvent(M, 0);
  fail_unless( e != NULL, NULL );

  fail_unless(  Event_isSetId       (e), NULL );
  fail_unless(  Event_isSetName     (e), NULL );
  fail_unless(  Event_isSetTimeUnits(e), NULL );
  fail_unless( !Event_isSetTrigger  (e), NULL );
  fail_unless( !Event_isSetDelay    (e), NULL );

  fail_unless( !strcmp( Event_getId       (e), "e1"      ), NULL );
  fail_unless( !strcmp( Event_getName     (e), "MyEvent" ), NULL );
  fail_unless( !strcmp( Event_getTimeUnits(e), "time"    ), NULL );
}
END_TEST


START_TEST (test_element_Event_trigger)
{
  Event_t*         e;
  const ASTNode_t* math;
  char*            formula;

  const char* s = wrapSBML2
  (
    "<event>"
    "  <trigger>"
    "    <math>"
    "      <apply>"
    "        <leq/>"
    "        <ci> P1 </ci>"
    "        <ci> t  </ci>"
    "      </apply>"
    "    </math>"
    " </trigger>"
    "</event>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumEvents(M) == 1, NULL );

  e = Model_getEvent(M, 0);
  fail_unless( e != NULL, NULL );

  fail_unless( !Event_isSetDelay  (e), NULL );
  fail_unless(  Event_isSetTrigger(e), NULL );

  math = Event_getTrigger(e);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "leq(P1, t)"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_element_Event_delay)
{
  Event_t*         e;
  const ASTNode_t* math;
  char*            formula;

  const char* s = wrapSBML2
  (
    "<event> <delay> <math> <cn> 5 </cn> </math> </delay> </event>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumEvents(M) == 1, NULL );

  e = Model_getEvent(M, 0);
  fail_unless( e != NULL, NULL );

  fail_unless(  Event_isSetDelay  (e), NULL );
  fail_unless( !Event_isSetTrigger(e), NULL );

  math = Event_getDelay(e);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp(formula, "5"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_element_EventAssignment)
{
  Event_t*           e;
  EventAssignment_t* ea;
  const ASTNode_t*   math;
  char*              formula;

  const char* s = wrapSBML2
  (
    "<event>"
    "  <listOfEventAssignments>"
    "    <eventAssignment variable='k2'>"
    "      <math> <cn> 0 </cn> </math>"
    "    </eventAssignment>"
    "  </listOfEventAssignments>"
    "</event>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumEvents(M) == 1, NULL );

  e = Model_getEvent(M, 0);
  fail_unless( e != NULL, NULL );

  fail_unless( Event_getNumEventAssignments(e) == 1, NULL );

  ea = Event_getEventAssignment(e, 0);
  fail_unless( ea != NULL, NULL );

  fail_unless( EventAssignment_isSetVariable(ea), NULL );
  fail_unless( !strcmp(EventAssignment_getVariable(ea), "k2"), NULL );

  fail_unless( EventAssignment_isSetMath(ea), NULL );
  math = EventAssignment_getMath(ea);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL, NULL );

  fail_unless( !strcmp( formula, "0"), NULL );

  safe_free(formula);
}
END_TEST


START_TEST (test_element_metaid)
{
  SBase_t*  sb;

  const char* s = wrapSBML2
  (
    "<functionDefinition metaid='fd'/>"
    "<unitDefinition     metaid='ud'/>"
    "<compartment        metaid='c'/>"
    "<species            metaid='s'/>"
    "<parameter          metaid='p'/>"
    "<rateRule           metaid='rr'/>"
    "<reaction           metaid='rx'/>"
    "<event              metaid='e'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  sb = (SBase_t *) Model_getFunctionDefinition(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "fd"), NULL );


  sb = (SBase_t *) Model_getUnitDefinition(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "ud"), NULL );


  sb = (SBase_t *) Model_getCompartment(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "c"), NULL );


  sb = (SBase_t *) Model_getSpecies(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "s"), NULL );


  sb = (SBase_t *) Model_getParameter(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "p"), NULL );


  sb = (SBase_t *) Model_getRule(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "rr"), NULL );


  sb = (SBase_t *) Model_getReaction(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "rx"), NULL );


  sb = (SBase_t *) Model_getEvent(M, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "e"), NULL );
}
END_TEST


START_TEST (test_element_metaid_Unit)
{
  SBase_t*          sb;
  UnitDefinition_t* ud;

  const char* s = wrapSBML2
  (
    "<unitDefinition metaid='ud'>"
    "  <listOfUnits metaid='lou'>"
    "    <unit metaid='u'/>"
    "  </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  ud = Model_getUnitDefinition(M, 0);
  sb = (SBase_t *) ud;

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "ud"), NULL );


  sb = (SBase_t *) UnitDefinition_getListOfUnits(ud);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lou"), NULL );


  sb = (SBase_t *) UnitDefinition_getUnit(ud, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "u"), NULL );
}
END_TEST


START_TEST (test_element_metaid_Reaction)
{
  SBase_t*    sb;
  Reaction_t* r;

  const char* s = wrapSBML2
  (
    "<reaction metaid='r'>"
    "  <listOfReactants metaid='lor'>"
    "    <speciesReference metaid='sr1'/>"
    "  </listOfReactants>"
    "  <listOfProducts metaid='lop'>"
    "    <speciesReference metaid='sr2'/>"
    "  </listOfProducts>"
    "  <listOfModifiers metaid='lom'>"
    "    <modifierSpeciesReference metaid='msr'/>"
    "  </listOfModifiers>"
    "  <kineticLaw metaid='kl'/>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  r  = Model_getReaction(M, 0);
  sb = (SBase_t *) r;

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "r"), NULL );


  sb = (SBase_t *) Reaction_getListOfReactants(r);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lor"), NULL );


  sb = (SBase_t *) Reaction_getReactant(r, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "sr1"), NULL );


  sb = (SBase_t *) Reaction_getListOfProducts(r);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lop"), NULL );


  sb = (SBase_t *) Reaction_getProduct(r, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "sr2"), NULL );


  sb = (SBase_t *) Reaction_getListOfModifiers(r);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lom"), NULL );


  sb = (SBase_t *) Reaction_getModifier(r, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "msr"), NULL );


  sb = (SBase_t *) Reaction_getKineticLaw(r);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "kl"), NULL );
}
END_TEST


START_TEST (test_element_metaid_Event)
{
  SBase_t* sb;
  Event_t* e;

  const char* s = wrapSBML2
  (
    "<event metaid='e'>"
    "  <listOfEventAssignments metaid='loea'>"
    "    <eventAssignment metaid='ea'/>"
    "  </listOfEventAssignments>"
    "</event>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );


  e  = Model_getEvent(M, 0);
  sb = (SBase_t *) e;

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "e"), NULL );


  sb = (SBase_t *) Event_getListOfEventAssignments(e);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "loea"), NULL );


  sb = (SBase_t *) Event_getEventAssignment(e, 0);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "ea"), NULL );
}
END_TEST


START_TEST (test_element_metaid_ListOf)
{
  SBase_t*  sb;

  const char* s = wrapSBML2
  (
    "<listOfFunctionDefinitions metaid='lofd'/>"
    "<listOfUnitDefinitions     metaid='loud'/>"
    "<listOfCompartments        metaid='loc'/>"
    "<listOfSpecies             metaid='los'/>"
    "<listOfParameters          metaid='lop'/>"
    "<listOfRules               metaid='lor'/>"
    "<listOfReactions           metaid='lorx'/>"
    "<listOfEvents              metaid='loe'/>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  sb = (SBase_t *) Model_getListOfFunctionDefinitions(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lofd"), NULL );


  sb = (SBase_t *) Model_getListOfUnitDefinitions(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "loud"), NULL );


  sb = (SBase_t *) Model_getListOfCompartments(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "loc"), NULL );


  sb = (SBase_t *) Model_getListOfSpecies(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "los"), NULL );


  sb = (SBase_t *) Model_getListOfParameters(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lop"), NULL );


  sb = (SBase_t *) Model_getListOfRules(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lor"), NULL );


  sb = (SBase_t *) Model_getListOfReactions(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "lorx"), NULL );


  sb = (SBase_t *) Model_getListOfEvents(M);

  fail_unless( SBase_isSetMetaId(sb), NULL );
  fail_unless( !strcmp(SBase_getMetaId(sb), "loe"), NULL );
}
END_TEST


START_TEST (test_element_notes)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <notes>This is a test note.</notes>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  r  = Model_getReaction(M, 0);
  kl = Reaction_getKineticLaw(r);

  fail_unless( SBase_getNotes(kl) != NULL, NULL );
  fail_unless( !strcmp(SBase_getNotes(kl), "This is a test note."), NULL );
}
END_TEST


START_TEST (test_element_notes_after)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "    <notes>This note is <b>after</b> everything else.</notes>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  r  = Model_getReaction(M, 0);

  kl = Reaction_getKineticLaw(r);

  fail_unless( SBase_getNotes(kl) != NULL, NULL );
  fail_unless( !strcmp(SBase_getNotes(kl),
                       "This note is <b>after</b> everything else."), NULL );
}
END_TEST


START_TEST (test_element_notes_xmlns)
{
  const char* n =
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some text... </body>";

  const char* s = wrapSBML
  (
    "<notes>"
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some text... </body>"
    "</notes>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getNotes(M), n), NULL );
}
END_TEST


START_TEST (test_element_notes_entity_reference)
{
  const char* n =
#ifdef USE_EXPAT
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some\xc2\xa0text... "
    "</body>";
#else
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some&#xA0;text... "
    "</body>";
#endif

  const char* s = wrapSBML
  (
    "<notes>"
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some&#xA0;text... </body>"
    "</notes>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getNotes(M), n), NULL );
}
END_TEST


START_TEST (test_element_notes_nested)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* n = "This is a test <notes>nested</notes> note.";

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <notes>This is a test <notes>nested</notes> note.</notes>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  r  = Model_getReaction(M, 0);

  kl = Reaction_getKineticLaw(r);

  fail_unless( !strcmp(SBase_getNotes(kl), n), NULL );
  fail_unless( SBMLDocument_getNumWarnings(D) == 1, NULL );
}
END_TEST


START_TEST (test_element_notes_sbml)
{
  const char* n =
    "Notes are not allowed as part of the SBML element.";

  const char* s = wrapXML
  (
    "<sbml level='1' version='1'>"
    "  <notes>Notes are not allowed as part of the SBML element.</notes>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getNotes(D), n), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 1, NULL );
}
END_TEST


START_TEST (test_element_notes_sbml_L2)
{
  const char* n =
    "Notes *are* allowed as part of the SBML element in L2.";

  const char* s = wrapXML
  (
    "<sbml level='2' version='1'>"
    "  <notes>Notes *are* allowed as part of the SBML element in L2.</notes>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getNotes(D), n), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 0, NULL );
}
END_TEST


START_TEST (test_element_notes_ListOf)
{
  SBase_t*  sb;

  const char* s = wrapSBML2
  (
    "<listOfFunctionDefinitions>"
    "  <notes> My Functions </notes>"
    "</listOfFunctionDefinitions>"

    "<listOfUnitDefinitions>"
    "  <notes> My Units </notes>"
    "</listOfUnitDefinitions>"

    "<listOfCompartments>"
    "  <notes> My Compartments </notes>"
    "</listOfCompartments>"

    "<listOfSpecies>"
    "  <notes> My Species </notes>"
    "</listOfSpecies>"

    "<listOfParameters>"
    "  <notes> My Parameters </notes>"
    "</listOfParameters>"

    "<listOfRules>"
    "  <notes> My Rules </notes>"
    "</listOfRules>"

    "<listOfReactions>"
    "  <notes> My Reactions </notes>"
    "</listOfReactions>"

    "<listOfEvents>"
    "  <notes> My Events </notes>"
    "</listOfEvents>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  sb = (SBase_t *) Model_getListOfFunctionDefinitions(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Functions "), NULL );


  sb = (SBase_t *) Model_getListOfUnitDefinitions(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Units "), NULL );


  sb = (SBase_t *) Model_getListOfCompartments(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Compartments "), NULL );


  sb = (SBase_t *) Model_getListOfSpecies(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Species "), NULL );


  sb = (SBase_t *) Model_getListOfParameters(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Parameters "), NULL );


  sb = (SBase_t *) Model_getListOfRules(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Rules "), NULL );


  sb = (SBase_t *) Model_getListOfReactions(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Reactions "), NULL );


  sb = (SBase_t *) Model_getListOfEvents(M);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Events "), NULL );

}
END_TEST


START_TEST (test_element_notes_ListOf_Units)
{
  UnitDefinition_t* ud;
  SBase_t*          sb;

  const char* s = wrapSBML2
  (
    "<unitDefinition>"
    "  <listOfUnits>"
    "    <notes> My Units </notes>"
    "  </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  ud = Model_getUnitDefinition(M, 0);
  sb = (SBase_t *) UnitDefinition_getListOfUnits(ud);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Units "), NULL );
}
END_TEST


START_TEST (test_element_notes_ListOf_Reactions)
{
  Reaction_t*   r;
  KineticLaw_t* kl;
  SBase_t*      sb;

  const char* s = wrapSBML2
  (
    "<reaction>"
    "  <listOfReactants>"
    "    <notes> My Reactants </notes>"
    "  </listOfReactants>"
    "  <listOfProducts>"
    "    <notes> My Products </notes>"
    "  </listOfProducts>"
    "  <listOfModifiers>"
    "    <notes> My Modifiers </notes>"
    "  </listOfModifiers>"
    "  <kineticLaw>"
    "    <listOfParameters>"
    "    <notes> My Parameters </notes>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  r = Model_getReaction(M, 0);

  sb = (SBase_t *) Reaction_getListOfReactants(r);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Reactants "), NULL );


  sb = (SBase_t *) Reaction_getListOfProducts(r);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Products "), NULL );

  sb = (SBase_t *) Reaction_getListOfModifiers(r);

  fail_unless( !strcmp(SBase_getNotes(sb), " My Modifiers "), NULL );

  kl = Reaction_getKineticLaw(r);
  sb = (SBase_t *) KineticLaw_getListOfParameters(kl);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Parameters "), NULL );
}
END_TEST


START_TEST (test_element_notes_ListOf_EventAssignments)
{
  Event_t*  e;
  SBase_t*  sb;

  const char* s = wrapSBML2
  (
    "<event>"
    "  <listOfEventAssignments>"
    "  <notes> My Assignments </notes>"
    "  </listOfAssignments>"
    "</event>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  e  = Model_getEvent(M, 0);
  sb = (SBase_t *) Event_getListOfEventAssignments(e);

  fail_unless( SBase_isSetNotes(sb), NULL );
  fail_unless( !strcmp(SBase_getNotes(sb), " My Assignments "), NULL );
}
END_TEST


START_TEST (test_element_annotation)
{
  const char* a =
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>";

  const char* s = wrapSBML
  (
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( SBase_getAnnotation(M) != NULL, NULL );
  fail_unless( !strcmp(SBase_getAnnotation(M), a), NULL );
}
END_TEST


START_TEST (test_element_annotations)
{
  const char* a =
    "<annotations xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotations>";

  const char* s = wrapSBML
  (
    "<annotations xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotations>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( SBase_getAnnotation(M) != NULL, NULL );
  fail_unless( !strcmp(SBase_getAnnotation(M), a), NULL );
}
END_TEST


START_TEST (test_element_annotation_after)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* a =
    "<annotation xmlns:vc=\"http://www.sbml.org/2001/ns/vcell\">"
    "      <vc:modifiers vc:name=\"k1\"></vc:modifiers>"
    "    </annotation>";

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "    <annotation xmlns:vc=\"http://www.sbml.org/2001/ns/vcell\">"
    "      <vc:modifiers vc:name=\"k1\"/>"
    "    </annotation>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  r  = Model_getReaction(M, 0);
  kl = Reaction_getKineticLaw(r);

  fail_unless( !strcmp(SBase_getAnnotation(kl), a), NULL );
}
END_TEST


START_TEST (test_element_annotation_nested)
{
  const char* a =
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <annotation type=\"bogus\">"
    "    Why would you want to nest annotations?"
    "  </annotation>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>";

  const char* s = wrapSBML
  (
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <annotation type=\"bogus\">"
    "    Why would you want to nest annotations?"
    "  </annotation>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getAnnotation(M), a), NULL );
}
END_TEST


START_TEST (test_element_annotation_sbml)
{
  const char* a =
    "<annotation xmlns:jd=\"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion=\"1.0\"></VersionHeader>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor=\"15728639\">"
    "</SBMLGraphicsHeader>"
    "    </jd:display>"
    "  </annotation>";

  const char* s = wrapXML
  (
    "<sbml level=\"1\" version=\"1\">"
    "  <annotation xmlns:jd = \"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion = \"1.0\"/>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor = \"15728639\"/>"
    "    </jd:display>"
    "  </annotation>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getAnnotation(D), a), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 1, NULL );
}
END_TEST


START_TEST (test_element_annotation_sbml_L2)
{
  const char* a =
    "<annotation xmlns:jd=\"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion=\"1.0\"></VersionHeader>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor=\"15728639\">"
    "</SBMLGraphicsHeader>"
    "    </jd:display>"
    "  </annotation>";

  const char* s = wrapXML
  (
    "<sbml level=\"2\" version=\"1\">"
    "  <annotation xmlns:jd = \"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion = \"1.0\"/>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor = \"15728639\"/>"
    "    </jd:display>"
    "  </annotation>"
    "</sbml>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( !strcmp(SBase_getAnnotation(D), a), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 0, NULL );
}
END_TEST


START_TEST (test_element_line_col_numbers)
{
  SBase_t*  sb;

  const char* s =
    "<?xml version='1.0' encoding='UTF-8'?>\n"
    "<sbml level='1' version='1'>\n"
    "  <model name='testModel'>\n"
    "    <listOfReactions> <reaction/> </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n";

  /*          1         2         3         4         5         6 */
  /* 123456789012345678901234567890123456789012345678901234567890 */

  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( M != NULL, NULL );

  /**
   * Xerces-C++ and Expat report line and column numbers differently.
   *
   * Expat reports the line and column numbers at the start of a token
   * (like every other parser program in the world) while Xerces reports
   * the end of a token (!?).
   *
   * I thought turning source offset calculation on would fix this, e.g.:
   *
   *   XMLReader.setFeature( XMLUni::fgXercesCalculateSrcOfs, true );
   *
   * but it has no effect.  Perhaps I misunderstood its meaning. :(
   */

  sb = (SBase_t *) M;

#if USE_EXPAT
  fail_unless ( SBase_getLine  (sb) == 3, NULL );
  fail_unless ( SBase_getColumn(sb) == 2, NULL );
#else
  fail_unless ( SBase_getLine  (sb) ==  3, NULL );
  fail_unless ( SBase_getColumn(sb) == 27, NULL );
#endif


  sb = (SBase_t *) Model_getListOfReactions(M);

#if USE_EXPAT
  fail_unless ( SBase_getLine  (sb) == 4, NULL );
  fail_unless ( SBase_getColumn(sb) == 4, NULL );
#else
  fail_unless ( SBase_getLine  (sb) ==  4, NULL );
  fail_unless ( SBase_getColumn(sb) == 22, NULL );
#endif


  sb = (SBase_t *) Model_getReaction(M, 0);

#if USE_EXPAT
  fail_unless ( SBase_getLine  (sb) ==  4, NULL );
  fail_unless ( SBase_getColumn(sb) == 22, NULL );
#else
  fail_unless ( SBase_getLine  (sb) ==  4, NULL );
  fail_unless ( SBase_getColumn(sb) == 34, NULL );
#endif
}
END_TEST


/**
 * This test ensures the SBMLHandler does not invoke the MathML parser if
 * it encounters a <math> element inside <notes> or <annotation> elements.
 */
START_TEST (test_element_math_in_notes_bug)
{
  AssignmentRule_t* ar;

  const char *n =
    "    <math> <apply> <ci> fn </ci> </apply> </math>  ";

  const char *s = wrapSBML2
  (
    "<assignmentRule variable='x'>"
    "  <notes>"
    "    <math> <apply> <ci> fn </ci> </apply> </math>"
    "  </notes>"
    "</assignmentRule>"
  );


  D = readSBMLFromString(s);
  M = SBMLDocument_getModel(D);

  fail_unless( Model_getNumRules(M) == 1, NULL );

  ar = (AssignmentRule_t *) Model_getRule(M, 0);
  fail_unless( ar != NULL, NULL );

  fail_unless( !Rule_isSetMath ((Rule_t  *) ar), NULL );
  fail_unless( SBase_isSetNotes((SBase_t *) ar), NULL );

  fail_unless( !strcmp(SBase_getNotes((SBase_t *) ar), n), NULL );
}
END_TEST


Suite *
create_suite_SBMLHandler (void)
{
  Suite *suite = suite_create("SBMLHandler");
  TCase *tcase = tcase_create("SBMLHandler");


  tcase_add_checked_fixture( tcase,
                             SBMLHandlerTest_setup,
                             SBMLHandlerTest_teardown );

  tcase_add_test( tcase, test_element_SBML                                 );
  tcase_add_test( tcase, test_element_Model                                );
  tcase_add_test( tcase, test_element_Model_L2                             );
  tcase_add_test( tcase, test_element_FunctionDefinition                   );
  tcase_add_test( tcase, test_element_UnitDefinition                       );
  tcase_add_test( tcase, test_element_UnitDefinition_L2                    );
  tcase_add_test( tcase, test_element_Unit                                 );
  tcase_add_test( tcase, test_element_Unit_L2                              );
  tcase_add_test( tcase, test_element_Unit_defaults_L1_L2                  );
  tcase_add_test( tcase, test_element_Compartment                          );
  tcase_add_test( tcase, test_element_Compartment_L2                       );
  tcase_add_test( tcase, test_element_Compartment_defaults                 );
  tcase_add_test( tcase, test_element_Compartment_defaults_L2              );
  tcase_add_test( tcase, test_element_Specie                               );
  tcase_add_test( tcase, test_element_Specie_defaults                      );
  tcase_add_test( tcase, test_element_Species                              );
  tcase_add_test( tcase, test_element_Species_L2_1                         );
  tcase_add_test( tcase, test_element_Species_L2_2                         );
  tcase_add_test( tcase, test_element_Species_L2_defaults                  );
  tcase_add_test( tcase, test_element_Parameter                            );
  tcase_add_test( tcase, test_element_Parameter_L2                         );
  tcase_add_test( tcase, test_element_Parameter_L2_defaults                );
  tcase_add_test( tcase, test_element_Reaction                             );
  tcase_add_test( tcase, test_element_Reaction_defaults                    );
  tcase_add_test( tcase, test_element_Reaction_L2                          );
  tcase_add_test( tcase, test_element_Reaction_L2_defaults                 );
  tcase_add_test( tcase, test_element_SpecieReference_Reactant             );
  tcase_add_test( tcase, test_element_SpecieReference_Product              );
  tcase_add_test( tcase, test_element_SpecieReference_defaults             );
  tcase_add_test( tcase, test_element_SpeciesReference_defaults            );
  tcase_add_test( tcase, test_element_SpeciesReference_StoichiometryMath_1 );
  tcase_add_test( tcase, test_element_SpeciesReference_StoichiometryMath_2 );
  tcase_add_test( tcase, test_element_KineticLaw                           );
  tcase_add_test( tcase, test_element_KineticLaw_L2                        );
  tcase_add_test( tcase, test_element_KineticLaw_Parameter                 );
  tcase_add_test( tcase, test_element_AssignmentRule                       );
  tcase_add_test( tcase, test_element_RateRule                             );
  tcase_add_test( tcase, test_element_AlgebraicRule                        );
  tcase_add_test( tcase, test_element_AlgebraicRule_L2                     );
  tcase_add_test( tcase, test_element_CompartmentVolumeRule                );
  tcase_add_test( tcase, test_element_ParameterRule                        );
  tcase_add_test( tcase, test_element_SpecieConcentrationRule              );
  tcase_add_test( tcase, test_element_SpecieConcentrationRule_rate         );
  tcase_add_test( tcase, test_element_SpeciesConcentrationRule             );
  tcase_add_test( tcase, test_element_Event                                );
  tcase_add_test( tcase, test_element_Event_trigger                        );
  tcase_add_test( tcase, test_element_Event_delay                          );
  tcase_add_test( tcase, test_element_EventAssignment                      );
  tcase_add_test( tcase, test_element_metaid                               );
  tcase_add_test( tcase, test_element_metaid_Unit                          );
  tcase_add_test( tcase, test_element_metaid_Reaction                      );  
  tcase_add_test( tcase, test_element_metaid_Event                         );  
  tcase_add_test( tcase, test_element_metaid_ListOf                        );  
  tcase_add_test( tcase, test_element_notes                                );
  tcase_add_test( tcase, test_element_notes_after                          );
  tcase_add_test( tcase, test_element_notes_xmlns                          );
  tcase_add_test( tcase, test_element_notes_entity_reference               );
  tcase_add_test( tcase, test_element_notes_nested                         );
  tcase_add_test( tcase, test_element_notes_sbml                           );
  tcase_add_test( tcase, test_element_notes_sbml_L2                        );
  tcase_add_test( tcase, test_element_notes_ListOf                         );
  tcase_add_test( tcase, test_element_notes_ListOf_Units                   );
  tcase_add_test( tcase, test_element_notes_ListOf_Reactions               );
  tcase_add_test( tcase, test_element_notes_ListOf_EventAssignments        );
  tcase_add_test( tcase, test_element_annotation                           );
  tcase_add_test( tcase, test_element_annotations                          );
  tcase_add_test( tcase, test_element_annotation_after                     );
  tcase_add_test( tcase, test_element_annotation_nested                    );
  tcase_add_test( tcase, test_element_annotation_sbml                      );
  tcase_add_test( tcase, test_element_annotation_sbml_L2                   );
  tcase_add_test( tcase, test_element_line_col_numbers                     );
  tcase_add_test( tcase, test_element_math_in_notes_bug                    );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
