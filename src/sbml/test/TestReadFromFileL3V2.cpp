/**
 * \file    TestReadFromFileL3V2.cpp
 * \brief   Reads test-data/l3v2-all.xml into memory and tests it.
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


START_TEST (test_read_l3v2_all)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  Compartment*      c;
  Species*          s;
  Parameter*        p;
  AssignmentRule*   ar;
  Reaction*         r;
  SpeciesReference* sr;
  KineticLaw*       kl;
  UnitDefinition*   ud;
  Constraint*       con;
  Event*            e;
  Delay*            delay;
  Trigger*          trigger;
  EventAssignment*  ea;
  FunctionDefinition* fd;
  InitialAssignment* ia;
  AlgebraicRule*   alg;
  RateRule*        rr;
  Unit* u;
  ListOfEvents *loe;
  Event *e1;
  ListOfEventAssignments *loea;
  EventAssignment *ea1;
  ListOfFunctionDefinitions *lofd;
  FunctionDefinition * fd1;
  LocalParameter *lp;
  Priority * prior;
  
  const ASTNode*   ast;

  std::string filename(TestDataDirectory);
  filename += "l3v2-all.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-all.xml\") returned a NULL pointer.");
  }



  //
  // <sbml level="3" version="2" ...>
  //
  fail_unless( d->getLevel  () == 3, NULL );
  fail_unless( d->getVersion() == 2, NULL );


  //<model timeUnits="second" extentUnits="mole"
  //       volumeUnits="litre" areaUnits="metre" lengthUnits="metre"
  //       conversionFactor="p">
  m = d->getModel();
  fail_unless( m != NULL, NULL );

  fail_unless(m->isSetSubstanceUnits());
  fail_unless(m->isSetTimeUnits());
  fail_unless(m->isSetVolumeUnits());
  fail_unless(m->isSetLengthUnits());
  fail_unless(m->isSetAreaUnits());
  fail_unless(m->isSetExtentUnits());
  fail_unless(m->isSetConversionFactor());

  fail_unless(m->getSubstanceUnits() == "mole");
  fail_unless(m->getTimeUnits() == "second");
  fail_unless(m->getVolumeUnits() == "litre");
  fail_unless(m->getLengthUnits() == "metre");
  fail_unless(m->getAreaUnits() == "metre");
  fail_unless(m->getExtentUnits() == "mole");
  fail_unless(m->getConversionFactor() == "p");

   //<listOfUnitDefinitions>
   //  <unitDefinition id="nonsense">
   //     <listOfUnits>
   //        <unit kind="mole"   exponent="-1" scale="2" multiplier="1.3"/>
   //        <unit kind="litre"  exponent="1.5"  scale="10" multiplier="0.5"/>
   //        <unit kind="second" exponent="1" scale="0" multiplier="1"/>
   //     </listOfUnits>
   //  </unitDefinition>
   //</listOfUnitDefinitions>

  fail_unless(m->getNumUnitDefinitions() == 1);

  ud = m->getUnitDefinition(0);

  fail_unless(ud->getNumUnits() == 3);

  u = ud->getUnit(0);

  fail_unless(u->isSetExponent());
  fail_unless(u->isSetScale());
  fail_unless(u->isSetMultiplier());
  fail_unless(u->getExponent() == -1);
  fail_unless(u->getExponentAsDouble() == -1);
  fail_unless(u->getScale() == 2);
  fail_unless(u->getMultiplier() == 1.3);

  u = ud->getUnit(1);

  fail_unless(u->isSetExponent());
  fail_unless(u->isSetScale());
  fail_unless(u->isSetMultiplier());
  fail_unless(u->getExponentAsDouble() == 1.5);
  fail_unless(u->getScale() == 10);
  fail_unless(u->getMultiplier() == 0.5);

  u = ud->getUnit(2);

  fail_unless(u->isSetExponent());
  fail_unless(u->isSetScale());
  fail_unless(u->isSetMultiplier());
  fail_unless(u->getExponent() == 1);
  fail_unless(u->getScale() == 0);
  fail_unless(u->getMultiplier() == 1);


   //<listOfCompartments>
   //  <compartment id="cell" size="1e-14" spatialDimensions="3" units="litre" constant="true"/>
   // <compartment id="comp" spatialDimensions="4.6"/>
   //</listOfCompartments>
  fail_unless( m->getNumCompartments() == 2, NULL );

  c = m->getCompartment(0);
  fail_unless(c->isSetSize());
  fail_unless(c->isSetSpatialDimensions());
  fail_unless(c->isSetConstant());
  fail_unless(c->getId() == "cell");
  fail_unless(c->getSize() == 1e-14);
  fail_unless(c->getSpatialDimensions() == 3);
  fail_unless(c->getSpatialDimensionsAsDouble() == 3);
  fail_unless(c->getUnits() == "litre");
  fail_unless(c->getConstant() == true);

  c = m->getCompartment(1);

  fail_unless(!c->isSetSize());
  fail_unless(c->isSetSpatialDimensions());
  fail_unless(c->isSetConstant());
  fail_unless(c->getId() == "comp");
  fail_unless(c->getSpatialDimensionsAsDouble() == 4.6);
 
  //<listOfConstraints>
  //  <constraint>
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <lt/>
  //        <ci> x </ci>
  //        <cn type="integer"> 3 </cn>
  //      </apply>
  //    </math>
  //  </constraint>
  //</listOfConstraints>
  fail_unless( m->getNumConstraints() == 1, NULL );

  con = m->getConstraint(0);
  fail_unless( con         != NULL  , NULL );

  ast = con->getMath();
  char * formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "lt(x, 3)"), NULL);           
  safe_free(formula);

  //<event id="e1" sboTerm="SBO:0000231">
  //  <trigger sboTerm="SBO:0000231">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <lt/>
  //        <ci> x </ci>
  //        <cn type="integer"> 3 </cn>
  //      </apply>
  //    </math>
  //  </trigger>
  //  <delay sboTerm="SBO:0000064">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <plus/>
  //        <ci> x </ci>
  //        <cn type="integer"> 3 </cn>
  //      </apply>
  //    </math>
  //  </delay>
  //  <listOfEventAssignments>
  //    <eventAssignment variable="a" sboTerm="SBO:0000064">
  //      <math xmlns="http://www.w3.org/1998/Math/MathML">
  //        <apply>
  //          <times/>
  //          <ci> x </ci>
  //          <ci> p3 </ci>
  //        </apply>
  //      </math>
  //    </eventAssignment>
  //  </listOfEventAssignments>
  //</event>
  fail_unless( m->getNumEvents() == 1, NULL );

  e = m->getEvent(0);
  fail_unless(e != NULL, NULL);

  fail_unless(e->getId() == "e1", NULL);

  fail_unless(e->getSBOTerm() == 231, NULL);
  fail_unless(e->getSBOTermID() == "SBO:0000231");

  fail_unless(e->isSetDelay(), NULL);
  
  delay = e->getDelay();
  fail_unless(delay != NULL, NULL);

  fail_unless(delay->getSBOTerm() == 64, NULL);
  fail_unless(delay->getSBOTermID() == "SBO:0000064");

  ast = delay->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "p + 3"), NULL);
  safe_free(formula);

  fail_unless(e->isSetPriority(), NULL);
  
  prior = e->getPriority();
  fail_unless(prior != NULL, NULL);

  fail_unless(prior->getSBOTerm() == 64, NULL);
  fail_unless(prior->getSBOTermID() == "SBO:0000064");

  ast = prior->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "1"), NULL);
  safe_free(formula);

  fail_unless(e->isSetTrigger(), NULL);
  
  trigger = e->getTrigger();
  fail_unless(trigger != NULL, NULL);

  ast = trigger->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "true"), NULL);
  safe_free(formula);

  loe = m->getListOfEvents();
  e1 = loe->get(0);
  fail_unless( e1 == e);

  e1 = loe->get("e1");
  fail_unless( e1 == e);

  fail_unless( e->getNumEventAssignments() == 1, NULL );

  ea = e->getEventAssignment(0);
  fail_unless(ea != NULL, NULL);

  fail_unless(ea->getVariable() == "Keq", NULL);
  fail_unless(ea->getSBOTerm() == 64, NULL);
  fail_unless(ea->getSBOTermID() == "SBO:0000064");

  ast = ea->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "X0 * p"), NULL);
  safe_free(formula);

  loea = e->getListOfEventAssignments();
  ea1 = loea->get(0);
  fail_unless( ea1 == ea);

  ea1 = loea->get("Keq");
  fail_unless( ea1 == ea);

  //<listOfFunctionDefinitions>
  //  <functionDefinition id="fd" sboTerm="SBO:0000064">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <lambda>
  //        <bvar>
  //          <ci> x </ci>
  //        </bvar>
  //        <apply>
  //          <power/>
  //          <ci> x </ci>
  //          <cn type="integer"> 3 </cn>
  //        </apply>
  //      </lambda>
  //    </math>
  //  </functionDefinition>
  //</listOfFunctionDefinitions>

  fail_unless( m->getNumFunctionDefinitions() == 1, NULL );

  fd = m->getFunctionDefinition(0);
  fail_unless(fd != NULL, NULL);

  fail_unless(fd->getId() == "fd", NULL);

  fail_unless(fd->getSBOTerm() == 64, NULL);
  fail_unless(fd->getSBOTermID() == "SBO:0000064");

  ast = fd->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "lambda(x, pow(x, 3))"), NULL);
  safe_free(formula);

  lofd = m->getListOfFunctionDefinitions();
  fd1 = lofd->get(0);
  fail_unless( fd1 == fd);

  fd1 = lofd->get("fd");
  fail_unless( fd1 == fd);

  //<listOfInitialAssignments>
  //  <initialAssignment symbol="p1">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <times/>
  //        <ci> x </ci>
  //        <ci> p3 </ci>
  //      </apply>
  //    </math>
  //  </initialAssignment>
  //</listOfInitialAssignments>
  fail_unless( m->getNumInitialAssignments() == 1, NULL );

  ia = m->getInitialAssignment(0);
  fail_unless( ia         != NULL  , NULL );
  fail_unless(ia->getSymbol() == "p1", NULL);

  ast = ia->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "x * p3"), NULL);
  safe_free(formula);

  //<listOfRules>
  fail_unless( m->getNumRules() == 3, NULL );
  
  
  //  <algebraicRule sboTerm="SBO:0000064">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <power/>
  //        <ci> x </ci>
  //        <cn type="integer"> 3 </cn>
  //      </apply>
  //    </math>
  //  </algebraicRule>
  alg = static_cast<AlgebraicRule*>( m->getRule(0));

  fail_unless( alg         != NULL  , NULL );
  fail_unless(alg->getSBOTerm() == 64, NULL);
  fail_unless(alg->getSBOTermID() == "SBO:0000064");

  ast = alg->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "pow(x, 3)"), NULL);
  safe_free(formula);

  //  <assignmentRule variable="p2" sboTerm="SBO:0000064">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <times/>
  //        <ci> x </ci>
  //        <ci> p3 </ci>
  //      </apply>
  //    </math>
  //  </assignmentRule>
  ar = static_cast <AssignmentRule*>(m->getRule(1));

  fail_unless( ar         != NULL  , NULL );
  fail_unless( ar->getVariable() == "p2", NULL);
  fail_unless(ar->getSBOTerm() == 64, NULL);
  fail_unless(ar->getSBOTermID() == "SBO:0000064");

  ast = ar->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "x * p3"), NULL);
  safe_free(formula);


  //  <rateRule variable="p3" sboTerm="SBO:0000064">
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <divide/>
  //        <ci> p1 </ci>
  //        <ci> p </ci>
  //      </apply>
  //    </math>
  //  </rateRule>
  rr = static_cast<RateRule*> (m->getRule(2));

  fail_unless( rr         != NULL  , NULL );
  fail_unless( rr->getVariable() == "p3", NULL);
  fail_unless(rr->getSBOTerm() == 64, NULL);
  fail_unless(rr->getSBOTermID() == "SBO:0000064");

  ast = rr->getMath();
  formula = SBML_formulaToString(ast);
  fail_unless(!strcmp(formula, "p1 / p"), NULL);
  safe_free(formula);

  //<listOfSpecies>
  //  <species id="X0" compartment="comp" initialAmount="0" 
  //   substanceUnits="mole" hasOnlySubstanceUnits="false" 
  //   boundaryCondition="false" constant="false" conversionFactor="p"/>
  //  <species id="P" compartment="comp" initialAmount="0" 
  //   substanceUnits="mole" hasOnlySubstanceUnits="false" 
  //   boundaryCondition="false" constant="false" conversionFactor="p"/>
  //</listOfSpecies>

  fail_unless( m->getNumSpecies() == 2, NULL );

  s = m->getSpecies(0);
  fail_unless( s          != NULL  , NULL );
  fail_unless( s->getId() == "X0", NULL );
  fail_unless( s->getCompartment() == "comp", NULL );
  fail_unless(s->isSetConversionFactor());
  fail_unless(s->getConversionFactor() == "p");
  fail_unless(s->isSetBoundaryCondition());
  fail_unless(s->getBoundaryCondition() == false);
  fail_unless(s->isSetHasOnlySubstanceUnits());
  fail_unless(s->getHasOnlySubstanceUnits() == false);
  fail_unless(s->isSetSubstanceUnits());
  fail_unless(s->getSubstanceUnits() == "mole");
  fail_unless(s->isSetConstant());
  fail_unless(s->getConstant() == false);
  fail_unless(s->isSetInitialAmount());
  fail_unless(s->getInitialAmount() == 0);
  fail_unless(!s->isSetInitialConcentration());
  fail_unless(util_isNaN(s->getInitialConcentration()));

  //<listOfReactions>
  //<reaction id="in" reversible="false" fast="false" compartment="comp">
  //  <listOfReactants>
  //    <speciesReference species="X0" stoichiometry="1" constant="true"/>
  //  </listOfReactants>
  //  <listOfProducts>
  //    <speciesReference species="T" constant="false"/>
  //  </listOfProducts>
  //  <kineticLaw>
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <times/>
  //        <ci> k1 </ci>
  //        <ci> X0 </ci>
  //        <ci> cell </ci>
  //      </apply>
  //    </math>
  //    <listOfLocalParameters>
  //      <localParameter id="k1" value="0.1" units="per_second"/>
  //      <localParameter id="k2"/>
  //    </listOfLocalParameters>
  //  </kineticLaw>
  //</reaction>
  //</listOfReactions>
  fail_unless( m->getNumReactions() == 1, NULL );

  r = m->getReaction(0);

  fail_unless(!r->isSetFast());
  fail_unless(r->getFast() == false);
  fail_unless(r->isSetReversible());
  fail_unless(r->getReversible() == false);
  fail_unless(r->isSetCompartment());
  fail_unless(r->getCompartment() == "comp");

  sr = r->getReactant(0);

  fail_unless(sr->isSetConstant());
  fail_unless(sr->getConstant() == true);
  fail_unless(sr->isSetStoichiometry());
  fail_unless(sr->getStoichiometry() == 1);

  sr = r->getProduct(0);

  fail_unless(sr->isSetConstant());
  fail_unless(sr->getConstant() == false);
  fail_unless(!sr->isSetStoichiometry());
  fail_unless(util_isNaN(sr->getStoichiometry()));


  kl = r->getKineticLaw();

  fail_unless(kl->getNumLocalParameters() == 2);
  fail_unless(kl->getNumParameters() == 2);

  p = kl->getParameter(0);
  
  fail_unless(p->isSetUnits());
  fail_unless(p->getUnits() == "per_second");
  fail_unless(p->isSetValue());
  fail_unless(p->getValue() == 0.1);

  lp = kl->getLocalParameter(0);

  fail_unless(lp->isSetUnits());
  fail_unless(lp->getUnits() == "per_second");
  fail_unless(lp->isSetValue());
  fail_unless(lp->getValue() == 0.1);

  lp = kl->getLocalParameter(1);

  fail_unless(!lp->isSetUnits());
  fail_unless(lp->getUnits() == "");
  fail_unless(!lp->isSetValue());
  fail_unless(util_isNaN(lp->getValue()));



  delete d;
}
END_TEST


START_TEST(test_echo_l3v2_all)
{
  std::string filename(TestDataDirectory);
  filename += "l3v2-all.xml";

  std::string fileout(TestDataDirectory);
  fileout += "tempOut.xml";

  SBMLDocument* d = readSBML(filename.c_str());

  if (d == NULL)
  {
    fail("readSBML(\"l3v2-all.xml\") returned a NULL pointer.");
  }

  writeSBML(d, fileout.c_str());

  SBMLDocument* outD = readSBML(fileout.c_str());

  char * doc = d->toSBML();
  char * out = outD->toSBML();
  fail_unless( strcmp(doc, out) == 0 );

  safe_free(doc);
  safe_free(out);

  delete d;
  delete outD;
}
END_TEST

Suite *
create_suite_TestReadFromFileL3V2 (void)
{ 
  Suite *suite = suite_create("test-data/l3v2-all.xml");
  TCase *tcase = tcase_create("test-data/l3v2-all.xml");


  tcase_add_test(tcase, test_read_l3v2_all);
  tcase_add_test(tcase, test_echo_l3v2_all);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
