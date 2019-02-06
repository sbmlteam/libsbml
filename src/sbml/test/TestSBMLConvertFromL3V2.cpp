/**
 * \file    TestSBMLConvertFromL3V2.c
 * \brief   SBMLConvert unit tests for strict conversion from L3V2
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
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLTypes.h>

#include <check.h>



#include <sbml/common/extern.h>

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

START_TEST (test_SBMLConvertFromL3V2_convertMissingTrigger_strict)
{
  // add a dummy trigger that never fires
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Event *e = m->createEvent();
  e->setUseValuesFromTriggerTime(true);
  EventAssignment *ea = e->createEventAssignment();
  ea->setVariable("p");
  std::string math = "2";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ea->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(e->getLevel() == 3);
  fail_unless(e->getVersion() == 2);
  fail_unless(e->isSetTrigger() == false);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingTriggerElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getEvent(0)->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->isSetTrigger() == true);
  fail_unless(doc->getModel()->getEvent(0)->getTrigger()->isSetMath() == true);

  const ASTNode * node1 = doc->getModel()->getEvent(0)->getTrigger()->getMath();
  char * str = SBML_formulaToL3String(node1);

  fail_unless(!strcmp(str, "false"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingTriggerMath_strict)
{
  // add math to the trigger that never fires
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Event *e = m->createEvent();
  e->setUseValuesFromTriggerTime(true);
  Trigger *t = e->createTrigger();
  t->setPersistent(false);
  t->setInitialValue(false);
  EventAssignment *ea = e->createEventAssignment();
  ea->setVariable("p");
  std::string math = "2";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ea->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(e->getLevel() == 3);
  fail_unless(e->getVersion() == 2);
  fail_unless(e->isSetTrigger() == true);
  fail_unless(e->getTrigger()->isSetMath() == false);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingMathElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getEvent(0)->getVersion() == 1);

  fail_unless(doc->getModel()->getEvent(0)->isSetTrigger() == true);
  fail_unless(doc->getModel()->getEvent(0)->getTrigger()->isSetMath() == true);

  const ASTNode * node1 = doc->getModel()->getEvent(0)->getTrigger()->getMath();
  char * str = SBML_formulaToL3String(node1);

  fail_unless(!strcmp(str, "false"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingTrigger_nonstrict)
{
  // add a dummy trigger that never fires
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Event *e = m->createEvent();
  e->setUseValuesFromTriggerTime(true);
  EventAssignment *ea = e->createEventAssignment();
  ea->setVariable("p");
  std::string math = "2";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ea->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(e->getLevel() == 3);
  fail_unless(e->getVersion() == 2);
  fail_unless(e->isSetTrigger() == false);

  bool done = doc->setLevelAndVersion(3, 1, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingTriggerElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getEvent(0)->getVersion() == 1);

  fail_unless(doc->getModel()->getEvent(0)->isSetTrigger() == true);
  fail_unless(doc->getModel()->getEvent(0)->getTrigger()->isSetMath() == true);

  const ASTNode * node1 = doc->getModel()->getEvent(0)->getTrigger()->getMath();
  char * str = SBML_formulaToL3String(node1);

  fail_unless(!strcmp(str, "false"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingTriggerMath_nonstrict)
{
  // add math to the trigger that never fires
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Event *e = m->createEvent();
  e->setUseValuesFromTriggerTime(true);
  Trigger *t = e->createTrigger();
  t->setPersistent(false);
  t->setInitialValue(false);
  EventAssignment *ea = e->createEventAssignment();
  ea->setVariable("p");
  std::string math = "2";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ea->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(e->getLevel() == 3);
  fail_unless(e->getVersion() == 2);
  fail_unless(e->isSetTrigger() == true);
  fail_unless(e->getTrigger()->isSetMath() == false);

  bool done = doc->setLevelAndVersion(3, 1, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingMathElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getEvent(0)->getVersion() == 1);

  fail_unless(doc->getModel()->getEvent(0)->isSetTrigger() == true);
  fail_unless(doc->getModel()->getEvent(0)->getTrigger()->isSetMath() == true);

  const ASTNode * node1 = doc->getModel()->getEvent(0)->getTrigger()->getMath();
  char * str = SBML_formulaToL3String(node1);

  fail_unless(!strcmp(str, "false"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertnewMath_strict)
{
  // do not convert model with new math
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  AssignmentRule *ar = m->createAssignmentRule();
  ar->setVariable("p");
  std::string math = "min(2, 3)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == false);

  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(doc->getModel()->getRule(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getRule(0)->getVersion() == 2);

  fail_unless(doc->getModel()->getRule(0)->isSetMath() == true);
  fail_unless(doc->getModel()->getRule(0)->getMath()->usesL3V2MathConstructs() == true);

  const ASTNode * node1 = doc->getModel()->getRule(0)->getMath();
  char * str = SBML_formulaToL3String(node1);
  fail_unless(!strcmp(str, "min(2, 3)"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertnewMath_nonstrict)
{
  // convert the model but leave invalid new math
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  AssignmentRule *ar = m->createAssignmentRule();
  ar->setVariable("p");
  std::string math = "min(2, 3)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);

  bool done = doc->setLevelAndVersion(2, 4, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);
  fail_unless(doc->getModel()->getRule(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getRule(0)->getVersion() == 4);

  fail_unless(doc->getModel()->getRule(0)->isSetMath() == true);
  fail_unless(doc->getModel()->getRule(0)->getMath()->usesL3V2MathConstructs() == true);

  const ASTNode * node1 = doc->getModel()->getRule(0)->getMath();
  char * str = SBML_formulaToL3String(node1);
  fail_unless(!strcmp(str, "min(2, 3)"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertEmptyLO_strict)
{
  // remove list of with no children
  const char * expected =
    "<model>\n"
    "  <listOfParameters>\n"
    "    <annotation>some annotation</annotation>\n"
    "  </listOfParameters>\n"
    "</model>";

  const char * expected1 =
    "<model/>";

  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  ListOfParameters* lo = m->getListOfParameters();
  lo->setAnnotation("some annotation");

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(lo->getLevel() == 3);
  fail_unless(lo->getVersion() == 2);

  fail_unless(lo->isSetAnnotation() == true);

  char * sbml = m->toSBML();

  fail_unless(equals(expected, sbml));

  free(sbml);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(EmptyListOfElementNotSupported) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);
  fail_unless(doc->getModel()->getListOfParameters()->getLevel() == 2);
  fail_unless(doc->getModel()->getListOfParameters()->getVersion() == 4);

  Model* m1 = doc->getModel();

  sbml = m1->toSBML();

  fail_unless(equals(expected1, sbml));

  free(sbml);
  
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertEmptyLO_nonstrict)
{
  // remove list of with no children
  const char * expected =
    "<model>\n"
    "  <listOfParameters>\n"
    "    <annotation>some annotation</annotation>\n"
    "  </listOfParameters>\n"
    "</model>";

  const char * expected1 =
    "<model/>";

  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  ListOfParameters* lo = m->getListOfParameters();
  lo->setAnnotation("some annotation");

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(lo->getLevel() == 3);
  fail_unless(lo->getVersion() == 2);

  fail_unless(lo->isSetAnnotation() == true);

  char * sbml = m->toSBML();

  fail_unless(equals(expected, sbml));

  free(sbml);

  bool done = doc->setLevelAndVersion(2, 4, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(EmptyListOfElementNotSupported) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);
  fail_unless(doc->getModel()->getListOfParameters()->getLevel() == 2);
  fail_unless(doc->getModel()->getListOfParameters()->getVersion() == 4);

  Model* m1 = doc->getModel();

  sbml = m1->toSBML();

  fail_unless(equals(expected1, sbml));

  free(sbml);
  
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAnewMath_strict)
{
  // replace IA with new math by value
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "min(2, 3)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);
  fail_unless(ar->getMath()->usesRateOf() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 4);

  fail_unless(m->getNumInitialAssignments() == 0);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == true);
  fail_unless(util_isEqual(doc->getModel()->getParameter(0)->getValue(), 2.0));

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAnewMath_nonstrict)
{
  // replace IA with new math by value
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "min(2, 3)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);
  fail_unless(ar->getMath()->usesRateOf() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 4);

  fail_unless(m->getNumInitialAssignments() == 0);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == true);
  fail_unless(util_isEqual(doc->getModel()->getParameter(0)->getValue(), 2.0));

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAnewMath1_strict)
{
  // dont convert if IA has rateOf
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Parameter * p1 = m->createParameter();
  p1->setId("x");
  p1->setConstant(true);
  p1->setValue(1);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "rateOf(x)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);
  fail_unless(ar->getMath()->usesRateOf() == true);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == false);

  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 2);

  fail_unless(m->getNumInitialAssignments() == 1);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == false);

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAnewMath1_nonstrict)
{
  // convert the IA withh rateOf but leave invalid
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Parameter * p1 = m->createParameter();
  p1->setId("x");
  p1->setConstant(true);
  p1->setValue(1);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "rateOf(x)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == true);
  fail_unless(ar->getMath()->usesRateOf() == true);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 4);

  fail_unless(m->getNumInitialAssignments() == 1);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == false);

  const ASTNode * node1 = doc->getModel()->getInitialAssignment(0)->getMath();
  char * str = SBML_formulaToL3String(node1);
  fail_unless(!strcmp(str, "rateOf(x)"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIA_strict)
{
// convert IA to IA
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "2*3";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == false);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 4);

  fail_unless(m->getNumInitialAssignments() == 1);
  const ASTNode * node1 = doc->getModel()->getInitialAssignment(0)->getMath();
  char * str = SBML_formulaToL3String(node1);
  fail_unless(!strcmp(str, "2 * 3"));
  safe_free(str);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == false);

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIA_nonstrict)
{
  // convert IA to IA
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "2*3";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 4, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == false);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 4);

  fail_unless(m->getNumInitialAssignments() == 1);
  const ASTNode * node1 = doc->getModel()->getInitialAssignment(0)->getMath();
  char * str = SBML_formulaToL3String(node1);
  fail_unless(!strcmp(str, "2 * 3"));
  safe_free(str);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == false);

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAvalue_strict)
{
  // convert IA to value
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "2 * 3";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == false);
  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == false);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 1);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 1);

  fail_unless(m->getNumInitialAssignments() == 0);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == true);
  fail_unless(util_isEqual(doc->getModel()->getParameter(0)->getValue(), 6.0));

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertIAvalue_nonstrict)
{
  // convert IA to value
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  InitialAssignment *ar = m->createInitialAssignment();
  ar->setSymbol("p");
  std::string math = "2 * 3";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  ar->setMath(node);
  delete node;

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == true);
  fail_unless(ar->getMath()->usesL3V2MathConstructs() == false);

  fail_unless(m->getNumInitialAssignments() == 1);
  fail_unless(p->isSetValue() == false);

  bool done = doc->setLevelAndVersion(2, 1, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(ConvertibleMathInitialAssignment) == false);
  fail_unless(doc->getErrorLog()->contains(MathMLElementNotSupported) == false);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 1);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 2);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 1);

  fail_unless(m->getNumInitialAssignments() == 0);

  fail_unless(doc->getModel()->getParameter(0)->isSetValue() == true);
  fail_unless(util_isEqual(doc->getModel()->getParameter(0)->getValue(), 6.0));

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingEAMath_bug)
{
  /* // bug reported https://www.pivotaltracker.com/story/show/128923467 */
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Event *e = m->createEvent();
  e->setUseValuesFromTriggerTime(true);
  Trigger *t = e->createTrigger();
  t->setPersistent(false);
  t->setInitialValue(false);
  std::string math = "true";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  t->setMath(node);
  delete node;
  EventAssignment *ea = e->createEventAssignment();
  ea->setVariable("p");

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(e->getLevel() == 3);
  fail_unless(e->getVersion() == 2);
  fail_unless(e->isSetTrigger() == true);
  fail_unless(e->getTrigger()->isSetMath() == true);
  fail_unless(e->getNumEventAssignments() == 1);
  fail_unless(e->getEventAssignment(0)->isSetMath() == false);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingMathElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getEvent(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getEvent(0)->getVersion() == 1);

  fail_unless(doc->getModel()->getEvent(0)->isSetTrigger() == true);
  fail_unless(doc->getModel()->getEvent(0)->getTrigger()->isSetMath() == true);
  fail_unless(e->getNumEventAssignments() == 0);

  const ASTNode * node1 = doc->getModel()->getEvent(0)->getTrigger()->getMath();
  char * str = SBML_formulaToL3String(node1);

  fail_unless(!strcmp(str, "true"));
  safe_free(str);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingMath_strict)
{
  // remove element with missing math
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  AssignmentRule *ar = m->createAssignmentRule();
  ar->setVariable("p");

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == false);

  fail_unless(m->getNumRules() == 1);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingMathElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 1);

  fail_unless(m->getNumRules() == 0);

  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingMath_nonstrict)
{
  // remove element with missing math
  SBMLDocument * doc = new SBMLDocument(3, 2);
  Model * m = doc->createModel();
  Parameter * p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  AssignmentRule *ar = m->createAssignmentRule();
  ar->setVariable("p");

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == false);

  fail_unless(m->getNumRules() == 1);

  bool done = doc->setLevelAndVersion(3, 1, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingMathElementNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);

  fail_unless(doc->getModel()->getParameter(0)->getLevel() == 3);
  fail_unless(doc->getModel()->getParameter(0)->getVersion() == 1);

  fail_unless(m->getNumRules() == 1);

  ar = doc->getModel()->getAssignmentRule("p");
  fail_unless(ar->isSetMath() == false);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingReactantProducts_strict)
{
  std::string filename(TestDataDirectory);
  filename += "l3v2-reaction-no-sr.xml";
  SBMLDocument * doc = readSBML(filename.c_str());

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  Reaction *r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);

  fail_unless(r->getNumProducts() == 0);
  fail_unless(r->getNumReactants() == 0);
  fail_unless(r->getNumModifiers() == 0);

  fail_unless(r->isSetKineticLaw() == true);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == false);

  fail_unless(doc->getErrorLog()->contains(MissingParticipantsNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  delete doc;
}
END_TEST



START_TEST (test_SBMLConvertFromL3V2_convertMissingReactantProducts_nonstrict)
{
  std::string filename(TestDataDirectory);
  filename += "l3v2-reaction-no-sr.xml";
  SBMLDocument * doc = readSBML(filename.c_str());

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  Reaction *r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);

  fail_unless(r->getNumProducts() == 0);
  fail_unless(r->getNumReactants() == 0);
  fail_unless(r->getNumModifiers() == 0);

  fail_unless(r->isSetKineticLaw() == true);

  bool done = doc->setLevelAndVersion(3, 1, false);

  fail_unless(done == true);

  fail_unless(doc->getErrorLog()->contains(MissingParticipantsNotSupported) == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);

  fail_unless(r->getNumProducts() == 0);
  fail_unless(r->getNumReactants() == 0);
  fail_unless(r->getNumModifiers() == 0);

  fail_unless(r->isSetKineticLaw() == true);
  delete doc;
}
END_TEST


START_TEST (test_SBMLConvertFromL3V2_convertMissingFast)
{
  std::string filename(TestDataDirectory);
  filename += "l3v2-reaction.xml";
  SBMLDocument * doc = readSBML(filename.c_str());

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  Reaction *r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);

  fail_unless(r->isSetFast() == false);

  bool done = doc->setLevelAndVersion(3, 1, true);

  fail_unless(done == true);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 1);

  r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);

  fail_unless(r->isSetFast() == true);
  fail_unless(r->getFast() == false);

  delete doc;
}
END_TEST


START_TEST(test_SBMLConvertFromL3V2_convertMissingMathForStoich_strict)
{
  std::string filename(TestDataDirectory);
  filename += "l3v2-stoich-math-missing.xml";
  SBMLDocument * doc = readSBML(filename.c_str());
  Model* m = doc->getModel();
  AssignmentRule* ar = (AssignmentRule*)(m->getRule(0));
  SpeciesReference *sr = m->getReaction(0)->getProduct(0);

  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  fail_unless(ar->getLevel() == 3);
  fail_unless(ar->getVersion() == 2);

  fail_unless(ar->isSetMath() == false);

  fail_unless(m->getNumRules() == 1);

  fail_unless(ar->getVariable() == sr->getId());

  bool done = doc->setLevelAndVersion(2, 4, true);

  fail_unless(done == true);

  fail_unless(doc->getLevel() == 2);
  fail_unless(doc->getVersion() == 4);

  m = doc->getModel();

  // expect the rule to have gone and no stoichiometryMath
  fail_unless(m->getNumRules() == 0);
  sr = m->getReaction(0)->getProduct(0);

  fail_unless(sr->isSetStoichiometryMath() == false);
  fail_unless(sr->isSetStoichiometry() == true);


  delete doc;
}
END_TEST



Suite *
create_suite_SBMLConvertFromL3V2 (void) 
{ 
  Suite *suite = suite_create("SBMLConvertFromL3V2");
  TCase *tcase = tcase_create("SBMLConvertFromL3V2");


  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingTrigger_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingTriggerMath_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingTrigger_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingTriggerMath_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertnewMath_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertnewMath_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertEmptyLO_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertEmptyLO_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAnewMath_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAnewMath_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAnewMath1_strict );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAnewMath1_nonstrict );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIA_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIA_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAvalue_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertIAvalue_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingEAMath_bug       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingMath_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingMath_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingReactantProducts_strict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingReactantProducts_nonstrict       );
  tcase_add_test( tcase, test_SBMLConvertFromL3V2_convertMissingFast );
  tcase_add_test(tcase, test_SBMLConvertFromL3V2_convertMissingMathForStoich_strict);
  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS

