/**
 * \file    TestSBMLParentObject.cpp
 * \brief   SBML parent object unit tests
 * \author  Sarah Keating
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


#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/Compartment.h>
#include <sbml/CompartmentType.h>
#include <sbml/Constraint.h>
#include <sbml/Delay.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>
#include <sbml/FunctionDefinition.h>
#include <sbml/InitialAssignment.h>
#include <sbml/KineticLaw.h>
#include <sbml/ListOf.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Reaction.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Species.h>
#include <sbml/SpeciesReference.h>
#include <sbml/SpeciesType.h>
#include <sbml/Unit.h>
#include <sbml/UnitDefinition.h>
#include <sbml/units/FormulaUnitsData.h>

#include <sbml/math/ASTNode.h>

#include <check.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


CK_CPPSTART
START_TEST ( test_Compartment_parent_add )
{
  Compartment *c = new Compartment();
  Model *m = new Model();

  m->addCompartment(c);

  delete c;

  ListOf *lo = m->getListOfCompartments();

  fail_unless(lo == m->getCompartment(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_CompartmentType_parent_add )
{
  CompartmentType *ct = new CompartmentType();
  Model *m = new Model();

  m->addCompartmentType(ct);

  delete ct;

  ListOf *lo = m->getListOfCompartmentTypes();

  fail_unless(lo == m->getCompartmentType(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Constraint_parent_add )
{
  Constraint *ct = new Constraint();
  Model *m = new Model();

  m->addConstraint(ct);

  delete ct;

  ListOf *lo = m->getListOfConstraints();

  fail_unless(lo == m->getConstraint(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Delay_parent_add )
{
  Delay *d = new Delay();
  Event *e = new Event();

  e->setDelay(d);

  delete d;

  fail_unless(e == e->getDelay()->getParentSBMLObject());

  delete e;
}
END_TEST


START_TEST ( test_Event_parent_add )
{
  Event *e = new Event();
  Model *m = new Model();

  m->addEvent(e);

  delete e;

  ListOf *lo = m->getListOfEvents();

  fail_unless(lo == m->getEvent(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_EventAssignment_parent_add )
{
  Event *e = new Event();
  EventAssignment *ea = new EventAssignment();

  e->addEventAssignment(ea);

  delete ea;

  ListOf *lo = e->getListOfEventAssignments();

  fail_unless(lo == e->getEventAssignment(0)->getParentSBMLObject());
  fail_unless(e == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_FunctionDefinition_parent_add )
{
  FunctionDefinition *fd = new FunctionDefinition();
  Model *m = new Model();

  m->addFunctionDefinition(fd);

  delete fd;

  ListOf *lo = m->getListOfFunctionDefinitions();

  fail_unless(lo == m->getFunctionDefinition(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_InitialAssignment_parent_add )
{
  InitialAssignment *ia = new InitialAssignment();
  Model *m = new Model();

  m->addInitialAssignment(ia);

  delete ia;

  ListOf *lo = m->getListOfInitialAssignments();

  fail_unless(lo == m->getInitialAssignment(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_KineticLaw_parent_add )
{
  KineticLaw* kl=new KineticLaw();
  
  Reaction * r = new Reaction();

  r->setKineticLaw(kl);

  fail_unless(r == r->getKineticLaw()->getParentSBMLObject());

  delete r;
}
END_TEST


START_TEST ( test_KineticLaw_Parameter_parent_add )
{
  KineticLaw* kl=new KineticLaw();
  
  Parameter * p = new Parameter("jake");
  kl->addParameter(p);
  delete p;

  fail_unless(kl->getNumParameters() == 1);
  fail_unless(kl->getParameter(0)->getId() == "jake");

  ListOfParameters *lop = kl->getListOfParameters();

  fail_unless(kl == lop->getParentSBMLObject());
  fail_unless(lop == kl->getParameter(0)->getParentSBMLObject());

  delete kl;
}
END_TEST


START_TEST ( test_Model_parent_add )
{
  SBMLDocument *d = new SBMLDocument();
  Model *m = new Model();

  d->setModel(m);

  fail_unless(d == d->getModel()->getParentSBMLObject());

  delete d;
}
END_TEST


START_TEST ( test_Parameter_parent_add )
{
  Parameter *ia = new Parameter();
  Model *m = new Model();

  m->addParameter(ia);

  delete ia;

  ListOf *lo = m->getListOfParameters();

  fail_unless(lo == m->getParameter(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Reaction_parent_add )
{
  Reaction *ia = new Reaction();
  Model *m = new Model();

  m->addReaction(ia);

  delete ia;

  ListOf *lo = m->getListOfReactions();

  fail_unless(lo == m->getReaction(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST



START_TEST ( test_Rule_parent_add )
{
  Rule *ia = new RateRule("a");
  Model *m = new Model();

  m->addRule(ia);

  delete ia;

  ListOf *lo = m->getListOfRules();

  fail_unless(lo == m->getRule(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST



START_TEST ( test_Species_parent_add )
{
  Species *ia = new Species();
  Model *m = new Model();

  m->addSpecies(ia);

  delete ia;

  ListOf *lo = m->getListOfSpecies();

  fail_unless(lo == m->getSpecies(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST



START_TEST ( test_SpeciesReference_Product_parent_add )
{
  SpeciesReference *sr = new SpeciesReference();
  Reaction *r = new Reaction();

  r->addProduct(sr);

  delete sr;

  ListOf *lo = r->getListOfProducts();

  fail_unless(lo == r->getProduct(0)->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Reactant_parent_add )
{
  SpeciesReference *sr = new SpeciesReference();
  Reaction *r = new Reaction();

  r->addReactant(sr);

  delete sr;

  ListOf *lo = r->getListOfReactants();

  fail_unless(lo == r->getReactant(0)->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Modifier_parent_add )
{
  ModifierSpeciesReference *sr = new ModifierSpeciesReference();
  Reaction *r = new Reaction();

  r->addModifier(sr);

  delete sr;

  ListOf *lo = r->getListOfModifiers();

  fail_unless(lo == r->getModifier(0)->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesType_parent_add )
{
  SpeciesType *ia = new SpeciesType();
  Model *m = new Model();

  m->addSpeciesType(ia);

  delete ia;

  ListOf *lo = m->getListOfSpeciesTypes();

  fail_unless(lo == m->getSpeciesType(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_StoichiometryMath_parent_add )
{
  StoichiometryMath *m = new StoichiometryMath();
  SpeciesReference *sr = new SpeciesReference();

  sr->setStoichiometryMath(m);

  delete m;

  fail_unless(sr == sr->getStoichiometryMath()->getParentSBMLObject());

  delete sr;
}
END_TEST


START_TEST ( test_Trigger_parent_add )
{
  Trigger *d = new Trigger();
  Event *e = new Event();

  e->setTrigger(d);

  delete d;

  fail_unless(e == e->getTrigger()->getParentSBMLObject());

  delete e;
}
END_TEST


START_TEST ( test_Unit_parent_add )
{
  UnitDefinition* ud=new UnitDefinition();
  
  Unit * u = new Unit();
  ud->addUnit(u);
  delete u;

  fail_unless(ud->getNumUnits() == 1);

  ListOf *lo = ud->getListOfUnits();

  fail_unless(lo == ud->getUnit(0)->getParentSBMLObject());
  fail_unless(ud == lo->getParentSBMLObject());

  delete ud;
}
END_TEST


START_TEST ( test_UnitDefinition_parent_add )
{
  UnitDefinition *ia = new UnitDefinition();
  Model *m = new Model();

  m->addUnitDefinition(ia);

  delete ia;

  ListOf *lo = m->getListOfUnitDefinitions();

  fail_unless(lo == m->getUnitDefinition(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Compartment_parent_create )
{
  Model *m = new Model();
  Compartment *c = m->createCompartment();

  ListOf *lo = m->getListOfCompartments();

  fail_unless(lo == m->getCompartment(0)->getParentSBMLObject());
  fail_unless(lo == c->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_CompartmentType_parent_create )
{
  Model *m = new Model();
  CompartmentType *ct = m->createCompartmentType();

  ListOf *lo = m->getListOfCompartmentTypes();

  fail_unless(lo == m->getCompartmentType(0)->getParentSBMLObject());
  fail_unless(lo == ct->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Constraint_parent_create )
{
  Model *m = new Model();
  Constraint *ct = m->createConstraint();

  ListOf *lo = m->getListOfConstraints();

  fail_unless(lo == m->getConstraint(0)->getParentSBMLObject());
  fail_unless(lo == ct->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Event_parent_create )
{
  Model *m = new Model();
  Event *e = m->createEvent();

  ListOf *lo = m->getListOfEvents();

  fail_unless(lo == m->getEvent(0)->getParentSBMLObject());
  fail_unless(lo == e->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_EventAssignment_parent_create )
{
  Event *e = new Event();

  EventAssignment *ea = e->createEventAssignment();

  ListOf *lo = e->getListOfEventAssignments();

  fail_unless(lo == e->getEventAssignment(0)->getParentSBMLObject());
  fail_unless(lo == ea->getParentSBMLObject());
  fail_unless(e == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_EventAssignment_parent_create_model )
{
  Model *m = new Model();
  Event *e = m->createEvent();

  EventAssignment *ea = m->createEventAssignment();

  ListOf *lo = e->getListOfEventAssignments();

  fail_unless(lo == e->getEventAssignment(0)->getParentSBMLObject());
  fail_unless(lo == ea->getParentSBMLObject());
  fail_unless(e == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_FunctionDefinition_parent_create )
{
  Model *m = new Model();
  FunctionDefinition *fd = m->createFunctionDefinition();

  ListOf *lo = m->getListOfFunctionDefinitions();

  fail_unless(lo == m->getFunctionDefinition(0)->getParentSBMLObject());
  fail_unless(lo == fd->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_InitialAssignment_parent_create )
{
  Model *m = new Model();
  InitialAssignment *ia = m->createInitialAssignment();

  ListOf *lo = m->getListOfInitialAssignments();

  fail_unless(lo == m->getInitialAssignment(0)->getParentSBMLObject());
  fail_unless(lo == ia->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_KineticLaw_parent_create )
{
  Reaction * r = new Reaction();
  KineticLaw* kl = r->createKineticLaw();

  fail_unless(r == kl->getParentSBMLObject());

  delete r;
}
END_TEST


START_TEST ( test_KineticLaw_parent_create_model )
{
  Model *m = new Model();
  Reaction * r = m->createReaction();
  KineticLaw* kl = r->createKineticLaw();

  fail_unless(r == kl->getParentSBMLObject());
  fail_unless(r == r->getKineticLaw()->getParentSBMLObject());

  delete r;
}
END_TEST


START_TEST ( test_KineticLaw_Parameter_parent_create )
{
  KineticLaw* kl=new KineticLaw();
  Parameter * p = kl->createParameter();

  fail_unless(kl->getNumParameters() == 1);

  ListOfParameters *lop = kl->getListOfParameters();

  fail_unless(kl == lop->getParentSBMLObject());
  fail_unless(lop == p->getParentSBMLObject());
  fail_unless(lop == kl->getParameter(0)->getParentSBMLObject());

  delete kl;
}
END_TEST


START_TEST ( test_KineticLaw_Parameter_parent_create_model )
{
  Model *m = new Model();
  Reaction *r = m->createReaction();
  KineticLaw* kl = m->createKineticLaw();
  Parameter * p = m->createKineticLawParameter();

  fail_unless(kl->getNumParameters() == 1);

  ListOfParameters *lop = kl->getListOfParameters();

  fail_unless(r == kl->getParentSBMLObject());
  fail_unless(kl == lop->getParentSBMLObject());
  fail_unless(lop == p->getParentSBMLObject());
  fail_unless(lop == kl->getParameter(0)->getParentSBMLObject());

  delete kl;
}
END_TEST


START_TEST ( test_Model_parent_create )
{
  SBMLDocument *d = new SBMLDocument();
  Model *m = d->createModel();

  fail_unless(d == m->getParentSBMLObject());

  delete d;
}
END_TEST


START_TEST ( test_Parameter_parent_create )
{
  Model *m = new Model();
  Parameter *p = m->createParameter();

  ListOf *lo = m->getListOfParameters();

  fail_unless(lo == m->getParameter(0)->getParentSBMLObject());
  fail_unless(lo == p->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Reaction_parent_create )
{
  Model *m = new Model();
  Reaction *r = m->createReaction();

  ListOf *lo = m->getListOfReactions();

  fail_unless(lo == m->getReaction(0)->getParentSBMLObject());
  fail_unless(lo == r->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST



START_TEST ( test_AlgebraicRule_parent_create )
{
  Model *m = new Model();
  AlgebraicRule *r = m->createAlgebraicRule();

  ListOf *lo = m->getListOfRules();

  fail_unless(lo == m->getRule(0)->getParentSBMLObject());
  fail_unless(lo == r->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_AssignmentRule_parent_create )
{
  Model *m = new Model();
  AssignmentRule *r = m->createAssignmentRule();

  ListOf *lo = m->getListOfRules();

  fail_unless(lo == m->getRule(0)->getParentSBMLObject());
  fail_unless(lo == r->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_RateRule_parent_create )
{
  Model *m = new Model();
  RateRule *r = m->createRateRule();

  ListOf *lo = m->getListOfRules();

  fail_unless(lo == m->getRule(0)->getParentSBMLObject());
  fail_unless(lo == r->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Species_parent_create )
{
  Model *m = new Model();
  Species *s = m->createSpecies();

  ListOf *lo = m->getListOfSpecies();

  fail_unless(lo == s->getParentSBMLObject());
  fail_unless(lo == m->getSpecies(0)->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Product_parent_create )
{
  Reaction *r = new Reaction();
  SpeciesReference *sr = r->createProduct();

  ListOf *lo = r->getListOfProducts();

  fail_unless(lo == r->getProduct(0)->getParentSBMLObject());
  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Product_parent_create_model )
{
  Model *m = new Model();
  Reaction *r = m->createReaction();
  SpeciesReference *sr = m->createProduct();

  ListOf *lo = r->getListOfProducts();

  fail_unless(lo == r->getProduct(0)->getParentSBMLObject());
  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Reactant_parent_create )
{
  Reaction *r = new Reaction();
  SpeciesReference *sr = r->createReactant();

  ListOf *lo = r->getListOfReactants();

  fail_unless(lo == r->getReactant(0)->getParentSBMLObject());
  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Reactant_parent_create_model )
{
  Model *m = new Model();
  Reaction *r = m->createReaction();
  SpeciesReference *sr = m->createReactant();

  ListOf *lo = r->getListOfReactants();

  fail_unless(lo == r->getReactant(0)->getParentSBMLObject());
  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Modifier_parent_create )
{
  Reaction *r = new Reaction();
  ModifierSpeciesReference *sr = r->createModifier();

  ListOf *lo = r->getListOfModifiers();

  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(lo == r->getModifier(0)->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesReference_Modifier_parent_create_model )
{
  Model *m = new Model();
  Reaction *r = m->createReaction();
  ModifierSpeciesReference *sr = m->createModifier();

  ListOf *lo = r->getListOfModifiers();

  fail_unless(lo == sr->getParentSBMLObject());
  fail_unless(lo == r->getModifier(0)->getParentSBMLObject());
  fail_unless(r == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_SpeciesType_parent_create )
{
  Model *m = new Model();
  SpeciesType *st = m->createSpeciesType();

  ListOf *lo = m->getListOfSpeciesTypes();

  fail_unless(lo == m->getSpeciesType(0)->getParentSBMLObject());
  fail_unless(lo == st->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


START_TEST ( test_Unit_parent_create )
{
  UnitDefinition* ud = new UnitDefinition();
  Unit * u = ud->createUnit();

  fail_unless(ud->getNumUnits() == 1);

  ListOf *lo = ud->getListOfUnits();

  fail_unless(lo == ud->getUnit(0)->getParentSBMLObject());
  fail_unless(lo == u->getParentSBMLObject());
  fail_unless(ud == lo->getParentSBMLObject());

  delete ud;
}
END_TEST


START_TEST ( test_Unit_parent_create_model )
{
  Model *m = new Model();
  UnitDefinition* ud = m->createUnitDefinition();
  Unit * u = m->createUnit();

  fail_unless(ud->getNumUnits() == 1);

  ListOf *lo = ud->getListOfUnits();

  fail_unless(lo == ud->getUnit(0)->getParentSBMLObject());
  fail_unless(lo == u->getParentSBMLObject());
  fail_unless(ud == lo->getParentSBMLObject());

  delete ud;
}
END_TEST


START_TEST ( test_UnitDefinition_parent_create )
{
  Model *m = new Model();
  UnitDefinition *ud = m->createUnitDefinition();

  ListOf *lo = m->getListOfUnitDefinitions();

  fail_unless(lo == m->getUnitDefinition(0)->getParentSBMLObject());
  fail_unless(lo == ud->getParentSBMLObject());
  fail_unless(m == lo->getParentSBMLObject());
}
END_TEST


Suite *
create_suite_ParentObject (void)
{
  Suite *suite = suite_create("ParentObject");
  TCase *tcase = tcase_create("ParentObject");

  tcase_add_test( tcase, test_Compartment_parent_add );
  tcase_add_test( tcase, test_CompartmentType_parent_add );
  tcase_add_test( tcase, test_Constraint_parent_add );
  tcase_add_test( tcase, test_Delay_parent_add );
  tcase_add_test( tcase, test_Event_parent_add );
  tcase_add_test( tcase, test_EventAssignment_parent_add );
  tcase_add_test( tcase, test_FunctionDefinition_parent_add );
  tcase_add_test( tcase, test_InitialAssignment_parent_add );
  tcase_add_test( tcase, test_KineticLaw_parent_add );
  tcase_add_test( tcase, test_KineticLaw_Parameter_parent_add );
  tcase_add_test( tcase, test_Model_parent_add );
  tcase_add_test( tcase, test_Parameter_parent_add );
  tcase_add_test( tcase, test_Reaction_parent_add );
  tcase_add_test( tcase, test_Rule_parent_add );
  tcase_add_test( tcase, test_Species_parent_add );
  tcase_add_test( tcase, test_SpeciesReference_Product_parent_add );
  tcase_add_test( tcase, test_SpeciesReference_Reactant_parent_add );
  tcase_add_test( tcase, test_SpeciesReference_Modifier_parent_add );
  tcase_add_test( tcase, test_SpeciesType_parent_add );
  tcase_add_test( tcase, test_StoichiometryMath_parent_add );
  tcase_add_test( tcase, test_Trigger_parent_add );
  tcase_add_test( tcase, test_Unit_parent_add );
  tcase_add_test( tcase, test_UnitDefinition_parent_add );
  tcase_add_test( tcase, test_Compartment_parent_create );
  tcase_add_test( tcase, test_CompartmentType_parent_create );
  tcase_add_test( tcase, test_Constraint_parent_create );
  tcase_add_test( tcase, test_Event_parent_create );
  tcase_add_test( tcase, test_EventAssignment_parent_create );
  tcase_add_test( tcase, test_EventAssignment_parent_create_model );
  tcase_add_test( tcase, test_FunctionDefinition_parent_create );
  tcase_add_test( tcase, test_InitialAssignment_parent_create );
  tcase_add_test( tcase, test_KineticLaw_parent_create );
  tcase_add_test( tcase, test_KineticLaw_parent_create_model );
  tcase_add_test( tcase, test_KineticLaw_Parameter_parent_create );
  tcase_add_test( tcase, test_KineticLaw_Parameter_parent_create_model );
  tcase_add_test( tcase, test_Model_parent_create );
  tcase_add_test( tcase, test_Parameter_parent_create );
  tcase_add_test( tcase, test_Reaction_parent_create );
  tcase_add_test( tcase, test_AlgebraicRule_parent_create );
  tcase_add_test( tcase, test_AssignmentRule_parent_create );
  tcase_add_test( tcase, test_RateRule_parent_create );
  tcase_add_test( tcase, test_Species_parent_create );
  tcase_add_test( tcase, test_SpeciesReference_Product_parent_create );
  tcase_add_test( tcase, test_SpeciesReference_Product_parent_create_model );
  tcase_add_test( tcase, test_SpeciesReference_Reactant_parent_create );
  tcase_add_test( tcase, test_SpeciesReference_Reactant_parent_create_model );
  tcase_add_test( tcase, test_SpeciesReference_Modifier_parent_create );
  tcase_add_test( tcase, test_SpeciesReference_Modifier_parent_create_model );
  tcase_add_test( tcase, test_SpeciesType_parent_create );
  tcase_add_test( tcase, test_Unit_parent_create );
  tcase_add_test( tcase, test_Unit_parent_create_model );
  tcase_add_test( tcase, test_UnitDefinition_parent_create );

  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND
