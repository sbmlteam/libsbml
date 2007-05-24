/**
 * \file    TestReadSBML.cpp
 * \brief   Read SBML unit tests
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


#include "common/common.h"

#include "SBase.h"
#include "Compartment.h"
#include "CompartmentType.h"
#include "Constraint.h"
#include "Delay.h"
#include "Event.h"
#include "EventAssignment.h"
#include "FunctionDefinition.h"
#include "InitialAssignment.h"
#include "KineticLaw.h"
#include "ListOf.h"
#include "Model.h"
#include "Parameter.h"
#include "Reaction.h"
#include "Species.h"
#include "SpeciesReference.h"
#include "SpeciesType.h"
#include "Unit.h"
#include "UnitDefinition.h"
#include "units/FormulaUnitsData.h"

#include "math/ASTNode.h"

#include <check.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


CK_CPPSTART
START_TEST ( test_Compartment_copyConstructor )
{
    Compartment* o1=new Compartment();
    o1->setId("c");
    o1->setOutside("c2");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getOutside() == "c2");

    Compartment* o2=new Compartment(*o1);

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getOutside() == "c2");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Compartment_assignmentOperator )
{
    Compartment* o1=new Compartment();
    o1->setId("c");
    o1->setOutside("c2");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getOutside() == "c2");
    
    Compartment* o2 = new Compartment();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getOutside() == "c2");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Compartment_clone )
{
    Compartment* o1=new Compartment();
    o1->setId("c");
    o1->setOutside("c2");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getOutside() == "c2");

    Compartment* o2=static_cast<Compartment*>(o1->clone());
   
    fail_unless(o2->getId() == "c");
    fail_unless(o2->getOutside() == "c2");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_CompartmentType_copyConstructor )
{
    CompartmentType* o1=new CompartmentType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    CompartmentType* o2=new CompartmentType(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_CompartmentType_assignmentOperator )
{
    CompartmentType* o1=new CompartmentType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    CompartmentType* o2 = new CompartmentType();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_CompartmentType_clone )
{
    CompartmentType* o1=new CompartmentType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    CompartmentType* o2=static_cast<CompartmentType*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Constraint_copyConstructor )
{
    Constraint* o1=new Constraint();
    o1->setMetaId("c");
    
    fail_unless(o1->getMetaId() == "c");

    ASTNode * math = new ASTNode (AST_CONSTANT_PI);
    XMLNode * message = new XMLNode();

    o1->setMath(math);
    o1->setMessage(message);

    delete math;
    delete message;

    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    Constraint* o2=new Constraint(*o1);

    fail_unless(o2->getMetaId() == "c");
    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Constraint_assignmentOperator )
{
    Constraint* o1=new Constraint();
    o1->setMetaId("c");
    
    fail_unless(o1->getMetaId() == "c");
    
    ASTNode * math = new ASTNode (AST_CONSTANT_PI);
    XMLNode * message = new XMLNode();

    o1->setMath(math);
    o1->setMessage(message);

    delete math;
    delete message;

    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    Constraint* o2 = new Constraint();;
    (*o2)=*o1;

    fail_unless(o2->getMetaId() == "c");
    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Constraint_clone )
{
    Constraint* o1=new Constraint();
    o1->setMetaId("c");
    
    fail_unless(o1->getMetaId() == "c");

    ASTNode * math = new ASTNode (AST_CONSTANT_PI);
    XMLNode * message = new XMLNode();

    o1->setMath(math);
    o1->setMessage(message);

    delete math;
    delete message;

    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    Constraint* o2=static_cast<Constraint*>(o1->clone());
   
    fail_unless(o2->getMetaId() == "c");
    fail_unless(o1->getMath() != NULL);
    fail_unless(o1->getMessage() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Delay_copyConstructor )
{
    Delay* o1=new Delay();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Delay* o2=new Delay(*o1);

    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Delay_assignmentOperator )
{
    Delay* o1=new Delay();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Delay* o2 = new Delay();
    (*o2)=*o1;

    fail_unless(o1->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Delay_clone )
{
    Delay* o1=new Delay();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Delay* o2 = static_cast <Delay *>(o1->clone());

    fail_unless(o1->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Event_copyConstructor )
{
    Event* o1=new Event();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Event* o2=new Event(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Event_assignmentOperator )
{
    Event* o1=new Event();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    Event* o2 = new Event();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Event_clone )
{
    Event* o1=new Event();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Event* o2=static_cast<Event*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_EventAssignment_copyConstructor )
{
    EventAssignment* o1=new EventAssignment();
    o1->setVariable("c2");
    
    fail_unless(o1->getVariable() == "c2");

    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    EventAssignment* o2=new EventAssignment(*o1);

    fail_unless(o2->getVariable() == "c2");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_EventAssignment_assignmentOperator )
{
    EventAssignment* o1=new EventAssignment();
    o1->setVariable("c2");
    
    fail_unless(o1->getVariable() == "c2");

    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    EventAssignment* o2 = new EventAssignment();;
    (*o2)=*o1;

    fail_unless(o2->getVariable() == "c2");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_EventAssignment_clone )
{
    EventAssignment* o1=new EventAssignment();
    o1->setVariable("c2");
    
    fail_unless(o1->getVariable() == "c2");

    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    EventAssignment* o2=static_cast<EventAssignment*>(o1->clone());

    fail_unless(o2->getVariable() == "c2");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_FunctionDefinition_copyConstructor )
{
    FunctionDefinition* o1=new FunctionDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    FunctionDefinition* o2=new FunctionDefinition(*o1);

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_FunctionDefinition_assignmentOperator )
{
    FunctionDefinition* o1=new FunctionDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    FunctionDefinition* o2 = new FunctionDefinition();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_FunctionDefinition_clone )
{
    FunctionDefinition* o1=new FunctionDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    FunctionDefinition* o2=static_cast<FunctionDefinition*>(o1->clone());
   
    fail_unless(o2->getId() == "c");
    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_InitialAssignment_copyConstructor )
{
    InitialAssignment* o1=new InitialAssignment();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    InitialAssignment* o2=new InitialAssignment(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_InitialAssignment_assignmentOperator )
{
    InitialAssignment* o1=new InitialAssignment();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    InitialAssignment* o2 = new InitialAssignment();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_InitialAssignment_clone )
{
    InitialAssignment* o1=new InitialAssignment();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    InitialAssignment* o2=static_cast<InitialAssignment*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_KineticLaw_copyConstructor )
{
    KineticLaw* o1=new KineticLaw();
    o1->setId("c");
    
    Parameter * p = new Parameter("jake");
    o1->addParameter(p);
    delete p;

    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getParameter(0)->getId() == "jake");
    fail_unless(o1->getId() == "c");

    KineticLaw* o2=new KineticLaw(*o1);

    fail_unless(o2->getNumParameters() == 1);
    fail_unless(o2->getParameter(0)->getId() == "jake");
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_KineticLaw_assignmentOperator )
{
    KineticLaw* o1=new KineticLaw();
    o1->setId("c");
    
    Parameter * p = new Parameter("jake");
    o1->addParameter(p);
    delete p;

    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getParameter(0)->getId() == "jake");
    fail_unless(o1->getId() == "c");

    KineticLaw* o2 = new KineticLaw();;
    (*o2)=*o1;

    fail_unless(o2->getNumParameters() == 1);
    fail_unless(o2->getParameter(0)->getId() == "jake");
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_KineticLaw_clone )
{
    KineticLaw* o1=new KineticLaw();
    o1->setId("c");
    
    Parameter * p = new Parameter("jake");
    o1->addParameter(p);
    delete p;

    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getParameter(0)->getId() == "jake");
    fail_unless(o1->getId() == "c");

    KineticLaw* o2=static_cast<KineticLaw*>(o1->clone());

    fail_unless(o2->getNumParameters() == 1);
    fail_unless(o2->getParameter(0)->getId() == "jake");
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ListOf_copyConstructor )
{
    ListOf* o1=new ListOf();
    
    o1->setId("c");
    fail_unless(o1->getId() == "c");
    
    Species * s = new Species("species_1");
    o1->append(s);

    delete s;
    
    ListOf* o2=new ListOf(*o1);
    fail_unless(o2->size() == 1);
    fail_unless(o2->getId() == "c");
    fail_unless(static_cast<Species *> (o2->get(0))->getId()
      == "species_1");
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ListOf_assignmentOperator )
{
    ListOf* o1=new ListOf();
    
    o1->setId("c");  
    fail_unless(o1->getId() == "c");
    
    Species * s = new Species("species_1");
    o1->append(s);

    delete s;

    ListOf* o2 = new ListOf();;
    (*o2)=*o1;

    fail_unless(o2->size() == 1);
    fail_unless(o2->getId() == "c");
    fail_unless(static_cast<Species *> (o2->get(0))->getId()
      == "species_1");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ListOf_clone )
{
    ListOf* o1=new ListOf();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    Species * s = new Species("species_1");
    o1->append(s);

    delete s;


    ListOf* o2=static_cast<ListOf*>(o1->clone());
   
    fail_unless(o2->size() == 1);
    fail_unless(o2->getId() == "c");
    fail_unless(static_cast<Species *> (o2->get(0))->getId()
      == "species_1");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Model_copyConstructor )
{
    Model* o1=new Model();
    o1->setId("c");

    Parameter *p = new Parameter("alex");
    o1->addParameter(p);
    delete p;
    FormulaUnitsData *fud = new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    Model* o2=new Model(*o1);

    fail_unless(o2->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Model_assignmentOperator )
{
    Model* o1=new Model();
    o1->setId("c");

    Parameter *p = new Parameter("alex");
    o1->addParameter(p);
    delete p;

    FormulaUnitsData *fud = new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    Model* o2=new Model();
    (*o2) = *o1;

    fail_unless(o2->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Model_clone )
{
    Model* o1=new Model();
    o1->setId("c");

    Parameter *p = new Parameter("alex");
    o1->addParameter(p);
    delete p;

    FormulaUnitsData *fud = new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    Model* o2=static_cast<Model*>(o1->clone());

    fail_unless(o2->getId() == "c");
    fail_unless(o1->getNumParameters() == 1);
    fail_unless(o1->getNumFormulaUnitsData() == 1);
    fail_unless(o1->getParameter(0)->getId() == "alex");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Parameter_copyConstructor )
{
    Parameter* o1=new Parameter();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Parameter* o2=new Parameter(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Parameter_assignmentOperator )
{
    Parameter* o1=new Parameter();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    Parameter* o2 = new Parameter();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Parameter_clone )
{
    Parameter* o1=new Parameter();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Parameter* o2=static_cast<Parameter*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Reaction_copyConstructor )
{
    Reaction* o1=new Reaction();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    KineticLaw *kl = new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;

    fail_unless(o1->isSetKineticLaw() == 1);
    fail_unless (o1->getKineticLaw() != NULL);

    Reaction* o2=new Reaction(*o1);

    fail_unless(o2->getId() == "c");
    fail_unless(o2->isSetKineticLaw() == 1);
    fail_unless(o2->getKineticLaw() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Reaction_assignmentOperator )
{
    Reaction* o1=new Reaction();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    KineticLaw *kl = new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;

    fail_unless(o1->isSetKineticLaw() == 1);
    fail_unless (o1->getKineticLaw() != NULL);

    Reaction* o2 = new Reaction();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");
    fail_unless(o2->isSetKineticLaw() == 1);
    fail_unless(o2->getKineticLaw() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Reaction_clone )
{
    Reaction* o1=new Reaction();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    KineticLaw *kl = new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;

    fail_unless(o1->isSetKineticLaw() == 1);
    fail_unless (o1->getKineticLaw() != NULL);
   
    Reaction* o2=static_cast<Reaction*>(o1->clone());
   
    fail_unless(o2->getId() == "c");
    fail_unless(o2->isSetKineticLaw() == 1);
    fail_unless(o2->getKineticLaw() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Rule_copyConstructor )
{
    Rule* o1=new RateRule("a");
    
    fail_unless(o1->getId() == "a");
    
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->isSetMath() == 1);
    
    Rule* o2=new Rule(*o1);
    
    fail_unless(o2->getId() == "a");
    fail_unless(o2->isSetMath() == 1);
    
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Rule_assignmentOperator )
{
    Rule* o1=new RateRule("a");
    
    fail_unless(o1->getId() == "a");
    
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->isSetMath() == 1);
    
    Rule* o2 = new RateRule();
    (*o2)=*o1;
    
    fail_unless(o2->getId() == "a");
    fail_unless(o2->isSetMath() == 1);
    
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Rule_clone )
{
    Rule* o1=new RateRule("a");
    
    fail_unless(o1->getId() == "a");
    
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->isSetMath() == 1);
    
    Rule* o2= static_cast <Rule*> (o1->clone());
    
    fail_unless(o2->getId() == "a");
    fail_unless(o2->isSetMath() == 1);
    
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Species_copyConstructor )
{
    Species* o1=new Species();
    o1->setId("c");
    o1->setSpeciesType("c1");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getSpeciesType() == "c1");

    Species* o2=new Species(*o1);

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getSpeciesType() == "c1");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Species_assignmentOperator )
{
    Species* o1=new Species();
    o1->setId("c");
    o1->setSpeciesType("c1");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getSpeciesType() == "c1");
    
    Species* o2 = new Species();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");
    fail_unless(o2->getSpeciesType() == "c1");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Species_clone )
{
    Species* o1=new Species();
    o1->setId("c");
    o1->setSpeciesType("c1");
    
    fail_unless(o1->getId() == "c");
    fail_unless(o1->getSpeciesType() == "c1");

    Species* o2=static_cast<Species*>(o1->clone());
   
    fail_unless(o2->getId() == "c");
    fail_unless(o2->getSpeciesType() == "c1");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesReference_copyConstructor )
{
    SpeciesReference* o1=new SpeciesReference();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    SpeciesReference* o2=new SpeciesReference(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SpeciesReference_assignmentOperator )
{
    SpeciesReference* o1=new SpeciesReference();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    SpeciesReference* o2 = new SpeciesReference();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesReference_clone )
{
    SpeciesReference* o1=new SpeciesReference();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    SpeciesReference* o2=static_cast<SpeciesReference*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesType_copyConstructor )
{
    SpeciesType* o1=new SpeciesType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    SpeciesType* o2=new SpeciesType(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SpeciesType_assignmentOperator )
{
    SpeciesType* o1=new SpeciesType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    SpeciesType* o2 = new SpeciesType();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesType_clone )
{
    SpeciesType* o1=new SpeciesType();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    SpeciesType* o2=static_cast<SpeciesType*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Trigger_copyConstructor )
{
    Trigger* o1=new Trigger();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Trigger* o2=new Trigger(*o1);

    fail_unless(o2->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Trigger_assignmentOperator )
{
    Trigger* o1=new Trigger();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Trigger* o2 = new Trigger();
    (*o2)=*o1;

    fail_unless(o1->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Trigger_clone )
{
    Trigger* o1=new Trigger();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;

    fail_unless(o1->getMath() != NULL);

    Trigger* o2 = static_cast <Trigger *>(o1->clone());

    fail_unless(o1->getMath() != NULL);

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Unit_copyConstructor )
{
    Unit* o1=new Unit();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Unit* o2=new Unit(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Unit_assignmentOperator )
{
    Unit* o1=new Unit();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    Unit* o2 = new Unit();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Unit_clone )
{
    Unit* o1=new Unit();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    Unit* o2=static_cast<Unit*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_UnitDefinition_copyConstructor )
{
    UnitDefinition* o1=new UnitDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    UnitDefinition* o2=new UnitDefinition(*o1);

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_UnitDefinition_assignmentOperator )
{
    UnitDefinition* o1=new UnitDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");
    
    UnitDefinition* o2 = new UnitDefinition();;
    (*o2)=*o1;

    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_UnitDefinition_clone )
{
    UnitDefinition* o1=new UnitDefinition();
    o1->setId("c");
    
    fail_unless(o1->getId() == "c");

    UnitDefinition* o2=static_cast<UnitDefinition*>(o1->clone());
   
    fail_unless(o2->getId() == "c");

    delete o2;
    delete o1;
}
END_TEST


Suite *
create_suite_CopyAndClone (void)
{
  Suite *suite = suite_create("CopyAndClone");
  TCase *tcase = tcase_create("CopyAndClone");

  tcase_add_test( tcase, test_Compartment_copyConstructor );
  tcase_add_test( tcase, test_Compartment_assignmentOperator );
  tcase_add_test( tcase, test_Compartment_clone );
  tcase_add_test( tcase, test_CompartmentType_copyConstructor );
  tcase_add_test( tcase, test_CompartmentType_assignmentOperator );
  tcase_add_test( tcase, test_CompartmentType_clone );
  tcase_add_test( tcase, test_Constraint_copyConstructor );
  tcase_add_test( tcase, test_Constraint_assignmentOperator );
  tcase_add_test( tcase, test_Constraint_clone );
  tcase_add_test( tcase, test_Delay_copyConstructor );
  tcase_add_test( tcase, test_Delay_assignmentOperator );
  tcase_add_test( tcase, test_Delay_clone );
  tcase_add_test( tcase, test_Event_copyConstructor );
  tcase_add_test( tcase, test_Event_assignmentOperator );
  tcase_add_test( tcase, test_Event_clone );
  tcase_add_test( tcase, test_EventAssignment_copyConstructor );
  tcase_add_test( tcase, test_EventAssignment_assignmentOperator );
  tcase_add_test( tcase, test_EventAssignment_clone );
  tcase_add_test( tcase, test_FunctionDefinition_copyConstructor );
  tcase_add_test( tcase, test_FunctionDefinition_assignmentOperator );
  tcase_add_test( tcase, test_FunctionDefinition_clone );
  tcase_add_test( tcase, test_InitialAssignment_copyConstructor );
  tcase_add_test( tcase, test_InitialAssignment_assignmentOperator );
  tcase_add_test( tcase, test_InitialAssignment_clone );
  tcase_add_test( tcase, test_KineticLaw_copyConstructor );
  tcase_add_test( tcase, test_KineticLaw_assignmentOperator );
  tcase_add_test( tcase, test_KineticLaw_clone );
  tcase_add_test( tcase, test_ListOf_copyConstructor );
  tcase_add_test( tcase, test_ListOf_assignmentOperator );
  tcase_add_test( tcase, test_ListOf_clone );
  tcase_add_test( tcase, test_Model_copyConstructor );
  tcase_add_test( tcase, test_Model_assignmentOperator );
  tcase_add_test( tcase, test_Model_clone );
  tcase_add_test( tcase, test_Parameter_copyConstructor );
  tcase_add_test( tcase, test_Parameter_assignmentOperator );
  tcase_add_test( tcase, test_Parameter_clone );
  tcase_add_test( tcase, test_Reaction_copyConstructor );
  tcase_add_test( tcase, test_Reaction_assignmentOperator );
  tcase_add_test( tcase, test_Reaction_clone );
  tcase_add_test( tcase, test_Rule_copyConstructor );
  tcase_add_test( tcase, test_Rule_assignmentOperator );
  tcase_add_test( tcase, test_Rule_clone );
  tcase_add_test( tcase, test_Species_copyConstructor );
  tcase_add_test( tcase, test_Species_assignmentOperator );
  tcase_add_test( tcase, test_Species_clone );
  tcase_add_test( tcase, test_SpeciesReference_copyConstructor );
  tcase_add_test( tcase, test_SpeciesReference_assignmentOperator );
  tcase_add_test( tcase, test_SpeciesReference_clone );
  tcase_add_test( tcase, test_SpeciesType_copyConstructor );
  tcase_add_test( tcase, test_SpeciesType_assignmentOperator );
  tcase_add_test( tcase, test_SpeciesType_clone );
  tcase_add_test( tcase, test_Trigger_copyConstructor );
  tcase_add_test( tcase, test_Trigger_assignmentOperator );
  tcase_add_test( tcase, test_Trigger_clone );
  tcase_add_test( tcase, test_Unit_copyConstructor );
  tcase_add_test( tcase, test_Unit_assignmentOperator );
  tcase_add_test( tcase, test_Unit_clone );
  tcase_add_test( tcase, test_UnitDefinition_copyConstructor );
  tcase_add_test( tcase, test_UnitDefinition_assignmentOperator );
  tcase_add_test( tcase, test_UnitDefinition_clone );

  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND
