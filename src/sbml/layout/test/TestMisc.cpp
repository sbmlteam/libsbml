/**
 * Filename    : TestMisc.cpp
 * Description : Unit tests for sevaral classes
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2005-05-03
 *
 * Copyright 2005 European Media Laboratories Research gGmbH
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
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */

#include <string>
#include <sstream>

#include <common/common.h>
#include <common/extern.h>


#include "sbml/Trigger.h"
#include "math/ASTNode.h"
//#include "sbml/SpeciesType.h"
//#include "sbml/Unit.h"
//#include "sbml/UnitDefinition.h"
//#include "xml/XMLNode.h"
//#include "xml/XMLAttributes.h"
//#include "xml/XMLTriple.h"
//#include "xml/XMLToken.h"
#include "annotation/CVTerm.h"
#include "annotation/ModelHistory.h"
#include "xml/XMLInputStream.h"
#include "xml/XMLOutputStream.h"
#include "units/FormulaUnitsData.h"
//#include "units/UnitKindList.h"
//#include "sbml/Compartment.h"
#include "sbml/Constraint.h"
#include "sbml/Delay.h"
#include "sbml/Event.h"
//#include "sbml/SBML.h"
#include "sbml/SBMLDocument.h"
#include "xml/XMLErrorLog.h"
//#include "sbml/SBMLReader.h"
//#include "sbml/SBMLErrorLog.h"
//#include "sbml/SBMLVisitor.h"
//#include "sbml/SBMLWriter.h"
#include "sbml/Species.h"
#include "sbml/SpeciesReference.h"
#include "sbml/EventAssignment.h"
#include "sbml/FunctionDefinition.h"
#include "sbml/InitialAssignment.h"
#include "sbml/KineticLaw.h"
#include "sbml/ListOf.h"
#include "sbml/Model.h"
//#include "sbml/Parameter.h"
#include "sbml/Reaction.h"
#include "sbml/Rule.h"




#include <check.h>

BEGIN_C_DECLS



void
MiscTest_setup (void)
{
}

void 
MiscTest_teardown (void)
{
}

START_TEST ( test_ListOf_copyConstructor )
{
    ListOf* o1=new ListOf();
    Species* s=new Species("species_1");
    o1->append(s);
    delete s;
    ListOf* o2=new ListOf(*o1);
    fail_unless(o2->size()==1);
    // make sure the object got copied
    fail_unless(static_cast<Species*>(o2->get(0))->getId()=="species_1");
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ListOf_assignmentOperator )
{
    ListOf* o1=new ListOf();
    Species* s=new Species("species_1");
    o1->append(s);
    delete s;
    ListOf* o2;
    (*o2)=*o1;
    fail_unless(o2->size()==1);
    // make sure the object got copied
    fail_unless(static_cast<Species*>(o2->get(0))->getId()=="species_1");
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ListOf_clone )
{
    ListOf* o1=new ListOf();
    Species* s=new Species("species_1");
    o1->append(s);
    delete s;
    ListOf* o2=static_cast<ListOf*>(o1->clone());
    fail_unless(o2->size()==1);
    // make sure the object got copied
    fail_unless(static_cast<Species*>(o2->get(0))->getId()=="species_1");
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Model_copyConstructor )
{
    Model* o1=new Model("model_1");
    FormulaUnitsData* fud=new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    Model* o2=new Model(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Model_assignmentOperator )
{
    Model* o1=new Model("model_1");
    FormulaUnitsData* fud=new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    Model* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Model_clone )
{
    Model* o1=new Model("model_1");
    FormulaUnitsData* fud=new FormulaUnitsData();
    o1->addFormulaUnitsData(fud);
    delete fud;
    Model* o2=static_cast<Model*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Reaction_copyConstructor )
{
    Reaction* o1=new Reaction("reaction_1");
    KineticLaw* kl=new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;
    Reaction* o2=new Reaction(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Reaction_assignmentOperator )
{
    Reaction* o1=new Reaction("reaction_1");
    KineticLaw* kl=new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;
    Reaction* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Reaction_clone )
{
    Reaction* o1=new Reaction("reaction_1");
    KineticLaw* kl=new KineticLaw();
    o1->setKineticLaw(kl);
    delete kl;
    Reaction* o2=static_cast<Reaction*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Rule_copyConstructor )
{
    Rule* o1=new RateRule("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    Rule* o2=new Rule(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Rule_assignmentOperator )
{
    Rule* o1=new RateRule("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    Rule* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Rule_clone )
{
    Rule* o1=new RateRule("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    Rule* o2=static_cast<Rule*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_KineticLaw_copyConstructor )
{
    KineticLaw* o1=new KineticLaw();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    KineticLaw* o2=new KineticLaw(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_KineticLaw_assignmentOperator )
{
    KineticLaw* o1=new KineticLaw();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    KineticLaw* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_KineticLaw_clone )
{
    KineticLaw* o1=new KineticLaw();
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    KineticLaw* o2=static_cast<KineticLaw*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_InitialAssignment_copyConstructor )
{
    InitialAssignment* o1=new InitialAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    InitialAssignment* o2=new InitialAssignment(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_InitialAssignment_assignmentOperator )
{
    InitialAssignment* o1=new InitialAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    InitialAssignment* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_InitialAssignment_clone )
{
    InitialAssignment* o1=new InitialAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    InitialAssignment* o2=static_cast<InitialAssignment*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_FunctionDefinition_copyConstructor )
{
    FunctionDefinition* o1=new FunctionDefinition("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    FunctionDefinition* o2=new FunctionDefinition(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_FunctionDefinition_assignmentOperator )
{
    FunctionDefinition* o1=new FunctionDefinition("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    FunctionDefinition* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_FunctionDefinition_clone )
{
    FunctionDefinition* o1=new FunctionDefinition("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    FunctionDefinition* o2=static_cast<FunctionDefinition*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_EventAssignment_copyConstructor )
{
    EventAssignment* o1=new EventAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    EventAssignment* o2=new EventAssignment(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_EventAssignment_assignmentOperator )
{
    EventAssignment* o1=new EventAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    EventAssignment* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_EventAssignment_clone )
{
    EventAssignment* o1=new EventAssignment("a");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setMath(node);
    delete node;
    EventAssignment* o2=static_cast<EventAssignment*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


/*
START_TEST ( test_XMLErrorLog_copyConstructor )
{
    XMLErrorLog* o1=new XMLErrorLog();
    XMLErrorLog* o2=new XMLErrorLog(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLErrorLog_assignmentOperator )
{
    XMLErrorLog* o1=new XMLErrorLog();
    XMLErrorLog* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLErrorLog_clone )
{
    XMLErrorLog* o1=new XMLErrorLog();
    XMLErrorLog* o2=static_cast<XMLErrorLog*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

START_TEST ( test_SpeciesReference_copyConstructor )
{
    SpeciesReference* o1=new SpeciesReference("species_1");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setStoichiometryMath(node);
    delete node;
    SpeciesReference* o2=new SpeciesReference(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SpeciesReference_assignmentOperator )
{
    SpeciesReference* o1=new SpeciesReference("species_1");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setStoichiometryMath(node);
    delete node;
    SpeciesReference* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesReference_clone )
{
    SpeciesReference* o1=new SpeciesReference("species_1");
    ASTNode* node=new ASTNode(AST_CONSTANT_PI);
    o1->setStoichiometryMath(node);
    delete node;
    SpeciesReference* o2=static_cast<SpeciesReference*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLDocument_copyConstructor )
{
    SBMLDocument* o1=new SBMLDocument(2,2);
    Model* pModel=new Model("model_1","Test Model");
    o1->setModel(pModel);
    delete pModel;
    SBMLDocument* o2=new SBMLDocument(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBMLDocument_assignmentOperator )
{
    SBMLDocument* o1=new SBMLDocument(2,2);
    Model* pModel=new Model("model_1","Test Model");
    o1->setModel(pModel);
    delete pModel;
    SBMLDocument* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLDocument_clone )
{
    SBMLDocument* o1=new SBMLDocument(2,2);
    Model* pModel=new Model("model_1","Test Model");
    o1->setModel(pModel);
    delete pModel;
    SBMLDocument* o2=static_cast<SBMLDocument*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Event_copyConstructor )
{
    Event* o1=new Event();
    Delay d;
    Trigger t;
    o1->setDelay(&d);
    o1->setTrigger(&t);
    Event* o2=new Event(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Event_assignmentOperator )
{
    Event* o1=new Event();
    Delay d;
    Trigger t;
    o1->setDelay(&d);
    o1->setTrigger(&t);
    Event* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Event_clone )
{
    Event* o1=new Event();
    Delay d;
    Trigger t;
    o1->setDelay(&d);
    o1->setTrigger(&t);
    Event* o2=static_cast<Event*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Delay_copyConstructor )
{
    Delay* o1=new Delay();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Delay* o2=new Delay(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Delay_assignmentOperator )
{
    Delay* o1=new Delay();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Delay* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Delay_clone )
{
    Delay* o1=new Delay();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Delay* o2=static_cast<Delay*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Constraint_copyConstructor )
{
    Constraint* o1=new Constraint();
    XMLNode node;
    ASTNode node2(AST_CONSTANT_PI);
    o1->setMath(&node2);
    o1->setMessage(&node);
    Constraint* o2=new Constraint(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Constraint_assignmentOperator )
{
    Constraint* o1=new Constraint();
    XMLNode node;
    ASTNode node2(AST_CONSTANT_PI);
    o1->setMath(&node2);
    o1->setMessage(&node);
    Constraint* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Constraint_clone )
{
    Constraint* o1=new Constraint();
    XMLNode node;
    ASTNode node2(AST_CONSTANT_PI);
    o1->setMath(&node2);
    o1->setMessage(&node);
    Constraint* o2=static_cast<Constraint*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_FormulaUnitsData_copyConstructor )
{
    FormulaUnitsData* o1=new FormulaUnitsData();
    o1->setUnitDefinition(new UnitDefinition());
    o1->setPerTimeUnitDefinition(new UnitDefinition());
    o1->setEventTimeUnitDefinition(new UnitDefinition());
    FormulaUnitsData* o2=new FormulaUnitsData(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_FormulaUnitsData_assignmentOperator )
{
    FormulaUnitsData* o1=new FormulaUnitsData();
    o1->setUnitDefinition(new UnitDefinition());
    o1->setPerTimeUnitDefinition(new UnitDefinition());
    o1->setEventTimeUnitDefinition(new UnitDefinition());
    FormulaUnitsData* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLOutputStream_copyConstructor )
{
    std::ostringstream stream;
    XMLOutputStream* o1=new XMLOutputStream(stream);
    XMLOutputStream* o2=new XMLOutputStream(*o1);
    delete o2;
    delete o1;
}
END_TEST

/**
 * Assignment operator is not generated by the compiler.
START_TEST ( test_XMLOutputStream_assignmentOperator )
{
    std::ostringstream stream;
    XMLOutputStream* o1=new XMLOutputStream(stream);
    XMLOutputStream* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST
*/

START_TEST ( test_XMLInputStream_copyConstructor )
{
    const char* content = 
      "<annotation>\n"
      "<listOfLayouts xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
      "  <layout id=\"layout_1\">\n"
      "    <dimensions width=\"200\" height=\"400\"/>\n" 
      "    <listOfCompartmentGlyphs>\n"
      "      <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </compartmentGlyph>\n"
      "    </listOfCompartmentGlyphs>\n"
      "    <listOfSpeciesGlyphs>\n"
      "      <speciesGlyph id=\"speciesGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </speciesGlyph>\n"
      "    </listOfSpeciesGlyphs>\n"
      "    <listOfReactionGlyphs>\n"
      "      <reactionGlyph id=\"reactionGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </reactionGlyph>\n"
      "    </listOfReactionGlyphs>\n"
      "    <listOfTextGlyphs>\n"
      "      <textGlyph id=\"textGlyph_1\" text=\"test\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </textGlyph>\n"
      "    </listOfTextGlyphs>\n"
      "    <listOfAdditionalGraphicalObjects>\n"
      "      <graphicalObject id=\"graphicalObject_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </graphicalObject>\n"
      "    </listOfAdditionalGraphicalObjects>\n"
      "  </layout>\n"
      "</listOfLayouts>\n"
      "</annotation>\n"
    ;
    XMLInputStream* o1=new XMLInputStream(content,false);
    XMLInputStream* o2=new XMLInputStream(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLInputStream_assignmentOperator )
{
    const char* content = 
      "<annotation>\n"
      "<listOfLayouts xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
      "  <layout id=\"layout_1\">\n"
      "    <dimensions width=\"200\" height=\"400\"/>\n" 
      "    <listOfCompartmentGlyphs>\n"
      "      <compartmentGlyph id=\"compartmentGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </compartmentGlyph>\n"
      "    </listOfCompartmentGlyphs>\n"
      "    <listOfSpeciesGlyphs>\n"
      "      <speciesGlyph id=\"speciesGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </speciesGlyph>\n"
      "    </listOfSpeciesGlyphs>\n"
      "    <listOfReactionGlyphs>\n"
      "      <reactionGlyph id=\"reactionGlyph_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </reactionGlyph>\n"
      "    </listOfReactionGlyphs>\n"
      "    <listOfTextGlyphs>\n"
      "      <textGlyph id=\"textGlyph_1\" text=\"test\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </textGlyph>\n"
      "    </listOfTextGlyphs>\n"
      "    <listOfAdditionalGraphicalObjects>\n"
      "      <graphicalObject id=\"graphicalObject_1\">\n"
      "        <boundingBox>\n"
      "          <position x=\"0\" y=\"0\"/>\n"
      "          <dimensions width=\"0\" height=\"0\"/>\n" 
      "        </boundingBox>\n"  
      "      </graphicalObject>\n"
      "    </listOfAdditionalGraphicalObjects>\n"
      "  </layout>\n"
      "</listOfLayouts>\n"
      "</annotation>\n"
    ;
    XMLInputStream* o1=new XMLInputStream(content,false);
    XMLInputStream* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST



START_TEST ( test_ASTNode_copyConstructor )
{
    ASTNode* o1=new ASTNode(AST_FUNCTION_COS);
    o1->addChild(new ASTNode(AST_CONSTANT_PI));
    o1->setName("cos");
    ASTNode* o2=new ASTNode(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ASTNode_assignmentOperator )
{
    ASTNode* o1=new ASTNode(AST_FUNCTION_COS);
    o1->addChild(new ASTNode(AST_CONSTANT_PI));
    o1->setName("cos");
    ASTNode* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ModelHistory_copyConstructor )
{
    ModelHistory* o1=new ModelHistory();
    o1->setCreatedDate(new Date());
    o1->setModifiedDate(new Date());
    o1->addCreator(new ModelCreator());
    ModelHistory* o2=new ModelHistory(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ModelHistory_assignmentOperator )
{
    ModelHistory* o1=new ModelHistory();
    o1->setCreatedDate(new Date());
    o1->setModifiedDate(new Date());
    o1->addCreator(new ModelCreator());
    ModelHistory* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ModelHistory_clone )
{
    ModelHistory* o1=new ModelHistory();
    o1->setCreatedDate(new Date());
    o1->setModifiedDate(new Date());
    o1->addCreator(new ModelCreator());
    ModelHistory* o2=static_cast<ModelHistory*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_CVTerm_copyConstructor )
{
    CVTerm* o1=new CVTerm();
    o1->addResource("test_ressource");
    CVTerm* o2=new CVTerm(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_CVTerm_assignmentOperator )
{
    CVTerm* o1=new CVTerm();
    o1->addResource("test_ressource");
    CVTerm* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_CVTerm_clone )
{
    CVTerm* o1=new CVTerm();
    o1->addResource("test_ressource");
    CVTerm* o2=static_cast<CVTerm*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBase_copyConstructor )
{
    Species* o1=new Species();
    XMLNode* notes=new XMLNode();
    o1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    o1->setAnnotation(annotation);
    ModelHistory* mh=new ModelHistory();
    o1->setModelHistory(mh);
    CVTerm* cv=new CVTerm();
    o1->addCVTerm(cv);
    SBase* o2=new Species(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBase_assignmentOperator )
{
    SBase* o1=new Species();
    XMLNode* notes=new XMLNode();
    o1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    o1->setAnnotation(annotation);
    ModelHistory* mh=new ModelHistory();
    o1->setModelHistory(mh);
    CVTerm* cv=new CVTerm();
    o1->addCVTerm(cv);
    SBase* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBase_clone )
{
    SBase* o1=new Species();
    XMLNode* notes=new XMLNode();
    o1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    o1->setAnnotation(annotation);
    ModelHistory* mh=new ModelHistory();
    o1->setModelHistory(mh);
    CVTerm* cv=new CVTerm();
    o1->addCVTerm(cv);
    SBase* o2=o1->clone();
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Trigger_copyConstructor )
{
    Trigger* o1=new Trigger();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Trigger* o2=new Trigger(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Trigger_assignmentOperator )
{
    Trigger* o1=new Trigger();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Trigger* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Trigger_clone )
{
    Trigger* o1=new Trigger();
    ASTNode node(AST_CONSTANT_PI);
    o1->setMath(&node);
    Trigger* o2=static_cast<Trigger*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST

/*
START_TEST ( test_UnitDefinition_copyConstructor )
{
    UnitDefinition* o1=new UnitDefinition();
    UnitDefinition* o2=new UnitDefinition(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_UnitDefinition_assignmentOperator )
{
    UnitDefinition* o1=new UnitDefinition();
    UnitDefinition* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_UnitDefinition_clone )
{
    UnitDefinition* o1=new UnitDefinition();
    UnitDefinition* o2=static_cast<UnitDefinition*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_Unit_copyConstructor )
{
    Unit* o1=new Unit();
    Unit* o2=new Unit(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Unit_assignmentOperator )
{
    Unit* o1=new Unit();
    Unit* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Unit_clone )
{
    Unit* o1=new Unit();
    Unit* o2=static_cast<Unit*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SpeciesType_copyConstructor )
{
    SpeciesType* o1=new SpeciesType();
    SpeciesType* o2=new SpeciesType(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SpeciesType_assignmentOperator )
{
    SpeciesType* o1=new SpeciesType();
    SpeciesType* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SpeciesType_clone )
{
    SpeciesType* o1=new SpeciesType();
    SpeciesType* o2=static_cast<SpeciesType*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_XMLNode_copyConstructor )
{
    XMLNode* o1=new XMLNode();
    XMLNode* o2=new XMLNode(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLNode_assignmentOperator )
{
    XMLNode* o1=new XMLNode();
    XMLNode* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLNode_clone )
{
    XMLNode* o1=new XMLNode();
    XMLNode* o2=static_cast<XMLNode*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_XMLAttributes_copyConstructor )
{
    XMLAttributes* o1=new XMLAttributes();
    XMLAttributes* o2=new XMLAttributes(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLAttributes_assignmentOperator )
{
    XMLAttributes* o1=new XMLAttributes();
    XMLAttributes* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLAttributes_clone )
{
    XMLAttributes* o1=new XMLAttributes();
    XMLAttributes* o2=static_cast<XMLAttributes*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_XMLTriple_copyConstructor )
{
    XMLTriple* o1=new XMLTriple();
    XMLTriple* o2=new XMLTriple(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLTriple_assignmentOperator )
{
    XMLTriple* o1=new XMLTriple();
    XMLTriple* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLTriple_clone )
{
    XMLTriple* o1=new XMLTriple();
    XMLTriple* o2=static_cast<XMLTriple*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_XMLToken_copyConstructor )
{
    XMLToken* o1=new XMLToken();
    XMLToken* o2=new XMLToken(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_XMLToken_assignmentOperator )
{
    XMLToken* o1=new XMLToken();
    XMLToken* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_XMLToken_clone )
{
    XMLToken* o1=new XMLToken();
    XMLToken* o2=static_cast<XMLToke*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_ModelHistory_copyConstructor )
{
    ModelHistory* o1=new ModelHistory();
    ModelHistory* o2=new ModelHistory(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ModelHistory_assignmentOperator )
{
    ModelHistory* o1=new ModelHistory();
    ModelHistory* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ModelHistory_clone )
{
    ModelHistory* o1=new ModelHistory();
    ModelHistory* o2=static_cast<ModelHistory*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_Date_copyConstructor )
{
    Date* o1=new Date();
    Date* o2=new Date(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Date_assignmentOperator )
{
    Date* o1=new Date();
    Date* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Date_clone )
{
    Date* o1=new Date();
    Date* o2=static_cast<Date*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_UnitKindList_copyConstructor )
{
    UnitKindList* o1=new UnitKindList();
    UnitKindList* o2=new UnitKindList(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_UnitKindList_assignmentOperator )
{
    UnitKindList* o1=new UnitKindList();
    UnitKindList* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_UnitKindList_clone )
{
    UnitKindList* o1=new UnitKindList();
    UnitKindList* o2=static_cast<UnitKindList*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_Compartment_copyConstructor )
{
    Compartment* o1=new Compartment();
    Compartment* o2=new Compartment(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Compartment_assignmentOperator )
{
    Compartment* o1=new Compartment();
    Compartment* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Compartment_clone )
{
    Compartment* o1=new Compartment();
    Compartment* o2=static_cast<Compartment*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SBML_copyConstructor )
{
    SBML* o1=new SBML();
    SBML* o2=new SBML(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBML_assignmentOperator )
{
    SBML* o1=new SBML();
    SBML* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBML_clone )
{
    SBML* o1=new SBML();
    SBML* o2=static_cast<SBML*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SBMLReader_copyConstructor )
{
    SBMLReader* o1=new SBMLReader();
    SBMLReader* o2=new SBMLReader(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBMLReader_assignmentOperator )
{
    SBMLReader* o1=new SBMLReader();
    SBMLReader* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLReader_clone )
{
    SBMLReader* o1=new SBMLReader();
    SBMLReader* o2=static_cast<SBMLReader*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SBMLErrorLog_copyConstructor )
{
    SBMLErrorLog* o1=new SBMLErrorLog();
    SBMLErrorLog* o2=new SBMLErrorLog(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBMLErrorLog_assignmentOperator )
{
    SBMLErrorLog* o1=new SBMLErrorLog();
    SBMLErrorLog* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLErrorLog_clone )
{
    SBMLErrorLog* o1=new SBMLErrorLog();
    SBMLErrorLog* o2=static_cast<SBMLErrorLog*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SBMLVisitor_copyConstructor )
{
    SBMLVisitor* o1=new SBMLVisitor();
    SBMLVisitor* o2=new SBMLVisitor(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBMLVisitor_assignmentOperator )
{
    SBMLVisitor* o1=new SBMLVisitor();
    SBMLVisitor* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLVisitor_clone )
{
    SBMLVisitor* o1=new SBMLVisitor();
    SBMLVisitor* o2=static_cast<SBMLVisitor*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_SBMLWriter_copyConstructor )
{
    SBMLWriter* o1=new SBMLWriter();
    SBMLWriter* o2=new SBMLWriter(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SBMLWriter_assignmentOperator )
{
    SBMLWriter* o1=new SBMLWriter();
    SBMLWriter* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SBMLWriter_clone )
{
    SBMLWriter* o1=new SBMLWriter();
    SBMLWriter* o2=static_cast<SBMLWriter*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_SimpleSpeciesReference_copyConstructor )
{
    SimpleSpeciesReference* o1=new SimpleSpeciesReference();
    SimpleSpeciesReference* o2=new SimpleSpeciesReference(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_SimpleSpeciesReference_assignmentOperator )
{
    SimpleSpeciesReference* o1=new SimpleSpeciesReference();
    SimpleSpeciesReference* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_SimpleSpeciesReference_clone )
{
    SimpleSpeciesReference* o1=new SimpleSpeciesReference();
    SimpleSpeciesReference* o2=static_cast<SimpleSpeciesReference*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/


/*
START_TEST ( test_Parameter_copyConstructor )
{
    Parameter* o1=new Parameter();
    Parameter* o2=new Parameter(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Parameter_assignmentOperator )
{
    Parameter* o1=new Parameter();
    Parameter* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Parameter_clone )
{
    Parameter* o1=new Parameter();
    Parameter* o2=static_cast<Parameter*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_ModelCreator_copyConstructor )
{
    ModelCreator* o1=new ModelCreator();
    ModelCreator* o2=new ModelCreator(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_ModelCreator_assignmentOperator )
{
    ModelCreator* o1=new ModelCreator();
    ModelCreator* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_ModelCreator_clone )
{
    ModelCreator* o1=new ModelCreator();
    ModelCreator* o2=static_cast<ModelCreator*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/

/*
START_TEST ( test_Species_copyConstructor )
{
    Species* o1=new Species();
    Species* o2=new Species(*o1);
    delete o2;
    delete o1;
}
END_TEST

START_TEST ( test_Species_assignmentOperator )
{
    Species* o1=new Species();
    Species* o2;
    (*o2)=*o1;
    delete o2;
    delete o1;
}
END_TEST


START_TEST ( test_Species_clone )
{
    Species* o1=new Species();
    Species* o2=static_cast<Species*>(o1->clone());
    delete o2;
    delete o1;
}
END_TEST
*/



Suite *
create_suite_Misc (void)
{
  Suite *suite = suite_create("Misc");
  TCase *tcase = tcase_create("Misc");

  tcase_add_checked_fixture( tcase,
                             MiscTest_setup,
                             MiscTest_teardown ); 

  tcase_add_test( tcase, test_SBase_copyConstructor                       );
  tcase_add_test( tcase, test_SBase_assignmentOperator                    );
  tcase_add_test( tcase, test_SBase_clone                                 );
  tcase_add_test( tcase, test_Trigger_copyConstructor                     );
  tcase_add_test( tcase, test_Trigger_assignmentOperator                  );
  tcase_add_test( tcase, test_Trigger_clone                               );
  tcase_add_test( tcase, test_Delay_copyConstructor                       );
  tcase_add_test( tcase, test_Delay_assignmentOperator                    );
  tcase_add_test( tcase, test_Delay_clone                                 );
  tcase_add_test( tcase, test_Event_copyConstructor                       );
  tcase_add_test( tcase, test_Event_assignmentOperator                    );
  tcase_add_test( tcase, test_Event_clone                                 );
  tcase_add_test( tcase, test_Constraint_copyConstructor                  );
  tcase_add_test( tcase, test_Constraint_assignmentOperator               );
  tcase_add_test( tcase, test_Constraint_clone                            );
  tcase_add_test( tcase, test_FormulaUnitsData_copyConstructor            );
  tcase_add_test( tcase, test_FormulaUnitsData_assignmentOperator         );
  tcase_add_test( tcase, test_XMLOutputStream_copyConstructor             );
  tcase_add_test( tcase, test_XMLInputStream_copyConstructor              );
  tcase_add_test( tcase, test_XMLInputStream_assignmentOperator           );
  tcase_add_test( tcase, test_ASTNode_copyConstructor                     );
  tcase_add_test( tcase, test_ASTNode_assignmentOperator                  );
  tcase_add_test( tcase, test_SBase_copyConstructor                       );
  tcase_add_test( tcase, test_SBase_assignmentOperator                    );
  tcase_add_test( tcase, test_SBase_clone                                 );
  tcase_add_test( tcase, test_ModelHistory_copyConstructor                );
  tcase_add_test( tcase, test_ModelHistory_assignmentOperator             );
  tcase_add_test( tcase, test_ModelHistory_clone                          );
  tcase_add_test( tcase, test_CVTerm_copyConstructor                      );
  tcase_add_test( tcase, test_CVTerm_assignmentOperator                   );
  tcase_add_test( tcase, test_CVTerm_clone                                );
  tcase_add_test( tcase, test_SBMLDocument_copyConstructor                );
  tcase_add_test( tcase, test_SBMLDocument_assignmentOperator             );
  tcase_add_test( tcase, test_SBMLDocument_clone                          );
  tcase_add_test( tcase, test_SpeciesReference_copyConstructor            );
  tcase_add_test( tcase, test_SpeciesReference_assignmentOperator         );
  tcase_add_test( tcase, test_SpeciesReference_clone                      );
  tcase_add_test( tcase, test_EventAssignment_copyConstructor             );
  tcase_add_test( tcase, test_EventAssignment_assignmentOperator          );
  tcase_add_test( tcase, test_EventAssignment_clone                       );
  tcase_add_test( tcase, test_FunctionDefinition_copyConstructor          );
  tcase_add_test( tcase, test_FunctionDefinition_assignmentOperator       );
  tcase_add_test( tcase, test_FunctionDefinition_clone                    );
  tcase_add_test( tcase, test_InitialAssignment_copyConstructor           );
  tcase_add_test( tcase, test_InitialAssignment_assignmentOperator        );
  tcase_add_test( tcase, test_InitialAssignment_clone                     );
  tcase_add_test( tcase, test_KineticLaw_copyConstructor                  );
  tcase_add_test( tcase, test_KineticLaw_assignmentOperator               );
  tcase_add_test( tcase, test_KineticLaw_clone                            );
  tcase_add_test( tcase, test_ListOf_copyConstructor                      );
  tcase_add_test( tcase, test_ListOf_assignmentOperator                   );
  tcase_add_test( tcase, test_ListOf_clone                                );
  tcase_add_test( tcase, test_Model_copyConstructor                       );
  tcase_add_test( tcase, test_Model_assignmentOperator                    );
  tcase_add_test( tcase, test_Model_clone                                 );
  tcase_add_test( tcase, test_Reaction_copyConstructor                    );
  tcase_add_test( tcase, test_Reaction_assignmentOperator                 );
  tcase_add_test( tcase, test_Reaction_clone                              );
  tcase_add_test( tcase, test_Rule_copyConstructor                        );
  tcase_add_test( tcase, test_Rule_assignmentOperator                     );
  tcase_add_test( tcase, test_Rule_clone                                  );
//  tcase_add_test( tcase, test_ModelCreator_copyConstructor                );
//  tcase_add_test( tcase, test_ModelCreator_assignmentOperator             );
//  tcase_add_test( tcase, test_ModelCreator_clone                          );
//  tcase_add_test( tcase, test_SpeciesType_copyConstructor                 );
//  tcase_add_test( tcase, test_SpeciesType_assignmentOperator              );
//  tcase_add_test( tcase, test_SpeciesType_clone                           );
//  tcase_add_test( tcase, test_Unit_copyConstructor                        );
//  tcase_add_test( tcase, test_Unit_assignmentOperator                     );
//  tcase_add_test( tcase, test_Unit_clone                                  );
//  tcase_add_test( tcase, test_UnitDefinition_copyConstructor              );
//  tcase_add_test( tcase, test_UnitDefinition_assignmentOperator           );
//  tcase_add_test( tcase, test_UnitDefinition_clone                        );
//  tcase_add_test( tcase, test_XMLNode_copyConstructor                     );
//  tcase_add_test( tcase, test_XMLNode_assignmentOperator                  );
//  tcase_add_test( tcase, test_XMLNode_clone                               );
//  tcase_add_test( tcase, test_XMLAttributes_copyConstructor               );
//  tcase_add_test( tcase, test_XMLAttributes_assignmentOperator            );
//  tcase_add_test( tcase, test_XMLAttributes_clone                         );
//  tcase_add_test( tcase, test_XMLTriple_copyConstructor                   );
//  tcase_add_test( tcase, test_XMLTriple_assignmentOperator                );
//  tcase_add_test( tcase, test_XMLTriple_clone                             );
//  tcase_add_test( tcase, test_XMLToken_copyConstructor                    );
//  tcase_add_test( tcase, test_XMLToken_assignmentOperator                 );
//  tcase_add_test( tcase, test_XMLToken_clone                              );
//  tcase_add_test( tcase, test_ModelHistory_copyConstructor                );
//  tcase_add_test( tcase, test_ModelHistory_assignmentOperator             );
//  tcase_add_test( tcase, test_ModelHistory_clone                          );
//  tcase_add_test( tcase, test_Date_copyConstructor                        );
//  tcase_add_test( tcase, test_Date_assignmentOperator                     );
//  tcase_add_test( tcase, test_Date_clone                                  );
//  tcase_add_test( tcase, test_UnitKindList_copyConstructor                );
//  tcase_add_test( tcase, test_UnitKindList_assignmentOperator             );
//  tcase_add_test( tcase, test_UnitKindList_clone                          );
//  tcase_add_test( tcase, test_SBML_copyConstructor                        );
//  tcase_add_test( tcase, test_SBML_assignmentOperator                     );
//  tcase_add_test( tcase, test_SBML_clone                                  );
//  tcase_add_test( tcase, test_SBMLReader_copyConstructor                  );
//  tcase_add_test( tcase, test_SBMLReader_assignmentOperator               );
//  tcase_add_test( tcase, test_SBMLReader_clone                            );
//  tcase_add_test( tcase, test_SBMLErrorLog_copyConstructor                );
//  tcase_add_test( tcase, test_SBMLErrorLog_assignmentOperator             );
//  tcase_add_test( tcase, test_SBMLErrorLog_clone                          );
//  tcase_add_test( tcase, test_SBMLVisitor_copyConstructor                 ); 
//  tcase_add_test( tcase, test_SBMLVisitor_assignmentOperator              );
//  tcase_add_test( tcase, test_SBMLVisitor_clone                           );
//  tcase_add_test( tcase, test_SBMLWriter_copyConstructor                  );
//  tcase_add_test( tcase, test_SBMLWriter_assignmentOperator               );
//  tcase_add_test( tcase, test_SBMLWriter_clone                            );
//  tcase_add_test( tcase, test_SimpleSpeciesReference_copyConstructor      );
//  tcase_add_test( tcase, test_SimpleSpeciesReference_assignmentOperator   );
//  tcase_add_test( tcase, test_SimpleSpeciesReference_clone                );
//  tcase_add_test( tcase, test_ModifierSpeciesReference_copyConstructor    );
//  tcase_add_test( tcase, test_ModifierSpeciesReference_assignmentOperator );
//  tcase_add_test( tcase, test_ModifierSpeciesReference_clone              );
//  tcase_add_test( tcase, test_Parameter_copyConstructor                   );
//  tcase_add_test( tcase, test_Parameter_assignmentOperator                );
//  tcase_add_test( tcase, test_Parameter_clone                             );
//  tcase_add_test( tcase, test_Species_copyConstructor                     );
//  tcase_add_test( tcase, test_Species_assignmentOperator                  );
//  tcase_add_test( tcase, test_Species_clone                               );
//  tcase_add_test( tcase, test_XMLErrorLog_copyConstructor                 );
//  tcase_add_test( tcase, test_XMLErrorLog_assignmentOperator              );
//  tcase_add_test( tcase, test_XMLErrorLog_clone                           );
                        
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
