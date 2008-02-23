##
## @file    TestPython.py
## @brief   Acceptance tests for libSBML Python module
## @author  Ben Bornstein
## @author  Ben Kovitz
## @author  Akiya Jouraku
## 
## $Id$
## $Source$
##
##<!---------------------------------------------------------------------------
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##
## Copyright 2005-2007 California Institute of Technology.
## Copyright 2002-2005 California Institute of Technology and
##                     Japan Science and Technology Corporation.
## 
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation.  A copy of the license agreement is provided
## in the file named "LICENSE.txt" included with this software distribution
## and also available online as http://sbml.org/software/libsbml/license.html
##----------------------------------------------------------------------- -->*/

import sys
import unittest
import libsbml

  
class TestPython(unittest.TestCase):

  def testEquality(self):
     lo = libsbml.ListOf()
     c1 = libsbml.Compartment()
  
     lo.append(c1)
     c2 = lo.get(0)
   
     self.assert_(c1.toSBML() == c2.toSBML())
     self.assert_( c1 != c2 )
  
     c2 = 7
     self.assert_( c1 != c2 )
  
  
  def testImplicitDowncastOfRule(self):
     m = libsbml.Model()
     self.assert_(m)
  
     r = libsbml.AssignmentRule("x", "1 + 1")
     self.assert_(r)
  
     m.addRule(r)
     self.assert_(m.getNumRules() == 1)
  
     r = m.getRule(0)
     self.assert_(r.getVariable() == "x")
     self.assert_(r.getFormula()  == "1 + 1")
  
     self.assert_("AssignmentRule" in r.__class__.__name__)
  
  
  def testImplicitDowncastOfSBase(self):
     species = libsbml.Species()
     self.assert_(species)
  
     species.setMetaId('foo')
     self.assert_(species.getMetaId() == 'foo')
  
     lst = libsbml.ListOf()
     self.assert_(len(lst) == 0)
     self.assert_(lst.size() == 0)
  
     lst.append(species)
     self.assert_(lst.size() == 1)
  
     theSameSpecies = lst.get(0)
     self.assert_(theSameSpecies.getMetaId() == "foo")
  
     noElement = lst.get(51)
     self.assert_(noElement is None)
  
     alsoTheSameSpecies = lst[0]
     self.assert_(alsoTheSameSpecies.getMetaId() == "foo")
  
  
  def testSpecies(self):
     species = libsbml.Species()
     species.setMetaId('foo')
     self.assert_(species.getMetaId() == "foo")
  
     self.assert_(species.getInitialConcentration() == 0.0)
  
     species.unsetInitialConcentration()
  
     str(species)
  
  
  def testReaction(self):
     reaction = libsbml.Reaction("R", "R", libsbml.KineticLaw("1 + 1"))
     self.assert_(reaction)
  
     speciesReference = libsbml.SpeciesReference("SR")
     self.assert_(speciesReference)
  
     # Reactant
  
     reaction.addReactant(speciesReference)
     self.assert_(reaction.getNumReactants() == 1)
  
     theSameSpeciesReference = reaction.getReactant(0)
     self.assert_(theSameSpeciesReference.getSpecies() == speciesReference.getSpecies())
     
     theSameSpeciesReference = reaction.getListOfReactants()[0]
     self.assert_(theSameSpeciesReference.getSpecies() == speciesReference.getSpecies())
  
     # KineticLaw
  
     kineticLaw = libsbml.KineticLaw("1 + 1")
     self.assert_(kineticLaw)
     self.assert_(kineticLaw.getFormula() == "1 + 1")
  
     reaction = libsbml.Reaction("R", "R", None, False)
     reaction.setKineticLaw(kineticLaw)
     self.assert_(kineticLaw.toSBML() == reaction.getKineticLaw().toSBML())
     self.assert_( kineticLaw != reaction.getKineticLaw() )
  
     # Product
  
     productReference = libsbml.SpeciesReference("product")
     self.assert_(productReference)
  
     reaction.addProduct(productReference)
     self.assert_(reaction.getNumProducts() == 1)
  
     theSameSpeciesReference = reaction.getProduct(0)
     self.assert_(theSameSpeciesReference.getSpecies() == productReference.getSpecies())
  
     # Modifier
  
     modifierReference = libsbml.ModifierSpeciesReference("modifier")
     self.assert_(modifierReference)
  
     reaction.addModifier(modifierReference)
     self.assert_(reaction.getNumModifiers() == 1)
  
     theSameSpeciesReference = reaction.getModifier(0)
     self.assert_(theSameSpeciesReference.getSpecies() == modifierReference.getSpecies())
  
     str(reaction)
  
  
  def testDocument(self):
     d = libsbml.SBMLDocument()
     self.assert_(d)
     
     m = libsbml.Model()
     self.assert_(m)
  
     d.setModel(m)
     self.assert_(m.toSBML() == d.getModel().toSBML())
     self.assert_( m != d.getModel() )
  
     str(d)
     str(d.getModel())
  
  
  def testFunctionDefinition(self):
     fd = libsbml.FunctionDefinition()
     self.assert_(fd)
  
     fd = libsbml.FunctionDefinition("id1", "formula0")
     self.assert_(fd)
  
     formula1 = libsbml.parseFormula("2 + 8")
     self.assert_(formula1.thisown == 1)
  
     fd = libsbml.FunctionDefinition("id", formula1)
     self.assert_(fd and fd.thisown == 1)
  
     formula = libsbml.parseFormula("1 + 1")
     fd.setMath(formula)
  
     str(fd)
  
  
  def testUnitDefinition(self):
     ud = libsbml.UnitDefinition()
     self.assert_(ud)
  
     unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1)
     self.assert_(unit.thisown)
     ud.addUnit(unit);
  
     str(ud)
  
  
  def testSpeciesReference(self):
     speciesReference = libsbml.SpeciesReference("ref")
     self.assert_(speciesReference)
  
     formula = libsbml.parseFormula("2 + 2")
     smath   = libsbml.StoichiometryMath(formula)
     speciesReference.setStoichiometryMath(smath)
  
     str(speciesReference)
  
  
  def testEvent(self):
     event = libsbml.Event()
     self.assert_(event)
  
     event = libsbml.Event("eventId0", "trigger")
     self.assert_(event)
  
     # setTrigger()
  
     formula = libsbml.parseFormula("3 + 3")
     trigger = libsbml.Trigger()
     self.assert_(trigger)
     trigger.setMath(formula)
     event.setTrigger(trigger)
  
     # setDelay()
  
     formula = libsbml.parseFormula("4 + 4")
     delay = libsbml.Delay()
     self.assert_(delay)
     delay.setMath(formula)
     event.setDelay(delay)
  
     # addEventAssignment()
  
     eventAssignment = libsbml.EventAssignment()
     self.assert_(eventAssignment)
  
     event.addEventAssignment(eventAssignment)
     self.assert_(event.getNumEventAssignments() == 1)
  
     str(event)
  
  
  def testEventAssignment(self):
     formula1 = libsbml.parseFormula("9 + 9")
     self.assert_(formula1.thisown == 1)
  
     eventAssignment = libsbml.EventAssignment("var", formula1)
     self.assert_(eventAssignment)
  
     formula = libsbml.parseFormula("10 + 10")
     self.assert_(formula.thisown == 1)
  
     eventAssignment.setMath(formula)
  
     str(eventAssignment)
  
  
  def testRules(self):
     formula1 = libsbml.parseFormula("3 + 4")
     formula2 = libsbml.parseFormula("5 + 6")
     formula3 = libsbml.parseFormula("7 + 8")
  
     self.assert_(formula1.thisown == 1)
     self.assert_(formula2.thisown == 1)
     self.assert_(formula3.thisown == 1)
  
     rules = [
        libsbml.AssignmentRule(),
        libsbml.AssignmentRule("var", "formula"),
        libsbml.AlgebraicRule(),
        libsbml.AlgebraicRule("formula"),
        libsbml.AlgebraicRule(formula2),
        libsbml.RateRule(),
        libsbml.RateRule("var", "formula"),
        libsbml.RateRule("var", formula3),
     ]
  
     for rule in rules:
        self.assert_(rule)
  
        formula = libsbml.parseFormula("11 + 12")
        self.assert_(formula.thisown == 1)
  
        rule.setMath(formula)
        self.assert_(formula.thisown == 1)
  
        str(rule)
  
  
  def testListOf(self):
     lo = libsbml.ListOf()
     self.assert_(lo.size() == 0)
     self.assert_(len(lo) == 0)
  
     compartment = libsbml.Compartment()
     self.assert_(compartment)
  
     # append()
  
     lo.append(compartment)
     self.assert_(lo.size() == 1)
  
     theSameCompartment = lo.remove(0)
     self.assert_(lo.size() == 0)
  
     self.assert_(theSameCompartment.toSBML() == compartment.toSBML())
     self.assert_( theSameCompartment != compartment )
  
     # prepend()
  
     compartment2 = libsbml.Compartment("compartment2")
     self.assert_(compartment2)
  
  
  def testModel(self):
     model = libsbml.Model()
     self.assert_(model)
  
     # addFunctionDefinition()
  
     functionDefinition = libsbml.FunctionDefinition()
     self.assert_(functionDefinition)
     functionDefinition.setMath(libsbml.parseFormula("15 + 15"))
  
     model.addFunctionDefinition(functionDefinition)
  
     # addUnitDefinition()
  
     unitDefinition = libsbml.UnitDefinition()
     self.assert_(unitDefinition)
  
     unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1)
     unitDefinition.addUnit(unit);
  
     model.addUnitDefinition(unitDefinition)
  
     # addCompartment()
  
     compartment = libsbml.Compartment()
     self.assert_(compartment)
  
     model.addCompartment(compartment)
  
     # addSpecies()
  
     species = libsbml.Species()
     species.setMetaId('foo')
     self.assert_(species.getMetaId() == 'foo')
  
     model.addSpecies(species)
  
     # addParameter()
  
     parameter = libsbml.Parameter("paramId")
     self.assert_(parameter)
  
     model.addParameter(parameter)
  
     # addReaction()
  
     reaction = libsbml.Reaction("R", "reac", libsbml.KineticLaw("1 + 1"))
     self.assert_(reaction)
  
     model.addReaction(reaction)
  
     # addEvent()
  
     event = libsbml.Event()
     self.assert_(event)
     trigger = libsbml.Trigger()
     self.assert_(trigger)
     trigger.setMath(libsbml.parseFormula("3 + 3"))
     event.setTrigger(trigger)
  
     model.addEvent(event)
  
     str(model)
  
  
  def testKineticLaw(self):
     kineticLaw = libsbml.KineticLaw("1 + 1")
     self.assert_(kineticLaw.getFormula() == "1 + 1")
  
     # setMath()
  
     formula = libsbml.parseFormula("15 + 15")
  
     kineticLaw.setMath(formula)
  
     # addParameter()
  
     parameter = libsbml.Parameter("paramId")
     self.assert_(parameter)
  
     kineticLaw.addParameter(parameter)
  
     str(kineticLaw)
  
     
  def testASTNode(self):
     astNode = libsbml.ASTNode(libsbml.AST_PLUS)
     self.assert_(astNode)
  
     leftNode = libsbml.parseFormula("200")
     self.assert_(leftNode.thisown == 1)
     rightNode = libsbml.parseFormula("2000")
     self.assert_(rightNode.thisown == 1)
  
     astNode.prependChild(leftNode)
     self.assert_(leftNode.thisown == 0)
     astNode.addChild(rightNode)
     self.assert_(rightNode.thisown == 0)
  
     s = libsbml.formulaToString(astNode)
  
     str(astNode)
  
  
  def testSBMLWriter(self):
     doc = libsbml.SBMLDocument()
     model = doc.createModel("modelId")
  
     s = libsbml.writeSBMLToString(doc)
  
     str(doc)
  
  
  def testSBMLReader(self):
     sbmlString = \
  """<?xml version="1.0" encoding="UTF-8"?>
  <sbml xmlns="http://www.sbml.org/sbml/level2" level="2" version="1">
    <model id="modelId"/>
  </sbml>
  """
  
     reader = libsbml.SBMLReader()
  
     doc = reader.readSBMLFromString(sbmlString)
     self.assert_(doc.getNumErrors() == 0)
     self.assert_(doc.getModel())
  
     doc = libsbml.readSBMLFromString(sbmlString)
     self.assert_(doc.getNumErrors() == 0)
     self.assert_(doc.getModel())
  
     str(reader)
     str(doc)
  
     filename = "../../sbml/test/test-data/l2v1-branch.xml"
     doc      = reader.readSBML(filename)
     self.assert_(doc.getNumErrors() == 0)
     self.assert_(doc.getModel())
  
  
  
  def testListOfAsIterator(self):
     model = libsbml.Model()
     model.addReaction(libsbml.Reaction("R1", "reac1", libsbml.KineticLaw("1 + 2")))
     model.addReaction(libsbml.Reaction("R2", "reac2", libsbml.KineticLaw("2 + 3")))
     model.addReaction(libsbml.Reaction("R3", "reac3", libsbml.KineticLaw("3 + 4")))
     model.addReaction(libsbml.Reaction("R4", "reac4", libsbml.KineticLaw("4 + 5")))
  
     listOf = model.getListOfReactions()
     self.assert_(len(listOf) == 4)
  
     reactions = []
     for reaction in listOf:
        reactions.append(reaction)
  
     self.assert_(len(reactions) == 4)
  
     self.assert_(reactions[0].getId() == "R1")
     self.assert_(reactions[1].getId() == "R2")
     self.assert_(reactions[2].getId() == "R3")
     self.assert_(reactions[3].getId() == "R4")
     self.assert_(reactions[0].getName() == "reac1")
     self.assert_(reactions[1].getName() == "reac2")
     self.assert_(reactions[2].getName() == "reac3")
     self.assert_(reactions[3].getName() == "reac4")
     self.assert_(reactions[0].getKineticLaw().getFormula() == "1 + 2")
     self.assert_(reactions[1].getKineticLaw().getFormula() == "2 + 3")
     self.assert_(reactions[2].getKineticLaw().getFormula() == "3 + 4")
     self.assert_(reactions[3].getKineticLaw().getFormula() == "4 + 5")
     self.assert_(listOf[-1].getId() == "R4")
     self.assert_(listOf[-2].getId() == "R3")
     self.assert_(listOf[-3].getId() == "R2")
     self.assert_(listOf[-4].getId() == "R1")
  
     self.assertRaises(IndexError, lambda : listOf[4])
  
     middle2Reactions = listOf[1:3]
     self.assert_(len(middle2Reactions) == 2)
     self.assert_(middle2Reactions[0].getId()   == "R2")
     self.assert_(middle2Reactions[1].getId()   == "R3")
     self.assert_(middle2Reactions[0].getName() == "reac2")
     self.assert_(middle2Reactions[1].getName() == "reac3")
  
     first2Reactions = listOf[:2]
     self.assert_(len(first2Reactions) == 2)
     self.assert_(first2Reactions[0].getId()   == "R1")
     self.assert_(first2Reactions[1].getId()   == "R2")
     self.assert_(first2Reactions[0].getName() == "reac1")
     self.assert_(first2Reactions[1].getName() == "reac2")
  
     last2Reactions = listOf[2:]
     self.assert_(len(last2Reactions) == 2)
     self.assert_(last2Reactions[0].getId()   == "R3")
     self.assert_(last2Reactions[1].getId()   == "R4")
     self.assert_(last2Reactions[0].getName() == "reac3")
     self.assert_(last2Reactions[1].getName() == "reac4")
  
     alsoLast2Reactions = listOf[-2:]
     self.assert_(len(alsoLast2Reactions) == 2)
     self.assert_(alsoLast2Reactions[0].getId()   == "R3")
     self.assert_(alsoLast2Reactions[1].getId()   == "R4")
     self.assert_(alsoLast2Reactions[0].getName() == "reac3")
     self.assert_(alsoLast2Reactions[1].getName() == "reac4")
  
  def testXMLOutputStream(self):
     filename = "../../sbml/test/test-data/l2v1-branch.xml"
     doc      = libsbml.readSBML(filename)
     str1     = libsbml.writeSBMLToString(doc)
  
     oss = libsbml.ostringstream()
     xos = libsbml.XMLOutputStream(oss)
     doc.write(xos)
     libsbml.endl(oss)
     str2 = oss.str()
  
     self.assert_(str1 == str2)
  
  def testClone(self):
     self.checkClone(libsbml.Compartment(),"Compartment")
     self.checkClone(libsbml.CompartmentType(),"CompartmentType")
     self.checkClone(libsbml.Constraint(),"Constraint")
     self.checkClone(libsbml.Delay(),"Delay")
     self.checkClone(libsbml.Event(),"Event")
     self.checkClone(libsbml.EventAssignment(),"EventAssignment")
     self.checkClone(libsbml.FunctionDefinition(),"FunctionDefinition")
     self.checkClone(libsbml.InitialAssignment(),"InitialAssignment")
     self.checkClone(libsbml.KineticLaw(),"KineticLaw")
     self.checkClone(libsbml.ListOf(),"ListOf")
     self.checkClone(libsbml.ListOfCompartmentTypes(),"ListOfCompartmentTypes")
     self.checkClone(libsbml.ListOfCompartments(),"ListOfCompartments")
     self.checkClone(libsbml.ListOfConstraints(),"ListOfConstraints")
     self.checkClone(libsbml.ListOfEventAssignments(),"ListOfEventAssignments")
     self.checkClone(libsbml.ListOfEvents(),"ListOfEvents")
     self.checkClone(libsbml.ListOfFunctionDefinitions(),"ListOfFunctionDefinitions")
     self.checkClone(libsbml.ListOfInitialAssignments(),"ListOfInitialAssignments")
     self.checkClone(libsbml.ListOfParameters(),"ListOfParameters")
     self.checkClone(libsbml.ListOfReactions(),"ListOfReactions")
     self.checkClone(libsbml.ListOfRules(),"ListOfRules")
     self.checkClone(libsbml.ListOfSpecies(),"ListOfSpecies")
     self.checkClone(libsbml.ListOfSpeciesReferences(),"ListOfSpeciesReferences")
     self.checkClone(libsbml.ListOfSpeciesTypes(),"ListOfSpeciesTypes")
     self.checkClone(libsbml.ListOfUnitDefinitions(),"ListOfUnitDefinitions")
     self.checkClone(libsbml.ListOfUnits(),"ListOfUnits")
     self.checkClone(libsbml.ModifierSpeciesReference(),"ModifierSpeciesReference")
     self.checkClone(libsbml.Model(),"Model")
     self.checkClone(libsbml.Parameter(),"Parameter")
     self.checkClone(libsbml.AlgebraicRule(),"AlgebraicRule")
     self.checkClone(libsbml.AssignmentRule(),"AssignmentRule")
     self.checkClone(libsbml.RateRule(),"RateRule")
     self.checkClone(libsbml.Reaction(),"Reaction")
     self.checkClone(libsbml.SBMLDocument(),"SBMLDocument")
     self.checkClone(libsbml.Species(),"Species")
     self.checkClone(libsbml.SpeciesReference(),"SpeciesReference")
     self.checkClone(libsbml.SpeciesType(),"SpeciesType")
     self.checkClone(libsbml.StoichiometryMath(),"StoichiometryMath")
     self.checkClone(libsbml.Trigger(),"Trigger")
     self.checkClone(libsbml.Unit(),"Unit")
     self.checkClone(libsbml.UnitDefinition(),"UnitDefinition")

  def checkClone(self,obj,name):
    clone = obj.clone()
    self.assert_(clone.__class__.__name__ == name)
    self.assert_(clone.thisown == 1)

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestPython))

  return suite
  
if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
  
