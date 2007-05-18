##
## Filename    : accept.py
## Description : Acceptance tests for libSBML Python module
## Author(s)   : SBML Team <sbml-team@caltech.edu>
## Created     : 2004-04-02
## Revision    : $Id$
## Source      : $Source$
##
## Copyright 2004 California Institute of Technology and
## Japan Science and Technology Corporation.
##
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published
## by the Free Software Foundation; either version 2.1 of the License, or
## any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
## MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
## documentation provided hereunder is on an "as is" basis, and the
## California Institute of Technology and Japan Science and Technology
## Corporation have no obligations to provide maintenance, support,
## updates, enhancements or modifications.  In no event shall the
## California Institute of Technology or the Japan Science and Technology
## Corporation be liable to any party for direct, indirect, special,
## incidental or consequential damages, including lost profits, arising
## out of the use of this software and its documentation, even if the
## California Institute of Technology and/or Japan Science and Technology
## Corporation have been advised of the possibility of such damage.  See
## the GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this library; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
##
## The original code contained here was initially developed by:
##
##     Ben Bornstein and Ben Kovitz
##
##     The SBML Team
##     Control and Dynamical Systems, MC 107-81
##     California Institute of Technology
##     Pasadena, CA, 91125, USA
##
##     http://sbml.org
##     mailto:sbml-team@caltech.edu
##
## Contributor(s):
##


import sys
import operator
import re
import string

import libsbml


def isNaN(x):
   return str(x).lower() == "nan"


def stackTraceLineNum():
   return sys.exc_info()[2].tb_next.tb_lineno


def testEquality():
   lo = libsbml.ListOf()
   c1 = libsbml.Compartment()

   lo.append(c1)
   c2 = lo.get(0)

   #FIXME
   #assert c1 == c2

   c2 = 7
   assert not c1 == c2


def testImplicitDowncastOfRule():
   m = libsbml.Model()
   assert m

   r = libsbml.AssignmentRule("x", "1 + 1")
   assert r

   m.addRule(r)
   assert m.getNumRules() == 1

   r = m.getRule(0)
   assert r.getVariable() == "x"
   assert r.getFormula()  == "1 + 1"

   assert "AssignmentRule" in r.__class__.__name__


def testImplicitDowncastOfSBase():
   species = libsbml.Species()
   assert species

   species.setMetaId('foo')
   assert species.getMetaId() == 'foo'

   lst = libsbml.ListOf()
   assert len(lst) == 0
   assert lst.size() == 0

   lst.append(species)
   assert lst.size() == 1

   theSameSpecies = lst.get(0)
   assert theSameSpecies.getMetaId() == "foo"

   #noElement = lst.get(51)
   #FIXME
   #assert noElement is None

   #alsoTheSameSpecies = lst[0]
   #assert alsoTheSameSpecies.getMetaId() == "foo"


def testSpecies():
   species = libsbml.Species()
   species.setMetaId('foo')
   assert species.getMetaId() == "foo"

   assert species.getInitialConcentration() == 0.0

   species.unsetInitialConcentration()
   #print species.getInitialConcentration()

   str(species)


def testReaction():
   reaction = libsbml.Reaction("R", libsbml.KineticLaw("1 + 1"))
   assert reaction

   speciesReference = libsbml.SpeciesReference("SR")
   assert speciesReference

   # Reactant

   reaction.addReactant(speciesReference)
   assert reaction.getNumReactants() == 1

   theSameSpeciesReference = reaction.getReactant(0)
   assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()
   
   theSameSpeciesReference = reaction.getListOfReactants()[0]
   #FIXME
   #assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()

   # KineticLaw

   kineticLaw = libsbml.KineticLaw("1 + 1")
   assert kineticLaw
   assert kineticLaw.getFormula() == "1 + 1"

   reaction = libsbml.Reaction("R", "R", None, False)
   reaction.setKineticLaw(kineticLaw)
   #FIXME
   #assert kineticLaw == reaction.getKineticLaw()

   # Product

   productReference = libsbml.SpeciesReference("product")
   assert productReference

   reaction.addProduct(productReference)
   assert reaction.getNumProducts() == 1

   theSameSpeciesReference = reaction.getProduct(0)
   assert theSameSpeciesReference.getSpecies() == productReference.getSpecies()

   # Modifier

   modifierReference = libsbml.ModifierSpeciesReference("modifier")
   assert modifierReference

   reaction.addModifier(modifierReference)
   assert reaction.getNumModifiers() == 1

   theSameSpeciesReference = reaction.getModifier(0)
   assert theSameSpeciesReference.getSpecies() == modifierReference.getSpecies()

   str(reaction)


def testDocument():
   d = libsbml.SBMLDocument()
   assert d
   
   m = libsbml.Model()
   assert m

   d.setModel(m)
   #FIXME
   #assert m == d.getModel()

   str(d)
   str(d.getModel())


def testFunctionDefinition():
   fd = libsbml.FunctionDefinition()
   assert fd

   fd = libsbml.FunctionDefinition("id1", "formula0")
   assert fd

   formula1 = libsbml.parseFormula("2 + 8")
   assert formula1.thisown == 1

   fd = libsbml.FunctionDefinition("id", formula1)
   assert fd and fd.thisown == 1

   formula = libsbml.parseFormula("1 + 1")
   fd.setMath(formula)

   str(fd)


def testUnitDefinition():
   ud = libsbml.UnitDefinition()
   assert ud

   unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1)
   assert unit.thisown
   ud.addUnit(unit);

   str(ud)


def testSpeciesReference():
   speciesReference = libsbml.SpeciesReference("ref")
   assert speciesReference

   formula = libsbml.parseFormula("2 + 2")
   smath   = libsbml.StoichiometryMath(formula)
   speciesReference.setStoichiometryMath(smath)

   str(speciesReference)


def testEvent():
   event = libsbml.Event()
   assert event

   event = libsbml.Event("eventId0", "trigger")
   assert event

   # setTrigger()

   formula = libsbml.parseFormula("3 + 3")
   trigger = libsbml.Trigger()
   assert trigger
   trigger.setMath(formula)
   event.setTrigger(trigger)

   # setDelay()

   formula = libsbml.parseFormula("4 + 4")
   delay = libsbml.Delay()
   assert delay
   delay.setMath(formula)
   event.setDelay(delay)

   # addEventAssignment()

   eventAssignment = libsbml.EventAssignment()
   assert eventAssignment

   event.addEventAssignment(eventAssignment)
   assert event.getNumEventAssignments() == 1

   str(event)


def testEventAssignment():
   formula1 = libsbml.parseFormula("9 + 9")
   assert formula1.thisown == 1

   eventAssignment = libsbml.EventAssignment("var", formula1)
   assert eventAssignment

   formula = libsbml.parseFormula("10 + 10")
   assert formula.thisown == 1

   eventAssignment.setMath(formula)

   str(eventAssignment)


def testRules():
   formula1 = libsbml.parseFormula("3 + 4")
   formula2 = libsbml.parseFormula("5 + 6")
   formula3 = libsbml.parseFormula("7 + 8")

   assert formula1.thisown == 1
   assert formula2.thisown == 1
   assert formula3.thisown == 1

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
      assert rule

      formula = libsbml.parseFormula("11 + 12")
      assert formula.thisown == 1

      rule.setMath(formula)
      assert formula.thisown == 1

      str(rule)


def testListOf():
   lo = libsbml.ListOf()
   assert lo.size() == 0
   assert len(lo) == 0

   compartment = libsbml.Compartment()
   assert compartment

   # append()

   lo.append(compartment)
   assert lo.size() == 1

   theSameCompartment = lo.remove(0)
   assert lo.size() == 0

   #FIXME
   #assert theSameCompartment == compartment

   # prepend()

   compartment2 = libsbml.Compartment("compartment2")
   assert compartment2

   str(lo)


def testModel():
   model = libsbml.Model()
   assert model

   # addFunctionDefinition()

   functionDefinition = libsbml.FunctionDefinition()
   assert functionDefinition
   functionDefinition.setMath(libsbml.parseFormula("15 + 15"))

   model.addFunctionDefinition(functionDefinition)

   # addUnitDefinition()

   unitDefinition = libsbml.UnitDefinition()
   assert unitDefinition

   unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1)
   unitDefinition.addUnit(unit);

   model.addUnitDefinition(unitDefinition)

   # addCompartment()

   compartment = libsbml.Compartment()
   assert compartment

   model.addCompartment(compartment)

   # addSpecies()

   species = libsbml.Species()
   species.setMetaId('foo')
   assert species.getMetaId() == 'foo'

   model.addSpecies(species)

   # addParameter()

   parameter = libsbml.Parameter("paramId")
   assert parameter

   model.addParameter(parameter)

   # addReaction()

   reaction = libsbml.Reaction("R", libsbml.KineticLaw("1 + 1"))
   assert reaction

   model.addReaction(reaction)

   # addEvent()

   event = libsbml.Event()
   assert event
   trigger = libsbml.Trigger()
   assert trigger
   trigger.setMath(libsbml.parseFormula("3 + 3"))
   event.setTrigger(trigger)

   model.addEvent(event)

   str(model)


def testKineticLaw():
   kineticLaw = libsbml.KineticLaw("1 + 1")
   assert kineticLaw.getFormula() == "1 + 1"

   # setMath()

   formula = libsbml.parseFormula("15 + 15")

   kineticLaw.setMath(formula)

   # addParameter()

   parameter = libsbml.Parameter("paramId")
   assert parameter

   kineticLaw.addParameter(parameter)

   str(kineticLaw)

   
def testASTNode():
   astNode = libsbml.ASTNode(libsbml.AST_PLUS)
   assert astNode

   leftNode = libsbml.parseFormula("200")
   assert leftNode.thisown == 1
   rightNode = libsbml.parseFormula("2000")
   assert rightNode.thisown == 1

   astNode.prependChild(leftNode)
   assert leftNode.thisown == 0
   astNode.addChild(rightNode)
   assert rightNode.thisown == 0

   s = libsbml.formulaToString(astNode)

   str(astNode)


def testSBMLWriter():
   doc = libsbml.SBMLDocument()
   model = doc.createModel("modelId")

   s = libsbml.writeSBMLToString(doc)

   str(doc)


def testSBMLReader():
   sbmlString = \
"""<?xml version="1.0" encoding="UTF-8"?>
<sbml xmlns="http://www.sbml.org/sbml/level2" level="2" version="1">
  <model id="modelId"/>
</sbml>
"""

   reader = libsbml.SBMLReader()

   doc = reader.readSBMLFromString(sbmlString)
   assert doc.getNumErrors() == 0
   assert doc.getModel()

   doc = libsbml.readSBMLFromString(sbmlString)
   assert doc.getNumErrors() == 0
   assert doc.getModel()

   str(reader)
   str(doc)

   filename = "../../sbml/test/test-data/l2v1-branch.xml"
   doc      = reader.readSBML(filename)
   assert doc.getNumErrors() == 0
   assert doc.getModel()



def testListOfAsIterator():
   model = libsbml.Model()
   model.addReaction(libsbml.Reaction("R1", libsbml.KineticLaw("1 + 2")))
   model.addReaction(libsbml.Reaction("R2", libsbml.KineticLaw("2 + 3")))
   model.addReaction(libsbml.Reaction("R3", libsbml.KineticLaw("3 + 4")))
   model.addReaction(libsbml.Reaction("R4", libsbml.KineticLaw("4 + 5")))

   listOf = model.getListOfReactions()
   assert len(listOf) == 4

   reactions = []
   for reaction in listOf:
      reactions.append(reactions)

   assert len(reactions) == 4

   assert listOf[-1].getId() == "R4"

   try:
      print listOf[4]
      assert False, "failed to raise IndexError"
   except IndexError:
      pass

   middle2Reactions = listOf[1:3]
   assert len(middle2Reactions) == 2
   assert middle2Reactions[0].getId() == "R2"
   assert middle2Reactions[1].getId() == "R3"

   first2Reactions = listOf[:2]
   assert len(first2Reactions) == 2
   assert first2Reactions[0].getId() == "R1"
   assert first2Reactions[1].getId() == "R2"

   last2Reactions = listOf[2:]
   assert len(last2Reactions) == 2
   assert last2Reactions[0].getId() == "R3"
   assert last2Reactions[1].getId() == "R4"

   alsoLast2Reactions = listOf[-2:]
   assert len(alsoLast2Reactions) == 2
   assert alsoLast2Reactions[0].getId() == "R3"
   assert alsoLast2Reactions[1].getId() == "R4"

   item  = "\<libsbml\.ReactionPtr\; proxy of C\+\+ Reaction instance at "
   item += "_[0-9a-f]+_p_Reaction\>";
   items = string.join([item] * 4, ", ")

   #FIXME
   #expect = re.compile("\[" + items + "\]")
   #assert expect.match(repr(listOf))

def testXMLOutputStream():
   filename = "../../sbml/test/test-data/l2v1-branch.xml"
   doc      = libsbml.readSBML(filename)
   str1     = libsbml.writeSBMLToString(doc)

   oss = libsbml.ostringstream()
   xos = libsbml.XMLOutputStream(oss)
   doc.write(xos)
   libsbml.endl(oss)
   str2 = oss.str()

   assert str1 == str2

def testClone():
   checkClone(libsbml.Compartment(),"Compartment")
   checkClone(libsbml.CompartmentType(),"CompartmentType")
   checkClone(libsbml.Constraint(),"Constraint")
   checkClone(libsbml.Delay(),"Delay")
   checkClone(libsbml.Event(),"Event")
   checkClone(libsbml.EventAssignment(),"EventAssignment")
   checkClone(libsbml.FormulaUnitsData(),"FormulaUnitsData")
   checkClone(libsbml.FunctionDefinition(),"FunctionDefinition")
   checkClone(libsbml.InitialAssignment(),"InitialAssignment")
   checkClone(libsbml.KineticLaw(),"KineticLaw")
   checkClone(libsbml.ListFormulaUnitsData(),"ListFormulaUnitsData")
   checkClone(libsbml.ListOf(),"ListOf")
   checkClone(libsbml.ListOfCompartmentTypes(),"ListOfCompartmentTypes")
   checkClone(libsbml.ListOfCompartments(),"ListOfCompartments")
   checkClone(libsbml.ListOfConstraints(),"ListOfConstraints")
   checkClone(libsbml.ListOfEventAssignments(),"ListOfEventAssignments")
   checkClone(libsbml.ListOfEvents(),"ListOfEvents")
   checkClone(libsbml.ListOfFunctionDefinitions(),"ListOfFunctionDefinitions")
   checkClone(libsbml.ListOfInitialAssignments(),"ListOfInitialAssignments")
   checkClone(libsbml.ListOfParameters(),"ListOfParameters")
   checkClone(libsbml.ListOfReactions(),"ListOfReactions")
   checkClone(libsbml.ListOfRules(),"ListOfRules")
   checkClone(libsbml.ListOfSpecies(),"ListOfSpecies")
   checkClone(libsbml.ListOfSpeciesReferences(),"ListOfSpeciesReferences")
   checkClone(libsbml.ListOfSpeciesTypes(),"ListOfSpeciesTypes")
   checkClone(libsbml.ListOfUnitDefinitions(),"ListOfUnitDefinitions")
   checkClone(libsbml.ListOfUnits(),"ListOfUnits")
   checkClone(libsbml.ModifierSpeciesReference(),"ModifierSpeciesReference")
   checkClone(libsbml.Model(),"Model")
   checkClone(libsbml.Parameter(),"Parameter")
   checkClone(libsbml.AlgebraicRule(),"AlgebraicRule")
   checkClone(libsbml.AssignmentRule(),"AssignmentRule")
   checkClone(libsbml.RateRule(),"RateRule")
   checkClone(libsbml.Reaction(),"Reaction")
   checkClone(libsbml.SBMLDocument(),"SBMLDocument")
   checkClone(libsbml.Species(),"Species")
   checkClone(libsbml.SpeciesReference(),"SpeciesReference")
   checkClone(libsbml.SpeciesType(),"SpeciesType")
   checkClone(libsbml.StoichiometryMath(),"StoichiometryMath")
   checkClone(libsbml.Trigger(),"Trigger")
   checkClone(libsbml.Unit(),"Unit")
   checkClone(libsbml.UnitDefinition(),"UnitDefinition")

def checkClone(obj,name):
   clone = obj.clone()
   assert clone.__class__.__name__ == name
   assert clone.thisown == 1

class TestRunner:

   def __init__(self):
      self.numTestsRun = 0
      self.numTestsFailed = 0

      for testFunc in [
         globals()[name] for name in globals() if name.startswith("test")
      ]:
         self.numTestsRun += 1
         print testFunc.__name__ #DEBUG
         try:
               testFunc()
         except AssertionError, e:
            print testFunc.__name__, "failed at line", stackTraceLineNum()
            self.numTestsFailed += 1


   def printSummary(self):
      if self.numTestsFailed > 0:
         print

      print "%s run" % self.testsPlural(self.numTestsRun)
      if self.numTestsFailed:
         print "%s FAILED" % self.testsPlural(self.numTestsFailed)
      else:
         print "All tests PASSED"


   def testsPlural(self, n):
      if n == 1:
         return "1 test"
      else:
         return "%d tests" % n


   def shellResultCode(self):
      return self.numTestsRun == 0 or self.numTestsFailed > 0


runner = TestRunner()
runner.printSummary()
sys.exit(runner.shellResultCode())
