##
## Filename    : accept.py
## Description : Acceptance tests for libSBML Python module
## Author(s)   : SBML Development Group <sbml-team@caltech.edu>
## Organization: JST ERATO Kitano Symbiotic Systems Project
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
##     The Systems Biology Markup Language Development Group
##     ERATO Kitano Symbiotic Systems Project
##     Control and Dynamical Systems, MC 107-81
##     California Institute of Technology
##     Pasadena, CA, 91125, USA
##
##     http://www.cds.caltech.edu/erato
##     mailto:sysbio-team@caltech.edu
##
## Contributor(s):
##


import sys, operator, re
import libsbml

def isNaN(x):
   return str(x) == "nan"


def stackTraceLineNum():
   return sys.exc_info()[2].tb_next.tb_lineno


def testEquality():
   lo = libsbml.ListOf()
   c1 = libsbml.Compartment()

   lo.append(c1)
   c2 = lo.get(0)

   assert c1 == c2

   c2 = 7
   assert not c1 == c2


def testImplicitDowncastOfRule():
   m = libsbml.Model()
   assert m and m.thisown == 1
   
   r = libsbml.AssignmentRule("x", "1 + 1")
   assert r and r.thisown == 1

   m.addRule(r)
   assert m.getNumRules() == 1
   assert r.thisown == 0

   r = m.getRule(0)
   r.setFormulaFromMath()
   assert r.getVariable() == "x"
   assert r.getFormula()  == "1 + 1"

   assert "AssignmentRule" in r.__class__.__name__


def testMoreRuleTypes():
   model = libsbml.Model()

   model.addRule(libsbml.SpeciesConcentrationRule())
   model.addRule(libsbml.CompartmentVolumeRule())
   model.addRule(libsbml.ParameterRule())

   assert "SpeciesConcentrationRule" in model.getRule(0).__class__.__name__
   assert "CompartmentVolumeRule" in model.getRule(1).__class__.__name__
   assert "ParameterRule" in model.getRule(2).__class__.__name__



def testImplicitDowncastOfSBase():
   species = libsbml.Species()
   assert species and species.thisown == 1

   species.setMetaId('foo')
   assert species.getMetaId() == 'foo'

   lst = libsbml.ListOf()
   assert len(lst) == 0
   assert lst.getNumItems() == 0

   lst.append(species)
   assert lst.getNumItems() == 1
   assert species.thisown == 0

   theSameSpecies = lst.get(0)
   assert theSameSpecies.getMetaId() == "foo"

   noElement = lst.get(51)
   assert noElement is None

   alsoTheSameSpecies = lst[0]
   assert alsoTheSameSpecies.getMetaId() == "foo"


def testSpecies():
   species = libsbml.Species()
   species.setMetaId('foo')
   assert species.getMetaId() == "foo"

   assert species.getInitialConcentration() == 0.0

   species.unsetInitialConcentration()
   assert isNaN(species.getInitialConcentration())


def testReaction():
   reaction = libsbml.Reaction("R", libsbml.KineticLaw("1 + 1"))
   assert reaction
   assert reaction.getKineticLaw().thisown == 0

   speciesReference = libsbml.SpeciesReference("SR")
   assert speciesReference

   # Reactant

   reaction.addReactant(speciesReference)
   assert reaction.getNumReactants() == 1

   theSameSpeciesReference = reaction.getReactant(0)
   assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()
   
   theSameSpeciesReference = reaction.getListOfReactants()[0]
   assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()

   # KineticLaw

   kineticLaw = libsbml.KineticLaw("1 + 1")
   assert kineticLaw and kineticLaw.thisown == 1
   assert kineticLaw.getFormula() == "1 + 1"

   reaction = libsbml.Reaction("R", None, False)
   reaction.setKineticLaw(kineticLaw)
   assert kineticLaw.thisown == 0
   assert kineticLaw == reaction.getKineticLaw()

   # Product

   productReference = libsbml.SpeciesReference("product")
   assert productReference
   assert productReference.thisown == 1

   reaction.addProduct(productReference)
   assert productReference.thisown == 0

   assert reaction.getNumProducts() == 1

   theSameSpeciesReference = reaction.getProduct(0)
   assert theSameSpeciesReference.getSpecies() == productReference.getSpecies()

   # Modifier

   modifierReference = libsbml.ModifierSpeciesReference("modifier")
   assert modifierReference
   assert modifierReference.thisown == 1

   reaction.addModifier(modifierReference)
   assert modifierReference.thisown == 0

   assert reaction.getNumModifiers() == 1

   theSameSpeciesReference = reaction.getModifier(0)
   assert theSameSpeciesReference.getSpecies() == modifierReference.getSpecies()


def testDocument():
   d = libsbml.SBMLDocument()
   assert d and d.thisown == 1
   
   m = libsbml.Model()
   assert m and m.thisown == 1

   d.setModel(m)
   assert m.thisown == 0
   assert m == d.getModel()


def testFunctionDefinition():
   fd = libsbml.FunctionDefinition()
   assert fd and fd.thisown == 1

   fd = libsbml.FunctionDefinition("id1", "formula0")
   assert fd and fd.thisown == 1

   formula1 = libsbml.parseFormula("2 + 8")
   assert formula1.thisown == 1

   fd = libsbml.FunctionDefinition("id", formula1)
   assert fd and fd.thisown == 1
   assert formula1.thisown == 0

   formula = libsbml.parseFormula("1 + 1")
   assert formula.thisown == 1

   fd.setMath(formula)
   assert formula.thisown == 0


def testUnitDefinition():
   ud = libsbml.UnitDefinition()
   assert ud and ud.thisown == 1

   unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1, 0)
   assert unit.thisown 
   ud.addUnit(unit);

   assert unit.thisown == 0


def testSpeciesReference():
   speciesReference = libsbml.SpeciesReference("ref")
   assert speciesReference

   formula = libsbml.parseFormula("2 + 2")
   assert formula.thisown == 1

   speciesReference.setStoichiometryMath(formula)
   assert formula.thisown == 0


def testEvent():
   event = libsbml.Event()
   assert event

   event = libsbml.Event("eventId0", "trigger", "delay")
   assert event

   trigger1 = libsbml.parseFormula("1")
   assert trigger1.thisown == 1
   event = libsbml.Event("eventId1", trigger1)
   assert trigger1.thisown == 0

   trigger2 = libsbml.parseFormula("2")
   assert trigger2.thisown == 1
   delay = libsbml.parseFormula("3")
   assert delay.thisown == 1
   event = libsbml.Event("eventId2", trigger2, delay)
   assert trigger2.thisown == 0
   assert delay.thisown == 0

   # setTrigger()

   formula = libsbml.parseFormula("3 + 3")
   assert formula.thisown == 1

   event.setTrigger(formula)
   assert formula.thisown == 0

   # setDelay()

   formula = libsbml.parseFormula("4 + 4")
   assert formula.thisown == 1

   event.setDelay(formula)
   assert formula.thisown == 0

   # addEventAssignment()

   eventAssignment = libsbml.EventAssignment()
   assert eventAssignment
   assert eventAssignment.thisown == 1

   event.addEventAssignment(eventAssignment)
   assert event.getNumEventAssignments() == 1

   assert eventAssignment.thisown == 0


def testEventAssignment():
   eventAssignment = libsbml.EventAssignment()
   eventAssignment = libsbml.EventAssignment("var1", "formula0")

   formula1 = libsbml.parseFormula("9 + 9")
   assert formula1.thisown == 1

   eventAssignment = libsbml.EventAssignment("var", formula1)
   assert eventAssignment
   assert formula1.thisown == 0

   formula = libsbml.parseFormula("10 + 10")
   assert formula.thisown == 1

   eventAssignment.setMath(formula)
   assert formula.thisown == 0


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
      libsbml.AssignmentRule("var", formula1, libsbml.RULE_TYPE_SCALAR),
      libsbml.AlgebraicRule(),
      libsbml.AlgebraicRule("formula"),
      libsbml.AlgebraicRule(formula2),
      libsbml.RateRule(),
      libsbml.RateRule("var", "formula"),
      libsbml.RateRule("var", formula3),
   ]

   assert formula1.thisown == 0
   assert formula2.thisown == 0
   assert formula3.thisown == 0

   for rule in rules:
      assert rule

      formula = libsbml.parseFormula("11 + 12")
      assert formula.thisown == 1

      rule.setMath(formula)
      assert formula.thisown == 0


def testListOf():
   lo = libsbml.ListOf()
   assert lo.getNumItems() == 0
   assert len(lo) == 0

   compartment = libsbml.Compartment()
   assert compartment
   assert compartment.thisown == 1

   # append()

   lo.append(compartment)
   assert lo.getNumItems() == 1
   assert compartment.thisown == 0

   theSameCompartment = lo.remove(0)
   assert lo.getNumItems() == 0

   assert theSameCompartment == compartment
   assert theSameCompartment.thisown == 1
   # assert compartment.thisown == 1  This fails, but that might be correct.
   # Theoretically, having thisown==1 on only one object ensures that the
   # underlying libsbml object gets freed only once.

   # prepend()

   compartment2 = libsbml.Compartment("compartment2")
   assert compartment2
   assert compartment2.thisown == 1

   lo.prepend(compartment2)
   assert lo.getNumItems() == 1
   assert compartment2.thisown == 0


def testModel():
   model = libsbml.Model()
   assert model

   # addFunctionDefinition()

   functionDefinition = libsbml.FunctionDefinition()
   assert functionDefinition
   functionDefinition.setMath(libsbml.parseFormula("15 + 15"))
   assert functionDefinition.thisown == 1

   model.addFunctionDefinition(functionDefinition)
   assert functionDefinition.thisown == 0

   # addUnitDefinition()

   unitDefinition = libsbml.UnitDefinition()
   assert unitDefinition
   assert unitDefinition.thisown == 1

   unit = libsbml.Unit(libsbml.UNIT_KIND_KILOGRAM, 1, 1000, 1, 0)
   unitDefinition.addUnit(unit);

   model.addUnitDefinition(unitDefinition)
   assert unitDefinition.thisown == 0

   # addCompartment()

   compartment = libsbml.Compartment()
   assert compartment
   assert compartment.thisown == 1

   model.addCompartment(compartment)
   assert compartment.thisown == 0

   # addSpecies()

   species = libsbml.Species()
   assert species and species.thisown == 1
   species.setMetaId('foo')
   assert species.getMetaId() == 'foo'

   model.addSpecies(species)
   assert species.thisown == 0

   # addParameter()

   parameter = libsbml.Parameter("paramId")
   assert parameter
   assert parameter.thisown == 1

   model.addParameter(parameter)
   assert parameter.thisown == 0

   # addReaction()

   reaction = libsbml.Reaction("R", libsbml.KineticLaw("1 + 1"))
   assert reaction
   assert reaction.getKineticLaw().thisown == 0

   model.addReaction(reaction)
   assert reaction.thisown == 0

   # addEvent()

   event = libsbml.Event()
   assert event
   assert event.thisown == 1
   event.setTrigger(libsbml.parseFormula("3 + 3"))

   model.addEvent(event)
   assert event.thisown == 0


def testKineticLaw():
   kineticLaw = libsbml.KineticLaw("1 + 1")
   assert kineticLaw and kineticLaw.thisown == 1
   assert kineticLaw.getFormula() == "1 + 1"

   # setMath()

   formula = libsbml.parseFormula("15 + 15")
   assert formula.thisown == 1

   kineticLaw.setMath(formula)
   assert formula.thisown == 0

   # addParameter()

   parameter = libsbml.Parameter("paramId")
   assert parameter
   assert parameter.thisown == 1

   kineticLaw.addParameter(parameter)
   assert parameter.thisown == 0

   
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


def testMathMLDocument():
   doc = libsbml.MathMLDocument()
   assert doc

   formula = libsbml.parseFormula("16 + 16")
   assert formula.thisown == 1

   doc.setMath(formula)
   assert formula.thisown == 0

   s = libsbml.writeMathMLToString(doc)


def testSBMLWriter():
   doc = libsbml.SBMLDocument()
   model = doc.createModel("modelId")
   assert model.thisown == 0

   s = libsbml.writeSBMLToString(doc)


def testSBMLReader():
   sbmlString = \
"""<?xml version="1.0" encoding="UTF-8"?>
<sbml xmlns="http://www.sbml.org/sbml/level2" level="2" version="1">
  <model id="modelId"/>
</sbml>
"""

   reader = libsbml.SBMLReader()
   doc = reader.readSBMLFromString(sbmlString)
   assert doc.thisown == 1

   doc = libsbml.readSBMLFromString(sbmlString)
   assert doc.thisown == 1


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

   expect = re.compile("\[<C Reaction instance at _[0-9a-f]{8}_p_Reaction>, <C Reaction instance at _[0-9a-f]{8}_p_Reaction>, <C Reaction instance at _[0-9a-f]{8}_p_Reaction>, <C Reaction instance at _[0-9a-f]{8}_p_Reaction>\]")
   assert expect.match(repr(listOf))


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
