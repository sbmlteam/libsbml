import sys
import libsbml

def isNaN(x):
   return str(x) == "nan"


def stackTraceLineNum():
   return sys.exc_info()[2].tb_next.tb_lineno


def testList():
   species = libsbml.Species()
   species.setMetaId('foo')

   lst = libsbml.ListOf()
   assert lst
   assert lst.getNumItems() == 0

   lst.append(species)
   assert lst.getNumItems() == 1

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

   speciesReference = libsbml.SpeciesReference("SR")
   assert speciesReference

   reaction.addReactant(speciesReference)
   assert reaction.getNumReactants() == 1

   theSameSpeciesReference = reaction.getReactant(0)
   assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()
   
   theSameSpeciesReference = reaction.getListOfReactants()[0]
   assert theSameSpeciesReference.getSpecies() == speciesReference.getSpecies()

   """
   kineticLaw = libsbml.KineticLaw("1 + 1")
   assert kineticLaw
   print kineticLaw

   assert kineticLaw.getFormula() == "1 + 1"

   reaction = libsbml.Reaction("R", None, False)
   reaction.setKineticLaw(kineticLaw)

   """
   #assert kineticLaw == reaction.getKineticLaw()


def testDocument():
   d = libsbml.SBMLDocument()
   m = libsbml.Model()
   d.setModel(m)


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
