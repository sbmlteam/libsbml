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
