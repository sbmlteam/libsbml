##
## Filename    : Makefile.am
## Description : Process this file with automake (1.7.2) to produce Makefile.in
## Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
## Organization: JST ERATO Kitano Symbiotic Systems Project
## Created     : 2003-02-13
## Revision    : $Id$
## Source      : $Source$
##
## Copyright 2002 California Institute of Technology and
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
##     Ben Bornstein
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


lib_LTLIBRARIES    = libsbml.la
libsbml_la_LDFLAGS = -no-undefined -version-info 2:1:0

nobase_include_HEADERS =          \
  sbml/AlgebraicRule.h            \
  sbml/AssignmentRule.h           \
  sbml/ASTNode.h                  \
  sbml/Compartment.h              \
  sbml/CompartmentVolumeRule.h    \
  sbml/Event.h                    \
  sbml/EventAssignment.h          \
  sbml/FormulaFormatter.h         \
  sbml/FormulaParser.h            \
  sbml/FormulaTokenizer.h         \
  sbml/FunctionDefinition.h       \
  sbml/KineticLaw.h               \
  sbml/List.h                     \
  sbml/ListOf.h                   \
  sbml/MathMLDocument.h           \
  sbml/MathMLReader.h             \
  sbml/MathMLWriter.h             \
  sbml/Model.h                    \
  sbml/ModifierSpeciesReference.h \
  sbml/Parameter.h                \
  sbml/ParameterRule.h            \
  sbml/ParseMessage.h             \
  sbml/RateRule.h                 \
  sbml/Reaction.h                 \
  sbml/Rule.h                     \
  sbml/RuleType.h                 \
  sbml/SBase.h                    \
  sbml/SBMLConvert.h              \
  sbml/SBMLDocument.h             \
  sbml/SBMLReader.h               \
  sbml/SBMLTypeCodes.h            \
  sbml/SBMLTypes.h                \
  sbml/SBMLWriter.h               \
  sbml/SimpleSpeciesReference.h   \
  sbml/Species.h                  \
  sbml/SpeciesConcentrationRule.h \
  sbml/SpeciesReference.h         \
  sbml/Stack.h                    \
  sbml/StringBuffer.h             \
  sbml/UnitDefinition.h           \
  sbml/Unit.h                     \
  sbml/UnitKind.h                 \
  sbml/common.h                   \
  sbml/config.h                   \
  sbml/extern.h                   \
  sbml/memory.h                   \
  sbml/util.h

libsbml_la_SOURCES =              \
  sbml/common.hpp                 \
  sbml/MathMLFormatter.hpp        \
  sbml/MathMLHandler.hpp          \
  sbml/MathMLTagCodes.hpp         \
  sbml/MathMLUnicodeConstants.hpp \
  sbml/SBMLFormatter.hpp          \
  sbml/SBMLHandler.hpp            \
  sbml/SBMLTagCodes.hpp           \
  sbml/SBMLUnicodeConstants.hpp   \
  sbml/XMLStringFormatter.hpp     \
  sbml/XMLUnicodeConstants.hpp    \
  sbml/XMLUtil.hpp                \
  MathMLDocument.c                \
  MathMLFormatter.cpp             \
  MathMLHandler.cpp               \
  MathMLReader.cpp                \
  MathMLTagCodes.cpp              \
  MathMLWriter.cpp                \
  SBMLFormatter.cpp               \
  SBMLHandler.cpp                 \
  SBMLReader.cpp                  \
  SBMLTagCodes.cpp                \
  SBMLWriter.cpp                  \
  XMLStringFormatter.cpp          \
  XMLUtil.cpp                     \
  AlgebraicRule.c                 \
  AssignmentRule.c                \
  ASTNode.c                       \
  Compartment.c                   \
  CompartmentVolumeRule.c         \
  Event.c                         \
  EventAssignment.c               \
  FormulaFormatter.c              \
  FormulaParser.c                 \
  FormulaTokenizer.c              \
  FunctionDefinition.c            \
  KineticLaw.c                    \
  List.c                          \
  ListOf.c                        \
  Model.c                         \
  ModifierSpeciesReference.c      \
  Parameter.c                     \
  ParameterRule.c                 \
  ParseMessage.c                  \
  RateRule.c                      \
  Reaction.c                      \
  Rule.c                          \
  RuleType.c                      \
  SBase.c                         \
  SBMLConvert.c                   \
  SBMLDocument.c                  \
  SimpleSpeciesReference.c        \
  Species.c                       \
  SpeciesConcentrationRule.c      \
  SpeciesReference.c              \
  Stack.c                         \
  StringBuffer.c                  \
  Unit.c                          \
  UnitDefinition.c                \
  UnitKind.c                      \
  memory.c                        \
  util.c


if LIBCHECK_ENABLED

TestRunner_SOURCES =              \
  test-data                       \
  sbml/SAX2AttributesMock.hpp     \
  SAX2AttributesMock.cpp          \
  TestMathMLFormatter.cpp         \
  TestMathMLHandler.cpp           \
  TestSAX2AttributesMock.cpp      \
  TestSBMLFormatter.cpp           \
  TestSBMLHandler.cpp             \
  TestXMLStringFormatter.cpp      \
  TestXMLUtil.cpp                 \
  TestAlgebraicRule.c             \
  TestAssignmentRule.c            \
  TestASTNode.c                   \
  TestCompartment.c               \
  TestCompartmentVolumeRule.c     \
  TestEvent.c                     \
  TestEventAssignment.c           \
  TestFormulaFormatter.c          \
  TestFormulaParser.c             \
  TestFormulaTokenizer.c          \
  TestFunctionDefinition.c        \
  TestKineticLaw.c                \
  TestList.c                      \
  TestListOf.c                    \
  TestMathMLDocument.c            \
  TestMathMLWriter.c              \
  TestMemory.c                    \
  TestModel.c                     \
  TestModifierSpeciesReference.c  \
  TestParameter.c                 \
  TestParameterRule.c             \
  TestParseMessage.c              \
  TestRateRule.c                  \
  TestReaction.c                  \
  TestRule.c                      \
  TestRuleType.c                  \
  TestSBase.c                     \
  TestSBMLConvert.c               \
  TestSBMLDocument.c              \
  TestSBMLReader.c                \
  TestSBMLWriter.c                \
  TestSimpleSpeciesReference.c    \
  TestSpecies.c                   \
  TestSpeciesConcentrationRule.c  \
  TestSpeciesReference.c          \
  TestStack.c                     \
  TestStringBuffer.c              \
  TestReadFromFile1.c             \
  TestReadFromFile2.c             \
  TestReadFromFile3.c             \
  TestReadFromFile4.c             \
  TestRunner.c                    \
  TestUnitDefinition.c            \
  TestUnitKind.c                  \
  TestUnit.c                      \
  TestUtil.c

TestRunner_LDADD = libsbml.la

TESTS          = TestRunner
check_PROGRAMS = TestRunner
check_DATA     = test-data

check-TESTS: $(TESTS)
	@echo
	@echo
	@echo Running Tests
	@echo -------------
	@ srcdir=@TestDataDirectory@; export srcdir; ./TestRunner
	@echo
	@echo

endif  ## LIBCHECK_ENABLED
