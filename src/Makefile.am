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
##   Stephan Hoops
##


lib_LTLIBRARIES    = libsbml.la
libsbml_la_LDFLAGS = -no-undefined -version-info 2:4:0


if USE_EXPAT
libsbml_la_CPPFLAGS = -I../expat
libsbml_la_LIBADD   = -L../expat -lexpat-compat
endif  ## USE_EXPAT


nobase_include_HEADERS =            \
  sbml/AlgebraicRule.h              \
  sbml/AlgebraicRule.hpp            \
  sbml/AssignmentRule.h             \
  sbml/AssignmentRule.hpp           \
  sbml/ASTNode.h                    \
  sbml/ASTNode.hpp                  \
  sbml/ASTNodeType.h                \
  sbml/Compartment.h                \
  sbml/Compartment.hpp              \
  sbml/CompartmentVolumeRule.h      \
  sbml/CompartmentVolumeRule.hpp    \
  sbml/Event.h                      \
  sbml/Event.hpp                    \
  sbml/EventAssignment.h            \
  sbml/EventAssignment.hpp          \
  sbml/FormulaFormatter.h           \
  sbml/FormulaParser.h              \
  sbml/FormulaTokenizer.h           \
  sbml/FunctionDefinition.h         \
  sbml/FunctionDefinition.hpp       \
  sbml/KineticLaw.h                 \
  sbml/KineticLaw.hpp               \
  sbml/List.h                       \
  sbml/List.hpp                     \
  sbml/ListOf.h                     \
  sbml/ListOf.hpp                   \
  sbml/MathMLDocument.h             \
  sbml/MathMLDocument.hpp           \
  sbml/MathMLReader.h               \
  sbml/MathMLWriter.h               \
  sbml/Model.h                      \
  sbml/Model.hpp                    \
  sbml/ModifierSpeciesReference.h   \
  sbml/ModifierSpeciesReference.hpp \
  sbml/Parameter.h                  \
  sbml/Parameter.hpp                \
  sbml/ParameterRule.h              \
  sbml/ParameterRule.hpp            \
  sbml/ParseMessage.h               \
  sbml/ParseMessage.hpp             \
  sbml/RateRule.h                   \
  sbml/RateRule.hpp                 \
  sbml/Reaction.h                   \
  sbml/Reaction.hpp                 \
  sbml/Rule.h                       \
  sbml/Rule.hpp                     \
  sbml/RuleType.h                   \
  sbml/SBase.h                      \
  sbml/SBase.hpp                    \
  sbml/SBMLConvert.h                \
  sbml/SBMLDocument.h               \
  sbml/SBMLDocument.hpp             \
  sbml/SBMLReader.h                 \
  sbml/SBMLReader.hpp               \
  sbml/SBMLTypeCodes.h              \
  sbml/SBMLTypes.h                  \
  sbml/SBMLTypes.hpp                \
  sbml/SBMLWriter.h                 \
  sbml/SimpleSpeciesReference.h     \
  sbml/SimpleSpeciesReference.hpp   \
  sbml/Species.h                    \
  sbml/Species.hpp                  \
  sbml/SpeciesConcentrationRule.h   \
  sbml/SpeciesConcentrationRule.hpp \
  sbml/SpeciesReference.h           \
  sbml/SpeciesReference.hpp         \
  sbml/Stack.h                      \
  sbml/StringBuffer.h               \
  sbml/StringMap.h                  \
  sbml/UnitDefinition.h             \
  sbml/UnitDefinition.hpp           \
  sbml/Unit.h                       \
  sbml/Unit.hpp                     \
  sbml/UnitKind.h                   \
  sbml/Validator.h                  \
  sbml/XMLSchemaValidation.h        \
  sbml/common.h                     \
  sbml/config.h                     \
  sbml/extern.h                     \
  sbml/memory.h                     \
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
  MathMLDocument.cpp              \
  MathMLFormatter.cpp             \
  MathMLHandler.cpp               \
  MathMLReader.cpp                \
  MathMLTagCodes.cpp              \
  MathMLWriter.cpp                \
  SBMLTagCodes.cpp                \
  XMLStringFormatter.cpp          \
  XMLUtil.cpp                     \
  AlgebraicRule.cpp               \
  AssignmentRule.cpp              \
  ASTNode.cpp                     \
  Compartment.cpp                 \
  CompartmentVolumeRule.cpp       \
  Event.cpp                       \
  EventAssignment.cpp             \
  FormulaFormatter.c              \
  FormulaParser.c                 \
  FormulaTokenizer.c              \
  FunctionDefinition.cpp          \
  KineticLaw.cpp                  \
  List.cpp                        \
  ListOf.cpp                      \
  Model.cpp                       \
  ModifierSpeciesReference.cpp    \
  Parameter.cpp                   \
  ParameterRule.cpp               \
  ParseMessage.cpp                \
  RateRule.cpp                    \
  Reaction.cpp                    \
  Rule.cpp                        \
  RuleType.c                      \
  SBase.cpp                       \
  SBMLConvert.c                   \
  SBMLDocument.cpp                \
  SBMLFormatter.cpp               \
  SBMLHandler.cpp                 \
  SBMLReader.cpp                  \
  SBMLWriter.cpp                  \
  SimpleSpeciesReference.cpp      \
  Species.cpp                     \
  SpeciesConcentrationRule.cpp    \
  SpeciesReference.cpp            \
  Stack.c                         \
  StringBuffer.c                  \
  StringMap.c                     \
  Unit.cpp                        \
  UnitDefinition.cpp              \
  UnitKind.c                      \
  ValidationRules.c               \
  Validator.c                     \
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
  TestReadFromFile1.c             \
  TestReadFromFile2.c             \
  TestReadFromFile3.c             \
  TestReadFromFile4.c             \
  TestRule.c                      \
  TestRuleType.c                  \
  TestRunner.c                    \
  TestSBase.cpp                   \
  TestSBMLConvert.c               \
  TestSBMLDocument.c              \
  TestSBMLFormatter.cpp           \
  TestSBMLHandler.cpp             \
  TestSBMLReader.c                \
  TestSBMLWriter.c                \
  TestSimpleSpeciesReference.c    \
  TestSpecies.c                   \
  TestSpeciesConcentrationRule.c  \
  TestSpeciesReference.c          \
  TestStack.c                     \
  TestStringBuffer.c              \
  TestStringMap.c                 \
  TestUnitDefinition.c            \
  TestUnitKind.c                  \
  TestUnit.c                      \
  TestUtil.c                      \
  TestValidator.c


if USE_EXPAT
TestRunner_CPPFLAGS = -I../expat
endif

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
