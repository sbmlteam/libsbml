##
## Filename    : Makefile.am
## Description : Process this file with automake (1.7.2) to produce Makefile.in
## Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
## Organization: Caltech ERATO Kitano Systems Biology Project
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
##     The Systems Biology Workbench Development Group
##     ERATO Kitano Systems Biology Project
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
libsbml_la_LDFLAGS = -version-info 2:0:1

nobase_include_HEADERS =          \
  sbml/AlgebraicRule.h            \
  sbml/AssignmentRule.h           \
  sbml/Compartment.h              \
  sbml/CompartmentVolumeRule.h    \
  sbml/KineticLaw.h               \
  sbml/List.h                     \
  sbml/Model.h                    \
  sbml/Parameter.h                \
  sbml/ParameterRule.h            \
  sbml/Reaction.h                 \
  sbml/Rule.h                     \
  sbml/RuleType.h                 \
  sbml/SBase.h                    \
  sbml/SBMLDocument.h             \
  sbml/SBMLReader.h               \
  sbml/SBMLTypeCodes.h            \
  sbml/SBMLTypes.h                \
  sbml/Species.h                  \
  sbml/SpeciesConcentrationRule.h \
  sbml/SpeciesReference.h         \
  sbml/Stack.h                    \
  sbml/UnitDefinition.h           \
  sbml/Unit.h                     \
  sbml/UnitKind.h                 \
  sbml/common.h                   \
  sbml/config.h                   \
  sbml/memory.h                   \
  sbml/util.h

libsbml_la_SOURCES =            \
  sbml/common.hpp               \
  sbml/SAX2SBMLHandler.hpp      \
  sbml/SBMLFormatter.hpp        \
  sbml/SBMLHashCodes.hpp        \
  sbml/SBMLUnicodeConstants.hpp \
  sbml/XMLStringFormatter.hpp   \
  sbml/XMLUtil.hpp              \
  SAX2SBMLHandler.cpp           \
  SBMLFormatter.cpp             \
  SBMLHashCodes.cpp             \
  SBMLReader.cpp                \
  XMLStringFormatter.cpp        \
  XMLUtil.cpp                   \
  AlgebraicRule.c               \
  AssignmentRule.c              \
  Compartment.c                 \
  CompartmentVolumeRule.c       \
  KineticLaw.c                  \
  List.c                        \
  Model.c                       \
  Parameter.c                   \
  ParameterRule.c               \
  Reaction.c                    \
  Rule.c                        \
  RuleType.c                    \
  SBase.c                       \
  SBMLDocument.c                \
  Species.c                     \
  SpeciesConcentrationRule.c    \
  SpeciesReference.c            \
  Stack.c                       \
  Unit.c                        \
  UnitDefinition.c              \
  UnitKind.c                    \
  memory.c                      \
  util.c

TestRunner_SOURCES =             \
  test-data                      \
  sbml/SAX2AttributesMock.hpp    \
  SAX2AttributesMock.cpp         \
  TestSAX2AttributesMock.cpp     \
  TestSAX2SBMLHandler.cpp        \
  TestSBMLFormatter.cpp          \
  TestXMLStringFormatter.cpp     \
  TestXMLUtil.cpp                \
  TestAlgebraicRule.c            \
  TestAssignmentRule.c           \
  TestCompartment.c              \
  TestCompartmentVolumeRule.c    \
  TestKineticLaw.c               \
  TestList.c                     \
  TestMemory.c                   \
  TestModel.c                    \
  TestParameterRule.c            \
  TestParameter.c                \
  TestReaction.c                 \
  TestRule.c                     \
  TestRuleType.c                 \
  TestSBase.c                    \
  TestSBMLDocument.c             \
  TestSBMLReader.c               \
  TestSpecies.c                  \
  TestSpeciesConcentrationRule.c \
  TestSpeciesReference.c         \
  TestStack.c                    \
  TestReadFromFile1.c            \
  TestReadFromFile2.c            \
  TestReadFromFile3.c            \
  TestReadFromFile4.c            \
  TestRunner.c                   \
  TestUnitDefinition.c           \
  TestUnitKind.c                 \
  TestUnit.c                     \
  TestUtil.c

noinst_PROGRAMS  = readSBML
readSBML_SOURCES = readSBML.c
readSBML_LDADD   = libsbml.la

TestRunner_LDADD = libsbml.la

TESTS          = TestRunner
check_PROGRAMS = TestRunner
check_DATA     = test-data

check-TESTS: $(TESTS)
	@echo
	@echo
	@echo Running Tests
	@echo -------------
	@ srcdir=$(srcdir); export srcdir; ./TestRunner
	@echo
	@echo
