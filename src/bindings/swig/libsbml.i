/**
 * \file    libsbml.i
 * \brief   Language-independent SWIG directives for wrapping libSBML
 * \author  Ben Bornstein and Ben Kovitz
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
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
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein and Ben Kovitz
 *     
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


%module libsbml


%{
#include "libsbml.h"
#ifdef USE_LAYOUT
#include "../swig/layout.h"
#endif /* USE_LAYOUT */
#include "local.cpp"

void SBaseTest_setup() { /* empty, but required to link. */ }
%}


%include local.i


/**
 * Unfortunately, SWIG makes no distinction between const and
 * non-const member functions (SWIG 1.3 Manual, Section 6.25), but in
 * libSBML C++ we have both const and non-const versions of
 * getListOfXXX().  To avoid a ton of Warning(516) messages, we turn
 * explicitly tell to ignore the const version (since it wouldn't wrap
 * it anyway).  We don't want to turn-off warning 516 altogether as it
 * might alert us to a genuine conflict at some later date.
 */
%ignore Model::getListOfFunctionDefinitions() const;
%ignore Model::getListOfUnitDefinitions    () const;
%ignore Model::getListOfCompartments       () const;
%ignore Model::getListOfSpecies            () const;
%ignore Model::getListOfParameters         () const;
%ignore Model::getListOfRules              () const;
%ignore Model::getListOfReactions          () const;
%ignore Model::getListOfEvents             () const;

%ignore UnitDefinition::getListOfUnits() const;

%ignore Reaction::getListOfReactants() const;
%ignore Reaction::getListOfProducts () const;
%ignore Reaction::getListOfModifiers() const;

%ignore KineticLaw::getListOfParameters() const;

%ignore Event::getListOfEventAssignments() const;


/**
 * Ignore the Visitor pattern accept() method (for now) on all SBML
 * objects.
 */
%ignore *::accept;


%ignore ASTNode(Token_t*);
%ignore ASTNode::getListOfNodes;
%ignore ASTNode::fillListOfNodes;
%ignore ASTNode::freeName;
%ignore ASTNode::setValue(int);
%ignore ASTNode::swapChildren(ASTNode*);

%ignore ListOf::countIf;
%ignore ListOf::find;
%ignore ListOf::freeItems;

%ignore Model::getListOfByTypecode(SBMLTypeCode_t);


/**
 * SWIG doesn't wrap FILE* or std::ostream very well so ignore these
 * methods.
 */
%ignore SBMLDocument::printWarnings;
%ignore SBMLDocument::printErrors;
%ignore SBMLDocument::printFatals;
%ignore SBMLWriter  ::write(const SBMLDocument&  , std::ostream&);
%ignore MathMLWriter::write(const MathMLDocument&, std::ostream&);

#ifdef USE_LAYOUT
%ignore Model::getListOfLayouts() const;
#endif /* USE_LAYOUT */

/**
 * The following methods will create new objects.  To prevent memory
 * leaks we must inform SWIG of this.
 */

%typemap(newfree) char * "free($1);";

%newobject SBase::toSBML;

%newobject SBMLReader::readSBMLFromString;
%newobject SBMLReader::readSBML;
%newobject readSBML(const char *);
%newobject readSBMLFromString(const char *);

%newobject SBMLWriter::writeToString;
%newobject writeSBMLToString;

%newobject readMathMLFromString;
%newobject writeMathMLToString;

%newobject SBML_formulaToString;
%newobject SBML_parseFormula;

%newobject ASTNode::deepCopy;


/**
 * In the wrapped languages, these methods will appear as:
 *
 *  - libsbml.formulaToString()
 *  - libsbml.parseFormula()
 */
%rename(formulaToString) SBML_formulaToString;
%rename(parseFormula)    SBML_parseFormula;


/**
 * Wrap these files.
 */

%include "std_string.i"

%import  common/extern.h
%import  common/sbmlfwd.h

%include sbml/SBMLReader.h
%include sbml/SBMLWriter.h

%include sbml/SBMLTypeCodes.h
%include sbml/SBase.h
%include sbml/ListOf.h
%include sbml/Model.h
%include sbml/SBMLDocument.h
%include sbml/FunctionDefinition.h
%include sbml/Unit.h
%include sbml/UnitDefinition.h
%include sbml/Compartment.h
%include sbml/Species.h
%include sbml/Parameter.h
%include sbml/Rule.h
%include sbml/AlgebraicRule.h
%include sbml/AssignmentRule.h
%include sbml/RateRule.h
%include sbml/SpeciesConcentrationRule.h
%include sbml/CompartmentVolumeRule.h
%include sbml/ParameterRule.h
%include sbml/Reaction.h
%include sbml/KineticLaw.h
%include sbml/SimpleSpeciesReference.h
%include sbml/SpeciesReference.h
%include sbml/ModifierSpeciesReference.h
%include sbml/Event.h
%include sbml/EventAssignment.h
%include sbml/UnitKind.h
%include sbml/RuleType.h

%include math/MathMLReader.h
%include math/MathMLWriter.h

%include math/ASTNode.h
%include math/ASTNodeType.h
%include math/FormulaParser.h
%include math/MathMLDocument.h

%include xml/ParseMessage.h
%include xml/XMLNamespace.h
%include xml/XMLNamespaceList.h
%include xml/XMLSchemaValidation.h

#ifdef USE_LAYOUT
%include ../swig/layout.i
#endif /* USE_LAYOUT */

/**
 * This Java specific typemap applies only to SBML_formulaToString()
 * and overrides the default call to getCPtrAndDisown() (see
 * bindings/java/local.i).  Unfortunately, due to the nature of SWIG
 * typemaps, this is the only place to put it.
 */
#ifdef SWIGJAVA
%typemap(javain) ASTNode * "ASTNode.getCPtr($javainput)";
#endif

/**
 * @return the given formula AST as an SBML L1 string formula.  The caller
 * owns the returned string and is responsible for freeing it.
 */
LIBSBML_EXTERN
char *
SBML_formulaToString (const ASTNode_t *tree);
