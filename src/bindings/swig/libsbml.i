/**
 * \file    libsbml.i
 * \brief   Language-independent SWIG directives for wrapping libSBML
 * \author  Ben Bornstein and Ben Kovitz
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


%module libsbml


%{
#include "libsbml.h"
#ifdef USE_LAYOUT
#include "../swig/layout.h"
#endif /* USE_LAYOUT */
#include "local.cpp"
%}


%include local.i

/**
 * Unfortunately, SWIG makes no distinction between const and non-const
 * member functions (SWIG 1.3 Manual, Section 6.25), but in libSBML C++ we
 * have both const and non-const versions of most getter methods.  To avoid
 * a ton of warning messages about 'const' methods not being wrapped, we
 * disable Warning(516).
 */
#pragma SWIG nowarn=516


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

/**
 * Ignore operator= and operator<< on all SBML objects.
 */
%ignore *::operator=;
%ignore *::operator<<;

/**
 * Ignore certain internal implementation methods on all objects.
 */
%ignore *::writeElements;
%ignore *::getElementPosition;

/**
 * Ignore methods whose pointer argument serves as both input and output
 */
%ignore XMLAttributes::readInto;

/**
 * Ignore methods which receive or return List*.
 */
%ignore ModelHistory::getCreator;
%ignore SBase::getCVTerms;
%ignore RDFAnnotationParser::parseRDFAnnotation(const XMLNode * annotation, List * CVTerms);

/**
 * Ignore methods which receive std::list.
 */
%ignore XMLErrorLog::add(const std::list<XMLError>& errors);

/**
 * Ignore 'static ParentMap mParent;' in SBO.h
 */
%ignore mParent;

/**
 * The following methods will create new objects.  To prevent memory
 * leaks we must inform SWIG of this.
 */

%typemap(newfree) char * "free($1);";

%newobject *::clone;
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
%newobject ListOf::remove;
%newobject RDFAnnotationParser::parseRDFAnnotation(XMLNode *);
%newobject RDFAnnotationParser::deleteRDFAnnotation;
%newobject RDFAnnotationParser::parseCVTerms;
%newobject RDFAnnotationParser::parseModelHistory;
%newobject RDFAnnotationParser::createRDFAnnotation;
%newobject RDFAnnotationParser::createAnnotation;
%newobject RDFAnnotationParser::createRDFDescription;
%newobject RDFAnnotationParser::createCVTerms;
%newobject convertUnitToSI;
%newobject convertToSI;

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
%import  xml/XMLExtern.h

%include sbml/SBMLReader.h
%include sbml/SBMLWriter.h
%include sbml/SBMLTypeCodes.h
%include sbml/SBase.h
%include sbml/ListOf.h
%include sbml/Model.h
%include sbml/SBMLDocument.h
%include sbml/FunctionDefinition.h
%include sbml/UnitKind.h
%include sbml/Unit.h
%include sbml/UnitDefinition.h
%include sbml/CompartmentType.h
%include sbml/SpeciesType.h
%include sbml/Compartment.h
%include sbml/Species.h
%include sbml/Parameter.h
%include sbml/InitialAssignment.h
%include sbml/Rule.h
%include sbml/Constraint.h
%include sbml/Reaction.h
%include sbml/KineticLaw.h
%include sbml/SpeciesReference.h
%include sbml/Event.h
%include sbml/EventAssignment.h
%include sbml/Trigger.h
%include sbml/Delay.h
%include sbml/SBO.h
%include sbml/StoichiometryMath.h

%include math/MathML.h
%include math/ASTNode.h
%include math/FormulaParser.h

%include xml/XMLAttributes.h
%include xml/XMLNamespaces.h
%include xml/XMLToken.h
%include xml/XMLNode.h
%include xml/XMLTriple.h
%include xml/XMLInputStream.h
%include xml/XMLOutputStream.h
%include xml/XMLError.h
%include xml/XMLErrorLog.h
%include xml/XMLHandler.h
%include xml/XMLParser.h
%include xml/XMLTokenizer.h

%include sbml/SBMLErrorLog.h

%include units/FormulaUnitsData.h
%include units/UnitFormulaFormatter.h
%include units/Utils_Unit.h
%include units/Utils_UnitDefinition.h

%include annotation/CVTerm.h
%include annotation/ModelHistory.h
%include annotation/RDFAnnotation.h

#ifdef USE_LAYOUT
%include ../swig/layout.i
#endif /* USE_LAYOUT */

/**
 * @return the given formula AST as an SBML L1 string formula.  The caller
 * owns the returned string and is responsible for freeing it.
 */
LIBSBML_EXTERN
char *
SBML_formulaToString (const ASTNode_t *tree);

