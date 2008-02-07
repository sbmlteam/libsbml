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
 * Ignore internal implementation methods in MathML.h
 */
%ignore readMathML;
%ignore writeMathML;

/**
 * Ignore methods whose pointer argument serves as both input and output
 */
%ignore XMLAttributes::readInto;

/**
 * Ignore methods which receive or return List*.
 */
%ignore ModelHistory::getListCreators;
%ignore SBase::getCVTerms;
%ignore RDFAnnotationParser::parseRDFAnnotation(const XMLNode * annotation, List * CVTerms);

/**
 * Ignore methods which receive std::list.
 */
%ignore XMLErrorLog::add(const std::list<XMLError>& errors);
%ignore SBMLErrorLog::add(const std::list<SBMLError>& errors);

/**
 * Ignore 'static ParentMap mParent;' in SBO.h
 */
%ignore mParent;

/**
 * Ignore 'struct xmlErrorTableEntry' in XMLError.h.
 */
%ignore xmlErrorTableEntry;

/**
 * Both "const std::string& SBase::getMetaId() const" and
 * "std:string& SBase::getMetaId()" are defined in SBase.cpp.
 * By default, SWIG doesn't convert non-const std:string& to and from
 * target language string.
 * So we ignore the non-const version.
 */
%ignore SBase::getMetaId();

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
%newobject XMLNode::convertStringToXMLNode;
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

%import  sbml/common/extern.h
%import  sbml/common/sbmlfwd.h
%import  sbml/xml/XMLExtern.h

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

%include sbml/math/MathML.h
%include sbml/math/ASTNode.h
%include sbml/math/FormulaParser.h

%include sbml/xml/XMLAttributes.h
%include sbml/xml/XMLNamespaces.h
%include sbml/xml/XMLToken.h
%include sbml/xml/XMLNode.h
%include sbml/xml/XMLTriple.h
%include sbml/xml/XMLInputStream.h
%include sbml/xml/XMLOutputStream.h
%include sbml/xml/XMLError.h
%include sbml/xml/XMLErrorLog.h
%include sbml/xml/XMLHandler.h
%include sbml/xml/XMLParser.h
%include sbml/xml/XMLTokenizer.h

%include sbml/SBMLErrorLog.h
%include sbml/SBMLError.h

%include sbml/units/FormulaUnitsData.h
%include sbml/units/Utils_Unit.h
%include sbml/units/Utils_UnitDefinition.h

%include sbml/annotation/CVTerm.h
%include sbml/annotation/ModelHistory.h
%include sbml/annotation/RDFAnnotation.h

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

