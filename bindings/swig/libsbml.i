/**
 * Filename    : libsbml.i
 * Description : Language-independent SWIG directives for wrapping libSBML
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-04-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


%module libsbml


%{
#include "libsbml.hpp"
#include "local.cpp"

void SBaseTest_setup() { /* empty, but required to link. */ }
%}


%include local.i


%rename(formulaToString) SBML_formulaToString;
%rename(parseFormula)    SBML_parseFormula;

%ignore SBMLDocument::printWarnings;
%ignore SBMLDocument::printErrors;
%ignore SBMLDocument::printFatals;

%ignore ASTNode(Token_t*);
%ignore ASTNode::getListOfNodes;
%ignore ASTNode::fillListOfNodes;

%ignore ListOf::find;
%ignore ListOf::countIf;


%newobject SBMLReader::readSBMLFromString;
%newobject SBMLReader::readSBML;


%include "std_string.i"

%import  sbml/extern.h
%include sbml/ASTNodeType.h
%include sbml/ASTNode.h
%include sbml/ASTNode.hpp
%include sbml/MathMLDocument.hpp
%include sbml/SBMLTypeCodes.h
%include sbml/SBase.hpp
%include sbml/ListOf.hpp
%include sbml/Model.hpp
%include sbml/SBMLDocument.hpp
%include sbml/FunctionDefinition.hpp
%include sbml/Unit.hpp
%include sbml/UnitDefinition.hpp
%include sbml/Compartment.hpp
%include sbml/Species.hpp
%include sbml/Parameter.hpp
%include sbml/Rule.hpp
%include sbml/AlgebraicRule.hpp
%include sbml/AssignmentRule.hpp
%include sbml/RateRule.hpp
%include sbml/SpeciesConcentrationRule.hpp
%include sbml/CompartmentVolumeRule.hpp
%include sbml/ParameterRule.hpp
%include sbml/Reaction.hpp
%include sbml/KineticLaw.hpp
%include sbml/SimpleSpeciesReference.hpp
%include sbml/SpeciesReference.hpp
%include sbml/ModifierSpeciesReference.hpp
%include sbml/Event.hpp
%include sbml/EventAssignment.hpp
%include sbml/UnitKind.h
%include sbml/RuleType.h
%include sbml/ParseMessage.hpp
%include sbml/SBMLReader.hpp
%include sbml/XMLSchemaValidation.h


%typemap(newfree) char * "free($1);";

LIBSBML_EXTERN
int
writeSBML (SBMLDocument* d, const char *filename);

%newobject writeSBMLToString;
LIBSBML_EXTERN
char *
writeSBMLToString (SBMLDocument* d);


/**
 * This Java specific typemap applies only to SBML_formulaToString()
 * and overrides the default call to getCPtrAndDisown() (see
 * bindings/java/local.i).  Unfortunately, due to the nature of SWIG
 * typemaps, this is the only place to put it.
 */
#ifdef SWIGJAVA
%typemap(javain) ASTNode * "ASTNode.getCPtr($javainput)";
#endif

%newobject SBML_formulaToString;
LIBSBML_EXTERN
char *
SBML_formulaToString (const ASTNode* tree);

%newobject SBML_parseFormula;
LIBSBML_EXTERN
ASTNode*
SBML_parseFormula (const char *formula);

LIBSBML_EXTERN
MathMLDocument*
readMathMLFromString (const char *xml);

LIBSBML_EXTERN
int
writeMathML (MathMLDocument* d, const char *filename);

%newobject writeMathMLToString;
LIBSBML_EXTERN
char *
writeMathMLToString (MathMLDocument* d);

%newobject readSBML(const char *);
LIBSBML_EXTERN
SBMLDocument*
readSBML(const char*);

%newobject readSBMLFromString(const char *);
LIBSBML_EXTERN
SBMLDocument*
readSBMLFromString(const char*);
