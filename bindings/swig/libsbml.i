/**
 * Filename    : libsbml.i
 * Description : SWIG interface description for wrapping libSBML API
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


%module  libsbml
%{
typedef void SBMLDocument_t;

#include "sbml/SBMLTypeCodes.h"
#include "sbml/UnitKind.h"
#include "sbml/RuleType.h"

#include "sbml/ASTNode.hpp"
#include "sbml/ASTNodeType.h"

#include "sbml/MathMLDocument.hpp"
#include "sbml/SBase.hpp"
#include "sbml/ListOf.hpp"
#include "sbml/Compartment.hpp"
#include "sbml/SBMLDocument.hpp"
#include "sbml/Event.hpp"
#include "sbml/EventAssignment.hpp"
#include "sbml/FunctionDefinition.hpp"
#include "sbml/KineticLaw.hpp"
#include "sbml/ListOf.hpp"
#include "sbml/Model.hpp"
#include "sbml/Parameter.hpp"
#include "sbml/Reaction.hpp"
#include "sbml/Species.hpp"
#include "sbml/SpeciesReference.hpp"
#include "sbml/ModifierSpeciesReference.hpp"
#include "sbml/UnitDefinition.hpp"
#include "sbml/Unit.hpp"
#include "sbml/AlgebraicRule.hpp"
#include "sbml/AssignmentRule.hpp"
#include "sbml/RateRule.hpp"
#include "sbml/SpeciesConcentrationRule.hpp"
#include "sbml/CompartmentVolumeRule.hpp"
#include "sbml/ParameterRule.hpp"

#include "sbml/ParseMessage.hpp"

#include "sbml/SBMLReader.hpp"
#include "sbml/SBMLWriter.h"

#include "sbml/MathMLReader.h"
#include "sbml/MathMLWriter.h"

#include "sbml/FormulaFormatter.h"
#include "sbml/FormulaParser.h"


void SBaseTest_setup() { }

struct swig_type_info*
polymorphic_type_info (SBase* sbase)
{
   if (sbase == NULL) return SWIGTYPE_p_SBase;


   switch (sbase->getTypeCode())
   {
     case SBML_COMPARTMENT:
       return SWIGTYPE_p_Compartment;

     case SBML_DOCUMENT:
       return SWIGTYPE_p_SBMLDocument;

     case SBML_EVENT:
       return SWIGTYPE_p_Event;

     case SBML_EVENT_ASSIGNMENT:
       return SWIGTYPE_p_EventAssignment;

     case SBML_FUNCTION_DEFINITION:
       return SWIGTYPE_p_FunctionDefinition;

     case SBML_KINETIC_LAW:
       return SWIGTYPE_p_KineticLaw;

     case SBML_LIST_OF:
       return SWIGTYPE_p_ListOf;

     case SBML_MODEL:
       return SWIGTYPE_p_Model;

     case SBML_PARAMETER:
       return SWIGTYPE_p_Parameter;

     case SBML_REACTION:
       return SWIGTYPE_p_Reaction;

     case SBML_SPECIES:
       return SWIGTYPE_p_Species;

     case SBML_SPECIES_REFERENCE:
       return SWIGTYPE_p_SpeciesReference;

     case SBML_MODIFIER_SPECIES_REFERENCE:
       return SWIGTYPE_p_ModifierSpeciesReference;

     case SBML_UNIT_DEFINITION:
       return SWIGTYPE_p_UnitDefinition;

     case SBML_UNIT:
       return SWIGTYPE_p_Unit;

     case SBML_ALGEBRAIC_RULE:
       return SWIGTYPE_p_AlgebraicRule;

     case SBML_ASSIGNMENT_RULE:
       return SWIGTYPE_p_AssignmentRule;

     case SBML_RATE_RULE:
       return SWIGTYPE_p_RateRule;

     case SBML_SPECIES_CONCENTRATION_RULE:
       return SWIGTYPE_p_SpeciesConcentrationRule;

     case SBML_COMPARTMENT_VOLUME_RULE:
       return SWIGTYPE_p_CompartmentVolumeRule;

     case SBML_PARAMETER_RULE:
       return SWIGTYPE_p_ParameterRule;

     default:
       return SWIGTYPE_p_SBase;
   }
}

%}


%extend ListOf
{
  SBase* __getitem__(int index)
  {
    return self->get(index);
  }
}


%typemap(out) SBase* {
   $result = SWIG_NewPointerObj($1, polymorphic_type_info($1), 0);
}


%feature("shadow") ListOf::append(SBase*) %{
   def append(*args):
      args[0].thisown = 0
      return _libsbml.ListOf_append(*args)
%}

%feature("shadow") SBMLDocument::setModel(Model*) %{
   def setModel(*args):
      args[0].thisown = 0
      return _libsbml.SBMLDocument_setModel(*args)
%}

%feature("shadow") Reaction::Reaction(const std::string&, KineticLaw*, bool) %{
   def __init__(self, *args):
      _swig_setattr(self, Reaction, 'this', _libsbml.new_Reaction(*args))
      _swig_setattr(self, Reaction, 'thisown', 1)
      # print args
      # args[1].thisown = 0
%}


%include "std_string.i"
%import  ../../src/sbml/extern.h
%include ../../src/sbml/ASTNodeType.h
%include ../../src/sbml/ASTNode.h
%include ../../src/sbml/ASTNode.hpp
%include ../../src/sbml/MathMLDocument.hpp
%include ../../src/sbml/SBMLTypeCodes.h
%include ../../src/sbml/SBase.hpp
%include ../../src/sbml/ListOf.hpp
%include ../../src/sbml/Model.hpp
%include ../../src/sbml/SBMLDocument.hpp
%include ../../src/sbml/FunctionDefinition.hpp
%include ../../src/sbml/Unit.hpp
%include ../../src/sbml/UnitDefinition.hpp
%include ../../src/sbml/Compartment.hpp
%include ../../src/sbml/Species.hpp
%include ../../src/sbml/Parameter.hpp
%include ../../src/sbml/Rule.hpp
%include ../../src/sbml/AlgebraicRule.hpp
%include ../../src/sbml/AssignmentRule.hpp
%include ../../src/sbml/RateRule.hpp
%include ../../src/sbml/SpeciesConcentrationRule.hpp
%include ../../src/sbml/CompartmentVolumeRule.hpp
%include ../../src/sbml/ParameterRule.hpp
%include ../../src/sbml/Reaction.hpp
%include ../../src/sbml/KineticLaw.hpp
%include ../../src/sbml/SimpleSpeciesReference.hpp
%include ../../src/sbml/SpeciesReference.hpp
%include ../../src/sbml/ModifierSpeciesReference.hpp
%include ../../src/sbml/Event.hpp
%include ../../src/sbml/EventAssignment.hpp
%include ../../src/sbml/UnitKind.h
%include ../../src/sbml/RuleType.h
%include ../../src/sbml/ParseMessage.hpp
%include ../../src/sbml/SBMLReader.hpp
%include ../../src/sbml/XMLSchemaValidation.h

%rename(formulaToString) SBML_formulaToString;
%rename(parseFormula)    SBML_parseFormula;

LIBSBML_EXTERN
int
writeSBML (SBMLDocument* d, const char *filename);

LIBSBML_EXTERN
char *
writeSBMLToString (SBMLDocument* d);

LIBSBML_EXTERN
char *
SBML_formulaToString (const ASTNode* tree);

LIBSBML_EXTERN
ASTNode*
SBML_parseFormula (const char *formula);

LIBSBML_EXTERN
MathMLDocument*
readMathMLFromString (const char *xml);

LIBSBML_EXTERN
int
writeMathML (MathMLDocument* d, const char *filename);

LIBSBML_EXTERN
char *
writeMathMLToString (MathMLDocument* d);
