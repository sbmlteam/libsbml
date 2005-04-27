/**
 * \file    sbmlfwd.h
 * \brief   Forward declarations for all opaque C types
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef sbmlfwd_h
#define sbmlfwd_h


#include "libsbml-config.h"


/**
 * Forward declaration of all opaque C types.
 *
 * Declaring all types up-front avoids "redefinition of type 'Foo'" compile
 * errors and allows our combined C/C++ headers to depend minimally upon
 * each other.  Put another way, the type definitions below serve the same
 * purpose as "class Foo;" forward declarations in C++ code.
 */

#ifdef __cplusplus
#  define CLASS_OR_STRUCT class
#else
#  define CLASS_OR_STRUCT struct
#endif  /* __cplusplus */


typedef CLASS_OR_STRUCT SBase                     SBase_t;
typedef CLASS_OR_STRUCT SBMLDocument              SBMLDocument_t;
typedef CLASS_OR_STRUCT Model                     Model_t;

typedef CLASS_OR_STRUCT FunctionDefinition        FunctionDefinition_t;
typedef CLASS_OR_STRUCT Unit                      Unit_t;
typedef CLASS_OR_STRUCT UnitDefinition            UnitDefinition_t;
typedef CLASS_OR_STRUCT Compartment               Compartment_t;
typedef CLASS_OR_STRUCT Species                   Species_t;
typedef CLASS_OR_STRUCT Parameter                 Parameter_t;

typedef CLASS_OR_STRUCT Rule                      Rule_t;
typedef CLASS_OR_STRUCT AssignmentRule            AssignmentRule_t;
typedef CLASS_OR_STRUCT AlgebraicRule             AlgebraicRule_t;
typedef CLASS_OR_STRUCT RateRule                  RateRule_t;
typedef CLASS_OR_STRUCT CompartmentVolumeRule     CompartmentVolumeRule_t;
typedef CLASS_OR_STRUCT ParameterRule             ParameterRule_t;
typedef CLASS_OR_STRUCT SpeciesConcentrationRule  SpeciesConcentrationRule_t;

typedef CLASS_OR_STRUCT Reaction                  Reaction_t;
typedef CLASS_OR_STRUCT KineticLaw                KineticLaw_t;
typedef CLASS_OR_STRUCT SimpleSpeciesReference    SimpleSpeciesReference_t;
typedef CLASS_OR_STRUCT ModifierSpeciesReference  ModifierSpeciesReference_t;
typedef CLASS_OR_STRUCT SpeciesReference          SpeciesReference_t;

typedef CLASS_OR_STRUCT Event                     Event_t;
typedef CLASS_OR_STRUCT EventAssignment           EventAssignment_t;

typedef CLASS_OR_STRUCT SBMLReader                SBMLReader_t;
typedef CLASS_OR_STRUCT SBMLWriter                SBMLWriter_t;

typedef CLASS_OR_STRUCT MathMLDocument            MathMLDocument_t;
typedef CLASS_OR_STRUCT MathMLReader              MathMLReader_t;
typedef CLASS_OR_STRUCT MathMLWriter              MathMLWriter_t;

typedef CLASS_OR_STRUCT ASTNode                   ASTNode_t;
typedef CLASS_OR_STRUCT List                      List_t;
typedef CLASS_OR_STRUCT ListOf                    ListOf_t;
typedef CLASS_OR_STRUCT ParseMessage              ParseMessage_t;


#undef CLASS_OR_STRUCT

#ifdef USE_LAYOUT
#  include "sbml/layout/layoutfwd.h"
#endif

#endif  /* sbmlfwd_h  */
