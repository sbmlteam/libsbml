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


/**
 * Forward declaration of all opaque C types.
 *
 * Declaring all types up-front avoids "redefinition of type 'Foo'" compile
 * errors and allows our combined C/C++ headers to depend minimally upon
 * each other.  Put another way, the type definitions below serve the same
 * purpose as "class Foo;" forward declarations in C++ code.
 */

typedef struct SBase                     SBase_t;
typedef struct SBMLDocument              SBMLDocument_t;
typedef struct Model                     Model_t;

typedef struct FunctionDefinition        FunctionDefinition_t;
typedef struct Unit                      Unit_t;
typedef struct UnitDefinition            UnitDefinition_t;
typedef struct Compartment               Compartment_t;
typedef struct Species                   Species_t;
typedef struct Parameter                 Parameter_t;

typedef struct Rule                      Rule_t;
typedef struct AssignmentRule            AssignmentRule_t;
typedef struct AlgebraicRule             AlgebraicRule_t;
typedef struct RateRule                  RateRule_t;
typedef struct CompartmentVolumeRule     CompartmentVolumeRule_t;
typedef struct ParameterRule             ParameterRule_t;
typedef struct SpeciesConcentrationRule  SpeciesConcentrationRule_t;

typedef struct Reaction                  Reaction_t;
typedef struct KineticLaw                KineticLaw_t;
typedef struct SimpleSpeciesReference    SimpleSpeciesReference_t;
typedef struct ModifierSpeciesReference  ModifierSpeciesReference_t;
typedef struct SpeciesReference          SpeciesReference_t;

typedef struct Event                     Event_t;
typedef struct EventAssignment           EventAssignment_t;

typedef struct SBMLReader                SBMLReader_t;

typedef struct MathMLDocument            MathMLDocument_t;
typedef struct MathMLReader              MathMLReader_t;
typedef struct MathMLWriter              MathMLWriter_t;

typedef struct ASTNode                   ASTNode_t;
typedef struct List                      List_t;
typedef struct ListOf                    ListOf_t;
typedef struct ParseMessage              ParseMessage_t;


#endif  /* sbmlfwd_h  */
