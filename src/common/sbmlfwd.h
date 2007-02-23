/**
 * \file    sbmlfwd.h
 * \brief   Forward declarations for all opaque C types
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
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


#ifndef sbmlfwd_h
#define sbmlfwd_h


#include <sbml/common/libsbml-config.h>


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
typedef CLASS_OR_STRUCT CompartmentType           CompartmentType_t;
typedef CLASS_OR_STRUCT SpeciesType               SpeciesType_t;
typedef CLASS_OR_STRUCT Compartment               Compartment_t;
typedef CLASS_OR_STRUCT Species                   Species_t;
typedef CLASS_OR_STRUCT Parameter                 Parameter_t;
typedef CLASS_OR_STRUCT InitialAssignment         InitialAssignment_t;

typedef CLASS_OR_STRUCT Rule                      Rule_t;
typedef CLASS_OR_STRUCT Constraint                Constraint_t;

typedef CLASS_OR_STRUCT Reaction                  Reaction_t;
typedef CLASS_OR_STRUCT KineticLaw                KineticLaw_t;

/**
 * In C a SpeciesReference_t is actually a synonym for the
 * SimpleSpeciesReference base class.
 */
typedef CLASS_OR_STRUCT SimpleSpeciesReference    SpeciesReference_t;

typedef CLASS_OR_STRUCT Event                     Event_t;
typedef CLASS_OR_STRUCT EventAssignment           EventAssignment_t;

typedef CLASS_OR_STRUCT SBMLReader                SBMLReader_t;
typedef CLASS_OR_STRUCT SBMLWriter                SBMLWriter_t;

typedef CLASS_OR_STRUCT ASTNode                   ASTNode_t;
typedef CLASS_OR_STRUCT List                      List_t;
typedef CLASS_OR_STRUCT ListOf                    ListOf_t;

typedef CLASS_OR_STRUCT XMLError                  XMLError_t;
typedef CLASS_OR_STRUCT XMLErrorLog               XMLErrorLog_t;
typedef CLASS_OR_STRUCT XMLNode                   XMLNode_t;
typedef CLASS_OR_STRUCT XMLAttributes             XMLAttributes_t;
typedef CLASS_OR_STRUCT XMLNamespaces		          XMLNamespaces_t;
typedef CLASS_OR_STRUCT XMLToken                  XMLToken_t;
typedef CLASS_OR_STRUCT XMLTriple                 XMLTriple_t;

typedef CLASS_OR_STRUCT Trigger                   Trigger_t;
typedef CLASS_OR_STRUCT Delay                     Delay_t;

typedef CLASS_OR_STRUCT CVTerm                    CVTerm_t;
typedef CLASS_OR_STRUCT Date                      Date_t;
typedef CLASS_OR_STRUCT ModelCreator              ModelCreator_t;
typedef CLASS_OR_STRUCT ModelHistory              ModelHistory_t;


#undef CLASS_OR_STRUCT

#ifdef USE_LAYOUT
#  include <sbml/layout/layoutfwd.h>
#endif

#endif  /* sbmlfwd_h  */
