/**
 * @file    SBMLErrorTable.
 * @brief   Represents SBML errors and other diagnostics
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <string>
#include <iostream>
#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/SBMLError.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


typedef struct {
  unsigned int            code;
  SBMLError::SBMLCategory category;
  SBMLError::SBMLSeverity l1v1_severity;
  SBMLError::SBMLSeverity l1v2_severity;
  SBMLError::SBMLSeverity l2v1_severity;
  SBMLError::SBMLSeverity l2v2_severity;
  SBMLError::SBMLSeverity l2v3_severity;
  const char*             message;
} sbmlErrorTableEntry;

static const sbmlErrorTableEntry errorTable[] =
{
  // 0
  { SBMLError::UnknownError, 
    SBMLError::SBML, 
    SBMLError::Fatal,
    SBMLError::Fatal,
    SBMLError::Fatal,
    SBMLError::Fatal,
    SBMLError::Fatal,
    "Unrecognized error encountered" },

  //10101
  {
    SBMLError::NotUTF8,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "An SBML XML file must use UTF-8 as the character encoding. More "
    "precisely, the 'encoding' attribute of the XML declaration at the "
    "beginning of the XML data stream cannot have a value other than "
    "'UTF-8'. An example valid declaration is "
    "'<?xml version=\"1.0\" encoding=\"UTF-8\"?>'. (References: L2V2 Section "
    "4.1.)"
  },

  //10102
  {
    SBMLError::UnrecognizedElement,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "An SBML XML document must not contain undefined elements or attributes "
    "in the SBML namespace. Documents containing unknown elements or "
    "attributes placed in the SBML namespace do not conform to the SBML "
    "specification. (References: L2V2 Section 4.1.)"  
  },

  //10103
  {
    SBMLError::NotSchemaConformant,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "An SBML XML document must conform to the XML Schema for the corresponding "
    "SBML Level, Version and Revision. The XML Schema for SBML defines the "
    "basic SBML object structure, the data types used by those objects, and the "
    "order in which the objects may appear in an SBML document. (References: "
    "L2V2 Section 4.1.)"
  },

  //10201
  {
    SBMLError::InvalidMathElement,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "All MathML content in SBML must appear within a 'math' element, and the "
    "'math' element must be either explicitly or implicitly in the XML "
    "namespace 'http://www.w3.org/1998/Math/MathML'. (References: L2V2 "
    "Section 3.5.)"
  },

  //10202
  {
    SBMLError::DisallowedMathMLSymbol,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The only permitted MathML 2.0 elements in SBML Level 2 are "
    "the following: cn, ci, csymbol, sep, apply, piecewise, piece, otherwise, "
    "eq, neq, gt, lt, geq, leq, plus, minus, times, divide, power, root, abs, "
    "exp, ln, log, floor, ceiling, factorial, and, or, xor, not, degree, bvar, "
    "logbase, sin, cos, tan, sec, csc, cot, sinh, cosh, tanh, sech, csch, "
    "coth, arcsin, arccos, arctan, arcsec, arccsc, arccot, arcsinh, arccosh, "
    "arctanh, arcsech, arccsch, arccoth, true, false, notanumber, pi, "
    "infinity, exponentiale, semantics, annotation, and annotation-xml. "
    "(References: L2V2 Section 3.5.1.)"
  },

  //10203
  {
    SBMLError::DisallowedMathMLEncodingUse,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "encoding is only permitted on csymbol. No other MathML elements may "
    "have a encoding attribute. (References: L2V2 Section 3.5.1.)."
  },

  //10204
  {
    SBMLError::DisallowedDefinitionURLUse,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "definitionURL is only permitted on csymbol. No other MathML elements "
    "may have a definitionURL attribute. (References: L2V2 Section 3.5.1.)."
  },

  //10205
  {
    SBMLError::BadCsymbolDefinitionURLValue,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "In SBML Level 2 Versions 1 and Version 2, the only values permitted for "
    "definitionURL on a csymbol are \"http://www.sbml.org/sbml/symbols/time\" "
    "and \"http://www.sbml.org/sbml/symbols/delay\".(References: L2V2 "
    "Section 3.5.5.)." 
  },

  //10206
  {
    SBMLError::DisallowedMathTypeAttributeUse,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "type is only permitted on the cn construct. No other MathML elements "
    "may have a type attribute. (References: L2V2 Section 3.5.1.)." 
  },

  //10207
  {
    SBMLError::DisallowedMathTypeAttributeValue,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The only permitted values for the 'type' attribute on MathML 'cn' "
    "elements are 'e-notation', 'real', 'integer', and 'rational'. "
    "(References: L2V2 Section 3.5.2.)"
  },

  //10208
  {
    SBMLError::LambdaOnlyAllowedInFunctionDef,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10209
  {
    SBMLError::BooleanOpsNeedBooleanArgs,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10210
  {
    SBMLError::NumericOpsNeedNumericArgs,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10211
  {
    SBMLError::ArgsToEqNeedSameType,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10212
  {
    SBMLError::PiecewiseNeedsConsistentTypes,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10213
  {
    SBMLError::PieceNeedsBoolean,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10214
  {
    SBMLError::ApplyCiMustBeUserFunction,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10215
  {
    SBMLError::ApplyCiMustBeModelComponent,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10216
  {
    SBMLError::KineticLawParametersAreLocalOnly,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10217
  {
    SBMLError::MathResultMustBeNumeric,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10218
  {
    SBMLError::OpsNeedCorrectNumberOfArgs,
    SBMLError::SBMLConsistencyMathML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10301
  {
    SBMLError::DuplicateComponentId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10302
  {
    SBMLError::DuplicateUnitDefinitionId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10303
  {
    SBMLError::DuplicateLocalParameterId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10304
  {
    SBMLError::MultipleAssignmentOrRateRules,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10305
  {
    SBMLError::MultipleEventAssignmentsForId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10306
  {
    SBMLError::EventAndAssignmentRuleForId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10307
  {
    SBMLError::DuplicateMetaId,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10308
  {
    SBMLError::InvalidSBOTermSyntax,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10309
  {
    SBMLError::InvalidMetaidSyntax,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10310
  {
    SBMLError::InvalidIdSyntax,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10311
  {
    SBMLError::InvalidUnitIdSyntax,
    SBMLError::SBMLConsistencyIdentifier,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10403
  {
    SBMLError::SBMLNamespaceInAnnotation,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10501
  {
    SBMLError::InconsistentArgUnits,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10511
  {
    SBMLError::AssignRuleCompartmentMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10512
  {
    SBMLError::AssignRuleSpeciesMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10513
  {
    SBMLError::AssignRuleParameterMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10521
  {
    SBMLError::InitAssignCompartmenMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10522
  {
    SBMLError::InitAssignSpeciesMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10523
  {
    SBMLError::InitAssignParameterMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10531
  {
    SBMLError::RateRuleCompartmentMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10532
  {
    SBMLError::RateRuleSpeciesMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10533
  {
    SBMLError::RateRuleParameterMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10541
  {
    SBMLError::KineticLawNotSubstancePerTime,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10551
  {
    SBMLError::DelayUnitsNotTime,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10561
  {
    SBMLError::EventAssignCompartmentMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10562
  {
    SBMLError::EventAssignSpeciesMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10563
  {
    SBMLError::EventAssignParameterMismatch,
    SBMLError::SBMLConsistencyUnits,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10601
  {
    SBMLError::OverdeterminedSystem,
    SBMLError::SBML,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10701
  {
    SBMLError::InvalidModelSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10702
  {
    SBMLError::InvalidFunctionDefSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10703
  {
    SBMLError::InvalidParameterSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10704
  {
    SBMLError::InvalidInitAssignSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10705
  {
    SBMLError::InvalidRuleSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10706
  {
    SBMLError::InvalidConstraintSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10707
  {
    SBMLError::InvalidReactionSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10708
  {
    SBMLError::InvalidSpeciesReferenceSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10709
  {
    SBMLError::InvalidKineticLawSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10710
  {
    SBMLError::InvalidEventSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10711
  {
    SBMLError::InvalidEventAssignmentSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10712
  {
    SBMLError::InvalidCompartmentSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10713
  {
    SBMLError::InvalidSpeciesSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10714
  {
    SBMLError::InvalidCompartmentTypeSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10715
  {
    SBMLError::InvalidSpeciesTypeSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10716
  {
    SBMLError::InvalidTriggerSBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10717
  {
    SBMLError::InvalidDelaySBOTerm,
    SBMLError::SBMLConsistencySBO,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //10801
  {
    SBMLError::NotesNotInXHTMLNamespace,
    SBMLError::SBML,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10802
  {
    SBMLError::NotesContainsXMLDecl,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10803
  {
    SBMLError::NotesContainsDOCTYPE,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //10804
  {
    SBMLError::InvalidNotesContent,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20101
  {
    SBMLError::InvalidNamespaceOnSBML,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The 'sbml' container element must declare the XML Namespace for SBML, "
    "and this declaration must be consistent with the values of the 'level' "
    "and 'version' attributes on the 'sbml' element. (References: L2V2 "
    "Section 4.1.)" 
  },

  //20102
  {
    SBMLError::MissingOrInconsistentLevel,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The 'sbml' container element must declare the SBML Level using the "
    "attribute 'level', and this declaration must be consistent with the XML "
    "Namespace declared for the 'sbml' element. (References: L2V2 Section "
    "4.1.)" 
  },

  //20103
  {
    SBMLError::MissingOrInconsistentVersion,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The 'sbml' container element must declare the SBML Version using the "
    "attribute 'version', and this declaration must be consistent with the "
    "XML Namespace declared for the 'sbml' element. (References: L2V2 "
    "Section 4.1.)" 
  },

  //20104
  {
    SBMLError::AnnotationNotesNotAllowedLevel1,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    "The 'sbml' container element cannot contain notes or annotations in an "
    "SBML Level 1 document. (References: )" 
  },

  //20201
  {
    SBMLError::MissingModel,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "An SBML document must contain a <model> definition. (References: L2V1 "
    "and L2V2 Section 4.1)." 
  },

  //20202
  {
    SBMLError::IncorrectOrderInModel,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The order of subelements within Model must be the following (where any "
    "one may be optional, but the ordering must be maintained): "
    "listOfFunctionDefinitions, listOfUnitDefinitions, listOfCompartmentTypes, "
    "listOfSpeciesTypes, listOfCompartments, listOfSpecies, listOfParameters, "
    "listOfInitialAssignments, listOfRules, listOfConstraints, listOfReactions "
    "and listOfEvents. (References: L2V2 Section 4.2.)" 
  },

  //20203
  {
    SBMLError::EmptyListElement,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    "The 'listOf___' containers in a <model> are optional, but if present, "
    "the lists cannot be empty. Specifically, if any of the following are "
    "present in a <model>, they must not be empty: "
    "'listOfFunctionDefinitions', 'listOfUnitDefinitions', "
    "'listOfCompartmentTypes', 'listOfSpeciesTypes', 'listOfCompartments',  "
    "'listOfSpecies', 'listOfParameters', 'listOfInitialAssignments', "
    "'listOfRules', 'listOfConstraints', 'listOfReactions' and "
    "'listOfEvents'. (References: This is a requirement stemming from the "
    "XML Schema used for SBML.)" 
  },

  //20204
  {
    SBMLError::NeedCompartmentIfHaveSpecies,
    SBMLError::SBMLConsistency,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20301
  {
    SBMLError::FunctionDefMathNotLambda,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20302
  {
    SBMLError::InvalidApplyCiInLambda,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20303
  {
    SBMLError::RecursiveFunctionDefinition,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20304
  {
    SBMLError::InvalidCiInLambda,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20305
  {
    SBMLError::InvalidFunctionDefReturnType,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20306
  {
    SBMLError::ArgsPassedAsVariables,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    ""
  },

  //20401
  {
    SBMLError::InvalidUnitDefId,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20402
  {
    SBMLError::InvalidSubstanceRedefinition,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20403
  {
    SBMLError::InvalidLengthRedefinition,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20404
  {
    SBMLError::InvalidAreaRedefinition,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20405
  {
    SBMLError::InvalidTimeRedefinition,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20406
  {
    SBMLError::InvalidVolumeRedefinition,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20407
  {
    SBMLError::VolumeLitreDefExponentNotOne,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20408
  {
    SBMLError::VolumeMetreDefExponentNot3,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20409
  {
    SBMLError::EmptyListOfUnits,
    SBMLError::SBML,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::Error,
    SBMLError::Error,
    "The 'listOfUnits' container in a <unitDefinition> cannot be empty. "
    "(References: L2V2 Section 4.4.)" 
  },

  //20410
  {
    SBMLError::InvalidUnitKind,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20411
  {
    SBMLError::OffsetNoLongerValid,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    ""
  },

  //20412
  {
    SBMLError::CelsiusNoLongerValid,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    ""
  },

  //20501
  {
    SBMLError::ZeroDimensionalCompartmentSize,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20502
  {
    SBMLError::ZeroDimensionalCompartmentUnits,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20503
  {
    SBMLError::ZeroDimensionalCompartmentConst,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20504
  {
    SBMLError::UndefinedOutsideCompartment,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20505
  {
    SBMLError::RecursiveCompartmentContainment,
    SBMLError::SBMLConsistency,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20506
  {
    SBMLError::ZeroDCompartmentContainment,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20507
  {
    SBMLError::Invalid1DCompartmentUnits,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20508
  {
    SBMLError::Invalid2DCompartmentUnits,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20509
  {
    SBMLError::Invalid3DCompartmentUnits,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20510
  {
    SBMLError::InvalidCompartmentTypeRef,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20601
  {
    SBMLError::InvalidSpeciesCompartmentRef,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20602
  {
    SBMLError::HasOnlySubsNoSpatialUnits,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //20603
  {
    SBMLError::NoSpatialUnitsInZeroD,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //20604
  {
    SBMLError::NoConcentrationInZeroD,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20605
  {
    SBMLError::SpatialUnitsInOneD,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //20606
  {
    SBMLError::SpatialUnitsInTwoD,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //20607
  {
    SBMLError::SpatialUnitsInThreeD,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //20608
  {
    SBMLError::InvalidSpeciesSusbstanceUnits,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20609
  {
    SBMLError::BothAmountAndConcentrationSet,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20610
  {
    SBMLError::NonBoundarySpeciesAssignedAndUsed,
    SBMLError::SBMLConsistency,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20611
  {
    SBMLError::NonConstantSpeciesUsed,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20612
  {
    SBMLError::InvalidSpeciesTypeRef,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20613
  {
    SBMLError::MultSpeciesSameTypeInCompartment,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20614
  {
    SBMLError::MissingSpeciesCompartment,
    SBMLError::SBMLConsistency,
    SBMLError::SchemaError,
    SBMLError::SchemaError,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20615
  {
    SBMLError::SpatialSizeUnitsRemoved,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::SchemaError,
    ""
  },

  //20701
  {
    SBMLError::InvalidParameterUnits,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20801
  {
    SBMLError::InvalidInitAssignSymbol,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20802
  {
    SBMLError::MultipleInitAssignments,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20803
  {
    SBMLError::InitAssignmentAndRuleForSameId,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20901
  {
    SBMLError::InvalidAssignRuleVariable,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20902
  {
    SBMLError::InvalidRateRuleVariable,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20903
  {
    SBMLError::AssignmentToConstantEntity,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20904
  {
    SBMLError::RateRuleForConstantEntity,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //20905
  {
    SBMLError::RepeatedRule10304,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    ""
  },

  //20906
  {
    SBMLError::CircularRuleDependency,
    SBMLError::SBMLConsistency,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21001
  {
    SBMLError::ConstraintMathNotBoolean,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21002
  {
    SBMLError::IncorrectOrderInConstraint,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21003
  {
    SBMLError::ConstraintNotInXHTMLNamespace,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21004
  {
    SBMLError::ConstraintContainsXMLDecl,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21005
  {
    SBMLError::ConstraintContainsDOCTYPE,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21006
  {
    SBMLError::InvalidConstraintContent,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21101
  {
    SBMLError::NoReactantsOrProducts,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21102
  {
    SBMLError::IncorrectOrderInReaction,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21103
  {
    SBMLError::EmptyListInReaction,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21104
  {
    SBMLError::InvalidReactantsProductsList,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21105
  {
    SBMLError::InvalidModifiersList,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21111
  {
    SBMLError::InvalidSpeciesReference,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21112
  {
    SBMLError::RepeatedRule20611,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    ""
  },

  //21113
  {
    SBMLError::BothStoichiometryAndMath,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21121
  {
    SBMLError::UndeclaredSpeciesRef,
    SBMLError::SBMLConsistency,
    SBMLError::GeneralWarning,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21122
  {
    SBMLError::IncorrectOrderInKineticLaw,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21123
  {
    SBMLError::EmptyListInKineticLaw,
    SBMLError::SBML,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21124
  {
    SBMLError::NonConstantLocalParameter,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21125
  {
    SBMLError::SubsUnitsNoLongerValid,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21126
  {
    SBMLError::TimeUnitsNoLongerValid,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21127
  {
    SBMLError::SubsUnitsAllowedInKL,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    ""
  },

  //21128
  {
    SBMLError::TimeUnitsAllowedInKL,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    ""
  },

  //21129
  {
    SBMLError::FormulaInLevel1KL,
    SBMLError::SBMLConsistency,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    ""
  },

  //21131
  {
    SBMLError::UndeclaredSpeciesInStoichMath,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21201
  {
    SBMLError::MissingTriggerInEvent,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::SchemaError,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21202
  {
    SBMLError::TriggerMathNotBoolean,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21203
  {
    SBMLError::MissingEventAssignment,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21204
  {
    SBMLError::TimeUnitsEvent,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::NotApplicable,
    ""
  },

  //21205
  {
    SBMLError::IncorrectOrderInEvent,
    SBMLError::SBML,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21206
  {
    SBMLError::TimeUnitsRemoved,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    ""
  },

  //21211
  {
    SBMLError::InvalidEventAssignmentVariable,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::Error,
    SBMLError::Error,
    SBMLError::Error,
    ""
  },

  //21212
  {
    SBMLError::EventAssignmentForConstantEntity,
    SBMLError::SBMLConsistency,
    SBMLError::NotApplicable,
    SBMLError::NotApplicable,
    SBMLError::GeneralWarning,
    SBMLError::Error,
    SBMLError::Error,
    ""
  }

};