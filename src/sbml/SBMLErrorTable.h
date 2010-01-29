/**
 * @file    SBMLErrorTable.h
 * @brief   Represents SBML errors and other diagnostics
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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


LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygen-libsbml-internal */

typedef struct {
  unsigned int code;
  const char*  shortMessage;
  unsigned int category;
  unsigned int l1v1_severity;
  unsigned int l1v2_severity;
  unsigned int l2v1_severity;
  unsigned int l2v2_severity;
  unsigned int l2v3_severity;
  unsigned int l2v4_severity;
  const char*  message;
} sbmlErrorTableEntry;


static const sbmlErrorTableEntry errorTable[] =
{
  // 10000
  { UnknownError, 
    "Unknown internal libSBML error",
    LIBSBML_CAT_INTERNAL, 
    LIBSBML_SEV_FATAL,
    LIBSBML_SEV_FATAL,
    LIBSBML_SEV_FATAL,
    LIBSBML_SEV_FATAL,
    LIBSBML_SEV_FATAL,
    LIBSBML_SEV_FATAL,
    "Unrecognized error encountered by libSBML" },

  //10101
  {
    NotUTF8,
    "Not UTF8",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An SBML XML file must use UTF-8 as the character encoding. More "
    "precisely, the 'encoding' attribute of the XML declaration at the "
    "beginning of the XML data stream cannot have a value other than "
    "'UTF-8'. An example valid declaration is "
    "'<?xml version=\"1.0\" encoding=\"UTF-8\"?>'. (References: L2V2 Section "
    "4.1; L2V3 Section 4.1; L2V4 Section 4.1)"
  },

  //10102
  {
    UnrecognizedElement,
    "Unrecognized element",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An SBML XML document must not contain undefined elements or attributes "
    "in the SBML namespace. Documents containing unknown elements or "
    "attributes placed in the SBML namespace do not conform to the SBML "
    "specification. (References: L2V2 Section 4.1; L2V3 Section 4.1; L2V4 "
    "Section 4.1)"  
  },

  //10103
  {
    NotSchemaConformant,
    "Not conformant to SBML XML schema",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An SBML XML document must conform to the XML Schema for the corresponding "
    "SBML Level, Version and Release. The XML Schema for SBML defines the "
    "basic SBML object structure, the data types used by those objects, and the "
    "order in which the objects may appear in an SBML document. (References: "
    "L2V2 Section 4.1; L2V3 Section 4.1; L2V4 Section 4.1)"
  },

  //10201
  {
    InvalidMathElement,
    "Invalid MathML",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "All MathML content in SBML must appear within a <math> element, and the "
    "<math> element must be either explicitly or implicitly in the XML "
    "namespace \"http://www.w3.org/1998/Math/MathML\". (References: L2V2 "
    "Section 3.5; L2V3 Section 3.4; L2V4 Section 3.4)"
  },

  //10202
  {
    DisallowedMathMLSymbol,
    "Disallowed MathML symbol",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The only permitted MathML 2.0 elements in SBML Level 2 are "
    "the following: <cn>, <ci>, <csymbol>, <sep>, <apply>, <piecewise>, "
    "<piece>, <otherwise>, <eq>, <neq>, <gt>, <lt>, <geq>, <leq>, <plus>, "
    "<minus>, <times>, <divide>, <power>, <root>, <abs>, <exp>, <ln>, <log>, "
    "<floor>, <ceiling>, <factorial>, <and>, <or>, <xor>, <not>, <degree>, "
    "<bvar>, <logbase>, <sin>, <cos>, <tan>, <sec>, <csc>, <cot>, <sinh>, "
    "<cosh>, <tanh>, <sech>, <csch>, <coth>, <arcsin>, <arccos>, <arctan>, "
    "<arcsec>, <arccsc>, <arccot>, <arcsinh>, <arccosh>, <arctanh>, "
    "<arcsech>, <arccsch>, <arccoth>, <true>, <false>, <notanumber>, <pi>, "
    "<infinity>, <exponentiale>, <semantics>, <annotation>, and "
    "<annotation-xml>. (References: L2V2 Section 3.5.1; L2V3 Section 3.4.1; "
    "L2V4 Section 3.4.1)"
  },

  //10203
  {
    DisallowedMathMLEncodingUse,
    "Disallowed use of MathML 'encoding' attribute",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "'encoding' is only permitted on <csymbol>. No other MathML elements may "
    "have an 'encoding' attribute. (References: L2V2 Section 3.5.1; L2V3 "
    "Section 3.4.1; L2V4 Section 3.4.1)."
  },

  //10204
  { /**
      * This is a rule that was not implemented in libSBML-2.3.4
      Category_* To be strictly in line with sbml specifications it should
      * only allow semantics on csymbol until l2v3 rel 2
      * BUT since no one ever applied it anyway I havent implemented
      * this as it would require a seperate rule that then
      * reverted to this id
      */
    DisallowedDefinitionURLUse,
    "Disallowed use of MathML 'definitionURL' attribute",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "'definitionURL' is only permitted on <csymbol>, <semantics> or <bvar>. "
    "No other MathML elements may have a 'definitionURL' attribute. "
    "(References: L2V2 Section 3.5.1; L2V3 Section 3.4.1; L2V4 Section 3.4.1)."
  },

  //10205
  {
    BadCsymbolDefinitionURLValue,
    "Invalid <csymbol> 'definitionURL' attribute value",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "In SBML Level 2, the only values permitted for 'definitionURL' on "
    "a <csymbol> element are \"http://www.sbml.org/sbml/symbols/time\" "
    "and \"http://www.sbml.org/sbml/symbols/delay\". (References: L2V2 "
    "Section 3.5.5; L2V3 Section 3.4.6; L2V4 Section 3.4.6)." 
  },

  //10206
  {
    DisallowedMathTypeAttributeUse,
    "Disallowed use of MathML 'type' attribute",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "'type' is only permitted on the <cn> construct. No other MathML elements "
    "may have a 'type' attribute. (References: L2V2 Section 3.5.1; L2V3 "
    "Section 3.4.1; L2V4 Section 3.4.1.)." 
  },

  //10207
  {
    DisallowedMathTypeAttributeValue,
    "Disallowed MathML 'type' attribute value",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The only permitted values for the 'type' attribute on MathML <cn> "
    "elements are 'e-notation', 'real', 'integer', and 'rational'. "
    "(References: L2V2 Section 3.5.2; L2V3 Section 3.4.2; L2V4 Section 3.4.2)"
  },

  //10208
  {
    LambdaOnlyAllowedInFunctionDef,
    "Use of <lambda> not permitted outside of a <functionDefinition>",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "MathML 'lambda' elements are only permitted as the first element inside "
    "the 'math' element of a <functionDefinition> or as the first element "
    "of a semantics element immediately inside inside the math element "
    "of a <functionDefinition>; they may not be used "
    "elsewhere in an SBML model. (References: L2V2 Sections 3.5.1 and 4.3; L2V3 "
    "Sections 3.4.1 and 4.3.2; L2V4 Sections 3.4.1 and 4.3.2.)"
  },

  //10209
  {
    BooleanOpsNeedBooleanArgs,
    "Non-boolean argument given to boolean operator",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The arguments of the MathML logical operators 'and', 'or', 'xor', and "
    "'not' must have boolean values. (References: L2V2 Section 3.5.8; "
    "L2V3 Section 3.4.9; L2V4 Section 3.4.9)"
  },

  //10210
  {
    NumericOpsNeedNumericArgs,
    "Non-numerical argument given to numerical operator",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The arguments to the following MathML constructs must have a numeric "
    "type: 'plus', 'minus', 'times', 'divide', 'power', 'root', 'abs', "
    "'exp', 'ln', 'log', 'floor', 'ceiling', 'factorial', 'sin', 'cos', "
    "'tan', 'sec', 'csc', 'cot', 'sinh', 'cosh', 'tanh', 'sech', 'csch', "
    "'coth', 'arcsin', 'arccos', 'arctan', 'arcsec', 'arccsc', 'arccot', "
    "'arcsinh', 'arccosh', 'arctanh', 'arcsech', 'arccsch', 'arccoth'. "
    "(References: L2V2 Section 3.5.8; L2V3 Section 3.4.9; L2V4 Section 3.4.9)"
  },

  //10211
  {
    ArgsToEqNeedSameType,
    "Arguments to <eq> or <neq> have inconsistent data types",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The values of all arguments to 'eq' and 'neq' operators must have the "
    "same type (either all boolean or all numeric). (References: L2V2 "
    "Section 3.5.8.; L2V3 Section 3.4.9; L2V4 Section 3.4.9)"
  },

  //10212
  {
    PiecewiseNeedsConsistentTypes,
    "<piecewise> terms have inconsistent data types",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The types of values within 'piecewise' operators must all be "
    "consistent: the set of expressions that make up the first arguments of "
    "the 'piece' and 'otherwise' operators within the same 'piecewise' "
    "operator should all return values of the same type. (References: L2V2 "
    "Section 3.5.8; L2V3 Section 3.4.9; L2V4 Section 3.4.9)"
  },

  //10213
  {
    PieceNeedsBoolean,
    "Second argument of <piece> must yield a boolean value",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The second argument of a MathML 'piece' operator must have a boolean "
    "value. (References: L2V2 Section 3.5.8; L2V3 Section 3.4.9; L2V4 "
    "Section 3.4.9)"
  },

  //10214
  {
    ApplyCiMustBeUserFunction,
    "<ci> does not refer to a function definition",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Outside of a <functionDefinition>, if a 'ci' element is the first "
    "element within a MathML 'apply', then the 'ci''s value can only be "
    "chosen from the set of identifiers of <functionDefinition>s defined in "
    "the SBML model. (References: L2V2 Section 4.3.2; L2V3 Section 4.3.2; "
    "L2V4 Section 4.3.2)"
  },

  //10215
  {
    ApplyCiMustBeModelComponent,
    "<ci>'s value is not a component in this model",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Outside of a <functionDefinition>, if a 'ci' element is not the first "
    "element within a MathML 'apply', then the 'ci''s value can only be "
    "chosen from the set of identifiers of <species>, <compartment>, "
    "<parameter> or <reaction> objects defined in the SBML model (In L2V1, the "
    "'ci''s value can't be chosen from the identifiers of <reaction> objects). "
    "(References: L2V2 Section 3.5.4; L2V3 Section 3.4.3; L2V4 Section 3.4.3)"
  },

  //10216
  {
    KineticLawParametersAreLocalOnly,
    "Cannot use <kineticLaw> parameter outside local scope",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'id' value of a <parameter> defined within a <kineticLaw> can only "
    "be used in 'ci' elements within the MathML content of that same "
    "<kineticLaw>; the identifier is not visible to other parts of the "
    "model. (References: L2V2 Sections 3.4.1, 3.5.3 and 4.13.5; L2V3 Sections "
    "3.3.1, 3.4.3 and 4.13.5; L2V4 Sections 3.3.1, 3.4.3 and 4.13.5)"
  },

  //10217
  {
    MathResultMustBeNumeric,
    "Formula result is not a numerical value",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The MathML formulas in the following elements must yield numeric "
    "expressions: 'math' in <kineticLaw>, 'stoichiometryMath' in "
    "<speciesReference>, 'math' in <initialAssignment>, 'math' in "
    "<assignmentRule>, 'math' in <rateRule>, 'math' in <algebraicRule>, and "
    "'delay' in <event>, and 'math' in <eventAssignment>. "
    "(References: L2V2 Sections 4.10, 4.11, 4.12 and 4.13; L2V3 Sections "
    "4.10, 4.11, 4.12 and 4.13; L2V4 Sections 4.10, 4.11, 4.12 and 4.13)"
  },

  //10218
  {
    OpsNeedCorrectNumberOfArgs,
    "Incorrect number of arguments to operator",
    LIBSBML_CAT_MATHML_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A MathML operator must be supplied the number of arguments "
    "appropriate for that operator. (References: SBML L2V2 Section "
    "SBML 3.5.1; L2V3 Section 3.4.1; L2V4 Section 3.4.1)"
  },

  //10219
  {
    InvalidNoArgsPassedToFunctionDef,
    "Incorrect number of arguments to function",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "The number of arguments used in a call to a function defined by a "
    "<functionDefinition> must equal the number of arguments accepted by "
    "that function, or in other words, the number of <bvar> elements "
    "inside the <lambda> element of the function definition.  "
    "(References: L2V4 Section 4.3.4.)"
  },

  //10301
  {
    DuplicateComponentId,
    "Duplicate component identifier",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'id' field on every instance of the following type of "
    "object in a model must be unique: <model>, <functionDefinition>, "
    "<compartmentType>, <compartment>, <speciesType>, <species>, <reaction>, "
    "<speciesReference>, <modifierSpeciesReference>, <event>, and model-wide "
    "<parameter>s. Note that <unitDefinition> and parameters defined inside "
    "a reaction are treated separately. (References: L2V1 Section 3.5; L2V2 "
    "Section 3.4.1; L2V3 Section 3.3; L2V4 Section 3.3)"
  },

  //10302
  {
    DuplicateUnitDefinitionId,
    "Duplicate unit definition identifier",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'id' field of every <unitDefinition> must be unique "
    "across the set of all <unitDefinition>s in the entire model. "
    "(References: L2V1 Sections 3.5 and 4.4; L2V2 Sections 3.4.1 and 4.4; "
    "L2V3 Sections 3.3 and 4.4; L2V4 Sections 3.3 and 4.4)"
  },

  //10303
  {
    DuplicateLocalParameterId,
    "Duplicate local parameter identifier",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'id' field of each parameter defined locally within a "
    "<kineticLaw> must be unique across the set of all such parameter "
    "definitions in that <kineticLaw>. (References: L2V1 Sections 3.4.1 and "
    "4.13.9; L2V2 Sections 3.4.1 and 4.13.5; L2V3 Sections 3.3.1 and 4.13.5; "
    "L2V4 Sections 3.3.1 and 4.13.5)"
  },

  //10304
  {
    MultipleAssignmentOrRateRules,
    "Multiple rules for the same variable",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'variable' field in all <assignmentRule> and "
    "<rateRule> definitions must be unique across the set of all such rule "
    "definitions in a model. (References: L2V1 Section 4.8.4; L2V2 Section "
    "4.11.3; L2V3 Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10305
  {
    MultipleEventAssignmentsForId,
    "Multiple event assignments for the same variable",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "In each <event>, the value of the 'variable' field within every "
    "<eventAssignment> definition must be unique across the set of all "
    "<eventAssignment>s within that <event>. (References: L2V1 erratum 17; "
    "L2V2 Section 4.14.2; L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  //10306
  {
    EventAndAssignmentRuleForId,
    "'variable' value used in both event assignments and assignment rules",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An identifier used as the value of 'variable' in an <eventAssignment> "
    "cannot also appear as the value of 'variable' in an <assignmentRule>. "
    "(References: L2V1 Section 4.10.5; L2V2 Section 4.14; L2V3 Section 4.14.4; "
    "L2V4 section 4.14.4)"
  },

  //10307
  {
    DuplicateMetaId,
    "Duplicate 'metaid' identifier",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Every 'metaid' attribute value must be unique across the set of all "
    "'metaid' values in a model. (References: L2V2 Sections 3.3.1 and "
    "3.1.6; L2V3 Sections 3.1.6 and 3.2.1; L2V4 Sections 3.1.6 and 3.2.1;)"
  },

  //10308
  {
    InvalidSBOTermSyntax,
    "Invalid 'sboTerm' value syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of an 'sboTerm' attribute must have the data type 'SBOTerm', "
    "which is a string consisting of the characters 'S', 'B', 'O', ':' "
    "followed by exactly seven digits. (References: L2V2 Section 3.1.9; L2V3 "
    "Section 3.1.9; L2V4 Section 3.1.9)"
  },

  //10309
  {
    InvalidMetaidSyntax,
    "Invalid 'metaid' value syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The syntax of 'metaid' attribute values must conform to the syntax of the "
    "XML type 'ID'. (References: L2V2 Sections 3.3.1 and 3.1.6; L2V3 Sections "
    "3.2.1 and 3.1.6; L2V4 Sections 3.2.1 and 3.1.6)" 
  },

  //10310
  {
    InvalidIdSyntax,
    "Invalid identifier syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The syntax of 'id' attribute values must conform to the syntax of the SBML "
    "type 'SId'. (References: L2V2 Sections 3.1.7; L2V3 Section 3.1.7; L2V4 "
    "Section 3.1.7)" 
  },

  //10311
  {
    InvalidUnitIdSyntax,
    "Invalid unit identifier syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The syntax of unit identifiers (i.e., the values of the id attribute "
    "on UnitDefinition, the units attribute on Compartment, the units "
    "attribute on Parameter, and the substanceUnits attribute on Species) "
    "must conform to the syntax of the SBML type UnitSId. "
    "(References: L2V3 Section 3.1.8; L2V4 Section 3.1.8)"
  },

  //10401
  {
    MissingAnnotationNamespace,
    "Missing declaration of XML namespace for annotation",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Every top-level element within an annotation element must "
    "have a namespace declared. (References: L2V2 Section 3.3.3; "
    "L2V3 Section 3.2.4; L2V4 Section 3.2.4)" 
  },

  //10402
  {
    DuplicateAnnotationNamespaces,
    "Multiple annotations using same XML namespace",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "There cannot be more than one top-level element using a "
    "given namespace inside a given annotation element. "
    "(References: L2V2 Section 3.3.3; L2V3 Section 3.2.4; "
    "L2V4 Section 3.2.4)" 
  },

  //10403
  {
    SBMLNamespaceInAnnotation,
    "Invalid use of SBML XML namespace in annotation",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Top-level elements within an annotation element cannot use any SBML "
    "namespace, whether explicitly (by declaring the namespace to be one of "
    "the URIs \"http://www.sbml.org/sbml/level1\", "
    "\"http://www.sbml.org/sbml/level2\", "
    "\"http://www.sbml.org/sbml/level2/version2\", or "
    "\"http://www.sbml.org/sbml/level2/version3\", or "
    "\"http://www.sbml.org/sbml/level2/version4\", or implicitly (by failing "
    "to declare any namespace). (References: L2V2 Section 3.3.3; L2V3 "
    "Section 3.2.4; L2V4 Section 3.2.4)" 
  },

  //10501
  {
    InconsistentArgUnits,
    "Units of arguments to function call do not match function's definition",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The units of the expressions used as arguments to a function call are expected to "
    "match the units expected for the arguments of that function. "
    "(References: L2V2 Section 3.5; L2V3 Section 3.4; L2V4 Section 3.4) "
  },

  //10511
  {
    AssignRuleCompartmentMismatch,
    "Mismatched units in assignment rule for compartment",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <assignmentRule> refers to a <compartment>, "
    "the units of the rule's right-hand side are expected to be consistent with the "
    "units of that compartment's size. (References: L2V2 Section 4.11.3; "
    "L2V3 Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10512
  {
    AssignRuleSpeciesMismatch,
    "Mismatched units in assignment rule for species",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <assignmentRule> refers to a <species>, the "
    "units of the rule's right-hand side are expected to be consistent with the units "
    "of the species' quantity. (References: L2V2 Section 4.11.3; L2V3 "
    "Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10513
  {
    AssignRuleParameterMismatch,
    "Mismatched units in assignment rule for parameter",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <assignmentRule> refers to a <parameter>, the "
    "units of the rule's right-hand side are expected to be consistent with the units "
    "declared for that parameter. (References: L2V2 Section 4.11.3; L2V3 "
    "Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10521
  {
    InitAssignCompartmenMismatch,
    "Mismatched units in initial assignment to compartment",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <initialAssignment> refers to a "
    "<compartment>, the units of the <initialAssignment>'s <math> expression "
    "are expected to be consistent with the units of that compartment's size. "
    "(References: L2V2 Section 4.10.4; L2V3 Section 4.10; L2V4 Section 4.10)"
  },

  //10522
  {
    InitAssignSpeciesMismatch,
    "Mismatched units in initial assignment to species",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <initialAssignment> refers to a <species>, "
    "the units of the <initialAssignment>'s <math> expression are expected to be "
    "consistent with the units of that species' quantity. (References: L2V2 "
    "Section 4.10.4; L2V3 Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10523
  {
    InitAssignParameterMismatch,
    "Mismatched units in initial assignment to parameter",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <initialAssignment> refers to a <parameter>, "
    "the units of the <initialAssignment>'s <math> expression are expected to be "
    "consistent with the units declared for that parameter. (References: "
    "L2V2 Section 4.10.4; L2V3 Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //10531
  {
    RateRuleCompartmentMismatch,
    "Mismatched units in rate rule for compartment",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in a <rateRule> definition refers to a "
    "<compartment>, the units of the rule's right-hand side are expected to be of the "
    "form _x per time_, where _x_ is either the 'units' in that "
    "<compartment> definition, or (in the absence of explicit units declared "
    "for the compartment size) the default units for that compartment, and "
    "_time_ refers to the units of time for the model. (References: L2V2 "
    "Section 4.11.4; L2V3 Section 4.11.4; L2V4 Section 4.11.4)"
  },

  //10532
  {
    RateRuleSpeciesMismatch,
    "Mismatched units in rate rule for species",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in a <rateRule> definition refers to a <species>, "
    "the units of the rule's right-hand side are expected to be of the form _x per "
    "time_, where _x_ is the units of that species' quantity, and _time_ "
    "refers to the units of time for the model. (References: L2V2 Section "
    "4.11.4; L2V3 Section 4.11.4; L2V4 Section 4.11.4)"
  },

  //10533
  {
    RateRuleParameterMismatch,
    "Mismatched units in rate rule for parameter",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in a <rateRule> definition refers to a <parameter>, "
    "the units of the rule's right-hand side are expected to be of the form _x per "
    "time_, where _x_ is the 'units' in that <parameter> definition, and "
    "_time_ refers to the units of time for the model. (References: L2V2 "
    "Section 4.11.4; L2V3 Section 4.11.4; L2V4 Section 4.11.4)"
  },

  //10541
  {
    KineticLawNotSubstancePerTime,
    "Kinetic law units are not 'substance'/'time'",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The units of the 'math' formula in a <kineticLaw> definition are expected to be "
    "the equivalent of _substance per time_. (References: L2V2 Section "
    "4.13.5; L2V3 Section 4.13.5; L2V4 Section 4.13.5)"
  },

  //10551
  {
    DelayUnitsNotTime,
    "Units of delay are not units of time",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When a value for <delay> is given in a <event> definition, the units of "
    "the delay formula are expected to correspond to either the value of 'timeUnits' in "
    "the <event> or (if no 'timeUnits' are given), the model's default units "
    "of time. (References: L2V2 Section 4.14; L2V3 Section 4.14.3; L2V4 Section 4.14.3)"
  },

  //10561
  {
    EventAssignCompartmentMismatch,
    "Mismatched units in event assignment for compartment",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <eventAssignment> refers to a <compartment>, "
    "the units of the <eventAssignment>'s <math> expression are expected to be consistent "
    "with the units of that compartment's size. (References: L2V2 Section "
    "4.14.2; L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  //10562
  {
    EventAssignSpeciesMismatch,
    "Mismatched units in event assignment for species",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <eventAssignment> refers to a <species>, the "
    "units of the <eventAssignment>'s <math> expression are expected to be consistent "
    "with the units of the species' quantity. (References: L2V2 Section "
    "4.14.2; L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  //10563
  {
    EventAssignParameterMismatch,
    "Mismatched units in event assignment for parameter",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "When the 'variable' in an <eventAssignment> refers to a <parameter>, the "
    "units of the <eventAssignment>'s <math> expression are expected to be consistent "
    "with the units declared for that parameter. (References: L2V2 Section "
    "4.14.2; L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  //10601
  {
    OverdeterminedSystem,
    "Model is overdetermined",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The system of equations created from an SBML model must not be "
    "overdetermined. (References: L2V2 Section 4.11.5; L2V3 Section 4.11.5; "
    "L2V4 Section 4.11.5)"
  },

  //10701
  {
    InvalidModelSBOTerm,
    "Invalid 'sboTerm' value for model",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <model> is expected to "
    "be an SBO identifier (http://www.biomodels.net/SBO/).  In SBML "
    "Level 2 prior to Version 4 the value is expected to be a term "
    "derived from SBO:0000004, \"modeling framework\"; in Version 4 "
    "and above it is expected to be a term derived from SBO:0000231 "
    "\"interaction\" (References: L2V2 Section 4.2.1; L2V3 "
    "Sections 4.2.2 and 5; L2V4 Sections 4.2.2 and 5)"
  },

  //10702
  {
    InvalidFunctionDefSBOTerm,
    "Invalid 'sboTerm' value for function definition",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <functionDefinition> is expected to be "
    "an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
    "mathematical expression (i.e., terms derived from SBO:0000064, "
    "\"mathematical expression\"). "
    "(References: L2V2 Section 4.3.3; L2V3 Section 4.3.3; L2V4 Section 4.3.3)"
  },

  //10703
  {
    InvalidParameterSBOTerm,
    "Invalid 'sboTerm' value for parameter",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <parameter> is expected to be an "
    "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
    "quantitative parameter defined in SBO (i.e., terms derived from "
    "SBO:0000002, \"quantitative parameter\"). "
    "(References: L2V2 Section 4.9.5; L2V3 Section 4.9.5; L2V4 Section 4.9.5)"
  },

  //10704
  {
    InvalidInitAssignSBOTerm,
    "Invalid 'sboTerm' value for initial assignment",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on an <initialAssignment> is expected to "
    "be an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
    "mathematical expression (i.e., terms derived from SBO:0000064, "
    "\"mathematical expression\"). "
    "(References: L2V2 Section 4.10.3; L2V3 Section 4.10.3; L2V4 Section 4.10.3)"
  },

  //10705
  {
    InvalidRuleSBOTerm,
    "Invalid 'sboTerm' value for rule",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a rule is expected to be an SBO identifier "
    "(http://www.biomodels.net/SBO/) referring to a mathematical expression "
    "(i.e., terms derived from SBO:0000064, \"mathematical expression\"). "
    "Note: This applies to Algebraic Rules in addition to Rate and Assignment "
    "Rules. (References: L2V2 Section 4.11.1; L2V3 Section 4.11.1; L2V4 Section 4.11.1)"
  },

  //10706
  {
    InvalidConstraintSBOTerm,
    "Invalid 'sboTerm' value for constraint",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <constraint> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
    "expression (i.e., terms derived from SBO:0000064, \"mathematical "
    "expression\"). "
    "(References: L2V2 Section 4.12.3; L2V3 Section 4.12.3; L2V4 Section 4.12.3)"
  },

  //10707
  {
    InvalidReactionSBOTerm,
    "Invalid 'sboTerm' value for reaction",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <reaction> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to an interaction defined "
    "in SBO (i.e., terms derived from SBO:0000231, \"interaction\"). "
    "(References: L2V2 Section 4.13.1; L2V3 Section 4.13.1; L2V4 Section 4.13.1)"
  },

  //10708
  {
    InvalidSpeciesReferenceSBOTerm,
    "Invalid 'sboTerm' value for species reference",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <speciesReference> "
    "or <modifierSpeciesReference> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to a participant "
    "role. The appropriate term depends on whether the object is a reactant, "
    "product or modifier. If a reactant, then it should be a term in the "
    "SBO:0000010, \"reactant\" hierarchy; if a product, then it should be a "
    "term in the SBO:0000011, \"product\" hierarchy; and if a modifier, then "
    "it should be a term in the SBO:0000019, \"modifier\" hierarchy. "
    "(References: L2V2 Section 4.13.2; L2V3 Sections 4.13.1 and 5; L2V4 "
    "Sections 4.13.1 and 5)"
  },

  //10709
  {
    InvalidKineticLawSBOTerm,
    "Invalid 'sboTerm' value for kinetic law",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <kineticLaw> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring rate law defined "
    "in SBO (i.e., terms derived from SBO:0000001, \"rate law\"). "
    "(References: L2V2 Section 4.13.5; L2V3 Section 4.13.1; L2V4 Section 4.13.1)"
  },

  //10710
  {
    InvalidEventSBOTerm,
    "Invalid 'sboTerm' value for event",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on an <event> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to an interaction "
    "defined in SBO (i.e., terms derived from SBO:0000231, \"interaction\"). "
    "(References: L2V2 Section 4.14.1; L2V3 Section 4.14.1; L2V4 Section 4.14.1)"
  },

  //10711
  {
    InvalidEventAssignmentSBOTerm,
    "Invalid 'sboTerm' value for event assignment",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on an <eventAssignment> is expected to be an "
    "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
    "mathematical expression (i.e., terms derived from SBO:0000064, "
    "\"mathematical expression\"). "
    "(References: L2V2 Section 4.14.2; L2V3 Section 4.14.1; L2V4 Section 4.14.1)"
  },

  //10712
  {
    InvalidCompartmentSBOTerm,
    "Invalid 'sboTerm' value for compartment",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <compartment> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/). In SBML Level 2 prior "
    "to Version 4 it is expected to refer to a participant "
    "physical type (i.e., terms derived from SBO:0000236, \"participant "
    "physical type\"); in Versions 4 and above it is expected to refer "
    "to a material entity (i.e., terms derived from SBO:0000240, "
    "\"material entity\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10713
  {
    InvalidSpeciesSBOTerm,
    "Invalid 'sboTerm' value for species",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <species> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/). In SBML Level 2 prior "
    "to Version 4 it is expected to refer to a participant "
    "physical type (i.e., terms derived from SBO:0000236, \"participant "
    "physical type\"); in Versions 4 and above it is expected to refer "
    "to a material entity (i.e., terms derived from SBO:0000240, "
    "\"material entity\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10714
  {
    InvalidCompartmentTypeSBOTerm,
    "Invalid 'sboTerm' value for compartment type",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <compartmentType> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/). In SBML Level 2 prior "
    "to Version 4 it is expected to refer to a participant "
    "physical type (i.e., terms derived from SBO:0000236, \"participant "
    "physical type\"); in Versions 4 and above it is expected to refer "
    "to a material entity (i.e., terms derived from SBO:0000240, "
    "\"material entity\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10715
  {
    InvalidSpeciesTypeSBOTerm,
    "Invalid 'sboTerm' value for species type",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <speciesType> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/). In SBML Level 2 prior "
    "to Version 4 it is expected to refer to a participant "
    "physical type (i.e., terms derived from SBO:0000236, \"participant "
    "physical type\"); in Versions 4 and above it is expected to refer "
    "to a material entity (i.e., terms derived from SBO:0000240, "
    "\"material entity\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10716
  {
    InvalidTriggerSBOTerm,
    "Invalid 'sboTerm' value for event trigger",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <trigger> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
    "expression (i.e., terms derived from SBO:0000064, \"mathematical "
    "expression\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10717
  {
    InvalidDelaySBOTerm,
    "Invalid 'sboTerm' value for event delay",
    LIBSBML_CAT_SBO_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_WARNING,
    "The value of the 'sboTerm' attribute on a <delay> is expected to be an SBO "
    "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
    "expression (i.e., terms derived from SBO:0000064, \"mathematical "
    "expression\"). (References: L2V3 Section 5; L2V4 Section 5)"
  },

  //10801
  {
    NotesNotInXHTMLNamespace,
    "Notes not placed in XHTML namespace",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the <notes> element must be explicitly placed in the "
    "XHTML XML namespace. (References: L2V3 Section 3.2.3; L2V4 Section 3.2.3)" 
  },

  //10802
  {
    NotesContainsXMLDecl,
    "XML declarations not permitted in notes",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the <notes> element must not contain an XML declaration "
    "(i.e., a string of the form \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\" "
    "or similar). (References: L2V2 Section 3.3.2; L2V3 Section 3.2.3; L2V4 "
    "Section 3.2.3)"
  },

  //10803
  {
    NotesContainsDOCTYPE,
    "XML DOCTYPE not permitted in notes",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the <notes> element must not contain an XML DOCTYPE "
    "declaration (i.e., a string beginning with the characters \"<!DOCTYPE\". "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3; L2V4 Section 3.2.3)"
  },

  //10804
  {
    InvalidNotesContent,
    "Invalid notes content",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The XHTML content inside a <notes> element can only take one of the "
    "following general forms: (1) a complete XHTML document beginning with "
    "the element <html> and ending with </html>; (2) the \"body\" portion of "
    "a document beginning with the element <body> and ending with </body>; or "
    "(3) XHTML content that is permitted within a <body> ... </body> elements. "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3; L2V4 Section 3.2.3)"
  },

  //20101
  {
    InvalidNamespaceOnSBML,
    "Invalid XML namespace for SBML container",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The <sbml> container element must declare the XML Namespace for SBML, "
    "and this declaration must be consistent with the values of the 'level' "
    "and 'version' attributes on the <sbml> element. (References: L2V2 "
    "Section 4.1; L2V3 Section 4.1; L2V4 Section 4.1)" 
  },

  //20102
  {
    MissingOrInconsistentLevel,
    "Missing or inconsistent value for 'level' attribute",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The <sbml> container element must declare the SBML Level using the "
    "attribute 'level', and this declaration must be consistent with the XML "
    "Namespace declared for the <sbml> element. (References: L2V2 Section "
    "4.1; L2V3 Section 4.1; L2V4 Section 4.1)" 
  },

  //20103
  {
    MissingOrInconsistentVersion,
    "Missing or inconsistent value for 'version' attribute",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The <sbml> container element must declare the SBML Version using the "
    "attribute 'version', and this declaration must be consistent with the "
    "XML Namespace declared for the <sbml> element. (References: L2V2 "
    "Section 4.1; L2V3 Section 4.1; L2V4 Section 4.1)" 
  },

  //20104   FIXME
  {
    AnnotationNotesNotAllowedLevel1,
    "Annotation on <sbml> not permitted in SBML Level 1",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <sbml> container element cannot contain <notes> or <annotations> in "
    "an SBML Level 1 document."
  },

  //20201
  {
    MissingModel,
    "Missing model",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An SBML document must contain a <model> definition. (References: L2V1, "
    "L2V2 and L2V3 Section 4.1; L2V4 Section 4.1)" 
  },

  //20202
  {
    IncorrectOrderInModel,
    "Incorrect ordering of components in model definition",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The order of subelements within a <model> must be the following (where "
    "any one may be optional, but the ordering must be maintained): "
    "<listOfFunctionDefinitions>, <listOfUnitDefinitions>, "
    "<listOfCompartmentTypes>, <listOfSpeciesTypes>, <listOfCompartments>, "
    "<listOfSpecies>, <listOfParameters>, <listOfInitialAssignments>, "
    "<listOfRules>, <listOfConstraints>, <listOfReactions> "
    "and <listOfEvents>. (References: L2V2 Section 4.2; L2V3 Section 4.2; "
    "L2V4 Section 4.2)" 
  },

  //20203
  {
    EmptyListElement,
    "A given listOf___, if present, cannot be empty",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The <listOf___> containers in a <model> are optional, but if present, "
    "the lists cannot be empty. Specifically, if any of the following are "
    "present in a <model>, they must not be empty: "
    "<listOfFunctionDefinitions>, <listOfUnitDefinitions>, "
    "<listOfCompartmentTypes>, <listOfSpeciesTypes>, <listOfCompartments>,  "
    "<listOfSpecies>, <listOfParameters>, <listOfInitialAssignments>, "
    "<listOfRules>, <listOfConstraints>, <listOfReactions> and "
    "<listOfEvents>. (References: This is a requirement stemming from the "
    "XML Schema used for SBML; L2V3 Section 4.2; L2V4 Section 4.2)" 
  },

  //20204
  {
    NeedCompartmentIfHaveSpecies,
    "Missing compartment in species definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If a model defines any species, then the model must also define at "
    "least one compartment. This is an implication of the fact that the "
    "'compartment' attribute on the <species> element is not optional. "
    "(References: L2V1 Section 4.5; L2V2 Section 4.8.3; L2V3 Section "
    "4.8.3; L2V4 Section 4.8.3)"
  },

  //20301
  {
    FunctionDefMathNotLambda,
    "Invalid expression in function definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The top-level element within <math> in a <functionDefinition> is "
    "restricted. (References: L2V1 Section 4.3.2; L2V2 Section 4.3.2; "
    "L2V3 Section 4.3.2; L2V4 Section 4.3.2)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20302
  {
    InvalidApplyCiInLambda,
    "Invalid forward reference in <apply><ci>...</ci></apply> value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    "Inside the 'lambda' of a <functionDefinition>, if a 'ci' element is the "
    "first element within a MathML 'apply', then the 'ci''s value can only "
    "be chosen from the set of identifiers of other SBML "
    "<functionDefinition>s defined prior to that point in the SBML model. In "
    "other words, forward references to user-defined functions are not "
    "permitted. (References: L2V2 Section 3.5.3; L2V3 Section 3.4.3; L2V4 "
    "Section 3.4.3)"
  },

  //20303
  {
    RecursiveFunctionDefinition,
    "Recursive function definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Inside the <lambda> of a <functionDefinition>, the identifier of that "
    "<functionDefinition> cannot appear as the value of a <ci> element. SBML "
    "functions are not permitted to be recursive. (References: L2V2 Sections "
    "3.5.3 and 4.3.2; L2V3 Sections 3.4.3 and 4.3.2; L2V4 Sections 3.4.3 and "
    "4.3.2)"
  },

  //20304
  {
    InvalidCiInLambda,
    "Unknown <ci> reference in <lambda>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Inside the 'lambda' of a <functionDefinition>, if a 'ci' element is not "
    "the first element within a MathML 'apply', then the 'ci''s value can "
    "only be the value of a 'bvar' element declared in that 'lambda'. In "
    "other words, all model entities referenced inside a function definition "
    "must be passed arguments to that function. (References: L2V2 Sections "
    "3.5.3 and 4.3.2; L2V3 Sections 3.4.3 and 4.3.2; L2V4 Sections 3.4.3 and "
    "4.3.2)"
  },

  //20305
  {
    InvalidFunctionDefReturnType,
    "Function return type must be either numerical or boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value type returned by a <functionDefinition>'s <lambda> must be "
    "either boolean or numeric. (References: L2V2 Section 3.5.8; L2V3 "
    "Section 3.4.9; L2V4 Section 3.4.9)"
  },

  //20401
  {
    InvalidUnitDefId,
    "Invalid 'id' value for unit definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'id' attribute in a <unitDefinition> must be of "
    "type 'UnitSId' and not be identical to any unit predefined in SBML. "
    "(References: L2V1 erratum 14; L2V2 Section 4.4.2; L2V3 Section 4.4.2; "
    "L2V4 Section 4.4.2)"
    // actual unit are listed in constraint 
    // as these vary between levels/versions
  },

  //20402
  {
    InvalidSubstanceRedefinition,
    "Invalid redefinition of 'substance'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Redefinitions of the built-in unit 'substance' are restricted."
    "(References: L2V1 Section 4.4.3; L2V2 Section 4.4.3; L2V3 Section 4.4.3; "
    "L2V4 Section 4.4.3)"
     // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20403
  {
    InvalidLengthRedefinition,
    "Invalid redefinition of 'length'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Redefinitions of the built-in unit 'length' are restricted. (References: L2V2 "
    "Section 4.4.3; L2V3 Section 4.4.3; L2V4 Section 4.4.3)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20404
  {
    InvalidAreaRedefinition,
    "Invalid redefinition of 'area'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Redefinitions of the built-in unit 'area' are restricted. "
    "(References: L2V2 Section 4.4.3; L2V3 Section 4.4.3; L2V4 Section 4.4.3)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20405
  {
    InvalidTimeRedefinition,
    "Invalid redefinition of 'time'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Redefinitions of the built-in unit 'time' are restricted. (References: L2V1 "
    "Section 4.4.3; L2V2 Section 4.4.3; L2V3 Section 4.4.3; L2V4 Section 4.4.3)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20406
  {
    InvalidVolumeRedefinition,
    "Invalid redefinition of 'volume'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Redefinitions of the built-in unit 'volume' are restricted. (References: L2V1 "
    "Section 4.4.3; L2V2 Section 4.4.3; L2V3 Section 4.4.3; L2V4 Section 4.4.3)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20407
  {
    VolumeLitreDefExponentNotOne,
    "Must use 'exponent'=1 when defining 'volume' in terms of litres",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <unitDefinition> for 'volume' simplifies to a <unit> in which the "
    "'kind' attribute value is 'litre', then its 'exponent' attribute value "
    "must be '1'. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3; L2V3 "
    "Section 4.4.3.)"
  },

  //20408
  {
    VolumeMetreDefExponentNot3,
    "Must use 'exponent'=3 when defining 'volume' in terms of metres",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <unitDefinition> for 'volume' simplifies to a <unit> in which the "
    "'kind' attribute value is 'metre', then its 'exponent' attribute value "
    "must be '3'. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3; L2V3 "
    "Section 4.4.3.)"
  },

  //20409
  {
    EmptyListOfUnits,
    "Empty list of units not permitted",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The <listOfUnits> container in a <unitDefinition> cannot be empty. "
    "(References: L2V2 Section 4.4; L2V3 Section 4.4; L2V4 Section 4.4)" 
  },

  //20410
  {
    InvalidUnitKind,
    "Invalid value of 'kind' in unit definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'kind' attribute of a <unit> can only be one of the "
    "base units enumerated by 'UnitKind'; that is, the SBML unit "
    "system is not hierarchical and user-defined units cannot be defined "
    "using other user-defined units. (References: L2V2 Section 4.4.1; L2V3 "
    "Section 4.4.2; L2V4 Section 4.4.2)"
  },

  //20411
  {
    OffsetNoLongerValid,
    "'offset' not supported in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)"
  },

  //20412
  {
    CelsiusNoLongerValid,
    "'Celsius' not defined in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The predefined unit 'Celsius', previously available in SBML Level 1 and "
    "Level 2 Version 1, has been removed as of SBML Level 2 Version 2. "
    "(References: L2V2 Section 4.4.)"
  },

  //20501
  {
    ZeroDimensionalCompartmentSize,
    "Use of 'size' is invalid for a zero-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The size of a <compartment> must not be set if the compartment's "
    "'spatialDimensions' attribute has value '0'. (References: L2V1 Section "
    "4.5.3; L2V2 Section 4.7.4; L2V3 Section 4.7.5; L2V4 Section 4.7.5)"
  },

  //20502
  {
    ZeroDimensionalCompartmentUnits,
    "Use of 'units' is invalid for a zero-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If a <compartment> definition has a 'spatialDimensions' value of '0', "
    "then its 'units' attribute must not be set. If the compartment has no "
    "dimensions, then no units can be associated with a non-existent size. "
    "(References: L2V1 Section 4.5.4; L2V2 Section 4.7.5; L2V3 Section 4.7.5; "
    "L2V4 Section 4.7.5)"
  },

  //20503
  {
    ZeroDimensionalCompartmentConst,
    "Zero-dimensional compartments cannot be non-constant",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If a <compartment> definition has a 'spatialDimensions' value of '0', "
    "then its 'constant' attribute value must either default to or be set to "
    "'true'. If the compartment has no dimensions, then its size can never "
    "change. (References: L2V1 Section 4.5.5; L2V2 Section 4.7.4; L2V3 "
    "Section 4.7.6; L2V4 Section 4.7.6)"
  },

  //20504
  {
    UndefinedOutsideCompartment,
    "Undefined compartment used as 'outside' value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'outside' attribute value of a <compartment> must be the identifier of "
    "another <compartment> defined in the model. (References: L2V1 Section "
    "4.5.6; L2V2 Section 4.7.7; L2V3 Section 4.7.7; L2V4 Section 4.7.7)"
  },

  //20505
  {
    RecursiveCompartmentContainment,
    "Recursive nesting of compartments via 'outside'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <compartment> may not enclose itself through a chain of references "
    "involving the 'outside' field. This means that a compartment cannot "
    "have its own identifier as the value of 'outside', nor can it point to "
    "another compartment whose 'outside' field points directly or indirectly "
    "to the compartment. (References: L2V1 erratum 11; L2V2 Section 4.7.7 " 
    "L2V3 Section 4.7.7; L2V4 Section 4.7.7) "
  },

  //20506
  {
    ZeroDCompartmentContainment,
    "Invalid nesting of zero-dimensional compartments",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'outside' attribute value of a <compartment> cannot be a compartment "
    "whose 'spatialDimensions' value is '0', unless both compartments have "
    "'spatialDimensions'='0'. Simply put, a zero-dimensional compartment "
    "cannot enclose compartments that have anything other than zero "
    "dimensions themselves. (References: L2V2 Section 4.7.7; L2V3 Section "
    "4.7.7; L2V4 Section 4.7.7)"
  },

  //20507
  {
    Invalid1DCompartmentUnits,
    "Invalid value of 'units' for a one-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'units' attribute on a <compartment> having "
    "'spatialDimensions' of '1' is restricted. "
    "(References: L2V2 Section 4.7.5; L2V3 Section 4.7.5; L2V4 Section 4.7.5)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20508
  {
    Invalid2DCompartmentUnits,
    "Invalid value of 'units' for a two-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'units' attribute on a <compartment> having "
    "'spatialDimensions' of '2' is restricted. (References: L2V1 Section "
    "4.5.4; L2V2 Section 4.7.5; L2V3 Section 4.7.5; L2V4 Section 4.7.5)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20509
  {
    Invalid3DCompartmentUnits,
    "Invalid value of 'units' for a three-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of the 'units' attribute on a <compartment> having "
    "'spatialDimensions' of '3' is restricted. (References: L2V1 Section "
    "4.5.4; L2V2 Section 4.7.5; L2V3 Section 4.7.5; L2V4 Section 4.7.5)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20510
  {
    InvalidCompartmentTypeRef,
    "Invalid 'compartmentType' reference",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If the 'compartmentType' attribute is given a value in a <compartment> "
    "definition, it must contain the identifier of an existing "
    "<compartmentType>. (References: L2V2 Section 4.7.2; L2V3 Section 4.7.2; "
    "L2V4 Section 4.7.2)"
  },

  //20601
  {
    InvalidSpeciesCompartmentRef,
    "Invalid 'compartment' reference",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of 'compartment' in a <species> definition must be the "
    "identifier of an existing <compartment> defined in the model. "
    "(References: L2V1 Section 4.6.2; L2V2 Section 4.8.3; L2V3 Section 4.8.3; "
    "L2V4 Section 4.8.3)"
  },

  //20602
  {
    HasOnlySubsNoSpatialUnits,
    "No 'spatialSizeUnits' permitted if 'hasOnlySubstanceUnits'='true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <species> definition sets 'hasOnlySubstanceUnits' to 'true', then "
    "it must not have a value for 'spatialSizeUnits'. (References: L2V1 "
    "Section 4.6.4; L2V2 Section 4.8.5.)"
  },

  //20603
  {
    NoSpatialUnitsInZeroD,
    "No 'spatialSizeUnits' permitted if compartment is zero-dimensional",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "A <species> definition must not set 'spatialSizeUnits' if the "
    "<compartment> in which it is located has a 'spatialDimensions' value of "
    "'0'. (References: L2V1 Section 4.6.4; L2V2 Section 4.8.5.)"
  },

  //20604
  {
    NoConcentrationInZeroD,
    "No 'initialConcentration' permitted if compartment is zero-dimensional",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If a <species> located in a <compartment> whose 'spatialDimensions' is "
    "set to '0', then that <species> definition cannot set "
    "'initialConcentration'. (References: L2V1 Section 4.6.3; L2V2 Section "
    "4.8.4; L2V3 Section 4.8.4; L2V4 Section 4.8.4)"
  },

  //20605
  {
    SpatialUnitsInOneD,
    "Invalid value of 'spatialSizeUnits' for a one-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '1', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'length', 'metre', 'dimensionless', or "
    "the identifier of a <unitDefinition> derived from 'metre' (with an "
    "'exponent' value of '1') or 'dimensionless'. (References: L2V1 Section "
    "4.6.4; L2V2 Section 4.8.5.)"
  },

  //20606
  {
    SpatialUnitsInTwoD,
    "Invalid value of 'spatialSizeUnits' for a two-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '2', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'area', 'dimensionless', or the "
    "identifier of a <unitDefinition> derived from either 'metre' (with an "
    "'exponent' value of '2') or 'dimensionless'. (References: L2V1 Section "
    "4.6.4; L2V2 Section 4.8.5.)"
  },

  //20607
  {
    SpatialUnitsInThreeD,
    "Invalid value of 'spatialSizeUnits' for a three-dimensional compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '3', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'volume', 'litre', 'dimensionless', or "
    "the identifier of a <unitDefinition> derived from either 'litre', "
    "'metre' (with an 'exponent' value of '3') or 'dimensionless'. "
    "(References: L2V1 Section 4.6.4; L2V2 Section 4.8.5.)"
  },

  //20608
  {
    InvalidSpeciesSusbstanceUnits,
    "Invalid value of 'units'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of a <species>'s 'units' attribute is restricted. "
    "(References: L2V1 Section 4.6.4; L2V2 Section 4.8.5; L2V3 Section 4.8.5; "
    "L2V4 Section 4.8.5)"
    // actual restrictions are listed in constraint 
    // as these vary between levels/versions
  },

  //20609
  {
    BothAmountAndConcentrationSet,
    "Cannot set both 'initialConcentration' and 'initialAmount'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <species> cannot set values for both 'initialConcentration' and "
    "'initialAmount' because they are mutually exclusive. (References: L2V1 "
    "Section 4.6.3; L2V2 Section 4.8.4; L2V3 Section 4.8.4; L2V4 Section 4.8.4)"
  },

  //20610
  {
    NonBoundarySpeciesAssignedAndUsed,
    "Cannot use non-boundary species in both reactions and rules simultaneously",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <species>'s quantity cannot be determined simultaneously by both "
    "reactions and rules. More formally, if the identifier of a <species> "
    "definition having 'boundaryCondition'='false' and 'constant'='false' is "
    "referenced by a <speciesReference> anywhere in a model, then this "
    "identifier cannot also appear as the value of a 'variable' in an "
    "<assignmentRule> or a <rateRule>. (References: L2V1 Section 4.6.5; L2V2 "
    "Section 4.8.6; L2V3 Section 4.8.6; L2V4 Section 4.8.6)" 
  },

  //20611
  {
    NonConstantSpeciesUsed,
    "Cannot use non-boundary, constant species as reactant or product",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <species> having boundaryCondition='false' cannot appear as a "
    "reactant or product in any reaction if that Species also has "
    "constant='true'. (References: L2V1 Section 4.6.5; L2V2 Section "
    "4.8.6; L2V3 Section 4.8.6; L2V4 Section 4.8.6)"
  },

  //20612
  {
    InvalidSpeciesTypeRef,
    "Invalid 'speciesType' reference",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of 'speciesType' in a <species> definition must be the "
    "identifier of an existing <speciesType>. (References: L2V2 Section "
    "4.8.2; L2V3 Section 4.8.2; L2V4 Section 4.8.2)"
  },

  //20613
  {
    MultSpeciesSameTypeInCompartment,
    "Cannot have multiple species of the same type in the same compartment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "There cannot be more than one species of a given <speciesType> in the "
    "same compartment of a model. More formally, for any given compartment, "
    "there cannot be more than one <species> definition in which both of the "
    "following hold simultaneously: (i) the <species>' 'compartment' value "
    "is set to that compartment's identifier and (ii) the <species>' "
    "'speciesType' is set the same value as the 'speciesType' of another "
    "<species> that also sets its 'compartment' to that compartment "
    "identifier. (References: L2V2 Section 4.8.2; L2V3 Section 4.8.2; L2V4 "
    "Section 4.8.2)"
  },

  //20614
  {
    MissingSpeciesCompartment,
    "Missing 'compartment' value for species",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'compartment' attribute in a <species> is mandatory. A <species> "
    "definition in a model must include a value for this attribute. "
    "(References: L2V2 Section 4.8.3; L2V3 Section 4.8.3; L2V4 Section 4.8.3)"
  },

  //20615
  {
    SpatialSizeUnitsRemoved,
    "Attribute 'spatialSizeUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'spatialSizeUnits' attribute on <species>, previously available "
    "in SBML Level 2 versions prior to Version 3, has been removed as "
    "of SBML Level 2 Version 3. (References: L2V3 Section 4.8; L2V4 Section 4.8)"
  },

  //20701
  {
    InvalidParameterUnits,
    "Invalid value for 'units' in parameter definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'units' in a <Parameter> definition must be a value chosen from "
    "among the following: a predefined unit (e.g., 'substance', 'time', "
    "etc.), the identifier of a <UnitDefinition> in the model, or one of the "
    "base units in SBML (e.g., 'litre', 'mole', 'metre', etc.) (References: "
    "L2V1 Section 4.7.3; L2V2 Section 4.9.3; L2V3 Section 4.9.3; L2V4 "
    "Section 4.9.3)"
  },

  //20801
  {
    InvalidInitAssignSymbol,
    "Invalid 'symbol' reference in initial assignment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of 'symbol' in an <initialAssignment> definition must be the "
    "identifier of an existing <compartment>, <species>, or <parameter> "
    "defined in the model. (References: L2V2 Section 4.10; L2V3 Section 4.10; "
    "L2V4 Section 4.10)"
  },

  //20802
  {
    MultipleInitAssignments,
    "Multiple initial assignments for the same 'symbol' value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A given identifier cannot appear as the value of more than one 'symbol' "
    "field across the set of <initialAssignment>s in a model. (References: "
    "L2V2 Section 4.10; L2V3 Section 4.10; L2V4 Section 4.10)"
  },

  //20803
  {
    InitAssignmentAndRuleForSameId,
    "Cannot set a value with both initial assignments and assignment rules simultaneously",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of a 'symbol' field in any <initialAssignment> definition "
    "cannot also appear as the value of a 'variable' field in an "
    "<assignmentRule>. (References: L2V2 Section 4.10.4; L2V3 Section 4.10; "
    "L2V4 Section 4.10)"
  },

  //20901
  {
    InvalidAssignRuleVariable,
    "Invalid 'variable' reference in assignment rule",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of an <assignmentRule>'s 'variable' must be the identifier of "
    "an existing <compartment>, <species>, or globally-defined <parameter>. "
    "(References: L2V1 Section 4.8.2; L2V2 Section 4.11.3; L2V3 Section "
    "4.11.3; L2V4 Section 4.11.3)"
  },

  //20902
  {
    InvalidRateRuleVariable,
    "Invalid 'variable' reference in rate rule",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of a <rateRule>'s 'variable' must be the identifier of an "
    "existing <compartment>, <species>, or globally-defined <parameter>. "
    "(References: L2V1 Section 4.8.3; L2V2 Section 4.11.4; L2V3 Section "
    "4.11.4; L2V4 Section 4.11.4)"
  },

  //20903
  {
    AssignmentToConstantEntity,
    "Cannot reassign a constant in an assignment rule",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Any <compartment>, <species> or <parameter> whose identifier is the "
    "value of a 'variable' attribute in an <assignmentRule>, must have a value "
    "of 'false' for 'constant'. (References: L2V1 Section 4.8.4; L2V2 "
    "Section 4.11.3; L2V3 Section 4.11.3; L2V4 Section 4.11.3)"
  },

  //20904
  {
    RateRuleForConstantEntity,
    "Cannot reassign a constant in a rate rule",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Any <compartment>, <species> or <parameter> whose identifier is the "
    "value of a 'variable' attribute in an <rateRule>, must have a value of "
    "'false' for 'constant'. (References: L2V1 Section 4.8.4; L2V2 Section "
    "4.11.4; L2V3 Section 4.11.4; L2V4 Section 4.11.4)"
  },

  //20905
  {
    RepeatedRule10304,
    "",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    ""
  },

  //20906
  {
    CircularRuleDependency,
    "Circular dependency involving rules and reactions",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "There must not be circular dependencies in the combined set of "
    "<initialAssignment>, <assignmentRule> and <kineticLaw> definitions in a "
    "model. Each of these constructs has the effect of assigning a value to "
    "an identifier (i.e. the identifier given in the field 'symbol' in "
    "<initialAssignment>, the field 'variable' in <assignmentRule>, and the "
    "field 'id' on the <kineticLaw>'s enclosing <reaction>). Each of these "
    "constructs computes the value using a mathematical formula. The formula "
    "for a given identifier cannot make reference to a second identifier "
    "whose own definition depends directly or indirectly on the first "
    "identifier. (References: L2V2 Section 4.11.5; L2V3 Section 4.11.5; "
    "L2V4 Section 4.11.5)" 
  },

  //21001
  {
    ConstraintMathNotBoolean,
    "Non-boolean math expression in constraint definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <constraint>'s <math> expression must evaluate to a value of type "
    "boolean. (References: L2V2 Section 4.12.1; L2V3 Section 4.12; L2V4 "
    "Section 4.12)"
  },

  //21002
  {
    IncorrectOrderInConstraint,
    "Incorrect order of elements in constraint definition",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The order of subelements within <constraint> must be the following: "
    "<math>, <message>. The <message> element is optional, but if present, "
    "must follow the <math> element. (References: L2V2 Section 4.12; L2V3 "
    "Section 4.12; L2V4 Section 4.12)"
  },

  //21003
  {
    ConstraintNotInXHTMLNamespace,
    "Constraint message is not in XHTML XML namespace",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the <message> element in a <constraint> must be "
    "explicitly placed in the XHTML XML namespace. (References: L2V3 "
    "Section 4.12.2; L2V4 Section 4.12.2)" 
  },

  //21004
  {
    ConstraintContainsXMLDecl,
    "XML declarations not permitted in constraint messages",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the <message> element must not contain an XML declaration "
    "(i.e., a string of the form \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\" "
    "or similar). (References: L2V3 Section 4.12.2; "
    "L2V4 Section 4.12.2)" 
  },

  //21005
  {
    ConstraintContainsDOCTYPE,
    "XML DOCTYPE not permitted in constraint messages",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The contents of the message element must not contain an XML DOCTYPE "
    "declaration (i.e., a string beginning with the characters \"<!DOCTYPE\". "
    "(References: L2V3 Section 4.12.2; L2V4 Section 4.12.2)" 
  },

  //21006
  {
    InvalidConstraintContent,
    "Invalid content for constraint message",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The XHTML content inside a <constraint>s message element can only take one "
    "of the following general forms: (1) a complete XHTML document beginning "
    "with the element <html> and ending with </html>; (2) the \"body\" portion "
    "of a document beginning with the element <body> and ending with </body>; "
    "or (3) XHTML content that is permitted within a <body> ... </body> "
    "elements. (References: L2V3 Section 4.12.2; L2V4 Section 4.12.2)" 
  },

  //21101
  {
    NoReactantsOrProducts,
    "Cannot have a reaction with neither reactants nor products",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <reaction> definition must contain at least one <speciesReference>, "
    "either in its <listOfReactants> or its <listOfProducts>. A reaction "
    "without any reactant or product species is not permitted, regardless of "
    "whether the reaction has any modifier species. (References: L2V2 "
    "Section 4.13.1; L2V3 Section 4.13.3; L2V4 Section 4.13.3)"
  },

  //21102
  {
    IncorrectOrderInReaction,
    "Incorrect ordering of components in reaction definition",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The order of subelements within <reaction> must be the following: "
    "<listOfReactants> (optional), <listOfProducts> (optional), "
    "<listOfModifiers> (optional), <kineticLaw>. (References: L2V2 Section "
    "4.13; L2V3 Section 4.13; L2V4 Section 4.13)"
  },

  //21103
  {
    EmptyListInReaction,
    "Reaction components, if present, cannot be empty",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The following containers are all optional in a <reaction>, but if any "
    "is present, it must not be empty: <listOfReactants>, <listOfProducts>, "
    "<listOfModifiers>, <kineticLaw>. (References: L2V2 Section 4.13; L2V3 "
    "Section 4.13; L2V4 Section 4.13)" 
  },

  //21104
  {
    InvalidReactantsProductsList,
    "Invalid element in list of reactants or products",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The list of reactants (<listOfReactants>) and list of products "
    "(<listOfProducts>) in a <reaction> can only contain <speciesReference> "
    "elements. (References: L2V1 Section 4.9; L2V2 Section 4.13; L2V3 Section "
    "4.13; L2V4 Section 4.13)"
  },

  //21105
  {
    InvalidModifiersList,
    "Invalid element in list of modifiers",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The list of modifiers (<listOfModifiers>) in a <reaction> can only "
    "contain <modifierSpeciesReference> elements. (References: L2V1 Section "
    "4.9; L2V2 Section 4.13; L2V3 Section 4.13; L2V4 Section 4.13)" 
  },

  //21111
  {
    InvalidSpeciesReference,
    "Invalid 'species' value in species reference",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of a <speciesReference> 'species' attribute must be the "
    "identifier of an existing <species> in the model. (References: L2V1 "
    "Section 4.9.5; L2V2 Section 4.13.2. L2V3 Section 4.13.3; L2V4 Section "
    "4.13.3)"
  },

  //21112
  {
    RepeatedRule20611,
    "",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    ""
  },

  //21113
  {
    BothStoichiometryAndMath,
    "Cannot use both 'stoichiometry' and <stoichiometryMath> simultaneously",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <speciesReference> must not have a value for both 'stoichiometry' and "
    "'stoichiometryMath'; they are mutually exclusive. (References: L2V1 "
    "Section 4.9.5; L2V2 Section 4.13.3; L2V3 Section 4.13.3; L2V4 Section "
    "4.13.3)"
  },

  //21121
  {
    UndeclaredSpeciesRef,
    "Undeclared species referenced in kinetic law formula",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "All species referenced in the <kineticLaw> formula of a given reaction "
    "must first be declared using <speciesReference> or "
    "<modifierSpeciesReference>. More formally, if a <species> identifier "
    "appears in a 'ci' element of a <reaction>'s <kineticLaw> formula, that "
    "same identifier must also appear in at least one <speciesReference> or "
    "<modifierSpeciesReference> in the <reaction> definition. (References: "
    "L2V2 Section 4.13.5; L2V3 Section 4.13.5; L2V4 Section 4.13.5)"
  },

  //21122
  {
    IncorrectOrderInKineticLaw,
    "Incorrect ordering of components in kinetic law definition",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The order of subelements within <kineticLaw> must be the following: "
    "<math>, <listOfParameters>. The <listOfParameters> is optional, but "
    "if present, must follow <math>. (References: L2V2 Section 4.13.9; "
    "L2V3 Section 4.13.5; L2V4 Section 4.13.5)" 
  },

  //21123
  {
    EmptyListInKineticLaw,
    "The list of parameters component, if present, cannot be empty",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "If present, the <listOfParameters> in a <kineticLaw> must not be an "
    "empty list. (References: L2V2 Section 4.13.5; L2V3 Section 4.13; L2V4 "
    "Section 4.13)" 
  },

  //21124
  {
    NonConstantLocalParameter,
    "Parameters local to a kinetic law must have 'constant'='true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'constant' attribute on a <parameter> local to a <kineticLaw> cannot "
    "have a value other than 'true'. The values of parameters local to "
    "<kineticLaw> definitions cannot be changed, and therefore they are "
    "always constant. (References: L2V2 Section 4.9.4; L2V3 Section 4.9.4; "
    "L2V4 Section 4.9.4)"
  },

  //21125
  {
    SubsUnitsNoLongerValid,
    "'substanceUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //21126
  {
    TimeUnitsNoLongerValid,
    "'timeUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //21131
  {
    UndeclaredSpeciesInStoichMath,
    "Undeclared species referenced in <stoichiometryMath> formula",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "All species referenced in the <stoichiometryMath> formula of a given "
    "reaction must first be declared using <speciesReference> or "
    "<modifierSpeciesReference>. More formally, if a <species> identifier "
    "appears in a <ci> element of a <reaction>'s <stoichiometryMath> "
    "formula, that same identifier must also appear in at least one "
    "<speciesReference> or <modifierSpeciesReference> in the <reaction> "
    "definition. (References: L2V2 Sections 4.13.2 and 4.13.4; L2V3 Sections "
    "4.13.2 and 4.13.4; L2V4 Sections 4.13.2 and 4.13.4)"
  },

  //21201
  {
    MissingTriggerInEvent,
    "Missing trigger in event definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_SCHEMA_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An <event> object must have a 'trigger'. (References: L2V1 Section "
    "4.10.2; L2V2 Section 4.14.1; L2V3 Section 4.14.2; L2V4 Section 4.14.2)"
  },

  //21202
  {
    TriggerMathNotBoolean,
    "Non-boolean math expression in trigger definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An <event>'s <trigger> expression must evaluate to a value of type "
    "boolean. (References: L2V1 Section 4.10.2; L2V2 Section 4.14.1; L2V3 "
    "Section 4.14.2; L2V4 Section 4.14.2)"
  },

  //21203
  {
    MissingEventAssignment,
    "Missing event assignment in event definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "An <event> object must have at least one <eventAssignment> object in "
    "its <listOfEventAssignments>. (References: L2V1 Section 4.10.5; L2V2 "
    "Section 4.14; L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //21204
  {
    TimeUnitsEvent,
    "Units of 'timeUnits' are not time units",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The value of an <event>'s 'timeUnits' attribute must be 'time', 'second', "
    "'dimensionless', or the identifier of a <unitDefinition> derived from "
    "either 'second' (with an 'exponent' value of '1') or 'dimensionless'. "
    "(References: L2V1 Section 4.10.4; L2V2 Section 4.14.)"
  },

  //21205
  {
    IncorrectOrderInEvent,
    "Incorrect ordering of components in event definition",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The order of subelements within <event> must be the following: "
    "<trigger>, <delay>, <listOfEventAssignments>. The <delay> element "
    "is optional, but if present, must follow <trigger>. (References: L2V2 "
    "Section 4.14; L2V3 Section 4.14; L2V4 Section 4.14)" 
  },

  //21206
  {
    ValuesFromTriggerTimeNeedDelay,
    "'useValuesFromTriggerTime'='false', but no delay defined in event",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "If an <event>'s 'useValuesFromTriggerTime' attribute has the "
    "value 'false', then the <event> must contain a <delay> "
    "element.  The implication of 'useValuesFromTriggerTime=false' "
    "is that there is a delay between the time of trigger and the "
    "time of value assignments performed by the <event>.  "
    "(References: L2V4 Section 4.14.)"
  },

  //21211
  {
    InvalidEventAssignmentVariable,
    "Invalid value for 'variable' in event assignment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The value of 'variable' in an <eventAssignment> can only be the "
    "identifier of a <compartment>, <species>, or model-wide <parameter> "
    "definition. (References: L2V1 Section 4.10.5; L2V2 Section 4.14.2; "
    "L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  //21212
  {
    EventAssignmentForConstantEntity,
    "Cannot assign to a constant component in an event assignment",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Any <compartment>, <species> or <parameter> definition whose identifier "
    "is used as the value of 'variable' in an <eventAssignment> must have a "
    "value of 'false' for its 'constant' attribute. (References: L2V1 Section "
    "4.10.5; L2V2 Section 4.14.2; L2V3 Section 4.14.4; L2V4 Section 4.14.4)"
  },

  // 80501
  {
    CompartmentShouldHaveSize,
    "It's best to define a size for every compartment in a model",
    LIBSBML_CAT_MODELING_PRACTICE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "As a principle of best modeling practice, the size of a <compartment> "
    "should be set to a value rather than be left undefined. Doing so "
    "improves the portability of models between different simulation and "
    "analysis systems, and helps make it easier to detect potential errors "
    "in models."
  },


  // 80701
  {
    ParameterShouldHaveUnits,
    "It's best to declare units for every parameter in a model",
    LIBSBML_CAT_MODELING_PRACTICE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "As a principle of best modeling practice, the units of a <parameter> "
    "should be declared rather than be left undefined. Doing so "
    "improves the ability of software to check the consistency of units "
    "and helps make it easier to detect potential errors in models."
  },


  // 81121
  {
    LocalParameterShadowsId,
    "Local parameters defined in a kinetic law shadow global parameters",
    LIBSBML_CAT_MODELING_PRACTICE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "In SBML's simple symbol system, there is no separation of symbols "
    "by class of object; consequently, inside the kinetic law mathematical "
    "formula, the value of a local parameter having the same identifier as "
    "a species or compartment or other global model entity will override "
    "the global value. Modelers may wish to take precautions to avoid this "
    "happening accidentally."
  },

  //90001
  {
    CannotConvertToL1V1,
    "Cannot convert to SBML Level 1 Version 1",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Use of SBML Level 1 Version 1 is not recommended and conversion is "
    "not supported by libSBML."
  },

  ////90501
  //{
  //  L2V4InconsistentArgUnits,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "The units of the expressions used as arguments to a function call should "
  //  "match the units expected for the arguments of that function. "
  //},

  ////90502
  //// This is an internal error that reverts to 90501
  //{
  //  L2V4InconsistentArgUnitsWarnings,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  ""
  //},

  ////90503
  //// This is an internal error that reverts to 90501
  //{
  //  L2V4InconsistentPowerUnitsWarnings,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  ""
  //},

  ////90504
  //// This is an internal error that reverts to 90501
  //{
  //  L2V4InconsistentExponUnitsWarnings,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  ""
  //},

  ////90511
  //{
  //  L2V4AssignRuleCompartmentMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <assignmentRule> refers to a <compartment>, "
  //  "the units of the rule's right-hand side should be consistent with the "
  //  "units of that compartment's size."
  //},

  ////90512
  //{
  //  L2V4AssignRuleSpeciesMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <assignmentRule> refers to a <species>, the "
  //  "units of the rule's right-hand side should be consistent with the units "
  //  "of the species' quantity."
  //},

  ////90513
  //{
  //  L2V4AssignRuleParameterMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <assignmentRule> refers to a <parameter>, the "
  //  "units of the rule's right-hand side should be consistent with the units "
  //  "declared for that parameter."
  //},

  ////90521
  //{
  //  L2V4InitAssignCompartmenMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <initialAssignment> refers to a "
  //  "<compartment>, the units of the <initialAssignment>'s <math> expression "
  //  "should be consistent with the units of that compartment's size."
  //},

  ////90522
  //{
  //  L2V4InitAssignSpeciesMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <initialAssignment> refers to a <species>, "
  //  "the units of the <initialAssignment>'s <math> expression should be "
  //  "consistent with the units of that species' quantity."
  //},

  ////90523
  //{
  //  L2V4InitAssignParameterMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <initialAssignment> refers to a <parameter>, "
  //  "the units of the <initialAssignment>'s <math> expression should be "
  //  "consistent with the units declared for that parameter."
  //},

  ////90531
  //{
  //  L2V4RateRuleCompartmentMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in a <rateRule> definition refers to a "
  //  "<compartment>, the units of the rule's right-hand side should be of the "
  //  "form _x per time_, where _x_ is either the 'units' in that "
  //  "<compartment> definition, or (in the absence of explicit units declared "
  //  "for the compartment size) the default units for that compartment, and "
  //  "_time_ refers to the units of time for the model."
  //},

  ////90532
  //{
  //  L2V4RateRuleSpeciesMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in a <rateRule> definition refers to a <species>, "
  //  "the units of the rule's right-hand side should be of the form _x per "
  //  "time_, where _x_ is the units of that species' quantity, and _time_ "
  //  "refers to the units of time for the model."
  //},

  ////90533
  //{
  //  L2V4RateRuleParameterMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in a <rateRule> definition refers to a <parameter>, "
  //  "the units of the rule's right-hand side should be of the form _x per "
  //  "time_, where _x_ is the 'units' in that <parameter> definition, and "
  //  "_time_ refers to the units of time for the model."
  //},

  ////90541
  //{
  //  L2V4KineticLawNotSubstancePerTime,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "The units of the 'math' formula in a <kineticLaw> definition should be "
  //  "the equivalent of _substance per time_."
  //},

  ////90551
  //{
  //  L2V4DelayUnitsNotTime,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When a value for <delay> is given in a <event> definition, the units of "
  //  "the delay formula should correspond to either the value of 'timeUnits' in "
  //  "the <event> or (if no 'timeUnits' are given), the model's default units "
  //  "of time."
  //},

  ////90561
  //{
  //  L2V4EventAssignCompartmentMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <eventAssignment> refers to a <compartment>, "
  //  "the units of the <eventAssignment>'s <math> expression should be consistent "
  //  "with the units of that compartment's size."
  //},

  ////90562
  //{
  //  L2V4EventAssignSpeciesMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <eventAssignment> refers to a <species>, the "
  //  "units of the <eventAssignment>'s <math> expression should be consistent "
  //  "with the units of the species' quantity."
  //},

  ////90563
  //{
  //  L2V4EventAssignParameterMismatch,
  //  LIBSBML_CAT_UNITS_CONSISTENCY,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_NOT_APPLICABLE,
  //  LIBSBML_SEV_GENERAL_WARNING,
  //  "When the 'variable' in an <eventAssignment> refers to a <parameter>, the "
  //  "units of the <eventAssignment>'s <math> expression should be consistent "
  //  "with the units declared for that parameter."
  //},


  //91001
  {
    NoEventsInL1,
    "SBML Level 1 does not support events",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A model with <event> definitions cannot be represented in SBML Level 1."
  },

  //91002
  {
    NoFunctionDefinitionsInL1,
    "SBML Level 1 does not support function definitions",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Conversion of a model with <functionDefinition>s to SBML Level 1 is not "
    "yet supported."
  },

  //91003
  {
    NoConstraintsInL1,
    "SBML Level 1 does not support constraints",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "Conversion of a model with <constraint>s to SBML Level 1 may result "
    "in loss of information."
  },

  //91004
  {
    NoInitialAssignmentsInL1,
    "SBML Level 1 does not support initial assignments",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Conversion of a model with <initialAssignment>s to SBML Level 1 is not "
    "yet supported."
  },

  //91005
  {
    NoSpeciesTypesInL1,
    "SBML Level 1 does not support species types",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<speciesType> definitions cannot be represented in SBML Level 1."
  },

  //91006
  {
    NoCompartmentTypeInL1,
    "SBML Level 1 does not support compartment types",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<compartmentType> definitions cannot be represented in SBML Level 1."
  },

  //91007
  {
    NoNon3DComparmentsInL1,
    "SBML Level 1 only supports three-dimensional compartments",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <compartment> with 'spatialDimensions' not equal to 3 cannot be "
    "represented in SBML Level 1."
  },

  //91008
  {
    NoFancyStoichiometryMathInL1,
    "SBML Level 1 does not support non-integer nor non-rational stoichiometry formulas",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <speciesReference> containing a non-integer or non-rational "
    "<stoichiometryMath> subelement cannot be represented in SBML Level 1."
  },

  //91009
  {
    NoNonIntegerStoichiometryInL1,
    "SBML Level 1 does not support non-integer 'stoichiometry' attribute values",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <speciesReference> containing a non-integer 'stoichiometry' attribute "
    "value cannot be represented in SBML Level 1."
  },

  //91010
  {
    NoUnitMultipliersOrOffsetsInL1,
    "SBML Level 1 does not support multipliers or offsets in unit definitions",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <unit> containing multipliers or offsets cannot be represented in "
    "SBML Level 1."
  },

  //91011
  {
    SpeciesCompartmentRequiredInL1,
    "In SBML Level 1, a value for 'compartment' is mandatory in species definitions",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "A <species> that does not identify its compartment cannot be "
    "represented in SBML Level 1."
  },

  //91012
  {
    NoSpeciesSpatialSizeUnitsInL1,
    "SBML Level 1 does not support species 'spatialSizeUnits' settings",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The information represented by the value of a 'spatialSizeUnit' "
    "attribute on a <species> definition cannot be represented in "
    "SBML Level 1."
  },

  //91013
  {
    NoSBOTermsInL1,
    "SBML Level 1 does not support the 'sboTerm' attribute",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "SBO terms cannot be represented directly in SBML Level 1."
  },

  //91014
  {
    StrictUnitsRequiredInL1,
    "SBML Level 1 requires strict unit consistency",
    LIBSBML_CAT_SBML_L1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all units "
    "be consistent.  This model contains units that produce "
    "inconsistencies and thus conversion to Level 1 "
    "would produce an invalid model."
  },

  //92001
  {
    NoConstraintsInL2v1,
    "SBML Level 2 Version 1 does not support constraints",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "Conversion of a model with <constraints> to SBML Level 2 Version 1 may "
    "result in loss of information."
  },

  //92002
  {
    NoInitialAssignmentsInL2v1,
    "SBML Level 2 Version 1 does not support initial assignments",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "Conversion of a model with <initialAssignments> to SBML Level 2 Version 1 "
    "is not yet supported."
  },

  //92003
  {
    NoSpeciesTypeInL2v1,
    "SBML Level 2 Version 1 does not support species types",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<speciesType> definitions cannot be represented in SBML Level 2 "
    "Version 1."
  },

  //92004
  {
    NoCompartmentTypeInL2v1,
    "SBML Level 2 Version 1 does not support compartment types",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<compartmentType> definitions cannot be represented in SBML Level 2 "
    "Version 1."
  },

  //92005
  {
    NoSBOTermsInL2v1,
    "SBML Level 2 Version 1 does not support the 'sboTerm' attribute",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "SBO terms cannot be represented directly in SBML Level 2 Version 1."
  },

  //92006
  {
    NoIdOnSpeciesReferenceInL2v1,
    "SBML Level 2 Version 1 does not support the 'id' attribute on species references",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "The 'id' attribute value of a <speciesReference> cannot be "
    "represented directly in SBML Level 2 Version 1."
  },

  //92007
  {
    NoDelayedEventAssignmentInL2v1,
    "Attribute 'useValuesFromTriggerTime' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "The implication of 'useValuesFromTriggerTime=false' "
    "is that there is a delay between the time of trigger and the "
    "time of value assignments performed by the <event>.  This interpretation is "
    "not supported by SBML Level 2 versions prior to Version 4. "
    "(References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //92008
  {
    StrictUnitsRequiredInL2v1,
    "SBML Level 2 Version 1 requires strict unit consistency",
    LIBSBML_CAT_SBML_L2V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all units "
    "be consistent.  This model contains units that produce "
    "inconsistencies and thus conversion to Level 2 Version 1 "
    "would produce an invalid model."
  },

  //93001
  {
    SBOTermNotUniversalInL2v2,
    "The 'sboTerm' attribute is invalid for this component in Level 2 Version 2",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "In SBML Level 2 Version 2, an 'sboTerm' attribute is only permitted on "
    "the following elements: <model>, <functionDefinition>, <parameter>, "
    "<initialAssignment>, <rule>, <constraint>, <reaction>, "
    "<speciesReference>, <kineticLaw>, <event> and <eventAssignment>."
  },

  //93002
  {
    NoUnitOffsetInL2v2,
    "The unit 'offset' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)"
  },

  //93003
  {
    NoKineticLawTimeUnitsInL2v2,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //93004
  {
    NoKineticLawSubstanceUnitsInL2v2,
    "The 'substanceUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //93005
  {
    NoDelayedEventAssignmentInL2v2,
    "Attribute 'useValuesFromTriggerTime' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "The implication of 'useValuesFromTriggerTime=false' "
    "is that there is a delay between the time of trigger and the "
    "time of value assignments performed by the <event>.  This interpretation is "
    "not supported by SBML Level 2 versions prior to Version 4. "
    "(References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //93006
  {
    ModelSBOBranchChangedBeyondL2v2,
    "The allowable 'sboTerm' values for model differ for this SBML Level+Version",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "In SBML Level 2 prior to Version 4 the value  of the 'sboTerm' "
    "attribute on a <model> is expected to be a term "
    "derived from SBO:0000004, \"modeling framework\"; in Version 4 "
    "and above it is expected to be a term derived from SBO:0000231 "
    "\"interaction\". Using the existing term will create an invalid "
    " L2V2 model. (References: L2V2 Section 4.2.1; L2V3 "
    "Sections 4.2.2 and 5; L2V4 Sections 4.2.2 and 5)"
  },

  //93007
  {
    StrictUnitsRequiredInL2v2,
    "SBML Level 2 Version 2 requires strict unit consistency",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all units "
    "be consistent.  This model contains units that produce "
    "inconsistencies and thus conversion to Level 2 Version 2 "
    "would produce an invalid model."
  },

  //93008
  {
    StrictSBORequiredInL2v2,
    "SBML Level 2 Version 2 requires strict sbo consistency",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all sboTerms "
    "be drawn from a given branch of SBO (http://www.biomodels.net/SBO/). "
    "This model contains sboTerms that produce "
    "inconsistencies and thus conversion to Level 2 Version 2 "
    "would produce an invalid model."
  },

  //93009
  {
    DuplicateAnnotationInvalidInL2v2,
    "Duplicate top level annotations invalid in L2V2",
    LIBSBML_CAT_SBML_L2V2_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML prior to Level 2 Version 1 annotations it was permissible "
    "to have multiple top level elements using the same namespace. "
    "This is no longer valid.  (References: L2V2 Section 3.3.3; "
    "L2V3 Section 3.2.4; L2V4 Section 3.2.4.)  Any duplicate top level "
    "elements will be placed inside a new top level element named "
    "\"duplicateTopLevelElements\" with the namespace "
    "\"http://www.sbml.org/libsbml/annotation\""
  },

  //94001
  {
    NoUnitOffsetInL2v3,
    "Attribute 'offset' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)"
  },

  //94002
  {
    NoKineticLawTimeUnitsInL2v3,
    "Attribute 'timeUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //94003
  {
    NoKineticLawSubstanceUnitsInL2v3,
    "Attribute 'substanceUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //94004
  {
    NoSpeciesSpatialSizeUnitsInL2v3,
    "Attribute 'spatialSizeUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'spatialSizeUnits' attribute on <species>, previously available "
    "in SBML Level 2 versions prior to Version 3, has been removed as "
    "of SBML Level 2 Version 3. (References: L2V3 Section 4.8; L2V4 Section 4.8)"
  },

  //94005
  {
    NoEventTimeUnitsInL2v3,
    "Attribute 'timeUnits' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <event>, previously available in SBML "
    "Level 2 versions prior to Version 3, has been removed as of SBML "
    "Level 2 Version 3. (References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //94006
  {
    NoDelayedEventAssignmentInL2v3,
    "Attribute 'useValuesFromTriggerTime' not supported in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "The implication of 'useValuesFromTriggerTime=false' "
    "is that there is a delay between the time of trigger and the "
    "time of value assignments performed by the <event>.  This interpretation is "
    "supported by SBML Level 2 versions prior to Version 4. "
    "(References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //94007
  {
    ModelSBOBranchChangedBeyondL2v3,
    "The allowable 'sboTerm' values for model differ for this SBML Level+Version",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    "In SBML Level 2 prior to Version 4 the value  of the 'sboTerm' "
    "attribute on a <model> is expected to be a term "
    "derived from SBO:0000004, \"modeling framework\"; in Version 4 "
    "and above it is expected to be a term derived from SBO:0000231 "
    "\"interaction\". Using the existing term will create an invalid "
    " L2V3 model. (References: L2V2 Section 4.2.1; L2V3 "
    "Sections 4.2.2 and 5; L2V4 Sections 4.2.2 and 5)"
  },

  //94008
  {
    StrictUnitsRequiredInL2v3,
    "SBML Level 2 Version 3 requires strict unit consistency",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all units "
    "be consistent.  This model contains units that produce "
    "inconsistencies and thus conversion to Level 2 Version 3 "
    "would produce an invalid model."
  },

  //94009
  {
    StrictSBORequiredInL2v3,
    "SBML Level 2 Version 3 requires strict sbo consistency",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    "SBML Level 2 Version 4 removed the requirement that all sboTerms "
    "be drawn from a given branch of SBO (http://www.biomodels.net/SBO/). "
    "This model contains sboTerms that produce "
    "inconsistencies and thus conversion to Level 2 Version 3 "
    "would produce an invalid model."
  },

  //94010
  {
    DuplicateAnnotationInvalidInL2v3,
    "Duplicate top level annotations invalid in L2V3",
    LIBSBML_CAT_SBML_L2V3_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML prior to Level 2 Version 1 annotations it was permissible "
    "to have multiple top level elements using the same namespace. "
    "This is no longer valid.  (References: L2V2 Section 3.3.3; "
    "L2V3 Section 3.2.4; L2V4 Section 3.2.4.)  Any duplicate top level "
    "elements will be placed inside a new top level element named "
    "\"duplicateTopLevelElements\" with the namespace "
    "\"http://www.sbml.org/libsbml/annotation\""
  },

  //95001
  {
    NoUnitOffsetInL2v4,
    "The unit 'offset' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)"
  },

  //95002
  {
    NoKineticLawTimeUnitsInL2v4,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //95003
  {
    NoKineticLawSubstanceUnitsInL2v4,
    "The 'substanceUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //95004
  {
    NoSpeciesSpatialSizeUnitsInL2v4,
    "The 'spatialSizeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'spatialSizeUnits' attribute on <species>, previously available "
    "in SBML Level 2 versions prior to Version 3, has been removed as "
    "of SBML Level 2 Version 3. (References: L2V3 Section 4.8; L2V4 Section 4.8)"
  },

  //95005
  {
    NoEventTimeUnitsInL2v4,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <event>, previously available in SBML "
    "Level 2 versions prior to Version 3, has been removed as of SBML "
    "Level 2 Version 3. (References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //95006
  {
    ModelSBOBranchChangedInL2v4,
    "The allowable 'sboTerm' values for model differ for this SBML Level+Version",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML Level 2 prior to Version 4 the value  of the 'sboTerm' "
    "attribute on a <model> is expected to be a term "
    "derived from SBO:0000004, \"modeling framework\"; in Version 4 "
    "and above it is expected to be a term derived from SBO:0000231 "
    "\"interaction\". Using the existing term will create an invalid "
    " L2V4 model. (References: L2V2 Section 4.2.1; L2V3 "
    "Sections 4.2.2 and 5; L2V4 Sections 4.2.2 and 5)"
  },

  //95007
  {
    DuplicateAnnotationInvalidInL2v4,
    "Duplicate top level annotations invalid in L2V4",
    LIBSBML_CAT_SBML_L2V4_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML prior to Level 2 Version 1 annotations it was permissible "
    "to have multiple top level elements using the same namespace. "
    "This is no longer valid.  (References: L2V2 Section 3.3.3; "
    "L2V3 Section 3.2.4; L2V4 Section 3.2.4.)  Any duplicate top level "
    "elements will be placed inside a new top level element named "
    "\"duplicateTopLevelElements\" with the namespace "
    "\"http://www.sbml.org/libsbml/annotation\""
  },

  //96001
  {
    NoSpeciesTypeInL3v1,
    "SBML Level 3 Version 1 does not support species types",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<speciesType> definitions cannot be represented in SBML Level 3 "
    "Version 1."
  },

  //96002
  {
    NoCompartmentTypeInL3v1,
    "SBML Level 3 Version 1 does not support compartment types",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "<compartmentType> definitions cannot be represented in SBML Level 3 "
    "Version 1."
  },

  //96003
  {
    NoUnitOffsetInL3v1,
    "The unit 'offset' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)"
  },

  //96004
  {
    NoKineticLawTimeUnitsInL3v1,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //96005
  {
    NoKineticLawSubstanceUnitsInL3v1,
    "The 'substanceUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 3, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)"
  },

  //96006
  {
    NoSpeciesSpatialSizeUnitsInL3v1,
    "The 'spatialSizeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'spatialSizeUnits' attribute on <species>, previously available "
    "in SBML Level 2 versions prior to Version 3, has been removed as "
    "of SBML Level 2 Version 3. (References: L2V3 Section 4.8; L2V4 Section 4.8)"
  },

  //96007
  {
    NoEventTimeUnitsInL3v1,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'timeUnits' attribute on <event>, previously available in SBML "
    "Level 2 versions prior to Version 3, has been removed as of SBML "
    "Level 2 Version 3. (References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //96008
  {
    ModelSBOBranchChangedInL3v1,
    "The allowable 'sboTerm' values for model differ for this SBML Level+Version",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML Level 2 prior to Version 4 the value  of the 'sboTerm' "
    "attribute on a <model> is expected to be a term "
    "derived from SBO:0000004, \"modeling framework\"; in Version 4 "
    "and above it is expected to be a term derived from SBO:0000231 "
    "\"interaction\". Using the existing term will create an invalid "
    " L2V4 model. (References: L2V2 Section 4.2.1; L2V3 "
    "Sections 4.2.2 and 5; L2V4 Sections 4.2.2 and 5)"
  },

  //96009
  {
    DuplicateAnnotationInvalidInL3v1,
    "Duplicate top level annotations invalid in L3V1",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In SBML prior to Level 2 Version 1 annotations it was permissible "
    "to have multiple top level elements using the same namespace. "
    "This is no longer valid.  (References: L2V2 Section 3.3.3; "
    "L2V3 Section 3.2.4; L2V4 Section 3.2.4.)  Any duplicate top level "
    "elements will be placed inside a new top level element named "
    "\"duplicateTopLevelElements\" with the namespace "
    "\"http://www.sbml.org/libsbml/annotation\""
  },

  //96010
  {
    NoCompartmentOutsideInL3v1,
    "The 'outside' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "The 'outside' attribute on <compartment>, previously available "
    "in SBML Level 2 has been removed as "
    "of SBML Level 3 Version 1."
  },

  //96011
  {
    NoStoichiometryMathInL3v1,
    "SBML Level 3 Version 1 does not support stoichiometryMath",
    LIBSBML_CAT_SBML_L3V1_COMPAT,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "Conversion of a model with <stoichiometryMath> to SBML Level 3 is not "
    "yet supported."
  },

  //99101
  {
    InvalidSBMLLevelVersion,
    "Invalid SBML Level and Version",
    LIBSBML_CAT_SBML,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The level and version specified for the document must be "
    "consistent with a valid published SBML specification.  These are "
    "Level 1, Versions 1 and 2 and Level 2, Versions 1, 2, 3 and 4."
  },

  //99106
  {
    InvalidRuleOrdering,
    "Invalid ordering of rules",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "The math field of an assignment rule structure can contain any "
    "identifier in a MathML ci element except for the following: (a) "
    "identifiers for which there exists a subsequent assignment rule, "
    "and (b) the identifier for which the rule is defined. (L2V1 Section "
    "4.8.4)."
  },

  //99127
  {
    SubsUnitsAllowedInKL,
    "Disallowed value for attribute 'substanceUnits'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "A KineticLaw's substanceUnits must be 'substance', 'item', 'mole', or "
    "the id of a UnitDefinition that defines a variant of 'item' or 'mole' "
    "(L2v1 Section 4.9.7)."
  },

  //99128
  {
    TimeUnitsAllowedInKL,
    "Disallowed value for attribute 'timeUnits'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "A KineticLaw's timeUnits must be 'time', 'second', or the id of a "
    "UnitDefnition that defines a variant of 'second' with exponent='1' "
    "(L2v1 Section 4.9.7)."
  },

  //99129
  {
    FormulaInLevel1KL,
    "Only predefined functions are permitted in SBML Level 1 formulas",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    "In a Level 1 model, only predefined functions are permitted "
    "within the formula inside the KineticLaw. (L1V2 Appendix C) "
  },

  //99206
  {
    TimeUnitsRemoved,
    "The 'timeUnits' attribute is invalid in this Level+Version of SBML",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_NOT_APPLICABLE,
    LIBSBML_SEV_ERROR,
    LIBSBML_SEV_ERROR,
    "The 'timeUnits' attribute on <event>, previously available in SBML "
    "Level 2 versions prior to Version 3, has been removed as of SBML "
    "Level 2 Version 3. (References: L2V3 Section 4.14; L2V4 Section 4.14)"
  },

  //99219
  {   
    BadMathML,   
    "Invalid MathML expression",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Invalid MathML. The body of an <apply> tag must begin with an operator."   
  },

  //99220
  {   
    FailedMathMLReadOfDouble,   
    "Failed to read floating-point number",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Failed to read a valid double value from MathML."   
  },

  //99221
  {   
    FailedMathMLReadOfInteger,   
    "Failed to read an integer",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Failed to read a valid integer value from MathML."   
  },

  //99222
  {   
    FailedMathMLReadOfExponential,   
    "Failed to read an exponential expression",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Failed to read a valid exponential value from MathML."   
  },

  //99223
  {   
    FailedMathMLReadOfRational,   
    "Failed to read a rational expression",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Failed to read a valid rational value from MathML."   
  },

  //99224
  {   
    BadMathMLNodeType,   
    "Invalid MathML element",
    LIBSBML_CAT_MATHML_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "Invalid MathML. The body of a <math> tag or the body of "
    "a <lambda> element must begin with one of "   
    "the following: <cn>, <ci>, <csymbol>, <apply>, <piecewise>, "
    "<true>, <false>, <notanumber>, <pi>, <infinity>, <exponentiale>, "
    "<semantics>. (References: L2V3 Appendix B)"
  },

  //99301
  {   
    NoTimeSymbolInFunctionDef,   
    "<csymbol> for 'time' used within the <math> of a function definition",
    LIBSBML_CAT_GENERAL_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_GENERAL_WARNING,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    "The csymbol 'time' should not be used within a the <math> element "
    "of a <FunctionDefinition>. (References: L2V3 Section 4.3.2; " 
    "L2V4 Section 4.3.2)"
  },

  //99502
  // This is an internal error that reverts to 10501
  {
    InconsistentArgUnitsWarnings,
    "",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    ""
  },

  //99503
  // This is an internal error that reverts to 10501
  {
    InconsistentPowerUnitsWarnings,
    "",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    ""
  },

  //99504
  // This is an internal error that reverts to 10501
  {
    InconsistentExponUnitsWarnings,
    "",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_GENERAL_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    ""
  },

  //99505
  {
    UndeclaredUnits,
    "Undeclared units",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    LIBSBML_SEV_WARNING,
    "In situations when a mathematical expression contains literal numbers "
    "or parameters whose units have not been declared, it is not possible "
    "to verify accurately the consistency of the units in the expression. "
  },

  //99701
  {   
    UnrecognisedSBOTerm,   
    "Unrecognized 'sboTerm' value",
    LIBSBML_CAT_SBO_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_WARNING,   
    LIBSBML_SEV_WARNING,   
    LIBSBML_SEV_WARNING,
    "The SBOTerm used is not recognised by libSBML and therefore the correct "
    "parentage cannot be checked. However, since libSBML is referring to a "
    "snapshot of the SBO tree the term may now exist."
  },

  //99701
  {   
    ObseleteSBOTerm,   
    "Obsolete 'sboTerm' value",
    LIBSBML_CAT_SBO_CONSISTENCY,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_WARNING,   
    LIBSBML_SEV_WARNING,   
    LIBSBML_SEV_WARNING,
    "The SBOTerm value used is considered obsolete."
  },

  //99901
  {   
    IncorrectCompartmentSpatialDimensions,   
    "in SBML Level 1, only three-dimensional compartments are permitted",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'spatialDimensions' attribute on <compartment> was not available "
    "in SBML Level 1. In order for the internal respresentation of a "
    "<compartment> to be correct, the value for the spatialDimensions "
    "member variable should be '3'."
  },

  //99902
  {   
    CompartmentTypeNotValidAttribute,   
    "Compartment types not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'compartmentType' attribute on <compartment> was not available "
    "in SBML Level 1 or SBML Level 2 Version 1. In order for the internal "
    "respresentation of a <compartment> to be correct, the value for the "
    "compartmentType member variable should be not be set."
  },

  //99903
  {   
    ConstantNotValidAttribute,   
    "Attribute 'constant' not supported on this component in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'constant' attribute on <compartment> and <parameter> was not "
    "available in SBML Level 1. In order for the internal respresentation "
    "of <compartment> and <parameter> to be correct, the value for the "
    "constant member variable should be 'false' if the <compartment> or "
    "<parameter> is the variable of a <rule> otherwise it can be either "
    "'true' or 'false' without affecting the interpretation of the model."
  },

  //99904
  {   
    MetaIdNotValidAttribute,   
    "Attribute 'metaid' not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'metaId' attribute on all <SBase> objects was not available "
    "in SBML Level 1. In order for the internal "
    "respresentation to be correct, the value for the "
    "metaId member variable should be not be set."
  },

  //99905
  {   
    SBOTermNotValidAttributeBeforeL2V3,   
    "'sboTerm' not available on this component before SBML Level 2 Version 3",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'sboTerm' attribute on <compartment>, <compartmentType> "
    "<delay>, <species>, <speciesType>, <stoichiometryMath>, <trigger> "
    "<unit> and <unitDefinition> was not available before SBML Level 2 "
    "Version 3. In order for the internal "
    "respresentation of these components to be correct, the value for the "
    "sboTerm member variable should be '-1'."
  },

  //99906
  {   
    InvalidL1CompartmentUnits,   
    "Invalid units for a compartment in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The value of the 'units' attribute on a <compartment> must be either "
    "'volume', 'litre', 'liter' or the identifier of a <unitDefinition> "
    "based on either 'litre'/'liter' or 'metre'/'meter' (with "
    "'exponent' equal to '3')."
  },

  //99907
  {   
    L1V1CompartmentVolumeReqd,   
    "Compartment volume must be specified",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "A value for the compartment 'volume' attribute must be specified "
    "in this Level+Version of SBML."
  },


  //99908
  {   
    CompartmentTypeNotValidComponent,   
    "Compartment types not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <compartmentType> component was introduced in SBML Level 2 Version "
    "2. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfCompartmentTypes> component should be empty."
  },

  //99909
  {   
    ConstraintNotValidComponent,   
    "Constraints not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <constraint> component was introduced in SBML Level 2 Version "
    "2. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfConstraints> component should be empty."
  },

  //99910
  {   
    EventNotValidComponent,   
    "Events not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <event> component was introduced in SBML Level 2 Version "
    "1. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfEvents> component should be empty."
  },

  //99911
  {   
    SBOTermNotValidAttributeBeforeL2V2,   
    "The 'sboTerm' attribute is invalid for this component before Level 2 Version 2",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'sboTerm' attribute on <event>, <eventAssignment> "
    "<functionDefinition>, <kineticLaw>, <model>, <parameter>, <reaction> "
    "<rule> and <speciesReferenece> was not available before SBML Level 2 "
    "Version 2. In order for the internal "
    "respresentation of these components to be correct, the value for the "
    "sboTerm member variable should be '-1'."
  },

  //99912
  {   
    FuncDefNotValidComponent,   
    "Function definitions are not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <functionDefinition> component was introduced in SBML Level 2 Version "
    "1. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfFunctionDefinitions> component should be empty."
  },

  //99913
  {   
    InitialAssignNotValidComponent,   
    "Initial assignments are not supported in this Level+Version of SBML",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <initialAssignment> component was introduced in SBML Level 2 Version "
    "2. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfInitialAssignments> component should be empty."
  },

  //99914
  {   
    VariableNotValidAttribute,   
    "Attribute 'variable' not valid",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,
    "An <algebraicRule> does not assign a value to a particular variable "
    "and does not carry an attribute named 'variable'."
  },

  //99915
  {   
    UnitsNotValidAttribute,   
    "Attribute 'units' not valid",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,
    "The 'units' attribute on <rule> was only applicable to a <parameterRule> "
    "in SBML Level 1. In order for the internal respresentation of a <rule> to "
    "be correct, the value for the units member variable should not be set."
  },

  //99916
  {   
    ConstantSpeciesNotValidAttribute,   
    "Attribute 'constant' on species not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'constant' attribute on <species> was not available in SBML Level "
    "1. In order for the internal respresentation "
    "of <species> to be correct, the value for the "
    "constant member variable should be 'false' if (1) the <species> is the "
    "variable of a <rule> or (2) the 'boundaryCondition' attribute is 'false' "
    "and the <species> is a procduct/reactant in a <reaction>. Otherwise it "
    "can be either 'true' or 'false' without affecting the interpretation of "
    "the model."
  },

  //99917
  {   
    SpatialSizeUnitsNotValidAttribute,   
    "Attribute 'spatialSizeUnits' on species not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'spatialSizeUnits' attribute on <species> was not available "
    "in SBML Level 1. In order for the internal "
    "respresentation of a <species> to be correct, the value for the "
    "spatialSizeUnits member variable should be not be set."
  },

  //99918
  {   
    SpeciesTypeNotValidAttribute,   
    "Attribute 'speciesType' on species not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'speciesType' attribute on <species> was not available "
    "in SBML Level 1 or SBML Level 2 Version 1. In order for the internal "
    "respresentation of a <species> to be correct, the value for the "
    "speciesType member variable should be not be set."
  },

  //99919
  {   
    HasOnlySubsUnitsNotValidAttribute,   
    "Attribute 'hasOnlySubstanceUnits' on species not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'hasOnlySubstanceUnits' attribute on <species> was not available "
    "in SBML Level 1. In order for the internal "
    "respresentation of a <species> to be correct, the value for the "
    "hasOnlySubstanceUnits member variable should be 'false'."
  },

  //99920
  {   
    IdNotValidAttribute,   
    "Attribute 'id' on species references not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'id' attribute on <speciesReference> was not available "
    "in SBML Level 1 or SBML Level 2 Version 1. In order for the internal "
    "respresentation of a <speciesReference> to be correct, the value for the "
    "id member variable should be not be set."
  },

    //99921
  {   
    NameNotValidAttribute,   
    "Attribute 'name' on species references not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'name' attribute on <speciesReference> was not available "
    "in SBML Level 1 or SBML Level 2 Version 1. In order for the internal "
    "respresentation of a <speciesReference> to be correct, the value for the "
    "name member variable should be not be set."
   },

  //99922
  {   
    SpeciesTypeNotValidComponent,   
    "Species types not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <speciesType> component was introduced in SBML Level 2 Version "
    "2. In order for the internal respresentation of a <model> to "
    "be correct, the <listOfSpeciesTypes> component should be empty."
  },

  //99923
  {   
    StoichiometryMathNotValidComponent,   
    "<stoichiometryMath> not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The <stoichiometryMath> component on a <speciesReference> was "
    "introduced in SBML Level 2. "
    "In order for the internal respresentation of a <speciesReference> to "
    "be correct, the <stoichiometryMath> component should be 'NULL'."
  },

  //99924
  {   
    MultiplierNotValidAttribute,   
    "Attribute 'multiplier' on units not supported in SBML Level 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'multiplier' attribute on <unit> was not available in SBML Level 1. "
    "In order for the internal respresentation of a <unit> to "
    "be correct, the value for the multiplier member variable should be '1.0'."
  }, 

  //99925
  {   
    OffsetNotValidAttribute,   
    "Attribute 'offset' on units only available in SBML Level 2 Version 1",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,   
    LIBSBML_SEV_NOT_APPLICABLE,
    "The 'offset' attribute on <unit> was only available in SBML Level 2 "
    "Version 1. In order for the internal respresentation of a <unit> to "
    "be correct, the value for the offset member variable should be '0'."
  }, 

  //99997
  {   
    InvalidTargetLevelVersion,   
    "The target Level/Version does not exist",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,
    "Conversion is only possible to an existing SBML Level and Version."
  },

  //99998
  {   
    L3NotSupported,   
    "SBML Level 3 is not yet supported",
    LIBSBML_CAT_INTERNAL_CONSISTENCY,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,   
    LIBSBML_SEV_ERROR,
    "Validation of or conversion from L3 documents/models is not yet "
    "supported."
  } 

};

LIBSBML_CPP_NAMESPACE_END

/** @endcond doxygen-libsbml-internal */
