/**
 * @file    SBMLError.cpp
 * @brief   Represents SBML errors and other diagnostics
 * @author  Michael Hucka
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
  SBMLError::SBMLSeverity severity;
  const char*             message;
} sbmlErrorTableEntry;


static const sbmlErrorTableEntry errorTable[] =
{
  // 0
  { SBMLError::UnknownError, SBMLError::SBML, SBMLError::Fatal,
    "Unrecognized error encountered" },

  // 10101
  { SBMLError::NotUTF8, SBMLError::SBML, SBMLError::Error,
    "An SBML XML file must use UTF-8 as the character encoding. More "
    "precisely, the 'encoding' attribute of the XML declaration at the "
    "beginning of the XML data stream cannot have a value other than "
    "'UTF-8'. An example valid declaration is "
    "'<?xml version=\"1.0\" encoding=\"UTF-8\"?>'. (References: L2V2 Section "
    "4.1.)" },

  // 10102
  { SBMLError::UnrecognizedElement, SBMLError::SBML, SBMLError::Error,
    "An SBML XML document must not contain undefined elements or attributes "
    "in the SBML namespace. Documents containing unknown elements or "
    "attributes placed in the SBML namespace do not conform to the SBML "
    "Level 2 specification. (References: L2V2 Section 4.1.)" },

  // 10103
  { SBMLError::NotSchemaConformant, SBMLError::SBML, SBMLError::Error,
    "An SBML XML document must conform to the XML Schema for the corresponding "
    "SBML Level, Version and Revision. The XML Schema for SBML defines the "
    "basic SBML object structure, the data types used by those objects, and the "
    "order in which the objects may appear in an SBML document. (References: "
    "L2V2 Section 4.1.)" },

  // 10201
  { SBMLError::InvalidMathElement, SBMLError::SBML, SBMLError::Error,
    "All MathML content in SBML must appear within a 'math' element, and the "
    "'math' element must be either explicitly or implicitly in the XML "
    "namespace 'http://www.w3.org/1998/Math/MathML'. (References: L2V2 "
    "Section 3.5.)" },

  // 10202
  { SBMLError::DisallowedMathMLSymbol, SBMLError::SBML, SBMLError::Error,
    "The only permitted MathML 2.0 elements in SBML Level 2 are "
    "the following: cn, ci, csymbol, sep, apply, piecewise, piece, otherwise, "
    "eq, neq, gt, lt, geq, leq, plus, minus, times, divide, power, root, abs, "
    "exp, ln, log, floor, ceiling, factorial, and, or, xor, not, degree, bvar, "
    "logbase, sin, cos, tan, sec, csc, cot, sinh, cosh, tanh, sech, csch, "
    "coth, arcsin, arccos, arctan, arcsec, arccsc, arccot, arcsinh, arccosh, "
    "arctanh, arcsech, arccsch, arccoth, true, false, notanumber, pi, "
    "infinity, exponentiale, semantics, annotation, and annotation-xml. "
    "(References: L2V2 Section 3.5.1.)" },

  // 10203
  { SBMLError::DisallowedMathMLEncodingUse, SBMLError::SBML, SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "encoding is only permitted on csymbol. No other MathML elements may "
    "have a encoding attribute. (References: L2V2 Section 3.5.1.)." },

  // 10204
  { SBMLError::DisallowedDefinitionURLUse, SBMLError::SBML, SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "definitionURL is only permitted on csymbol. No other MathML elements "
    "may have a definitionURL attribute. (References: L2V2 Section 3.5.1.)." },

  // 10205
  { SBMLError::BadCsymbolDefinitionURLValue, SBMLError::SBML, SBMLError::Error,
    "In SBML Level 2 Versions 1 and Version 2, the only values permitted for "
    "definitionURL on a csymbol are \"http://www.sbml.org/sbml/symbols/time\" "
    "and \"http://www.sbml.org/sbml/symbols/delay\".(References: L2V2 "
    "Section 3.5.5.)." },

  // 10206
  { SBMLError::DisallowedMathTypeAttributeUse, SBMLError::SBML, SBMLError::Error,
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "type is only permitted on the cn construct. No other MathML elements "
    "may have a type attribute. (References: L2V2 Section 3.5.1.)." },

  // 10207
  { SBMLError::DisallowedMathTypeAttributeValue, SBMLError::SBML, SBMLError::Error,
    "The only permitted values for the 'type' attribute on MathML 'cn' "
    "elements are 'e-notation', 'real', 'integer', and 'rational'. "
    "(References: L2V2 Section 3.5.2.)"},

  // 10308
  { SBMLError::InvalidSBOTermSyntax, SBMLError::SBML, SBMLError::Error,
    "The value of a sboTerm attribute must have the data type SBOTerm, "
    "which is a string consisting of the characters 'S', 'B', 'O', ':' "
    "followed by exactly seven digits. (References: L2V2 Section 3.1.8.)"},

  // 10309
  { SBMLError::InvalidMetaidSyntax, SBMLError::SBML, SBMLError::Error,
    "The syntax of 'metaid' attribute values must conform to the syntax of the "
    "XML type 'ID'. (References: L2V2 Sections 3.3.1 and 3.1.6.)" },

  // 10310
  { SBMLError::InvalidIdSyntax, SBMLError::SBML, SBMLError::Error,
    "The syntax of 'id' attribute values must conform to the syntax of the SBML "
    "type 'SId'. (References: L2V2 Sections 3.1.7.)" },

  // 10401
  { SBMLError::MissingAnnotationNamespace, SBMLError::SBML, SBMLError::Error,
    "Every top-level element within an annotation element must have a "
    "namespace declared. (References: L2V2 Section 3.3.3.)" },

  // 10402
  { SBMLError::DuplicateAnnotationNamespaces, SBMLError::SBML, SBMLError::Error,
    "There cannot be more than one top-level element using a given namespace "
    "inside a given annotation element. (References: L2V2 Section 3.3.3.)" },

  // 10403
  { SBMLError::SBMLNamespaceInAnnotation, SBMLError::SBML, SBMLError::Error,
    "Top-level elements within an annotation element cannot use any SBML "
    "namespace, whether explicitly (by declaring the namespace to be one of "
    "the URIs, or implicitly (by failing to declare any namespace). "
    "(References: L2V2 Section 3.3.3.)" },

  // 10801
  { SBMLError::NotesNotInXHTMLNamespace, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must be explicitly placed in the XHTML "
    "XML namespace. (References: L2V3 Section 3.2.3.)" },

  // 10802
  { SBMLError::NotesContainsXMLDecl, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must not contain an XML declaration "
    "(i.e., a string of the form \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\" "
    "or similar). (References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 10803
  { SBMLError::NotesContainsDOCTYPE, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must not contain an XML DOCTYPE "
    "declaration (i.e., a string beginning with the characters \"<!DOCTYPE\". "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 10804
  { SBMLError::InvalidNotesContent, SBMLError::SBML, SBMLError::Error,
    "The XHTML content inside a notes element can only take one of the "
    "following general forms: (1) a complete XHTML document beginning with "
    "the element <html> and ending with </html>; (2) the \"body\" portion of "
    "a document beginning with the element <body> and ending with </body>; or "
    "(3) XHTML content that is permitted within a <body> ... </body> elements. "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 20101
  { SBMLError::InvalidNamespaceOnSBML, SBMLError::SBML, SBMLError::Error,
    "The 'sbml' container element must declare the XML Namespace for SBML, "
    "and this declaration must be consistent with the values of the 'level' "
    "and 'version' attributes on the 'sbml' element. (References: L2V2 "
    "Section 4.1.)" },

  // 20102
  { SBMLError::MissingOrInconsistentLevel, SBMLError::SBML, SBMLError::Error,
    "The 'sbml' container element must declare the SBML Level using the "
    "attribute 'level', and this declaration must be consistent with the XML "
    "Namespace declared for the 'sbml' element. (References: L2V2 Section "
    "4.1.)" },

  // 20103
  { SBMLError::MissingOrInconsistentVersion, SBMLError::SBML, SBMLError::Error,
    "The 'sbml' container element must declare the SBML Version using the "
    "attribute 'version', and this declaration must be consistent with the "
    "XML Namespace declared for the 'sbml' element. (References: L2V2 "
    "Section 4.1.)" },

  // 20201
  { SBMLError::MissingModel, SBMLError::SBML, SBMLError::Error,
    "An SBML document must contain a <model> definition. (References: L2V1 "
    "and L2V2 Section 4.1)." },

  // 20202
  { SBMLError::IncorrectOrderInModel, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Model must be the following (where any "
    "one may be optional, but the ordering must be maintained): "
    "listOfFunctionDefinitions, listOfUnitDefinitions, listOfCompartmentTypes, "
    "listOfSpeciesTypes, listOfCompartments, listOfSpecies, listOfParameters, "
    "listOfInitialAssignments, listOfRules, listOfConstraints, listOfReactions "
    "and listOfEvents. (References: L2V2 Section 4.2.)" },

  // 20203
  { SBMLError::EmptyListElement, SBMLError::SBML, SBMLError::Error,
    "The 'listOf___' containers in a <model> are optional, but if present, "
    "the lists cannot be empty. Specifically, if any of the following are "
    "present in a <model>, they must not be empty: "
    "'listOfFunctionDefinitions', 'listOfUnitDefinitions', "
    "'listOfCompartmentTypes', 'listOfSpeciesTypes', 'listOfCompartments',  "
    "'listOfSpecies', 'listOfParameters', 'listOfInitialAssignments', "
    "'listOfRules', 'listOfConstraints', 'listOfReactions' and "
    "'listOfEvents'. (References: This is a requirement stemming from the "
    "XML Schema used for SBML.)" },

  // 20409
  { SBMLError::EmptyListOfUnits, SBMLError::SBML, SBMLError::Error,
    "The 'listOfUnits' container in a <unitDefinition> cannot be empty. "
    "(References: L2V2 Section 4.4.)" },

  // 21002
  { SBMLError::IncorrectOrderInConstraint, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Constraint must be the following: math, "
    "message. The message element is optional, but if present, must follow the "
    "math element. (References: L2V2 Section 4.12.)" },

  // 21003
  { SBMLError::ConstraintNotInXHTMLNamespace, SBMLError::SBML, SBMLError::Error,
    "The contents of the message element in a Constraint must be explicitly "
    "placed in the XHTML XML namespace. (References: L2V3 Section 3.2.3.)" },

  // 21004
  { SBMLError::ConstraintContainsXMLDecl, SBMLError::SBML, SBMLError::Error,
    "The contents of the message element must not contain an XML declaration "
    "(i.e., a string of the form \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\" "
    "or similar). (References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 21005
  { SBMLError::ConstraintContainsDOCTYPE, SBMLError::SBML, SBMLError::Error,
    "The contents of the message element must not contain an XML DOCTYPE "
    "declaration (i.e., a string beginning with the characters \"<!DOCTYPE\". "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 21006
  { SBMLError::InvalidConstraintContent, SBMLError::SBML, SBMLError::Error,
    "The XHTML content inside a Constraint's message element can only take one "
    "of the following general forms: (1) a complete XHTML document beginning "
    "with the element <html> and ending with </html>; (2) the \"body\" portion "
    "of a document beginning with the element <body> and ending with </body>; "
    "or (3) XHTML content that is permitted within a <body> ... </body> "
    "elements. (References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 21102
  { SBMLError::IncorrectOrderInReaction, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Reaction must be the following: "
    "listOfReactants (optional), listOfProducts (optional), "
    "listOfModifiers (optional), kineticLaw. (References: L2V2 Section 4.13.)" },

  // 21103
  { SBMLError::EmptyListInReaction, SBMLError::SBML, SBMLError::Error,
    "The following containers are all optional in a <reaction>, but if any "
    "present is, it must not be empty: 'listOfReactants', 'listOfProducts', "
    "'listOfModifiers', 'kineticLaw'. (References: L2V2 Section 4.13.)" },

  // 21104
  { SBMLError::InvalidReactantsProductsList, SBMLError::SBML, SBMLError::Error,
    "The list of reactants ('listOfReactants') and list of products "
    "('listOfProducts') in a <reaction> can only contain 'speciesReference' "
    "elements. (References: L2V1 Section 4.9; L2V2 Section 4.13.)" },

  // 21105
  { SBMLError::InvalidModifiersList, SBMLError::SBML, SBMLError::Error,
    "The list of modifiers ('listOfModifiers') in a <reaction> can only "
    "contain 'modifierSpeciesReference' elements. (References: L2V1 Section "
    "4.9; L2V2 Section 4.13.)" },

  // 21122
  { SBMLError::IncorrectOrderInKineticLaw, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within KineticLaw must be the following: math, "
    "listOfParameters. The listOfParameters is optional, but if present, must "
    "follow math. (References: L2V2 Section 4.13.9.)." },

  // 21123
  { SBMLError::EmptyListInKineticLaw, SBMLError::SBML, SBMLError::Error,
    "If present, the 'listOfParameters' in a <kineticLaw> must not be an "
    "empty list. (References: L2V2 Section 4.13.)" },

  // 21205
  { SBMLError::IncorrectOrderInEvent, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Event must be the following: trigger, "
    "delay, listOfEventAssignments. The delay element is optional, but if "
    "present, must follow trigger. (References: L2V2 Section 4.14.)" },
};





SBMLError::SBMLError (  const unsigned int            id
                      , const std::string&            details
                      , const unsigned int            line
  		      , const unsigned int            column
		      , const SBMLError::SBMLSeverity severity
                      , const SBMLError::SBMLCategory category )
{
  // Check if the given id is one we have in our table of error codes.  If
  // it is, fill in the fields of the error object with the appropriate
  // content.  If it's not in the table, take the content as-is.

  mId     = id;
  mLine   = line;
  mColumn = column;

  if ( id > XMLError::ErrorCodesUpperBound
       && id < SBMLError::ErrorCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
    {
      if ( errorTable[i].code == id )
      {
	ostringstream newMsg;

	if ( !details.empty() ) newMsg << details << "." << endl;

	newMsg << "This fails to satisfy SBML validation rule number "
	       << id << ":" << errorTable[i].message << endl;

	mMessage  = newMsg.str();
	mSeverity = errorTable[i].severity;
	mCategory = errorTable[i].category;
	return;
      }
    }

    // The id is in the range of error numbers that are supposed to be in
    // the XML layer, but it's NOT in our table.  This is an internal error.
    // Unfortunately, we don't have an error log or anywhere to report it
    // except the measure of last resort: the standard error output.
    
    cerr << "Internal error: unknown error code '" << id
	 << "' encountered while processing error" << endl;
  }

  // It's not an error code in the XML layer, so assume the caller has
  // filled in all the relevant additional data.  (If they didn't, the
  // following merely assigns the defaults.)

  mMessage  = details;
  mSeverity = severity;
  mCategory = category;
}
