/**
 * \file    SBMLErrorLog.cpp
 * \brief   Stores errors (and messages) encountered during an SBML parse
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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


#include <string>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLParser.h>

#include "SBMLErrorLog.h"


using namespace std;


/**
 * Creates a new empty SBMLErrorLog.
 */
SBMLErrorLog::SBMLErrorLog ()
{
}


/**
 * Destroys this SBMLErrorLog.
 */
SBMLErrorLog::~SBMLErrorLog ()
{
}


/**
 * Logs an error message for the given SBML error code.
 */
void
SBMLErrorLog::logError (unsigned int error, unsigned int inRead)
{
  string msg;

  const string msg00001 = 
    "File not found.";

  const string msg00002 = 
    "No encoding specified.";

  const string msg00005 =
    "Parse error encountered.";

  const string msg10101 = 
    "An SBML XML file must use UTF-8 as the character encoding. More "
    "precisely, the 'encoding' attribute of the XML declaration at the "
    "beginning of the XML data stream cannot have a value other than "
    "'UTF-8'. An example valid declaration is "
    "'<?xml version=\"1.0\" encoding=\"UTF-8\"?>'. (References: L2V2 Section "
    "4.1.)";

  const string msg10103 =
    "An SBML XML document must conform to the XML Schema for the corresponding "
    "SBML Level, Version and Revision. The XML Schema for SBML defines the "
    "basic SBML object structure, the data types used by those objects, and the "
    "order in which the objects may appear in an SBML document. (References: "
    "L2V2 Section 4.1.)";

  const string msg10201 =
    "All MathML content in SBML must appear within a 'math' element, and the "
    "'math' element must be either explicitly or implicitly in the XML "
    "namespace 'http://www.w3.org/1998/Math/MathML'. (References: L2V2 "
    "Section 3.5.)";

  const string msg10202 =
    "The only permitted MathML 2.0 elements in SBML Level 2 are "
    "the following: cn, ci, csymbol, sep, apply, piecewise, piece, otherwise, "
    "eq, neq, gt, lt, geq, leq, plus, minus, times, divide, power, root, abs, "
    "exp, ln, log, floor, ceiling, factorial, and, or, xor, not, degree, bvar, "
    "logbase, sin, cos, tan, sec, csc, cot, sinh, cosh, tanh, sech, csch, "
    "coth, arcsin, arccos, arctan, arcsec, arccsc, arccot, arcsinh, arccosh, "
    "arctanh, arcsech, arccsch, arccoth, true, false, notanumber, pi, "
    "infinity, exponentiale, semantics, annotation, and annotation-xml. "
    "(References: L2V2 Section 3.5.1.)";

  const string msg10203 =
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "encoding is only permitted on csymbol. No other MathML elements may "
    "have a encoding attribute. (References: L2V2 Section 3.5.1.).";

  const string msg10204 =
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "definitionURL is only permitted on csymbol. No other MathML elements "
    "may have a definitionURL attribute. (References: L2V2 Section 3.5.1.).";

  const string msg10206 =
    "In the SBML subset of MathML 2.0, the MathML attribute "
    "type is only permitted on the cn construct. No other MathML elements "
    "may have a type attribute. (References: L2V2 Section 3.5.1.).";

  const string msg10207 =
    "The only permitted values for the 'type' attribute on MathML 'cn' "
    "elements are 'e-notation', 'real', 'integer', and 'rational'. "
    "(References: L2V2 Section 3.5.2.)";

  const string msg10309 =
    "The syntax of 'metaid' attribute values must conform to the syntax of the "
    "XML type 'ID'. (References: L2V2 Sections 3.3.1 and 3.1.6.)";

  const string msg10310 =
    "The syntax of 'id' attribute values must conform to the syntax of the SBML "
    "type 'SId'. (References: L2V2 Sections 3.1.7.)";

  const string msg10401 =
    "Every top-level element within an annotation element must have a "
    "namespace declared. (References: L2V2 Section 3.3.3.)";

  const string msg10402 =
    "There cannot be more than one top-level element using a given namespace "
    "inside a given annotation element. (References: L2V2 Section 3.3.3.)";

  const string msg10403 = 
    "Top-level elements within an annotation element cannot use any SBML "
    "namespace, whether explicitly (by declaring the namespace to be one of "
    "the URIs, or implicitly (by failing to declare any namespace). "
    "(References: L2V2 Section 3.3.3.)";

  const string msg20101 =
    "The 'sbml' container element must declare the XML Namespace for SBML, "
    "and this declaration must be consistent with the values of the 'level' "
    "and 'version' attributes on the 'sbml' element. (References: L2V2 "
    "Section 4.1.)";

  const string msg20102 =
    "The 'sbml' container element must declare the SBML Level using the "
    "attribute 'level', and this declaration must be consistent with the XML "
    "Namespace declared for the 'sbml' element. (References: L2V2 Section "
    "4.1.)";

  const string msg20103 =
    "The 'sbml' container element must declare the SBML Version using the "
    "attribute 'version', and this declaration must be consistent with the "
    "XML Namespace declared for the 'sbml' element. (References: L2V2 "
    "Section 4.1.)";

  const string msg20201 =
    "An SBML document must contain a <model> definition. (References: L2V1 "
    "and L2V2 Section 4.1).";

  const string msg20202 =
    "The order of subelements within Model must be the following (where any "
    "one may be optional, but the ordering must be maintained): "
    "listOfFunctionDefinitions, listOfUnitDefinitions, listOfCompartmentTypes, "
    "listOfSpeciesTypes, listOfCompartments, listOfSpecies, listOfParameters, "
    "listOfInitialAssignments, listOfRules, listOfConstraints, listOfReactions "
    "and listOfEvents. (References: L2V2 Section 4.2.)";

  const string msg20203 =
    "The 'listOf___' containers in a <model> are optional, but if present, "
    "the lists cannot be empty. Specifically, if any of the following are "
    "present in a <model>, they must not be empty: "
    "'listOfFunctionDefinitions', 'listOfUnitDefinitions', "
    "'listOfCompartmentTypes', 'listOfSpeciesTypes', 'listOfCompartments',  "
    "'listOfSpecies', 'listOfParameters', 'listOfInitialAssignments', "
    "'listOfRules', 'listOfConstraints', 'listOfReactions' and "
    "'listOfEvents'. (References: This is a requirement stemming from the "
    "XML Schema used for SBML.)";

  const string msg20409 =
    "The 'listOfUnits' container in a <unitDefinition> cannot be empty. "
    "(References: L2V2 Section 4.4.)";
  
  const string msg21002 =
    "The order of subelements within Constraint must be the following: math, "
    "message. The message element is optional, but if present, must follow the "
    "math element. (References: L2V2 Section 4.12.)";

  const string msg21102 =
    "The order of subelements within Reaction must be the following: "
    "listOfReactants (optional), listOfProducts (optional), "
    "listOfModifiers (optional), kineticLaw. (References: L2V2 Section 4.13.)";

  const string msg21103 =
    "The following containers are all optional in a <reaction>, but if any "
    "present is, it must not be empty: 'listOfReactants', 'listOfProducts', "
    "'listOfModifiers', 'kineticLaw'. (References: L2V2 Section 4.13.)";

  const string msg21104 =
    "The list of reactants ('listOfReactants') and list of products "
    "('listOfProducts') in a <reaction> can only contain 'speciesReference' "
    "elements. (References: L2V1 Section 4.9; L2V2 Section 4.13.)";

  const string msg21105 =
    "The list of modifiers ('listOfModifiers') in a <reaction> can only "
    "contain 'modifierSpeciesReference' elements. (References: L2V1 Section "
    "4.9; L2V2 Section 4.13.)";

  const string msg21122 =
    "The order of subelements within KineticLaw must be the following: math, "
    "listOfParameters. The listOfParameters is optional, but if present, must "
    "follow math. (References: L2V2 Section 4.13.9.).";

  const string msg21123 =
    "If present, the 'listOfParameters' in a <kineticLaw> must not be an "
    "empty list. (References: L2V2 Section 4.13.)";
     
  const string msg21205 =
    "The order of subelements within Event must be the following: trigger, "
    "delay, listOfEventAssignments. The delay element is optional, but if "
    "present, must follow trigger. (References: L2V2 Section 4.14.)";

  const string msg99999 =
    "The value of the sboTerm field on a Model must be an SBO identifier "
    "(http://www.biomodels.net/SBO/) referring to a modeling framework "
    "defined in SBO (i.e., terms derived from SBO:0000004, “modeling framework”). "
    "(References: L2V2 Section 4.2.1.)";


  switch (error)
  {
    case 00001: msg = msg00001; break;
    case 00002: msg = msg00002; break;
    case 00005: msg = msg00005; break;
    case 10101: msg = msg10101; break;
    case 10103: msg = msg10103; break;
    case 10201: msg = msg10201; break;
    case 10202: msg = msg10202; break;
    case 10203: msg = msg10203; break;
    case 10204: msg = msg10204; break;
    case 10206: msg = msg10206; break;
    case 10207: msg = msg10207; break;
    case 10309: msg = msg10309; break;
    case 10310: msg = msg10310; break;
    case 10401: msg = msg10401; break;
    case 10402: msg = msg10402; break;
    case 10403: msg = msg10403; break;
    case 20101: msg = msg20101; break;
    case 20102: msg = msg20102; break;
    case 20103: msg = msg20103; break;
    case 20201: msg = msg20201; break;
    case 20202: msg = msg20202; break;
    case 20203: msg = msg20203; break;
    case 20409: msg = msg20409; break;
    case 21002: msg = msg21002; break;
    case 21102: msg = msg21102; break;
    case 21122: msg = msg21122; break;
    case 21103: msg = msg21103; break;
    case 21104: msg = msg21104; break;
    case 21105: msg = msg21105; break;
    case 21123: msg = msg21123; break;
    case 21205: msg = msg21205; break;
    case 99999: msg = msg99999; break;

    default:    msg = "Unrecognized error code."; break;
  }

  /**
   * this is a horrible hack to cope with the fact that xerces does 
   * not report a parse error
   */

#if ((defined(USE_XERCES)) || (defined(CYGWIN) && ! defined(USE_EXPAT)))
  if (error == 5)
  {
    add( XMLError(error, msg, XMLError::Error, "", 1, 1));
  }
  else if (inRead == 1)
  {
    add( XMLError(error, msg) );
  }
  else
  {
    add( XMLError(error, msg, XMLError::Error, "", 1, 1));
  }
#else
  if (inRead == 1)
  {
    add( XMLError(error, msg) );
  }
  else
  {
    add( XMLError(error, msg, XMLError::Error, "", 1, 1));
  }
#endif

}


/**
 * Logs an error message indicating the XML element is unrecognized.
 */
void
SBMLErrorLog::unrecognizedElement (const XMLToken& element)
{
  string msg = "The element '" + element.getName() + "' is not recognised. "
        "An SBML XML document must not contain undefined elements or attributes "
    "in the SBML namespace. Documents containing unknown elements or "
    "attributes placed in the SBML namespace do not conform to the SBML "
    "Level 2 specification. (References: L2V2 Section 4.1.)";

  add( XMLError(10102, msg) );
}
