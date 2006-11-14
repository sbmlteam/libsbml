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
SBMLErrorLog::logError (unsigned int error)
{
  string msg;

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
    "The syntax of 'metaid' field values must conform to the syntax of the "
    "XML type 'ID'. (References: L2V2 Sections 3.3.1 and 3.1.6.)";

  const string msg10310 =
    "The syntax of 'id' field values must conform to the syntax of the SBML "
    "type 'SId'. (References: L2V2 Sections 3.1.7.)";

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

  switch (error)
  {
    case 20202: msg = msg20202; break;
    case 21002: msg = msg21002; break;
    case 21102: msg = msg21102; break;
    case 21122: msg = msg21122; break;
    case 21205: msg = msg21205; break;
    case 10309: msg = msg10309; break;
    case 10310: msg = msg10310; break;
    case 20203: msg = msg20203; break;
    case 20409: msg = msg20409; break;
    case 21103: msg = msg21103; break;
    case 21104: msg = msg21104; break;
    case 21105: msg = msg21105; break;
    case 21123: msg = msg21123; break;
    case 10202: msg = msg10202; break;
    case 10203: msg = msg10203; break;
    case 10204: msg = msg10204; break;
    case 10206: msg = msg10206; break;
    case 10207: msg = msg10207; break;

    default:    msg = "Unrecognized error code."; break;
  }

  add( XMLError(error, msg) );
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
