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

  const string msg20202 =
    "The order of subelements within Model must be the following (where any "
    "one may be optional, but the ordering must be maintained): "
    "listOfFunctionDefinitions, listOfUnitDefinitions, listOfCompartmentTypes, "
    "listOfSpeciesTypes, listOfCompartments, listOfSpecies, listOfParameters, "
    "listOfInitialAssignments, listOfRules, listOfConstraints, listOfReactions "
    "and listOfEvents. (References: L2V2 Section 4.2.)";

  const string msg21002 =
    "The order of subelements within Constraint must be the following: math, "
    "message. The message element is optional, but if present, must follow the "
    "math element. (References: L2V2 Section 4.12.)";

  const string msg21102 =
    "The order of subelements within Reaction must be the following: "
    "listOfReactants (optional), listOfProducts (optional), "
    "listOfModifiers (optional), kineticLaw. (References: L2V2 Section 4.13.)";

  const string msg21122 =
    "The order of subelements within KineticLaw must be the following: math, "
    "listOfParameters. The listOfParameters is optional, but if present, must "
    "follow math. (References: L2V2 Section 4.13.9.).";

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
