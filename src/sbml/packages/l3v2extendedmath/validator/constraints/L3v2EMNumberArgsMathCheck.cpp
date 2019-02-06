/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    L3v2EMNumberArgsMathCheck.cpp
 * @brief   Ensures number of arguments to functions are appropriate.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/Model.h>
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/Parameter.h>
#include <sbml/UnitDefinition.h>
#include <sbml/Event.h>
#include <sbml/Reaction.h>
#include <sbml/EventAssignment.h>
#include <sbml/SpeciesReference.h>
#include <sbml/Rule.h>
#include <sbml/math/FormulaFormatter.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathASTPlugin.h>

#include "L3v2EMNumberArgsMathCheck.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new Constraint with the given @p id.
 */
L3v2EMNumberArgsMathCheck::L3v2EMNumberArgsMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
L3v2EMNumberArgsMathCheck::~L3v2EMNumberArgsMathCheck ()
{
}


/*
  * Checks the MathML of the ASTnode 
  * is appropriate for the function being performed
  *
  * If an inconsistency is found, an error message is logged.
  */
void
L3v2EMNumberArgsMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{

  const L3v2extendedmathASTPlugin * astPlugin = dynamic_cast<const L3v2extendedmathASTPlugin*>(node.getPlugin("l3v2extendedmath"));

  if (astPlugin != 0 && astPlugin->defines(node.getType()))
  {
    stringstream error;
    int result = astPlugin->checkNumArguments(&node, error);
    if (result == -1)
    {
      string errmsg = "The function '";
      errmsg += node.getName();
      errmsg += "' takes " + error.str();
      logPackageMathConflict(node, sb, errmsg);
    }
  }

  checkChildren(m, node, sb);

}



/*
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given @p id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
L3v2EMNumberArgsMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream oss_msg;

  //oss_msg << getPreamble();

  char * formula = SBML_formulaToString(&node);
  oss_msg << "The formula '" << formula;
  oss_msg << "' in the " << getFieldname() << " element of the <" << object.getElementName();
  oss_msg << "> ";
  switch(object.getTypeCode()) {
  case SBML_INITIAL_ASSIGNMENT:
  case SBML_EVENT_ASSIGNMENT:
  case SBML_ASSIGNMENT_RULE:
  case SBML_RATE_RULE:
    //LS DEBUG:  could use other attribute values, or 'isSetActualId'.
    break;
  default:
    if (object.isSetIdAttribute()) {
      oss_msg << "with id '" << object.getIdAttribute() << "' ";
    }
    break;
  }
  oss_msg << "has an inappropriate number of arguments.";
  safe_free(formula);

  return oss_msg.str();
}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */

