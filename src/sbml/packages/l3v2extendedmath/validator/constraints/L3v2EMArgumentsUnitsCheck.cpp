/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    L3v2EMArgumentsUnitsCheck.cpp
 * @brief   Ensures math units are consistent.
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


#include "L3v2EMArgumentsUnitsCheck.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new Constraint with the given @p id.
 */
L3v2EMArgumentsUnitsCheck::L3v2EMArgumentsUnitsCheck (unsigned int id, Validator& v) : ArgumentsUnitsCheck(id, v)
{
}


/*
 * Destroys this Constraint.
 */
L3v2EMArgumentsUnitsCheck::~L3v2EMArgumentsUnitsCheck ()
{
}


/*
  * Checks that the units of the result of the assignment rule
  * are consistent with variable being assigned
  *
  * If an inconsistent variable is found, an error message is logged.
  */
void
L3v2EMArgumentsUnitsCheck::checkUnits (const Model& m, const ASTNode& node, const SBase & sb,
                                 bool inKL, int reactNo)
{
  ASTNodeType_t type = node.getType();

  switch (type) 
  {
    /* functions that act on same units */
    case AST_FUNCTION_MIN:
    case AST_FUNCTION_MAX:
  
      checkSameUnitsAsArgs(m, node, sb, inKL, reactNo);
      break;

    case AST_FUNCTION:

      checkFunction(m, node, sb, inKL, reactNo);
      break;

    default:
      checkChildren(m, node, sb, inKL, reactNo);
      break;

  }
}

  
///*
//  * Checks that the units of the function are consistent
//  * for a function returning value with same units as argument(s)
//  *
//  * If inconsistent units are found, an error message is logged.
//  */
//void 
//L3v2EMArgumentsUnitsCheck::checkSameUnitsAsArgs (const Model& m, 
//                                              const ASTNode& node, 
//                                              const SBase & sb, bool inKL, 
//                                              int reactNo)
//{
//  /* check that node has children */
//  if (node.getNumChildren() == 0)
//  {
//    return;
//  }
//
//  UnitDefinition * ud;
//  UnitDefinition * tempUD = NULL;
//  unsigned int n;
//  unsigned int i = 0;
//  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);
//
//  ud = unitFormat->getUnitDefinition(node.getChild(i), inKL, reactNo);
//
//  /* get the first child that is not a parameter with undeclared units */
//  while ((unitFormat->getContainsUndeclaredUnits() || (ud != NULL && ud->getNumUnits() == 0)) && 
//    i < node.getNumChildren()-1)
//  {
//    delete ud; 
//    i++;
//    unitFormat->resetFlags();
//    ud = unitFormat->getUnitDefinition(node.getChild(i), inKL, reactNo);
//  }
//
//
//  /* check that all children have the same units 
//   * unless one of the children is a parameter with undeclared units 
//   * which is not tested */
//  for (n = i+1; n < node.getNumChildren(); n++)
//  {
//    unitFormat->resetFlags();
//    tempUD = unitFormat->getUnitDefinition(node.getChild(n), inKL, reactNo);
//
//    if (!unitFormat->getContainsUndeclaredUnits())
//    {
//      if (!UnitDefinition::areIdenticalSIUnits(ud, tempUD))
//      {
//        logInconsistentSameUnits(node, sb);
//      }
//    }
//    delete tempUD;
//  }
//
//  delete unitFormat;
//  delete ud;
//
//  for (n = 0; n < node.getNumChildren(); n++)
//  {
//    checkUnits(m, *node.getChild(n), sb, inKL, reactNo);
//  }
//}
//

///*
// * Logs a message about a function that should return same units
// * as the arguments
// */
//void 
//L3v2EMArgumentsUnitsCheck::logInconsistentSameUnits (const ASTNode & node, 
//                                             const SBase & sb)
//{
//  char * formula = SBML_formulaToString(&node);
//  msg = "The formula '" ;
//  msg += formula;
//  msg += "' in the math element of the <";
//  msg += sb.getElementName();
//  msg += "> ";
//  switch(sb.getTypeCode()) {
//  case SBML_INITIAL_ASSIGNMENT:
//  case SBML_EVENT_ASSIGNMENT:
//  case SBML_ASSIGNMENT_RULE:
//  case SBML_RATE_RULE:
//    //LS DEBUG:  could use other attribute values, or 'isSetActualId'.
//    break;
//  default:
//    if (sb.isSetId()) {
//      msg += "with id '";
//      msg += sb.getId() + "' ";
//    }
//    break;
//  }
//  msg += "can only act on variables with the same units.";
//  safe_free(formula);
//
//  logFailure(sb, msg);
//
//}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */

