/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    MultiMathCiCheckRepresentationType.cpp
 * @brief   checks &lt;ci&gt; has a valid representationType attribute
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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


#include "MultiMathCiCheckRepresentationType.h"
#include <sbml/packages/multi/common/MultiExtensionTypes.h>
#include <sbml/packages/multi/validator/MultiValidator.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus
/**
 * Creates a new Constraint with the given @p id.
 */
MultiMathCiCheckRepresentationType::MultiMathCiCheckRepresentationType (unsigned int id, MultiValidator& v) : MathMLBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
MultiMathCiCheckRepresentationType::~MultiMathCiCheckRepresentationType ()
{
}


/**
  * Checks the MathML of the ASTnode 
  * is appropriate for the function being performed
  *
  * If an inconsistency is found, an error message is logged.
  */
void
MultiMathCiCheckRepresentationType::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();
    
  /* if the node is a &lt;ci&gt; element it will have type AST_NAME
   * check if its representationType attribute has a valid value. */
  switch (type) 
  {
    case AST_NAME:
    	checkCiRepresentationType(m, node, sb);
      break;

    default:
      checkChildren(m, node, sb);
      break;
  }
}

  
/**
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given @p id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
MultiMathCiCheckRepresentationType::getMessage (const ASTNode& node, const SBase& object)
{
  ostringstream oss_msg;

  //oss_msg << getPreamble();
  char * formula = SBML_formulaToString(&node);
  oss_msg << "The 'multi:representationType' attribute of the ci element '" << formula;
  oss_msg << "' in <" << object.getElementName();
  oss_msg << "> does not have a valid RepresentationType value.";
  safe_free(formula);

  return oss_msg.str();
}

void
MultiMathCiCheckRepresentationType::checkCiRepresentationType(const Model& m, const ASTNode& node, const SBase & sb)
{
	if (!node.isCiNumber()) return;

	const MultiASTPlugin * astPlugin = dynamic_cast<const MultiASTPlugin*>(node.getPlugin("multi"));

	if (astPlugin == 0 || !astPlugin->isSetRepresentationType()) return;


    std::string representationType = astPlugin->getRepresentationType();

    if (representationType != "sum" && representationType != "numericValue"){
    	logMathConflict(node, sb);
    }
}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */

#endif /* __cplusplus */
