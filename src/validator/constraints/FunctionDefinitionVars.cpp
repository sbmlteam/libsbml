/**
 * @file    FunctionDefinitionVars.cpp
 * @brief   Ensures FunctionDefinitions contain no undefined variables.
 * @author  Ben Bornstein
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


#include <sbml/Model.h>
#include <sbml/FunctionDefinition.h>
#include <sbml/util/List.h>
#include <sbml/math/ASTNode.h>

#include "FunctionDefinitionVars.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
FunctionDefinitionVars::FunctionDefinitionVars (unsigned int id, Validator& v) :
  TConstraint<FunctionDefinition>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
FunctionDefinitionVars::~FunctionDefinitionVars ()
{
}


/**
 * Checks that all variables referenced in FunctionDefinition bodies are
 * bound variables (function arguments).
 */
void
FunctionDefinitionVars::check_ (const Model& m, const FunctionDefinition& fd)
{
  if ( fd.getLevel() == 1         ) return;
  if ( !fd.isSetMath()            ) return;
  if ( !fd.getBody()              ) return;
  if (  fd.getNumArguments() == 0 ) return;


  List* variables = fd.getBody()->getListOfNodes( ASTNode_isName );


  for (unsigned int n = 0; n < variables->getSize(); ++n)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(n) );
    string   name = node->getName() ? node->getName() : "";

    if ( !fd.getArgument(name) ) logUndefined(fd, name);
  }
}


/**
 * Logs a message about an undefined variable in the given
 * FunctionDefinition.
 */
void
FunctionDefinitionVars::logUndefined ( const FunctionDefinition& fd,
                                       const string& varname )
{
  msg =
  msg =
    "Inside the 'lambda' of a <functionDefinition>, if a 'ci' element is not "
    "the first element within a MathML 'apply', then the 'ci''s value can "
    "only be the value of a 'bvar' element declared in that 'lambda'. In "
    "other words, all model entities referenced inside a function definition "
    "must be passed arguments to that function. (References: L2V2 Section "
    "4.3.2.)" 
    "The variable '";

  msg += varname;
  msg += "' is not listed as a <bvar> of FunctionDefinition '";
  msg += fd.getId();
  msg += "'.";
  
  logFailure(fd);
}
