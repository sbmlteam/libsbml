/**
 * \file    FunctionDefinitionVars.cpp
 * \brief   Ensures FunctionDefinitions contain no undefined variables.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <string>

#include "sbml/Model.h"
#include "sbml/FunctionDefinition.h"
#include "math/ASTNode.h"
#include "xml/ParseMessage.h"

#include "FunctionDefinitionVars.h"


using namespace std;


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
    "A FunctionDefinition may only reference variables declared in its "
    "list of bound variables (function parameters) (L2v1 Section 3.6.3).  "
    "The variable '";

  msg += varname;
  msg += "' is not listed as a <bvar> of FunctionDefinition '";
  msg += fd.getId();
  msg += "'.";
  
  logFailure(fd);
}
