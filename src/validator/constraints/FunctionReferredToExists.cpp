/**
 * \file    FunctionReferredToExists.cpp
 * \brief   Ensures unique variables assigned by rules and events
 * \author  Sarah Keating
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


#include <string>

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>

#include "FunctionReferredToExists.h"
#include "IdList.h"


using namespace std;




/**
 * Creates a new Constraint with the given constraint id.
 */
FunctionReferredToExists::FunctionReferredToExists (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
FunctionReferredToExists::~FunctionReferredToExists ()
{
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
FunctionReferredToExists::check_ (const Model& m, const Model& object)
{
  unsigned int n;

  for (n = 0; n < m.getNumFunctionDefinitions(); ++n)
  {
    const ASTNode* node = m.getFunctionDefinition(n)->getBody();

    if (node != NULL && node->getType() == AST_FUNCTION)
    {
      if (!mFunctions.contains(node->getName()))
      {
        logUndefined(*m.getFunctionDefinition(n), node->getName());
      }
    }
    mFunctions.append(m.getFunctionDefinition(n)->getId());

  }  
}

/**
  * Logs a message about an undefined <ci> element in the given
  * FunctionDefinition.
  */
void
FunctionReferredToExists::logUndefined ( const FunctionDefinition& fd,
                                       const string& varname )
{
  msg =
    "Inside the 'lambda' of a <functionDefinition>, if a 'ci' element is the "
    "first element within a MathML 'apply', then the 'ci''s value can only "
    "be chosen from the set of identifiers of other SBML "
    "<functionDefinition>s defined prior to that point in the SBML model. In "
    "other words, forward references to user-functions are not permitted. "
    "(References: L2V2 Section 4.3.2.)";

  msg += varname;
  msg += "' is not listed as the id of an existing FunctionDefinition.";

  
  logFailure(fd);
}
