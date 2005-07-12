/**
 * \file    BooleanReturnFromEventTrigger.cpp
 * \brief   Ensures the event trigger functions return a boolean
 * \author  Sarah Keating
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
 *     Sarah Keating
 *
 *     The SBML Team
 *     STRI
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <string>
#include <sstream>
#include <map>

#include "sbml/SBase.h"
#include "xml/ParseMessage.h"

#include "sbml/Model.h"
#include "sbml/Event.h"
#include "sbml/FunctionDefinition.h"
#include "math/ASTNode.h"
#include "BooleanReturnFromEventTrigger.h"


using namespace std;


static const char* PREAMBLE =
  "An Event trigger must return a boolean value (L2v1 Section 4.10.2).";


/**
 * Creates a new Constraint with the given constraint id.
 */
BooleanReturnFromEventTrigger::BooleanReturnFromEventTrigger (unsigned int id): GlobalConstraint(id)
{
    mHolds = false;
}


/**
 * Destroys this Constraint.
 */
BooleanReturnFromEventTrigger::~BooleanReturnFromEventTrigger ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
BooleanReturnFromEventTrigger::getPreamble ()
{
  return PREAMBLE;
}

/**
 * @return true if at all event triggers return boolean, false otherwise.
 */
bool
BooleanReturnFromEventTrigger::check (const Model& m)
{
  doCheck(m);
  return mHolds;
}

/**
 * Checks that all event trigger functions
 * on the following Model objects return a boolean
 * This supercedes Constraint 1801 as it is 
 * necessary to check the return type of a function definition
 * used as the event trigger function
 */
void
BooleanReturnFromEventTrigger::doCheck (const Model& m)
{
  unsigned int n, numEvents, p = 0;
  bool validOutput;

  numEvents = m.getNumEvents();
  for (n = 0; n < numEvents; ++n) 
  {
    Event *e = m.getEvent(n);
    if ( e->isSetTrigger())
    {
      const ASTNode *t = e->getTrigger();
      const ASTNodeType_t type = t->getType();
      const char *name = t->getName();
      if (!(t->isBoolean()))
      {
        // check whether the trigger function is a predefined
        // function definition that returns a boolean
        if (!(t->isFunction())) 
        {
          logFailure ("The event trigger is not a function.", e);
        }
        else 
        {
          validOutput = checkBooleanReturn(t->getChild(p), m, e);
          p++;
          while (validOutput && p < t->getNumChildren()) {
            validOutput = checkBooleanReturn(t->getChild(p), m, e);
            p++;
          }

        }// end of else isFunction
      }// end of isBoolean
    } // end of is setTrigger
  }// end of for loop
}

/**
 * Checks that a function returns a boolean
 * allows rescursive checks where a piecewise might
 * call a function definition etc...
 */
bool
BooleanReturnFromEventTrigger::checkBooleanReturn(ASTNode * function, const Model& m, Event* e)
{
  unsigned int fd, p;
  const ASTNodeType_t type = function->getType();
  const char *name = function->getName();
  bool validOutput = true;

  switch (type) {

    case AST_FUNCTION_PIECEWISE:
      p = 0;
      validOutput = checkBooleanReturn(function->getChild(p), m, e);
      
      while (validOutput && p < function->getNumChildren()) {
        p++;
        validOutput = checkBooleanReturn(function->getChild(p), m, e);
      }

      if (!validOutput)
        logFailure("The piecewise function does not return a boolean.", e);
    
    break;

    case AST_FUNCTION:
      for (fd = 0; fd < m.getNumFunctionDefinitions(); fd++)
      {
        FunctionDefinition *f = m.getFunctionDefinition(fd);

        if (name==f->getId())
        {
          if (!(((f->getMath())->getRightChild())->isBoolean()))
          {
            logFailure("The function definition used in the trigger does not return a boolean.", e);
            validOutput = false;
          }
        }
      }
    break;


    default:
      if (!(function->isBoolean()))
      {
        logFailure("This function does not return a boolean.",e);
        validOutput = false;
      }
    break;
  }// end of switch

  return validOutput;
} 

/**
  * Logs a message that the object has failed to satisfy this constraint.
  * for the reason specified
  */
void
BooleanReturnFromEventTrigger::logFailure (const std::string& fail, const SBase* object)
{
  const string& msg = getPreamble();

  string result;
  result = msg + fail;

  //
  // Here getId() refers to this Constraints unique identifier.
  //
  logMessage
  (
    ParseMessage(getId(), result, object->getLine(), object->getColumn())
  );

  mHolds = false;
}

