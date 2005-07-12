/**
 * \file    BooleanReturnFromEventTrigger.h
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
 *
 * Contributor(s):
 */


#ifndef BooleanReturnFromEventTrigger_h
#define BooleanReturnFromEventTrigger_h


#ifdef __cplusplus

#include "validator/GlobalConstraint.h"
#include <string>
#include <map>


class BooleanReturnFromEventTrigger: public GlobalConstraint
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  BooleanReturnFromEventTrigger (unsigned int id);

  /**
   * Destroys this Constraint.
   */
  virtual ~BooleanReturnFromEventTrigger ();
  /**
   * Resets the state of this GlobalConstraint by clearing its internal
   * list of error messages.
   */
 // virtual void reset ();


protected:

  /**
   * @return the preamble to use when logging constraint violations.
   */
  virtual const char* getPreamble ();

  /**
   * Checks that all event trigger functions
   * on the following Model objects return a boolean
   * This supercedes Constraint 1801 as it is 
   * necessary to check the return type of a function definition
   * used as the event trigger function
   */
  virtual void doCheck (const Model& m);

  /**
   * @return true if at all event triggers return boolean, false otherwise.
   */
  virtual bool check (const Model& m);
  
  /**
   * Logs a message that the object has failed to satisfy this constraint.
   * for the reason specified
   */
  void logFailure (const std::string& fail, const SBase* object);

 /**
  * Checks that a function returns a boolean
  * allows rescursive checks where a piecewise might
  * call a function definition etc...
  */
 bool checkBooleanReturn(ASTNode * function, const Model& m, Event * e);

};


#endif  /* __cplusplus */
#endif  /* BooleanReturnFromEventTrigger_h */
