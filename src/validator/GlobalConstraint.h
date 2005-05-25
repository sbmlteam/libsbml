/**
 * \file    GlobalConstraint.h
 * \brief   Applies to an SBML Model and can log multiple error messages.
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
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef GlobalConstraint_h
#define GlobalConstraint_h


#ifdef __cplusplus


#include <list>

#include "../xml/ParseMessage.h"
#include "Constraint.h"


class Model;


/**
 * Unlike a LocalConstraint, which checks a single SBML object and logs a
 * single error message, a GlobalConstraint applies to the entire SBML
 * model and may log multiple error messages.
 */
class GlobalConstraint: public Constraint
{
public:

  /**
   * Creates a new GlobalConstraint with the given id.
   */
  GlobalConstraint (unsigned int id) : Constraint(id) { }

  /**
   * Destroys this GlobalConstraint.
   */
  virtual ~GlobalConstraint () { };


  /**
   * @return true if this contraint holds when applied to the given SBML
   * Model.
   */
  bool holds (const Model& m) { return check(m); }

  /**
   * @return a list of error messages logged by this GlobalConstraint.
   */
  const std::list<ParseMessage>& getMessages () const { return mMessages; }

  /**
   * Resets the state of this GlobalConstraint by clearing its internal
   * list of error messages.
   */
  virtual void reset () { mHolds = true; mMessages.clear(); }


protected:

  /**
   * The holds method delegates to this pure virtual method.
   */
  virtual bool check (const Model& m) { return true; }

  /**
   * Adds the given message to this list of messages.
   */
  void logMessage (const ParseMessage& msg) { mMessages.push_back(msg); }


  std::list<ParseMessage> mMessages;
};


#endif  /* __cplusplus */
#endif  /* GlobalConstraint_h */
