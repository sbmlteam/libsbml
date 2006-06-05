/**
 * \file    AssignmentCycles.h
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


#ifndef AssignmentCycles_h
#define AssignmentCycles_h


#ifdef __cplusplus

#include <sbml/validator/Constraint.h>

#include "IdList.h"


class AssignmentCycles: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  AssignmentCycles (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~AssignmentCycles ();


protected:

  /**
   * Checks that <ci> element after an apply is already listed as a FunctionDefinition.
   */
  virtual void check_ (const Model& m, const Model& object);

  void checkInitialAssignment(const Model &, const InitialAssignment &);
  void checkReaction(const Model &, const Reaction &);
  void checkRule(const Model &, const Rule &);
  /**
   * Logs a message about an undefined <ci> element in the given
   * FunctionDefinition.
   */
  void logUndefined (const SBase& object, const SBase& conflict);

  IdList mVariables;
  IdList mTempList;
  IdList mCheckedList;

};


#endif  /* __cplusplus */
#endif  /* AssignmentCycles_h */
