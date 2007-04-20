/**
 * @file    UniqueVarsInInitialAssignmentsAndRules.h
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
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


#ifndef UniqueVarsInInitialAssignmentsAndRules_h
#define UniqueVarsInInitialAssignmentsAndRules_h


#ifdef __cplusplus

#include <string>

#include "UniqueIdBase.h"


class UniqueVarsInInitialAssignmentsAndRules: public UniqueIdBase
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueVarsInInitialAssignmentsAndRules (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueVarsInInitialAssignmentsAndRules ();


protected:

  /**
   * @return the preamble to use when logging constraint violations.
   */
  virtual const char* getPreamble ();

  /**
   * Checks that all ids on the following Model objects are unique:
   * FunctionDefinitions, Species, Compartments, global Parameters,
   * Reactions, and Events.
   */
  virtual void doCheck (const Model& m);
};


#endif  /* __cplusplus */
#endif  /* UniqueVarsInInitialAssignmentsAndRules_h */
