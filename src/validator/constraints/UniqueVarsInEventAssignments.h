/**
 * @file    UniqueVarsInEventAssignments.h
 * @brief   Ensures variables within a set of EventAssignments are unique
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



#ifndef UniqueVarsInEventAssignments_h
#define UniqueVarsInEventAssignments_h


#ifdef __cplusplus

#include <string>

#include "UniqueIdBase.h"


class UniqueVarsInEventAssignments: public UniqueIdBase
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueVarsInEventAssignments (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueVarsInEventAssignments ();


protected:

  /**
   * @return the fieldname ("variable") to use when logging constraint
   * violations.
   */
  virtual const char* getFieldname ();

  /**
   * @return the preamble to use when logging constraint violations.
   */
  virtual const char* getPreamble ();

  /**
   * Checks that all ids on KineticLaw Parameters are unique.
   */
  virtual void doCheck (const Model& m);
};


#endif  /* __cplusplus */
#endif  /* UniqueVarsInEventAssignments_h */
