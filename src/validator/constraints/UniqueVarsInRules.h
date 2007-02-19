/**
 * \file    UniqueVarsInRules.h
 * \brief   Ensures variables for all Rules are unique
 * \author  Ben Bornstein
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


#ifndef UniqueVarsInRules_h
#define UniqueVarsInRules_h


#ifdef __cplusplus

#include <string>

#include "UniqueIdBase.h"


class UniqueVarsInRules: public UniqueIdBase
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueVarsInRules (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueVarsInRules ();


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
#endif  /* UniqueVarsInRules_h */
