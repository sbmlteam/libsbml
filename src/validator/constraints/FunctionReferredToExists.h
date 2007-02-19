/**
 * \file    FunctionReferredToExists.h
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


#ifndef FunctionReferredToExists_h
#define FunctionReferredToExists_h


#ifdef __cplusplus

#include <string>

#include <sbml/validator/Constraint.h>

#include "IdList.h"


class FunctionReferredToExists: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  FunctionReferredToExists (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~FunctionReferredToExists ();


protected:

  /**
   * Checks that <ci> element after an apply is already listed as a FunctionDefinition.
   */
  virtual void check_ (const Model& m, const Model& object);

  /**
   * Logs a message about an undefined <ci> element in the given
   * FunctionDefinition.
   */
  void logUndefined (const FunctionDefinition& fd, const std::string& varname);

  IdList mFunctions;

};


#endif  /* __cplusplus */
#endif  /* FunctionReferredToExists_h */
