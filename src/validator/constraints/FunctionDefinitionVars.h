/**
 * @file    FunctionDefinitionVars.h
 * @brief   Ensures FunctionDefinitions contain no undefined variables.
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


#ifndef FunctionDefinitionVars_h
#define FunctionDefinitionVars_h


#ifdef __cplusplus


#include <string>
#include <sbml/validator/VConstraint.h>


class FunctionDefinition;


class FunctionDefinitionVars: public TConstraint<FunctionDefinition>
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  FunctionDefinitionVars (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~FunctionDefinitionVars ();


protected:

  /**
   * Checks that all variables referenced in FunctionDefinition bodies are
   * bound variables (function arguments).
   */
  virtual void check_ (const Model& m, const FunctionDefinition& object);

  /**
   * Logs a message about an undefined variable in the given
   * FunctionDefinition.
   */
  void logUndefined (const FunctionDefinition& fd, const std::string& varname);
};


#endif  /* __cplusplus */
#endif  /* FunctionDefinitionVars_h */
