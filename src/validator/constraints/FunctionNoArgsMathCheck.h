/**
 * @cond doxygen-libsbml-internal
 *
 * @file    FunctionNoArgsMathCheck.h
 * @brief   Ensures correct number of arguments to a function definition.
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
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


#ifndef FunctionNoArgsMathCheck_h
#define FunctionNoArgsMathCheck_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <math.h>

#include <sbml/validator/VConstraint.h>

#include "MathMLBase.h"

LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;


class FunctionNoArgsMathCheck: public MathMLBase
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  FunctionNoArgsMathCheck (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~FunctionNoArgsMathCheck ();


protected:

  /**
   * Checks the MathML of the ASTnode 
   * is appropriate for the function being performed
   *
   * If an inconsistency is found, an error message is logged.
   */
  virtual void checkMath (const Model& m, const ASTNode& node, const SBase & sb);
  
  /**
   * @return the preamble to use when logging constraint violations.  The
   * preamble will be prepended to each log message.  If not overriden,
   * returns an empty string.
   */
  virtual const char* getPreamble ();

  /**
   * Checks that the functionDefinition referred to by a <ci> element 
   * has the appropriate number of arguments.
   *
   * If not, an error message is logged.
   */
  void checkNumArgs (const Model& m, const ASTNode& node, const SBase & sb);

  /**
   * @return the error message to use when logging constraint violations.
   * This method is called by logFailure.
   *
   * If at all possible please use getPreamble() and getFieldname() when
   * constructing error messages.  This will help to make your constraint
   * easily customizable.
   */
  virtual const std::string
  getMessage (const ASTNode& node, const SBase& object);

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* FunctionNoArgsMathCheck_h */

/** @endcond */
