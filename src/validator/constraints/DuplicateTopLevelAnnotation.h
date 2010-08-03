/**
 * @cond doxygen-libsbml-internal
 *
 * @file    DuplicateTopLevelAnnotation.h
 * @brief   Checks for duplicate top level annotations
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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


#ifndef DuplicateTopLevelAnnotation_h
#define DuplicateTopLevelAnnotation_h


#ifdef __cplusplus

#include <string>

#include <sbml/validator/VConstraint.h>

#include "IdList.h"

LIBSBML_CPP_NAMESPACE_BEGIN

class DuplicateTopLevelAnnotation: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  DuplicateTopLevelAnnotation (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~DuplicateTopLevelAnnotation ();


protected:

  /**
   * Checks that <ci> element after an apply is already listed as a FunctionDefinition.
   */
  virtual void check_ (const Model& m, const Model& object);

  /**
   * Checks that <ci> element after an apply is already listed as a FunctionDefinition.
   */
  void checkAnnotation(const SBase& object);

  /**
   * Logs a message about an undefined <ci> element in the given
   * FunctionDefinition.
   */
  void logDuplicate (const std::string name, const SBase& object);

  IdList mNamespaces;

};

LIBSBML_CPP_NAMESPACE_END
#endif  /* __cplusplus */
#endif  /* DuplicateTopLevelAnnotation_h */

/** @endcond */
