/**
 * @file    AssignmentRuleOrdering.h
 * @brief   Checks rule ordering for l2v1 and l1
 * @author  Sarah Keating
 *
 * $Id: AssignmentRuleOrdering.h 7249 2008-06-26 22:48:40Z mhucka $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/validator/constraints/AssignmentRuleOrdering.h $
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


#ifndef AssignmentRuleOrdering_h
#define AssignmentRuleOrdering_h


#ifdef __cplusplus

#include <string>
#include <sbml/validator/VConstraint.h>

#include "IdList.h"


class AssignmentRuleOrdering: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  AssignmentRuleOrdering (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~AssignmentRuleOrdering ();


protected:

  /**
   * Checks that <ci> element after an apply is already listed as a FunctionDefinition.
   */
  virtual void check_ (const Model& m, const Model& object);

  void checkRuleForVariable(const Model &, const Rule &);
  void checkRuleForLaterVariables(const Model &, const Rule &,
                                                        unsigned int);
  /**
   * Logs a message about an undefined <ci> element in the given
   * FunctionDefinition.
   */
void logForwardReference (const ASTNode & node,
                                             const SBase& object,
                                             std::string name);
  void logRuleRefersToSelf ( const ASTNode & node, const SBase& object);

  IdList mVariables;
  IdList mTempList;
  IdList mVariableList;

};


#endif  /* __cplusplus */
#endif  /* AssignmentRuleOrdering_h */
