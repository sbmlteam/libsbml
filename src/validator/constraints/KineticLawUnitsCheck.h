/**
 * @file    KineticLawUnitsCheck.h
 * @brief   Ensures units consistent with math
 * @author  Sarah Keating
 *
 * $Id: KineticLawUnitsCheck.h 10131 2009-08-28 12:27:16Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/validator/constraints/KineticLawUnitsCheck.h $
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


#ifndef KineticLawUnitsCheck_h
#define KineticLawUnitsCheck_h


#ifdef __cplusplus


#include <string>
#include <math.h>

#include <sbml/validator/VConstraint.h>

#include <sbml/util/memory.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;


class KineticLawUnitsCheck: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  KineticLawUnitsCheck (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~KineticLawUnitsCheck ();


protected:

  /**
   * Checks that the units of the any math within the model
   * are appropriate for the function being performed
   */
  virtual void check_(const Model& m, const Model& object);

 
 
  /**
   * @return the fieldname to use logging constraint violations.  If not
   * overridden, "id" is returned.
   */
  virtual const char* getFieldname ();

  /**
   * @return the preamble to use when logging constraint violations.  The
   * preamble will be prepended to each log message.  If not overriden,
   * returns an empty string.
   */
  virtual const char* getPreamble ();

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

  /**
   * @return the typename of the given SBase object.
   */
  const char* getTypename (const SBase& object);

  /**
   * Logs a message that the given id (and its corresponding object) have
   * failed to satisfy this constraint.
   */
  void logKLConflict (const ASTNode& node, const SBase& object);

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* KineticLawUnitsCheck_h */
