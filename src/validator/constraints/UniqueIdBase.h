/**
 * \file    UniqueIdBase.cpp
 * \brief   Base class for unique id constraints
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


#ifndef UniqueIdBase_h
#define UniqueIdBase_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <map>

#include "IdBase.h"


class SBase;
class Validator;


class UniqueIdBase: public IdBase
{
public:

  /**
   * Creates a new UniqueIdBase with the given constraint id.
   */
  UniqueIdBase (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueIdBase ();


protected:

  /**
   * Resets the state of this GlobalConstraint by clearing its internal
   * list of error messages.
   */
  void reset ();

  /**
   * Called by check().  Override this method to define your own subset.
   */
  virtual void doCheck (const Model& m) = 0;

  /**
   * Checks that the id associated with the given object is unique.  If it
   * is not, logFailure is called.
   */
  void doCheckId (const std::string& id, const SBase& object);


  /**
   * @return the error message to use when logging constraint violations.
   * This method is called by logFailure.
   *
   * Returns a message that the given id and its corresponding object are
   * in  conflict with an object previously defined.
   */
  virtual const std::string
  getMessage (const std::string& id, const SBase& object);


  typedef std::map<std::string, const SBase*> IdObjectMap;
  IdObjectMap mIdObjectMap;
};


#endif  /* __cplusplus */
#endif  /* UniqueIdBase_h */
