/**
 * \file    UniqueMetaId.h
 * \brief   Base class for Id constraints
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


#ifndef UniqueMetaId_h
#define UniqueMetaId_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <map>

#include <sbml/validator/Constraint.h>


class SBase;
class Validator;


/**
 * The UniqueMetaId Cosntraint is the base class for all SBML id constraints.  It
 * provides mechanisms for checking only certain subsets of ids within an
 * SBML model and tailoring the error messages logged.
 *
 * To customize:
 *
 *   1.  Override doCheck() to iterate over the SBML objects you are
 *       interested in and call checkId() for each.
 *
 *       checkId() does the work of extracting the unique identifier
 *       (whether it be an id or variable name) from the SBML object and
 *       then delegates to doCheckId().
 *
 *   2.  Override doCheckId() to perform the actual check.  If the check
 *       fails, call logFailure().
 *
 *   3.  Override getMesage() to log error messages.  GetMessage() should
 *       use getPreamble() and getFieldName() when constructing the error
 *       message.
 *
 *   4.  Override getPreabmle() to customize the part of the actual error
 *       message that remains constant (e.g. the part that doesn't report
 *       line numbers, SBML object ids and typenames, etc).
 *
 *   5.  Override getFieldName() if you are checking a field that isn't
 *       called 'id', for instance, the 'variable' field of a Rule.
 *
 * Finally, if you need the type name of the SBML object that failed,
 * e.g. 'Compartment' or 'Species', when constructing an error message,
 * call getTypename().
 */
class UniqueMetaId: public TConstraint<Model>
{
public:

  /**
   * Creates a new UniqueMetaId with the given constraint id.
   */
  UniqueMetaId (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueMetaId ();


protected:
  /**
   * Resets the state of this GlobalConstraint by clearing its internal
   * list of error messages.
   */
  void reset ();

  /**
   * Called by check().  Override this method to define your own subset.
   */
  void doCheck (const Model& m);

  /**
   * Checks that the id associated with the given object adheres to this
   * constraint.  If it does not, logFailure is called.
   */
  void doCheckId (const std::string& id, const SBase& object);

  /**
   * @return the fieldname to use logging constraint violations.  If not
   * overridden, "id" is returned.
   */
  const char* getFieldname ();

  /**
   * @return the preamble to use when logging constraint violations.  The
   * preamble will be prepended to each log message.  If not overriden,
   * returns an empty string.
   */
  const char* getPreamble ();

  /**
   * @return the error message to use when logging constraint violations.
   * This method is called by logFailure.
   *
   * If at all possible please use getPreamble() and getFieldname() when
   * constructing error messages.  This will help to make your constraint
   * easily customizable.
   */
  const std::string
  getMessage (const std::string& id, const SBase& object);



  /* ------------------------------------------------------------ */
  /*  You should not need to override methods beyond this point.  */
  /* ------------------------------------------------------------ */


  /**
   * Checks that all ids for some given subset of the Model adhere to this
   * Constraint.  Override the doCheck() method to define your own subset.
   */
  virtual void check_ (const Model& m, const Model& object);

  /**
   * Checks that the id associated with the given object is unique.  If it
   * is not, logFailure is called.
   */
  void doCheckMetaId (const SBase& object);


  /**
   * @return the typename of the given SBase object.
   */
  const char* getTypename (const SBase& object);

  /**
   * Logs a message that the given id (and its corresponding object) have
   * failed to satisfy this constraint.
   */
  void logIdConflict (const std::string& id, const SBase& object);


  typedef std::map<const std::string, const SBase*> IdObjectMap;
  IdObjectMap mMetaIdObjectMap;
};


#endif  /* __cplusplus */
#endif  /* UniqueMetaId_h */
