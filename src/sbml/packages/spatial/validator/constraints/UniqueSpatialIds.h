/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueSpatialIds.h
 * @brief   Unique values for spatial id
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 * 
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#ifndef UniqueSpatialIds_h
#define UniqueSpatialIds_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <map>

#include <sbml/validator/VConstraint.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBase;
class Validator;


class UniqueSpatialIds: public TConstraint<Model>
{
public:

  /**
   * Creates a new UniqueSpatialIds with the given constraint id.
   */
  UniqueSpatialIds (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueSpatialIds ();


protected:

  /**
   * Check CSGObjects
   */
  void checkCSG(const CSGNode& csg);

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
  void logId (const SBase& object);


  /**
   * Returns the error message to use when logging constraint violations.
   * This method is called by logFailure.
   *
   * If at all possible please use getPreamble() and getFieldname() when
   * constructing error messages.  This will help to make your constraint
   * easily customizable.
   *
   * @return the error message to use when logging constraint violations.
   *
   * @see getPreamble()
   * @see getFieldname()
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
  void doCheckId (const SBase& object);


  /**
   * Returns a non-owning character pointer to the typename of the given SBase 
   * @p object, as constructed from its typecode and package.
   *
   * @return the typename of the given SBase object.
   */
  const char* getTypename (const SBase& object);

  /**
   * Logs a message that the given @p id (and its corresponding object) have
   * failed to satisfy this constraint.
   */
  void logIdConflict (const std::string& id, const SBase& object);


  typedef std::map<std::string, const SBase*> IdObjectMap;
  IdObjectMap mIdMap;
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* UniqueSpatialIds_h */
/** @endcond */
