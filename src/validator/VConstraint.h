/**
 * @file    VConstraint.h
 * @brief   Base class for all SBML Validator Constraints
 * @author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/


#ifndef Validator_Constraint_h
#define Validator_Constraint_h


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/validator/Validator.h>
#include <sbml/units/UnitFormulaFormatter.h>


class Model;


class VConstraint
{
public:

  VConstraint (unsigned int id, Validator& v);
  virtual ~VConstraint ();


  /**
   * Get the constraint identifier of this constraint.
   *
   * Note that constraint identifiers are unrelated to SBML identifiers put
   * on SBML components.  Constraint identifiers are a superset of the
   * validation rule numbers. (These "validation rules" are defined in the
   * SBML specifications beginning with SBML Level&nbsp;2 Version&nbsp;2.)
   * The set of possible constraint identifiers includes all SBML
   * validation rule numbers, and in addition, there exist extra
   * constraints defined by libSBML itself.
   * 
   * @return the id of this Constraint.
   */
  unsigned int getId () const;


  /**
   * Get the severity of this constraint.
   *
   * Severity codes are defined by the enumeration SBMLErrorSeverity_t.
   * See the documentation included in SBMLError for more information.
   * 
   * @return the severity for violating this Constraint.
   *
   * @see SBMLErrorSeverity_t
   */
  unsigned int getSeverity () const;


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Logs a constraint failure to the validator for the given SBML object.
   */
  void logFailure (const SBase& object);

  /**
   * Logs a constraint failure to the validator for the given SBML object.
   * The parameter message is used instead of the constraint's member
   * variable msg.
   */
  void logFailure (const SBase& object, const std::string& message);


  unsigned int mId;
  unsigned int mSeverity;
  Validator&   mValidator;
  bool         mLogMsg;

  std::string  msg;

  /** @endcond doxygen-libsbml-internal */
};


/** @cond doxygen-libsbml-internal */

template <typename T>
class TConstraint : public VConstraint
{
public:

  TConstraint (unsigned int id, Validator& v) : VConstraint(id, v) { }
  virtual ~TConstraint () { };

  /**
   * Checks to see if this Contraint holds when applied to the given SBML
   * object of type T.  The Model object should contain object and is
   * passed in to provide additional context information, should the
   * contraint need it.
   */
  void check (const Model& m, const T& object)
  {
    mLogMsg = false;

    check_(m, object);

    if (mLogMsg) logFailure(object);
  }


protected:

  /**
   * The check method delegates to this virtual method.
   */
  virtual void check_ (const Model& m, const T& object) { };

};

/** @endcond doxygen-libsbml-internal */


#endif  /* __cplusplus  */
#endif  /* Validator_Constraint_h */
