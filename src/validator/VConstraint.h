/**
 * \file    Constraint.h
 * \brief   Base class for all SBML Validator Constraints
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
   * @return the id of this Constraint.
   */
  unsigned int getId () const;

  /**
   * @return the severity for violating this Constraint.
   */
  unsigned int getSeverity () const;


protected:

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
};


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


#endif  /* __cplusplus  */
#endif  /* Validator_Constraint_h */
