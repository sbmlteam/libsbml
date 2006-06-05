/**
 * \file    UnitsBase.h
 * \brief   Ensures units consistent with math
 * \author  Sarah Keating
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


#ifndef UnitsBase_h
#define UnitsBase_h


#ifdef __cplusplus


#include <string>
#include <sbml/validator/Constraint.h>


class ASTNode;


class UnitsBase: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  UnitsBase (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UnitsBase ();


protected:

  /**
   * Checks that the units of the any math within the model
   * are appropriate for the function being performed
   */
  void check_(const Model& m, const Model& object);

  /**
   * Checks that the units of the ASTnode 
   * are appropriate for the function being performed
   *
   * If inconsistent units are found, an error message is logged.
   */
  virtual void checkUnits (const Model& m, const ASTNode& node, const SBase & sb) = 0;
 
  /**
   * Checks that the units of the ASTnode 
   * are appropriate for the function being performed
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkChildren (const Model& m, const ASTNode& node, const SBase & sb);
 
  /**
   * Checks that the units of function definition 
   * are appropriate for the function being performed
   * forces recursion through the AST tree
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkFunction (const Model& m, const ASTNode& node, 
    const SBase & sb);
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
  getMessage (const ASTNode& node, const SBase& object) = 0;

  /**
   * @return the typename of the given SBase object.
   */
  const char* getTypename (const SBase& object);

  /**
   * Logs a message that the given id (and its corresponding object) have
   * failed to satisfy this constraint.
   */
  void logUnitConflict (const ASTNode& node, const SBase& object);

  ///**
  // * Checks that the units of the children of ASTnode 
  // * are appropriate for the function being performed
  // * forces recursion through the AST tree
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkChildren (const Model& m, const ASTNode* node, 
  //  const SBase * sb);
  //
  ///**
  // * Checks that the units of function definition 
  // * are appropriate for the function being performed
  // * forces recursion through the AST tree
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkFunction (const Model& m, const ASTNode* node, 
  //  const SBase * sb);
 
  ///**
  // * Checks that the units of the power function are consistent
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkUnitsFromPower (const Model& m, const ASTNode* node, 
  //                            const SBase * sb);

  ///**
  // * Checks that the units of the root function are consistent
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkUnitsFromRoot (const Model& m, const ASTNode* node, 
  //                            const SBase * sb);

  ///**
  //  * Checks that the units of the delay function are consistent
  //  *
  //  * If inconsistent units are found, an error message is logged.
  //  */
  //void checkUnitsFromDelay (const Model& m, const ASTNode* node, 
  //                                        const SBase * sb);

  ///**
  //  * Checks that the units of the piecewise function are consistent
  //  *
  //  * If inconsistent units are found, an error message is logged.
  //  */
  //void checkUnitsFromPiecewise (const Model& m, const ASTNode* node, 
  //                                        const SBase * sb);

  ///**
  // * Checks that the units of the function are consistent
  // * for a function returning value with same units as argument(s)
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkSameUnitsAsArgs (const Model& m, const ASTNode* node, 
  //                            const SBase * sb);

  ///**
  // * Checks that the units of the arguments 
  // * of the function are dimensionless
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkDimensionlessArgs (const Model& m, const ASTNode* node, 
  //                            const SBase * sb);

  ///**
  // * Logs a message about an unknown astnode
  // */
  //void logUnknown (const ASTNode* node, const SBase *);
  //
  ///**
  //* Logs a message about a power function
  //*/
  //void logInconsistentPower (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about a root function
  //*/
  //void logInconsistentRoot (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about a function that should return same units
  //* as the arguments
  //*/
  //void logInconsistentSameUnits (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about a function that should have dmensionless
  //* as the arguments
  //*/
  //void logInconsistentDimensionless (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about a function that should 
  //* have only 1 or 2 arguments
  //*/
  //void logInconsistentNumber (const ASTNode * node, const SBase * sb,
  //                        int number);

  ///**
  //* Logs a message about a delay function that should have time units
  //*/
  //void logInconsistentDelay (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about a piecewise function that should same units
  //*/
  //void logInconsistentPiecewise (const ASTNode * node, const SBase * sb);

  ///**
  //* Logs a message about the conditional part of a piecewise function 
  //* that should have dimensionless units
  //*/
  //void logInconsistentPiecewiseCondition (const ASTNode * node, 
  //                                                  const SBase * sb);

};


#endif  /* __cplusplus */
#endif  /* UnitsBase_h */
