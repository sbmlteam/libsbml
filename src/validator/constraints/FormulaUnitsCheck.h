/**
 * \file    FormulaUnitsCheck.h
 * \brief   Ensures units consistent with math
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *
 *     The SBML Team
 *     Science and Technology Research Institute
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef FormulaUnitsCheck_h
#define FormulaUnitsCheck_h


#ifdef __cplusplus


#include <string>
#include "validator/Constraint.h"


class ASTNode;


class FormulaUnitsCheck: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  FormulaUnitsCheck (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~FormulaUnitsCheck ();


protected:

  /**
   * Checks that the units of the any math within the model
   * are appropriate for the function being performed
   */
  virtual void check_(const Model& m, const Model& object);

  /**
   * Checks that the units of the ASTnode 
   * are appropriate for the function being performed
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkUnits (const Model& m, const ASTNode* node, const SBase * sb);
  
  /**
   * Checks that the units of the children of ASTnode 
   * are appropriate for the function being performed
   * forces recursion through the AST tree
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkChildren (const Model& m, const ASTNode* node, 
    const SBase * sb);
  
  /**
   * Checks that the units of function definition 
   * are appropriate for the function being performed
   * forces recursion through the AST tree
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkFunction (const Model& m, const ASTNode* node, 
    const SBase * sb);
 
  /**
   * Checks that the units of the power function are consistent
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkUnitsFromPower (const Model& m, const ASTNode* node, 
                              const SBase * sb);

  /**
   * Checks that the units of the root function are consistent
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkUnitsFromRoot (const Model& m, const ASTNode* node, 
                              const SBase * sb);

  /**
    * Checks that the units of the delay function are consistent
    *
    * If inconsistent units are found, an error message is logged.
    */
  void checkUnitsFromDelay (const Model& m, const ASTNode* node, 
                                          const SBase * sb);

  /**
    * Checks that the units of the piecewise function are consistent
    *
    * If inconsistent units are found, an error message is logged.
    */
  void checkUnitsFromPiecewise (const Model& m, const ASTNode* node, 
                                          const SBase * sb);

  /**
   * Checks that the units of the function are consistent
   * for a function returning value with same units as argument(s)
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkSameUnitsAsArgs (const Model& m, const ASTNode* node, 
                              const SBase * sb);

  /**
   * Checks that the units of the arguments 
   * of the function are dimensionless
   *
   * If inconsistent units are found, an error message is logged.
   */
  void checkDimensionlessArgs (const Model& m, const ASTNode* node, 
                              const SBase * sb);

  /**
   * Logs a message about an unknown astnode
   */
  void logUnknown (const ASTNode* node, const SBase *);
  
  /**
  * Logs a message about a power function
  */
  void logInconsistentPower (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about a root function
  */
  void logInconsistentRoot (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about a function that should return same units
  * as the arguments
  */
  void logInconsistentSameUnits (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about a function that should have dmensionless
  * as the arguments
  */
  void logInconsistentDimensionless (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about a function that should 
  * have only 1 or 2 arguments
  */
  void logInconsistentNumber (const ASTNode * node, const SBase * sb,
                          int number);

  /**
  * Logs a message about a delay function that should have time units
  */
  void logInconsistentDelay (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about a piecewise function that should same units
  */
  void logInconsistentPiecewise (const ASTNode * node, const SBase * sb);

  /**
  * Logs a message about the conditional part of a piecewise function 
  * that should have dimensionless units
  */
  void logInconsistentPiecewiseCondition (const ASTNode * node, 
                                                    const SBase * sb);

};


#endif  /* __cplusplus */
#endif  /* FormulaUnitsCheck_h */
