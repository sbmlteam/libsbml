/**
 * Filename    : Rule.cpp
 * Description : SBML Rule
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-26
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Rule_hpp
#define Rule_hpp


#include <string>

#include "extern.h"

#include "SBase.hpp"
#include "ASTNode.hpp"


class Rule : public SBase
{
public:

  /**
   * Creates a new Rule, optionally with its formula attribute set.
   */
  LIBSBML_EXTERN
  Rule (const std::string& formula = "");

  /**
   * Creates a new Rule with its math attribute set.
   */
  LIBSBML_EXTERN
  Rule (ASTNode* math);

  /**
   * Destroys this Rule.
   */
  LIBSBML_EXTERN
  virtual ~Rule ();


  /**
   * @return the formula for this Rule.
   */
  LIBSBML_EXTERN
  const std::string& getFormula () const;

  /**
   * @return the math for this Rule.
   */
  LIBSBML_EXTERN
  const ASTNode* getMath () const;

  /**
   * @return true if the formula for this Rule has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetFormula () const;

  /**
   * @return true if the math for this Rule has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * Sets the formula of this Rule to a copy of string.
   */
  LIBSBML_EXTERN
  void setFormula (const std::string& str);

  /**
   * Sets the formula of this Rule based on the current value of its math
   * field.  This convenience method is equivalent to:
   *
   *   setFormula( SBML_formulaToString( getMath() ))
   *
   * except you do not need to track and free the value returned by
   * SBML_formulaToString().
   *
   * If !isSetMath(r), this method has no effect.
   */
  LIBSBML_EXTERN
  void setFormulaFromMath ();

  /**
   * Sets the math of this Rule to the given ASTNode.
   *
   * The node <b>is not copied</b> and this Rule <b>takes ownership</b> of
   * it; i.e. subsequent calls to this method or deleting this Rule will
   * delete the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode *math);

  /**
   * Sets the math of this Rule from its current formula string.  This
   * convenience method is equivalent to:
   *
   *   setMath( SBML_parseFormula( getFormula() ))
   *
   * If !isSetFormula(), this method has no effect.
   */
  LIBSBML_EXTERN
  void setMathFromFormula ();


protected:

  std::string formula;
  ASTNode*    math;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Rule_hpp
