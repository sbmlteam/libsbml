/**
 * Filename    : RateRule.hpp
 * Description : SBML RateRule
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-40-29
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#ifndef RateRule_hpp
#define RateRule_hpp


#include <string>

#include "extern.h"
#include "Rule.hpp"


class SBMLVisitor;


class RateRule : public Rule
{
public:

  /**
   * Creates a new RateRule, optionally with its variable and math (via
   * an infix formula string) attributes set.
   */
  LIBSBML_EXTERN
  RateRule (const std::string& variable = "", const std::string& formula = "");


  /**
   * Creates a new RateRule with its variable and math attributes set.
   */
  LIBSBML_EXTERN
  RateRule (const std::string& variable, ASTNode* math);

  /**
   * Destroys this RateRule.
   */
  LIBSBML_EXTERN
  virtual ~RateRule ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  LIBSBML_EXTERN
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return the variable for this RateRule.
   */
  LIBSBML_EXTERN
  const std::string& getVariable () const;

  /**
   * @return true if the variable of this RateRule has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetVariable () const;

  /**
   * Sets the variable of this RateRule to a copy of sid.
   */
  LIBSBML_EXTERN
  void setVariable (const std::string& sid);


protected:

  std::string variable;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // RateRule_hpp
