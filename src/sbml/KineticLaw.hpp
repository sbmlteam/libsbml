/**
 * Filename    : KineticLaw.hpp
 * Description : SBML KineticLaw
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-25
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef KineticLaw_hpp
#define KineticLaw_hpp


#include <string>

#include "extern.h"

#include "SBase.hpp"
#include "ListOf.hpp"
#include "Parameter.hpp"
#include "ASTNode.h"


class KineticLaw : public SBase
{
public:

  /**
   * Creates a new KineticLaw, optionally with its formula, timeUnits
   * and/or substanceUnits set.
   */
  LIBSBML_EXTERN
  KineticLaw (   const std::string& formula        = ""
               , const std::string& timeUnits      = ""
               , const std::string& substanceUnits = "" );

  /**
   * Destroys this KineticLaw.
   */
  LIBSBML_EXTERN
  virtual ~KineticLaw ();


  /**
   * @return the formula of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getFormula () const;

  /**
   * @return the math of this KineticLaw.
   */
  LIBSBML_EXTERN
  const ASTNode_t * getMath () const;

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  LIBSBML_EXTERN
  ListOf& getListOfParameters ();

  /**
   * @return the timeUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getTimeUnits () const;

  /**
   * @return the substanceUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getSubstanceUnits () const;

  /**
   * @return true if the formula of this KineticLaw has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetFormula () const;

  /**
   * @return true if the math of this KineticLaw has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * @return true if the timeUnits of this KineticLaw has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetTimeUnits () const;

  /**
   * @return true if the substanceUnits of this KineticLaw has been set,
   * false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSubstanceUnits () const;

  /**
   * Sets the formula of this KineticLaw to a copy of string.
   */
  LIBSBML_EXTERN
  void setFormula (const std::string& formula);

  /**
   * Sets the formula of this KineticLaw based on the current value of its
   * math field.  This convenience method is equivalent to:
   *
   *   setFormula( SBML_formulaToString( getMath() ))
   *
   * except you do not need to track and free the value returned by
   * SBML_formulaToString().
   *
   * If !isSetMath(), this method has no effect.
   */
  LIBSBML_EXTERN
  void setFormulaFromMath ();

  /**
   * Sets the math of this KineticLaw to the given ASTNode.
   *
   * The node <b>is not copied</b> and this KineticLaw <b>takes
   * ownership</b> of it; i.e. subsequent calls to this method or deleting
   * this KineticLaw will delete the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode_t *math);

  /**
   * Sets the math of this KineticLaw from its current formula string.
   * This convenience method is equivalent to:
   *
   *   setMath( SBML_parseFormula( getFormula() ))
   *
   * If !isSetFormula(), this method has no effect.
   */
  LIBSBML_EXTERN
  void setMathFromFormula ();

  /**
   * Sets the timeUnits of this KineticLaw to a copy of sname.
   */
  LIBSBML_EXTERN
  void setTimeUnits (const std::string& sname);

  /**
   * Sets the substanceUnits of this KineticLaw to a copy of sname.
   */
  LIBSBML_EXTERN
  void setSubstanceUnits (const std::string& sname);

  /**
   * Adds the given Parameter to this KineticLaw.
   */
  LIBSBML_EXTERN
  void addParameter (Parameter& p);

  /**
   * @return the nth Parameter of this KineticLaw.
   */
  LIBSBML_EXTERN
  Parameter* getParameter (unsigned int n) const;

  /**
   * @return the number of Parameters in this KineticLaw.
   */
  LIBSBML_EXTERN
  unsigned int getNumParameters () const;

  /**
   * Unsets the timeUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  void unsetTimeUnits ();

  /**
   * Unsets the substanceUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  void unsetSubstanceUnits ();


protected:

  std::string formula;
  ASTNode_t*  math;
  ListOf      parameter;
  std::string timeUnits;
  std::string substanceUnits;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // KineticLaw_hpp
