/**
 * Filename    : SpeciesReference.hpp
 * Description : SBML SpeciesReference
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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


#ifndef SpeciesReference_hpp
#define SpeciesReference_hpp


#include <string>

#include "extern.h"
#include "SimpleSpeciesReference.hpp"
#include "ASTNode.hpp"


class SpeciesReference : public SimpleSpeciesReference
{
public:

  /**
   * Creates a new SpeciesReference, optionally with its species,
   * stoichiometry, and denominator attributes set.
   */
  LIBSBML_EXTERN
  SpeciesReference (   const std::string& species       = ""
                     , double             stoichiometry = 1.0
                     , int                denominator   = 1   );

  /**
   * Destroys this SpeciesReference.
   */
  LIBSBML_EXTERN
  virtual ~SpeciesReference ();


  /**
   * Initializes the fields of this SpeciesReference to their defaults:
   *
   *   - stoichiometry = 1
   *   - denominator   = 1
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the stoichiometry of this SpeciesReference.
   */
  LIBSBML_EXTERN
  double getStoichiometry () const;

  /**
   * @return the stoichiometryMath of this SpeciesReference.
   */
  LIBSBML_EXTERN
  const ASTNode* getStoichiometryMath () const;

  /**
   * @return the denominator of this SpeciesReference.
   */
  LIBSBML_EXTERN
  int SpeciesReference::getDenominator () const;

  /**
   * @return true if the stoichiometryMath of this SpeciesReference has
   * been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetStoichiometryMath () const;

  /**
   * Sets the stoichiometry of this SpeciesReference to value.
   */
  LIBSBML_EXTERN
  void setStoichiometry (double value);

  /**
   * Sets the stoichiometryMath of this SpeciesReference to the given
   * ASTNode.
   *
   * The node <b>is not copied</b> and this SpeciesReference <b>takes
   * ownership</b> of it; i.e. subsequent calls to this function or a call
   * to SpeciesReference_free() will free the ASTNode (and any child
   * nodes).
   */
  LIBSBML_EXTERN
  void setStoichiometryMath (ASTNode* math);

  /**
   * Sets the stoichiometryMath of this SpeciesReference to the given
   * formula string.
   */
  LIBSBML_EXTERN
  void setStoichiometryMath (const std::string& formula);

  /**
   * Sets the denominator of this SpeciesReference to value.
   */
  LIBSBML_EXTERN
  void setDenominator (int value);


protected:

  double    stoichiometry;
  int       denominator;
  ASTNode*  stoichiometryMath;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // SpeciesReference_hpp
