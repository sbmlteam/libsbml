/**
 * Filename    : CompartmentVolumeRule.hpp
 * Description : SBML CompartmentVolumeRule
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


#ifndef CompartmentVolumeRule_hpp
#define CompartmentVolumeRule_hpp


#include <string>

#include "extern.h"
#include "AssignmentRule.hpp"


class CompartmentVolumeRule : public AssignmentRule
{
public:

  /**
   * Creates a new CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  CompartmentVolumeRule ();

  /**
   * Creates a new CompartmentVolumeRule with its compartment, formula and
   * (optionally) type attributes set.
   */
  LIBSBML_EXTERN
  CompartmentVolumeRule
  (
      const std::string&  compartment
    , const std::string&  formula 
    , RuleType_t          type = RULE_TYPE_SCALAR
  );

  /**
   * Destroys this CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  virtual ~CompartmentVolumeRule ();

  /**
   * @return the compartment of this CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  const std::string& getCompartment () const;

  /**
   * @return true if the compartment of this CompartmentVolumeRule has been
   * set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetCompartment () const;

  /**
   * Sets the compartment of this CompartmentVolumeRule to a copy of sname.
   */
  LIBSBML_EXTERN
  void setCompartment (const std::string& sname);


protected:


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // CompartmentVolumeRule_hpp
