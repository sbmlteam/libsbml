/**
 * Filename    : SimpleSpeciesReference.hpp
 * Description : SBML SimpleSpeciesReference
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-29
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


#ifndef SimpleSpeciesReference_hpp
#define SimpleSpeciesReference_hpp


#include <string>

#include "extern.h"
#include "SBase.hpp"


class SBMLVisitor;


class SimpleSpeciesReference : public SBase
{
public:

  /**
   * Creates a new SimpleSpeciesReference, optionally with its species
   * attribute set.
   */
  LIBSBML_EXTERN
  SimpleSpeciesReference (const std::string& species = "");

  /**
   * Destroys this SimpleSpeciesReference.
   */
  LIBSBML_EXTERN
  virtual ~SimpleSpeciesReference() { }


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Reaction's next
   * SimpleSpeciesReference (if available).
   */
  LIBSBML_EXTERN
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return the species for this SimpleSpeciesReference.
   */
  LIBSBML_EXTERN
  const std::string& getSpecies () const;

  /**
   * @return true if the species for this SimpleSpeciesReference has been
   * set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSpecies () const;

  /**
   * Sets the species of this SimpleSpeciesReference to a copy of sid.
   */
  LIBSBML_EXTERN
  void setSpecies (const std::string& sid);


protected:

  std::string species;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // SimpleSpeciesReference_hpp
