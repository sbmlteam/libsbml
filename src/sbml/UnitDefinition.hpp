/**
 * Filename    : UnitDefinition.hpp
 * Description : SBML UnitDefinition
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef UnitDefinition_hpp
#define UnitDefinition_hpp


#include <string>

#include "extern.h"
#include "SBase.hpp"
#include "ListOf.hpp"
#include "Unit.hpp"


class UnitDefinition : public SBase
{
public:

  /**
   * Creates a new UnitDefinition, optionally with its id and name
   * attributes set.
   */
  LIBSBML_EXTERN UnitDefinition (  const std::string& id   = ""
                                 , const std::string& name = "" );

  /**
   * Destroys this UnitDefinition.
   */
  LIBSBML_EXTERN virtual ~UnitDefinition ();

  /**
   * @return the id of this UnitDefinition.
   */
  LIBSBML_EXTERN const std::string& getId () const;

  /**
   * @return the name of this UnitDefinition.
   */
  LIBSBML_EXTERN const std::string& getName () const;

  /**
   * @return true if the id of this UnitDefinition has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN bool isSetId () const;

  /**
   * @return true if the name of this UnitDefinition has been set, false
   * otherwise.
   *
   * In SBML L1, a UnitDefinition name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN bool isSetName () const;

  /**
   * Moves the id field of this UnitDefinition to its name field (iff name
   * is not already set).  This method is used for converting from L2 to
   * L1.
   */
  LIBSBML_EXTERN
  void moveIdToName ();

  /**
   * Moves the name field of this UnitDefinition to its id field (iff id is
   * not already set).  This method is used for converting from L1 to L2.
   */
  LIBSBML_EXTERN
  void moveNameToId ();

  /**
   * Sets the id of this UnitDefinition to a copy of sid.
   */
  LIBSBML_EXTERN void setId (const std::string& sid);

  /**
   * Sets the name of this UnitDefinition to a copy of string (SName in
   * L1).
   */
  LIBSBML_EXTERN void setName (const std::string& str);

  /**
   * Unsets the name of this UnitDefinition.
   *
   * In SBML L1, a UnitDefinition name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN void unsetName ();

  /**
   * Adds the given Unit to this UnitDefinition.
   */
  LIBSBML_EXTERN void addUnit (Unit& u);

  /**
   * @return the list of Units for this UnitDefinition.
   */
  LIBSBML_EXTERN ListOf& getListOfUnits ();

  /**
   * @return the nth Unit of this UnitDefinition
   */
  LIBSBML_EXTERN Unit* getUnit (unsigned int n) const;

  /**
   * @return the number of Units in this UnitDefinition.
   */
  LIBSBML_EXTERN unsigned int getNumUnits () const;


protected:

  std::string id;
  std::string name;
  ListOf      unit;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // UnitDefinition_hpp
