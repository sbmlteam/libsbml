/**
 * Filename    : Parameter.hpp
 * Description : SBML Parameter
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-21
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


#ifndef Parameter_hpp
#define Parameter_hpp


#include <string>
#include "extern.h"
#include "SBase.hpp"


class Parameter : public SBase
{
public:

  /**
   * Creates a new Parameter, optionally with its id attribute set.
   */
  LIBSBML_EXTERN
  Parameter (const std::string& id = "");

  /**
   * Creates a new Parameter, with its id and value attributes set and
   * optionally its units and constant attributes.
   */
  LIBSBML_EXTERN
  Parameter (   const std::string&  id
              , double              value
              , const std::string&  units    = ""
              , bool                constant = true );

  /**
   * Destroys this Parameter.
   */
  LIBSBML_EXTERN
  virtual ~Parameter ();

  /**
   * Initializes the fields of this Parameter to their defaults:
   *
   *   - constant = true  (L2 only)
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the id of this Parameter
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Parameter.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the value of this Parameter.
   */
  LIBSBML_EXTERN
  double getValue () const;

  /**
   * @return the units of this Parameter.
   */
  LIBSBML_EXTERN
  const std::string& getUnits () const;

  /**
   * @return true if this Parameter is constant, false otherwise.
   */
  LIBSBML_EXTERN
  bool getConstant () const;

  /**
   * @return true if the id of this Parameter has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Parameter has been set, false
   * otherwise.
   *
   * In SBML L1, a Parameter name is required and therefore <b>should always
   * be set</b>.  In L2, name is optional and as such may or may not be
   * set.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the value of this Parameter has been set, false
   * otherwise.
   *
   * In SBML L1v1, a Parameter value is required and therefore <b>should
   * always be set</b>.  In L1v2 and beyond, a value is optional and as
   * such may or may not be set.
   */
  LIBSBML_EXTERN
  bool isSetValue () const;

  /**
   * @return true if the units of this Parameter has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetUnits () const;

  /**
   * Sets the id of this Parameter to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Parameter to a copy of string (SName in L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the initialAmount of this Parameter to value and marks the field
   * as set.
   */
  LIBSBML_EXTERN
  void setValue (double value);

  /**
   * Sets the units of this Parameter to a copy of sid.
   */
  LIBSBML_EXTERN
  void setUnits (const std::string& sname);

  /**
   * Sets the constant field of this Parameter to value.
   */
  LIBSBML_EXTERN
  void setConstant (bool value);

  /**
   * Unsets the name of this Parameter.
   *
   * In SBML L1, a Parameter name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Unsets the value of this Parameter.
   *
   * In SBML L1v1, a Parameter value is required and therefore <b>should
   * always be set</b>.  In L1v2 and beyond, a value is optional and as
   * such may or may not be set.
   */
  LIBSBML_EXTERN
  void unsetValue ();

  /**
   * Unsets the units of this Parameter.
   */
  LIBSBML_EXTERN
  void unsetUnits ();


protected:

  std::string id;
  std::string name;
  double      value;
  std::string units;
  bool        constant;

  struct
  {
    unsigned int value:1;
  } isSet;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Parameter_hpp
