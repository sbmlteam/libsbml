/**
 * Filename    : Compartment.hpp
 * Description : SBML Compartment
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-13
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


#ifndef Compartment_hpp
#define Compartment_hpp


#include <string>

#include "extern.h"
#include "SBase.hpp"


class SBMLVisitor;


class Compartment : public SBase
{
public:

  /**
   * Creates a new Compartment, optionally with its id attribute set.
   */
  LIBSBML_EXTERN
  Compartment (const std::string& id = "");

  /**
   * Destroys this Compartment.
   */
  LIBSBML_EXTERN
  virtual ~Compartment ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * Compartment (if available).
   */
  LIBSBML_EXTERN
  bool accept (SBMLVisitor& v) const;

  /**
   * Initializes the fields of this Compartment to their defaults:
   *
   *   - volume            = 1.0          (L1 only)
   *   - spatialDimensions = 3            (L2 only)
   *   - constant          = 1    (true)  (L2 only)
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the id of this Compartment.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Compartment.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the spatialDimensions of this Compartment.
   */
  LIBSBML_EXTERN
  unsigned int getSpatialDimensions () const;

  /**
   * @return the size (volume in L1) of this Compartment.
   */
  LIBSBML_EXTERN
  double getSize () const;

  /**
   * @return the volume (size in L2) of this Compartment.
   */
  LIBSBML_EXTERN
  double getVolume () const;

  /**
   * @return the units of this Compartment.
   */
  LIBSBML_EXTERN
  const std::string& getUnits () const;

  /**
   * @return the outside of this Compartment.
   */
  LIBSBML_EXTERN
  const std::string& getOutside () const;

  /**
   * @return true if this Compartment is constant, false otherwise.
   */
  LIBSBML_EXTERN
  bool getConstant () const;

  /**
   * @return true if the id of this Compartment has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Compartment has been set, false
   * otherwise.
   *
   * In SBML L1, a Compartment name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the size (volume in L1) of this Compartment has been
   * set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSize () const;

  /**
   * @return true if the volume (size in L2) of this Compartment has been
   * set, false otherwise.
   *
   * In SBML L1, a Compartment volume has a default value (1.0) and
   * therefore <b>should always be set</b>.  In L2, volume (size) is
   * optional with no default value and as such may or may not be set.
   */
  LIBSBML_EXTERN
  bool isSetVolume () const;

  /**
   * @return true if the units of this Compartment has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetUnits () const;

  /**
   * @return true if the outside of this Compartment has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetOutside () const;

  /**
   * Moves the id field of this Compartment to its name field (iff name is
   * not already set).  This method is used for converting from L2 to L1.
   */
  LIBSBML_EXTERN
  void moveIdToName ();

  /**
   * Moves the name field of this Compartment to its id field (iff id is
   * not already set).  This method is used for converting from L1 to L2.
   */
  LIBSBML_EXTERN
  void moveNameToId ();

  /**
   * Sets the id of this Compartment to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Compartment to a copy of string (SName in L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the spatialDimensions of this Compartment to value.
   *
   * If value is not one of [0, 1, 2, 3] the function will have no effect
   * (i.e. spatialDimensions will not be set).
   */
  LIBSBML_EXTERN
  void setSpatialDimensions (unsigned int value);

  /**
   * Sets the size (volume in L1) of this Compartment to value.
   */
  LIBSBML_EXTERN
  void setSize (double value);

  /**
   * Sets the volume (size in L2) of this Compartment to value.
   */
  LIBSBML_EXTERN
  void setVolume (double value);

  /**
   * Sets the units of this Compartment to a copy of sid.
   */
  LIBSBML_EXTERN
  void setUnits (const std::string& sid);

  /**
   * Sets the outside of this Compartment to a copy of sid.
   */
  LIBSBML_EXTERN
  void setOutside (const std::string& sid);

  /**
   * Sets the constant field of this Compartment to value.
   */
  LIBSBML_EXTERN
  void setConstant (bool value);

  /**
   * Unsets the name of this Compartment.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Unsets the size (volume in L1) of this Compartment.
   */
  LIBSBML_EXTERN
  void unsetSize ();

  /**
   * Unsets the volume (size in L2) of this Compartment.
   *
   * In SBML L1, a Compartment volume has a default value (1.0) and
   * therefore <b>should always be set</b>.  In L2, volume is optional with
   * no default value and as such may or may not be set.
   */
  LIBSBML_EXTERN
  void unsetVolume ();

  /**
   * Unsets the units of this Compartment.
   */
  LIBSBML_EXTERN
  void unsetUnits ();

  /**
   * Unsets the outside of this Compartment.
   */
  LIBSBML_EXTERN
  void unsetOutside ();


protected:

  std::string  id;
  std::string  name;
  unsigned int spatialDimensions;
  double       size;
  std::string  units;
  std::string  outside;
  bool         constant;

  struct
  {
    unsigned int size  :1;
    unsigned int volume:1;
  } isSet;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Compartment_hpp
