/**
 * Filename    : Species.hpp
 * Description : SBML Species
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Species_hpp
#define Species_hpp


#include <string>
#include "extern.h"
#include "SBase.hpp"


class Species : public SBase
{
public:

  /**
   * Creates a new Species, optionally with its id attribute set.
   */
  LIBSBML_EXTERN
  Species (const std::string& id = "");

  /**
   * Destroys this Species.
   */
  LIBSBML_EXTERN
  virtual ~Species ();

  /**
   * Initializes the fields of this Species to their defaults:
   *
   *   - boundaryCondition = false
   *   - constant          = false  (L2 only)
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the id of this Species
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Species.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the compartment of this Species.
   */
  LIBSBML_EXTERN
  const std::string& getCompartment () const;

  /**
   * @return the initialAmount of this Species.
   */
  LIBSBML_EXTERN
  double getInitialAmount () const;

  /**
   * @return the initialConcentration of this Species.
   */
  LIBSBML_EXTERN
  double getInitialConcentration () const;

  /**
   * @return the substanceUnits of this Species.
   */
  LIBSBML_EXTERN
  const std::string& getSubstanceUnits () const;

  /**
   * @return the spatialSizeUnits of this Species.
   */
  LIBSBML_EXTERN
  const std::string& getSpatialSizeUnits () const;

  /**
   * @return the units of this Species (L1 only).
   */
  LIBSBML_EXTERN
  const std::string& getUnits () const;

  /**
   * @return true if this Species hasOnlySubstanceUnits, false otherwise.
   */
  LIBSBML_EXTERN
  bool getHasOnlySubstanceUnits () const;

  /**
   * @return the boundaryCondition of this Species.
   */
  LIBSBML_EXTERN
  bool getBoundaryCondition () const;

  /**
   * @return the charge of this Species.
   */
  LIBSBML_EXTERN
  int getCharge () const;

  /**
   * @return true if this Species is constant, false otherwise.
   */
  LIBSBML_EXTERN
  bool getConstant () const;

  /**
   * @return true if the id of this Species has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Species has been set, false
   * otherwise.
   *
   * In SBML L1, a Species name is required and therefore <b>should always
   * be set</b>.  In L2, name is optional and as such may or may not be
   * set.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the compartment of this Species has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetCompartment () const;

  /**
   * @return true if the initialAmount of this Species has been set, false
   * otherwise.
   *
   * In SBML L1, a Species initialAmount is required and therefore
   * <b>should always be set</b>.  In L2, initialAmount is optional and as
   * such may or may not be set.
   */
  LIBSBML_EXTERN
  bool isSetInitialAmount () const;

  /**
   * @return true if the initialConcentration of this Species has been set,
   * false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetInitialConcentration () const;

  /**
   * @return true if the substanceUnits of this Species has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSubstanceUnits () const;

  /**
   * @return true if the spatialSizeUnits of this Species has been set,
   * false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSpatialSizeUnits () const;

  /**
   * @return true if the units of this Species has been set, false
   * otherwise (L1 only).
   */
  LIBSBML_EXTERN
  bool isSetUnits () const;

  /**
   * @return true if the charge of this Species has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetCharge () const;

  /**
   * Sets the id of this Species to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Species to a copy of string (SName in L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the compartment of this Species to a copy of sid.
   */
  LIBSBML_EXTERN
  void setCompartment (const std::string& sid);

  /**
   * Sets the initialAmount of this Species to value and marks the field as
   * set.  This method also unsets the initialConentration field.
   */
  LIBSBML_EXTERN
  void setInitialAmount (double value);

  /**
   * Sets the initialConcentration of this Species to value and marks the
   * field as set.  This method also unsets the initialAmount field.
   */
  LIBSBML_EXTERN
  void setInitialConcentration (double value);

  /**
   * Sets the substanceUnits of this Species to a copy of sid.
   */
  LIBSBML_EXTERN
  void setSubstanceUnits (const std::string& sid);

  /**
   * Sets the spatialSizeUnits of this Species to a copy of sid.
   */
  LIBSBML_EXTERN
  void setSpatialSizeUnits (const std::string& sid);

  /**
   * Sets the units of this Species to a copy of sname (L1 only).
   */
  LIBSBML_EXTERN
  void setUnits (const std::string& sname);

  /**
   * Sets the hasOnlySubstanceUnits field of this Species to value.
   */
  LIBSBML_EXTERN
  void setHasOnlySubstanceUnits (bool value);

  /**
   * Sets the boundaryCondition of this Species to value.
   */
  LIBSBML_EXTERN
  void setBoundaryCondition (bool value);

  /**
   * Sets the charge of this Species to value and marks the field as set.
   */
  LIBSBML_EXTERN
  void setCharge (int value);

  /**
   * Sets the constant field of this Species to value.
   */
  LIBSBML_EXTERN
  void setConstant (bool value);

  /**
   * Unsets the name of this Species.
   *
   * In SBML L1, a Species name is required and therefore <b>should always
   * be set</b>.  In L2, name is optional and as such may or may not be
   * set.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Marks the initialAmount of this Species as unset.
   */
  LIBSBML_EXTERN
  void unsetInitialAmount ();

  /**
   * Unsets the initialConcentration of this Species.
   */
  LIBSBML_EXTERN
  void unsetInitialConcentration ();

  /**
   * Unsets the substanceUnits of this Species.
   */
  LIBSBML_EXTERN
  void unsetSubstanceUnits ();

  /**
   * Unsets the spatialSizeUnits of this Species.
   */
  LIBSBML_EXTERN
  void unsetSpatialSizeUnits ();

  /**
   * Unsets the units of this Species (L1 only).
   */
  LIBSBML_EXTERN
  void unsetUnits ();

  /**
   * Unsets the charge of this Species.
   */
  LIBSBML_EXTERN
  void unsetCharge ();


protected:

  std::string id;
  std::string name;
  std::string compartment;

  union
  {
    double Amount;
    double Concentration;
  } initial;


  std::string substanceUnits;
  std::string spatialSizeUnits;

  bool hasOnlySubstanceUnits;
  bool boundaryCondition;
  int  charge;
  bool constant;

  struct
  {
    unsigned int initialAmount       :1;
    unsigned int initialConcentration:1;
    unsigned int charge              :1;
  } isSet;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Species_hpp
