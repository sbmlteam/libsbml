/**
 * Filename    : Species.cpp
 * Description : SBML Species
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


#include "sbml/util.h"
#include "sbml/Species.h"
#include "sbml/Species.hpp"


/**
 * Creates a new Species, optionally with its id attribute set..
 */
LIBSBML_EXTERN
Species::Species (const std::string& id) :
    SBase                 ()
  , id                    ( id    )
  , hasOnlySubstanceUnits ( false )
  , boundaryCondition     ( false )
  , charge                ( 0     )
  , constant              ( false )
{
  init(SBML_SPECIES);

  initial.Amount = 0.0;

  isSet.initialAmount        = 0;
  isSet.initialConcentration = 0;
  isSet.charge               = 0;
}


/**
 * Destroys this Species.
 */
LIBSBML_EXTERN
Species::~Species ()
{
}


/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = false
 *   - constant          = false  (L2 only)
 */
LIBSBML_EXTERN
void
Species::initDefaults ()
{
  setBoundaryCondition(false);
  setConstant         (false);
}


/**
 * @return the id of this Species
 */
LIBSBML_EXTERN
const std::string&
Species::getId () const
{
  return id;
}


/**
 * @return the name of this Species.
 */
LIBSBML_EXTERN
const std::string&
Species::getName () const
{
  return name;
}


/**
 * @return the compartment of this Species.
 */
LIBSBML_EXTERN
const std::string&
Species::getCompartment () const
{
  return compartment;
}


/**
 * @return the initialAmount of this Species.
 */
LIBSBML_EXTERN
double
Species::getInitialAmount () const
{
  return initial.Amount;
}


/**
 * @return the initialConcentration of this Species.
 */
LIBSBML_EXTERN
double
Species::getInitialConcentration () const
{
  return initial.Concentration;
}


/**
 * @return the substanceUnits of this Species.
 */
LIBSBML_EXTERN
const std::string&
Species::getSubstanceUnits () const
{
  return substanceUnits;
}


/**
 * @return the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
const std::string&
Species::getSpatialSizeUnits () const
{
  return spatialSizeUnits;
}


/**
 * @return the units of this Species (L1 only).
 */
LIBSBML_EXTERN
const std::string&
Species::getUnits () const
{
  return substanceUnits;
}


/**
 * @return true if this Species hasOnlySubstanceUnits, false otherwise.
 */
LIBSBML_EXTERN
bool
Species::getHasOnlySubstanceUnits () const
{
  return hasOnlySubstanceUnits;
}


/**
 * @return the boundaryCondition of this Species.
 */
LIBSBML_EXTERN
bool
Species::getBoundaryCondition () const
{
  return boundaryCondition;
}


/**
 * @return the charge of this Species.
 */
LIBSBML_EXTERN
int
Species::getCharge () const
{
  return charge;
}


/**
 * @return true if this Species is constant, false otherwise.
 */
LIBSBML_EXTERN
bool
Species::getConstant () const
{
  return constant;
}


/**
 * @return true if the id of this Species has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Species has been set, false otherwise.
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
bool
Species::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the compartment of this Species has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetCompartment () const
{
  return ! compartment.empty();
}


/**
 * @return true if the initialAmount of this Species has been set, false
 * otherwise.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
bool
Species::isSetInitialAmount () const
{
  return (bool) isSet.initialAmount;
}


/**
 * @return true if the initialConcentration of this Species has been set,
 * false otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetInitialConcentration () const
{
  return (bool) isSet.initialConcentration;
}


/**
 * @return true if the substanceUnits of this Species has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetSubstanceUnits () const
{
  return ! substanceUnits.empty();
}


/**
 * @return true if the spatialSizeUnits of this Species has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetSpatialSizeUnits () const
{
  return ! spatialSizeUnits.empty();
}


/**
 * @return true if the units of this Species has been set, false otherwise
 * (L1 only).
 */
LIBSBML_EXTERN
bool
Species::isSetUnits () const
{
  return isSetSubstanceUnits();
}


/**
 * @return true if the charge of this Species has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Species::isSetCharge () const
{
  return isSet.charge;
}


/**
 * Sets the id of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Species to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Species::setName (const std::string& string)
{
  name = string;
}


/**
 * Sets the compartment of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species::setCompartment (const std::string& sid)
{
  compartment = sid;
}


/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
LIBSBML_EXTERN
void
Species::setInitialAmount (double value)
{
  initial.Amount             = value;
  isSet.initialAmount        = 1;
  isSet.initialConcentration = 0;
}


/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
LIBSBML_EXTERN
void
Species::setInitialConcentration (double value)
{
  initial.Concentration      = value;
  isSet.initialAmount        = 0;
  isSet.initialConcentration = 1;
}


/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species::setSubstanceUnits (const std::string& sid)
{
  substanceUnits = sid;
}


/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species::setSpatialSizeUnits (const std::string& sid)
{
  spatialSizeUnits = sid;
}


/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
LIBSBML_EXTERN
void
Species::setUnits (const std::string& sname)
{
  setSubstanceUnits(sname);
}


/**
 * Sets the hasOnlySubstanceUnits field of this Species to value.
 */
LIBSBML_EXTERN
void
Species::setHasOnlySubstanceUnits (bool value)
{
  hasOnlySubstanceUnits = value;
}


/**
 * Sets the boundaryCondition of this Species to value.
 */
LIBSBML_EXTERN
void
Species::setBoundaryCondition (bool value)
{
  boundaryCondition = value;
}


/**
 * Sets the charge of this Species to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Species::setCharge (int value)
{
  charge       = value;
  isSet.charge = 1;
}


/**
 * Sets the constant field of this Species to value.
 */
LIBSBML_EXTERN
void
Species::setConstant (bool value)
{
  constant = value;
}


/**
 * Unsets the name of this Species.
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Species::unsetName ()
{
  name.erase();
}


/**
 * Marks the initialAmount of this Species as unset.
 */
LIBSBML_EXTERN
void
Species::unsetInitialAmount ()
{
  initial.Amount      = util_NaN();
  isSet.initialAmount = 0;
}


/**
 * Unsets the initialConcentration of this Species.
 */
LIBSBML_EXTERN
void
Species::unsetInitialConcentration ()
{
  initial.Concentration      = util_NaN();
  isSet.initialConcentration = 0;
}


/**
 * Unsets the substanceUnits of this Species.
 */
LIBSBML_EXTERN
void
Species::unsetSubstanceUnits ()
{
  substanceUnits.erase();
}


/**
 * Unsets the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
void
Species::unsetSpatialSizeUnits ()
{
  spatialSizeUnits.erase();
}


/**
 * Unsets the units of this Species (L1 only).
 */
LIBSBML_EXTERN
void
Species::unsetUnits ()
{
  unsetSubstanceUnits();
}


/**
 * Unsets the charge of this Species.
 */
LIBSBML_EXTERN
void
Species::unsetCharge ()
{
  charge       = 0;
  isSet.charge = 0;
}




/**
 * Creates a new Species and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_create (void)
{
  return new(std::nothrow) Species;
}


/**
 * Creates a new Species with the given id, compartment, initialAmount,
 * substanceUnits, boundaryCondition and charge and returns a pointer to
 * it.  This convenience function is functionally equivalent to:
 *
 *   Species_t *s = Species_create();
 *   Species_setId(s, sid); Species_setCompartment(s, compartment); ...;
 */
LIBSBML_EXTERN
Species_t *
Species_createWith( const char *sid,
                    const char *compartment,
                    double      initialAmount,
                    const char *substanceUnits,
                    int         boundaryCondition,
                    int         charge )
{
  Species* s = new(std::nothrow) Species;


  if (s != 0)
  {
    s->setId                ( sid            ? sid            : "" );
    s->setCompartment       ( compartment    ? compartment    : "" );
    s->setSubstanceUnits    ( substanceUnits ? substanceUnits : "" );
    s->setInitialAmount     ( initialAmount );
    s->setBoundaryCondition ( (bool) boundaryCondition  );
    s->setCharge            ( charge );
  }

  return s;
}


/**
 * Frees the given Species.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s)
{
  delete static_cast<Species*>(s);
}


/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = 0  (false)
 *   - constant          = 0  (false)  (L2 only)
 */
LIBSBML_EXTERN
void
Species_initDefaults (Species_t *s)
{
  static_cast<Species*>(s)->initDefaults();
}


/**
 * @return the id of this Species
 */
LIBSBML_EXTERN
const char *
Species_getId (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getName (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the compartment of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getCompartment (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetCompartment() ? x->getCompartment().c_str() : NULL;
}


/**
 * @return the initialAmount of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialAmount (const Species_t *s)
{
  return static_cast<const Species*>(s)->getInitialAmount();
}


/**
 * @return the initialConcentration of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialConcentration (const Species_t *s)
{
  return static_cast<const Species*>(s)->getInitialConcentration();
}


/**
 * @return the substanceUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSubstanceUnits (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetSubstanceUnits() ? x->getSubstanceUnits().c_str() : NULL;
}


/**
 * @return the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpatialSizeUnits (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetSpatialSizeUnits() ? x->getSpatialSizeUnits().c_str() : NULL;
}


/**
 * @return the units of this Species (L1 only).
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s)
{
  const Species* x = static_cast<const Species*>(s);


  return x->isSetUnits() ? x->getUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Species hasOnlySubstanceUnits, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getHasOnlySubstanceUnits (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->getHasOnlySubstanceUnits();
}


/**
 * @return the boundaryCondition of this Species.
 */
LIBSBML_EXTERN
int
Species_getBoundaryCondition (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->getBoundaryCondition();
}


/**
 * @return the charge of this Species.
 */
LIBSBML_EXTERN
int
Species_getCharge (const Species_t *s)
{ 
  return (int) static_cast<const Species*>(s)->getCharge();
}


/**
 * @return true (non-zero) if this Species is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getConstant (const Species_t *s)
{
  return static_cast<const Species*>(s)->getConstant();
}


/**
 * @return 1 if the id of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetId (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetId();
}


/**
 * @return 1 if the name of this Species has been set, 0 otherwise.
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetName (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetName();
}


/**
 * @return 1 if the compartment of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCompartment (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetCompartment();
}


/**
 * @return 1 if the initialAmount of this Species has been set, 0
 * otherwise.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetInitialAmount();
}


/**
 * @return 1 if the initialConcentration of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialConcentration (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetInitialConcentration();
}


/**
 * @return 1 if the substanceUnits of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSubstanceUnits (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetSubstanceUnits();
}

/**
 * @return 1 if the spatialSizeUnits of this Species has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpatialSizeUnits (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetSpatialSizeUnits();
}


/**
 * @return 1 if the units of this Species has been set, 0 otherwise
 * (L1 only).
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetUnits();
}


/**
 * @return 1 if the charge of this Species has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s)
{
  return (int) static_cast<const Species*>(s)->isSetCharge();
}


/**
 * Sets the id of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setId (Species_t *s, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Species*>(s)->setId("");
  }
  else
  {
    static_cast<Species*>(s)->setId(sid);
  }
}


/**
 * Sets the name of this Species to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *string)
{
  if (string == NULL)
  {
    static_cast<Species*>(s)->unsetName();
  }
  else
  {
    static_cast<Species*>(s)->setName(string);
  }
}


/**
 * Sets the compartment of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Species*>(s)->setCompartment("");
  }
  else
  {
    static_cast<Species*>(s)->setCompartment(sid);
  }
}


/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value)
{
  static_cast<Species*>(s)->setInitialAmount(value);
}


/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
LIBSBML_EXTERN
void
Species_setInitialConcentration (Species_t *s, double value)
{
  static_cast<Species*>(s)->setInitialConcentration(value);
}


/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSubstanceUnits (Species_t *s, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Species*>(s)->unsetSubstanceUnits();
  }
  else
  {
    static_cast<Species*>(s)->setSubstanceUnits(sid);
  }
}


/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpatialSizeUnits (Species_t *s, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Species*>(s)->unsetSpatialSizeUnits();
  }
  else
  {
    static_cast<Species*>(s)->setSpatialSizeUnits(sid);
  }
}


/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname)
{
  if (sname == NULL)
  {
    static_cast<Species*>(s)->unsetUnits();
  }
  else
  {
    static_cast<Species*>(s)->setUnits(sname);
  }
}


/**
 * Sets the hasOnlySubstanceUnits field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setHasOnlySubstanceUnits (Species_t *s, int value)
{
  static_cast<Species*>(s)->setHasOnlySubstanceUnits((bool) value);
}


/**
 * Sets the boundaryCondition of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setBoundaryCondition (Species_t *s, int value)
{
  static_cast<Species*>(s)->setBoundaryCondition((bool) value);
}


/**
 * Sets the charge of this Species to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Species_setCharge (Species_t *s, int value)
{
  static_cast<Species*>(s)->setCharge(value);
}


/**
 * Sets the constant field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setConstant (Species_t *s, int value)
{
  static_cast<Species*>(s)->setConstant((bool) value);
}


/**
 * Unsets the name of this Species.  This is equivalent to:
 * safe_free(s->name); s->name = NULL;
 *
 * In SBML L1, a Species name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Species_unsetName (Species_t *s)
{
  static_cast<Species*>(s)->unsetName();
}


/**
 * Marks the initialAmount of this Species as unset.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s)
{
  static_cast<Species*>(s)->unsetInitialAmount();
}


/**
 * Unsets the initialConcentration of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetInitialConcentration (Species_t *s)
{
  static_cast<Species*>(s)->unsetInitialConcentration();
}


/**
 * Unsets the substanceUnits of this Species.  This is equivalent to:
 * safe_free(s->substanceUnits); s->substanceUnits = NULL;
 */
LIBSBML_EXTERN
void
Species_unsetSubstanceUnits (Species_t *s)
{
  static_cast<Species*>(s)->unsetSubstanceUnits();
}


/**
 * Unsets the spatialSizeUnits of this Species.  This is equivalent to:
 * safe_free(s->spatialSizeUnits); s->spatialSizeUnits = NULL;
 */
LIBSBML_EXTERN
void
Species_unsetSpatialSizeUnits (Species_t *s)
{
  static_cast<Species*>(s)->unsetSpatialSizeUnits();
}


/**
 * Unsets the units of this Species (L1 only).
 */
LIBSBML_EXTERN
void
Species_unsetUnits (Species_t *s)
{
  static_cast<Species*>(s)->unsetUnits();
}


/**
 * Unsets the charge of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s)
{
  static_cast<Species*>(s)->unsetCharge();
}


/**
 * The SpeciesIdCmp function compares the string sid to s->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than s->id.
 * Returns -1 if either sid or s->id is NULL.
 */
LIBSBML_EXTERN
int
SpeciesIdCmp (const char *sid, const Species_t *s)
{
  int result = -1;


  if (sid != NULL && Species_isSetId(s))
  {
    result = strcmp(sid, Species_getId(s));
  }

  return result;
}
