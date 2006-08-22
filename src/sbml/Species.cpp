/**
 * \file    Species.cpp
 * \brief   SBML Species
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <limits>

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "Species.h"


using namespace std;


/**
 * Creates a new Species, optionally with its id and name attributes set.
 */
Species::Species (const string& id, const string& name) :
    SBase                     ( id, name )
  , mInitialAmount            ( 0.0   )
  , mInitialConcentration     ( 0.0   )
  , mHasOnlySubstanceUnits    ( false )
  , mBoundaryCondition        ( false )
  , mCharge                   ( 0     )
  , mConstant                 ( false )
  , mIsSetInitialAmount       ( false )
  , mIsSetInitialConcentration( false )
  , mIsSetCharge              ( false )
{
}


/**
 * Destroys this Species.
 */
Species::~Species ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Species (if available).
 */
bool
Species::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Species.
 */
SBase*
Species::clone () const
{
  return new Species(*this);
}


/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = false
 *   - constant          = false  (L2 only)
 */
void
Species::initDefaults ()
{
  setBoundaryCondition(false);
  setConstant         (false);
}


/**
 * @return the speciesType of this Species.
 */
const string&
Species::getSpeciesType () const
{
  return mSpeciesType;
}


/**
 * @return the compartment of this Species.
 */
const string&
Species::getCompartment () const
{
  return mCompartment;
}


/**
 * @return the initialAmount of this Species.
 */
double
Species::getInitialAmount () const
{
  return mInitialAmount;
}


/**
 * @return the initialConcentration of this Species.
 */
double
Species::getInitialConcentration () const
{
  return mInitialConcentration;
}


/**
 * @return the substanceUnits of this Species.
 */
const string&
Species::getSubstanceUnits () const
{
  return mSubstanceUnits;
}


/**
 * @return the spatialSizeUnits of this Species.
 */
const string&
Species::getSpatialSizeUnits () const
{
  return mSpatialSizeUnits;
}


/**
 * @return the units of this Species (L1 only).
 */
const string&
Species::getUnits () const
{
  return mSubstanceUnits;
}


/**
 * @return true if this Species hasOnlySubstanceUnits, false otherwise.
 */
bool
Species::getHasOnlySubstanceUnits () const
{
  return mHasOnlySubstanceUnits;
}


/**
 * @return the boundaryCondition of this Species.
 */
bool
Species::getBoundaryCondition () const
{
  return mBoundaryCondition;
}


/**
 * @return the charge of this Species.
 */
int
Species::getCharge () const
{
  return mCharge;
}


/**
 * @return true if this Species is constant, false otherwise.
 */
bool
Species::getConstant () const
{
  return mConstant;
}


/**
 * @return true if the speciesType of this Species has been set, false
 * otherwise.
 */
bool
Species::isSetSpeciesType () const
{
  return (mSpeciesType.empty() == false);
}


/**
 * @return true if the compartment of this Species has been set, false
 * otherwise.
 */
bool
Species::isSetCompartment () const
{
  return (mCompartment.empty() == false);
}


/**
 * @return true if the initialAmount of this Species has been set, false
 * otherwise.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
bool
Species::isSetInitialAmount () const
{
  return mIsSetInitialAmount;
}


/**
 * @return true if the initialConcentration of this Species has been set,
 * false otherwise.
 */
bool
Species::isSetInitialConcentration () const
{
  return mIsSetInitialConcentration;
}


/**
 * @return true if the substanceUnits of this Species has been set, false
 * otherwise.
 */
bool
Species::isSetSubstanceUnits () const
{
  return (mSubstanceUnits.empty() == false);
}


/**
 * @return true if the spatialSizeUnits of this Species has been set, false
 * otherwise.
 */
bool
Species::isSetSpatialSizeUnits () const
{
  return (mSpatialSizeUnits.empty() == false);
}


/**
 * @return true if the units of this Species has been set, false otherwise
 * (L1 only).
 */
bool
Species::isSetUnits () const
{
  return isSetSubstanceUnits();
}


/**
 * @return true if the charge of this Species has been set, false
 * otherwise.
 */
bool
Species::isSetCharge () const
{
  return mIsSetCharge;
}


/**
 * Sets the speciesType field of this Species to a copy of sid.
 */
void
Species::setSpeciesType (const string& sid)
{
  mSpeciesType = sid;
}


/**
 * Sets the compartment of this Species to a copy of sid.
 */
void
Species::setCompartment (const string& sid)
{
  mCompartment = sid;
}


/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
void
Species::setInitialAmount (double value)
{
  mInitialAmount      = value;
  mIsSetInitialAmount = true;

  unsetInitialConcentration();
}


/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
void
Species::setInitialConcentration (double value)
{
  mInitialConcentration      = value;
  mIsSetInitialConcentration = true;

  unsetInitialAmount();
}


/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
void
Species::setSubstanceUnits (const string& sid)
{
  mSubstanceUnits = sid;
}


/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
void
Species::setSpatialSizeUnits (const string& sid)
{
  mSpatialSizeUnits = sid;
}


/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
void
Species::setUnits (const string& sname)
{
  setSubstanceUnits(sname);
}


/**
 * Sets the hasOnlySubstanceUnits field of this Species to value.
 */
void
Species::setHasOnlySubstanceUnits (bool value)
{
  mHasOnlySubstanceUnits = value;
}


/**
 * Sets the boundaryCondition of this Species to value.
 */
void
Species::setBoundaryCondition (bool value)
{
  mBoundaryCondition = value;
}


/**
 * Sets the charge of this Species to value and marks the field as set.
 */
void
Species::setCharge (int value)
{
  mCharge      = value;
  mIsSetCharge = true;
}


/**
 * Sets the constant field of this Species to value.
 */
void
Species::setConstant (bool value)
{
  mConstant = value;
}


/**
 * Unsets the speciesType of this Species.
 */
void
Species::unsetSpeciesType ()
{
  mSpeciesType.erase();
}


/**
 * Marks the initialAmount of this Species as unset.
 */
void
Species::unsetInitialAmount ()
{
  mInitialAmount      = numeric_limits<double>::quiet_NaN();
  mIsSetInitialAmount = false;
}


/**
 * Unsets the initialConcentration of this Species.
 */
void
Species::unsetInitialConcentration ()
{
  mInitialConcentration      = numeric_limits<double>::quiet_NaN();
  mIsSetInitialConcentration = false;
}


/**
 * Unsets the substanceUnits of this Species.
 */
void
Species::unsetSubstanceUnits ()
{
  mSubstanceUnits.erase();
}


/**
 * Unsets the spatialSizeUnits of this Species.
 */
void
Species::unsetSpatialSizeUnits ()
{
  mSpatialSizeUnits.erase();
}


/**
 * Unsets the units of this Species (L1 only).
 */
void
Species::unsetUnits ()
{
  unsetSubstanceUnits();
}


/**
 * Unsets the charge of this Species.
 */
void
Species::unsetCharge ()
{
  mCharge      = 0;
  mIsSetCharge = false;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Species::getTypeCode () const
{
  return SBML_SPECIES;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Species::getElementName () const
{
  static const string specie  = "specie";
  static const string species = "species";

  return (getLevel() == 1 && getVersion() == 1) ? specie : species;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Species::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  attributes.readInto(id, mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);

  //
  // speciesType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2)
  {
    attributes.readInto("speciesType", mSpeciesType);
  }

  //
  // compartment: SName  { use="required" }  (L1v1, L2v1)
  // compartment: SId    { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("compartment", mCompartment);

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1, L2v2)
  //
  mIsSetInitialAmount = attributes.readInto("initialAmount", mInitialAmount);

  //
  // initialConcentration: double  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2)
  {
    mIsSetInitialConcentration =
      attributes.readInto("initialConcentration", mInitialConcentration);
  }

  //
  //          units: SName  { use="optional" }  (L1v1, L1v2)
  // substanceUntis: SId    { use="optional" }  (L2v1, L2v2)
  //
  const string units = (level == 1) ? "units" : "substanceUnits";
  attributes.readInto(units, mSubstanceUnits);

  if (level == 2)
  {
    //
    // spatialSizeUnits: SId  { use="optional" }  (L2v1, L2v2)
    //
    attributes.readInto("spatialSizeUnits", mSpatialSizeUnits);

    //
    // hasOnlySubstanceUnits: boolean
    // { use="optional" default="false" }  (L2v1, L2v2)
    //
    attributes.readInto("hasOnlySubstanceUnits", mHasOnlySubstanceUnits);
  }

  //
  // boundaryCondition: boolean
  // { use="optional" default="false" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("boundaryCondition", mBoundaryCondition);

  //
  // charge: integer  { use="optional" }  (L1v1, L1v2, L2v1)
  // charge: integer  { use="optional" }  deprecated (L2v2)
  //
  mIsSetCharge = attributes.readInto("charge", mCharge);

  if (level == 2)
  {
    //
    // constant: boolean  { use="optional" default="false" }  (L2v1, L2v2)
    //
    attributes.readInto("constant", mConstant);
  }
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Species::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  stream.writeAttribute(id, mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) stream.writeAttribute("name", mName);

  //
  // speciesType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2)
  {
    stream.writeAttribute("speciesType", mSpeciesType);
  }

  //
  // compartment: SName  { use="required" }  (L1v1, L2v1)
  // compartment: SId    { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("compartment", mCompartment);

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1, L2v2)
  //
  if ( isSetInitialAmount() )
  {
    stream.writeAttribute("initialAmount", mInitialAmount);
  }

  //
  // initialConcentration: double  { use="optional" }  (L2v1, L2v2)
  //
  else if ( level == 2 && isSetInitialConcentration() )
  {
    stream.writeAttribute("initialConcentration", mInitialConcentration);
  }

  //
  // If user is converting a model from L2 to L1, two possiblities exist
  // that have not been covered:
  //
  //   1.  InitialConcentration has been used, so it must be converted to
  //       initialAmount
  //
  //   2.  No initialAmount/initialAmount has been set, but initialAmount
  //       is required in L1
  //
  else if (level == 1)
  {
    if ( isSetInitialConcentration() )
    {
      const Model*       m = getModel();
      const Compartment* c = m ? m->getCompartment( getCompartment() ) : 0;

      if (c)
      {
        double amount = mInitialConcentration * c->getSize();
        stream.writeAttribute("initialAmount", amount);
      }
      else
      {
        stream.writeAttribute("initialAmount", mInitialConcentration);
      }
    }
    else
    {
      stream.writeAttribute("initialAmount", mInitialAmount);
    }
  }

  //
  //          units: SName  { use="optional" }  (L1v1, L1v2)
  // substanceUntis: SId    { use="optional" }  (L2v1, L2v2)
  //
  const string units = (level == 1) ? "units" : "substanceUnits";
  stream.writeAttribute( units, getUnits() );

  if (level == 2)
  {
    //
    // spatialSizeUnits: SId  { use="optional" }  (L2v1, L2v2)
    //
    stream.writeAttribute("spatialSizeUnits", mSpatialSizeUnits);

    //
    // hasOnlySubstanceUnits: boolean
    // { use="optional" default="false" }  (L2v1, L2v2)
    //
    if (mHasOnlySubstanceUnits)
    {
      stream.writeAttribute( "hasOnlySubstanceUnits", mHasOnlySubstanceUnits );
    }
  }

  //
  // boundaryCondition: boolean
  // { use="optional" default="false" }  (L1v1, L1v2, L2v1, L2v2)
  //
  if (mBoundaryCondition)
  {
    stream.writeAttribute("boundaryCondition", mBoundaryCondition);
  }

  //
  // charge: integer  { use="optional" }  (L1v1, L1v2, L2v1)
  // charge: integer  { use="optional" }  deprecated (L2v2)
  //
  if ( isSetCharge() )
  {
    stream.writeAttribute("charge", mCharge);
  }

  //
  // constant: boolean  { use="optional" default="false" }  (L2v1, L2v2)
  //
  if (mConstant)
  {
    stream.writeAttribute("constant", mConstant);
  }
}


/**
 * @return a (deep) copy of this ListOfSpecies.
 */
SBase*
ListOfSpecies::clone () const
{
  return new ListOfSpecies(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpecies::getItemTypeCode () const
{
  return SBML_SPECIES;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfSpecies::getElementName () const
{
  static const string name = "listOfSpecies";
  return name;
}


/**
 * returns expected position of ListOfSpecies in a model
 */
int
ListOfSpecies::getElementPosition() const
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  int position = 1;
  /**
   * the expected position of each element depends on the level and version
   * and also on whether other preceding elements have been declared
   * since other elements are optional 
   */

  if (this->getSBMLDocument()->getModel()->getNumFunctionDefinitions() != 0)
    position++;

  if (this->getSBMLDocument()->getModel()->getNumUnitDefinitions() != 0)
    position++;

  if (this->getSBMLDocument()->getModel()->getNumCompartmentTypes() != 0)
    position++;

  if (this->getSBMLDocument()->getModel()->getNumSpeciesTypes() != 0)
    position++;

  if (this->getSBMLDocument()->getModel()->getNumCompartments() != 0)
    position++;

  return position;

}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpecies::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "species" || name == "specie")
  {
    object = new Species();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new Species and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_create ()
{
  return new(nothrow) Species;
}


/**
 * Frees the given Species.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s)
{
  delete s;
}


/**
 * @return a (deep) copy of this Species.
 */
LIBSBML_EXTERN
Species_t *
Species_clone (const Species_t *s)
{
  return static_cast<Species*>( s->clone() );
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
  s->initDefaults();
}


/**
 * @return the id of this Species
 */
LIBSBML_EXTERN
const char *
Species_getId (const Species_t *s)
{
  return s->isSetId() ? s->getId().c_str() : NULL;
}


/**
 * @return the name of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getName (const Species_t *s)
{
  return s->isSetName() ? s->getName().c_str() : NULL;
}


/**
 * @return the speciesType of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpeciesType (const Species_t *s)
{
  return s->isSetSpeciesType() ? s->getSpeciesType().c_str() : NULL;
}


/**
 * @return the compartment of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getCompartment (const Species_t *s)
{
  return s->isSetCompartment() ? s->getCompartment().c_str() : NULL;
}


/**
 * @return the initialAmount of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialAmount (const Species_t *s)
{
  return s->getInitialAmount();
}


/**
 * @return the initialConcentration of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialConcentration (const Species_t *s)
{
  return s->getInitialConcentration();
}


/**
 * @return the substanceUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSubstanceUnits (const Species_t *s)
{
  return s->isSetSubstanceUnits() ? s->getSubstanceUnits().c_str() : NULL;
}


/**
 * @return the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpatialSizeUnits (const Species_t *s)
{
  return s->isSetSpatialSizeUnits() ? s->getSpatialSizeUnits().c_str() : NULL;
}


/**
 * @return the units of this Species (L1 only).
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s)
{
  return s->isSetUnits() ? s->getUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Species hasOnlySubstanceUnits, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getHasOnlySubstanceUnits (const Species_t *s)
{
  return static_cast<int>( s->getHasOnlySubstanceUnits() );
}


/**
 * @return the boundaryCondition of this Species.
 */
LIBSBML_EXTERN
int
Species_getBoundaryCondition (const Species_t *s)
{
  return static_cast<int>( s->getBoundaryCondition() );
}


/**
 * @return the charge of this Species.
 */
LIBSBML_EXTERN
int
Species_getCharge (const Species_t *s)
{ 
  return s->getCharge();
}


/**
 * @return true (non-zero) if this Species is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getConstant (const Species_t *s)
{
  return static_cast<int>( s->getConstant() );
}


/**
 * @return true (non-zero) if the id of this Species has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetId (const Species_t *s)
{
  return static_cast<int>( s->isSetId() );
}


/**
 * @return true (non-zero) if the name of this Species has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetName (const Species_t *s)
{
  return static_cast<int>( s->isSetName() );
}


/**
 * @return true (non-zero) if the speciesType of this Species has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpeciesType (const Species_t *s)
{
  return static_cast<int>( s->isSetSpeciesType() );
}


/**
 * @return true (non-zero) if the compartment of this Species has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCompartment (const Species_t *s)
{
  return static_cast<int>( s->isSetCompartment() );
}


/**
 * @return true (non-zero) if the initialAmount of this Species has been
 * set, false (0) otherwise.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s)
{
  return static_cast<int>( s->isSetInitialAmount() );
}


/**
 * @return true (non-zero) if the initialConcentration of this Species has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialConcentration (const Species_t *s)
{
  return static_cast<int>( s->isSetInitialConcentration() );
}


/**
 * @return true (non-zero) if the substanceUnits of this Species has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSubstanceUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetSubstanceUnits() );
}


/**
 * @return true (non-zero) if the spatialSizeUnits of this Species has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpatialSizeUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetSpatialSizeUnits() );
}


/**
 * @return true (non-zero) if the units of this Species has been set, false
 * (0) otherwise (L1 only).
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetUnits() );
}


/**
 * @return true (non-zero) if the charge of this Species has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s)
{
  return static_cast<int>( s->isSetCharge() );
}


/**
 * Sets the id of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setId (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetId() : s->setId(sid);
}


/**
 * Sets the name of this Species to a copy of string.
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *str)
{
  (str == NULL) ? s->unsetName() : s->setName(str);
}


/**
 * Sets the speciesType of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpeciesType (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSpeciesType() : s->setSpeciesType(sid);
}


/**
 * Sets the compartment of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->setCompartment("") : s->setCompartment(sid);
}


/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value)
{
  s->setInitialAmount(value);
}


/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
LIBSBML_EXTERN
void
Species_setInitialConcentration (Species_t *s, double value)
{
  s->setInitialConcentration(value);
}


/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSubstanceUnits (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSubstanceUnits() : s->setSubstanceUnits(sid);
}


/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpatialSizeUnits (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSpatialSizeUnits() : s->setSpatialSizeUnits(sid);
}


/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname)
{
  (sname == NULL) ? s->unsetUnits() : s->setUnits(sname);
}


/**
 * Sets the hasOnlySubstanceUnits field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setHasOnlySubstanceUnits (Species_t *s, int value)
{
  s->setHasOnlySubstanceUnits( static_cast<bool>(value) );
}


/**
 * Sets the boundaryCondition of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setBoundaryCondition (Species_t *s, int value)
{
  s->setBoundaryCondition( static_cast<bool>(value) );
}


/**
 * Sets the charge of this Species to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Species_setCharge (Species_t *s, int value)
{
  s->setCharge(value);
}


/**
 * Sets the constant field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setConstant (Species_t *s, int value)
{
  s->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the name of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetName (Species_t *s)
{
  s->unsetName();
}


/**
 * Unsets the speciesType of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSpeciesType (Species_t *s)
{
  s->unsetSpeciesType();
}


/**
 * Marks the initialAmount of this Species as unset.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s)
{
  s->unsetInitialAmount();
}


/**
 * Unsets the initialConcentration of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetInitialConcentration (Species_t *s)
{
  s->unsetInitialConcentration();
}


/**
 * Unsets the substanceUnits of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSubstanceUnits (Species_t *s)
{
  s->unsetSubstanceUnits();
}


/**
 * Unsets the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSpatialSizeUnits (Species_t *s)
{
  s->unsetSpatialSizeUnits();
}


/**
 * Unsets the units of this Species (L1 only).
 */
LIBSBML_EXTERN
void
Species_unsetUnits (Species_t *s)
{
  s->unsetUnits();
}


/**
 * Unsets the charge of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s)
{
  s->unsetCharge();
}
