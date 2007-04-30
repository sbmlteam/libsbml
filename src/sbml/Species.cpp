/**
 * @file    Species.cpp
 * @brief   SBML Species
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <limits>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Species.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


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
 * Copy constructor. Creates a copy of this Species.
 */
Species::Species(const Species& orig) :
      SBase(orig)
{
    this->mSpeciesType = orig.mSpeciesType;
    this->mCompartment = orig.mCompartment;

    this->mInitialAmount = orig.mInitialAmount;
    this->mInitialConcentration = orig.mInitialConcentration;

    this->mSubstanceUnits = orig.mSubstanceUnits;
    this->mSpatialSizeUnits = orig.mSpatialSizeUnits;

    this->mHasOnlySubstanceUnits = orig.mHasOnlySubstanceUnits;
    this->mBoundaryCondition = orig.mBoundaryCondition;
    this->mCharge = orig.mCharge;
    this->mConstant = orig.mConstant;

    this->mIsSetInitialAmount = orig.mIsSetInitialAmount;
    this->mIsSetInitialConcentration = orig.mIsSetInitialConcentration;
    this->mIsSetCharge = orig.mIsSetCharge;
}


/**
 * Assignment operator.
 */
Species& Species::operator=(const Species& orig)
{
  this->SBase::operator =(orig);
    this->mSpeciesType = orig.mSpeciesType;
    this->mCompartment = orig.mCompartment;

    this->mInitialAmount = orig.mInitialAmount;
    this->mInitialConcentration = orig.mInitialConcentration;

    this->mSubstanceUnits = orig.mSubstanceUnits;
    this->mSpatialSizeUnits = orig.mSpatialSizeUnits;

    this->mHasOnlySubstanceUnits = orig.mHasOnlySubstanceUnits;
    this->mBoundaryCondition = orig.mBoundaryCondition;
    this->mCharge = orig.mCharge;
    this->mConstant = orig.mConstant;

    this->mIsSetInitialAmount = orig.mIsSetInitialAmount;
    this->mIsSetInitialConcentration = orig.mIsSetInitialConcentration;
    this->mIsSetCharge = orig.mIsSetCharge;
  return *this;
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
 *   - boundaryCondition     = false
 *   - constant              = false  (L2 only)
 *   - hasOnlySubstanceUnits = false  (L2 only)
 */
void
Species::initDefaults ()
{
  setBoundaryCondition     (false);
  setConstant              (false);
  setHasOnlySubstanceUnits (false);
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
 * @return true if this Species has boundaryCondition
 * true, false otherwise.
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
 * set.  This method also unsets the initialConcentration field.
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
 * Unsets the initialAmount of this Species.
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
 * @return the name of this element ie "specie" (L1) or "species" (L2).
 */
const string&
Species::getElementName () const
{
  static const string specie  = "specie";
  static const string species = "species";

  return (getLevel() == 1 && getVersion() == 1) ? specie : species;
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Species::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
    read = true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    read = true;
  }

  return read;
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
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);

  //
  // speciesType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3))
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
    // spatialSizeUnits: SId  { use="optional" }  (L2v1, L2v2) removed in l2v3
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

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 3) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
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
  if (level == 2 && (version == 2 || version == 3))
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
    if (version != 3)
    {
      //
      // spatialSizeUnits: SId  { use="optional" }  (L2v1, L2v2)
      //
      stream.writeAttribute("spatialSizeUnits", mSpatialSizeUnits);
    }

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

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
    SBO::writeTerm(stream, mSBOTerm);
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
 * @return the name of this element ie "listOfSpecies".
 */
const string&
ListOfSpecies::getElementName () const
{
  static const string name = "listOfSpecies";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpecies::getElementPosition () const
{
  return 6;
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
 * Creates a new Species with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_createWith (const char *sid, const char *name)
{
  return new(nothrow) Species(sid ? sid : "", name ? name : "");
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
 * @return true (non-zero) if this Species has boundaryCondition
 * true, false (0) otherwise.
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
 * set.  This method also unsets the initialConcentration field.
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
