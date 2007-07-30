/**
 * @file    Species.cpp
 * @brief   Implementations of Species and ListOfSpecies.
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
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Species.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Species, optionally with its id and name attributes set.
 */
Species::Species (const std::string& id, const std::string& name) :
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
  double initialAmount = mInitialAmount;
  
  // need to cover case where user has changed level 
  // and expects an initial amount where there was none
  if ( getLevel() == 1 && isSetInitialConcentration() )
  {
    const Compartment *c = getModel()->getCompartment(getCompartment());
    if (c)
    {
      initialAmount = mInitialConcentration * c->getSize();
    }
  }

  return initialAmount;
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
Species::setSpeciesType (const std::string& sid)
{
  mSpeciesType = sid;
}


/**
 * Sets the compartment of this Species to a copy of sid.
 */
void
Species::setCompartment (const std::string& sid)
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
Species::setSubstanceUnits (const std::string& sid)
{
  mSubstanceUnits = sid;
}


/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
void
Species::setSpatialSizeUnits (const std::string& sid)
{
  mSpatialSizeUnits = sid;
}


/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
void
Species::setUnits (const std::string& sname)
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


/** @cond doxygen-libsbml-internal */
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
  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("compartment");
  expectedAttributes.push_back("initialAmount");
  expectedAttributes.push_back("boundaryCondition");
  expectedAttributes.push_back("charge");
  if (level == 1)
  {
    expectedAttributes.push_back("units");
  }
  else
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("id");
    expectedAttributes.push_back("initialConcentration");
    expectedAttributes.push_back("substanceUnits");
    expectedAttributes.push_back("hasOnlySubstanceUnits");
    expectedAttributes.push_back("constant");

    if (version != 1)
    {
      expectedAttributes.push_back("speciesType");
    }

    if (version != 3)
    {
      expectedAttributes.push_back("spatialSizeUnits");
    }

    if (version == 3)
    {
      expectedAttributes.push_back("sboTerm");
    }
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      getErrorLog()->logError(SBMLError::NotSchemaConformant, level, version,
        "Attribute " + name + " is not part of Species");
    }
  }


  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  attributes.readInto(id, mId, getErrorLog(), true);
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
  attributes.readInto("compartment", mCompartment, getErrorLog(), true);

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 1)
  {
    mIsSetInitialAmount = attributes.readInto("initialAmount", mInitialAmount,
                                                  getErrorLog(), true);
  }
  else
  {
    mIsSetInitialAmount = attributes.readInto("initialAmount", mInitialAmount);
  }

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
  SBase::checkUnitSyntax();

  if (level == 2)
  {
    //
    // spatialSizeUnits: SId  { use="optional" }  (L2v1, L2v2) removed in l2v3
    //
    attributes.readInto("spatialSizeUnits", mSpatialSizeUnits);
    SBase::checkUnitSyntax(1);

    //
    // hasOnlySubstanceUnits: boolean
    // { use="optional" default="false" }  (L2v1, L2v2)
    //
    attributes.readInto("hasOnlySubstanceUnits", mHasOnlySubstanceUnits);
  }
  else
  {
    // inlevel 1 the units of a species were considered to be substance units
    mHasOnlySubstanceUnits = 1;
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */



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


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpecies::getElementPosition () const
{
  return 6;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty Species_t structure and returns a pointer to
 * it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * has no attribute values set and that there are no default values
 * assigned to such things as identifiers and names.  In SBML Level 2
 * and beyond, the "id" (identifier) attribute of a Species_t is
 * required to have a value.  Thus, callers are cautioned to assign a value
 * after calling this constructor, for example using Species_setName().
 *
 * @return a pointer to the newly created Species_t structure.
 */
LIBSBML_EXTERN
Species_t *
Species_create ()
{
  return new(nothrow) Species;
}


/**
 * Creates a new Species_t structure with identifier @p sid and
 * name @p name.
 *
 * In SBML Level 2 and beyond, the identifier attribute of a Species_t
 * structure is required to have a value, but the name is optional.
 * Programs calling this function can legitimately use an empty string for
 * the @p name argument.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   Species_t *c = Species_create();
 *   Species_setId(c, id);
 *   Species_setName(c, name);
 * @endcode
 *
 * @param sid the value to assign as the identifier of this species
 * 
 * @param name the value to assign as the name of this species
 * 
 * @return a pointer to the newly created Species_t structure.
 */
LIBSBML_EXTERN
Species_t *
Species_createWith (const char *sid, const char *name)
{
  return new(nothrow) Species(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given Species_t structure.
 *
 * @param s the Species_t structure to be freed.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s)
{
  delete s;
}


/**
 * Creates a deep copy of the given Species_t structure
 * 
 * @param p the Species_t structure to be copied
 * 
 * @return a (deep) copy of the given Species_t structure.
 */
LIBSBML_EXTERN
Species_t *
Species_clone (const Species_t *s)
{
  return static_cast<Species*>( s->clone() );
}


/**
 * Initializes the attributes of the given Species_t structure to the
 * defaults defined in the specification of the relevant Level/Version of
 * SBML.
 * 
 * @li sets "boundaryCondition" to @c 1 (true)
 * @li (Level 2 only) sets "constant" to @c 0 (false)
 * @li (Level 2 only) sets "hasOnlySubstanceUnits" to @c 0 (false)
 *
 * @param s the Species_t structure.
 */
LIBSBML_EXTERN
void
Species_initDefaults (Species_t *s)
{
  s->initDefaults();
}


/**
 * Takes a Species_t structure and returns its identifier.
 *
 * @param s the Species_t structure whose identifier is sought
 * 
 * @return the identifier of the Species_t structure @p s, as a pointer
 * to a string.
 */
LIBSBML_EXTERN
const char *
Species_getId (const Species_t *s)
{
  return s->isSetId() ? s->getId().c_str() : NULL;
}


/**
 * Takes a Species_t structure and returns its name.
 *
 * @param s the Species_t structure whose name is sought.
 *
 * @return the name of the Species_t structure @p s, as a pointer to a
 * string.
 */
LIBSBML_EXTERN
const char *
Species_getName (const Species_t *s)
{
  return s->isSetName() ? s->getName().c_str() : NULL;
}


/**
 * Get the species type of this Species_t structure, as indicated by the
 * Species_t structure object's "speciesType" attribute value.
 *
 * @param s the Species_t structure
 * 
 * @return the value of the "speciesType" attribute of the
 * Species_t structure @p s as a string.
 */
LIBSBML_EXTERN
const char *
Species_getSpeciesType (const Species_t *s)
{
  return s->isSetSpeciesType() ? s->getSpeciesType().c_str() : NULL;
}


/**
 * Get the compartment in which this species is located.
 *
 * @param s the Species_t structure
 * 
 * @return the value of the "compartment" attribute of the given Species_t
 * structure, as a string.
 */
LIBSBML_EXTERN
const char *
Species_getCompartment (const Species_t *s)
{
  return s->isSetCompartment() ? s->getCompartment().c_str() : NULL;
}


/**
 * Get the value of the "initialAmount" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the "initialAmount" attribute of the given Species_t structure,
 * as a float-point number.
 */
LIBSBML_EXTERN
double
Species_getInitialAmount (const Species_t *s)
{
  return s->getInitialAmount();
}


/**
 * Get the value of the "initialConcentration" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the "initialConcentration" of the given Species_t structure, as
 * a float-point number.
 */
LIBSBML_EXTERN
double
Species_getInitialConcentration (const Species_t *s)
{
  return s->getInitialConcentration();
}


/**
 * Get the value of the "substanceUnit" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the "substanceUnits" attribute of the given Species_t structure,
 * as a string.
 */
LIBSBML_EXTERN
const char *
Species_getSubstanceUnits (const Species_t *s)
{
  return s->isSetSubstanceUnits() ? s->getSubstanceUnits().c_str() : NULL;
}


/**
 * Get the value of the "spatialSizeUnits" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the spatialSizeUnits of the given Species.
 * 
 * @warning In versions of SBML Level~2 before Version 3, the Species_t
 * structure included an attribute called "spatialSizeUnits", which allowed
 * explicitly setting the units of size for initial concentration.  This
 * attribute was removed in SBML Level 2 Version 3.  LibSBML retains this
 * attribute for compatibility with older definitions of Level 2, but its
 * use is strongly discouraged because it is incompatible with Level 2
 * Version 3.
 */
LIBSBML_EXTERN
const char *
Species_getSpatialSizeUnits (const Species_t *s)
{
  return s->isSetSpatialSizeUnits() ? s->getSpatialSizeUnits().c_str() : NULL;
}


/**
 * (SBML Level 1 only) Get the value of the "units" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the units of the given Species_t structure.
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s)
{
  return s->isSetUnits() ? s->getUnits().c_str() : NULL;
}


/**
 * Get the value of the "hasOnlySubstanceUnits" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return nonzero (true) if the given Species_t structure's
 * "hasOnlySubstanceUnits" attribute value is nonzero, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_getHasOnlySubstanceUnits (const Species_t *s)
{
  return static_cast<int>( s->getHasOnlySubstanceUnits() );
}


/**
 * Get the value of the "boundaryCondition" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return nonzero (true) if the given Species_t structure's
 * "boundaryCondition" attribute value is nonzero, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_getBoundaryCondition (const Species_t *s)
{
  return static_cast<int>( s->getBoundaryCondition() );
}


/**
 * Get the value of the "charge" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return the charge of the given Species_t structure.
 *
 * @note Beginning in SBML Level 2 Version 2, the "charge" attribute on
 * Species_t is deprecated and its use strongly discouraged.  Its presence
 * is considered a misfeature in earlier definitions of SBML because its
 * implications for the mathematics of a model were never defined, and in
 * any case, no known modeling system ever used it.  Instead, models take
 * account of charge values directly in their definitions of species by
 * (for example) having separate species identities for the charged and
 * uncharged versions of the same species.  This allows the condition to
 * affect model mathematics directly.  LibSBML retains this method for
 * easier compatibility with SBML Level 1.
 */
LIBSBML_EXTERN
int
Species_getCharge (const Species_t *s)
{ 
  return s->getCharge();
}


/**
 * Get the value of the "constant" attribute.
 *
 * @param s the Species_t structure
 * 
 * @return nonzero (true) if the given Species_t structure's "constant"
 * attribute value is nonzero, @c false otherwise.
 */
LIBSBML_EXTERN
int
Species_getConstant (const Species_t *s)
{
  return static_cast<int>( s->getConstant() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "id" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "id" attribute of the given Species_t
 * structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetId (const Species_t *s)
{
  return static_cast<int>( s->isSetId() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "name" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "name" attribute of the given Species_t
 * structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetName (const Species_t *s)
{
  return static_cast<int>( s->isSetName() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "speciesType" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "speciesType" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpeciesType (const Species_t *s)
{
  return static_cast<int>( s->isSetSpeciesType() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "compartment" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "compartment" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCompartment (const Species_t *s)
{
  return static_cast<int>( s->isSetCompartment() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "initialAmount" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "initialAmount" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 *
 * @note In SBML Level 1, Species_t' "initialAmount" is required and
 * therefore <em>should always be set</em>.  (However, in Level 1, the
 * attribute has no default value either, so this method will not return
 * nonzero until a value has been assigned.)  In SBML Level 2,
 * "initialAmount" is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s)
{
  return static_cast<int>( s->isSetInitialAmount() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "compartment" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "compartment" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialConcentration (const Species_t *s)
{
  return static_cast<int>( s->isSetInitialConcentration() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "substanceUnits" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "substanceUnits" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSubstanceUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetSubstanceUnits() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "spatialSizeUnits" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "spatialSizeUnits" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 * 
 * @warning In versions of SBML Level~2 before Version 3, the class
 * Species included an attribute called "spatialSizeUnits", which allowed
 * explicitly setting the units of size for initial concentration.  This
 * attribute was removed in SBML Level 2 Version 3.  LibSBML retains this
 * attribute for compatibility with older definitions of Level 2, but its
 * use is strongly discouraged because it is incompatible with Level 2
 * Version 3.
 */
LIBSBML_EXTERN
int
Species_isSetSpatialSizeUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetSpatialSizeUnits() );
}


/**
 * (SBML Level 1 only) Predicate returning true or false depending on
 * whether the attribute "units" of the given Species_t structure has been
 * set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "units" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s)
{
  return static_cast<int>( s->isSetUnits() );
}


/**
 * Predicate returning true or false depending on whether the attribute
 * "charge" of the given Species_t structure has been set.
 *
 * @param s the Species_t structure
 * 
 * @return true (nonzero) if the "charge" attribute of the given
 * Species_t structure has been set, false (0) otherwise.
 *
 * @note Beginning in SBML Level 2 Version 2, the "charge" attribute on
 * Species in SBML is deprecated and its use strongly discouraged.  Its
 * presence is considered a misfeature in earlier definitions of SBML
 * because its implications for the mathematics of a model were never
 * defined, and in any case, no known modeling system ever used it.
 * Instead, models take account of charge values directly in their
 * definitions of species by (for example) having separate species
 * identities for the charged and uncharged versions of the same species.
 * This allows the condition to affect model mathematics directly.
 * LibSBML retains this method for easier compatibility with SBML Level 1.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s)
{
  return static_cast<int>( s->isSetCharge() );
}


/**
 * Sets the "id" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p sid.  If the string is
 * NULL, this function performs unsetId() instead.
 *
 * @param s the Species_t structure
 * 
 * @param sid the identifier string to which the "id" attribute should be
 * set.
 */
LIBSBML_EXTERN
void
Species_setId (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetId() : s->setId(sid);
}


/**
 * Sets the "name" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p name.  If the string is
 * NULL, this function performs unsetName() instead.
 *
 * @param s the Species_t structure
 * 
 * @param name the name string to which the "name" attribute should be set.
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *name)
{
  (name == NULL) ? s->unsetName() : s->setName(name);
}


/**
 * Sets the "speciesType" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p sid.  If the string
 * is NULL, this function performs unsetSpeciesType() instead.
 *
 * @param s the Species_t structure
 * 
 * @param speciesType the identifer to which the "speciesType" attribute
 * should be set.
 */
LIBSBML_EXTERN
void
Species_setSpeciesType (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSpeciesType() : s->setSpeciesType(sid);
}


/**
 * Sets the "compartment" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p compartment.  If the string
 * is NULL, this function performs unsetCompartment() instead.
 *
 * @param s the Species_t structure
 * 
 * @param speciesType the identifer to which the "speciesType" attribute
 * should be set.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->setCompartment("") : s->setCompartment(sid);
}


/**
 * Sets the "initialAmount" attribute value of the given Species_t
 * structure.
 *
 * As a side-effect, calling this function also unsets the
 * "initialConcentration" attribute.
 *
 * @param s the Species_t structure
 *
 * @param value the numerical value for the "initialAmount" attribute
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value)
{
  s->setInitialAmount(value);
}


/**
 * Sets the "initialConcentration" attribute value of the given Species_t
 * structure.
 *
 * As a side-effect, calling this function also unsets the "initialAmount"
 * attribute.
 *
 * @param s the Species_t structure
 *
 * @param value the numerical value for the "initialConcentration"
 * attribute
 */
LIBSBML_EXTERN
void
Species_setInitialConcentration (Species_t *s, double value)
{
  s->setInitialConcentration(value);
}


/**
 * Sets the "substanceUnits" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p sid.  If the string
 * is NULL, this function performs unsetSubstanceUnits() instead.
 *
 * @param s the Species_t structure
 * 
 * @param substanceUnits the identifer to which the "substanceUnits"
 * attribute should be set.
 */
LIBSBML_EXTERN
void
Species_setSubstanceUnits (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSubstanceUnits() : s->setSubstanceUnits(sid);
}


/**
 * Sets the "spatialSizeUnits" attribute of the given Species_t structure.
 *
 * This function copies the string given in @p sid.  If the string is NULL,
 * this function performs unsetSpatialSizeUnits() instead.
 *
 * @param s the Species_t structure
 * 
 * @param spatialSizeUnits the identifer to which the "spatialSizeUnits"
 * attribute should be set.
 * 
 * @warning In versions of SBML Level~2 before Version 3, the class
 * Species included an attribute called "spatialSizeUnits", which allowed
 * explicitly setting the units of size for initial concentration.  This
 * attribute was removed in SBML Level 2 Version 3.  LibSBML retains this
 * attribute for compatibility with older definitions of Level 2, but its
 * use is strongly discouraged because it is incompatible with Level 2
 * Version 3.
 */
LIBSBML_EXTERN
void
Species_setSpatialSizeUnits (Species_t *s, const char *sid)
{
  (sid == NULL) ? s->unsetSpatialSizeUnits() : s->setSpatialSizeUnits(sid);
}


/**
 * (SBML Level 1 only) Sets the "units" attribute of the given Species_t
 * structure.
 *
 * This function copies the string given in @p sid.  If the string is NULL,
 * this function performs unsetUnits() instead.
 *
 * @param s the Species_t structure
 * 
 * @param units the identifer to which the "units" attribute
 * should be set.
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname)
{
  (sname == NULL) ? s->unsetUnits() : s->setUnits(sname);
}


/**
 * Sets the "hasOnlySubstanceUnits" attribute of the given Species_t
 * structure.
 *
 * @param s the Species_t structure
 * 
 * @param value nonzero to indicate true, zero to indicate false.
 */
LIBSBML_EXTERN
void
Species_setHasOnlySubstanceUnits (Species_t *s, int value)
{
  s->setHasOnlySubstanceUnits( static_cast<bool>(value) );
}


/**
 * Sets the "boundaryCondition" attribute of the given Species_t
 * structure.
 *
 * @param s the Species_t structure
 * 
 * @param value nonzero to indicate true, zero to indicate false.
 */
LIBSBML_EXTERN
void
Species_setBoundaryCondition (Species_t *s, int value)
{
  s->setBoundaryCondition( static_cast<bool>(value) );
}


/**
 * Sets the "charge" attribute of the given Species_t
 * structure.
 *
 * @param s the Species_t structure
 * 
 * @param value the value of charge to assign to the "charge" attribute
 *
 * @note Beginning in SBML Level 2 Version 2, the "charge" attribute on
 * Species in SBML is deprecated and its use strongly discouraged.  Its
 * presence is considered a misfeature in earlier definitions of SBML
 * because its implications for the mathematics of a model were never
 * defined, and in any case, no known modeling system ever used it.
 * Instead, models take account of charge values directly in their
 * definitions of species by (for example) having separate species
 * identities for the charged and uncharged versions of the same species.
 * This allows the condition to affect model mathematics directly.
 * LibSBML retains this method for easier compatibility with SBML Level 1.
 */
LIBSBML_EXTERN
void
Species_setCharge (Species_t *s, int value)
{
  s->setCharge(value);
}


/**
 * Sets the "constant" attribute of the given Species_t
 * structure.
 *
 * @param s the Species_t structure
 * 
 * @param value nonzero to indicate true, zero to indicate false.
 */
LIBSBML_EXTERN
void
Species_setConstant (Species_t *s, int value)
{
  s->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the "name" attribute of the given Species_t structure.
 */
LIBSBML_EXTERN
void
Species_unsetName (Species_t *s)
{
  s->unsetName();
}


/**
 * Unsets the "speciesType" attribute of the given Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 */
LIBSBML_EXTERN
void
Species_unsetSpeciesType (Species_t *s)
{
  s->unsetSpeciesType();
}


/**
 * Unsets the "initialAmount" attribute of the given Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s)
{
  s->unsetInitialAmount();
}


/**
 * Unsets the "initialConcentration" attribute of the given
 * Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 */
LIBSBML_EXTERN
void
Species_unsetInitialConcentration (Species_t *s)
{
  s->unsetInitialConcentration();
}


/**
 * Unsets the "substanceUnits" attribute of the given
 * Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 */
LIBSBML_EXTERN
void
Species_unsetSubstanceUnits (Species_t *s)
{
  s->unsetSubstanceUnits();
}


/**
 * Unsets the "spatialSizeUnits" attribute of the given
 * Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 * 
 * @warning In versions of SBML Level~2 before Version 3, the class
 * Species included an attribute called "spatialSizeUnits", which allowed
 * explicitly setting the units of size for initial concentration.  This
 * attribute was removed in SBML Level 2 Version 3.  LibSBML retains this
 * attribute for compatibility with older definitions of Level 2, but its
 * use is strongly discouraged because it is incompatible with Level 2
 * Version 3.
 */
LIBSBML_EXTERN
void
Species_unsetSpatialSizeUnits (Species_t *s)
{
  s->unsetSpatialSizeUnits();
}


/**
 * Unsets the "units" attribute of the given
 * Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 */
LIBSBML_EXTERN
void
Species_unsetUnits (Species_t *s)
{
  s->unsetUnits();
}


/**
 * Unsets the "charge" attribute of the given
 * Species_t structure.
 *
 * @param s the Species_t structure whose attribute is to be unset.
 *
 * @note Beginning in SBML Level 2 Version 2, the "charge" attribute on
 * Species in SBML is deprecated and its use strongly discouraged.  Its
 * presence is considered a misfeature in earlier definitions of SBML
 * because its implications for the mathematics of a model were never
 * defined, and in any case, no known modeling system ever used it.
 * Instead, models take account of charge values directly in their
 * definitions of species by (for example) having separate species
 * identities for the charged and uncharged versions of the same species.
 * This allows the condition to affect model mathematics directly.
 * LibSBML retains this method for easier compatibility with SBML Level 1.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s)
{
  s->unsetCharge();
}



/** @endcond doxygen-c-only */
