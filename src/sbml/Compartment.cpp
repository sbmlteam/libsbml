/**
 * \file    Compartment.cpp
 * \brief   SBML Compartment
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "Compartment.h"


using namespace std;


/**
 * Creates a new Compartment, optionally with its id attribute set.
 */
Compartment::Compartment (const string& id, const string& name) :
   SBase             ( id, name )
 , mSpatialDimensions( 3        )
 , mSize             ( 1.0      )
 , mConstant         ( true     )
 , mIsSetSize        ( false    )
{
}


/**
 * Destroys this Compartment.
 */
Compartment::~Compartment ()
{
}


/**
 * Copy constructor.
 */
Compartment::Compartment(const Compartment& rhs) :
   SBase             ( rhs )
 , mSpatialDimensions( rhs.mSpatialDimensions  )
 , mSize             ( rhs.mSize      )
 , mConstant         ( rhs.mConstant     )
 , mIsSetSize        ( rhs.mIsSetSize    )
 , mCompartmentType  ( rhs.mCompartmentType)
 , mUnits            ( rhs.mUnits )
 , mOutside          ( rhs.mOutside )
{
}


/**
 * Assignment operator
 */
Compartment& Compartment::operator=(const Compartment& rhs)
{
  this->SBase::operator =(rhs);
  mSpatialDimensions= rhs.mSpatialDimensions  ;
  mSize             = rhs.mSize      ;
  mConstant         = rhs.mConstant     ;
  mIsSetSize        = rhs.mIsSetSize    ;
  mCompartmentType  = rhs.mCompartmentType;
  mUnits            = rhs.mUnits ;
  mOutside          = rhs.mOutside ;
  return *this;
}



/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Compartment (if available).
 */
bool
Compartment::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Compartment.
 */
SBase*
Compartment::clone () const
{
  return new Compartment(*this);
}


/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment::initDefaults ()
{
  mSize      = 1.0;    // Actually, setting L1 volume not
  mIsSetSize = false;  // L2 size.

  setSpatialDimensions(3);
  setConstant(1);
}


/**
 * @return the compartmentType of this Compartment.
 */
const string&
Compartment::getCompartmentType () const
{
  return mCompartmentType;
}


/**
 * @return the spatialDimensions of this Compartment.
 */
unsigned int
Compartment::getSpatialDimensions () const
{
  return mSpatialDimensions;
}


/**
 * @return the size (volume in L1) of this Compartment.
 */
double
Compartment::getSize () const
{
  return mSize;
}


/**
 * @return the volume (size in L2) of this Compartment.
 */
double
Compartment::getVolume () const
{
  return getSize();
}


/**
 * @return the units of this Compartment.
 */
const string&
Compartment::getUnits () const
{
  return mUnits;
}


/**
 * @return the outside of this Compartment.
 */
const string&
Compartment::getOutside () const
{
  return mOutside;
}


/**
 * @return true if this Compartment is constant, false otherwise.
 */
bool
Compartment::getConstant () const
{
  return mConstant;
}


/**
 * @return true if the compartmentType of this Compartment has been set,
 * false otherwise. 
 */
bool
Compartment::isSetCompartmentType () const
{
  return (mCompartmentType.empty() == false);
}


/**
 * @return true if the size (volume in L1) of this Compartment has been
 * set, false otherwise.
 */
bool
Compartment::isSetSize () const
{
  return mIsSetSize;
}


/**
 * @return true if the volume (size in L2) of this Compartment has been
 * set, false otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
bool
Compartment::isSetVolume () const
{
  return (getLevel() == 1) ? true : isSetSize();
}


/**
 * @return true if the units of this Compartment has been set, false
 * otherwise.
 */
bool
Compartment::isSetUnits () const
{
  return (mUnits.empty() == false);
}


/**
 * @return true if the outside of this Compartment has been set, false
 * otherwise.
 */
bool
Compartment::isSetOutside () const
{
  return (mOutside.empty() == false);
}


/**
 * Sets the compartmentType field of this Compartment to a copy of sid.
 */
void
Compartment::setCompartmentType (const string& sid)
{
  mCompartmentType = sid;
}


/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
void
Compartment::setSpatialDimensions (unsigned int value)
{
  if (value >= 0 && value <= 3) mSpatialDimensions = value;
}


/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
void
Compartment::setSize (double value)
{
  mSize      = value;
  mIsSetSize = true;
}


/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
void
Compartment::setVolume (double value)
{
  setSize(value);
}


/**
 * Sets the units of this Compartment to a copy of sid.
 */
void
Compartment::setUnits (const std::string& sid)
{
  mUnits = sid;
}


/**
 * Sets the outside of this Compartment to a copy of sid.
 */
void
Compartment::setOutside (const std::string& sid)
{
  mOutside = sid;
}


/**
 * Sets the constant field of this Compartment to value.
 */
void
Compartment::setConstant (bool value)
{
  mConstant = value;
}


/**
 * Unsets the compartmentType of this Compartment.
 */
void
Compartment::unsetCompartmentType ()
{
  mCompartmentType.erase();
}


/**
 * Unsets the size (volume in L1) of this Compartment.
 */
void
Compartment::unsetSize ()
{
  mSize      = numeric_limits<double>::quiet_NaN();
  mIsSetSize = false;
}


/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume is optional with no default
 * value and as such may or may not be set.
 */
void
Compartment::unsetVolume ()
{
  unsetSize();
}


/**
 * Unsets the units of this Compartment.
 */
void
Compartment::unsetUnits ()
{
  mUnits.erase();
}


/**
 * Unsets the outside of this Compartment.
 */
void
Compartment::unsetOutside ()
{
  mOutside.erase();
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Compartment::getTypeCode () const
{
  return SBML_COMPARTMENT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Compartment::getElementName () const
{
  static const string name = "compartment";
  return name;
}

/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Compartment::readOtherXML (XMLInputStream& stream)
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
    checkNotes();
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
Compartment::readAttributes (const XMLAttributes& attributes)
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
  // compartmentType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3))
  {
    attributes.readInto("compartmentType", mCompartmentType);
  }

  //
  // spatialDimensions { maxInclusive="3" minInclusive="0" use="optional"
  //                     default="3" }  (L2v1, L2v2)
  //
  if (level == 2)
  {
    attributes.readInto("spatialDimensions", mSpatialDimensions);
  }

  //
  // volume  { use="optional" default="1" }  (L1v1, L1v2)
  // size    { use="optional" }              (L2v1, L2v2)
  //
  const string size = (level == 1) ? "volume" : "size";
  mIsSetSize = attributes.readInto(size, mSize);

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("units", mUnits);

  //
  // outside  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("outside", mOutside);

  //
  // constant  { use="optional" default="true" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("constant", mConstant);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 3) 
    mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Compartment::writeAttributes (XMLOutputStream& stream) const
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
  // compartmentType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3))
  {
    stream.writeAttribute("compartmentType", mCompartmentType);
  }

  //
  // spatialDimensions { maxInclusive="3" minInclusive="0" use="optional"
  //                     default="3" }  (L2v1, L2v2)
  //
  if (level == 2)
  {
    if (mSpatialDimensions >= 0 && mSpatialDimensions <= 2)
    {
      stream.writeAttribute("spatialDimensions", mSpatialDimensions);
    }
  }

  //
  // volume  { use="optional" default="1" }  (L1v1, L1v2)
  // size    { use="optional" }              (L2v1, L2v2)
  //
  if (mIsSetSize)
  {
    const string size = (level == 1) ? "volume" : "size";
    stream.writeAttribute(size, mSize);
  }

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  stream.writeAttribute("units", mUnits);

  //
  // outside  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  stream.writeAttribute("outside", mOutside);

  //
  // constant  { use="optional" default="true" }  (L2v1, L2v2)
  //
  if (level == 2 && mConstant != true)
  {
    stream.writeAttribute("constant", mConstant);
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
    SBML::writeSBOTerm(stream, mSBOTerm);
}


/**
 * @return a (deep) copy of this ListOfCompartments.
 */
SBase*
ListOfCompartments::clone () const
{
  return new ListOfCompartments(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfCompartments::getItemTypeCode () const
{
  return SBML_COMPARTMENT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfCompartments::getElementName () const
{
  static const string name = "listOfCompartments";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfCompartments::getElementPosition () const
{
  return 5;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCompartments::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "compartment")
  {
    object = new Compartment();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new Compartment and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void)
{
  return new(nothrow) Compartment;
}


/**
 * Creates a new Compartment with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith (const char *sid, const char *name)
{
  return new(nothrow) Compartment(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given Compartment.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c)
{
  delete c;
}


/**
 * @return a (deep) copy of the given Compartment.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_clone (const Compartment_t* c)
{
  return static_cast<Compartment*>( c->clone() );
}


/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c)
{
  c->initDefaults();
}


/**
 * @return the id of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c)
{
  return c->isSetId() ? c->getId().c_str() : NULL;
}


/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c)
{
  return c->isSetName() ? c->getName().c_str() : NULL;
}


/**
 * @return the compartmentType of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getCompartmentType (const Compartment_t *c)
{
  return c->isSetCompartmentType() ? c->getCompartmentType().c_str() : NULL;
}


/**
 * @return the spatialDimensions of this Compartment.
 */
LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c)
{
  return c->getSpatialDimensions();
}


/**
 * @return the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c)
{
  return c->getSize();
}


/**
 * @return the volume (size in L2) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c)
{
  return c->getVolume();
}


/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c)
{
  return c->isSetUnits() ? c->getUnits().c_str() : NULL;
}


/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c)
{
  return c->isSetOutside() ? c->getOutside().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Compartment is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c)
{
  return static_cast<int>( c->getConstant() );
}


/**
 * @return true (non-zero) if the id of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c)
{
  return static_cast<int>( c->isSetId() );
}


/**
 * @return true (non-zero) if the name of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c)
{
  return static_cast<int>( c->isSetName() );
}


/**
 * @return true (non-zero) if the compartmentType of this Compartment has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetCompartmentType (const Compartment_t *c)
{
  return static_cast<int>( c->isSetCompartmentType() );
}

/**
 * @return true (non-zero) if the size (volume in L1) of this Compartment
 * has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c)
{
  return static_cast<int>( c->isSetSize() );
}


/**
 * @return true (non-zero) if the volume (size in L2) of this Compartment
 * has been set, false (0) otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c)
{
  return static_cast<int>( c->isSetVolume() );
}


/**
 * @return true (non-zero) if the units of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c)
{
  return static_cast<int>( c->isSetUnits() );
}


/**
 * @return true (non-zero) if the outside of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c)
{
  return static_cast<int>( c->isSetOutside() );
}


/**
 * Sets the id of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setId (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetId() : c->setId(sid);
}


/**
 * Sets the name of this Compartment to a copy of string.
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *str)
{
  (str == NULL) ? c->unsetName() : c->setName(str);
}


/**
 * Sets the compartmentType of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setCompartmentType (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetCompartmentType() : c->setCompartmentType(sid);
}


/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
LIBSBML_EXTERN
void
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value)
{
  c->setSpatialDimensions(value);
}


/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setSize (Compartment_t *c, double value)
{
  c->setSize(value);
}


/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value)
{
  c->setVolume(value);
}


/**
 * Sets the units of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetUnits() : c->setUnits(sid);
}


/**
 * Sets the outside of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetOutside() : c->setOutside(sid);
}


/**
 * Sets the constant field of this Compartment to value (boolean).
 */
LIBSBML_EXTERN
void
Compartment_setConstant (Compartment_t *c, int value)
{
  c->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the name of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetName (Compartment_t *c)
{
  c->unsetName();
}


/**
 * Unsets the compartmentType of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetCompartmentType (Compartment_t *c)
{
  c->unsetCompartmentType();
}


/**
 * Unsets the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetSize (Compartment_t *c)
{
  c->unsetSize();
}


/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume is optional with no default
 * value and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c)
{
  c->unsetVolume();
}


/**
 * Unsets the units of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetUnits (Compartment_t *c)
{
  c->unsetUnits();
}


/**
 * Unsets the outside of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetOutside (Compartment_t *c)
{
  c->unsetOutside();
}
