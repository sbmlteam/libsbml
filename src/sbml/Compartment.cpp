/**
 * @file    Compartment.cpp
 * @brief   Implementations of Compartment and ListOfCompartments.
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
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Compartment.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Compartment, optionally with its id attribute set.
 */
Compartment::Compartment (const std::string& id, const std::string& name) :
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
 * Copy constructor. Creates a copy of this compartment.
 */
Compartment::Compartment(const Compartment& orig) :
   SBase             ( orig )
 , mCompartmentType  ( orig.mCompartmentType)
 , mSpatialDimensions( orig.mSpatialDimensions  )
 , mSize             ( orig.mSize      )
 , mUnits            ( orig.mUnits )
 , mOutside          ( orig.mOutside )
 , mConstant         ( orig.mConstant     )
 , mIsSetSize        ( orig.mIsSetSize    )
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
Compartment::setCompartmentType (const std::string& sid)
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
  if (getLevel() == 1)
  {
    setSize(1.0);
  }
  else
  {
    unsetSize();
  }
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
 * @return the name of this element ie "compartment".
 */
const string&
Compartment::getElementName () const
{
  static const string name = "compartment";
  return name;
}


/** @cond doxygen-libsbml-internal */
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

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("units");
  expectedAttributes.push_back("outside");

  if (level == 1)
  {
    expectedAttributes.push_back("volume");
  }
  else
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("id");
    expectedAttributes.push_back("size");
    expectedAttributes.push_back("spatialDimensions");
    expectedAttributes.push_back("constant");

    if (version != 1)
    {
      expectedAttributes.push_back("compartmentType");
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
      logUnknownAttribute(name, level, version, "<compartment>");
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
  if (level == 2) attributes.readInto("name", mName, getErrorLog(), false);

  //
  // compartmentType: SId  { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3))
  {
    attributes.readInto("compartmentType", mCompartmentType, 
                                                      getErrorLog(), false);
  }

  //
  // spatialDimensions { maxInclusive="3" minInclusive="0" use="optional"
  //                     default="3" }  (L2v1, L2v2)
  //
  if (level == 2)
  {
    attributes.readInto("spatialDimensions", mSpatialDimensions, 
                                                      getErrorLog(), false);
  }

  //
  // volume  { use="optional" default="1" }  (L1v1, L1v2)
  // size    { use="optional" }              (L2v1, L2v2)
  //
  const string size = (level == 1) ? "volume" : "size";
  mIsSetSize = attributes.readInto(size, mSize, getErrorLog(), false);

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("units", mUnits, getErrorLog(), false);
  SBase::checkUnitSyntax();

  //
  // outside  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("outside", mOutside, getErrorLog(), false);

  //
  // constant  { use="optional" default="true" }  (L2v1, L2v2)
  //
  if (level == 2) 
    attributes.readInto("constant", mConstant, getErrorLog(), false);

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
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


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
 * @return the name of this element ie "listOfCompartments".
 */
const string&
ListOfCompartments::getElementName () const
{
  static const string name = "listOfCompartments";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfCompartments::getElementPosition () const
{
  return 5;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty Compartment_t structure and returns a pointer to
 * it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * has no attribute values set and that there are no default values
 * assigned to such things as identifiers and names.  (Exception: in SBML
 * Level 1, the "volume" of a compartment has a default.)  In SBML Level 2
 * and beyond, the "id" (identifier) attribute of a Compartment_t is
 * required to have a value.  Thus, callers are cautioned to assign a value
 * after calling this constructor, for example using Compartment_setName().
 *
 * @return a pointer to the newly created Compartment_t structure.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void)
{
  return new(nothrow) Compartment;
}


/**
 * Creates a new Compartment_t structure with identifier @p sid and
 * name @p name.
 *
 * In SBML Level 2 and beyond, the identifier attribute of a Compartment_t
 * structure is required to have a value, but the name is optional.
 * Programs calling this function can legitimately use an empty string for
 * the @p name argument.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   Compartment_t *c = Compartment_create();
 *   Compartment_setId(c, id);
 *   Compartment_setName(c, name);
 * @endcode
 *
 * @param sid the value to assign as the identifier of this compartment
 *
 * @param name the value to assign as the name of this compartment
 * 
 * @return a pointer to the newly created Compartment_t structure.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith (const char *sid, const char *name)
{
  return new(nothrow) Compartment(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given Compartment_t structure.
 *
 * @param c the Compartment_t structure to be freed.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c)
{
  delete c;
}


/**
 * Creates a deep copy of the given Compartment_t structure
 * 
 * @param p the Compartment_t structure to be copied
 * 
 * @return a (deep) copy of the given Compartment_t structure.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_clone (const Compartment_t* c)
{
  return static_cast<Compartment*>( c->clone() );
}


/**
 * Initializes the attributes of this Compartment_t structure to their defaults.
 *
 * The exact results depends on the %SBML Level and Version in use.  The
 * cases are currently the following:
 * 
 * @li (SBML Level 1 only) sets attribute "volume" to @c 1.0
 * @li (SBML Level 2 only) sets attribute "spatialDimensions" to @c 3
 * @li (SBML Level 2 only) sets attribute "constant" to @c 1 (true)
 *
 * @param p the Compartment_t structure to initialize
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c)
{
  c->initDefaults();
}


/**
 * Takes a Compartment_t structure and returns its identifier.
 *
 * @param c the Compartment_t structure whose identifier is sought
 * 
 * @return the identifier of the Compartment_t structure @p c, as a pointer
 * to a string.
 */
LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c)
{
  return c->isSetId() ? c->getId().c_str() : NULL;
}


/**
 * Takes a Compartment_t structure and returns its name.
 *
 * @param c the Compartment_t whose name is sought.
 *
 * @return the name of the Compartment_t structure @p c, as a pointer to a
 * string.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c)
{
  return c->isSetName() ? c->getName().c_str() : NULL;
}


/**
 * Get the compartment type of this Compartment, as indicated by the
 * Compartment_t structure's "compartmentType" attribute.
 *
 * @param c the Compartment_t structure
 * 
 * @return the value of the "compartmentType" attribute of the
 * Compartment_t structure @p c as a string.
 */
LIBSBML_EXTERN
const char *
Compartment_getCompartmentType (const Compartment_t *c)
{
  return c->isSetCompartmentType() ? c->getCompartmentType().c_str() : NULL;
}


/**
 * Get the number of spatial dimensions of this Compartment_t structure.
 *
 * @param c the Compartment_t structure
 * 
 * @return the value of the "spatialDimensions" attribute of the
 * Compartment_t structure @p c as an unsigned integer
 */
LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c)
{
  return c->getSpatialDimensions();
}


/**
 * Get the size of this Compartment.
 *
 * This method is identical to Compartment_getVolume().  In SBML Level 1,
 * compartments are always three-dimensional constructs and only have
 * volumes, whereas in SBML Level 2, compartments may be other than
 * three-dimensional and therefore the "volume" attribute is named "size"
 * in Level 2.  LibSBML provides both Compartment_getSize() and
 * Compartment_getVolume() for easier compatibility between SBML Levels.
 *
 * @param c the Compartment_t structure
 *
 * @return the value of the "size" attribute ("volume" in Level 1) of
 * the Compartment_t structure @p c as a float-point number.
 *
 * @see Compartment_isSetSize()
 */
LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c)
{
  return c->getSize();
}


/**
 * (For SBML Level 1) Get the volume of this Compartment
 * 
 * This method is identical to Compartment_getSize().  In SBML Level 1,
 * compartments are always three-dimensional constructs and only have
 * volumes, whereas in SBML Level 2, compartments may be other than
 * three-dimensional and therefore the "volume" attribute is named "size"
 * in Level 2.  LibSBML provides both Compartment_getSize() and
 * Compartment_getVolume() for easier compatibility between SBML Levels.
 *
 * @param c the Compartment_t structure
 *
 * @return the value of the "volume" attribute ("size" in Level 2) of
 * the Compartment_t structure @p c, as a floating-point number.
 *
 * @see Compartment_isSetVolume()
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c)
{
  return c->getVolume();
}


/**
 * Get the units of this compartment's size or volume.
 *
 * @param c the Compartment_t structure
 * 
 * @return the value of the "units" attribute of the Compartment_t
 * structure @p c.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c)
{
  return c->isSetUnits() ? c->getUnits().c_str() : NULL;
}


/**
 * Get the identifier, if any, of the compartment that is designated
 * as being outside of this one.
 *
 * @param c the Compartment_t structure
 * 
 * @return the value of the "outside" attribute of the Compartment_t
 * structure @p c.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c)
{
  return c->isSetOutside() ? c->getOutside().c_str() : NULL;
}


/**
 * Get the value of the "constant" attribute of this Compartment.
 *
 * @param c the Compartment_t structure
 *
 * @return @c true if the Compartment_t structure's size is flagged as
 * being constant, @c false otherwise.
 */
LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c)
{
  return static_cast<int>( c->getConstant() );
}


/**
 * Predicate indicating whether the identifier of the given Compartment_t
 * structure has been set.
 * 
 * @param c the Compartment_t structure
 * 
 * @return true (non-zero) if the "id" attribute of the Compartment_t
 * structure @p c has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c)
{
  return static_cast<int>( c->isSetId() );
}


/**
 * Predicate indicating whether the name of the given Compartment_t
 * structure has been set.
 * 
 * @param c the Compartment_t structure
 * 
 * @return true (non-zero) if the "name" attribute of the Compartment_t
 * structure @p c has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c)
{
  return static_cast<int>( c->isSetName() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structures's "compartmentType" attribute has been set.
 *
 * @param c the Compartment_t structure
 * 
 * @return @c true if the "compartmentType" attribute of the Compartment_t
 * structure @p c has been set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetCompartmentType (const Compartment_t *c)
{
  return static_cast<int>( c->isSetCompartmentType() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structure's "size" attribute has been set.
 *
 * This method is similar but not identical to Compartment_isSetVolume().
 * The latter should be used in the context of SBML Level 1 models instead
 * of Compartment_isSetSize() because Compartment_isSetVolume() performs
 * extra processing to take into account the difference in default values
 * between SBML Levels 1 and 2.
 *
 * @param c the Compartment_t structure
 * 
 * @return @c true if the "size" attribute ("volume" in Level) of the
 * Compartment_t structure @p c has been set, @c false otherwise.
 *
 * @see Compartment_isSetVolume(), Compartment_setSize()
 */
LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c)
{
  return static_cast<int>( c->isSetSize() );
}


/**
 * (For SBML Level 1) Predicate returning @c true or @c false depending on
 * whether the given Compartment_t structures's "volume" attribute has been
 * set.
 * 
 * This method is similar but not identical to Compartment_isSetSize().
 * The latter should not be used in the context of SBML Level 1 models
 * because this method (Compartment_isSetVolume()) performs extra
 * processing to take into account the difference in default values between
 * SBML Levels 1 and 2.
 *
 * @param c the Compartment_t structure
 * 
 * @return @c true if the "volume" attribute ("size" in L2) of the given
 * Compartment_t structure @p c has been set, @c false otherwise.
 *
 * @see Compartment_isSetSize(), Compartment_setVolume()
 *
 * @note In SBML Level 1, a compartment's volume has a default value (@c
 * 1.0) and therefore this method will always return @c true.  In Level
 * 2, a compartment's size (the equivalent of SBML Level 1's "volume") is
 * optional and has no default value, and therefore may or may not be
 * set.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c)
{
  return static_cast<int>( c->isSetVolume() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structures's "units" attribute has been set.
 *
 * @param c the Compartment_t structure
 * 
 * @return @c true if the "units" attribute of the Compartment_t structure
 * @p c has been set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c)
{
  return static_cast<int>( c->isSetUnits() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Compartment_t structure's "outside" attribute has been set.
 *
 * @param c the Compartment_t structure
 * 
 * @return @c true if the "outside" attribute of the Compartment_t
 * structure @p c has been set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c)
{
  return static_cast<int>( c->isSetOutside() );
}


/**
 * Sets the identifier of the given Compartment_t structure.
 *
 * This function copies the string given in @p sid.  If the string is
 * NULL, this function performs unsetId() instead.
 *
 * @param c the Compartment_t structure.
 * @oaram sid the identifier to which the structures "id" attribute should
 * be set.
 */
LIBSBML_EXTERN
void
Compartment_setId (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetId() : c->setId(sid);
}


/**
 * Sets the name of the given Compartment_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * NULL, this function performs unsetName() instead.
 *
 * @param c the Compartment_t structure
 *
 * @oaram string the identifier to which the structures "id" attribute
 * should be set.
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *name)
{
  (name == NULL) ? c->unsetName() : c->setName(name);
}


/**
 * Sets the "compartmentType" attribute of the given Compartment_t
 * structure.
 *
 * This function copies the string given in @p string.  If the string is
 * NULL, this function performs unsetName() instead.
 *
 * @param c the Compartment_t structure
 * @param sid, the identifier of a CompartmentType object defined
 * elsewhere in the enclosing Model_t structure.
 */
LIBSBML_EXTERN
void
Compartment_setCompartmentType (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetCompartmentType() : c->setCompartmentType(sid);
}


/**
 * Sets the "spatialDimensions" attribute of the given Compartment_t
 * structure.
 *
 * If @p value is not one of @c 0, @c 1, @c 2, or @c 3, this method will
 * have no effect (i.e., the "spatialDimensions" attribute will not be
 * set).
 * 
 *
 * @param c the Compartment_t structure
 * @param value an unsigned integer indicating the number of dimensions
 * of the given compartment.
 */
LIBSBML_EXTERN
void
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value)
{
  c->setSpatialDimensions(value);
}


/**
 * Sets the "size" attribute (or "volume" in SBML Level 1) of the given
 * Compartment_t structure.
 *
 * This method is identical to Compartment_setVolume() and is provided for
 * compatibility between SBML Level 1 and Level 2.
 *
 * @param c the Compartment_t structure
 * @param value a @c double representing the size of the given
 * Compartment_t structure in whatever units are in effect
 */
LIBSBML_EXTERN
void
Compartment_setSize (Compartment_t *c, double value)
{
  c->setSize(value);
}


/**
 * Sets the "volume" attribute (or "size" in SBML Level 2) of the givenq
 * Compartment_t structure.
 *
 * This method is identical to setVolume() and is provided for
 * compatibility between SBML Level 1 and Level 2.
 *
 * @param c the Compartment_t structure
 * 
 * @param value a @c double representing the volume of the given
 * Compartment_t structure in whatever units are in effect
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value)
{
  c->setVolume(value);
}


/**
 * Sets the "units" attribute of the given Compartment_t structure.
 *
 * @param c the Compartment_t structure
 * 
 * @param sid the identifier of the defined units to use.  The string will
 * be copied.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetUnits() : c->setUnits(sid);
}


/**
 * Sets the "outside" attribute of the given Compartment_t structure.
 *
 * @param c the Compartment_t structure
 * 
 * @param sid the identifier of a compartment that encloses this one.  The
 * string will be copied.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sid)
{
  (sid == NULL) ? c->unsetOutside() : c->setOutside(sid);
}


/**
 * Sets the value of the "constant" attribute of the given Compartment_t
 * structure.
 *
 * @param c the Compartment_t structure
 * @param value an integer indicating whether the size/volume of the
 * compartment @p c should be considered constant (nonzero) or variable (zero).
 */
LIBSBML_EXTERN
void
Compartment_setConstant (Compartment_t *c, int value)
{
  c->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the name of the given Compartment_t structure.
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetName (Compartment_t *c)
{
  c->unsetName();
}


/**
 * Unsets the value of the "compartmentType" attribute of the given
 * Compartment_t structure.
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetCompartmentType (Compartment_t *c)
{
  c->unsetCompartmentType();
}


/**
 * Unsets the value of the "size" attribute of the given Compartment_t
 * structure. 
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetSize (Compartment_t *c)
{
  c->unsetSize();
}


/**
 * (For SBML Level 1) Unsets the value of the "volume" attribute of the 
 * given Compartment_t structure.
 *
 * In SBML Level 1, a Compartment_t structure's "volume" attribute has a
 * default value (1.0) and therefore <em>should always be set</em>.  In
 * Level 2, "size" is optional with no default value and as such may or may
 * not be set.
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c)
{
  c->unsetVolume();
}


/**
 * Unsets the value of the "units" attribute of the given Compartment_t
 * structure.
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetUnits (Compartment_t *c)
{
  c->unsetUnits();
}


/**
 * Unsets the value of the "outside" attribute of the given Compartment_t
 * structure.
 *
 * @param c the Compartment_t structure
 */
LIBSBML_EXTERN
void
Compartment_unsetOutside (Compartment_t *c)
{
  c->unsetOutside();
}



/** @endcond doxygen-c-only */
