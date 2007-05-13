/**
 * @file    UnitDefinition.cpp
 * @brief   Implementation of %SBML's UnitDefinition.
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Unit.h>
#include <sbml/UnitDefinition.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new UnitDefinition, optionally with its id and name
 * attributes set.
 */
UnitDefinition::UnitDefinition (const string& sid, const string& name) :
  SBase(sid, name)
{
}


/**
 * Destroys this UnitDefinition.
 */
UnitDefinition::~UnitDefinition ()
{
}


/**
 * Copy constructor. Creates a copy of this UnitDefinition.
 */
UnitDefinition::UnitDefinition(const UnitDefinition& orig) :
          SBase     (orig)
        , mUnits    (orig.mUnits)
{
}


/**
 * Assignment operator.
 */
UnitDefinition& UnitDefinition::operator=(const UnitDefinition& rhs)
{
  this->SBase::operator =(rhs);
  mUnits = rhs.mUnits;
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * UnitDefinition (if available).
 */
bool
UnitDefinition::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);
  mUnits.accept(v);

  return result;
}


/**
 * @return a (deep) copy of this UnitDefinition.
 */
SBase*
UnitDefinition::clone () const
{
  return new UnitDefinition(*this);
}


/**
 * @return true if this UnitDefinition is a variant of the builtin type
 * area. i.e. square metres with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfArea () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = u->isMetre() && u->getExponent() == 2;
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of the builtin type
 * length. i.e. metres with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfLength () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = u->isMetre() && u->getExponent() == 1;
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of the builtin type
 * substance. i.e. moles or items with only abritrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfSubstance () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = (u->isMole() || u->isItem()) && u->getExponent() == 1;
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of the builtin type
 * time. i.e. seconds with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfTime () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = u->isSecond() && u->getExponent() == 1;
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of the builtin type
 * volume. i.e. litre or cubic metre with only abritrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfVolume () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = (u->isLitre() && u->getExponent() == 1) ||
                    (u->isMetre() && u->getExponent() == 3);
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of dimensionless.
 * i.e. dimensionless with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfDimensionless () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = u->isDimensionless();
  }

  return result;
}


/**
 * @return true if this UnitDefinition is a variant of mass. ie gram or
 * kilogram with only abritrary variations in scale or multiplier
 * values, false otherwise.
 */
bool
UnitDefinition::isVariantOfMass () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = (u->isGram() || u->isKilogram());
  }

  return result;
}


/**
 * Adds a copy of the given Unit to this UnitDefinition.
 */
void
UnitDefinition::addUnit (const Unit* u)
{
  mUnits.append(u);
}


/**
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
Unit*
UnitDefinition::createUnit ()
{
  Unit* u = new Unit;
  mUnits.appendAndOwn(u);

  return u;
}


/**
 * @return the list of Units for this UnitDefinition.
 */
const ListOfUnits*
UnitDefinition::getListOfUnits () const
{
  return &mUnits;
}


/**
 * @return the list of Units for this UnitDefinition.
 */
ListOfUnits*
UnitDefinition::getListOfUnits ()
{
  return &mUnits;
}


/**
 * @return the nth Unit of this UnitDefinition
 */
const Unit*
UnitDefinition::getUnit (unsigned int n) const
{
  return static_cast<const Unit*>( mUnits.get(n) );
}


/**
 * @return the nth Unit of this UnitDefinition
 */
Unit*
UnitDefinition::getUnit (unsigned int n)
{
  return static_cast<Unit*>( mUnits.get(n) );
}


/**
 * @return the number of Units in this UnitDefinition.
 */
unsigned int
UnitDefinition::getNumUnits () const
{
  return mUnits.size();
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
UnitDefinition::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mUnits.setSBMLDocument(d);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
UnitDefinition::getTypeCode () const
{
  return SBML_UNIT_DEFINITION;
}


/**
 * @return the name of this element ie "unitDefinition".
 */
const string&
UnitDefinition::getElementName () const
{
  static const string name = "unitDefinition";
  return name;

}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
UnitDefinition::readOtherXML (XMLInputStream& stream)
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
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
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
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
UnitDefinition::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  if (name == "listOfUnits")
  {
    if (mUnits.size() != 0)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    return &mUnits;
  }
  
  return 0;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
UnitDefinition::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
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
UnitDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
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
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
    SBO::writeTerm(stream, mSBOTerm);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
UnitDefinition::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if ( getNumUnits() > 0 ) mUnits.write(stream);
}




/**
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfUnitDefinitions::clone () const
{
  return new ListOfUnitDefinitions(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfUnitDefinitions::getItemTypeCode () const
{
  return SBML_UNIT_DEFINITION;
}


/**
 * @return the name of this element ie "listOfUnitDefinitions".
 */
const string&
ListOfUnitDefinitions::getElementName () const
{
  static const string name = "listOfUnitDefinitions";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfUnitDefinitions::getElementPosition () const
{
  return 2;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfUnitDefinitions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "unitDefinition")
  {
    object = new UnitDefinition();
    mItems.push_back(object);
  }

  return object;
}


/** @cond doxygen-c-only */


/**
 * Creates a new, empty UnitDefinition_t structure.
 *
 * @note It is worth emphasizing that the attribute "id" value of a
 * UnitDefinition_t is a required attribute.  UnitDefinition_create() does
 * not assign a valid "id" to the constructed unit; instead, it leaves it
 * as the empty string.  Callers are cautioned to set the newly-constructed
 * UnitDefinition_t's "id" using UnitDefinition_setId() soon after calling
 * this function.
 *
 * @return the new UnitDefinition_t structure
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void)
{
  return new(nothrow) UnitDefinition;
}


/**
 * Creates a new UnitDefinition_t structure with the given identifier @p id
 * and returns a pointer to the structure.
 * 
 * The permitted values of the identifier @p id @em exclude the predefined
 * base units in SBML and two spelling variants @c "meter" and @c "liter".
 * The following is the set of base unit names which may @em not be used as
 * a value of @p id:
 *
 * <table align="center" style="font-family: Courier, fixed; font-weight: bold; font-size: 12px;" cellspacing="7" border="0">
 * <tr><td>ampere</td><td>gram</td><td>katal</td><td>metre</td><td>second</td><td>watt</td></tr>
 * <tr><td>becquerel</td><td>gray</td><td>kelvin</td><td>mole</td><td>siemens</td><td>weber</td></tr>
 * <tr><td>candela</td><td>henry</td><td>kilogram</td><td>newton</td><td>sievert</td><td>Celsius</td></tr>
 * <tr><td>coulomb</td><td>hertz</td><td>litre</td><td>ohm</td><td>steradian</td><td>meter</td></tr>
 * <tr><td>dimensionless</td><td>item</td><td>lumen</td><td>pascal</td><td>tesla</td><td>liter</td></tr>
 * <tr><td>farad</td><td>joule</td><td>lux</td><td>radian</td><td>volt</td></tr>
 * </table>
 *
 * In addition, there is a set of predefined identifiers for the built-in
 * default units in SBML.  These identifiers are @c substance, @c volume,
 * @c area, @c length, and @c time.  Using one of these values for the
 * attribute @p id of a UnitDefinition_t has the effect of redefining the
 * model-wide default units for the corresponding quantities.  The list
 * of built-in units is given in the table below:
 * @image html built-in-units.jpg "SBML's built-in units"
 * @image latex built-in-units.jpg "SBML's built-in units"
 * 
 * Finally, note that SBML imposes two limitations on redefining the
 * built-in units listed above:
 * 
 * - The UnitDefinition_t of a redefined built-in unit can only
 *   contain a single Unit object within it.
 * 
 * - The value of the "kind" attribute in a Unit instance must be drawn
 *   from one of the values in the second column of the table above.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   UnitDefinition_setId(UnitDefinition_create(), id);
 * @endcode
 *
 * @param id the identifier to assign to the new unit definition.
 *
 * @return the new UnitDefinition_t structure.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *id)
{
  return new(nothrow) UnitDefinition(id ? id : "");
}


/**
 * Creates a new UnitDefinition_t structure with the given @p name
 * and returns a pointer to the structure.
 * 
 * @note It is worth emphasizing that the attribute "id" value of a
 * UnitDefinition_t is a required attribute.  UnitDefinition_create() does
 * not assign a valid "id" to the constructed unit; instead, it leaves it
 * as the empty string.  Callers are cautioned to set the newly-constructed
 * UnitDefinition_t's "id" using UnitDefinition_setId() soon after calling
 * this function.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   UnitDefinition_setName(UnitDefinition_create(), name);
 * @endcode
 *
 * @param name the name to assign to the new unit definition.
 *
 * @return the new UnitDefinition_t structure.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *name)
{
  return new(nothrow) UnitDefinition("", name ? name : "");
}


/**
 * Frees the given UnitDefinition_t.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  delete ud;
}


/**
 * Creates and returns a deep copy of the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure to copy
 * 
 * @return a (deep) copy of UnitDefinition_t.
 */
LIBSBML_EXTERN
UnitDefinition_t*
UnitDefinition_clone (const UnitDefinition_t *ud)
{
  return static_cast<UnitDefinition_t*>( ud->clone() );
}


/**
 * Returns the identifier of this UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose identifier is sought
 * 
 * @return the value of the "id" attribute of this UnitDefinition_t.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  return ud->isSetId() ? ud->getId().c_str() : NULL;
}


/**
 * Returns the name of this UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose name is sought
 * 
 * @return the value of the "name" attribute of this UnitDefinition_t.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  return ud->isSetName() ? ud->getName().c_str() : NULL;
}


/**
 * Predicate to test whether the "id" attribute of the given UnitDefinition_t
 * @p ud has been set.
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return nonzero (true) if the "id" attribute of the given
 * UnitDefinition_t has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetId() );
}


/**
 * Predicate to test whether the "name" attribute of the given
 * UnitDefinition_t @p ud has been set.
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return nonzero (true) if the "name" attribute of the given
 * UnitDefinition_t has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetName() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "area".
 * 
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if the UnitDefinition_t structure @p ud is a variant of
 * the built-in unit @c area, meaning square metres with only abritrary
 * variations in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfArea (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfArea() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "length".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition_t is a variant of the built-in
 * unit @c length, meaning metres with only abritrary variations in scale
 * or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfLength (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfLength() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "substance".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit substance, meaning moles or items with only abritrary variations
 * in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstance (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfSubstance() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "time".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit time, meaning seconds with only abritrary variations in scale or
 * multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfTime (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfTime() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "volume".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit volume, meaning litre or cubic metre with only abritrary
 * variations in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfVolume (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfVolume() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the unit @c "dimensionless".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of dimensionless,
 * meaning dimensionless with only abritrary variations in scale or
 * multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int 
UnitDefinition_isVariantOfDimensionless (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfDimensionless() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "mass".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of mass, meaning
 * gram or kilogram with only abritrary variations in scale or multiplier
 * values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfMass (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfMass() );
}


/**
 * Sets the attribute "id" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose id is to be set
 * @param id a string, the new identifier for the UnitDefinition_t structure
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *id)
{
  (id == NULL) ? ud->unsetId() : ud->setId(id);
}


/**
 * Sets the attribute "name" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose name is to be set
 * @param name a string, the new name for the UnitDefinition_t structure
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  (name == NULL) ? ud->unsetName() : ud->setName(name);
}


/**
 * Unsets the name of the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose name is to be unset.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  ud->unsetName();
}


/**
 * Adds a copy of the given Unit to the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure.
 * @param u the Unit instance to add.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u)
{
  if (u != NULL) ud->addUnit(u);
}


/**
 * Creates a new and empty Unit_t structure, adds it to the given
 * UnitDefinition_t structure's list of units, and returns the Unit_t
 * structure.
 *
 * @return a newly constructed (and empty) Unit_t structure.
 * 
 * @note It is worth emphasizing that the attribute "kind" value of a
 * Unit_t is a required attribute for a valid Unit_t definition.  The
 * UnitDefinition_createUnit() method does not assign a valid kind to the
 * constructed unit (instead, it sets the "kind" to UNIT_KIND_INVALID).
 * Callers are cautioned to set the newly-constructed Unit's kind using
 * UnitDefinition_setKind() soon after calling this method.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_createUnit (UnitDefinition_t *ud)
{
  return ud->createUnit();
}


/**
 * Returns the list of Units for the given UnitDefinition_t structure.
 * 
 * @param ud the UnitDefinition_t to use
 *
 * @return the ListOfUnits value for the given UnitDefinition_t.
 */
LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud)
{
  return ud->getListOfUnits();
}


/**
 * Returns a specific Unit_t instance belonging to the given
 * UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure in question
 * @param n an integer, the index of the Unit_t structure to be returned.
 * 
 * @return the nth Unit_t of this UnitDefinition_t structure.
 *
 * @see UnitDefinition_getNumUnits()
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (UnitDefinition_t *ud, unsigned int n)
{
  return ud->getUnit(n);
}


/**
 * Returns the number of Unit_t structures contained within this
 * UnitDefinition_t.
 *
 * @param ud the UnitDefinition_t structure in question
 * 
 * @return an integer representing the number of Unit_t structures in this
 * UnitDefinition_t structure.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return ud->getNumUnits();
}


/** @endcond doxygen-c-only */
