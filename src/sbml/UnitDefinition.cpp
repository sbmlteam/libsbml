/**
 * \file    UnitDefinition.cpp
 * \brief   SBML UnitDefinition
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include "SBML.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Unit.h"
#include "UnitDefinition.h"

#include <sbml/SBML.h>

using namespace std;


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
 * area, i.e. square metres with only abritrary variations in scale,
 * multiplier, or offset values, false otherwise.
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
 * length, i.e. metres with only abritrary variations in scale,
 * multiplier, or offset values, false otherwise.
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
 * substance, i.e. moles or items with only abritrary variations in
 * scale, multiplier, or offset values, false otherwise.
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
 * time, i.e. seconds with only abritrary variations in scale,
 * multiplier, or offset values, false otherwise.
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
 * volume, i.e. litre or cubic metre with only abritrary variations in
 * scale, multiplier, or offset values, false otherwise.
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
 * @return true if this UnitDefinition is a variant of dimensionless
 * i.e. dimensionless with only abritrary variations in scale, multiplier,
 * or offset values, false otherwise.
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
 * @return true if this UnitDefinition is a variant of mass i.e. gram or
 * kilogram with only abritrary variations in scale, multiplier, or offset
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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
    mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
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
    SBML::writeSBOTerm(stream, mSBOTerm);
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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




/**
 * Creates a new UnitDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void)
{
  return new(nothrow) UnitDefinition;
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setId(UnitDefinition_create(), sid);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *sid)
{
  return new(nothrow) UnitDefinition(sid ? sid : "");
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setName(UnitDefinition_create(), name);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *name)
{
  return new(nothrow) UnitDefinition("", name ? name : "");
}


/**
 * Frees the given UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  delete ud;
}


/**
 * @return a (deep) copy of this UnitDefinition.
 */
LIBSBML_EXTERN
UnitDefinition_t*
UnitDefinition_clone (const UnitDefinition_t *ud)
{
  return static_cast<UnitDefinition_t*>( ud->clone() );
}


/**
 * @return the id of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  return ud->isSetId() ? ud->getId().c_str() : NULL;
}


/**
 * @return the name of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  return ud->isSetName() ? ud->getName().c_str() : NULL;
}


/**
 * @return non-zero if the id of this UnitDefinition has been set, zero
 * otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetId() );
}


/**
 * @return non-zero if the name of this UnitDefinition has been set, zero
 * otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetName() );
}


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * area, i.e. square metres with only abritrary variations in scale,
 * multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfArea (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfArea() );
}


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * length, i.e. metres with only abritrary variations in scale, multiplier,
 * or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfLength (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfLength() );
}


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * substance, i.e. moles or items with only abritrary variations in scale,
 * multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstance (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfSubstance() );
}


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * time, i.e. seconds with only abritrary variations in scale, multiplier,
 * or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfTime (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfTime() );
}


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * volume, i.e. litre or cubic metre with only abritrary variations in
 * scale, multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfVolume (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfVolume() );
}


/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *sid)
{
  (sid == NULL) ? ud->unsetId() : ud->setId(sid);
}


/**
 * Sets the name of this UnitDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  (name == NULL) ? ud->unsetName() : ud->setName(name);
}


/**
 * Unsets the name of this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  ud->unsetName();
}


/**
 * Adds a copy of the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u)
{
  if (u != NULL) ud->addUnit(u);
}


/**
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_createUnit (UnitDefinition_t *ud)
{
  return ud->createUnit();
}


/**
 * @return the list of Units for this UnitDefinition.
 */
LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud)
{
  return ud->getListOfUnits();
}


/**
 * @return the nth Unit of this UnitDefinition
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (UnitDefinition_t *ud, unsigned int n)
{
  return ud->getUnit(n);
}


/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return ud->getNumUnits();
}
