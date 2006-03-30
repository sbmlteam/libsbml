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


#include "xml/XMLAttributes.h"
#include "xml/XMLInputStream.h"
#include "xml/XMLOutputStream.h"

#include "SBMLVisitor.h"
#include "Unit.h"
#include "UnitDefinition.h"


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
 * Adds a copy of the given Unit to this UnitDefinition.
 */
void
UnitDefinition::addUnit (const Unit* u)
{
  mUnits.append(u);
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
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
UnitDefinition::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  return (name == "listOfUnits") ? &mUnits : 0;
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
}

 
/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
UnitDefinition::writeAttributes (XMLOutputStream& stream)
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();

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
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
UnitDefinition::writeElements (XMLOutputStream& stream)
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
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
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
 * Sets the name of this UnitDefinition to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  (name == NULL) ? ud->unsetName() : ud->setName(name);
}


/**
 * Unsets the name of this UnitDefinition.  This is equivalent to:
 * safe_free(ud->name); ud->name = NULL;
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
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
  ud->getUnit(n);
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
