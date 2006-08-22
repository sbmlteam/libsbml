/**
 * \file    Parameter.cpp
 * \brief   SBML Parameter
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

#include "SBML.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "KineticLaw.h"
#include "Parameter.h"


using namespace std;


/**
 * Creates a new Parameter, optionally with its id and name attributes
 * set.
 */
Parameter::Parameter (const string& id, const string& name) :
    SBase      ( id, name )
  , mValue     ( 0.0      )
  , mConstant  ( true     )
  , mSBOTerm   ( -1       )
  , mIsSetValue( false    )
{
}


/**
 * Creates a new Parameter, with its id and value attributes set and
 * optionally its units and constant attributes.
 */
Parameter::Parameter (   const string&  id
                       , double         value
                       , const string&  units
                       , bool           constant ) :
    SBase      ( id       )
  , mValue     ( value    )
  , mUnits     ( units    )
  , mConstant  ( constant )
  , mSBOTerm   ( -1       )
  , mIsSetValue( true     )

{
}


/**
 * Destroys this Parameter.
 */
Parameter::~Parameter ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the parent Model's or
 * KineticLaw's next Parameter (if available).
 */
bool
Parameter::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Parameter.
 */
SBase*
Parameter::clone () const
{
  return new Parameter(*this);
}


/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = true  (L2 only)
 */
void
Parameter::initDefaults ()
{
  setConstant(true);
}


/**
 * @return the value of this Parameter.
 */
double
Parameter::getValue () const
{
  return mValue;
}


/**
 * @return the units of this Parameter.
 */
const string&
Parameter::getUnits () const
{
  return mUnits;
}


/**
 * @return true if this Parameter is constant, false otherwise.
 */
bool
Parameter::getConstant () const
{
  return mConstant;
}


/**
 * @return the sboTerm of this Parameter as an integer.  If not set,
 * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
int
Parameter::getSBOTerm () const
{
  return mSBOTerm;
}


/**
 * @return true if the value of this Parameter has been set, false
 * otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
bool
Parameter::isSetValue () const
{
  return mIsSetValue;
}


/**
 * @return true if the units of this Parameter has been set, false
 * otherwise.
 */
bool
Parameter::isSetUnits () const
{
  return (mUnits.empty() == false);
}


/**
 * @return true if the sboTerm of this Parameter has been set, false
 * otherwise.
 */
bool
Parameter::isSetSBOTerm () const
{
  return SBML::checkSBOTerm(mSBOTerm);
}


/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
void
Parameter::setValue (double value)
{
  mValue      = value;
  mIsSetValue = true;
}


/**
 * Sets the units of this Parameter to a copy of sid.
 */
void
Parameter::setUnits (const string& sid)
{
  mUnits = sid;
}


/**
 * Sets the constant field of this Parameter to value.
 */
void
Parameter::setConstant (bool value)
{
  mConstant = value;
}


/**
 * Sets the sboTerm field of this Parameter to value.
 */
void
Parameter::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
void
Parameter::unsetValue ()
{
  mValue      = numeric_limits<double>::quiet_NaN();
  mIsSetValue = false;
}


/**
 * Unsets the units of this Parameter.
 */
void
Parameter::unsetUnits ()
{
  mUnits.erase();
}


/**
 * Unsets the sboTerm of this Parameter.
 */
void
Parameter::unsetSBOTerm ()
{
  mSBOTerm = -1;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Parameter::getTypeCode () const
{
  return SBML_PARAMETER;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Parameter::getElementName () const
{
  static const string name = "parameter";
  return name;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Parameter::readAttributes (const XMLAttributes& attributes)
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
  // value: double  { use="required" }  (L1v2)
  // value: double  { use="optional" }  (L1v2, L2v1, L2v2)
  //
  mIsSetValue = attributes.readInto("value", mValue);

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1, L2v2)
  //
  attributes.readInto("units", mUnits);

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("constant", mConstant);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) mSBOTerm = SBML::readSBOTerm(attributes);
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Parameter::writeAttributes (XMLOutputStream& stream) const
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
  // value: double  { use="required" }  (L1v1)
  // value: double  { use="optional" }  (L1v2, L2v1, L2v2)
  //
  if (mIsSetValue || (level == 1 && version == 1))
  {
    stream.writeAttribute("value", mValue);
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1, L2v2)
  //
  stream.writeAttribute("units", mUnits);

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1, L2v2)
  //
  if (level == 2 && mConstant != true)
  {
    stream.writeAttribute("constant", mConstant);
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) SBML::writeSBOTerm(stream, mSBOTerm);
}




/**
 * @return a (deep) copy of this ListOfParameters.
 */
SBase*
ListOfParameters::clone () const
{
  return new ListOfParameters(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfParameters::getItemTypeCode () const
{
  return SBML_UNIT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfParameters::getElementName () const
{
  static const string name = "listOfParameters";
  return name;
}


/**
 * returns expected position of ListOfParameters in a model
 */
int
ListOfParameters::getElementPosition() const
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

  if (this->getSBMLDocument()->getModel()->getNumSpecies() != 0)
    position++;

  return position;

}


/**
 * returns expected position of ListOfParameters in a kinetic law
 */
int
ListOfParameters::getElementPosition(unsigned int reactionNo) const
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  int position = 1;
  /**
   * the expected position of each element depends on the type
   * and any lists ahead
   */

  const Reaction * r = this->getSBMLDocument()->getModel()->getReaction(reactionNo-1);

  /**
   * in a level 1 model the kineticLaw is a formula NOT mathML
   * and so is not counted as an element
   */
  if (level == 2) 
  {
    if (r->isSetKineticLaw() && r->getKineticLaw()->isSetMath())
      position++;
  }
  
  return position;

}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfParameters::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "parameter")
  {
    object = new Parameter();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void)
{
  return new(nothrow) Parameter;
}


/**
 * Creates a new Parameter with the given id, value and units and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setName(p, id); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *sid, double value, const char *units)
{
  return
    new(nothrow) Parameter(sid ? sid : "", value, units ? units : "");
}


/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p)
{
  delete p;
}


/**
 * @return a (deep) copy of the given Parameter.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_clone (const Parameter_t *p)
{
  return static_cast<Parameter_t*>( p->clone() );
}


/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = 1  (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p)
{
  p->initDefaults();
}


/**
 * @return the id of this Parameter
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p)
{
  return p->isSetId() ? p->getId().c_str() : NULL;
}


/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p)
{
  return p->isSetName() ? p->getName().c_str() : NULL;
}


/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p)
{
  return p->getValue();
}


/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p)
{
  return p->isSetUnits() ? p->getUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Parameter is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p)
{
  return p->getConstant();
}

/**
 * @return the sboTerm of this Parameter as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Parameter_getSBOTerm (const Parameter_t *p)
{
  return p->getSBOTerm();
}


/**
 * @return true (non-zero) if the id of this Parameter has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p)
{
  return static_cast<int>( p->isSetId() );
}


/**
 * @return true (non-zero) if the name of this Parameter has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p)
{
  return static_cast<int>( p->isSetName() );
}


/**
 * @return true (non-zero) if the value of this Parameter has been set,
 * false (0) otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p)
{
  return static_cast<int>( p->isSetValue() );
}


/**
 * @return true (non-zero) if the units of this Parameter has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p)
{
  return static_cast<int>( p->isSetUnits() );
}


/**
 * @return true (non-zero) if the sboTerm of this Parameter has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetSBOTerm (const Parameter_t *p)
{
  return static_cast<int>( p->isSetSBOTerm() );
}


/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid)
{
  (sid == NULL) ? p->unsetId() : p->setId(sid);
}


/**
 * Sets the name of this Parameter to a copy of string.
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *str)
{
  (str == NULL) ? p->unsetName() : p->setName(str);
}


/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value)
{
  p->setValue(value);
}


/**
 * Sets the units field of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sid)
{
  (sid == NULL) ? p->unsetUnits() : p->setUnits(sid);
}


/**
 * Sets the constant field of this Parameter to value (boolean).
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value)
{
  p->setConstant( static_cast<bool>(value) );
}


/**
 * Sets the sboTerm field of this Parameter to value.
 */
LIBSBML_EXTERN
void
Parameter_setSBOTerm (Parameter_t *p, int sboTerm)
{
  p->setSBOTerm(sboTerm);
}


/**
 * Unsets the name of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p)
{
  p->unsetName();
}


/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p)
{
  p->unsetValue();
}


/**
 * Unsets the units of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p)
{
  p->unsetUnits();
}


/**
 * Unsets the sboTerm of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter_unsetSBOTerm (Parameter_t *p)
{
  p->unsetSBOTerm();
}
