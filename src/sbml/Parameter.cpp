/**
 * @file    Parameter.cpp
 * @brief   Definitions of Parameter and ListOfParamters.
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <limits>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/KineticLaw.h>
#include <sbml/Parameter.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Parameter, optionally with its id and name attributes
 * set.
 */
Parameter::Parameter (const string& id, const string& name) :
    SBase      ( id, name, -1 )
  , mValue     ( 0.0      )
  , mConstant  ( true     )
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
    SBase      ( id  ,"", -1     )
  , mValue     ( value    )
  , mUnits     ( units    )
  , mConstant  ( constant )
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
 * Copy constructor. Creates a copy of this Parameter.
 */
Parameter::Parameter(const Parameter& rhs) :
        SBase      ( rhs     )
      , mValue     ( rhs.mValue    )
      , mUnits     ( rhs.mUnits    )
      , mConstant  ( rhs.mConstant )
      , mIsSetValue( rhs.mIsSetValue)
{
}


/**
 * Assignment operator.
 */
Parameter& Parameter::operator=(const Parameter& rhs)
{
  this->SBase::operator =(rhs);
     mValue     = rhs.mValue    ;
     mUnits     = rhs.mUnits    ;
     mConstant  = rhs.mConstant ;
     mIsSetValue= rhs.mIsSetValue;
  return *this;
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
Parameter::setUnits (const string& units)
{
  mUnits = units;
}


/**
 * Sets the constant field of this Parameter to value.
 */
void
Parameter::setConstant (bool flag)
{
  mConstant = flag;
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
 * @return the name of this element ie "parameter".
 */
const string&
Parameter::getElementName () const
{
  static const string name = "parameter";
  return name;
}

/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @param stream the XMLInputStream to use.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Parameter::readOtherXML (XMLInputStream& stream)
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
 *
 * @param attributes the XMLAttributes object to use
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
  SBase::checkIdSyntax();

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
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 *
 * @param stream the XMLOutputStream to use
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
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
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
  return SBML_PARAMETER;
}


/**
 * @return the name of this element ie "listOfParameters".
 */
const string&
ListOfParameters::getElementName () const
{
  static const string name = "listOfParameters";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfParameters::getElementPosition () const
{
  return 7;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 *
 * @param stream the XMLInputStream to use
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


/** @cond doxygen-c-only */


/**
 * Creates a new, empty Parameter_t structure and returns a pointer to it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * is empty and that there are no default values assigned to such things as
 * identifiers and names.  Note that in SBML Level 2 and beyond, the
 * "id" (identifier) attribute of a Parameter is required to have a
 * value.  Thus, callers are cautioned to assign a value after calling this
 * constructor, for example using Parameter_setName().
 *
 * @return a pointer to the newly created Parameter_t structure.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void)
{
  return new(nothrow) Parameter;
}


/**
 * Creates a new Parameter_t structure with the given @p id and @p name
 * attribute values.
 *
 * In SBML Level 2 and beyond, the identifier attribute of a Parameter is
 * required to have a value, but the name is optional.  Programs calling
 * this function can legitimately use an empty string for the @p name
 * argument.  Likewise, the units of parameters are also optional, and
 * therefore the @p units argument legitimately can be an empty string in
 * an invocation.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setId(p, id);
 *   Parameter_setValue(p, value);
 *   Parameter_setUnits(p, units);
 * @endcode
 *
 * @param sid the value to assign as the identifier of this Parameter
 * @param value the value to assign as the name of this Parameter
 * @param units the value to assign as the units of this Parameter
 *
 * @return a pointer to the newly created Parameter_t structure.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *id, double value, const char *units)
{
  return
    new(nothrow) Parameter(id ? id : "", value, units ? units : "");
}


/**
 * Frees the given Parameter_t structure.
 *
 * @param p the Parameter_t structure to be freed.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p)
{
  delete p;
}


/**
 * Creates a deep copy of the given Parameter_t structure
 * 
 * @param p the Parameter_t structure to be copied
 * 
 * @return a (deep) copy of the given Parameter_t structure.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_clone (const Parameter_t *p)
{
  return static_cast<Parameter_t*>( p->clone() );
}


/**
 * Initializes the attributes of this Parameter_t structure to their defaults.
 *
 * The exact results depends on the %SBML Level and Version in use.  The
 * cases are currently the following:
 * 
 * @li (%SBML Level 2 only) constant = 1 (true)
 *
 * @param p the Parameter_t structure to initialize
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p)
{
  p->initDefaults();
}


/**
 * Takes a Parameter_t structure and returns its identifier.
 *
 * @param p the Parameter_t structure whose identifier is sought
 * 
 * @return the identifier of this Parameter_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p)
{
  return p->isSetId() ? p->getId().c_str() : NULL;
}


/**
 * Takes a Parameter_t structure and returns its name.
 *
 * @param p the Parameter_t whose name is sought.

 * @return the name of this Parameter_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p)
{
  return p->isSetName() ? p->getName().c_str() : NULL;
}


/**
 * Takes a Parameter_t structure and returns its value.
 *
 * @param p the Parameter_t whose value is sought.
 *
 * @return the value assigned to this Parameter_t structure, as a @c double.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p)
{
  return p->getValue();
}


/**
 * Takes a Parameter_t structure and returns its units.
 *
 * @param p the Parameter_t whose units are sought.
 *
 * @return the units assigned to this Parameter_t structure, as a pointer
 * to a string.  
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p)
{
  return p->isSetUnits() ? p->getUnits().c_str() : NULL;
}


/**
 * Takes a Parameter_t structure and returns zero or nonzero, depending
 * on the value of the parameter's "constant" attribute.
 *
 * @param p the Parameter_t whose constant value is sought.
 *
 * @return the value of the "constant" attribute, with nonzero meaning
 * true and zero meaning false.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p)
{
  return p->getConstant();
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Parameter_t structure's identifier has been set.
 *
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "id" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p)
{
  return static_cast<int>( p->isSetId() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Parameter_t structure's name has been set.
 *
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "name" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p)
{
  return static_cast<int>( p->isSetName() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Parameter_t structure's value has been set.
 *
 * @note In SBML Level 1 Version 1, a Parameter value is required and
 * therefore <em>should always be set</em>.  In Level 1 Version 2 and
 * later, the value is optional, and as such, may or may not be set.
 * 
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "value" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 *
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p)
{
  return static_cast<int>( p->isSetValue() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Parameter_t structure's units have been set.
 *
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "units" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p)
{
  return static_cast<int>( p->isSetUnits() );
}


/**
 * Assigns the identifier of a Parameter_t structure.
 *
 * This makes a copy of the string passed in the param @p sid.
 *
 * @param p the Parameter_t structure to set.
 * @param sid the string to use as the identifier.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid)
{
  (sid == NULL) ? p->unsetId() : p->setId(sid);
}


/**
 * Assign the name of a Parameter_t structure.
 *
 * This makes a copy of the string passed in as the argument @p name.
 *
 * @param p the Parameter_t structure to set.
 * @param name the string to use as the name.
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *name)
{
  (name == NULL) ? p->unsetName() : p->setName(name);
}


/**
 * Assign the value of a Parameter_t structure.
 *
 * @param p the Parameter_t structure to set.
 * @param value the @c double value to use.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value)
{
  p->setValue(value);
}


/**
 * Assign the units of a Parameter_t structure.
 *
 * This makes a copy of the string passed in as the argument @p units.
 *
 * @param p the Parameter_t structure to set.
 * @param units the string to use as the identifier of the units to assign.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *units)
{
  (units == NULL) ? p->unsetUnits() : p->setUnits(units);
}


/**
 * Assign the "constant" attribute of a Parameter_t structure.
 *
 * @param p the Parameter_t structure to set.
 * @param value the value to assign as the "constant" attribute
 * of the parameter, either zero for false or nonzero for true.
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value)
{
  p->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the name of this Parameter_t structure.
 * 
 * @param p the Parameter_t structure whose name is to be unset.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p)
{
  p->unsetName();
}


/**
 * Unsets the value of this Parameter_t structure.
 *
 * In SBML Level 1 Version 1, a parameter is required to have a value and
 * therefore this attribute <em>should always be set</em>.  In Level 1
 * Version 2 and beyond, a value is optional, and as such, may or may not be
 * set.
 *
 * @param p the Parameter_t structure whose value is to be unset.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p)
{
  p->unsetValue();
}


/**
 * Unsets the units of this Parameter_t structure.
 * 
 * @param p the Parameter_t structure whose units are to be unset.
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p)
{
  p->unsetUnits();
}


/** @endcond doxygen-c-only */
