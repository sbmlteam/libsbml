/**
 * @file    Parameter.cpp
 * @brief   Implementations of Parameter and ListOfParameters.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/KineticLaw.h>
#include <sbml/Parameter.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

LIBSBML_CPP_NAMESPACE_BEGIN

Parameter::Parameter (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mId        ( ""       )
 , mName      ( ""       )
 , mValue     ( 0.0      )
 , mUnits     ( ""       )
 , mConstant  ( true     )
 , mIsSetValue( false    )
 , mIsSetConstant (false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (level == 3)
  {
    mValue = numeric_limits<double>::quiet_NaN();
  }
}


Parameter::Parameter (SBMLNamespaces * sbmlns) :
   SBase      ( sbmlns   )
 , mId        ( ""       )
 , mName      ( ""       )
 , mUnits     ( ""       )
 , mValue     ( 0.0      )
 , mConstant  ( true     )
 , mIsSetValue( false    )
 , mIsSetConstant (false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (sbmlns->getLevel() == 3)
  {
    mValue = numeric_limits<double>::quiet_NaN();
  }
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Parameter::Parameter() :
  SBase()
{
}

/** @endcond doxygen-libsbml-internal */
                          
/*
 * Destroys this Parameter.
 */
Parameter::~Parameter ()
{
}


/*
 * Copy constructor. Creates a copy of this Parameter.
 */
Parameter::Parameter(const Parameter& orig) :
    SBase      ( orig             )
  , mId        ( orig.mId         )  
  , mName      ( orig.mName       )
  , mValue     ( orig.mValue      )
  , mUnits     ( orig.mUnits      )
  , mConstant  ( orig.mConstant   )
  , mIsSetValue( orig.mIsSetValue )
  , mIsSetConstant (orig.mIsSetConstant )
{
}


/*
 * Assignment operator.
 */
Parameter& Parameter::operator=(const Parameter& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mValue      = rhs.mValue    ;
    mUnits      = rhs.mUnits    ;
    mConstant   = rhs.mConstant ;
    mIsSetValue = rhs.mIsSetValue;
    mId = rhs.mId;
    mName = rhs.mName;
    mIsSetConstant = rhs.mIsSetConstant;
  }

  return *this;
}


/*
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


/*
 * @return a (deep) copy of this Parameter.
 */
Parameter*
Parameter::clone () const
{
  return new Parameter(*this);
}


/*
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = true  (L2 only)
 */
void
Parameter::initDefaults ()
{
  // level 3 has no defaults
  //if (getLevel() < 3)
  //{
    setConstant(true);
  //}
}


/*
 * @return the id of this SBML object.
 */
const string&
Parameter::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
Parameter::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/*
 * @return the value of this Parameter.
 */
double
Parameter::getValue () const
{
  return mValue;
}


/*
 * @return the units of this Parameter.
 */
const string&
Parameter::getUnits () const
{
  return mUnits;
}


/*
 * @return true if this Parameter is constant, false otherwise.
 */
bool
Parameter::getConstant () const
{
  return mConstant;
}


/*
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
Parameter::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
Parameter::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
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


/*
 * @return true if the units of this Parameter has been set, false
 * otherwise.
 */
bool
Parameter::isSetUnits () const
{
  return (mUnits.empty() == false);
}


/*
 * @return true if the constant of this Parameter has been set, false
 * otherwise.
 */
bool
Parameter::isSetConstant () const
{
  return mIsSetConstant;
}


/*
 * Sets the id of this SBML object to a copy of sid.
 */
int
Parameter::setId (const std::string& sid)
{
  /* since the setId function has been used as an
   * alias for setName we cant require it to only
   * be used on a L2 model
   */
/*  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
*/
  if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the name of this SBML object to a copy of name.
 */
int
Parameter::setName (const std::string& name)
{
  /* if this is setting an L2 name the type is string
   * whereas if it is setting an L1 name its type is SId
   */
  if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidSBMLSId(name)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mId = name;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of this Parameter to value and marks the field as set.
 */
int
Parameter::setValue (double value)
{
  mValue      = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the units of this Parameter to a copy of sid.
 */
int
Parameter::setUnits (const std::string& units)
{
  if (!(SyntaxChecker::isValidUnitSId(units)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mUnits = units;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the constant field of this Parameter to value.
 */
int
Parameter::setConstant (bool flag)
{
  if ( getLevel() < 2 )
  {
    mConstant = flag;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mConstant = flag;
    mIsSetConstant = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
Parameter::unsetName ()
{
  if (getLevel() == 1) 
  {
    mId.erase();
  }
  else 
  {
    mName.erase();
  }

  if (getLevel() == 1 && mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
int
Parameter::unsetValue ()
{
  mValue      = numeric_limits<double>::quiet_NaN();
  mIsSetValue = false;
  if (!isSetValue())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the units of this Parameter.
 */
int
Parameter::unsetUnits ()
{
  mUnits.erase();

  if (mUnits.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
  * Constructs and returns a UnitDefinition that expresses the units of this 
  * Parameter.
  */
UnitDefinition *
Parameter::getDerivedUnitDefinition()
{
  /* if we have the whole model but it is not in a document
   * it is still possible to determine the units
   */
  Model * m = static_cast <Model *> (getAncestorOfType(SBML_MODEL));

  if (m)
  {
    if (!m->isPopulatedListFormulaUnitsData())
    {
      m->populateListFormulaUnitsData();
    }
    
    /* need to distinguish between a global and local parameter
    * for a global parameter a unit definition will have been created
    * for a local parameter need to create one based on the units field
    */
    bool globalParameter = false;
    if (getParentSBMLObject()->getParentSBMLObject()->getTypeCode() == SBML_MODEL)
      globalParameter = true;
    if (globalParameter)
    {
      if (m->getFormulaUnitsData(getId(), getTypeCode()))
      {
        return m->getFormulaUnitsData(getId(), getTypeCode())
                                              ->getUnitDefinition();
    }
    else
    {
      return NULL;
    }
  }
  else
  {
    UnitDefinition *ud = NULL;
    const char * units = getUnits().c_str();
    if (!strcmp(units, ""))
    {
      ud   = new UnitDefinition(getSBMLNamespaces());
      return ud;
    }
    else
    {
      if (UnitKind_isValidUnitKindString(units, 
                                getLevel(), getVersion()))
      {
        Unit * unit = new Unit(getSBMLNamespaces());
        unit->setKind(UnitKind_forName(units));
        ud   = new UnitDefinition(getSBMLNamespaces());
        
        ud->addUnit(unit);

        delete unit;
      }
      else
      {
        /* must be a unit definition */
        ud = static_cast <Model *> (getAncestorOfType(SBML_MODEL))->getUnitDefinition(units);
      }
      return ud;
      }
    }
  }
  else
  {
    return NULL;
  }
}


/*
  * Constructs and returns a UnitDefinition that expresses the units of this 
  * Compartment.
  */
const UnitDefinition *
Parameter::getDerivedUnitDefinition() const
{
  return const_cast <Parameter *> (this)->getDerivedUnitDefinition();
}


/*
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


/*
 * @return the name of this element ie "parameter".
 */
const string&
Parameter::getElementName () const
{
  static const string name = "parameter";
  return name;
}


bool 
Parameter::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for parameter: id (name in L1)
   * and value (in L1V1 only)
   * and constant (in L3) */

  if (!isSetId())
    allPresent = false;

  if (getLevel() == 1
    && getVersion() == 1
    && !isSetValue())
    allPresent = false;

  if (getLevel() > 2 && !isSetConstant())
    allPresent = false;

  return allPresent;
}


/** @cond doxygen-libsbml-internal */
/*
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
  switch (level)
  {
  case 1:
    readL1Attributes(attributes);
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
  //const unsigned int level   = getLevel  ();
  //const unsigned int version = getVersion();

  //std::vector<std::string> expectedAttributes;
  //expectedAttributes.clear();
  //expectedAttributes.push_back("name");
  //expectedAttributes.push_back("units");
  //expectedAttributes.push_back("value");

  //if (level > 1)
  //{
  //  expectedAttributes.push_back("metaid");
  //  expectedAttributes.push_back("id");
  //  if (this->getTypeCode() == SBML_PARAMETER)
  //  {
  //    expectedAttributes.push_back("constant");
  //  }

  //  if (!(level == 2 && version == 1))
  //  {
  //    expectedAttributes.push_back("sboTerm");
  //  }
  //}

  //// check that all attributes are expected
  //for (int i = 0; i < attributes.getLength(); i++)
  //{
  //  std::vector<std::string>::const_iterator end = expectedAttributes.end();
  //  std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
  //  std::string name = attributes.getName(i);
  //  if (std::find(begin, end, name) == end)
  //  {
  //    if (this->getTypeCode() == SBML_PARAMETER)
  //    {
  //      logUnknownAttribute(name, level, version, "<parameter>");
  //    }
  //    else
  //    {
  //      logUnknownAttribute(name, level, version, "<localParameter>");
  //    }
  //  }
  //}
  //// name: SName   { use="required" }  (L1v1, L1v2)
  ////   id: SId     { use="required" }  (L2v1, L2v2)
  ////
  //const string id = (level == 1) ? "name" : "id";
  //bool assigned = attributes.readInto(id, mId, getErrorLog(), true);
  //if (assigned && mId.size() == 0)
  //{
  //  logEmptyString(id, level, version, "<parameter>");
  //}
  //if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  ////
  //// value: double  { use="required" }  (L1v2)
  //// value: double  { use="optional" }  (L1v2->)
  ////
  //if (level == 1 && version == 1)
  //{
  //  mIsSetValue = attributes.readInto("value", mValue, getErrorLog(), true);
  //}
  //else
  //{
  //  mIsSetValue = attributes.readInto("value", mValue);
  //}

  ////
  //// units: SName  { use="optional" }  (L1v1, L1v2)
  //// units: SId    { use="optional" }  (L2v1, L2v2)
  ////
  //assigned = attributes.readInto("units", mUnits);
  //if (assigned && mUnits.size() == 0)
  //{
  //  logEmptyString("units", level, version, "<parameter>");
  //}
  //if (!SyntaxChecker::isValidUnitSId(mUnits))
  //{
  //  logError(InvalidUnitIdSyntax);
  //}

  //if (level > 1)
  //{
  //  //
  //  // name: string  { use="optional" }  (L2v1 ->)
  //  //
  //  attributes.readInto("name", mName);

  //  //
  //  // constant: boolean  { use="optional" default="true" }  (L2v1->)
  //  // constant: boolean  { use="required" }  (L3v1->)
  //  //
  //  if (level == 2)
  //  {
  //    attributes.readInto("constant", mConstant);
  //  }
  //  else if (this->getTypeCode() == SBML_PARAMETER)
  //  {
  //    mIsSetConstant = attributes.readInto("constant", mConstant,
  //                                         getErrorLog(), true);
  //  }

  //  //
  //  // sboTerm: SBOTerm { use="optional" }  (L2v2->)
  //  //
  //  if (!(level == 2 && version == 1)) 
  //    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
  //}
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 *
 * @param attributes the XMLAttributes object to use
 */
void
Parameter::readL1Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("units");
  expectedAttributes.push_back("value");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      if (this->getTypeCode() == SBML_PARAMETER)
      {
        logUnknownAttribute(name, level, version, "<parameter>");
      }
      else
      {
        logUnknownAttribute(name, level, version, "<localParameter>");
      }
    }
  }
  // name: SName   { use="required" }  (L1v1, L1v2)
  //
  bool assigned = attributes.readInto("name", mId, getErrorLog(), true);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("name", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // value: double  { use="required" }  (L1v2)
  // value: double  { use="optional" }  (L1v2->)
  //
  if (version == 1)
  {
    mIsSetValue = attributes.readInto("value", mValue, getErrorLog(), true);
  }
  else
  {
    mIsSetValue = attributes.readInto("value", mValue);
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  //
  assigned = attributes.readInto("units", mUnits);
  if (assigned && mUnits.size() == 0)
  {
    logEmptyString("units", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidUnitSId(mUnits))
  {
    logError(InvalidUnitIdSyntax);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 *
 * @param attributes the XMLAttributes object to use
 */
void
Parameter::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("units");
  expectedAttributes.push_back("value");
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("id");
  if (this->getTypeCode() == SBML_PARAMETER)
  {
    expectedAttributes.push_back("constant");
  }

  if (version > 1)
  {
    expectedAttributes.push_back("sboTerm");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      if (this->getTypeCode() == SBML_PARAMETER)
      {
        logUnknownAttribute(name, level, version, "<parameter>");
      }
      else
      {
        logUnknownAttribute(name, level, version, "<localParameter>");
      }
    }
  }
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog(), true);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // value: double  { use="optional" }  (L1v2->)
  //
  mIsSetValue = attributes.readInto("value", mValue);

  //
  // units: SId    { use="optional" }  (L2v1, L2v2)
  //
  assigned = attributes.readInto("units", mUnits);
  if (assigned && mUnits.size() == 0)
  {
    logEmptyString("units", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidUnitSId(mUnits))
  {
    logError(InvalidUnitIdSyntax);
  }

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1->)
  //
  attributes.readInto("constant", mConstant);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2->)
  //
  if (version > 1) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 *
 * @param attributes the XMLAttributes object to use
 */
void
Parameter::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("units");
  expectedAttributes.push_back("value");
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("id");
  if (this->getTypeCode() == SBML_PARAMETER)
  {
    expectedAttributes.push_back("constant");
  }
  expectedAttributes.push_back("sboTerm");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      if (this->getTypeCode() == SBML_PARAMETER)
      {
        logUnknownAttribute(name, level, version, "<parameter>");
      }
      else
      {
        logUnknownAttribute(name, level, version, "<localParameter>");
      }
    }
  }
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog());
  if (!assigned)
  {
    if (this->getTypeCode() == SBML_PARAMETER)
    {
      getErrorLog()->logError(AllowedAttributesOnParameter, level, version);
    }
    else
    {
      getErrorLog()->logError(AllowedAttributesOnLocalParameter, 
                              level, version);
    }
  }
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // value: double  { use="optional" }  (L1v2->)
  //
  mIsSetValue = attributes.readInto("value", mValue);

  //
  // units: SId    { use="optional" }  (L2v1, L2v2)
  //
  assigned = attributes.readInto("units", mUnits);
  if (assigned && mUnits.size() == 0)
  {
    logEmptyString("units", level, version, "<parameter>");
  }
  if (!SyntaxChecker::isValidUnitSId(mUnits))
  {
    logError(InvalidUnitIdSyntax);
  }

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  if (this->getTypeCode() == SBML_PARAMETER)
  {
    mIsSetConstant = attributes.readInto("constant", mConstant,
                                          getErrorLog());
    if (!mIsSetConstant)
    {
      getErrorLog()->logError(AllowedAttributesOnParameter, level, version);
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2->)
  //
  mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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
  //   id: SId     { use="required" }  (L2v1->)
  //
  const string id = (level == 1) ? "name" : "id";
  stream.writeAttribute(id, mId);

  if (level > 1)
  {
    //
    // name: string  { use="optional" }  (L2v1->)
    //
    stream.writeAttribute("name", mName);
  }

  //
  // value: double  { use="required" }  (L1v1)
  // value: double  { use="optional" }  (L1v2->)
  //
  if (mIsSetValue || (level == 1 && version == 1))
  {
    stream.writeAttribute("value", mValue);
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1-> )
  //
  stream.writeAttribute("units", mUnits);

  if (level > 1)
  {
    //
    // constant: boolean  { use="optional" default="true" }  (L2v1->)
    // constant: boolean  { use="required" }  (L3v1->)
    //
    if (level == 2)
    {
      if (mConstant != true)
      {
        stream.writeAttribute("constant", mConstant);
      }
    }
    else if (this->getTypeCode() == SBML_PARAMETER)
    {
      stream.writeAttribute("constant", mConstant);
    }

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2->)
    //
    if (!(level == 2 && version == 1)) 
      SBO::writeTerm(stream, mSBOTerm);
  }
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfParameters.
 */
ListOfParameters*
ListOfParameters::clone () const
{
  return new ListOfParameters(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfParameters::getItemTypeCode () const
{
  return SBML_PARAMETER;
}


/*
 * @return the name of this element ie "listOfParameters".
 */
const string&
ListOfParameters::getElementName () const
{
  static const string name = "listOfParameters";
  return name;
}


/* return nth item in list */
Parameter *
ListOfParameters::get(unsigned int n)
{
  return static_cast<Parameter*>(ListOf::get(n));
}


/* return nth item in list */
const Parameter *
ListOfParameters::get(unsigned int n) const
{
  return static_cast<const Parameter*>(ListOf::get(n));
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqP : public unary_function<SBase*, bool>
{
  const string& id;

  IdEqP (const string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Parameter *> (sb)->getId() == id; }
};


/* return item by id */
Parameter*
ListOfParameters::get (const std::string& sid)
{
  return const_cast<Parameter*>( 
    static_cast<const ListOfParameters&>(*this).get(sid) );
}


/* return item by id */
const Parameter*
ListOfParameters::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqP(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Parameter*> (*result);
}


/* Removes the nth item from this list */
Parameter*
ListOfParameters::remove (unsigned int n)
{
   return static_cast<Parameter*>(ListOf::remove(n));
}


/* Removes item in this list by id */
Parameter*
ListOfParameters::remove (const std::string& sid)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqP(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Parameter*> (item);
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfParameters::getElementPosition () const
{
  return 7;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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
    try
    {
      object = new Parameter(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new Parameter(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new Parameter(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new Parameter_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Parameter
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Parameter
 *
 * @return a pointer to the newly created Parameter_t structure.
 *
 * @note Once a Parameter has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Parameter.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (unsigned int level, unsigned int version)
{
  try
  {
    Parameter* obj = new Parameter(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Parameter_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Parameter
 *
 * @return a pointer to the newly created Parameter_t structure.
 *
 * @note Once a Parameter has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Parameter.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Parameter* obj = new Parameter(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
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
 * Returns a list of XMLNamespaces_t associated with this Parameter_t
 * structure.
 *
 * @param p the Parameter_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Parameter_getNamespaces(Parameter_t *p)
{
  return p->getNamespaces();
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
 *
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
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "value" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 *
 * @note In SBML Level 1 Version 1, a Parameter value is required and
 * therefore <em>should always be set</em>.  In Level 1 Version 2 and
 * later, the value is optional, and as such, may or may not be set.
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
 * Predicate returning @c true or @c false depending on whether the given
 * Parameter_t structure's constant attribute have been set.
 *
 * @param p the Parameter_t structure to query
 * 
 * @return @c non-zero (true) if the "constant" attribute of the given
 * Parameter_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetConstant (const Parameter_t *p)
{
  return static_cast<int>( p->isSetConstant() );
}


/**
 * Assigns the identifier of a Parameter_t structure.
 *
 * This makes a copy of the string passed in the param @p sid.
 *
 * @param p the Parameter_t structure to set.
 * @param sid the string to use as the identifier.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "id" attribute.
 */
LIBSBML_EXTERN
int
Parameter_setId (Parameter_t *p, const char *sid)
{
  return (sid == NULL) ? p->setId("") : p->setId(sid);
}


/**
 * Assign the name of a Parameter_t structure.
 *
 * This makes a copy of the string passed in as the argument @p name.
 *
 * @param p the Parameter_t structure to set.
 * @param name the string to use as the name.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "name" attribute.
 */
LIBSBML_EXTERN
int
Parameter_setName (Parameter_t *p, const char *name)
{
  return (name == NULL) ? p->unsetName() : p->setName(name);
}


/**
 * Assign the value of a Parameter_t structure.
 *
 * @param p the Parameter_t structure to set.
 * @param value the @c double value to use.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Parameter_setValue (Parameter_t *p, double value)
{
  return p->setValue(value);
}


/**
 * Assign the units of a Parameter_t structure.
 *
 * This makes a copy of the string passed in as the argument @p units.
 *
 * @param p the Parameter_t structure to set.
 * @param units the string to use as the identifier of the units to assign.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with units set to NULL is equivalent to
 * unsetting the "units" attribute.
 */
LIBSBML_EXTERN
int
Parameter_setUnits (Parameter_t *p, const char *units)
{
  return (units == NULL) ? p->unsetUnits() : p->setUnits(units);
}


/**
 * Assign the "constant" attribute of a Parameter_t structure.
 *
 * @param p the Parameter_t structure to set.
 * @param value the value to assign as the "constant" attribute
 * of the parameter, either zero for false or nonzero for true.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
Parameter_setConstant (Parameter_t *p, int value)
{
  return p->setConstant( static_cast<bool>(value) );
}


/**
 * Unsets the name of this Parameter_t structure.
 * 
 * @param p the Parameter_t structure whose name is to be unset.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Parameter_unsetName (Parameter_t *p)
{
  return p->unsetName();
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
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Parameter_unsetValue (Parameter_t *p)
{
  return p->unsetValue();
}


/**
 * Unsets the units of this Parameter_t structure.
 * 
 * @param p the Parameter_t structure whose units are to be unset.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Parameter_unsetUnits (Parameter_t *p)
{
  return p->unsetUnits();
}


/**
 * Constructs and returns a UnitDefinition_t structure that expresses 
 * the units of this Parameter_t structure.
 *
 * @param p the Parameter_t structure whose units are to be returned.
 *
 * @return a UnitDefinition_t structure that expresses the units 
 * of this Parameter_t strucuture.
 *
 * @note This function returns the units of the Parameter_t expressed 
 * as a UnitDefinition_t. The units may be those explicitly declared. 
 * In the case where no units have been declared, NULL is returned.
 */
LIBSBML_EXTERN
UnitDefinition_t * 
Parameter_getDerivedUnitDefinition(Parameter_t *p)
{
  return p->getDerivedUnitDefinition();
}


/**
  * Predicate returning @c true or @c false depending on whether
  * all the required attributes for this Parameter object
  * have been set.
  *
 * @param p the Parameter_t structure to check.
 *
  * @note The required attributes for a Parameter object are:
  * @li id (name in L1)
  * @li constant (in L3 only)
  *
  * @return a true if all the required
  * attributes for this object have been defined, false otherwise.
  */
LIBSBML_EXTERN
int
Parameter_hasRequiredAttributes(Parameter_t *p)
{
  return static_cast<int>(p->hasRequiredAttributes());
}


/**
 * @return item in this ListOfParameter with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
Parameter_t *
ListOfParameters_getById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfParameters *> (lo)->get(sid) : NULL;
}


/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is responsible for
 * deleting it.
 */
LIBSBML_EXTERN
Parameter_t *
ListOfParameters_removeById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfParameters *> (lo)->remove(sid) : NULL;
}

/** @endcond doxygen-c-only */
LIBSBML_CPP_NAMESPACE_END
