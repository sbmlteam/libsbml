/**
 * @file UncertParameter.cpp
 * @brief Implementation of the UncertParameter class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/distrib/sbml/UncertParameter.h>
#include <sbml/packages/distrib/sbml/ListOfUncertParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/math/MathML.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UncertParameter using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
UncertParameter::UncertParameter(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mValue (util_NaN())
  , mIsSetValue (false)
  , mVar ("")
  , mUnits ("")
  , mType (DISTRIB_UNCERTTYPE_INVALID)
  , mDefinitionURL ("")
  , mUncertParameters (new ListOfUncertParameters (level, version, pkgVersion))
  , mMath (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new UncertParameter using the given DistribPkgNamespaces object.
 */
UncertParameter::UncertParameter(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mValue (util_NaN())
  , mIsSetValue (false)
  , mVar ("")
  , mUnits ("")
  , mType (DISTRIB_UNCERTTYPE_INVALID)
  , mDefinitionURL ("")
  , mUncertParameters (new ListOfUncertParameters (distribns))
  , mMath (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for UncertParameter.
 */
UncertParameter::UncertParameter(const UncertParameter& orig)
  : DistribBase( orig )
  , mValue ( orig.mValue )
  , mIsSetValue ( orig.mIsSetValue )
  , mVar ( orig.mVar )
  , mUnits ( orig.mUnits )
  , mType ( orig.mType )
  , mDefinitionURL ( orig.mDefinitionURL )
  , mUncertParameters ( NULL )
  , mMath ( NULL )
{
  if (orig.mMath != NULL)
  {
    mMath = orig.mMath->deepCopy();
  }

  if (orig.mUncertParameters != NULL)
  {
    mUncertParameters = orig.mUncertParameters->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for UncertParameter.
 */
UncertParameter&
UncertParameter::operator=(const UncertParameter& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mValue = rhs.mValue;
    mIsSetValue = rhs.mIsSetValue;
    mVar = rhs.mVar;
    mUnits = rhs.mUnits;
    mType = rhs.mType;
    mDefinitionURL = rhs.mDefinitionURL;
    delete mUncertParameters;
    if (rhs.mUncertParameters != NULL)
    {
      mUncertParameters = rhs.mUncertParameters->clone();
    }
    else
    {
      mUncertParameters = NULL;
    }

    delete mMath;
    if (rhs.mMath != NULL)
    {
      mMath = rhs.mMath->deepCopy();
    }
    else
    {
      mMath = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this UncertParameter object.
 */
UncertParameter*
UncertParameter::clone() const
{
  return new UncertParameter(*this);
}


/*
 * Destructor for UncertParameter.
 */
UncertParameter::~UncertParameter()
{
  delete mMath;
  mMath = NULL;
  delete mUncertParameters;
  mUncertParameters = NULL;
}


/*
 * Returns the value of the "value" attribute of this UncertParameter.
 */
double
UncertParameter::getValue() const
{
  return mValue;
}


/*
 * Returns the value of the "var" attribute of this UncertParameter.
 */
const std::string&
UncertParameter::getVar() const
{
  return mVar;
}


/*
 * Returns the value of the "units" attribute of this UncertParameter.
 */
const std::string&
UncertParameter::getUnits() const
{
  return mUnits;
}


/*
 * Returns the value of the "type" attribute of this UncertParameter.
 */
UncertType_t
UncertParameter::getType() const
{
  return mType;
}


/*
 * Returns the value of the "type" attribute of this UncertParameter.
 */
std::string
UncertParameter::getTypeAsString() const
{
  return UncertType_toString(mType);
}


/*
 * Returns the value of the "definitionURL" attribute of this UncertParameter.
 */
const std::string&
UncertParameter::getDefinitionURL() const
{
  return mDefinitionURL;
}


/*
 * Predicate returning @c true if this UncertParameter's "value" attribute is
 * set.
 */
bool
UncertParameter::isSetValue() const
{
  return mIsSetValue;
}


/*
 * Predicate returning @c true if this UncertParameter's "var" attribute is
 * set.
 */
bool
UncertParameter::isSetVar() const
{
  return (mVar.empty() == false);
}


/*
 * Predicate returning @c true if this UncertParameter's "units" attribute is
 * set.
 */
bool
UncertParameter::isSetUnits() const
{
  return (mUnits.empty() == false);
}


/*
 * Predicate returning @c true if this UncertParameter's "type" attribute is
 * set.
 */
bool
UncertParameter::isSetType() const
{
  return (mType != DISTRIB_UNCERTTYPE_INVALID);
}


/*
 * Predicate returning @c true if this UncertParameter's "definitionURL"
 * attribute is set.
 */
bool
UncertParameter::isSetDefinitionURL() const
{
  return (mDefinitionURL.empty() == false);
}


/*
 * Sets the value of the "value" attribute of this UncertParameter.
 */
int
UncertParameter::setValue(double value)
{
  mValue = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "var" attribute of this UncertParameter.
 */
int
UncertParameter::setVar(const std::string& var)
{
  if (!(SyntaxChecker::isValidInternalSId(var)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVar = var;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "units" attribute of this UncertParameter.
 */
int
UncertParameter::setUnits(const std::string& units)
{
  if (!(SyntaxChecker::isValidInternalUnitSId(units)))
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
 * Sets the value of the "type" attribute of this UncertParameter.
 */
int
UncertParameter::setType(const UncertType_t type)
{
  if (UncertType_isValid(type) == 0)
  {
    mType = DISTRIB_UNCERTTYPE_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mType = type;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "type" attribute of this UncertParameter.
 */
int
UncertParameter::setType(const std::string& type)
{
  mType = UncertType_fromString(type.c_str());

  if (mType == DISTRIB_UNCERTTYPE_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "definitionURL" attribute of this UncertParameter.
 */
int
UncertParameter::setDefinitionURL(const std::string& definitionURL)
{
  mDefinitionURL = definitionURL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "value" attribute of this UncertParameter.
 */
int
UncertParameter::unsetValue()
{
  mValue = util_NaN();
  mIsSetValue = false;

  if (isSetValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "var" attribute of this UncertParameter.
 */
int
UncertParameter::unsetVar()
{
  mVar.erase();

  if (mVar.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "units" attribute of this UncertParameter.
 */
int
UncertParameter::unsetUnits()
{
  mUnits.erase();

  if (mUnits.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "type" attribute of this UncertParameter.
 */
int
UncertParameter::unsetType()
{
  mType = DISTRIB_UNCERTTYPE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "definitionURL" attribute of this UncertParameter.
 */
int
UncertParameter::unsetDefinitionURL()
{
  mDefinitionURL.erase();

  if (mDefinitionURL.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "math" element of this UncertParameter.
 */
const ASTNode*
UncertParameter::getMath() const
{
  return mMath;
}


/*
 * Returns the value of the "math" element of this UncertParameter.
 */
ASTNode*
UncertParameter::getMath()
{
  return mMath;
}


/*
 * Predicate returning @c true if this UncertParameter's "math" element is set.
 */
bool
UncertParameter::isSetMath() const
{
  return (mMath != NULL);
}


/*
 * Sets the value of the "math" element of this UncertParameter.
 */
int
UncertParameter::setMath(const ASTNode* math)
{
  if (mMath == math)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != NULL) ? math->deepCopy() : NULL;
    if (mMath != NULL)
    {
      mMath->setParentSBMLObject(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "math" element of this UncertParameter.
 */
int
UncertParameter::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfUncertParameters * from this UncertParameter.
 */
const ListOfUncertParameters *
UncertParameter::getListOfUncertParameters() const
{
  return mUncertParameters;
}


/*
 * Returns the ListOfUncertParameters * from this UncertParameter.
 */
ListOfUncertParameters *
UncertParameter::getListOfUncertParameters()
{
  return mUncertParameters;
}


/*
 * Get an UncertParameter from the UncertParameter.
 */
UncertParameter*
UncertParameter::getUncertParameter(unsigned int n)
{
  return mUncertParameters->get(n);
}


/*
 * Get an UncertParameter from the UncertParameter.
 */
const UncertParameter*
UncertParameter::getUncertParameter(unsigned int n) const
{
  return mUncertParameters->get(n);
}


/*
 * Get an UncertParameter from the UncertParameter based on the element to
 * which it refers.
 */
const UncertParameter*
UncertParameter::getUncertParameterByVar(const std::string& sid) const
{
  return mUncertParameters->getByVar(sid);
}


/*
 * Get an UncertParameter from the UncertParameter based on the element to
 * which it refers.
 */
UncertParameter*
UncertParameter::getUncertParameterByVar(const std::string& sid)
{
  return mUncertParameters->getByVar(sid);
}


/*
 * Adds a copy of the given UncertParameter to this UncertParameter.
 */
int
UncertParameter::addUncertParameter(const UncertParameter* up1)
{
  if (up1 == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up1->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up1->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up1->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up1)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mUncertParameters->append(up1);
  }
}


/*
* Adds a copy of the given UncertParameter to this UncertParameter.
*/
int
UncertParameter::addUncertSpan(const UncertSpan* up1)
{
  if (up1 == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up1->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up1->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up1->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up1)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mUncertParameters->append(up1);
  }
}


/*
 * Get the number of UncertParameter objects in this UncertParameter.
 */
unsigned int
UncertParameter::getNumUncertParameters() const
{
  return mUncertParameters->size();
}


/*
 * Creates a new UncertParameter object, adds it to this UncertParameter object
 * and returns the UncertParameter object created.
 */
UncertParameter*
UncertParameter::createUncertParameter()
{
  UncertParameter* up1 = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up1 = new UncertParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up1 != NULL)
  {
    mUncertParameters->appendAndOwn(up1);
  }

  return up1;
}


/*
* Creates a new UncertParameter object, adds it to this UncertParameter object
* and returns the UncertParameter object created.
*/
UncertSpan*
UncertParameter::createUncertSpan()
{
  UncertSpan* up1 = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up1 = new UncertSpan(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up1 != NULL)
  {
    mUncertParameters->appendAndOwn(up1);
  }

  return up1;
}


/*
 * Removes the nth UncertParameter from this UncertParameter and returns a
 * pointer to it.
 */
UncertParameter*
UncertParameter::removeUncertParameter(unsigned int n)
{
  return mUncertParameters->remove(n);
}


void
UncertParameter::renameSIdRefs(const std::string& oldid,
                               const std::string& newid)
{
  DistribBase::renameSIdRefs(oldid, newid);
  if (isSetVar() && mVar == oldid)
  {
    setVar(newid);
  }

  if (isSetMath()) 
  {
      mMath->renameSIdRefs(oldid, newid);
  }
}

void
UncertParameter::renameUnitSIdRefs(const std::string& oldid, const std::string& newid)
{
    DistribBase::renameUnitSIdRefs(oldid, newid);

    if (isSetUnits() && mUnits == oldid)
    {
        setUnits(newid);
    }

    if (isSetMath()) 
    {
        mMath->renameUnitSIdRefs(oldid, newid);
    }
}

/** @cond doxygenLibsbmlInternal */
void
UncertParameter::replaceSIDWithFunction(const std::string& id, const ASTNode* function)
{
    if (isSetMath()) {
        if (mMath->getType() == AST_NAME && mMath->getName() == id) {
            delete mMath;
            mMath = function->deepCopy();
        }
        else {
            mMath->replaceIDWithFunction(id, function);
        }
    }
}
/** @endcond */



/*
 * Returns the XML element name of this UncertParameter object.
 */
const std::string&
UncertParameter::getElementName() const
{
  static const string name = "uncertParameter";
  return name;
}


/*
 * Returns the libSBML type code for this UncertParameter object.
 */
int
UncertParameter::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTPARAMETER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UncertParameter object have been set.
 */
bool
UncertParameter::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  if (isSetType() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * UncertParameter object have been set.
 */
bool
UncertParameter::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UncertParameter::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  if (isSetMath() == true)
  {
    writeMathML(getMath(), stream, getSBMLNamespaces());
  }

  if (getNumUncertParameters() > 0)
  {
    mUncertParameters->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
UncertParameter::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mUncertParameters->accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UncertParameter::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);

  mUncertParameters->setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
UncertParameter::connectToChild()
{
  DistribBase::connectToChild();

  mUncertParameters->connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UncertParameter::enablePackageInternal(const std::string& pkgURI,
                                       const std::string& pkgPrefix,
                                       bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mUncertParameters->enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
UncertParameter::updateSBMLNamespace(const std::string& package,
                                     unsigned int level,
                                     unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);

  mUncertParameters->updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "value")
  {
    value = getValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::getAttribute(const std::string& attributeName,
                              std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "var")
  {
    value = getVar();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "units")
  {
    value = getUnits();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "type")
  {
    value = getTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "definitionURL")
  {
    value = getDefinitionURL();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this UncertParameter's attribute
 * "attributeName" is set.
 */
bool
UncertParameter::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  if (attributeName == "value")
  {
    value = isSetValue();
  }
  else if (attributeName == "var")
  {
    value = isSetVar();
  }
  else if (attributeName == "units")
  {
    value = isSetUnits();
  }
  else if (attributeName == "type")
  {
    value = isSetType();
  }
  else if (attributeName == "definitionURL")
  {
    value = isSetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::setAttribute(const std::string& attributeName, double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "value")
  {
    return_value = setValue(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::setAttribute(const std::string& attributeName,
                              const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "var")
  {
    return_value = setVar(value);
  }
  else if (attributeName == "units")
  {
    return_value = setUnits(value);
  }
  else if (attributeName == "type")
  {
    return_value = setType(value);
  }
  else if (attributeName == "definitionURL")
  {
    return_value = setDefinitionURL(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this UncertParameter.
 */
int
UncertParameter::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  if (attributeName == "value")
  {
    value = unsetValue();
  }
  else if (attributeName == "var")
  {
    value = unsetVar();
  }
  else if (attributeName == "units")
  {
    value = unsetUnits();
  }
  else if (attributeName == "type")
  {
    value = unsetType();
  }
  else if (attributeName == "definitionURL")
  {
    value = unsetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this UncertParameter.
 */
SBase*
UncertParameter::createChildObject(const std::string& elementName)
{
  DistribBase* obj = NULL;

  if (elementName == "uncertParameter")
  {
    return createUncertParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this UncertParameter.
 */
int
UncertParameter::addChildObject(const std::string& elementName,
                                const SBase* element)
{
  if (elementName == "uncertParameter" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTPARAMETER)
  {
    return addUncertParameter((const UncertParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * UncertParameter.
 */
SBase*
UncertParameter::removeChildObject(const std::string& elementName,
                                   const std::string& id)
{
  if (elementName == "uncertParameter")
  {
    for (unsigned int i = 0; i < getNumUncertParameters(); i++)
    {
      if (getUncertParameter(i)->getId() == id)
      {
        return removeUncertParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this UncertParameter.
 */
unsigned int
UncertParameter::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "uncertParameter")
  {
    return getNumUncertParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this UncertParameter.
 */
SBase*
UncertParameter::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "uncertParameter")
  {
    return getUncertParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
UncertParameter::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mUncertParameters->getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
UncertParameter::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertParameters->getMetaId() == metaid)
  {
    return mUncertParameters;
  }

  obj = mUncertParameters->getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
UncertParameter::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mUncertParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
UncertParameter::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfUncertParameters")
  {
    if (mUncertParameters->size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = mUncertParameters;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
UncertParameter::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);

  attributes.add("value");

  attributes.add("var");

  attributes.add("units");

  attributes.add("type");

  attributes.add("definitionURL");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UncertParameter::readAttributes(const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfUncertParameters*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribUncertParameterAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribUncertParameterLOUncertParametersAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  DistribBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribUncertParameterAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribUncertParameterAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // value double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValue = attributes.readInto("value", mValue);

  if ( mIsSetValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'value' from the "
        "<UncertParameter> element must be an integer.";
      log->logPackageError("distrib", DistribUncertParameterValueMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // var SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("var", mVar);

  if (assigned == true)
  {
    if (mVar.empty() == true)
    {
      logEmptyString(mVar, level, version, "<UncertParameter>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVar) == false)
    {
      std::string msg = "The var attribute on the <" + getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVar + "', which does not conform to the syntax.";
      log->logPackageError("distrib", DistribUncertParameterVarMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // units UnitSIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("units", mUnits);

  if (assigned == true)
  {
    if (mUnits.empty() == true)
    {
      logEmptyString(mUnits, level, version, "<UncertParameter>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUnits) == false)
    {
      std::string msg = "The units attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mUnits + "', which does not conform to the syntax.";
      log->logPackageError("distrib", DistribUncertParameterUnitsMustBeUnitSId,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // type enum (use = "required" )
  // 

  std::string type;
  assigned = attributes.readInto("type", type);

  if (assigned == true)
  {
    if (type.empty() == true)
    {
      logEmptyString(type, level, version, "<UncertParameter>");
    }
    else
    {
      mType = UncertType_fromString(type.c_str());

      if (UncertType_isValid(mType) == 0)
      {
        std::string msg = "The type on the <UncertParameter> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + type + "', which is not a valid option.";

        log->logPackageError("distrib",
          DistribUncertParameterTypeMustBeUncertTypeEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Distrib attribute 'type' is missing.";
    log->logPackageError("distrib", DistribUncertParameterAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // definitionURL string (use = "optional" )
  // 

  assigned = attributes.readInto("definitionURL", mDefinitionURL);

  if (assigned == true)
  {
    if (mDefinitionURL.empty() == true)
    {
      logEmptyString(mDefinitionURL, level, version, "<UncertParameter>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads other XML such as math/notes etc.
 */
bool
UncertParameter::readOtherXML(XMLInputStream& stream)
{
  bool read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);
    if (stream.getSBMLNamespaces() == NULL)
    {
      SBMLNamespaces sbmlns(getLevel(), getVersion());
      stream.setSBMLNamespaces(&sbmlns);
    }

    delete mMath;
    mMath = readMathML(stream, prefix);
    read = true;
  }

  if (DistribBase::readOtherXML(stream))
  {
    read = true;
  }

  return read;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UncertParameter::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

  if (isSetValue() == true)
  {
    stream.writeAttribute("value", getPrefix(), mValue);
  }

  if (isSetVar() == true)
  {
    stream.writeAttribute("var", getPrefix(), mVar);
  }

  if (isSetUnits() == true)
  {
    stream.writeAttribute("units", getPrefix(), mUnits);
  }

  if (isSetType() == true)
  {
    stream.writeAttribute("type", getPrefix(), UncertType_toString(mType));
  }

  if (isSetDefinitionURL() == true)
  {
    stream.writeAttribute("definitionURL", getPrefix(), mDefinitionURL);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new UncertParameter_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
UncertParameter_t *
UncertParameter_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new UncertParameter(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UncertParameter_t object.
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_clone(const UncertParameter_t* up)
{
  if (up != NULL)
  {
    return static_cast<UncertParameter_t*>(up->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UncertParameter_t object.
 */
LIBSBML_EXTERN
void
UncertParameter_free(UncertParameter_t* up)
{
  if (up != NULL)
  {
    delete up;
  }
}


/*
 * Returns the value of the "value" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
double
UncertParameter_getValue(const UncertParameter_t * up)
{
  return (up != NULL) ? up->getValue() : util_NaN();
}


/*
 * Returns the value of the "var" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
char *
UncertParameter_getVar(const UncertParameter_t * up)
{
  if (up == NULL)
  {
    return NULL;
  }

  return up->getVar().empty() ? NULL : safe_strdup(up->getVar().c_str());
}


/*
 * Returns the value of the "units" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
char *
UncertParameter_getUnits(const UncertParameter_t * up)
{
  if (up == NULL)
  {
    return NULL;
  }

  return up->getUnits().empty() ? NULL : safe_strdup(up->getUnits().c_str());
}


/*
 * Returns the value of the "type" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
UncertType_t
UncertParameter_getType(const UncertParameter_t * up)
{
  if (up == NULL)
  {
    return DISTRIB_UNCERTTYPE_INVALID;
  }

  return up->getType();
}


/*
 * Returns the value of the "type" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
char *
UncertParameter_getTypeAsString(const UncertParameter_t * up)
{
  return (char*)(UncertType_toString(up->getType()));
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * UncertParameter_t.
 */
LIBSBML_EXTERN
char *
UncertParameter_getDefinitionURL(const UncertParameter_t * up)
{
  if (up == NULL)
  {
    return NULL;
  }

  return up->getDefinitionURL().empty() ? NULL :
    safe_strdup(up->getDefinitionURL().c_str());
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "value"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetValue(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "var" attribute
 * is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetVar(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetVar()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "units"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetUnits(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetUnits()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "type" attribute
 * is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetType(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "definitionURL"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetDefinitionURL(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetDefinitionURL()) : 0;
}


/*
 * Sets the value of the "value" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setValue(UncertParameter_t * up, double value)
{
  return (up != NULL) ? up->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "var" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setVar(UncertParameter_t * up, const char * var)
{
  return (up != NULL) ? up->setVar(var) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "units" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setUnits(UncertParameter_t * up, const char * units)
{
  return (up != NULL) ? up->setUnits(units) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setType(UncertParameter_t * up, UncertType_t type)
{
  return (up != NULL) ? up->setType(type) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setTypeAsString(UncertParameter_t * up, const char * type)
{
  return (up != NULL) ? up->setType(type): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "definitionURL" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setDefinitionURL(UncertParameter_t * up,
                                 const char * definitionURL)
{
  return (up != NULL) ? up->setDefinitionURL(definitionURL) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetValue(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "var" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetVar(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetVar() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "units" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetUnits(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetUnits() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "type" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetType(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "definitionURL" attribute of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetDefinitionURL(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetDefinitionURL() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "math" element of this UncertParameter_t.
 */
LIBSBML_EXTERN
const ASTNode_t*
UncertParameter_getMath(const UncertParameter_t * up)
{
  if (up == NULL)
  {
    return NULL;
  }

  return (ASTNode_t*)(up->getMath());
}


/*
 * Predicate returning @c 1 (true) if this UncertParameter_t's "math" element
 * is set.
 */
LIBSBML_EXTERN
int
UncertParameter_isSetMath(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->isSetMath()) : 0;
}


/*
 * Sets the value of the "math" element of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_setMath(UncertParameter_t * up, const ASTNode_t* math)
{
  return (up != NULL) ? up->setMath(math) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "math" element of this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_unsetMath(UncertParameter_t * up)
{
  return (up != NULL) ? up->unsetMath() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing UncertParameter_t objects from this
 * UncertParameter_t.
 */
LIBSBML_EXTERN
ListOf_t*
UncertParameter_getListOfUncertParameters(UncertParameter_t* up)
{
  return (up != NULL) ? up->getListOfUncertParameters() : NULL;
}


/*
 * Get an UncertParameter_t from the UncertParameter_t.
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_getUncertParameter(UncertParameter_t* up, unsigned int n)
{
  return (up != NULL) ? up->getUncertParameter(n) : NULL;
}


/*
 * Get an UncertParameter_t from the UncertParameter_t based on the element to
 * which it refers.
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_getUncertParameterByVar(UncertParameter_t* up,
                                        const char *sid)
{
  return (up != NULL && sid != NULL) ? up->getUncertParameterByVar(sid) : NULL;
}


/*
 * Adds a copy of the given UncertParameter_t to this UncertParameter_t.
 */
LIBSBML_EXTERN
int
UncertParameter_addUncertParameter(UncertParameter_t* up,
                                   const UncertParameter_t* up1)
{
  return (up != NULL) ? up->addUncertParameter(up1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of UncertParameter_t objects in this UncertParameter_t.
 */
LIBSBML_EXTERN
unsigned int
UncertParameter_getNumUncertParameters(UncertParameter_t* up)
{
  return (up != NULL) ? up->getNumUncertParameters() : SBML_INT_MAX;
}


/*
 * Creates a new UncertParameter_t object, adds it to this UncertParameter_t
 * object and returns the UncertParameter_t object created.
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_createUncertParameter(UncertParameter_t* up)
{
  return (up != NULL) ? up->createUncertParameter() : NULL;
}


/*
 * Removes the nth UncertParameter_t from this UncertParameter_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_removeUncertParameter(UncertParameter_t* up, unsigned int n)
{
  return (up != NULL) ? up->removeUncertParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertParameter_t object have been set.
 */
LIBSBML_EXTERN
int
UncertParameter_hasRequiredAttributes(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * UncertParameter_t object have been set.
 */
LIBSBML_EXTERN
int
UncertParameter_hasRequiredElements(const UncertParameter_t * up)
{
  return (up != NULL) ? static_cast<int>(up->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


