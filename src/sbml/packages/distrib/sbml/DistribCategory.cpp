/**
 * @file DistribCategory.cpp
 * @brief Implementation of the DistribCategory class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/DistribCategory.h>
#include <sbml/packages/distrib/sbml/ListOfCategories.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribCategory using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribCategory::DistribCategory(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
  : SBase(level, version)
  , mRank (SBML_INT_MAX)
  , mIsSetRank (false)
  , mProbability (NULL)
  , mValue (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribCategory using the given DistribPkgNamespaces object.
 */
DistribCategory::DistribCategory(DistribPkgNamespaces *distribns)
  : SBase(distribns)
  , mRank (SBML_INT_MAX)
  , mIsSetRank (false)
  , mProbability (NULL)
  , mValue (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribCategory.
 */
DistribCategory::DistribCategory(const DistribCategory& orig)
  : SBase( orig )
  , mRank ( orig.mRank )
  , mIsSetRank ( orig.mIsSetRank )
  , mProbability ( NULL )
  , mValue ( NULL )
{
  if (orig.mProbability != NULL)
  {
    mProbability = orig.mProbability->clone();
  }

  if (orig.mValue != NULL)
  {
    mValue = orig.mValue->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribCategory.
 */
DistribCategory&
DistribCategory::operator=(const DistribCategory& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mRank = rhs.mRank;
    mIsSetRank = rhs.mIsSetRank;
    delete mProbability;
    if (rhs.mProbability != NULL)
    {
      mProbability = rhs.mProbability->clone();
    }
    else
    {
      mProbability = NULL;
    }

    delete mValue;
    if (rhs.mValue != NULL)
    {
      mValue = rhs.mValue->clone();
    }
    else
    {
      mValue = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribCategory object.
 */
DistribCategory*
DistribCategory::clone() const
{
  return new DistribCategory(*this);
}


/*
 * Destructor for DistribCategory.
 */
DistribCategory::~DistribCategory()
{
  delete mProbability;
  mProbability = NULL;
  delete mValue;
  mValue = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribCategory.
 */
const std::string&
DistribCategory::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribCategory.
 */
const std::string&
DistribCategory::getName() const
{
  return mName;
}


/*
 * Returns the value of the "rank" attribute of this DistribCategory.
 */
unsigned int
DistribCategory::getRank() const
{
  return mRank;
}


/*
 * Predicate returning @c true if this DistribCategory's "id" attribute is set.
 */
bool
DistribCategory::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribCategory's "name" attribute is
 * set.
 */
bool
DistribCategory::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this DistribCategory's "rank" attribute is
 * set.
 */
bool
DistribCategory::isSetRank() const
{
  return mIsSetRank;
}


/*
 * Sets the value of the "id" attribute of this DistribCategory.
 */
int
DistribCategory::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribCategory.
 */
int
DistribCategory::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "rank" attribute of this DistribCategory.
 */
int
DistribCategory::setRank(unsigned int rank)
{
  mRank = rank;
  mIsSetRank = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribCategory.
 */
int
DistribCategory::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "name" attribute of this DistribCategory.
 */
int
DistribCategory::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "rank" attribute of this DistribCategory.
 */
int
DistribCategory::unsetRank()
{
  mRank = SBML_INT_MAX;
  mIsSetRank = false;

  if (isSetRank() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "probability" element of this DistribCategory.
 */
const DistribUncertValue*
DistribCategory::getProbability() const
{
  return mProbability;
}


/*
 * Returns the value of the "probability" element of this DistribCategory.
 */
DistribUncertValue*
DistribCategory::getProbability()
{
  return mProbability;
}


/*
 * Returns the value of the "value" element of this DistribCategory.
 */
const DistribUncertValue*
DistribCategory::getValue() const
{
  return mValue;
}


/*
 * Returns the value of the "value" element of this DistribCategory.
 */
DistribUncertValue*
DistribCategory::getValue()
{
  return mValue;
}


/*
 * Predicate returning @c true if this DistribCategory's "probability" element
 * is set.
 */
bool
DistribCategory::isSetProbability() const
{
  return (mProbability != NULL);
}


/*
 * Predicate returning @c true if this DistribCategory's "value" element is
 * set.
 */
bool
DistribCategory::isSetValue() const
{
  return (mValue != NULL);
}


/*
 * Sets the value of the "probability" element of this DistribCategory.
 */
int
DistribCategory::setProbability(const DistribUncertValue* probability)
{
  if (probability == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (probability->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != probability->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != probability->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != probability->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mProbability;
    mProbability = (probability != NULL) ?
      static_cast<DistribUncertValue*>(probability->clone()) : NULL;
    if (mProbability != NULL) mProbability->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "value" element of this DistribCategory.
 */
int
DistribCategory::setValue(const DistribUncertValue* value)
{
  if (value == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (value->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != value->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != value->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != value->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mValue;
    mValue = (value != NULL) ? static_cast<DistribUncertValue*>(value->clone())
      : NULL;
    if (mValue != NULL) mValue->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this DistribCategory
 * object and returns the DistribUncertValue object created.
 */
DistribUncertValue*
DistribCategory::createProbability()
{
  if (mProbability != NULL)
  {
    delete mProbability;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mProbability = new DistribUncertValue(distribns);

  mProbability->setElementName("probability");

  delete distribns;

  connectToChild();

  return mProbability;
}


/*
 * Creates a new DistribUncertValue object, adds it to this DistribCategory
 * object and returns the DistribUncertValue object created.
 */
DistribUncertValue*
DistribCategory::createValue()
{
  if (mValue != NULL)
  {
    delete mValue;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mValue = new DistribUncertValue(distribns);

  mValue->setElementName("value");

  delete distribns;

  connectToChild();

  return mValue;
}


/*
 * Unsets the value of the "probability" element of this DistribCategory.
 */
int
DistribCategory::unsetProbability()
{
  delete mProbability;
  mProbability = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "value" element of this DistribCategory.
 */
int
DistribCategory::unsetValue()
{
  delete mValue;
  mValue = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribCategory object.
 */
const std::string&
DistribCategory::getElementName() const
{
  static const string name = "category";
  return name;
}


/*
 * Returns the libSBML type code for this DistribCategory object.
 */
int
DistribCategory::getTypeCode() const
{
  return SBML_DISTRIB_CATEGORY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribCategory object have been set.
 */
bool
DistribCategory::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribCategory object have been set.
 */
bool
DistribCategory::hasRequiredElements() const
{
  bool allPresent = true;

  if (isSetValue() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribCategory::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (isSetProbability() == true)
  {
    mProbability->write(stream);
  }

  if (isSetValue() == true)
  {
    mValue->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribCategory::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mProbability != NULL)
  {
    mProbability->accept(v);
  }

  if (mValue != NULL)
  {
    mValue->accept(v);
  }

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribCategory::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  if (mProbability != NULL)
  {
    mProbability->setSBMLDocument(d);
  }

  if (mValue != NULL)
  {
    mValue->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribCategory::connectToChild()
{
  SBase::connectToChild();

  if (mProbability != NULL)
  {
    mProbability->connectToParent(this);
  }

  if (mValue != NULL)
  {
    mValue->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribCategory::enablePackageInternal(const std::string& pkgURI,
                                       const std::string& pkgPrefix,
                                       bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetProbability())
  {
    mProbability->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetValue())
  {
    mValue->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribCategory::updateSBMLNamespace(const std::string& package,
                                     unsigned int level,
                                     unsigned int version)
{
  SBase::updateSBMLNamespace(package, level, version);

  if (mProbability != NULL)
  {
    mProbability->updateSBMLNamespace(package, level, version);
  }

  if (mValue != NULL)
  {
    mValue->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "rank")
  {
    value = getRank();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::getAttribute(const std::string& attributeName,
                              std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribCategory's attribute
 * "attributeName" is set.
 */
bool
DistribCategory::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "rank")
  {
    value = isSetRank();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "rank")
  {
    return_value = setRank(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::setAttribute(const std::string& attributeName,
                              const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this DistribCategory.
 */
int
DistribCategory::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "rank")
  {
    value = unsetRank();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this DistribCategory.
 */
SBase*
DistribCategory::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "probability")
  {
    return createProbability();
  }
  else if (elementName == "value")
  {
    return createValue();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribCategory.
 */
int
DistribCategory::addChildObject(const std::string& elementName,
                                const SBase* element)
{
  if (elementName == "probability" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setProbability((const DistribUncertValue*)(element));
  }
  else if (elementName == "value" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setValue((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribCategory.
 */
SBase*
DistribCategory::removeChildObject(const std::string& elementName,
                                   const std::string& id)
{
  if (elementName == "probability")
  {
    DistribUncertValue * obj = getProbability();
    if (unsetProbability() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "value")
  {
    DistribUncertValue * obj = getValue();
    if (unsetValue() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribCategory.
 */
unsigned int
DistribCategory::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "probability")
  {
    if (isSetProbability())
    {
      return 1;
    }
  }
  else if (elementName == "value")
  {
    if (isSetValue())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribCategory.
 */
SBase*
DistribCategory::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "probability")
  {
    return getProbability();
  }
  else if (elementName == "value")
  {
    return getValue();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribCategory::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mProbability != NULL)
  {
    if (mProbability->getId() == id)
    {
      return mProbability;
    }

    obj = mProbability->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mValue != NULL)
  {
    if (mValue->getId() == id)
    {
      return mValue;
    }

    obj = mValue->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
DistribCategory::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mProbability != NULL)
  {
    if (mProbability->getMetaId() == metaid)
    {
      return mProbability;
    }

    obj = mProbability->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mValue != NULL)
  {
    if (mValue->getMetaId() == metaid)
    {
      return mValue;
    }

    obj = mValue->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
DistribCategory::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mProbability, filter);
  ADD_FILTERED_POINTER(ret, sublist, mValue, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribCategory::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "probability")
  {
    if (isSetProbability())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribCategoryAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mProbability;
    mProbability = new DistribUncertValue(distribns);
    mProbability->setElementName(name);
    obj = mProbability;
  }
  else if (name == "value")
  {
    if (isSetValue())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribCategoryAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mValue;
    mValue = new DistribUncertValue(distribns);
    mValue->setElementName(name);
    obj = mValue;
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribCategory::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("rank");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("rank");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribCategory::readAttributes(const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfCategories*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribCategoryAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

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
          DistribDistribCategoryAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribCategoryAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribCategory::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribCategory>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribCategory>");
    }
  }

  // 
  // rank uint (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRank = attributes.readInto("rank", mRank);

  if ( mIsSetRank == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'rank' from the "
        "<DistribCategory> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribCategoryRankMustBeNonNegativeInteger, pkgVersion, level,
          version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribCategory::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribCategory>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;

  // 
  // rank uint (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRank = attributes.readInto("rank", mRank);

  if ( mIsSetRank == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'rank' from the "
        "<DistribCategory> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribCategoryRankMustBeNonNegativeInteger, pkgVersion, level,
          version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCategory::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCategory::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetRank() == true)
  {
    stream.writeAttribute("rank", getPrefix(), mRank);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCategory::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
  if (isSetRank() == true)
  {
    stream.writeAttribute("rank", getPrefix(), mRank);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribCategory_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribCategory_t *
DistribCategory_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new DistribCategory(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribCategory_t object.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategory_clone(const DistribCategory_t* dc)
{
  if (dc != NULL)
  {
    return static_cast<DistribCategory_t*>(dc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribCategory_t object.
 */
LIBSBML_EXTERN
void
DistribCategory_free(DistribCategory_t* dc)
{
  if (dc != NULL)
  {
    delete dc;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
char *
DistribCategory_getId(const DistribCategory_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return dc->getId().empty() ? NULL : safe_strdup(dc->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
char *
DistribCategory_getName(const DistribCategory_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return dc->getName().empty() ? NULL : safe_strdup(dc->getName().c_str());
}


/*
 * Returns the value of the "rank" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
unsigned int
DistribCategory_getRank(const DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->getRank() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this DistribCategory_t's "id" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribCategory_isSetId(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribCategory_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribCategory_isSetName(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribCategory_t's "rank" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribCategory_isSetRank(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetRank()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_setId(DistribCategory_t * dc, const char * id)
{
  return (dc != NULL) ? dc->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_setName(DistribCategory_t * dc, const char * name)
{
  return (dc != NULL) ? dc->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rank" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_setRank(DistribCategory_t * dc, unsigned int rank)
{
  return (dc != NULL) ? dc->setRank(rank) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_unsetId(DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_unsetName(DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rank" attribute of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_unsetRank(DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->unsetRank() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "probability" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCategory_getProbability(const DistribCategory_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dc->getProbability());
}


/*
 * Returns the value of the "value" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCategory_getValue(const DistribCategory_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dc->getValue());
}


/*
 * Predicate returning @c 1 (true) if this DistribCategory_t's "probability"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribCategory_isSetProbability(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetProbability()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribCategory_t's "value" element
 * is set.
 */
LIBSBML_EXTERN
int
DistribCategory_isSetValue(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetValue()) : 0;
}


/*
 * Sets the value of the "probability" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_setProbability(DistribCategory_t * dc,
                               const DistribUncertValue_t* probability)
{
  return (dc != NULL) ? dc->setProbability(probability) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "value" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_setValue(DistribCategory_t * dc,
                         const DistribUncertValue_t* value)
{
  return (dc != NULL) ? dc->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this DistribCategory_t
 * object and returns the DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCategory_createProbability(DistribCategory_t* dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dc->createProbability());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this DistribCategory_t
 * object and returns the DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCategory_createValue(DistribCategory_t* dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dc->createValue());
}


/*
 * Unsets the value of the "probability" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_unsetProbability(DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->unsetProbability() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" element of this DistribCategory_t.
 */
LIBSBML_EXTERN
int
DistribCategory_unsetValue(DistribCategory_t * dc)
{
  return (dc != NULL) ? dc->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCategory_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCategory_hasRequiredAttributes(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCategory_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCategory_hasRequiredElements(const DistribCategory_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


