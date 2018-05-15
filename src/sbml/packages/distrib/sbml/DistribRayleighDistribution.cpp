/**
 * @file DistribRayleighDistribution.cpp
 * @brief Implementation of the DistribRayleighDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribRayleighDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribRayleighDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribRayleighDistribution::DistribRayleighDistribution(unsigned int level,
                                                         unsigned int version,
                                                         unsigned int
                                                           pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mScale (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribRayleighDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribRayleighDistribution::DistribRayleighDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mScale (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribRayleighDistribution.
 */
DistribRayleighDistribution::DistribRayleighDistribution(const
  DistribRayleighDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mScale ( NULL )
{
  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribRayleighDistribution.
 */
DistribRayleighDistribution&
DistribRayleighDistribution::operator=(const DistribRayleighDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mScale;
    if (rhs.mScale != NULL)
    {
      mScale = rhs.mScale->clone();
    }
    else
    {
      mScale = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribRayleighDistribution object.
 */
DistribRayleighDistribution*
DistribRayleighDistribution::clone() const
{
  return new DistribRayleighDistribution(*this);
}


/*
 * Destructor for DistribRayleighDistribution.
 */
DistribRayleighDistribution::~DistribRayleighDistribution()
{
  delete mScale;
  mScale = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribRayleighDistribution.
 */
const std::string&
DistribRayleighDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribRayleighDistribution.
 */
const std::string&
DistribRayleighDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribRayleighDistribution's "id"
 * attribute is set.
 */
bool
DistribRayleighDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribRayleighDistribution's "name"
 * attribute is set.
 */
bool
DistribRayleighDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::unsetId()
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
 * Unsets the value of the "name" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::unsetName()
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
 * Returns the value of the "scale" element of this
 * DistribRayleighDistribution.
 */
const DistribUncertValue*
DistribRayleighDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this
 * DistribRayleighDistribution.
 */
DistribUncertValue*
DistribRayleighDistribution::getScale()
{
  return mScale;
}


/*
 * Predicate returning @c true if this DistribRayleighDistribution's "scale"
 * element is set.
 */
bool
DistribRayleighDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Sets the value of the "scale" element of this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setScale(const DistribUncertValue* scale)
{
  if (scale == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (scale->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != scale->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != scale->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != scale->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mScale;
    mScale = (scale != NULL) ? static_cast<DistribUncertValue*>(scale->clone())
      : NULL;
    if (mScale != NULL) mScale->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribRayleighDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribRayleighDistribution::createScale()
{
  if (mScale != NULL)
  {
    delete mScale;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mScale = new DistribUncertValue(distribns);

  mScale->setElementName("scale");

  delete distribns;

  connectToChild();

  return mScale;
}


/*
 * Unsets the value of the "scale" element of this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribRayleighDistribution object.
 */
const std::string&
DistribRayleighDistribution::getElementName() const
{
  static const string name = "rayleighDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribRayleighDistribution object.
 */
int
DistribRayleighDistribution::getTypeCode() const
{
  return SBML_DISTRIB_RAYLEIGHDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribRayleighDistribution object have been set.
 */
bool
DistribRayleighDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribRayleighDistribution object have been set.
 */
bool
DistribRayleighDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetScale() == false)
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
DistribRayleighDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetScale() == true)
  {
    mScale->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribRayleighDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mScale != NULL)
  {
    mScale->accept(v);
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
DistribRayleighDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mScale != NULL)
  {
    mScale->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribRayleighDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mScale != NULL)
  {
    mScale->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribRayleighDistribution::enablePackageInternal(const std::string& pkgURI,
                                                   const std::string&
                                                     pkgPrefix,
                                                   bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetScale())
  {
    mScale->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribRayleighDistribution::updateSBMLNamespace(const std::string& package,
                                                 unsigned int level,
                                                 unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mScale != NULL)
  {
    mScale->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::getAttribute(const std::string& attributeName,
                                          bool& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::getAttribute(const std::string& attributeName,
                                          int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::getAttribute(const std::string& attributeName,
                                          double& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::getAttribute(const std::string& attributeName,
                                          unsigned int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::getAttribute(const std::string& attributeName,
                                          std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this DistribRayleighDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribRayleighDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setAttribute(const std::string& attributeName,
                                          bool value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setAttribute(const std::string& attributeName,
                                          int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setAttribute(const std::string& attributeName,
                                          double value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setAttribute(const std::string& attributeName,
                                          unsigned int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::setAttribute(const std::string& attributeName,
                                          const std::string& value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

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
 * Unsets the value of the "attributeName" attribute of this
 * DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribRayleighDistribution.
 */
SBase*
DistribRayleighDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "scale")
  {
    return createScale();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribRayleighDistribution.
 */
int
DistribRayleighDistribution::addChildObject(const std::string& elementName,
                                            const SBase* element)
{
  if (elementName == "scale" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setScale((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribRayleighDistribution.
 */
SBase*
DistribRayleighDistribution::removeChildObject(const std::string& elementName,
                                               const std::string& id)
{
  if (elementName == "scale")
  {
    DistribUncertValue * obj = getScale();
    if (unsetScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribRayleighDistribution.
 */
unsigned int
DistribRayleighDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "scale")
  {
    if (isSetScale())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribRayleighDistribution.
 */
SBase*
DistribRayleighDistribution::getObject(const std::string& elementName,
                                       unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "scale")
  {
    return getScale();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribRayleighDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mScale != NULL)
  {
    if (mScale->getId() == id)
    {
      return mScale;
    }

    obj = mScale->getElementBySId(id);
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
DistribRayleighDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mScale != NULL)
  {
    if (mScale->getMetaId() == metaid)
    {
      return mScale;
    }

    obj = mScale->getElementByMetaId(metaid);
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
DistribRayleighDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribRayleighDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "scale")
  {
    if (isSetScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribRayleighDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mScale;
    mScale = new DistribUncertValue(distribns);
    mScale->setElementName(name);
    obj = mScale;
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
DistribRayleighDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribRayleighDistribution::readAttributes(const XMLAttributes& attributes,
                                            const ExpectedAttributes&
                                              expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribContinuousUnivariateDistribution::readAttributes(attributes,
    expectedAttributes);

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
          DistribDistribRayleighDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribRayleighDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
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
DistribRayleighDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribRayleighDistribution>");
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
      logEmptyString(mName, level, version, "<DistribRayleighDistribution>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribRayleighDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribRayleighDistribution>");
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
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribRayleighDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

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
DistribRayleighDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribRayleighDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribRayleighDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t *
DistribRayleighDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
{
  return new DistribRayleighDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribRayleighDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t*
DistribRayleighDistribution_clone(const DistribRayleighDistribution_t* drd)
{
  if (drd != NULL)
  {
    return static_cast<DistribRayleighDistribution_t*>(drd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribRayleighDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribRayleighDistribution_free(DistribRayleighDistribution_t* drd)
{
  if (drd != NULL)
  {
    delete drd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribRayleighDistribution_getId(const DistribRayleighDistribution_t * drd)
{
  if (drd == NULL)
  {
    return NULL;
  }

  return drd->getId().empty() ? NULL : safe_strdup(drd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribRayleighDistribution_getName(const DistribRayleighDistribution_t * drd)
{
  if (drd == NULL)
  {
    return NULL;
  }

  return drd->getName().empty() ? NULL : safe_strdup(drd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribRayleighDistribution_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_isSetId(const DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? static_cast<int>(drd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribRayleighDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_isSetName(const DistribRayleighDistribution_t *
  drd)
{
  return (drd != NULL) ? static_cast<int>(drd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_setId(DistribRayleighDistribution_t * drd,
                                  const char * id)
{
  return (drd != NULL) ? drd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_setName(DistribRayleighDistribution_t * drd,
                                    const char * name)
{
  return (drd != NULL) ? drd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_unsetId(DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? drd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_unsetName(DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? drd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "scale" element of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribRayleighDistribution_getScale(const DistribRayleighDistribution_t * drd)
{
  if (drd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(drd->getScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribRayleighDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_isSetScale(const DistribRayleighDistribution_t *
  drd)
{
  return (drd != NULL) ? static_cast<int>(drd->isSetScale()) : 0;
}


/*
 * Sets the value of the "scale" element of this DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_setScale(DistribRayleighDistribution_t * drd,
                                     const DistribUncertValue_t* scale)
{
  return (drd != NULL) ? drd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribRayleighDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribRayleighDistribution_createScale(DistribRayleighDistribution_t* drd)
{
  if (drd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(drd->createScale());
}


/*
 * Unsets the value of the "scale" element of this
 * DistribRayleighDistribution_t.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_unsetScale(DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? drd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribRayleighDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_hasRequiredAttributes(const
  DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? static_cast<int>(drd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribRayleighDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribRayleighDistribution_hasRequiredElements(const
  DistribRayleighDistribution_t * drd)
{
  return (drd != NULL) ? static_cast<int>(drd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


