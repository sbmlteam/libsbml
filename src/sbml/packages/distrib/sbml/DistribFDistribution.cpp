/**
 * @file DistribFDistribution.cpp
 * @brief Implementation of the DistribFDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribFDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribFDistribution using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribFDistribution::DistribFDistribution(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mNumerator (NULL)
  , mDenominator (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribFDistribution using the given DistribPkgNamespaces
 * object.
 */
DistribFDistribution::DistribFDistribution(DistribPkgNamespaces *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mNumerator (NULL)
  , mDenominator (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribFDistribution.
 */
DistribFDistribution::DistribFDistribution(const DistribFDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mNumerator ( NULL )
  , mDenominator ( NULL )
{
  if (orig.mNumerator != NULL)
  {
    mNumerator = orig.mNumerator->clone();
  }

  if (orig.mDenominator != NULL)
  {
    mDenominator = orig.mDenominator->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribFDistribution.
 */
DistribFDistribution&
DistribFDistribution::operator=(const DistribFDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mNumerator;
    if (rhs.mNumerator != NULL)
    {
      mNumerator = rhs.mNumerator->clone();
    }
    else
    {
      mNumerator = NULL;
    }

    delete mDenominator;
    if (rhs.mDenominator != NULL)
    {
      mDenominator = rhs.mDenominator->clone();
    }
    else
    {
      mDenominator = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribFDistribution object.
 */
DistribFDistribution*
DistribFDistribution::clone() const
{
  return new DistribFDistribution(*this);
}


/*
 * Destructor for DistribFDistribution.
 */
DistribFDistribution::~DistribFDistribution()
{
  delete mNumerator;
  mNumerator = NULL;
  delete mDenominator;
  mDenominator = NULL;
}


/*
 * Returns the value of the "numerator" element of this DistribFDistribution.
 */
const DistribUncertValue*
DistribFDistribution::getNumerator() const
{
  return mNumerator;
}


/*
 * Returns the value of the "numerator" element of this DistribFDistribution.
 */
DistribUncertValue*
DistribFDistribution::getNumerator()
{
  return mNumerator;
}


/*
 * Returns the value of the "denominator" element of this DistribFDistribution.
 */
const DistribUncertValue*
DistribFDistribution::getDenominator() const
{
  return mDenominator;
}


/*
 * Returns the value of the "denominator" element of this DistribFDistribution.
 */
DistribUncertValue*
DistribFDistribution::getDenominator()
{
  return mDenominator;
}


/*
 * Predicate returning @c true if this DistribFDistribution's "numerator"
 * element is set.
 */
bool
DistribFDistribution::isSetNumerator() const
{
  return (mNumerator != NULL);
}


/*
 * Predicate returning @c true if this DistribFDistribution's "denominator"
 * element is set.
 */
bool
DistribFDistribution::isSetDenominator() const
{
  return (mDenominator != NULL);
}


/*
 * Sets the value of the "numerator" element of this DistribFDistribution.
 */
int
DistribFDistribution::setNumerator(const DistribUncertValue* numerator)
{
  if (numerator == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (numerator->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != numerator->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != numerator->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != numerator->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mNumerator;
    mNumerator = (numerator != NULL) ?
      static_cast<DistribUncertValue*>(numerator->clone()) : NULL;
    if (mNumerator != NULL) mNumerator->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "denominator" element of this DistribFDistribution.
 */
int
DistribFDistribution::setDenominator(const DistribUncertValue* denominator)
{
  if (denominator == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (denominator->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != denominator->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != denominator->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != denominator->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDenominator;
    mDenominator = (denominator != NULL) ?
      static_cast<DistribUncertValue*>(denominator->clone()) : NULL;
    if (mDenominator != NULL) mDenominator->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribFDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribFDistribution::createNumerator()
{
  if (mNumerator != NULL)
  {
    delete mNumerator;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mNumerator = new DistribUncertValue(distribns);

  mNumerator->setElementName("numerator");

  delete distribns;

  connectToChild();

  return mNumerator;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribFDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribFDistribution::createDenominator()
{
  if (mDenominator != NULL)
  {
    delete mDenominator;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDenominator = new DistribUncertValue(distribns);

  mDenominator->setElementName("denominator");

  delete distribns;

  connectToChild();

  return mDenominator;
}


/*
 * Unsets the value of the "numerator" element of this DistribFDistribution.
 */
int
DistribFDistribution::unsetNumerator()
{
  delete mNumerator;
  mNumerator = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "denominator" element of this DistribFDistribution.
 */
int
DistribFDistribution::unsetDenominator()
{
  delete mDenominator;
  mDenominator = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribFDistribution object.
 */
const std::string&
DistribFDistribution::getElementName() const
{
  static const string name = "fDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribFDistribution object.
 */
int
DistribFDistribution::getTypeCode() const
{
  return SBML_DISTRIB_FDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribFDistribution object have been set.
 */
bool
DistribFDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribFDistribution object have been set.
 */
bool
DistribFDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetNumerator() == false)
  {
    allPresent = false;
  }

  if (isSetDenominator() == false)
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
DistribFDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetNumerator() == true)
  {
    mNumerator->write(stream);
  }

  if (isSetDenominator() == true)
  {
    mDenominator->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribFDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mNumerator != NULL)
  {
    mNumerator->accept(v);
  }

  if (mDenominator != NULL)
  {
    mDenominator->accept(v);
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
DistribFDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mNumerator != NULL)
  {
    mNumerator->setSBMLDocument(d);
  }

  if (mDenominator != NULL)
  {
    mDenominator->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribFDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mNumerator != NULL)
  {
    mNumerator->connectToParent(this);
  }

  if (mDenominator != NULL)
  {
    mDenominator->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribFDistribution::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetNumerator())
  {
    mNumerator->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetDenominator())
  {
    mDenominator->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribFDistribution::updateSBMLNamespace(const std::string& package,
                                          unsigned int level,
                                          unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mNumerator != NULL)
  {
    mNumerator->updateSBMLNamespace(package, level, version);
  }

  if (mDenominator != NULL)
  {
    mDenominator->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFDistribution.
 */
int
DistribFDistribution::getAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::getAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::getAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::getAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::getAttribute(const std::string& attributeName,
                                   std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribFDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribFDistribution::isSetAttribute(const std::string& attributeName) const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFDistribution.
 */
int
DistribFDistribution::setAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::setAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::setAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::setAttribute(const std::string& attributeName,
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
 * DistribFDistribution.
 */
int
DistribFDistribution::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribFDistribution.
 */
int
DistribFDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribFDistribution.
 */
SBase*
DistribFDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "numerator")
  {
    return createNumerator();
  }
  else if (elementName == "denominator")
  {
    return createDenominator();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribFDistribution.
 */
int
DistribFDistribution::addChildObject(const std::string& elementName,
                                     const SBase* element)
{
  if (elementName == "numerator" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumerator((const DistribUncertValue*)(element));
  }
  else if (elementName == "denominator" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setDenominator((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribFDistribution.
 */
SBase*
DistribFDistribution::removeChildObject(const std::string& elementName,
                                        const std::string& id)
{
  if (elementName == "numerator")
  {
    DistribUncertValue * obj = getNumerator();
    if (unsetNumerator() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "denominator")
  {
    DistribUncertValue * obj = getDenominator();
    if (unsetDenominator() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribFDistribution.
 */
unsigned int
DistribFDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "numerator")
  {
    if (isSetNumerator())
    {
      return 1;
    }
  }
  else if (elementName == "denominator")
  {
    if (isSetDenominator())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribFDistribution.
 */
SBase*
DistribFDistribution::getObject(const std::string& elementName,
                                unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "numerator")
  {
    return getNumerator();
  }
  else if (elementName == "denominator")
  {
    return getDenominator();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribFDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumerator != NULL)
  {
    if (mNumerator->getId() == id)
    {
      return mNumerator;
    }

    obj = mNumerator->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDenominator != NULL)
  {
    if (mDenominator->getId() == id)
    {
      return mDenominator;
    }

    obj = mDenominator->getElementBySId(id);
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
DistribFDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumerator != NULL)
  {
    if (mNumerator->getMetaId() == metaid)
    {
      return mNumerator;
    }

    obj = mNumerator->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDenominator != NULL)
  {
    if (mDenominator->getMetaId() == metaid)
    {
      return mDenominator;
    }

    obj = mDenominator->getElementByMetaId(metaid);
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
DistribFDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mNumerator, filter);
  ADD_FILTERED_POINTER(ret, sublist, mDenominator, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribFDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "numerator")
  {
    if (isSetNumerator())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribFDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mNumerator;
    mNumerator = new DistribUncertValue(distribns);
    mNumerator->setElementName(name);
    obj = mNumerator;
  }
  else if (name == "denominator")
  {
    if (isSetDenominator())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribFDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDenominator;
    mDenominator = new DistribUncertValue(distribns);
    mDenominator->setElementName(name);
    obj = mDenominator;
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
DistribFDistribution::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
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
DistribFDistribution::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribFDistributionAllowedCoreAttributes, pkgVersion, level,
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
DistribFDistribution::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribFDistribution::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribFDistribution::writeAttributes(XMLOutputStream& stream) const
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
DistribFDistribution::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribFDistribution::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribFDistribution_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribFDistribution_t *
DistribFDistribution_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion)
{
  return new DistribFDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribFDistribution_t object.
 */
LIBSBML_EXTERN
DistribFDistribution_t*
DistribFDistribution_clone(const DistribFDistribution_t* dfd)
{
  if (dfd != NULL)
  {
    return static_cast<DistribFDistribution_t*>(dfd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribFDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribFDistribution_free(DistribFDistribution_t* dfd)
{
  if (dfd != NULL)
  {
    delete dfd;
  }
}


/*
 * Returns the value of the "numerator" element of this DistribFDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribFDistribution_getNumerator(const DistribFDistribution_t * dfd)
{
  if (dfd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dfd->getNumerator());
}


/*
 * Returns the value of the "denominator" element of this
 * DistribFDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribFDistribution_getDenominator(const DistribFDistribution_t * dfd)
{
  if (dfd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dfd->getDenominator());
}


/*
 * Predicate returning @c 1 (true) if this DistribFDistribution_t's "numerator"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribFDistribution_isSetNumerator(const DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? static_cast<int>(dfd->isSetNumerator()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribFDistribution_t's
 * "denominator" element is set.
 */
LIBSBML_EXTERN
int
DistribFDistribution_isSetDenominator(const DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? static_cast<int>(dfd->isSetDenominator()) : 0;
}


/*
 * Sets the value of the "numerator" element of this DistribFDistribution_t.
 */
LIBSBML_EXTERN
int
DistribFDistribution_setNumerator(DistribFDistribution_t * dfd,
                                  const DistribUncertValue_t* numerator)
{
  return (dfd != NULL) ? dfd->setNumerator(numerator) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "denominator" element of this DistribFDistribution_t.
 */
LIBSBML_EXTERN
int
DistribFDistribution_setDenominator(DistribFDistribution_t * dfd,
                                    const DistribUncertValue_t* denominator)
{
  return (dfd != NULL) ? dfd->setDenominator(denominator) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribFDistribution_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribFDistribution_createNumerator(DistribFDistribution_t* dfd)
{
  if (dfd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dfd->createNumerator());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribFDistribution_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribFDistribution_createDenominator(DistribFDistribution_t* dfd)
{
  if (dfd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dfd->createDenominator());
}


/*
 * Unsets the value of the "numerator" element of this DistribFDistribution_t.
 */
LIBSBML_EXTERN
int
DistribFDistribution_unsetNumerator(DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? dfd->unsetNumerator() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "denominator" element of this
 * DistribFDistribution_t.
 */
LIBSBML_EXTERN
int
DistribFDistribution_unsetDenominator(DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? dfd->unsetDenominator() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribFDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribFDistribution_hasRequiredAttributes(const DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? static_cast<int>(dfd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribFDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribFDistribution_hasRequiredElements(const DistribFDistribution_t * dfd)
{
  return (dfd != NULL) ? static_cast<int>(dfd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


