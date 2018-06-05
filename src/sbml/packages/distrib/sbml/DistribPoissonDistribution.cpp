/**
 * @file DistribPoissonDistribution.cpp
 * @brief Implementation of the DistribPoissonDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribPoissonDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribPoissonDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribPoissonDistribution::DistribPoissonDistribution(unsigned int level,
                                                       unsigned int version,
                                                       unsigned int pkgVersion)
  : DistribDiscreteUnivariateDistribution(level, version, pkgVersion)
  , mRate (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribPoissonDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribPoissonDistribution::DistribPoissonDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDiscreteUnivariateDistribution(distribns)
  , mRate (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribPoissonDistribution.
 */
DistribPoissonDistribution::DistribPoissonDistribution(const
  DistribPoissonDistribution& orig)
  : DistribDiscreteUnivariateDistribution( orig )
  , mRate ( NULL )
{
  if (orig.mRate != NULL)
  {
    mRate = orig.mRate->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribPoissonDistribution.
 */
DistribPoissonDistribution&
DistribPoissonDistribution::operator=(const DistribPoissonDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDiscreteUnivariateDistribution::operator=(rhs);
    delete mRate;
    if (rhs.mRate != NULL)
    {
      mRate = rhs.mRate->clone();
    }
    else
    {
      mRate = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribPoissonDistribution object.
 */
DistribPoissonDistribution*
DistribPoissonDistribution::clone() const
{
  return new DistribPoissonDistribution(*this);
}


/*
 * Destructor for DistribPoissonDistribution.
 */
DistribPoissonDistribution::~DistribPoissonDistribution()
{
  delete mRate;
  mRate = NULL;
}


/*
 * Returns the value of the "rate" element of this DistribPoissonDistribution.
 */
const DistribUncertValue*
DistribPoissonDistribution::getRate() const
{
  return mRate;
}


/*
 * Returns the value of the "rate" element of this DistribPoissonDistribution.
 */
DistribUncertValue*
DistribPoissonDistribution::getRate()
{
  return mRate;
}


/*
 * Predicate returning @c true if this DistribPoissonDistribution's "rate"
 * element is set.
 */
bool
DistribPoissonDistribution::isSetRate() const
{
  return (mRate != NULL);
}


/*
 * Sets the value of the "rate" element of this DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setRate(const DistribUncertValue* rate)
{
  if (rate == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (rate->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != rate->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != rate->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != rate->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mRate;
    mRate = (rate != NULL) ? static_cast<DistribUncertValue*>(rate->clone()) :
      NULL;
    if (mRate != NULL) mRate->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribPoissonDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribPoissonDistribution::createRate()
{
  if (mRate != NULL)
  {
    delete mRate;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mRate = new DistribUncertValue(distribns);

  mRate->setElementName("rate");

  delete distribns;

  connectToChild();

  return mRate;
}


/*
 * Unsets the value of the "rate" element of this DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::unsetRate()
{
  delete mRate;
  mRate = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribPoissonDistribution object.
 */
const std::string&
DistribPoissonDistribution::getElementName() const
{
  static const string name = "poissonDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribPoissonDistribution object.
 */
int
DistribPoissonDistribution::getTypeCode() const
{
  return SBML_DISTRIB_POISSONDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribPoissonDistribution object have been set.
 */
bool
DistribPoissonDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribPoissonDistribution object have been set.
 */
bool
DistribPoissonDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredElements();

  if (isSetRate() == false)
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
DistribPoissonDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribDiscreteUnivariateDistribution::writeElements(stream);

  if (isSetRate() == true)
  {
    mRate->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribPoissonDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mRate != NULL)
  {
    mRate->accept(v);
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
DistribPoissonDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDiscreteUnivariateDistribution::setSBMLDocument(d);

  if (mRate != NULL)
  {
    mRate->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribPoissonDistribution::connectToChild()
{
  DistribDiscreteUnivariateDistribution::connectToChild();

  if (mRate != NULL)
  {
    mRate->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribPoissonDistribution::enablePackageInternal(const std::string& pkgURI,
                                                  const std::string& pkgPrefix,
                                                  bool flag)
{
  DistribDiscreteUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetRate())
  {
    mRate->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribPoissonDistribution::updateSBMLNamespace(const std::string& package,
                                                unsigned int level,
                                                unsigned int version)
{
  DistribDiscreteUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mRate != NULL)
  {
    mRate->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::getAttribute(const std::string& attributeName,
                                         bool& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::getAttribute(const std::string& attributeName,
                                         int& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::getAttribute(const std::string& attributeName,
                                         double& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::getAttribute(const std::string& attributeName,
                                         unsigned int& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::getAttribute(const std::string& attributeName,
                                         std::string& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribPoissonDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribPoissonDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value =
    DistribDiscreteUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setAttribute(const std::string& attributeName,
                                         bool value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setAttribute(const std::string& attributeName,
                                         int value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setAttribute(const std::string& attributeName,
                                         double value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setAttribute(const std::string& attributeName,
                                         unsigned int value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::setAttribute(const std::string& attributeName,
                                         const std::string& value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribDiscreteUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribPoissonDistribution.
 */
SBase*
DistribPoissonDistribution::createChildObject(const std::string& elementName)
{
  DistribDiscreteUnivariateDistribution* obj = NULL;

  if (elementName == "rate")
  {
    return createRate();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribPoissonDistribution.
 */
int
DistribPoissonDistribution::addChildObject(const std::string& elementName,
                                           const SBase* element)
{
  if (elementName == "rate" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setRate((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribPoissonDistribution.
 */
SBase*
DistribPoissonDistribution::removeChildObject(const std::string& elementName,
                                              const std::string& id)
{
  if (elementName == "rate")
  {
    DistribUncertValue * obj = getRate();
    if (unsetRate() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribPoissonDistribution.
 */
unsigned int
DistribPoissonDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "rate")
  {
    if (isSetRate())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribPoissonDistribution.
 */
SBase*
DistribPoissonDistribution::getObject(const std::string& elementName,
                                      unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "rate")
  {
    return getRate();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribPoissonDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRate != NULL)
  {
    if (mRate->getId() == id)
    {
      return mRate;
    }

    obj = mRate->getElementBySId(id);
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
DistribPoissonDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRate != NULL)
  {
    if (mRate->getMetaId() == metaid)
    {
      return mRate;
    }

    obj = mRate->getElementByMetaId(metaid);
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
DistribPoissonDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mRate, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribPoissonDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDiscreteUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "rate")
  {
    if (isSetRate())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribPoissonDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mRate;
    mRate = new DistribUncertValue(distribns);
    mRate->setElementName(name);
    obj = mRate;
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
DistribPoissonDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDiscreteUnivariateDistribution::addExpectedAttributes(attributes);

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
DistribPoissonDistribution::readAttributes(const XMLAttributes& attributes,
                                           const ExpectedAttributes&
                                             expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribDiscreteUnivariateDistribution::readAttributes(attributes,
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
          DistribDistribPoissonDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  else
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
DistribPoissonDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
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
DistribPoissonDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
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
DistribPoissonDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribDiscreteUnivariateDistribution::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  else
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
DistribPoissonDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribPoissonDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribPoissonDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t *
DistribPoissonDistribution_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribPoissonDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribPoissonDistribution_t object.
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribPoissonDistribution_clone(const DistribPoissonDistribution_t* dpd)
{
  if (dpd != NULL)
  {
    return static_cast<DistribPoissonDistribution_t*>(dpd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribPoissonDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribPoissonDistribution_free(DistribPoissonDistribution_t* dpd)
{
  if (dpd != NULL)
  {
    delete dpd;
  }
}


/*
 * Returns the value of the "rate" element of this
 * DistribPoissonDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribPoissonDistribution_getRate(const DistribPoissonDistribution_t * dpd)
{
  if (dpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dpd->getRate());
}


/*
 * Predicate returning @c 1 (true) if this DistribPoissonDistribution_t's
 * "rate" element is set.
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_isSetRate(const DistribPoissonDistribution_t * dpd)
{
  return (dpd != NULL) ? static_cast<int>(dpd->isSetRate()) : 0;
}


/*
 * Sets the value of the "rate" element of this DistribPoissonDistribution_t.
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_setRate(DistribPoissonDistribution_t * dpd,
                                   const DistribUncertValue_t* rate)
{
  return (dpd != NULL) ? dpd->setRate(rate) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribPoissonDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribPoissonDistribution_createRate(DistribPoissonDistribution_t* dpd)
{
  if (dpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dpd->createRate());
}


/*
 * Unsets the value of the "rate" element of this DistribPoissonDistribution_t.
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_unsetRate(DistribPoissonDistribution_t * dpd)
{
  return (dpd != NULL) ? dpd->unsetRate() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribPoissonDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_hasRequiredAttributes(const
  DistribPoissonDistribution_t * dpd)
{
  return (dpd != NULL) ? static_cast<int>(dpd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribPoissonDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_hasRequiredElements(const
  DistribPoissonDistribution_t * dpd)
{
  return (dpd != NULL) ? static_cast<int>(dpd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


