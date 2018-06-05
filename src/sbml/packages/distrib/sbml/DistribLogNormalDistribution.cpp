/**
 * @file DistribLogNormalDistribution.cpp
 * @brief Implementation of the DistribLogNormalDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribLogNormalDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribLogNormalDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribLogNormalDistribution::DistribLogNormalDistribution(unsigned int level,
                                                           unsigned int
                                                             version,
                                                           unsigned int
                                                             pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version, pkgVersion)
  , mShape (NULL)
  , mLogScale (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribLogNormalDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribLogNormalDistribution::DistribLogNormalDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mShape (NULL)
  , mLogScale (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribLogNormalDistribution.
 */
DistribLogNormalDistribution::DistribLogNormalDistribution(const
  DistribLogNormalDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mShape ( NULL )
  , mLogScale ( NULL )
{
  if (orig.mShape != NULL)
  {
    mShape = orig.mShape->clone();
  }

  if (orig.mLogScale != NULL)
  {
    mLogScale = orig.mLogScale->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribLogNormalDistribution.
 */
DistribLogNormalDistribution&
DistribLogNormalDistribution::operator=(const DistribLogNormalDistribution&
  rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mShape;
    if (rhs.mShape != NULL)
    {
      mShape = rhs.mShape->clone();
    }
    else
    {
      mShape = NULL;
    }

    delete mLogScale;
    if (rhs.mLogScale != NULL)
    {
      mLogScale = rhs.mLogScale->clone();
    }
    else
    {
      mLogScale = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribLogNormalDistribution object.
 */
DistribLogNormalDistribution*
DistribLogNormalDistribution::clone() const
{
  return new DistribLogNormalDistribution(*this);
}


/*
 * Destructor for DistribLogNormalDistribution.
 */
DistribLogNormalDistribution::~DistribLogNormalDistribution()
{
  delete mShape;
  mShape = NULL;
  delete mLogScale;
  mLogScale = NULL;
}


/*
 * Returns the value of the "shape" element of this
 * DistribLogNormalDistribution.
 */
const DistribUncertValue*
DistribLogNormalDistribution::getShape() const
{
  return mShape;
}


/*
 * Returns the value of the "shape" element of this
 * DistribLogNormalDistribution.
 */
DistribUncertValue*
DistribLogNormalDistribution::getShape()
{
  return mShape;
}


/*
 * Returns the value of the "logScale" element of this
 * DistribLogNormalDistribution.
 */
const DistribUncertValue*
DistribLogNormalDistribution::getLogScale() const
{
  return mLogScale;
}


/*
 * Returns the value of the "logScale" element of this
 * DistribLogNormalDistribution.
 */
DistribUncertValue*
DistribLogNormalDistribution::getLogScale()
{
  return mLogScale;
}


/*
 * Predicate returning @c true if this DistribLogNormalDistribution's "shape"
 * element is set.
 */
bool
DistribLogNormalDistribution::isSetShape() const
{
  return (mShape != NULL);
}


/*
 * Predicate returning @c true if this DistribLogNormalDistribution's
 * "logScale" element is set.
 */
bool
DistribLogNormalDistribution::isSetLogScale() const
{
  return (mLogScale != NULL);
}


/*
 * Sets the value of the "shape" element of this DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setShape(const DistribUncertValue* shape)
{
  if (shape == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (shape->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != shape->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != shape->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != shape->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mShape;
    mShape = (shape != NULL) ? static_cast<DistribUncertValue*>(shape->clone())
      : NULL;
    if (mShape != NULL) mShape->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "logScale" element of this
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setLogScale(const DistribUncertValue* logScale)
{
  if (logScale == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (logScale->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != logScale->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != logScale->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != logScale->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mLogScale;
    mLogScale = (logScale != NULL) ?
      static_cast<DistribUncertValue*>(logScale->clone()) : NULL;
    if (mLogScale != NULL) mLogScale->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribLogNormalDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribLogNormalDistribution::createShape()
{
  if (mShape != NULL)
  {
    delete mShape;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mShape = new DistribUncertValue(distribns);

  mShape->setElementName("shape");

  delete distribns;

  connectToChild();

  return mShape;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribLogNormalDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribLogNormalDistribution::createLogScale()
{
  if (mLogScale != NULL)
  {
    delete mLogScale;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mLogScale = new DistribUncertValue(distribns);

  mLogScale->setElementName("logScale");

  delete distribns;

  connectToChild();

  return mLogScale;
}


/*
 * Unsets the value of the "shape" element of this
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::unsetShape()
{
  delete mShape;
  mShape = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "logScale" element of this
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::unsetLogScale()
{
  delete mLogScale;
  mLogScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribLogNormalDistribution object.
 */
const std::string&
DistribLogNormalDistribution::getElementName() const
{
  static const string name = "logNormalDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribLogNormalDistribution object.
 */
int
DistribLogNormalDistribution::getTypeCode() const
{
  return SBML_DISTRIB_LOGNORMALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribLogNormalDistribution object have been set.
 */
bool
DistribLogNormalDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribLogNormalDistribution object have been set.
 */
bool
DistribLogNormalDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetShape() == false)
  {
    allPresent = false;
  }

  if (isSetLogScale() == false)
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
DistribLogNormalDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetShape() == true)
  {
    mShape->write(stream);
  }

  if (isSetLogScale() == true)
  {
    mLogScale->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribLogNormalDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mShape != NULL)
  {
    mShape->accept(v);
  }

  if (mLogScale != NULL)
  {
    mLogScale->accept(v);
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
DistribLogNormalDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mShape != NULL)
  {
    mShape->setSBMLDocument(d);
  }

  if (mLogScale != NULL)
  {
    mLogScale->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribLogNormalDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mShape != NULL)
  {
    mShape->connectToParent(this);
  }

  if (mLogScale != NULL)
  {
    mLogScale->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribLogNormalDistribution::enablePackageInternal(const std::string& pkgURI,
                                                    const std::string&
                                                      pkgPrefix,
                                                    bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetShape())
  {
    mShape->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetLogScale())
  {
    mLogScale->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribLogNormalDistribution::updateSBMLNamespace(const std::string& package,
                                                  unsigned int level,
                                                  unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mShape != NULL)
  {
    mShape->updateSBMLNamespace(package, level, version);
  }

  if (mLogScale != NULL)
  {
    mLogScale->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::getAttribute(const std::string& attributeName,
                                           std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribLogNormalDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribLogNormalDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribLogNormalDistribution.
 */
SBase*
DistribLogNormalDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "shape")
  {
    return createShape();
  }
  else if (elementName == "logScale")
  {
    return createLogScale();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribLogNormalDistribution.
 */
int
DistribLogNormalDistribution::addChildObject(const std::string& elementName,
                                             const SBase* element)
{
  if (elementName == "shape" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setShape((const DistribUncertValue*)(element));
  }
  else if (elementName == "logScale" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setLogScale((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribLogNormalDistribution.
 */
SBase*
DistribLogNormalDistribution::removeChildObject(const std::string& elementName,
                                                const std::string& id)
{
  if (elementName == "shape")
  {
    DistribUncertValue * obj = getShape();
    if (unsetShape() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "logScale")
  {
    DistribUncertValue * obj = getLogScale();
    if (unsetLogScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribLogNormalDistribution.
 */
unsigned int
DistribLogNormalDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "shape")
  {
    if (isSetShape())
    {
      return 1;
    }
  }
  else if (elementName == "logScale")
  {
    if (isSetLogScale())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribLogNormalDistribution.
 */
SBase*
DistribLogNormalDistribution::getObject(const std::string& elementName,
                                        unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "shape")
  {
    return getShape();
  }
  else if (elementName == "logScale")
  {
    return getLogScale();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribLogNormalDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mShape != NULL)
  {
    if (mShape->getId() == id)
    {
      return mShape;
    }

    obj = mShape->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mLogScale != NULL)
  {
    if (mLogScale->getId() == id)
    {
      return mLogScale;
    }

    obj = mLogScale->getElementBySId(id);
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
DistribLogNormalDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mShape != NULL)
  {
    if (mShape->getMetaId() == metaid)
    {
      return mShape;
    }

    obj = mShape->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mLogScale != NULL)
  {
    if (mLogScale->getMetaId() == metaid)
    {
      return mLogScale;
    }

    obj = mLogScale->getElementByMetaId(metaid);
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
DistribLogNormalDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mShape, filter);
  ADD_FILTERED_POINTER(ret, sublist, mLogScale, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribLogNormalDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "shape")
  {
    if (isSetShape())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribLogNormalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mShape;
    mShape = new DistribUncertValue(distribns);
    mShape->setElementName(name);
    obj = mShape;
  }
  else if (name == "logScale")
  {
    if (isSetLogScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribLogNormalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mLogScale;
    mLogScale = new DistribUncertValue(distribns);
    mLogScale->setElementName(name);
    obj = mLogScale;
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
DistribLogNormalDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
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
DistribLogNormalDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribLogNormalDistributionAllowedCoreAttributes, pkgVersion,
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
DistribLogNormalDistribution::readL3V1V1Attributes(const XMLAttributes&
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
DistribLogNormalDistribution::readL3V2V1Attributes(const XMLAttributes&
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
DistribLogNormalDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

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
DistribLogNormalDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribLogNormalDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribLogNormalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t *
DistribLogNormalDistribution_create(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion)
{
  return new DistribLogNormalDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribLogNormalDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t*
DistribLogNormalDistribution_clone(const DistribLogNormalDistribution_t* dlnd)
{
  if (dlnd != NULL)
  {
    return static_cast<DistribLogNormalDistribution_t*>(dlnd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribLogNormalDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribLogNormalDistribution_free(DistribLogNormalDistribution_t* dlnd)
{
  if (dlnd != NULL)
  {
    delete dlnd;
  }
}


/*
 * Returns the value of the "shape" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribLogNormalDistribution_getShape(const DistribLogNormalDistribution_t *
  dlnd)
{
  if (dlnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlnd->getShape());
}


/*
 * Returns the value of the "logScale" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribLogNormalDistribution_getLogScale(const DistribLogNormalDistribution_t *
  dlnd)
{
  if (dlnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlnd->getLogScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribLogNormalDistribution_t's
 * "shape" element is set.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_isSetShape(const DistribLogNormalDistribution_t *
  dlnd)
{
  return (dlnd != NULL) ? static_cast<int>(dlnd->isSetShape()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribLogNormalDistribution_t's
 * "logScale" element is set.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_isSetLogScale(const DistribLogNormalDistribution_t
  * dlnd)
{
  return (dlnd != NULL) ? static_cast<int>(dlnd->isSetLogScale()) : 0;
}


/*
 * Sets the value of the "shape" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_setShape(DistribLogNormalDistribution_t * dlnd,
                                      const DistribUncertValue_t* shape)
{
  return (dlnd != NULL) ? dlnd->setShape(shape) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "logScale" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_setLogScale(DistribLogNormalDistribution_t * dlnd,
                                         const DistribUncertValue_t* logScale)
{
  return (dlnd != NULL) ? dlnd->setLogScale(logScale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribLogNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribLogNormalDistribution_createShape(DistribLogNormalDistribution_t* dlnd)
{
  if (dlnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlnd->createShape());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribLogNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribLogNormalDistribution_createLogScale(DistribLogNormalDistribution_t*
  dlnd)
{
  if (dlnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlnd->createLogScale());
}


/*
 * Unsets the value of the "shape" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_unsetShape(DistribLogNormalDistribution_t * dlnd)
{
  return (dlnd != NULL) ? dlnd->unsetShape() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "logScale" element of this
 * DistribLogNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_unsetLogScale(DistribLogNormalDistribution_t *
  dlnd)
{
  return (dlnd != NULL) ? dlnd->unsetLogScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribLogNormalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_hasRequiredAttributes(const
  DistribLogNormalDistribution_t * dlnd)
{
  return (dlnd != NULL) ? static_cast<int>(dlnd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribLogNormalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribLogNormalDistribution_hasRequiredElements(const
  DistribLogNormalDistribution_t * dlnd)
{
  return (dlnd != NULL) ? static_cast<int>(dlnd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


