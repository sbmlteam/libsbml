/**
 * @file DistribGammaDistribution.cpp
 * @brief Implementation of the DistribGammaDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribGammaDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribGammaDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribGammaDistribution::DistribGammaDistribution(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version, pkgVersion)
  , mShape (NULL)
  , mScale (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribGammaDistribution using the given DistribPkgNamespaces
 * object.
 */
DistribGammaDistribution::DistribGammaDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mShape (NULL)
  , mScale (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribGammaDistribution.
 */
DistribGammaDistribution::DistribGammaDistribution(const
  DistribGammaDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mShape ( NULL )
  , mScale ( NULL )
{
  if (orig.mShape != NULL)
  {
    mShape = orig.mShape->clone();
  }

  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribGammaDistribution.
 */
DistribGammaDistribution&
DistribGammaDistribution::operator=(const DistribGammaDistribution& rhs)
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
 * Creates and returns a deep copy of this DistribGammaDistribution object.
 */
DistribGammaDistribution*
DistribGammaDistribution::clone() const
{
  return new DistribGammaDistribution(*this);
}


/*
 * Destructor for DistribGammaDistribution.
 */
DistribGammaDistribution::~DistribGammaDistribution()
{
  delete mShape;
  mShape = NULL;
  delete mScale;
  mScale = NULL;
}


/*
 * Returns the value of the "shape" element of this DistribGammaDistribution.
 */
const DistribUncertValue*
DistribGammaDistribution::getShape() const
{
  return mShape;
}


/*
 * Returns the value of the "shape" element of this DistribGammaDistribution.
 */
DistribUncertValue*
DistribGammaDistribution::getShape()
{
  return mShape;
}


/*
 * Returns the value of the "scale" element of this DistribGammaDistribution.
 */
const DistribUncertValue*
DistribGammaDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this DistribGammaDistribution.
 */
DistribUncertValue*
DistribGammaDistribution::getScale()
{
  return mScale;
}


/*
 * Predicate returning @c true if this DistribGammaDistribution's "shape"
 * element is set.
 */
bool
DistribGammaDistribution::isSetShape() const
{
  return (mShape != NULL);
}


/*
 * Predicate returning @c true if this DistribGammaDistribution's "scale"
 * element is set.
 */
bool
DistribGammaDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Sets the value of the "shape" element of this DistribGammaDistribution.
 */
int
DistribGammaDistribution::setShape(const DistribUncertValue* shape)
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
 * Sets the value of the "scale" element of this DistribGammaDistribution.
 */
int
DistribGammaDistribution::setScale(const DistribUncertValue* scale)
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
 * DistribGammaDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribGammaDistribution::createShape()
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
 * DistribGammaDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribGammaDistribution::createScale()
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
 * Unsets the value of the "shape" element of this DistribGammaDistribution.
 */
int
DistribGammaDistribution::unsetShape()
{
  delete mShape;
  mShape = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scale" element of this DistribGammaDistribution.
 */
int
DistribGammaDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribGammaDistribution object.
 */
const std::string&
DistribGammaDistribution::getElementName() const
{
  static const string name = "gammaDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribGammaDistribution object.
 */
int
DistribGammaDistribution::getTypeCode() const
{
  return SBML_DISTRIB_GAMMADISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribGammaDistribution object have been set.
 */
bool
DistribGammaDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribGammaDistribution object have been set.
 */
bool
DistribGammaDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetShape() == false)
  {
    allPresent = false;
  }

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
DistribGammaDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetShape() == true)
  {
    mShape->write(stream);
  }

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
DistribGammaDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mShape != NULL)
  {
    mShape->accept(v);
  }

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
DistribGammaDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mShape != NULL)
  {
    mShape->setSBMLDocument(d);
  }

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
DistribGammaDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mShape != NULL)
  {
    mShape->connectToParent(this);
  }

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
DistribGammaDistribution::enablePackageInternal(const std::string& pkgURI,
                                                const std::string& pkgPrefix,
                                                bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetShape())
  {
    mShape->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

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
DistribGammaDistribution::updateSBMLNamespace(const std::string& package,
                                              unsigned int level,
                                              unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mShape != NULL)
  {
    mShape->updateSBMLNamespace(package, level, version);
  }

  if (mScale != NULL)
  {
    mScale->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::getAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::getAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::getAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::getAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribGammaDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribGammaDistribution::isSetAttribute(const std::string& attributeName)
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::setAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::setAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::setAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::setAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::setAttribute(const std::string& attributeName,
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
 * DistribGammaDistribution.
 */
int
DistribGammaDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribGammaDistribution.
 */
SBase*
DistribGammaDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "shape")
  {
    return createShape();
  }
  else if (elementName == "scale")
  {
    return createScale();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribGammaDistribution.
 */
int
DistribGammaDistribution::addChildObject(const std::string& elementName,
                                         const SBase* element)
{
  if (elementName == "shape" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setShape((const DistribUncertValue*)(element));
  }
  else if (elementName == "scale" && element->getTypeCode() ==
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
 * DistribGammaDistribution.
 */
SBase*
DistribGammaDistribution::removeChildObject(const std::string& elementName,
                                            const std::string& id)
{
  if (elementName == "shape")
  {
    DistribUncertValue * obj = getShape();
    if (unsetShape() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "scale")
  {
    DistribUncertValue * obj = getScale();
    if (unsetScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribGammaDistribution.
 */
unsigned int
DistribGammaDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "shape")
  {
    if (isSetShape())
    {
      return 1;
    }
  }
  else if (elementName == "scale")
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
 * Returns the nth object of "objectName" in this DistribGammaDistribution.
 */
SBase*
DistribGammaDistribution::getObject(const std::string& elementName,
                                    unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "shape")
  {
    return getShape();
  }
  else if (elementName == "scale")
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
DistribGammaDistribution::getElementBySId(const std::string& id)
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
DistribGammaDistribution::getElementByMetaId(const std::string& metaid)
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
DistribGammaDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mShape, filter);
  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribGammaDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "shape")
  {
    if (isSetShape())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribGammaDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mShape;
    mShape = new DistribUncertValue(distribns);
    mShape->setElementName(name);
    obj = mShape;
  }
  else if (name == "scale")
  {
    if (isSetScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribGammaDistributionAllowedElements, getPackageVersion(),
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
DistribGammaDistribution::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribGammaDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribGammaDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribGammaDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribGammaDistribution_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribGammaDistribution_t *
DistribGammaDistribution_create(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion)
{
  return new DistribGammaDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribGammaDistribution_t object.
 */
LIBSBML_EXTERN
DistribGammaDistribution_t*
DistribGammaDistribution_clone(const DistribGammaDistribution_t* dgd)
{
  if (dgd != NULL)
  {
    return static_cast<DistribGammaDistribution_t*>(dgd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribGammaDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribGammaDistribution_free(DistribGammaDistribution_t* dgd)
{
  if (dgd != NULL)
  {
    delete dgd;
  }
}


/*
 * Returns the value of the "shape" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribGammaDistribution_getShape(const DistribGammaDistribution_t * dgd)
{
  if (dgd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dgd->getShape());
}


/*
 * Returns the value of the "scale" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribGammaDistribution_getScale(const DistribGammaDistribution_t * dgd)
{
  if (dgd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dgd->getScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribGammaDistribution_t's "shape"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_isSetShape(const DistribGammaDistribution_t * dgd)
{
  return (dgd != NULL) ? static_cast<int>(dgd->isSetShape()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribGammaDistribution_t's "scale"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_isSetScale(const DistribGammaDistribution_t * dgd)
{
  return (dgd != NULL) ? static_cast<int>(dgd->isSetScale()) : 0;
}


/*
 * Sets the value of the "shape" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_setShape(DistribGammaDistribution_t * dgd,
                                  const DistribUncertValue_t* shape)
{
  return (dgd != NULL) ? dgd->setShape(shape) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scale" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_setScale(DistribGammaDistribution_t * dgd,
                                  const DistribUncertValue_t* scale)
{
  return (dgd != NULL) ? dgd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribGammaDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribGammaDistribution_createShape(DistribGammaDistribution_t* dgd)
{
  if (dgd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dgd->createShape());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribGammaDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribGammaDistribution_createScale(DistribGammaDistribution_t* dgd)
{
  if (dgd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dgd->createScale());
}


/*
 * Unsets the value of the "shape" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_unsetShape(DistribGammaDistribution_t * dgd)
{
  return (dgd != NULL) ? dgd->unsetShape() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scale" element of this DistribGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_unsetScale(DistribGammaDistribution_t * dgd)
{
  return (dgd != NULL) ? dgd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribGammaDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_hasRequiredAttributes(const DistribGammaDistribution_t
  * dgd)
{
  return (dgd != NULL) ? static_cast<int>(dgd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribGammaDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribGammaDistribution_hasRequiredElements(const DistribGammaDistribution_t *
  dgd)
{
  return (dgd != NULL) ? static_cast<int>(dgd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


