/**
 * @file TransformationComponent.cpp
 * @brief Implementation of the TransformationComponent class.
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
#include <sbml/packages/spatial/sbml/TransformationComponent.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new TransformationComponent using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
TransformationComponent::TransformationComponent(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
  : SBase(level, version)
  , mComponents (NULL)
  , mComponentsLength (SBML_INT_MAX)
  , mActualComponentsLength (0)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponent")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new TransformationComponent using the given SpatialPkgNamespaces
 * object.
 */
TransformationComponent::TransformationComponent(SpatialPkgNamespaces
  *spatialns)
  : SBase(spatialns)
  , mComponents (NULL)
  , mActualComponentsLength (0)
  , mComponentsLength (SBML_INT_MAX)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponent")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for TransformationComponent.
 */
TransformationComponent::TransformationComponent(const TransformationComponent&
  orig)
  : SBase( orig )
  , mComponents ( NULL )
  , mComponentsLength ( orig.mComponentsLength )
  , mIsSetComponentsLength ( orig.mIsSetComponentsLength )
  , mElementName ( orig.mElementName )
{
  setComponents(orig.mComponents, orig.mActualComponentsLength);

}


/*
 * Assignment operator for TransformationComponent.
 */
TransformationComponent&
TransformationComponent::operator=(const TransformationComponent& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mComponents = NULL;
    setComponents(rhs.mComponents, rhs.mActualComponentsLength);
    mComponentsLength = rhs.mComponentsLength;
    mIsSetComponentsLength = rhs.mIsSetComponentsLength;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this TransformationComponent object.
 */
TransformationComponent*
TransformationComponent::clone() const
{
  return new TransformationComponent(*this);
}


/*
 * Destructor for TransformationComponent.
 */
TransformationComponent::~TransformationComponent()
{
  if (mComponents != NULL)
  {
    delete [] mComponents;
  }

  mComponents = NULL;
}


/*
 * Returns the value of the "components" attribute of this
 * TransformationComponent.
 */
void
TransformationComponent::getComponents(double* outArray) const
{
  if (outArray == NULL || mComponents == NULL)
  {
    return;
  }

  memcpy(outArray, mComponents, sizeof(double)*mActualComponentsLength);
}


/*
 * Returns the value of the "componentsLength" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getComponentsLength() const
{
  return mComponentsLength;
}

size_t TransformationComponent::getActualComponentsLength() const
{
  return mActualComponentsLength;
}


/*
 * Predicate returning @c true if this TransformationComponent's "components"
 * attribute is set.
 */
bool
TransformationComponent::isSetComponents() const
{
  return (mComponents != NULL);
}


/*
 * Predicate returning @c true if this TransformationComponent's
 * "componentsLength" attribute is set.
 */
bool
TransformationComponent::isSetComponentsLength() const
{
  return mIsSetComponentsLength;
}


/*
 * Sets the value of the "components" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setComponents(double* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  if (mComponents != NULL)
  {
    delete[] mComponents;
  }

  mComponents = new double[arrayLength];
  memcpy(mComponents, inArray, sizeof(double)*arrayLength);
  mActualComponentsLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "componentsLength" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setComponentsLength(int componentsLength)
{
  mComponentsLength = componentsLength;
  mIsSetComponentsLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "components" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::unsetComponents()
{
  if (mComponents != NULL)
  {
    delete[] mComponents;
  }

  mComponents = NULL;

  return unsetComponentsLength();
}


/*
 * Unsets the value of the "componentsLength" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::unsetComponentsLength()
{
  mComponentsLength = SBML_INT_MAX;
  mIsSetComponentsLength = false;

  if (isSetComponentsLength() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this TransformationComponent object.
 */
const std::string&
TransformationComponent::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this TransformationComponent object.
 */
void
TransformationComponent::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this TransformationComponent object.
 */
int
TransformationComponent::getTypeCode() const
{
  return SBML_SPATIAL_TRANSFORMATIONCOMPONENT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * TransformationComponent object have been set.
 */
bool
TransformationComponent::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetComponents() == false)
  {
    allPresent = false;
  }

  if (isSetComponentsLength() == false)
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
TransformationComponent::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
TransformationComponent::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
TransformationComponent::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * used to write arrays
 */
void
TransformationComponent::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  stream.endElement(getElementName(), getPrefix());
}

/** @endcond */

string TransformationComponent::getComponentsString() const
{
  stringstream out;
  for (size_t i = 0; i < mActualComponentsLength; ++i)
  {
    out << (double)mComponents[i] << " ";
  }
  return out.str();
}


/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
TransformationComponent::enablePackageInternal(const std::string& pkgURI,
                                               const std::string& pkgPrefix,
                                               bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getAttribute(const std::string& attributeName,
                                      bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getAttribute(const std::string& attributeName,
                                      int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "componentsLength")
  {
    value = getComponentsLength();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getAttribute(const std::string& attributeName,
                                      double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getAttribute(const std::string& attributeName,
                                      unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::getAttribute(const std::string& attributeName,
                                      std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this TransformationComponent's attribute
 * "attributeName" is set.
 */
bool
TransformationComponent::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "components")
  {
    value = isSetComponents();
  }
  else if (attributeName == "componentsLength")
  {
    value = isSetComponentsLength();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setAttribute(const std::string& attributeName,
                                      bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setAttribute(const std::string& attributeName,
                                      int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "componentsLength")
  {
    return_value = setComponentsLength(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setAttribute(const std::string& attributeName,
                                      double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setAttribute(const std::string& attributeName,
                                      unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::setAttribute(const std::string& attributeName,
                                      const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * TransformationComponent.
 */
int
TransformationComponent::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "components")
  {
    value = unsetComponents();
  }
  else if (attributeName == "componentsLength")
  {
    value = unsetComponentsLength();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
TransformationComponent::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("componentsLength");
  attributes.add("components");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
TransformationComponent::readAttributes(const XMLAttributes& attributes,
                                        const ExpectedAttributes&
                                          expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

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
        log->logPackageError("spatial",
          SpatialTransformationComponentAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialTransformationComponentAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // componentsLength int (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetComponentsLength = attributes.readInto("componentsLength",
    mComponentsLength);

  if ( mIsSetComponentsLength == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'componentsLength' from the "
        "<TransformationComponent> element must be an integer.";
      log->logPackageError("spatial",
        SpatialTransformationComponentComponentsLengthMustBeInteger, pkgVersion,
          level, version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'componentsLength' is missing "
        "from the <TransformationComponent> element.";
      log->logPackageError("spatial",
        SpatialTransformationComponentAllowedAttributes, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
  }

  std::string s;
  attributes.readInto("components", s);
  if(!s.empty())
  {
    if (this->parseTransformation(s)) {
      std::string message = "Spatial attribute 'components' contains elements that are not numeric.";
      log->logPackageError("spatial",
        SpatialTransformationComponentComponentsMustBeDoubleArray, pkgVersion, level,
        version, message, getLine(), getColumn());

    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
TransformationComponent::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetComponentsLength() == true)
  {
    stream.writeAttribute("componentsLength", getPrefix(), mComponentsLength);
  }

  if (mActualComponentsLength > 0)
  {
    stream.writeAttribute("components", getPrefix(), getComponentsString());
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the array data as a text element
 */
bool
TransformationComponent::parseTransformation(const std::string& text)
{
  stringstream strStream(text);
  double val;
  vector<double> valuesVector;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
    if (!strStream.eof() && strStream.peek() == ',') {
      strStream.get();
    }
    if (!strStream.eof() && strStream.peek() == ';') {
      strStream.get();
    }
    if (strStream.fail()) {
      return true;
    }
  }
  if (!strStream.eof()) {
    return true;
  }

  mActualComponentsLength = (unsigned int)valuesVector.size();

  if (mActualComponentsLength > 0)
  {
    double* data = new double[mActualComponentsLength];
    for (unsigned int i = 0; i < mActualComponentsLength; ++i)
    {
      data[i] = valuesVector.at(i);
    }

    setComponents(data, mActualComponentsLength);
    delete[] data;
  }
  return false;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new TransformationComponent_t using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
TransformationComponent_t *
TransformationComponent_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
{
  return new TransformationComponent(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this TransformationComponent_t object.
 */
LIBSBML_EXTERN
TransformationComponent_t*
TransformationComponent_clone(const TransformationComponent_t* tc)
{
  if (tc != NULL)
  {
    return static_cast<TransformationComponent_t*>(tc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this TransformationComponent_t object.
 */
LIBSBML_EXTERN
void
TransformationComponent_free(TransformationComponent_t* tc)
{
  if (tc != NULL)
  {
    delete tc;
  }
}


/*
 * Returns the value of the "componentsLength" attribute of this
 * TransformationComponent_t.
 */
LIBSBML_EXTERN
int
TransformationComponent_getComponentsLength(const TransformationComponent_t *
  tc)
{
  return (tc != NULL) ? tc->getComponentsLength() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this TransformationComponent_t's
 * "components" attribute is set.
 */
LIBSBML_EXTERN
int
TransformationComponent_isSetComponents(const TransformationComponent_t * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponents()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this TransformationComponent_t's
 * "componentsLength" attribute is set.
 */
LIBSBML_EXTERN
int
TransformationComponent_isSetComponentsLength(const TransformationComponent_t *
  tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponentsLength()) : 0;
}


/*
 * Sets the value of the "components" attribute of this
 * TransformationComponent_t.
 */
LIBSBML_EXTERN
int
TransformationComponent_setComponents(TransformationComponent_t* tc,
                                      double* components,
                                      int arrayLength)
{
  return (tc != NULL) ? tc->setComponents(components, arrayLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "componentsLength" attribute of this
 * TransformationComponent_t.
 */
LIBSBML_EXTERN
int
TransformationComponent_setComponentsLength(TransformationComponent_t * tc,
                                            int componentsLength)
{
  return (tc != NULL) ? tc->setComponentsLength(componentsLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "components" attribute of this
 * TransformationComponent_t.
 */
LIBSBML_EXTERN
int
TransformationComponent_unsetComponents(TransformationComponent_t * tc)
{
  return (tc != NULL) ? tc->unsetComponents() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "componentsLength" attribute of this
 * TransformationComponent_t.
 */
LIBSBML_EXTERN
int
TransformationComponent_unsetComponentsLength(TransformationComponent_t * tc)
{
  return (tc != NULL) ? tc->unsetComponentsLength() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * TransformationComponent_t object have been set.
 */
LIBSBML_EXTERN
int
TransformationComponent_hasRequiredAttributes(const TransformationComponent_t *
  tc)
{
  return (tc != NULL) ? static_cast<int>(tc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


