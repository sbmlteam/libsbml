/**
 * @file TransformationComponents.cpp
 * @brief Implementation of the TransformationComponents class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
#include <sbml/packages/spatial/sbml/TransformationComponents.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new TransformationComponents using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
TransformationComponents::TransformationComponents(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
  : SBase(level, version)
  , mComponents (NULL)
  , mComponentsLength (SBML_INT_MAX)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponents")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new TransformationComponents using the given SpatialPkgNamespaces
 * object.
 */
TransformationComponents::TransformationComponents(SpatialPkgNamespaces
  *spatialns)
  : SBase(spatialns)
  , mComponents (NULL)
  , mComponentsLength (SBML_INT_MAX)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponents")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for TransformationComponents.
 */
TransformationComponents::TransformationComponents(const
  TransformationComponents& orig)
  : SBase( orig )
  , mComponents ( NULL )
  , mComponentsLength ( orig.mComponentsLength )
  , mIsSetComponentsLength ( orig.mIsSetComponentsLength )
  , mElementName ( orig.mElementName )
{
  setComponents(orig.mComponents, orig.mComponentsLength);

}


/*
 * Assignment operator for TransformationComponents.
 */
TransformationComponents&
TransformationComponents::operator=(const TransformationComponents& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mComponents = NULL;
    setComponents(rhs.mComponents, rhs.mComponentsLength);
    mComponentsLength = rhs.mComponentsLength;
    mIsSetComponentsLength = rhs.mIsSetComponentsLength;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this TransformationComponents object.
 */
TransformationComponents*
TransformationComponents::clone() const
{
  return new TransformationComponents(*this);
}


/*
 * Destructor for TransformationComponents.
 */
TransformationComponents::~TransformationComponents()
{
  if (mComponents != NULL)
  {
    delete [] mComponents;
  }

  mComponents = NULL;
}


/*
 * Returns the value of the "components" attribute of this
 * TransformationComponents.
 */
void
TransformationComponents::getComponents(double* outArray) const
{
  if (outArray == NULL || mComponents == NULL)
  {
    return;
  }

  memcpy(outArray, mComponents, sizeof(double)*mComponentsLength);
}


/*
 * Returns the value of the "componentsLength" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getComponentsLength() const
{
  return mComponentsLength;
}


/*
 * Predicate returning @c true if this TransformationComponents's "components"
 * attribute is set.
 */
bool
TransformationComponents::isSetComponents() const
{
  return (mComponents != NULL);
}


/*
 * Predicate returning @c true if this TransformationComponents's
 * "componentsLength" attribute is set.
 */
bool
TransformationComponents::isSetComponentsLength() const
{
  return mIsSetComponentsLength;
}


/*
 * Sets the value of the "components" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setComponents(double* inArray, int arrayLength)
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
  mIsSetComponentsLength = true;
  mComponentsLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "componentsLength" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setComponentsLength(int componentsLength)
{
  mComponentsLength = componentsLength;
  mIsSetComponentsLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "components" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::unsetComponents()
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
 * TransformationComponents.
 */
int
TransformationComponents::unsetComponentsLength()
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
 * Returns the XML element name of this TransformationComponents object.
 */
const std::string&
TransformationComponents::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this TransformationComponents object.
 */
void
TransformationComponents::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this TransformationComponents object.
 */
int
TransformationComponents::getTypeCode() const
{
  return SBML_SPATIAL_TRANSFORMATIONCOMPONENTS;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * TransformationComponents object have been set.
 */
bool
TransformationComponents::hasRequiredAttributes() const
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
TransformationComponents::writeElements(XMLOutputStream& stream) const
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
TransformationComponents::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
TransformationComponents::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * used to write arrays
 */
void
TransformationComponents::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);

  if (isSetComponents())
  {
    for (int i = 0; i < mComponentsLength; ++i)
    {
      stream << (double)mComponents[i] << " ";
    }
  }

  stream.endElement(getElementName(), getPrefix());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
TransformationComponents::enablePackageInternal(const std::string& pkgURI,
                                                const std::string& pkgPrefix,
                                                bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
                                       bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
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
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
                                       double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
                                       unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::getAttribute(const std::string& attributeName,
                                       const char* value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this TransformationComponents's attribute
 * "attributeName" is set.
 */
bool
TransformationComponents::isSetAttribute(const std::string& attributeName)
  const
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
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
                                       bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
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
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
                                       double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
                                       unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
                                       const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::setAttribute(const std::string& attributeName,
                                       const char* value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * TransformationComponents.
 */
int
TransformationComponents::unsetAttribute(const std::string& attributeName)
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
TransformationComponents::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("componentsLength");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
TransformationComponents::readAttributes(const XMLAttributes& attributes,
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
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("spatial",
        SpatialTransformationComponentsAllowedAttributes, pkgVersion, level,
          version, details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("spatial",
        SpatialTransformationComponentsAllowedCoreAttributes, pkgVersion, level,
          version, details);
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
        "<TransformationComponents> element must be an integer.";
      log->logPackageError("spatial",
        SpatialTransformationComponentsComponentsLengthMustBeInteger, pkgVersion,
          level, version, message);
    }
    else
    {
      std::string message = "Spatial attribute 'componentsLength' is missing "
        "from the <TransformationComponents> element.";
      log->logPackageError("spatial",
        SpatialTransformationComponentsAllowedAttributes, pkgVersion, level,
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
TransformationComponents::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetComponentsLength() == true)
  {
    stream.writeAttribute("componentsLength", getPrefix(), mComponentsLength);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the array data as a text element
 */
void
TransformationComponents::setElementText(const std::string& text)
{
  stringstream strStream(text);
  double val;
  vector<double> valuesVector;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  unsigned int length = (unsigned int)valuesVector.size();

  if (length > 0)
  {
    double* data = new double[length];
    for (unsigned int i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }

    setComponents(data, length);
    delete[] data;
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new TransformationComponents_t using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_create(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion)
{
  return new TransformationComponents(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this TransformationComponents_t object.
 */
LIBSBML_EXTERN
TransformationComponents_t*
TransformationComponents_clone(const TransformationComponents_t* tc)
{
  if (tc != NULL)
  {
    return static_cast<TransformationComponents_t*>(tc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this TransformationComponents_t object.
 */
LIBSBML_EXTERN
void
TransformationComponents_free(TransformationComponents_t* tc)
{
  if (tc != NULL)
  {
    delete tc;
  }
}


/*
 * Returns the value of the "componentsLength" attribute of this
 * TransformationComponents_t.
 */
LIBSBML_EXTERN
int
TransformationComponents_getComponentsLength(const TransformationComponents_t *
  tc)
{
  return (tc != NULL) ? tc->getComponentsLength() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 if this TransformationComponents_t's "components"
 * attribute is set.
 */
LIBSBML_EXTERN
int
TransformationComponents_isSetComponents(const TransformationComponents_t * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponents()) : 0;
}


/*
 * Predicate returning @c 1 if this TransformationComponents_t's
 * "componentsLength" attribute is set.
 */
LIBSBML_EXTERN
int
TransformationComponents_isSetComponentsLength(const TransformationComponents_t
  * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponentsLength()) : 0;
}


/*
 * Sets the value of the "components" attribute of this
 * TransformationComponents_t.
 */
LIBSBML_EXTERN
int
TransformationComponents_setComponents(TransformationComponents_t* tc,
                                       double* components,
                                       int arrayLength)
{
  return (tc != NULL) ? tc->setComponents(components, arrayLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "componentsLength" attribute of this
 * TransformationComponents_t.
 */
LIBSBML_EXTERN
int
TransformationComponents_setComponentsLength(TransformationComponents_t * tc,
                                             int componentsLength)
{
  return (tc != NULL) ? tc->setComponentsLength(componentsLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "components" attribute of this
 * TransformationComponents_t.
 */
LIBSBML_EXTERN
int
TransformationComponents_unsetComponents(TransformationComponents_t * tc)
{
  return (tc != NULL) ? tc->unsetComponents() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "componentsLength" attribute of this
 * TransformationComponents_t.
 */
LIBSBML_EXTERN
int
TransformationComponents_unsetComponentsLength(TransformationComponents_t * tc)
{
  return (tc != NULL) ? tc->unsetComponentsLength() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this
 * TransformationComponents_t object have been set.
 */
LIBSBML_EXTERN
int
TransformationComponents_hasRequiredAttributes(const TransformationComponents_t
  * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


