/**
 * @file:   TransformationComponents.cpp
 * @brief:  Implementation of the TransformationComponents class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/spatial/sbml/TransformationComponents.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new TransformationComponents with the given level, version, and package version.
 */
TransformationComponents::TransformationComponents (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mComponents (NULL)
  , mComponentsLength (SBML_INT_MAX)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponents")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new TransformationComponents with the given SpatialPkgNamespaces object.
 */
TransformationComponents::TransformationComponents (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mComponents (NULL)
  , mComponentsLength (SBML_INT_MAX)
  , mIsSetComponentsLength (false)
  , mElementName("transformationComponents")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for TransformationComponents.
 */
TransformationComponents::TransformationComponents (const TransformationComponents& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mComponents  = NULL;
    setComponents(orig.mComponents, orig.mComponentsLength);
    mComponentsLength  = orig.mComponentsLength;
    mIsSetComponentsLength  = orig.mIsSetComponentsLength;
    mElementName = orig.mElementName;
  }
}


/*
 * Assignment for TransformationComponents.
 */
TransformationComponents&
TransformationComponents::operator=(const TransformationComponents& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mComponents  = NULL;
    setComponents(rhs.mComponents, rhs.mComponentsLength);
    mComponentsLength  = rhs.mComponentsLength;
    mIsSetComponentsLength  = rhs.mIsSetComponentsLength;
    mElementName = rhs.mElementName;
  }
  return *this;
}


/*
 * Clone for TransformationComponents.
 */
TransformationComponents*
TransformationComponents::clone () const
{
  return new TransformationComponents(*this);
}


/*
 * Destructor for TransformationComponents.
 */
TransformationComponents::~TransformationComponents ()
{
  if (mComponents != NULL)
    delete[] mComponents;
  mComponents = NULL;
}


/*
 * The "components" attribute of this TransformationComponents is returned in an double* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
TransformationComponents::getComponents(double* outArray) const
{
   if (outArray == NULL || mComponents == NULL) return;

   memcpy(outArray , mComponents, sizeof(double)*mComponentsLength);
}


/*
 * Returns the value of the "componentsLength" attribute of this TransformationComponents.
 */
int
TransformationComponents::getComponentsLength() const
{
  return mComponentsLength;
}


/*
 * Returns true/false if components is set.
 */
bool
TransformationComponents::isSetComponents() const
{
  return (mComponents != NULL);
}


/*
 * Returns true/false if componentsLength is set.
 */
bool
TransformationComponents::isSetComponentsLength() const
{
  return mIsSetComponentsLength;
}


/*
 * Sets the "components" element of this TransformationComponents.
 *
 * @param inArray; double* array to be set (it will be copied).
 * @param arrayLength; the length of the array.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
TransformationComponents::setComponents(double* inArray, int arrayLength)
{
  if (inArray == NULL) return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (mComponents != NULL) delete[] mComponents;
  mComponents = new double[arrayLength];
  memcpy(mComponents, inArray, sizeof(double)*arrayLength);
  mIsSetComponentsLength = true;
  mComponentsLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}
/*
 * Sets componentsLength and returns value indicating success.
 */
int
TransformationComponents::setComponentsLength(int componentsLength)
{
  mComponentsLength = componentsLength;
  mIsSetComponentsLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets components and returns value indicating success.
 */
int
TransformationComponents::unsetComponents()
{
  if (mComponents != NULL)
   delete[] mComponents;
  mComponents = NULL;
  return unsetComponentsLength();
}


/*
 * Unsets componentsLength and returns value indicating success.
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
 * Returns the XML element name of this object
 */
const std::string&
TransformationComponents::getElementName () const
{
  return mElementName;
}


/*
 * Sets the element name for this object
 */
void
TransformationComponents::setElementName(const std::string& name)
{
  mElementName = name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
TransformationComponents::getTypeCode () const
{
  return SBML_SPATIAL_TRANSFORMATIONCOMPONENTS;
}


/*
 * check if all the required attributes are set
 */
bool
TransformationComponents::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetComponents() == false)
    allPresent = false;

  if (isSetComponentsLength() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
TransformationComponents::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
TransformationComponents::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
TransformationComponents::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
TransformationComponents::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
TransformationComponents::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("componentsLength");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
TransformationComponents::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  //bool assigned = false;

  //
  // componentsLength int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetComponentsLength = attributes.readInto("componentsLength", mComponentsLength);

  if (mIsSetComponentsLength == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'componentsLength' is missing from 'transformationComponents' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
TransformationComponents::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetComponentsLength() == true)
    stream.writeAttribute("componentsLength", getPrefix(), mComponentsLength);

}


  /** @endcond doxygenLibsbmlInternal */


void
TransformationComponents::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  if(isSetComponents())
  {
    for (int i = 0; i < mComponentsLength; ++i)
    {
      stream << (double)mComponents[i] << " ";
    }
  }
  stream.endElement(getElementName(), getPrefix());
}


void
TransformationComponents::setElementText(const std::string &text)
{
  stringstream strStream(text); // Insert the string into a stream
  double val;
  vector<double> valuesVector;
  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  // convert the vector to an array
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
LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_create(unsigned int level, unsigned int version,
                                unsigned int pkgVersion)
{
  return new TransformationComponents(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
TransformationComponents_free(TransformationComponents_t * tc)
{
  if (tc != NULL)
    delete tc;
}


LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_clone(TransformationComponents_t * tc)
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


LIBSBML_EXTERN
int
TransformationComponents_getComponentsLength(const TransformationComponents_t * tc)
{
	return (tc != NULL) ? tc->getComponentsLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
TransformationComponents_isSetComponents(const TransformationComponents_t * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponents()) : 0;
}


LIBSBML_EXTERN
int
TransformationComponents_isSetComponentsLength(const TransformationComponents_t * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->isSetComponentsLength()) : 0;
}


LIBSBML_EXTERN
int
TransformationComponents_setComponentsLength(TransformationComponents_t * tc, int componentsLength)
{
  if (tc != NULL)
    return tc->setComponentsLength(componentsLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
TransformationComponents_unsetComponents(TransformationComponents_t * tc)
{
  return (tc != NULL) ? tc->unsetComponents() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
TransformationComponents_unsetComponentsLength(TransformationComponents_t * tc)
{
  return (tc != NULL) ? tc->unsetComponentsLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
TransformationComponents_hasRequiredAttributes(const TransformationComponents_t * tc)
{
  return (tc != NULL) ? static_cast<int>(tc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


