/**
 * @file:   PolygonObject.cpp
 * @brief:  Implementation of the PolygonObject class
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


#include <sbml/packages/spatial/sbml/PolygonObject.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new PolygonObject with the given level, version, and package version.
 */
PolygonObject::PolygonObject (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new PolygonObject with the given SpatialPkgNamespaces object.
 */
PolygonObject::PolygonObject (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for PolygonObject.
 */
PolygonObject::PolygonObject (const PolygonObject& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mPointIndex  = NULL;
    setPointIndex(orig.mPointIndex, orig.mPointIndexLength);
    mPointIndexLength  = orig.mPointIndexLength;
    mIsSetPointIndexLength  = orig.mIsSetPointIndexLength;
  }
}


/*
 * Assignment for PolygonObject.
 */
PolygonObject&
PolygonObject::operator=(const PolygonObject& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mPointIndex  = NULL;
    setPointIndex(rhs.mPointIndex, rhs.mPointIndexLength);
    mPointIndexLength  = rhs.mPointIndexLength;
    mIsSetPointIndexLength  = rhs.mIsSetPointIndexLength;
  }
  return *this;
}


/*
 * Clone for PolygonObject.
 */
PolygonObject*
PolygonObject::clone () const
{
  return new PolygonObject(*this);
}


/*
 * Destructor for PolygonObject.
 */
PolygonObject::~PolygonObject ()
{
  if (mPointIndex != NULL)
    delete[] mPointIndex;
  mPointIndex = NULL;
}


/*
 * The "pointIndex" attribute of this PolygonObject is returned in an int* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
PolygonObject::getPointIndex(int* outArray) const
{
   if (outArray == NULL || mPointIndex == NULL) return;

   memcpy(outArray , mPointIndex, sizeof(int)*mPointIndexLength);
}


/*
 * Returns the value of the "pointIndexLength" attribute of this PolygonObject.
 */
int
PolygonObject::getPointIndexLength() const
{
  return mPointIndexLength;
}


/*
 * Returns true/false if pointIndex is set.
 */
bool
PolygonObject::isSetPointIndex() const
{
  return (mPointIndex != NULL);
}


/*
 * Returns true/false if pointIndexLength is set.
 */
bool
PolygonObject::isSetPointIndexLength() const
{
  return mIsSetPointIndexLength;
}


/*
 * Sets the "pointIndex" element of this PolygonObject.
 *
 * @param inArray; int* array to be set (it will be copied).
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
PolygonObject::setPointIndex(int* inArray, int arrayLength)
{
  if (inArray == NULL) return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (mPointIndex != NULL) delete[] mPointIndex;
  mPointIndex = new int[arrayLength];
  memcpy(mPointIndex, inArray, sizeof(int)*arrayLength);
  mIsSetPointIndexLength = true;
  mPointIndexLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}
/*
 * Sets pointIndexLength and returns value indicating success.
 */
int
PolygonObject::setPointIndexLength(int pointIndexLength)
{
  mPointIndexLength = pointIndexLength;
  mIsSetPointIndexLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets pointIndex and returns value indicating success.
 */
int
PolygonObject::unsetPointIndex()
{
  if (mPointIndex != NULL)
   delete[] mPointIndex;
  mPointIndex = NULL;
  return unsetPointIndexLength();
}


/*
 * Unsets pointIndexLength and returns value indicating success.
 */
int
PolygonObject::unsetPointIndexLength()
{
  mPointIndexLength = SBML_INT_MAX;
  mIsSetPointIndexLength = false;

  if (isSetPointIndexLength() == false)
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
PolygonObject::getElementName () const
{
  static const string name = "polygonObject";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
PolygonObject::getTypeCode () const
{
  return SBML_SPATIAL_POLYGONOBJECT;
}


/*
 * check if all the required attributes are set
 */
bool
PolygonObject::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetPointIndex() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
PolygonObject::writeElements (XMLOutputStream& stream) const
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
PolygonObject::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
PolygonObject::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
PolygonObject::enablePackageInternal(const std::string& pkgURI,
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
PolygonObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("pointIndexLength");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
PolygonObject::readAttributes (const XMLAttributes& attributes,
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
  // pointIndexLength int   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetPointIndexLength = attributes.readInto("pointIndexLength", mPointIndexLength);

  if (mIsSetPointIndexLength == false)
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
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
PolygonObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetPointIndexLength() == true)
    stream.writeAttribute("pointIndexLength", getPrefix(), mPointIndexLength);

}


  /** @endcond doxygenLibsbmlInternal */


void
PolygonObject::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  if(isSetPointIndex())
  {
    for (int i = 0; i < mPointIndexLength; ++i)
    {
      stream << (long)mPointIndex[i] << " ";
    }
  }
  stream.endElement(getElementName(), getPrefix());
}


void
PolygonObject::setElementText(const std::string &text)
{
  stringstream strStream(text); // Insert the string into a stream
  int val;
  vector<int> valuesVector;
  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  // convert the vector to an array
  unsigned int length = (unsigned int)valuesVector.size();
  if (length > 0)
  {

    int* data = new int[length];
    for (unsigned int i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }

    setPointIndex(data, length);
  }
}
LIBSBML_EXTERN
PolygonObject_t *
PolygonObject_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new PolygonObject(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
PolygonObject_free(PolygonObject_t * po)
{
  if (po != NULL)
    delete po;
}


LIBSBML_EXTERN
PolygonObject_t *
PolygonObject_clone(PolygonObject_t * po)
{
  if (po != NULL)
  {
    return static_cast<PolygonObject_t*>(po->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
PolygonObject_getPointIndexLength(const PolygonObject_t * po)
{
	return (po != NULL) ? po->getPointIndexLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
PolygonObject_isSetPointIndex(const PolygonObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndex()) : 0;
}


LIBSBML_EXTERN
int
PolygonObject_isSetPointIndexLength(const PolygonObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndexLength()) : 0;
}


LIBSBML_EXTERN
int
PolygonObject_setPointIndexLength(PolygonObject_t * po, int pointIndexLength)
{
  if (po != NULL)
    return po->setPointIndexLength(pointIndexLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PolygonObject_unsetPointIndex(PolygonObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndex() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PolygonObject_unsetPointIndexLength(PolygonObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndexLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PolygonObject_hasRequiredAttributes(const PolygonObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


