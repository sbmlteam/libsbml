/**
 * @file    Transformation.cpp
 * @brief   class representing a 3D affine transformation
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/packages/render/sbml/Transformation.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/Image.h>
#include <sbml/packages/render/sbml/Ellipse.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/sbml/ListOfDrawables.h>
#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RenderCurve.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygenLibsbmlInternal */
const double Transformation::IDENTITY3D[12]={1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0};
const double Transformation::NAN3D[12] = { std::numeric_limits<double>::quiet_NaN(),
std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), 
std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(), 
std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
std::numeric_limits<double>::quiet_NaN() };
/** @endcond */

#ifdef __cplusplus


/*
 * Creates a new Transformation using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Transformation::Transformation(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
  : SBase(level, version)
  , mTransformLength (12)
{
    setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));  
    setMatrix(NAN3D);
  connectToChild();
}


/*
 * Creates a new Transformation using the given RenderPkgNamespaces object.
 */
Transformation::Transformation(RenderPkgNamespaces *renderns)
  : SBase(renderns)
  , mTransformLength(12)
{
  setMatrix(NAN3D);

    // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Transformation object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Transformation object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Transformation
 * object to be instantiated.
 */
Transformation::Transformation(const XMLNode& node, unsigned int l2version)
  : SBase(2, l2version)
  , mTransformLength(12)
{
  mURI = RenderExtension::getXmlnsL3V1V1();
  setMatrix(NAN3D);

  
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Copy constructor.
 */
Transformation::Transformation(const Transformation& orig)
  : SBase( orig )
  , mTransformLength(12)
{
  setMatrix(orig.getMatrix());
}


/*
 * Assignment operator for Transformation.
 */
Transformation&
Transformation::operator=(const Transformation& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    setMatrix(rhs.getMatrix());
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Transformation object.
 */
Transformation*
Transformation::clone() const
{
  return new Transformation(*this);
}


/*
 * Destructor for Transformation.
 */
Transformation::~Transformation()
{
}


/*
 * Returns the value of the "transform" attribute of this Transformation.
 */
void
Transformation::getTransform(double* outArray) const
{
  if (outArray == NULL || !isSetTransform())
  {
    return;
  }

  memcpy(outArray, mMatrix, sizeof(double)*mTransformLength);
}


/*
 * Returns the value of the "name" attribute of this Transformation.
 */
const std::string&
Transformation::getName() const
{
  return mName;
}


/*
 * Returns the matrix which is an array of double values of length 12.
 *
 * @return a pointer to the array of numbers for the transformation.
 */
const double* Transformation::getMatrix() const
{
    return mMatrix;
}


/*
 * Returns a 3D identity matrix.
 * The matrix contains 12 double values.
 */
const double* Transformation::getIdentityMatrix()
{
    return IDENTITY3D;
}


/*
* Returns true if the matrix has been set or false otherwise.
* The matrix is considered as set if none of the values in the matrix is NaN.
*
* @return @c true or false depending on whether a NaN was found.
*/
bool Transformation::isSetTransform() const
{
  return isSetMatrix();
}


/*
 * Predicate returning @c true if this Transformation's "name" attribute is
 * set.
 */
bool
Transformation::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true if the matrix has been set or false otherwise.
 * The matrix is considered as set if none of the values in the matrix is NaN.
 *
 * @return @c true or false depending on whether a NaN was found.
 */
bool Transformation::isSetMatrix() const
{
  bool result=true;
  
  for(int i=0;result && i<mTransformLength;++i)
  {
      result=(mMatrix[i]==mMatrix[i]);
  }
  return result;
}


/*
 * Sets the value of the "transform" attribute of this Transformation.
 */
int
Transformation::setTransform(double* inArray)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  for (int i = 0; i<mTransformLength; ++i)
  {
    mMatrix[i] = inArray[i];
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "name" attribute of this Transformation.
 */
int
Transformation::setName(const std::string& name)
{
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the matrix to the values given in the array.
*
* @param m array with new values to be set for this Transformation object.
*/
void Transformation::setMatrix(const double m[12])
{
  for (int i = 0; i<mTransformLength; ++i)
  {
    mMatrix[i] = m[i];
  }
}


/*
* Unsets the value of the "transform" attribute of this Transformation.
*/
int
Transformation::unsetTransform()
{
  return unsetMatrix();
}


/*
 * Unsets the value of the "name" attribute of this Transformation.
 */
int
Transformation::unsetName()
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
* Unsets the value of the "transform" attribute of this Transformation.
*/
int
Transformation::unsetMatrix()
{
  for (unsigned int i = 0; i < 12; ++i)
  {
    mMatrix[i] = std::numeric_limits<double>::quiet_NaN();
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * Image
 */
bool
Transformation::isImage() const
{
  return dynamic_cast<const Image*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * Ellipse
 */
bool
Transformation::isEllipse() const
{
  return dynamic_cast<const Ellipse*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * Rectangle
 */
bool
Transformation::isRectangle() const
{
  return dynamic_cast<const Rectangle*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * Polygon
 */
bool
Transformation::isPolygon() const
{
  return dynamic_cast<const Polygon*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * RenderGroup
 */
bool
Transformation::isRenderGroup() const
{
  return dynamic_cast<const RenderGroup*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * LineEnding
 */
bool
Transformation::isLineEnding() const
{
  return dynamic_cast<const LineEnding*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * Text
 */
bool
Transformation::isText() const
{
  return dynamic_cast<const Text*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation is of type
 * RenderCurve
 */
bool
Transformation::isRenderCurve() const
{
  return dynamic_cast<const RenderCurve*>(this) != NULL;
}


/*
 * Returns the XML element name of this Transformation object.
 */
const std::string&
Transformation::getElementName() const
{
  static const string name = "transformation";
  return name;
}


/*
 * Returns the libSBML type code for this Transformation object.
 */
int
Transformation::getTypeCode() const
{
  return SBML_RENDER_TRANSFORMATION;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Transformation::writeElements(XMLOutputStream& stream) const
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
Transformation::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */


/*
 * used to write arrays
 */
//void
//Transformation::write(XMLOutputStream& stream) const
//{
//  stream.startElement(getElementName(), getPrefix());
//  writeAttributes(stream);
//
//  if (isSetTransform())
//  {
//    for (int i = 0; i < mTransformLength; ++i)
//    {
//      stream << (double)mMatrix[i] << " ";
//    }
//  }
//
//  stream.endElement(getElementName(), getPrefix());
//}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Transformation's attribute
 * "attributeName" is set.
 */
bool
Transformation::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "transform")
  {
    value = isSetTransform();
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
 * Sets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Transformation.
 */
int
Transformation::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "transform")
  {
    value = unsetTransform();
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
 * Adds the expected attributes for this element
 */
void
Transformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("name");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Transformation::readAttributes(const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  //unsigned int pkgVersion = getPackageVersion();
//  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  SBase::readAttributes(attributes, expectedAttributes);

  //if (log)
  //{
  //  numErrs = log->getNumErrors();

  //  for (int n = numErrs-1; n >= 0; n--)
  //  {
  //    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
  //    {
  //      const std::string details = log->getError(n)->getMessage();
  //      log->remove(UnknownPackageAttribute);
  //      log->logPackageError("render", RenderTransformationAllowedAttributes,
  //        pkgVersion, level, version, details);
  //    }
  //    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
  //    {
  //      const std::string details = log->getError(n)->getMessage();
  //      log->remove(UnknownCoreAttribute);
  //      log->logPackageError("render",
  //        RenderTransformationAllowedCoreAttributes, pkgVersion, level, version,
  //          details);
  //    }
  //  }
  //}

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (log && assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<Transformation>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Transformation::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Image (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createImage(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
{
  return new Image(level, version, pkgVersion);
}


/*
 * Creates a new Ellipse (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createEllipse(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
{
  return new Ellipse(level, version, pkgVersion);
}


/*
 * Creates a new Rectangle (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRectangle(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
{
  return new Rectangle(level, version, pkgVersion);
}


/*
 * Creates a new Polygon (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createPolygon(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
{
  return new Polygon(level, version, pkgVersion);
}


/*
 * Creates a new RenderGroup (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRenderGroup(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
{
  return new RenderGroup(level, version, pkgVersion);
}


/*
 * Creates a new LineEnding (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createLineEnding(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion)
{
  return new LineEnding(level, version, pkgVersion);
}


/*
 * Creates a new Text (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createText(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new Text(level, version, pkgVersion);
}


/*
 * Creates a new RenderCurve (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRenderCurve(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
{
  return new RenderCurve(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Transformation_t object.
 */
LIBSBML_EXTERN
Transformation_t*
Transformation_clone(const Transformation_t* t)
{
  if (t != NULL)
  {
    return static_cast<Transformation_t*>(t->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Transformation_t object.
 */
LIBSBML_EXTERN
void
Transformation_free(Transformation_t* t)
{
  if (t != NULL)
  {
    delete t;
  }
}


/*
 * Returns the value of the "name" attribute of this Transformation_t.
 */
LIBSBML_EXTERN
char *
Transformation_getName(const Transformation_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return t->getName().empty() ? NULL : safe_strdup(t->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this Transformation_t's "transform"
 * attribute is set.
 */
LIBSBML_EXTERN
int
Transformation_isSetTransform(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetTransform()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Transformation_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
Transformation_isSetName(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetName()) : 0;
}


/*
 * Sets the value of the "transform" attribute of this Transformation_t.
 */
LIBSBML_EXTERN
int
Transformation_setTransform(Transformation_t* t,
                            double* transform)
{
  return (t != NULL) ? t->setTransform(transform) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this Transformation_t.
 */
LIBSBML_EXTERN
int
Transformation_setName(Transformation_t * t, const char * name)
{
  return (t != NULL) ? t->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "transform" attribute of this Transformation_t.
 */
LIBSBML_EXTERN
int
Transformation_unsetTransform(Transformation_t * t)
{
  return (t != NULL) ? t->unsetTransform() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this Transformation_t.
 */
LIBSBML_EXTERN
int
Transformation_unsetName(Transformation_t * t)
{
  return (t != NULL) ? t->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type Image_t
 */
LIBSBML_EXTERN
int
Transformation_isImage(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isImage()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type Ellipse_t
 */
LIBSBML_EXTERN
int
Transformation_isEllipse(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isEllipse()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type Rectangle_t
 */
LIBSBML_EXTERN
int
Transformation_isRectangle(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isRectangle()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type Polygon_t
 */
LIBSBML_EXTERN
int
Transformation_isPolygon(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isPolygon()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type RenderGroup_t
 */
LIBSBML_EXTERN
int
Transformation_isRenderGroup(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isRenderGroup()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type LineEnding_t
 */
LIBSBML_EXTERN
int
Transformation_isLineEnding(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isLineEnding()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type Text_t
 */
LIBSBML_EXTERN
int
Transformation_isText(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isText()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation_t is of type RenderCurve_t
 */
LIBSBML_EXTERN
int
Transformation_isRenderCurve(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isRenderCurve()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Transformation_t object have been set.
 */
LIBSBML_EXTERN
int
Transformation_hasRequiredAttributes(const Transformation_t * t)
{
  return (t != NULL) ? static_cast<int>(t->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


