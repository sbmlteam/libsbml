/**
 * @file    RenderCubicBezier.cpp
 * @brief Implementation of the RenderCubicBezier class.
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

#include <sbml/packages/render/sbml/RenderCubicBezier.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/xml/XMLNode.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>



using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new RenderCubicBezier using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RenderCubicBezier::RenderCubicBezier(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
:    RenderPoint(level,version,pkgVersion)
      , mBasePoint1_X(RelAbsVector(0.0,0.0))
      , mBasePoint1_Y(RelAbsVector(0.0,0.0))
      , mBasePoint1_Z(RelAbsVector(0.0,0.0))
      , mBasePoint2_X(RelAbsVector(0.0,0.0))
      , mBasePoint2_Y(RelAbsVector(0.0,0.0))
      , mBasePoint2_Z(RelAbsVector(0.0,0.0))
{
  RenderPkgNamespaces* renderns = new RenderPkgNamespaces(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(renderns);

  connectToChild();

  loadPlugins(renderns);
}


/*
 * Creates a new RenderCubicBezier object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RenderCubicBezier::RenderCubicBezier (RenderPkgNamespaces* renderns):
    RenderPoint(renderns)
  , mBasePoint1_X(RelAbsVector(0.0, 0.0))
  , mBasePoint1_Y(RelAbsVector(0.0, 0.0))
  , mBasePoint1_Z(RelAbsVector(0.0, 0.0))
  , mBasePoint2_X(RelAbsVector(0.0, 0.0))
  , mBasePoint2_Y(RelAbsVector(0.0, 0.0))
  , mBasePoint2_Z(RelAbsVector(0.0, 0.0))
{
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/*
 * Creates a CubicBezier with the given points.
 *
 * @param bp1_x x coordinate of the first base point.
 * @param bp1_y y coordinate of the first base point.
 * @param bp1_z z coordinate of the first base point.
 * @param bp1_x x coordinate of the second base point.
 * @param bp1_y y coordinate of the second base point.
 * @param bp1_z z coordinate of the second base point.
 * @param bp1_x x coordinate of the end point.
 * @param bp1_y y coordinate of the end point.
 * @param bp1_z z coordinate of the end point.
 */
RenderCubicBezier::RenderCubicBezier (RenderPkgNamespaces* renderns, const RelAbsVector& bp1_x,const RelAbsVector& bp1_y,const RelAbsVector& bp1_z,const RelAbsVector& bp2_x,const RelAbsVector& bp2_y,const RelAbsVector& bp2_z,const RelAbsVector& end_x,const RelAbsVector& end_y,const RelAbsVector& end_z):RenderPoint(renderns, end_x,end_y,end_z),
    mBasePoint1_X(bp1_x),
    mBasePoint1_Y(bp1_y),
    mBasePoint1_Z(bp1_z),
    mBasePoint2_X(bp2_x),
    mBasePoint2_Y(bp2_y),
    mBasePoint2_Z(bp2_z)
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/*
* Creates a new RenderCubicBezier object from the given XMLNode object.
* The XMLNode object has to contain a valid XML representation of a
* RenderCubicBezier object as defined in the render extension specification.
* This method is normally called when render information is read from a file and
* should normally not have to be called explicitly.
*
* @param node the XMLNode object reference that describes the RenderCubicBezier
* object to be instantiated.
*/
RenderCubicBezier::RenderCubicBezier(const XMLNode& node, unsigned int l2version)
  :RenderPoint(node, l2version)
{
  const XMLAttributes& attributes = node.getAttributes();
  const XMLNode* child;
  ExpectedAttributes ea;
  addExpectedAttributes(ea);
  this->readAttributes(attributes, ea);
  unsigned int n = 0, nMax = node.getNumChildren();
  while (n<nMax)
  {
    child = &node.getChild(n);
    const std::string& childName = child->getName();
    if (childName == "annotation")
    {
      this->mAnnotation = new XMLNode(node);
    }
    else if (childName == "notes")
    {
      this->mNotes = new XMLNode(node);
    }
    else
    {
      //throw;
    }
    ++n;
  }


  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2, l2version));

  connectToChild();
}


/*
 * Copy constructor for RenderCubicBezier objects.
 */
RenderCubicBezier::RenderCubicBezier(const RenderCubicBezier& orig)
  :RenderPoint(orig)
{
    mBasePoint1_X=orig.mBasePoint1_X;
    mBasePoint1_Y=orig.mBasePoint1_Y;
    mBasePoint1_Z=orig.mBasePoint1_Z;
    mBasePoint2_X=orig.mBasePoint2_X;
    mBasePoint2_Y=orig.mBasePoint2_Y;
    mBasePoint2_Z=orig.mBasePoint2_Z;
}



/*
 * Assignment operator for RenderCubicBezier objects.
 */
RenderCubicBezier& RenderCubicBezier::operator=(const RenderCubicBezier& orig)
{
    if(&orig!=this)
    {
        this->RenderPoint::operator=(orig);
        mBasePoint1_X=orig.mBasePoint1_X;
        mBasePoint1_Y=orig.mBasePoint1_Y;
        mBasePoint1_Z=orig.mBasePoint1_Z;
        mBasePoint2_X=orig.mBasePoint2_X;
        mBasePoint2_Y=orig.mBasePoint2_Y;
        mBasePoint2_Z=orig.mBasePoint2_Z;
    }
    return *this;
}


/*
 * Comparison operator for RenderCubicBezier objects.
 */
bool RenderCubicBezier::operator==(const RenderCubicBezier& left) const
{
    return (this->RenderPoint::operator==(left) &&   
            this->mBasePoint1_X == left.mBasePoint1_X &&
            this->mBasePoint1_Y == left.mBasePoint1_Y &&
            this->mBasePoint1_Z == left.mBasePoint1_Z &&
            this->mBasePoint2_X == left.mBasePoint2_X &&
            this->mBasePoint2_Y == left.mBasePoint2_Y &&
            this->mBasePoint2_Z == left.mBasePoint2_Z
           );
}


/*
* Creates and returns a deep copy of the RenderCubicBezier object.
*/
RenderCubicBezier*
RenderCubicBezier::clone() const
{
  return new RenderCubicBezier(*this);
}


/*
 * Destroys the RenderCubicBezier object.
 */ 
RenderCubicBezier::~RenderCubicBezier ()
{
}


/*
 * Returns the x value of the first base point of the curve (the one closer to the
 */ 
const RelAbsVector& RenderCubicBezier::getBasePoint1_x() const
{
  return this->mBasePoint1_X;
}


RelAbsVector& RenderCubicBezier::getBasePoint1_x()
{
  return this->mBasePoint1_X;
}


const RelAbsVector& RenderCubicBezier::basePoint1_X() const
{
    return this->mBasePoint1_X;
}


RelAbsVector& RenderCubicBezier::basePoint1_X()
{
  return this->mBasePoint1_X;
}


/*
 * Returns the y value of the first base point of the curve (the one closer to the
 */ 
const RelAbsVector& RenderCubicBezier::getBasePoint1_y() const
{
    return this->mBasePoint1_Y;
}


RelAbsVector& RenderCubicBezier::getBasePoint1_y()
{
  return this->mBasePoint1_Y;
}


const RelAbsVector& RenderCubicBezier::basePoint1_Y() const
{
  return this->mBasePoint1_Y;
}


RelAbsVector& RenderCubicBezier::basePoint1_Y()
{
  return this->mBasePoint1_Y;
}


/*
 * Returns the z value of the first base point of the curve (the one closer to the
 */ 
const RelAbsVector& RenderCubicBezier::getBasePoint1_z() const
{
    return this->mBasePoint1_Z;
}


RelAbsVector& RenderCubicBezier::getBasePoint1_z()
{
  return this->mBasePoint1_Z;
}


const RelAbsVector& RenderCubicBezier::basePoint1_Z() const
{
  return this->mBasePoint1_Z;
}


RelAbsVector& RenderCubicBezier::basePoint1_Z()
{
  return this->mBasePoint1_Z;
}


/*
 * Returns the x value of the second base point of the curve (the one further from the
 */ 
const RelAbsVector& RenderCubicBezier::getBasePoint2_x() const
{
    return this->mBasePoint2_X;
}


RelAbsVector& RenderCubicBezier::getBasePoint2_x()
{
  return this->mBasePoint2_X;
}


const RelAbsVector& RenderCubicBezier::basePoint2_X() const
{
  return this->mBasePoint2_X;
}


RelAbsVector& RenderCubicBezier::basePoint2_X()
{
  return this->mBasePoint2_X;
}



/*
 * Returns the y value of the second base point of the curve (the one further from the
 * @return const reference to y value of second base point
 */ 
const RelAbsVector& RenderCubicBezier::getBasePoint2_y() const
{
    return this->mBasePoint2_Y;
}


RelAbsVector& RenderCubicBezier::getBasePoint2_y()
{
  return this->mBasePoint2_Y;
}


const RelAbsVector& RenderCubicBezier::basePoint2_Y() const
{
  return this->mBasePoint2_Y;
}


RelAbsVector& RenderCubicBezier::basePoint2_Y()
{
  return this->mBasePoint2_Y;
}


/*
 * Returns the z value of the second base point of the curve (the one further from the
 */ 
RelAbsVector& RenderCubicBezier::getBasePoint2_z()
{
    return this->mBasePoint2_Z;
}


const RelAbsVector& RenderCubicBezier::getBasePoint2_z() const
{
  return this->mBasePoint2_Z;
}


const RelAbsVector& RenderCubicBezier::basePoint2_Z() const
{
  return this->mBasePoint2_Z;
}


RelAbsVector& RenderCubicBezier::basePoint2_Z()
{
  return this->mBasePoint2_Z;
}




/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint1_x"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint1_x() const
{
  return mBasePoint1_X.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint1_y"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint1_y() const
{
  return mBasePoint1_Y.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint1_z"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint1_z() const
{
  return mBasePoint1_Z.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint2_x"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint2_x() const
{
  return mBasePoint2_X.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint2_y"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint2_y() const
{
  return mBasePoint2_Y.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderCubicBezier's "basePoint2_z"
 * element is set.
 */
bool
RenderCubicBezier::isSetBasePoint2_z() const
{
  return mBasePoint2_Z.isSetCoordinate();
}


/*
 * Sets the x value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param x x coordinate of first base point.
 */ 
int RenderCubicBezier::setBasePoint1_X(const RelAbsVector& v)
{
    this->mBasePoint1_X=v;;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint1_x(const RelAbsVector& v)
{
  this->mBasePoint1_X = v;;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the y value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param y y coordinate of first base point.
 */ 
int RenderCubicBezier::setBasePoint1_Y(const RelAbsVector& v)
{
    this->mBasePoint1_Y=v;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint1_y(const RelAbsVector& v)
{
  this->mBasePoint1_Y = v;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the z value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param z z coordinate of first base point.
 */ 
int RenderCubicBezier::setBasePoint1_Z(const RelAbsVector& v)
{
    this->mBasePoint1_Z=v;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint1_z(const RelAbsVector& v)
{
  this->mBasePoint1_Z = v;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the x value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param x value of second base point.
 */ 
int RenderCubicBezier::setBasePoint2_X(const RelAbsVector& v)
{
    this->mBasePoint2_X=v;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint2_x(const RelAbsVector& v)
{
  this->mBasePoint2_X = v;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the y value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param y value of second base point.
 */ 
int RenderCubicBezier::setBasePoint2_Y(const RelAbsVector& v)
{
    this->mBasePoint2_Y=v;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint2_y(const RelAbsVector& v)
{
  this->mBasePoint2_Y = v;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the z value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param z value of second base point.
 */ 
int RenderCubicBezier::setBasePoint2_Z(const RelAbsVector& v)
{
    this->mBasePoint2_Z=v;
    return LIBSBML_OPERATION_SUCCESS;
}


int RenderCubicBezier::setBasePoint2_z(const RelAbsVector& v)
{
  this->mBasePoint2_Z = v;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the first basepoint to the given coordinatees.
*
* @param x coordinate of second base point.
* @param y coordinate of second base point.
* @param z coordinate of second base point.
* If the z coodinate is omitted, it is set to 0.
*/
void RenderCubicBezier::setBasePoint1(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
  this->mBasePoint1_X = x;
  this->mBasePoint1_Y = y;
  this->mBasePoint1_Z = z;
}


/*
* Sets the second basepoint to the given coordinatees.
*
* @param x coordinate of second base point.
* @param y coordinate of second base point.
* @param z coordinate of second base point.
* If the z coodinate is omitted, it is set to 0.
*/
void RenderCubicBezier::setBasePoint2(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
  this->mBasePoint2_X = x;
  this->mBasePoint2_Y = y;
  this->mBasePoint2_Z = z;
}


/*
 * Unsets the value of the "basePoint1_x" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint1_x()
{
  mBasePoint1_X.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "basePoint1_y" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint1_y()
{
  mBasePoint1_Y.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "basePoint1_z" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint1_z()
{
  mBasePoint1_Z.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "basePoint2_x" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint2_x()
{
  mBasePoint2_X.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "basePoint2_y" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint2_y()
{
  mBasePoint2_Y.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "basePoint2_z" element of this RenderCubicBezier.
 */
int
RenderCubicBezier::unsetBasePoint2_z()
{
  mBasePoint2_Z.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this RenderCubicBezier object.
 */
const std::string&
RenderCubicBezier::getElementName() const
{
  static const string name = "element";
  return name;
}


/*
 * Returns the libSBML type code for this RenderCubicBezier object.
 */
int
RenderCubicBezier::getTypeCode() const
{
  return SBML_RENDER_CUBICBEZIER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * RenderCubicBezier object have been set.
 */
bool
RenderCubicBezier::hasRequiredAttributes() const
{
  bool result = this->RenderPoint::hasRequiredAttributes();
  if (!isSetBasePoint1_x())
  {
    result = false;
  }

  if (!isSetBasePoint1_y())
  {
    result = false;
  }

  if (!isSetBasePoint2_x())
  {
    result = false;
  }

  if (!isSetBasePoint2_y())
  {
    result = false;
  }

  // z must not be nan
  result = result &&
    (this->mBasePoint1_Z.getAbsoluteValue() == this->mBasePoint1_Z.getAbsoluteValue()) &&
    (this->mBasePoint1_Z.getRelativeValue() == this->mBasePoint1_Z.getRelativeValue());

  // z must not be nan
  result = result &&
    (this->mBasePoint2_Z.getAbsoluteValue() == this->mBasePoint2_Z.getAbsoluteValue()) &&
    (this->mBasePoint2_Z.getRelativeValue() == this->mBasePoint2_Z.getRelativeValue());
  return result;
}


/** @cond doxygenLibsbmlInternal */
/*
* Creates an XMLNode object from this RenderCubicBezier object.
*
* @return the XMLNode with the XML representation for the
* RenderCubicBezier object.
*/
XMLNode RenderCubicBezier::toXML(const std::string& name) const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
void
RenderCubicBezier::addExpectedAttributes(ExpectedAttributes& attributes)
{
  RenderPoint::addExpectedAttributes(attributes);

  attributes.add("basePoint1_x");
  attributes.add("basePoint1_y");
  attributes.add("basePoint1_z");
  attributes.add("basePoint2_x");
  attributes.add("basePoint2_y");
  attributes.add("basePoint2_z");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderCubicBezier::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  RenderPoint::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs - 1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderRenderCubicBezierAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRenderCubicBezierAllowedCoreAttributes, pkgVersion, level, version,
          details, getLine(), getColumn());
      }
    }
  }

  string elplusid = "<renderCubicBezier> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }

  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // basepoint1_x RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("basePoint1_x", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    std::string message = "The required attribute 'basePoint1_x' is missing from the "
      + elplusid + ".";
    if (log) {
      log->logPackageError("render", RenderRenderCubicBezierAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    this->mBasePoint1_X = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      std::string message = "The syntax '" + s + "' of the attribute 'basePoint1_x' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      if (log)
      {
        log->logPackageError("render", RenderRenderCubicBezierBasePoint1_xMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      this->mBasePoint1_X = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
    }
    else
    {
      this->setBasePoint1_x(v);
    }
    v.erase();
  }

  //
  // basepoint1_y RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("basePoint1_y", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    std::string message = "The required attribute 'basePoint1_y' is missing from the "
      + elplusid + ".";
    if (log)
    {
      log->logPackageError("render", RenderRenderCubicBezierAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    this->mBasePoint1_Y = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'basePoint1_y' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRenderCubicBezierBasePoint1_yMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      this->mBasePoint1_Y = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
    }
    else
    {
      this->setBasePoint1_y(v);
    }
    v.erase();
  }

  //
  // basepoint1_z RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("basePoint1_z", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mBasePoint1_Z = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'basePoint1_z' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRenderCubicBezierBasePoint1_zMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
 //   else
    {
      this->setBasePoint1_z(v);
    }
    v.erase();
  }

  //
  // basepoint2_x RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("basePoint2_x", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    if (log)
    {
      std::string message = "The required attribute 'basePoint2_x' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRenderCubicBezierAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    this->mBasePoint2_X = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'basePoint2_x' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRenderCubicBezierBasePoint2_xMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      this->mBasePoint2_X = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
    }
    else
    {
      this->setBasePoint2_x(v);
    }
    v.erase();
  }

  //
  // basepoint2_y RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("basePoint2_y", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    if (log)
    {
      std::string message = "The required attribute 'basePoint2_y' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRenderCubicBezierAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    this->mBasePoint2_Y = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'basePoint2_y' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRenderCubicBezierBasePoint2_yMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      this->mBasePoint2_Y = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
    }
    else
    {
      this->setBasePoint2_y(v);
    }
    v.erase();
  }

  //
  // basepoint2_z RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("basePoint2_z", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mBasePoint2_Z = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'basePoint2_z' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRenderCubicBezierBasePoint2_zMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    //else
    {
      this->setBasePoint2_z(v);
    }
    v.erase();
  }

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.  For example:
 *
 *   SBase::writeAttributes(stream);
 *   stream.writeAttribute( "id"  , mId   );
 *   stream.writeAttribute( "name", mName );
 *   ...
 */
void RenderCubicBezier::writeAttributes (XMLOutputStream& stream) const
{
    SBase::writeAttributes(stream);
    XMLTriple triple("type","","xsi");
    stream.writeAttribute(triple,std::string("RenderCubicBezier"));
    std::ostringstream os;
    os << this->mXOffset;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << this->mYOffset;
    stream.writeAttribute("y", getPrefix(), os.str());
    if(this->mZOffset!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mZOffset;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mBasePoint1_X;
    stream.writeAttribute("basePoint1_x", getPrefix(), os.str());
    os.str("");
    os << this->mBasePoint1_Y;
    stream.writeAttribute("basePoint1_y", getPrefix(), os.str());
    if(this->mBasePoint1_Z!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mBasePoint1_Z;
        stream.writeAttribute("basePoint1_z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mBasePoint2_X;
    stream.writeAttribute("basePoint2_x", getPrefix(), os.str());
    os.str("");
    os << this->mBasePoint2_Y;
    stream.writeAttribute("basePoint2_y", getPrefix(), os.str());
    if(this->mBasePoint2_Z!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mBasePoint2_Z;
        stream.writeAttribute("basePoint2_z", getPrefix(), os.str());
    }
}
/** @endcond */

void
RenderCubicBezier::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  xmlns.add(LayoutExtension::getXmlnsXSI(), "xsi");
  stream << xmlns;
}

#endif /* __cplusplus */


/*
 * Creates a new RenderCubicBezier_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderCubicBezier_t *
RenderCubicBezier_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
{
  return new RenderCubicBezier(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RenderCubicBezier_t object.
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
RenderCubicBezier_clone(const RenderCubicBezier_t* rcb)
{
  if (rcb != NULL)
  {
    return static_cast<RenderCubicBezier_t*>(rcb->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RenderCubicBezier_t object.
 */
LIBSBML_EXTERN
void
RenderCubicBezier_free(RenderCubicBezier_t* rcb)
{
  if (rcb != NULL)
  {
    delete rcb;
  }
}


/*
 * Returns the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_x(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint1_x()));
}


/*
 * Returns the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_y(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint1_y()));
}


/*
 * Returns the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_z(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint1_z()));
}


/*
 * Returns the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_x(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint2_x()));
}


/*
 * Returns the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_y(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint2_y()));
}


/*
 * Returns the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_z(const RenderCubicBezier_t * rcb)
{
  if (rcb == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rcb->getBasePoint2_z()));
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_x"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_x(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint1_x()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_y"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_y(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint1_y()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_z"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_z(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint1_z()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_x"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_x(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint2_x()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_y"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_y(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint2_y()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_z"
 * element is set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_z(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->isSetBasePoint2_z()) : 0;
}


/*
 * Sets the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_x(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_x)
{
  return (rcb != NULL) ? rcb->setBasePoint1_x(*basePoint1_x) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_y(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_y)
{
  return (rcb != NULL) ? rcb->setBasePoint1_y(*basePoint1_y) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_z(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_z)
{
  return (rcb != NULL) ? rcb->setBasePoint1_z(*basePoint1_z) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_x(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_x)
{
  return (rcb != NULL) ? rcb->setBasePoint2_x(*basePoint2_x) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_y(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_y)
{
  return (rcb != NULL) ? rcb->setBasePoint2_y(*basePoint2_y) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_z(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_z)
{
  return (rcb != NULL) ? rcb->setBasePoint2_z(*basePoint2_z) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_x(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint1_x() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_y(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint1_y() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_z(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint1_z() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_x(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint2_x() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_y(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint2_y() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_z(RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? rcb->unsetBasePoint2_z() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderCubicBezier_t object have been set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_hasRequiredAttributes(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * RenderCubicBezier_t object have been set.
 */
LIBSBML_EXTERN
int
RenderCubicBezier_hasRequiredElements(const RenderCubicBezier_t * rcb)
{
  return (rcb != NULL) ? static_cast<int>(rcb->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


