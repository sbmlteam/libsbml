/**
 * @file    RadialGradient.cpp
 * @brief Implementation of the RadialGradient class.
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
 * ---------------------------------------------------------------------- -->
 */
#include <sbml/packages/render/sbml/RadialGradient.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <limits>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN

/** @endcond */



#ifdef __cplusplus


/*
 * Creates a new RadialGradient using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RadialGradient::RadialGradient(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
:    GradientBase(level,version, pkgVersion)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
    ,mFZ(RelAbsVector(0.0,50.0))
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new RadialGradient using the given RenderPkgNamespaces object.
 */
RadialGradient::RadialGradient(RenderPkgNamespaces *renderns)
  : GradientBase(renderns)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
    ,mFZ(RelAbsVector(0.0,50.0))
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RadialGradient object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RadialGradient object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the RadialGradient
 * object to be instantiated.
 */
RadialGradient::RadialGradient(const XMLNode& node, unsigned int l2version):GradientBase(node, l2version)
{
  ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Constructor which creates a RadialGradient with no gradient stops.
 * The id is set to the given value.
 * The RadialGradient object is invalid until it has an id and at least two 
 * gradient stops.
 * The start and the end of the linear gradient vector are set to (0,0,0).
 * A linear gradient with a vector of length zero should also be considered invalid.
 *
 * @param id the new id for the RadialGradient.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
RadialGradient::RadialGradient(RenderPkgNamespaces* renderns, const std::string& id)
    :GradientBase(renderns, id)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
     ,mFZ(RelAbsVector(0.0,50.0))
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RadialGradient::RadialGradient(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
/*
 * Copy constructor for RadialGradient.
 */
RadialGradient::RadialGradient(const RadialGradient& orig)
  : GradientBase( orig )
  , mCX(orig.mCX)
  , mCY(orig.mCY)
  , mCZ(orig.mCZ)
  , mRadius(orig.mRadius)
  , mFX(orig.mFX)
  , mFY(orig.mFY)
  , mFZ(orig.mFZ)
{
  connectToChild();
}


/*
 * Assignment operator for RadialGradient.
 */
RadialGradient&
RadialGradient::operator=(const RadialGradient& rhs)
{
  if (&rhs != this)
  {
    GradientBase::operator=(rhs);
    mCX = rhs.mCX;
    mCY = rhs.mCY;
    mCZ = rhs.mCZ;
    mRadius = rhs.mRadius;
    mFX = rhs.mFX;
    mFY = rhs.mFY;
    mFZ = rhs.mFZ;
  
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this RadialGradient object.
 */
RadialGradient*
RadialGradient::clone() const
{
  return new RadialGradient(*this);
}


/*
 * Destructor for RadialGradient.
 */
RadialGradient::~RadialGradient()
{
}


/*
 * Returns the value of the "cx" element of this RadialGradient.
 */
const RelAbsVector&
RadialGradient::getCx() const
{
  return this->mCX;
}


/*
 * Returns the value of the "cx" element of this RadialGradient.
 */
RelAbsVector&
RadialGradient::getCx()
{
  return this->mCX;
}


/*
 * Returns the x coordinate for the start point as a const reference.
 */
const RelAbsVector& 
RadialGradient::getCenterX() const
{
    return this->mCX;
}


RelAbsVector&
RadialGradient::getCenterX()
{
  return this->mCX;
}


/*
* Returns the value of the "cy" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getCy() const
{
  return this->mCY;
}


/*
* Returns the value of the "cy" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getCy()
{
  return this->mCY;
}


/*
* Returns the y coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getCenterY() const
{
  return this->mCY;
}


RelAbsVector&
RadialGradient::getCenterY()
{
  return this->mCY;
}


/*
* Returns the value of the "cz" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getCz() const
{
  return this->mCZ;
}


/*
* Returns the value of the "cz" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getCz()
{
  return this->mCZ;
}


/*
* Returns the z coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getCenterZ() const
{
  return this->mCZ;
}


RelAbsVector&
RadialGradient::getCenterZ()
{
  return this->mCZ;
}


/*
* Returns the value of the "r" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getR() const
{
  return this->mRadius;
}


/*
* Returns the value of the "r" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getR()
{
  return this->mRadius;
}


/*
* Returns the z coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getRadius() const
{
  return this->mRadius;
}


RelAbsVector&
RadialGradient::getRadius()
{
  return this->mRadius;
}


/*
* Returns the value of the "fx" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getFx() const
{
  return this->mFX;
}


/*
* Returns the value of the "fx" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getFx()
{
  return this->mFX;
}


/*
* Returns the x coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getFocalPointX() const
{
  return this->mFX;
}

RelAbsVector&
RadialGradient::getFocalPointX()
{
  return this->mFX;
}

/*
* Returns the value of the "fy" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getFy() const
{
  return this->mFY;
}


/*
* Returns the value of the "fy" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getFy()
{
  return this->mFY;
}


/*
* Returns the y coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getFocalPointY() const
{
  return this->mFY;
}


RelAbsVector&
RadialGradient::getFocalPointY()
{
  return this->mFY;
}

/*
* Returns the value of the "fz" element of this RadialGradient.
*/
const RelAbsVector&
RadialGradient::getFz() const
{
  return this->mFZ;
}


/*
* Returns the value of the "fz" element of this RadialGradient.
*/
RelAbsVector&
RadialGradient::getFz()
{
  return this->mFZ;
}


/*
* Returns the z coordinate for the start point as a const reference.
*/
const RelAbsVector&
RadialGradient::getFocalPointZ() const
{
  return this->mFZ;
}


RelAbsVector&
RadialGradient::getFocalPointZ()
{
  return this->mFZ;
}


/*
 * Predicate returning @c true if this RadialGradient's "cx" element is set.
 */
bool
RadialGradient::isSetCx() const
{
  return mCX.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RadialGradient's "cy" element is set.
 */
bool
RadialGradient::isSetCy() const
{
  return mCY.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RadialGradient's "cz" element is set.
 */
bool
RadialGradient::isSetCz() const
{
  return mCZ.isSetCoordinate();
}


/*
* Predicate returning @c true if this RadialGradient's "r" element is set.
*/
bool
RadialGradient::isSetR() const
{
  return mRadius.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RadialGradient's "fx" element is set.
 */
bool
RadialGradient::isSetFx() const
{
  return mFX.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RadialGradient's "fy" element is set.
 */
bool
RadialGradient::isSetFy() const
{
  return mFY.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RadialGradient's "fz" element is set.
 */
bool
RadialGradient::isSetFz() const
{
  return mFZ.isSetCoordinate();
}


/*
 * Sets the value of the "cx" element of this RadialGradient.
 */
int
RadialGradient::setCx(const RelAbsVector& cx)
{
  this->mCX = cx;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "cy" element of this RadialGradient.
*/
int
RadialGradient::setCy(const RelAbsVector& cy)
{
  this->mCY = cy;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "cz" element of this RadialGradient.
*/
int
RadialGradient::setCz(const RelAbsVector& cz)
{
  this->mCZ = cz;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "r" element of this RadialGradient.
*/
int
RadialGradient::setR(const RelAbsVector& r)
{
  this->mRadius = r;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "r" element of this RadialGradient.
*/
int
RadialGradient::setRadius(const RelAbsVector& r)
{
  this->mRadius = r;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "fx" element of this RadialGradient.
*/
int
RadialGradient::setFx(const RelAbsVector& fx)
{
  this->mFX = fx;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "fy" element of this RadialGradient.
*/
int
RadialGradient::setFy(const RelAbsVector& fy)
{
  this->mFY = fy;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "fz" element of this RadialGradient.
*/
int
RadialGradient::setFz(const RelAbsVector& fz)
{
  this->mFZ = fz;
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
/*
* Sets the 3D coordinates for the center and the focal
* point as well as the radius.
* Each value can be a combination of absolute and relative value and is represented by
* a RelAbsVector object.
*
* @param x x value of the center point of the radial gradient vector
* @param y y value of the center point of the radial gradient vector
* @param z z value of the center point of the radial gradient vector
* @param r x value of the radius of the radial gradient vector
* @param fx x value of the focal point of the radial gradient vector
* @param fy y value of the focal point of the radial gradient vector
* @param fz z value of the focal point of the radial gradient vector
*/
void RadialGradient::setCoordinates(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z
  , const RelAbsVector& r, const RelAbsVector& fx, const RelAbsVector& fy, const RelAbsVector& fz)
{
  this->setCenter(x, y, z);
  this->setRadius(r);
  this->setFocalPoint(fx, fy, fz);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Sets the 2D coordinates for the center and the focal
* point as well as the radius.
* The z values are automatically set to 0.
* Each value can be a combination of absolute and relative value and is represented by
* a RelAbsVector object.
*
* @param x x value of the center point of the radial gradient vector
* @param y y value of the center point of the radial gradient vector
* @param r x value of the radius of the radial gradient vector
* @param fx x value of the focal point of the radial gradient vector
* @param fy y value of the focal point of the radial gradient vector
*/
void RadialGradient::setCoordinates(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& r, const RelAbsVector& fx, const RelAbsVector& fy)
{
  this->setCoordinates(x, y, RelAbsVector(0.0, 50.0), r, fx, fy, RelAbsVector(0.0, 50.0));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Sets the coordinates for the center point.
*
* @param x x value of the center point of the radial gradient vector
* @param y y value of the center point of the radial gradient vector
* @param z z value of the center point of the radial gradient vector
* The z argument can be omitted. In that case it is set to 0.
*/
void RadialGradient::setCenter(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
  this->mCX = x;
  this->mCY = y;
  this->mCZ = z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Sets the coordinates for the focal point.
*
* @param x x value of the focal point of the radial gradient vector
* @param y y value of the focal point of the radial gradient vector
* @param z z value of the focal point of the radial gradient vector.
* The z argument can be omitted. In that case it is set to 0.
*/
void RadialGradient::setFocalPoint(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
  this->mFX = x;
  this->mFY = y;
  this->mFZ = z;
}
/** @endcond */

/*
 * Unsets the value of the "cx" element of this RadialGradient.
 */
int
RadialGradient::unsetCx()
{
  mCX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "cy" element of this RadialGradient.
 */
int
RadialGradient::unsetCy()
{
  mCY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "cz" element of this RadialGradient.
 */
int
RadialGradient::unsetCz()
{
  mCZ.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "r" element of this RadialGradient.
 */
int
RadialGradient::unsetR()
{
  mRadius.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "fx" element of this RadialGradient.
 */
int
RadialGradient::unsetFx()
{
  mFX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "fy" element of this RadialGradient.
 */
int
RadialGradient::unsetFy()
{
  mFY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "fz" element of this RadialGradient.
 */
int
RadialGradient::unsetFz()
{
  mFZ.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this RadialGradient object.
 */
const std::string&
RadialGradient::getElementName() const
{
  static const string name = "radialGradient";
  return name;
}


/*
 * Returns the libSBML type code for this RadialGradient object.
 */
int
RadialGradient::getTypeCode() const
{
  return SBML_RENDER_RADIALGRADIENT;
}


/** @cond doxygenLibsbmlInternal */
/*
* Accepts the given SBMLVisitor for this instance of RadialGradient.
*
* @param v the SBMLVisitor instance to be used.
*
* @return the result of calling <code>v.visit()</code>.
*/
bool RadialGradient::accept(SBMLVisitor& /*visitor*/) const
{
  return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Creates an XMLNode object from this RadialGradient object.
*
* @return the XMLNode with the XML representation for the
* RadialGradient object.
*/
XMLNode RadialGradient::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
RadialGradient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GradientBase::addExpectedAttributes(attributes);

  attributes.add("cx");
  attributes.add("cy");
  attributes.add("cz");
  attributes.add("fx");
  attributes.add("fy");
  attributes.add("fz");
  attributes.add("r");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
RadialGradient::readAttributes(const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GradientBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderRadialGradientAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRadialGradientAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  string elplusid = "<radialGradient> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }

  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // cx RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("cx", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        this->mCX=RelAbsVector(0.0,50.0);   
    }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientCxMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setCx(v);
    }
    v.erase();
  }


  //
  // cy RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("cy", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        // default 50%
        this->mCY=RelAbsVector(0.0,50.0);   
    }
    else
    {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientCyMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setCy(v);
    }
    v.erase();
  }


  //
  // cz RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("cz", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        this->mCZ=RelAbsVector(0.0,50.0);   
    }
    else
    {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientCzMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setCz(v);
    }
    v.erase();
  }



  //
  // r RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("r", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mRadius = RelAbsVector(0.0, 50.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientRMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setR(v);
    }
    v.erase();
  }



  //
  // fx RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("fx", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        this->mFX=this->mCX;
    }
    else
    {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientFxMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setFx(v);
    }
    v.erase();
    }


  //
  // fy RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("fy", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        this->mFY=mCY;
    }
    else
    {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientFyMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setFy(v);
    }
    v.erase();
    }


  //
  // fz RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("fz", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
        this->mFZ=mCZ;
    }
    else
    {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderRadialGradientFzMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setFz(v);
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
void RadialGradient::writeAttributes (XMLOutputStream& stream) const
{
  GradientBase::writeAttributes(stream);
  std::ostringstream os;
  RelAbsVector dflt(0.0,50.0);
  if(this->mCX != dflt)
  {
      os << this->mCX;
      stream.writeAttribute("cx", getPrefix(), os.str());
  }
  os.str("");
  if(this->mCY != dflt)
  {
      os << this->mCY;
      stream.writeAttribute("cy", getPrefix(), os.str());
  }
  if(this->mCZ!=dflt)
  {
      os.str("");
      os << this->mCZ;
      stream.writeAttribute("cz", getPrefix(), os.str());
  }
  os.str("");
  if(this->mFX!=this->mCX)
  {
      os << this->mFX;
      stream.writeAttribute("fx", getPrefix(), os.str());
  }
  os.str("");
  if(this->mFY!=this->mCY)
  {
      os << this->mFY;
      stream.writeAttribute("fy", getPrefix(), os.str());
  }
  if(this->mFZ!=this->mCZ)
  {
      os.str("");
      os << this->mFZ;
      stream.writeAttribute("fz", getPrefix(), os.str());
  }
  os.str("");
  if(this->mRadius != dflt)
  {
      os  << this->mRadius;
      stream.writeAttribute("r", getPrefix(), os.str());
  }
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new RadialGradient_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RadialGradient_t *
RadialGradient_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion)
{
  return new RadialGradient(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RadialGradient_t object.
 */
LIBSBML_EXTERN
RadialGradient_t*
RadialGradient_clone(const RadialGradient_t* rg)
{
  if (rg != NULL)
  {
    return static_cast<RadialGradient_t*>(rg->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RadialGradient_t object.
 */
LIBSBML_EXTERN
void
RadialGradient_free(RadialGradient_t* rg)
{
  if (rg != NULL)
  {
    delete rg;
  }
}


/*
 * Returns the value of the "cx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCx(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getCx()));
}


/*
 * Returns the value of the "cy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCy(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getCy()));
}


/*
 * Returns the value of the "cz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCz(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getCz()));
}


/*
 * Returns the value of the "r" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getR(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getR()));
}


/*
 * Returns the value of the "fx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFx(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getFx()));
}


/*
 * Returns the value of the "fy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFy(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getFy()));
}


/*
 * Returns the value of the "fz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFz(const RadialGradient_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getFz()));
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cx" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCx(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetCx()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cy" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCy(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetCy()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cz" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCz(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetCz()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "r" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetR(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetR()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fx" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFx(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFx()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fy" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFy(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFy()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fz" element is
 * set.
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFz(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFz()) : 0;
}


/*
 * Sets the value of the "cx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setCx(RadialGradient_t * rg, const RelAbsVector_t* cx)
{
  return (rg != NULL) ? rg->setCx(*cx) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "cy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setCy(RadialGradient_t * rg, const RelAbsVector_t* cy)
{
  return (rg != NULL) ? rg->setCy(*cy) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "cz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setCz(RadialGradient_t * rg, const RelAbsVector_t* cz)
{
  return (rg != NULL) ? rg->setCz(*cz) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "r" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setR(RadialGradient_t * rg, const RelAbsVector_t* r)
{
  return (rg != NULL) ? rg->setR(*r) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setFx(RadialGradient_t * rg, const RelAbsVector_t* fx)
{
  return (rg != NULL) ? rg->setFx(*fx) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setFy(RadialGradient_t * rg, const RelAbsVector_t* fy)
{
  return (rg != NULL) ? rg->setFy(*fy) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_setFz(RadialGradient_t * rg, const RelAbsVector_t* fz)
{
  return (rg != NULL) ? rg->setFz(*fz) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "cx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCx(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetCx() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "cy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCy(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetCy() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "cz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCz(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetCz() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "r" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetR(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetR() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fx" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFx(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetFx() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fy" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFy(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetFy() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fz" element of this RadialGradient_t.
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFz(RadialGradient_t * rg)
{
  return (rg != NULL) ? rg->unsetFz() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RadialGradient_t object have been set.
 */
LIBSBML_EXTERN
int
RadialGradient_hasRequiredAttributes(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * RadialGradient_t object have been set.
 */
LIBSBML_EXTERN
int
RadialGradient_hasRequiredElements(const RadialGradient_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


