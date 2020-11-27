/**
 * @file    LinearGradient.cpp
 * @brief Implementation of the LinearGradient class.
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

#include <sbml/packages/render/sbml/LinearGradient.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>


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




#ifdef __cplusplus


/*
 * Creates a new LinearGradient using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LinearGradient::LinearGradient(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
:    GradientBase(level,version, pkgVersion)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
    ,mZ2(RelAbsVector(0.0,100.0))
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new LinearGradient using the given RenderPkgNamespaces object.
 */
LinearGradient::LinearGradient(RenderPkgNamespaces *renderns)
:    GradientBase(renderns)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
    ,mZ2(RelAbsVector(0.0,100.0))
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LinearGradient object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LinearGradient object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the LinearGradient
 * object to be instantiated.
 */
LinearGradient::LinearGradient(const XMLNode& node, unsigned int l2version):GradientBase(node, l2version)
{
   ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LinearGradient with no gradient stops.
 * The id is set to the given value.
 * The LinearGradient object is invalid until it has an id and at least two 
 * gradient stops.
 * The start and the end of the linear gradient vector are set to (0,0,0).
 * A linear gradient with a vector of length zero should also be considered invalid.
 *
 * @param id the new id for the LinearGradient.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
    LinearGradient::LinearGradient(RenderPkgNamespaces* renderns, const std::string& id)
    :GradientBase(renderns, id)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
     ,mZ2(RelAbsVector(0.0,100.0))
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LinearGradient::LinearGradient(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED


/*
 * Copy constructor for LinearGradient.
 */
LinearGradient::LinearGradient(const LinearGradient& orig)
  : GradientBase( orig )
  , mX1 ( orig.mX1 )
  , mY1 ( orig.mY1 )
  , mZ1 ( orig.mZ1 )
  , mX2 ( orig.mX2 )
  , mY2 ( orig.mY2 )
  , mZ2 ( orig.mZ2 )
{
  connectToChild();
}


/*
 * Assignment operator for LinearGradient.
 */
LinearGradient&
LinearGradient::operator=(const LinearGradient& rhs)
{
  if (&rhs != this)
  {
    GradientBase::operator=(rhs);
    mX1 = rhs.mX1;
    mY1 = rhs.mY1;
    mZ1 = rhs.mZ1;
    mX2 = rhs.mX2;
    mY2 = rhs.mY2;
    mZ2 = rhs.mZ2;

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this LinearGradient object.
 */
LinearGradient*
LinearGradient::clone() const
{
  return new LinearGradient(*this);
}


/*
 * Destructor for LinearGradient.
 */
LinearGradient::~LinearGradient()
{
}


/*
 * Returns the value of the "x1" element of this LinearGradient.
 */
const RelAbsVector&
LinearGradient::getX1() const
{
  return this->mX1;
}


/*
 * Returns the value of the "x1" element of this LinearGradient.
 */
RelAbsVector&
LinearGradient::getX1()
{
  return this->mX1;
}


/*
 * Returns the x coordinate for the start point as a const reference.
 */
const RelAbsVector& 
LinearGradient::getXPoint1() const
{
    return this->mX1;
}


RelAbsVector&
LinearGradient::getXPoint1()
{
  return this->mX1;
}


/*
* Returns the value of the "y1" element of this LinearGradient.
*/
const RelAbsVector&
LinearGradient::getY1() const
{
  return this->mY1;
}


/*
* Returns the value of the "y1" element of this LinearGradient.
*/
RelAbsVector&
LinearGradient::getY1()
{
  return this->mY1;
}


/*
* Returns the y coordinate for the start point as a const reference.
*/
const RelAbsVector&
LinearGradient::getYPoint1() const
{
  return this->mY1;
}


RelAbsVector&
LinearGradient::getYPoint1()
{
  return this->mY1;
}


/*
* Returns the value of the "z1" element of this LinearGradient.
*/
const RelAbsVector&
LinearGradient::getZ1() const
{
  return this->mZ1;
}


/*
* Returns the value of the "z1" element of this LinearGradient.
*/
RelAbsVector&
LinearGradient::getZ1()
{
  return this->mZ1;
}


/*
* Returns the z coordinate for the start point as a const reference.
*/
const RelAbsVector&
LinearGradient::getZPoint1() const
{
  return this->mZ1;
}


RelAbsVector&
LinearGradient::getZPoint1()
{
  return this->mZ1;
}


/*
* Returns the value of the "x2" element of this LinearGradient.
*/
const RelAbsVector&
LinearGradient::getX2() const
{
  return this->mX2;
}


/*
* Returns the value of the "x2" element of this LinearGradient.
*/
RelAbsVector&
LinearGradient::getX2()
{
  return this->mX2;
}


/*
* Returns the x coordinate for the start point as a const reference.
*/
const RelAbsVector&
LinearGradient::getXPoint2() const
{
  return this->mX2;
}

RelAbsVector&
LinearGradient::getXPoint2()
{
  return this->mX2;
}

/*
* Returns the value of the "y2" element of this LinearGradient.
*/
const RelAbsVector&
LinearGradient::getY2() const
{
  return this->mY2;
}


/*
* Returns the value of the "y2" element of this LinearGradient.
*/
RelAbsVector&
LinearGradient::getY2()
{
  return this->mY2;
}


/*
* Returns the y coordinate for the start point as a const reference.
*/
const RelAbsVector&
LinearGradient::getYPoint2() const
{
  return this->mY2;
}


RelAbsVector&
LinearGradient::getYPoint2()
{
  return this->mY2;
}

/*
* Returns the value of the "z2" element of this LinearGradient.
*/
const RelAbsVector&
LinearGradient::getZ2() const
{
  return this->mZ2;
}


/*
* Returns the value of the "z2" element of this LinearGradient.
*/
RelAbsVector&
LinearGradient::getZ2()
{
  return this->mZ2;
}


/*
* Returns the z coordinate for the start point as a const reference.
*/
const RelAbsVector&
LinearGradient::getZPoint2() const
{
  return this->mZ2;
}


RelAbsVector&
LinearGradient::getZPoint2()
{
  return this->mZ2;
}


/*
 * Predicate returning @c true if this LinearGradient's "x1" element is set.
 */
bool
LinearGradient::isSetX1() const
{
  return mX1.isSetCoordinate();
}


/*
 * Predicate returning @c true if this LinearGradient's "y1" element is set.
 */
bool
LinearGradient::isSetY1() const
{
  return mY1.isSetCoordinate();
}


/*
 * Predicate returning @c true if this LinearGradient's "z1" element is set.
 */
bool
LinearGradient::isSetZ1() const
{
  return mZ1.isSetCoordinate();
}


/*
 * Predicate returning @c true if this LinearGradient's "x2" element is set.
 */
bool
LinearGradient::isSetX2() const
{
  return mX2.isSetCoordinate();
}


/*
 * Predicate returning @c true if this LinearGradient's "y2" element is set.
 */
bool
LinearGradient::isSetY2() const
{
  return mY2.isSetCoordinate();
}


/*
 * Predicate returning @c true if this LinearGradient's "z2" element is set.
 */
bool
LinearGradient::isSetZ2() const
{
  return mZ2.isSetCoordinate();
}


/*
 * Sets the value of the "x1" element of this LinearGradient.
 */
int
LinearGradient::setX1(const RelAbsVector& x1)
{
  this->mX1 = x1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "y1" element of this LinearGradient.
*/
int
LinearGradient::setY1(const RelAbsVector& y1)
{
  this->mY1 = y1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "z1" element of this LinearGradient.
*/
int
LinearGradient::setZ1(const RelAbsVector& z1)
{
  this->mZ1 = z1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "x2" element of this LinearGradient.
*/
int
LinearGradient::setX2(const RelAbsVector& x2)
{
  this->mX2 = x2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "y2" element of this LinearGradient.
*/
int
LinearGradient::setY2(const RelAbsVector& y2)
{
  this->mY2 = y2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "z2" element of this LinearGradient.
*/
int
LinearGradient::setZ2(const RelAbsVector& z2)
{
  this->mZ2 = z2;
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 3D coordinates for the start and the end point of the linear gradient vector.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x1 x value of the start point of the linear gradient vector
 * @param y1 y value of the start point of the linear gradient vector
 * @param z1 z value of the start point of the linear gradient vector
 * @param x2 x value of the end point of the linear gradient vector
 * @param y2 y value of the end point of the linear gradient vector
 * @param z2 z value of the end point of the linear gradient vector
 */
void LinearGradient::setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& z1
        ,const RelAbsVector& x2,const RelAbsVector& y2,const RelAbsVector& z2)
{
    this->setPoint1(x1,y1,z1);
    this->setPoint2(x2,y2,z2);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D coordinates for the start and the end point of the linear gradient vector.
 * The z values are automatically set to 0.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x1 x value of the start point of the linear gradient vector
 * @param y1 y value of the start point of the linear gradient vector
 * @param x2 x value of the end point of the linear gradient vector
 * @param y2 y value of the end point of the linear gradient vector
 */
void LinearGradient::setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& x2,const RelAbsVector& y2)
{
    this->setCoordinates(x1,y1,RelAbsVector(0.0,0.0),x2,y2,RelAbsVector(0.0,100.0));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the start point of the linear gradient vector.
 *
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * The z value can be omitted. In that case it is set to 0.
 *
 * @param x x value of the start point of the linear gradient vector
 * @param y y value of the start point of the linear gradient vector
 * @param z z value of the start point of the linear gradient vector
 *
 */
void LinearGradient::setPoint1(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX1=x;
    this->mY1=y;
    this->mZ1=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the end point of the linear gradient vector.
 *
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * The z value can be omitted. In that case it is set to 0.
 *
 * @param x x value of the end point of the linear gradient vector
 * @param y y value of the end point of the linear gradient vector
 * @param z z value of the end point of the linear gradient vector
 *
 */
void LinearGradient::setPoint2(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX2=x;
    this->mY2=y;
    this->mZ2=z;
}
/** @endcond */

/*
 * Unsets the value of the "x1" element of this LinearGradient.
 */
int
LinearGradient::unsetX1()
{
  mX1.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "y1" element of this LinearGradient.
*/
int
LinearGradient::unsetY1()
{
  mY1.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "z1" element of this LinearGradient.
*/
int
LinearGradient::unsetZ1()
{
  mZ1.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "x2" element of this LinearGradient.
*/
int
LinearGradient::unsetX2()
{
  mX2.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "y2" element of this LinearGradient.
*/
int
LinearGradient::unsetY2()
{
  mY2.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "z2" element of this LinearGradient.
*/
int
LinearGradient::unsetZ2()
{
  mZ2.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this LinearGradient object.
 */
const std::string&
LinearGradient::getElementName() const
{
  static const string name = "linearGradient";
  return name;
}


/*
 * Returns the libSBML type code for this LinearGradient object.
 */
int
LinearGradient::getTypeCode() const
{
  return SBML_RENDER_LINEARGRADIENT;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
LinearGradient::accept(SBMLVisitor& v) const
{
  v.visit(*this);
  v.leave(*this);
  return true;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Creates an XMLNode object from this LinearGradient object.
*/
XMLNode LinearGradient::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
LinearGradient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GradientBase::addExpectedAttributes(attributes);

  attributes.add("x1");
  attributes.add("y1");
  attributes.add("z1");
  attributes.add("x2");
  attributes.add("y2");
  attributes.add("z2");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
LinearGradient::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("render", RenderLinearGradientAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderLinearGradientAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  string elplusid = "<linearGradient> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }

  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // x1 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("x1", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mX1 = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientX1MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setX1(v);
    }
    v.erase();
  }


  //
  // y1 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("y1", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mY1 = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientY1MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setY1(v);
    }
    v.erase();
  }


  //
  // z1 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("z1", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mZ1 = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z1' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientZ1MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setZ1(v);
    }
    v.erase();
  }


  //
  // x2 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("x2", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mX2 = RelAbsVector(0.0, 100.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientX2MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setX2(v);
    }
    v.erase();
  }


  //
  // y2 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("y2", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mY2 = RelAbsVector(0.0, 100.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientY2MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setY2(v);
    }
    v.erase();
  }


  //
  // z2 RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("z2", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mZ2 = RelAbsVector(0.0, 100.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z2' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderLinearGradientZ2MustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setZ2(v);
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
void LinearGradient::writeAttributes (XMLOutputStream& stream) const
{
    GradientBase::writeAttributes(stream);
    std::ostringstream os;
    RelAbsVector tmp(0.0,0.0);
    if(this->mX1 != tmp)
    {
        os << this->mX1;
        stream.writeAttribute("x1", getPrefix(), os.str());
    }
    os.str("");
    if(this->mY1 != tmp)
    {
        os << this->mY1;
        stream.writeAttribute("y1", getPrefix(), os.str());
    }
    if(this->mZ1!=tmp)
    {
        os.str("");
        os << this->mZ1;
        stream.writeAttribute("z1", getPrefix(), os.str());
    }
    tmp=RelAbsVector(0.0,100.0);
    os.str("");
    if(this->mX2 != tmp)
    {
        os << this->mX2;
        stream.writeAttribute("x2", getPrefix(), os.str());
    }
    os.str("");
    if(this->mY2 != tmp)
    {
        os << this->mY2;
        stream.writeAttribute("y2", getPrefix(), os.str());
    }
    if(this->mZ2!=tmp)
    {
        os.str("");
        os << this->mZ2;
        stream.writeAttribute("z2", getPrefix(), os.str());
    }
}
/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new LinearGradient_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
LinearGradient_t *
LinearGradient_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion)
{
  return new LinearGradient(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this LinearGradient_t object.
 */
LIBSBML_EXTERN
LinearGradient_t*
LinearGradient_clone(const LinearGradient_t* lg)
{
  if (lg != NULL)
  {
    return static_cast<LinearGradient_t*>(lg->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this LinearGradient_t object.
 */
LIBSBML_EXTERN
void
LinearGradient_free(LinearGradient_t* lg)
{
  if (lg != NULL)
  {
    delete lg;
  }
}


/*
 * Returns the value of the "x1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getX1(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getX1()));
}


/*
 * Returns the value of the "y1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getY1(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getY1()));
}


/*
 * Returns the value of the "z1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getZ1(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getZ1()));
}


/*
 * Returns the value of the "x2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getX2(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getX2()));
}


/*
 * Returns the value of the "y2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getY2(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getY2()));
}


/*
 * Returns the value of the "z2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getZ2(const LinearGradient_t * lg)
{
  if (lg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(lg->getZ2()));
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "x1" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetX1(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetX1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "y1" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetY1(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetY1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "z1" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetZ1(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetZ1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "x2" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetX2(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetX2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "y2" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetY2(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetY2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LinearGradient_t's "z2" element is
 * set.
 */
LIBSBML_EXTERN
int
LinearGradient_isSetZ2(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->isSetZ2()) : 0;
}


/*
 * Sets the value of the "x1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setX1(LinearGradient_t * lg, const RelAbsVector_t* x1)
{
  return (lg != NULL) ? lg->setX1(*x1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setY1(LinearGradient_t * lg, const RelAbsVector_t* y1)
{
  return (lg != NULL) ? lg->setY1(*y1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setZ1(LinearGradient_t * lg, const RelAbsVector_t* z1)
{
  return (lg != NULL) ? lg->setZ1(*z1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "x2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setX2(LinearGradient_t * lg, const RelAbsVector_t* x2)
{
  return (lg != NULL) ? lg->setX2(*x2) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setY2(LinearGradient_t * lg, const RelAbsVector_t* y2)
{
  return (lg != NULL) ? lg->setY2(*y2) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_setZ2(LinearGradient_t * lg, const RelAbsVector_t* z2)
{
  return (lg != NULL) ? lg->setZ2(*z2) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetX1(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetX1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetY1(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetY1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z1" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetZ1(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetZ1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetX2(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetX2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetY2(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetY2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z2" element of this LinearGradient_t.
 */
LIBSBML_EXTERN
int
LinearGradient_unsetZ2(LinearGradient_t * lg)
{
  return (lg != NULL) ? lg->unsetZ2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LinearGradient_t object have been set.
 */
LIBSBML_EXTERN
int
LinearGradient_hasRequiredAttributes(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * LinearGradient_t object have been set.
 */
LIBSBML_EXTERN
int
LinearGradient_hasRequiredElements(const LinearGradient_t * lg)
{
  return (lg != NULL) ? static_cast<int>(lg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


