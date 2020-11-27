/**
 * @file    RenderPoint.cpp
 * @brief Implementation of the RenderPoint class.
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

#include <sstream>
#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/ListOfCurveElements.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/RenderCubicBezier.h>


using namespace std;

#include <limits>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new RenderPoint using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RenderPoint::RenderPoint(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : SBase(level, version)
    ,mXOffset(RelAbsVector(0.0,0.0))
    ,mYOffset(RelAbsVector(0.0,0.0))
    ,mZOffset(RelAbsVector(0.0,0.0))
    ,mElementName("element")
{
  RenderPkgNamespaces* renderns = new RenderPkgNamespaces(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(renderns);

  connectToChild();

  loadPlugins(renderns);
}


/*
 * Creates a new RenderPoint using the given RenderPkgNamespaces object.
 */
RenderPoint::RenderPoint(RenderPkgNamespaces *renderns)
  : SBase(renderns)
    ,mXOffset(RelAbsVector(0.0,0.0))
    ,mYOffset(RelAbsVector(0.0,0.0))
    ,mZOffset(RelAbsVector(0.0,0.0))
    ,mElementName("element")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/*
 * Creates a new point with the given coordinates.
 *
 * @param x x coordinate of the RenderPoint object
 * @param y y coordinate of the RenderPoint object
 * @param z z coordinate of the RenderPoint object
 * If the z value is omitted, it is set to 0.
 */ 
RenderPoint::RenderPoint(RenderPkgNamespaces* renderns, const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z) :
    SBase  (renderns)
    , mXOffset(x)
    , mYOffset(y)
    , mZOffset(z)
    , mElementName("element")  
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/*
 * Creates a new RenderPoint object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderPoint object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the RenderPoint
 * object to be instantiated.
 */
RenderPoint::RenderPoint(const XMLNode& node, unsigned int l2version) : SBase(2, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(node);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(node);
        }
        else
        {
            //throw;
        }
        ++n;
    }    

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/*
 * Copy constructor for RenderPoint.
 */
RenderPoint::RenderPoint(const RenderPoint& orig)
  : SBase( orig )
{
    this->mXOffset=orig.mXOffset;
    this->mYOffset=orig.mYOffset;
    this->mZOffset=orig.mZOffset;
    this->mElementName=orig.mElementName;
}


/*
 * Assignment operator for RenderPoint.
 */
RenderPoint&
RenderPoint::operator=(const RenderPoint& rhs)
{
    if(&rhs!=this)
    {
        this->SBase::operator=(rhs);
        this->mXOffset= rhs.mXOffset;
        this->mYOffset= rhs.mYOffset;
        this->mZOffset= rhs.mZOffset;
        this->mElementName= rhs.mElementName;
    }
    return *this;
}


/*
* Comparison operator for RenderPoint objects.
*/
bool RenderPoint::operator==(const RenderPoint& left) const
{
  return (this->mXOffset == left.mXOffset 
    && this->mYOffset == left.mYOffset 
    && this->mZOffset == left.mZOffset);
}


/*
 * Creates and returns a deep copy of this RenderPoint object.
 */
RenderPoint*
RenderPoint::clone() const
{
  return new RenderPoint(*this);
}


/*
 * Destructor for RenderPoint.
 */
RenderPoint::~RenderPoint()
{
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the Z offset to 0.0.
 */
void RenderPoint::initDefaults ()
{
    this->setZ(0.0);
}
/** @endcond */


/*
* Returns the value of the "x" element of this RenderPoint.
*/
const RelAbsVector&
RenderPoint::getX() const
{
  return this->mXOffset;
}


/*
* Returns the value of the "x" element of this RenderPoint.
*/
RelAbsVector&
RenderPoint::getX()
{
  return this->mXOffset;
}


/*
 * Returns the value of the "x" element of this RenderPoint.
 */
const RelAbsVector&
RenderPoint::x() const
{
  return this->mXOffset;
}


/*
 * Returns the value of the "x" element of this RenderPoint.
 */
RelAbsVector&
RenderPoint::x()
{
  return this->mXOffset;
}


/*
* Returns the value of the "y" element of this RenderPoint.
*/
const RelAbsVector&
RenderPoint::getY() const
{
  return this->mYOffset;
}


/*
* Returns the value of the "y" element of this RenderPoint.
*/
RelAbsVector&
RenderPoint::getY()
{
  return this->mYOffset;
}


/*
 * Returns the value of the "y" element of this RenderPoint.
*/
const RelAbsVector&
RenderPoint::y() const
{
  return this->mYOffset;
}


/*
 * Returns the value of the "y" element of this RenderPoint.
*/
RelAbsVector&
RenderPoint::y()
{
  return this->mYOffset;
}


/*
* Returns the value of the "z" element of this RenderPoint.
*/
const RelAbsVector&
RenderPoint::getZ() const
{
  return this->mZOffset;
}

/*
* Returns the value of the "z" element of this RenderPoint.
*/
RelAbsVector&
RenderPoint::getZ()
{
  return this->mZOffset;
}


/*
 * Returns the value of the "z" element of this RenderPoint.
*/
const RelAbsVector&
RenderPoint::z() const
{
  return this->mZOffset;
}

/*
 * Returns the value of the "z" element of this RenderPoint.
*/
RelAbsVector&
RenderPoint::z()
{
  return this->mZOffset;
}


/*
 * Predicate returning @c true if this RenderPoint's "x" element is set.
 */
bool
RenderPoint::isSetX() const
{
  return mXOffset.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderPoint's "y" element is set.
 */
bool
RenderPoint::isSetY() const
{
  return mYOffset.isSetCoordinate();
}


/*
 * Predicate returning @c true if this RenderPoint's "z" element is set.
 */
bool
RenderPoint::isSetZ() const
{
  return mZOffset.isSetCoordinate();
}


/*
 * Sets the value of the "x" element of this RenderPoint.
 */
int
RenderPoint::setX(const RelAbsVector& x)
{
  this->mXOffset = x;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "y" element of this RenderPoint.
 */
int
RenderPoint::setY(const RelAbsVector& y)
{
  this->mYOffset = y;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "z" element of this RenderPoint.
 */
int
RenderPoint::setZ(const RelAbsVector& z)
{
  this->mZOffset = z;
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates of the RenderPoint to the given values.
 *
 * @param x x coordinate to be set.
 * @param y y coordinate to be set.
 * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
 */ 
    void
RenderPoint::setCoordinates (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
    this->setX(x);
    this->setY(y);
    this->setZ(z);
}
/** @endcond */

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates of the RenderPoint to the given values.
 * This method is deprecated, please use setCoordinates. 
 *
 * @param x x coordinate to be set.
 * @param y y coordinate to be set.
 * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
 */ 
    void
RenderPoint::setOffsets (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. \"void RenderPoint::setOffsets (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    this->setCoordinates(x,y,z);
}
/** @endcond */
#endif // OMIT_DEPRECATED


/*
 * Unsets the value of the "x" element of this RenderPoint.
 */
int
RenderPoint::unsetX()
{
  mXOffset.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "y" element of this RenderPoint.
 */
int
RenderPoint::unsetY()
{
  mYOffset.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "z" element of this RenderPoint.
 */
int
RenderPoint::unsetZ()
{
  mZOffset.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract RenderPoint is of type
 * RenderPoint
 */
bool
RenderPoint::isRenderPoint() const
{
  return dynamic_cast<const RenderPoint*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract RenderPoint is of type
 * RenderCubicBezier
 */
bool
RenderPoint::isRenderCubicBezier() const
{
  return dynamic_cast<const RenderCubicBezier*>(this) != NULL;
}


/*
 * Returns the XML element name of this RenderPoint object.
 */
const std::string&
RenderPoint::getElementName() const
{
  static std::string name = "element";
  return name;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this RenderPoint object.
 */
void
RenderPoint::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this RenderPoint object.
 */
int
RenderPoint::getTypeCode() const
{
  return SBML_RENDER_POINT;
}


/* function returns true if component has all the required
* attributes
*/
bool 
RenderPoint::hasRequiredAttributes() const
{
  bool result = this->SBase::hasRequiredAttributes();
  // the offsets should not be NaN
  if (!isSetX())
  {
    result = false;
  }

  if (!isSetY())
  {
    result = false;
  }

  // z must not be nan
  result = result &&
    (this->mZOffset.getAbsoluteValue() == this->mZOffset.getAbsoluteValue()) &&
    (this->mZOffset.getRelativeValue() == this->mZOffset.getRelativeValue());
  return result;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.  For example:
 *
 *   SBase::writeElements(stream);
 *   mReactants.write(stream);
 *   mProducts.write(stream);
 *   ...
 */
void RenderPoint::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
RenderPoint::accept(SBMLVisitor& v) const
{
  //v.visit(*this);
  return false;
}

/** @endcond */


XMLNode RenderPoint::toXML(const std::string& name) const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */
void
RenderPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("xsi:type");
  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void RenderPoint::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
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

    for (int n = numErrs - 1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderRenderPointAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRenderPointAllowedCoreAttributes, pkgVersion, level, version,
          details, getLine(), getColumn());
      }
    }
  }

  string elplusid = "<renderPoint> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }

  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // x RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("x", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    if (log)
    {
      std::string message = "The required attribute 'x' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRenderPointAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    setX(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()));
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'x' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderEllipseCxMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      setX(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()));
    }
    else
    {
      this->setX(v);
    }
    v.erase();
  }
  //
  // y RelAbsVector (use = required) 
  //
  s = "";
  assigned = attributes.readInto("y", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    if (log)
    {
      std::string message = "The required attribute 'y' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRenderPointAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    setY(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()));
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'y' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderEllipseCyMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
      setY(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()));
    }
    else
    {
      this->setY(v);
    }
    v.erase();
  }

  //
  // z RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("z", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mZOffset = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'c' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderEllipseCzMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setZ(v);
    }
    v.erase();
  }

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderPoint::writeAttributes (XMLOutputStream& stream) const
{
    SBase::writeAttributes(stream);
    XMLTriple triple("type","","xsi");
    stream.writeAttribute(triple,std::string("RenderPoint"));
    std::ostringstream os;
    os << mXOffset;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << mYOffset;
    stream.writeAttribute("y", getPrefix(), os.str());
    if(this->mZOffset!=RelAbsVector(0.0,0.0))
    {
        os.str("");  
        os << mZOffset;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
RenderPoint::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  xmlns.add(LayoutExtension::getXmlnsXSI(), "xsi");
  stream << xmlns;
}
/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new RenderPoint (RenderPoint_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderPoint_t *
RenderPoint_createRenderPoint(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion)
{
  return new RenderPoint(level, version, pkgVersion);
}


/*
 * Creates a new RenderCubicBezier (RenderPoint_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderPoint_t *
RenderPoint_createRenderCubicBezier(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion)
{
  return new RenderCubicBezier(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RenderPoint_t object.
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderPoint_clone(const RenderPoint_t* rp)
{
  if (rp != NULL)
  {
    return static_cast<RenderPoint_t*>(rp->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RenderPoint_t object.
 */
LIBSBML_EXTERN
void
RenderPoint_free(RenderPoint_t* rp)
{
  if (rp != NULL)
  {
    delete rp;
  }
}


/*
 * Returns the value of the "x" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getX(const RenderPoint_t * rp)
{
  if (rp == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rp->getX()));
}


/*
 * Returns the value of the "y" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getY(const RenderPoint_t * rp)
{
  if (rp == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rp->getY()));
}


/*
 * Returns the value of the "z" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getZ(const RenderPoint_t * rp)
{
  if (rp == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rp->getZ()));
}


/*
 * Predicate returning @c 1 (true) if this RenderPoint_t's "x" element is set.
 */
LIBSBML_EXTERN
int
RenderPoint_isSetX(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->isSetX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderPoint_t's "y" element is set.
 */
LIBSBML_EXTERN
int
RenderPoint_isSetY(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->isSetY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderPoint_t's "z" element is set.
 */
LIBSBML_EXTERN
int
RenderPoint_isSetZ(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->isSetZ()) : 0;
}


/*
 * Sets the value of the "x" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_setX(RenderPoint_t * rp, const RelAbsVector_t* x)
{
  return (rp != NULL) ? rp->setX(*x) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_setY(RenderPoint_t * rp, const RelAbsVector_t* y)
{
  return (rp != NULL) ? rp->setY(*y) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_setZ(RenderPoint_t * rp, const RelAbsVector_t* z)
{
  return (rp != NULL) ? rp->setZ(*z) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_unsetX(RenderPoint_t * rp)
{
  return (rp != NULL) ? rp->unsetX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_unsetY(RenderPoint_t * rp)
{
  return (rp != NULL) ? rp->unsetY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z" element of this RenderPoint_t.
 */
LIBSBML_EXTERN
int
RenderPoint_unsetZ(RenderPoint_t * rp)
{
  return (rp != NULL) ? rp->unsetZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this RenderPoint_t is of type RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isRenderPoint(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->isRenderPoint()) : 0;
}


/*
 * Predicate returning @c 1 if this RenderPoint_t is of type
 * RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderPoint_isRenderCubicBezier(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->isRenderCubicBezier()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * RenderPoint_t object have been set.
 */
LIBSBML_EXTERN
int
RenderPoint_hasRequiredAttributes(const RenderPoint_t * rp)
{
  return (rp != NULL) ? static_cast<int>(rp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


