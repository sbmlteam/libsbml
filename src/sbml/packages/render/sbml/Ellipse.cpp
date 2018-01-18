/**
 * @file    Ellipse.cpp
 * @brief   class for ellipse objects.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include "Ellipse.h"

#include <string>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string Ellipse::ELEMENT_NAME="ellipse";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Ellipse object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Ellipse::Ellipse (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : GraphicalPrimitive2D(level, version, pkgVersion)
  , mCX(0.0)
  , mCY(0.0)
  , mCZ(0.0)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Ellipse object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Ellipse::Ellipse (RenderPkgNamespaces* renderns)
  : GraphicalPrimitive2D(renderns)
  , mCX(0.0)
  , mCY(0.0)
  , mCZ(0.0)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


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
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Destroy this object.
 */
Ellipse::~Ellipse ()
{
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new ellipse object with the center set to 0,0,0
 * and the radii also set to 0.
 * The id is set to the given string.
 *
 * @param id the id of the ellipse.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
    Ellipse::Ellipse(RenderPkgNamespaces* renderns, const std::string& id)
 : GraphicalPrimitive2D(renderns,id)
 , mCX(0.0)
 , mCY(0.0)
 , mCZ(0.0)
 , mRatio(util_NaN())
 , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(0.0,0.0);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with 2D center and radius.
 * instantiates a new ellipse object with the center.
 * The z coordinate of the center is set to 0.
 * The id is unset and both radii are set to the given radius.
 *
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param r radius along both axis
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r)
 : GraphicalPrimitive2D(renderns)
  , mCX(cx)
  , mCY(cy)
  , mCZ(RelAbsVector(0.0,50.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(r,r);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with 2D center and radii.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry)
 : GraphicalPrimitive2D(renderns)
 , mCX(cx)
 , mCY(cy)
 , mCZ(0.0)
 , mRatio(util_NaN())
 , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(rx,ry);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with 3D center and radii.
 * instantiates a new ellipse object with the center and radii.
 * The id is unset.
 *
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param cz z value of the center point 
 * @param rx radius along the x axis
 * @param ry radius along the y axis
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry)
 : GraphicalPrimitive2D(renderns)
  , mCX(cx)
  , mCY(cy)
  , mCZ(cz)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(rx,ry);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id, 2D center and radius.
 * instantiates a new ellipse object with the given @p id and center.
 * Both radii are set to the given radius r. This actually yields a circle.
 *
 * @param id id for the ellipse
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param r radius along both axis
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r)
 : GraphicalPrimitive2D(renderns,id)
  , mCX(cx)
  , mCY(cy)
  , mCZ(0.0)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(r,r);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id, 2D center and radii.
 * instantiates a new ellipse object with the given @p id, center and radii.
 *
 * @param id id for the ellipse
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param rx radius along the x axis
 * @param ry radius along the y axis
 *
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry)
 : GraphicalPrimitive2D(renderns,id)
 , mCX(cx)
 , mCY(cy)
 , mCZ(0.0)
 , mRatio(util_NaN())
 , mIsSetRatio(false)

{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(rx,ry);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id, 3D center and radii.
 * instantiates a new ellipse object with the given @p id, center and radii.
 *
 * @param id id for the ellipse
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param cz z value of the center point 
 * @param rx radius along the x axis
 * @param ry radius along the y axis
 *
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Ellipse::Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry)
 : GraphicalPrimitive2D(renderns, id)
 , mCX(cx)
 , mCY(cy)
 , mCZ(cz)
 , mRatio(util_NaN())
 , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Ellipse::Ellipse(const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setRadii(rx,ry);
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
* Returns the value of the "ratio" attribute of this Ellipse.
*/
double
Ellipse::getRatio() const
{
  return mRatio;
}


/*
* Predicate returning @c true if this Ellipse's "ratio" attribute is set.
*/
bool
Ellipse::isSetRatio() const
{
  return mIsSetRatio;
}


/*
* Sets the value of the "ratio" attribute of this Ellipse.
*/
int
Ellipse::setRatio(double ratio)
{
  mRatio = ratio;
  mIsSetRatio = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the value of the "ratio" attribute of this Ellipse.
*/
int
Ellipse::unsetRatio()
{
  mRatio = util_NaN();
  mIsSetRatio = false;

  if (isSetRatio() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the center point as a const reference.
 *
 * @return const reference to the x coordinate of the center point.
 */
const RelAbsVector& Ellipse::getCX() const
{
    return this->mCX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the center point as a const reference.
 *
 * @return const reference to the y coordinate of the center point.
 */
const RelAbsVector& Ellipse::getCY() const
{
    return this->mCY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the center point as a const reference.
 *
 * @return const reference to the z coordinate of the center point.
 */
const RelAbsVector& Ellipse::getCZ() const
{
    return this->mCZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius along the x axis as a const reference.
 *
 * @return const reference to the radius along the x axis
 */
const RelAbsVector& Ellipse::getRX() const
{
    return this->mRX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius along the y axis as a const reference.
 *
 * @return const reference to the radius along the y axis
 */
const RelAbsVector& Ellipse::getRY() const
{
    return this->mRY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the center point as a reference.
 *
 * @return reference to the x coordinate of the center point.
 */
RelAbsVector& Ellipse::getCX()
{
    return this->mCX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the center point as a reference.
 *
 * @return reference to the y coordinate of the center point.
 */
RelAbsVector& Ellipse::getCY()
{
    return this->mCY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the center point as a reference.
 *
 * @return reference to the z coordinate of the center point.
 */
RelAbsVector& Ellipse::getCZ()
{
    return this->mCZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius along the x axis as a reference.
 *
 * @return reference to the radius along the x axis
 */
RelAbsVector& Ellipse::getRX()
{
    return this->mRX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius along the y axis as a reference.
 *
 * @return reference to the radius along the y axis
 */
RelAbsVector& Ellipse::getRY()
{
    return this->mRY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x coordinates for the center point.
 *
 * @param cx x value of the center point 
 */
void Ellipse::setCX(const RelAbsVector& cx)
{
    this->mCX=cx;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y coordinates for the center point.
 *
 * @param cy y value of the center point 
 */
void Ellipse::setCY(const RelAbsVector& cy)
{
    this->mCY=cy;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z coordinates for the center point.
 *
 * @param cz z value of the center point 
 */
void Ellipse::setCZ(const RelAbsVector& cz)
{
    this->mCZ=cz;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the radius along the x axis
 *
 * @param rx radius along the x axis
 */
void Ellipse::setRX(const RelAbsVector& rx)
{
    this->mRX=rx;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the radius along the y axis
 *
 * @param ry radius along the y axis
 */
void Ellipse::setRY(const RelAbsVector& ry)
{
    this->mRY=ry;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D coordinates for the center point.
 * The z coodintate is set to 50%
 *
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 */
void Ellipse::setCenter2D(const RelAbsVector& cx,const RelAbsVector& cy)
{
    this->mCX=cx;
    this->mCY=cy;
    this->mCZ=RelAbsVector(0.0,50.0);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 3D coordinates for the center point.
 *
 * @param cx x value of the center point 
 * @param cy y value of the center point 
 * @param cz z value of the center point 
 */
void Ellipse::setCenter3D(const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz)
{
    this->mCX=cx;
    this->mCY=cy;
    this->mCZ=cz;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the radii of the ellipse
 *
 * @param rx radius along the x axis
 * @param ry radius along the y axis
 */
void Ellipse::setRadii(const RelAbsVector& rx,const RelAbsVector& ry)
{
    this->mRX=rx;
    this->mRY=ry;
}
/** @endcond */


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
void Ellipse::writeElements (XMLOutputStream& stream) const
{
    GraphicalPrimitive2D::writeElements(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this Ellipse object.
 * 
 * @return a (deep) copy of this Ellipse object
 */
Ellipse* 
Ellipse::clone () const
{
    return new Ellipse(*this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
    SBase*
Ellipse::createObject (XMLInputStream& stream)
{
    SBase*        object = NULL;

    return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Ellipse::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("cx");
  attributes.add("cy");
  attributes.add("cz");
  attributes.add("rx");
  attributes.add("ry");
  attributes.add("rz");
  attributes.add("ratio");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Ellipse::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
      ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->GraphicalPrimitive2D::readAttributes(attributes, ea);
    std::string s;
    RelAbsVector v=RelAbsVector();
    attributes.readInto("cx", s,this->getErrorLog(),true, getLine(), getColumn());
    v.setCoordinate(s);
    this->setCX(v);
    s="";
    attributes.readInto("cy", s,this->getErrorLog(),true, getLine(), getColumn());
    v.setCoordinate(s);
    this->setCY(v);
    s="";
    if(!attributes.readInto("cz", s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mCZ=RelAbsVector(0.0,0.0);
    }
    else
    {
        v.setCoordinate(s);
        this->mCZ=v;
    }
    s="";
    bool xSet=false;
    if(attributes.readInto("rx", s,this->getErrorLog(),true, getLine(), getColumn()))
    {
        v.setCoordinate(s);
        this->setRX(v);
        xSet=true;
    }
    s="";
    if(!attributes.readInto("ry", s, getErrorLog(), false, getLine(), getColumn()))
    {
        if(xSet)
        {
            this->mRY=this->mRX;
        }
        else
        {
            this->mRY=RelAbsVector(0.0,0.0);
        }
    }
    else
    {
        v.setCoordinate(s);
        this->mRY=v;
    }
    // TODO actually check if RY has been set, otherwise throw an error
    // because either rx or ry has to be set.
    if(!xSet)
    {
        this->mRX=this->mRY;
    }

    // 
    // ratio double (use = "optional" )
    // 
    mIsSetRatio = attributes.readInto("ratio", mRatio);

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
void Ellipse::writeAttributes (XMLOutputStream& stream) const
{
    GraphicalPrimitive2D::writeAttributes(stream);
    std::ostringstream os;
    os << mCX;
    stream.writeAttribute("cx",getPrefix(), os.str());
    os.str("");
    os << mCY;
    stream.writeAttribute("cy",getPrefix(), os.str());
    if(this->mCZ!=RelAbsVector(0.0,0.0))
    {
        os.str("");  
        os << mCZ;
        stream.writeAttribute("cz",getPrefix(), os.str());
    }
    os.str("");
    os << mRX;
    stream.writeAttribute("rx",getPrefix(), os.str());
    if(this->mRY!=this->mRX)
    {
        os.str("");  
        os << mRY;
        stream.writeAttribute("ry",getPrefix(), os.str());
    }

    if (isSetRatio() == true)
    {
      stream.writeAttribute("ratio", getPrefix(), mRatio);
    }


}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Ellipse object.
 *
 * @return the XMLNode with the XML representation for the 
 * Ellipse object.
 */
XMLNode Ellipse::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for this %SBML object.
 * 
 * @if clike LibSBML attaches an identifying code to every
 * kind of SBML object.  These are known as <em>SBML type codes</em>.
 * The set of possible type codes is defined in the enumeration
 * #SBMLTypeCode_t.  The names of the type codes all begin with the
 * characters @c SBML_. @endif@if java LibSBML attaches an
 * identifying code to every kind of SBML object.  These are known as
 * <em>SBML type codes</em>.  In other languages, the set of type codes
 * is stored in an enumeration; in the Java language interface for
 * libSBML, the type codes are defined as static integer constants in
 * interface class {@link libsbmlConstants}.  The names of the type codes
 * all begin with the characters @c SBML_. @endif
 *
 * @return the SBML type code for this object, or @c SBML_UNKNOWN (default).
 *
 * @see getElementName()
 */
int
Ellipse::getTypeCode () const
{
    return SBML_RENDER_ELLIPSE;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Ellipse defines it as returning "ellipse",
 */
const std::string& Ellipse::getElementName() const
{
  static std::string name = Ellipse::ELEMENT_NAME;
  return name;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the SBML object's next
 * sibling object (if available).
 */
bool Ellipse::accept (SBMLVisitor& v) const
{
    //v.visit(*this);
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool Ellipse::hasRequiredAttributes() const
{
    bool result = this->GraphicalPrimitive2D::hasRequiredAttributes();
    // the center should not contain NaN
    result = result && 
        (this->mCX.getAbsoluteValue() == this->mCX.getAbsoluteValue()) &&
        (this->mCX.getRelativeValue() == this->mCX.getRelativeValue());
    result = result && 
        (this->mCY.getAbsoluteValue() == this->mCY.getAbsoluteValue()) &&
        (this->mCY.getRelativeValue() == this->mCY.getRelativeValue());
    result = result && 
        (this->mCZ.getAbsoluteValue() == this->mCZ.getAbsoluteValue()) &&
        (this->mCZ.getRelativeValue() == this->mCZ.getRelativeValue());
    // the radii should not contain NaN
    result = result && 
        (this->mRX.getAbsoluteValue() == this->mRX.getAbsoluteValue()) &&
        (this->mRX.getRelativeValue() == this->mRX.getRelativeValue());
    result = result && 
        (this->mRY.getAbsoluteValue() == this->mRY.getAbsoluteValue()) &&
        (this->mRY.getRelativeValue() == this->mRY.getRelativeValue());
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool Ellipse::hasRequiredElements() const 
{
    bool result = this->GraphicalPrimitive2D::hasRequiredElements();
    return result;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END
