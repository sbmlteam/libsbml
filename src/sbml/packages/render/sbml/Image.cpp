/**
 * @file    Image.cpp
 * @brief Implementation of the Image class.
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

#include <sbml/packages/render/sbml/Image.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED



using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Image using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Image::Image(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
  :  Transformation2D(level, version, pkgVersion)
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
    ,mHeight(RelAbsVector(0.0,0.0))
    ,mHref("")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}
/** @endcond */


/*
 * Creates a new Image using the given RenderPkgNamespaces object.
 */
Image::Image(RenderPkgNamespaces *renderns)
  : Transformation2D(renderns)
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
    ,mHeight(RelAbsVector(0.0,0.0))
    ,mHref("")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Image object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Image object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Image
 * object to be instantiated.
 */
Image::Image(const XMLNode& node, unsigned int l2version):Transformation2D(node, l2version)
////    ,mId("")
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
                                  ,mHeight(RelAbsVector(0.0,0.0))
                                  ,mHref("")
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
 * Instantiates an Image object with the given @p id.
 * The image reference is unset, the position and the dimensions
 * values of the image are set to 0.
 *
 * For the image to be valid, the reference has to be set and it has to 
 * have dimensions different from and larger than 0.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
    Image::Image(RenderPkgNamespaces* renderns, const std::string& id)
    :Transformation2D(renderns)
//    ,mId(id)
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
     ,mHeight(RelAbsVector(0.0,0.0))
     ,mHref("")
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Image::Image(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
  setId(id);

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
 * Copy constructor for Image.
 */
Image::Image(const Image& orig)
  : Transformation2D( orig )
  , mX ( orig.mX )
  , mY ( orig.mY )
  , mZ ( orig.mZ )
  , mWidth ( orig.mWidth )
  , mHeight ( orig.mHeight )
  , mHref ( orig.mHref )
{
  connectToChild();
}


/*
 * Assignment operator for Image.
 */
Image&
Image::operator=(const Image& rhs)
{
  if (&rhs != this)
  {
    Transformation2D::operator=(rhs);
    mHref = rhs.mHref;
    mX = rhs.mX;
    mY = rhs.mY;
    mZ = rhs.mZ;
    mWidth = rhs.mWidth;
    mHeight = rhs.mHeight;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Image object.
 */
Image*
Image::clone() const
{
  return new Image(*this);
}


/*
 * Destructor for Image.
 */
Image::~Image()
{
}


/*
 * Returns the value of the "id" attribute of this Image.
 */
const std::string&
Image::getId() const
{
  return mId;
}


/*
 * Returns the value of the "href" attribute of this Image.
 */
const std::string&
Image::getHref() const
{
  return mHref;
}


/*
 * Returns the value of the "href" attribute of this Image.
 */
const std::string&
Image::getImageReference() const
{
  return mHref;
}


/*
 * Predicate returning @c true if this Image's "id" attribute is set.
 */
bool
Image::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Image's "href" attribute is set.
 */
bool
Image::isSetHref() const
{
  return (mHref.empty() == false);
}


/*
 * Predicate returning @c true if this Image's "href" attribute is set.
 */
bool
Image::isSetImageReference() const
{
  return (mHref.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this Image.
 */
int
Image::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "href" attribute of this Image.
 */
int
Image::setHref(const std::string& href)
{
  mHref = href;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "href" attribute of this Image.
 */
int
Image::setImageReference(const std::string& href)
{
  return setHref(href);
}


/*
 * Unsets the value of the "id" attribute of this Image.
 */
int
Image::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "href" attribute of this Image.
 */
int
Image::unsetHref()
{
  mHref.erase();

  if (mHref.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "href" attribute of this Image.
 */
int
Image::unsetImageReference()
{
  return unsetHref();
}


/*
* Returns a const reference to the x coordinate of the image position.
*/
const RelAbsVector& 
Image::getX() const
{
  return this->mX;
}


RelAbsVector& 
Image::getX()
{
  return this->mX;
}


/*
* Returns a const reference to the y coordinate of the image position.
*/
const RelAbsVector& 
Image::getY() const
{
  return this->mY;
}


RelAbsVector& 
Image::getY()
{
  return this->mY;
}


/*
* Returns a const reference to the z coordinate of the image position.
*/
const RelAbsVector& 
Image::getZ() const
{
  return this->mZ;
}


RelAbsVector& 
Image::getZ()
{
  return this->mZ;
}


/*
* Returns a const reference to the width of the image.
*/
const RelAbsVector& 
Image::getWidth() const
{
  return this->mWidth;
}


RelAbsVector&
Image::getWidth()
{
  return this->mWidth;
}


/*
* Returns a const reference to the height of the image.
*/
const RelAbsVector& 
Image::getHeight() const
{
  return this->mHeight;
}


RelAbsVector& 
Image::getHeight()
{
  return this->mHeight;
}


/*
 * Predicate returning @c true if this Image's "x" element is set.
 */
bool
Image::isSetX() const
{
  return mX.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Image's "y" element is set.
 */
bool
Image::isSetY() const
{
  return mY.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Image's "z" element is set.
 */
bool
Image::isSetZ() const
{
  return mZ.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Image's "width" element is set.
 */
bool
Image::isSetWidth() const
{
  return mWidth.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Image's "height" element is set.
 */
bool
Image::isSetHeight() const
{
  return mHeight.isSetCoordinate();
}


/** @cond doxygenLibsbmlInternal */
/*
* Sets the position of the image relative to its viewport.
* The position can either be specified in relative or in absolute coordinates
* or a combination of both.
* The z coordinate can be omitted. In that case it is set to 0.
*
* @param x x coordinate of the image position
* @param y y coordinate of the image position
* @param z z coordinate of the image position
*/
void Image::setCoordinates(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
  this->mX = x;
  this->mY = y;
  this->mZ = z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the dimensions of the image.
 * The dimensions can be set as relative values or absolute values, or 
 * a combination of both.
 *
 * @param width the width of the image when rendered
 * @param height the height of the image when rendered
 */
void Image::setDimensions(const RelAbsVector& width,const RelAbsVector& height)
{
    this->mWidth=width;
    this->mHeight=height;
}
/** @endcond */


/*
* Sets the x coordinate of the image position.
*/
int
Image::setX(const RelAbsVector& coord)
{
  this->mX = coord;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the y coordinate of the image position.
*/
int
Image::setY(const RelAbsVector& coord)
{
  this->mY = coord;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the z coordinate of the image position.
*/
int
Image::setZ(const RelAbsVector& coord)
{
  this->mZ = coord;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the width of the image when rendered.
 */
int 
Image::setWidth(const RelAbsVector& width)
{
    this->mWidth=width;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the height of the image when rendered.
 */
int 
Image::setHeight(const RelAbsVector& height)
{
    this->mHeight=height;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "x" element of this Image.
 */
int
Image::unsetX()
{
  mX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "y" element of this Image.
 */
int
Image::unsetY()
{
  mY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "z" element of this Image.
 */
int
Image::unsetZ()
{
  mZ.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "width" element of this Image.
 */
int
Image::unsetWidth()
{
  mWidth.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "height" element of this Image.
 */
int
Image::unsetHeight()
{
  mHeight.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this Image object.
 */
const std::string&
Image::getElementName() const
{
  static const string name = "image";
  return name;
}


/*
 * Returns the libSBML type code for this Image object.
 */
int
Image::getTypeCode() const
{
  return SBML_RENDER_IMAGE;
}


/*
 * Predicate returning @c true if all the required attributes for this Image
 * object have been set.
 */
bool
Image::hasRequiredAttributes() const
{
  bool allPresent = Transformation2D::hasRequiredAttributes();

  if (isSetHref() == false)
  {
    allPresent = false;
  }

  if (isSetX() == false)
  {
    allPresent = false;
  }

  if (isSetY() == false)
  {
    allPresent = false;
  }

  if (isSetWidth() == false)
  {
    allPresent = false;
  }

  if (isSetHeight() == false)
  {
    allPresent = false;
  }
  return allPresent;
}

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor
 */
bool
Image::accept(SBMLVisitor& v) const
{
  //render - FIX_ME
  v.visit(*this);

  //if (mX != NULL)
  //{
  //  mX->accept(v);
  //}

  //if (mY != NULL)
  //{
  //  mY->accept(v);
  //}

  //if (mZ != NULL)
  //{
  //  mZ->accept(v);
  //}

  //if (mWidth != NULL)
  //{
  //  mWidth->accept(v);
  //}

  //if (mHeight != NULL)
  //{
  //  mHeight->accept(v);
  //}

  //v.leave(*this);
  return true;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Image object.
 *
 * @return the XMLNode with the XML representation for the 
 * Image object.
 */
XMLNode Image::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
Image::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation2D::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
  attributes.add("width");
  attributes.add("height");
  attributes.add("href");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Image::readAttributes(const XMLAttributes& attributes,
                      const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  Transformation2D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderImageAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderImageAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true && log)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<Image>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  string elplusid = "<image> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }

  // 
  // href string (use = "required" )
  // 

  assigned = attributes.readInto("href", mHref);

  if (log)
  {
    if (assigned == true)
    {
      if (mHref.empty() == true)
      {
        logEmptyString(mHref, level, version, "<Image>");
      }
    }
    else
    {
      std::string message = "Render attribute 'href' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderImageAllowedAttributes, pkgVersion,
        level, version, message, getLine(), getColumn());
    }
  }
 
  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // x RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("x", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'x' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderImageAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderImageXMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

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
  assigned = attributes.readInto("y", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'y' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderImageAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderImageYMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

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
    this->mZ = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderImageZMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setZ(v);
    }
    v.erase();
  }

  //
  // width RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("width", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'width' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderImageAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'width' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderImageWidthMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setWidth(v);
    }
    v.erase();
  }

  //
  // height RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("height", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'height' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderImageAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'heigth' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderImageHeightMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setHeight(v);
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
void Image::writeAttributes (XMLOutputStream& stream) const
{
    Transformation2D::writeAttributes(stream);
    if(this->isSetId())
    {
        stream.writeAttribute("id", getPrefix(), this->getId());
    }
    std::ostringstream os;
    os << this->mX;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << this->mY;
    stream.writeAttribute("y", getPrefix(), os.str());
    RelAbsVector tmp(0.0,0.0);
    if(this->mZ!=tmp)
    {
        os.str("");
        os << this->mZ;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mWidth;
    stream.writeAttribute("width", getPrefix(), os.str());
    os.str("");
    os << this->mHeight;
    stream.writeAttribute("height", getPrefix(), os.str());
    stream.writeAttribute("href", getPrefix(), mHref);
}
/** @endcond */



#endif /* __cplusplus */


/*
 * Creates a new Image_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Image_t *
Image_create(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
{
  return new Image(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Image_t object.
 */
LIBSBML_EXTERN
Image_t*
Image_clone(const Image_t* i)
{
  if (i != NULL)
  {
    return static_cast<Image_t*>(i->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Image_t object.
 */
LIBSBML_EXTERN
void
Image_free(Image_t* i)
{
  if (i != NULL)
  {
    delete i;
  }
}


/*
 * Returns the value of the "id" attribute of this Image_t.
 */
LIBSBML_EXTERN
char *
Image_getId(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return i->getId().empty() ? NULL : safe_strdup(i->getId().c_str());
}


/*
 * Returns the value of the "href" attribute of this Image_t.
 */
LIBSBML_EXTERN
char *
Image_getHref(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return i->getHref().empty() ? NULL : safe_strdup(i->getHref().c_str());
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Image_isSetId(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "href" attribute is set.
 */
LIBSBML_EXTERN
int
Image_isSetHref(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetHref()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setId(Image_t * i, const char * id)
{
  return (i != NULL) ? i->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "href" attribute of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setHref(Image_t * i, const char * href)
{
  return (i != NULL) ? i->setHref(href) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetId(Image_t * i)
{
  return (i != NULL) ? i->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "href" attribute of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetHref(Image_t * i)
{
  return (i != NULL) ? i->unsetHref() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "x" element of this Image_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getX(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(i->getX()));
}


/*
 * Returns the value of the "y" element of this Image_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getY(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(i->getY()));
}


/*
 * Returns the value of the "z" element of this Image_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getZ(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(i->getZ()));
}


/*
 * Returns the value of the "width" element of this Image_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getWidth(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(i->getWidth()));
}


/*
 * Returns the value of the "height" element of this Image_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getHeight(const Image_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(i->getHeight()));
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "x" element is set.
 */
LIBSBML_EXTERN
int
Image_isSetX(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "y" element is set.
 */
LIBSBML_EXTERN
int
Image_isSetY(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "z" element is set.
 */
LIBSBML_EXTERN
int
Image_isSetZ(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetZ()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "width" element is set.
 */
LIBSBML_EXTERN
int
Image_isSetWidth(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetWidth()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Image_t's "height" element is set.
 */
LIBSBML_EXTERN
int
Image_isSetHeight(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetHeight()) : 0;
}


/*
 * Sets the value of the "x" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setX(Image_t * i, const RelAbsVector_t* x)
{
  return (i != NULL) ? i->setX(*x) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setY(Image_t * i, const RelAbsVector_t* y)
{
  return (i != NULL) ? i->setY(*y) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setZ(Image_t * i, const RelAbsVector_t* z)
{
  return (i != NULL) ? i->setZ(*z) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "width" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setWidth(Image_t * i, const RelAbsVector_t* width)
{
  return (i != NULL) ? i->setWidth(*width) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "height" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_setHeight(Image_t * i, const RelAbsVector_t* height)
{
  return (i != NULL) ? i->setHeight(*height) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetX(Image_t * i)
{
  return (i != NULL) ? i->unsetX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetY(Image_t * i)
{
  return (i != NULL) ? i->unsetY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetZ(Image_t * i)
{
  return (i != NULL) ? i->unsetZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "width" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetWidth(Image_t * i)
{
  return (i != NULL) ? i->unsetWidth() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "height" element of this Image_t.
 */
LIBSBML_EXTERN
int
Image_unsetHeight(Image_t * i)
{
  return (i != NULL) ? i->unsetHeight() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Image_t object have been set.
 */
LIBSBML_EXTERN
int
Image_hasRequiredAttributes(const Image_t * i)
{
  return (i != NULL) ? static_cast<int>(i->hasRequiredAttributes()) : 0;
}



LIBSBML_CPP_NAMESPACE_END


