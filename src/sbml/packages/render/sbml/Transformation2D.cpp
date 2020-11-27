/**
 * @file    Transformation2D.cpp
 * @brief   class representing a 2D affine transformation
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

#include <sbml/packages/render/sbml/Transformation2D.h>

#include <sbml/packages/render/sbml/ListOfDrawables.h>
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


using namespace std;




#include <string.h>
#include <stdlib.h>
#include <sstream>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus

/** @cond doxygenLibsbmlInternal */
const double Transformation2D::IDENTITY2D[6]={1.0,0.0,0.0,1.0,0.0,0.0};
/** @endcond */

/*
 * Creates a new Transformation2D using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Transformation2D::Transformation2D(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
:    Transformation(level,version, pkgVersion)
  , mElementName("transformation2D")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
    this->updateMatrix2D();
}


/*
 * Creates a new Transformation2D using the given RenderPkgNamespaces object.
 */
Transformation2D::Transformation2D(RenderPkgNamespaces *renderns)
  : Transformation(renderns)
  , mElementName("transformation2D")
{
    this->updateMatrix2D();
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}

/*
 * Creates a new Transformation2D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Transformation2D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Transformation2D
 * object to be instantiated.
 */
Transformation2D::Transformation2D(const XMLNode& node, unsigned int l2version)
  :Transformation(node, l2version)
{
      ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/*
 * Copy constructor for Transformation2D.
 */
Transformation2D::Transformation2D(const Transformation2D& orig)
  : Transformation( orig )
  , mElementName ( orig.mElementName )
{
  setMatrix2D(orig.getMatrix2D());
}


/*
 * Assignment operator for Transformation2D.
 */
Transformation2D&
Transformation2D::operator=(const Transformation2D& rhs)
{
  if (&rhs != this)
  {
    Transformation::operator=(rhs);
    mElementName = rhs.mElementName;
  setMatrix2D(rhs.getMatrix2D());
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Transformation2D object.
 */
Transformation2D*
Transformation2D::clone() const
{
  return (Transformation2D*)(Transformation::clone());
}


/*
 * Destructor for Transformation2D.
 */
Transformation2D::~Transformation2D()
{
}



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id and values for the matrix.
 */
Transformation2D::Transformation2D(RenderPkgNamespaces* renderns, const double matrix[6]):Transformation(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Transformation2D::Transformation2D(const double matrix[6]) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    this->setMatrix2D(matrix);
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
 * Predicate returning @c true if this abstract Transformation2D is of type
 * Image
 */
bool
Transformation2D::isImage() const
{
  return dynamic_cast<const Image*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * Ellipse
 */
bool
Transformation2D::isEllipse() const
{
  return dynamic_cast<const Ellipse*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * Rectangle
 */
bool
Transformation2D::isRectangle() const
{
  return dynamic_cast<const Rectangle*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * Polygon
 */
bool
Transformation2D::isPolygon() const
{
  return dynamic_cast<const Polygon*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * RenderGroup
 */
bool
Transformation2D::isRenderGroup() const
{
  return dynamic_cast<const RenderGroup*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * LineEnding
 */
bool
Transformation2D::isLineEnding() const
{
  return dynamic_cast<const LineEnding*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * Text
 */
bool
Transformation2D::isText() const
{
  return dynamic_cast<const Text*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Transformation2D is of type
 * RenderCurve
 */
bool
Transformation2D::isRenderCurve() const
{
  return dynamic_cast<const RenderCurve*>(this) != NULL;
}


/*
 * Returns a 2D identity matrix.
 * The matrix contains 6 double values.
 */
const double* Transformation2D::getIdentityMatrix2D()
{
    return IDENTITY2D;
}


/*
 * Sets the 2D matrix to the values given in the array.
 * The 3D matrix is updated accordingly.
 *
 * @param m array with new values to be set for this Transformation object.
 */
void Transformation2D::setMatrix2D(const double m[6])
{
    unsigned int i;
    for(i=0;i<6;++i)
    {
        mMatrix2D[i]=m[i];
    }
    this->updateMatrix3D();
}


/*
 * Sets the 2D matrix to the values given in the array.
 * The 2D matrix is updated accordingly.
 *
 * @param m array with new values to be set for this Transformation object.
 */
void Transformation2D::setMatrix(const double m[12])
{
    this->Transformation::setMatrix(m);
    this->updateMatrix2D();
}


/*
 * Returns the 2D matrix which is an array of double values of length 6.
 *
 * @return a pointer to the array of numbers for the 2D transformation.
 */
const double* Transformation2D::getMatrix2D() const
{
    return mMatrix2D;
}
/*
 * Returns the XML element name of this Transformation2D object.
 */
const std::string&
Transformation2D::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this Transformation2D object.
 */
void
Transformation2D::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this Transformation2D object.
 */
int
Transformation2D::getTypeCode() const
{
  return SBML_RENDER_TRANSFORMATION2D;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Transformation2D object have been set.
 */
bool
Transformation2D::hasRequiredAttributes() const
{
  bool allPresent = Transformation::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Transformation2D::writeElements(XMLOutputStream& stream) const
{
  Transformation::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Transformation2D::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Transformation2D::setSBMLDocument(SBMLDocument* d)
{
  Transformation::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Transformation2D::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix,
                                        bool flag)
{
  Transformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::getAttribute(const std::string& attributeName,
                               bool& value) const
{
  int return_value = Transformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::getAttribute(const std::string& attributeName,
                               int& value) const
{
  int return_value = Transformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::getAttribute(const std::string& attributeName,
                               double& value) const
{
  int return_value = Transformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::getAttribute(const std::string& attributeName,
                               unsigned int& value) const
{
  int return_value = Transformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::getAttribute(const std::string& attributeName,
                               std::string& value) const
{
  int return_value = Transformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Transformation2D's attribute
 * "attributeName" is set.
 */
bool
Transformation2D::isSetAttribute(const std::string& attributeName) const
{
  bool value = Transformation::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = Transformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::setAttribute(const std::string& attributeName, int value)
{
  int return_value = Transformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::setAttribute(const std::string& attributeName, double value)
{
  int return_value = Transformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::setAttribute(const std::string& attributeName,
                               unsigned int value)
{
  int return_value = Transformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::setAttribute(const std::string& attributeName,
                               const std::string& value)
{
  int return_value = Transformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Transformation2D.
 */
int
Transformation2D::unsetAttribute(const std::string& attributeName)
{
  int value = Transformation::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Transformation2D::createObject(XMLInputStream& stream)
{
  SBase* obj = Transformation::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Transformation2D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation::addExpectedAttributes(attributes);

  attributes.add("transform");
}


/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Transformation2D::readAttributes(const XMLAttributes& attributes,
                                 const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDrawables*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
    }
  }

  Transformation::readAttributes(attributes, expectedAttributes);

  //if (log)
  //{
  //  numErrs = log->getNumErrors();

  //  for (int n = numErrs-1; n >= 0; n--)
  //  {
  //    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
  //    {
  //      const std::string details = log->getError(n)->getMessage();
  //      log->remove(UnknownPackageAttribute);
  //      log->logPackageError("render", RenderUnknown, pkgVersion, level,
  //        version, details);
  //    }
  //    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
  //    {
  //      const std::string details = log->getError(n)->getMessage();
  //      log->remove(UnknownCoreAttribute);
  //      log->logPackageError("render",
  //        RenderTransformation2DAllowedCoreAttributes, pkgVersion, level,
  //          version, details);
  //    }
  //  }
  //}
    std::string s;
    attributes.readInto("transform", s);
    if(!s.empty())
    {
        this->parseTransformation(s);
    }
    else
    {
        this->updateMatrix2D();
    }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Transformation2D::writeAttributes(XMLOutputStream& stream) const
{
  Transformation::writeAttributes(stream);

  if(this->isSetMatrix() && memcmp(this->mMatrix,getIdentityMatrix(),12*sizeof(double))!=0)
    {
        stream.writeAttribute("transform", getPrefix(), this->get2DTransformationString());
    }
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Tries to parse the numerical values from the given string
 * and fill the matrix with them. The method will accept strings
 * representing 6 or 12 numerical values and fill the 2D or 3D matrix
 * accordingly.
 * The other matrix is updated automatically.
 *
 * @param transformationString string representing 6 or 12 numerical values.
 */
void Transformation2D::parseTransformation(const std::string& transformationString)
{
    // the string should contain a list of 6 comma seperated numbers
    // if it doesn't, we set the matrix to the identity matrix
    bool result=true;
    std::string delimiter=",";
    std::size_t lastPos=transformationString.find_first_not_of(delimiter);
    std::size_t pos;
    unsigned int index=0;
    while(lastPos!=std::string::npos)
    {
        if(index > 5)
        {
            result=false;
            break;
        }
        pos=transformationString.find_first_of(delimiter,lastPos);
        double value=strtod(transformationString.substr(lastPos,pos-lastPos).c_str(),NULL);
        mMatrix2D[index]=value;
        index++;
        lastPos=transformationString.find_first_not_of(delimiter,pos);
    }
    if(!result || index != 6)
    {
        this->setMatrix2D(Transformation2D::IDENTITY2D);
    }
    this->updateMatrix3D();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Adds the transformation attribute to the given XMLAttributes object.
 *
 * @param transformation the transformation to add as attribute.
 *
 * @param att XMLAttributes where the attribute needs to be added to
 */
void Transformation2D::addTransformation2DAttributes(const Transformation2D& transformation,XMLAttributes& att)
{
    if(transformation.isSetMatrix() && memcmp(transformation.mMatrix,getIdentityMatrix(),12*sizeof(double))!=0)
    {
        att.add("transform",transformation.get2DTransformationString());
    }
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Returns the transformation array as a string for storage in an XML
 * attribute.
 */
std::string Transformation2D::get2DTransformationString() const
{
    // transform the matrix to a string
    std::ostringstream os;
    unsigned int i;
    os << mMatrix2D[0];
    for(i=1;i<6;++i)
    {
        os << "," << mMatrix2D[i];
    }
    return os.str();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 3D matrix from the 2D matrix.
 */
void Transformation2D::updateMatrix3D()
{
    mMatrix[0]=mMatrix2D[0];
    mMatrix[1]=mMatrix2D[1];
    mMatrix[2]=0.0;
    mMatrix[3]=mMatrix2D[2];
    mMatrix[4]=mMatrix2D[3];
    mMatrix[5]=0.0;
    mMatrix[6]=0.0;
    mMatrix[7]=0.0;
    mMatrix[8]=1.0;
    mMatrix[9]=mMatrix2D[4];
    mMatrix[10]=mMatrix2D[5];
    mMatrix[11]=0.0;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Fills the 2D matrix with data from the 3D matrix.
 */
void Transformation2D::updateMatrix2D()
{
    mMatrix2D[0]=mMatrix[0];
    mMatrix2D[1]=mMatrix[1];
    mMatrix2D[2]=mMatrix[3];
    mMatrix2D[3]=mMatrix[4];
    mMatrix2D[4]=mMatrix[9];
    mMatrix2D[5]=mMatrix[10];
}
/** @endcond */




#endif /* __cplusplus */


/*
* Creates a new Image (Transformation_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createImage(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Image(level, version, pkgVersion);
}


/*
* Creates a new Ellipse (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Ellipse(level, version, pkgVersion);
}


/*
* Creates a new Rectangle (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Rectangle(level, version, pkgVersion);
}


/*
* Creates a new Polygon (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Polygon(level, version, pkgVersion);
}


/*
* Creates a new RenderGroup (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new RenderGroup(level, version, pkgVersion);
}


/*
* Creates a new Text (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createText(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Text(level, version, pkgVersion);
}


/*
* Creates a new RenderCurve (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRenderCurve(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new RenderCurve(level, version, pkgVersion);
}


/*
* Creates a new LineEnding (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new LineEnding(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Transformation2D_t object.
 */
LIBSBML_EXTERN
Transformation2D_t*
Transformation2D_clone(const Transformation2D_t* td)
{
  if (td != NULL)
  {
    return static_cast<Transformation2D_t*>(td->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Transformation2D_t object.
 */
LIBSBML_EXTERN
void
Transformation2D_free(Transformation2D_t* td)
{
  if (td != NULL)
  {
    delete td;
  }
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type Image_t
 */
LIBSBML_EXTERN
int
Transformation2D_isImage(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isImage()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type Ellipse_t
 */
LIBSBML_EXTERN
int
Transformation2D_isEllipse(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isEllipse()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type Rectangle_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRectangle(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isRectangle()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type Polygon_t
 */
LIBSBML_EXTERN
int
Transformation2D_isPolygon(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isPolygon()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type RenderGroup_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRenderGroup(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isRenderGroup()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type LineEnding_t
 */
LIBSBML_EXTERN
int
Transformation2D_isLineEnding(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isLineEnding()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type Text_t
 */
LIBSBML_EXTERN
int
Transformation2D_isText(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isText()) : 0;
}


/*
 * Predicate returning @c 1 if this Transformation2D_t is of type RenderCurve_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRenderCurve(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->isRenderCurve()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Transformation2D_t object have been set.
 */
LIBSBML_EXTERN
int
Transformation2D_hasRequiredAttributes(const Transformation2D_t * td)
{
  return (td != NULL) ? static_cast<int>(td->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


