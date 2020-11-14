/**
 * @file    GraphicalPrimitive1D.cpp
 * @brief   abstract base class for graphical 1D objects
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

#include <sbml/packages/render/sbml/GraphicalPrimitive1D.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

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

#include <limits>
#include <sstream>
#include <string.h> 
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GraphicalPrimitive1D using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : Transformation2D(level, version, pkgVersion)
    ,mStroke("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
  , mIsSetStrokeWidth (false)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new GraphicalPrimitive1D using the given RenderPkgNamespaces
 * object.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(RenderPkgNamespaces *renderns)
  : Transformation2D(renderns)
  , mStroke ("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
  , mIsSetStrokeWidth (false)
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/*
 * Creates a new GraphicalPrimitive1D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GraphicalPrimitive1D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the GraphicalPrimitive1D
 * object to be instantiated.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(const XMLNode& node, unsigned int l2version)
  :Transformation2D(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;    
    
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes,ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        ++n;
    }

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GraphicalPrimitive1D.
 * The transformation properties are not set, neither is the stroke or the stroke width.
 * The id is set to the given string.
 *
 * @param id The id for the GraphicalPrimitive1D object
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(RenderPkgNamespaces* renderns, const std::string& id):
    Transformation2D(renderns)
//    ,mId(id)
    ,mStroke("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GraphicalPrimitive1D::GraphicalPrimitive1D(const std::string& id) is deprecated." << std::endl;
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
 * Copy constructor for GraphicalPrimitive1D.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(const GraphicalPrimitive1D& orig)
  : Transformation2D( orig )
  , mStroke ( orig.mStroke )
  , mStrokeWidth ( orig.mStrokeWidth )
  , mIsSetStrokeWidth ( orig.mIsSetStrokeWidth )
  , mStrokeDashArray (orig.mStrokeDashArray)
{
}


/*
 * Assignment operator for GraphicalPrimitive1D.
 */
GraphicalPrimitive1D&
GraphicalPrimitive1D::operator=(const GraphicalPrimitive1D& rhs)
{
  if (&rhs != this)
  {
    Transformation2D::operator=(rhs);
    mStroke = rhs.mStroke;
    mStrokeWidth = rhs.mStrokeWidth;
    mIsSetStrokeWidth = rhs.mIsSetStrokeWidth;
    mStrokeDashArray = rhs.mStrokeDashArray;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GraphicalPrimitive1D object.
 */
GraphicalPrimitive1D*
GraphicalPrimitive1D::clone() const
{
  return (GraphicalPrimitive1D*)(Transformation::clone());
}


/*
 * Destructor for GraphicalPrimitive1D.
 */
GraphicalPrimitive1D::~GraphicalPrimitive1D()
{
}


/*
 * Returns the value of the "id" attribute of this GraphicalPrimitive1D.
 */
const std::string&
GraphicalPrimitive1D::getId() const
{
  return mId;
}


/*
 * Returns the value of the "stroke" attribute of this GraphicalPrimitive1D.
 */
const std::string&
GraphicalPrimitive1D::getStroke() const
{
  return mStroke;
}


/*
 * Returns the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D.
 */
double
GraphicalPrimitive1D::getStrokeWidth() const
{
  return mStrokeWidth;
}


/*
 * Returns the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D.
 */
const std::vector<unsigned int>&
GraphicalPrimitive1D::getStrokeDashArray() const
{
  return this->mStrokeDashArray;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the stroke dasharray.
 *
 * @return const reference to stroke dash array
 */
const std::vector<unsigned int>& GraphicalPrimitive1D::getDashArray() const
{
    return this->mStrokeDashArray;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the stroke dasharray.
 *
 * @return reference to stroke dash array
 */
std::vector<unsigned int>& GraphicalPrimitive1D::getDashArray()
{
    return this->mStrokeDashArray;
}
/** @endcond */

/*
 * Predicate returning @c true if this GraphicalPrimitive1D's "id" attribute is
 * set.
 */
bool
GraphicalPrimitive1D::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this GraphicalPrimitive1D's "stroke"
 * attribute is set.
 */
bool
GraphicalPrimitive1D::isSetStroke() const
{
  return (!this->mStroke.empty()) && (this->mStroke != "none");
}


/*
 * Predicate returning @c true if this GraphicalPrimitive1D's "stroke-width"
 * attribute is set.
 */
bool
GraphicalPrimitive1D::isSetStrokeWidth() const
{
  return mIsSetStrokeWidth;
}


/*
 * Predicate returning @c true if this GraphicalPrimitive1D's
 * "stroke-dasharray" attribute is set.
 */
bool
GraphicalPrimitive1D::isSetStrokeDashArray() const
{
    bool result=true;
    if(mStrokeDashArray.empty() || mStrokeDashArray[0]!=mStrokeDashArray[0])
    {
        result=false;
    }
    return result;
}

/*
 * Returns true if the dash array has been set or false otherwise.
 * The array is considered set if it is not empty and if the first entry is
 * not NaN.
 *
 * @true if the dasharray is set.
 */
bool GraphicalPrimitive1D::isSetDashArray() const
{
    bool result=true;
    if(mStrokeDashArray.empty() || mStrokeDashArray[0]!=mStrokeDashArray[0])
    {
        result=false;
    }
    return result;
}


/*
 * Sets the value of the "id" attribute of this GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "stroke" attribute of this GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setStroke(const std::string& stroke)
{
  mStroke = stroke;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "stroke-width" attribute of this GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setStrokeWidth(double strokeWidth)
{
  mStrokeWidth = strokeWidth;
  mIsSetStrokeWidth = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setStrokeDashArray(const std::vector<unsigned int>& array)
{
  this->mStrokeDashArray = array;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the dasharray to the values in the given array.
 *
 * @param array Array of alternating stroke and gap length values.
 */
void GraphicalPrimitive1D::setDashArray(const std::vector<unsigned int>& array)
{
    this->mStrokeDashArray=array;
}


/*
 * Sets the dasharray from the given string.
 * If the string is not a valid dasharray string, false
 * is returned and the dasharray remains in the state is was
 * before the call.
 *
 * The individual numerical values in the string have to be separated by kommas.
 *
 * @param arrayString a string with number representing a dash array.
 *
 * @return @c true if setting the dasharray from the string succeed or @c false otherwise.
 */
bool GraphicalPrimitive1D::setDashArray(const std::string& arrayString)
{
    std::vector<unsigned int> array;
    bool result=false;
    if(this->parseDashArray(arrayString,array)==true)
    {
        this->mStrokeDashArray=array;
        result=true;
    }
    return result;
}


/* 
 * Returns the number of defined dashes.
 */
unsigned int 
GraphicalPrimitive1D::getNumDashes() const
{
  return (unsigned int)mStrokeDashArray.size();
}

/*
 * Returns the dash at the given index. 
 */
unsigned int 
GraphicalPrimitive1D::getDashByIndex(unsigned int index) const
{
  if (index >= getNumDashes())
    return -1;
  return mStrokeDashArray[index];
}

/*
 * Adds a new length of a dash to the 'stroke-dasharray' attribute.
 */
void 
GraphicalPrimitive1D::addDash(unsigned int dash)
{
  mStrokeDashArray.push_back(dash);
}

/*
 * Unsets the 'stroke-dasharray' attribute.
 */
void 
GraphicalPrimitive1D::clearDashes()
{
  mStrokeDashArray.clear();
}

/* 
 * Sets the dash at the given position.
 */
void 
GraphicalPrimitive1D::setDashByIndex(unsigned int index, unsigned int dash)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray[index]=dash;
}

/*
 * Removes the dash at the given index
 */
void 
GraphicalPrimitive1D::removeDash(unsigned int index)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray.erase(mStrokeDashArray.begin() + index);
}

/* 
 * Inserts the dash at the given position.
 */
void 
GraphicalPrimitive1D::insertDash(unsigned int index, unsigned int dash)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray.insert(mStrokeDashArray.begin() + index, dash);
}


/*
 * Unsets the value of the "id" attribute of this GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::unsetId()
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
 * Unsets the value of the "stroke" attribute of this GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::unsetStroke()
{
  mStroke.erase();

  if (mStroke.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::unsetStrokeWidth()
{
  mStrokeWidth = util_NaN();
  mIsSetStrokeWidth = false;

  if (isSetStrokeWidth() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::unsetStrokeDashArray()
{
  mStrokeDashArray.clear();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type Ellipse
 */
bool
GraphicalPrimitive1D::isEllipse() const
{
  return dynamic_cast<const Ellipse*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type Rectangle
 */
bool
GraphicalPrimitive1D::isRectangle() const
{
  return dynamic_cast<const Rectangle*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type Polygon
 */
bool
GraphicalPrimitive1D::isPolygon() const
{
  return dynamic_cast<const Polygon*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type RenderGroup
 */
bool
GraphicalPrimitive1D::isRenderGroup() const
{
  return dynamic_cast<const RenderGroup*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type LineEnding
 */
bool
GraphicalPrimitive1D::isLineEnding() const
{
  return dynamic_cast<const LineEnding*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type Text
 */
bool
GraphicalPrimitive1D::isText() const
{
  return dynamic_cast<const Text*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
 * type RenderCurve
 */
bool
GraphicalPrimitive1D::isRenderCurve() const
{
  return dynamic_cast<const RenderCurve*>(this) != NULL;
}


/*
 * Returns the libSBML type code for this GraphicalPrimitive1D object.
 */
int
GraphicalPrimitive1D::getTypeCode() const
{
  return SBML_RENDER_GRAPHICALPRIMITIVE1D;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * GraphicalPrimitive1D object have been set.
 */
bool
GraphicalPrimitive1D::hasRequiredAttributes() const
{
  bool allPresent = Transformation2D::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
GraphicalPrimitive1D::writeElements(XMLOutputStream& stream) const
{
  Transformation2D::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GraphicalPrimitive1D::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GraphicalPrimitive1D::setSBMLDocument(SBMLDocument* d)
{
  Transformation2D::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GraphicalPrimitive1D::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  Transformation2D::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = Transformation2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = Transformation2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = Transformation2D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "stroke-width")
  {
    value = getStrokeWidth();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = Transformation2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::getAttribute(const std::string& attributeName,
                                   std::string& value) const
{
  int return_value = Transformation2D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "stroke")
  {
    value = getStroke();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GraphicalPrimitive1D's attribute
 * "attributeName" is set.
 */
bool
GraphicalPrimitive1D::isSetAttribute(const std::string& attributeName) const
{
  bool value = Transformation2D::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "stroke")
  {
    value = isSetStroke();
  }
  else if (attributeName == "stroke-width")
  {
    value = isSetStrokeWidth();
  }
  else if (attributeName == "stroke-dasharray")
  {
    value = isSetStrokeDashArray();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = Transformation2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = Transformation2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = Transformation2D::setAttribute(attributeName, value);

  if (attributeName == "stroke-width")
  {
    return_value = setStrokeWidth(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = Transformation2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value = Transformation2D::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "stroke")
  {
    return_value = setStroke(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * GraphicalPrimitive1D.
 */
int
GraphicalPrimitive1D::unsetAttribute(const std::string& attributeName)
{
  int value = Transformation2D::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "stroke")
  {
    value = unsetStroke();
  }
  else if (attributeName == "stroke-width")
  {
    value = unsetStrokeWidth();
  }
  else if (attributeName == "stroke-dasharray")
  {
    value = unsetStrokeDashArray();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
GraphicalPrimitive1D::createObject(XMLInputStream& stream)
{
  SBase* obj = Transformation2D::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GraphicalPrimitive1D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation2D::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("stroke");

  attributes.add("stroke-width");

  attributes.add("stroke-dasharray");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GraphicalPrimitive1D::readAttributes(const XMLAttributes& attributes,
                                     const ExpectedAttributes&
                                       expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  Transformation2D::readAttributes(attributes, expectedAttributes);

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true && log)
    {
      logEmptyString(mId, level, version, "<GraphicalPrimitive1D>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && log)
    {
      log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // stroke string (use = "optional" )
  // 

  assigned = attributes.readInto("stroke", mStroke);

  if (assigned == true)
  {
    if (mStroke.empty() == true && log)
    {
      logEmptyString(mStroke, level, version, "<GraphicalPrimitive1D>");
    }
  }

  // 
  // stroke-width double (use = "optional" )
  // 
  if (log)  numErrs = log->getNumErrors();
  mIsSetStrokeWidth = attributes.readInto("stroke-width", mStrokeWidth);

  if (mIsSetStrokeWidth == false)
  {
    if (log && log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Render attribute 'stroke-width' from the "
        "<GraphicalPrimitive1D> element must be a double.";
      log->logPackageError("render",
        RenderGraphicalPrimitive1DStrokeWidthMustBeDouble, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
    mStrokeWidth = std::numeric_limits<double>::quiet_NaN();
  }


    // parse the stroke-dasharray
    std::string s;
    if(attributes.readInto("stroke-dasharray", s, getErrorLog(), false, getLine(), getColumn()) && !s.empty())
    {
        this->setDashArray(s);
    }
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
GraphicalPrimitive1D::writeAttributes(XMLOutputStream& stream) const
{
  Transformation2D::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetStroke() == true)
  {
    stream.writeAttribute("stroke", getPrefix(), mStroke);
  }

  if (isSetStrokeWidth() == true)
  {
    stream.writeAttribute("stroke-width", getPrefix(), mStrokeWidth);
  }

    if(this->isSetDashArray())
    {
        std::ostringstream os;
        std::vector<unsigned int>::const_iterator it=this->getDashArray().begin();
        std::vector<unsigned int>::const_iterator endit=this->getDashArray().end();
        os << *it;
        ++it;
        while(it!=endit)
        {
            os << " , " << *it;
            ++it;
        }
        stream.writeAttribute("stroke-dasharray", getPrefix(), os.str());
    }

  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Adds all set attributes specific to the given GraphicalPrimitive1D objects to the given
 * XMLAttributes object.
 */
void GraphicalPrimitive1D::addGraphicalPrimitive1DAttributes(const GraphicalPrimitive1D& primitive,XMLAttributes& attributes) 
{
    if(primitive.isSetId())
    {
        attributes.add("id",primitive.getId());
    }
    // stroke, stroke_width
    if(primitive.isSetStroke())
    {
        attributes.add("stroke",primitive.getStroke());
    }
    if(primitive.isSetStrokeWidth())
    {
        std::ostringstream os;
        os << primitive.getStrokeWidth();
        attributes.add("stroke-width",os.str());
    }
    if(primitive.isSetDashArray())
    {
        std::ostringstream os;
        std::vector<unsigned int>::const_iterator it=primitive.getDashArray().begin();
        std::vector<unsigned int>::const_iterator endit=primitive.getDashArray().end();
        os << *it;
        ++it;
        while(it!=endit)
        {
            os << " , " << *it;
            ++it;
        }
        attributes.add("stroke-dasharray",os.str());
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 *  This method parses a dasharray string into the given vector.
 *  The vector is first cleared.
 *  If the dasharray is invalid, false is returned.
 */
bool GraphicalPrimitive1D::parseDashArray(const std::string& s,std::vector<unsigned int>& array)
{
    array.clear();
    bool result=true;
    if(!s.empty())
    {
        std::istringstream is(s);
        size_t size=s.size()+1;
        char* tmp=new char[size];
        char* tmp2=tmp;
        char* tmp3;
        char** endptr;
        long value;
        is.getline(tmp2,size,',');
        // continue until the string is empty or the stream is
        // no longer usable
        while(tmp2[0] != 0 && is.good())
        {
            endptr=&tmp2;
            tmp3=tmp2;
            value=strtol(tmp2,endptr,10);
            if(value < 0 || *endptr == tmp3 || ( **endptr != '\0' && **endptr != ' ' && **endptr != '\t'))
            {
                result=false;
                array.clear();
            }
            else
            {
                array.push_back((unsigned int)value);
            } 
            // read next element 
            is.getline(tmp2,size,',');
        }
        // check if the string was parsed to the end
        if(is.eof())
        {
            // we have to parse the current tmp2 
            // and check if that was ok
            endptr=&tmp2;
            tmp3=tmp2;
            value=strtol(tmp2,endptr,10);
            if(value < 0 || *endptr == tmp3 || ( **endptr != '\0' && **endptr != ' ' && **endptr != '\t'))
            {
                result=false;
                array.clear();
            }
            else
            {
                array.push_back((unsigned int)value);
            }  
        }
        else
        {
            // since we did not parse to the end, 
            // there most have been an error
            result=false;
            array.clear(); 
        } 
        delete[] tmp;
    }
    return result;
}
/** @endcond */




#endif /* __cplusplus */


/*
* Creates a new Ellipse (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Ellipse(level, version, pkgVersion);
}


/*
* Creates a new Rectangle (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Rectangle(level, version, pkgVersion);
}


/*
* Creates a new Polygon (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Polygon(level, version, pkgVersion);
}


/*
* Creates a new RenderGroup (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new RenderGroup(level, version, pkgVersion);
}


/*
* Creates a new Text (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createText(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Text(level, version, pkgVersion);
}


/*
* Creates a new RenderCurve (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRenderCurve(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new RenderCurve(level, version, pkgVersion);
}


/*
* Creates a new LineEnding (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new LineEnding(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GraphicalPrimitive1D_t object.
 */
LIBSBML_EXTERN
GraphicalPrimitive1D_t*
GraphicalPrimitive1D_clone(const GraphicalPrimitive1D_t* gpd)
{
  if (gpd != NULL)
  {
    return static_cast<GraphicalPrimitive1D_t*>(gpd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GraphicalPrimitive1D_t object.
 */
LIBSBML_EXTERN
void
GraphicalPrimitive1D_free(GraphicalPrimitive1D_t* gpd)
{
  if (gpd != NULL)
  {
    delete gpd;
  }
}


/*
 * Returns the value of the "id" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive1D_getId(const GraphicalPrimitive1D_t * gpd)
{
  if (gpd == NULL)
  {
    return NULL;
  }

  return gpd->getId().empty() ? NULL : safe_strdup(gpd->getId().c_str());
}


/*
 * Returns the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive1D_getStroke(const GraphicalPrimitive1D_t * gpd)
{
  if (gpd == NULL)
  {
    return NULL;
  }

  return gpd->getStroke().empty() ? NULL :
    safe_strdup(gpd->getStroke().c_str());
}


/*
 * Returns the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
double
GraphicalPrimitive1D_getStrokeWidth(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? gpd->getStrokeWidth() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetId(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's "stroke"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStroke(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetStroke()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's
 * "stroke-width" attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStrokeWidth(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetStrokeWidth()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's
 * "stroke-dasharray" attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStrokeDashArray(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetStrokeDashArray()) : 0;
}


/*
 * Sets the value of the "id" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setId(GraphicalPrimitive1D_t * gpd, const char * id)
{
  return (gpd != NULL) ? gpd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStroke(GraphicalPrimitive1D_t * gpd,
                               const char * stroke)
{
  return (gpd != NULL) ? gpd->setStroke(stroke) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStrokeWidth(GraphicalPrimitive1D_t * gpd,
                                    double strokeWidth)
{
  return (gpd != NULL) ? gpd->setStrokeWidth(strokeWidth) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStrokeDashArray(GraphicalPrimitive1D_t* gpd,
                                        const char* strokeDash)
{
  return (gpd != NULL) ? gpd->setDashArray(strokeDash)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetId(GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStroke(GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetStroke() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStrokeWidth(GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetStrokeWidth() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStrokeDashArray(GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetStrokeDashArray() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Ellipse_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isEllipse(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isEllipse()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * Rectangle_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRectangle(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isRectangle()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Polygon_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isPolygon(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isPolygon()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * RenderGroup_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRenderGroup(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isRenderGroup()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * LineEnding_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isLineEnding(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isLineEnding()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Text_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isText(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isText()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * RenderCurve_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRenderCurve(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isRenderCurve()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GraphicalPrimitive1D_t object have been set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_hasRequiredAttributes(const GraphicalPrimitive1D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


