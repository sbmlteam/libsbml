/**
 * @file    RenderCurve.cpp
 * @brief Implementation of the RenderCurve class.
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
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/render/sbml/RenderCurve.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/RenderCubicBezier.h>


using namespace std;


#include <sbml/xml/XMLInputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new RenderCurve using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RenderCurve::RenderCurve(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : GraphicalPrimitive1D(level, version, pkgVersion)
  , mStartHead ("")
  , mEndHead ("")
  , mRenderPoints (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new RenderCurve using the given RenderPkgNamespaces object.
 */
RenderCurve::RenderCurve(RenderPkgNamespaces *renderns)
  : GraphicalPrimitive1D(renderns)
  , mStartHead ("")
  , mEndHead ("")
  , mRenderPoints (renderns)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderCurve object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderCurve object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the RenderCurve
 * object to be instantiated.
 */
RenderCurve::RenderCurve(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive1D(node, l2version)
  , mRenderPoints(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    const XMLNode* child;	
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfElements")
        {
            this->mRenderPoints=ListOfCurveElements(*child);
        }
        ++n;
    }
    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @cond doxygenLibsbmlInternal */
#ifndef OMIT_DEPRECATED
RenderCurve::RenderCurve(RenderPkgNamespaces* renderns, const std::string& id)
  : GraphicalPrimitive1D(renderns, id)
  , mStartHead("")
  , mEndHead("")
  , mRenderPoints(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RenderCurve::RenderCurve(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
#endif // OMIT_DEPRECATED
/** @endcond */


/*
 * Copy constructor for RenderCurve.
 */
RenderCurve::RenderCurve(const RenderCurve& orig)
  : GraphicalPrimitive1D( orig )
  , mStartHead ( orig.mStartHead )
  , mEndHead ( orig.mEndHead )
  , mRenderPoints ( orig.mRenderPoints )
{
  connectToChild();
}


/*
 * Assignment operator for RenderCurve.
 */
RenderCurve&
RenderCurve::operator=(const RenderCurve& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive1D::operator=(rhs);
    mStartHead = rhs.mStartHead;
    mEndHead = rhs.mEndHead;
    mRenderPoints = rhs.mRenderPoints;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this RenderCurve object.
 */
RenderCurve*
RenderCurve::clone() const
{
  return new RenderCurve(*this);
}


/*
 * Destructor for RenderCurve.
 */
RenderCurve::~RenderCurve()
{
}


/*
 * Returns the value of the "startHead" attribute of this RenderCurve.
 */
const std::string&
RenderCurve::getStartHead() const
{
  return mStartHead;
}


/*
 * Returns the value of the "endHead" attribute of this RenderCurve.
 */
const std::string&
RenderCurve::getEndHead() const
{
  return mEndHead;
}


/*
 * Predicate returning @c true if this RenderCurve's "startHead" attribute is
 * set.
 */
bool
RenderCurve::isSetStartHead() const
{
  return (mStartHead.empty() == false && mStartHead != "none");
}


/*
 * Predicate returning @c true if this RenderCurve's "endHead" attribute is
 * set.
 */
bool
RenderCurve::isSetEndHead() const
{
  return (mEndHead.empty() == false && mEndHead != "none");
}


/*
 * Sets the value of the "startHead" attribute of this RenderCurve.
 */
int
RenderCurve::setStartHead(const std::string& startHead)
{
  if (!(SyntaxChecker::isValidInternalSId(startHead)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mStartHead = startHead;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "endHead" attribute of this RenderCurve.
 */
int
RenderCurve::setEndHead(const std::string& endHead)
{
  if (!(SyntaxChecker::isValidInternalSId(endHead)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mEndHead = endHead;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "startHead" attribute of this RenderCurve.
 */
int
RenderCurve::unsetStartHead()
{
  mStartHead.erase();

  if (mStartHead.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "endHead" attribute of this RenderCurve.
 */
int
RenderCurve::unsetEndHead()
{
  mEndHead.erase();

  if (mEndHead.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfCurveElements from this RenderCurve.
 */
const ListOfCurveElements*
RenderCurve::getListOfElements() const
{
  return &mRenderPoints;
}


/*
 * Returns the ListOfCurveElements from this RenderCurve.
 */
ListOfCurveElements*
RenderCurve::getListOfElements()
{
  return &mRenderPoints;
}


/*
 * Get a RenderPoint from the RenderCurve.
 */
RenderPoint*
RenderCurve::getElement(unsigned int n)
{
  return mRenderPoints.get(n);
}


/*
 * Get a RenderPoint from the RenderCurve.
 */
const RenderPoint*
RenderCurve::getElement(unsigned int n) const
{
  return mRenderPoints.get(n);
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
* Returns a const pointer to the curve segment with the given index or NULL if
* the id is invalid.
*
* This method call is deprecated, please use getElement instead.
*
* @param index the index of the curve element to be returned
*
* @return a const pointer to the curve element with the given index or NULL
* if the index was out of bounds.
*/
const RenderPoint* RenderCurve::getCurveElement(unsigned int index) const
{
#ifdef DEPRECATION_WARNINGS
  std::cerr << "Warning. \"const RenderPoint* RenderCurve::getCurveElement(unsigned int index) const\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
  return this->getElement(index);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
* Returns a pointer to the curve segment with the given index or NULL if
* the id is invalid.
*
* This method call is deprecated, please use getElement instead.
*
* @param index the index of the curve element to be returned
*
* @return a pointer to the curve element with the given index or NULL
* if the index was out of bounds.
*/
RenderPoint* RenderCurve::getCurveElement(unsigned int index)
{
#ifdef DEPRECATION_WARNINGS
  std::cerr << "Warning. \"RenderPoint* RenderCurve::getCurveElement(unsigned int index)\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
  return this->getElement(index);
}
/** @endcond */
#endif // OMIT_DEPRECATED


/*
 * Adds a copy of the given RenderPoint to this RenderCurve.
 */
int
RenderCurve::addElement(const RenderPoint* rp)
{
  if (rp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (rp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (rp->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != rp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != rp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(rp)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mRenderPoints.append(rp);
  }
}


/*
 * Get the number of RenderPoint objects in this RenderCurve.
 */
unsigned int
RenderCurve::getNumElements() const
{
  return mRenderPoints.size();
}


/*
 * Creates a new RenderPoint object, adds it to this RenderCurve object and
 * returns the RenderPoint object created.
 */
RenderPoint*
RenderCurve::createPoint()
{
  RenderPoint* rp = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rp = new RenderPoint(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rp != NULL)
  {
    mRenderPoints.appendAndOwn(rp);
  }

  return rp;
}


/*
 * Creates a new RenderCubicBezier object, adds it to this RenderCurve object
 * and returns the RenderCubicBezier object created.
 */
RenderCubicBezier*
RenderCurve::createCubicBezier()
{
  RenderCubicBezier* rcb = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rcb = new RenderCubicBezier(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rcb != NULL)
  {
    mRenderPoints.appendAndOwn(rcb);
  }

  return rcb;
}


/*
 * Removes the nth RenderPoint from this RenderCurve and returns a pointer to
 * it.
 */
RenderPoint*
RenderCurve::removeElement(unsigned int n)
{
  return mRenderPoints.remove(n);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
RenderCurve::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetStartHead() && mStartHead == oldid)
  {
    setStartHead(newid);
  }

  if (isSetEndHead() && mEndHead == oldid)
  {
    setEndHead(newid);
  }
}


/*
 * Returns the XML element name of this RenderCurve object.
 */
const std::string&
RenderCurve::getElementName() const
{
  static const string name = "curve";
  return name;
}


/*
 * Returns the libSBML type code for this RenderCurve object.
 */
int
RenderCurve::getTypeCode() const
{
  return SBML_RENDER_CURVE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * RenderCurve object have been set.
 */
bool
RenderCurve::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive1D::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * RenderCurve object have been set.
 */
bool
RenderCurve::hasRequiredElements() const
{
  bool allPresent = GraphicalPrimitive1D::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
RenderCurve::writeElements(XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeElements(stream);

  if (getNumElements() > 0)
  {
    mRenderPoints.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
RenderCurve::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mRenderPoints.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
RenderCurve::setSBMLDocument(SBMLDocument* d)
{
  GraphicalPrimitive1D::setSBMLDocument(d);

  mRenderPoints.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
RenderCurve::connectToChild()
{
  GraphicalPrimitive1D::connectToChild();

  mRenderPoints.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
RenderCurve::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  GraphicalPrimitive1D::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mRenderPoints.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
* Sets the parent SBML object of this SBML object.
*
* @param sb the SBML object to use
*/
void
RenderCurve::setParentSBMLObject(SBase* sb)
{
  this->mParentSBMLObject = sb;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "startHead")
  {
    value = getStartHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "endHead")
  {
    value = getEndHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this RenderCurve's attribute "attributeName"
 * is set.
 */
bool
RenderCurve::isSetAttribute(const std::string& attributeName) const
{
  bool value = GraphicalPrimitive1D::isSetAttribute(attributeName);

  if (attributeName == "startHead")
  {
    value = isSetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = isSetEndHead();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  if (attributeName == "startHead")
  {
    return_value = setStartHead(value);
  }
  else if (attributeName == "endHead")
  {
    return_value = setEndHead(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this RenderCurve.
 */
int
RenderCurve::unsetAttribute(const std::string& attributeName)
{
  int value = GraphicalPrimitive1D::unsetAttribute(attributeName);

  if (attributeName == "startHead")
  {
    value = unsetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = unsetEndHead();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this RenderCurve.
 */
SBase*
RenderCurve::createChildObject(const std::string& elementName)
{
  GraphicalPrimitive1D* obj = NULL;

  // TO DO

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this RenderCurve.
 */
int
RenderCurve::addChildObject(const std::string& elementName,
                            const SBase* element)
{
  // TO DO

  return -1;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * RenderCurve.
 */
SBase*
RenderCurve::removeChildObject(const std::string& elementName,
                               const std::string& id)
{
  // TO DO

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this RenderCurve.
 */
unsigned int
RenderCurve::getNumObjects(const std::string& elementName)
{
  // TO DO

  return 0;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this RenderCurve.
 */
SBase*
RenderCurve::getObject(const std::string& elementName, unsigned int index)
{
  // TO DO

  return NULL;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
RenderCurve::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mRenderPoints.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
RenderCurve::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRenderPoints.getMetaId() == metaid)
  {
    return &mRenderPoints;
  }

  obj = mRenderPoints.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
RenderCurve::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mRenderPoints, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
* Creates an XMLNode object from this RenderCurve object.
*
* @return the XMLNode with the XML representation for the
* RenderCurve object.
*/
XMLNode RenderCurve::toXML() const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
RenderCurve::createObject(XMLInputStream& stream)
{
  SBase* obj = GraphicalPrimitive1D::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfCurveElements")
  {
    if (mRenderPoints.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render",
        RenderRenderCurveAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = &mRenderPoints;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
RenderCurve::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive1D::addExpectedAttributes(attributes);

  attributes.add("startHead");

  attributes.add("endHead");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
RenderCurve::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GraphicalPrimitive1D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderRenderCurveAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderRenderCurveAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // startHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("startHead", mStartHead);

  if (assigned == true)
  {
    if (mStartHead.empty() == true && log)
    {
      logEmptyString(mStartHead, level, version, "<RenderCurve>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mStartHead) == false && log)
    {
      std::string msg = "The startHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mStartHead + "', which does not conform to the syntax.";
      log->logPackageError("render",
        RenderRenderCurveStartHeadMustBeLineEnding, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }

  // 
  // endHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("endHead", mEndHead);

  if (assigned == true && log)
  {
    if (mEndHead.empty() == true)
    {
      logEmptyString(mEndHead, level, version, "<RenderCurve>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mEndHead) == false)
    {
      std::string msg = "The endHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mEndHead + "', which does not conform to the syntax.";
      log->logPackageError("render", RenderRenderCurveEndHeadMustBeLineEnding,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
RenderCurve::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeAttributes(stream);

  if (isSetStartHead() == true)
  {
    stream.writeAttribute("startHead", getPrefix(), mStartHead);
  }

  if (isSetEndHead() == true)
  {
    stream.writeAttribute("endHead", getPrefix(), mEndHead);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new RenderCurve_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderCurve_t *
RenderCurve_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new RenderCurve(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RenderCurve_t object.
 */
LIBSBML_EXTERN
RenderCurve_t*
RenderCurve_clone(const RenderCurve_t* rc)
{
  if (rc != NULL)
  {
    return static_cast<RenderCurve_t*>(rc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RenderCurve_t object.
 */
LIBSBML_EXTERN
void
RenderCurve_free(RenderCurve_t* rc)
{
  if (rc != NULL)
  {
    delete rc;
  }
}


/*
 * Returns the value of the "startHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
char *
RenderCurve_getStartHead(const RenderCurve_t * rc)
{
  if (rc == NULL)
  {
    return NULL;
  }

  return rc->getStartHead().empty() ? NULL :
    safe_strdup(rc->getStartHead().c_str());
}


/*
 * Returns the value of the "endHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
char *
RenderCurve_getEndHead(const RenderCurve_t * rc)
{
  if (rc == NULL)
  {
    return NULL;
  }

  return rc->getEndHead().empty() ? NULL :
    safe_strdup(rc->getEndHead().c_str());
}


/*
 * Predicate returning @c 1 (true) if this RenderCurve_t's "startHead"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderCurve_isSetStartHead(const RenderCurve_t * rc)
{
  return (rc != NULL) ? static_cast<int>(rc->isSetStartHead()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderCurve_t's "endHead" attribute
 * is set.
 */
LIBSBML_EXTERN
int
RenderCurve_isSetEndHead(const RenderCurve_t * rc)
{
  return (rc != NULL) ? static_cast<int>(rc->isSetEndHead()) : 0;
}


/*
 * Sets the value of the "startHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
int
RenderCurve_setStartHead(RenderCurve_t * rc, const char * startHead)
{
  return (rc != NULL) ? rc->setStartHead(startHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "endHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
int
RenderCurve_setEndHead(RenderCurve_t * rc, const char * endHead)
{
  return (rc != NULL) ? rc->setEndHead(endHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "startHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
int
RenderCurve_unsetStartHead(RenderCurve_t * rc)
{
  return (rc != NULL) ? rc->unsetStartHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "endHead" attribute of this RenderCurve_t.
 */
LIBSBML_EXTERN
int
RenderCurve_unsetEndHead(RenderCurve_t * rc)
{
  return (rc != NULL) ? rc->unsetEndHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing RenderPoint_t objects from this
 * RenderCurve_t.
 */
LIBSBML_EXTERN
ListOf_t*
RenderCurve_getListOfElements(RenderCurve_t* rc)
{
  return (rc != NULL) ? rc->getListOfElements() : NULL;
}


/*
 * Get a RenderPoint_t from the RenderCurve_t.
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_getElement(RenderCurve_t* rc, unsigned int n)
{
  return (rc != NULL) ? rc->getElement(n) : NULL;
}


/*
 * Adds a copy of the given RenderPoint_t to this RenderCurve_t.
 */
LIBSBML_EXTERN
int
RenderCurve_addElement(RenderCurve_t* rc, const RenderPoint_t* rp)
{
  return (rc != NULL) ? rc->addElement(rp) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of RenderPoint_t objects in this RenderCurve_t.
 */
LIBSBML_EXTERN
unsigned int
RenderCurve_getNumElements(RenderCurve_t* rc)
{
  return (rc != NULL) ? rc->getNumElements() : SBML_INT_MAX;
}


/*
 * Creates a new RenderPoint_t object, adds it to this RenderCurve_t object and
 * returns the RenderPoint_t object created.
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_createPoint(RenderCurve_t* rc)
{
  return (rc != NULL) ? rc->createPoint() : NULL;
}


/*
 * Creates a new RenderCubicBezier_t object, adds it to this RenderCurve_t
 * object and returns the RenderCubicBezier_t object created.
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
RenderCurve_createCubicBezier(RenderCurve_t* rc)
{
  return (rc != NULL) ? rc->createCubicBezier() : NULL;
}


/*
 * Removes the nth RenderPoint_t from this RenderCurve_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_removeElement(RenderCurve_t* rc, unsigned int n)
{
  return (rc != NULL) ? rc->removeElement(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderCurve_t object have been set.
 */
LIBSBML_EXTERN
int
RenderCurve_hasRequiredAttributes(const RenderCurve_t * rc)
{
  return (rc != NULL) ? static_cast<int>(rc->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * RenderCurve_t object have been set.
 */
LIBSBML_EXTERN
int
RenderCurve_hasRequiredElements(const RenderCurve_t * rc)
{
  return (rc != NULL) ? static_cast<int>(rc->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


