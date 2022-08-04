/**
 * @file    LineEnding.cpp
 * @brief   class representing line endings,e.g. arrow heads
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

#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new LineEnding using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LineEnding::LineEnding(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
  : GraphicalPrimitive2D(level, version, pkgVersion)
    ,mEnableRotationalMapping(true)
  , mIsSetEnableRotationalMapping (true)
  , mGroup (NULL)
  , mBoundingBox (NULL)
{
  mGroup = new RenderGroup(level, version, pkgVersion);
  mBoundingBox = new BoundingBox(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new LineEnding using the given RenderPkgNamespaces object.
 */
LineEnding::LineEnding(RenderPkgNamespaces *renderns)
  : GraphicalPrimitive2D(renderns)
    ,mEnableRotationalMapping(true)
  , mIsSetEnableRotationalMapping (true)
  , mGroup (NULL)
  , mBoundingBox (NULL)
{
  mGroup = new RenderGroup(renderns);
  mBoundingBox = new BoundingBox(renderns->getLevel(), renderns->getVersion());
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LineEnding object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LineEnding object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the LineEnding
 * object to be instantiated.
 */
LineEnding::LineEnding(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mGroup(NULL)
  , mBoundingBox(NULL)
{
    const XMLNode* child;
    const XMLAttributes& attributes=node.getAttributes();
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="boundingBox")
        {
            mBoundingBox= new BoundingBox(*child);
        }
        else if(childName=="g")
        {
            mGroup=new RenderGroup(*child);
        }
        ++n;
    }
    if (mBoundingBox == NULL)
    {
        mBoundingBox = new BoundingBox(2, l2version);
    }
    if (mGroup == NULL)
    {
        mGroup = new RenderGroup(2, l2version);
    }

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();

}
/** @endcond */

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LineEnding with an empty group object,
 * and a viewport with a size of 0.
 * The id is set to the given value.
 * In order to get a valid object, the group object has to be valid,
 * the group object has to have descendants other than groups and
 * the viewport has to have a positive size.
 *
 * @param id The id for the LineEnding.
 *
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
LineEnding::LineEnding(RenderPkgNamespaces* renderns, const std::string& id) :
    GraphicalPrimitive2D(renderns)
//    ,mId(id)
    ,mEnableRotationalMapping(true)
//    ,mGroup(renderns)    
{
  mGroup = new RenderGroup(renderns);
  mBoundingBox = new BoundingBox(renderns->getLevel(), renderns->getVersion());

    //this->mBoundingBox.setParentSBMLObject(this);
    //this->mBoundingBox.setIsInRenderContext(true);
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LineEnding::LineEnding(const std::string& id) is deprecated." << std::endl;
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
 * Copy constructor for LineEnding.
 */
LineEnding::LineEnding(const LineEnding& orig)
  : GraphicalPrimitive2D( orig )
  , mEnableRotationalMapping ( orig.mEnableRotationalMapping )
  , mIsSetEnableRotationalMapping ( orig.mIsSetEnableRotationalMapping )
  , mGroup ( NULL )
  , mBoundingBox ( NULL )
{
  if (orig.mGroup != NULL)
  {
    mGroup = orig.mGroup->clone();
  }

  if (orig.mBoundingBox != NULL)
  {
    mBoundingBox = orig.mBoundingBox->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for LineEnding.
 */
LineEnding&
LineEnding::operator=(const LineEnding& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive2D::operator=(rhs);
    mEnableRotationalMapping = rhs.mEnableRotationalMapping;
    mIsSetEnableRotationalMapping = rhs.mIsSetEnableRotationalMapping;
    delete mGroup;
    if (rhs.mGroup != NULL)
    {
      mGroup = rhs.mGroup->clone();
    }
    else
    {
      mGroup = NULL;
    }

    delete mBoundingBox;
    if (rhs.mBoundingBox != NULL)
    {
      mBoundingBox = rhs.mBoundingBox->clone();
    }
    else
    {
      mBoundingBox = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this LineEnding object.
 */
LineEnding*
LineEnding::clone() const
{
  return new LineEnding(*this);
}


/*
 * Destructor for LineEnding.
 */
LineEnding::~LineEnding()
{
  delete mGroup;
  mGroup = NULL;
  delete mBoundingBox;
  mBoundingBox = NULL;
}


/*
 * Returns the value of the "id" attribute of this LineEnding.
 */
const std::string&
LineEnding::getId() const
{
  return mId;
}


/*
 * Returns the value of the "enableRotationalMapping" attribute of this
 * LineEnding.
 */
bool
LineEnding::getEnableRotationalMapping() const
{
  return mEnableRotationalMapping;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns whether rotational mapping is enabled or not.
 *
 * @return bool value that specifies if rotational mapping is 
 * enabled for the LineEnding or not.
 */
bool LineEnding::getIsEnabledRotationalMapping() const
{
    return this->mEnableRotationalMapping;
}
/** @endcond */

/*
 * Predicate returning @c true if this LineEnding's "id" attribute is set.
 */
bool
LineEnding::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this LineEnding's "enableRotationalMapping"
 * attribute is set.
 */
bool
LineEnding::isSetEnableRotationalMapping() const
{
  return mIsSetEnableRotationalMapping;
}


/*
 * Sets the value of the "id" attribute of this LineEnding.
 */
int
LineEnding::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "enableRotationalMapping" attribute of this
 * LineEnding.
 */
int
LineEnding::setEnableRotationalMapping(bool enableRotationalMapping)
{
  mEnableRotationalMapping = enableRotationalMapping;
  mIsSetEnableRotationalMapping = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this LineEnding.
 */
int
LineEnding::unsetId()
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
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * LineEnding.
 */
int
LineEnding::unsetEnableRotationalMapping()
{
  mEnableRotationalMapping = false;
  mIsSetEnableRotationalMapping = false;

  if (isSetEnableRotationalMapping() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "group" element of this LineEnding.
 */
const RenderGroup*
LineEnding::getGroup() const
{
  return mGroup;
}


/*
 * Returns the value of the "group" element of this LineEnding.
 */
RenderGroup*
LineEnding::getGroup()
{
  return mGroup;
}


/*
 * Returns the value of the "boundingBox" element of this LineEnding.
 */
const BoundingBox*
LineEnding::getBoundingBox() const
{
  return mBoundingBox;
}


/*
 * Returns the value of the "boundingBox" element of this LineEnding.
 */
BoundingBox*
LineEnding::getBoundingBox()
{
  return mBoundingBox;
}


/*
 * Predicate returning @c true if this LineEnding's "group" element is set.
 */
bool
LineEnding::isSetGroup() const
{
  return (mGroup != NULL);
}


/*
 * Predicate returning @c true if this LineEnding's "boundingBox" element is
 * set.
 */
bool
LineEnding::isSetBoundingBox() const
{
  return (mBoundingBox != NULL);
}


/*
 * Sets the value of the "group" element of this LineEnding.
 */
int
LineEnding::setGroup(const RenderGroup* group)
{
  if (mGroup == group)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (group == NULL)
  {
    delete mGroup;
    mGroup = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mGroup;
    mGroup = (group != NULL) ? group->clone() : NULL;
    if (mGroup != NULL)
    {
      //render - FIX_ME
//      mGroup->setElementName("group");
      mGroup->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "boundingBox" element of this LineEnding.
 */
int
LineEnding::setBoundingBox(const BoundingBox* boundingBox)
{
  if (mBoundingBox == boundingBox)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (boundingBox == NULL)
  {
    delete mBoundingBox;
    mBoundingBox = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mBoundingBox;
    mBoundingBox = (boundingBox != NULL) ? boundingBox->clone() : NULL;
    if (mBoundingBox != NULL)
    {
      mBoundingBox->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new RenderGroup object, adds it to this LineEnding object and
 * returns the RenderGroup object created.
 */
RenderGroup*
LineEnding::createGroup()
{
  if (mGroup != NULL)
  {
    delete mGroup;
  }

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());
  mGroup = new RenderGroup(renderns);

  //render - FIX_ME
//  mGroup->setElementName("g");

  delete renderns;

  connectToChild();

  return mGroup;
}


/*
 * Creates a new BoundingBox object, adds it to this LineEnding object and
 * returns the BoundingBox object created.
 */
BoundingBox*
LineEnding::createBoundingBox()
{
  if (mBoundingBox != NULL)
  {
    delete mBoundingBox;
  }

  LAYOUT_CREATE_NS(layoutns, getSBMLNamespaces());
  mBoundingBox = new BoundingBox(layoutns);

  delete layoutns;

  connectToChild();

  return mBoundingBox;
}


/*
 * Unsets the value of the "group" element of this LineEnding.
 */
int
LineEnding::unsetGroup()
{
  delete mGroup;
  mGroup = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "boundingBox" element of this LineEnding.
 */
int
LineEnding::unsetBoundingBox()
{
  delete mBoundingBox;
  mBoundingBox = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the libSBML type code for this LineEnding object.
 */
int
LineEnding::getTypeCode() const
{
  return SBML_RENDER_LINEENDING;
}


/*
 * Returns the XML element name of this LineEnding object.
 */
const std::string&
LineEnding::getElementName() const
{
  static const string name = "lineEnding";
  return name;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * LineEnding object have been set.
 */
bool
LineEnding::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive2D::hasRequiredAttributes();

  if (isSetId() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this LineEnding
 * object have been set.
 */
bool
LineEnding::hasRequiredElements() const
{
  bool allPresent = GraphicalPrimitive2D::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
LineEnding::writeElements(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeElements(stream);

  if (isSetBoundingBox() == true)
  {
    mBoundingBox->write(stream);
  }

  if (isSetGroup() == true)
  {
    mGroup->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
LineEnding::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mGroup != NULL)
  {
    mGroup->accept(v);
  }

  if (mBoundingBox != NULL)
  {
    mBoundingBox->accept(v);
  }

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
LineEnding::setSBMLDocument(SBMLDocument* d)
{
  GraphicalPrimitive2D::setSBMLDocument(d);

  if (mGroup != NULL)
  {
    mGroup->setSBMLDocument(d);
  }

  if (mBoundingBox != NULL)
  {
    mBoundingBox->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
LineEnding::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();

  if (mGroup != NULL)
  {
    mGroup->connectToParent(this);
  }

  if (mBoundingBox != NULL)
  {
    mBoundingBox->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
LineEnding::enablePackageInternal(const std::string& pkgURI,
                                  const std::string& pkgPrefix,
                                  bool flag)
{
  GraphicalPrimitive2D::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetGroup())
  {
    mGroup->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetBoundingBox())
  {
    mBoundingBox->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "enableRotationalMapping")
  {
    value = getEnableRotationalMapping();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::getAttribute(const std::string& attributeName,
                         double& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::getAttribute(const std::string& attributeName,
                         unsigned int& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::getAttribute(const std::string& attributeName,
                         std::string& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this LineEnding's attribute "attributeName"
 * is set.
 */
bool
LineEnding::isSetAttribute(const std::string& attributeName) const
{
  bool value = GraphicalPrimitive2D::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "enableRotationalMapping")
  {
    value = isSetEnableRotationalMapping();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  if (attributeName == "enableRotationalMapping")
  {
    return_value = setEnableRotationalMapping(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::setAttribute(const std::string& attributeName,
                         const std::string& value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this LineEnding.
 */
int
LineEnding::unsetAttribute(const std::string& attributeName)
{
  int value = GraphicalPrimitive2D::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "enableRotationalMapping")
  {
    value = unsetEnableRotationalMapping();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this LineEnding.
 */
SBase*
LineEnding::createChildObject(const std::string& elementName)
{
  GraphicalPrimitive2D* obj = NULL;

  if (elementName == "group")
  {
    return createGroup();
  }
  else if (elementName == "boundingBox")
  {
    return createBoundingBox();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this LineEnding.
 */
int
LineEnding::addChildObject(const std::string& elementName,
                           const SBase* element)
{
  if (elementName == "group" && element->getTypeCode() == SBML_RENDER_GROUP)
  {
    return setGroup((const RenderGroup*)(element));
  }
  else if (elementName == "boundingBox" && element->getTypeCode() ==
    SBML_RENDER_GROUP)
  {
    return setBoundingBox((const BoundingBox*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * LineEnding.
 */
SBase*
LineEnding::removeChildObject(const std::string& elementName,
                              const std::string& id)
{
  if (elementName == "group")
  {
    RenderGroup * obj = getGroup();
    if (unsetGroup() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "boundingBox")
  {
    BoundingBox * obj = getBoundingBox();
    if (unsetBoundingBox() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this LineEnding.
 */
unsigned int
LineEnding::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "group")
  {
    if (isSetGroup())
    {
      return 1;
    }
  }
  else if (elementName == "boundingBox")
  {
    if (isSetBoundingBox())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this LineEnding.
 */
SBase*
LineEnding::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "group")
  {
    return getGroup();
  }
  else if (elementName == "boundingBox")
  {
    return getBoundingBox();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
LineEnding::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGroup != NULL)
  {
    if (mGroup->getId() == id)
    {
      return mGroup;
    }

    obj = mGroup->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mBoundingBox != NULL)
  {
    if (mBoundingBox->getId() == id)
    {
      return mBoundingBox;
    }

    obj = mBoundingBox->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
LineEnding::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGroup != NULL)
  {
    if (mGroup->getMetaId() == metaid)
    {
      return mGroup;
    }

    obj = mGroup->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mBoundingBox != NULL)
  {
    if (mBoundingBox->getMetaId() == metaid)
    {
      return mBoundingBox;
    }

    obj = mBoundingBox->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
LineEnding::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mGroup, filter);
  ADD_FILTERED_POINTER(ret, sublist, mBoundingBox, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this LineEnding object.
 *
 * @return the XMLNode with the XML representation for the 
 * LineEnding object.
 */
XMLNode LineEnding::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
LineEnding::createObject(XMLInputStream& stream)
{
  SBase* obj = GraphicalPrimitive2D::createObject(stream);

  const std::string& name = stream.peek().getName();

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  LAYOUT_CREATE_NS(layoutns, getSBMLNamespaces());

  if (name == "g")
  {
    //render - FIX_ME
    //hack because original render code always creates a group when it create a line ending
    //if (isSetGroup())
    //{
    //  getErrorLog()->logPackageError("render", RenderLineEndingAllowedElements,
    //    getPackageVersion(), getLevel(), getVersion());
    //}

    delete mGroup;
    mGroup = new RenderGroup(renderns);
    //render - FIX_ME
    mGroup->setElementName(name);
    obj = mGroup;
  }
  else if (name == "boundingBox")
  {
    //hack because original render code always creates a boundng box when it create a line ending
    if (isSetBoundingBox() && mBoundingBox->getDimensionsExplicitlySet() && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render", RenderLineEndingAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }

    delete mBoundingBox;
    mBoundingBox = new BoundingBox(layoutns);
    obj = mBoundingBox;
  }

  delete renderns;

  delete layoutns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
LineEnding::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("enableRotationalMapping");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
LineEnding::readAttributes(const XMLAttributes& attributes,
                           const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfLineEndings*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderLineEndingAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRenderInformationBaseLOLineEndingsAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderLineEndingAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderLineEndingAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (log && mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<LineEnding>");
    }
    else if (log && SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Render attribute 'id' is missing from the "
      "<LineEnding> element.";
    if (log)
    {
      log->logPackageError("render", RenderLineEndingAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // enableRotationalMapping bool (use = "optional" )
  // 
  if (log)
  {
    numErrs = log->getNumErrors();
  }

  mIsSetEnableRotationalMapping =
    attributes.readInto("enableRotationalMapping", mEnableRotationalMapping);

  if (mIsSetEnableRotationalMapping == false)
  {
    if (log && log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("render",
        RenderLineEndingEnableRotationalMappingMustBeBoolean, pkgVersion, level,
          version, "", getLine(), getColumn());
    }
    else
    {
      mEnableRotationalMapping = true;
//      mIsSetEnableRotationalMapping = true;
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
LineEnding::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);

  //if (isSetId() == true)
  //{
  //  stream.writeAttribute("id", getPrefix(), mId);
  //}

  //render - FIX_ME
  // not writing out defaults so need to write default Value
  if (isSetEnableRotationalMapping() == true && getEnableRotationalMapping() == false)
  {
    stream.writeAttribute("enableRotationalMapping", getPrefix(),
      mEnableRotationalMapping);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new LineEnding_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
LineEnding_t *
LineEnding_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion)
{
  return new LineEnding(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this LineEnding_t object.
 */
LIBSBML_EXTERN
LineEnding_t*
LineEnding_clone(const LineEnding_t* le)
{
  if (le != NULL)
  {
    return static_cast<LineEnding_t*>(le->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this LineEnding_t object.
 */
LIBSBML_EXTERN
void
LineEnding_free(LineEnding_t* le)
{
  if (le != NULL)
  {
    delete le;
  }
}


/*
 * Returns the value of the "id" attribute of this LineEnding_t.
 */
LIBSBML_EXTERN
char *
LineEnding_getId(const LineEnding_t * le)
{
  if (le == NULL)
  {
    return NULL;
  }

  return le->getId().empty() ? NULL : safe_strdup(le->getId().c_str());
}


/*
 * Returns the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_getEnableRotationalMapping(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->getEnableRotationalMapping()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LineEnding_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
LineEnding_isSetId(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LineEnding_t's
 * "enableRotationalMapping" attribute is set.
 */
LIBSBML_EXTERN
int
LineEnding_isSetEnableRotationalMapping(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->isSetEnableRotationalMapping()) :
    0;
}


/*
 * Sets the value of the "id" attribute of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_setId(LineEnding_t * le, const char * id)
{
  return (le != NULL) ? le->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_setEnableRotationalMapping(LineEnding_t * le,
                                      int enableRotationalMapping)
{
  return (le != NULL) ? le->setEnableRotationalMapping(enableRotationalMapping)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_unsetId(LineEnding_t * le)
{
  return (le != NULL) ? le->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_unsetEnableRotationalMapping(LineEnding_t * le)
{
  return (le != NULL) ? le->unsetEnableRotationalMapping() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "group" element of this LineEnding_t.
 */
LIBSBML_EXTERN
const RenderGroup_t*
LineEnding_getGroup(const LineEnding_t * le)
{
  if (le == NULL)
  {
    return NULL;
  }

  return (RenderGroup_t*)(le->getGroup());
}


/*
 * Returns the value of the "boundingBox" element of this LineEnding_t.
 */
LIBSBML_EXTERN
const BoundingBox_t*
LineEnding_getBoundingBox(const LineEnding_t * le)
{
  if (le == NULL)
  {
    return NULL;
  }

  return (BoundingBox_t*)(le->getBoundingBox());
}


/*
 * Predicate returning @c 1 (true) if this LineEnding_t's "group" element is
 * set.
 */
LIBSBML_EXTERN
int
LineEnding_isSetGroup(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->isSetGroup()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this LineEnding_t's "boundingBox" element
 * is set.
 */
LIBSBML_EXTERN
int
LineEnding_isSetBoundingBox(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->isSetBoundingBox()) : 0;
}


/*
 * Sets the value of the "group" element of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_setGroup(LineEnding_t * le, const RenderGroup_t* group)
{
  return (le != NULL) ? le->setGroup(group) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "boundingBox" element of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_setBoundingBox(LineEnding_t * le, const BoundingBox_t* boundingBox)
{
  return (le != NULL) ? le->setBoundingBox(boundingBox) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new RenderGroup_t object, adds it to this LineEnding_t object and
 * returns the RenderGroup_t object created.
 */
LIBSBML_EXTERN
RenderGroup_t*
LineEnding_createGroup(LineEnding_t* le)
{
  if (le == NULL)
  {
    return NULL;
  }

  return (RenderGroup_t*)(le->createGroup());
}


/*
 * Creates a new BoundingBox_t object, adds it to this LineEnding_t object and
 * returns the BoundingBox_t object created.
 */
LIBSBML_EXTERN
BoundingBox_t*
LineEnding_createBoundingBox(LineEnding_t* le)
{
  if (le == NULL)
  {
    return NULL;
  }

  return (BoundingBox_t*)(le->createBoundingBox());
}


/*
 * Unsets the value of the "group" element of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_unsetGroup(LineEnding_t * le)
{
  return (le != NULL) ? le->unsetGroup() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "boundingBox" element of this LineEnding_t.
 */
LIBSBML_EXTERN
int
LineEnding_unsetBoundingBox(LineEnding_t * le)
{
  return (le != NULL) ? le->unsetBoundingBox() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LineEnding_t object have been set.
 */
LIBSBML_EXTERN
int
LineEnding_hasRequiredAttributes(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * LineEnding_t object have been set.
 */
LIBSBML_EXTERN
int
LineEnding_hasRequiredElements(const LineEnding_t * le)
{
  return (le != NULL) ? static_cast<int>(le->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


