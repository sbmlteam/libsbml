/**
 * @file    Style.cpp
 * @brief Implementation of the Style class.
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

#include <sbml/packages/render/sbml/Style.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/GlobalStyle.h>
#include <sbml/packages/render/sbml/ListOfGlobalStyles.h>
#include <sbml/packages/render/sbml/LocalStyle.h>
#include <sbml/packages/render/sbml/ListOfLocalStyles.h>

#include <sbml/util/ElementFilter.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/xml/XMLInputStream.h>

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Style using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Style::Style(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
  : SBase(level, version)
    ,mGroup(level,version) 
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Style using the given RenderPkgNamespaces object.
 */
Style::Style(RenderPkgNamespaces *renderns)
  : SBase(renderns)
    ,mGroup(renderns) 
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/*
 * Creates a new Style object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Style object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Style
 * object to be instantiated.
 */
Style::Style(const XMLNode& node, unsigned int l2version) :
    SBase(2,l2version), 
    mGroup(2,l2version)
{
    mURI = RenderExtension::getXmlnsL3V1V1();
    
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
        if(childName=="g")
        {
            this->mGroup=RenderGroup(*child);
            // set the unset values to the defaults
            if(!this->mGroup.isSetStroke())
            {
                this->mGroup.setStroke("none");
            }
            if(!this->mGroup.isSetStrokeWidth())
            {
                this->mGroup.setStrokeWidth(0.0);
            }
            if(!this->mGroup.isSetDashArray())
            {
                this->mGroup.setDashArray(std::vector<unsigned int>());
            }
            if(!this->mGroup.isSetFillColor())
            {
                this->mGroup.setFillColor("none");
            }
            if(!this->mGroup.isSetFillRule())
            {
                this->mGroup.setFillRule(GraphicalPrimitive2D::NONZERO);
            }
            if(!this->mGroup.isSetFontFamily())
            {
                this->mGroup.setFontFamily("sans-serif");
            }
            if(!this->mGroup.isSetFontSize())
            {
                this->mGroup.setFontSize(0);
            }
            if(!this->mGroup.isSetFontWeight())
            {
                this->mGroup.setFontWeight(Text::WEIGHT_NORMAL);
            }
            if(!this->mGroup.isSetFontStyle())
            {
                this->mGroup.setFontStyle(Text::STYLE_NORMAL);
            }
            if(!this->mGroup.isSetStartHead())
            {
                this->mGroup.setStartHead("none");
            }
            if(!this->mGroup.isSetEndHead())
            {
                this->mGroup.setEndHead("none");
            }
        }
        else if(childName=="annotation")
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
 * Constructor which creates a Style with an empty group
 * and empty role and type list.
 * The group has to be filled before the * object is valid.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Style::Style(RenderPkgNamespaces* renderns, const std::string& id):
    SBase(renderns)
//    ,mId(id) 
    ,mGroup(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Style::Style(const std::string& id) is deprecated." << std::endl;
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
 * Copy constructor for Style.
 */
Style::Style(const Style& orig)
  : SBase( orig )
  , mRoleList ( orig.mRoleList )
  , mTypeList ( orig.mTypeList )
  , mGroup ( orig.mGroup )
{
  connectToChild();
}


/*
 * Assignment operator for Style.
 */
Style&
Style::operator=(const Style& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mRoleList = rhs.mRoleList;
    mTypeList = rhs.mTypeList;
    mGroup = rhs.mGroup;

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Style object.
 */
Style*
Style::clone() const
{
  return new Style(*this);
}


/*
 * Destructor for Style.
 */
Style::~Style()
{
}


/*
 * Returns the value of the "id" attribute of this Style.
 */
const std::string&
Style::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Style.
 */
const std::string&
Style::getName() const
{
  return mName;
}

/*
 * Predicate returning @c true if this Style's "id" attribute is set.
 */
bool
Style::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Style's "name" attribute is set.
 */
bool
Style::isSetName() const
{
  return (mName.empty() == false);
}

/*
 * Sets the value of the "id" attribute of this Style.
 */
int
Style::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this Style.
 */
int
Style::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this Style.
 */
int
Style::unsetId()
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
 * Unsets the value of the "name" attribute of this Style.
 */
int
Style::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns a const reference to the role list.
 */
std::set<std::string>& Style::getRoleList()
{
    return this->mRoleList;
}


/*
 * Returns a const reference to the role list.
 */
const std::set<std::string>& Style::getRoleList() const
{
    return this->mRoleList;
}


/*
 * Returns the number of ids in the role list.
 */
unsigned int Style::getNumRoles() const
{
    return (unsigned int)this->mRoleList.size();
}


/*
 * Checks whether a given role is in the role list.
 */
bool Style::isInRoleList(const std::string& id) const
{
    return (this->mRoleList.find(id)!=this->mRoleList.end());
}


/*
 * Adds an id to the role list.
 */
int
Style::addRole(const std::string& id)
{
    this->mRoleList.insert(id);
    return LIBSBML_OPERATION_SUCCESS;
}


/* 
 * @return the string of all roles
 */
std::string 
Style::createRoleString() const
{
  return createStringFromSet(mRoleList);
}


/*
 * Removes the given role from the role list.
 */
int
Style::removeRole(const std::string& id)
{
    this->mRoleList.erase(id);
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the complete role list to a copy of the given list.
 */
int
Style::setRoleList(const std::set<std::string>& roleList)
{
    // I do not check roles on purpose since this is more versatil in the light
    // of future extensions
    this->mRoleList=roleList;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the type list.
 */
const std::set<std::string>& Style::getTypeList() const
{
    return this->mTypeList;
}


/*
 * Returns the type list.
 */
std::set<std::string>& Style::getTypeList()
{
    return this->mTypeList;
}


/*
 * Returns the number of types in the type list.
 */
unsigned int Style::getNumTypes() const
{
    return (unsigned int)this->mTypeList.size();
}


/*
* Checks whether a given type string is in the type list.
*/
bool Style::isInTypeList(const std::string& id) const
{
  return (this->mTypeList.find(id) != this->mTypeList.end());
}


/*
 * Adds a type string to the type list.
 */
int
Style::addType(const std::string& id)
{
    this->mTypeList.insert(id);
    return LIBSBML_OPERATION_SUCCESS;
}


/* 
 * @return the string of all types
 */
std::string 
Style::createTypeString() const
{
  return createStringFromSet(mTypeList);
}


/*
 * Removes a type string from the type list.
 */
int
Style::removeType(const std::string& id)
{
    this->mTypeList.erase(id);
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the complete type list to a copy of the given list.
 */
int
Style::setTypeList(const std::set<std::string>& typeList)
{
    // I do not check types on purpose since this is more versatil in the light
    // of future extensions
    this->mTypeList=typeList;
    return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Returns the value of the "group" element of this Style.
 */
const RenderGroup*
Style::getGroup() const
{
    return &this->mGroup;
}


/*
 * Returns the value of the "group" element of this Style.
 */
RenderGroup*
Style::getGroup()
{
    return &this->mGroup;
}


/*
 * Predicate returning @c true if this Style's "group" element is set.
 */
bool
Style::isSetGroup() const
{
  return (&mGroup != NULL);
}


/*
 * Sets the value of the "group" element of this Style.
 */
int
Style::setGroup(const RenderGroup* group)
{
  if (&mGroup == group)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (group == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    this->mGroup=*group;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new RenderGroup object, adds it to this Style object and returns
 * the RenderGroup object created.
 */
RenderGroup*
Style::createGroup()
{
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());
  RenderGroup *g = new RenderGroup(renderns);

  g->setElementName("g");

  delete renderns;
  this->setGroup(g);

  connectToChild();

  return &mGroup;
}


/*
 * Unsets the value of the "group" element of this Style.
 */
int
Style::unsetGroup()
{
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract Style is of type GlobalStyle
 */
bool
Style::isGlobalStyle() const
{
  return dynamic_cast<const GlobalStyle*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Style is of type LocalStyle
 */
bool
Style::isLocalStyle() const
{
  return dynamic_cast<const LocalStyle*>(this) != NULL;
}


/*
 * Returns the XML element name of this Style object.
 */
const std::string&
Style::getElementName() const
{
  static const string name = "style";
  return name;
}


/*
 * Returns the libSBML type code for this Style object.
 */
int
Style::getTypeCode() const
{
  return SBML_RENDER_STYLE_BASE;
}


/*
 * Predicate returning @c true if all the required attributes for this Style
 * object have been set.
 */
bool
Style::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Style::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  mGroup.write(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Style::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mGroup.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Style::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mGroup.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Style::connectToChild()
{
  SBase::connectToChild();

  mGroup.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Style::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix,
                             bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGroup.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Style.
 */
int
Style::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Style.
 */
int
Style::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Style.
 */
int
Style::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Style.
 */
int
Style::getAttribute(const std::string& attributeName,
                    unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Style.
 */
int
Style::getAttribute(const std::string& attributeName,
                    std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  //else if (attributeName == "roleList")
  //{
  //  value = getRoleList();
  //  return_value = LIBSBML_OPERATION_SUCCESS;
  //}
  //else if (attributeName == "typeList")
  //{
  //  value = getTypeList();
  //  return_value = LIBSBML_OPERATION_SUCCESS;
  //}

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Style's attribute "attributeName" is
 * set.
 */
bool
Style::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  //else if (attributeName == "roleList")
  //{
  //  value = isSetRoleList();
  //}
  //else if (attributeName == "typeList")
  //{
  //  value = isSetTypeList();
  //}

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Style.
 */
int
Style::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Style.
 */
int
Style::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Style.
 */
int
Style::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Style.
 */
int
Style::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Style.
 */
int
Style::setAttribute(const std::string& attributeName,
                    const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  //else if (attributeName == "roleList")
  //{
  //  return_value = setRoleList(value);
  //}
  //else if (attributeName == "typeList")
  //{
  //  return_value = setTypeList(value);
  //}

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Style.
 */
int
Style::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  //else if (attributeName == "roleList")
  //{
  //  value = unsetRoleList();
  //}
  //else if (attributeName == "typeList")
  //{
  //  value = unsetTypeList();
  //}

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Style.
 */
SBase*
Style::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "group")
  {
    return createGroup();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Style.
 */
int
Style::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "group" && element->getTypeCode() == SBML_RENDER_GROUP)
  {
    return setGroup((const RenderGroup*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Style.
 */
SBase*
Style::removeChildObject(const std::string& elementName,
                         const std::string& id)
{
  if (elementName == "group")
  {
    RenderGroup * obj = getGroup();
    if (unsetGroup() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Style.
 */
unsigned int
Style::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "group")
  {
    if (isSetGroup())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Style.
 */
SBase*
Style::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "group")
  {
    return getGroup();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Style::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (&mGroup != NULL)
  {
    if (mGroup.getId() == id)
    {
      return &mGroup;
    }

    obj = mGroup.getElementBySId(id);
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
Style::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (&mGroup != NULL)
  {
    if (mGroup.getMetaId() == metaid)
    {
      return &mGroup;
    }

    obj = mGroup.getElementByMetaId(metaid);
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
Style::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_ELEMENT(ret, sublist, mGroup, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Creates an XMLNode object from this Style object.
 */
XMLNode Style::toXML() const
{
  return getXmlNodeForSBase(this);
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Style::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "g")
  {
    // cannot check this here as a group is always set
    //if (isSetGroup())
    //{
    //  getErrorLog()->logPackageError("render", RenderStyleAllowedElements,
    //    getPackageVersion(), getLevel(), getVersion());
    //}

    RenderGroup* g = new RenderGroup(renderns);
    g->setElementName(name);
    setGroup(g);
    obj = &mGroup;
  }

  delete renderns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Style::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("roleList");

  attributes.add("typeList");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Style::readAttributes(const XMLAttributes& attributes,
  const ExpectedAttributes& expectedAttributes)
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
        log->logPackageError("render", RenderStyleAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderStyleAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (log && assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<Style>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
        "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (log && assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<Style>");
    }
  }

  string elplusid = "<style> element";
  if (!getId().empty()) {
    elplusid += " with the id '" + mId + "'";
  }
  // 
  // roleList string (use = "optional" )
  // 
  readListOfRoles(attributes);

  // 
  // typeList string (use = "optional" )
  // 
  readListOfTypes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Style::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  // write role list
  this->writeRolesList(stream);
  // write type list
  this->writeTypeList(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Style::readListOfRoles(const XMLAttributes& attr)
{
    std::string s;
    attr.readInto("roleList",s, getErrorLog(), false, getLine(), getColumn());
    // seperate the individual elements in the list
    // they are seperated by whitespaces
    if(!s.empty())
    {
        readIntoSet(s,this->mRoleList);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Style::readListOfTypes(const XMLAttributes& attr)
{
    std::string s;
    attr.readInto("typeList",s, getErrorLog(), false, getLine(), getColumn());
    // seperate the individual elements in the list
    // they are seperated by whitespaces
    if(!s.empty())
    {
        readIntoSet(s,this->mTypeList);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Devides a string into individual elements and stores them in the given set.
 */
void Style::readIntoSet(const std::string& s,std::set<std::string>& set)
{
    std::string delimiter="\n\r\t ";
    std::size_t lastPos=s.find_first_not_of(delimiter);
    std::size_t pos;
    while(lastPos!=std::string::npos)
    {
        pos=s.find_first_of(delimiter,lastPos);
        set.insert(s.substr(lastPos,pos-lastPos));
        lastPos=s.find_first_not_of(delimiter,pos);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Concatenates individual elements from a set to a string.
 * The string is returned.
 */
std::string Style::createStringFromSet(const std::set<std::string>& set)
{
    std::ostringstream os;
    std::set<std::string>::const_iterator it=set.begin(),endit=set.end();
    while(it!=endit)
    {
        os << *it << " ";
        ++it;
    }
    if(!os.str().empty())
    {
        os.str(os.str().substr(0,os.str().size()-1));
    }
    return os.str();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds the typeList attribute to an XMLAttributes object.
 */
void Style::addListOfTypes(XMLAttributes& attr) const
{
    std::string s=createStringFromSet(this->mTypeList);
    if(!s.empty())
    {
        attr.add("typeList",s);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds the typeList attribute to an XMLAttributes object.
 */
void Style::addListOfRoles(XMLAttributes& attr) const
{
    std::string s=createStringFromSet(this->mRoleList);
    if(!s.empty())
    {
        attr.add("roleList",s);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Writes the type list to an XML stream.
 */
void Style::writeTypeList(XMLOutputStream& stream) const
{
    std::string s=createStringFromSet(this->mTypeList);
    if(!s.empty())
    {
        stream.writeAttribute("typeList", getPrefix(),  s);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Writes the role list to an XML stream.
 */
void Style::writeRolesList(XMLOutputStream& stream) const
{
    std::string s=createStringFromSet(this->mRoleList);
    if(!s.empty())
    {
        stream.writeAttribute("roleList", getPrefix(),  s);
    }
}
/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new GlobalStyle (Style_t) using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Style_t *
Style_createGlobalStyle(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion)
{
  return new GlobalStyle(level, version, pkgVersion);
}


/*
 * Creates a new LocalStyle (Style_t) using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Style_t *
Style_createLocalStyle(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new LocalStyle(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Style_t object.
 */
LIBSBML_EXTERN
Style_t*
Style_clone(const Style_t* s)
{
  if (s != NULL)
  {
    return static_cast<Style_t*>(s->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Style_t object.
 */
LIBSBML_EXTERN
void
Style_free(Style_t* s)
{
  if (s != NULL)
  {
    delete s;
  }
}


/*
 * Returns the value of the "id" attribute of this Style_t.
 */
LIBSBML_EXTERN
char *
Style_getId(const Style_t * s)
{
  if (s == NULL)
  {
    return NULL;
  }

  return s->getId().empty() ? NULL : safe_strdup(s->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this Style_t.
 */
LIBSBML_EXTERN
char *
Style_getName(const Style_t * s)
{
  if (s == NULL)
  {
    return NULL;
  }

  return s->getName().empty() ? NULL : safe_strdup(s->getName().c_str());
}


///*
// * Returns the value of the "roleList" attribute of this Style_t.
// */
//LIBSBML_EXTERN
//char *
//Style_getRoleList(const Style_t * s)
//{
//  if (s == NULL)
//  {
//    return NULL;
//  }
//
//  return s->getRoleList().empty() ? NULL :
//    safe_strdup(s->getRoleList().c_str());
//}
//
//
///*
// * Returns the value of the "typeList" attribute of this Style_t.
// */
//LIBSBML_EXTERN
//char *
//Style_getTypeList(const Style_t * s)
//{
//  if (s == NULL)
//  {
//    return NULL;
//  }
//
//  return s->getTypeList().empty() ? NULL :
//    safe_strdup(s->getTypeList().c_str());
//}
//
//
/*
 * Predicate returning @c 1 (true) if this Style_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Style_isSetId(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Style_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
Style_isSetName(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_setId(Style_t * s, const char * id)
{
  return (s != NULL) ? s->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_setName(Style_t * s, const char * name)
{
  return (s != NULL) ? s->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "roleList" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_setRoleList(Style_t * s, const char * roleList)
{
  return (s != NULL) ? s->addRole(roleList) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "typeList" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_setTypeList(Style_t * s, const char * typeList)
{
  return (s != NULL) ? s->addType(typeList) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_unsetId(Style_t * s)
{
  return (s != NULL) ? s->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this Style_t.
 */
LIBSBML_EXTERN
int
Style_unsetName(Style_t * s)
{
  return (s != NULL) ? s->unsetName() : LIBSBML_INVALID_OBJECT;
}



/*
 * Returns the value of the "group" element of this Style_t.
 */
LIBSBML_EXTERN
const RenderGroup_t*
Style_getGroup(const Style_t * s)
{
  if (s == NULL)
  {
    return NULL;
  }

  return (RenderGroup_t*)(s->getGroup());
}


/*
 * Predicate returning @c 1 (true) if this Style_t's "group" element is set.
 */
LIBSBML_EXTERN
int
Style_isSetGroup(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->isSetGroup()) : 0;
}


/*
 * Sets the value of the "group" element of this Style_t.
 */
LIBSBML_EXTERN
int
Style_setGroup(Style_t * s, const RenderGroup_t* group)
{
  return (s != NULL) ? s->setGroup(group) : LIBSBML_INVALID_OBJECT;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderGroup_t object, adds it to this Style_t object and
 * returns the RenderGroup_t object created.
 */
LIBSBML_EXTERN
RenderGroup_t*
Style_createGroup(Style_t* s)
{
  if (s == NULL)
  {
    return NULL;
  }

  return (RenderGroup_t*)(s->createGroup());
}
/** @endcond */


/*
 * Unsets the value of the "group" element of this Style_t.
 */
LIBSBML_EXTERN
int
Style_unsetGroup(Style_t * s)
{
  return (s != NULL) ? s->unsetGroup() : LIBSBML_INVALID_OBJECT;
}
/** @endcond */


/*
 * Predicate returning @c 1 if this Style_t is of type GlobalStyle_t
 */
LIBSBML_EXTERN
int
Style_isGlobalStyle(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->isGlobalStyle()) : 0;
}


/*
 * Predicate returning @c 1 if this Style_t is of type LocalStyle_t
 */
LIBSBML_EXTERN
int
Style_isLocalStyle(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->isLocalStyle()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Style_t object have been set.
 */
LIBSBML_EXTERN
int
Style_hasRequiredAttributes(const Style_t * s)
{
  return (s != NULL) ? static_cast<int>(s->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


