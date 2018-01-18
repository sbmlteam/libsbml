/**
 * @file    Style.cpp
 * @brief   abstract base class for local and global styles
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

#include "Style.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <sbml/util/ElementFilter.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/xml/XMLInputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Style object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Style::Style (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    SBase(level,version)
////    ,mId("") 
    ,mGroup(level,version) 
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));  
  connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Style object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Style::Style (RenderPkgNamespaces* renderns):
    SBase(renderns)
////    ,mId("")
    ,mGroup(renderns) 
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */



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
 * Destroy this object.
 */
Style::~Style ()
{
}


List*
Style::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_ELEMENT(ret, sublist, mGroup, filter);  

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the group of the Style.
 *
 * @param group New group to be set on the style.
 * The group is copied.
 */
void Style::setGroup(const RenderGroup* group)
{
    this->mGroup=*group;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the group of the Style.
 *
 * @return const pointer to the group.
 */
const RenderGroup* Style::getGroup() const
{
    return &this->mGroup;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the group of the Style.
 *
 * @return pointer to the group.
 */
RenderGroup* Style::getGroup() 
{
    return &this->mGroup;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of ids in the role list.
 *
 * @return the number of roles in the role list. 
 */
unsigned int Style::getNumRoles() const
{
    return (unsigned int)this->mRoleList.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds an id to the role list.
 *
 * @param role New role to be added to the role list.
 */
void Style::addRole(const std::string& id)
{
    this->mRoleList.insert(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Checks whether a given role is in the role list.
 *
 * @param role role string to check for in the role list.
 */
bool Style::isInRoleList(const std::string& id) const
{
    return (this->mRoleList.find(id)!=this->mRoleList.end());
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Removes the given role from the role list.
 *
 * @param role role string to be removed from the role list.
 */
void Style::removeRole(const std::string& id)
{
    this->mRoleList.erase(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of types in the type list.
 *
 * @return number of types in type list.
 */
unsigned int Style::getNumTypes() const
{
    return (unsigned int)this->mTypeList.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds a type string to the type list.
 *
 * @param type new type string to be added to the type list
 */
void Style::addType(const std::string& id)
{
    this->mTypeList.insert(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Checks whether a given type string is in the type list.
 *
 * @param type string to be searched for in the type list
 *
 * @return true or false depending on whether the given string was
 * found in the type list.
 */
bool Style::isInTypeList(const std::string& id) const
{
    return (this->mTypeList.find(id)!=this->mTypeList.end());
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Removes a type string from the type list.
 *
 * @param type type string to be removed from the type list.
 */
void Style::removeType(const std::string& id)
{
    this->mTypeList.erase(id);
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
void Style::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
    this->mGroup.write(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* Style::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = NULL;

  if (name == "g")
  {
    object = &this->mGroup;
  }
  return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
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
void Style::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    SBase::readAttributes(attributes, expectedAttributes);
    attributes.readInto("id", mId, getErrorLog(), false, getLine(), getColumn());
    attributes.readInto("name", mName, getErrorLog(), false, getLine(), getColumn());
    readListOfRoles(attributes);
    readListOfTypes(attributes);
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
void Style::writeAttributes (XMLOutputStream& stream) const
{
    SBase::writeAttributes(stream);
    stream.writeAttribute("id", getPrefix(), this->mId);
    if(this->isSetName())
    {
        stream.writeAttribute("name", getPrefix(), this->mName);
    }
    // write role list
    this->writeRolesList(stream);
    // write type list
    this->writeTypeList(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Style object.
 *
 * @return the XMLNode with the XML representation for the 
 * Style object.
 */
XMLNode Style::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool Style::accept (SBMLVisitor& v) const
{
    //v.visit(*this);
    return false;
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

/* 
 * @return the string of all roles
 */
std::string 
Style::createRoleString() const
{
  return createStringFromSet(mRoleList);
}

/* 
 * @return the string of all types
 */
std::string 
Style::createTypeString() const
{
  return createStringFromSet(mTypeList);
}

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

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Model defines it as returning "model",
 * CompartmentType defines it as returning "compartmentType", etc.
 */
const std::string& Style::getElementName() const
{
  static std::string name = "style";
  return name;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Style object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Style object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the role list.
 *
 * @return const reference to the role list.
 */
const std::set<std::string>& Style::getRoleList() const
{
    return this->mRoleList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the role list.
 *
 * @return reference to the role list.
 */
std::set<std::string>& Style::getRoleList()
{
    return this->mRoleList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the type list.
 *
 * @return reference to the type list.
 */
std::set<std::string>& Style::getTypeList()
{
    return this->mTypeList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the type list.
 *
 * @return const reference to the type list.
 */
const std::set<std::string>& Style::getTypeList() const
{
    return this->mTypeList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the complete role list to a copy of the given list.
 *
 * @param roleList New list of role strings to be set on the style.
 */
void Style::setRoleList(const std::set<std::string>& roleList)
{
    // I do not check roles on purpose since this is more versatil in the light
    // of future extensions
    this->mRoleList=roleList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the complete type list to a copy of the given list.
 *
 * @param typeList the list of types to be set for the style.
 */
void Style::setTypeList(const std::set<std::string>& typeList)
{
    // I do not check types on purpose since this is more versatil in the light
    // of future extensions
    this->mTypeList=typeList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the value of the "name" attribute of this Style.
 *
 * @return the name of the Style
 */
const std::string& Style::getName () const
{
    return mName;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning @c true or @c false depending on whether this
 * Style's "name" attribute has been set.
 *
 * @return returns true or false depending on whether the name on the 
 * Style has been set.
 */
bool Style::isSetName () const
{
    return (mName.empty() == false);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the value of the "name" attribute of this Style.
 *
 * @param name the new name for the Style 
 *
 * @return status if the operation succeeded
 */
int Style::setName (const std::string& name)
{
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Unsets the value of the "name" attribute of this Style.
 */
int Style::unsetName ()
{
    mName.erase();
    if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the value of the "id" attribute of this Style.
 *
 * @return the id of the Style
 */
const std::string& Style::getId () const
{
    return mId;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning @c true or @c false depending on whether this
 * Style's "id" attribute has been set.
 *
 * @return returns true or false depending on whether the id on the 
 * Style has been set.
 */
bool Style::isSetId () const
{
    return (mId.empty() == false);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the value of the "id" attribute of this Style.
 *
 * @param id the new id for the Style 
 *
 * @return status if the operation succeeded
 */
int Style::setId (const std::string& id)
{
    if (!(SyntaxChecker::isValidSBMLSId(id)))
    {
        return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
        mId = id;
        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Unsets the value of the "id" attribute of this Style.
 */
int Style::unsetId ()
{
    mId.erase();
    if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d The SBMLDocument to set on the objects and it's children if there are any.
 */
    void
Style::setSBMLDocument (SBMLDocument* d)
{
    SBase::setSBMLDocument(d);
    this->mGroup.setSBMLDocument(d);
}
/** @endcond */


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
 */
void
Style::connectToChild()
{
  SBase::connectToChild();
  mGroup.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
Style::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mGroup.enablePackageInternal(pkgURI,pkgPrefix,flag);
}



/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this SBML object.
 *
 * @param sb the SBML object to use
 */
    void 
Style::setParentSBMLObject (SBase* sb)
{
    this->mParentSBMLObject = sb;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool Style::hasRequiredAttributes() const
{
    bool result = this->SBase::hasRequiredAttributes();
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool Style::hasRequiredElements() const 
{
    bool result = this->SBase::hasRequiredElements();
    return result;
}
/** @endcond */



LIBSBML_CPP_NAMESPACE_END 
