/**
 * @file    GlobalRenderInformation.cpp
 * @brief   class for the storage of global render information
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

#include "GlobalRenderInformation.h"

#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/DefaultValues.h>

#include <algorithm>
#include <sstream>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/util/ElementFilter.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string ListOfGlobalRenderInformation::ELEMENT_NAME="listOfGlobalRenderInformation";
const std::string GlobalRenderInformation::ELEMENT_NAME="renderInformation";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalRenderInformation object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
GlobalRenderInformation::GlobalRenderInformation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : RenderInformationBase(level,version, pkgVersion)
  , mListOfStyles(level, version, pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalRenderInformation object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
GlobalRenderInformation::GlobalRenderInformation (RenderPkgNamespaces* renderns):
    RenderInformationBase(renderns), mListOfStyles(renderns)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */

    
/*
 * Destroy this object.
 */
GlobalRenderInformation::~GlobalRenderInformation ()
{
}



/** @cond doxygenLibsbmlInternal */
/*
 * Parses the xml information in the given node and sets the attributes.
 * This method should never be called by the user. It is only used to read render 
 * information from annotations.
 *
 * @param node the XMLNode object reference that describes the GlobalRenderInformation
 * object to be instantiated.
 */
void GlobalRenderInformation::parseXML(const XMLNode& node)
{
    this->RenderInformationBase::parseXML(node);
    const XMLNode* child;
    unsigned int n=0,nMax = node.getNumChildren();
    const XMLAttributes& attributes=node.getAttributes();
        ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(attributes, ea);
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfStyles")
        {
            this->mListOfStyles=ListOfGlobalStyles(*child);
            this->mListOfStyles.setSBMLDocument(this->mSBML);
        }
        ++n;
    }

}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GlobalRenderInformation with the given @p id
 * and all lists empty.
 *
 * @param id the new id for the GlobalRenderInformation.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GlobalRenderInformation::GlobalRenderInformation(RenderPkgNamespaces* renderns, const std::string& id)
  : RenderInformationBase(renderns,id), mListOfStyles(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GlobalRenderInformation::GlobalRenderInformation(const std::string& id) is deprecated." << std::endl;
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

List*
GlobalRenderInformation::getAllElements(ElementFilter* filter)
{
  List* ret = RenderInformationBase::getAllElements(filter);
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfStyles, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of styles.
 *
 * @return the number of global styles in the global render information object
 */
unsigned int GlobalRenderInformation::getNumStyles() const
{
    return this->mListOfStyles.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the ListOfGlobalStyles object.
 *
 * @return pointer to the list of global styles.
 */
ListOfGlobalStyles* GlobalRenderInformation::getListOfStyles()
{
    return &(this->mListOfStyles);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the ListOfGlobalStyles object.
 *
 * @return const pointer to the list of global styles.
 */
const ListOfGlobalStyles* GlobalRenderInformation::getListOfStyles() const
{
    return &(this->mListOfStyles);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the style with the given @p id.
 * If the id is invalid, @c NULL is returned.
 * 
 * @param id id of the GlobalStyle to be returned.
 * 
 * @return pointer to the style with the given @p id or @c NULL
 */
GlobalStyle* GlobalRenderInformation::getStyle(const std::string& id)
{
    return this->mListOfStyles.get(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the style with the given @p id.
 * If the id is invalid, @c NULL is returned.
 * 
 * @param id id of the GlobalStyle to be returned.
 * 
 * @return const pointer to the style with the given @p id or @c NULL
 */
const GlobalStyle* GlobalRenderInformation::getStyle(const std::string& id) const
{
    return this->mListOfStyles.get(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the style with the given index.
 * If the index is invalid, @c NULL is returned.
 * 
 * @param i index of the GlobalStyle to be returned.
 * 
 * @return pointer to the style with the given index or NULL
 */
GlobalStyle* GlobalRenderInformation::getStyle(unsigned int i)
{
    return (i<this->mListOfStyles.size())?static_cast<GlobalStyle*>(this->mListOfStyles.get(i)):NULL;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the style with the given index.
 * If the index is invalid, @c NULL is returned.
 * 
 * @param i index of the GlobalStyle to be returned.
 * 
 * @return const pointer to the style with the given index or NULL
 */
const GlobalStyle* GlobalRenderInformation::getStyle(unsigned int i) const
{
    return (i<this->mListOfStyles.size())?static_cast<const GlobalStyle*>(this->mListOfStyles.get(i)):NULL;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalStyle object. The object is added to and owned
 * by the GlobalRenderInformation object.
 * 
 * @param id for the new style.
 * 
 * @ return a pointer to the newly created GlobalStyle object.
 */
GlobalStyle* GlobalRenderInformation::createStyle(const std::string& id)
{
    GlobalStyle* pStyle=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pStyle = new GlobalStyle(renderns);
      pStyle->setId(id);
	 delete renderns;
    }
    catch (...)
    {
        /* here we do not create a default object as the level/version must
         * match the parent object
         *
         * so do nothing
         */
    }

    if(pStyle != NULL)
    {
        this->mListOfStyles.appendAndOwn(pStyle);
    }
    return pStyle;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of a GlobalStyle to the GlobalRenderInformation object.
 * The style is only added if it is valid, i.e. it has to have an id and
 * a valid group.
 *
 * @param pointer to the global style object to be added.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 *
 * @note This method should be used with some caution.  The fact that
 * this method @em copies the object passed to it means that the caller
 * will be left holding a physically different object instance than the
 * one contained in this GlobalRenderInformation.  Changes made to the original object
 * instance (such as resetting attribute values) will <em>not affect the
 * instance in the GlobalRenderInformation</em>.  In addition, the caller should make
 * sure to free the original object if it is no longer being used, or
 * else a memory leak will result.  Please see GlobalRenderInformation::createStyle()
 * for a method that does not lead to these issues.
 *
 * @see createStyle()
 */
int GlobalRenderInformation::addStyle(const GlobalStyle* pStyle)
{
    if (pStyle == NULL)
    {
        return LIBSBML_OPERATION_FAILED;
    }
    else if (!(pStyle->hasRequiredAttributes()) || !(pStyle->hasRequiredElements()))
    {
        return LIBSBML_INVALID_OBJECT;
    }
    else if (getLevel() != pStyle->getLevel())
    {
        return LIBSBML_LEVEL_MISMATCH;
    }
    else if (getVersion() != pStyle->getVersion())
    {
        return LIBSBML_VERSION_MISMATCH;
    }
    else if (pStyle->isSetId() 
            && (getListOfStyles()->get(pStyle->getId())) != NULL)
    {
        // an object with this id already exists
        return LIBSBML_DUPLICATE_OBJECT_ID;
    }
    else
    {

        this->mListOfStyles.append(pStyle);

        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void GlobalRenderInformation::writeElements (XMLOutputStream& stream) const
{
    RenderInformationBase::writeElements(stream);
    if ( this->mListOfStyles.size() > 0 ) this->mListOfStyles.write(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
GlobalRenderInformation* 
GlobalRenderInformation::clone () const
{
    return new GlobalRenderInformation(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
SBase*
GlobalRenderInformation::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "listOfStyles")
  {
    object = &this->mListOfStyles;
  }
  else
  {
      object=this->RenderInformationBase::createObject(stream);
  }
  return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
GlobalRenderInformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  RenderInformationBase::addExpectedAttributes(attributes);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void GlobalRenderInformation::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->RenderInformationBase::readAttributes(attributes, ea);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void GlobalRenderInformation::writeAttributes (XMLOutputStream& stream) const
{
    RenderInformationBase::writeAttributes(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this GlobalRenderInformation object.
 *
 * @return the XMLNode with the XML representation for the 
 * GlobalRenderInformation object.
 *
 */
XMLNode GlobalRenderInformation::toXML() const
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
GlobalRenderInformation::getTypeCode () const
{
    return SBML_RENDER_GLOBALRENDERINFORMATION;
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
bool GlobalRenderInformation::accept (SBMLVisitor& v) const
{
    //v.visit(*this);
    return false;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the ListOfGlobalRenderInformation object.
 *
 * @return a (deep) copy of this ListOfGlobalRenderInformation
 */
ListOfGlobalRenderInformation* ListOfGlobalRenderInformation::clone () const
{
    return new ListOfGlobalRenderInformation(*this);
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Parses the xml information in the given node and sets the attributes.
 * This method should never be called by the user. It is only used to read render 
 * information from annotations.
 *
 * @param node the XMLNode object reference that describes the ListOfGlobalRenderInformation
 * object to be instantiated.
 */
void ListOfGlobalRenderInformation::parseXML(const XMLNode& node)
{
    const XMLNode* child;
    unsigned int n=0,nMax = node.getNumChildren();
    const XMLAttributes& attributes=node.getAttributes();
        ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="renderInformation")
        {
            GlobalRenderInformation* pGRI=new GlobalRenderInformation(this->getLevel(),this->getVersion());
            pGRI->parseXML(*child);
            this->appendAndOwn(pGRI);
        }
        ++n;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Copy constructor for ListOfGlobalRenderInformation objects.
 */ 
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(const ListOfGlobalRenderInformation& source)
  : ListOf(source)
  , mVersionMajor(source.mVersionMajor)
  , mIsSetVersionMajor(source.mIsSetVersionMajor)
  , mVersionMinor(source.mVersionMinor)
  , mIsSetVersionMinor(source.mIsSetVersionMinor)
  , mDefaultValues(NULL)
{
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for ListOfGlobalRenderInformationObjects.
 */
ListOfGlobalRenderInformation& ListOfGlobalRenderInformation::operator=(const ListOfGlobalRenderInformation& source)
{
    if(&source!=this)
    {
        this->ListOf::operator=(source);
        mVersionMajor = source.mVersionMajor;
        mIsSetVersionMajor = source.mIsSetVersionMajor;
        mVersionMinor = source.mVersionMinor;
        mIsSetVersionMinor = source.mIsSetVersionMinor;
        setDefaultValues(source.getDefaultValues());
    }
    return *this;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for the objects contained in this ListOf
 * (i.e., GradientDefinition objects, if the list is non-empty).
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
 * @return the SBML type code for the objects contained in this ListOf
 * instance, or @c SBML_UNKNOWN (default).
 *
 * @see getElementName()
 */
int ListOfGlobalRenderInformation::getItemTypeCode () const
{
    return SBML_RENDER_GLOBALRENDERINFORMATION;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * ListOfGlobalRenderInformation, is always @c "listOfGlobalRenderInformation".
 * 
 * @return the name of this element, i.e., @c "listOfGlobalRenderInformation".
 */
const std::string& ListOfGlobalRenderInformation::getElementName () const
{
  static std::string name = ListOfGlobalRenderInformation::ELEMENT_NAME;
  return name;
}
/** @endcond */


/*
 * Ctor.
 */
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(RenderPkgNamespaces* renderns)
 : ListOf(renderns)
 , mVersionMajor(1)
 , mIsSetVersionMajor(false)
 , mVersionMinor(0)
 , mIsSetVersionMinor(false)
 , mDefaultValues(NULL)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(renderns->getURI());
}


/*
 * Ctor.
 */
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
 , mVersionMajor(1)
 , mIsSetVersionMajor(false)
 , mVersionMinor(0)
 , mIsSetVersionMinor(false)
 , mDefaultValues(NULL)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));
}

ListOfGlobalRenderInformation::~ListOfGlobalRenderInformation()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * GlobalRenderInformation, is always @c "renderInformation".
 * 
 * @return the name of this element, i.e., @c "renderInformation".
 */
const std::string& GlobalRenderInformation::getElementName () const
{
  static std::string name = GlobalRenderInformation::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
ListOfGlobalRenderInformation::writeXMLNS (XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  xmlns.add(getURI(), getPrefix());
  stream << xmlns;
}
/** @endcond */

void 
ListOfGlobalRenderInformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("versionMajor");
  attributes.add("versionMinor");
}

/** @cond doxygenLibsbmlInternal */
void ListOfGlobalRenderInformation::writeAttributes (XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  if (isSetVersionMajor() == true)
  {
    stream.writeAttribute("versionMajor", getPrefix(), mVersionMajor);
  }

  if (isSetVersionMinor() == true)
  {
    stream.writeAttribute("versionMinor", getPrefix(), mVersionMinor);
  }

  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfGlobalRenderInformation object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfGlobalRenderInformation object.
 */
XMLNode ListOfGlobalRenderInformation::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* ListOfGlobalRenderInformation::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL;


    if (name == "renderInformation")
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      object = new GlobalRenderInformation(renderns);
      if(object != NULL) this->mItems.push_back(object);
	    delete renderns;
    }

    if (name == "defaultValues")
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      DefaultValues newDV(renderns);
      setDefaultValues(&newDV);
      object = getDefaultValues();
      delete renderns;
    }

    return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the version of the render information list.
 * The version consists of a major and a minor version number.
 *
 * @param major major version number
 * @param minor minor version number
 */
void ListOfGlobalRenderInformation::setVersion(unsigned int major,unsigned int minor)
{
  setVersionMajor(major);
  setVersionMinor(minor);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the major version of the render information list.
 *
 * @return the major version number of the global render information list
 */
unsigned int ListOfGlobalRenderInformation::getMajorVersion() const
{
    return this->mVersionMajor;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the minor version of the render information list.
 *
 * @return the minor version number of the global render information list
 */
unsigned int ListOfGlobalRenderInformation::getMinorVersion() const
{
    return this->mVersionMinor;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the version as a string.
 *
 * @return the version of the GlobalRenderInformation object
 * as a string
 */
std::string ListOfGlobalRenderInformation::getVersionString() const
{
    std::ostringstream os;
    os << this->mVersionMajor << "." << this->mVersionMinor;
    return os.str();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the GlobalRenderInformation with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the GlobalRenderInformation object to be returned
 * 
 * @return pointer to the GlobalRenderInformation at the given index or NULL.
 */
GlobalRenderInformation* ListOfGlobalRenderInformation::get(unsigned int i)
{
    return static_cast<GlobalRenderInformation*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the GlobalRenderInformation with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the GlobalRenderInformation object to be returned
 * 
 * @return const pointer to the GlobalRenderInformation at the given index or NULL.
 */
const GlobalRenderInformation* ListOfGlobalRenderInformation::get(unsigned int i) const
{
    return static_cast<const GlobalRenderInformation*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqGlobalRenderInformation : public std::unary_function<SBase*, bool>
{
    const std::string& id;

    IdEqGlobalRenderInformation (const std::string& id) : id(id) { }
    bool operator() (SBase* sb) 
    { return static_cast <GlobalRenderInformation *> (sb)->getId() == id; }
};
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the GlobalRenderInformation with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the GlobalRenderInformation object to be returned
 * 
 * @return pointer to the GlobalRenderInformation at the given @p id or @c NULL.
 */
GlobalRenderInformation* ListOfGlobalRenderInformation::get(const std::string& id)
{
    return const_cast<GlobalRenderInformation*>( 
            static_cast<const ListOfGlobalRenderInformation*>(this)->get(id) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the GlobalRenderInformation with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the GlobalRenderInformation object to be returned
 * 
 * @return const pointer to the GlobalRenderInformation at the given @p id or @c NULL.
 */
const GlobalRenderInformation* ListOfGlobalRenderInformation::get(const std::string& id) const
{
    std::vector<SBase*>::const_iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqGlobalRenderInformation(id) );
    return (result == mItems.end()) ? 0 : static_cast <GlobalRenderInformation*> (*result);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* Removes the nth item from this list */
    GlobalRenderInformation*
ListOfGlobalRenderInformation::remove (unsigned int n)
{
    return static_cast<GlobalRenderInformation*>(ListOf::remove(n));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* Removes item in this list by id */
    GlobalRenderInformation*
ListOfGlobalRenderInformation::remove (const std::string& sid)
{
    SBase* item = NULL;
    std::vector<SBase*>::iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqGlobalRenderInformation(sid) );

    if (result != mItems.end())
    {
        item = *result;
        mItems.erase(result);
    }

    return static_cast <GlobalRenderInformation*> (item);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d The SBMLDocument to set on the objects and it's children if there are any.
 */
void GlobalRenderInformation::setSBMLDocument (SBMLDocument* d)
{
    RenderInformationBase::setSBMLDocument(d);
    mListOfStyles.setSBMLDocument(d);
}
/** @endcond */

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
 */
void
GlobalRenderInformation::connectToChild()
{
  RenderInformationBase::connectToParent(this);
  mListOfStyles.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
GlobalRenderInformation::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag)
{
  RenderInformationBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mListOfStyles.enablePackageInternal(pkgURI,pkgPrefix,flag);  
}

bool ListOfGlobalRenderInformation::isValidTypeForList(SBase * item)
{
  if (item == NULL) return false;
  int typeCode = item->getTypeCode();
  return (
    typeCode == SBML_RENDER_GLOBALRENDERINFORMATION
    );
}

/*
* Returns the value of the "defaultValues" element of this
* ListOfGlobalRenderInformation.
*/
const DefaultValues*
ListOfGlobalRenderInformation::getDefaultValues() const
{
  return mDefaultValues;
}


/*
* Returns the value of the "defaultValues" element of this
* ListOfGlobalRenderInformation.
*/
DefaultValues*
ListOfGlobalRenderInformation::getDefaultValues()
{
  return mDefaultValues;
}


/*
* Predicate returning @c true if this ListOfGlobalRenderInformation's
* "defaultValues" element is set.
*/
bool
ListOfGlobalRenderInformation::isSetDefaultValues() const
{
  return (mDefaultValues != NULL);
}


/*
* Sets the value of the "defaultValues" element of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::setDefaultValues(const DefaultValues*
  defaultValues)
{
  if (mDefaultValues == defaultValues)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (defaultValues == NULL)
  {
    delete mDefaultValues;
    mDefaultValues = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mDefaultValues;
    mDefaultValues = (defaultValues != NULL) ? defaultValues->clone() : NULL;
    if (mDefaultValues != NULL)
    {
      mDefaultValues->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
* Creates a new DefaultValues object, adds it to this
* ListOfGlobalRenderInformation object and returns the DefaultValues object
* created.
*/
DefaultValues*
ListOfGlobalRenderInformation::createDefaultValues()
{
  if (mDefaultValues != NULL)
  {
    delete mDefaultValues;
  }

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());
  mDefaultValues = new DefaultValues(renderns);

  delete renderns;

  connectToChild();

  return mDefaultValues;
}


/*
* Unsets the value of the "defaultValues" element of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::unsetDefaultValues()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
* Write any contained elements
*/
void
ListOfGlobalRenderInformation::writeElements(XMLOutputStream& stream) const
{
  ListOf::writeElements(stream);

  if (isSetDefaultValues() == true)
  {
    mDefaultValues->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
* Connects to child elements
*/
void
ListOfGlobalRenderInformation::connectToChild()
{
  ListOf::connectToChild();

  if (mDefaultValues != NULL)
  {
    mDefaultValues->connectToParent(this);
  }
}

/** @endcond */

/*
* Predicate returning @c true if this ListOfGlobalRenderInformation's
* "versionMajor" attribute is set.
*/
bool
ListOfGlobalRenderInformation::isSetVersionMajor() const
{
  return mIsSetVersionMajor;
}


/*
* Predicate returning @c true if this ListOfGlobalRenderInformation's
* "versionMinor" attribute is set.
*/
bool
ListOfGlobalRenderInformation::isSetVersionMinor() const
{
  return mIsSetVersionMinor;
}

/*
* Unsets the value of the "versionMajor" attribute of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::unsetVersionMajor()
{
  mVersionMajor = SBML_INT_MAX;
  mIsSetVersionMajor = false;

  if (isSetVersionMajor() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
* Unsets the value of the "versionMinor" attribute of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::unsetVersionMinor()
{
  mVersionMinor = SBML_INT_MAX;
  mIsSetVersionMinor = false;

  if (isSetVersionMinor() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
* Sets the value of the "versionMajor" attribute of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::setVersionMajor(unsigned int versionMajor)
{
  mVersionMajor = versionMajor;
  mIsSetVersionMajor = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the value of the "versionMinor" attribute of this
* ListOfGlobalRenderInformation.
*/
int
ListOfGlobalRenderInformation::setVersionMinor(unsigned int versionMinor)
{
  mVersionMinor = versionMinor;
  mIsSetVersionMinor = true;
  return LIBSBML_OPERATION_SUCCESS;
}


LIBSBML_CPP_NAMESPACE_END 
