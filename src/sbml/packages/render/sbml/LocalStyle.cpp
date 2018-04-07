/**
 * @file    LocalStyle.cpp
 * @brief   class for representing a local style
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

#include "LocalStyle.h"

#include <sbml/xml/XMLInputStream.h>

#include <algorithm>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string LocalStyle::ELEMENT_NAME="style";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LocalStyle object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
LocalStyle::LocalStyle (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    Style(level,version, pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LocalStyle object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
LocalStyle::LocalStyle (RenderPkgNamespaces* renderns):
    Style(renderns)
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

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LocalStyle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LocalStyle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the LocalStyle
 * object to be instantiated.
 */
LocalStyle::LocalStyle(const XMLNode& node, unsigned int l2version):Style(node, l2version)
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
LocalStyle::~LocalStyle ()
{
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LocalStyle with an empty group
 * and empty id, role and type list.
 * The group has to be filled before the
 * object is valid.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
LocalStyle::LocalStyle(RenderPkgNamespaces* renderns, const std::string& id):Style(renderns,id)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LocalStyle::LocalStyle(const std::string& id) is deprecated." << std::endl;
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

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of ids in the id set.
 *
 * @return the number of ids in the id set
 */
unsigned int LocalStyle::getNumIds() const
{
    return (unsigned int)this->mIdList.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the id list.
 *
 * @param idList The list of ids to be set on the local style.
 */
void LocalStyle::setIdList(const std::set<std::string>& idList)
{
    this->mIdList=idList;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id list.
 *
 * @return the const reference to the list of ids for the local style.
 */
const std::set<std::string>& LocalStyle::getIdList() const
{
    return this->mIdList;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id list.
 *
 * @return the reference to the list of ids for the local style.
 */
std::set<std::string>& LocalStyle::getIdList()
{
    return this->mIdList;
}
/** @endcond */

std::string 
LocalStyle::createIdString() const
{
  return createStringFromSet(mIdList);
}

/** @cond doxygenLibsbmlInternal */
/*
 * Adds another id to the set.
 *
 * @param id the id string to be added to the id list.
 */
void LocalStyle::addId(const std::string& id)
{
    this->mIdList.insert(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Removes an id from the set.
 *
 * @param the id to be removed from the id list.
 */
void LocalStyle::removeId(const std::string& id)
{
    this->mIdList.erase(id);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
LocalStyle::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Style::addExpectedAttributes(attributes);

  attributes.add("idList");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void LocalStyle::readAttributes(const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    Style::readAttributes(attributes, expectedAttributes);
    // isList
    // typeList
    std::string s;
    attributes.readInto("idList", s, getErrorLog(), false, getLine(), getColumn());
    // split the idList
    if(!s.empty())
    {
        readIntoSet(s,this->mIdList);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this LocalStyle object.
 * 
 * @return a (deep) copy of this LocalStyle object
 */
LocalStyle* LocalStyle::clone() const
{
    return new LocalStyle(*this);
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
const std::string& LocalStyle::getElementName () const
{
  static std::string name = LocalStyle::ELEMENT_NAME;
  return name;
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
int LocalStyle::getTypeCode() const
{
    return SBML_RENDER_LOCALSTYLE;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Checks whether a given @p id is in the id list.
 *
 * @param id the id to be searched for
 *
 * @return true or false depending on whether the given @p id is in the id list or not.
 */
bool LocalStyle::isInIdList(const std::string& id) const
{
    return (this->mIdList.find(id)!=this->mIdList.end());
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * This method adds the attribute for the list of ids to
 * the given XMLnode.
 *
 * @param node the node where the attribute needs to be added
 */
void LocalStyle::addListOfIds(XMLToken& node) const
{
    std::string s=createStringFromSet(this->mIdList);
    if(!s.empty())
    {
        node.addAttr("idList",s);
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
void LocalStyle::writeAttributes (XMLOutputStream& stream) const
{
  Style::writeAttributes(stream);
  this->writeIdList(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this LocalStyle object.
 *
 * @return the XMLNode with the XML representation for the 
 * LocalStyle object.
 */
XMLNode LocalStyle::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Writes the id list to an XML stream.
 */
void LocalStyle::writeIdList(XMLOutputStream& stream) const
{
    std::string s=createStringFromSet(this->mIdList);
    if(!s.empty())
    {
        stream.writeAttribute("idList", getPrefix(), s);
    }
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 
