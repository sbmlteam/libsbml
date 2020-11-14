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

#include <sbml/packages/render/sbml/LocalStyle.h>
#include <sbml/packages/render/sbml/ListOfLocalStyles.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>

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

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new LocalStyle using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LocalStyle::LocalStyle(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
:    Style(level,version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}
/*
 * Creates a new LocalStyle using the given RenderPkgNamespaces object.
 */
LocalStyle::LocalStyle(RenderPkgNamespaces *renderns)
  : Style(renderns)
{
  setElementNamespace(renderns->getURI());
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LocalStyle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LocalStyle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
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

/*
 * Copy constructor for LocalStyle.
 */
LocalStyle::LocalStyle(const LocalStyle& orig)
  : Style( orig )
  , mIdList ( orig.mIdList )
{
}


/*
 * Assignment operator for LocalStyle.
 */
LocalStyle&
LocalStyle::operator=(const LocalStyle& rhs)
{
  if (&rhs != this)
  {
    Style::operator=(rhs);
    mIdList = rhs.mIdList;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this LocalStyle object.
 */
LocalStyle*
LocalStyle::clone() const
{
  return new LocalStyle(*this);
}


/*
 * Destructor for LocalStyle.
 */
LocalStyle::~LocalStyle()
{
}


/*
 * Returns the id list.
 */
const std::set<std::string>& LocalStyle::getIdList() const
{
    return this->mIdList;
}


/*
 * Returns the id list.
 */
std::set<std::string>& LocalStyle::getIdList()
{
    return this->mIdList;
}


/*
 * Returns the number of ids in the id set.
 */
unsigned int LocalStyle::getNumIds() const
{
    return (unsigned int)this->mIdList.size();
}


/*
 * Checks whether a given @p id is in the id list.
 */
bool LocalStyle::isInIdList(const std::string& id) const
{
    return (this->mIdList.find(id)!=this->mIdList.end());
}


/*
 * Adds another id to the set.
 *
 * @param id the id string to be added to the id list.
 */
int
LocalStyle::addId(const std::string& id)
{
    this->mIdList.insert(id);
    return LIBSBML_OPERATION_SUCCESS;
}


std::string 
LocalStyle::createIdString() const
{
  return createStringFromSet(mIdList);
}


/*
 * Removes an id from the set.
 */
int
LocalStyle::removeId(const std::string& id)
{
    this->mIdList.erase(id);
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the id list.
 */
int
LocalStyle::setIdList(const std::set<std::string>& idList)
{
    this->mIdList=idList;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this LocalStyle object.
 */
const std::string&
LocalStyle::getElementName() const
{
  static const string name = "style";
  return name;
}


/*
 * Returns the libSBML type code for this LocalStyle object.
 */
int
LocalStyle::getTypeCode() const
{
  return SBML_RENDER_LOCALSTYLE;
}


/*
 * Creates an XMLNode object from this LocalStyle object.
 */
XMLNode LocalStyle::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
LocalStyle::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Style::addExpectedAttributes(attributes);

  attributes.add("idList");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
LocalStyle::readAttributes(const XMLAttributes& attributes,
                           const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfLocalStyles*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderLocalStyleAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderLocalRenderInformationLOLocalStylesAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  Style::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderLocalStyleAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderLocalStyleAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // idList string (use = "optional" )
  // 

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
 * Writes the attributes to the stream
 */
void
LocalStyle::writeAttributes(XMLOutputStream& stream) const
{
  Style::writeAttributes(stream);
  this->writeIdList(stream);
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





#endif /* __cplusplus */


/*
 * Creates a new LocalStyle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
LocalStyle_t *
LocalStyle_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion)
{
  return new LocalStyle(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this LocalStyle_t object.
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalStyle_clone(const LocalStyle_t* ls)
{
  if (ls != NULL)
  {
    return static_cast<LocalStyle_t*>(ls->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this LocalStyle_t object.
 */
LIBSBML_EXTERN
void
LocalStyle_free(LocalStyle_t* ls)
{
  if (ls != NULL)
  {
    delete ls;
  }
}


///*
// * Returns the value of the "idList" attribute of this LocalStyle_t.
// */
//LIBSBML_EXTERN
//char *
//LocalStyle_getIdList(const LocalStyle_t * ls)
//{
//  if (ls == NULL)
//  {
//    return NULL;
//  }
//
//  return ls->getIdList().empty() ? NULL : safe_strdup(ls->getIdList().c_str());
//}


/*
 * Predicate returning @c 1 (true) if this LocalStyle_t's "idList" attribute is
 * set.
 */
LIBSBML_EXTERN
int
LocalStyle_isSetIdList(const LocalStyle_t * ls)
{
  return (ls != NULL) ? static_cast<int>(ls->getNumIds()) : 0;
}


/*
 * Sets the value of the "idList" attribute of this LocalStyle_t.
 */
LIBSBML_EXTERN
int
LocalStyle_setIdList(LocalStyle_t * ls, const char * idList)
{
  return (ls != NULL) ? ls->addId(idList) : LIBSBML_INVALID_OBJECT;
}


///*
// * Unsets the value of the "idList" attribute of this LocalStyle_t.
// */
//LIBSBML_EXTERN
//int
//LocalStyle_unsetIdList(LocalStyle_t * ls)
//{
//  return (ls != NULL) ? ls->unsetIdList() : LIBSBML_INVALID_OBJECT;
//}
//

/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LocalStyle_t object have been set.
 */
LIBSBML_EXTERN
int
LocalStyle_hasRequiredAttributes(const LocalStyle_t * ls)
{
  return (ls != NULL) ? static_cast<int>(ls->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


