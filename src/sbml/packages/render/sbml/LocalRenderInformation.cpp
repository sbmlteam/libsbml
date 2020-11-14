/**
 * @file    LocalRenderInformation.cpp
 * @brief Implementation of the LocalRenderInformation class.
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

#include <sbml/packages/render/sbml/LocalRenderInformation.h>
#include <sbml/packages/render/sbml/ListOfLocalRenderInformation.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/xml/XMLInputStream.h>

#include <algorithm>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new LocalRenderInformation using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LocalRenderInformation::LocalRenderInformation(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
  : RenderInformationBase(level, version, pkgVersion)
  , mLocalStyles (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new LocalRenderInformation using the given RenderPkgNamespaces
 * object.
 */
LocalRenderInformation::LocalRenderInformation(RenderPkgNamespaces *renderns)
  : RenderInformationBase(renderns)
  , mLocalStyles (renderns)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LocalRenderInformation with the given @p id
 * and all lists empty.
 *
 * @param id the new id for the LocalRenderInformation.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
LocalRenderInformation::LocalRenderInformation(RenderPkgNamespaces* renderns, const std::string& id)
  : RenderInformationBase(renderns, id)
  , mLocalStyles(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LocalRenderInformation::LocalRenderInformation(const std::string& id) is deprecated." << std::endl;
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
 * Copy constructor for LocalRenderInformation.
 */
LocalRenderInformation::LocalRenderInformation(const LocalRenderInformation&
  orig)
  : RenderInformationBase( orig )
  , mLocalStyles ( orig.mLocalStyles )
{
  connectToChild();
}


/*
 * Assignment operator for LocalRenderInformation.
 */
LocalRenderInformation&
LocalRenderInformation::operator=(const LocalRenderInformation& rhs)
{
  if (&rhs != this)
  {
    RenderInformationBase::operator=(rhs);
    mLocalStyles = rhs.mLocalStyles;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this LocalRenderInformation object.
 */
LocalRenderInformation*
LocalRenderInformation::clone() const
{
  return new LocalRenderInformation(*this);
}


/*
 * Destructor for LocalRenderInformation.
 */
LocalRenderInformation::~LocalRenderInformation()
{
}


/*
 * Returns the ListOfLocalStyles from this LocalRenderInformation.
 */
const ListOfLocalStyles*
LocalRenderInformation::getListOfLocalStyles() const
{
  return &mLocalStyles;
}


const ListOfLocalStyles*
LocalRenderInformation::getListOfStyles() const
{
  return &mLocalStyles;
}


/*
 * Returns the ListOfLocalStyles from this LocalRenderInformation.
 */
ListOfLocalStyles*
LocalRenderInformation::getListOfLocalStyles()
{
  return &mLocalStyles;
}


ListOfLocalStyles*
LocalRenderInformation::getListOfStyles()
{
  return &mLocalStyles;
}


/*
 * Get a LocalStyle from the LocalRenderInformation.
 */
LocalStyle*
LocalRenderInformation::getLocalStyle(unsigned int n)
{
  return mLocalStyles.get(n);
}


LocalStyle*
LocalRenderInformation::getStyle(unsigned int n)
{
  return mLocalStyles.get(n);
}

/*
 * Get a LocalStyle from the LocalRenderInformation.
 */
const LocalStyle*
LocalRenderInformation::getLocalStyle(unsigned int n) const
{
  return mLocalStyles.get(n);
}

const LocalStyle*
LocalRenderInformation::getStyle(unsigned int n) const
{
  return mLocalStyles.get(n);
}

/*
* Returns a pointer to the style with the given @p id.
*/
const LocalStyle* LocalRenderInformation::getLocalStyle(const std::string& id) const
{
  return this->mLocalStyles.get(id);
}



LocalStyle* LocalRenderInformation::getLocalStyle(const std::string& id)
{
  return this->mLocalStyles.get(id);
}


const LocalStyle* LocalRenderInformation::getStyle(const std::string& id) const
{
  return this->mLocalStyles.get(id);
}



LocalStyle* LocalRenderInformation::getStyle(const std::string& id)
{
  return this->mLocalStyles.get(id);
}



/*
 * Adds a copy of the given LocalStyle to this LocalRenderInformation.
 */
int
LocalRenderInformation::addLocalStyle(const LocalStyle* ls)
{
  if (ls == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ls->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ls->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ls->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ls)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mLocalStyles.append(ls);
  }
}

int
LocalRenderInformation::addStyle(const LocalStyle* ls)
{
  return addLocalStyle(ls);
}


/*
 * Get the number of LocalStyle objects in this LocalRenderInformation.
 */
unsigned int
LocalRenderInformation::getNumLocalStyles() const
{
  return mLocalStyles.size();
}



unsigned int
LocalRenderInformation::getNumStyles() const
{
  return mLocalStyles.size();
}

/*
 * Creates a new LocalStyle object, adds it to this LocalRenderInformation
 * object and returns the LocalStyle object created.
 */
LocalStyle*
LocalRenderInformation::createLocalStyle()
{
  LocalStyle* ls = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    ls = new LocalStyle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (ls != NULL)
  {
    mLocalStyles.appendAndOwn(ls);
  }

  return ls;
}

/** @cond doxygenLibsbmlInternal */
LocalStyle*
LocalRenderInformation::createStyle(const std::string& id)
{
  LocalStyle* ls = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    ls = new LocalStyle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (ls != NULL)
  {
    ls->setId(id);
    mLocalStyles.appendAndOwn(ls);
  }

  return ls;

}
/** @endcond */


/*
 * Removes the nth LocalStyle from this LocalRenderInformation and returns a
 * pointer to it.
 */
LocalStyle*
LocalRenderInformation::removeLocalStyle(unsigned int n)
{
  return mLocalStyles.remove(n);
}


LocalStyle*
LocalRenderInformation::removeStyle(unsigned int n)
{
  return mLocalStyles.remove(n);
}


/*
* Removes the LocalStyle from this LocalRenderInformation based on its
* identifier and returns a pointer to it.
*/
LocalStyle*
LocalRenderInformation::removeLocalStyle(const std::string& id)
{
  return mLocalStyles.remove(id);
}


LocalStyle*
LocalRenderInformation::removeStyle(const std::string& id)
{
  return mLocalStyles.remove(id);
}


/*
 * Returns the XML element name of this LocalRenderInformation object.
 */
const std::string&
LocalRenderInformation::getElementName() const
{
  static const string name = "renderInformation";
  return name;
}


/*
 * Returns the libSBML type code for this LocalRenderInformation object.
 */
int
LocalRenderInformation::getTypeCode() const
{
  return SBML_RENDER_LOCALRENDERINFORMATION;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
LocalRenderInformation::writeElements(XMLOutputStream& stream) const
{
  RenderInformationBase::writeElements(stream);

  if (getNumLocalStyles() > 0)
  {
    mLocalStyles.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
LocalRenderInformation::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mLocalStyles.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
LocalRenderInformation::setSBMLDocument(SBMLDocument* d)
{
  RenderInformationBase::setSBMLDocument(d);

  mLocalStyles.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
LocalRenderInformation::connectToChild()
{
  RenderInformationBase::connectToChild();

  mLocalStyles.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
LocalRenderInformation::enablePackageInternal(const std::string& pkgURI,
                                              const std::string& pkgPrefix,
                                              bool flag)
{
  RenderInformationBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mLocalStyles.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * LocalRenderInformation.
 */
SBase*
LocalRenderInformation::createChildObject(const std::string& elementName)
{
  RenderInformationBase* obj = NULL;

  if (elementName == "localStyle")
  {
    return createLocalStyle();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this LocalRenderInformation.
 */
int
LocalRenderInformation::addChildObject(const std::string& elementName,
                                       const SBase* element)
{
  if (elementName == "localStyle" && element->getTypeCode() ==
    SBML_RENDER_LOCALSTYLE)
  {
    return addLocalStyle((const LocalStyle*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * LocalRenderInformation.
 */
SBase*
LocalRenderInformation::removeChildObject(const std::string& elementName,
                                          const std::string& id)
{
  if (elementName == "localStyle")
  {
    for (unsigned int i = 0; i < getNumLocalStyles(); i++)
    {
      if (getLocalStyle(i)->getId() == id)
      {
        return removeLocalStyle(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this LocalRenderInformation.
 */
unsigned int
LocalRenderInformation::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "localStyle")
  {
    return getNumLocalStyles();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this LocalRenderInformation.
 */
SBase*
LocalRenderInformation::getObject(const std::string& elementName,
                                  unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "localStyle")
  {
    return getLocalStyle(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
LocalRenderInformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mLocalStyles.getElementBySId(id);

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
LocalRenderInformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mLocalStyles.getMetaId() == metaid)
  {
    return &mLocalStyles;
  }

  obj = mLocalStyles.getElementByMetaId(metaid);

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
LocalRenderInformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mLocalStyles, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */
/*
 * Parses the xml information in the given node and sets the attributes.
 * This method should never be called by the user. It is only used to read render 
 * information from annotations.
 *
 * @param node the XMLNode object reference that describes the LocalRenderInformation
 * object to be instantiated.
 */
void LocalRenderInformation::parseXML(const XMLNode& node)
{
    this->RenderInformationBase::parseXML(node);
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfStyles")
        {
            this->mLocalStyles=ListOfLocalStyles(*child);
            this->mLocalStyles.setSBMLDocument(this->mSBML);
        }
        ++n;
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this LocalRenderInformation object.
 *
 * @return the XMLNode with the XML representation for the 
 * LocalRenderInformation object.
 *
 */
XMLNode LocalRenderInformation::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
LocalRenderInformation::createObject(XMLInputStream& stream)
{
  SBase* obj = RenderInformationBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfStyles")
  {
    if (mLocalStyles.size() != 0)
    {
      getErrorLog()->logPackageError("render",
        RenderLocalRenderInformationAllowedElements, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mLocalStyles;
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
LocalRenderInformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  RenderInformationBase::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
LocalRenderInformation::readAttributes(const XMLAttributes& attributes,
                                       const ExpectedAttributes&
                                         expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfLocalRenderInformation*>(getParentSBMLObject())->size() <
      2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render",
          RenderLayoutLOLocalRenderInformationAllowedAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderLayoutLOLocalRenderInformationAllowedCoreAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
    }
  }

  RenderInformationBase::readAttributes(attributes, expectedAttributes);

  if (log)
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
        log->logPackageError("render",
          RenderLocalRenderInformationAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
LocalRenderInformation::writeAttributes(XMLOutputStream& stream) const
{
  RenderInformationBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new LocalRenderInformation_t using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
LocalRenderInformation_t *
LocalRenderInformation_create(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion)
{
  return new LocalRenderInformation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this LocalRenderInformation_t object.
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
LocalRenderInformation_clone(const LocalRenderInformation_t* lri)
{
  if (lri != NULL)
  {
    return static_cast<LocalRenderInformation_t*>(lri->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this LocalRenderInformation_t object.
 */
LIBSBML_EXTERN
void
LocalRenderInformation_free(LocalRenderInformation_t* lri)
{
  if (lri != NULL)
  {
    delete lri;
  }
}


/*
 * Returns a ListOf_t * containing LocalStyle_t objects from this
 * LocalRenderInformation_t.
 */
LIBSBML_EXTERN
ListOf_t*
LocalRenderInformation_getListOfLocalStyles(LocalRenderInformation_t* lri)
{
  return (lri != NULL) ? lri->getListOfLocalStyles() : NULL;
}


/*
 * Get a LocalStyle_t from the LocalRenderInformation_t.
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_getLocalStyle(LocalRenderInformation_t* lri,
                                     unsigned int n)
{
  return (lri != NULL) ? lri->getLocalStyle(n) : NULL;
}


/*
 * Adds a copy of the given LocalStyle_t to this LocalRenderInformation_t.
 */
LIBSBML_EXTERN
int
LocalRenderInformation_addLocalStyle(LocalRenderInformation_t* lri,
                                     const LocalStyle_t* ls)
{
  return (lri != NULL) ? lri->addLocalStyle(ls) : LIBSBML_INVALID_OBJECT;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Get the number of LocalStyle_t objects in this LocalRenderInformation_t.
 */
LIBSBML_EXTERN
unsigned int
LocalRenderInformation_getNumLocalStyles(LocalRenderInformation_t* lri)
{
  return (lri != NULL) ? lri->getNumLocalStyles() : SBML_INT_MAX;
}
/** @endcond */


/*
 * Creates a new LocalStyle_t object, adds it to this LocalRenderInformation_t
 * object and returns the LocalStyle_t object created.
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_createLocalStyle(LocalRenderInformation_t* lri)
{
  return (lri != NULL) ? lri->createLocalStyle() : NULL;
}


/*
 * Removes the nth LocalStyle_t from this LocalRenderInformation_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_removeLocalStyle(LocalRenderInformation_t* lri,
                                        unsigned int n)
{
  return (lri != NULL) ? lri->removeLocalStyle(n) : NULL;
}
/** @endcond */


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LocalRenderInformation_t object have been set.
 */
LIBSBML_EXTERN
int
LocalRenderInformation_hasRequiredAttributes(const LocalRenderInformation_t *
  lri)
{
  return (lri != NULL) ? static_cast<int>(lri->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * LocalRenderInformation_t object have been set.
 */
LIBSBML_EXTERN
int
LocalRenderInformation_hasRequiredElements(const LocalRenderInformation_t *
  lri)
{
  return (lri != NULL) ? static_cast<int>(lri->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


