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

#include <sbml/packages/render/sbml/GlobalRenderInformation.h>
#include <sbml/packages/render/sbml/ListOfGlobalRenderInformation.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;


#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED


LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GlobalRenderInformation using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
GlobalRenderInformation::GlobalRenderInformation(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
  : RenderInformationBase(level, version, pkgVersion)
  , mGlobalStyles (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new GlobalRenderInformation using the given RenderPkgNamespaces
 * object.
 */
GlobalRenderInformation::GlobalRenderInformation(RenderPkgNamespaces *renderns)
  : RenderInformationBase(renderns)
  , mGlobalStyles (renderns)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}



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
  : RenderInformationBase(renderns,id), mGlobalStyles(renderns)
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


    
/*
 * Copy constructor for GlobalRenderInformation.
 */
GlobalRenderInformation::GlobalRenderInformation(const GlobalRenderInformation&
  orig)
  : RenderInformationBase( orig )
  , mGlobalStyles ( orig.mGlobalStyles )
{
  connectToChild();
}


/*
 * Assignment operator for GlobalRenderInformation.
 */
GlobalRenderInformation&
GlobalRenderInformation::operator=(const GlobalRenderInformation& rhs)
{
  if (&rhs != this)
  {
    RenderInformationBase::operator=(rhs);
    mGlobalStyles = rhs.mGlobalStyles;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GlobalRenderInformation object.
 */
GlobalRenderInformation*
GlobalRenderInformation::clone() const
{
  return new GlobalRenderInformation(*this);
}


/*
 * Destructor for GlobalRenderInformation.
 */
GlobalRenderInformation::~GlobalRenderInformation()
{
}


/*
 * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
 */
const ListOfGlobalStyles*
GlobalRenderInformation::getListOfGlobalStyles() const
{
  return &mGlobalStyles;
}


const ListOfGlobalStyles*
GlobalRenderInformation::getListOfStyles() const
{
  return &mGlobalStyles;
}


/*
 * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
 */
ListOfGlobalStyles*
GlobalRenderInformation::getListOfGlobalStyles()
{
  return &mGlobalStyles;
}


/*
* Returns the ListOfGlobalStyles from this GlobalRenderInformation.
*/
ListOfGlobalStyles*
GlobalRenderInformation::getListOfStyles()
{
  return &mGlobalStyles;
}


/*
 * Get a GlobalStyle from the GlobalRenderInformation.
 */
GlobalStyle*
GlobalRenderInformation::getGlobalStyle(unsigned int n)
{
  return mGlobalStyles.get(n);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
GlobalStyle*
GlobalRenderInformation::getStyle(unsigned int n)
{
  return mGlobalStyles.get(n);
}


/*
 * Get a GlobalStyle from the GlobalRenderInformation.
 */
const GlobalStyle*
GlobalRenderInformation::getGlobalStyle(unsigned int n) const
{
  return mGlobalStyles.get(n);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
const GlobalStyle*
GlobalRenderInformation::getStyle(unsigned int n) const
{
  return mGlobalStyles.get(n);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
GlobalStyle*
GlobalRenderInformation::getGlobalStyle(const std::string& id)
{
  return mGlobalStyles.get(id);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
GlobalStyle*
GlobalRenderInformation::getStyle(const std::string& id)
{
  return mGlobalStyles.get(id);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
const GlobalStyle*
GlobalRenderInformation::getGlobalStyle(const std::string& id) const
{
  return mGlobalStyles.get(id);
}


/*
* Get a GlobalStyle from the GlobalRenderInformation.
*/
const GlobalStyle*
GlobalRenderInformation::getStyle(const std::string& id) const
{
  return mGlobalStyles.get(id);
}


/*
 * Adds a copy of the given GlobalStyle to this GlobalRenderInformation.
 */
int
GlobalRenderInformation::addGlobalStyle(const GlobalStyle* gs)
{
  if (gs == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gs->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gs->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gs->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gs)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mGlobalStyles.append(gs);
  }
}


/*
* Adds a copy of the given GlobalStyle to this GlobalRenderInformation.
*/
int
GlobalRenderInformation::addStyle(const GlobalStyle* gs)
{
  return addGlobalStyle(gs);
}


/*
 * Get the number of GlobalStyle objects in this GlobalRenderInformation.
 */
unsigned int
GlobalRenderInformation::getNumGlobalStyles() const
{
  return mGlobalStyles.size();
}


/*
* Get the number of GlobalStyle objects in this GlobalRenderInformation.
*/
unsigned int
GlobalRenderInformation::getNumStyles() const
{
  return mGlobalStyles.size();
}


/*
 * Creates a new GlobalStyle object, adds it to this GlobalRenderInformation
 * object and returns the GlobalStyle object created.
 */
GlobalStyle*
GlobalRenderInformation::createGlobalStyle()
{
  GlobalStyle* gs = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    gs = new GlobalStyle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (gs != NULL)
  {
    mGlobalStyles.appendAndOwn(gs);
  }

  return gs;
}

/*
*Creates a new GlobalStyle object, adds it to this GlobalRenderInformation
* object and returns the GlobalStyle object created.
*/
GlobalStyle*
GlobalRenderInformation::createStyle(const std::string& id)
{
  GlobalStyle* gs = createGlobalStyle();
  if (gs != NULL)
  {
    gs->setId(id);
  }
  return gs;
}
  
/*
 * Removes the nth GlobalStyle from this GlobalRenderInformation and returns a
 * pointer to it.
 */
GlobalStyle*
GlobalRenderInformation::removeGlobalStyle(unsigned int n)
{
  return mGlobalStyles.remove(n);
}

GlobalStyle*
GlobalRenderInformation::removeGlobalStyle(const std::string& sid)
{
  return mGlobalStyles.remove(sid);
}

/*
* Removes the nth GlobalStyle from this GlobalRenderInformation and returns a
* pointer to it.
*/
GlobalStyle*
GlobalRenderInformation::removeStyle(unsigned int n)
{
  return mGlobalStyles.remove(n);
}



/*
 * Returns the XML element name of this GlobalRenderInformation object.
 */
const std::string&
GlobalRenderInformation::getElementName() const
{
  static const string name = "renderInformation";
  return name;
}


/*
 * Returns the libSBML type code for this GlobalRenderInformation object.
 */
int
GlobalRenderInformation::getTypeCode() const
{
  return SBML_RENDER_GLOBALRENDERINFORMATION;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
GlobalRenderInformation::writeElements(XMLOutputStream& stream) const
{
  RenderInformationBase::writeElements(stream);

  if (getNumGlobalStyles() > 0)
  {
    mGlobalStyles.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GlobalRenderInformation::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mGlobalStyles.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GlobalRenderInformation::setSBMLDocument(SBMLDocument* d)
{
  RenderInformationBase::setSBMLDocument(d);

  mGlobalStyles.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
GlobalRenderInformation::connectToChild()
{
  RenderInformationBase::connectToChild();

  mGlobalStyles.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GlobalRenderInformation::enablePackageInternal(const std::string& pkgURI,
                                               const std::string& pkgPrefix,
                                               bool flag)
{
  RenderInformationBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGlobalStyles.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * GlobalRenderInformation.
 */
SBase*
GlobalRenderInformation::createChildObject(const std::string& elementName)
{
  RenderInformationBase* obj = NULL;

  if (elementName == "globalStyle")
  {
    return createGlobalStyle();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this GlobalRenderInformation.
 */
int
GlobalRenderInformation::addChildObject(const std::string& elementName,
                                        const SBase* element)
{
  if (elementName == "globalStyle" && element->getTypeCode() ==
    SBML_RENDER_GLOBALSTYLE)
  {
    return addGlobalStyle((const GlobalStyle*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * GlobalRenderInformation.
 */
SBase*
GlobalRenderInformation::removeChildObject(const std::string& elementName,
                                           const std::string& id)
{
  if (elementName == "globalStyle")
  {
    for (unsigned int i = 0; i < getNumGlobalStyles(); i++)
    {
      if (getGlobalStyle(i)->getId() == id)
      {
        return removeGlobalStyle(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this GlobalRenderInformation.
 */
unsigned int
GlobalRenderInformation::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "globalStyle")
  {
    return getNumGlobalStyles();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this GlobalRenderInformation.
 */
SBase*
GlobalRenderInformation::getObject(const std::string& elementName,
                                   unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "globalStyle")
  {
    return getGlobalStyle(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
GlobalRenderInformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mGlobalStyles.getElementBySId(id);

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
GlobalRenderInformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGlobalStyles.getMetaId() == metaid)
  {
    return &mGlobalStyles;
  }

  obj = mGlobalStyles.getElementByMetaId(metaid);

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
GlobalRenderInformation::getAllElements(ElementFilter* filter)
{
  List* ret = RenderInformationBase::getAllElements(filter);
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mGlobalStyles, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
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
            this->mGlobalStyles=ListOfGlobalStyles(*child);
            this->mGlobalStyles.setSBMLDocument(this->mSBML);
        }
        ++n;
    }

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
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
GlobalRenderInformation::createObject(XMLInputStream& stream)
{
  SBase* obj = RenderInformationBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfStyles")
  {
    if (mGlobalStyles.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render",
        RenderGlobalRenderInformationAllowedElements, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mGlobalStyles;
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
GlobalRenderInformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  RenderInformationBase::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GlobalRenderInformation::readAttributes(const XMLAttributes& attributes,
                                        const ExpectedAttributes&
                                          expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGlobalRenderInformation*>(getParentSBMLObject())->size() <
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
          RenderListOfLayoutsLOGlobalRenderInformationAllowedAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
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
          RenderGlobalRenderInformationAllowedCoreAttributes, pkgVersion, level,
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
GlobalRenderInformation::writeAttributes(XMLOutputStream& stream) const
{
  RenderInformationBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new GlobalRenderInformation_t using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t *
GlobalRenderInformation_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
{
  return new GlobalRenderInformation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GlobalRenderInformation_t object.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
GlobalRenderInformation_clone(const GlobalRenderInformation_t* gri)
{
  if (gri != NULL)
  {
    return static_cast<GlobalRenderInformation_t*>(gri->clone());
  }
  else
  {
    return NULL;
  }
}
/** @endcond */


/*
 * Frees this GlobalRenderInformation_t object.
 */
LIBSBML_EXTERN
void
GlobalRenderInformation_free(GlobalRenderInformation_t* gri)
{
  if (gri != NULL)
  {
    delete gri;
  }
}


/*
 * Returns a ListOf_t * containing GlobalStyle_t objects from this
 * GlobalRenderInformation_t.
 */
LIBSBML_EXTERN
ListOf_t*
GlobalRenderInformation_getListOfGlobalStyles(GlobalRenderInformation_t* gri)
{
  return (gri != NULL) ? gri->getListOfGlobalStyles() : NULL;
}
/** @endcond */


/*
 * Get a GlobalStyle_t from the GlobalRenderInformation_t.
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_getGlobalStyle(GlobalRenderInformation_t* gri,
                                       unsigned int n)
{
  return (gri != NULL) ? gri->getGlobalStyle(n) : NULL;
}


/*
 * Adds a copy of the given GlobalStyle_t to this GlobalRenderInformation_t.
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_addGlobalStyle(GlobalRenderInformation_t* gri,
                                       const GlobalStyle_t* gs)
{
  return (gri != NULL) ? gri->addGlobalStyle(gs) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of GlobalStyle_t objects in this GlobalRenderInformation_t.
 */
LIBSBML_EXTERN
unsigned int
GlobalRenderInformation_getNumGlobalStyles(GlobalRenderInformation_t* gri)
{
  return (gri != NULL) ? gri->getNumGlobalStyles() : SBML_INT_MAX;
}


/*
 * Creates a new GlobalStyle_t object, adds it to this
 * GlobalRenderInformation_t object and returns the GlobalStyle_t object
 * created.
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_createGlobalStyle(GlobalRenderInformation_t* gri)
{
  return (gri != NULL) ? gri->createGlobalStyle() : NULL;
}


/*
 * Removes the nth GlobalStyle_t from this GlobalRenderInformation_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_removeGlobalStyle(GlobalRenderInformation_t* gri,
                                          unsigned int n)
{
  return (gri != NULL) ? gri->removeGlobalStyle(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GlobalRenderInformation_t object have been set.
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_hasRequiredAttributes(const GlobalRenderInformation_t *
  gri)
{
  return (gri != NULL) ? static_cast<int>(gri->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * GlobalRenderInformation_t object have been set.
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_hasRequiredElements(const GlobalRenderInformation_t *
  gri)
{
  return (gri != NULL) ? static_cast<int>(gri->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


