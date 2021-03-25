/**
 * @file FbcAnnotationPlugin.cpp
 * @brief Implementation of the FbcAnnotationPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/fbc/extension/FbcAnnotationPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new FbcAnnotationPlugin using the given URI, prefix and package
 * namespace.
 */
FbcAnnotationPlugin::FbcAnnotationPlugin(const std::string& uri,
                                         const std::string& prefix,
                                         FbcPkgNamespaces* fbcns)
  : SBasePlugin(uri, prefix, fbcns)
  , mKeyValuePairs (fbcns)
{
  connectToChild();
}


/*
 * Copy constructor for FbcAnnotationPlugin.
 */
FbcAnnotationPlugin::FbcAnnotationPlugin(const FbcAnnotationPlugin& orig)
  : SBasePlugin( orig )
  , mKeyValuePairs ( orig.mKeyValuePairs )
{
  connectToChild();
}


/*
 * Assignment operator for FbcAnnotationPlugin.
 */
FbcAnnotationPlugin&
FbcAnnotationPlugin::operator=(const FbcAnnotationPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    mKeyValuePairs = rhs.mKeyValuePairs;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this FbcAnnotationPlugin object.
 */
FbcAnnotationPlugin*
FbcAnnotationPlugin::clone() const
{
  return new FbcAnnotationPlugin(*this);
}


/*
 * Destructor for FbcAnnotationPlugin.
 */
FbcAnnotationPlugin::~FbcAnnotationPlugin()
{
}


/*
 * Returns the ListOfKeyValuePairs from this FbcAnnotationPlugin.
 */
const ListOfKeyValuePairs*
FbcAnnotationPlugin::getListOfKeyValuePairs() const
{
  return &mKeyValuePairs;
}


/*
 * Returns the ListOfKeyValuePairs from this FbcAnnotationPlugin.
 */
ListOfKeyValuePairs*
FbcAnnotationPlugin::getListOfKeyValuePairs()
{
  return &mKeyValuePairs;
}


/*
 * Get a KeyValuePair from the FbcAnnotationPlugin.
 */
KeyValuePair*
FbcAnnotationPlugin::getKeyValuePair(unsigned int n)
{
  return static_cast< KeyValuePair*>(mKeyValuePairs.get(n));
}


/*
 * Get a KeyValuePair from the FbcAnnotationPlugin.
 */
const KeyValuePair*
FbcAnnotationPlugin::getKeyValuePair(unsigned int n) const
{
  return static_cast<const KeyValuePair*>(mKeyValuePairs.get(n));
}


/*
 * Get a KeyValuePair from the FbcAnnotationPlugin based on its identifier.
 */
KeyValuePair*
FbcAnnotationPlugin::getKeyValuePair(const std::string& sid)
{
  return static_cast< KeyValuePair*>(mKeyValuePairs.get(sid));
}


/*
 * Get a KeyValuePair from the FbcAnnotationPlugin based on its identifier.
 */
const KeyValuePair*
FbcAnnotationPlugin::getKeyValuePair(const std::string& sid) const
{
  return static_cast<const KeyValuePair*>(mKeyValuePairs.get(sid));
}


/*
 * Adds a copy of the given KeyValuePair to this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::addKeyValuePair(const KeyValuePair* kvp)
{
  if (kvp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (kvp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != kvp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != kvp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != kvp->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else if (kvp->isSetId() && (mKeyValuePairs.get(kvp->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mKeyValuePairs.append(kvp);
  }
}


/*
 * Get the number of KeyValuePair objects in this FbcAnnotationPlugin.
 */
unsigned int
FbcAnnotationPlugin::getNumKeyValuePairs() const
{
  return mKeyValuePairs.size();
}


/*
 * Creates a new KeyValuePair object, adds it to this FbcAnnotationPlugin
 * object and returns the KeyValuePair object created.
 */
KeyValuePair*
FbcAnnotationPlugin::createKeyValuePair()
{
  KeyValuePair* kvp = NULL;

  try
  {
    FBC_CREATE_NS(fbcns, getSBMLNamespaces());
    kvp = new KeyValuePair(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (kvp != NULL)
  {
    mKeyValuePairs.appendAndOwn(kvp);
  }

  return kvp;
}


/*
 * Removes the nth KeyValuePair from this FbcAnnotationPlugin and returns a
 * pointer to it.
 */
KeyValuePair*
FbcAnnotationPlugin::removeKeyValuePair(unsigned int n)
{
  return static_cast<KeyValuePair*>(mKeyValuePairs.remove(n));
}


/*
 * Removes the KeyValuePair from this FbcAnnotationPlugin based on its
 * identifier and returns a pointer to it.
 */
KeyValuePair*
FbcAnnotationPlugin::removeKeyValuePair(const std::string& sid)
{
  return static_cast<KeyValuePair*>(mKeyValuePairs.remove(sid));
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
FbcAnnotationPlugin::writeElements(XMLOutputStream& stream) const
{
  if (getNumKeyValuePairs() > 0)
  {
    mKeyValuePairs.write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
FbcAnnotationPlugin::accept(SBMLVisitor& v) const
{
  const Annotation* a = static_cast<const
    Annotation*>(this->getParentSBMLObject());
  v.visit(*a);
  v.leave(*a);

  mKeyValuePairs.accept(v);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
FbcAnnotationPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mKeyValuePairs.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
FbcAnnotationPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
FbcAnnotationPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  mKeyValuePairs.connectToParent(base);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
FbcAnnotationPlugin::enablePackageInternal(const std::string& pkgURI,
                                           const std::string& pkgPrefix,
                                           bool flag)
{
  mKeyValuePairs.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
FbcAnnotationPlugin::updateSBMLNamespace(const std::string& package,
                                         unsigned int level,
                                         unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  mKeyValuePairs.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::getAttribute(const std::string& attributeName,
                                  bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::getAttribute(const std::string& attributeName,
                                  int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::getAttribute(const std::string& attributeName,
                                  double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::getAttribute(const std::string& attributeName,
                                  unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::getAttribute(const std::string& attributeName,
                                  std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcAnnotationPlugin's attribute
 * "attributeName" is set.
 */
bool
FbcAnnotationPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::setAttribute(const std::string& attributeName,
                                  bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::setAttribute(const std::string& attributeName,
                                  double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::setAttribute(const std::string& attributeName,
                                  unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::setAttribute(const std::string& attributeName,
                                  const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this FbcAnnotationPlugin.
 */
SBase*
FbcAnnotationPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "keyValuePair")
  {
    return createKeyValuePair();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this FbcAnnotationPlugin.
 */
int
FbcAnnotationPlugin::addChildObject(const std::string& elementName,
                                    const SBase* element)
{
  if (elementName == "keyValuePair" && element->getTypeCode() ==
    SBML_FBC_KEYVALUEPAIR)
  {
    return addKeyValuePair((const KeyValuePair*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * FbcAnnotationPlugin.
 */
SBase*
FbcAnnotationPlugin::removeChildObject(const std::string& elementName,
                                       const std::string& id)
{
  if (elementName == "keyValuePair")
  {
    return removeKeyValuePair(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this FbcAnnotationPlugin.
 */
unsigned int
FbcAnnotationPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "keyValuePair")
  {
    return getNumKeyValuePairs();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this FbcAnnotationPlugin.
 */
SBase*
FbcAnnotationPlugin::getObject(const std::string& elementName,
                               unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "keyValuePair")
  {
    return getKeyValuePair(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
FbcAnnotationPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mKeyValuePairs.getElementBySId(id);

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
FbcAnnotationPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mKeyValuePairs.getMetaId() == metaid)
  {
    return &mKeyValuePairs;
  }

  obj = mKeyValuePairs.getElementByMetaId(metaid);

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
FbcAnnotationPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mKeyValuePairs, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
FbcAnnotationPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const FbcAnnotationPlugin* plug = static_cast<const
    FbcAnnotationPlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  ret = mKeyValuePairs.appendFrom(plug->getListOfKeyValuePairs());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  return ret;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
FbcAnnotationPlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if (name == "listOfKeyValuePairs")
    {
      if (getErrorLog() && mKeyValuePairs.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcAnnotationAllowedElements,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(),
            getColumn());
      }

      obj = &mKeyValuePairs;
      if (targetPrefix.empty())
      {
        mKeyValuePairs.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
  }

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing KeyValuePair_t objects from this
 * FbcAnnotationPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
FbcAnnotationPlugin_getListOfKeyValuePairs(FbcAnnotationPlugin_t* fap)
{
  return (fap != NULL) ? fap->getListOfKeyValuePairs() : NULL;
}


/*
 * Get a KeyValuePair_t from the FbcAnnotationPlugin_t.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcAnnotationPlugin_getKeyValuePair(FbcAnnotationPlugin_t* fap,
                                    unsigned int n)
{
  return (fap != NULL) ? fap->getKeyValuePair(n) : NULL;
}


/*
 * Get a KeyValuePair_t from the FbcAnnotationPlugin_t based on its identifier.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcAnnotationPlugin_getKeyValuePairById(FbcAnnotationPlugin_t* fap,
                                        const char *sid)
{
  return (fap != NULL && sid != NULL) ? fap->getKeyValuePair(sid) : NULL;
}


/*
 * Adds a copy of the given KeyValuePair_t to this FbcAnnotationPlugin_t.
 */
LIBSBML_EXTERN
int
FbcAnnotationPlugin_addKeyValuePair(FbcAnnotationPlugin_t* fap,
                                    const KeyValuePair_t* kvp)
{
  return (fap != NULL) ? fap->addKeyValuePair(kvp) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of KeyValuePair_t objects in this FbcAnnotationPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
FbcAnnotationPlugin_getNumKeyValuePairs(FbcAnnotationPlugin_t* fap)
{
  return (fap != NULL) ? fap->getNumKeyValuePairs() : SBML_INT_MAX;
}


/*
 * Creates a new KeyValuePair_t object, adds it to this FbcAnnotationPlugin_t
 * object and returns the KeyValuePair_t object created.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcAnnotationPlugin_createKeyValuePair(FbcAnnotationPlugin_t* fap)
{
  return (fap != NULL) ? fap->createKeyValuePair() : NULL;
}


/*
 * Removes the nth KeyValuePair_t from this FbcAnnotationPlugin_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcAnnotationPlugin_removeKeyValuePair(FbcAnnotationPlugin_t* fap,
                                       unsigned int n)
{
  return (fap != NULL) ? fap->removeKeyValuePair(n) : NULL;
}


/*
 * Removes the KeyValuePair_t from this FbcAnnotationPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcAnnotationPlugin_removeKeyValuePairById(FbcAnnotationPlugin_t* fap,
                                           const char* sid)
{
  return (fap != NULL && sid != NULL) ? fap->removeKeyValuePair(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


