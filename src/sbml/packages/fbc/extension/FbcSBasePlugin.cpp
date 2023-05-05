/**
 * @file FbcSBasePlugin.cpp
 * @brief Implementation of the FbcSBasePlugin class.
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
#include <sbml/packages/fbc/extension/FbcSBasePlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new FbcSBasePlugin using the given URI, prefix and package
 * namespace.
 */
FbcSBasePlugin::FbcSBasePlugin(const std::string& uri,
                               const std::string& prefix,
                               FbcPkgNamespaces* fbcns)
  : SBasePlugin(uri, prefix, fbcns)
  , mKeyValuePairs (fbcns)
{
  connectToChild();
}


/*
 * Copy constructor for FbcSBasePlugin.
 */
FbcSBasePlugin::FbcSBasePlugin(const FbcSBasePlugin& orig)
  : SBasePlugin( orig )
  , mKeyValuePairs ( orig.mKeyValuePairs )
{
  connectToChild();
}


/*
 * Assignment operator for FbcSBasePlugin.
 */
FbcSBasePlugin&
FbcSBasePlugin::operator=(const FbcSBasePlugin& rhs)
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
 * Creates and returns a deep copy of this FbcSBasePlugin object.
 */
FbcSBasePlugin*
FbcSBasePlugin::clone() const
{
  return new FbcSBasePlugin(*this);
}


/*
 * Destructor for FbcSBasePlugin.
 */
FbcSBasePlugin::~FbcSBasePlugin()
{
}


/*
 * Returns the ListOfKeyValuePairs from this FbcSBasePlugin.
 */
const ListOfKeyValuePairs*
FbcSBasePlugin::getListOfKeyValuePairs() const
{
  return &mKeyValuePairs;
}


/*
 * Returns the ListOfKeyValuePairs from this FbcSBasePlugin.
 */
ListOfKeyValuePairs*
FbcSBasePlugin::getListOfKeyValuePairs()
{
  return &mKeyValuePairs;
}


/*
 * Get a KeyValuePair from the FbcSBasePlugin.
 */
KeyValuePair*
FbcSBasePlugin::getKeyValuePair(unsigned int n)
{
  return static_cast< KeyValuePair*>(mKeyValuePairs.get(n));
}


/*
 * Get a KeyValuePair from the FbcSBasePlugin.
 */
const KeyValuePair*
FbcSBasePlugin::getKeyValuePair(unsigned int n) const
{
  return static_cast<const KeyValuePair*>(mKeyValuePairs.get(n));
}


/*
 * Get a KeyValuePair from the FbcSBasePlugin based on its identifier.
 */
KeyValuePair*
FbcSBasePlugin::getKeyValuePair(const std::string& sid)
{
  return static_cast< KeyValuePair*>(mKeyValuePairs.get(sid));
}


/*
 * Get a KeyValuePair from the FbcSBasePlugin based on its identifier.
 */
const KeyValuePair*
FbcSBasePlugin::getKeyValuePair(const std::string& sid) const
{
  return static_cast<const KeyValuePair*>(mKeyValuePairs.get(sid));
}


/*
 * Adds a copy of the given KeyValuePair to this FbcSBasePlugin.
 */
int
FbcSBasePlugin::addKeyValuePair(const KeyValuePair* kvp)
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
 * Get the number of KeyValuePair objects in this FbcSBasePlugin.
 */
unsigned int
FbcSBasePlugin::getNumKeyValuePairs() const
{
  return mKeyValuePairs.size();
}


/*
 * Creates a new KeyValuePair object, adds it to this FbcSBasePlugin object and
 * returns the KeyValuePair object created.
 */
KeyValuePair*
FbcSBasePlugin::createKeyValuePair()
{
  KeyValuePair* kvp = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
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
 * Removes the nth KeyValuePair from this FbcSBasePlugin and returns a pointer
 * to it.
 */
KeyValuePair*
FbcSBasePlugin::removeKeyValuePair(unsigned int n)
{
  return static_cast<KeyValuePair*>(mKeyValuePairs.remove(n));
}


/*
 * Removes the KeyValuePair from this FbcSBasePlugin based on its identifier
 * and returns a pointer to it.
 */
KeyValuePair*
FbcSBasePlugin::removeKeyValuePair(const std::string& sid)
{
  return static_cast<KeyValuePair*>(mKeyValuePairs.remove(sid));
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
FbcSBasePlugin::writeAttributes(XMLOutputStream& stream) const
{
  if (getNumKeyValuePairs() > 0)
  {
    SBase* parent = const_cast<SBase*>(getParentSBMLObject());
    writeKeyValuePairsAnnotation(parent);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
FbcSBasePlugin::accept(SBMLVisitor& v) const
{
  const SBase* sb = static_cast<const SBase*>(this->getParentSBMLObject());
  v.visit(*sb);
  v.leave(*sb);

  mKeyValuePairs.accept(v);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
FbcSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);
  if (getNumKeyValuePairs() > 0)
  {
    mKeyValuePairs.setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
FbcSBasePlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
FbcSBasePlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);
  if (getNumKeyValuePairs() > 0)
  {
    mKeyValuePairs.connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
FbcSBasePlugin::enablePackageInternal(const std::string& pkgURI,
  const std::string& pkgPrefix,
  bool flag)
{
  if (getNumKeyValuePairs() > 0)
  {
    mKeyValuePairs.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
FbcSBasePlugin::updateSBMLNamespace(const std::string& package,
  unsigned int level,
  unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (getNumKeyValuePairs() > 0)
  {
    mKeyValuePairs.updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  if (attributeName == "xmlns")
  {
    value = getListOfKeyValuePairs()->getXmlns();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcSBasePlugin's attribute
 * "attributeName" is set.
 */
bool
FbcSBasePlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "xmlns")
  {
    value = getListOfKeyValuePairs()->isSetXmlns();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "xmlns")
  {
    return_value = getListOfKeyValuePairs()->setXmlns(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this FbcSBasePlugin.
 */
int
FbcSBasePlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);
  if (attributeName == "xmlns")
  {
    value = getListOfKeyValuePairs()->unsetXmlns();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this FbcSBasePlugin.
 */
SBase*
FbcSBasePlugin::createChildObject(const std::string& elementName)
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
 * Adds a new "elementName" object to this FbcSBasePlugin.
 */
int
FbcSBasePlugin::addChildObject(const std::string& elementName,
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
 * FbcSBasePlugin.
 */
SBase*
FbcSBasePlugin::removeChildObject(const std::string& elementName,
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
 * Returns the number of "elementName" in this FbcSBasePlugin.
 */
unsigned int
FbcSBasePlugin::getNumObjects(const std::string& elementName)
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
 * Returns the nth object of "objectName" in this FbcSBasePlugin.
 */
SBase*
FbcSBasePlugin::getObject(const std::string& elementName, unsigned int index)
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
FbcSBasePlugin::getElementBySId(const std::string& id)
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
FbcSBasePlugin::getElementByMetaId(const std::string& metaid)
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
FbcSBasePlugin::getAllElements(ElementFilter* filter)
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
FbcSBasePlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const FbcSBasePlugin* plug = dynamic_cast<const
    FbcSBasePlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
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
FbcSBasePlugin::createObject(XMLInputStream& stream)
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

/** @cond doxygenLibsbmlInternal */
/**
* Synchronizes the annotation of this SBML object.
*
* Annotation element (XMLNode* mAnnotation) is synchronized with the
* current CVTerm objects (List* mCVTerm).
* Currently, this method is called in getAnnotation, isSetAnnotation,
* and writeElements methods.
*/
void 
FbcSBasePlugin::writeKeyValuePairsAnnotation(SBase* parentObject) const
{
  if (parentObject == NULL) return;


  XMLNode *parentAnnotation = parentObject->getAnnotation();
  if (parentAnnotation != NULL && parentAnnotation->getNumChildren() > 0)
  {
//    deleteFbcAnnotation(parentAnnotation);
  }

  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
  XMLNode* annt = new XMLNode(ann_token);



  if (mKeyValuePairs.size() > 0)
  {
    XMLAttributes loga_attr = XMLAttributes();
    loga_attr.add("xmlns", mKeyValuePairs.getXmlns());
    XMLToken loga_token = XMLToken(XMLTriple("listOfKeyValuePairs", mKeyValuePairs.getXmlns(), ""), loga_attr);
    XMLNode loga = XMLNode(loga_token);

    for (unsigned int i = 0; i < mKeyValuePairs.size(); ++i)
      loga.addChild(mKeyValuePairs.get(i)->toXML());

    // then add the ones toXML()
    annt->addChild(loga);
  }


  if (annt && annt->getNumChildren() > 0)
  {
    parentObject->appendAnnotation(annt);
  }
  delete annt;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/**
* Parse L2 annotation if supported
*
*/
void 
FbcSBasePlugin::parseAnnotation(SBase *parentObject, XMLNode *pAnnotation)
{
  mKeyValuePairs.setSBMLDocument(mSBML);
  // don't read if we have an invalid node or already a gene associations object
  if (pAnnotation == NULL || mKeyValuePairs.size() > 0)
    return;

  // annotation element has been parsed by the parent element
  // (Model) of this plugin object, thus the annotation element 
  // set to the above pAnnotation variable is parsed in this block.

  XMLNode& listOfKeyValuePairs = pAnnotation->getChild("listOfKeyValuePairs");
  if (listOfKeyValuePairs.getNumChildren() == 0)
    return;

  XMLNamespaces oldNs = listOfKeyValuePairs.getNamespaces();
  
  // read the xml node, overriding that all errors are flagged as 
  // warnings  
  mKeyValuePairs.read(listOfKeyValuePairs, LIBSBML_OVERRIDE_WARNING);

  // unfortunately the namespaces are overwritten at that point with the one from the 
  // document, so restore it
  mKeyValuePairs.setXmlns(&oldNs, listOfKeyValuePairs.getPrefix());

  // remove listOfLayouts annotation  
  parentObject->removeTopLevelAnnotationElement("listOfKeyValuePairs", "", false);

}
/** @endcond */





#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing KeyValuePair_t objects from this
 * FbcSBasePlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
FbcSBasePlugin_getListOfKeyValuePairs(FbcSBasePlugin_t* fsbp)
{
  return (fsbp != NULL) ? fsbp->getListOfKeyValuePairs() : NULL;
}


/*
 * Get a KeyValuePair_t from the FbcSBasePlugin_t.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcSBasePlugin_getKeyValuePair(FbcSBasePlugin_t* fsbp, unsigned int n)
{
  return (fsbp != NULL) ? fsbp->getKeyValuePair(n) : NULL;
}


/*
 * Get a KeyValuePair_t from the FbcSBasePlugin_t based on its identifier.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcSBasePlugin_getKeyValuePairById(FbcSBasePlugin_t* fsbp, const char *sid)
{
  return (fsbp != NULL && sid != NULL) ? fsbp->getKeyValuePair(sid) : NULL;
}


/*
 * Adds a copy of the given KeyValuePair_t to this FbcSBasePlugin_t.
 */
LIBSBML_EXTERN
int
FbcSBasePlugin_addKeyValuePair(FbcSBasePlugin_t* fsbp,
                               const KeyValuePair_t* kvp)
{
  return (fsbp != NULL) ? fsbp->addKeyValuePair(kvp) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of KeyValuePair_t objects in this FbcSBasePlugin_t.
 */
LIBSBML_EXTERN
unsigned int
FbcSBasePlugin_getNumKeyValuePairs(FbcSBasePlugin_t* fsbp)
{
  return (fsbp != NULL) ? fsbp->getNumKeyValuePairs() : SBML_INT_MAX;
}


/*
 * Creates a new KeyValuePair_t object, adds it to this FbcSBasePlugin_t object
 * and returns the KeyValuePair_t object created.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcSBasePlugin_createKeyValuePair(FbcSBasePlugin_t* fsbp)
{
  return (fsbp != NULL) ? fsbp->createKeyValuePair() : NULL;
}


/*
 * Removes the nth KeyValuePair_t from this FbcSBasePlugin_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcSBasePlugin_removeKeyValuePair(FbcSBasePlugin_t* fsbp, unsigned int n)
{
  return (fsbp != NULL) ? fsbp->removeKeyValuePair(n) : NULL;
}


/*
 * Removes the KeyValuePair_t from this FbcSBasePlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
FbcSBasePlugin_removeKeyValuePairById(FbcSBasePlugin_t* fsbp, const char* sid)
{
  return (fsbp != NULL && sid != NULL) ? fsbp->removeKeyValuePair(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


