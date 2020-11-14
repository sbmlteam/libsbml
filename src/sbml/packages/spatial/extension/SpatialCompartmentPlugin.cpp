/**
 * @file SpatialCompartmentPlugin.cpp
 * @brief Implementation of the SpatialCompartmentPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialCompartmentPlugin using the given URI, prefix and
 * package namespace.
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin(const std::string& uri,
                                                   const std::string& prefix,
                                                   SpatialPkgNamespaces*
                                                     spatialns)
  : SBasePlugin(uri, prefix, spatialns)
  , mCompartmentMapping (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin(const
  SpatialCompartmentPlugin& orig)
  : SBasePlugin( orig )
  , mCompartmentMapping ( NULL )
{
  if (orig.mCompartmentMapping != NULL)
  {
    mCompartmentMapping = orig.mCompartmentMapping->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin&
SpatialCompartmentPlugin::operator=(const SpatialCompartmentPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    delete mCompartmentMapping;
    mCompartmentMapping = NULL;
    if (rhs.mCompartmentMapping != NULL)
    {
      mCompartmentMapping = rhs.mCompartmentMapping->clone();
    }
    else
    {
      mCompartmentMapping = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialCompartmentPlugin object.
 */
SpatialCompartmentPlugin*
SpatialCompartmentPlugin::clone() const
{
  return new SpatialCompartmentPlugin(*this);
}


/*
 * Destructor for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin::~SpatialCompartmentPlugin()
{
  delete mCompartmentMapping;
  mCompartmentMapping = NULL;
}


/*
 * Returns the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin.
 */
const CompartmentMapping*
SpatialCompartmentPlugin::getCompartmentMapping() const
{
  return mCompartmentMapping;
}


/*
 * Returns the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin.
 */
CompartmentMapping*
SpatialCompartmentPlugin::getCompartmentMapping()
{
  return mCompartmentMapping;
}


/*
 * Predicate returning @c true if this SpatialCompartmentPlugin's
 * "compartmentMapping" element is set.
 */
bool
SpatialCompartmentPlugin::isSetCompartmentMapping() const
{
  return (mCompartmentMapping != NULL);
}


/*
 * Sets the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setCompartmentMapping(const CompartmentMapping*
  compartmentMapping)
{
  if (compartmentMapping == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (compartmentMapping->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != compartmentMapping->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != compartmentMapping->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != compartmentMapping->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mCompartmentMapping;
    mCompartmentMapping =
      static_cast<CompartmentMapping*>(compartmentMapping->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new CompartmentMapping object, adds it to this
 * SpatialCompartmentPlugin object and returns the CompartmentMapping object
 * created.
 */
CompartmentMapping*
SpatialCompartmentPlugin::createCompartmentMapping()
{
  if (mCompartmentMapping != NULL)
  {
    delete mCompartmentMapping;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCompartmentMapping = new CompartmentMapping(spatialns);

  mCompartmentMapping->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mCompartmentMapping;
}


/*
 * Unsets the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::unsetCompartmentMapping()
{
  delete mCompartmentMapping;
  mCompartmentMapping = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SpatialCompartmentPlugin::writeElements(XMLOutputStream& stream) const
{
  if (isSetCompartmentMapping() == true)
  {
    mCompartmentMapping->write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SpatialCompartmentPlugin::accept(SBMLVisitor& v) const
{
  const Compartment* c = static_cast<const
    Compartment*>(this->getParentSBMLObject());
  v.visit(*c);
  v.leave(*c);

  if (mCompartmentMapping != NULL)
  {
    mCompartmentMapping->accept(v);
  }

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialCompartmentPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (mCompartmentMapping != NULL)
  {
    mCompartmentMapping->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
SpatialCompartmentPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
SpatialCompartmentPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  if (mCompartmentMapping != NULL)
  {
    mCompartmentMapping->connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialCompartmentPlugin::enablePackageInternal(const std::string& pkgURI,
                                                const std::string& pkgPrefix,
                                                bool flag)
{
  if (isSetCompartmentMapping())
  {
    mCompartmentMapping->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
SpatialCompartmentPlugin::updateSBMLNamespace(const std::string& package,
                                              unsigned int level,
                                              unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (mCompartmentMapping != NULL)
  {
    mCompartmentMapping->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::getAttribute(const std::string& attributeName,
                                       bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::getAttribute(const std::string& attributeName,
                                       int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::getAttribute(const std::string& attributeName,
                                       double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::getAttribute(const std::string& attributeName,
                                       unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialCompartmentPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialCompartmentPlugin::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setAttribute(const std::string& attributeName,
                                       bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setAttribute(const std::string& attributeName,
                                       int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setAttribute(const std::string& attributeName,
                                       double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setAttribute(const std::string& attributeName,
                                       unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::setAttribute(const std::string& attributeName,
                                       const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * SpatialCompartmentPlugin.
 */
SBase*
SpatialCompartmentPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "compartmentMapping")
  {
    return createCompartmentMapping();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this SpatialCompartmentPlugin.
 */
int
SpatialCompartmentPlugin::addChildObject(const std::string& elementName,
                                         const SBase* element)
{
  if (elementName == "compartmentMapping" && element->getTypeCode() ==
    SBML_SPATIAL_COMPARTMENTMAPPING)
  {
    return setCompartmentMapping((const CompartmentMapping*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * SpatialCompartmentPlugin.
 */
SBase*
SpatialCompartmentPlugin::removeChildObject(const std::string& elementName,
                                            const std::string& id)
{
  if (elementName == "compartmentMapping")
  {
    CompartmentMapping * obj = getCompartmentMapping();
    if (unsetCompartmentMapping() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this SpatialCompartmentPlugin.
 */
unsigned int
SpatialCompartmentPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "compartmentMapping")
  {
    if (isSetCompartmentMapping())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this SpatialCompartmentPlugin.
 */
SBase*
SpatialCompartmentPlugin::getObject(const std::string& elementName,
                                    unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "compartmentMapping")
  {
    return getCompartmentMapping();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
SpatialCompartmentPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCompartmentMapping != NULL)
  {
    if (mCompartmentMapping->getId() == id)
    {
      return mCompartmentMapping;
    }

    obj = mCompartmentMapping->getElementBySId(id);
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
SpatialCompartmentPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCompartmentMapping != NULL)
  {
    if (mCompartmentMapping->getMetaId() == metaid)
    {
      return mCompartmentMapping;
    }

    obj = mCompartmentMapping->getElementByMetaId(metaid);
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
SpatialCompartmentPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCompartmentMapping, filter);


  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
SpatialCompartmentPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const SpatialCompartmentPlugin* plug = static_cast<const
    SpatialCompartmentPlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  return ret;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
SpatialCompartmentPlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (prefix == targetPrefix)
  {
    if (name == "compartmentMapping")
    {
      if (isSetCompartmentMapping())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialCompartmentAllowedElements, getPackageVersion(), getLevel(),
            getVersion());
      }

      delete mCompartmentMapping;
        mCompartmentMapping = NULL;
      mCompartmentMapping = new CompartmentMapping(spatialns);
      obj = mCompartmentMapping;
    }
  }

  delete spatialns;

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin_t.
 */
LIBSBML_EXTERN
const CompartmentMapping_t*
SpatialCompartmentPlugin_getCompartmentMapping(const SpatialCompartmentPlugin_t
  * scp)
{
  if (scp == NULL)
  {
    return NULL;
  }

  return (CompartmentMapping_t*)(scp->getCompartmentMapping());
}


/*
 * Predicate returning @c 1 (true) if this SpatialCompartmentPlugin_t's
 * "compartmentMapping" element is set.
 */
LIBSBML_EXTERN
int
SpatialCompartmentPlugin_isSetCompartmentMapping(const
  SpatialCompartmentPlugin_t * scp)
{
  return (scp != NULL) ? static_cast<int>(scp->isSetCompartmentMapping()) : 0;
}


/*
 * Sets the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialCompartmentPlugin_setCompartmentMapping(
                                               SpatialCompartmentPlugin_t *
                                                 scp,
                                               const CompartmentMapping_t*
                                                 compartmentMapping)
{
  return (scp != NULL) ? scp->setCompartmentMapping(compartmentMapping) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new CompartmentMapping_t object, adds it to this
 * SpatialCompartmentPlugin_t object and returns the CompartmentMapping_t
 * object created.
 */
LIBSBML_EXTERN
CompartmentMapping_t*
SpatialCompartmentPlugin_createCompartmentMapping(SpatialCompartmentPlugin_t*
  scp)
{
  if (scp == NULL)
  {
    return NULL;
  }

  return (CompartmentMapping_t*)(scp->createCompartmentMapping());
}


/*
 * Unsets the value of the "compartmentMapping" element of this
 * SpatialCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialCompartmentPlugin_unsetCompartmentMapping(SpatialCompartmentPlugin_t *
  scp)
{
  return (scp != NULL) ? scp->unsetCompartmentMapping() :
    LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


