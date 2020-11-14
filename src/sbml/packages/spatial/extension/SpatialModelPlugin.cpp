/**
 * @file SpatialModelPlugin.cpp
 * @brief Implementation of the SpatialModelPlugin class.
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
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus
/*
 * Creates a new SpatialModelPlugin using the given URI, prefix and package
 * namespace.
 */
SpatialModelPlugin::SpatialModelPlugin(const std::string& uri,
                                       const std::string& prefix,
                                       SpatialPkgNamespaces* spatialns)
  : SBasePlugin(uri, prefix, spatialns)
  , mGeometry (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for SpatialModelPlugin.
 */
SpatialModelPlugin::SpatialModelPlugin(const SpatialModelPlugin& orig)
  : SBasePlugin( orig )
  , mGeometry ( NULL )
{
  if (orig.mGeometry != NULL)
  {
    mGeometry = orig.mGeometry->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for SpatialModelPlugin.
 */
SpatialModelPlugin&
SpatialModelPlugin::operator=(const SpatialModelPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    delete mGeometry;
    mGeometry = NULL;
    if (rhs.mGeometry != NULL)
    {
      mGeometry = rhs.mGeometry->clone();
    }
    else
    {
      mGeometry = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialModelPlugin object.
 */
SpatialModelPlugin*
SpatialModelPlugin::clone() const
{
  return new SpatialModelPlugin(*this);
}


/*
 * Destructor for SpatialModelPlugin.
 */
SpatialModelPlugin::~SpatialModelPlugin()
{
  delete mGeometry;
  mGeometry = NULL;
}


/*
 * Returns the value of the "geometry" element of this SpatialModelPlugin.
 */
const Geometry*
SpatialModelPlugin::getGeometry() const
{
  return mGeometry;
}


/*
 * Returns the value of the "geometry" element of this SpatialModelPlugin.
 */
Geometry*
SpatialModelPlugin::getGeometry()
{
  return mGeometry;
}


/*
 * Predicate returning @c true if this SpatialModelPlugin's "geometry" element
 * is set.
 */
bool
SpatialModelPlugin::isSetGeometry() const
{
  return (mGeometry != NULL);
}


/*
 * Sets the value of the "geometry" element of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setGeometry(const Geometry* geometry)
{
  if (geometry == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (geometry->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != geometry->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != geometry->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != geometry->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mGeometry;
    mGeometry = static_cast<Geometry*>(geometry->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Geometry object, adds it to this SpatialModelPlugin object and
 * returns the Geometry object created.
 */
Geometry*
SpatialModelPlugin::createGeometry()
{
  if (mGeometry != NULL)
  {
    delete mGeometry;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mGeometry = new Geometry(spatialns);

  mGeometry->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mGeometry;
}


/*
 * Unsets the value of the "geometry" element of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::unsetGeometry()
{
  delete mGeometry;
  mGeometry = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SpatialModelPlugin::writeElements(XMLOutputStream& stream) const
{
  if (isSetGeometry() == true)
  {
    mGeometry->write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SpatialModelPlugin::accept(SBMLVisitor& v) const
{
  const Model* m = static_cast<const Model*>(this->getParentSBMLObject());
  v.visit(*m);
  v.leave(*m);

  if (mGeometry != NULL)
  {
    mGeometry->accept(v);
  }

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialModelPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (mGeometry != NULL)
  {
    mGeometry->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
SpatialModelPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
SpatialModelPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  if (mGeometry != NULL)
  {
    mGeometry->connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  if (isSetGeometry())
  {
    mGeometry->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
SpatialModelPlugin::updateSBMLNamespace(const std::string& package,
                                        unsigned int level,
                                        unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (mGeometry != NULL)
  {
    mGeometry->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialModelPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialModelPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialModelPlugin.
 */
int
SpatialModelPlugin::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialModelPlugin.
 */
int
SpatialModelPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this SpatialModelPlugin.
 */
SBase*
SpatialModelPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "geometry")
  {
    return createGeometry();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this SpatialModelPlugin.
 */
int
SpatialModelPlugin::addChildObject(const std::string& elementName,
                                   const SBase* element)
{
  if (elementName == "geometry" && element->getTypeCode() ==
    SBML_SPATIAL_GEOMETRY)
  {
    return setGeometry((const Geometry*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * SpatialModelPlugin.
 */
SBase*
SpatialModelPlugin::removeChildObject(const std::string& elementName,
                                      const std::string& id)
{
  if (elementName == "geometry")
  {
    Geometry * obj = getGeometry();
    if (unsetGeometry() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this SpatialModelPlugin.
 */
unsigned int
SpatialModelPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "geometry")
  {
    if (isSetGeometry())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this SpatialModelPlugin.
 */
SBase*
SpatialModelPlugin::getObject(const std::string& elementName,
                              unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "geometry")
  {
    return getGeometry();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
SpatialModelPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGeometry != NULL)
  {
    if (mGeometry->getId() == id)
    {
      return mGeometry;
    }

    obj = mGeometry->getElementBySId(id);
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
SpatialModelPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGeometry != NULL)
  {
    if (mGeometry->getMetaId() == metaid)
    {
      return mGeometry;
    }

    obj = mGeometry->getElementByMetaId(metaid);
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
SpatialModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mGeometry, filter);


  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
SpatialModelPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const SpatialModelPlugin* plug = static_cast<const
    SpatialModelPlugin*>(model->getPlugin(getPrefix()));

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
SpatialModelPlugin::createObject(XMLInputStream& stream)
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
    if (name == "geometry")
    {
      if (isSetGeometry())
      {
        getErrorLog()->logPackageError("spatial", SpatialModelAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
      }

      delete mGeometry;
        mGeometry = NULL;
      mGeometry = new Geometry(spatialns);
      obj = mGeometry;
    }
  }

  delete spatialns;

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "geometry" element of this SpatialModelPlugin_t.
 */
LIBSBML_EXTERN
const Geometry_t*
SpatialModelPlugin_getGeometry(const SpatialModelPlugin_t * smp)
{
  if (smp == NULL)
  {
    return NULL;
  }

  return (Geometry_t*)(smp->getGeometry());
}


/*
 * Predicate returning @c 1 (true) if this SpatialModelPlugin_t's "geometry"
 * element is set.
 */
LIBSBML_EXTERN
int
SpatialModelPlugin_isSetGeometry(const SpatialModelPlugin_t * smp)
{
  return (smp != NULL) ? static_cast<int>(smp->isSetGeometry()) : 0;
}


/*
 * Sets the value of the "geometry" element of this SpatialModelPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialModelPlugin_setGeometry(SpatialModelPlugin_t * smp,
                               const Geometry_t* geometry)
{
  return (smp != NULL) ? smp->setGeometry(geometry) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new Geometry_t object, adds it to this SpatialModelPlugin_t object
 * and returns the Geometry_t object created.
 */
LIBSBML_EXTERN
Geometry_t*
SpatialModelPlugin_createGeometry(SpatialModelPlugin_t* smp)
{
  if (smp == NULL)
  {
    return NULL;
  }

  return (Geometry_t*)(smp->createGeometry());
}


/*
 * Unsets the value of the "geometry" element of this SpatialModelPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialModelPlugin_unsetGeometry(SpatialModelPlugin_t * smp)
{
  return (smp != NULL) ? smp->unsetGeometry() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


