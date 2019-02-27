/**
 * @file DistribSBasePlugin.cpp
 * @brief Implementation of the DistribSBasePlugin class.
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
#include <sbml/packages/distrib/extension/DistribSBasePlugin.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribSBasePlugin using the given URI, prefix and package
 * namespace.
 */
DistribSBasePlugin::DistribSBasePlugin(const std::string& uri,
                                       const std::string& prefix,
                                       DistribPkgNamespaces* distribns)
  : SBasePlugin(uri, prefix, distribns)
  , mUncertainty (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for DistribSBasePlugin.
 */
DistribSBasePlugin::DistribSBasePlugin(const DistribSBasePlugin& orig)
  : SBasePlugin( orig )
  , mUncertainty ( NULL )
{
  if (orig.mUncertainty != NULL)
  {
    mUncertainty = orig.mUncertainty->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribSBasePlugin.
 */
DistribSBasePlugin&
DistribSBasePlugin::operator=(const DistribSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    delete mUncertainty;
    if (rhs.mUncertainty != NULL)
    {
      mUncertainty = rhs.mUncertainty->clone();
    }
    else
    {
      mUncertainty = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribSBasePlugin object.
 */
DistribSBasePlugin*
DistribSBasePlugin::clone() const
{
  return new DistribSBasePlugin(*this);
}


/*
 * Destructor for DistribSBasePlugin.
 */
DistribSBasePlugin::~DistribSBasePlugin()
{
  delete mUncertainty;
  mUncertainty = NULL;
}


/*
 * Returns the value of the "uncertainty" element of this DistribSBasePlugin.
 */
const Uncertainty*
DistribSBasePlugin::getUncertainty() const
{
  return mUncertainty;
}


/*
 * Returns the value of the "uncertainty" element of this DistribSBasePlugin.
 */
Uncertainty*
DistribSBasePlugin::getUncertainty()
{
  return mUncertainty;
}


/*
 * Predicate returning @c true if this DistribSBasePlugin's "uncertainty"
 * element is set.
 */
bool
DistribSBasePlugin::isSetUncertainty() const
{
  return (mUncertainty != NULL);
}


/*
 * Sets the value of the "uncertainty" element of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setUncertainty(const Uncertainty* uncertainty)
{
  if (uncertainty == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (uncertainty->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != uncertainty->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != uncertainty->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != uncertainty->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mUncertainty;
    mUncertainty = static_cast<Uncertainty*>(uncertainty->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Uncertainty object, adds it to this DistribSBasePlugin object
 * and returns the Uncertainty object created.
 */
Uncertainty*
DistribSBasePlugin::createUncertainty()
{
  if (mUncertainty != NULL)
  {
    delete mUncertainty;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mUncertainty = new Uncertainty(distribns);

  mUncertainty->setSBMLDocument(this->getSBMLDocument());

  delete distribns;

  connectToChild();

  return mUncertainty;
}


/*
 * Unsets the value of the "uncertainty" element of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::unsetUncertainty()
{
  delete mUncertainty;
  mUncertainty = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribSBasePlugin::writeElements(XMLOutputStream& stream) const
{
  if (isSetUncertainty() == true)
  {
    mUncertainty->write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribSBasePlugin::accept(SBMLVisitor& v) const
{
  const SBase* sb = static_cast<const SBase*>(this->getParentSBMLObject());
  v.visit(*sb);
  v.leave(*sb);

  if (mUncertainty != NULL)
  {
    mUncertainty->accept(v);
  }

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (mUncertainty != NULL)
  {
    mUncertainty->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribSBasePlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
DistribSBasePlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  if (mUncertainty != NULL)
  {
    mUncertainty->connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  if (isSetUncertainty())
  {
    mUncertainty->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribSBasePlugin::updateSBMLNamespace(const std::string& package,
                                        unsigned int level,
                                        unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (mUncertainty != NULL)
  {
    mUncertainty->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribSBasePlugin's attribute
 * "attributeName" is set.
 */
bool
DistribSBasePlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribSBasePlugin.
 */
int
DistribSBasePlugin::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribSBasePlugin.
 */
int
DistribSBasePlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this DistribSBasePlugin.
 */
SBase*
DistribSBasePlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "uncertainty")
  {
    return createUncertainty();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribSBasePlugin.
 */
int
DistribSBasePlugin::addChildObject(const std::string& elementName,
                                   const SBase* element)
{
  if (elementName == "uncertainty" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTAINTY)
  {
    return setUncertainty((const Uncertainty*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribSBasePlugin.
 */
SBase*
DistribSBasePlugin::removeChildObject(const std::string& elementName,
                                      const std::string& id)
{
  if (elementName == "uncertainty")
  {
    Uncertainty * obj = getUncertainty();
    if (unsetUncertainty() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribSBasePlugin.
 */
unsigned int
DistribSBasePlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "uncertainty")
  {
    if (isSetUncertainty())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribSBasePlugin.
 */
SBase*
DistribSBasePlugin::getObject(const std::string& elementName,
                              unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "uncertainty")
  {
    return getUncertainty();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribSBasePlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertainty != NULL)
  {
    if (mUncertainty->getId() == id)
    {
      return mUncertainty;
    }

    obj = mUncertainty->getElementBySId(id);
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
DistribSBasePlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertainty != NULL)
  {
    if (mUncertainty->getMetaId() == metaid)
    {
      return mUncertainty;
    }

    obj = mUncertainty->getElementByMetaId(metaid);
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
DistribSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mUncertainty, filter);


  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
DistribSBasePlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const DistribSBasePlugin* plug = static_cast<const
    DistribSBasePlugin*>(model->getPlugin(getPrefix()));

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
DistribSBasePlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (prefix == targetPrefix)
  {
    if (name == "uncertainty")
    {
      if (isSetUncertainty())
      {
        getErrorLog()->logPackageError("distrib", DistribSBaseAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
      }

      delete mUncertainty;
      mUncertainty = new Uncertainty(distribns);
      obj = mUncertainty;
    }
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "uncertainty" element of this DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
const Uncertainty_t*
DistribSBasePlugin_getUncertainty(const DistribSBasePlugin_t * dsbp)
{
  if (dsbp == NULL)
  {
    return NULL;
  }

  return (Uncertainty_t*)(dsbp->getUncertainty());
}


/*
 * Predicate returning @c 1 (true) if this DistribSBasePlugin_t's "uncertainty"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_isSetUncertainty(const DistribSBasePlugin_t * dsbp)
{
  return (dsbp != NULL) ? static_cast<int>(dsbp->isSetUncertainty()) : 0;
}


/*
 * Sets the value of the "uncertainty" element of this DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_setUncertainty(DistribSBasePlugin_t * dsbp,
                                  const Uncertainty_t* uncertainty)
{
  return (dsbp != NULL) ? dsbp->setUncertainty(uncertainty) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new Uncertainty_t object, adds it to this DistribSBasePlugin_t
 * object and returns the Uncertainty_t object created.
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_createUncertainty(DistribSBasePlugin_t* dsbp)
{
  if (dsbp == NULL)
  {
    return NULL;
  }

  return (Uncertainty_t*)(dsbp->createUncertainty());
}


/*
 * Unsets the value of the "uncertainty" element of this DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_unsetUncertainty(DistribSBasePlugin_t * dsbp)
{
  return (dsbp != NULL) ? dsbp->unsetUncertainty() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


