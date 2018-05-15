/**
 * @file DistribSBasePlugin.cpp
 * @brief Implementation of the DistribSBasePlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/Model.h>
#include <sbml/util/ElementFilter.h>


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
  , mDistribUncertainty (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for DistribSBasePlugin.
 */
DistribSBasePlugin::DistribSBasePlugin(const DistribSBasePlugin& orig)
  : SBasePlugin( orig )
  , mDistribUncertainty ( NULL )
{
  if (orig.mDistribUncertainty != NULL)
  {
    mDistribUncertainty = orig.mDistribUncertainty->clone();
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
    delete mDistribUncertainty;
    if (rhs.mDistribUncertainty != NULL)
    {
      mDistribUncertainty = rhs.mDistribUncertainty->clone();
    }
    else
    {
      mDistribUncertainty = NULL;
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
  delete mDistribUncertainty;
  mDistribUncertainty = NULL;
}


/*
 * Returns the value of the "distribUncertainty" element of this
 * DistribSBasePlugin.
 */
const DistribUncertainty*
DistribSBasePlugin::getDistribUncertainty() const
{
  return mDistribUncertainty;
}


/*
 * Returns the value of the "distribUncertainty" element of this
 * DistribSBasePlugin.
 */
DistribUncertainty*
DistribSBasePlugin::getDistribUncertainty()
{
  return mDistribUncertainty;
}


/*
 * Predicate returning @c true if this DistribSBasePlugin's
 * "distribUncertainty" element is set.
 */
bool
DistribSBasePlugin::isSetDistribUncertainty() const
{
  return (mDistribUncertainty != NULL);
}


/*
 * Sets the value of the "distribUncertainty" element of this
 * DistribSBasePlugin.
 */
int
DistribSBasePlugin::setDistribUncertainty(const DistribUncertainty*
  distribUncertainty)
{
  if (distribUncertainty == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (distribUncertainty->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != distribUncertainty->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != distribUncertainty->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != distribUncertainty->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDistribUncertainty;
    mDistribUncertainty =
      static_cast<DistribUncertainty*>(distribUncertainty->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertainty object, adds it to this DistribSBasePlugin
 * object and returns the DistribUncertainty object created.
 */
DistribUncertainty*
DistribSBasePlugin::createDistribUncertainty()
{
  if (mDistribUncertainty != NULL)
  {
    delete mDistribUncertainty;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribUncertainty = new DistribUncertainty(distribns);

  mDistribUncertainty->setSBMLDocument(this->getSBMLDocument());

  delete distribns;

  connectToChild();

  return mDistribUncertainty;
}


/*
 * Unsets the value of the "distribUncertainty" element of this
 * DistribSBasePlugin.
 */
int
DistribSBasePlugin::unsetDistribUncertainty()
{
  delete mDistribUncertainty;
  mDistribUncertainty = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribSBasePlugin::writeElements(XMLOutputStream& stream) const
{
  if (isSetDistribUncertainty() == true)
  {
    mDistribUncertainty->write(stream);
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

  if (mDistribUncertainty != NULL)
  {
    mDistribUncertainty->accept(v);
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

  if (mDistribUncertainty != NULL)
  {
    mDistribUncertainty->setSBMLDocument(d);
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

  if (mDistribUncertainty != NULL)
  {
    mDistribUncertainty->connectToParent(base);
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
  if (isSetDistribUncertainty())
  {
    mDistribUncertainty->enablePackageInternal(pkgURI, pkgPrefix, flag);
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

  if (mDistribUncertainty != NULL)
  {
    mDistribUncertainty->updateSBMLNamespace(package, level, version);
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

  if (elementName == "distribUncertainty")
  {
    return createDistribUncertainty();
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
  if (elementName == "distribUncertainty" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTAINTY)
  {
    return setDistribUncertainty((const DistribUncertainty*)(element));
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
  if (elementName == "distribUncertainty")
  {
    DistribUncertainty * obj = getDistribUncertainty();
    if (unsetDistribUncertainty() == LIBSBML_OPERATION_SUCCESS) return obj;
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

  if (elementName == "distribUncertainty")
  {
    if (isSetDistribUncertainty())
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

  if (elementName == "distribUncertainty")
  {
    return getDistribUncertainty();
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

  if (mDistribUncertainty != NULL)
  {
    if (mDistribUncertainty->getId() == id)
    {
      return mDistribUncertainty;
    }

    obj = mDistribUncertainty->getElementBySId(id);
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

  if (mDistribUncertainty != NULL)
  {
    if (mDistribUncertainty->getMetaId() == metaid)
    {
      return mDistribUncertainty;
    }

    obj = mDistribUncertainty->getElementByMetaId(metaid);
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

  ADD_FILTERED_POINTER(ret, sublist, mDistribUncertainty, filter);


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
      if (isSetDistribUncertainty())
      {
        getErrorLog()->logPackageError("distrib", DistribSBaseAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
      }

      delete mDistribUncertainty;
      mDistribUncertainty = new DistribUncertainty(distribns);
      obj = mDistribUncertainty;
    }
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "distribUncertainty" element of this
 * DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
const DistribUncertainty_t*
DistribSBasePlugin_getDistribUncertainty(const DistribSBasePlugin_t * dsbp)
{
  if (dsbp == NULL)
  {
    return NULL;
  }

  return (DistribUncertainty_t*)(dsbp->getDistribUncertainty());
}


/*
 * Predicate returning @c 1 (true) if this DistribSBasePlugin_t's
 * "distribUncertainty" element is set.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_isSetDistribUncertainty(const DistribSBasePlugin_t * dsbp)
{
  return (dsbp != NULL) ? static_cast<int>(dsbp->isSetDistribUncertainty()) :
    0;
}


/*
 * Sets the value of the "distribUncertainty" element of this
 * DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_setDistribUncertainty(DistribSBasePlugin_t * dsbp,
                                         const DistribUncertainty_t*
                                           distribUncertainty)
{
  return (dsbp != NULL) ? dsbp->setDistribUncertainty(distribUncertainty) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertainty_t object, adds it to this
 * DistribSBasePlugin_t object and returns the DistribUncertainty_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertainty_t*
DistribSBasePlugin_createDistribUncertainty(DistribSBasePlugin_t* dsbp)
{
  if (dsbp == NULL)
  {
    return NULL;
  }

  return (DistribUncertainty_t*)(dsbp->createDistribUncertainty());
}


/*
 * Unsets the value of the "distribUncertainty" element of this
 * DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_unsetDistribUncertainty(DistribSBasePlugin_t * dsbp)
{
  return (dsbp != NULL) ? dsbp->unsetDistribUncertainty() :
    LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


