/**
 * @file DistribFunctionDefinitionPlugin.cpp
 * @brief Implementation of the DistribFunctionDefinitionPlugin class.
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
#include <sbml/packages/distrib/extension/DistribFunctionDefinitionPlugin.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/Model.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribFunctionDefinitionPlugin using the given URI, prefix
 * and package namespace.
 */
DistribFunctionDefinitionPlugin::DistribFunctionDefinitionPlugin(
                                                                 const
                                                                   std::string&
                                                                     uri,
                                                                 const
                                                                   std::string&
                                                                     prefix,
                                                                 DistribPkgNamespaces*
                                                                   distribns)
  : SBasePlugin(uri, prefix, distribns)
  , mDistribDrawFromDistribution (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin::DistribFunctionDefinitionPlugin(const
  DistribFunctionDefinitionPlugin& orig)
  : SBasePlugin( orig )
  , mDistribDrawFromDistribution ( NULL )
{
  if (orig.mDistribDrawFromDistribution != NULL)
  {
    mDistribDrawFromDistribution = orig.mDistribDrawFromDistribution->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin&
DistribFunctionDefinitionPlugin::operator=(const
  DistribFunctionDefinitionPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    delete mDistribDrawFromDistribution;
    if (rhs.mDistribDrawFromDistribution != NULL)
    {
      mDistribDrawFromDistribution = rhs.mDistribDrawFromDistribution->clone();
    }
    else
    {
      mDistribDrawFromDistribution = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribFunctionDefinitionPlugin
 * object.
 */
DistribFunctionDefinitionPlugin*
DistribFunctionDefinitionPlugin::clone() const
{
  return new DistribFunctionDefinitionPlugin(*this);
}


/*
 * Destructor for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin::~DistribFunctionDefinitionPlugin()
{
  delete mDistribDrawFromDistribution;
  mDistribDrawFromDistribution = NULL;
}


/*
 * Returns the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin.
 */
const DistribDrawFromDistribution*
DistribFunctionDefinitionPlugin::getDistribDrawFromDistribution() const
{
  return mDistribDrawFromDistribution;
}


/*
 * Returns the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin.
 */
DistribDrawFromDistribution*
DistribFunctionDefinitionPlugin::getDistribDrawFromDistribution()
{
  return mDistribDrawFromDistribution;
}


/*
 * Predicate returning @c true if this DistribFunctionDefinitionPlugin's
 * "distribDrawFromDistribution" element is set.
 */
bool
DistribFunctionDefinitionPlugin::isSetDistribDrawFromDistribution() const
{
  return (mDistribDrawFromDistribution != NULL);
}


/*
 * Sets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setDistribDrawFromDistribution(const
  DistribDrawFromDistribution* distribDrawFromDistribution)
{
  if (distribDrawFromDistribution == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (distribDrawFromDistribution->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != distribDrawFromDistribution->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != distribDrawFromDistribution->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() !=
    distribDrawFromDistribution->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDistribDrawFromDistribution;
    mDistribDrawFromDistribution =
      static_cast<DistribDrawFromDistribution*>(distribDrawFromDistribution->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribDrawFromDistribution object, adds it to this
 * DistribFunctionDefinitionPlugin object and returns the
 * DistribDrawFromDistribution object created.
 */
DistribDrawFromDistribution*
DistribFunctionDefinitionPlugin::createDistribDrawFromDistribution()
{
  if (mDistribDrawFromDistribution != NULL)
  {
    delete mDistribDrawFromDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribDrawFromDistribution = new DistribDrawFromDistribution(distribns);

  mDistribDrawFromDistribution->setSBMLDocument(this->getSBMLDocument());

  delete distribns;

  connectToChild();

  return mDistribDrawFromDistribution;
}


/*
 * Unsets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::unsetDistribDrawFromDistribution()
{
  delete mDistribDrawFromDistribution;
  mDistribDrawFromDistribution = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribFunctionDefinitionPlugin::writeElements(XMLOutputStream& stream) const
{
  if (isSetDistribDrawFromDistribution() == true)
  {
    mDistribDrawFromDistribution->write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribFunctionDefinitionPlugin::accept(SBMLVisitor& v) const
{
  const FunctionDefinition* fd = static_cast<const
    FunctionDefinition*>(this->getParentSBMLObject());
  v.visit(*fd);
  v.leave(*fd);

  if (mDistribDrawFromDistribution != NULL)
  {
    mDistribDrawFromDistribution->accept(v);
  }

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribFunctionDefinitionPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (mDistribDrawFromDistribution != NULL)
  {
    mDistribDrawFromDistribution->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribFunctionDefinitionPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
DistribFunctionDefinitionPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  if (mDistribDrawFromDistribution != NULL)
  {
    mDistribDrawFromDistribution->connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribFunctionDefinitionPlugin::enablePackageInternal(
                                                       const std::string&
                                                         pkgURI,
                                                       const std::string&
                                                         pkgPrefix,
                                                       bool flag)
{
  if (isSetDistribDrawFromDistribution())
  {
    mDistribDrawFromDistribution->enablePackageInternal(pkgURI, pkgPrefix,
      flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribFunctionDefinitionPlugin::updateSBMLNamespace(
                                                     const std::string&
                                                       package,
                                                     unsigned int level,
                                                     unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (mDistribDrawFromDistribution != NULL)
  {
    mDistribDrawFromDistribution->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::getAttribute(const std::string& attributeName,
                                              bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::getAttribute(const std::string& attributeName,
                                              int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::getAttribute(const std::string& attributeName,
                                              double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::getAttribute(const std::string& attributeName,
                                              unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::getAttribute(const std::string& attributeName,
                                              std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribFunctionDefinitionPlugin's
 * attribute "attributeName" is set.
 */
bool
DistribFunctionDefinitionPlugin::isSetAttribute(const std::string&
  attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setAttribute(const std::string& attributeName,
                                              bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setAttribute(const std::string& attributeName,
                                              int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setAttribute(const std::string& attributeName,
                                              double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setAttribute(const std::string& attributeName,
                                              unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::setAttribute(const std::string& attributeName,
                                              const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::unsetAttribute(const std::string&
  attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribFunctionDefinitionPlugin.
 */
SBase*
DistribFunctionDefinitionPlugin::createChildObject(const std::string&
  elementName)
{
  SBase* obj = NULL;

  if (elementName == "distribDrawFromDistribution")
  {
    return createDistribDrawFromDistribution();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribFunctionDefinitionPlugin.
 */
int
DistribFunctionDefinitionPlugin::addChildObject(const std::string& elementName,
                                                const SBase* element)
{
  if (elementName == "distribDrawFromDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_DRAWFROMDISTRIBUTION)
  {
    return setDistribDrawFromDistribution((const
      DistribDrawFromDistribution*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribFunctionDefinitionPlugin.
 */
SBase*
DistribFunctionDefinitionPlugin::removeChildObject(
                                                   const std::string&
                                                     elementName,
                                                   const std::string& id)
{
  if (elementName == "distribDrawFromDistribution")
  {
    DistribDrawFromDistribution * obj = getDistribDrawFromDistribution();
    if (unsetDistribDrawFromDistribution() == LIBSBML_OPERATION_SUCCESS) return
      obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribFunctionDefinitionPlugin.
 */
unsigned int
DistribFunctionDefinitionPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "distribDrawFromDistribution")
  {
    if (isSetDistribDrawFromDistribution())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this
 * DistribFunctionDefinitionPlugin.
 */
SBase*
DistribFunctionDefinitionPlugin::getObject(const std::string& elementName,
                                           unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "distribDrawFromDistribution")
  {
    return getDistribDrawFromDistribution();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribFunctionDefinitionPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribDrawFromDistribution != NULL)
  {
    if (mDistribDrawFromDistribution->getId() == id)
    {
      return mDistribDrawFromDistribution;
    }

    obj = mDistribDrawFromDistribution->getElementBySId(id);
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
DistribFunctionDefinitionPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribDrawFromDistribution != NULL)
  {
    if (mDistribDrawFromDistribution->getMetaId() == metaid)
    {
      return mDistribDrawFromDistribution;
    }

    obj = mDistribDrawFromDistribution->getElementByMetaId(metaid);
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
DistribFunctionDefinitionPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mDistribDrawFromDistribution, filter);


  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
DistribFunctionDefinitionPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const DistribFunctionDefinitionPlugin* plug = static_cast<const
    DistribFunctionDefinitionPlugin*>(model->getPlugin(getPrefix()));

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
DistribFunctionDefinitionPlugin::createObject(XMLInputStream& stream)
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
    if (name == "drawFromDistribution")
    {
      if (isSetDistribDrawFromDistribution())
      {
        getErrorLog()->logPackageError("distrib",
          DistribFunctionDefinitionAllowedElements, getPackageVersion(),
            getLevel(), getVersion());
      }

      delete mDistribDrawFromDistribution;
      mDistribDrawFromDistribution = new
        DistribDrawFromDistribution(distribns);
      obj = mDistribDrawFromDistribution;
    }
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 */
LIBSBML_EXTERN
const DistribDrawFromDistribution_t*
DistribFunctionDefinitionPlugin_getDistribDrawFromDistribution(const
  DistribFunctionDefinitionPlugin_t * dfdp)
{
  if (dfdp == NULL)
  {
    return NULL;
  }

  return
    (DistribDrawFromDistribution_t*)(dfdp->getDistribDrawFromDistribution());
}


/*
 * Predicate returning @c 1 (true) if this DistribFunctionDefinitionPlugin_t's
 * "distribDrawFromDistribution" element is set.
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_isSetDistribDrawFromDistribution(const
  DistribFunctionDefinitionPlugin_t * dfdp)
{
  return (dfdp != NULL) ?
    static_cast<int>(dfdp->isSetDistribDrawFromDistribution()) : 0;
}


/*
 * Sets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_setDistribDrawFromDistribution(
                                                               DistribFunctionDefinitionPlugin_t
                                                                 * dfdp,
                                                               const
                                                                 DistribDrawFromDistribution_t*
                                                                   distribDrawFromDistribution)
{
  return (dfdp != NULL) ?
    dfdp->setDistribDrawFromDistribution(distribDrawFromDistribution) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribDrawFromDistribution_t object, adds it to this
 * DistribFunctionDefinitionPlugin_t object and returns the
 * DistribDrawFromDistribution_t object created.
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t*
DistribFunctionDefinitionPlugin_createDistribDrawFromDistribution(DistribFunctionDefinitionPlugin_t*
  dfdp)
{
  if (dfdp == NULL)
  {
    return NULL;
  }

  return
    (DistribDrawFromDistribution_t*)(dfdp->createDistribDrawFromDistribution());
}


/*
 * Unsets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_unsetDistribDrawFromDistribution(DistribFunctionDefinitionPlugin_t
  * dfdp)
{
  return (dfdp != NULL) ? dfdp->unsetDistribDrawFromDistribution() :
    LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


