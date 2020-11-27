/**
 * @file DistribSBasePlugin.cpp
 * @brief Implementation of the DistribSBasePlugin class.
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
  , mUncertainties (distribns)
{
  connectToChild();
}


/*
 * Copy constructor for DistribSBasePlugin.
 */
DistribSBasePlugin::DistribSBasePlugin(const DistribSBasePlugin& orig)
  : SBasePlugin( orig )
  , mUncertainties ( orig.mUncertainties )
{
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
    mUncertainties = rhs.mUncertainties;
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
}


/*
 * Returns the ListOfUncertainties from this DistribSBasePlugin.
 */
const ListOfUncertainties*
DistribSBasePlugin::getListOfUncertainties() const
{
  return &mUncertainties;
}


/*
 * Returns the ListOfUncertainties from this DistribSBasePlugin.
 */
ListOfUncertainties*
DistribSBasePlugin::getListOfUncertainties()
{
  return &mUncertainties;
}


/*
 * Get an Uncertainty from the DistribSBasePlugin.
 */
Uncertainty*
DistribSBasePlugin::getUncertainty(unsigned int n)
{
  return static_cast< Uncertainty*>(mUncertainties.get(n));
}


/*
 * Get an Uncertainty from the DistribSBasePlugin.
 */
const Uncertainty*
DistribSBasePlugin::getUncertainty(unsigned int n) const
{
  return static_cast<const Uncertainty*>(mUncertainties.get(n));
}


/*
 * Adds a copy of the given Uncertainty to this DistribSBasePlugin.
 */
int
DistribSBasePlugin::addUncertainty(const Uncertainty* u)
{
  if (u == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (u->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != u->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != u->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != u->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mUncertainties.append(u);
  }
}


/*
 * Get the number of Uncertainty objects in this DistribSBasePlugin.
 */
unsigned int
DistribSBasePlugin::getNumUncertainties() const
{
  return mUncertainties.size();
}


/*
 * Creates a new Uncertainty object, adds it to this DistribSBasePlugin object
 * and returns the Uncertainty object created.
 */
Uncertainty*
DistribSBasePlugin::createUncertainty()
{
  Uncertainty* u = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    u = new Uncertainty(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (u != NULL)
  {
    mUncertainties.appendAndOwn(u);
  }

  return u;
}


/*
 * Removes the nth Uncertainty from this DistribSBasePlugin and returns a
 * pointer to it.
 */
Uncertainty*
DistribSBasePlugin::removeUncertainty(unsigned int n)
{
  return static_cast<Uncertainty*>(mUncertainties.remove(n));
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribSBasePlugin::writeElements(XMLOutputStream& stream) const
{
  if (getNumUncertainties() > 0)
  {
    mUncertainties.write(stream);
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

  mUncertainties.accept(v);

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

  mUncertainties.setSBMLDocument(d);
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

  mUncertainties.connectToParent(base);
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
  //We can't enable this package on mUncertainties itself, as this will cause an infinite loop.
  if (pkgURI != mURI)
  {
    mUncertainties.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  else
  {
  //So, only enable the package on the children of mUncertainties.
    for (unsigned int u = 0; u < mUncertainties.getNumUncertainties(); u++)
    {
      mUncertainties.get(u)->enablePackageInternal(pkgURI, pkgPrefix, flag);
    }
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

  mUncertainties.updateSBMLNamespace(package, level, version);
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
    return addUncertainty((const Uncertainty*)(element));
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
    for (unsigned int i = 0; i < getNumUncertainties(); i++)
    {
      if (getUncertainty(i)->getId() == id)
      {
        return removeUncertainty(i);
      }
    }
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
    return getNumUncertainties();
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
    return getUncertainty(index);
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

  obj = mUncertainties.getElementBySId(id);

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
DistribSBasePlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertainties.getMetaId() == metaid)
  {
    return &mUncertainties;
  }

  obj = mUncertainties.getElementByMetaId(metaid);

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
DistribSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mUncertainties, filter);

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

  ret = mUncertainties.appendFrom(plug->getListOfUncertainties());

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
DistribSBasePlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if (name == "listOfUncertainties")
    {
      if (mUncertainties.size() != 0)
      {
        getErrorLog()->logPackageError("distrib", DistribSBaseAllowedElements,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(),
            getColumn());
      }

      obj = &mUncertainties;
      if (targetPrefix.empty())
      {
        mUncertainties.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
  }

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing Uncertainty_t objects from this
 * DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribSBasePlugin_getListOfUncertainties(DistribSBasePlugin_t* dsbp)
{
  return (dsbp != NULL) ? dsbp->getListOfUncertainties() : NULL;
}


/*
 * Get an Uncertainty_t from the DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_getUncertainty(DistribSBasePlugin_t* dsbp, unsigned int n)
{
  return (dsbp != NULL) ? dsbp->getUncertainty(n) : NULL;
}


/*
 * Adds a copy of the given Uncertainty_t to this DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_addUncertainty(DistribSBasePlugin_t* dsbp,
                                  const Uncertainty_t* u)
{
  return (dsbp != NULL) ? dsbp->addUncertainty(u) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Uncertainty_t objects in this DistribSBasePlugin_t.
 */
LIBSBML_EXTERN
unsigned int
DistribSBasePlugin_getNumUncertainties(DistribSBasePlugin_t* dsbp)
{
  return (dsbp != NULL) ? dsbp->getNumUncertainties() : SBML_INT_MAX;
}


/*
 * Creates a new Uncertainty_t object, adds it to this DistribSBasePlugin_t
 * object and returns the Uncertainty_t object created.
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_createUncertainty(DistribSBasePlugin_t* dsbp)
{
  return (dsbp != NULL) ? dsbp->createUncertainty() : NULL;
}


/*
 * Removes the nth Uncertainty_t from this DistribSBasePlugin_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_removeUncertainty(DistribSBasePlugin_t* dsbp,
                                     unsigned int n)
{
  return (dsbp != NULL) ? dsbp->removeUncertainty(n) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


