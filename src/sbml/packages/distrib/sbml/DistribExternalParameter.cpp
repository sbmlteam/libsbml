/**
 * @file DistribExternalParameter.cpp
 * @brief Implementation of the DistribExternalParameter class.
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
#include <sbml/packages/distrib/sbml/DistribExternalParameter.h>
#include <sbml/packages/distrib/sbml/ListOfDistribExternalParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribExternalParameter using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribExternalParameter::DistribExternalParameter(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
  : DistribUncertValue(level, version, pkgVersion)
  , mDefinitionURL ("")
  , mDistribExternalParameters (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribExternalParameter using the given DistribPkgNamespaces
 * object.
 */
DistribExternalParameter::DistribExternalParameter(DistribPkgNamespaces
  *distribns)
  : DistribUncertValue(distribns)
  , mDefinitionURL ("")
  , mDistribExternalParameters (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribExternalParameter.
 */
DistribExternalParameter::DistribExternalParameter(const
  DistribExternalParameter& orig)
  : DistribUncertValue( orig )
  , mDefinitionURL ( orig.mDefinitionURL )
  , mDistribExternalParameters ( NULL )
{
  if (orig.mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters = orig.mDistribExternalParameters->clone();
  }
  connectToChild();
}


/*
 * Assignment operator for DistribExternalParameter.
 */
DistribExternalParameter&
DistribExternalParameter::operator=(const DistribExternalParameter& rhs)
{
  if (&rhs != this)
  {
    DistribUncertValue::operator=(rhs);
    mDefinitionURL = rhs.mDefinitionURL;
    if (rhs.mDistribExternalParameters != NULL)
    {
      mDistribExternalParameters = rhs.mDistribExternalParameters->clone();
    }
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribExternalParameter object.
 */
DistribExternalParameter*
DistribExternalParameter::clone() const
{
  return new DistribExternalParameter(*this);
}


/*
 * Destructor for DistribExternalParameter.
 */
DistribExternalParameter::~DistribExternalParameter()
{
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalParameter.
 */
const std::string&
DistribExternalParameter::getDefinitionURL() const
{
  return mDefinitionURL;
}


/*
 * Predicate returning @c true if this DistribExternalParameter's
 * "definitionURL" attribute is set.
 */
bool
DistribExternalParameter::isSetDefinitionURL() const
{
  return (mDefinitionURL.empty() == false);
}


/*
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setDefinitionURL(const std::string& definitionURL)
{
  mDefinitionURL = definitionURL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::unsetDefinitionURL()
{
  mDefinitionURL.erase();

  if (mDefinitionURL.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfDistribExternalParameters from this
 * DistribExternalParameter.
 */
const ListOfDistribExternalParameters*
DistribExternalParameter::getListOfDistribExternalParameters() const
{
  return mDistribExternalParameters;
}


/*
 * Returns the ListOfDistribExternalParameters from this
 * DistribExternalParameter.
 */
ListOfDistribExternalParameters*
DistribExternalParameter::getListOfDistribExternalParameters()
{
  return mDistribExternalParameters;
}


/*
 * Get a DistribExternalParameter from the DistribExternalParameter.
 */
DistribExternalParameter*
DistribExternalParameter::getDistribExternalParameter(unsigned int n)
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->get(n)
    : NULL;
}


/*
 * Get a DistribExternalParameter from the DistribExternalParameter.
 */
const DistribExternalParameter*
DistribExternalParameter::getDistribExternalParameter(unsigned int n) const
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->get(n)
    : NULL;
}


/*
 * Get a DistribExternalParameter from the DistribExternalParameter based on
 * its identifier.
 */
DistribExternalParameter*
DistribExternalParameter::getDistribExternalParameter(const std::string& sid)
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->get(sid)
    : NULL;
}


/*
 * Get a DistribExternalParameter from the DistribExternalParameter based on
 * its identifier.
 */
const DistribExternalParameter*
DistribExternalParameter::getDistribExternalParameter(const std::string& sid)
  const
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->get(sid)
    : NULL;
}


/*
 * Adds a copy of the given DistribExternalParameter to this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::addDistribExternalParameter(const
  DistribExternalParameter* dep)
{
  if (dep == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dep->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dep->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dep->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(dep)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (dep->isSetId() && (mDistribExternalParameters->get(dep->getId())) !=
    NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    if (mDistribExternalParameters == NULL)
    {
      DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
        getPackageVersion());
      mDistribExternalParameters = new ListOfDistribExternalParameters(distribns);
      delete distribns;
    }
    return mDistribExternalParameters->append(dep);
  }
}


/*
 * Get the number of DistribExternalParameter objects in this
 * DistribExternalParameter.
 */
unsigned int
DistribExternalParameter::getNumDistribExternalParameters() const
{
  if (mDistribExternalParameters != NULL)
  {
    return mDistribExternalParameters->size();
  }
  return 0;
}


/*
 * Creates a new DistribExternalParameter object, adds it to this
 * DistribExternalParameter object and returns the DistribExternalParameter
 * object created.
 */
DistribExternalParameter*
DistribExternalParameter::createDistribExternalParameter()
{
  DistribExternalParameter* dep = NULL;
  DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
    getPackageVersion());

  try
  {
    dep = new DistribExternalParameter(distribns);
  }
  catch (...)
  {
  }

  if (dep != NULL)
  {
    if (mDistribExternalParameters == NULL)
    {
      mDistribExternalParameters = new ListOfDistribExternalParameters(distribns);
    }
    mDistribExternalParameters->appendAndOwn(dep);
  }
  
  delete distribns;
  
  return dep;
}


/*
 * Removes the nth DistribExternalParameter from this DistribExternalParameter
 * and returns a pointer to it.
 */
DistribExternalParameter*
DistribExternalParameter::removeDistribExternalParameter(unsigned int n)
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->remove(n)
    : NULL;
}


/*
 * Removes the DistribExternalParameter from this DistribExternalParameter
 * based on its identifier and returns a pointer to it.
 */
DistribExternalParameter*
DistribExternalParameter::removeDistribExternalParameter(const std::string&
  sid)
{
  return (mDistribExternalParameters != NULL) ? mDistribExternalParameters->remove(sid)
    : NULL;
}


/*
 * Returns the XML element name of this DistribExternalParameter object.
 */
const std::string&
DistribExternalParameter::getElementName() const
{
  static const string name = "externalParameter";
  return name;
}


/*
 * Returns the libSBML type code for this DistribExternalParameter object.
 */
int
DistribExternalParameter::getTypeCode() const
{
  return SBML_DISTRIB_EXTERNALPARAMETER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribExternalParameter object have been set.
 */
bool
DistribExternalParameter::hasRequiredAttributes() const
{
  bool allPresent = DistribUncertValue::hasRequiredAttributes();

  if (isSetDefinitionURL() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribExternalParameter object have been set.
 */
bool
DistribExternalParameter::hasRequiredElements() const
{
  bool allPresent = DistribUncertValue::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribExternalParameter::writeElements(XMLOutputStream& stream) const
{
  DistribUncertValue::writeElements(stream);

  if (getNumDistribExternalParameters() > 0)
  {
    mDistribExternalParameters->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribExternalParameter::accept(SBMLVisitor& v) const
{
  v.visit(*this);
  
  if (mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters->accept(v);
  }

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribExternalParameter::setSBMLDocument(SBMLDocument* d)
{
  DistribUncertValue::setSBMLDocument(d);

  if (mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribExternalParameter::connectToChild()
{
  DistribUncertValue::connectToChild();

  if (mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribExternalParameter::enablePackageInternal(const std::string& pkgURI,
                                                const std::string& pkgPrefix,
                                                bool flag)
{
  DistribUncertValue::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribExternalParameter::updateSBMLNamespace(const std::string& package,
                                              unsigned int level,
                                              unsigned int version)
{
  DistribUncertValue::updateSBMLNamespace(package, level, version);

  if (mDistribExternalParameters != NULL)
  {
    mDistribExternalParameters->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::getAttribute(const std::string& attributeName,
                                       bool& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::getAttribute(const std::string& attributeName,
                                       int& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::getAttribute(const std::string& attributeName,
                                       double& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::getAttribute(const std::string& attributeName,
                                       unsigned int& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "definitionURL")
  {
    value = getDefinitionURL();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribExternalParameter's attribute
 * "attributeName" is set.
 */
bool
DistribExternalParameter::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = DistribUncertValue::isSetAttribute(attributeName);

  if (attributeName == "definitionURL")
  {
    value = isSetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setAttribute(const std::string& attributeName,
                                       bool value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setAttribute(const std::string& attributeName,
                                       int value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setAttribute(const std::string& attributeName,
                                       double value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setAttribute(const std::string& attributeName,
                                       unsigned int value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::setAttribute(const std::string& attributeName,
                                       const std::string& value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  if (attributeName == "definitionURL")
  {
    return_value = setDefinitionURL(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribExternalParameter.
 */
int
DistribExternalParameter::unsetAttribute(const std::string& attributeName)
{
  int value = DistribUncertValue::unsetAttribute(attributeName);

  if (attributeName == "definitionURL")
  {
    value = unsetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribExternalParameter.
 */
SBase*
DistribExternalParameter::createChildObject(const std::string& elementName)
{
  DistribUncertValue* obj = NULL;

  if (elementName == "externalParameter")
  {
    return createDistribExternalParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribExternalParameter.
 */
int
DistribExternalParameter::addChildObject(const std::string& elementName,
                                         const SBase* element)
{
  if (elementName == "externalParameter" && element->getTypeCode() ==
    SBML_DISTRIB_EXTERNALPARAMETER)
  {
    return addDistribExternalParameter((const
      DistribExternalParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribExternalParameter.
 */
SBase*
DistribExternalParameter::removeChildObject(const std::string& elementName,
                                            const std::string& id)
{
  if (elementName == "externalParameter")
  {
    return removeDistribExternalParameter(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribExternalParameter.
 */
unsigned int
DistribExternalParameter::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "externalParameter")
  {
    return getNumDistribExternalParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribExternalParameter.
 */
SBase*
DistribExternalParameter::getObject(const std::string& elementName,
                                    unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "externalParameter")
  {
    return getDistribExternalParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribExternalParameter::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribExternalParameters != NULL)
  {
  if (mDistribExternalParameters->getId() == id)
  {
    return mDistribExternalParameters;
  }
    obj = mDistribExternalParameters->getElementBySId(id);
  }

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
DistribExternalParameter::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribExternalParameters != NULL)
  {
    if (mDistribExternalParameters->getMetaId() == metaid)
    {
      return mDistribExternalParameters;
    }

    obj = mDistribExternalParameters->getElementByMetaId(metaid);
  }

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
DistribExternalParameter::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_POINTER(ret, sublist, mDistribExternalParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribExternalParameter::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribUncertValue::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfExternalParameters")
  {
    if (mDistribExternalParameters != NULL &&
      mDistribExternalParameters->size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribExternalParameterAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    if (mDistribExternalParameters == NULL)
    {
      DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
        getPackageVersion());
      mDistribExternalParameters = new ListOfDistribExternalParameters(distribns);
      delete distribns;
    }
    obj = mDistribExternalParameters;
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
DistribExternalParameter::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribUncertValue::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("definitionURL");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("definitionURL");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribExternalParameter::readAttributes(const XMLAttributes& attributes,
                                         const ExpectedAttributes&
                                           expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDistribExternalParameters*>(getParentSBMLObject())->size()
      < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribExternalDistributionLODistribExternalParametersAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribExternalDistributionLODistribExternalParametersAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  DistribUncertValue::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribExternalParameterAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribExternalParameterAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribExternalParameter::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // definitionURL string (use = "required" )
  // 

  assigned = attributes.readInto("definitionURL", mDefinitionURL);

  if (assigned == true)
  {
    if (mDefinitionURL.empty() == true)
    {
      logEmptyString(mDefinitionURL, level, version,
        "<DistribExternalParameter>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'definitionURL' is missing from "
      "the <DistribExternalParameter> element.";
    log->logPackageError("distrib",
      DistribDistribExternalParameterAllowedAttributes, pkgVersion, level,
        version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribExternalParameter::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // definitionURL string (use = "required" )
  // 

  assigned = attributes.readInto("definitionURL", mDefinitionURL);

  if (assigned == true)
  {
    if (mDefinitionURL.empty() == true)
    {
      logEmptyString(mDefinitionURL, level, version,
        "<DistribExternalParameter>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'definitionURL' is missing from "
      "the <DistribExternalParameter> element.";
    log->logPackageError("distrib",
      DistribDistribExternalParameterAllowedAttributes, pkgVersion, level,
        version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribExternalParameter::writeAttributes(XMLOutputStream& stream) const
{
  DistribUncertValue::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribExternalParameter::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetDefinitionURL() == true)
  {
    stream.writeAttribute("definitionURL", getPrefix(), mDefinitionURL);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribExternalParameter::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
  if (isSetDefinitionURL() == true)
  {
    stream.writeAttribute("definitionURL", getPrefix(), mDefinitionURL);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribExternalParameter_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribExternalParameter_t *
DistribExternalParameter_create(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion)
{
  return new DistribExternalParameter(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribExternalParameter_t object.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_clone(const DistribExternalParameter_t* dep)
{
  if (dep != NULL)
  {
    return static_cast<DistribExternalParameter_t*>(dep->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribExternalParameter_t object.
 */
LIBSBML_EXTERN
void
DistribExternalParameter_free(DistribExternalParameter_t* dep)
{
  if (dep != NULL)
  {
    delete dep;
  }
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
char *
DistribExternalParameter_getDefinitionURL(const DistribExternalParameter_t *
  dep)
{
  if (dep == NULL)
  {
    return NULL;
  }

  return dep->getDefinitionURL().empty() ? NULL :
    safe_strdup(dep->getDefinitionURL().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribExternalParameter_t's
 * "definitionURL" attribute is set.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_isSetDefinitionURL(const DistribExternalParameter_t *
  dep)
{
  return (dep != NULL) ? static_cast<int>(dep->isSetDefinitionURL()) : 0;
}


/*
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_setDefinitionURL(DistribExternalParameter_t * dep,
                                          const char * definitionURL)
{
  return (dep != NULL) ? dep->setDefinitionURL(definitionURL) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_unsetDefinitionURL(DistribExternalParameter_t * dep)
{
  return (dep != NULL) ? dep->unsetDefinitionURL() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribExternalParameter_getListOfDistribExternalParameters(DistribExternalParameter_t*
  dep)
{
  return (dep != NULL) ? dep->getListOfDistribExternalParameters() : NULL;
}


/*
 * Get a DistribExternalParameter_t from the DistribExternalParameter_t.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_getDistribExternalParameter(
                                                     DistribExternalParameter_t*
                                                       dep,
                                                     unsigned int n)
{
  return (dep != NULL) ? dep->getDistribExternalParameter(n) : NULL;
}


/*
 * Get a DistribExternalParameter_t from the DistribExternalParameter_t based
 * on its identifier.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_getDistribExternalParameterById(
                                                         DistribExternalParameter_t*
                                                           dep,
                                                         const char *sid)
{
  return (dep != NULL && sid != NULL) ? dep->getDistribExternalParameter(sid) :
    NULL;
}


/*
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_addDistribExternalParameter(
                                                     DistribExternalParameter_t*
                                                       dep,
                                                     const
                                                       DistribExternalParameter_t*
                                                         newdep)
{
  return (dep != NULL) ? dep->addDistribExternalParameter(newdep) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DistribExternalParameter_t objects in this
 * DistribExternalParameter_t.
 */
LIBSBML_EXTERN
unsigned int
DistribExternalParameter_getNumDistribExternalParameters(DistribExternalParameter_t*
  dep)
{
  return (dep != NULL) ? dep->getNumDistribExternalParameters() : SBML_INT_MAX;
}


/*
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribExternalParameter_t object and returns the DistribExternalParameter_t
 * object created.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_createDistribExternalParameter(DistribExternalParameter_t*
  dep)
{
  return (dep != NULL) ? dep->createDistribExternalParameter() : NULL;
}


/*
 * Removes the nth DistribExternalParameter_t from this
 * DistribExternalParameter_t and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_removeDistribExternalParameter(
                                                        DistribExternalParameter_t*
                                                          dep,
                                                        unsigned int n)
{
  return (dep != NULL) ? dep->removeDistribExternalParameter(n) : NULL;
}


/*
 * Removes the DistribExternalParameter_t from this DistribExternalParameter_t
 * based on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_removeDistribExternalParameterById(
                                                            DistribExternalParameter_t*
                                                              dep,
                                                            const char* sid)
{
  return (dep != NULL && sid != NULL) ?
    dep->removeDistribExternalParameter(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribExternalParameter_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_hasRequiredAttributes(const DistribExternalParameter_t
  * dep)
{
  return (dep != NULL) ? static_cast<int>(dep->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribExternalParameter_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExternalParameter_hasRequiredElements(const DistribExternalParameter_t *
  dep)
{
  return (dep != NULL) ? static_cast<int>(dep->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


