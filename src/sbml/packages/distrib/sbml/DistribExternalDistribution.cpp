/**
 * @file DistribExternalDistribution.cpp
 * @brief Implementation of the DistribExternalDistribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/distrib/sbml/DistribExternalDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribExternalDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribExternalDistribution::DistribExternalDistribution(unsigned int level,
                                                         unsigned int version,
                                                         unsigned int
                                                           pkgVersion)
  : DistribDistribution(level, version, pkgVersion)
  , mDefinitionURL ("")
  , mDistribExternalParameters (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribExternalDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribExternalDistribution::DistribExternalDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDistribution(distribns)
  , mDefinitionURL ("")
  , mDistribExternalParameters (distribns)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribExternalDistribution.
 */
DistribExternalDistribution::DistribExternalDistribution(const
  DistribExternalDistribution& orig)
  : DistribDistribution( orig )
  , mDefinitionURL ( orig.mDefinitionURL )
  , mDistribExternalParameters ( orig.mDistribExternalParameters )
{
  connectToChild();
}


/*
 * Assignment operator for DistribExternalDistribution.
 */
DistribExternalDistribution&
DistribExternalDistribution::operator=(const DistribExternalDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDistribution::operator=(rhs);
    mDefinitionURL = rhs.mDefinitionURL;
    mDistribExternalParameters = rhs.mDistribExternalParameters;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribExternalDistribution object.
 */
DistribExternalDistribution*
DistribExternalDistribution::clone() const
{
  return new DistribExternalDistribution(*this);
}


/*
 * Destructor for DistribExternalDistribution.
 */
DistribExternalDistribution::~DistribExternalDistribution()
{
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalDistribution.
 */
const std::string&
DistribExternalDistribution::getDefinitionURL() const
{
  return mDefinitionURL;
}


/*
 * Predicate returning @c true if this DistribExternalDistribution's
 * "definitionURL" attribute is set.
 */
bool
DistribExternalDistribution::isSetDefinitionURL() const
{
  return (mDefinitionURL.empty() == false);
}


/*
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setDefinitionURL(const std::string& definitionURL)
{
  mDefinitionURL = definitionURL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::unsetDefinitionURL()
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
 * DistribExternalDistribution.
 */
const ListOfDistribExternalParameters*
DistribExternalDistribution::getListOfDistribExternalParameters() const
{
  return &mDistribExternalParameters;
}


/*
 * Returns the ListOfDistribExternalParameters from this
 * DistribExternalDistribution.
 */
ListOfDistribExternalParameters*
DistribExternalDistribution::getListOfDistribExternalParameters()
{
  return &mDistribExternalParameters;
}


/*
 * Get a DistribExternalParameter from the DistribExternalDistribution.
 */
DistribExternalParameter*
DistribExternalDistribution::getDistribExternalParameter(unsigned int n)
{
  return mDistribExternalParameters.get(n);
}


/*
 * Get a DistribExternalParameter from the DistribExternalDistribution.
 */
const DistribExternalParameter*
DistribExternalDistribution::getDistribExternalParameter(unsigned int n) const
{
  return mDistribExternalParameters.get(n);
}


/*
 * Adds a copy of the given DistribExternalParameter to this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::addDistribExternalParameter(const
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
  else
  {
    return mDistribExternalParameters.append(dep);
  }
}


/*
 * Get the number of DistribExternalParameter objects in this
 * DistribExternalDistribution.
 */
unsigned int
DistribExternalDistribution::getNumDistribExternalParameters() const
{
  return mDistribExternalParameters.size();
}


/*
 * Creates a new DistribExternalParameter object, adds it to this
 * DistribExternalDistribution object and returns the DistribExternalParameter
 * object created.
 */
DistribExternalParameter*
DistribExternalDistribution::createDistribExternalParameter()
{
  DistribExternalParameter* dep = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    dep = new DistribExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (dep != NULL)
  {
    mDistribExternalParameters.appendAndOwn(dep);
  }

  return dep;
}


/*
 * Removes the nth DistribExternalParameter from this
 * DistribExternalDistribution and returns a pointer to it.
 */
DistribExternalParameter*
DistribExternalDistribution::removeDistribExternalParameter(unsigned int n)
{
  return mDistribExternalParameters.remove(n);
}


/*
 * Returns the XML element name of this DistribExternalDistribution object.
 */
const std::string&
DistribExternalDistribution::getElementName() const
{
  static const string name = "externalDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribExternalDistribution object.
 */
int
DistribExternalDistribution::getTypeCode() const
{
  return SBML_DISTRIB_EXTERNALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribExternalDistribution object have been set.
 */
bool
DistribExternalDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribDistribution::hasRequiredAttributes();

  if (isSetDefinitionURL() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribExternalDistribution object have been set.
 */
bool
DistribExternalDistribution::hasRequiredElements() const
{
  bool allPresent = DistribDistribution::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribExternalDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribDistribution::writeElements(stream);

  if (getNumDistribExternalParameters() > 0)
  {
    mDistribExternalParameters.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribExternalDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mDistribExternalParameters.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribExternalDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDistribution::setSBMLDocument(d);

  mDistribExternalParameters.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribExternalDistribution::connectToChild()
{
  DistribDistribution::connectToChild();

  mDistribExternalParameters.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribExternalDistribution::enablePackageInternal(const std::string& pkgURI,
                                                   const std::string&
                                                     pkgPrefix,
                                                   bool flag)
{
  DistribDistribution::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mDistribExternalParameters.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribExternalDistribution::updateSBMLNamespace(const std::string& package,
                                                 unsigned int level,
                                                 unsigned int version)
{
  DistribDistribution::updateSBMLNamespace(package, level, version);

  mDistribExternalParameters.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::getAttribute(const std::string& attributeName,
                                          bool& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::getAttribute(const std::string& attributeName,
                                          int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::getAttribute(const std::string& attributeName,
                                          double& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::getAttribute(const std::string& attributeName,
                                          unsigned int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::getAttribute(const std::string& attributeName,
                                          std::string& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this DistribExternalDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribExternalDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = DistribDistribution::isSetAttribute(attributeName);

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
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setAttribute(const std::string& attributeName,
                                          bool value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setAttribute(const std::string& attributeName,
                                          int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setAttribute(const std::string& attributeName,
                                          double value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setAttribute(const std::string& attributeName,
                                          unsigned int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::setAttribute(const std::string& attributeName,
                                          const std::string& value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

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
 * DistribExternalDistribution.
 */
int
DistribExternalDistribution::unsetAttribute(const std::string& attributeName)
{
  int value = DistribDistribution::unsetAttribute(attributeName);

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
 * DistribExternalDistribution.
 */
SBase*
DistribExternalDistribution::createChildObject(const std::string& elementName)
{
  DistribDistribution* obj = NULL;

  if (elementName == "externalParameter")
  {
    return createDistribExternalParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribExternalDistribution.
 */
int
DistribExternalDistribution::addChildObject(const std::string& elementName,
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
 * DistribExternalDistribution.
 */
SBase*
DistribExternalDistribution::removeChildObject(const std::string& elementName,
                                               const std::string& id)
{
  if (elementName == "externalParameter")
  {
    for (unsigned int i = 0; i < getNumDistribExternalParameters(); i++)
    {
      if (getDistribExternalParameter(i)->getId() == id)
      {
        return removeDistribExternalParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribExternalDistribution.
 */
unsigned int
DistribExternalDistribution::getNumObjects(const std::string& elementName)
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
 * Returns the nth object of "objectName" in this DistribExternalDistribution.
 */
SBase*
DistribExternalDistribution::getObject(const std::string& elementName,
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
DistribExternalDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribExternalParameters.getId() == id)
  {
    return &mDistribExternalParameters;
  }

  obj = mDistribExternalParameters.getElementBySId(id);

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
DistribExternalDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribExternalParameters.getMetaId() == metaid)
  {
    return &mDistribExternalParameters;
  }

  obj = mDistribExternalParameters.getElementByMetaId(metaid);

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
DistribExternalDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mDistribExternalParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribExternalDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfExternalParameters")
  {
    if (mDistribExternalParameters.size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribExternalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    obj = &mDistribExternalParameters;
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
DistribExternalDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDistribution::addExpectedAttributes(attributes);

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
DistribExternalDistribution::readAttributes(const XMLAttributes& attributes,
                                            const ExpectedAttributes&
                                              expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribDistribution::readAttributes(attributes, expectedAttributes);

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
          DistribDistribExternalDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribExternalDistributionAllowedCoreAttributes, pkgVersion,
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
DistribExternalDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
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
        "<DistribExternalDistribution>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'definitionURL' is missing from "
      "the <DistribExternalDistribution> element.";
    log->logPackageError("distrib",
      DistribDistribExternalDistributionAllowedAttributes, pkgVersion, level,
        version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribExternalDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
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
        "<DistribExternalDistribution>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'definitionURL' is missing from "
      "the <DistribExternalDistribution> element.";
    log->logPackageError("distrib",
      DistribDistribExternalDistributionAllowedAttributes, pkgVersion, level,
        version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribExternalDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribDistribution::writeAttributes(stream);

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
DistribExternalDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
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
DistribExternalDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetDefinitionURL() == true)
  {
    stream.writeAttribute("definitionURL", getPrefix(), mDefinitionURL);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribExternalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribExternalDistribution_t *
DistribExternalDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
{
  return new DistribExternalDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribExternalDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribExternalDistribution_clone(const DistribExternalDistribution_t* ded)
{
  if (ded != NULL)
  {
    return static_cast<DistribExternalDistribution_t*>(ded->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribExternalDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribExternalDistribution_free(DistribExternalDistribution_t* ded)
{
  if (ded != NULL)
  {
    delete ded;
  }
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribExternalDistribution_getDefinitionURL(const
  DistribExternalDistribution_t * ded)
{
  if (ded == NULL)
  {
    return NULL;
  }

  return ded->getDefinitionURL().empty() ? NULL :
    safe_strdup(ded->getDefinitionURL().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribExternalDistribution_t's
 * "definitionURL" attribute is set.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_isSetDefinitionURL(const
  DistribExternalDistribution_t * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->isSetDefinitionURL()) : 0;
}


/*
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_setDefinitionURL(
                                             DistribExternalDistribution_t *
                                               ded,
                                             const char * definitionURL)
{
  return (ded != NULL) ? ded->setDefinitionURL(definitionURL) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_unsetDefinitionURL(DistribExternalDistribution_t *
  ded)
{
  return (ded != NULL) ? ded->unsetDefinitionURL() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribExternalDistribution_getListOfDistribExternalParameters(DistribExternalDistribution_t*
  ded)
{
  return (ded != NULL) ? ded->getListOfDistribExternalParameters() : NULL;
}


/*
 * Get a DistribExternalParameter_t from the DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_getDistribExternalParameter(
                                                        DistribExternalDistribution_t*
                                                          ded,
                                                        unsigned int n)
{
  return (ded != NULL) ? ded->getDistribExternalParameter(n) : NULL;
}


/*
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_addDistribExternalParameter(
                                                        DistribExternalDistribution_t*
                                                          ded,
                                                        const
                                                          DistribExternalParameter_t*
                                                            dep)
{
  return (ded != NULL) ? ded->addDistribExternalParameter(dep) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DistribExternalParameter_t objects in this
 * DistribExternalDistribution_t.
 */
LIBSBML_EXTERN
unsigned int
DistribExternalDistribution_getNumDistribExternalParameters(DistribExternalDistribution_t*
  ded)
{
  return (ded != NULL) ? ded->getNumDistribExternalParameters() : SBML_INT_MAX;
}


/*
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribExternalDistribution_t object and returns the
 * DistribExternalParameter_t object created.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_createDistribExternalParameter(DistribExternalDistribution_t*
  ded)
{
  return (ded != NULL) ? ded->createDistribExternalParameter() : NULL;
}


/*
 * Removes the nth DistribExternalParameter_t from this
 * DistribExternalDistribution_t and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_removeDistribExternalParameter(
                                                           DistribExternalDistribution_t*
                                                             ded,
                                                           unsigned int n)
{
  return (ded != NULL) ? ded->removeDistribExternalParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribExternalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_hasRequiredAttributes(const
  DistribExternalDistribution_t * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribExternalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_hasRequiredElements(const
  DistribExternalDistribution_t * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


