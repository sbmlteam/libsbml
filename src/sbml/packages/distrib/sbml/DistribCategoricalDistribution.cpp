/**
 * @file DistribCategoricalDistribution.cpp
 * @brief Implementation of the DistribCategoricalDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribCategoricalDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribCategoricalDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribCategoricalDistribution::DistribCategoricalDistribution(
                                                               unsigned int
                                                                 level,
                                                               unsigned int
                                                                 version,
                                                               unsigned int
                                                                 pkgVersion)
  : DistribCategoricalUnivariateDistribution(level, version)
  , mDistribCategories (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribCategoricalDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribCategoricalDistribution::DistribCategoricalDistribution(DistribPkgNamespaces
  *distribns)
  : DistribCategoricalUnivariateDistribution(distribns)
  , mDistribCategories (distribns)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribCategoricalDistribution.
 */
DistribCategoricalDistribution::DistribCategoricalDistribution(const
  DistribCategoricalDistribution& orig)
  : DistribCategoricalUnivariateDistribution( orig )
  , mDistribCategories ( orig.mDistribCategories )
{
  connectToChild();
}


/*
 * Assignment operator for DistribCategoricalDistribution.
 */
DistribCategoricalDistribution&
DistribCategoricalDistribution::operator=(const DistribCategoricalDistribution&
  rhs)
{
  if (&rhs != this)
  {
    DistribCategoricalUnivariateDistribution::operator=(rhs);
    mDistribCategories = rhs.mDistribCategories;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribCategoricalDistribution
 * object.
 */
DistribCategoricalDistribution*
DistribCategoricalDistribution::clone() const
{
  return new DistribCategoricalDistribution(*this);
}


/*
 * Destructor for DistribCategoricalDistribution.
 */
DistribCategoricalDistribution::~DistribCategoricalDistribution()
{
}


/*
 * Returns the value of the "id" attribute of this
 * DistribCategoricalDistribution.
 */
const std::string&
DistribCategoricalDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribCategoricalDistribution.
 */
const std::string&
DistribCategoricalDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribCategoricalDistribution's "id"
 * attribute is set.
 */
bool
DistribCategoricalDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribCategoricalDistribution's "name"
 * attribute is set.
 */
bool
DistribCategoricalDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfCategories from this DistribCategoricalDistribution.
 */
const ListOfCategories*
DistribCategoricalDistribution::getListOfDistribCategories() const
{
  return &mDistribCategories;
}


/*
 * Returns the ListOfCategories from this DistribCategoricalDistribution.
 */
ListOfCategories*
DistribCategoricalDistribution::getListOfDistribCategories()
{
  return &mDistribCategories;
}


/*
 * Get a DistribCategory from the DistribCategoricalDistribution.
 */
DistribCategory*
DistribCategoricalDistribution::getDistribCategory(unsigned int n)
{
  return mDistribCategories.get(n);
}


/*
 * Get a DistribCategory from the DistribCategoricalDistribution.
 */
const DistribCategory*
DistribCategoricalDistribution::getDistribCategory(unsigned int n) const
{
  return mDistribCategories.get(n);
}


/*
 * Get a DistribCategory from the DistribCategoricalDistribution based on its
 * identifier.
 */
DistribCategory*
DistribCategoricalDistribution::getDistribCategory(const std::string& sid)
{
  return mDistribCategories.get(sid);
}


/*
 * Get a DistribCategory from the DistribCategoricalDistribution based on its
 * identifier.
 */
const DistribCategory*
DistribCategoricalDistribution::getDistribCategory(const std::string& sid)
  const
{
  return mDistribCategories.get(sid);
}


/*
 * Adds a copy of the given DistribCategory to this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::addDistribCategory(const DistribCategory* dc)
{
  if (dc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(dc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (dc->isSetId() && (mDistribCategories.get(dc->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mDistribCategories.append(dc);
  }
}


/*
 * Get the number of DistribCategory objects in this
 * DistribCategoricalDistribution.
 */
unsigned int
DistribCategoricalDistribution::getNumDistribCategories() const
{
  return mDistribCategories.size();
}


/*
 * Creates a new DistribCategory object, adds it to this
 * DistribCategoricalDistribution object and returns the DistribCategory object
 * created.
 */
DistribCategory*
DistribCategoricalDistribution::createDistribCategory()
{
  DistribCategory* dc = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    dc = new DistribCategory(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (dc != NULL)
  {
    mDistribCategories.appendAndOwn(dc);
  }

  return dc;
}


/*
 * Removes the nth DistribCategory from this DistribCategoricalDistribution and
 * returns a pointer to it.
 */
DistribCategory*
DistribCategoricalDistribution::removeDistribCategory(unsigned int n)
{
  return mDistribCategories.remove(n);
}


/*
 * Removes the DistribCategory from this DistribCategoricalDistribution based
 * on its identifier and returns a pointer to it.
 */
DistribCategory*
DistribCategoricalDistribution::removeDistribCategory(const std::string& sid)
{
  return mDistribCategories.remove(sid);
}


/*
 * Returns the XML element name of this DistribCategoricalDistribution object.
 */
const std::string&
DistribCategoricalDistribution::getElementName() const
{
  static const string name = "categoricalDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribCategoricalDistribution
 * object.
 */
int
DistribCategoricalDistribution::getTypeCode() const
{
  return SBML_DISTRIB_CATEGORICALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribCategoricalDistribution object have been set.
 */
bool
DistribCategoricalDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribCategoricalUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribCategoricalDistribution object have been set.
 */
bool
DistribCategoricalDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribCategoricalUnivariateDistribution::hasRequiredElements();

  if (getNumDistribCategories() == 0)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribCategoricalDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribCategoricalUnivariateDistribution::writeElements(stream);

  if (getNumDistribCategories() > 0)
  {
    mDistribCategories.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribCategoricalDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mDistribCategories.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribCategoricalDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribCategoricalUnivariateDistribution::setSBMLDocument(d);

  mDistribCategories.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribCategoricalDistribution::connectToChild()
{
  DistribCategoricalUnivariateDistribution::connectToChild();

  mDistribCategories.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribCategoricalDistribution::enablePackageInternal(
                                                      const std::string&
                                                        pkgURI,
                                                      const std::string&
                                                        pkgPrefix,
                                                      bool flag)
{
  DistribCategoricalUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  mDistribCategories.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribCategoricalDistribution::updateSBMLNamespace(const std::string& package,
                                                    unsigned int level,
                                                    unsigned int version)
{
  DistribCategoricalUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  mDistribCategories.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::getAttribute(const std::string& attributeName,
                                             bool& value) const
{
  int return_value =
    DistribCategoricalUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::getAttribute(const std::string& attributeName,
                                             int& value) const
{
  int return_value =
    DistribCategoricalUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::getAttribute(const std::string& attributeName,
                                             double& value) const
{
  int return_value =
    DistribCategoricalUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::getAttribute(const std::string& attributeName,
                                             unsigned int& value) const
{
  int return_value =
    DistribCategoricalUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::getAttribute(const std::string& attributeName,
                                             std::string& value) const
{
  int return_value =
    DistribCategoricalUnivariateDistribution::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribCategoricalDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribCategoricalDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value =
    DistribCategoricalUnivariateDistribution::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setAttribute(const std::string& attributeName,
                                             bool value)
{
  int return_value =
    DistribCategoricalUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setAttribute(const std::string& attributeName,
                                             int value)
{
  int return_value =
    DistribCategoricalUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setAttribute(const std::string& attributeName,
                                             double value)
{
  int return_value =
    DistribCategoricalUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setAttribute(const std::string& attributeName,
                                             unsigned int value)
{
  int return_value =
    DistribCategoricalUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::setAttribute(const std::string& attributeName,
                                             const std::string& value)
{
  int return_value =
    DistribCategoricalUnivariateDistribution::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value =
    DistribCategoricalUnivariateDistribution::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribCategoricalDistribution.
 */
SBase*
DistribCategoricalDistribution::createChildObject(const std::string&
  elementName)
{
  DistribCategoricalUnivariateDistribution* obj = NULL;

  if (elementName == "distribCategory")
  {
    return createDistribCategory();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribCategoricalDistribution.
 */
int
DistribCategoricalDistribution::addChildObject(const std::string& elementName,
                                               const SBase* element)
{
  if (elementName == "distribCategory" && element->getTypeCode() ==
    SBML_DISTRIB_CATEGORY)
  {
    return addDistribCategory((const DistribCategory*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribCategoricalDistribution.
 */
SBase*
DistribCategoricalDistribution::removeChildObject(
                                                  const std::string&
                                                    elementName,
                                                  const std::string& id)
{
  if (elementName == "distribCategory")
  {
    return removeDistribCategory(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribCategoricalDistribution.
 */
unsigned int
DistribCategoricalDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "distribCategory")
  {
    return getNumDistribCategories();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this
 * DistribCategoricalDistribution.
 */
SBase*
DistribCategoricalDistribution::getObject(const std::string& elementName,
                                          unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "distribCategory")
  {
    return getDistribCategory(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribCategoricalDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mDistribCategories.getElementBySId(id);

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
DistribCategoricalDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribCategories.getMetaId() == metaid)
  {
    return &mDistribCategories;
  }

  obj = mDistribCategories.getElementByMetaId(metaid);

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
DistribCategoricalDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mDistribCategories, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribCategoricalDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribCategoricalUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfCategories")
  {
    if (mDistribCategories.size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribCategoricalDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    obj = &mDistribCategories;
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
DistribCategoricalDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribCategoricalUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribCategoricalDistribution::readAttributes(const XMLAttributes& attributes,
                                               const ExpectedAttributes&
                                                 expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribCategoricalUnivariateDistribution::readAttributes(attributes,
    expectedAttributes);

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
          DistribDistribCategoricalDistributionAllowedAttributes, pkgVersion,
            level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribCategoricalDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCategoricalDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribCategoricalUnivariateDistribution::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new DistribCategoricalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t *
DistribCategoricalDistribution_create(unsigned int level,
                                      unsigned int version,
                                      unsigned int pkgVersion)
{
  return new DistribCategoricalDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribCategoricalDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribCategoricalDistribution_clone(const DistribCategoricalDistribution_t*
  dcd)
{
  if (dcd != NULL)
  {
    return static_cast<DistribCategoricalDistribution_t*>(dcd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribCategoricalDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribCategoricalDistribution_free(DistribCategoricalDistribution_t* dcd)
{
  if (dcd != NULL)
  {
    delete dcd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribCategoricalDistribution_getId(const DistribCategoricalDistribution_t *
  dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return dcd->getId().empty() ? NULL : safe_strdup(dcd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribCategoricalDistribution_getName(const DistribCategoricalDistribution_t *
  dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return dcd->getName().empty() ? NULL : safe_strdup(dcd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribCategoricalDistribution_t's
 * "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_isSetId(const DistribCategoricalDistribution_t *
  dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribCategoricalDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_isSetName(const DistribCategoricalDistribution_t
  * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_setId(DistribCategoricalDistribution_t * dcd,
                                     const char * id)
{
  return (dcd != NULL) ? dcd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_setName(DistribCategoricalDistribution_t * dcd,
                                       const char * name)
{
  return (dcd != NULL) ? dcd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_unsetId(DistribCategoricalDistribution_t * dcd)
{
  return (dcd != NULL) ? dcd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_unsetName(DistribCategoricalDistribution_t *
  dcd)
{
  return (dcd != NULL) ? dcd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing DistribCategory_t objects from this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribCategoricalDistribution_getListOfDistribCategories(DistribCategoricalDistribution_t*
  dcd)
{
  return (dcd != NULL) ? dcd->getListOfDistribCategories() : NULL;
}


/*
 * Get a DistribCategory_t from the DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_getDistribCategory(
                                                  DistribCategoricalDistribution_t*
                                                    dcd,
                                                  unsigned int n)
{
  return (dcd != NULL) ? dcd->getDistribCategory(n) : NULL;
}


/*
 * Get a DistribCategory_t from the DistribCategoricalDistribution_t based on
 * its identifier.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_getDistribCategoryById(
                                                      DistribCategoricalDistribution_t*
                                                        dcd,
                                                      const char *sid)
{
  return (dcd != NULL && sid != NULL) ? dcd->getDistribCategory(sid) : NULL;
}


/*
 * Adds a copy of the given DistribCategory_t to this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_addDistribCategory(
                                                  DistribCategoricalDistribution_t*
                                                    dcd,
                                                  const DistribCategory_t* dc)
{
  return (dcd != NULL) ? dcd->addDistribCategory(dc) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DistribCategory_t objects in this
 * DistribCategoricalDistribution_t.
 */
LIBSBML_EXTERN
unsigned int
DistribCategoricalDistribution_getNumDistribCategories(DistribCategoricalDistribution_t*
  dcd)
{
  return (dcd != NULL) ? dcd->getNumDistribCategories() : SBML_INT_MAX;
}


/*
 * Creates a new DistribCategory_t object, adds it to this
 * DistribCategoricalDistribution_t object and returns the DistribCategory_t
 * object created.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_createDistribCategory(DistribCategoricalDistribution_t*
  dcd)
{
  return (dcd != NULL) ? dcd->createDistribCategory() : NULL;
}


/*
 * Removes the nth DistribCategory_t from this DistribCategoricalDistribution_t
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_removeDistribCategory(
                                                     DistribCategoricalDistribution_t*
                                                       dcd,
                                                     unsigned int n)
{
  return (dcd != NULL) ? dcd->removeDistribCategory(n) : NULL;
}


/*
 * Removes the DistribCategory_t from this DistribCategoricalDistribution_t
 * based on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_removeDistribCategoryById(
                                                         DistribCategoricalDistribution_t*
                                                           dcd,
                                                         const char* sid)
{
  return (dcd != NULL && sid != NULL) ? dcd->removeDistribCategory(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCategoricalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_hasRequiredAttributes(const
  DistribCategoricalDistribution_t * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCategoricalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_hasRequiredElements(const
  DistribCategoricalDistribution_t * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


