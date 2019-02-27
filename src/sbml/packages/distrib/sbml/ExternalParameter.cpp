/**
 * @file ExternalParameter.cpp
 * @brief Implementation of the ExternalParameter class.
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
#include <sbml/packages/distrib/sbml/ExternalParameter.h>
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ExternalParameter using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
ExternalParameter::ExternalParameter(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : UncertValue(level, version, pkgVersion)
  , mDefinitionURL ("")
  , mExternalParameters (new ListOfExternalParameters (level, version,
    pkgVersion))
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new ExternalParameter using the given DistribPkgNamespaces object.
 */
ExternalParameter::ExternalParameter(DistribPkgNamespaces *distribns)
  : UncertValue(distribns)
  , mDefinitionURL ("")
  , mExternalParameters (new ListOfExternalParameters (distribns))
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for ExternalParameter.
 */
ExternalParameter::ExternalParameter(const ExternalParameter& orig)
  : UncertValue( orig )
  , mDefinitionURL ( orig.mDefinitionURL )
  , mExternalParameters ( NULL )
{
  if (orig.mExternalParameters != NULL)
  {
    mExternalParameters = orig.mExternalParameters->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for ExternalParameter.
 */
ExternalParameter&
ExternalParameter::operator=(const ExternalParameter& rhs)
{
  if (&rhs != this)
  {
    UncertValue::operator=(rhs);
    mDefinitionURL = rhs.mDefinitionURL;
    delete mExternalParameters;
    if (rhs.mExternalParameters != NULL)
    {
      mExternalParameters = rhs.mExternalParameters->clone();
    }
    else
    {
      mExternalParameters = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ExternalParameter object.
 */
ExternalParameter*
ExternalParameter::clone() const
{
  return new ExternalParameter(*this);
}


/*
 * Destructor for ExternalParameter.
 */
ExternalParameter::~ExternalParameter()
{
  delete mExternalParameters;
  mExternalParameters = NULL;
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * ExternalParameter.
 */
const std::string&
ExternalParameter::getDefinitionURL() const
{
  return mDefinitionURL;
}


/*
 * Predicate returning @c true if this ExternalParameter's "definitionURL"
 * attribute is set.
 */
bool
ExternalParameter::isSetDefinitionURL() const
{
  return (mDefinitionURL.empty() == false);
}


/*
 * Sets the value of the "definitionURL" attribute of this ExternalParameter.
 */
int
ExternalParameter::setDefinitionURL(const std::string& definitionURL)
{
  mDefinitionURL = definitionURL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "definitionURL" attribute of this ExternalParameter.
 */
int
ExternalParameter::unsetDefinitionURL()
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
 * Returns the ListOfExternalParameters * from this ExternalParameter.
 */
const ListOfExternalParameters *
ExternalParameter::getListOfExternalParameters() const
{
  return mExternalParameters;
}


/*
 * Returns the ListOfExternalParameters * from this ExternalParameter.
 */
ListOfExternalParameters *
ExternalParameter::getListOfExternalParameters()
{
  return mExternalParameters;
}


/*
 * Get an ExternalParameter from the ExternalParameter.
 */
ExternalParameter*
ExternalParameter::getExternalParameter(unsigned int n)
{
  return mExternalParameters->get(n);
}


/*
 * Get an ExternalParameter from the ExternalParameter.
 */
const ExternalParameter*
ExternalParameter::getExternalParameter(unsigned int n) const
{
  return mExternalParameters->get(n);
}


/*
 * Adds a copy of the given ExternalParameter to this ExternalParameter.
 */
int
ExternalParameter::addExternalParameter(const ExternalParameter* ep1)
{
  if (ep1 == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ep1->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ep1->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ep1->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ep1)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mExternalParameters->append(ep1);
  }
}


/*
 * Get the number of ExternalParameter objects in this ExternalParameter.
 */
unsigned int
ExternalParameter::getNumExternalParameters() const
{
  return mExternalParameters->size();
}


/*
 * Creates a new ExternalParameter object, adds it to this ExternalParameter
 * object and returns the ExternalParameter object created.
 */
ExternalParameter*
ExternalParameter::createExternalParameter()
{
  ExternalParameter* ep1 = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    ep1 = new ExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (ep1 != NULL)
  {
    mExternalParameters->appendAndOwn(ep1);
  }

  return ep1;
}


/*
 * Removes the nth ExternalParameter from this ExternalParameter and returns a
 * pointer to it.
 */
ExternalParameter*
ExternalParameter::removeExternalParameter(unsigned int n)
{
  return mExternalParameters->remove(n);
}


/*
 * Returns the XML element name of this ExternalParameter object.
 */
const std::string&
ExternalParameter::getElementName() const
{
  static const string name = "externalParameter";
  return name;
}


/*
 * Returns the libSBML type code for this ExternalParameter object.
 */
int
ExternalParameter::getTypeCode() const
{
  return SBML_DISTRIB_EXTERNALPARAMETER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ExternalParameter object have been set.
 */
bool
ExternalParameter::hasRequiredAttributes() const
{
  bool allPresent = UncertValue::hasRequiredAttributes();

  if (isSetDefinitionURL() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * ExternalParameter object have been set.
 */
bool
ExternalParameter::hasRequiredElements() const
{
  bool allPresent = UncertValue::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
ExternalParameter::writeElements(XMLOutputStream& stream) const
{
  UncertValue::writeElements(stream);

  if (getNumExternalParameters() > 0)
  {
    mExternalParameters->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
ExternalParameter::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mExternalParameters->accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
ExternalParameter::setSBMLDocument(SBMLDocument* d)
{
  UncertValue::setSBMLDocument(d);

  mExternalParameters->setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
ExternalParameter::connectToChild()
{
  UncertValue::connectToChild();

  mExternalParameters->connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
ExternalParameter::enablePackageInternal(const std::string& pkgURI,
                                         const std::string& pkgPrefix,
                                         bool flag)
{
  UncertValue::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mExternalParameters->enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
ExternalParameter::updateSBMLNamespace(const std::string& package,
                                       unsigned int level,
                                       unsigned int version)
{
  UncertValue::updateSBMLNamespace(package, level, version);

  mExternalParameters->updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = UncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = UncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = UncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = UncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = UncertValue::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this ExternalParameter's attribute
 * "attributeName" is set.
 */
bool
ExternalParameter::isSetAttribute(const std::string& attributeName) const
{
  bool value = UncertValue::isSetAttribute(attributeName);

  if (attributeName == "definitionURL")
  {
    value = isSetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = UncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::setAttribute(const std::string& attributeName, int value)
{
  int return_value = UncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = UncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = UncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = UncertValue::setAttribute(attributeName, value);

  if (attributeName == "definitionURL")
  {
    return_value = setDefinitionURL(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this ExternalParameter.
 */
int
ExternalParameter::unsetAttribute(const std::string& attributeName)
{
  int value = UncertValue::unsetAttribute(attributeName);

  if (attributeName == "definitionURL")
  {
    value = unsetDefinitionURL();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this ExternalParameter.
 */
SBase*
ExternalParameter::createChildObject(const std::string& elementName)
{
  UncertValue* obj = NULL;

  if (elementName == "externalParameter")
  {
    return createExternalParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this ExternalParameter.
 */
int
ExternalParameter::addChildObject(const std::string& elementName,
                                  const SBase* element)
{
  if (elementName == "externalParameter" && element->getTypeCode() ==
    SBML_DISTRIB_EXTERNALPARAMETER)
  {
    return addExternalParameter((const ExternalParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * ExternalParameter.
 */
SBase*
ExternalParameter::removeChildObject(const std::string& elementName,
                                     const std::string& id)
{
  if (elementName == "externalParameter")
  {
    for (unsigned int i = 0; i < getNumExternalParameters(); i++)
    {
      if (getExternalParameter(i)->getId() == id)
      {
        return removeExternalParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this ExternalParameter.
 */
unsigned int
ExternalParameter::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "externalParameter")
  {
    return getNumExternalParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this ExternalParameter.
 */
SBase*
ExternalParameter::getObject(const std::string& elementName,
                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "externalParameter")
  {
    return getExternalParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
ExternalParameter::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mExternalParameters->getId() == id)
  {
    return mExternalParameters;
  }

  obj = mExternalParameters->getElementBySId(id);

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
ExternalParameter::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mExternalParameters->getMetaId() == metaid)
  {
    return mExternalParameters;
  }

  obj = mExternalParameters->getElementByMetaId(metaid);

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
ExternalParameter::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_POINTER(ret, sublist, mExternalParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
ExternalParameter::createObject(XMLInputStream& stream)
{
  SBase* obj = UncertValue::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfExternalParameters")
  {
    if (mExternalParameters->size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribExternalParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    obj = mExternalParameters;
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
ExternalParameter::addExpectedAttributes(ExpectedAttributes& attributes)
{
  UncertValue::addExpectedAttributes(attributes);

  attributes.add("definitionURL");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ExternalParameter::readAttributes(const XMLAttributes& attributes,
                                  const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfExternalParameters*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribExternalParameterLOExternalParametersAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribExternalParameterLOExternalParametersAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  UncertValue::readAttributes(attributes, expectedAttributes);

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
          DistribExternalParameterAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribExternalParameterAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // definitionURL string (use = "required" )
  // 

  assigned = attributes.readInto("definitionURL", mDefinitionURL);

  if (assigned == true)
  {
    if (mDefinitionURL.empty() == true)
    {
      logEmptyString(mDefinitionURL, level, version, "<ExternalParameter>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'definitionURL' is missing from "
      "the <ExternalParameter> element.";
    log->logPackageError("distrib", DistribExternalParameterAllowedAttributes,
      pkgVersion, level, version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ExternalParameter::writeAttributes(XMLOutputStream& stream) const
{
  UncertValue::writeAttributes(stream);

  if (isSetDefinitionURL() == true)
  {
    stream.writeAttribute("definitionURL", getPrefix(), mDefinitionURL);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new ExternalParameter_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
ExternalParameter_t *
ExternalParameter_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
{
  return new ExternalParameter(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this ExternalParameter_t object.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_clone(const ExternalParameter_t* ep)
{
  if (ep != NULL)
  {
    return static_cast<ExternalParameter_t*>(ep->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this ExternalParameter_t object.
 */
LIBSBML_EXTERN
void
ExternalParameter_free(ExternalParameter_t* ep)
{
  if (ep != NULL)
  {
    delete ep;
  }
}


/*
 * Returns the value of the "definitionURL" attribute of this
 * ExternalParameter_t.
 */
LIBSBML_EXTERN
char *
ExternalParameter_getDefinitionURL(const ExternalParameter_t * ep)
{
  if (ep == NULL)
  {
    return NULL;
  }

  return ep->getDefinitionURL().empty() ? NULL :
    safe_strdup(ep->getDefinitionURL().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ExternalParameter_t's
 * "definitionURL" attribute is set.
 */
LIBSBML_EXTERN
int
ExternalParameter_isSetDefinitionURL(const ExternalParameter_t * ep)
{
  return (ep != NULL) ? static_cast<int>(ep->isSetDefinitionURL()) : 0;
}


/*
 * Sets the value of the "definitionURL" attribute of this ExternalParameter_t.
 */
LIBSBML_EXTERN
int
ExternalParameter_setDefinitionURL(ExternalParameter_t * ep,
                                   const char * definitionURL)
{
  return (ep != NULL) ? ep->setDefinitionURL(definitionURL) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "definitionURL" attribute of this
 * ExternalParameter_t.
 */
LIBSBML_EXTERN
int
ExternalParameter_unsetDefinitionURL(ExternalParameter_t * ep)
{
  return (ep != NULL) ? ep->unsetDefinitionURL() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing ExternalParameter_t objects from this
 * ExternalParameter_t.
 */
LIBSBML_EXTERN
ListOf_t*
ExternalParameter_getListOfExternalParameters(ExternalParameter_t* ep)
{
  return (ep != NULL) ? ep->getListOfExternalParameters() : NULL;
}


/*
 * Get an ExternalParameter_t from the ExternalParameter_t.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_getExternalParameter(ExternalParameter_t* ep,
                                       unsigned int n)
{
  return (ep != NULL) ? ep->getExternalParameter(n) : NULL;
}


/*
 * Adds a copy of the given ExternalParameter_t to this ExternalParameter_t.
 */
LIBSBML_EXTERN
int
ExternalParameter_addExternalParameter(ExternalParameter_t* ep,
                                       const ExternalParameter_t* ep1)
{
  return (ep != NULL) ? ep->addExternalParameter(ep1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of ExternalParameter_t objects in this ExternalParameter_t.
 */
LIBSBML_EXTERN
unsigned int
ExternalParameter_getNumExternalParameters(ExternalParameter_t* ep)
{
  return (ep != NULL) ? ep->getNumExternalParameters() : SBML_INT_MAX;
}


/*
 * Creates a new ExternalParameter_t object, adds it to this
 * ExternalParameter_t object and returns the ExternalParameter_t object
 * created.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_createExternalParameter(ExternalParameter_t* ep)
{
  return (ep != NULL) ? ep->createExternalParameter() : NULL;
}


/*
 * Removes the nth ExternalParameter_t from this ExternalParameter_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_removeExternalParameter(ExternalParameter_t* ep,
                                          unsigned int n)
{
  return (ep != NULL) ? ep->removeExternalParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ExternalParameter_t object have been set.
 */
LIBSBML_EXTERN
int
ExternalParameter_hasRequiredAttributes(const ExternalParameter_t * ep)
{
  return (ep != NULL) ? static_cast<int>(ep->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * ExternalParameter_t object have been set.
 */
LIBSBML_EXTERN
int
ExternalParameter_hasRequiredElements(const ExternalParameter_t * ep)
{
  return (ep != NULL) ? static_cast<int>(ep->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


