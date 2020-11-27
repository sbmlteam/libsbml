/**
 * @file Uncertainty.cpp
 * @brief Implementation of the Uncertainty class.
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
#include <sbml/packages/distrib/sbml/Uncertainty.h>
#include <sbml/packages/distrib/sbml/ListOfUncertainties.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Uncertainty using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
Uncertainty::Uncertainty(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mUncertParameters (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new Uncertainty using the given DistribPkgNamespaces object.
 */
Uncertainty::Uncertainty(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mUncertParameters (distribns)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for Uncertainty.
 */
Uncertainty::Uncertainty(const Uncertainty& orig)
  : DistribBase( orig )
  , mUncertParameters ( orig.mUncertParameters )
{
  connectToChild();
}


/*
 * Assignment operator for Uncertainty.
 */
Uncertainty&
Uncertainty::operator=(const Uncertainty& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mUncertParameters = rhs.mUncertParameters;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Uncertainty object.
 */
Uncertainty*
Uncertainty::clone() const
{
  return new Uncertainty(*this);
}


/*
 * Destructor for Uncertainty.
 */
Uncertainty::~Uncertainty()
{
}


/*
 * Returns the ListOfUncertParameters from this Uncertainty.
 */
const ListOfUncertParameters*
Uncertainty::getListOfUncertParameters() const
{
  return &mUncertParameters;
}


/*
 * Returns the ListOfUncertParameters from this Uncertainty.
 */
ListOfUncertParameters*
Uncertainty::getListOfUncertParameters()
{
  return &mUncertParameters;
}


/*
 * Get an UncertParameter from the Uncertainty.
 */
UncertParameter*
Uncertainty::getUncertParameter(unsigned int n)
{
  return mUncertParameters.get(n);
}


/*
 * Get an UncertParameter from the Uncertainty.
 */
const UncertParameter*
Uncertainty::getUncertParameter(unsigned int n) const
{
  return mUncertParameters.get(n);
}


/*
 * Get an UncertParameter from the Uncertainty based on the element to which it
 * refers.
 */
const UncertParameter*
Uncertainty::getUncertParameterByVar(const std::string& sid) const
{
  return mUncertParameters.getByVar(sid);
}


const UncertParameter*
Uncertainty::getUncertParameterByType(UncertType_t utype) const
{
  return mUncertParameters.getByType(utype);
}


/*
 * Get an UncertParameter from the Uncertainty based on the element to which it
 * refers.
 */
UncertParameter*
Uncertainty::getUncertParameterByVar(const std::string& sid)
{
  return mUncertParameters.getByVar(sid);
}


UncertParameter*
Uncertainty::getUncertParameterByType(UncertType_t utype)
{
  return mUncertParameters.getByType(utype);
}


/*
 * Adds a copy of the given UncertParameter to this Uncertainty.
 */
int
Uncertainty::addUncertParameter(const UncertParameter* up)
{
  if (up == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mUncertParameters.append(up);
  }
}


/*
* Adds a copy of the given UncertParameter to this Uncertainty.
*/
int
Uncertainty::addUncertSpan(const UncertSpan* up)
{
  if (up == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mUncertParameters.append((UncertParameter*)(up));
  }
}


/*
 * Get the number of UncertParameter objects in this Uncertainty.
 */
unsigned int
Uncertainty::getNumUncertParameters() const
{
  return mUncertParameters.size();
}


UncertParameter*
Uncertainty::createUncertParameter()
{
  UncertParameter* up = NULL;
  
  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up = new UncertParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up != NULL) mUncertParameters.appendAndOwn(up);

  return up;
}


UncertSpan*
Uncertainty::createUncertSpan()
{
  UncertSpan* up = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up = new UncertSpan(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up != NULL) mUncertParameters.appendAndOwn((UncertParameter*)(up));

  return up;
}



/*
 * Removes the nth UncertParameter from this Uncertainty and returns a pointer
 * to it.
 */
UncertParameter*
Uncertainty::removeUncertParameter(unsigned int n)
{
  return mUncertParameters.remove(n);
}


/*
 * Returns the XML element name of this Uncertainty object.
 */
const std::string&
Uncertainty::getElementName() const
{
  static const string name = "uncertainty";
  return name;
}


/*
 * Returns the libSBML type code for this Uncertainty object.
 */
int
Uncertainty::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTAINTY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Uncertainty object have been set.
 */
bool
Uncertainty::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * Uncertainty object have been set.
 */
bool
Uncertainty::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Uncertainty::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  for (unsigned int i = 0; i < getNumUncertParameters(); i++)
  {
    getUncertParameter(i)->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Uncertainty::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mUncertParameters.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Uncertainty::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);

  mUncertParameters.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Uncertainty::connectToChild()
{
  DistribBase::connectToChild();

  mUncertParameters.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Uncertainty::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mUncertParameters.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Uncertainty::updateSBMLNamespace(const std::string& package,
                                 unsigned int level,
                                 unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);

  mUncertParameters.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Uncertainty's attribute "attributeName"
 * is set.
 */
bool
Uncertainty::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Uncertainty.
 */
SBase*
Uncertainty::createChildObject(const std::string& elementName)
{
  DistribBase* obj = NULL;

  if (elementName == "uncertParameter")
  {
    return createUncertParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Uncertainty.
 */
int
Uncertainty::addChildObject(const std::string& elementName,
                            const SBase* element)
{
  if (elementName == "uncertParameter" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTPARAMETER)
  {
    return addUncertParameter((const UncertParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Uncertainty.
 */
SBase*
Uncertainty::removeChildObject(const std::string& elementName,
                               const std::string& id)
{
  if (elementName == "uncertParameter")
  {
    for (unsigned int i = 0; i < getNumUncertParameters(); i++)
    {
      if (getUncertParameter(i)->getId() == id)
      {
        return removeUncertParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Uncertainty.
 */
unsigned int
Uncertainty::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "uncertParameter")
  {
    return getNumUncertParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Uncertainty.
 */
SBase*
Uncertainty::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "uncertParameter")
  {
    return getUncertParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Uncertainty::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mUncertParameters.getElementBySId(id);

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
Uncertainty::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertParameters.getMetaId() == metaid)
  {
    return &mUncertParameters;
  }

  obj = mUncertParameters.getElementByMetaId(metaid);

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
Uncertainty::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mUncertParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Uncertainty::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "uncertParameter")
  {
    obj = mUncertParameters.createObject(stream);
  }
  else if (name == "uncertSpan")
  {
    obj = mUncertParameters.createObject(stream);
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
Uncertainty::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Uncertainty::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfUncertainties*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribSBaseLOUncertaintiesAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }

  DistribBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribUncertaintyAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
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
Uncertainty::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Uncertainty_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
Uncertainty_t *
Uncertainty_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new Uncertainty(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Uncertainty_t object.
 */
LIBSBML_EXTERN
Uncertainty_t*
Uncertainty_clone(const Uncertainty_t* u)
{
  if (u != NULL)
  {
    return static_cast<Uncertainty_t*>(u->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Uncertainty_t object.
 */
LIBSBML_EXTERN
void
Uncertainty_free(Uncertainty_t* u)
{
  if (u != NULL)
  {
    delete u;
  }
}


/*
 * Returns a ListOf_t * containing UncertParameter_t objects from this
 * Uncertainty_t.
 */
LIBSBML_EXTERN
ListOf_t*
Uncertainty_getListOfUncertParameters(Uncertainty_t* u)
{
  return (u != NULL) ? u->getListOfUncertParameters() : NULL;
}


/*
 * Get an UncertParameter_t from the Uncertainty_t.
 */
LIBSBML_EXTERN
UncertParameter_t*
Uncertainty_getUncertParameter(Uncertainty_t* u, unsigned int n)
{
  return (u != NULL) ? u->getUncertParameter(n) : NULL;
}


/*
 * Get an UncertParameter_t from the Uncertainty_t based on the element to
 * which it refers.
 */
LIBSBML_EXTERN
UncertParameter_t*
Uncertainty_getUncertParameterByVar(Uncertainty_t* u, const char *sid)
{
  return (u != NULL && sid != NULL) ? u->getUncertParameterByVar(sid) : NULL;
}


/*
 * Adds a copy of the given UncertParameter_t to this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_addUncertParameter(Uncertainty_t* u, const UncertParameter_t* up)
{
  return (u != NULL) ? u->addUncertParameter(up) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of UncertParameter_t objects in this Uncertainty_t.
 */
LIBSBML_EXTERN
unsigned int
Uncertainty_getNumUncertParameters(Uncertainty_t* u)
{
  return (u != NULL) ? u->getNumUncertParameters() : SBML_INT_MAX;
}


/*
 * Removes the nth UncertParameter_t from this Uncertainty_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
UncertParameter_t*
Uncertainty_removeUncertParameter(Uncertainty_t* u, unsigned int n)
{
  return (u != NULL) ? u->removeUncertParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Uncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredAttributes(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * Uncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredElements(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


