/**
 * @file ListOfDistribExternalParameters.cpp
 * @brief Implementation of the ListOfDistribExternalParameters class.
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
#include <sbml/packages/distrib/sbml/ListOfDistribExternalParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDistribExternalParameters using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
ListOfDistribExternalParameters::ListOfDistribExternalParameters(
                                                                 unsigned int
                                                                   level,
                                                                 unsigned int
                                                                   version,
                                                                 unsigned int
                                                                   pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDistribExternalParameters using the given
 * DistribPkgNamespaces object.
 */
ListOfDistribExternalParameters::ListOfDistribExternalParameters(DistribPkgNamespaces
  *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfDistribExternalParameters.
 */
ListOfDistribExternalParameters::ListOfDistribExternalParameters(const
  ListOfDistribExternalParameters& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDistribExternalParameters.
 */
ListOfDistribExternalParameters&
ListOfDistribExternalParameters::operator=(const
  ListOfDistribExternalParameters& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDistribExternalParameters
 * object.
 */
ListOfDistribExternalParameters*
ListOfDistribExternalParameters::clone() const
{
  return new ListOfDistribExternalParameters(*this);
}


/*
 * Destructor for ListOfDistribExternalParameters.
 */
ListOfDistribExternalParameters::~ListOfDistribExternalParameters()
{
}


/*
 * Returns the value of the "id" attribute of this
 * ListOfDistribExternalParameters.
 */
const std::string&
ListOfDistribExternalParameters::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * ListOfDistribExternalParameters.
 */
const std::string&
ListOfDistribExternalParameters::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this ListOfDistribExternalParameters's "id"
 * attribute is set.
 */
bool
ListOfDistribExternalParameters::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ListOfDistribExternalParameters's "name"
 * attribute is set.
 */
bool
ListOfDistribExternalParameters::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this
 * ListOfDistribExternalParameters.
 */
int
ListOfDistribExternalParameters::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * ListOfDistribExternalParameters.
 */
int
ListOfDistribExternalParameters::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * ListOfDistribExternalParameters.
 */
int
ListOfDistribExternalParameters::unsetId()
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
 * ListOfDistribExternalParameters.
 */
int
ListOfDistribExternalParameters::unsetName()
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
 * Get a DistribExternalParameter from the ListOfDistribExternalParameters.
 */
DistribExternalParameter*
ListOfDistribExternalParameters::get(unsigned int n)
{
  return static_cast<DistribExternalParameter*>(ListOf::get(n));
}


/*
 * Get a DistribExternalParameter from the ListOfDistribExternalParameters.
 */
const DistribExternalParameter*
ListOfDistribExternalParameters::get(unsigned int n) const
{
  return static_cast<const DistribExternalParameter*>(ListOf::get(n));
}


/*
 * Get a DistribExternalParameter from the ListOfDistribExternalParameters
 * based on its identifier.
 */
DistribExternalParameter*
ListOfDistribExternalParameters::get(const std::string& sid)
{
  return const_cast<DistribExternalParameter*>(static_cast<const
    ListOfDistribExternalParameters&>(*this).get(sid));
}


/*
 * Get a DistribExternalParameter from the ListOfDistribExternalParameters
 * based on its identifier.
 */
const DistribExternalParameter*
ListOfDistribExternalParameters::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<DistribExternalParameter>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    DistribExternalParameter*> (*result);
}


/*
 * Removes the nth DistribExternalParameter from this
 * ListOfDistribExternalParameters and returns a pointer to it.
 */
DistribExternalParameter*
ListOfDistribExternalParameters::remove(unsigned int n)
{
  return static_cast<DistribExternalParameter*>(ListOf::remove(n));
}


/*
 * Removes the DistribExternalParameter from this
 * ListOfDistribExternalParameters based on its identifier and returns a
 * pointer to it.
 */
DistribExternalParameter*
ListOfDistribExternalParameters::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<DistribExternalParameter>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribExternalParameter*> (item);
}


/*
 * Adds a copy of the given DistribExternalParameter to this
 * ListOfDistribExternalParameters.
 */
int
ListOfDistribExternalParameters::addDistribExternalParameter(const
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
    return append(dep);
  }
}


/*
 * Get the number of DistribExternalParameter objects in this
 * ListOfDistribExternalParameters.
 */
unsigned int
ListOfDistribExternalParameters::getNumDistribExternalParameters() const
{
  return size();
}


/*
 * Creates a new DistribExternalParameter object, adds it to this
 * ListOfDistribExternalParameters object and returns the
 * DistribExternalParameter object created.
 */
DistribExternalParameter*
ListOfDistribExternalParameters::createDistribExternalParameter()
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
    appendAndOwn(dep);
  }

  return dep;
}


/*
 * Returns the XML element name of this ListOfDistribExternalParameters object.
 */
const std::string&
ListOfDistribExternalParameters::getElementName() const
{
  static const string name = "listOfExternalParameters";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDistribExternalParameters
 * object.
 */
int
ListOfDistribExternalParameters::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDistribExternalParameters object.
 */
int
ListOfDistribExternalParameters::getItemTypeCode() const
{
  return SBML_DISTRIB_EXTERNALPARAMETER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfDistribExternalParameters object have been set.
 */
bool
ListOfDistribExternalParameters::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribExternalParameter in this
 * ListOfDistribExternalParameters
 */
SBase*
ListOfDistribExternalParameters::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "externalParameter")
  {
    object = new DistribExternalParameter(distribns);
    appendAndOwn(object);
  }

  delete distribns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ListOfDistribExternalParameters::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  ListOf::addExpectedAttributes(attributes);

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
ListOfDistribExternalParameters::readAttributes(
                                                const XMLAttributes&
                                                  attributes,
                                                const ExpectedAttributes&
                                                  expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  ListOf::readAttributes(attributes, expectedAttributes);

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
ListOfDistribExternalParameters::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<ListOfDistribExternalParameters>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version,
        "<ListOfDistribExternalParameters>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfDistribExternalParameters::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<ListOfDistribExternalParameters>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfDistribExternalParameters::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

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
ListOfDistribExternalParameters::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfDistribExternalParameters::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Distrib package
 */
void
ListOfDistribExternalParameters::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(DistribExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(DistribExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfDistribExternalParameters_getId(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const
    ListOfDistribExternalParameters*>(lo)->getId().empty() ? NULL :
      safe_strdup(static_cast<const
        ListOfDistribExternalParameters*>(lo)->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfDistribExternalParameters_getName(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const
    ListOfDistribExternalParameters*>(lo)->getName().empty() ? NULL :
      safe_strdup(static_cast<const
        ListOfDistribExternalParameters*>(lo)->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_isSetId(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfDistribExternalParameters*>(lo)->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_isSetName(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfDistribExternalParameters*>(lo)->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_setId(ListOf_t * lo, const char * id)
{
  return (static_cast<ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfDistribExternalParameters*>(lo)->setId(id) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_setName(ListOf_t * lo, const char * name)
{
  return (static_cast<ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfDistribExternalParameters*>(lo)->setName(name) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_unsetId(ListOf_t * lo)
{
  return (static_cast<ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfDistribExternalParameters*>(lo)->unsetId() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribExternalParameters_unsetName(ListOf_t * lo)
{
  return (static_cast<ListOfDistribExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfDistribExternalParameters*>(lo)->unsetName() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Get a DistribExternalParameter_t from the ListOf_t.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfDistribExternalParameters_getDistribExternalParameter(ListOf_t* lo,
                                                            unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribExternalParameters*>(lo)->get(n);
}


/*
 * Get a DistribExternalParameter_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfDistribExternalParameters_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfDistribExternalParameters*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth DistribExternalParameter_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfDistribExternalParameters_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribExternalParameters*>(lo)->remove(n);
}


/*
 * Removes the DistribExternalParameter_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfDistribExternalParameters_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfDistribExternalParameters*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


