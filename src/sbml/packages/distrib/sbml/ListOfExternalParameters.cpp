/**
 * @file ListOfExternalParameters.cpp
 * @brief Implementation of the ListOfExternalParameters class.
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
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfExternalParameters using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
ListOfExternalParameters::ListOfExternalParameters(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfExternalParameters using the given DistribPkgNamespaces
 * object.
 */
ListOfExternalParameters::ListOfExternalParameters(DistribPkgNamespaces
  *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfExternalParameters.
 */
ListOfExternalParameters::ListOfExternalParameters(const
  ListOfExternalParameters& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfExternalParameters.
 */
ListOfExternalParameters&
ListOfExternalParameters::operator=(const ListOfExternalParameters& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfExternalParameters object.
 */
ListOfExternalParameters*
ListOfExternalParameters::clone() const
{
  return new ListOfExternalParameters(*this);
}


/*
 * Destructor for ListOfExternalParameters.
 */
ListOfExternalParameters::~ListOfExternalParameters()
{
}


/*
 * Returns the value of the "id" attribute of this ListOfExternalParameters.
 */
const std::string&
ListOfExternalParameters::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ListOfExternalParameters.
 */
const std::string&
ListOfExternalParameters::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this ListOfExternalParameters's "id"
 * attribute is set.
 */
bool
ListOfExternalParameters::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ListOfExternalParameters's "name"
 * attribute is set.
 */
bool
ListOfExternalParameters::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this ListOfExternalParameters.
 */
int
ListOfExternalParameters::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ListOfExternalParameters.
 */
int
ListOfExternalParameters::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this ListOfExternalParameters.
 */
int
ListOfExternalParameters::unsetId()
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
 * Unsets the value of the "name" attribute of this ListOfExternalParameters.
 */
int
ListOfExternalParameters::unsetName()
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
 * Get an ExternalParameter from the ListOfExternalParameters.
 */
ExternalParameter*
ListOfExternalParameters::get(unsigned int n)
{
  return static_cast<ExternalParameter*>(ListOf::get(n));
}


/*
 * Get an ExternalParameter from the ListOfExternalParameters.
 */
const ExternalParameter*
ListOfExternalParameters::get(unsigned int n) const
{
  return static_cast<const ExternalParameter*>(ListOf::get(n));
}


/*
 * Get an ExternalParameter from the ListOfExternalParameters based on its
 * identifier.
 */
ExternalParameter*
ListOfExternalParameters::get(const std::string& sid)
{
  return const_cast<ExternalParameter*>(static_cast<const
    ListOfExternalParameters&>(*this).get(sid));
}


/*
 * Get an ExternalParameter from the ListOfExternalParameters based on its
 * identifier.
 */
const ExternalParameter*
ListOfExternalParameters::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<ExternalParameter>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const ExternalParameter*>
    (*result);
}


/*
 * Removes the nth ExternalParameter from this ListOfExternalParameters and
 * returns a pointer to it.
 */
ExternalParameter*
ListOfExternalParameters::remove(unsigned int n)
{
  return static_cast<ExternalParameter*>(ListOf::remove(n));
}


/*
 * Removes the ExternalParameter from this ListOfExternalParameters based on
 * its identifier and returns a pointer to it.
 */
ExternalParameter*
ListOfExternalParameters::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<ExternalParameter>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <ExternalParameter*> (item);
}


/*
 * Adds a copy of the given ExternalParameter to this ListOfExternalParameters.
 */
int
ListOfExternalParameters::addExternalParameter(const ExternalParameter* ep)
{
  if (ep == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ep->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ep->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ep->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ep)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(ep);
  }
}


/*
 * Get the number of ExternalParameter objects in this
 * ListOfExternalParameters.
 */
unsigned int
ListOfExternalParameters::getNumExternalParameters() const
{
  return size();
}


/*
 * Creates a new ExternalParameter object, adds it to this
 * ListOfExternalParameters object and returns the ExternalParameter object
 * created.
 */
ExternalParameter*
ListOfExternalParameters::createExternalParameter()
{
  ExternalParameter* ep = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    ep = new ExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (ep != NULL)
  {
    appendAndOwn(ep);
  }

  return ep;
}


/*
 * Returns the XML element name of this ListOfExternalParameters object.
 */
const std::string&
ListOfExternalParameters::getElementName() const
{
  static const string name = "listOfExternalParameters";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfExternalParameters object.
 */
int
ListOfExternalParameters::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfExternalParameters object.
 */
int
ListOfExternalParameters::getItemTypeCode() const
{
  return SBML_DISTRIB_EXTERNALPARAMETER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfExternalParameters object have been set.
 */
bool
ListOfExternalParameters::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ExternalParameter in this ListOfExternalParameters
 */
SBase*
ListOfExternalParameters::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "externalParameter")
  {
    object = new ExternalParameter(distribns);
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
ListOfExternalParameters::addExpectedAttributes(ExpectedAttributes& attributes)
{
  ListOf::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfExternalParameters::readAttributes(const XMLAttributes& attributes,
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

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<ListOfExternalParameters>");
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

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<ListOfExternalParameters>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfExternalParameters::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Distrib package
 */
void
ListOfExternalParameters::writeXMLNS(XMLOutputStream& stream) const
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
ListOfExternalParameters_getId(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfExternalParameters*>(lo)->getId().empty() ?
    NULL : safe_strdup(static_cast<const
      ListOfExternalParameters*>(lo)->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfExternalParameters_getName(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfExternalParameters*>(lo)->getName().empty() ?
    NULL : safe_strdup(static_cast<const
      ListOfExternalParameters*>(lo)->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_isSetId(const ListOf_t * lo)
{
  return (static_cast<const ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfExternalParameters*>(lo)->isSetId())
      : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_isSetName(const ListOf_t * lo)
{
  return (static_cast<const ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfExternalParameters*>(lo)->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_setId(ListOf_t * lo, const char * id)
{
  return (static_cast<ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfExternalParameters*>(lo)->setId(id) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_setName(ListOf_t * lo, const char * name)
{
  return (static_cast<ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfExternalParameters*>(lo)->setName(name) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_unsetId(ListOf_t * lo)
{
  return (static_cast<ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfExternalParameters*>(lo)->unsetId() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfExternalParameters_unsetName(ListOf_t * lo)
{
  return (static_cast<ListOfExternalParameters*>(lo) != NULL) ?
    static_cast<ListOfExternalParameters*>(lo)->unsetName() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Get an ExternalParameter_t from the ListOf_t.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_getExternalParameter(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfExternalParameters*>(lo)->get(n);
}


/*
 * Get an ExternalParameter_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfExternalParameters*>(lo)->get(sid)
    : NULL;
}


/*
 * Removes the nth ExternalParameter_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfExternalParameters*>(lo)->remove(n);
}


/*
 * Removes the ExternalParameter_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
ExternalParameter_t*
ListOfExternalParameters_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfExternalParameters*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


