/**
 * @file ListOfDistribCategories.cpp
 * @brief Implementation of the ListOfDistribCategories class.
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
#include <sbml/packages/distrib/sbml/ListOfDistribCategories.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDistribCategories using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
ListOfDistribCategories::ListOfDistribCategories(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDistribCategories using the given DistribPkgNamespaces
 * object.
 */
ListOfDistribCategories::ListOfDistribCategories(DistribPkgNamespaces
  *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfDistribCategories.
 */
ListOfDistribCategories::ListOfDistribCategories(const ListOfDistribCategories&
  orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDistribCategories.
 */
ListOfDistribCategories&
ListOfDistribCategories::operator=(const ListOfDistribCategories& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDistribCategories object.
 */
ListOfDistribCategories*
ListOfDistribCategories::clone() const
{
  return new ListOfDistribCategories(*this);
}


/*
 * Destructor for ListOfDistribCategories.
 */
ListOfDistribCategories::~ListOfDistribCategories()
{
}


/*
 * Returns the value of the "id" attribute of this ListOfDistribCategories.
 */
const std::string&
ListOfDistribCategories::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ListOfDistribCategories.
 */
const std::string&
ListOfDistribCategories::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this ListOfDistribCategories's "id" attribute
 * is set.
 */
bool
ListOfDistribCategories::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ListOfDistribCategories's "name"
 * attribute is set.
 */
bool
ListOfDistribCategories::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this ListOfDistribCategories.
 */
int
ListOfDistribCategories::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ListOfDistribCategories.
 */
int
ListOfDistribCategories::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this ListOfDistribCategories.
 */
int
ListOfDistribCategories::unsetId()
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
 * Unsets the value of the "name" attribute of this ListOfDistribCategories.
 */
int
ListOfDistribCategories::unsetName()
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
 * Get a DistribCategory from the ListOfDistribCategories.
 */
DistribCategory*
ListOfDistribCategories::get(unsigned int n)
{
  return static_cast<DistribCategory*>(ListOf::get(n));
}


/*
 * Get a DistribCategory from the ListOfDistribCategories.
 */
const DistribCategory*
ListOfDistribCategories::get(unsigned int n) const
{
  return static_cast<const DistribCategory*>(ListOf::get(n));
}


/*
 * Get a DistribCategory from the ListOfDistribCategories based on its
 * identifier.
 */
DistribCategory*
ListOfDistribCategories::get(const std::string& sid)
{
  return const_cast<DistribCategory*>(static_cast<const
    ListOfDistribCategories&>(*this).get(sid));
}


/*
 * Get a DistribCategory from the ListOfDistribCategories based on its
 * identifier.
 */
const DistribCategory*
ListOfDistribCategories::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribCategory>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const DistribCategory*>
    (*result);
}


/*
 * Removes the nth DistribCategory from this ListOfDistribCategories and
 * returns a pointer to it.
 */
DistribCategory*
ListOfDistribCategories::remove(unsigned int n)
{
  return static_cast<DistribCategory*>(ListOf::remove(n));
}


/*
 * Removes the DistribCategory from this ListOfDistribCategories based on its
 * identifier and returns a pointer to it.
 */
DistribCategory*
ListOfDistribCategories::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribCategory>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribCategory*> (item);
}


/*
 * Adds a copy of the given DistribCategory to this ListOfDistribCategories.
 */
int
ListOfDistribCategories::addDistribCategory(const DistribCategory* dc)
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
  else
  {
    return append(dc);
  }
}


/*
 * Get the number of DistribCategory objects in this ListOfDistribCategories.
 */
unsigned int
ListOfDistribCategories::getNumDistribCategories() const
{
  return size();
}


/*
 * Creates a new DistribCategory object, adds it to this
 * ListOfDistribCategories object and returns the DistribCategory object
 * created.
 */
DistribCategory*
ListOfDistribCategories::createDistribCategory()
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
    appendAndOwn(dc);
  }

  return dc;
}


/*
 * Returns the XML element name of this ListOfDistribCategories object.
 */
const std::string&
ListOfDistribCategories::getElementName() const
{
  static const string name = "listOfCategories";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDistribCategories object.
 */
int
ListOfDistribCategories::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDistribCategories object.
 */
int
ListOfDistribCategories::getItemTypeCode() const
{
  return SBML_DISTRIB_CATEGORY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfDistribCategories object have been set.
 */
bool
ListOfDistribCategories::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribCategory in this ListOfDistribCategories
 */
SBase*
ListOfDistribCategories::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "category")
  {
    object = new DistribCategory(distribns);
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
ListOfDistribCategories::addExpectedAttributes(ExpectedAttributes& attributes)
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
ListOfDistribCategories::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribCategoricalDistributionLODistribCategoriesAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribCategoricalDistributionLODistribCategoriesAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  else
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
ListOfDistribCategories::readL3V1V1Attributes(const XMLAttributes& attributes)
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
      logEmptyString(mId, level, version, "<ListOfDistribCategories>");
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
      logEmptyString(mName, level, version, "<ListOfDistribCategories>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfDistribCategories::readL3V2V1Attributes(const XMLAttributes& attributes)
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
      logEmptyString(mId, level, version, "<ListOfDistribCategories>");
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
ListOfDistribCategories::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  else
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
ListOfDistribCategories::writeL3V1V1Attributes(XMLOutputStream& stream) const
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
ListOfDistribCategories::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Distrib package
 */
void
ListOfDistribCategories::writeXMLNS(XMLOutputStream& stream) const
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
ListOfDistribCategories_getId(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfDistribCategories*>(lo)->getId().empty() ?
    NULL : safe_strdup(static_cast<const
      ListOfDistribCategories*>(lo)->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfDistribCategories_getName(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfDistribCategories*>(lo)->getName().empty() ?
    NULL : safe_strdup(static_cast<const
      ListOfDistribCategories*>(lo)->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_isSetId(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfDistribCategories*>(lo)->isSetId())
      : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_isSetName(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfDistribCategories*>(lo)->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_setId(ListOf_t * lo, const char * id)
{
  return (static_cast<ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<ListOfDistribCategories*>(lo)->setId(id) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_setName(ListOf_t * lo, const char * name)
{
  return (static_cast<ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<ListOfDistribCategories*>(lo)->setName(name) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_unsetId(ListOf_t * lo)
{
  return (static_cast<ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<ListOfDistribCategories*>(lo)->unsetId() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_unsetName(ListOf_t * lo)
{
  return (static_cast<ListOfDistribCategories*>(lo) != NULL) ?
    static_cast<ListOfDistribCategories*>(lo)->unsetName() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Get a DistribCategory_t from the ListOf_t.
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_getDistribCategory(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribCategories*>(lo)->get(n);
}


/*
 * Get a DistribCategory_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDistribCategories*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth DistribCategory_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribCategories*>(lo)->remove(n);
}


/*
 * Removes the DistribCategory_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfDistribCategories*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


