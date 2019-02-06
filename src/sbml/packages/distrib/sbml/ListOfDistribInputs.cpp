/**
 * @file ListOfDistribInputs.cpp
 * @brief Implementation of the ListOfDistribInputs class.
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
#include <sbml/packages/distrib/sbml/ListOfDistribInputs.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDistribInputs using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
ListOfDistribInputs::ListOfDistribInputs(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDistribInputs using the given DistribPkgNamespaces
 * object.
 */
ListOfDistribInputs::ListOfDistribInputs(DistribPkgNamespaces *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfDistribInputs.
 */
ListOfDistribInputs::ListOfDistribInputs(const ListOfDistribInputs& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDistribInputs.
 */
ListOfDistribInputs&
ListOfDistribInputs::operator=(const ListOfDistribInputs& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDistribInputs object.
 */
ListOfDistribInputs*
ListOfDistribInputs::clone() const
{
  return new ListOfDistribInputs(*this);
}


/*
 * Destructor for ListOfDistribInputs.
 */
ListOfDistribInputs::~ListOfDistribInputs()
{
}


/*
 * Returns the value of the "id" attribute of this ListOfDistribInputs.
 */
const std::string&
ListOfDistribInputs::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ListOfDistribInputs.
 */
const std::string&
ListOfDistribInputs::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this ListOfDistribInputs's "id" attribute is
 * set.
 */
bool
ListOfDistribInputs::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ListOfDistribInputs's "name" attribute
 * is set.
 */
bool
ListOfDistribInputs::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this ListOfDistribInputs.
 */
int
ListOfDistribInputs::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ListOfDistribInputs.
 */
int
ListOfDistribInputs::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this ListOfDistribInputs.
 */
int
ListOfDistribInputs::unsetId()
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
 * Unsets the value of the "name" attribute of this ListOfDistribInputs.
 */
int
ListOfDistribInputs::unsetName()
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
 * Get a DistribInput from the ListOfDistribInputs.
 */
DistribInput*
ListOfDistribInputs::get(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
DistribInput*
ListOfDistribInputs::getByIndex(unsigned int n)
{
  return const_cast<DistribInput*>(
    static_cast<const ListOfDistribInputs&>(*this).getByIndex(n));
}


#ifndef SWIG
template<class CNAME>
struct IndexEq : public std::unary_function<SBase*, bool>
{
  unsigned int n;

  IndexEq (unsigned int n) : n(n) { }
  bool operator() (SBase* sb) 
       { return static_cast <CNAME*> (sb)->getIndex() == n; }
};
#endif /* SWIG */

/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
const DistribInput*
ListOfDistribInputs::getByIndex(unsigned int n) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IndexEq<DistribInput>(n) );
  return (result == mItems.end()) ? 0 : static_cast <DistribInput*> (*result);
}


/*
 * Get a DistribInput from the ListOfDistribInputs.
 */
const DistribInput*
ListOfDistribInputs::get(unsigned int n) const
{
  return static_cast<const DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs based on its identifier.
 */
DistribInput*
ListOfDistribInputs::get(const std::string& sid)
{
  return const_cast<DistribInput*>(static_cast<const
    ListOfDistribInputs&>(*this).get(sid));
}


/*
 * Get a DistribInput from the ListOfDistribInputs based on its identifier.
 */
const DistribInput*
ListOfDistribInputs::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribInput>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const DistribInput*>
    (*result);
}


/*
 * Removes the nth DistribInput from this ListOfDistribInputs and returns a
 * pointer to it.
 */
DistribInput*
ListOfDistribInputs::remove(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::remove(n));
}


/*
 * Removes the DistribInput from this ListOfDistribInputs based on its
 * identifier and returns a pointer to it.
 */
DistribInput*
ListOfDistribInputs::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribInput>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribInput*> (item);
}


/*
 * Adds a copy of the given DistribInput to this ListOfDistribInputs.
 */
int
ListOfDistribInputs::addDistribInput(const DistribInput* di)
{
  if (di == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (di->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != di->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != di->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(di)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(di);
  }
}


/*
 * Get the number of DistribInput objects in this ListOfDistribInputs.
 */
unsigned int
ListOfDistribInputs::getNumDistribInputs() const
{
  return size();
}


/*
 * Creates a new DistribInput object, adds it to this ListOfDistribInputs
 * object and returns the DistribInput object created.
 */
DistribInput*
ListOfDistribInputs::createDistribInput()
{
  DistribInput* di = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    di = new DistribInput(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (di != NULL)
  {
    appendAndOwn(di);
  }

  return di;
}


/*
 * Returns the XML element name of this ListOfDistribInputs object.
 */
const std::string&
ListOfDistribInputs::getElementName() const
{
  static const string name = "listOfDistribInputs";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDistribInputs object.
 */
int
ListOfDistribInputs::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDistribInputs object.
 */
int
ListOfDistribInputs::getItemTypeCode() const
{
  return SBML_DISTRIB_DISTRIBINPUT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfDistribInputs object have been set.
 */
bool
ListOfDistribInputs::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribInput in this ListOfDistribInputs
 */
SBase*
ListOfDistribInputs::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "distribInput")
  {
    object = new DistribInput(distribns);
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
ListOfDistribInputs::addExpectedAttributes(ExpectedAttributes& attributes)
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
ListOfDistribInputs::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribDrawFromDistributionLODistribInputsAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribDrawFromDistributionLODistribInputsAllowedCoreAttributes,
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
ListOfDistribInputs::readL3V1V1Attributes(const XMLAttributes& attributes)
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
      logEmptyString(mId, level, version, "<ListOfDistribInputs>");
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
      logEmptyString(mName, level, version, "<ListOfDistribInputs>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfDistribInputs::readL3V2V1Attributes(const XMLAttributes& attributes)
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
      logEmptyString(mId, level, version, "<ListOfDistribInputs>");
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
ListOfDistribInputs::writeAttributes(XMLOutputStream& stream) const
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
ListOfDistribInputs::writeL3V1V1Attributes(XMLOutputStream& stream) const
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
ListOfDistribInputs::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Distrib package
 */
void
ListOfDistribInputs::writeXMLNS(XMLOutputStream& stream) const
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
ListOfDistribInputs_getId(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfDistribInputs*>(lo)->getId().empty() ? NULL :
    safe_strdup(static_cast<const ListOfDistribInputs*>(lo)->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfDistribInputs_getName(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfDistribInputs*>(lo)->getName().empty() ? NULL
    : safe_strdup(static_cast<const
      ListOfDistribInputs*>(lo)->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_isSetId(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfDistribInputs*>(lo)->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_isSetName(const ListOf_t * lo)
{
  return (static_cast<const ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfDistribInputs*>(lo)->isSetName()) :
      0;
}


/*
 * Sets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_setId(ListOf_t * lo, const char * id)
{
  return (static_cast<ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<ListOfDistribInputs*>(lo)->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_setName(ListOf_t * lo, const char * name)
{
  return (static_cast<ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<ListOfDistribInputs*>(lo)->setName(name) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_unsetId(ListOf_t * lo)
{
  return (static_cast<ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<ListOfDistribInputs*>(lo)->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfDistribInputs_unsetName(ListOf_t * lo)
{
  return (static_cast<ListOfDistribInputs*>(lo) != NULL) ?
    static_cast<ListOfDistribInputs*>(lo)->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Get a DistribInput_t from the ListOf_t.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getDistribInput(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribInputs*>(lo)->get(n);
}


/*
 * Get a DistribInput_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDistribInputs*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth DistribInput_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribInputs*>(lo)->remove(n);
}


/*
 * Removes the DistribInput_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDistribInputs*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


