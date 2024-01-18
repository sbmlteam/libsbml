/**
 * @file ListOfKeyValuePairs.cpp
 * @brief Implementation of the ListOfKeyValuePairs class.
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
#include <sbml/packages/fbc/sbml/ListOfKeyValuePairs.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfKeyValuePairs using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
ListOfKeyValuePairs::ListOfKeyValuePairs(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
  , mXmlns ("http://sbml.org/fbc/keyvaluepair")
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfKeyValuePairs using the given FbcPkgNamespaces object.
 */
ListOfKeyValuePairs::ListOfKeyValuePairs(FbcPkgNamespaces *fbcns)
  : ListOf(fbcns)
  , mXmlns ("http://sbml.org/fbc/keyvaluepair")
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Copy constructor for ListOfKeyValuePairs.
 */
ListOfKeyValuePairs::ListOfKeyValuePairs(const ListOfKeyValuePairs& orig)
  : ListOf( orig )
  , mXmlns ( orig.mXmlns )
{
}


/*
 * Assignment operator for ListOfKeyValuePairs.
 */
ListOfKeyValuePairs&
ListOfKeyValuePairs::operator=(const ListOfKeyValuePairs& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mXmlns = rhs.mXmlns;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfKeyValuePairs object.
 */
ListOfKeyValuePairs*
ListOfKeyValuePairs::clone() const
{
  return new ListOfKeyValuePairs(*this);
}


/*
 * Destructor for ListOfKeyValuePairs.
 */
ListOfKeyValuePairs::~ListOfKeyValuePairs()
{
}


/*
 * Returns the value of the "xmlns" attribute of this ListOfKeyValuePairs.
 */
const std::string&
ListOfKeyValuePairs::getXmlns() const
{
  return mXmlns;
}


/*
 * Predicate returning @c true if this ListOfKeyValuePairs's "xmlns" attribute
 * is set.
 */
bool
ListOfKeyValuePairs::isSetXmlns() const
{
  return (mXmlns.empty() == false);
}

int 
ListOfKeyValuePairs::setXmlns(const XMLNamespaces* xmlns, const std::string& prefix)
{
  if (xmlns == NULL)
    mXmlns = "";
  else
    mXmlns = xmlns->getURI(prefix);

  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the value of the "xmlns" attribute of this ListOfKeyValuePairs.
 */
int
ListOfKeyValuePairs::setXmlns(const std::string& xmlns)
{
  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    mXmlns = xmlns;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Unsets the value of the "xmlns" attribute of this ListOfKeyValuePairs.
 */
int
ListOfKeyValuePairs::unsetXmlns()
{
  mXmlns.erase();

  if (mXmlns.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Get a KeyValuePair from the ListOfKeyValuePairs.
 */
KeyValuePair*
ListOfKeyValuePairs::get(unsigned int n)
{
  return static_cast<KeyValuePair*>(ListOf::get(n));
}


/*
 * Get a KeyValuePair from the ListOfKeyValuePairs.
 */
const KeyValuePair*
ListOfKeyValuePairs::get(unsigned int n) const
{
  return static_cast<const KeyValuePair*>(ListOf::get(n));
}


/*
 * Get a KeyValuePair from the ListOfKeyValuePairs based on its identifier.
 */
KeyValuePair*
ListOfKeyValuePairs::get(const std::string& sid)
{
  return const_cast<KeyValuePair*>(static_cast<const
    ListOfKeyValuePairs&>(*this).get(sid));
}


/*
 * Get a KeyValuePair from the ListOfKeyValuePairs based on its identifier.
 */
const KeyValuePair*
ListOfKeyValuePairs::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<KeyValuePair>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const KeyValuePair*>
    (*result);
}


/*
 * Removes the nth KeyValuePair from this ListOfKeyValuePairs and returns a
 * pointer to it.
 */
KeyValuePair*
ListOfKeyValuePairs::remove(unsigned int n)
{
  return static_cast<KeyValuePair*>(ListOf::remove(n));
}


/*
 * Removes the KeyValuePair from this ListOfKeyValuePairs based on its
 * identifier and returns a pointer to it.
 */
KeyValuePair*
ListOfKeyValuePairs::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<KeyValuePair>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <KeyValuePair*> (item);
}


/*
 * Adds a copy of the given KeyValuePair to this ListOfKeyValuePairs.
 */
int
ListOfKeyValuePairs::addKeyValuePair(const KeyValuePair* kvp)
{
  if (kvp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (kvp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != kvp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(kvp)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(kvp);
  }
}


/*
 * Get the number of KeyValuePair objects in this ListOfKeyValuePairs.
 */
unsigned int
ListOfKeyValuePairs::getNumKeyValuePairs() const
{
  return size();
}


/*
 * Creates a new KeyValuePair object, adds it to this ListOfKeyValuePairs
 * object and returns the KeyValuePair object created.
 */
KeyValuePair*
ListOfKeyValuePairs::createKeyValuePair()
{
  KeyValuePair* kvp = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(),
      getPackageVersion());
    kvp = new KeyValuePair(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (kvp != NULL)
  {
    appendAndOwn(kvp);
  }

  return kvp;
}


/*
 * Returns the XML element name of this ListOfKeyValuePairs object.
 */
const std::string&
ListOfKeyValuePairs::getElementName() const
{
  static const string name = "listOfKeyValuePairs";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfKeyValuePairs object.
 */
int
ListOfKeyValuePairs::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfKeyValuePairs object.
 */
int
ListOfKeyValuePairs::getItemTypeCode() const
{
  return SBML_FBC_KEYVALUEPAIR;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfKeyValuePairs object have been set.
 */
bool
ListOfKeyValuePairs::hasRequiredAttributes() const
{
  bool allPresent = true;

  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    if (isSetXmlns() == false)
    {
      allPresent = false;
    }
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new KeyValuePair in this ListOfKeyValuePairs
 */
SBase*
ListOfKeyValuePairs::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());

  if (name == "keyValuePair")
  {
    object = new KeyValuePair(fbcns);
    appendAndOwn(object);
  }

  delete fbcns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ListOfKeyValuePairs::addExpectedAttributes(ExpectedAttributes& attributes)
{
  ListOf::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfKeyValuePairs::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("fbc", FbcSBaseLOKeyValuePairsAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc",
          FbcSBaseLOKeyValuePairsAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }

  if (pkgVersion >= 3)
  {
    readL3V1V3Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfKeyValuePairs::readL3V1V3Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // xmlns string (use = "required" )
  // 

  // xmlns is not one of the normal attributes and has to be handled differently
  //assigned = attributes.readInto("xmlns", mXmlns);
  setXmlns(getNamespaces());
  assigned = !mXmlns.empty();


  if (assigned == true)
  {
    if (mXmlns.empty() == true)
    {
      logEmptyString(mXmlns, level, version, "<ListOfKeyValuePairs>");
    }
  }
  // else
  // {
  //   if (log)
  //   {
  //     std::string message = "Fbc attribute 'xmlns' is missing from the "
  //       "<ListOfKeyValuePairs> element.";
  //     log->logPackageError("fbc", FbcKeyValuePairAllowedAttributes, pkgVersion, level, version,
  //       message, getLine(), getColumn());
  //   }
  // }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfKeyValuePairs::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    writeL3V1V3Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfKeyValuePairs::writeL3V1V3Attributes(XMLOutputStream& stream) const
{  
}

/** @endcond */


/*
 * Writes the namespace
 */
void
ListOfKeyValuePairs::writeXMLNS(XMLOutputStream& stream) const
{

  if (mXmlns.empty())
    return;

  XMLNamespaces xmlns;
  std::string prefix = getPrefix();
  xmlns.add(mXmlns, prefix);
  stream << xmlns;
}




#endif /* __cplusplus */


/*
 * Returns the value of the "xmlns" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfKeyValuePairs_getXmlns(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfKeyValuePairs*>(lo)->getXmlns().empty() ? NULL
    : safe_strdup(static_cast<const
      ListOfKeyValuePairs*>(lo)->getXmlns().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "xmlns" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_isSetXmlns(const ListOf_t * lo)
{
  return (static_cast<const ListOfKeyValuePairs*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfKeyValuePairs*>(lo)->isSetXmlns()) :
      0;
}


/*
 * Sets the value of the "xmlns" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_setXmlns(ListOf_t * lo, const char * xmlns)
{
  return (static_cast<ListOfKeyValuePairs*>(lo) != NULL) ?
    static_cast<ListOfKeyValuePairs*>(lo)->setXmlns(xmlns) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "xmlns" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_unsetXmlns(ListOf_t * lo)
{
  return (static_cast<ListOfKeyValuePairs*>(lo) != NULL) ?
    static_cast<ListOfKeyValuePairs*>(lo)->unsetXmlns() : LIBSBML_INVALID_OBJECT;
}


/*
 * Get a KeyValuePair_t from the ListOf_t.
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_getKeyValuePair(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfKeyValuePairs*>(lo)->get(n);
}


/*
 * Get a KeyValuePair_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfKeyValuePairs*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth KeyValuePair_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfKeyValuePairs*>(lo)->remove(n);
}


/*
 * Removes the KeyValuePair_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfKeyValuePairs*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


