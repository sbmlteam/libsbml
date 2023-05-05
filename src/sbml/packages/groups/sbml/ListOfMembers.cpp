/**
 * @file ListOfMembers.cpp
 * @brief Implementation of the ListOfMembers class.
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
#include <sbml/packages/groups/sbml/ListOfMembers.h>
#include <sbml/packages/groups/validator/GroupsSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfMembers using the given SBML Level, Version and
 * &ldquo;groups&rdquo; package version.
 */
ListOfMembers::ListOfMembers(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new GroupsPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfMembers using the given GroupsPkgNamespaces object.
 */
ListOfMembers::ListOfMembers(GroupsPkgNamespaces *groupsns)
  : ListOf(groupsns)
{
  setElementNamespace(groupsns->getURI());
}


/*
 * Copy constructor for ListOfMembers.
 */
ListOfMembers::ListOfMembers(const ListOfMembers& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfMembers.
 */
ListOfMembers&
ListOfMembers::operator=(const ListOfMembers& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfMembers object.
 */
ListOfMembers*
ListOfMembers::clone() const
{
  return new ListOfMembers(*this);
}


/*
 * Destructor for ListOfMembers.
 */
ListOfMembers::~ListOfMembers()
{
}


/*
 * Returns the value of the "id" attribute of this ListOfMembers.
 */
const std::string&
ListOfMembers::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ListOfMembers.
 */
const std::string&
ListOfMembers::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this ListOfMembers's "id" attribute is set.
 */
bool
ListOfMembers::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ListOfMembers's "name" attribute is set.
 */
bool
ListOfMembers::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this ListOfMembers.
 */
int
ListOfMembers::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ListOfMembers.
 */
int
ListOfMembers::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this ListOfMembers.
 */
int
ListOfMembers::unsetId()
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
 * Unsets the value of the "name" attribute of this ListOfMembers.
 */
int
ListOfMembers::unsetName()
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
 * Get a Member from the ListOfMembers.
 */
Member*
ListOfMembers::get(unsigned int n)
{
  return static_cast<Member*>(ListOf::get(n));
}


/*
 * Get a Member from the ListOfMembers.
 */
const Member*
ListOfMembers::get(unsigned int n) const
{
  return static_cast<const Member*>(ListOf::get(n));
}


/*
 * Get a Member from the ListOfMembers based on its identifier.
 */
Member*
ListOfMembers::get(const std::string& sid)
{
  return const_cast<Member*>(static_cast<const
    ListOfMembers&>(*this).get(sid));
}


/*
 * Get a Member from the ListOfMembers based on its identifier.
 */
const Member*
ListOfMembers::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Member>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Member*> (*result);
}


/*
 * Removes the nth Member from this ListOfMembers and returns a pointer to it.
 */
Member*
ListOfMembers::remove(unsigned int n)
{
  return static_cast<Member*>(ListOf::remove(n));
}


/*
 * Removes the Member from this ListOfMembers based on its identifier and
 * returns a pointer to it.
 */
Member*
ListOfMembers::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Member>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Member*> (item);
}


/*
 * Adds a copy of the given Member to this ListOfMembers.
 */
int
ListOfMembers::addMember(const Member* m)
{
  if (m == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (m->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != m->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != m->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(m)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(m);
  }
}


/*
 * Get the number of Member objects in this ListOfMembers.
 */
unsigned int
ListOfMembers::getNumMembers() const
{
  return size();
}


/*
 * Creates a new Member object, adds it to this ListOfMembers object and
 * returns the Member object created.
 */
Member*
ListOfMembers::createMember()
{
  Member* m = NULL;

  try
  {
    GROUPS_CREATE_NS(groupsns, getSBMLNamespaces());
    m = new Member(groupsns);
    delete groupsns;
  }
  catch (...)
  {
  }

  if (m != NULL)
  {
    appendAndOwn(m);
  }

  return m;
}


/*
 * Used by ListOfMembers::get() to lookup a Member based on its IdRef.
 */
struct IdEqIR
{
  const string& id;
   
  IdEqIR (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<Member*>(sb)->getIdRef() == id);
  }
};


/*
 * Get a Member from the ListOfMembers based on the element to which it refers.
 */
const Member*
ListOfMembers::getByIdRef(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqIR(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Member*> (*result);
}


/*
 * Get a Member from the ListOfMembers based on the element to which it refers.
 */
Member*
ListOfMembers::getByIdRef(const std::string& sid)
{
  return const_cast<Member*>(static_cast<const
    ListOfMembers&>(*this).getByIdRef(sid));
}


/*
 * Returns the XML element name of this ListOfMembers object.
 */
const std::string&
ListOfMembers::getElementName() const
{
  static const string name = "listOfMembers";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfMembers object.
 */
int
ListOfMembers::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfMembers object.
 */
int
ListOfMembers::getItemTypeCode() const
{
  return SBML_GROUPS_MEMBER;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfMembers object have been set.
 */
bool
ListOfMembers::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Member in this ListOfMembers
 */
SBase*
ListOfMembers::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  GROUPS_CREATE_NS(groupsns, getSBMLNamespaces());

  if (name == "member")
  {
    object = new Member(groupsns);
    appendAndOwn(object);
  }

  delete groupsns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ListOfMembers::addExpectedAttributes(ExpectedAttributes& attributes)
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
ListOfMembers::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
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
        log->logPackageError("groups", GroupsGroupLOMembersAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("groups",
          GroupsGroupLOMembersAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
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
      logEmptyString(mId, level, version, "<ListOfMembers>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("groups", GroupsIdSyntaxRule, pkgVersion, level,
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
      logEmptyString(mName, level, version, "<ListOfMembers>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfMembers::writeAttributes(XMLOutputStream& stream) const
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
 * Writes the namespace for the Groups package
 */
void
ListOfMembers::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(GroupsExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(GroupsExtension::getXmlnsL3V1V1(), prefix);
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
ListOfMembers_getId(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfMembers*>(lo)->getId().empty() ? NULL :
    safe_strdup(static_cast<const ListOfMembers*>(lo)->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
char *
ListOfMembers_getName(const ListOf_t * lo)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast<const ListOfMembers*>(lo)->getName().empty() ? NULL :
    safe_strdup(static_cast<const ListOfMembers*>(lo)->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfMembers_isSetId(const ListOf_t * lo)
{
  return (static_cast<const ListOfMembers*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfMembers*>(lo)->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
ListOfMembers_isSetName(const ListOf_t * lo)
{
  return (static_cast<const ListOfMembers*>(lo) != NULL) ?
    static_cast<int>(static_cast<const ListOfMembers*>(lo)->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfMembers_setId(ListOf_t * lo, const char * id)
{
  return (static_cast<ListOfMembers*>(lo) != NULL) ?
    static_cast<ListOfMembers*>(lo)->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfMembers_setName(ListOf_t * lo, const char * name)
{
  return (static_cast<ListOfMembers*>(lo) != NULL) ?
    static_cast<ListOfMembers*>(lo)->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfMembers_unsetId(ListOf_t * lo)
{
  return (static_cast<ListOfMembers*>(lo) != NULL) ?
    static_cast<ListOfMembers*>(lo)->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfMembers_unsetName(ListOf_t * lo)
{
  return (static_cast<ListOfMembers*>(lo) != NULL) ?
    static_cast<ListOfMembers*>(lo)->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Get a Member_t from the ListOf_t.
 */
LIBSBML_EXTERN
Member_t*
ListOfMembers_getMember(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfMembers*>(lo)->get(n);
}


/*
 * Get a Member_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Member_t*
ListOfMembers_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfMembers*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Member_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Member_t*
ListOfMembers_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfMembers*>(lo)->remove(n);
}


/*
 * Removes the Member_t from this ListOf_t based on its identifier and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
Member_t*
ListOfMembers_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfMembers*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


