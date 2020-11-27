/**
 * @file Group.cpp
 * @brief Implementation of the Group class.
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
#include <sbml/packages/groups/sbml/Group.h>
#include <sbml/packages/groups/sbml/ListOfGroups.h>
#include <sbml/packages/groups/validator/GroupsSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Group using the given SBML Level, Version and
 * &ldquo;groups&rdquo; package version.
 */
Group::Group(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
  : SBase(level, version)
  , mKind (GROUP_KIND_UNKNOWN)
  , mMembers (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new GroupsPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Group using the given GroupsPkgNamespaces object.
 */
Group::Group(GroupsPkgNamespaces *groupsns)
  : SBase(groupsns)
  , mKind (GROUP_KIND_UNKNOWN)
  , mMembers (groupsns)
{
  setElementNamespace(groupsns->getURI());
  connectToChild();
  loadPlugins(groupsns);
}


/*
 * Copy constructor for Group.
 */
Group::Group(const Group& orig)
  : SBase( orig )
  , mKind ( orig.mKind )
  , mMembers ( orig.mMembers )
{
  connectToChild();
}


/*
 * Assignment operator for Group.
 */
Group&
Group::operator=(const Group& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mKind = rhs.mKind;
    mMembers = rhs.mMembers;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Group object.
 */
Group*
Group::clone() const
{
  return new Group(*this);
}


/*
 * Destructor for Group.
 */
Group::~Group()
{
}


/*
 * Returns the value of the "id" attribute of this Group.
 */
const std::string&
Group::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Group.
 */
const std::string&
Group::getName() const
{
  return mName;
}


/*
 * Returns the value of the "kind" attribute of this Group.
 */
GroupKind_t
Group::getKind() const
{
  return mKind;
}


/*
 * Returns the value of the "kind" attribute of this Group.
 */
std::string
Group::getKindAsString() const
{
  std::string code_str = GroupKind_toString(mKind);
  return code_str;
}


/*
 * Predicate returning @c true if this Group's "id" attribute is set.
 */
bool
Group::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Group's "name" attribute is set.
 */
bool
Group::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this Group's "kind" attribute is set.
 */
bool
Group::isSetKind() const
{
  return (mKind != GROUP_KIND_UNKNOWN);
}


/*
 * Sets the value of the "id" attribute of this Group.
 */
int
Group::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this Group.
 */
int
Group::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "kind" attribute of this Group.
 */
int
Group::setKind(const GroupKind_t kind)
{
  if (GroupKind_isValid(kind) == 0)
  {
    mKind = GROUP_KIND_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mKind = kind;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "kind" attribute of this Group.
 */
int
Group::setKind(const std::string& kind)
{
  mKind = GroupKind_fromString(kind.c_str());

  if (mKind == GROUP_KIND_UNKNOWN)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this Group.
 */
int
Group::unsetId()
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
 * Unsets the value of the "name" attribute of this Group.
 */
int
Group::unsetName()
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
 * Unsets the value of the "kind" attribute of this Group.
 */
int
Group::unsetKind()
{
  mKind = GROUP_KIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfMembers from this Group.
 */
const ListOfMembers*
Group::getListOfMembers() const
{
  return &mMembers;
}


/*
 * Returns the ListOfMembers from this Group.
 */
ListOfMembers*
Group::getListOfMembers()
{
  return &mMembers;
}


/*
 * Get a Member from the Group.
 */
Member*
Group::getMember(unsigned int n)
{
  return mMembers.get(n);
}


/*
 * Get a Member from the Group.
 */
const Member*
Group::getMember(unsigned int n) const
{
  return mMembers.get(n);
}


/*
 * Get a Member from the Group based on its identifier.
 */
Member*
Group::getMember(const std::string& sid)
{
  return mMembers.get(sid);
}


/*
 * Get a Member from the Group based on its identifier.
 */
const Member*
Group::getMember(const std::string& sid) const
{
  return mMembers.get(sid);
}


/*
 * Get a Member from the Group based on the element to which it refers.
 */
const Member*
Group::getMemberByIdRef(const std::string& sid) const
{
  return mMembers.getByIdRef(sid);
}


/*
 * Get a Member from the Group based on the element to which it refers.
 */
Member*
Group::getMemberByIdRef(const std::string& sid)
{
  return mMembers.getByIdRef(sid);
}


/*
 * Adds a copy of the given Member to this Group.
 */
int
Group::addMember(const Member* m)
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
  else if (m->isSetId() && (mMembers.get(m->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mMembers.append(m);
  }
}


/*
 * Get the number of Member objects in this Group.
 */
unsigned int
Group::getNumMembers() const
{
  return mMembers.size();
}


/*
 * Creates a new Member object, adds it to this Group object and returns the
 * Member object created.
 */
Member*
Group::createMember()
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
    mMembers.appendAndOwn(m);
  }

  return m;
}


/*
 * Removes the nth Member from this Group and returns a pointer to it.
 */
Member*
Group::removeMember(unsigned int n)
{
  return mMembers.remove(n);
}


/*
 * Removes the Member from this Group based on its identifier and returns a
 * pointer to it.
 */
Member*
Group::removeMember(const std::string& sid)
{
  return mMembers.remove(sid);
}


/*
 * Returns the XML element name of this Group object.
 */
const std::string&
Group::getElementName() const
{
  static const string name = "group";
  return name;
}


/*
 * Returns the libSBML type code for this Group object.
 */
int
Group::getTypeCode() const
{
  return SBML_GROUPS_GROUP;
}


/*
 * Predicate returning @c true if all the required attributes for this Group
 * object have been set.
 */
bool
Group::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetKind() == false)
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
Group::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumMembers() > 0)
  {
    mMembers.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Group::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mMembers.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Group::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mMembers.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Group::connectToChild()
{
  SBase::connectToChild();

  mMembers.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Group::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix,
                             bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mMembers.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Group::updateSBMLNamespace(const std::string& package,
                           unsigned int level,
                           unsigned int version)
{
  SBase::updateSBMLNamespace(package, level, version);

  mMembers.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Group.
 */
int
Group::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Group.
 */
int
Group::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Group.
 */
int
Group::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Group.
 */
int
Group::getAttribute(const std::string& attributeName,
                    unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Group.
 */
int
Group::getAttribute(const std::string& attributeName,
                    std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

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
  else if (attributeName == "kind")
  {
    value = getKindAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Group's attribute "attributeName" is
 * set.
 */
bool
Group::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "kind")
  {
    value = isSetKind();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Group.
 */
int
Group::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Group.
 */
int
Group::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Group.
 */
int
Group::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Group.
 */
int
Group::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Group.
 */
int
Group::setAttribute(const std::string& attributeName,
                    const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "kind")
  {
    return_value = setKind(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Group.
 */
int
Group::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "kind")
  {
    value = unsetKind();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Group.
 */
SBase*
Group::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "member")
  {
    return createMember();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Group.
 */
int
Group::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "member" && element->getTypeCode() == SBML_GROUPS_MEMBER)
  {
    return addMember((const Member*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Group.
 */
SBase*
Group::removeChildObject(const std::string& elementName,
                         const std::string& id)
{
  if (elementName == "member")
  {
    return removeMember(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Group.
 */
unsigned int
Group::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "member")
  {
    return getNumMembers();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Group.
 */
SBase*
Group::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "member")
  {
    return getMember(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Group::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMembers.getId() == id)
  {
    return &mMembers;
  }

  obj = mMembers.getElementBySId(id);

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
Group::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMembers.getMetaId() == metaid)
  {
    return &mMembers;
  }

  obj = mMembers.getElementByMetaId(metaid);

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
Group::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mMembers, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Group::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  if (name == "listOfMembers")
  {
    if (mMembers.size() != 0)
    {
      getErrorLog()->logPackageError("groups", GroupsGroupAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), 
          getColumn());
    }

    obj = &mMembers;
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
Group::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("kind");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Group::readAttributes(const XMLAttributes& attributes,
                      const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGroups*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("groups", GroupsGroupAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("groups",
          GroupsModelLOGroupsAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("groups", GroupsGroupAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("groups", GroupsGroupAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
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
      logEmptyString(mId, level, version, "<Group>");
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
      logEmptyString(mName, level, version, "<Group>");
    }
  }

  // 
  // kind enum (use = "required" )
  // 

  std::string kind;
  assigned = attributes.readInto("kind", kind);

  if (assigned == true)
  {
    if (kind.empty() == true)
    {
      logEmptyString(kind, level, version, "<Group>");
    }
    else
    {
      mKind = GroupKind_fromString(kind.c_str());

      if (GroupKind_isValid(mKind) == 0)
      {
        std::string msg = "The kind on the <Group> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + kind + "', which is not a valid option.";

        log->logPackageError("groups", GroupsGroupKindMustBeGroupKindEnum,
          pkgVersion, level, version, msg);
      }
    }
  }
  else
  {
    std::string message = "Groups attribute 'kind' is missing.";
    log->logPackageError("groups", GroupsGroupAllowedAttributes, pkgVersion,
      level, version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Group::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetKind() == true)
  {
    stream.writeAttribute("kind", getPrefix(), GroupKind_toString(mKind));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Group_t using the given SBML Level, Version and
 * &ldquo;groups&rdquo; package version.
 */
LIBSBML_EXTERN
Group_t *
Group_create(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
{
  return new Group(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Group_t object.
 */
LIBSBML_EXTERN
Group_t*
Group_clone(const Group_t* g)
{
  if (g != NULL)
  {
    return static_cast<Group_t*>(g->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Group_t object.
 */
LIBSBML_EXTERN
void
Group_free(Group_t* g)
{
  if (g != NULL)
  {
    delete g;
  }
}


/*
 * Returns the value of the "id" attribute of this Group_t.
 */
LIBSBML_EXTERN
char *
Group_getId(const Group_t * g)
{
  if (g == NULL)
  {
    return NULL;
  }

  return g->getId().empty() ? NULL : safe_strdup(g->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this Group_t.
 */
LIBSBML_EXTERN
char *
Group_getName(const Group_t * g)
{
  if (g == NULL)
  {
    return NULL;
  }

  return g->getName().empty() ? NULL : safe_strdup(g->getName().c_str());
}


/*
 * Returns the value of the "kind" attribute of this Group_t.
 */
LIBSBML_EXTERN
GroupKind_t
Group_getKind(const Group_t * g)
{
  if (g == NULL)
  {
    return GROUP_KIND_UNKNOWN;
  }

  return g->getKind();
}


/*
 * Returns the value of the "kind" attribute of this Group_t.
 */
LIBSBML_EXTERN
char *
Group_getKindAsString(const Group_t * g)
{
  return (char*)(GroupKind_toString(g->getKind()));
}


/*
 * Predicate returning @c 1 (true) if this Group_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Group_isSetId(const Group_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Group_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
Group_isSetName(const Group_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Group_t's "kind" attribute is set.
 */
LIBSBML_EXTERN
int
Group_isSetKind(const Group_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetKind()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_setId(Group_t * g, const char * id)
{
  return (g != NULL) ? g->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_setName(Group_t * g, const char * name)
{
  return (g != NULL) ? g->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "kind" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_setKind(Group_t * g, GroupKind_t kind)
{
  return (g != NULL) ? g->setKind(kind) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "kind" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_setKindAsString(Group_t * g, const char * kind)
{
  return (g != NULL) ? g->setKind(kind): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_unsetId(Group_t * g)
{
  return (g != NULL) ? g->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_unsetName(Group_t * g)
{
  return (g != NULL) ? g->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "kind" attribute of this Group_t.
 */
LIBSBML_EXTERN
int
Group_unsetKind(Group_t * g)
{
  return (g != NULL) ? g->unsetKind() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing Member_t objects from this Group_t.
 */
LIBSBML_EXTERN
ListOf_t*
Group_getListOfMembers(Group_t* g)
{
  return (g != NULL) ? g->getListOfMembers() : NULL;
}


/*
 * Get a Member_t from the Group_t.
 */
LIBSBML_EXTERN
Member_t*
Group_getMember(Group_t* g, unsigned int n)
{
  return (g != NULL) ? g->getMember(n) : NULL;
}


/*
 * Get a Member_t from the Group_t based on its identifier.
 */
LIBSBML_EXTERN
Member_t*
Group_getMemberById(Group_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getMember(sid) : NULL;
}


/*
 * Get a Member_t from the Group_t based on the element to which it refers.
 */
LIBSBML_EXTERN
Member_t*
Group_getMemberByIdRef(Group_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getMemberByIdRef(sid) : NULL;
}


/*
 * Adds a copy of the given Member_t to this Group_t.
 */
LIBSBML_EXTERN
int
Group_addMember(Group_t* g, const Member_t* m)
{
  return (g != NULL) ? g->addMember(m) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Member_t objects in this Group_t.
 */
LIBSBML_EXTERN
unsigned int
Group_getNumMembers(Group_t* g)
{
  return (g != NULL) ? g->getNumMembers() : SBML_INT_MAX;
}


/*
 * Creates a new Member_t object, adds it to this Group_t object and returns
 * the Member_t object created.
 */
LIBSBML_EXTERN
Member_t*
Group_createMember(Group_t* g)
{
  return (g != NULL) ? g->createMember() : NULL;
}


/*
 * Removes the nth Member_t from this Group_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Member_t*
Group_removeMember(Group_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeMember(n) : NULL;
}


/*
 * Removes the Member_t from this Group_t based on its identifier and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Member_t*
Group_removeMemberById(Group_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeMember(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Group_t object have been set.
 */
LIBSBML_EXTERN
int
Group_hasRequiredAttributes(const Group_t * g)
{
  return (g != NULL) ? static_cast<int>(g->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


