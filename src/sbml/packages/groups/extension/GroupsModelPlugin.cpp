/**
 * @file GroupsModelPlugin.cpp
 * @brief Implementation of the GroupsModelPlugin class.
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
#include <sbml/packages/groups/extension/GroupsModelPlugin.h>
#include <sbml/packages/groups/validator/GroupsSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GroupsModelPlugin using the given URI, prefix and package
 * namespace.
 */
GroupsModelPlugin::GroupsModelPlugin(const std::string& uri,
                                     const std::string& prefix,
                                     GroupsPkgNamespaces* groupsns)
  : SBasePlugin(uri, prefix, groupsns)
  , mGroups (groupsns)
{
  connectToChild();
}


/*
 * Copy constructor for GroupsModelPlugin.
 */
GroupsModelPlugin::GroupsModelPlugin(const GroupsModelPlugin& orig)
  : SBasePlugin( orig )
  , mGroups ( orig.mGroups )
{
  connectToChild();
}


/*
 * Assignment operator for GroupsModelPlugin.
 */
GroupsModelPlugin&
GroupsModelPlugin::operator=(const GroupsModelPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    mGroups = rhs.mGroups;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GroupsModelPlugin object.
 */
GroupsModelPlugin*
GroupsModelPlugin::clone() const
{
  return new GroupsModelPlugin(*this);
}


/*
 * Destructor for GroupsModelPlugin.
 */
GroupsModelPlugin::~GroupsModelPlugin()
{
}


/*
 * Returns the ListOfGroups from this GroupsModelPlugin.
 */
const ListOfGroups*
GroupsModelPlugin::getListOfGroups() const
{
  return &mGroups;
}


/*
 * Returns the ListOfGroups from this GroupsModelPlugin.
 */
ListOfGroups*
GroupsModelPlugin::getListOfGroups()
{
  return &mGroups;
}


/*
 * Get a Group from the GroupsModelPlugin.
 */
Group*
GroupsModelPlugin::getGroup(unsigned int n)
{
  return static_cast< Group*>(mGroups.get(n));
}


/*
 * Get a Group from the GroupsModelPlugin.
 */
const Group*
GroupsModelPlugin::getGroup(unsigned int n) const
{
  return static_cast<const Group*>(mGroups.get(n));
}


/*
 * Get a Group from the GroupsModelPlugin based on its identifier.
 */
Group*
GroupsModelPlugin::getGroup(const std::string& sid)
{
  return static_cast< Group*>(mGroups.get(sid));
}


/*
 * Get a Group from the GroupsModelPlugin based on its identifier.
 */
const Group*
GroupsModelPlugin::getGroup(const std::string& sid) const
{
  return static_cast<const Group*>(mGroups.get(sid));
}


/*
 * Adds a copy of the given Group to this GroupsModelPlugin.
 */
int
GroupsModelPlugin::addGroup(const Group* g)
{
  if (g == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (g->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != g->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != g->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != g->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else if (g->isSetId() && (mGroups.get(g->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mGroups.append(g);
  }
}


/*
 * Get the number of Group objects in this GroupsModelPlugin.
 */
unsigned int
GroupsModelPlugin::getNumGroups() const
{
  return mGroups.size();
}


/*
 * Creates a new Group object, adds it to this GroupsModelPlugin object and
 * returns the Group object created.
 */
Group*
GroupsModelPlugin::createGroup()
{
  Group* g = NULL;

  try
  {
    GROUPS_CREATE_NS(groupsns, getSBMLNamespaces());
    g = new Group(groupsns);
    delete groupsns;
  }
  catch (...)
  {
  }

  if (g != NULL)
  {
    mGroups.appendAndOwn(g);
  }

  return g;
}


/*
 * Removes the nth Group from this GroupsModelPlugin and returns a pointer to
 * it.
 */
Group*
GroupsModelPlugin::removeGroup(unsigned int n)
{
  return static_cast<Group*>(mGroups.remove(n));
}


/*
 * Removes the Group from this GroupsModelPlugin based on its identifier and
 * returns a pointer to it.
 */
Group*
GroupsModelPlugin::removeGroup(const std::string& sid)
{
  return static_cast<Group*>(mGroups.remove(sid));
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
GroupsModelPlugin::writeElements(XMLOutputStream& stream) const
{
  if (getNumGroups() > 0)
  {
    mGroups.write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GroupsModelPlugin::accept(SBMLVisitor& v) const
{
  const Model* m = static_cast<const Model*>(this->getParentSBMLObject());
  v.visit(*m);
  v.leave(*m);

  mGroups.accept(v);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GroupsModelPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mGroups.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
GroupsModelPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
GroupsModelPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  mGroups.connectToParent(base);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GroupsModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                         const std::string& pkgPrefix,
                                         bool flag)
{
  mGroups.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
GroupsModelPlugin::updateSBMLNamespace(const std::string& package,
                                       unsigned int level,
                                       unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  mGroups.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GroupsModelPlugin's attribute
 * "attributeName" is set.
 */
bool
GroupsModelPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this GroupsModelPlugin.
 */
int
GroupsModelPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this GroupsModelPlugin.
 */
SBase*
GroupsModelPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "group")
  {
    return createGroup();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this GroupsModelPlugin.
 */
int
GroupsModelPlugin::addChildObject(const std::string& elementName,
                                  const SBase* element)
{
  if (elementName == "group" && element->getTypeCode() == SBML_GROUPS_GROUP)
  {
    return addGroup((const Group*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * GroupsModelPlugin.
 */
SBase*
GroupsModelPlugin::removeChildObject(const std::string& elementName,
                                     const std::string& id)
{
  if (elementName == "group")
  {
    return removeGroup(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this GroupsModelPlugin.
 */
unsigned int
GroupsModelPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "group")
  {
    return getNumGroups();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this GroupsModelPlugin.
 */
SBase*
GroupsModelPlugin::getObject(const std::string& elementName,
                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "group")
  {
    return getGroup(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
GroupsModelPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mGroups.getElementBySId(id);

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
GroupsModelPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGroups.getMetaId() == metaid)
  {
    return &mGroups;
  }

  obj = mGroups.getElementByMetaId(metaid);

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
GroupsModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mGroups, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
GroupsModelPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const GroupsModelPlugin* plug = static_cast<const
    GroupsModelPlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  ret = mGroups.appendFrom(plug->getListOfGroups());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  return ret;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
GroupsModelPlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if (name == "listOfGroups")
    {
      if (mGroups.size() != 0)
      {
        getErrorLog()->logPackageError("groups", GroupsModelAllowedElements,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), 
            getColumn());
      }

      obj = &mGroups;
      if (targetPrefix.empty())
      {
        mGroups.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
  }

  connectToChild();

  return obj;
}

/** @endcond */


void 
GroupsModelPlugin::copyInformationToNestedLists()
{
  bool change = true;
  while (change) 
  {
    change = false;
    
    for (unsigned int lnum = 0; lnum < getNumGroups(); lnum++) 
    {
      Group* group = getGroup(lnum);
      ListOf* listofmembers = group->getListOfMembers();
      
      for (unsigned int mnum = 0; mnum < group->getNumMembers(); mnum++) 
      {
        Member* member = group->getMember(mnum);
        SBase* referent = NULL;
        
        if (member->isSetIdRef()) 
        {
          referent = getElementBySId(member->getIdRef());
        }
        
        if (referent == NULL && member->isSetMetaIdRef()) 
        {
          referent = getElementByMetaId(member->getMetaIdRef());
        }
        
        if (referent != NULL && referent->getTypeCode() == SBML_LIST_OF) 
        {
          ListOf* list = static_cast<ListOf*>(referent);
          
          if (list->getItemTypeCode() == SBML_GROUPS_MEMBER) 
          {
            if (!referent->isSetSBOTerm()) 
            {
              if (listofmembers->isSetSBOTerm()) 
              {
                referent->setSBOTerm(listofmembers->getSBOTerm());
                change = true;
              }
            }
            
            if (!referent->isSetNotes()) 
            {
              if (listofmembers->isSetNotes()) 
              {
                referent->setNotes(listofmembers->getNotes());
                change = true;
              }
            }
            
            if (!referent->isSetAnnotation()) 
            {
              if (listofmembers->isSetAnnotation()) 
              {
                referent->setAnnotation(listofmembers->getAnnotation());
                change = true;
              }
            }
          }
        }
      }
    }
  }
}



#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing Group_t objects from this
 * GroupsModelPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
GroupsModelPlugin_getListOfGroups(GroupsModelPlugin_t* gmp)
{
  return (gmp != NULL) ? gmp->getListOfGroups() : NULL;
}


/*
 * Get a Group_t from the GroupsModelPlugin_t.
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_getGroup(GroupsModelPlugin_t* gmp, unsigned int n)
{
  return (gmp != NULL) ? gmp->getGroup(n) : NULL;
}


/*
 * Get a Group_t from the GroupsModelPlugin_t based on its identifier.
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_getGroupById(GroupsModelPlugin_t* gmp, const char *sid)
{
  return (gmp != NULL && sid != NULL) ? gmp->getGroup(sid) : NULL;
}


/*
 * Adds a copy of the given Group_t to this GroupsModelPlugin_t.
 */
LIBSBML_EXTERN
int
GroupsModelPlugin_addGroup(GroupsModelPlugin_t* gmp, const Group_t* g)
{
  return (gmp != NULL) ? gmp->addGroup(g) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Group_t objects in this GroupsModelPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
GroupsModelPlugin_getNumGroups(GroupsModelPlugin_t* gmp)
{
  return (gmp != NULL) ? gmp->getNumGroups() : SBML_INT_MAX;
}


/*
 * Creates a new Group_t object, adds it to this GroupsModelPlugin_t object and
 * returns the Group_t object created.
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_createGroup(GroupsModelPlugin_t* gmp)
{
  return (gmp != NULL) ? gmp->createGroup() : NULL;
}


/*
 * Removes the nth Group_t from this GroupsModelPlugin_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_removeGroup(GroupsModelPlugin_t* gmp, unsigned int n)
{
  return (gmp != NULL) ? gmp->removeGroup(n) : NULL;
}


/*
 * Removes the Group_t from this GroupsModelPlugin_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_removeGroupById(GroupsModelPlugin_t* gmp, const char* sid)
{
  return (gmp != NULL && sid != NULL) ? gmp->removeGroup(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


