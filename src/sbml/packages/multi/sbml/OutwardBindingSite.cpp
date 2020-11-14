/**
 * @file:   OutwardBindingSite.cpp
 * @brief:  Implementation of the OutwardBindingSite class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/multi/sbml/OutwardBindingSite.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new OutwardBindingSite with the given level, version, and package version.
 */
OutwardBindingSite::OutwardBindingSite (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mBindingStatus (MULTI_BINDING_STATUS_UNKNOWN)
   ,mComponent ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new OutwardBindingSite with the given MultiPkgNamespaces object.
 */
OutwardBindingSite::OutwardBindingSite (MultiPkgNamespaces* multins)
  : SBase(multins)
   ,mBindingStatus (MULTI_BINDING_STATUS_UNKNOWN)
   ,mComponent ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for OutwardBindingSite.
 */
OutwardBindingSite::OutwardBindingSite (const OutwardBindingSite& orig)
  : SBase(orig)
  , mBindingStatus  ( orig.mBindingStatus)
  , mComponent  ( orig.mComponent)
{
}


/*
 * Assignment for OutwardBindingSite.
 */
OutwardBindingSite&
OutwardBindingSite::operator=(const OutwardBindingSite& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mBindingStatus  = rhs.mBindingStatus;
    mComponent  = rhs.mComponent;
  }
  return *this;
}


/*
 * Clone for OutwardBindingSite.
 */
OutwardBindingSite*
OutwardBindingSite::clone () const
{
  return new OutwardBindingSite(*this);
}


/*
 * Destructor for OutwardBindingSite.
 */
OutwardBindingSite::~OutwardBindingSite ()
{
}


/*
 * Returns the value of the "id" attribute of this OutwardBindingSite.
 */
const std::string&
OutwardBindingSite::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this OutwardBindingSite.
 */
const std::string&
OutwardBindingSite::getName() const
{
  return mName;
}


/*
 * Returns the value of the "bindingStatus" attribute of this OutwardBindingSite.
 */
BindingStatus_t
OutwardBindingSite::getBindingStatus() const
{
  return mBindingStatus;
}


/*
 * Returns the value of the "component" attribute of this OutwardBindingSite.
 */
const std::string&
OutwardBindingSite::getComponent() const
{
  return mComponent;
}


/*
 * Returns true/false if id is set.
 */
bool
OutwardBindingSite::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
OutwardBindingSite::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if bindingStatus is set.
 */
bool
OutwardBindingSite::isSetBindingStatus() const
{
  return (mBindingStatus != MULTI_BINDING_STATUS_UNKNOWN);
}


/*
 * Returns true/false if component is set.
 */
bool
OutwardBindingSite::isSetComponent() const
{
  return (mComponent.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
OutwardBindingSite::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
OutwardBindingSite::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets bindingStatus and returns value indicating success.
 */
int
OutwardBindingSite::setBindingStatus(BindingStatus_t bindingStatus)
{
  if (OutwardBindingSite_isValidBindingStatus(bindingStatus) == 0)
  {
    mBindingStatus = MULTI_BINDING_STATUS_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mBindingStatus = bindingStatus;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "bindingStatus" attribute of this OutwardBindingSite.
 */
int
OutwardBindingSite::setBindingStatus(const std::string& bindingStatus)
{
  if (OutwardBindingSite_isValidBindingStatusString(bindingStatus.c_str()) == 0)
  //{
  //  mBindingStatus = MULTI_BINDING_STATUS_UNKNOWN;
  //  return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  //}
  //else
  //{
  //  mBindingStatus = BindingStatus_fromString(bindingStatus.c_str());
  //  return LIBSBML_OPERATION_SUCCESS;
  //}
  //bgoli22
  mBindingStatus = BindingStatus_fromString(bindingStatus.c_str());
  if (mBindingStatus == MULTI_BINDING_STATUS_UNKNOWN)
  {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets component and returns value indicating success.
 */
int
OutwardBindingSite::setComponent(const std::string& component)
{
  if (!(SyntaxChecker::isValidInternalSId(component)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mComponent = component;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
OutwardBindingSite::unsetId()
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
 * Unsets name and returns value indicating success.
 */
int
OutwardBindingSite::unsetName()
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
 * Unsets bindingStatus and returns value indicating success.
 */
int
OutwardBindingSite::unsetBindingStatus()
{
  mBindingStatus = MULTI_BINDING_STATUS_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets component and returns value indicating success.
 */
int
OutwardBindingSite::unsetComponent()
{
  mComponent.erase();

  if (mComponent.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
OutwardBindingSite::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetComponent() == true && mComponent == oldid)
  {
    setComponent(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
OutwardBindingSite::getElementName () const
{
  static const string name = "outwardBindingSite";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
OutwardBindingSite::getTypeCode () const
{
  return SBML_MULTI_OUTWARD_BINDING_SITE;
}


/*
 * check if all the required attributes are set
 */
bool
OutwardBindingSite::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetBindingStatus() == false)
    allPresent = false;

  if (isSetComponent() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
OutwardBindingSite::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
OutwardBindingSite::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
OutwardBindingSite::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
OutwardBindingSite::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
OutwardBindingSite::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("bindingStatus");
  attributes.add("component");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
OutwardBindingSite::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfOutwardBindingSites - which will have
   * happened immediately prior to this read
  */

  ListOfOutwardBindingSites * parentListOf =
      static_cast<ListOfOutwardBindingSites*>(getParentSBMLObject());

  if (getErrorLog() != NULL && parentListOf->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiLofOutBsts_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofOutBsts_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiOutBst_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiOutBst_AllowedCoreAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<OutwardBindingSite>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute id='" + mId + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }

  //
  // name string   ( use = "optional" )
  //
  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    // check string is not empty

    if (mName.empty() == true)
    {
      logEmptyString(mName, getLevel(), getVersion(), "<OutwardBindingSite>");
    }
  }


  //
  // component SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("component", mComponent);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mComponent.empty() == true)
    {
      logEmptyString(mComponent, getLevel(), getVersion(), "<OutwardBindingSite>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComponent) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute component='" + mComponent + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'component' is missing.";
    getErrorLog()->logPackageError("multi", MultiOutBst_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message,
                   getLine(), getColumn());
  }

  //
  // bindingStatus string   ( use = "optional" )
  //
  std::string bindingStatus;
  assigned = attributes.readInto("bindingStatus", bindingStatus, 
                                   getErrorLog(), false);

  if (assigned == true)
  {
    // check string is not empty

    if (bindingStatus.empty() == true)
    {
      logEmptyString(bindingStatus, getLevel(), getVersion(), 
                                    "<OutwardBindingSite>");
    }
    else 
    {
       mBindingStatus = BindingStatus_fromString( bindingStatus.c_str() );
       if (OutwardBindingSite_isValidBindingStatus(mBindingStatus) == 0)
       {
          getErrorLog()->logPackageError("multi", MultiOutBst_BdgStaAtt_Ref,
                       getPackageVersion(), sbmlLevel, sbmlVersion, "",
                       getLine(), getColumn());
       }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
OutwardBindingSite::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetBindingStatus() == true)
    stream.writeAttribute("bindingStatus", getPrefix(), 
    BindingStatus_toString(mBindingStatus));

  if (isSetComponent() == true)
    stream.writeAttribute("component", getPrefix(), mComponent);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfOutwardBindingSites::ListOfOutwardBindingSites(unsigned int level, 
                            unsigned int version, 
                            unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfOutwardBindingSites::ListOfOutwardBindingSites(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfOutwardBindingSites 
 */
ListOfOutwardBindingSites* 
ListOfOutwardBindingSites::clone () const
 {
  return new ListOfOutwardBindingSites(*this);
}


/*
 * Get a OutwardBindingSite from the ListOfOutwardBindingSites by index.
 */
OutwardBindingSite*
ListOfOutwardBindingSites::get(unsigned int n)
{
  return static_cast<OutwardBindingSite*>(ListOf::get(n));
}


/*
 * Get a OutwardBindingSite from the ListOfOutwardBindingSites by index.
 */
const OutwardBindingSite*
ListOfOutwardBindingSites::get(unsigned int n) const
{
  return static_cast<const OutwardBindingSite*>(ListOf::get(n));
}


/*
 * Get a OutwardBindingSite from the ListOfOutwardBindingSites by id.
 */
OutwardBindingSite*
ListOfOutwardBindingSites::get(const std::string& sid)
{
  return const_cast<OutwardBindingSite*>(
    static_cast<const ListOfOutwardBindingSites&>(*this).get(sid));
}


/*
 * Get a OutwardBindingSite from the ListOfOutwardBindingSites by id.
 */
const OutwardBindingSite*
ListOfOutwardBindingSites::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<OutwardBindingSite>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <OutwardBindingSite*> (*result);
}


/*
 * Removes the nth OutwardBindingSite from this ListOfOutwardBindingSites
 */
OutwardBindingSite*
ListOfOutwardBindingSites::remove(unsigned int n)
{
  return static_cast<OutwardBindingSite*>(ListOf::remove(n));
}


/*
 * Removes the OutwardBindingSite from this ListOfOutwardBindingSites with the given identifier
 */
OutwardBindingSite*
ListOfOutwardBindingSites::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<OutwardBindingSite>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <OutwardBindingSite*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfOutwardBindingSites::getElementName () const
{
  static const string name = "listOfOutwardBindingSites";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfOutwardBindingSites::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfOutwardBindingSites::getItemTypeCode () const
{
  return SBML_MULTI_OUTWARD_BINDING_SITE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new OutwardBindingSite in this ListOfOutwardBindingSites
 */
SBase*
ListOfOutwardBindingSites::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "outwardBindingSite")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new OutwardBindingSite(multins);
    appendAndOwn(object);
    delete multins;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Multi package.
 */
void
ListOfOutwardBindingSites::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(MultiExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(MultiExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */


LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion)
{
  return new OutwardBindingSite(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
OutwardBindingSite_free(OutwardBindingSite_t * obs)
{
  if (obs != NULL)
    delete obs;
}


LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_clone(OutwardBindingSite_t * obs)
{
  if (obs != NULL)
  {
    return static_cast<OutwardBindingSite_t*>(obs->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
OutwardBindingSite_getId(OutwardBindingSite_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getId().empty() ? NULL : safe_strdup(cr->getId().c_str());
}


LIBSBML_EXTERN
char *
OutwardBindingSite_getName(OutwardBindingSite_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getName().empty() ? NULL : safe_strdup(cr->getName().c_str());
}


LIBSBML_EXTERN
BindingStatus_t
OutwardBindingSite_getBindingStatus(OutwardBindingSite_t * obs)
{
  if (obs == NULL)
    return MULTI_BINDING_STATUS_UNKNOWN;

  return obs->getBindingStatus();
}


/*
 * Returns the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 */
LIBSBML_EXTERN
const char *
OutwardBindingSite_getBindingStatusAsString(const OutwardBindingSite_t * obs)
{
  return BindingStatus_toString(obs->getBindingStatus());
}


LIBSBML_EXTERN
char *
OutwardBindingSite_getComponent(OutwardBindingSite_t * obs)
{
  if (obs == NULL)
    return NULL;

  return obs->getComponent().empty() ? NULL : safe_strdup(obs->getComponent().c_str());
}


LIBSBML_EXTERN
int
OutwardBindingSite_isSetId(OutwardBindingSite_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetId()) : 0;
}


LIBSBML_EXTERN
int
OutwardBindingSite_isSetName(OutwardBindingSite_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
OutwardBindingSite_isSetBindingStatus(OutwardBindingSite_t * obs)
{
  return (obs != NULL) ? static_cast<int>(obs->isSetBindingStatus()) : 0;
}


LIBSBML_EXTERN
int
OutwardBindingSite_setBindingStatusAsString(OutwardBindingSite_t * obs,
  const char * bindingStatus)
{
  return (obs != NULL) ? obs->setBindingStatus(bindingStatus) :
    LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_isSetComponent(OutwardBindingSite_t * obs)
{
  return (obs != NULL) ? static_cast<int>(obs->isSetComponent()) : 0;
}


LIBSBML_EXTERN
int
OutwardBindingSite_setId(OutwardBindingSite_t * cr, const char * id)
{
  return (cr != NULL) ? cr->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_setName(OutwardBindingSite_t * cr, const char * name)
{
  return (cr != NULL) ? cr->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_setBindingStatus(OutwardBindingSite_t * obs, BindingStatus_t bindingStatus)
{
  return (obs != NULL) ? obs->setBindingStatus(bindingStatus) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_setComponent(OutwardBindingSite_t * obs, const char * component)
{
  return (obs != NULL) ? obs->setComponent(component) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_unsetId(OutwardBindingSite_t * cr)
{
  return (cr != NULL) ? cr->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_unsetName(OutwardBindingSite_t * cr)
{
  return (cr != NULL) ? cr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_unsetBindingStatus(OutwardBindingSite_t * obs)
{
  return (obs != NULL) ? obs->unsetBindingStatus() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_unsetComponent(OutwardBindingSite_t * obs)
{
  return (obs != NULL) ? obs->unsetComponent() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OutwardBindingSite_hasRequiredAttributes(OutwardBindingSite_t * obs)
{
  return (obs != NULL) ? static_cast<int>(obs->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfOutwardBindingSites *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfOutwardBindingSites *>(lo)->remove(sid) : NULL;
}

static
const char* BINDING_STATUS_STRINGS[] =
{
    "bound"
  , "unbound"
  , "either"
  , "unknown"
};


LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatus(BindingStatus_t bindingStatus)
{
  int max = MULTI_BINDING_STATUS_UNKNOWN;

  if (bindingStatus < MULTI_BINDING_STATUS_BOUND || bindingStatus >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}

LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatusString(const char* s)
{
  return OutwardBindingSite_isValidBindingStatus(BindingStatus_fromString(s));
}




LIBSBML_EXTERN
const char* 
BindingStatus_toString(BindingStatus_t bindingStatus)
{
  int max = MULTI_BINDING_STATUS_UNKNOWN;

  if (bindingStatus < MULTI_BINDING_STATUS_BOUND || bindingStatus >= max)
  {
    return NULL;
  }

  return BINDING_STATUS_STRINGS[bindingStatus];
}


LIBSBML_EXTERN
BindingStatus_t 
BindingStatus_fromString(const char* s)
{
  if (s == NULL) 
  {
    return MULTI_BINDING_STATUS_UNKNOWN;
  }

  int max = MULTI_BINDING_STATUS_UNKNOWN;
  for (int i = 0; i < max; i++)
  {
    if (strcmp(BINDING_STATUS_STRINGS[i], s) == 0)
    {
      return (BindingStatus_t)i;
    }
  }
  return MULTI_BINDING_STATUS_UNKNOWN;
}


LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


