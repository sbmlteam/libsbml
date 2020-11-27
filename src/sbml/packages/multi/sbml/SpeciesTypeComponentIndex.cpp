/**
 * @file:   SpeciesTypeComponentIndex.cpp
 * @brief:  Implementation of the SpeciesTypeComponentIndex class
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


#include <sbml/packages/multi/sbml/SpeciesTypeComponentIndex.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>

using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpeciesTypeComponentIndex with the given level, version, and package version.
 */
SpeciesTypeComponentIndex::SpeciesTypeComponentIndex (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
   ,mComponent ("")
////   ,mIdentifyingParent ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpeciesTypeComponentIndex with the given MultiPkgNamespaces object.
 */
SpeciesTypeComponentIndex::SpeciesTypeComponentIndex (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
   ,mComponent ("")
////   ,mIdentifyingParent ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for SpeciesTypeComponentIndex.
 */
SpeciesTypeComponentIndex::SpeciesTypeComponentIndex (const SpeciesTypeComponentIndex& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
  , mComponent  ( orig.mComponent)
//  , mIdentifyingParent  ( orig.mIdentifyingParent)
{
}


/*
 * Assignment for SpeciesTypeComponentIndex.
 */
SpeciesTypeComponentIndex&
SpeciesTypeComponentIndex::operator=(const SpeciesTypeComponentIndex& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mComponent  = rhs.mComponent;
    mIdentifyingParent  = rhs.mIdentifyingParent;
  }
  return *this;
}


/*
 * Clone for SpeciesTypeComponentIndex.
 */
SpeciesTypeComponentIndex*
SpeciesTypeComponentIndex::clone () const
{
  return new SpeciesTypeComponentIndex(*this);
}


/*
 * Destructor for SpeciesTypeComponentIndex.
 */
SpeciesTypeComponentIndex::~SpeciesTypeComponentIndex ()
{
}


/*
 * Returns the value of the "id" attribute of this SpeciesTypeComponentIndex.
 */
const std::string&
SpeciesTypeComponentIndex::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SpeciesTypeComponentIndex.
 */
const std::string&
SpeciesTypeComponentIndex::getName() const
{
  return mName;
}


/*
 * Returns the value of the "component" attribute of this SpeciesTypeComponentIndex.
 */
const std::string&
SpeciesTypeComponentIndex::getComponent() const
{
  return mComponent;
}


/*
 * Returns the value of the "identifyingParent" attribute of this SpeciesTypeComponentIndex.
 */
const std::string&
SpeciesTypeComponentIndex::getIdentifyingParent() const
{
  return mIdentifyingParent;
}


/*
 * Returns true/false if id is set.
 */
bool
SpeciesTypeComponentIndex::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SpeciesTypeComponentIndex::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if component is set.
 */
bool
SpeciesTypeComponentIndex::isSetComponent() const
{
  return (mComponent.empty() == false);
}


/*
 * Returns true/false if identifyingParent is set.
 */
bool
SpeciesTypeComponentIndex::isSetIdentifyingParent() const
{
  return (mIdentifyingParent.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets component and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::setComponent(const std::string& component)
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
 * Sets identifyingParent and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::setIdentifyingParent(const std::string& identifyingParent)
{
  if (!(SyntaxChecker::isValidInternalSId(identifyingParent)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mIdentifyingParent = identifyingParent;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::unsetId()
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
SpeciesTypeComponentIndex::unsetName()
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
 * Unsets component and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::unsetComponent()
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
 * Unsets identifyingParent and returns value indicating success.
 */
int
SpeciesTypeComponentIndex::unsetIdentifyingParent()
{
  mIdentifyingParent.erase();

  if (mIdentifyingParent.empty() == true)
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
SpeciesTypeComponentIndex::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetComponent() == true && mComponent == oldid)
  {
    setComponent(newid);
  }

  if (isSetIdentifyingParent() == true && mIdentifyingParent == oldid)
  {
    setIdentifyingParent(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpeciesTypeComponentIndex::getElementName () const
{
  static const string name = "speciesTypeComponentIndex";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpeciesTypeComponentIndex::getTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX;
}


/*
 * check if all the required attributes are set
 */
bool
SpeciesTypeComponentIndex::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
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
SpeciesTypeComponentIndex::writeElements (XMLOutputStream& stream) const
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
SpeciesTypeComponentIndex::accept (SBMLVisitor& v) const
{
  v.visit(*this);

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpeciesTypeComponentIndex::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpeciesTypeComponentIndex::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
SpeciesTypeComponentIndex::createObject(XMLInputStream& stream)
{
  return NULL;
  //const string& name = stream.peek().getName();
  //SBase* object = NULL;

  //MULTI_CREATE_NS(multins, getSBMLNamespaces());

  //delete multins;
  //return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
SpeciesTypeComponentIndex::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("component");
  attributes.add("identifyingParent");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpeciesTypeComponentIndex::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpeciesTypeComponentIndexes - which will have
   * happened immediately prior to this read
  */

  ListOfSpeciesTypeComponentIndexes * parentListOf =
      static_cast<ListOfSpeciesTypeComponentIndexes*>(getParentSBMLObject());

  if (getErrorLog() != NULL &&
      parentListOf->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSptCpoInds_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSptCpoInds_AllowedAtts,
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
        getErrorLog()->logPackageError("multi", MultiSptCpoInd_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSptCpoInd_AllowedCoreAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<SpeciesTypeComponentIndex>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute id='" + mId + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'id' is missing.";
    getErrorLog()->logPackageError("multi", MultiSptCpoInd_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message,
                   getLine(), getColumn());
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
       logEmptyString(mName, getLevel(), getVersion(), "<SpeciesTypeComponentIndex>");
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
      logEmptyString(mComponent, getLevel(), getVersion(), "<SpeciesTypeComponentIndex>");
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
    getErrorLog()->logPackageError("multi", MultiSptCpoInd_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message,
                   getLine(), getColumn());
  }

  //
  // identifyingParent SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("identifyingParent", mIdentifyingParent);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mIdentifyingParent.empty() == true)
    {
      logEmptyString(mIdentifyingParent, getLevel(), getVersion(), "<SpeciesTypeComponentIndex>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mIdentifyingParent) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute identifyingParent='" + mIdentifyingParent + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpeciesTypeComponentIndex::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetComponent() == true)
    stream.writeAttribute("component", getPrefix(), mComponent);

  if (isSetIdentifyingParent() == true)
    stream.writeAttribute("identifyingParent", getPrefix(), mIdentifyingParent);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfSpeciesTypeComponentIndexes::ListOfSpeciesTypeComponentIndexes(unsigned int level, 
                                   unsigned int version, 
                                   unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpeciesTypeComponentIndexes::ListOfSpeciesTypeComponentIndexes(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfSpeciesTypeComponentIndexes 
 */
ListOfSpeciesTypeComponentIndexes* 
ListOfSpeciesTypeComponentIndexes::clone () const
 {
  return new ListOfSpeciesTypeComponentIndexes(*this);
}


/*
 * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by index.
 */
SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::get(unsigned int n)
{
  return static_cast<SpeciesTypeComponentIndex*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by index.
 */
const SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::get(unsigned int n) const
{
  return static_cast<const SpeciesTypeComponentIndex*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by id.
 */
SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::get(const std::string& sid)
{
  return const_cast<SpeciesTypeComponentIndex*>(
    static_cast<const ListOfSpeciesTypeComponentIndexes&>(*this).get(sid));
}


/*
 * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by id.
 */
const SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeComponentIndex>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesTypeComponentIndex*> (*result);
}


/*
 * Removes the nth SpeciesTypeComponentIndex from this ListOfSpeciesTypeComponentIndexes
 */
SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::remove(unsigned int n)
{
  return static_cast<SpeciesTypeComponentIndex*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesTypeComponentIndex from this ListOfSpeciesTypeComponentIndexes with the given identifier
 */
SpeciesTypeComponentIndex*
ListOfSpeciesTypeComponentIndexes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeComponentIndex>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesTypeComponentIndex*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpeciesTypeComponentIndexes::getElementName () const
{
  static const string name = "listOfSpeciesTypeComponentIndexes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpeciesTypeComponentIndexes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpeciesTypeComponentIndexes::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes
 */
SBase*
ListOfSpeciesTypeComponentIndexes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesTypeComponentIndex")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesTypeComponentIndex(multins);
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
ListOfSpeciesTypeComponentIndexes::writeXMLNS(XMLOutputStream& stream) const
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
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_create(unsigned int level, unsigned int version,
                                 unsigned int pkgVersion)
{
  return new SpeciesTypeComponentIndex(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpeciesTypeComponentIndex_free(SpeciesTypeComponentIndex_t * stci)
{
  if (stci != NULL)
    delete stci;
}


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_clone(SpeciesTypeComponentIndex_t * stci)
{
  if (stci != NULL)
  {
    return static_cast<SpeciesTypeComponentIndex_t*>(stci->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getId(SpeciesTypeComponentIndex_t * stci)
{
  if (stci == NULL)
    return NULL;

  return stci->getId().empty() ? NULL : safe_strdup(stci->getId().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getName(SpeciesTypeComponentIndex_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getName().empty() ? NULL : safe_strdup(cr->getName().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getComponent(SpeciesTypeComponentIndex_t * stci)
{
  if (stci == NULL)
    return NULL;

  return stci->getComponent().empty() ? NULL : safe_strdup(stci->getComponent().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getIdentifyingParent(SpeciesTypeComponentIndex_t * stci)
{
  if (stci == NULL)
    return NULL;

  return stci->getIdentifyingParent().empty() ? NULL : safe_strdup(stci->getIdentifyingParent().c_str());
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetId(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? static_cast<int>(stci->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetName(SpeciesTypeComponentIndex_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetComponent(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? static_cast<int>(stci->isSetComponent()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetIdentifyingParent(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? static_cast<int>(stci->isSetIdentifyingParent()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setId(SpeciesTypeComponentIndex_t * stci, const char * id)
{
  return (stci != NULL) ? stci->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setName(SpeciesTypeComponentIndex_t * cr, const char * name)
{
  return (cr != NULL) ? cr->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setComponent(SpeciesTypeComponentIndex_t * stci, const char * component)
{
  return (stci != NULL) ? stci->setComponent(component) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setIdentifyingParent(SpeciesTypeComponentIndex_t * stci, const char * identifyingParent)
{
  return (stci != NULL) ? stci->setIdentifyingParent(identifyingParent) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetId(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? stci->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetName(SpeciesTypeComponentIndex_t * cr)
{
  return (cr != NULL) ? cr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetComponent(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? stci->unsetComponent() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetIdentifyingParent(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? stci->unsetIdentifyingParent() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_hasRequiredAttributes(SpeciesTypeComponentIndex_t * stci)
{
  return (stci != NULL) ? static_cast<int>(stci->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeComponentIndexes *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeComponentIndexes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


