/**
 * @file:   InSpeciesTypeBond.cpp
 * @brief:  Implementation of the InSpeciesTypeBond class
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


#include <sbml/packages/multi/sbml/InSpeciesTypeBond.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new InSpeciesTypeBond with the given level, version, and package version.
 */
InSpeciesTypeBond::InSpeciesTypeBond (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mBindingSite1 ("")
   ,mBindingSite2 ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new InSpeciesTypeBond with the given MultiPkgNamespaces object.
 */
InSpeciesTypeBond::InSpeciesTypeBond (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mBindingSite1 ("")
   ,mBindingSite2 ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for InSpeciesTypeBond.
 */
InSpeciesTypeBond::InSpeciesTypeBond (const InSpeciesTypeBond& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mBindingSite1  ( orig.mBindingSite1)
  , mBindingSite2  ( orig.mBindingSite2)
{
}


/*
 * Assignment for InSpeciesTypeBond.
 */
InSpeciesTypeBond&
InSpeciesTypeBond::operator=(const InSpeciesTypeBond& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mBindingSite1  = rhs.mBindingSite1;
    mBindingSite2  = rhs.mBindingSite2;
  }
  return *this;
}


/*
 * Clone for InSpeciesTypeBond.
 */
InSpeciesTypeBond*
InSpeciesTypeBond::clone () const
{
  return new InSpeciesTypeBond(*this);
}


/*
 * Destructor for InSpeciesTypeBond.
 */
InSpeciesTypeBond::~InSpeciesTypeBond ()
{
}


/*
 * Returns the value of the "id" attribute of this InSpeciesTypeBond.
 */
const std::string&
InSpeciesTypeBond::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this InSpeciesTypeBond.
 */
const std::string&
InSpeciesTypeBond::getName() const
{
  return mName;
}


/*
 * Returns the value of the "bindingSite1" attribute of this InSpeciesTypeBond.
 */
const std::string&
InSpeciesTypeBond::getBindingSite1() const
{
  return mBindingSite1;
}


/*
 * Returns the value of the "bindingSite2" attribute of this InSpeciesTypeBond.
 */
const std::string&
InSpeciesTypeBond::getBindingSite2() const
{
  return mBindingSite2;
}


/*
 * Returns true/false if id is set.
 */
bool
InSpeciesTypeBond::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
InSpeciesTypeBond::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if bindingSite1 is set.
 */
bool
InSpeciesTypeBond::isSetBindingSite1() const
{
  return (mBindingSite1.empty() == false);
}


/*
 * Returns true/false if bindingSite2 is set.
 */
bool
InSpeciesTypeBond::isSetBindingSite2() const
{
  return (mBindingSite2.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
InSpeciesTypeBond::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
InSpeciesTypeBond::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets bindingSite1 and returns value indicating success.
 */
int
InSpeciesTypeBond::setBindingSite1(const std::string& bindingSite1)
{
  if (!(SyntaxChecker::isValidInternalSId(bindingSite1)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mBindingSite1 = bindingSite1;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets bindingSite2 and returns value indicating success.
 */
int
InSpeciesTypeBond::setBindingSite2(const std::string& bindingSite2)
{
  if (!(SyntaxChecker::isValidInternalSId(bindingSite2)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mBindingSite2 = bindingSite2;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
InSpeciesTypeBond::unsetId()
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
InSpeciesTypeBond::unsetName()
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
 * Unsets bindingSite1 and returns value indicating success.
 */
int
InSpeciesTypeBond::unsetBindingSite1()
{
  mBindingSite1.erase();

  if (mBindingSite1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets bindingSite2 and returns value indicating success.
 */
int
InSpeciesTypeBond::unsetBindingSite2()
{
  mBindingSite2.erase();

  if (mBindingSite2.empty() == true)
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
InSpeciesTypeBond::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetBindingSite1() == true && mBindingSite1 == oldid)
  {
    setBindingSite1(newid);
  }

  if (isSetBindingSite2() == true && mBindingSite2 == oldid)
  {
    setBindingSite2(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
InSpeciesTypeBond::getElementName () const
{
  static const string name = "inSpeciesTypeBond";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
InSpeciesTypeBond::getTypeCode () const
{
  return SBML_MULTI_IN_SPECIES_TYPE_BOND;
}


/*
 * check if all the required attributes are set
 */
bool
InSpeciesTypeBond::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetBindingSite1() == false)
    allPresent = false;

  if (isSetBindingSite2() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
InSpeciesTypeBond::writeElements (XMLOutputStream& stream) const
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
InSpeciesTypeBond::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
InSpeciesTypeBond::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
InSpeciesTypeBond::enablePackageInternal(const std::string& pkgURI,
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
InSpeciesTypeBond::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("bindingSite1");
  attributes.add("bindingSite2");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
InSpeciesTypeBond::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfInSpeciesTypeBonds - which will have
   * happened immediately prior to this read
  */

  ListOfInSpeciesTypeBonds * parentListOf =
      static_cast<ListOfInSpeciesTypeBonds*>(getParentSBMLObject());

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
        getErrorLog()->logPackageError("multi", MultiLofInSptBnds_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofInSptBnds_AllowedAtts,
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
        getErrorLog()->logPackageError("multi", MultiInSptBnd_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiInSptBnd_AllowedCoreAtts,
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
      logEmptyString(mId, getLevel(), getVersion(), "<InSpeciesTypeBond>");
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
      logEmptyString(mName, getLevel(), getVersion(), "<InSpeciesTypeBond>");
    }
  }

  //
  // bindingSite1 SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("bindingSite1", mBindingSite1);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mBindingSite1.empty() == true)
    {
      logEmptyString(mBindingSite1, getLevel(), getVersion(), "<InSpeciesTypeBond>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mBindingSite1) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute bindingSite1='" + mBindingSite1 + "' does not conform.";
      getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                 getPackageVersion(), sbmlLevel, sbmlVersion, details,
                 getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'bindingSite1' is missing.";
    getErrorLog()->logPackageError("multi", MultiInSptBnd_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message,
                   getLine(), getColumn());
  }

  //
  // bindingSite2 SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("bindingSite2", mBindingSite2);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mBindingSite2.empty() == true)
    {
      logEmptyString(mBindingSite2, getLevel(), getVersion(), "<InSpeciesTypeBond>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mBindingSite2) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute bindingSite2='" + mBindingSite2 + "' does not conform.";
      getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
        getPackageVersion(), sbmlLevel, sbmlVersion, details,
        getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'bindingSite2' is missing.";
    getErrorLog()->logPackageError("multi", MultiInSptBnd_AllowedMultiAtts,
      getPackageVersion(), sbmlLevel, sbmlVersion, message,
      getLine(), getColumn());
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
InSpeciesTypeBond::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetBindingSite1() == true)
    stream.writeAttribute("bindingSite1", getPrefix(), mBindingSite1);

  if (isSetBindingSite2() == true)
    stream.writeAttribute("bindingSite2", getPrefix(), mBindingSite2);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfInSpeciesTypeBonds::ListOfInSpeciesTypeBonds(unsigned int level, 
                           unsigned int version, 
                           unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfInSpeciesTypeBonds::ListOfInSpeciesTypeBonds(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfInSpeciesTypeBonds 
 */
ListOfInSpeciesTypeBonds* 
ListOfInSpeciesTypeBonds::clone () const
 {
  return new ListOfInSpeciesTypeBonds(*this);
}


/*
 * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by index.
 */
InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::get(unsigned int n)
{
  return static_cast<InSpeciesTypeBond*>(ListOf::get(n));
}


/*
 * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by index.
 */
const InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::get(unsigned int n) const
{
  return static_cast<const InSpeciesTypeBond*>(ListOf::get(n));
}


/*
 * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by id.
 */
InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::get(const std::string& sid)
{
  return const_cast<InSpeciesTypeBond*>(
    static_cast<const ListOfInSpeciesTypeBonds&>(*this).get(sid));
}


/*
 * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by id.
 */
const InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<InSpeciesTypeBond>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <InSpeciesTypeBond*> (*result);
}


/*
 * Removes the nth InSpeciesTypeBond from this ListOfInSpeciesTypeBonds
 */
InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::remove(unsigned int n)
{
  return static_cast<InSpeciesTypeBond*>(ListOf::remove(n));
}


/*
 * Removes the InSpeciesTypeBond from this ListOfInSpeciesTypeBonds with the given identifier
 */
InSpeciesTypeBond*
ListOfInSpeciesTypeBonds::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<InSpeciesTypeBond>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <InSpeciesTypeBond*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfInSpeciesTypeBonds::getElementName () const
{
  static const string name = "listOfInSpeciesTypeBonds";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfInSpeciesTypeBonds::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfInSpeciesTypeBonds::getItemTypeCode () const
{
  return SBML_MULTI_IN_SPECIES_TYPE_BOND;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new InSpeciesTypeBond in this ListOfInSpeciesTypeBonds
 */
SBase*
ListOfInSpeciesTypeBonds::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "inSpeciesTypeBond")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new InSpeciesTypeBond(multins);
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
ListOfInSpeciesTypeBonds::writeXMLNS(XMLOutputStream& stream) const
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
InSpeciesTypeBond_t *
InSpeciesTypeBond_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion)
{
  return new InSpeciesTypeBond(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
InSpeciesTypeBond_free(InSpeciesTypeBond_t * istb)
{
  if (istb != NULL)
    delete istb;
}


LIBSBML_EXTERN
InSpeciesTypeBond_t *
InSpeciesTypeBond_clone(InSpeciesTypeBond_t * istb)
{
  if (istb != NULL)
  {
    return static_cast<InSpeciesTypeBond_t*>(istb->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
InSpeciesTypeBond_getId(InSpeciesTypeBond_t * istb)
{
  if (istb == NULL)
    return NULL;

  return istb->getId().empty() ? NULL : safe_strdup(istb->getId().c_str());
}


LIBSBML_EXTERN
char *
InSpeciesTypeBond_getName(InSpeciesTypeBond_t * istb)
{
  if (istb == NULL)
    return NULL;

  return istb->getName().empty() ? NULL : safe_strdup(istb->getName().c_str());
}


LIBSBML_EXTERN
char *
InSpeciesTypeBond_getBindingSite1(InSpeciesTypeBond_t * istb)
{
  if (istb == NULL)
    return NULL;

  return istb->getBindingSite1().empty() ? NULL : safe_strdup(istb->getBindingSite1().c_str());
}


LIBSBML_EXTERN
char *
InSpeciesTypeBond_getBindingSite2(InSpeciesTypeBond_t * istb)
{
  if (istb == NULL)
    return NULL;

  return istb->getBindingSite2().empty() ? NULL : safe_strdup(istb->getBindingSite2().c_str());
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetId(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? static_cast<int>(istb->isSetId()) : 0;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetName(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? static_cast<int>(istb->isSetName()) : 0;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetBindingSite1(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? static_cast<int>(istb->isSetBindingSite1()) : 0;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetBindingSite2(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? static_cast<int>(istb->isSetBindingSite2()) : 0;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_setId(InSpeciesTypeBond_t * istb, const char * id)
{
  return (istb != NULL) ? istb->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_setName(InSpeciesTypeBond_t * istb, const char * name)
{
  return (istb != NULL) ? istb->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_setBindingSite1(InSpeciesTypeBond_t * istb, const char * bindingSite1)
{
  return (istb != NULL) ? istb->setBindingSite1(bindingSite1) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_setBindingSite2(InSpeciesTypeBond_t * istb, const char * bindingSite2)
{
  return (istb != NULL) ? istb->setBindingSite2(bindingSite2) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetId(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? istb->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetName(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? istb->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetBindingSite1(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? istb->unsetBindingSite1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetBindingSite2(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? istb->unsetBindingSite2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InSpeciesTypeBond_hasRequiredAttributes(InSpeciesTypeBond_t * istb)
{
  return (istb != NULL) ? static_cast<int>(istb->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
InSpeciesTypeBond_t *
ListOfInSpeciesTypeBonds_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfInSpeciesTypeBonds *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
InSpeciesTypeBond_t *
ListOfInSpeciesTypeBonds_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfInSpeciesTypeBonds *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


