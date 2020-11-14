/**
 * @file:   CompartmentReference.cpp
 * @brief:  Implementation of the CompartmentReference class
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


#include <sbml/packages/multi/sbml/CompartmentReference.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CompartmentReference with the given level, version, and package version.
 */
CompartmentReference::CompartmentReference (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mCompartment ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CompartmentReference with the given MultiPkgNamespaces object.
 */
CompartmentReference::CompartmentReference (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mCompartment ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for CompartmentReference.
 */
CompartmentReference::CompartmentReference (const CompartmentReference& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mCompartment  ( orig.mCompartment)
{
}


/*
 * Assignment for CompartmentReference.
 */
CompartmentReference&
CompartmentReference::operator=(const CompartmentReference& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mCompartment  = rhs.mCompartment;
  }
  return *this;
}


/*
 * Clone for CompartmentReference.
 */
CompartmentReference*
CompartmentReference::clone () const
{
  return new CompartmentReference(*this);
}


/*
 * Destructor for CompartmentReference.
 */
CompartmentReference::~CompartmentReference ()
{
}


/*
 * Returns the value of the "id" attribute of this CompartmentReference.
 */
const std::string&
CompartmentReference::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this CompartmentReference.
 */
const std::string&
CompartmentReference::getName() const
{
  return mName;
}


/*
 * Returns the value of the "compartment" attribute of this CompartmentReference.
 */
const std::string&
CompartmentReference::getCompartment() const
{
  return mCompartment;
}


/*
 * Returns true/false if id is set.
 */
bool
CompartmentReference::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
CompartmentReference::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if compartment is set.
 */
bool
CompartmentReference::isSetCompartment() const
{
  return (mCompartment.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
CompartmentReference::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
CompartmentReference::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compartment and returns value indicating success.
 */
int
CompartmentReference::setCompartment(const std::string& compartment)
{
  if (!(SyntaxChecker::isValidInternalSId(compartment)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartment = compartment;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
CompartmentReference::unsetId()
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
CompartmentReference::unsetName()
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
 * Unsets compartment and returns value indicating success.
 */
int
CompartmentReference::unsetCompartment()
{
  mCompartment.erase();

  if (mCompartment.empty() == true)
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
CompartmentReference::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetCompartment() == true && mCompartment == oldid)
  {
    setCompartment(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
CompartmentReference::getElementName () const
{
  static const string name = "compartmentReference";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CompartmentReference::getTypeCode () const
{
  return SBML_MULTI_COMPARTMENT_REFERENCE;
}


/*
 * check if all the required attributes are set
 */
bool
CompartmentReference::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetCompartment() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CompartmentReference::writeElements (XMLOutputStream& stream) const
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
CompartmentReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CompartmentReference::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CompartmentReference::enablePackageInternal(const std::string& pkgURI,
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
CompartmentReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("compartment");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CompartmentReference::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfCompartmentReferences - which will have
   * happened immediately prior to this read
  */

  ListOfCompartmentReferences * parentListOfCompartmentReferences =
      static_cast<ListOfCompartmentReferences*>(getParentSBMLObject());

  if (getErrorLog() != NULL && parentListOfCompartmentReferences->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiLofCpaRefs_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOfCompartmentReferences->getLine(),
                  parentListOfCompartmentReferences->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofCpaRefs_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOfCompartmentReferences->getLine(),
                  parentListOfCompartmentReferences->getColumn());
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
        getErrorLog()->logPackageError("multi", MultiCpaRef_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiCpaRef_AllowedCoreAtts,
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
      logEmptyString(mId, getLevel(), getVersion(), "<CompartmentReference>");
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
      logEmptyString(mName, getLevel(), getVersion(), "<CompartmentReference>");
    }
  }

  //
  // compartment SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("compartment", mCompartment);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCompartment.empty() == true)
    {
      logEmptyString(mCompartment, getLevel(), getVersion(), "<CompartmentReference>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCompartment) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute compartment='" + mCompartment + "' does not conform.";
      getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                 getPackageVersion(), sbmlLevel, sbmlVersion, details,
                 getLine(), getColumn());

    }
  }
  else
  {
    std::string message = "The attribute 'compartment' is missing in the <compartmentReference> element.";
    getErrorLog()->logPackageError("multi", MultiCpaRef_AllowedMultiAtts,
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
CompartmentReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetCompartment() == true)
    stream.writeAttribute("compartment", getPrefix(), mCompartment);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfCompartmentReferences::ListOfCompartmentReferences(unsigned int level, 
                              unsigned int version, 
                              unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfCompartmentReferences::ListOfCompartmentReferences(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfCompartmentReferences 
 */
ListOfCompartmentReferences* 
ListOfCompartmentReferences::clone () const
 {
  return new ListOfCompartmentReferences(*this);
}


/*
 * Get a CompartmentReference from the ListOfCompartmentReferences by index.
 */
CompartmentReference*
ListOfCompartmentReferences::get(unsigned int n)
{
  return static_cast<CompartmentReference*>(ListOf::get(n));
}


/*
 * Get a CompartmentReference from the ListOfCompartmentReferences by index.
 */
const CompartmentReference*
ListOfCompartmentReferences::get(unsigned int n) const
{
  return static_cast<const CompartmentReference*>(ListOf::get(n));
}


/*
 * Get a CompartmentReference from the ListOfCompartmentReferences by id.
 */
CompartmentReference*
ListOfCompartmentReferences::get(const std::string& sid)
{
  return const_cast<CompartmentReference*>(
    static_cast<const ListOfCompartmentReferences&>(*this).get(sid));
}


/*
 * Get a CompartmentReference from the ListOfCompartmentReferences by id.
 */
const CompartmentReference*
ListOfCompartmentReferences::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CompartmentReference>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CompartmentReference*> (*result);
}


/*
 * Removes the nth CompartmentReference from this ListOfCompartmentReferences
 */
CompartmentReference*
ListOfCompartmentReferences::remove(unsigned int n)
{
  return static_cast<CompartmentReference*>(ListOf::remove(n));
}


/*
 * Removes the CompartmentReference from this ListOfCompartmentReferences with the given identifier
 */
CompartmentReference*
ListOfCompartmentReferences::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CompartmentReference>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CompartmentReference*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfCompartmentReferences::getElementName () const
{
  static const string name = "listOfCompartmentReferences";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfCompartmentReferences::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfCompartmentReferences::getItemTypeCode () const
{
  return SBML_MULTI_COMPARTMENT_REFERENCE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CompartmentReference in this ListOfCompartmentReferences
 */
SBase*
ListOfCompartmentReferences::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;
  const std::string& name = stream.peek().getName();

  if (name == "compartmentReference")
    {
      MULTI_CREATE_NS(multins, getSBMLNamespaces());
      object = new CompartmentReference(multins);
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
ListOfCompartmentReferences::writeXMLNS(XMLOutputStream& stream) const
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
CompartmentReference_t *
CompartmentReference_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new CompartmentReference(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CompartmentReference_free(CompartmentReference_t * cr)
{
  if (cr != NULL)
    delete cr;
}


LIBSBML_EXTERN
CompartmentReference_t *
CompartmentReference_clone(CompartmentReference_t * cr)
{
  if (cr != NULL)
  {
    return static_cast<CompartmentReference_t*>(cr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
CompartmentReference_getId(CompartmentReference_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getId().empty() ? NULL : safe_strdup(cr->getId().c_str());
}


LIBSBML_EXTERN
char *
CompartmentReference_getName(CompartmentReference_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getName().empty() ? NULL : safe_strdup(cr->getName().c_str());
}


LIBSBML_EXTERN
char *
CompartmentReference_getCompartment(CompartmentReference_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getCompartment().empty() ? NULL : safe_strdup(cr->getCompartment().c_str());
}


LIBSBML_EXTERN
int
CompartmentReference_isSetId(CompartmentReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetId()) : 0;
}


LIBSBML_EXTERN
int
CompartmentReference_isSetName(CompartmentReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
CompartmentReference_isSetCompartment(CompartmentReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetCompartment()) : 0;
}


LIBSBML_EXTERN
int
CompartmentReference_setId(CompartmentReference_t * cr, const char * id)
{
  return (cr != NULL) ? cr->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_setName(CompartmentReference_t * cr, const char * name)
{
  return (cr != NULL) ? cr->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_setCompartment(CompartmentReference_t * cr, const char * compartment)
{
  return (cr != NULL) ? cr->setCompartment(compartment) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_unsetId(CompartmentReference_t * cr)
{
  return (cr != NULL) ? cr->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_unsetName(CompartmentReference_t * cr)
{
  return (cr != NULL) ? cr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_unsetCompartment(CompartmentReference_t * cr)
{
  return (cr != NULL) ? cr->unsetCompartment() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentReference_hasRequiredAttributes(CompartmentReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
CompartmentReference_t *
ListOfCompartmentReferences_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCompartmentReferences *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
CompartmentReference_t *
ListOfCompartmentReferences_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCompartmentReferences *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


