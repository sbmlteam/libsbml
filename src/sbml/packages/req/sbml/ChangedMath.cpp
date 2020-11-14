/**
 * @file:   ChangedMath.cpp
 * @brief:  Implementation of the ChangedMath class
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
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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


#include <sbml/packages/req/sbml/ChangedMath.h>
#include <sbml/packages/req/validator/ReqSBMLError.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ChangedMath with the given level, version, and package version.
 */
ChangedMath::ChangedMath (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mChangedBy ("")
   ,mViableWithoutChange (false)
   ,mIsSetViableWithoutChange (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new ReqPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ChangedMath with the given ReqPkgNamespaces object.
 */
ChangedMath::ChangedMath (ReqPkgNamespaces* reqns)
  : SBase(reqns)
////   ,mId ("")
////   ,mName ("")
   ,mChangedBy ("")
   ,mViableWithoutChange (false)
   ,mIsSetViableWithoutChange (false)
{
  // set the element namespace of this object
  setElementNamespace(reqns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(reqns);
}


/*
 * Copy constructor for ChangedMath.
 */
ChangedMath::ChangedMath (const ChangedMath& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mChangedBy  ( orig.mChangedBy)
  , mViableWithoutChange  ( orig.mViableWithoutChange)
  , mIsSetViableWithoutChange  ( orig.mIsSetViableWithoutChange)
{
}


/*
 * Assignment for ChangedMath.
 */
ChangedMath&
ChangedMath::operator=(const ChangedMath& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mChangedBy  = rhs.mChangedBy;
    mViableWithoutChange  = rhs.mViableWithoutChange;
    mIsSetViableWithoutChange  = rhs.mIsSetViableWithoutChange;
  }
  return *this;
}


/*
 * Clone for ChangedMath.
 */
ChangedMath*
ChangedMath::clone () const
{
  return new ChangedMath(*this);
}


/*
 * Destructor for ChangedMath.
 */
ChangedMath::~ChangedMath ()
{
}


/*
 * Returns the value of the "id" attribute of this ChangedMath.
 */
const std::string&
ChangedMath::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ChangedMath.
 */
const std::string&
ChangedMath::getName() const
{
  return mName;
}


/*
 * Returns the value of the "changedBy" attribute of this ChangedMath.
 */
const std::string&
ChangedMath::getChangedBy() const
{
  return mChangedBy;
}


/*
 * Returns the value of the "viableWithoutChange" attribute of this ChangedMath.
 */
bool
ChangedMath::getViableWithoutChange() const
{
  return mViableWithoutChange;
}


/*
 * Returns true/false if id is set.
 */
bool
ChangedMath::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
ChangedMath::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if changedBy is set.
 */
bool
ChangedMath::isSetChangedBy() const
{
  return (mChangedBy.empty() == false);
}


/*
 * Returns true/false if viableWithoutChange is set.
 */
bool
ChangedMath::isSetViableWithoutChange() const
{
  return mIsSetViableWithoutChange;
}


/*
 * Sets id and returns value indicating success.
 */
int
ChangedMath::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
ChangedMath::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets changedBy and returns value indicating success.
 */
int
ChangedMath::setChangedBy(const std::string& changedBy)
{
  mChangedBy = changedBy;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets viableWithoutChange and returns value indicating success.
 */
int
ChangedMath::setViableWithoutChange(bool viableWithoutChange)
{
  mViableWithoutChange = viableWithoutChange;
  mIsSetViableWithoutChange = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
ChangedMath::unsetId()
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
ChangedMath::unsetName()
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
 * Unsets changedBy and returns value indicating success.
 */
int
ChangedMath::unsetChangedBy()
{
  mChangedBy.erase();

  if (mChangedBy.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets viableWithoutChange and returns value indicating success.
 */
int
ChangedMath::unsetViableWithoutChange()
{
  mViableWithoutChange = false;
  mIsSetViableWithoutChange = false;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ChangedMath::getElementName () const
{
  static const string name = "changedMath";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ChangedMath::getTypeCode () const
{
  return SBML_REQ_CHANGED_MATH;
}


/*
 * check if all the required attributes are set
 */
bool
ChangedMath::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetChangedBy() == false)
    allPresent = false;

  if (isSetViableWithoutChange() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
ChangedMath::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
ChangedMath::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
ChangedMath::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
ChangedMath::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
ChangedMath::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("changedBy");
  attributes.add("viableWithoutChange");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
ChangedMath::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfChangedMaths - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfChangedMaths*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("req", ReqUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("req", ReqUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details);
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
        getErrorLog()->logPackageError("req", ReqUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("req", ReqUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
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
      logEmptyString(mId, getLevel(), getVersion(), "<ChangedMath>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.");
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
      logEmptyString(mName, getLevel(), getVersion(), "<ChangedMath>");
    }
  }

  //
  // changedBy string   ( use = "required" )
  //
  assigned = attributes.readInto("changedBy", mChangedBy);

  if (assigned == true)
  {
    // check string is not empty

    if (mChangedBy.empty() == true)
    {
      logEmptyString(mChangedBy, getLevel(), getVersion(), "<ChangedMath>");
    }
  }
  else
  {
    std::string message = "Req attribute 'changedBy' is missing.";
    getErrorLog()->logPackageError("req", ReqUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

  //
  // viableWithoutChange bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetViableWithoutChange = attributes.readInto("viableWithoutChange", mViableWithoutChange);

  if (mIsSetViableWithoutChange == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("req", ReqUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Req attribute 'viableWithoutChange' is missing.";
        getErrorLog()->logPackageError("req", ReqUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
ChangedMath::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetChangedBy() == true)
    stream.writeAttribute("changedBy", getPrefix(), mChangedBy);

  if (isSetViableWithoutChange() == true)
    stream.writeAttribute("viableWithoutChange", getPrefix(), mViableWithoutChange);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfChangedMaths::ListOfChangedMaths(unsigned int level, 
                     unsigned int version, 
                     unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new ReqPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfChangedMaths::ListOfChangedMaths(ReqPkgNamespaces* reqns)
  : ListOf(reqns)
{
  setElementNamespace(reqns->getURI());
}


/*
 * Returns a deep copy of this ListOfChangedMaths 
 */
ListOfChangedMaths* 
ListOfChangedMaths::clone () const
 {
  return new ListOfChangedMaths(*this);
}


/*
 * Get a ChangedMath from the ListOfChangedMaths by index.
*/
ChangedMath*
ListOfChangedMaths::get(unsigned int n)
{
  return static_cast<ChangedMath*>(ListOf::get(n));
}


/*
 * Get a ChangedMath from the ListOfChangedMaths by index.
 */
const ChangedMath*
ListOfChangedMaths::get(unsigned int n) const
{
  return static_cast<const ChangedMath*>(ListOf::get(n));
}


/*
 * Get a ChangedMath from the ListOfChangedMaths by id.
 */
ChangedMath*
ListOfChangedMaths::get(const std::string& sid)
{
  return const_cast<ChangedMath*>(
    static_cast<const ListOfChangedMaths&>(*this).get(sid));
}


/*
 * Get a ChangedMath from the ListOfChangedMaths by id.
 */
const ChangedMath*
ListOfChangedMaths::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ChangedMath>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <ChangedMath*> (*result);
}


/*
 * Removes the nth ChangedMath from this ListOfChangedMaths
 */
ChangedMath*
ListOfChangedMaths::remove(unsigned int n)
{
  return static_cast<ChangedMath*>(ListOf::remove(n));
}


/*
 * Removes the ChangedMath from this ListOfChangedMaths with the given identifier
 */
ChangedMath*
ListOfChangedMaths::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ChangedMath>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <ChangedMath*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfChangedMaths::getElementName () const
{
  static const string name = "listOfChangedMaths";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfChangedMaths::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfChangedMaths::getItemTypeCode () const
{
  return SBML_REQ_CHANGED_MATH;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ChangedMath in this ListOfChangedMaths
 */
SBase*
ListOfChangedMaths::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "changedMath")
  {
    REQ_CREATE_NS(reqns, getSBMLNamespaces());
    object = new ChangedMath(reqns);
    appendAndOwn(object);
    delete reqns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Req package.
 */
void
ListOfChangedMaths::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(ReqExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(ReqExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


/*
 * 
 */
LIBSBML_EXTERN
ChangedMath_t *
ChangedMath_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion)
{
  return new ChangedMath(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
ChangedMath_free(ChangedMath_t * cm)
{
  if (cm != NULL)
    delete cm;
}


/*
 *
 */
LIBSBML_EXTERN
ChangedMath_t *
ChangedMath_clone(ChangedMath_t * cm)
{
  if (cm != NULL)
  {
    return static_cast<ChangedMath_t*>(cm->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 *
 */
LIBSBML_EXTERN
char *
ChangedMath_getId(ChangedMath_t * cm)
{
  if (cm == NULL)
    return NULL;

  return cm->getId().empty() ? NULL : safe_strdup(cm->getId().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
char *
ChangedMath_getName(ChangedMath_t * cm)
{
  if (cm == NULL)
    return NULL;

  return cm->getName().empty() ? NULL : safe_strdup(cm->getName().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
const char *
ChangedMath_getChangedBy(ChangedMath_t * cm)
{
  if (cm == NULL)
    return NULL;

  return cm->getChangedBy().empty() ? NULL : safe_strdup(cm->getChangedBy().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_getViableWithoutChange(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->getViableWithoutChange()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_isSetId(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetId()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_isSetName(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetName()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_isSetChangedBy(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetChangedBy()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_isSetViableWithoutChange(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetViableWithoutChange()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_setId(ChangedMath_t * cm, const char * id)
{
  return (cm != NULL) ? cm->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_setName(ChangedMath_t * cm, const char * name)
{
  return (cm != NULL) ? cm->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_setChangedBy(ChangedMath_t * cm, const char * changedBy)
{
  return (cm != NULL) ? cm->setChangedBy(changedBy) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_setViableWithoutChange(ChangedMath_t * cm, int viableWithoutChange)
{
  return (cm != NULL) ? cm->setViableWithoutChange(viableWithoutChange) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_unsetId(ChangedMath_t * cm)
{
  return (cm != NULL) ? cm->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_unsetName(ChangedMath_t * cm)
{
  return (cm != NULL) ? cm->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_unsetChangedBy(ChangedMath_t * cm)
{
  return (cm != NULL) ? cm->unsetChangedBy() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_unsetViableWithoutChange(ChangedMath_t * cm)
{
  return (cm != NULL) ? cm->unsetViableWithoutChange() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
ChangedMath_hasRequiredAttributes(ChangedMath_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
ChangedMath_t *
ListOfChangedMaths_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfChangedMaths *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
ChangedMath_t *
ListOfChangedMaths_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfChangedMaths *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


