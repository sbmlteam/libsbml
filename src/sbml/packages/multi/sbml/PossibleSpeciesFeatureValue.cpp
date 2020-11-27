/**
 * @file:   PossibleSpeciesFeatureValue.cpp
 * @brief:  Implementation of the PossibleSpeciesFeatureValue class
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


#include <sbml/packages/multi/sbml/PossibleSpeciesFeatureValue.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new PossibleSpeciesFeatureValue with the given level, version, and package version.
 */
PossibleSpeciesFeatureValue::PossibleSpeciesFeatureValue (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mNumericValue ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new PossibleSpeciesFeatureValue with the given MultiPkgNamespaces object.
 */
PossibleSpeciesFeatureValue::PossibleSpeciesFeatureValue (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mNumericValue ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for PossibleSpeciesFeatureValue.
 */
PossibleSpeciesFeatureValue::PossibleSpeciesFeatureValue (const PossibleSpeciesFeatureValue& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mNumericValue  ( orig.mNumericValue)
{
}


/*
 * Assignment for PossibleSpeciesFeatureValue.
 */
PossibleSpeciesFeatureValue&
PossibleSpeciesFeatureValue::operator=(const PossibleSpeciesFeatureValue& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mNumericValue  = rhs.mNumericValue;
  }
  return *this;
}


/*
 * Clone for PossibleSpeciesFeatureValue.
 */
PossibleSpeciesFeatureValue*
PossibleSpeciesFeatureValue::clone () const
{
  return new PossibleSpeciesFeatureValue(*this);
}


/*
 * Destructor for PossibleSpeciesFeatureValue.
 */
PossibleSpeciesFeatureValue::~PossibleSpeciesFeatureValue ()
{
}


/*
 * Returns the value of the "id" attribute of this PossibleSpeciesFeatureValue.
 */
const std::string&
PossibleSpeciesFeatureValue::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this PossibleSpeciesFeatureValue.
 */
const std::string&
PossibleSpeciesFeatureValue::getName() const
{
  return mName;
}


/*
 * Returns the value of the "numericValue" attribute of this PossibleSpeciesFeatureValue.
 */
const std::string&
PossibleSpeciesFeatureValue::getNumericValue() const
{
  return mNumericValue;
}


/*
 * Returns true/false if id is set.
 */
bool
PossibleSpeciesFeatureValue::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
PossibleSpeciesFeatureValue::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if numericValue is set.
 */
bool
PossibleSpeciesFeatureValue::isSetNumericValue() const
{
  return (mNumericValue.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
PossibleSpeciesFeatureValue::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
PossibleSpeciesFeatureValue::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets numericValue and returns value indicating success.
 */
int
PossibleSpeciesFeatureValue::setNumericValue(const std::string& numericValue)
{
  if (!(SyntaxChecker::isValidInternalSId(numericValue)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mNumericValue = numericValue;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
PossibleSpeciesFeatureValue::unsetId()
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
PossibleSpeciesFeatureValue::unsetName()
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
 * Unsets numericValue and returns value indicating success.
 */
int
PossibleSpeciesFeatureValue::unsetNumericValue()
{
  mNumericValue.erase();

  if (mNumericValue.empty() == true)
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
PossibleSpeciesFeatureValue::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetNumericValue() == true && mNumericValue == oldid)
  {
    setNumericValue(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
PossibleSpeciesFeatureValue::getElementName () const
{
  static const string name = "possibleSpeciesFeatureValue";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
PossibleSpeciesFeatureValue::getTypeCode () const
{
  return SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE;
}


/*
 * check if all the required attributes are set
 */
bool
PossibleSpeciesFeatureValue::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
PossibleSpeciesFeatureValue::writeElements (XMLOutputStream& stream) const
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
PossibleSpeciesFeatureValue::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
PossibleSpeciesFeatureValue::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
PossibleSpeciesFeatureValue::enablePackageInternal(const std::string& pkgURI,
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
PossibleSpeciesFeatureValue::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("numericValue");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
PossibleSpeciesFeatureValue::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfPossibleSpeciesFeatureValues - which will have
   * happened immediately prior to this read
  */

  ListOfPossibleSpeciesFeatureValues * parentListOf =
      static_cast<ListOfPossibleSpeciesFeatureValues*>(getParentSBMLObject());

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
        getErrorLog()->logPackageError("multi", MultiLofPsbSpeFtrVals_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofPsbSpeFtrVals_AllowedAtts,
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
        getErrorLog()->logPackageError("multi", MultiPsbSpeFtrVal_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiPsbSpeFtrVal_AllowedCoreAtts,
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
      logEmptyString(mId, getLevel(), getVersion(), "<PossibleSpeciesFeatureValue>");
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
    getErrorLog()->logPackageError("multi", MultiPsbSpeFtrVal_AllowedMultiAtts,
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
      logEmptyString(mName, getLevel(), getVersion(), "<PossibleSpeciesFeatureValue>");
    }
  }

  //
  // numericValue SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("numericValue", mNumericValue);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mNumericValue.empty() == true)
    {
      logEmptyString(mNumericValue, getLevel(), getVersion(), "<PossibleSpeciesFeatureValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mNumericValue) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute numericValue='" + mNumericValue + "' does not conform.";
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
PossibleSpeciesFeatureValue::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetNumericValue() == true)
    stream.writeAttribute("numericValue", getPrefix(), mNumericValue);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfPossibleSpeciesFeatureValues::ListOfPossibleSpeciesFeatureValues(unsigned int level, 
                                     unsigned int version, 
                                     unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfPossibleSpeciesFeatureValues::ListOfPossibleSpeciesFeatureValues(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfPossibleSpeciesFeatureValues 
 */
ListOfPossibleSpeciesFeatureValues* 
ListOfPossibleSpeciesFeatureValues::clone () const
 {
  return new ListOfPossibleSpeciesFeatureValues(*this);
}


/*
 * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by index.
 */
PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::get(unsigned int n)
{
  return static_cast<PossibleSpeciesFeatureValue*>(ListOf::get(n));
}


/*
 * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by index.
 */
const PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::get(unsigned int n) const
{
  return static_cast<const PossibleSpeciesFeatureValue*>(ListOf::get(n));
}


/*
 * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by id.
 */
PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::get(const std::string& sid)
{
  return const_cast<PossibleSpeciesFeatureValue*>(
    static_cast<const ListOfPossibleSpeciesFeatureValues&>(*this).get(sid));
}


/*
 * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by id.
 */
const PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<PossibleSpeciesFeatureValue>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <PossibleSpeciesFeatureValue*> (*result);
}


/*
 * Removes the nth PossibleSpeciesFeatureValue from this ListOfPossibleSpeciesFeatureValues
 */
PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::remove(unsigned int n)
{
  return static_cast<PossibleSpeciesFeatureValue*>(ListOf::remove(n));
}


/*
 * Removes the PossibleSpeciesFeatureValue from this ListOfPossibleSpeciesFeatureValues with the given identifier
 */
PossibleSpeciesFeatureValue*
ListOfPossibleSpeciesFeatureValues::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<PossibleSpeciesFeatureValue>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <PossibleSpeciesFeatureValue*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfPossibleSpeciesFeatureValues::getElementName () const
{
  static const string name = "listOfPossibleSpeciesFeatureValues";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfPossibleSpeciesFeatureValues::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfPossibleSpeciesFeatureValues::getItemTypeCode () const
{
  return SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new PossibleSpeciesFeatureValue in this ListOfPossibleSpeciesFeatureValues
 */
SBase*
ListOfPossibleSpeciesFeatureValues::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "possibleSpeciesFeatureValue")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new PossibleSpeciesFeatureValue(multins);
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
ListOfPossibleSpeciesFeatureValues::writeXMLNS(XMLOutputStream& stream) const
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
PossibleSpeciesFeatureValue_t *
PossibleSpeciesFeatureValue_create(unsigned int level, unsigned int version,
                                   unsigned int pkgVersion)
{
  return new PossibleSpeciesFeatureValue(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
PossibleSpeciesFeatureValue_free(PossibleSpeciesFeatureValue_t * psfv)
{
  if (psfv != NULL)
    delete psfv;
}


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
PossibleSpeciesFeatureValue_clone(PossibleSpeciesFeatureValue_t * psfv)
{
  if (psfv != NULL)
  {
    return static_cast<PossibleSpeciesFeatureValue_t*>(psfv->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getId(PossibleSpeciesFeatureValue_t * psfv)
{
  if (psfv == NULL)
    return NULL;

  return psfv->getId().empty() ? NULL : safe_strdup(psfv->getId().c_str());
}


LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getName(PossibleSpeciesFeatureValue_t * psfv)
{
  if (psfv == NULL)
    return NULL;

  return psfv->getName().empty() ? NULL : safe_strdup(psfv->getName().c_str());
}


LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getNumericValue(PossibleSpeciesFeatureValue_t * psfv)
{
  if (psfv == NULL)
    return NULL;

  return psfv->getNumericValue().empty() ? NULL : safe_strdup(psfv->getNumericValue().c_str());
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetId(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? static_cast<int>(psfv->isSetId()) : 0;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetName(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? static_cast<int>(psfv->isSetName()) : 0;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetNumericValue(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? static_cast<int>(psfv->isSetNumericValue()) : 0;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setId(PossibleSpeciesFeatureValue_t * psfv, const char * id)
{
  return (psfv != NULL) ? psfv->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setName(PossibleSpeciesFeatureValue_t * psfv, const char * name)
{
  return (psfv != NULL) ? psfv->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setNumericValue(PossibleSpeciesFeatureValue_t * psfv, const char * numericValue)
{
  return (psfv != NULL) ? psfv->setNumericValue(numericValue) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetId(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? psfv->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetName(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? psfv->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetNumericValue(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? psfv->unsetNumericValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_hasRequiredAttributes(PossibleSpeciesFeatureValue_t * psfv)
{
  return (psfv != NULL) ? static_cast<int>(psfv->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
ListOfPossibleSpeciesFeatureValues_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfPossibleSpeciesFeatureValues *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
ListOfPossibleSpeciesFeatureValues_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfPossibleSpeciesFeatureValues *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


