/**
 * @file:   SpeciesFeatureType.cpp
 * @brief:  Implementation of the SpeciesFeatureType class
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


#include <sbml/packages/multi/sbml/SpeciesFeatureType.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpeciesFeatureType with the given level, version, and package version.
 */
SpeciesFeatureType::SpeciesFeatureType (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mOccur (SBML_INT_MAX)
   ,mIsSetOccur (false)
   ,mPossibleSpeciesFeatureValues (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new SpeciesFeatureType with the given MultiPkgNamespaces object.
 */
SpeciesFeatureType::SpeciesFeatureType (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mOccur (SBML_INT_MAX)
   ,mIsSetOccur (false)
   ,mPossibleSpeciesFeatureValues (multins)
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for SpeciesFeatureType.
 */
SpeciesFeatureType::SpeciesFeatureType (const SpeciesFeatureType& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mOccur  ( orig.mOccur)
  , mIsSetOccur  ( orig.mIsSetOccur)
  , mPossibleSpeciesFeatureValues  ( orig.mPossibleSpeciesFeatureValues)
{
  // connect to child objects
  connectToChild();
}


/*
 * Assignment for SpeciesFeatureType.
 */
SpeciesFeatureType&
SpeciesFeatureType::operator=(const SpeciesFeatureType& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mOccur  = rhs.mOccur;
    mIsSetOccur  = rhs.mIsSetOccur;
    mPossibleSpeciesFeatureValues  = rhs.mPossibleSpeciesFeatureValues;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for SpeciesFeatureType.
 */
SpeciesFeatureType*
SpeciesFeatureType::clone () const
{
  return new SpeciesFeatureType(*this);
}


/*
 * Destructor for SpeciesFeatureType.
 */
SpeciesFeatureType::~SpeciesFeatureType ()
{
}


/*
 * Returns the value of the "id" attribute of this SpeciesFeatureType.
 */
const std::string&
SpeciesFeatureType::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SpeciesFeatureType.
 */
const std::string&
SpeciesFeatureType::getName() const
{
  return mName;
}


/*
 * Returns the value of the "occur" attribute of this SpeciesFeatureType.
 */
unsigned int
SpeciesFeatureType::getOccur() const
{
  return mOccur;
}


/*
 * Returns true/false if id is set.
 */
bool
SpeciesFeatureType::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SpeciesFeatureType::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if occur is set.
 */
bool
SpeciesFeatureType::isSetOccur() const
{
  return mIsSetOccur;
}


/*
 * Sets id and returns value indicating success.
 */
int
SpeciesFeatureType::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SpeciesFeatureType::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets occur and returns value indicating success.
 */
int
SpeciesFeatureType::setOccur(unsigned int occur)
{
  mOccur = occur;
  mIsSetOccur = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
SpeciesFeatureType::unsetId()
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
SpeciesFeatureType::unsetName()
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
 * Unsets occur and returns value indicating success.
 */
int
SpeciesFeatureType::unsetOccur()
{
  mOccur = SBML_INT_MAX;
  mIsSetOccur = false;

  if (isSetOccur() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the  "ListOfPossibleSpeciesFeatureValues" in this SpeciesFeatureType object.
 */
const ListOfPossibleSpeciesFeatureValues*
SpeciesFeatureType::getListOfPossibleSpeciesFeatureValues() const
{
  return &mPossibleSpeciesFeatureValues;
}


/*
 * Returns the  "ListOfPossibleSpeciesFeatureValues" in this SpeciesFeatureType object.
 */
ListOfPossibleSpeciesFeatureValues*
SpeciesFeatureType::getListOfPossibleSpeciesFeatureValues()
{
  return &mPossibleSpeciesFeatureValues;
}


/*
 * Removes the nth PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues.
 */
PossibleSpeciesFeatureValue*
SpeciesFeatureType::removePossibleSpeciesFeatureValue(unsigned int n)
{
  return mPossibleSpeciesFeatureValues.remove(n);
}


/*
 * Removes the a PossibleSpeciesFeatureValue with given id from the ListOfPossibleSpeciesFeatureValues.
 */
PossibleSpeciesFeatureValue*
SpeciesFeatureType::removePossibleSpeciesFeatureValue(const std::string& sid)
{
  return mPossibleSpeciesFeatureValues.remove(sid);
}


/*
 * Return the nth PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
 */
PossibleSpeciesFeatureValue*
SpeciesFeatureType::getPossibleSpeciesFeatureValue(unsigned int n)
{
  return mPossibleSpeciesFeatureValues.get(n);
}


/*
 * Return the nth PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
 */
const PossibleSpeciesFeatureValue*
SpeciesFeatureType::getPossibleSpeciesFeatureValue(unsigned int n) const
{
  return mPossibleSpeciesFeatureValues.get(n);
}


/*
 * Return a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by id.
 */
PossibleSpeciesFeatureValue*
SpeciesFeatureType::getPossibleSpeciesFeatureValue(const std::string& sid)
{
  return mPossibleSpeciesFeatureValues.get(sid);
}


/*
 * Return a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues by id.
 */
const PossibleSpeciesFeatureValue*
SpeciesFeatureType::getPossibleSpeciesFeatureValue(const std::string& sid) const
{
  return mPossibleSpeciesFeatureValues.get(sid);
}


/*
 * Adds a copy the given "PossibleSpeciesFeatureValue" to this SpeciesFeatureType.
 */
int
SpeciesFeatureType::addPossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue* psfv)
{
  if (psfv == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (psfv->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != psfv->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != psfv->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(psfv)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mPossibleSpeciesFeatureValues.append(psfv);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of PossibleSpeciesFeatureValue objects in this SpeciesFeatureType.
 */
unsigned int
SpeciesFeatureType::getNumPossibleSpeciesFeatureValues() const
{
  return mPossibleSpeciesFeatureValues.size();
}


/*
 * Creates a new PossibleSpeciesFeatureValue object, adds it to this SpeciesFeatureTypes
 */
PossibleSpeciesFeatureValue*
SpeciesFeatureType::createPossibleSpeciesFeatureValue()
{
  PossibleSpeciesFeatureValue* psfv = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    psfv = new PossibleSpeciesFeatureValue(multins);
    delete multins;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(psfv != NULL)
  {
    mPossibleSpeciesFeatureValues.appendAndOwn(psfv);
  }

  return psfv;
}


List*
SpeciesFeatureType::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mPossibleSpeciesFeatureValues, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpeciesFeatureType::getElementName () const
{
  static const string name = "speciesFeatureType";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpeciesFeatureType::getTypeCode () const
{
  return SBML_MULTI_SPECIES_FEATURE_TYPE;
}


/*
 * check if all the required attributes are set
 */
bool
SpeciesFeatureType::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetOccur() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
SpeciesFeatureType::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpeciesFeatureType::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumPossibleSpeciesFeatureValues() > 0)
  {
    mPossibleSpeciesFeatureValues.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
SpeciesFeatureType::accept (SBMLVisitor& v) const
{
  v.visit(*this);

  // PossibleSpeciesFeatureValue
  for(unsigned int i = 0; i < getNumPossibleSpeciesFeatureValues(); i++)
  {
    getPossibleSpeciesFeatureValue(i)->accept(v);
  }

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpeciesFeatureType::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mPossibleSpeciesFeatureValues.setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
SpeciesFeatureType::connectToChild()
{
  mPossibleSpeciesFeatureValues.connectToParent(this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpeciesFeatureType::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mPossibleSpeciesFeatureValues.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
SpeciesFeatureType::createObject(XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  SBase* object = NULL;

  MULTI_CREATE_NS(multins, getSBMLNamespaces());

  if (name == "listOfPossibleSpeciesFeatureValues")
  {
    if (mPossibleSpeciesFeatureValues.size() != 0)
    {
      getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_RestrictElt,
        getPackageVersion(), getLevel(), getVersion(),
        "<" + getPrefix() + "speciesFeatureType> may only have one <" + getPrefix()
        + "listOfPossibleSpeciesFeatureValues>",
        stream.peek().getLine(),
        stream.peek().getColumn());

    }

    object = &mPossibleSpeciesFeatureValues;
  }

  delete multins;
  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
SpeciesFeatureType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("occur");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpeciesFeatureType::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpeciesFeatureTypes - which will have
   * happened immediately prior to this read
  */

  ListOfSpeciesFeatureTypes * parentList =
      static_cast<ListOfSpeciesFeatureTypes*>(getParentSBMLObject());

  if (getErrorLog() != NULL &&
      parentList->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSpeFtrTyps_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentList->getLine(), parentList->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSpeFtrTyps_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentList->getLine(), parentList->getColumn());
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
        getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_AllowedCoreAtts,
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
      logEmptyString(mId, getLevel(), getVersion(), "<SpeciesFeatureType>");
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
    getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_AllowedMultiAtts,
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
      logEmptyString(mName, getLevel(), getVersion(), "<SpeciesFeatureType>");
    }
  }

  //
  // occur unsigned int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetOccur = attributes.readInto("occur", mOccur);

  if (mIsSetOccur == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        std::string details = getErrorLog()->getError(numErrs)->getMessage();
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_OccAtt_Ref,
                     getPackageVersion(), sbmlLevel, sbmlVersion, details,
                     getLine(), getColumn());
      }
      else
      {
        std::string message = "Multi attribute 'occur' is missing.";
        getErrorLog()->logPackageError("multi", MultiSpeFtrTyp_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message,
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
SpeciesFeatureType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetOccur() == true)
    stream.writeAttribute("occur", getPrefix(), mOccur);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfSpeciesFeatureTypes::ListOfSpeciesFeatureTypes(unsigned int level, 
                            unsigned int version, 
                            unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpeciesFeatureTypes::ListOfSpeciesFeatureTypes(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfSpeciesFeatureTypes 
 */
ListOfSpeciesFeatureTypes* 
ListOfSpeciesFeatureTypes::clone () const
 {
  return new ListOfSpeciesFeatureTypes(*this);
}


/*
 * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by index.
 */
SpeciesFeatureType*
ListOfSpeciesFeatureTypes::get(unsigned int n)
{
  return static_cast<SpeciesFeatureType*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by index.
 */
const SpeciesFeatureType*
ListOfSpeciesFeatureTypes::get(unsigned int n) const
{
  return static_cast<const SpeciesFeatureType*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by id.
 */
SpeciesFeatureType*
ListOfSpeciesFeatureTypes::get(const std::string& sid)
{
  return const_cast<SpeciesFeatureType*>(
    static_cast<const ListOfSpeciesFeatureTypes&>(*this).get(sid));
}


/*
 * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by id.
 */
const SpeciesFeatureType*
ListOfSpeciesFeatureTypes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeatureType>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesFeatureType*> (*result);
}


/*
 * Removes the nth SpeciesFeatureType from this ListOfSpeciesFeatureTypes
 */
SpeciesFeatureType*
ListOfSpeciesFeatureTypes::remove(unsigned int n)
{
  return static_cast<SpeciesFeatureType*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesFeatureType from this ListOfSpeciesFeatureTypes with the given identifier
 */
SpeciesFeatureType*
ListOfSpeciesFeatureTypes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeatureType>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesFeatureType*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpeciesFeatureTypes::getElementName () const
{
  static const string name = "listOfSpeciesFeatureTypes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpeciesFeatureTypes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpeciesFeatureTypes::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_FEATURE_TYPE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesFeatureType in this ListOfSpeciesFeatureTypes
 */
SBase*
ListOfSpeciesFeatureTypes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesFeatureType")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesFeatureType(multins);
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
ListOfSpeciesFeatureTypes::writeXMLNS(XMLOutputStream& stream) const
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
SpeciesFeatureType_t *
SpeciesFeatureType_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion)
{
  return new SpeciesFeatureType(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpeciesFeatureType_free(SpeciesFeatureType_t * sft)
{
  if (sft != NULL)
    delete sft;
}


LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_clone(SpeciesFeatureType_t * sft)
{
  if (sft != NULL)
  {
    return static_cast<SpeciesFeatureType_t*>(sft->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
SpeciesFeatureType_getId(SpeciesFeatureType_t * sft)
{
  if (sft == NULL)
    return NULL;

  return sft->getId().empty() ? NULL : safe_strdup(sft->getId().c_str());
}


LIBSBML_EXTERN
char *
SpeciesFeatureType_getName(SpeciesFeatureType_t * sft)
{
  if (sft == NULL)
    return NULL;

  return sft->getName().empty() ? NULL : safe_strdup(sft->getName().c_str());
}


LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getOccur(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? sft->getOccur() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetId(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? static_cast<int>(sft->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetName(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? static_cast<int>(sft->isSetName()) : 0;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetOccur(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? static_cast<int>(sft->isSetOccur()) : 0;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_setId(SpeciesFeatureType_t * sft, const char * id)
{
  return (sft != NULL) ? sft->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_setName(SpeciesFeatureType_t * sft, const char * name)
{
  return (sft != NULL) ? sft->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_setOccur(SpeciesFeatureType_t * sft, unsigned int occur)
{
  return (sft != NULL) ? sft->setOccur(occur) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetId(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? sft->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetName(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? sft->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetOccur(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? sft->unsetOccur() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing PossibleSpeciesFeatureValue_t objects from
 * this SpeciesFeatureType_t.
 */
LIBSBML_EXTERN
ListOf_t*
SpeciesFeatureType_getListOfPossibleSpeciesFeatureValues(SpeciesFeatureType_t*
  sft)
{
  return (sft != NULL) ? sft->getListOfPossibleSpeciesFeatureValues() : NULL;
}


/*
 * Get a PossibleSpeciesFeatureValue_t from the SpeciesFeatureType_t.
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t*
SpeciesFeatureType_getPossibleSpeciesFeatureValue(SpeciesFeatureType_t* sft,
                                                  unsigned int n)
{
  return (sft != NULL) ? sft->getPossibleSpeciesFeatureValue(n) : NULL;
}


/*
 * Get a PossibleSpeciesFeatureValue_t from the SpeciesFeatureType_t based on
 * its identifier.
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t*
SpeciesFeatureType_getPossibleSpeciesFeatureValueById(
                                                      SpeciesFeatureType_t*
                                                        sft,
                                                      const char *sid)
{
  return (sft != NULL && sid != NULL) ?
    sft->getPossibleSpeciesFeatureValue(sid) : NULL;
}


/*
 * Adds a copy of the given PossibleSpeciesFeatureValue_t to this
 * SpeciesFeatureType_t.
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_addPossibleSpeciesFeatureValue(SpeciesFeatureType_t* sft,
                                                  const
                                                    PossibleSpeciesFeatureValue_t*
                                                      psfv)
{
  return (sft != NULL) ? sft->addPossibleSpeciesFeatureValue(psfv) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of PossibleSpeciesFeatureValue_t objects in this
 * SpeciesFeatureType_t.
 */
LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getNumPossibleSpeciesFeatureValues(SpeciesFeatureType_t*
  sft)
{
  return (sft != NULL) ? sft->getNumPossibleSpeciesFeatureValues() :
    SBML_INT_MAX;
}


/*
 * Creates a new PossibleSpeciesFeatureValue_t object, adds it to this
 * SpeciesFeatureType_t object and returns the PossibleSpeciesFeatureValue_t
 * object created.
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t*
SpeciesFeatureType_createPossibleSpeciesFeatureValue(SpeciesFeatureType_t* sft)
{
  return (sft != NULL) ? sft->createPossibleSpeciesFeatureValue() : NULL;
}


/*
 * Removes the nth PossibleSpeciesFeatureValue_t from this SpeciesFeatureType_t
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t*
SpeciesFeatureType_removePossibleSpeciesFeatureValue(SpeciesFeatureType_t* sft,
                                                     unsigned int n)
{
  return (sft != NULL) ? sft->removePossibleSpeciesFeatureValue(n) : NULL;
}


/*
 * Removes the PossibleSpeciesFeatureValue_t from this SpeciesFeatureType_t
 * based on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t*
SpeciesFeatureType_removePossibleSpeciesFeatureValueById(
                                                         SpeciesFeatureType_t*
                                                           sft,
                                                         const char* sid)
{
  return (sft != NULL && sid != NULL) ?
    sft->removePossibleSpeciesFeatureValue(sid) : NULL;
}


LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredAttributes(SpeciesFeatureType_t * sft)
{
  return (sft != NULL) ? static_cast<int>(sft->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesFeatureTypes *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesFeatureTypes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


