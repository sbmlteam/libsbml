/**
 * @file:   SpeciesFeatureValue.cpp
 * @brief:  Implementation of the SpeciesFeatureValue class
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


#include <sbml/packages/multi/sbml/SpeciesFeatureValue.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpeciesFeatureValue with the given level, version, and package version.
 */
SpeciesFeatureValue::SpeciesFeatureValue (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mValue ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpeciesFeatureValue with the given MultiPkgNamespaces object.
 */
SpeciesFeatureValue::SpeciesFeatureValue (MultiPkgNamespaces* multins)
  : SBase(multins)
   ,mValue ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for SpeciesFeatureValue.
 */
SpeciesFeatureValue::SpeciesFeatureValue (const SpeciesFeatureValue& orig)
  : SBase(orig)
  , mValue  ( orig.mValue)
{
}


/*
 * Assignment for SpeciesFeatureValue.
 */
SpeciesFeatureValue&
SpeciesFeatureValue::operator=(const SpeciesFeatureValue& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mValue  = rhs.mValue;
  }
  return *this;
}


/*
 * Clone for SpeciesFeatureValue.
 */
SpeciesFeatureValue*
SpeciesFeatureValue::clone () const
{
  return new SpeciesFeatureValue(*this);
}


/*
 * Destructor for SpeciesFeatureValue.
 */
SpeciesFeatureValue::~SpeciesFeatureValue ()
{
}


/*
 * Returns the value of the "value" attribute of this SpeciesFeatureValue.
 */
const std::string&
SpeciesFeatureValue::getValue() const
{
  return mValue;
}


/*
 * Returns true/false if value is set.
 */
bool
SpeciesFeatureValue::isSetValue() const
{
  return (mValue.empty() == false);
}


/*
 * Sets value and returns value indicating success.
 */
int
SpeciesFeatureValue::setValue(const std::string& value)
{
  if (!(SyntaxChecker::isValidInternalSId(value)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mValue = value;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets value and returns value indicating success.
 */
int
SpeciesFeatureValue::unsetValue()
{
  mValue.erase();

  if (mValue.empty() == true)
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
SpeciesFeatureValue::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetValue() == true && mValue == oldid)
  {
    setValue(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpeciesFeatureValue::getElementName () const
{
  static const string name = "speciesFeatureValue";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpeciesFeatureValue::getTypeCode () const
{
  return SBML_MULTI_SPECIES_FEATURE_VALUE;
}


/*
 * check if all the required attributes are set
 */
bool
SpeciesFeatureValue::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetValue() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpeciesFeatureValue::writeElements (XMLOutputStream& stream) const
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
SpeciesFeatureValue::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpeciesFeatureValue::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpeciesFeatureValue::enablePackageInternal(const std::string& pkgURI,
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
SpeciesFeatureValue::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("value");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpeciesFeatureValue::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpeciesFeatureValues - which will have
   * happened immediately prior to this read
  */

  ListOfSpeciesFeatureValues * parentListOf =
      static_cast<ListOfSpeciesFeatureValues*>(getParentSBMLObject());

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
        getErrorLog()->logPackageError("multi", MultiLofSpeFtrVals_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSpeFtrVals_AllowedAtts,
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
        getErrorLog()->logPackageError("multi", MultiSpeFtrVal_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSpeFtrVal_AllowedCoreAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // value SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("value", mValue);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mValue.empty() == true)
    {
      logEmptyString(mValue, getLevel(), getVersion(), "<SpeciesFeatureValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mValue) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute value='" + mValue + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'value' is missing.";
    getErrorLog()->logPackageError("multi", MultiSpeFtrVal_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpeciesFeatureValue::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetValue() == true)
    stream.writeAttribute("value", getPrefix(), mValue);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfSpeciesFeatureValues::ListOfSpeciesFeatureValues(unsigned int level, 
                             unsigned int version, 
                             unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpeciesFeatureValues::ListOfSpeciesFeatureValues(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfSpeciesFeatureValues 
 */
ListOfSpeciesFeatureValues* 
ListOfSpeciesFeatureValues::clone () const
 {
  return new ListOfSpeciesFeatureValues(*this);
}


/*
 * Get a SpeciesFeatureValue from the ListOfSpeciesFeatureValues by index.
 */
SpeciesFeatureValue*
ListOfSpeciesFeatureValues::get(unsigned int n)
{
  return static_cast<SpeciesFeatureValue*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeatureValue from the ListOfSpeciesFeatureValues by index.
 */
const SpeciesFeatureValue*
ListOfSpeciesFeatureValues::get(unsigned int n) const
{
  return static_cast<const SpeciesFeatureValue*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeatureValue from the ListOfSpeciesFeatureValues by id.
 */
SpeciesFeatureValue*
ListOfSpeciesFeatureValues::get(const std::string& sid)
{
  return const_cast<SpeciesFeatureValue*>(
    static_cast<const ListOfSpeciesFeatureValues&>(*this).get(sid));
}


/*
 * Get a SpeciesFeatureValue from the ListOfSpeciesFeatureValues by id.
 */
const SpeciesFeatureValue*
ListOfSpeciesFeatureValues::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeatureValue>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesFeatureValue*> (*result);
}


/*
 * Removes the nth SpeciesFeatureValue from this ListOfSpeciesFeatureValues
 */
SpeciesFeatureValue*
ListOfSpeciesFeatureValues::remove(unsigned int n)
{
  return static_cast<SpeciesFeatureValue*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesFeatureValue from this ListOfSpeciesFeatureValues with the given identifier
 */
SpeciesFeatureValue*
ListOfSpeciesFeatureValues::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeatureValue>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesFeatureValue*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpeciesFeatureValues::getElementName () const
{
  static const string name = "listOfSpeciesFeatureValues";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpeciesFeatureValues::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpeciesFeatureValues::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_FEATURE_VALUE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesFeatureValue in this ListOfSpeciesFeatureValues
 */
SBase*
ListOfSpeciesFeatureValues::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesFeatureValue")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesFeatureValue(multins);
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
ListOfSpeciesFeatureValues::writeXMLNS(XMLOutputStream& stream) const
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
SpeciesFeatureValue_t *
SpeciesFeatureValue_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion)
{
  return new SpeciesFeatureValue(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpeciesFeatureValue_free(SpeciesFeatureValue_t * sfv)
{
  if (sfv != NULL)
    delete sfv;
}


LIBSBML_EXTERN
SpeciesFeatureValue_t *
SpeciesFeatureValue_clone(SpeciesFeatureValue_t * sfv)
{
  if (sfv != NULL)
  {
    return static_cast<SpeciesFeatureValue_t*>(sfv->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
SpeciesFeatureValue_getValue(SpeciesFeatureValue_t * sfv)
{
  if (sfv == NULL)
    return NULL;

  return sfv->getValue().empty() ? NULL : safe_strdup(sfv->getValue().c_str());
}


LIBSBML_EXTERN
int
SpeciesFeatureValue_isSetValue(SpeciesFeatureValue_t * sfv)
{
  return (sfv != NULL) ? static_cast<int>(sfv->isSetValue()) : 0;
}


LIBSBML_EXTERN
int
SpeciesFeatureValue_setValue(SpeciesFeatureValue_t * sfv, const char * value)
{
  return (sfv != NULL) ? sfv->setValue(value) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureValue_unsetValue(SpeciesFeatureValue_t * sfv)
{
  return (sfv != NULL) ? sfv->unsetValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesFeatureValue_hasRequiredAttributes(SpeciesFeatureValue_t * sfv)
{
  return (sfv != NULL) ? static_cast<int>(sfv->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
SpeciesFeatureValue_t *
ListOfSpeciesFeatureValues_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesFeatureValues *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
SpeciesFeatureValue_t *
ListOfSpeciesFeatureValues_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesFeatureValues *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


