/**
 * @file:   SpeciesTypeInstance.cpp
 * @brief:  Implementation of the SpeciesTypeInstance class
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


#include <sbml/packages/multi/sbml/SpeciesTypeInstance.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpeciesTypeInstance with the given level, version, and package version.
 */
SpeciesTypeInstance::SpeciesTypeInstance (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mSpeciesType ("")
   ,mCompartmentReference ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpeciesTypeInstance with the given MultiPkgNamespaces object.
 */
SpeciesTypeInstance::SpeciesTypeInstance (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mSpeciesType ("")
   ,mCompartmentReference ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for SpeciesTypeInstance.
 */
SpeciesTypeInstance::SpeciesTypeInstance (const SpeciesTypeInstance& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mSpeciesType  ( orig.mSpeciesType)
  , mCompartmentReference  ( orig.mCompartmentReference)
{
}


/*
 * Assignment for SpeciesTypeInstance.
 */
SpeciesTypeInstance&
SpeciesTypeInstance::operator=(const SpeciesTypeInstance& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mSpeciesType  = rhs.mSpeciesType;
    mCompartmentReference  = rhs.mCompartmentReference;
  }
  return *this;
}


/*
 * Clone for SpeciesTypeInstance.
 */
SpeciesTypeInstance*
SpeciesTypeInstance::clone () const
{
  return new SpeciesTypeInstance(*this);
}


/*
 * Destructor for SpeciesTypeInstance.
 */
SpeciesTypeInstance::~SpeciesTypeInstance ()
{
}


/*
 * Returns the value of the "id" attribute of this SpeciesTypeInstance.
 */
const std::string&
SpeciesTypeInstance::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SpeciesTypeInstance.
 */
const std::string&
SpeciesTypeInstance::getName() const
{
  return mName;
}


/*
 * Returns the value of the "speciesType" attribute of this SpeciesTypeInstance.
 */
const std::string&
SpeciesTypeInstance::getSpeciesType() const
{
  return mSpeciesType;
}


/*
 * Returns the value of the "compartmentReference" attribute of this SpeciesTypeInstance.
 */
const std::string&
SpeciesTypeInstance::getCompartmentReference() const
{
  return mCompartmentReference;
}


/*
 * Returns true/false if id is set.
 */
bool
SpeciesTypeInstance::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SpeciesTypeInstance::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if speciesType is set.
 */
bool
SpeciesTypeInstance::isSetSpeciesType() const
{
  return (mSpeciesType.empty() == false);
}


/*
 * Returns true/false if compartmentReference is set.
 */
bool
SpeciesTypeInstance::isSetCompartmentReference() const
{
  return (mCompartmentReference.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
SpeciesTypeInstance::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SpeciesTypeInstance::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets speciesType and returns value indicating success.
 */
int
SpeciesTypeInstance::setSpeciesType(const std::string& speciesType)
{
  if (!(SyntaxChecker::isValidInternalSId(speciesType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpeciesType = speciesType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets compartmentReference and returns value indicating success.
 */
int
SpeciesTypeInstance::setCompartmentReference(const std::string& compartmentReference)
{
  if (!(SyntaxChecker::isValidInternalSId(compartmentReference)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartmentReference = compartmentReference;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
SpeciesTypeInstance::unsetId()
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
SpeciesTypeInstance::unsetName()
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
 * Unsets speciesType and returns value indicating success.
 */
int
SpeciesTypeInstance::unsetSpeciesType()
{
  mSpeciesType.erase();

  if (mSpeciesType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets compartmentReference and returns value indicating success.
 */
int
SpeciesTypeInstance::unsetCompartmentReference()
{
  mCompartmentReference.erase();

  if (mCompartmentReference.empty() == true)
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
SpeciesTypeInstance::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetSpeciesType() == true && mSpeciesType == oldid)
  {
    setSpeciesType(newid);
  }

  if (isSetCompartmentReference() == true && mCompartmentReference == oldid)
  {
    setCompartmentReference(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpeciesTypeInstance::getElementName () const
{
  static const string name = "speciesTypeInstance";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpeciesTypeInstance::getTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_INSTANCE;
}


/*
 * check if all the required attributes are set
 */
bool
SpeciesTypeInstance::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetSpeciesType() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpeciesTypeInstance::writeElements (XMLOutputStream& stream) const
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
SpeciesTypeInstance::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpeciesTypeInstance::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpeciesTypeInstance::enablePackageInternal(const std::string& pkgURI,
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
SpeciesTypeInstance::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("speciesType");
  attributes.add("compartmentReference");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpeciesTypeInstance::readAttributes(const XMLAttributes& attributes,
    const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel = getLevel();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpeciesTypeInstances - which will have
   * happened immediately prior to this read
   */

  ListOfSpeciesTypeInstances * parentListOf =
      static_cast<ListOfSpeciesTypeInstances*>(getParentSBMLObject());

  if (getErrorLog() != NULL && parentListOf->size() < 2)
    {
      numErrs = getErrorLog()->getNumErrors();
      for (int n = numErrs - 1; n >= 0; n--)
        {
          if (getErrorLog()->getError(n)->getErrorId()
              == UnknownPackageAttribute)
            {
              const std::string details =
                  getErrorLog()->getError(n)->getMessage();
              getErrorLog()->remove(UnknownPackageAttribute);
              getErrorLog()->logPackageError("multi",
                  MultiLofSptInss_AllowedAtts, getPackageVersion(), sbmlLevel,
                  sbmlVersion, details, parentListOf->getLine(),
                  parentListOf->getColumn());
            }
          else if (getErrorLog()->getError(n)->getErrorId()
              == UnknownCoreAttribute)
            {
              const std::string details =
                  getErrorLog()->getError(n)->getMessage();
              getErrorLog()->remove(UnknownCoreAttribute);
              getErrorLog()->logPackageError("multi",
                  MultiLofSptInss_AllowedAtts, getPackageVersion(), sbmlLevel,
                  sbmlVersion, details, parentListOf->getLine(),
                  parentListOf->getColumn());
            }
        }
    }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
    {
      numErrs = getErrorLog()->getNumErrors();
      for (int n = numErrs - 1; n >= 0; n--)
        {
          if (getErrorLog()->getError(n)->getErrorId()
              == UnknownPackageAttribute)
            {
              const std::string details =
                  getErrorLog()->getError(n)->getMessage();
              getErrorLog()->remove(UnknownPackageAttribute);
              getErrorLog()->logPackageError("multi",
                  MultiSptIns_AllowedMultiAtts, getPackageVersion(), sbmlLevel,
                  sbmlVersion, details, getLine(), getColumn());
            }
          else if (getErrorLog()->getError(n)->getErrorId()
              == UnknownCoreAttribute)
            {
              const std::string details =
                  getErrorLog()->getError(n)->getMessage();
              getErrorLog()->remove(UnknownCoreAttribute);
              getErrorLog()->logPackageError("multi",
                  MultiSptIns_AllowedCoreAtts, getPackageVersion(), sbmlLevel,
                  sbmlVersion, details, getLine(), getColumn());
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
          logEmptyString(mId, getLevel(), getVersion(),
              "<SpeciesTypeInstance>");
        }
      else if (SyntaxChecker::isValidSBMLSId(mId)
          == false&& getErrorLog() != NULL)
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
      getErrorLog()->logPackageError("multi", MultiSptIns_AllowedMultiAtts,
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
          logEmptyString(mName, getLevel(), getVersion(),
              "<SpeciesTypeInstance>");
        }
    }

  //
  // speciesType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("speciesType", mSpeciesType);

  if (assigned == true)
    {
      // check string is not empty and correct syntax

      if (mSpeciesType.empty() == true)
        {
          logEmptyString(mSpeciesType, getLevel(), getVersion(),
              "<SpeciesTypeInstance>");
        }
      else if (SyntaxChecker::isValidSBMLSId(mSpeciesType)
          == false&& getErrorLog() != NULL)
        {
          std::string details = "The syntax of the attribute speciesType='" + mSpeciesType + "' does not conform.";
          getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                     getPackageVersion(), sbmlLevel, sbmlVersion, details,
                     getLine(), getColumn());
        }
    }
  else
    {
      std::string message = "Multi attribute 'speciesType' is missing.";
      getErrorLog()->logPackageError("multi", MultiSptIns_AllowedMultiAtts,
          getPackageVersion(), sbmlLevel, sbmlVersion, message,
          getLine(), getColumn());
    }

  //
  // compartmentReference SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("compartmentReference", mCompartmentReference);

  if (assigned == true)
    {
      // check string is not empty and correct syntax

      if (mCompartmentReference.empty() == true)
        {
          logEmptyString(mCompartmentReference, getLevel(), getVersion(),
              "<SpeciesTypeInstance>");
        }
      else if (SyntaxChecker::isValidSBMLSId(mCompartmentReference)
          == false&& getErrorLog() != NULL)
        {
          std::string details = "The syntax of the attribute compartmentReference='" + mCompartmentReference + "' does not conform.";
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
SpeciesTypeInstance::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetSpeciesType() == true)
    stream.writeAttribute("speciesType", getPrefix(), mSpeciesType);

  if (isSetCompartmentReference() == true)
    stream.writeAttribute("compartmentReference", getPrefix(), mCompartmentReference);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfSpeciesTypeInstances::ListOfSpeciesTypeInstances(unsigned int level, 
                             unsigned int version, 
                             unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpeciesTypeInstances::ListOfSpeciesTypeInstances(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfSpeciesTypeInstances 
 */
ListOfSpeciesTypeInstances* 
ListOfSpeciesTypeInstances::clone () const
 {
  return new ListOfSpeciesTypeInstances(*this);
}


/*
 * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by index.
 */
SpeciesTypeInstance*
ListOfSpeciesTypeInstances::get(unsigned int n)
{
  return static_cast<SpeciesTypeInstance*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by index.
 */
const SpeciesTypeInstance*
ListOfSpeciesTypeInstances::get(unsigned int n) const
{
  return static_cast<const SpeciesTypeInstance*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by id.
 */
SpeciesTypeInstance*
ListOfSpeciesTypeInstances::get(const std::string& sid)
{
  return const_cast<SpeciesTypeInstance*>(
    static_cast<const ListOfSpeciesTypeInstances&>(*this).get(sid));
}


/*
 * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by id.
 */
const SpeciesTypeInstance*
ListOfSpeciesTypeInstances::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeInstance>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesTypeInstance*> (*result);
}


/*
 * Removes the nth SpeciesTypeInstance from this ListOfSpeciesTypeInstances
 */
SpeciesTypeInstance*
ListOfSpeciesTypeInstances::remove(unsigned int n)
{
  return static_cast<SpeciesTypeInstance*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesTypeInstance from this ListOfSpeciesTypeInstances with the given identifier
 */
SpeciesTypeInstance*
ListOfSpeciesTypeInstances::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeInstance>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesTypeInstance*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpeciesTypeInstances::getElementName () const
{
  static const string name = "listOfSpeciesTypeInstances";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpeciesTypeInstances::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpeciesTypeInstances::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_INSTANCE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesTypeInstance in this ListOfSpeciesTypeInstances
 */
SBase*
ListOfSpeciesTypeInstances::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesTypeInstance")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesTypeInstance(multins);
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
ListOfSpeciesTypeInstances::writeXMLNS(XMLOutputStream& stream) const
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
SpeciesTypeInstance_t *
SpeciesTypeInstance_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion)
{
  return new SpeciesTypeInstance(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpeciesTypeInstance_free(SpeciesTypeInstance_t * sti)
{
  if (sti != NULL)
    delete sti;
}


LIBSBML_EXTERN
SpeciesTypeInstance_t *
SpeciesTypeInstance_clone(SpeciesTypeInstance_t * sti)
{
  if (sti != NULL)
  {
    return static_cast<SpeciesTypeInstance_t*>(sti->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
SpeciesTypeInstance_getId(SpeciesTypeInstance_t * sti)
{
  if (sti == NULL)
    return NULL;

  return sti->getId().empty() ? NULL : safe_strdup(sti->getId().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeInstance_getName(SpeciesTypeInstance_t * sti)
{
  if (sti == NULL)
    return NULL;

  return sti->getName().empty() ? NULL : safe_strdup(sti->getName().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeInstance_getSpeciesType(SpeciesTypeInstance_t * sti)
{
  if (sti == NULL)
    return NULL;

  return sti->getSpeciesType().empty() ? NULL : safe_strdup(sti->getSpeciesType().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeInstance_getCompartmentReference(SpeciesTypeInstance_t * sti)
{
  if (sti == NULL)
    return NULL;

  return sti->getCompartmentReference().empty() ? NULL : safe_strdup(sti->getCompartmentReference().c_str());
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetId(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? static_cast<int>(sti->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetName(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? static_cast<int>(sti->isSetName()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetSpeciesType(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? static_cast<int>(sti->isSetSpeciesType()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetCompartmentReference(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? static_cast<int>(sti->isSetCompartmentReference()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_setId(SpeciesTypeInstance_t * sti, const char * id)
{
  return (sti != NULL) ? sti->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_setName(SpeciesTypeInstance_t * sti, const char * name)
{
  return (sti != NULL) ? sti->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_setSpeciesType(SpeciesTypeInstance_t * sti, const char * speciesType)
{
  return (sti != NULL) ? sti->setSpeciesType(speciesType) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_setCompartmentReference(SpeciesTypeInstance_t * sti, const char * compartmentReference)
{
  return (sti != NULL) ? sti->setCompartmentReference(compartmentReference) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetId(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? sti->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetName(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? sti->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetSpeciesType(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? sti->unsetSpeciesType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetCompartmentReference(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? sti->unsetCompartmentReference() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeInstance_hasRequiredAttributes(SpeciesTypeInstance_t * sti)
{
  return (sti != NULL) ? static_cast<int>(sti->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
SpeciesTypeInstance_t *
ListOfSpeciesTypeInstances_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeInstances *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
SpeciesTypeInstance_t *
ListOfSpeciesTypeInstances_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeInstances *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


