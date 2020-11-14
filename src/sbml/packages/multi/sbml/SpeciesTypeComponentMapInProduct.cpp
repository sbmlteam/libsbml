/**
 * @file:   SpeciesTypeComponentMapInProduct.cpp
 * @brief:  Implementation of the SpeciesTypeComponentMapInProduct class
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


#include <sbml/packages/multi/sbml/SpeciesTypeComponentMapInProduct.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpeciesTypeComponentMapInProduct with the given level, version, and package version.
 */
SpeciesTypeComponentMapInProduct::SpeciesTypeComponentMapInProduct (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mReactant ("")
   ,mReactantComponent ("")
   ,mProductComponent ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpeciesTypeComponentMapInProduct with the given MultiPkgNamespaces object.
 */
SpeciesTypeComponentMapInProduct::SpeciesTypeComponentMapInProduct (MultiPkgNamespaces* multins)
  : SBase(multins)
   ,mReactant ("")
   ,mReactantComponent ("")
   ,mProductComponent ("")
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for SpeciesTypeComponentMapInProduct.
 */
SpeciesTypeComponentMapInProduct::SpeciesTypeComponentMapInProduct (const SpeciesTypeComponentMapInProduct& orig)
  : SBase(orig)
  , mReactant  ( orig.mReactant)
  , mReactantComponent  ( orig.mReactantComponent)
  , mProductComponent  ( orig.mProductComponent)
{
}


/*
 * Assignment for SpeciesTypeComponentMapInProduct.
 */
SpeciesTypeComponentMapInProduct&
SpeciesTypeComponentMapInProduct::operator=(const SpeciesTypeComponentMapInProduct& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mReactant  = rhs.mReactant;
    mReactantComponent  = rhs.mReactantComponent;
    mProductComponent  = rhs.mProductComponent;
  }
  return *this;
}


/*
 * Clone for SpeciesTypeComponentMapInProduct.
 */
SpeciesTypeComponentMapInProduct*
SpeciesTypeComponentMapInProduct::clone () const
{
  return new SpeciesTypeComponentMapInProduct(*this);
}


/*
 * Destructor for SpeciesTypeComponentMapInProduct.
 */
SpeciesTypeComponentMapInProduct::~SpeciesTypeComponentMapInProduct ()
{
}


/*
 * Returns the value of the "id" attribute of this SpeciesTypeComponentMapInProduct.
 */
const std::string&
SpeciesTypeComponentMapInProduct::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SpeciesTypeComponentMapInProduct.
 */
const std::string&
SpeciesTypeComponentMapInProduct::getName() const
{
  return mName;
}


/*
 * Returns the value of the "reactant" attribute of this SpeciesTypeComponentMapInProduct.
 */
const std::string&
SpeciesTypeComponentMapInProduct::getReactant() const
{
  return mReactant;
}


/*
 * Returns the value of the "reactantComponent" attribute of this SpeciesTypeComponentMapInProduct.
 */
const std::string&
SpeciesTypeComponentMapInProduct::getReactantComponent() const
{
  return mReactantComponent;
}


/*
 * Returns the value of the "productComponent" attribute of this SpeciesTypeComponentMapInProduct.
 */
const std::string&
SpeciesTypeComponentMapInProduct::getProductComponent() const
{
  return mProductComponent;
}


/*
 * Returns true/false if id is set.
 */
bool
SpeciesTypeComponentMapInProduct::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SpeciesTypeComponentMapInProduct::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if reactant is set.
 */
bool
SpeciesTypeComponentMapInProduct::isSetReactant() const
{
  return (mReactant.empty() == false);
}


/*
 * Returns true/false if reactantComponent is set.
 */
bool
SpeciesTypeComponentMapInProduct::isSetReactantComponent() const
{
  return (mReactantComponent.empty() == false);
}


/*
 * Returns true/false if productComponent is set.
 */
bool
SpeciesTypeComponentMapInProduct::isSetProductComponent() const
{
  return (mProductComponent.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets reactant and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::setReactant(const std::string& reactant)
{
  if (!(SyntaxChecker::isValidInternalSId(reactant)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mReactant = reactant;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets reactantComponent and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::setReactantComponent(const std::string& reactantComponent)
{
  if (!(SyntaxChecker::isValidInternalSId(reactantComponent)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mReactantComponent = reactantComponent;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets productComponent and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::setProductComponent(const std::string& productComponent)
{
  if (!(SyntaxChecker::isValidInternalSId(productComponent)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mProductComponent = productComponent;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::unsetId()
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
SpeciesTypeComponentMapInProduct::unsetName()
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
 * Unsets reactant and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::unsetReactant()
{
  mReactant.erase();

  if (mReactant.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets reactantComponent and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::unsetReactantComponent()
{
  mReactantComponent.erase();

  if (mReactantComponent.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets productComponent and returns value indicating success.
 */
int
SpeciesTypeComponentMapInProduct::unsetProductComponent()
{
  mProductComponent.erase();

  if (mProductComponent.empty() == true)
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
SpeciesTypeComponentMapInProduct::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetReactant() == true && mReactant == oldid)
  {
    setReactant(newid);
  }

  if (isSetReactantComponent() == true && mReactantComponent == oldid)
  {
    setReactantComponent(newid);
  }

  if (isSetProductComponent() == true && mProductComponent == oldid)
  {
    setProductComponent(newid);
  }

}



/*
 * Returns the XML element name of this object
 */
const std::string&
SpeciesTypeComponentMapInProduct::getElementName () const
{
  static const string name = "speciesTypeComponentMapInProduct";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpeciesTypeComponentMapInProduct::getTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT;
}


/*
 * check if all the required attributes are set
 */
bool
SpeciesTypeComponentMapInProduct::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetReactant() == false)
    allPresent = false;

  if (isSetReactantComponent() == false)
    allPresent = false;

  if (isSetProductComponent() == false)
    allPresent = false;

  return allPresent;
}



  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpeciesTypeComponentMapInProduct::writeElements (XMLOutputStream& stream) const
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
SpeciesTypeComponentMapInProduct::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpeciesTypeComponentMapInProduct::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpeciesTypeComponentMapInProduct::enablePackageInternal(const std::string& pkgURI,
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
SpeciesTypeComponentMapInProduct::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("reactant");
  attributes.add("reactantComponent");
  attributes.add("productComponent");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpeciesTypeComponentMapInProduct::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpeciesTypeComponentMapInProducts - which will have
   * happened immediately prior to this read
  */

  ListOfSpeciesTypeComponentMapInProducts * parentListOf =
      static_cast<ListOfSpeciesTypeComponentMapInProducts*>(getParentSBMLObject());

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
        getErrorLog()->logPackageError("multi", MultiLofSptCpoMapsInPro_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  parentListOf->getLine(), parentListOf->getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofSptCpoMapsInPro_AllowedAtts,
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
        getErrorLog()->logPackageError("multi", MultiSptCpoMapInPro_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSptCpoMapInPro_AllowedCoreAtts,
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
      logEmptyString(mId, getLevel(), getVersion(), "<SpeciesTypeComponentMapInProduct>");
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
      logEmptyString(mName, getLevel(), getVersion(), "<SpeciesTypeComponentMapInProduct>");
    }
  }

  //
  // reactant SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("reactant", mReactant);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mReactant.empty() == true)
    {
      logEmptyString(mReactant, getLevel(), getVersion(), "<SpeciesTypeComponentMapInProduct>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mReactant) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute reactant='" + mReactant + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());

    }
  }
  else
  {
    std::string message = "Multi attribute 'reactant' is missing.";
    getErrorLog()->logPackageError("multi", MultiSptCpoMapInPro_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // reactantComponent SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("reactantComponent", mReactantComponent);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mReactantComponent.empty() == true)
    {
      logEmptyString(mReactantComponent, getLevel(), getVersion(), "<SpeciesTypeComponentMapInProduct>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mReactantComponent) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute reactantComponent='" + mReactantComponent + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());

    }
  }
  else
  {
    std::string message = "Multi attribute 'reactantComponent' is missing.";
    getErrorLog()->logPackageError("multi", MultiSptCpoMapInPro_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // productComponent SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("productComponent", mProductComponent);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mProductComponent.empty() == true)
    {
      logEmptyString(mProductComponent, getLevel(), getVersion(), "<SpeciesTypeComponentMapInProduct>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mProductComponent) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute productComponent='" + mProductComponent + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'productComponent' is missing.";
    getErrorLog()->logPackageError("multi", MultiSptCpoMapInPro_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpeciesTypeComponentMapInProduct::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetReactant() == true)
    stream.writeAttribute("reactant", getPrefix(), mReactant);

  if (isSetReactantComponent() == true)
    stream.writeAttribute("reactantComponent", getPrefix(), mReactantComponent);

  if (isSetProductComponent() == true)
    stream.writeAttribute("productComponent", getPrefix(), mProductComponent);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfSpeciesTypeComponentMapInProducts::ListOfSpeciesTypeComponentMapInProducts(unsigned int level, 
                                          unsigned int version, 
                                          unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpeciesTypeComponentMapInProducts::ListOfSpeciesTypeComponentMapInProducts(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfSpeciesTypeComponentMapInProducts 
 */
ListOfSpeciesTypeComponentMapInProducts* 
ListOfSpeciesTypeComponentMapInProducts::clone () const
 {
  return new ListOfSpeciesTypeComponentMapInProducts(*this);
}


/*
 * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts by index.
 */
SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::get(unsigned int n)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts by index.
 */
const SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::get(unsigned int n) const
{
  return static_cast<const SpeciesTypeComponentMapInProduct*>(ListOf::get(n));
}


/*
 * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts by id.
 */
SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::get(const std::string& sid)
{
  return const_cast<SpeciesTypeComponentMapInProduct*>(
    static_cast<const ListOfSpeciesTypeComponentMapInProducts&>(*this).get(sid));
}


/*
 * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts by id.
 */
const SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeComponentMapInProduct>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesTypeComponentMapInProduct*> (*result);
}


/*
 * Removes the nth SpeciesTypeComponentMapInProduct from this ListOfSpeciesTypeComponentMapInProducts
 */
SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::remove(unsigned int n)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesTypeComponentMapInProduct from this ListOfSpeciesTypeComponentMapInProducts with the given identifier
 */
SpeciesTypeComponentMapInProduct*
ListOfSpeciesTypeComponentMapInProducts::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesTypeComponentMapInProduct>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesTypeComponentMapInProduct*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpeciesTypeComponentMapInProducts::getElementName () const
{
  static const string name = "listOfSpeciesTypeComponentMapsInProduct";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpeciesTypeComponentMapInProducts::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpeciesTypeComponentMapInProducts::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts
 */
SBase*
ListOfSpeciesTypeComponentMapInProducts::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesTypeComponentMapInProduct")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesTypeComponentMapInProduct(multins);
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
ListOfSpeciesTypeComponentMapInProducts::writeXMLNS(XMLOutputStream& stream) const
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
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_create(unsigned int level, unsigned int version,
                                        unsigned int pkgVersion)
{
  return new SpeciesTypeComponentMapInProduct(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpeciesTypeComponentMapInProduct_free(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  if (stcmip != NULL)
    delete stcmip;
}


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_clone(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  if (stcmip != NULL)
  {
    return static_cast<SpeciesTypeComponentMapInProduct_t*>(stcmip->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getId(SpeciesTypeComponentMapInProduct_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getId().empty() ? NULL : safe_strdup(cr->getId().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getName(SpeciesTypeComponentMapInProduct_t * cr)
{
  if (cr == NULL)
    return NULL;

  return cr->getName().empty() ? NULL : safe_strdup(cr->getName().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactant(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  if (stcmip == NULL)
    return NULL;

  return stcmip->getReactant().empty() ? NULL : safe_strdup(stcmip->getReactant().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  if (stcmip == NULL)
    return NULL;

  return stcmip->getReactantComponent().empty() ? NULL : safe_strdup(stcmip->getReactantComponent().c_str());
}


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  if (stcmip == NULL)
    return NULL;

  return stcmip->getProductComponent().empty() ? NULL : safe_strdup(stcmip->getProductComponent().c_str());
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetId(SpeciesTypeComponentMapInProduct_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetName(SpeciesTypeComponentMapInProduct_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactant(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? static_cast<int>(stcmip->isSetReactant()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? static_cast<int>(stcmip->isSetReactantComponent()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? static_cast<int>(stcmip->isSetProductComponent()) : 0;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setId(SpeciesTypeComponentMapInProduct_t * cr, const char * id)
{
  return (cr != NULL) ? cr->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setName(SpeciesTypeComponentMapInProduct_t * cr, const char * name)
{
  return (cr != NULL) ? cr->setName(name) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactant(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactant)
{
  return (stcmip != NULL) ? stcmip->setReactant(reactant) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactantComponent)
{
  return (stcmip != NULL) ? stcmip->setReactantComponent(reactantComponent) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * productComponent)
{
  return (stcmip != NULL) ? stcmip->setProductComponent(productComponent) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetId(SpeciesTypeComponentMapInProduct_t * cr)
{
  return (cr != NULL) ? cr->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetName(SpeciesTypeComponentMapInProduct_t * cr)
{
  return (cr != NULL) ? cr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactant(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? stcmip->unsetReactant() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? stcmip->unsetReactantComponent() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? stcmip->unsetProductComponent() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_hasRequiredAttributes(SpeciesTypeComponentMapInProduct_t * stcmip)
{
  return (stcmip != NULL) ? static_cast<int>(stcmip->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeComponentMapInProducts *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpeciesTypeComponentMapInProducts *>(lo)->remove(sid) : NULL;
}



LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


