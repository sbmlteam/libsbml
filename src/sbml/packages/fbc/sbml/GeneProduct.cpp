/**
 * @file   GeneProduct.cpp
 * @brief  Implementation of the GeneProduct class
 * @author SBMLTeam
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


#include <sbml/packages/fbc/sbml/GeneProduct.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new GeneProduct with the given level, version, and package version.
 */
GeneProduct::GeneProduct (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
//  , mId ("")
//  , mName ("")
  , mLabel ("")
  , mAssociatedSpecies ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new GeneProduct with the given FbcPkgNamespaces object.
 */
GeneProduct::GeneProduct (FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
//  , mId ("")
//  , mName ("")
  , mLabel ("")
  , mAssociatedSpecies ("")
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for GeneProduct.
 */
GeneProduct::GeneProduct (const GeneProduct& orig)
  : SBase(orig)
{
  mId  = orig.mId;
  mName  = orig.mName;
  mLabel  = orig.mLabel;
  mAssociatedSpecies  = orig.mAssociatedSpecies;
}


/*
 * Assignment for GeneProduct.
 */
GeneProduct&
GeneProduct::operator=(const GeneProduct& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mLabel  = rhs.mLabel;
    mAssociatedSpecies  = rhs.mAssociatedSpecies;
  }
  return *this;
}


/*
 * Clone for GeneProduct.
 */
GeneProduct*
GeneProduct::clone () const
{
  return new GeneProduct(*this);
}


/*
 * Destructor for GeneProduct.
 */
GeneProduct::~GeneProduct ()
{
}


/*
 * Returns the value of the "id" attribute of this GeneProduct.
 */
const std::string&
GeneProduct::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this GeneProduct.
 */
const std::string&
GeneProduct::getName() const
{
  return mName;
}


/*
 * Returns the value of the "label" attribute of this GeneProduct.
 */
const std::string&
GeneProduct::getLabel() const
{
  return mLabel;
}


/*
 * Returns the value of the "associatedSpecies" attribute of this GeneProduct.
 */
const std::string&
GeneProduct::getAssociatedSpecies() const
{
  return mAssociatedSpecies;
}


/*
 * Returns true/false if id is set.
 */
bool
GeneProduct::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
GeneProduct::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if label is set.
 */
bool
GeneProduct::isSetLabel() const
{
  return (mLabel.empty() == false);
}


/*
 * Returns true/false if associatedSpecies is set.
 */
bool
GeneProduct::isSetAssociatedSpecies() const
{
  return (mAssociatedSpecies.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
GeneProduct::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
GeneProduct::setName(const std::string& name)
{
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets label and returns value indicating success.
 */
int
GeneProduct::setLabel(const std::string& label)
{
  {
    mLabel = label;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets associatedSpecies and returns value indicating success.
 */
int
GeneProduct::setAssociatedSpecies(const std::string& associatedSpecies)
{
  if (!(SyntaxChecker::isValidInternalSId(associatedSpecies)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mAssociatedSpecies = associatedSpecies;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
GeneProduct::unsetId()
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
GeneProduct::unsetName()
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
 * Unsets label and returns value indicating success.
 */
int
GeneProduct::unsetLabel()
{
  mLabel.erase();

  if (mLabel.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets associatedSpecies and returns value indicating success.
 */
int
GeneProduct::unsetAssociatedSpecies()
{
  mAssociatedSpecies.erase();

  if (mAssociatedSpecies.empty() == true)
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
GeneProduct::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetAssociatedSpecies() == true && mAssociatedSpecies == oldid)
  {
    setAssociatedSpecies(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
GeneProduct::getElementName () const
{
  static const string name = "geneProduct";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
GeneProduct::getTypeCode () const
{
  return SBML_FBC_GENEPRODUCT;
}


/*
 * check if all the required attributes are set
 */
bool
GeneProduct::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetLabel() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
GeneProduct::writeElements (XMLOutputStream& stream) const
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
GeneProduct::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
GeneProduct::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
GeneProduct::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "label")
  {
    value = getLabel();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "associatedSpecies")
  {
    value = getAssociatedSpecies();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GeneProduct's attribute "attributeName"
 * is set.
 */
bool
GeneProduct::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "label")
  {
    value = isSetLabel();
  }
  else if (attributeName == "associatedSpecies")
  {
    value = isSetAssociatedSpecies();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "label")
  {
    return_value = setLabel(value);
  }
  else if (attributeName == "associatedSpecies")
  {
    return_value = setAssociatedSpecies(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this GeneProduct.
 */
int
GeneProduct::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "label")
  {
    value = unsetLabel();
  }
  else if (attributeName == "associatedSpecies")
  {
    value = unsetAssociatedSpecies();
  }

  return value;
}

/** @endcond */



  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
GeneProduct::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("label");
  attributes.add("associatedSpecies");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
GeneProduct::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the ListOfGeneProducts - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfGeneProducts*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedAttributes,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedCoreAttributes,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedAttributes,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedCoreAttributes,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
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
      logEmptyString(mId, getLevel(), getVersion(), "<GeneProduct>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Fbc attribute 'id' is missing from 'geneProduct' object.";
    getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedAttributes,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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
      logEmptyString(mName, getLevel(), getVersion(), "<GeneProduct>");
    }
  }

  //
  // label string   ( use = "required" )
  //
  assigned = attributes.readInto("label", mLabel);

  if (assigned == true)
  {
    // check string is not empty

    if (mLabel.empty() == true)
    {
      logEmptyString(mLabel, getLevel(), getVersion(), "<GeneProduct>");
    }
  }
  else
  {
    std::string message = "Fbc attribute 'label' is missing from 'geneProduct' object.";
    getErrorLog()->logPackageError("fbc", FbcGeneProductAllowedAttributes,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // associatedSpecies SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("associatedSpecies", mAssociatedSpecies);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mAssociatedSpecies.empty() == true)
    {
      logEmptyString(mAssociatedSpecies, getLevel(), getVersion(), "<GeneProduct>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mAssociatedSpecies) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute associatedSpecies='" + mAssociatedSpecies + "' does not conform.");
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
GeneProduct::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetLabel() == true)
    stream.writeAttribute("label", getPrefix(), mLabel);

  if (isSetAssociatedSpecies() == true)
    stream.writeAttribute("associatedSpecies", getPrefix(), mAssociatedSpecies);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfGeneProducts::ListOfGeneProducts(unsigned int level, 
                     unsigned int version, 
                     unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfGeneProducts::ListOfGeneProducts(FbcPkgNamespaces* fbcns)
  : ListOf(fbcns)
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Returns a deep copy of this ListOfGeneProducts 
 */
ListOfGeneProducts* 
ListOfGeneProducts::clone () const
 {
  return new ListOfGeneProducts(*this);
}


/*
 * Get a GeneProduct from the ListOfGeneProducts by index.
 */
GeneProduct*
ListOfGeneProducts::get(unsigned int n)
{
  return static_cast<GeneProduct*>(ListOf::get(n));
}


/*
 * Get a GeneProduct from the ListOfGeneProducts by index.
 */
const GeneProduct*
ListOfGeneProducts::get(unsigned int n) const
{
  return static_cast<const GeneProduct*>(ListOf::get(n));
}


/*
 * Get a GeneProduct from the ListOfGeneProducts by id.
 */
GeneProduct*
ListOfGeneProducts::get(const std::string& sid)
{
  return const_cast<GeneProduct*>(
    static_cast<const ListOfGeneProducts&>(*this).get(sid));
}


/*
 * Get a GeneProduct from the ListOfGeneProducts by id.
 */
const GeneProduct*
ListOfGeneProducts::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeneProduct>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <GeneProduct*> (*result);
}


/**
 * Adds a copy the given GeneProduct to this ListOfGeneProducts.
 *
 * @param gp the GeneProduct object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfGeneProducts::addGeneProduct(const GeneProduct* gp)
{
  if (gp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(gp)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
  return append(gp);
  }
}


/*
 * Get the number of GeneProduct objects in this ListOfGeneProducts.
 *
 * @return the number of GeneProduct objects in this ListOfGeneProducts
 */
unsigned int 
ListOfGeneProducts::getNumGeneProducts() const
{
  return size();
}


/*
 * Creates a new GeneProduct object, adds it to this ListOfGeneProducts
 * and returns the GeneProduct object created. 
 *
 * @return a new GeneProduct object instance
 *
 * @see addGeneProduct(const GeneProduct* gp)
 */
GeneProduct* 
ListOfGeneProducts::createGeneProduct()
{
  GeneProduct* gp = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    gp = new GeneProduct(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(gp != NULL)
  {
    appendAndOwn(gp);
  }

  return gp;
}

/*
 * Removes the nth GeneProduct from this ListOfGeneProducts
 */
GeneProduct*
ListOfGeneProducts::remove(unsigned int n)
{
  return static_cast<GeneProduct*>(ListOf::remove(n));
}


/*
 * Removes the GeneProduct from this ListOfGeneProducts with the given identifier
 */
GeneProduct*
ListOfGeneProducts::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeneProduct>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GeneProduct*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfGeneProducts::getElementName () const
{
  static const string name = "listOfGeneProducts";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfGeneProducts::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfGeneProducts::getItemTypeCode () const
{
  return SBML_FBC_GENEPRODUCT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GeneProduct in this ListOfGeneProducts
 */
SBase*
ListOfGeneProducts::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "geneProduct")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new GeneProduct(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Fbc package.
 */
void
ListOfGeneProducts::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(FbcExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(FbcExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */


LIBSBML_EXTERN
GeneProduct_t *
GeneProduct_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion)
{
  return new GeneProduct(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
GeneProduct_free(GeneProduct_t * gp)
{
  if (gp != NULL)
    delete gp;
}


LIBSBML_EXTERN
GeneProduct_t *
GeneProduct_clone(GeneProduct_t * gp)
{
  if (gp != NULL)
  {
    return static_cast<GeneProduct_t*>(gp->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
GeneProduct_getId(const GeneProduct_t * gp)
{
  return (gp != NULL && gp->isSetId()) ? gp->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProduct_getName(const GeneProduct_t * gp)
{
  return (gp != NULL && gp->isSetName()) ? gp->getName().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProduct_getLabel(const GeneProduct_t * gp)
{
  return (gp != NULL && gp->isSetLabel()) ? gp->getLabel().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProduct_getAssociatedSpecies(const GeneProduct_t * gp)
{
  return (gp != NULL && gp->isSetAssociatedSpecies()) ? gp->getAssociatedSpecies().c_str() : NULL;
}


LIBSBML_EXTERN
int
GeneProduct_isSetId(const GeneProduct_t * gp)
{
  return (gp != NULL) ? static_cast<int>(gp->isSetId()) : 0;
}


LIBSBML_EXTERN
int
GeneProduct_isSetName(const GeneProduct_t * gp)
{
  return (gp != NULL) ? static_cast<int>(gp->isSetName()) : 0;
}


LIBSBML_EXTERN
int
GeneProduct_isSetLabel(const GeneProduct_t * gp)
{
  return (gp != NULL) ? static_cast<int>(gp->isSetLabel()) : 0;
}


LIBSBML_EXTERN
int
GeneProduct_isSetAssociatedSpecies(const GeneProduct_t * gp)
{
  return (gp != NULL) ? static_cast<int>(gp->isSetAssociatedSpecies()) : 0;
}


LIBSBML_EXTERN
int
GeneProduct_setId(GeneProduct_t * gp, const char * id)
{
  if (gp != NULL)
    return (id == NULL) ? gp->setId("") : gp->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_setName(GeneProduct_t * gp, const char * name)
{
  if (gp != NULL)
    return (name == NULL) ? gp->setName("") : gp->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_setLabel(GeneProduct_t * gp, const char * label)
{
  if (gp != NULL)
    return (label == NULL) ? gp->setLabel("") : gp->setLabel(label);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_setAssociatedSpecies(GeneProduct_t * gp, const char * associatedSpecies)
{
  if (gp != NULL)
    return (associatedSpecies == NULL) ? gp->setAssociatedSpecies("") : gp->setAssociatedSpecies(associatedSpecies);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_unsetId(GeneProduct_t * gp)
{
  return (gp != NULL) ? gp->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_unsetName(GeneProduct_t * gp)
{
  return (gp != NULL) ? gp->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_unsetLabel(GeneProduct_t * gp)
{
  return (gp != NULL) ? gp->unsetLabel() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_unsetAssociatedSpecies(GeneProduct_t * gp)
{
  return (gp != NULL) ? gp->unsetAssociatedSpecies() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProduct_hasRequiredAttributes(const GeneProduct_t * gp)
{
  return (gp != NULL) ? static_cast<int>(gp->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
GeneProduct_t *
ListOfGeneProducts_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfGeneProducts *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
GeneProduct_t *
ListOfGeneProducts_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfGeneProducts *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */



