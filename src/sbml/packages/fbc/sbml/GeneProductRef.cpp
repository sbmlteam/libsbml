/**
 * @file   GeneProductRef.cpp
 * @brief  Implementation of the GeneProductRef class
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

#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new GeneProductRef with the given level, version, and package version.
 */
GeneProductRef::GeneProductRef (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : FbcAssociation(level, version)
//  , mId ("")
  , mGeneProduct ("")
//  , mName ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new GeneProductRef with the given FbcPkgNamespaces object.
 */
GeneProductRef::GeneProductRef (FbcPkgNamespaces* fbcns)
  : FbcAssociation(fbcns)
//  , mId ("")
  , mGeneProduct ("")
//  , mName ("")
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for GeneProductRef.
 */
GeneProductRef::GeneProductRef (const GeneProductRef& orig)
  : FbcAssociation(orig)
{
  mId  = orig.mId;
  mGeneProduct  = orig.mGeneProduct;
  mName  = orig.mName;
}


/*
 * Assignment for GeneProductRef.
 */
GeneProductRef&
GeneProductRef::operator=(const GeneProductRef& rhs)
{
  if (&rhs != this)
  {
    FbcAssociation::operator=(rhs);
    mId  = rhs.mId;
    mGeneProduct  = rhs.mGeneProduct;
    mName  = rhs.mName;
  }
  return *this;
}


/*
 * Clone for GeneProductRef.
 */
GeneProductRef*
GeneProductRef::clone () const
{
  return new GeneProductRef(*this);
}


/*
 * Destructor for GeneProductRef.
 */
GeneProductRef::~GeneProductRef ()
{
}


/*
 * Returns the value of the "id" attribute of this GeneProductRef.
 */
const std::string&
GeneProductRef::getId() const
{
  return mId;
}


/*
 * Returns the value of the "geneProduct" attribute of this GeneProductRef.
 */
const std::string&
GeneProductRef::getGeneProduct() const
{
  return mGeneProduct;
}


std::string 
GeneProductRef::toInfix(bool usingId) const
{
  const SBMLDocument* doc = getSBMLDocument();
  if (doc == NULL) return mGeneProduct;

  const Model* model = doc->getModel();
  if (model == NULL) return mGeneProduct;

  const FbcModelPlugin* plug = dynamic_cast<const FbcModelPlugin*>(model->getPlugin("fbc"));
  if (plug == NULL) return mGeneProduct;
  const GeneProduct* product = plug->getGeneProduct(mGeneProduct);
  if (product == NULL) return mGeneProduct;

  if (usingId)
  {
    return product->getId();
  }
  else
  {
    return product->getLabel();
  }
}



/*
 * Returns the value of the "name" attribute of this GeneProductRef.
 */
const std::string&
GeneProductRef::getName() const
{
  return mName;
}


/*
 * Returns true/false if id is set.
 */
bool
GeneProductRef::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if geneProduct is set.
 */
bool
GeneProductRef::isSetGeneProduct() const
{
  return (mGeneProduct.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
GeneProductRef::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
GeneProductRef::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets geneProduct and returns value indicating success.
 */
int
GeneProductRef::setGeneProduct(const std::string& geneProduct)
{
  if (!(SyntaxChecker::isValidInternalSId(geneProduct)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mGeneProduct = geneProduct;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets name and returns value indicating success.
 */
int
GeneProductRef::setName(const std::string& name)
{
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
GeneProductRef::unsetId()
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
 * Unsets geneProduct and returns value indicating success.
 */
int
GeneProductRef::unsetGeneProduct()
{
  mGeneProduct.erase();

  if (mGeneProduct.empty() == true)
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
GeneProductRef::unsetName()
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
 * rename attributes that are SIdRefs or instances in math
 */
void
GeneProductRef::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  FbcAssociation::renameSIdRefs(oldid, newid);
  if (isSetGeneProduct() == true && mGeneProduct == oldid)
  {
    setGeneProduct(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
GeneProductRef::getElementName () const
{
  static const string name = "geneProductRef";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
GeneProductRef::getTypeCode () const
{
  return SBML_FBC_GENEPRODUCTREF;
}


/*
 * check if all the required attributes are set
 */
bool
GeneProductRef::hasRequiredAttributes () const
{
  bool allPresent = FbcAssociation::hasRequiredAttributes();

  if (isSetGeneProduct() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
GeneProductRef::writeElements (XMLOutputStream& stream) const
{
  FbcAssociation::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
GeneProductRef::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
GeneProductRef::setSBMLDocument (SBMLDocument* d)
{
  FbcAssociation::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
GeneProductRef::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  FbcAssociation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

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
  else if (attributeName == "geneProduct")
  {
    value = getGeneProduct();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GeneProductRef's attribute
 * "attributeName" is set.
 */
bool
GeneProductRef::isSetAttribute(const std::string& attributeName) const
{
  bool value = FbcAssociation::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "geneProduct")
  {
    value = isSetGeneProduct();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::setAttribute(const std::string& attributeName, int value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::setAttribute(const std::string& attributeName, double value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "geneProduct")
  {
    return_value = setGeneProduct(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this GeneProductRef.
 */
int
GeneProductRef::unsetAttribute(const std::string& attributeName)
{
  int value = FbcAssociation::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "geneProduct")
  {
    value = unsetGeneProduct();
  }

  return value;
}

/** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
GeneProductRef::createObject(XMLInputStream& stream)
{
  SBase* object = FbcAssociation::createObject(stream);

  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
GeneProductRef::addExpectedAttributes(ExpectedAttributes& attributes)
{
  FbcAssociation::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("geneProduct");
  attributes.add("name");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
GeneProductRef::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  FbcAssociation::readAttributes(attributes, expectedAttributes);

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
        getErrorLog()->logPackageError("fbc", FbcGeneProdRefAllowedAttribs,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProdRefAllowedCoreAttribs,
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
  // id SId  ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<GeneProductRef>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }

  //
  // geneProduct SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("geneProduct", mGeneProduct);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mGeneProduct.empty() == true)
    {
      logEmptyString(mGeneProduct, getLevel(), getVersion(), "<GeneProductRef>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mGeneProduct) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(FbcGeneProdRefGeneProductSIdRef, getLevel(), getVersion(), 
        "The syntax of the attribute geneProduct='" + mGeneProduct + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Fbc attribute 'geneProduct' is missing from 'geneProductRef' object.";
    getErrorLog()->logPackageError("fbc", FbcGeneProdRefAllowedAttribs,
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
      logEmptyString(mName, getLevel(), getVersion(), "<GeneProductRef>");
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
GeneProductRef::writeAttributes (XMLOutputStream& stream) const
{
  FbcAssociation::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetGeneProduct() == true)
    stream.writeAttribute("geneProduct", getPrefix(), mGeneProduct);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

}


  /** @endcond */


LIBSBML_EXTERN
GeneProductRef_t *
GeneProductRef_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new GeneProductRef(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
GeneProductRef_free(GeneProductRef_t * gpr)
{
  if (gpr != NULL)
    delete gpr;
}


LIBSBML_EXTERN
GeneProductRef_t *
GeneProductRef_clone(GeneProductRef_t * gpr)
{
  if (gpr != NULL)
  {
    return static_cast<GeneProductRef_t*>(gpr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
GeneProductRef_getId(const GeneProductRef_t * gpr)
{
  return (gpr != NULL && gpr->isSetId()) ? gpr->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProductRef_getGeneProduct(const GeneProductRef_t * gpr)
{
  return (gpr != NULL && gpr->isSetGeneProduct()) ? gpr->getGeneProduct().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProductRef_getName(const GeneProductRef_t * gpr)
{
  return (gpr != NULL && gpr->isSetName()) ? gpr->getName().c_str() : NULL;
}


LIBSBML_EXTERN
int
GeneProductRef_isSetId(const GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? static_cast<int>(gpr->isSetId()) : 0;
}


LIBSBML_EXTERN
int
GeneProductRef_isSetGeneProduct(const GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? static_cast<int>(gpr->isSetGeneProduct()) : 0;
}


LIBSBML_EXTERN
int
GeneProductRef_isSetName(const GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? static_cast<int>(gpr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
GeneProductRef_setId(GeneProductRef_t * gpr, const char * id)
{
  if (gpr != NULL)
    return (id == NULL) ? gpr->setId("") : gpr->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_setGeneProduct(GeneProductRef_t * gpr, const char * geneProduct)
{
  if (gpr != NULL)
    return (geneProduct == NULL) ? gpr->setGeneProduct("") : gpr->setGeneProduct(geneProduct);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_setName(GeneProductRef_t * gpr, const char * name)
{
  if (gpr != NULL)
    return (name == NULL) ? gpr->setName("") : gpr->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_unsetId(GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? gpr->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_unsetGeneProduct(GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? gpr->unsetGeneProduct() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_unsetName(GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? gpr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductRef_hasRequiredAttributes(const GeneProductRef_t * gpr)
{
  return (gpr != NULL) ? static_cast<int>(gpr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */



