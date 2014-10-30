/**
 * @file:   Dimension.cpp
 * @brief:  Implementation of the Dimension class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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


#include <sbml/packages/arrays/sbml/Dimension.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new Dimension with the given level, version, and package version.
 */
Dimension::Dimension (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mId ("")
   ,mName ("")
   ,mSize ("")
   ,mArrayDimension (SBML_INT_MAX)
   ,mIsSetArrayDimension (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new Dimension with the given ArraysPkgNamespaces object.
 */
Dimension::Dimension (ArraysPkgNamespaces* arraysns)
  : SBase(arraysns)
   ,mId ("")
   ,mName ("")
   ,mSize ("")
   ,mArrayDimension (SBML_INT_MAX)
   ,mIsSetArrayDimension (false)
{
  // set the element namespace of this object
  setElementNamespace(arraysns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(arraysns);
}


/*
 * Copy constructor for Dimension.
 */
Dimension::Dimension (const Dimension& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mName  = orig.mName;
    mSize  = orig.mSize;
    mArrayDimension  = orig.mArrayDimension;
    mIsSetArrayDimension  = orig.mIsSetArrayDimension;
  }
}


/*
 * Assignment for Dimension.
 */
Dimension&
Dimension::operator=(const Dimension& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mSize  = rhs.mSize;
    mArrayDimension  = rhs.mArrayDimension;
    mIsSetArrayDimension  = rhs.mIsSetArrayDimension;
  }
  return *this;
}


/*
 * Clone for Dimension.
 */
Dimension*
Dimension::clone () const
{
  return new Dimension(*this);
}


/*
 * Destructor for Dimension.
 */
Dimension::~Dimension ()
{
}


/*
 * Returns the value of the "id" attribute of this Dimension.
 */
const std::string&
Dimension::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Dimension.
 */
const std::string&
Dimension::getName() const
{
  return mName;
}


/*
 * Returns the value of the "size" attribute of this Dimension.
 */
const std::string&
Dimension::getSize() const
{
  return mSize;
}


/*
 * Returns the value of the "arrayDimension" attribute of this Dimension.
 */
unsigned int
Dimension::getArrayDimension() const
{
  return mArrayDimension;
}


/*
 * Returns true/false if id is set.
 */
bool
Dimension::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
Dimension::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if size is set.
 */
bool
Dimension::isSetSize() const
{
  return (mSize.empty() == false);
}


/*
 * Returns true/false if arrayDimension is set.
 */
bool
Dimension::isSetArrayDimension() const
{
  return mIsSetArrayDimension;
}


/*
 * Sets id and returns value indicating success.
 */
int
Dimension::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
Dimension::setName(const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets size and returns value indicating success.
 */
int
Dimension::setSize(const std::string& size)
{
  if (&(size) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(size)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSize = size;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets arrayDimension and returns value indicating success.
 */
int
Dimension::setArrayDimension(unsigned int arrayDimension)
{
  mArrayDimension = arrayDimension;
  mIsSetArrayDimension = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
Dimension::unsetId()
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
Dimension::unsetName()
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
 * Unsets size and returns value indicating success.
 */
int
Dimension::unsetSize()
{
  mSize.erase();

  if (mSize.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets arrayDimension and returns value indicating success.
 */
int
Dimension::unsetArrayDimension()
{
  mArrayDimension = SBML_INT_MAX;
  mIsSetArrayDimension = false;

  if (isSetArrayDimension() == false)
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
Dimension::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetSize() == true && mSize == oldid)
  {
    setSize(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
Dimension::getElementName () const
{
  static const string name = "dimension";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Dimension::getTypeCode () const
{
  return SBML_ARRAYS_DIMENSION;
}


/*
 * check if all the required attributes are set
 */
bool
Dimension::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetSize() == false)
    allPresent = false;

  if (isSetArrayDimension() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Dimension::writeElements (XMLOutputStream& stream) const
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
Dimension::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Dimension::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Dimension::enablePackageInternal(const std::string& pkgURI,
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
Dimension::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("size");
  attributes.add("arrayDimension");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Dimension::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfDimensions - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfDimensions*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
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
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
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
      logEmptyString(mId, getLevel(), getVersion(), "<Dimension>");
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
      logEmptyString(mName, getLevel(), getVersion(), "<Dimension>");
    }
  }

  //
  // size SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("size", mSize);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mSize.empty() == true)
    {
      logEmptyString(mSize, getLevel(), getVersion(), "<Dimension>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSize) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute size='" + mSize + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Arrays attribute 'size' is missing.";
    getErrorLog()->logPackageError("arrays", ArraysUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

  //
  // arrayDimension unsigned int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetArrayDimension = attributes.readInto("arrayDimension", mArrayDimension);

  if (mIsSetArrayDimension == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Arrays attribute 'arrayDimension' is missing.";
        getErrorLog()->logPackageError("arrays", ArraysUnknownError,
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
Dimension::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetSize() == true)
    stream.writeAttribute("size", getPrefix(), mSize);

  if (isSetArrayDimension() == true)
    stream.writeAttribute("arrayDimension", getPrefix(), mArrayDimension);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfDimensions::ListOfDimensions(unsigned int level, 
                   unsigned int version, 
                   unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfDimensions::ListOfDimensions(ArraysPkgNamespaces* arraysns)
  : ListOf(arraysns)
{
  setElementNamespace(arraysns->getURI());
}


/*
 * Returns a deep copy of this ListOfDimensions 
 */
ListOfDimensions* 
ListOfDimensions::clone () const
 {
  return new ListOfDimensions(*this);
}


/*
 * Get a Dimension from the ListOfDimensions by index.
*/
Dimension*
ListOfDimensions::get(unsigned int n)
{
  return static_cast<Dimension*>(ListOf::get(n));
}


/*
 * Get a Dimension from the ListOfDimensions by index.
 */
const Dimension*
ListOfDimensions::get(unsigned int n) const
{
  return static_cast<const Dimension*>(ListOf::get(n));
}


/*
 * Get a Dimension from the ListOfDimensions by id.
 */
Dimension*
ListOfDimensions::get(const std::string& sid)
{
  return const_cast<Dimension*>(
    static_cast<const ListOfDimensions&>(*this).get(sid));
}


/*
 * Get a Dimension from the ListOfDimensions by id.
 */
const Dimension*
ListOfDimensions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Dimension>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Dimension*> (*result);
}


/*
 * Removes the nth Dimension from this ListOfDimensions
 */
Dimension*
ListOfDimensions::remove(unsigned int n)
{
  return static_cast<Dimension*>(ListOf::remove(n));
}


/*
 * Removes the Dimension from this ListOfDimensions with the given identifier
 */
Dimension*
ListOfDimensions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Dimension>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Dimension*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfDimensions::getElementName () const
{
  static const string name = "listOfDimensions";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfDimensions::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfDimensions::getItemTypeCode () const
{
  return SBML_ARRAYS_DIMENSION;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Dimension in this ListOfDimensions
 */
SBase*
ListOfDimensions::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "dimension")
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    object = new Dimension(arraysns);
    appendAndOwn(object);
    delete arraysns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Arrays package.
 */
void
ListOfDimensions::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(ArraysExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(ArraysExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


/*
 * 
 */
LIBSBML_EXTERN
Dimension_t *
Dimension_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion)
{
  return new Dimension(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
Dimension_free(Dimension_t * d)
{
  if (d != NULL)
    delete d;
}


/*
 *
 */
LIBSBML_EXTERN
Dimension_t *
Dimension_clone(Dimension_t * d)
{
  if (d != NULL)
  {
    return static_cast<Dimension_t*>(d->clone());
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
Dimension_getId(Dimension_t * d)
{
  if (d == NULL)
    return NULL;

  return d->getId().empty() ? NULL : safe_strdup(d->getId().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
char *
Dimension_getName(Dimension_t * d)
{
  if (d == NULL)
    return NULL;

  return d->getName().empty() ? NULL : safe_strdup(d->getName().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
char *
Dimension_getSize(Dimension_t * d)
{
  if (d == NULL)
    return NULL;

  return d->getSize().empty() ? NULL : safe_strdup(d->getSize().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
unsigned int
Dimension_getArrayDimension(Dimension_t * d)
{
  return (d != NULL) ? d->getArrayDimension() : SBML_INT_MAX;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_isSetId(Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetId()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_isSetName(Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetName()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_isSetSize(Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetSize()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_isSetArrayDimension(Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetArrayDimension()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_setId(Dimension_t * d, const char * id)
{
  return (d != NULL) ? d->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_setName(Dimension_t * d, const char * name)
{
  return (d != NULL) ? d->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_setSize(Dimension_t * d, const char * size)
{
  return (d != NULL) ? d->setSize(size) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_setArrayDimension(Dimension_t * d, unsigned int arrayDimension)
{
  return (d != NULL) ? d->setArrayDimension(arrayDimension) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_unsetId(Dimension_t * d)
{
  return (d != NULL) ? d->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_unsetName(Dimension_t * d)
{
  return (d != NULL) ? d->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_unsetSize(Dimension_t * d)
{
  return (d != NULL) ? d->unsetSize() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_unsetArrayDimension(Dimension_t * d)
{
  return (d != NULL) ? d->unsetArrayDimension() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Dimension_hasRequiredAttributes(Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
Dimension_t *
ListOfDimensions_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDimensions *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
Dimension_t *
ListOfDimensions_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDimensions *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


