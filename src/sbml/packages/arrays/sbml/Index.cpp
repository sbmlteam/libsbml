/**
 * @file:   Index.cpp
 * @brief:  Implementation of the Index class
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


#include <sbml/packages/arrays/sbml/Index.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>
#include <sbml/math/MathML.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new Index with the given level, version, and package version.
 */
Index::Index (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mReferencedAttribute ("")
   ,mArrayDimension (SBML_INT_MAX)
   ,mIsSetArrayDimension (false)
   ,mMath (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new Index with the given ArraysPkgNamespaces object.
 */
Index::Index (ArraysPkgNamespaces* arraysns)
  : SBase(arraysns)
   ,mReferencedAttribute ("")
   ,mArrayDimension (SBML_INT_MAX)
   ,mIsSetArrayDimension (false)
   ,mMath (NULL)
{
  // set the element namespace of this object
  setElementNamespace(arraysns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(arraysns);
}


/*
 * Copy constructor for Index.
 */
Index::Index (const Index& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mReferencedAttribute  = orig.mReferencedAttribute;
    mArrayDimension  = orig.mArrayDimension;
    mIsSetArrayDimension  = orig.mIsSetArrayDimension;
    if (orig.mMath != NULL)
    {
      mMath = orig.mMath->deepCopy();
    }
    else
    {
      mMath = NULL;
    }
  }
}


/*
 * Assignment for Index.
 */
Index&
Index::operator=(const Index& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mReferencedAttribute  = rhs.mReferencedAttribute;
    mArrayDimension  = rhs.mArrayDimension;
    mIsSetArrayDimension  = rhs.mIsSetArrayDimension;
    if (rhs.mMath != NULL)
    {
      mMath = rhs.mMath->deepCopy();
    }
    else
    {
      mMath = NULL;
    }
  }
  return *this;
}


/*
 * Clone for Index.
 */
Index*
Index::clone () const
{
  return new Index(*this);
}


/*
 * Destructor for Index.
 */
Index::~Index ()
{
  delete mMath;
}


/*
 * Returns the value of the "referencedAttribute" attribute of this Index.
 */
const std::string&
Index::getReferencedAttribute() const
{
  return mReferencedAttribute;
}


/*
 * Returns the value of the "arrayDimension" attribute of this Index.
 */
unsigned int
Index::getArrayDimension() const
{
  return mArrayDimension;
}


/*
 * Returns the value of the "math" attribute of this Index.
 */
const ASTNode*
Index::getMath() const
{
  return mMath;
}

ASTNode*
Index::getMath()
{
  return mMath;
}



/*
 * Returns true/false if referencedAttribute is set.
 */
bool
Index::isSetReferencedAttribute() const
{
  return (mReferencedAttribute.empty() == false);
}


/*
 * Returns true/false if arrayDimension is set.
 */
bool
Index::isSetArrayDimension() const
{
  return mIsSetArrayDimension;
}


/*
 * Returns true/false if math is set.
 */
bool
Index::isSetMath() const
{
  return (mMath != NULL);
}


/*
 * Sets referencedAttribute and returns value indicating success.
 */
int
Index::setReferencedAttribute(const std::string& referencedAttribute)
{
  if (&(referencedAttribute) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mReferencedAttribute = referencedAttribute;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets arrayDimension and returns value indicating success.
 */
int
Index::setArrayDimension(unsigned int arrayDimension)
{
  mArrayDimension = arrayDimension;
  mIsSetArrayDimension = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets math and returns value indicating success.
 */
int
Index::setMath(ASTNode* math)
{
  if (mMath == math)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != NULL) ?
      math->deepCopy() : NULL;
    if (mMath != NULL)
    {
      mMath->setParentSBMLObject(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets referencedAttribute and returns value indicating success.
 */
int
Index::unsetReferencedAttribute()
{
  mReferencedAttribute.erase();

  if (mReferencedAttribute.empty() == true)
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
Index::unsetArrayDimension()
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
 * Unsets math and returns value indicating success.
 */
int
Index::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
Index::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetMath() == true)
  {
    getMath()->renameSIdRefs(oldid, newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
Index::getElementName () const
{
  static const string name = "index";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Index::getTypeCode () const
{
  return SBML_ARRAYS_INDEX;
}


/*
 * check if all the required attributes are set
 */
bool
Index::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetReferencedAttribute() == false)
    allPresent = false;

  if (isSetArrayDimension() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
Index::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetMath() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Index::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (isSetMath() == true)
  {
    writeMathML(getMath(), stream, getSBMLNamespaces());
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
Index::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Index::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Index::enablePackageInternal(const std::string& pkgURI,
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
Index::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("referencedAttribute");
  attributes.add("arrayDimension");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Index::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfIndices - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfIndices*>(getParentSBMLObject())->size() < 2)
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
  // referencedAttribute string   ( use = "required" )
  //
  assigned = attributes.readInto("referencedAttribute", mReferencedAttribute);

  if (assigned == true)
  {
    // check string is not empty

    if (mReferencedAttribute.empty() == true)
    {
      logEmptyString(mReferencedAttribute, getLevel(), getVersion(), "<Index>");
    }
  }
  else
  {
    std::string message = "Arrays attribute 'referencedAttribute' is missing.";
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

bool
Index::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);

    if (stream.getSBMLNamespaces() == NULL)
    {
      stream.setSBMLNamespaces(new SBMLNamespaces(getLevel(), getVersion()));
    }

    delete mMath;
    mMath = readMathML(stream, prefix);
    if (mMath != NULL)
    {
      mMath->setParentSBMLObject(this);
    }
    read = true;
  }

  if (SBase::readOtherXML(stream))
  {
    read = true;
  }
  return read;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
Index::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetReferencedAttribute() == true)
    stream.writeAttribute("referencedAttribute", getPrefix(), mReferencedAttribute);

  if (isSetArrayDimension() == true)
    stream.writeAttribute("arrayDimension", getPrefix(), mArrayDimension);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfIndices::ListOfIndices(unsigned int level, 
               unsigned int version, 
               unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfIndices::ListOfIndices(ArraysPkgNamespaces* arraysns)
  : ListOf(arraysns)
{
  setElementNamespace(arraysns->getURI());
}


/*
 * Returns a deep copy of this ListOfIndices 
 */
ListOfIndices* 
ListOfIndices::clone () const
 {
  return new ListOfIndices(*this);
}


/*
 * Get a Index from the ListOfIndices by index.
*/
Index*
ListOfIndices::get(unsigned int n)
{
  return static_cast<Index*>(ListOf::get(n));
}


/*
 * Get a Index from the ListOfIndices by index.
 */
const Index*
ListOfIndices::get(unsigned int n) const
{
  return static_cast<const Index*>(ListOf::get(n));
}


/*
 * Get a Index from the ListOfIndices by id.
 */
Index*
ListOfIndices::get(const std::string& sid)
{
  return const_cast<Index*>(
    static_cast<const ListOfIndices&>(*this).get(sid));
}


/*
 * Get a Index from the ListOfIndices by id.
 */
const Index*
ListOfIndices::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Index>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Index*> (*result);
}


/*
 * Removes the nth Index from this ListOfIndices
 */
Index*
ListOfIndices::remove(unsigned int n)
{
  return static_cast<Index*>(ListOf::remove(n));
}


/*
 * Removes the Index from this ListOfIndices with the given identifier
 */
Index*
ListOfIndices::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Index>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Index*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfIndices::getElementName () const
{
  static const string name = "listOfIndices";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfIndices::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfIndices::getItemTypeCode () const
{
  return SBML_ARRAYS_INDEX;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Index in this ListOfIndices
 */
SBase*
ListOfIndices::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "index")
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    object = new Index(arraysns);
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
ListOfIndices::writeXMLNS(XMLOutputStream& stream) const
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
Index_t *
Index_create(unsigned int level, unsigned int version,
             unsigned int pkgVersion)
{
  return new Index(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
Index_free(Index_t * i)
{
  if (i != NULL)
    delete i;
}


/*
 *
 */
LIBSBML_EXTERN
Index_t *
Index_clone(Index_t * i)
{
  if (i != NULL)
  {
    return static_cast<Index_t*>(i->clone());
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
Index_getReferencedAttribute(Index_t * i)
{
  if (i == NULL)
    return NULL;

  return i->getReferencedAttribute().empty() ? NULL : safe_strdup(i->getReferencedAttribute().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
unsigned int
Index_getArrayDimension(Index_t * i)
{
  return (i != NULL) ? i->getArrayDimension() : SBML_INT_MAX;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_isSetReferencedAttribute(Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetReferencedAttribute()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_isSetArrayDimension(Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetArrayDimension()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_setReferencedAttribute(Index_t * i, const char * referencedAttribute)
{
  return (i != NULL) ? i->setReferencedAttribute(referencedAttribute) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_setArrayDimension(Index_t * i, unsigned int arrayDimension)
{
  return (i != NULL) ? i->setArrayDimension(arrayDimension) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_unsetReferencedAttribute(Index_t * i)
{
  return (i != NULL) ? i->unsetReferencedAttribute() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_unsetArrayDimension(Index_t * i)
{
  return (i != NULL) ? i->unsetArrayDimension() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Index_hasRequiredAttributes(Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
Index_t *
ListOfIndices_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfIndices *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
Index_t *
ListOfIndices_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfIndices *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


