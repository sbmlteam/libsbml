/**
 * @file:   AnalyticVolume.cpp
 * @brief:  Implementation of the AnalyticVolume class
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


#include <sbml/packages/spatial/sbml/AnalyticVolume.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/math/MathML.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new AnalyticVolume with the given level, version, and package version.
 */
AnalyticVolume::AnalyticVolume (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mFunctionType (FUNCTIONKIND_UNKNOWN)
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mDomainType ("")
  , mMath (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new AnalyticVolume with the given SpatialPkgNamespaces object.
 */
AnalyticVolume::AnalyticVolume (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mFunctionType (FUNCTIONKIND_UNKNOWN)
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mDomainType ("")
  , mMath (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AnalyticVolume.
 */
AnalyticVolume::AnalyticVolume (const AnalyticVolume& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mFunctionType  = orig.mFunctionType;
    mOrdinal  = orig.mOrdinal;
    mIsSetOrdinal  = orig.mIsSetOrdinal;
    mDomainType  = orig.mDomainType;
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
 * Assignment for AnalyticVolume.
 */
AnalyticVolume&
AnalyticVolume::operator=(const AnalyticVolume& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mFunctionType  = rhs.mFunctionType;
    mOrdinal  = rhs.mOrdinal;
    mIsSetOrdinal  = rhs.mIsSetOrdinal;
    mDomainType  = rhs.mDomainType;
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
 * Clone for AnalyticVolume.
 */
AnalyticVolume*
AnalyticVolume::clone () const
{
  return new AnalyticVolume(*this);
}


/*
 * Destructor for AnalyticVolume.
 */
AnalyticVolume::~AnalyticVolume ()
{
  delete mMath;
  mMath = NULL;
}


/*
 * Returns the value of the "id" attribute of this AnalyticVolume.
 */
const std::string&
AnalyticVolume::getId() const
{
  return mId;
}


/*
 * Returns the value of the "functionType" attribute of this AnalyticVolume.
 */
FunctionKind_t
AnalyticVolume::getFunctionType() const
{
  return mFunctionType;
}


/*
 * Returns the value of the "ordinal" attribute of this AnalyticVolume.
 */
int
AnalyticVolume::getOrdinal() const
{
  return mOrdinal;
}


/*
 * Returns the value of the "domainType" attribute of this AnalyticVolume.
 */
const std::string&
AnalyticVolume::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "math" attribute of this AnalyticVolume.
 */
const ASTNode*
AnalyticVolume::getMath() const
{
  return mMath;
}


/*
 * Returns true/false if id is set.
 */
bool
AnalyticVolume::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if functionType is set.
 */
bool
AnalyticVolume::isSetFunctionType() const
{
  return mFunctionType != FUNCTIONKIND_UNKNOWN;
}


/*
 * Returns true/false if ordinal is set.
 */
bool
AnalyticVolume::isSetOrdinal() const
{
  return mIsSetOrdinal;
}


/*
 * Returns true/false if domainType is set.
 */
bool
AnalyticVolume::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Returns true/false if math is set.
 */
bool
AnalyticVolume::isSetMath() const
{
  return (mMath != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
AnalyticVolume::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets functionType and returns value indicating success.
 */
int
AnalyticVolume::setFunctionType(FunctionKind_t functionType)
{
  mFunctionType = functionType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets functionType and returns value indicating success.
 */
int
AnalyticVolume::setFunctionType(const std::string& functionType)
{
  FunctionKind_t parsed = FunctionKind_parse(functionType.c_str());
  if (parsed == FUNCTIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mFunctionType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets ordinal and returns value indicating success.
 */
int
AnalyticVolume::setOrdinal(int ordinal)
{
  mOrdinal = ordinal;
  mIsSetOrdinal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets domainType and returns value indicating success.
 */
int
AnalyticVolume::setDomainType(const std::string& domainType)
{
  if (&(domainType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets math and returns value indicating success.
 */
int
AnalyticVolume::setMath(const ASTNode* math)
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
 * Unsets id and returns value indicating success.
 */
int
AnalyticVolume::unsetId()
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
 * Unsets functionType and returns value indicating success.
 */
int
AnalyticVolume::unsetFunctionType()
{
  mFunctionType = FUNCTIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets ordinal and returns value indicating success.
 */
int
AnalyticVolume::unsetOrdinal()
{
  mOrdinal = SBML_INT_MAX;
  mIsSetOrdinal = false;

  if (isSetOrdinal() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets domainType and returns value indicating success.
 */
int
AnalyticVolume::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
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
AnalyticVolume::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
AnalyticVolume::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() == true && mDomainType == oldid)
  {
    setDomainType(newid);
  }

  if (isSetMath() == true)
  {
    mMath->renameSIdRefs(oldid, newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
AnalyticVolume::getElementName () const
{
  static const string name = "analyticVolume";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
AnalyticVolume::getTypeCode () const
{
  return SBML_SPATIAL_ANALYTICVOLUME;
}


/*
 * check if all the required attributes are set
 */
bool
AnalyticVolume::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetFunctionType() == false)
    allPresent = false;

  if (isSetOrdinal() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
AnalyticVolume::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
AnalyticVolume::writeElements (XMLOutputStream& stream) const
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
AnalyticVolume::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
AnalyticVolume::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
AnalyticVolume::enablePackageInternal(const std::string& pkgURI,
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
AnalyticVolume::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("functionType");
  attributes.add("ordinal");
  attributes.add("domainType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
AnalyticVolume::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfAnalyticVolumes - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfAnalyticVolumes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
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
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
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
      logEmptyString(mId, getLevel(), getVersion(), "<AnalyticVolume>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'analyticVolume' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // functionType enum  ( use = "required" )
  //
   mFunctionType = FUNCTIONKIND_UNKNOWN;
   std::string stringValue;
   assigned = attributes.readInto("functionType", stringValue);

   if (assigned == true)
   {
     // parse enum

     mFunctionType = FunctionKind_parse(stringValue.c_str());
     if(mFunctionType == FUNCTIONKIND_UNKNOWN) {
       std::string message = "Unknown value for spatial attribute 'functionType' in 'analyticVolume' object: " + stringValue;
       getErrorLog()->logPackageError("spatial", SpatialUnknownError,
         getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
     }
   }
   if(mFunctionType == FUNCTIONKIND_UNKNOWN)
   {
    std::string message = "Spatial attribute 'functionType' is missing from 'analyticVolume' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // ordinal int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetOrdinal = attributes.readInto("ordinal", mOrdinal);

  if (mIsSetOrdinal == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'ordinal' is missing from 'analyticVolume' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

  //
  // domainType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, getLevel(), getVersion(), "<AnalyticVolume>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'analyticVolume' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

bool
AnalyticVolume::readOtherXML (XMLInputStream& stream)
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
AnalyticVolume::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetFunctionType() == true)
    stream.writeAttribute("functionType", getPrefix(), FunctionKind_toString(mFunctionType));

  if (isSetOrdinal() == true)
    stream.writeAttribute("ordinal", getPrefix(), mOrdinal);

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(unsigned int level, 
                        unsigned int version, 
                        unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfAnalyticVolumes 
 */
ListOfAnalyticVolumes* 
ListOfAnalyticVolumes::clone () const
 {
  return new ListOfAnalyticVolumes(*this);
}


/*
 * Get a AnalyticVolume from the ListOfAnalyticVolumes by index.
*/
AnalyticVolume*
ListOfAnalyticVolumes::get(unsigned int n)
{
  return static_cast<AnalyticVolume*>(ListOf::get(n));
}


/*
 * Get a AnalyticVolume from the ListOfAnalyticVolumes by index.
 */
const AnalyticVolume*
ListOfAnalyticVolumes::get(unsigned int n) const
{
  return static_cast<const AnalyticVolume*>(ListOf::get(n));
}


/*
 * Get a AnalyticVolume from the ListOfAnalyticVolumes by id.
 */
AnalyticVolume*
ListOfAnalyticVolumes::get(const std::string& sid)
{
	return const_cast<AnalyticVolume*>(
    static_cast<const ListOfAnalyticVolumes&>(*this).get(sid));
}


/*
 * Get a AnalyticVolume from the ListOfAnalyticVolumes by id.
 */
const AnalyticVolume*
ListOfAnalyticVolumes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <AnalyticVolume*> (*result);
}


/**
 * Adds a copy the given "AnalyticVolume" to this ListOfAnalyticVolumes.
 *
 * @param av; the AnalyticVolume object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfAnalyticVolumes::addAnalyticVolume(const AnalyticVolume* av)
{
  if (av == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (av->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != av->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != av->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(av)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(av);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of AnalyticVolume objects in this ListOfAnalyticVolumes.
 *
 * @return the number of AnalyticVolume objects in this ListOfAnalyticVolumes
 */
unsigned int 
ListOfAnalyticVolumes::getNumAnalyticVolumes() const
{
	return size();
}

/**
 * Creates a new AnalyticVolume object, adds it to this ListOfAnalyticVolumes
 * AnalyticVolume and returns the AnalyticVolume object created. 
 *
 * @return a new AnalyticVolume object instance
 *
 * @see addAnalyticVolume(const AnalyticVolume* av)
 */
AnalyticVolume* 
ListOfAnalyticVolumes::createAnalyticVolume()
{
  AnalyticVolume* av = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    av = new AnalyticVolume(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(av != NULL)
  {
    appendAndOwn(av);
  }

  return av;
}

/*
 * Removes the nth AnalyticVolume from this ListOfAnalyticVolumes
 */
AnalyticVolume*
ListOfAnalyticVolumes::remove(unsigned int n)
{
  return static_cast<AnalyticVolume*>(ListOf::remove(n));
}


/*
 * Removes the AnalyticVolume from this ListOfAnalyticVolumes with the given identifier
 */
AnalyticVolume*
ListOfAnalyticVolumes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <AnalyticVolume*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfAnalyticVolumes::getElementName () const
{
  static const string name = "listOfAnalyticVolumes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfAnalyticVolumes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfAnalyticVolumes::getItemTypeCode () const
{
  return SBML_SPATIAL_ANALYTICVOLUME;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new AnalyticVolume in this ListOfAnalyticVolumes
 */
SBase*
ListOfAnalyticVolumes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "analyticVolume")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new AnalyticVolume(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Spatial package.
 */
void
ListOfAnalyticVolumes::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticVolume_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new AnalyticVolume(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
AnalyticVolume_free(AnalyticVolume_t * av)
{
  if (av != NULL)
    delete av;
}


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticVolume_clone(AnalyticVolume_t * av)
{
  if (av != NULL)
  {
    return static_cast<AnalyticVolume_t*>(av->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
AnalyticVolume_getId(const AnalyticVolume_t * av)
{
	return (av != NULL && av->isSetId()) ? av->getId().c_str() : NULL;
}


LIBSBML_EXTERN
FunctionKind_t
AnalyticVolume_getFunctionType(const AnalyticVolume_t * av)
{
	return (av != NULL) ? av->getFunctionType() : FUNCTIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
AnalyticVolume_getOrdinal(const AnalyticVolume_t * av)
{
	return (av != NULL) ? av->getOrdinal() : SBML_INT_MAX;
}


LIBSBML_EXTERN
const char *
AnalyticVolume_getDomainType(const AnalyticVolume_t * av)
{
	return (av != NULL && av->isSetDomainType()) ? av->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
const ASTNode_t*
AnalyticVolume_getMath(const AnalyticVolume_t * av)
{
	if (av == NULL)
		return NULL;

	return (ASTNode_t*)(av->getMath());
}


LIBSBML_EXTERN
int
AnalyticVolume_isSetId(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->isSetId()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_isSetFunctionType(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->isSetFunctionType()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_isSetOrdinal(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->isSetOrdinal()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_isSetDomainType(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_isSetMath(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->isSetMath()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_setId(AnalyticVolume_t * av, const char * id)
{
  if (av != NULL)
    return (id == NULL) ? av->setId("") : av->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_setFunctionType(AnalyticVolume_t * av, FunctionKind_t functionType)
{
  if (av != NULL)
    return av->setFunctionType(functionType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_setOrdinal(AnalyticVolume_t * av, int ordinal)
{
  if (av != NULL)
    return av->setOrdinal(ordinal);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_setDomainType(AnalyticVolume_t * av, const char * domainType)
{
  if (av != NULL)
    return (domainType == NULL) ? av->setDomainType("") : av->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_setMath(AnalyticVolume_t * av, const ASTNode_t* math)
{
	return (av != NULL) ? av->setMath(math) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_unsetId(AnalyticVolume_t * av)
{
  return (av != NULL) ? av->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_unsetFunctionType(AnalyticVolume_t * av)
{
  return (av != NULL) ? av->unsetFunctionType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_unsetOrdinal(AnalyticVolume_t * av)
{
  return (av != NULL) ? av->unsetOrdinal() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_unsetDomainType(AnalyticVolume_t * av)
{
  return (av != NULL) ? av->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AnalyticVolume_hasRequiredAttributes(const AnalyticVolume_t * av)
{
  return (av != NULL) ? static_cast<int>(av->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
AnalyticVolume_hasRequiredElements(const AnalyticVolume_t * av)
{
	return (av != NULL) ? static_cast<int>(av->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfAnalyticVolumes *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfAnalyticVolumes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


