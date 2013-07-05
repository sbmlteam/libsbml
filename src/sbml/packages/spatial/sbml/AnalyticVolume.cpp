/**
 * @file    AnalyticVolume.cpp
 * @brief   Implementation of AnalyticVolume, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AnalyticVolume.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AnalyticVolume.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/packages/spatial/sbml/AnalyticVolume.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new AnalyticVolume with the given level, version, and package version.
 */
AnalyticVolume::AnalyticVolume (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomainType("")
   , mFunctionType("")
   , mOrdinal(0)
   , mMath(0)
   , mIsSetOrdinal(false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new AnalyticVolume with the given SpatialPkgNamespaces object.
 */
AnalyticVolume::AnalyticVolume(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomainType("")
  , mFunctionType("")
  , mOrdinal(0)
  , mMath(0)
  , mIsSetOrdinal(false)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
AnalyticVolume::AnalyticVolume(const AnalyticVolume& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomainType=source.mDomainType;
  this->mFunctionType=source.mFunctionType;
  this->mOrdinal=source.mOrdinal;
  this->mIsSetOrdinal=source.mIsSetOrdinal;

  if (source.mMath) 
  {
    mMath = source.mMath->deepCopy();
    mMath->setParentSBMLObject(this);
  }
}

/*
 * Assignment operator.
 */
AnalyticVolume& AnalyticVolume::operator=(const AnalyticVolume& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mDomainType = source.mDomainType;
	this->mFunctionType = source.mFunctionType;
	this->mOrdinal=source.mOrdinal;
	this->mIsSetOrdinal=source.mIsSetOrdinal;

	delete mMath;
    if (source.mMath) 
    {
      mMath = source.mMath->deepCopy();
      mMath->setParentSBMLObject(this);
    }
    else
    {
      mMath = 0;
    }
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
AnalyticVolume::~AnalyticVolume ()
{
	if(mMath) delete mMath;
}


/*
  * Returns the value of the "spatialId" attribute of this AnalyticVolume.
  */
const std::string& 
AnalyticVolume::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domainType" attribute of this AnalyticVolume.
  */
const std::string& 
AnalyticVolume::getDomainType () const
{
  return mDomainType;
}

/*
  * Returns the value of the "functionType" attribute of this AnalyticVolume.
  */
const std::string& 
AnalyticVolume::getFunctionType () const
{
  return mFunctionType;
}

/*
 * @return the ordinal of this Domaintype.
 */
unsigned int
AnalyticVolume::getOrdinal () const
{
	return mOrdinal;
}

/*
 * @return the math for this AnalyticVolume.
 */
const ASTNode*
AnalyticVolume::getMath () const
{
  return mMath;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AnalyticVolume's "spatialId" attribute has been set.
  */
bool 
AnalyticVolume::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AnalyticVolume's "domainType" attribute has been set.
  */
bool 
AnalyticVolume::isSetDomainType () const
{
  return (mDomainType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AnalyticVolume's "functionType" attribute has been set.
  */
bool 
AnalyticVolume::isSetFunctionType () const
{
  return (mFunctionType.empty() == false);
}

/*
 * @return true if the spatialDimenions of this AnalyticVolume has been set, false
 * otherwise.
 */
bool
AnalyticVolume::isSetOrdinal () const
{
  return mIsSetOrdinal;
}

/*
 * @return true if the math for this AnalyticVolume has been set,
 * false otherwise.
 */
bool
AnalyticVolume::isSetMath () const
{
  return (mMath != 0);
}

/*
  * Sets the value of the "spatialId" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domainType" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::setDomainType (const std::string& domainType)
{
  return SyntaxChecker::checkAndSetSId(domainType ,mDomainType);
}

/*
  * Sets the value of the "functionType" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::setFunctionType (const std::string& functionType)
{
  return SyntaxChecker::checkAndSetSId(functionType ,mFunctionType);
}

/*
 * Sets the ordinal of this AnalyticVolume to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. ordinal will not be set).
 */
int
AnalyticVolume::setOrdinal (unsigned int value)
{
    mOrdinal = value;
    mIsSetOrdinal  = true;
    return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the math of this AnalyticVolume to a copy of the given
 * ASTNode.
 */
int
AnalyticVolume::setMath (const ASTNode* math)
{
  if (mMath == math) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != 0) ? math->deepCopy() : 0;
    if (mMath) mMath->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Unsets the value of the "spatialId" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::unsetSpatialId ()
{
  mSpatialId.erase();
  if (mSpatialId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "domainType" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::unsetDomainType ()
{
  mDomainType.erase();
  if (mDomainType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "functionType" attribute of this AnalyticVolume.
  */
int 
AnalyticVolume::unsetFunctionType ()
{
  mFunctionType.erase();
  if (mFunctionType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the ordinal of this AnalyticVolume.
 */
int
AnalyticVolume::unsetOrdinal ()
{
  mIsSetOrdinal = false;
  
  if (!isSetOrdinal())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
AnalyticVolume::getElementName () const
{
  static const std::string name = "analyticVolume";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
AnalyticVolume::createObject (XMLInputStream& stream)
{
  // return 0;
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;

}

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
AnalyticVolume::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("domainType");
  attributes.add("functionType");
  attributes.add("ordinal");
}

bool 
AnalyticVolume::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for analyticVolume: math */

  if (!isSetMath())
    allPresent = false;

  return allPresent;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
AnalyticVolume::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<AnalyticVolume>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);
  
  assigned = attributes.readInto("domainType", mDomainType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomainType.empty())
  {
    logEmptyString(mDomainType, sbmlLevel, sbmlVersion, "<AnalyticVolume>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomainType)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("functionType", mFunctionType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mFunctionType.empty())
  {
    logEmptyString(mFunctionType, sbmlLevel, sbmlVersion, "<AnalyticVolume>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mFunctionType)) logError(InvalidIdSyntax);

  attributes.readInto("ordinal", mOrdinal, getErrorLog(), false, getLine(), getColumn());
  /*if (mOrdinal < 0)
  {
    std::string message = "The ordinal attribute on ";
    message += "an <AnalyticVolume> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }*/

}

/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
AnalyticVolume::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken elem = stream.peek();
    unsigned int match = 0;
    int n;
    if (elem.getNamespaces().getLength() != 0)
    {
      for (n = 0; n < elem.getNamespaces().getLength(); n++)
      {
        if (!strcmp(elem.getNamespaces().getURI(n).c_str(), "http://www.w3.org/1998/Math/MathML"))
        {
          match = 1;
          break;
        }
      }
    }
    if (match == 0)
    {
      if( mSBML->getNamespaces() != NULL)
      /* check for implicit declaration */
      {
        for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
        {
          if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(), 
                                                     "http://www.w3.org/1998/Math/MathML"))
          {
            match = 1;
            break;
          }
        }
      }
    }
    if (match == 0)
    {
      logError(InvalidMathElement);
    }

    delete mMath;
    mMath = readMathML(stream);
    if (mMath) mMath->setParentSBMLObject(this);
    read  = true;
  }

  /* ------------------------------
   *
   *   (EXTENSION)
   *
   *------------------------------- */
  if ( SBase::readOtherXML(stream) )
    read = true;

  return read;
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
AnalyticVolume::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domainType",   getPrefix(), mDomainType);
  stream.writeAttribute("functionType",   getPrefix(), mFunctionType);
  stream.writeAttribute("ordinal", getPrefix(), mOrdinal);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
AnalyticVolume::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mMath) writeMathML(mMath, stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
AnalyticVolume::getTypeCode () const
{
	return SBML_SPATIAL_ANALYTICVOLUME;
}

AnalyticVolume*
AnalyticVolume::clone() const
{
    return new AnalyticVolume(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
AnalyticVolume::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(SpatialPkgNamespaces* spatialns)
 : ListOf(spatialns)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());
}


/*
 * Ctor.
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfAnalyticVolumes*
ListOfAnalyticVolumes::clone () const
{
  return new ListOfAnalyticVolumes(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfAnalyticVolumes::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "analyticVolume")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new AnalyticVolume(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
AnalyticVolume *
ListOfAnalyticVolumes::get(unsigned int n)
{
  return static_cast<AnalyticVolume*>(ListOf::get(n));
}


/* return nth item in list */
const AnalyticVolume *
ListOfAnalyticVolumes::get(unsigned int n) const
{
  return static_cast<const AnalyticVolume*>(ListOf::get(n));
}


/* return item by spatialId */
AnalyticVolume*
ListOfAnalyticVolumes::get (const std::string& spatialId)
{
  return const_cast<AnalyticVolume*>( 
    static_cast<const ListOfAnalyticVolumes&>(*this).get(spatialId) );
}


/* return item by spatialId */
const AnalyticVolume*
ListOfAnalyticVolumes::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <AnalyticVolume*> (*result);
}


/* Removes the nth item from this list */
AnalyticVolume*
ListOfAnalyticVolumes::remove (unsigned int n)
{
   return static_cast<AnalyticVolume*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
AnalyticVolume*
ListOfAnalyticVolumes::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <AnalyticVolume*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfAnalyticVolumes::getItemTypeCode () const
{
	return SBML_SPATIAL_ANALYTICVOLUME;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfAnalyticVolumes::getElementName () const
{
  static const std::string name = "listOfAnalyticVolumes";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

