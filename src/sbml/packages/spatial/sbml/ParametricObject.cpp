/**
 * @file    ParametricObject.cpp
 * @brief   Implementation of ParametricObject, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: ParametricObject.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/ParametricObject.cpp $
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

#include <sbml/packages/spatial/sbml/PolygonObject.h>

#include <sbml/packages/spatial/sbml/ParametricObject.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new ParametricObject with the given level, version, and package version.
 */
ParametricObject::ParametricObject (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomain("")
   , mPolygonType("")
   , mPolygonObject(0)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new ParametricObject with the given SpatialPkgNamespaces object.
 */
ParametricObject::ParametricObject(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomain("")
  , mPolygonType("")
  , mPolygonObject(0)
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
ParametricObject::ParametricObject(const ParametricObject& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomain=source.mDomain;
  this->mPolygonType=source.mPolygonType;

  if (source.mPolygonObject) 
  {
    mPolygonObject = source.mPolygonObject->deepCopy();
    mPolygonObject->setParentSBMLObject(this);
  }

}

/*
 * Assignment operator.
 */
ParametricObject& ParametricObject::operator=(const ParametricObject& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	this->mSpatialId = source.mSpatialId;
	this->mDomain = source.mDomain;
	this->mPolygonType = source.mPolygonType;

  	delete mPolygonObject;
    if (source.mPolygonObject) 
    {
      mPolygonObject = source.mPolygonObject->deepCopy();
      mPolygonObject->setParentSBMLObject(this);
    }
    else
    {
      mPolygonObject = 0;
    }
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
ParametricObject::~ParametricObject ()
{
	// destroy 'PolygonObject'
	if (mPolygonObject) delete mPolygonObject;
}

/*
  * Returns the value of the "spatialId" attribute of this ParametricObject.
  */
const std::string& 
ParametricObject::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domain" attribute of this ParametricObject.
  */
const std::string& 
ParametricObject::getDomain () const
{
  return mDomain;
}

/*
  * Returns the value of the "polygonType" attribute of this ParametricObject.
  */
const std::string& 
ParametricObject::getPolygonType () const
{
  return mPolygonType;
}

/*
  * Returns the "PolygonObject" of this ParametricObject.
  */
const PolygonObject* 
ParametricObject::getPolygonObject () const
{
  return mPolygonObject;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ParametricObject's "spatialId" attribute has been set.
  */
bool 
ParametricObject::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ParametricObject's "domain" attribute has been set.
  */
bool 
ParametricObject::isSetDomain () const
{
  return (mDomain.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ParametricObject's "polygonType" attribute has been set.
  */
bool 
ParametricObject::isSetPolygonType () const
{
  return (mPolygonType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ParametricObject's "PolygonObject" has been set.
  */
bool 
ParametricObject::isSetPolygonObject () const
{
  return (mPolygonObject != 0);
}

/*
  * Sets the value of the "spatialId" attribute of this ParametricObject.
  */
int 
ParametricObject::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domain" attribute of this ParametricObject.
  */
int 
ParametricObject::setDomain (const std::string& domain)
{
  return SyntaxChecker::checkAndSetSId(domain ,mDomain);
}

/*
  * Sets the value of the "polygonType" attribute of this ParametricObject.
  */
int 
ParametricObject::setPolygonType (const std::string& polygonType)
{
  return SyntaxChecker::checkAndSetSId(polygonType ,mPolygonType);
}

/*
  * Sets the value of the "samples" attribute of this ParametricObject.
  */
int 
ParametricObject::setPolygonObject (const PolygonObject* PolygonObject)
{
  if (mPolygonObject == PolygonObject)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (PolygonObject == NULL)
  {
	  delete mPolygonObject;
	  mPolygonObject = 0;
      return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mPolygonObject;
    mPolygonObject = (PolygonObject != 0) ? PolygonObject->deepCopy() : 0;
    if (mPolygonObject) mPolygonObject->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

 /*
  * Unsets the value of the "spatialId" attribute of this ParametricObject.
  */
int 
ParametricObject::unsetSpatialId ()
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
  * Unsets the value of the "domain" attribute of this ParametricObject.
  */
int 
ParametricObject::unsetDomain ()
{
  mDomain.erase();
  if (mDomain.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

 /*
  * Unsets the value of the "polygonType" attribute of this ParametricObject.
  */
int 
ParametricObject::unsetPolygonType ()
{
  mPolygonType.erase();
  if (mPolygonType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new PolygonObject for this ParametricObject and returns it.  If this
 * ParametricObject had a previous PolygonObject, it will be destroyed.
 */
PolygonObject*
ParametricObject::createPolygonObject ()
{
  delete mPolygonObject;
  mPolygonObject = 0;

  try
  {
    SPATIAL_CREATE_NS(spatialNs, mSBMLNamespaces)
    mPolygonObject = new PolygonObject(spatialNs);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (mPolygonObject)
  {
    mPolygonObject->setParentSBMLObject(this);
  }

  return mPolygonObject;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ParametricObject::getElementName () const
{
  static const std::string name = "parametricObject";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ParametricObject::createObject (XMLInputStream& stream)
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
ParametricObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("domain");
  attributes.add("polygonType");
}

bool 
ParametricObject::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for parametricObject: PolygonObject */

  if (!isSetPolygonObject())
    allPresent = false;

  return allPresent;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
ParametricObject::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<ParametricObject>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domain", mDomain, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomain.empty())
  {
    logEmptyString(mDomain, sbmlLevel, sbmlVersion, "<ParametricObject>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomain)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("polygonType", mPolygonType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mPolygonType.empty())
  {
    logEmptyString(mPolygonType, sbmlLevel, sbmlVersion, "<ParametricObject>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mPolygonType)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, PolygonObject. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
ParametricObject::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "PolygonObject")
  {
 
    delete mPolygonObject;
	mPolygonObject = PolygonObject::readPolygonObject(stream);
    if (mPolygonObject) mPolygonObject->setParentSBMLObject(this);
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
ParametricObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domain",   getPrefix(), mDomain);
  stream.writeAttribute("polygonType",   getPrefix(), mPolygonType);

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
ParametricObject::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mPolygonObject) PolygonObject::writePolygonObject(mPolygonObject, stream);

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
ParametricObject::getTypeCode () const
{
	return SBML_SPATIAL_PARAMETRICOBJECT;
}

ParametricObject*
ParametricObject::clone() const
{
    return new ParametricObject(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
ParametricObject::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfParametricObjects::ListOfParametricObjects(SpatialPkgNamespaces* spatialns)
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
ListOfParametricObjects::ListOfParametricObjects(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfParametricObjects.
 */
ListOfParametricObjects*
ListOfParametricObjects::clone () const
{
  return new ListOfParametricObjects(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfParametricObjects::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "parametricObject")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new ParametricObject(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
ParametricObject *
ListOfParametricObjects::get(unsigned int n)
{
  return static_cast<ParametricObject*>(ListOf::get(n));
}


/* return nth item in list */
const ParametricObject *
ListOfParametricObjects::get(unsigned int n) const
{
  return static_cast<const ParametricObject*>(ListOf::get(n));
}


/* return item by spatialId */
ParametricObject*
ListOfParametricObjects::get (const std::string& spatialId)
{
  return const_cast<ParametricObject*>( 
    static_cast<const ListOfParametricObjects&>(*this).get(spatialId) );
}


/* return item by spatialId */
const ParametricObject*
ListOfParametricObjects::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ParametricObject>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <ParametricObject*> (*result);
}


/* Removes the nth item from this list */
ParametricObject*
ListOfParametricObjects::remove (unsigned int n)
{
   return static_cast<ParametricObject*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
ParametricObject*
ListOfParametricObjects::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ParametricObject>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <ParametricObject*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfParametricObjects::getItemTypeCode () const
{
	return SBML_SPATIAL_PARAMETRICOBJECT;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfParametricObjects::getElementName () const
{
  static const std::string name = "listOfParametricObjects";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

