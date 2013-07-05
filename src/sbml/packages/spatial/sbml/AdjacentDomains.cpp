/**
 * @file    AdjacentDomains.cpp
 * @brief   Implementation of AdjacentDomains, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AdjacentDomains.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AdjacentAdjacentDomains.cpp $
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

#include <sbml/packages/spatial/sbml/AdjacentDomains.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new AdjacentDomains with the given level, version, and package version.
 */
AdjacentDomains::AdjacentDomains (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomain1("")
   , mDomain2("")
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new AdjacentDomains with the given SpatialPkgNamespaces object.
 */
AdjacentDomains::AdjacentDomains(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomain1("")
  , mDomain2("")
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
AdjacentDomains::AdjacentDomains(const AdjacentDomains& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomain1=source.mDomain1;
  this->mDomain2=source.mDomain2;
}

/*
 * Assignment operator.
 */
AdjacentDomains& AdjacentDomains::operator=(const AdjacentDomains& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mDomain1 = source.mDomain1;
	this->mDomain2 = source.mDomain2;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
AdjacentDomains::~AdjacentDomains ()
{
}


/*
  * Returns the value of the "spatialId" attribute of this AdjacentDomains.
  */
const std::string& 
AdjacentDomains::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domain1" attribute of this AdjacentDomains.
  */
const std::string& 
AdjacentDomains::getDomain1 () const
{
  return mDomain1;
}

/*
  * Returns the value of the "domain2" attribute of this AdjacentDomains.
  */
const std::string& 
AdjacentDomains::getDomain2 () const
{
  return mDomain2;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AdjacentDomains's "spatialId" attribute has been set.
  */
bool 
AdjacentDomains::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AdjacentDomains's "domain1" attribute has been set.
  */
bool 
AdjacentDomains::isSetDomain1 () const
{
  return (mDomain1.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * AdjacentDomains's "domain2" attribute has been set.
  */
bool 
AdjacentDomains::isSetDomain2 () const
{
  return (mDomain2.empty() == false);
}

/*
  * Sets the value of the "spatialId" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domain1" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::setDomain1 (const std::string& domain1)
{
  return SyntaxChecker::checkAndSetSId(domain1 ,mDomain1);
}

/*
  * Sets the value of the "domain2" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::setDomain2 (const std::string& domain2)
{
  return SyntaxChecker::checkAndSetSId(domain2 ,mDomain2);
}

/*
  * Unsets the value of the "spatialId" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::unsetSpatialId ()
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
  * Unsets the value of the "domain1" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::unsetDomain1 ()
{
  mDomain1.erase();
  if (mDomain1.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "domain2" attribute of this AdjacentDomains.
  */
int 
AdjacentDomains::unsetDomain2 ()
{
  mDomain2.erase();
  if (mDomain2.empty())
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
AdjacentDomains::getElementName () const
{
  static const std::string name = "adjacentDomains";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
AdjacentDomains::createObject (XMLInputStream& stream)
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
AdjacentDomains::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("domain1");
  attributes.add("domain2");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
AdjacentDomains::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<AdjacentDomains>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domain1", mDomain1, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomain1.empty())
  {
    logEmptyString(mDomain1, sbmlLevel, sbmlVersion, "<AdjacentDomains>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomain1)) logError(InvalidIdSyntax);
  
  assigned = attributes.readInto("domain2", mDomain2, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomain2.empty())
  {
    logEmptyString(mDomain2, sbmlLevel, sbmlVersion, "<AdjacentDomains>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomain2)) logError(InvalidIdSyntax);

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
AdjacentDomains::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domain1",   getPrefix(), mDomain1);  
  stream.writeAttribute("domain2",   getPrefix(), mDomain2);

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
AdjacentDomains::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

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
AdjacentDomains::getTypeCode () const
{
	return SBML_SPATIAL_ADJACENTDOMAINS;
}

AdjacentDomains*
AdjacentDomains::clone() const
{
    return new AdjacentDomains(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
AdjacentDomains::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(SpatialPkgNamespaces* spatialns)
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
ListOfAdjacentDomains::ListOfAdjacentDomains(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfAdjacentDomains*
ListOfAdjacentDomains::clone () const
{
  return new ListOfAdjacentDomains(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfAdjacentDomains::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "adjacentDomains")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new AdjacentDomains(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
AdjacentDomains *
ListOfAdjacentDomains::get(unsigned int n)
{
  return static_cast<AdjacentDomains*>(ListOf::get(n));
}


/* return nth item in list */
const AdjacentDomains *
ListOfAdjacentDomains::get(unsigned int n) const
{
  return static_cast<const AdjacentDomains*>(ListOf::get(n));
}


/* return item by spatialId */
AdjacentDomains*
ListOfAdjacentDomains::get (const std::string& spatialId)
{
  return const_cast<AdjacentDomains*>( 
    static_cast<const ListOfAdjacentDomains&>(*this).get(spatialId) );
}


/* return item by spatialId */
const AdjacentDomains*
ListOfAdjacentDomains::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <AdjacentDomains*> (*result);
}


/* Removes the nth item from this list */
AdjacentDomains*
ListOfAdjacentDomains::remove (unsigned int n)
{
   return static_cast<AdjacentDomains*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
AdjacentDomains*
ListOfAdjacentDomains::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <AdjacentDomains*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfAdjacentDomains::getItemTypeCode () const
{
	return SBML_SPATIAL_ADJACENTDOMAINS;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfAdjacentDomains::getElementName () const
{
  static const std::string name = "listOfAdjacentDomains";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

