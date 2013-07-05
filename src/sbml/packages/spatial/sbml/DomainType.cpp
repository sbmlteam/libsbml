/**
 * @file    DomainType.cpp
 * @brief   Implementation of DomainType, the SBase derived class of spatial package.
 * @author  Akiya Jouraku
 *
 * $Id: DomainType.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/DomainType.cpp $
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

#include <sbml/packages/spatial/sbml/DomainType.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new DomainType with the given level, version, and package version.
 */
DomainType::DomainType (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId(""  )
   , mSpatialDimensions( 3        )
   , mIsSetSpatialDimensions ( false    )
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new DomainType with the given SpatialPkgNamespaces object.
 */
DomainType::DomainType(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mSpatialDimensions( 3        )
  , mIsSetSpatialDimensions ( false    )
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);

  // if level 3 values have no defaults
 // -------  mSpatialDimensionsDouble = numeric_limits<double>::quiet_NaN();

}


/*
 * Copy constructor.
 */
DomainType::DomainType(const DomainType& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mSpatialDimensions = source.mSpatialDimensions;
  this->mIsSetSpatialDimensions = source.mIsSetSpatialDimensions;
}

/*
 * Assignment operator.
 */
DomainType& DomainType::operator=(const DomainType& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mSpatialDimensions = source.mSpatialDimensions;
	this->mIsSetSpatialDimensions = source.mIsSetSpatialDimensions;
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
DomainType::~DomainType ()
{
}


/*
  * Returns the value of the "spatialId" attribute of this DomainType.
  */
const std::string& 
DomainType::getSpatialId () const
{
  return mSpatialId;
}

/*
 * @return the spatialDimensions of this Domaintype.
 */
unsigned int
DomainType::getSpatialDimensions () const
{
	return mSpatialDimensions;
}


/*
  * Predicate returning @c true or @c false depending on whether this
  * DomainType's "spatialId" attribute has been set.
  */
bool 
DomainType::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
 * @return true if the spatialDimenions of this DomainType has been set, false
 * otherwise.
 */
bool
DomainType::isSetSpatialDimensions () const
{
  return mIsSetSpatialDimensions;
}


/*
  * Sets the value of the "spatialId" attribute of this DomainType.
  */
int 
DomainType::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
 * Sets the spatialDimensions of this DomainType to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
int
DomainType::setSpatialDimensions (unsigned int value)
{
  if (value > 3)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpatialDimensions = value;
    mIsSetSpatialDimensions  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * Unsets the value of the "spatialId" attribute of this DomainType.
  */
int 
DomainType::unsetSpatialId ()
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
 * Unsets the spatialDimensions of this DomainType.
 */
int
DomainType::unsetSpatialDimensions ()
{
  mIsSetSpatialDimensions = false;
  
  if (!isSetSpatialDimensions())
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
DomainType::getElementName () const
{
  static const std::string name = "domainType";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
DomainType::createObject (XMLInputStream& stream)
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
DomainType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("spatialDimensions");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
DomainType::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<DomainType>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  //
  // spatialDimensions { maxInclusive="3" minInclusive="0" use="optional"
  //                     default="3" }  (L2v1 ->)
  // spatialDimensions { use="optional"}  (L3v1 ->)
  //
  attributes.readInto("spatialDimensions", mSpatialDimensions, getErrorLog(), false, getLine(), getColumn());
  if (mSpatialDimensions > 3)
  {
    std::string message = "The spatialDimensions attribute on ";
    message += "a <DomainType> may only have values 0, 1, 2 or 3.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }
}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
DomainType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);

  unsigned int sd = mSpatialDimensions;
  if (sd <= 3)
  {
    stream.writeAttribute("spatialDimensions", getPrefix(), sd);
  }

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
DomainType::writeElements (XMLOutputStream& stream) const
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
DomainType::getTypeCode () const
{
  return SBML_SPATIAL_DOMAINTYPE;
}

DomainType*
DomainType::clone() const
{
    return new DomainType(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
DomainType::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfDomainTypes::ListOfDomainTypes(SpatialPkgNamespaces* spatialns)
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
ListOfDomainTypes::ListOfDomainTypes(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfDomainTypes.
 */
ListOfDomainTypes*
ListOfDomainTypes::clone () const
{
  return new ListOfDomainTypes(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfDomainTypes::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "domainType")
  {
	  object = new DomainType(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
DomainType *
ListOfDomainTypes::get(unsigned int n)
{
  return static_cast<DomainType*>(ListOf::get(n));
}


/* return nth item in list */
const DomainType *
ListOfDomainTypes::get(unsigned int n) const
{
  return static_cast<const DomainType*>(ListOf::get(n));
}


/* return item by spatialId */
DomainType*
ListOfDomainTypes::get (const std::string& spatialId)
{
  return const_cast<DomainType*>( 
    static_cast<const ListOfDomainTypes&>(*this).get(spatialId) );
}


/* return item by spatialId */
const DomainType*
ListOfDomainTypes::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DomainType>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <DomainType*> (*result);
}


/* Removes the nth item from this list */
DomainType*
ListOfDomainTypes::remove (unsigned int n)
{
   return static_cast<DomainType*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
DomainType*
ListOfDomainTypes::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DomainType>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DomainType*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfDomainTypes::getItemTypeCode () const
{
  return SBML_SPATIAL_DOMAINTYPE;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfDomainTypes::getElementName () const
{
  static const std::string name = "listOfDomainTypes";
  return name;
}



LIBSBML_CPP_NAMESPACE_END

