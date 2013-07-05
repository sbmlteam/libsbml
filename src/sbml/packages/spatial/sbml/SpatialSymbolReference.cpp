/**
 * @file    SpatialSymbolReference.cpp
 * @brief   Implementation of SpatialSymbolReference, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SpatialSymbolReference.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SpatialSymbolReference.cpp $
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

#include <sbml/packages/spatial/sbml/SpatialSymbolReference.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new SpatialSymbolReference with the given level, version, and package version.
 */
SpatialSymbolReference::SpatialSymbolReference (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mType("")
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new SpatialSymbolReference with the given SpatialPkgNamespaces object.
 */
SpatialSymbolReference::SpatialSymbolReference(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mType("")
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
SpatialSymbolReference::SpatialSymbolReference(const SpatialSymbolReference& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mType=source.mType;
}

/*
 * Assignment operator.
 */
SpatialSymbolReference& SpatialSymbolReference::operator=(const SpatialSymbolReference& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mType=source.mType;
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
SpatialSymbolReference::~SpatialSymbolReference ()
{
}


/*
  * Returns the value of the "spatialId" attribute of this SpatialSymbolReference.
  */
const std::string& 
SpatialSymbolReference::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "type" attribute of this SpatialSymbolReference.
  */
const std::string& 
SpatialSymbolReference::getType () const
{
  return mType;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SpatialSymbolReference's "spatialId" attribute has been set.
  */
bool 
SpatialSymbolReference::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SpatialSymbolReference's "type" attribute has been set.
  */
bool 
SpatialSymbolReference::isSetType () const
{
  return (mType.empty() == false);
}

/*
  * Sets the value of the "spatialId" attribute of this SpatialSymbolReference.
  */
int 
SpatialSymbolReference::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "type" attribute of this SpatialSymbolReference.
  */
int 
SpatialSymbolReference::setType (const std::string& type)
{
  return SyntaxChecker::checkAndSetSId(type ,mType);
}

/*
  * Unsets the value of the "spatialId" attribute of this SpatialSymbolReference.
  */
int 
SpatialSymbolReference::unsetSpatialId ()
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
  * Unsets the value of the "type" attribute of this SpatialSymbolReference.
  */
int 
SpatialSymbolReference::unsetType ()
{
  mType.erase();
  if (mType.empty())
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
SpatialSymbolReference::getElementName () const
{
  static const std::string name = "spatialSymbolReference";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpatialSymbolReference::createObject (XMLInputStream& stream)
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
SpatialSymbolReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("type");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpatialSymbolReference::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<SpatialSymbolReference>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("type", mType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mType.empty())
  {
    logEmptyString(mType, sbmlLevel, sbmlVersion, "<SpatialSymbolReference>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mType)) logError(InvalidIdSyntax);
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SpatialSymbolReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("type",   getPrefix(), mType);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}

/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
SpatialSymbolReference::getTypeCode () const
{
  return SBML_SPATIAL_SPATIALSYMBOLREFERENCE;
}

SpatialSymbolReference*
SpatialSymbolReference::clone() const
{
    return new SpatialSymbolReference(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
SpatialSymbolReference::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


LIBSBML_CPP_NAMESPACE_END
