/**
 * @file    BoundaryCondition.cpp
 * @brief   Implementation of BoundaryCondition, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: BoundaryCondition.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/BoundaryCondition.cpp $
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

#include <sbml/packages/spatial/sbml/BoundaryCondition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new BoundaryCondition with the given level, version, and package version.
 */
BoundaryCondition::BoundaryCondition (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mVariable("")
   , mType("")
   , mCoordinateBoundary("")
   , mBoundaryDomainType("")
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new BoundaryCondition with the given SpatialPkgNamespaces object.
 */
BoundaryCondition::BoundaryCondition(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mVariable("")
  , mType("")
  , mCoordinateBoundary("")
  , mBoundaryDomainType("")
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
BoundaryCondition::BoundaryCondition(const BoundaryCondition& source) : SBase(source)
{
  this->mVariable=source.mVariable;
  this->mCoordinateBoundary=source.mCoordinateBoundary;
  this->mType=source.mType;
  this->mBoundaryDomainType=source.mBoundaryDomainType;
}

/*
 * Assignment operator.
 */
BoundaryCondition& BoundaryCondition::operator=(const BoundaryCondition& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mVariable = source.mVariable;
	this->mCoordinateBoundary = source.mCoordinateBoundary;
    this->mType = source.mType;
    this->mBoundaryDomainType = source.mBoundaryDomainType;
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
BoundaryCondition::~BoundaryCondition ()
{
}


/*
  * Returns the value of the "variable" attribute of this BoundaryCondition.
  */
const std::string& 
BoundaryCondition::getVariable () const
{
  return mVariable;
}

/*
  * Returns the value of the "coordinateBoundary" attribute of this BoundaryCondition.
  */
const std::string& 
BoundaryCondition::getCoordinateBoundary () const
{
  return mCoordinateBoundary;
}

/*
  * Returns the value of the "type" attribute of this BoundaryCondition.
  */
const std::string& 
BoundaryCondition::getType () const
{
  return mType;
}

/*
  * Returns the value of the "boundaryDomainType" attribute of this BoundaryCondition.
  */
const std::string& 
BoundaryCondition::getBoundaryDomainType () const
{
  return mBoundaryDomainType;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * BoundaryCondition's "variable" attribute has been set.
  */
bool 
BoundaryCondition::isSetVariable () const
{
  return (mVariable.empty() == false);
}


/*
  * Predicate returning @c true or @c false depending on whether this
  * BoundaryCondition's "coordinateBoundary" attribute has been set.
  */
bool 
BoundaryCondition::isSetCoordinateBoundary () const
{
  return (mCoordinateBoundary.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * BoundaryCondition's "type" attribute has been set.
  */
bool 
BoundaryCondition::isSetType () const
{
  return (mType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * BoundaryCondition's "boundaryDomainType" attribute has been set.
  */
bool 
BoundaryCondition::isSetBoundaryDomainType () const
{
  return (mBoundaryDomainType.empty() == false);
}

/*
  * Sets the value of the "variable" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::setVariable (const std::string& variable)
{
  return SyntaxChecker::checkAndSetSId(variable ,mVariable);
}

/*
  * Sets the value of the "coordinateBoundary" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::setCoordinateBoundary (const std::string& coordBoundary)
{
  return SyntaxChecker::checkAndSetSId(coordBoundary ,mCoordinateBoundary);
}

/*
  * Sets the value of the "type" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::setType (const std::string&  type)
{
	return SyntaxChecker::checkAndSetSId(type ,mType);
  }

/*
  * Sets the value of the "boundaryDomainType" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::setBoundaryDomainType (const std::string& boundaryDomainType)
  {
  return SyntaxChecker::checkAndSetSId(boundaryDomainType ,mBoundaryDomainType);

}

/*
  * Unsets the value of the "variable" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::unsetVariable ()
{
  mVariable.erase();
  if (mVariable.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "coordinateBoundary" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::unsetCoordinateBoundary ()
{
  mCoordinateBoundary.erase();
  if (mCoordinateBoundary.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "type" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::unsetType ()
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
  * Unsets the value of the "boundaryDomainType" attribute of this BoundaryCondition.
  */
int 
BoundaryCondition::unsetBoundaryDomainType ()
{
  mBoundaryDomainType.erase();
  if (mBoundaryDomainType.empty())
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
BoundaryCondition::getElementName () const
{
  static const std::string name = "boundaryCondition";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
BoundaryCondition::createObject (XMLInputStream& stream)
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
BoundaryCondition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");
  attributes.add("coordinateBoundary");
  attributes.add("type");
  attributes.add("boundaryDomainType");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
BoundaryCondition::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("variable", mVariable, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mVariable.empty())
  {
    logEmptyString(mVariable, sbmlLevel, sbmlVersion, "<BoundaryCondition>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mVariable)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("coordinateBoundary", mCoordinateBoundary, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mCoordinateBoundary.empty())
  {
    logEmptyString(mCoordinateBoundary, sbmlLevel, sbmlVersion, "<BoundaryCondition>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mCoordinateBoundary)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("type", mType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mType.empty())
  {
    logEmptyString(mType, sbmlLevel, sbmlVersion, "<BoundaryCondition>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mType)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("boundaryDomainType", mBoundaryDomainType, getErrorLog(), false, getLine(), getColumn());
  if (assigned && mBoundaryDomainType.empty())
  {
    logEmptyString(mBoundaryDomainType, sbmlLevel, sbmlVersion, "<BoundaryCondition>");
}
  if (!SyntaxChecker::isValidSBMLSId(mBoundaryDomainType)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
BoundaryCondition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("variable",   getPrefix(), mVariable);
  stream.writeAttribute("coordinateBoundary",   getPrefix(), mCoordinateBoundary);
  stream.writeAttribute("type",   getPrefix(), mType);
  stream.writeAttribute("boundaryDomainType",   getPrefix(), mBoundaryDomainType);

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
BoundaryCondition::getTypeCode () const
{
  return SBML_SPATIAL_BOUNDARYCONDITION;
}

BoundaryCondition*
BoundaryCondition::clone() const
{
    return new BoundaryCondition(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
BoundaryCondition::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


LIBSBML_CPP_NAMESPACE_END
