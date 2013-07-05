/**
 * @file    DiffusionCoefficient.cpp
 * @brief   Implementation of DiffusionCoefficient, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: DiffusionCoefficient.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/DiffusionCoefficient.cpp $
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

#include <sbml/packages/spatial/sbml/DiffusionCoefficient.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new DiffusionCoefficient with the given level, version, and package version.
 */
DiffusionCoefficient::DiffusionCoefficient (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mVariable("")
   , mCoordinateIndex(0)
   , mIsSetCoordinateIndex (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new DiffusionCoefficient with the given SpatialPkgNamespaces object.
 */
DiffusionCoefficient::DiffusionCoefficient(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mVariable("")
  , mCoordinateIndex(0)
  , mIsSetCoordinateIndex (false)
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
DiffusionCoefficient::DiffusionCoefficient(const DiffusionCoefficient& source) : SBase(source)
{
  this->mVariable=source.mVariable;
  this->mCoordinateIndex=source.mCoordinateIndex;
  this->mIsSetCoordinateIndex=source.mIsSetCoordinateIndex;
}

/*
 * Assignment operator.
 */
DiffusionCoefficient& DiffusionCoefficient::operator=(const DiffusionCoefficient& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mVariable = source.mVariable;
	this->mCoordinateIndex=source.mCoordinateIndex;
	this->mIsSetCoordinateIndex=source.mIsSetCoordinateIndex;
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
DiffusionCoefficient::~DiffusionCoefficient ()
{
}


/*
  * Returns the value of the "variable" attribute of this DiffusionCoefficient.
  */
const std::string& 
DiffusionCoefficient::getVariable () const
{
  return mVariable;
}

/*
  * Returns the value of the "coordinateIndex" attribute of this DiffusionCoefficient.
  */
unsigned int 
DiffusionCoefficient::getCoordinateIndex () const
{
  return mCoordinateIndex;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * DiffusionCoefficient's "variable" attribute has been set.
  */
bool 
DiffusionCoefficient::isSetVariable () const
{
  return (mVariable.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * DiffusionCoefficient's "coordinateIndex" attribute has been set.
  */
bool 
DiffusionCoefficient::isSetCoordinateIndex () const
{
  return mIsSetCoordinateIndex;
}

/*
  * Sets the value of the "variable" attribute of this DiffusionCoefficient.
  */
int 
DiffusionCoefficient::setVariable (const std::string& variable)
{
  return SyntaxChecker::checkAndSetSId(variable ,mVariable);
}

/*
  * Sets the value of the "coordinateIndex" attribute of this DiffusionCoefficient.
  */
int 
DiffusionCoefficient::setCoordinateIndex (unsigned int coordinateIndex)
{
    mCoordinateIndex = coordinateIndex;
    mIsSetCoordinateIndex  = true;
    return LIBSBML_OPERATION_SUCCESS;
}

/*
  * Unsets the value of the "variable" attribute of this DiffusionCoefficient.
  */
int 
DiffusionCoefficient::unsetVariable ()
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
  * Unsets the value of the "coordinateIndex" attribute of this DiffusionCoefficient.
  */
int 
DiffusionCoefficient::unsetCoordinateIndex ()
{
  mIsSetCoordinateIndex = false;
  
  if (!isSetCoordinateIndex())
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
DiffusionCoefficient::getElementName () const
{
  static const std::string name = "diffusionCoefficient";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
DiffusionCoefficient::createObject (XMLInputStream& stream)
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
DiffusionCoefficient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");
  attributes.add("coordinateIndex");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
DiffusionCoefficient::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("variable", mVariable, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mVariable.empty())
  {
    logEmptyString(mVariable, sbmlLevel, sbmlVersion, "<DiffusionCoefficient>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mVariable)) logError(InvalidIdSyntax);

  attributes.readInto("coordinateIndex", mCoordinateIndex, getErrorLog(), false, getLine(), getColumn());
  /*if (mCoordinateIndex < 0)
  {
    std::string message = "The coordinateIndex attribute on ";
    message += "a <DiffusionCoefficient> can only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }*/
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
DiffusionCoefficient::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("variable",   getPrefix(), mVariable);

  stream.writeAttribute("coordinateIndex",   getPrefix(), mCoordinateIndex);

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
DiffusionCoefficient::getTypeCode () const
{
  return SBML_SPATIAL_DIFFUSIONCOEFFICIENT;
}

DiffusionCoefficient*
DiffusionCoefficient::clone() const
{
    return new DiffusionCoefficient(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
DiffusionCoefficient::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


LIBSBML_CPP_NAMESPACE_END
