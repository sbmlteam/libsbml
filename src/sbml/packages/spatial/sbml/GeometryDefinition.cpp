/**
 * @file    GeometryDefinition.cpp
 * @brief   Implementation of GeometryDefinition, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: GeometryDefinition.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/GeometryDefinition.cpp $
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

#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Only subclasses may create GeometryDefinitions.
 */
GeometryDefinition::GeometryDefinition (SBMLSpatialTypeCode_t type, unsigned int level, unsigned int version) 
  : SBase (level,version)
   , mSpatialId("")
   , mType (type)
{
}

GeometryDefinition::GeometryDefinition(SBMLSpatialTypeCode_t type, SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mType (type)
{
}

/*
 * Assignment operator.
 */
GeometryDefinition& GeometryDefinition::operator=(const GeometryDefinition& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mType = source.mType;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
GeometryDefinition::~GeometryDefinition ()
{
}


/*
  * Returns the value of the "spatialId" attribute of this GeometryDefinition.
  */
const std::string& 
GeometryDefinition::getSpatialId () const
{
  return mSpatialId;
}


/*
  * Predicate returning @c true or @c false depending on whether this
  * GeometryDefinition's "spatialId" attribute has been set.
  */
bool 
GeometryDefinition::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Sets the value of the "spatialId" attribute of this GeometryDefinition.
  */
int 
GeometryDefinition::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}


/*
  * Unsets the value of the "spatialId" attribute of this GeometryDefinition.
  */
int 
GeometryDefinition::unsetSpatialId ()
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
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
GeometryDefinition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
GeometryDefinition::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<GeometryDefinition>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
GeometryDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}

/*
 * @return the type of this GeometryDefinition, GEOMETRICDEFN_TYPE_SAMPLEDFIELDGEOMETRY or
 * GEOMETRICDEFN_TYPE_PARAMETRICGEOMETRY or GEOMETRICDEFN_TYPE_ANALYTICGEOMETRY or
 * GEOMETRICDEFN_TYPE_CSGEOMETRY
 */
GeometryDefinitionType_t 
GeometryDefinition::getType () const
{
  if (mType == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY) return GEOMETRICDEFN_TYPE_SAMPLEDFIELDGEOMETRY;
  if (mType == SBML_SPATIAL_PARAMETRICGEOMETRY) return GEOMETRICDEFN_TYPE_PARAMETRICGEOMETRY;
  if (mType == SBML_SPATIAL_ANALYTICGEOMETRY) return GEOMETRICDEFN_TYPE_ANALYTICGEOMETRY;
  if (mType == SBML_SPATIAL_CSGGEOMETRY) return GEOMETRICDEFN_TYPE_CSGEOMETRY;
  return GEOMETRICDEFN_TYPE_INVALID;
}


/*
 * @return true if this GeometryDefinition is an SampledFieldGeometry, false otherwise.
 */
bool
GeometryDefinition::isSampledFieldGeometry () const
{
	return (mType == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY);
}


/*
 * @return true if this GeometryDefinition is an ParametricGeometry, false otherwise.
 */
bool
GeometryDefinition::isParametricGeometry () const
{
  return (mType == SBML_SPATIAL_PARAMETRICGEOMETRY);
}

/*
 * @return true if this GeometryDefinition is an AnalyticGeometry, false otherwise.
 */
bool
GeometryDefinition::isAnalyticGeometry () const
{
	return (mType == SBML_SPATIAL_ANALYTICGEOMETRY);
}


/*
 * @return true if this GeometryDefinition is an CSGeometry, false otherwise.
 */
bool
GeometryDefinition::isCSGeometry () const
{
  return (mType == SBML_SPATIAL_CSGGEOMETRY);
}

/*
 * @return the name of this element eg "analyticGeometry".
 
 */
const std::string&
GeometryDefinition::getElementName () const
{
  static const string analytic     = "analyticGeometry";
  static const string sampledField = "sampledFieldGeometry";
  static const string parametric   = "parametricGeometry";
  static const string csGeom       = "csGeometry";
  static const string invalid	   = "invalid";

  if ( isAnalyticGeometry() )
  {
    return analytic;
  } 
  else if ( isSampledFieldGeometry() )
  {
	  return sampledField;
  }
  else if ( isParametricGeometry() )
  {
	  return parametric;
  }
  else if ( isCSGeometry() )
  {
	  return csGeom;
  }

  return invalid;
}

/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
GeometryDefinition::getTypeCode () const
{
  return mType;
}

GeometryDefinition*
GeometryDefinition::clone() const
{
    return new GeometryDefinition(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
GeometryDefinition::accept (SBMLVisitor& v) const
{
 return v.visit(*this);
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
GeometryDefinition::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
GeometryDefinition::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

}

/** @cond doxygen-c-only */

/**
 * Creates and returns a deep copy of a given GeometryDefinition_t structure.
 *
 * @param g the GeometryDefinition_t structure to copy
 * 
 * @return a (deep) copy of this GeometryDefinition_t structure.
 */
LIBSBML_EXTERN
GeometryDefinition_t *
GeometryDefinition_clone (const GeometryDefinition_t *g)
{
  return static_cast<GeometryDefinition*>( g->clone() );
}

/*
 * Ctor.
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(SpatialPkgNamespaces* spatialns)
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
ListOfGeometryDefinitions::ListOfGeometryDefinitions(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfGeometryDefinitions.
 */
ListOfGeometryDefinitions*
ListOfGeometryDefinitions::clone () const
{
  return new ListOfGeometryDefinitions(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfGeometryDefinitions::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  GeometryDefinition*        object = 0;
  SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
  
  if (name == "analyticGeometry")
  {
	object = new AnalyticGeometry(spatialns);
  } 
  
  if (name == "sampledFieldGeometry")
  {
	object = new SampledFieldGeometry(spatialns);
  }

  if (name == "parametricGeometry") 
  {
	object = new ParametricGeometry(spatialns);
  }

  if (name == "csGeometry") 
  {
	object = new CSGeometry(spatialns);
  }

  appendAndOwn(object);

  return object;
}


/* return nth item in list */
GeometryDefinition *
ListOfGeometryDefinitions::get(unsigned int n)
{
  return static_cast<GeometryDefinition*>(ListOf::get(n));
}


/* return nth item in list */
const GeometryDefinition *
ListOfGeometryDefinitions::get(unsigned int n) const
{
  return static_cast<const GeometryDefinition*>(ListOf::get(n));
}


/* return item by spatialId */
GeometryDefinition*
ListOfGeometryDefinitions::get (const std::string& spatialId)
{
  return const_cast<GeometryDefinition*>( 
    static_cast<const ListOfGeometryDefinitions&>(*this).get(spatialId) );
}


/* return item by spatialId */
const GeometryDefinition*
ListOfGeometryDefinitions::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeometryDefinition>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <GeometryDefinition*> (*result);
}


/* Removes the nth item from this list */
GeometryDefinition*
ListOfGeometryDefinitions::remove (unsigned int n)
{
   return static_cast<GeometryDefinition*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
GeometryDefinition*
ListOfGeometryDefinitions::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeometryDefinition>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GeometryDefinition*> (item);
}

/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfGeometryDefinitions::getItemTypeCode () const
{
	return SBML_SPATIAL_GEOMETRYDEFINITION;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfGeometryDefinitions::getElementName () const
{
  static const std::string name = "listOfGeometryDefinitions";
  return name;
}

bool 
ListOfGeometryDefinitions::isValidTypeForList(SBase * item)
{
  int tc = item->getTypeCode();
  return ((tc == SBML_SPATIAL_ANALYTICGEOMETRY )
	  ||    (tc == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY )
      ||    (tc == SBML_SPATIAL_PARAMETRICGEOMETRY )
      ||    (tc == SBML_SPATIAL_CSGGEOMETRY));
}


LIBSBML_CPP_NAMESPACE_END
