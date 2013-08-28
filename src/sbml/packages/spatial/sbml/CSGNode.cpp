/**
 * @file    CSGNode.cpp
 * @brief   Implementation of CSGNode, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGNode.cpp $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGNode.cpp $
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

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Only subclasses may create CSGNodes.
 */
CSGNode::CSGNode (SBMLSpatialTypeCode_t type, unsigned int level, unsigned int version) 
  : SBase (level,version)
   , mSpatialId("")
   , mType (type)
{
}


CSGNode::CSGNode(SBMLSpatialTypeCode_t type,SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mType (type)
{
}


/*
 * Assignment operator.
 */
CSGNode& CSGNode::operator=(const CSGNode& source)
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
CSGNode::~CSGNode ()
{
}

/*
  * Returns the value of the "spatialId" attribute of this CSGNode.
  */
const std::string& 
CSGNode::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGNode's "spatialId" attribute has been set.
  */
bool 
CSGNode::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Sets the value of the "spatialId" attribute of this CSGNode.
  */
int 
CSGNode::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
 * Unsets the value of the "spatialId" attribute of this CSGNode.
 */
int 
CSGNode::unsetSpatialId ()
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
 * Creates a new CSGPrimitive for this CSGNode and returns it.  
 */
CSGPrimitive*
CSGNode::create_CSGPrimitive ()
{
  CSGPrimitive* n = new CSGPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGPseudoPrimitive for this CSGNode and returns it.  
 */
CSGPseudoPrimitive*
CSGNode::create_CSGPseudoPrimitive ()
{
  CSGPseudoPrimitive* n = new CSGPseudoPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGSetOperator for this CSGNode and returns it.  
 */
CSGSetOperator*
	CSGNode::create_CSGSetOperator ()
{
  CSGSetOperator* n = new CSGSetOperator(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGTranslation for this CSGNode and returns it.  
 */
CSGTranslation*
CSGNode::create_CSGTranslation ()
{
  CSGTranslation* n = new CSGTranslation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGRotation for this CSGNode and returns it.  
 */
CSGRotation*
CSGNode::create_CSGRotation ()
{
  CSGRotation* n = new CSGRotation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGScale for this CSGNode and returns it.  
 */
CSGScale*
CSGNode::create_CSGScale ()
{
  CSGScale* n = new CSGScale(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Creates a new CSGHomogeneousTransformation for this CSGNode and returns it.  
 */
CSGHomogeneousTransformation*
CSGNode::create_CSGHomogeneousTransformation ()
{
  CSGHomogeneousTransformation* n = new CSGHomogeneousTransformation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  return n;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
CSGNode::getElementName () const
{
  static const string csgTransformation     = "csgTransformation";
  static const string csgPrimitive			= "csgPrimitive";
  static const string csgPseudoPrimitive	= "csgPseudoPrimitive";
  static const string csgSetOperator		= "csgSetOperator";
  static const string invalid				= "invalid";

  if ( isCSGTransformation() )
  {
    return csgTransformation;
  } 
  else if ( isCSGPrimitive() )
  {
	  return csgPrimitive;
  }
    else if ( isCSGPseudoPrimitive() )
  {
	  return csgPseudoPrimitive;
  }
  else if ( isCSGSetOperator() )
  {
	  return csgSetOperator;
  }

  return invalid;
}


/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
CSGNode::addExpectedAttributes(ExpectedAttributes& attributes)
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
CSGNode::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<CSGNode>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGNode::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * @return the type of this CSGNode, CSGOBJ_TYPE_GEOMETRICPRIMITIVE or CSGOBJ_TYPE_CSGOPERATOR
 * or CSGOBJ_TYPE_AFFINETRANSFORMATION
 */
CSGNodeType_t 
CSGNode::getType () const
{
  if (mType == SBML_SPATIAL_CSGTRANSFORMATION) return CSGNODE_TYPE_CSGTRANSFORMATION;
  if (mType == SBML_SPATIAL_CSGPRIMITIVE) return CSGNODE_TYPE_CSGPRIMITIVE;
  if (mType == SBML_SPATIAL_CSGPSEUDOPRIMITIVE) return CSGNODE_TYPE_CSGPSEUDOPRIMITIVE;
  if (mType == SBML_SPATIAL_CSGSETOPERATOR) return CSGNODE_TYPE_CSGSETOPERATOR;
  return CSGNODE_TYPE_INVALID;
}

/*
 * @return true if this CSGNode is a CSGTransformation, false otherwise.
 */
bool
CSGNode::isCSGTransformation () const
{
	return ((mType == SBML_SPATIAL_CSGTRANSFORMATION));
	/*
	return ((mType == SBML_SPATIAL_CSGTRANSLATION) ||
		(mType == SBML_SPATIAL_CSGROTATION) ||
		(mType == SBML_SPATIAL_CSGSCALE) ||
		(mType == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)) ;
		*/
}


/*
 * @return true if this CSGNode is a CSGPrimitive, false otherwise.
 */
bool
CSGNode::isCSGPrimitive () const
{
	return (mType == SBML_SPATIAL_CSGPRIMITIVE);
}

/*
 * @return true if this CSGNode is a CSGPseudoPrimitive, false otherwise.
 */
bool
CSGNode::isCSGPseudoPrimitive () const
{
	return (mType == SBML_SPATIAL_CSGPSEUDOPRIMITIVE);
}

/*
 * @return true if this CSGNode is an CSGNode, false otherwise.
 */
bool
CSGNode::isCSGSetOperator () const
{
	return (mType == SBML_SPATIAL_CSGSETOPERATOR);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
CSGNode::getTypeCode () const
{
	return mType;
}

CSGNode*
CSGNode::clone() const
{
    return new CSGNode(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGNode::accept (SBMLVisitor& v) const
{
    return v.visit(*this);
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGNode::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGNode::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

}


/** @cond doxygenCOnly */

/**
 * Creates and returns a deep copy of a given CSGNode_t structure.
 *
 * @param g the CSGNode_t structure to copy
 * 
 * @return a (deep) copy of this CSGNode_t structure.
 */
LIBSBML_EXTERN
CSGNode_t *
CSGNode_clone (const CSGNode_t *csg)
{
  return static_cast<CSGNode*>( csg->clone() );
}


/*
 * Ctor.
 */
ListOfCSGNodes::ListOfCSGNodes(SpatialPkgNamespaces* spatialns)
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
ListOfCSGNodes::ListOfCSGNodes(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfCSGNodes.
 */
ListOfCSGNodes*
ListOfCSGNodes::clone () const
{
  return new ListOfCSGNodes(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCSGNodes::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  CSGNode*        object = 0;


  SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
  if (name == "csgTranslation")
  {
    object = new CSGTranslation(spatialns);	
  } 

  if (name == "csgRotation")
  {
    object = new CSGRotation(spatialns);	
  } 

  if (name == "csgScale")
  {
	object = new CSGScale(spatialns);
  } 

  if (name == "csgHomogeneousTransformation")
  {
    object = new CSGHomogeneousTransformation(spatialns);
  } 

  if (name == "csgPrimitive")
  {
	object = new CSGPrimitive(spatialns);
  }

  if (name == "csgPseudoPrimitive")
  {
	object = new CSGPseudoPrimitive(spatialns);
  }

  if (name == "csgSetOperator") 
  {
	object = new CSGSetOperator(spatialns);
  }

  appendAndOwn(object);

  return object;
}


/* return nth item in list */
CSGNode *
ListOfCSGNodes::get(unsigned int n)
{
  return static_cast<CSGNode*>(ListOf::get(n));
}


/* return nth item in list */
const CSGNode *
ListOfCSGNodes::get(unsigned int n) const
{
  return static_cast<const CSGNode*>(ListOf::get(n));
}


/* return item by spatialId */
CSGNode*
ListOfCSGNodes::get (const std::string& spatialId)
{
  return const_cast<CSGNode*>( 
    static_cast<const ListOfCSGNodes&>(*this).get(spatialId) );
}


/* return item by spatialId */
const CSGNode*
ListOfCSGNodes::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGNode>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <CSGNode*> (*result);
}


/* Removes the nth item from this list */
CSGNode*
ListOfCSGNodes::remove (unsigned int n)
{
   return static_cast<CSGNode*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
CSGNode*
ListOfCSGNodes::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGNode>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CSGNode*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfCSGNodes::getItemTypeCode () const
{
	return SBML_SPATIAL_CSGNODE;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfCSGNodes::getElementName () const
{
  static const std::string name = "listOfCSGNodes";
  return name;
}


bool 
ListOfCSGNodes::isValidTypeForList(SBase * item)
{
  int tc = item->getTypeCode();
  return ((tc == SBML_SPATIAL_CSGTRANSLATION )
	  ||    (tc == SBML_SPATIAL_CSGROTATION )
	  ||    (tc == SBML_SPATIAL_CSGSCALE )
	  ||    (tc == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION )
	  ||    (tc == SBML_SPATIAL_CSGPRIMITIVE )
	  ||    (tc == SBML_SPATIAL_CSGPSEUDOPRIMITIVE )
      ||    (tc == SBML_SPATIAL_CSGSETOPERATOR ));
}


LIBSBML_CPP_NAMESPACE_END

