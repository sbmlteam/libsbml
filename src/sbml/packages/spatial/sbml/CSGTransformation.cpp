/**
 * @file    CSGTransformation.cpp
 * @brief   Implementation of CSGTransformation, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGTransformation.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGTransformation.cpp $
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

#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/sbml/CSGObject.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGTransformation with the given level, version, and package version.
 */
CSGTransformation::CSGTransformation (SBMLSpatialTypeCode_t type, unsigned int level, unsigned int version) 
 : CSGNode (SBML_SPATIAL_CSGTRANSFORMATION, level,version)
   , mChild(0)
{
}


/*
 * Creates a new CSGTransformation with the given SpatialPkgNamespaces object.
 */
CSGTransformation::CSGTransformation(SBMLSpatialTypeCode_t type, SpatialPkgNamespaces* spatialns)
 : CSGNode (SBML_SPATIAL_CSGTRANSFORMATION, spatialns)
   , mChild(0)
{
}


/*
 * Assignment operator.
 */
CSGTransformation& CSGTransformation::operator=(const CSGTransformation& source)
{
  if(&source!=this)
  {
    this->CSGNode::operator=(source);
	this->mType = source.mType;

    delete mChild;
    if (source.mChild)
    {
		mChild = static_cast<CSGNode*>( source.mChild->clone() );
    }
    else
    {
		mChild = 0;
    }

	connectToChild();
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGTransformation::~CSGTransformation ()
{
	// subclasses take care of deletion
}

/*
 * @return the 'child' of this CSGTransformation.
 */
const CSGNode*
CSGTransformation::getChild () const
{
  return mChild;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGTransformation's "child" attribute has been set.
  */
bool 
CSGTransformation::isSetChild () const
{
  return (mChild != 0);
}

 
/*
  * Sets the value of the "child" attribute of this CSGTransformation.
  */
int 
CSGTransformation::setChild (const CSGNode* child)
{
  if (mChild == child)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (child == NULL)
  {
    delete mChild;
    mChild = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mChild;
    mChild = static_cast<CSGNode*>( child->clone() );

    if (mChild) mChild->connectToParent(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Unsets the child of this CSGTransformation.
 */
int
CSGTransformation::unsetChild ()
{
  delete mChild;
  mChild = 0;

  if (mChild == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new CSGPrimitive for this CSGTransformation and returns it.  
 */
CSGPrimitive*
CSGTransformation::createCSGPrimitive ()
{
  CSGPrimitive* n = CSGNode::create_CSGPrimitive();
  setChild(n);
  return static_cast<CSGPrimitive*>(mChild);
}

/*
 * Creates a new CSGPseudoPrimitive for this CSGTransformation and returns it.  
 */
CSGPseudoPrimitive*
CSGTransformation::createCSGPseudoPrimitive ()
{
  CSGPseudoPrimitive* n = CSGNode::create_CSGPseudoPrimitive();
  setChild(n);
  return static_cast<CSGPseudoPrimitive*>(mChild);
}

/*
 * Creates a new CSGSetOperator for this CSGTransformation and returns it.  
 */
CSGSetOperator*
CSGTransformation::createCSGSetOperator ()
{
  CSGSetOperator* n = CSGNode::create_CSGSetOperator();
  setChild(n);
  return static_cast<CSGSetOperator*>(mChild);
}

/*
 * Creates a new CSGTranslation for this CSGTransformation and returns it.  
 */
CSGTranslation*
CSGTransformation::createCSGTranslation ()
{
  CSGTranslation* n = CSGNode::create_CSGTranslation();
  setChild(n);
  return static_cast<CSGTranslation*>(mChild);
}

/*
 * Creates a new CSGRotation for this CSGTransformation and returns it.  
 */
CSGRotation*
CSGTransformation::createCSGRotation ()
{
  CSGRotation* n = CSGNode::create_CSGRotation();
  setChild(n);
  return static_cast<CSGRotation*>(mChild);
}

/*
 * Creates a new CSGScale for this CSGTransformation and returns it.  
 */
CSGScale*
CSGTransformation::createCSGScale ()
{
  CSGScale* n = CSGNode::create_CSGScale();
  setChild(n);
  return static_cast<CSGScale*>(mChild);
}

/*
 * Creates a new CSGHomogeneousTransformation for this CSGTransformation and returns it.  
 */
CSGHomogeneousTransformation*
CSGTransformation::createCSGHomogeneousTransformation ()
{
  CSGHomogeneousTransformation* n = CSGNode::create_CSGHomogeneousTransformation();
  setChild(n);
  return static_cast<CSGHomogeneousTransformation*>(mChild);
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */ 
SBase*
CSGTransformation::createObject (XMLInputStream& stream)
{

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "csgTranslation")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGTranslation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mChild = new CSGTranslation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  } 

  if (name == "csgRotation")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGRotation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mChild = new CSGRotation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  } 

  if (name == "csgScale")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGScale(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mChild = new CSGScale(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  } 

  if (name == "csgHomogeneousTransformation")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGHomogeneousTransformation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mChild = new CSGHomogeneousTransformation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  } 

  if (name == "csgPrimitive")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mChild = new CSGPrimitive(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  }

  if (name == "csgPseudoPrimitive")
  {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGPseudoPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mChild = new CSGPseudoPrimitive(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  }

  if (name == "csgSetOperator") {
	if (mChild) 
	{
	  logError(NotSchemaConformant);
	}
	delete mChild;

	try
	{
		mChild = new CSGSetOperator(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mChild = new CSGSetOperator(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mChild;
  }

  return object;
}


/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
CSGTransformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGTransformation::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGNode::readAttributes(attributes,expectedAttributes);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGTransformation::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * @return the type of this CSGTransformation, CSGTRANSFORMATION_TYPE_CSGTRANSLATION
  , CSGTRANSFORMATION_TYPE_CSGROTATION, CSGTRANSFORMATION_TYPE_CSGSCALE
  , CSGTRANSFORMATION_TYPE_CSGHOMOGENEOUSTRANSFORMATION
 */
CSGTransformationType_t 
CSGTransformation::getType () const
{
  if (mType == SBML_SPATIAL_CSGTRANSLATION) return CSGTRANSFORMATION_TYPE_CSGTRANSLATION;
  if (mType == SBML_SPATIAL_CSGSCALE) return CSGTRANSFORMATION_TYPE_CSGSCALE;
  if (mType == SBML_SPATIAL_CSGROTATION) return CSGTRANSFORMATION_TYPE_CSGROTATION;
  if (mType == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION) return CSGTRANSFORMATION_TYPE_CSGHOMOGENEOUSTRANSFORMATION;
  return CSGTRANSFORMATION_TYPE_INVALID;
}


/*
 * @return true if this CSGTransformation is an Translation, false otherwise.
 */
bool
	CSGTransformation::isCSGTranslation () const
{
	return (mType == SBML_SPATIAL_CSGTRANSLATION);
}


/*
 * @return true if this CSGTransformation is an Rotation, false otherwise.
 */
bool
CSGTransformation::isCSGRotation () const
{
  return (mType == SBML_SPATIAL_CSGROTATION);
}

/*
 * @return true if this CSGTransformation is an Scale, false otherwise.
 */
bool
	CSGTransformation::isCSGScale () const
{
	return (mType == SBML_SPATIAL_CSGSCALE);
}


/*
 * @return true if this CSGTransformation is an HomogeneousTrnasformation, false otherwise.
 */
bool
CSGTransformation::isCSGHomogeneousTransformation () const
{
  return (mType == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION);
}

/*
 * @return the name of this element eg "translation".
 
 */
const std::string&
CSGTransformation::getElementName () const
{
  static const string translation				= "csgTranslation";
  static const string rotation					= "csgRotation";
  static const string scale						= "csgScale";
  static const string homogeneousTransformation = "csgHomogeneousTransformation";
  static const string invalid					= "invalid";

  if ( isCSGTranslation() )
  {
    return translation;
  } 
  else if ( isCSGRotation() )
  {
	  return rotation;
  }
  else if ( isCSGScale() )
  {
	  return scale;
  }
  else if ( isCSGHomogeneousTransformation() )
  {
	  return homogeneousTransformation;
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
CSGTransformation::getTypeCode () const
{
  return mType;
}

/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
CSGTransformation::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mChild) mChild->write(stream);
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGTransformation*
CSGTransformation::clone() const
{
    return new CSGTransformation(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGTransformation::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}
/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
CSGTransformation::connectToChild()
{
  CSGNode::connectToChild();
	if (mChild) mChild->connectToParent(this);
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGTransformation::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
  if (mChild) mChild->setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGTransformation::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI,pkgPrefix,flag);
  if (mChild) mChild->enablePackage(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END

