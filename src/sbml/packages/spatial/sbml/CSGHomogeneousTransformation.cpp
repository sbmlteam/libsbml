/**
 * @file    CSGHomogeneousTransformation.cpp
 * @brief   Implementation of CSGHomogeneousTransformation, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGHomogeneousTransformation.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGHomogeneousTransformation.cpp $
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

#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGHomogeneousTransformation with the given level, version, and package version.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : CSGTransformation (SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION, level,version)
  , mForwardTransform(0)
  , mInverseTransform(0)
{
  mType = SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION;
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

}


/*
 * Creates a new CSGHomogeneousTransformation with the given SpatialPkgNamespaces object.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation(SpatialPkgNamespaces* spatialns)
   : CSGTransformation (SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION, spatialns)
   , mForwardTransform(0)
   , mInverseTransform(0)

{
  mType = SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION;
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
CSGHomogeneousTransformation::CSGHomogeneousTransformation(const CSGHomogeneousTransformation& source) : CSGTransformation(source)
{
  if (source.mChild)
  {
	  mChild = static_cast<CSGNode*>( source.mChild->clone() );
  }
  if (source.mForwardTransform) 
  {
    mForwardTransform = source.mForwardTransform->deepCopy();
    mForwardTransform->setParentSBMLObject(this);
  }
  if (source.mInverseTransform) 
  {
    mInverseTransform = source.mInverseTransform->deepCopy();
    mInverseTransform->setParentSBMLObject(this);
  }

  connectToChild();

}


/*
 * Assignment operator.
 */
CSGHomogeneousTransformation& CSGHomogeneousTransformation::operator=(const CSGHomogeneousTransformation& source)
{
  if(&source!=this)
  {
    this->CSGTransformation::operator=(source);

  	delete mForwardTransform;
    if (source.mForwardTransform) 
    {
      mForwardTransform = source.mForwardTransform->deepCopy();
      mForwardTransform->setParentSBMLObject(this);
    }
    else
    {
      mForwardTransform = 0;
    }

	delete mInverseTransform;
    if (source.mInverseTransform) 
    {
      mInverseTransform = source.mInverseTransform->deepCopy();
      mInverseTransform->setParentSBMLObject(this);
    }
    else
    {
      mInverseTransform = 0;
    }

  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGHomogeneousTransformation::~CSGHomogeneousTransformation ()
{
	// destroy 'mChild'
	if (mChild) delete mChild;
	// destroy 'forwardTransform'
	if (mForwardTransform) delete mForwardTransform;
	// destroy 'inverseTransform'
	if (mInverseTransform) delete mInverseTransform;
}


 /*
  * Returns the "forwardTransform" of this CSGHomogeneousTransformation.
  */
const TransformationComponents* 
CSGHomogeneousTransformation::getForwardTransform () const
{
  return mForwardTransform;
}


 /*
  * Returns the "inverseTransform" of this CSGHomogeneousTransformation.
  */
const TransformationComponents* 
CSGHomogeneousTransformation::getInverseTransform () const
{
  return mInverseTransform;
}

 /*
  * Predicate returning @c true or @c false depending on whether this
  * CSGHomogeneousTransformation's "forwardTransform" has been set.
  */
bool 
CSGHomogeneousTransformation::isSetForwardTransform () const
{
  return (mForwardTransform != 0);
}

 /*
  * Predicate returning @c true or @c false depending on whether this
  * CSGHomogeneousTransformation's "inverseTransform" has been set.
  */
bool 
CSGHomogeneousTransformation::isSetInverseTransform () const
{
  return (mInverseTransform != 0);
}



 /*
  * Sets the value of the "forwardTransform" attribute of this CSGHomogeneousTransformation.
  */
int 
CSGHomogeneousTransformation::setForwardTransform (const TransformationComponents* ftc)
{
  if (mForwardTransform == ftc)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (ftc == NULL)
  {
	  delete mForwardTransform;
	  mForwardTransform = 0;
      return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mForwardTransform;
    mForwardTransform = (ftc != 0) ? ftc->deepCopy() : 0;
    if (mForwardTransform) mForwardTransform->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

 /*
  * Sets the value of the "inverseTransform" attribute of this CSGHomogeneousTransformation.
  */
int 
CSGHomogeneousTransformation::setInverseTransform (const TransformationComponents* itc)
{
  if (mInverseTransform == itc)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (itc == NULL)
  {
	  delete mInverseTransform;
	  mInverseTransform = 0;
      return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mInverseTransform;
    mInverseTransform = (itc != 0) ? itc->deepCopy() : 0;
    if (mInverseTransform) mInverseTransform->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new forwardTransform for this CSGHomogeneousTransformation and returns it.  If this
 * CSGHomogeneousTransformation had a previous forwardTransform, it will be destroyed.
 */
TransformationComponents*
CSGHomogeneousTransformation::createForwardTransform ()
{
  delete mForwardTransform;
  mForwardTransform = 0;

  try
  {
    //const unsigned int sbmlLevel   = getLevel  ();
    //const unsigned int sbmlVersion = getVersion();
    SPATIAL_CREATE_NS(spatialNs, mSBMLNamespaces)
    mForwardTransform = new TransformationComponents(spatialNs);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (mForwardTransform)
  {
    mForwardTransform->setParentSBMLObject(this);
  }

  return mForwardTransform;
}


/*
 * Creates a new inverseTransform for this CSGHomogeneousTransformation and returns it.  If this
 * CSGHomogeneousTransformation had a previous inverseTransform, it will be destroyed.
 */
TransformationComponents*
CSGHomogeneousTransformation::createInverseTransform ()
{
  delete mInverseTransform;
  mInverseTransform = 0;

  try
  {
    SPATIAL_CREATE_NS(spatialNs, mSBMLNamespaces)
    mInverseTransform = new TransformationComponents(spatialNs);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (mInverseTransform)
  {
    mInverseTransform->setParentSBMLObject(this);
  }

  return mInverseTransform;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 
SBase*
CSGHomogeneousTransformation::createObject (XMLInputStream& stream)
{
  // return 0;
  
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;

}
*/
/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
CSGHomogeneousTransformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);
}


bool 
CSGHomogeneousTransformation::hasRequiredElements() const
{
  bool allPresent = true;

  /* required element for CSGHomogeneousTransformation: forwardTransform, inverseTransform */

  if (!isSetForwardTransform() || !isSetInverseTransform())
    allPresent = false;

  return allPresent;
}


/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, TransformationComponents. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
CSGHomogeneousTransformation::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "forwardTransform")
  {
    delete mForwardTransform;
	mForwardTransform = TransformationComponents::readTransformationComponents(stream);
    if (mForwardTransform) mForwardTransform->setParentSBMLObject(this);
    read  = true;
  }

  if (name == "inverseTransform")
  {
    delete mInverseTransform;
	mInverseTransform = TransformationComponents::readTransformationComponents(stream);
    if (mInverseTransform) mInverseTransform->setParentSBMLObject(this);
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
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGHomogeneousTransformation::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGTransformation::readAttributes(attributes,expectedAttributes);

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGHomogeneousTransformation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

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
CSGHomogeneousTransformation::writeElements (XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);

  if (mForwardTransform) TransformationComponents::writeTransformationComponents(mForwardTransform, stream);
  if (mInverseTransform) TransformationComponents::writeTransformationComponents(mInverseTransform, stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGHomogeneousTransformation*
CSGHomogeneousTransformation::clone() const
{
    return new CSGHomogeneousTransformation(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGHomogeneousTransformation::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGHomogeneousTransformation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGHomogeneousTransformation::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END
