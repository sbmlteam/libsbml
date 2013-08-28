/**
 * @file    TransformationComponents.cpp
 * @brief   Implementation of TransformationComponents, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: TransformationComponents.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/TransformationComponents.cpp $
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
#include <stdlib.h>
#include <vector>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/spatial/sbml/TransformationComponents.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/extension/SBMLExtensionException.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new TransformationComponents with the given level, version, and package version.
 */
TransformationComponents::TransformationComponents (unsigned int level, unsigned int version, unsigned int pkgVersion) 
{
  // initilaize components array
  mComponentsLength = 1;
  mComponents = new double[1];
  mComponents[0] = 0;

  mIsSetComponents = false;
  mParentSBMLObject = NULL;

  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  // Taken from SBase.setSBMLNamepsacesAndOwn();
  // setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
  /*
  SBMLNamespaces* sbmlns = new SpatialPkgNamespaces(level,version,pkgVersion);
  delete mSBMLNamespaces;
  mSBMLNamespaces = sbmlns;

  if(sbmlns) 
	mURI = sbmlns->getURI();
  */
}

/*
 * Creates a new TransformationComponents with the given SpatialPkgNamespaces object.
 */
TransformationComponents::TransformationComponents(SpatialPkgNamespaces* spatialns)
{
  //
  // set the element namespace of this object
  //
  // taken from (SBase->setElementNamespace())
  mURI = spatialns->getURI();
  mSBMLNamespaces = spatialns;

  // initilaize components array
  mComponentsLength = 1;
  mComponents = new double[mComponentsLength];
  mComponents[0] = 0;

  mIsSetComponents = false;
  mParentSBMLObject = NULL;


  // load package extensions bound with this object (if any) 
  // loadPlugins(spatialns);
}

/*
 * Copy constructor.
 */
TransformationComponents::TransformationComponents(const TransformationComponents& source)
{
  this->mComponentsLength=source.mComponentsLength;
  this->mComponents=source.mComponents;
  this->mIsSetComponents=source.mIsSetComponents;
  this->mParentSBMLObject=source.mParentSBMLObject;
  this->mURI=source.mURI;
}

/*
 * Assignment operator.
 */
TransformationComponents& TransformationComponents::operator=(const TransformationComponents& source)
{
  if(&source!=this)
  {
	this->mComponents = source.mComponents;
	this->mComponentsLength = source.mComponentsLength;
	this->mIsSetComponents=source.mIsSetComponents;
	this->mParentSBMLObject=source.mParentSBMLObject;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
TransformationComponents::~TransformationComponents ()
{
	// destroy 'components' array here --- ???
	delete[] mComponents;
	mComponentsLength = 0;
}

  /*
   * The "components" attribute of this TransformationComponents is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
  */
void 
TransformationComponents::getComponents (double* outputComponents) const
{
   if (outputComponents == 0) {
	 throw SBMLExtensionException("Cannot return null or 0-length array");
   }
   for (unsigned int i = 0; i < mComponentsLength; i++) {
	   outputComponents[i] = mComponents[i];
   }

}

unsigned int
TransformationComponents::getComponentsLength () const
{
	return mComponentsLength;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * TransformationComponents's "components" attribute has been set.
  */
bool 
TransformationComponents::isSetComponents () const
{
  return mIsSetComponents;
}

/*
  * Sets the value of the "components" attribute of this TransformationComponents.
  */
int 
TransformationComponents::setComponents (double* components, int componentsLength)
{
  if (components == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
	  mComponentsLength = componentsLength;
	  mComponents = new double[mComponentsLength];
	  for (unsigned int i = 0; i < mComponentsLength; i++) {
		mComponents[i] = components[i];
	  }
	  mIsSetComponents  = true;
      return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Unsets the value of the "components" attribute of this TransformationComponents.
  */
int 
TransformationComponents::unsetComponents ()
{
  mIsSetComponents = false;
  
  if (!isSetComponents())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Clones an transformationComponents object
 */
TransformationComponents*
TransformationComponents::deepCopy () const
{
  return new TransformationComponents(*this);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
TransformationComponents::getElementName () const
{
  static const std::string name = "transformationComponents";
  return name;
}

/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
TransformationComponents::getTypeCode () const
{
	return SBML_SPATIAL_TRANSFORMATIONCOMPONENTS;
}

/**
 * Reads the transformationComponents 'components' from the given XMLInputStream, 
 */
TransformationComponents* 
TransformationComponents::readTransformationComponents (XMLInputStream& stream)
 {
	TransformationComponents* tc = NULL;
	const string& name = stream.peek().getName();
	if (name == "transformationComponents") 
	{
		XMLToken nextToken = stream.next();
		while (!nextToken.isText()) {
			nextToken = stream.next();
		}
		const string& nextTokenChars = nextToken.getCharacters();

		// The transformationComponents element content is obtained as a string. Now parse 
		// the string to get individual doubles as strings
		stringstream strStream(nextTokenChars); // Insert the string into a stream
		string buffer;
		vector<double> doubleValuesVector;
		while (strStream >> buffer) 
		{
			// convert each string token (buf) to int & store in a vector<int>
			int val = atoi(buffer.c_str());
			doubleValuesVector.push_back(val);
		}

		// convert the vector<int> to an array of ints
		unsigned int componentsSize = (unsigned int)doubleValuesVector.size();
		double* components = new double[componentsSize];
		for (unsigned int i = 0; i < componentsSize; i++) 
		{
			components[i] = doubleValuesVector.at(i);
		}
		
		// create an TransformationComponents object and set the components on it.
		tc = new TransformationComponents; //(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
		tc->setComponents(components, componentsSize);
	}
	return tc;
 }

/*
 * Writes transformationComponents 'components' to given XMLOutputStream
 */
void TransformationComponents::writeTransformationComponents (TransformationComponents* transformationComponents, XMLOutputStream& stream)
{
  unsigned int length = transformationComponents->getComponentsLength();
  double* componentsArray = new double[length];
  transformationComponents->getComponents(componentsArray);
  string uri = transformationComponents->getURI();

  // Write out the values in 'componentsArray' as content of TransformationComponents element.
  if (length > 0) {
	  string prefix = "";
	  XMLNamespaces* xmlNamespaces = transformationComponents->getSBMLNamespaces()->getNamespaces();
	  if (xmlNamespaces->hasURI(uri)) 
	  {		
		prefix = xmlNamespaces->getPrefix(uri);
	  }
	  // write out the transformationComponents element with prefix, if present.
	  stream.startElement("transformationComponents", prefix);
	  for (unsigned int i = 0; i < length; i++) {
		stream << (double)componentsArray[i] << " ";
	  }
	  stream.endElement("transformationComponents", prefix);
  }

}

/** @cond doxygenLibsbmlInternal */
void TransformationComponents::setParentSBMLObject(SBase * sb)
{
  mParentSBMLObject = sb;
}
/** @endcond doxygenLibsbmlInternal */


SBase * TransformationComponents::getParentSBMLObject() const
{
  return mParentSBMLObject;
}

/* gets the SBMLnamespaces - internal use only*/
SBMLNamespaces *
TransformationComponents::getSBMLNamespaces() const
{
  if (mSBMLNamespaces != 0)
    return mSBMLNamespaces;
  else
    return new SBMLNamespaces();
}

std::string
TransformationComponents::getURI() const
{
	return mURI;
}


LIBSBML_CPP_NAMESPACE_END
