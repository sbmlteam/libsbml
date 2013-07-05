/**
 * @file    PolygonObject.cpp
 * @brief   Implementation of PolygonObject, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: PolygonObject.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/PolygonObject.cpp $
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

#include <sbml/packages/spatial/sbml/PolygonObject.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/extension/SBMLExtensionException.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new PolygonObject with the given level, version, and package version.
 */
PolygonObject::PolygonObject (unsigned int level, unsigned int version, unsigned int pkgVersion) 
{
  // initilaize pointIndices array
  mIndicesLength = 1;
  mPointIndices = new int[1];
  mPointIndices[0] = 0;

  mIsSetPointIndices = false;
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
 * Creates a new PolygonObject with the given SpatialPkgNamespaces object.
 */
PolygonObject::PolygonObject(SpatialPkgNamespaces* spatialns)
{
  //
  // set the element namespace of this object
  //
  // taken from (SBase->setElementNamespace())
  mURI = spatialns->getURI();
  mSBMLNamespaces = spatialns;

  // initilaize pointIndices array
  mIndicesLength = 1;
  mPointIndices = new int[mIndicesLength];
  mPointIndices[0] = 0;

  mIsSetPointIndices = false;
  mParentSBMLObject = NULL;


  // load package extensions bound with this object (if any) 
  // loadPlugins(spatialns);
}

/*
 * Copy constructor.
 */
PolygonObject::PolygonObject(const PolygonObject& source)
{
  this->mIndicesLength=source.mIndicesLength;
  this->mPointIndices=source.mPointIndices;
  this->mIsSetPointIndices=source.mIsSetPointIndices;
  this->mParentSBMLObject=source.mParentSBMLObject;
  this->mURI=source.mURI;
}

/*
 * Assignment operator.
 */
PolygonObject& PolygonObject::operator=(const PolygonObject& source)
{
  if(&source!=this)
  {
	this->mPointIndices = source.mPointIndices;
	this->mIndicesLength = source.mIndicesLength;
	this->mIsSetPointIndices=source.mIsSetPointIndices;
	this->mParentSBMLObject=source.mParentSBMLObject;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
PolygonObject::~PolygonObject ()
{
	// destroy 'pointIndices' array here --- ???
	delete[] mPointIndices;
	mIndicesLength = 0;
}

  /*
   * The "pointIndices" attribute of this PolygonObject is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
  */
void 
PolygonObject::getPointIndices (int* outputPointIndices) const
{
   if (outputPointIndices == 0) {
	 throw SBMLExtensionException("Cannot return null or 0-length array");
   }
   for (unsigned int i = 0; i < mIndicesLength; i++) {
	   outputPointIndices[i] = mPointIndices[i];
   }

  // return mPointIndices;
}

unsigned int
PolygonObject::getIndicesLength () const
{
	return mIndicesLength;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * PolygonObject's "pointIndices" attribute has been set.
  */
bool 
PolygonObject::isSetPointIndices () const
{
  return mIsSetPointIndices;
}

/*
  * Sets the value of the "pointIndices" attribute of this PolygonObject.
  */
int 
PolygonObject::setPointIndices (int* pointIndices, int indicesLength)
{
  if (pointIndices == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
	  mIndicesLength = indicesLength;
	  mPointIndices = new int[mIndicesLength];
	  for (unsigned int i = 0; i < mIndicesLength; i++) {
		mPointIndices[i] = pointIndices[i];
	  }
	  mIsSetPointIndices  = true;
      return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Unsets the value of the "pointIndices" attribute of this PolygonObject.
  */
int 
PolygonObject::unsetPointIndices ()
{
  mIsSetPointIndices = false;
  
  if (!isSetPointIndices())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Clones an PolygonObject object
 */
PolygonObject*
PolygonObject::deepCopy () const
{
  return new PolygonObject(*this);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
PolygonObject::getElementName () const
{
  static const std::string name = "PolygonObject";
  return name;
}

/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
PolygonObject::getTypeCode () const
{
	return SBML_SPATIAL_POLYGONOBJECT;
}

/**
 * Reads the PolygonObject 'pointIndices' from the given XMLInputStream, 
 */
PolygonObject* PolygonObject::readPolygonObject (XMLInputStream& stream)
 {
	PolygonObject* id = NULL;
	const string& name = stream.peek().getName();
	if (name == "PolygonObject") 
	{
		XMLToken nextToken = stream.next();
		while (!nextToken.isText()) {
			nextToken = stream.next();
		}
		const string& nextTokenChars = nextToken.getCharacters();

		// The PolygonObject element content is obtained as a string. Now parse 
		// the string to get individual ints as strings
		stringstream strStream(nextTokenChars); // Insert the string into a stream
		string buffer;
		vector<int> intValuesVector;
		while (strStream >> buffer) 
		{
			// convert each string token (buf) to int & store in a vector<int>
			int val = atoi(buffer.c_str());
			intValuesVector.push_back(val);
		}

		// convert the vector<int> to an array of ints
		unsigned int indicesSize = (unsigned int)intValuesVector.size();
		int* pointIndices = new int[indicesSize];
		for (unsigned int i = 0; i < indicesSize; i++) 
		{
			pointIndices[i] = intValuesVector.at(i);
		}
		
		// create an PolygonObject object and set the pointIndices on it.
		id = new PolygonObject; //(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
		id->setPointIndices(pointIndices, indicesSize);
	}
	return id;
 }

/*
 * Writes PolygonObject 'pointIndices' to given XMLOutputStream
 */
void PolygonObject::writePolygonObject (PolygonObject* PolygonObject, XMLOutputStream& stream)
{
  unsigned int length = PolygonObject->getIndicesLength();
  int* indicesArray = new int[length];
  PolygonObject->getPointIndices(indicesArray);
  string uri = PolygonObject->getURI();

  // Write out the values in 'indicesArray' as content of PolygonObject element.
  if (length > 0) {
	  string prefix = "";
	  XMLNamespaces* xmlNamespaces = PolygonObject->getSBMLNamespaces()->getNamespaces();
	  if (xmlNamespaces->hasURI(uri)) 
	  {		
		prefix = xmlNamespaces->getPrefix(uri);
	  }
	  // write out the PolygonObject element with prefix, if present.
	  stream.startElement("PolygonObject", prefix);
	  for (unsigned int i = 0; i < length; i++) {
		stream << (long)indicesArray[i] << " ";
	  }
	  stream.endElement("PolygonObject", prefix);
  }

}

/** @cond doxygen-libsbml-internal */
void PolygonObject::setParentSBMLObject(SBase * sb)
{
  mParentSBMLObject = sb;
}
/** @endcond doxygen-libsbml-internal */


SBase * PolygonObject::getParentSBMLObject() const
{
  return mParentSBMLObject;
}

/* gets the SBMLnamespaces - internal use only*/
SBMLNamespaces *
PolygonObject::getSBMLNamespaces() const
{
  if (mSBMLNamespaces != 0)
    return mSBMLNamespaces;
  else
    return new SBMLNamespaces();
}

std::string
PolygonObject::getURI() const
{
	return mURI;
}


LIBSBML_CPP_NAMESPACE_END
