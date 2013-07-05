/**
 * @file    CSGPrimitive.cpp
 * @brief   Implementation of CSGPrimitive, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGPrimitive.cpp  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGPrimitive.cpp $
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
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGPrimitive with the given level, version, and package version.
 */
CSGPrimitive::CSGPrimitive (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: CSGNode (SBML_SPATIAL_CSGPRIMITIVE, level,version)
   , mPrimitiveType("")
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

}


/*
 * Creates a new CSGPrimitive with the given SpatialPkgNamespaces object.
 */
CSGPrimitive::CSGPrimitive(SpatialPkgNamespaces* spatialns)
 : CSGNode (SBML_SPATIAL_CSGPRIMITIVE, spatialns)
  , mPrimitiveType("")
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  if (!hasValidLevelVersionNamespaceCombination())
  {
    std::string err(getElementName());
    XMLNamespaces* xmlns = spatialns->getNamespaces();
    if (xmlns)
    {
      std::ostringstream oss;
      XMLOutputStream xos(oss);
      xos << *xmlns;
      err.append(oss.str());
    }
    throw SBMLConstructorException(err);
  }

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
CSGPrimitive::CSGPrimitive(const CSGPrimitive& source) : CSGNode(source)
{
  this->mPrimitiveType=source.mPrimitiveType;
}

/*
 * Assignment operator.
 */
CSGPrimitive& CSGPrimitive::operator=(const CSGPrimitive& source)
{
  if(&source!=this)
  {
    this->CSGNode::operator=(source);
	this->mPrimitiveType = source.mPrimitiveType;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGPrimitive::~CSGPrimitive ()
{
}

/*
  * Returns the value of the "PrimitiveType" attribute of this CSGPrimitive.
  */
const std::string& 
CSGPrimitive::getPrimitiveType () const
{
  return mPrimitiveType;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGPrimitive's "PrimitiveType" attribute has been set.
  */
bool 
CSGPrimitive::isSetPrimitiveType () const
{
  return (mPrimitiveType.empty() == false);
}

/*
  * Sets the value of the "PrimitiveType" attribute of this CSGPrimitive.
  */
int 
CSGPrimitive::setPrimitiveType (const std::string& primitiveType)
{
  return SyntaxChecker::checkAndSetSId(primitiveType ,mPrimitiveType);
}

 /*
  * Unsets the value of the "PrimitiveType" attribute of this CSGPrimitive.
  */
int 
CSGPrimitive::unsetPrimitiveType ()
{
  mPrimitiveType.erase();
  if (mPrimitiveType.empty())
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
 
const std::string&
CSGPrimitive::getElementName () const
{
  static const std::string name = "geometricPrimitive";
  return name;
}
*/

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CSGPrimitive::createObject (XMLInputStream& stream)
{
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
CSGPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);
  
  attributes.add("primitiveType");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGPrimitive::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGNode::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("primitiveType", mPrimitiveType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mPrimitiveType.empty())
  {
    logEmptyString(mPrimitiveType, sbmlLevel, sbmlVersion, "<CSGPrimitive>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mPrimitiveType)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGPrimitive::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  stream.writeAttribute("primitiveType",   getPrefix(), mPrimitiveType);

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
CSGPrimitive::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGPrimitive*
CSGPrimitive::clone() const
{
    return new CSGPrimitive(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGPrimitive::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGPrimitive::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGPrimitive::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI,pkgPrefix,flag);
}


LIBSBML_CPP_NAMESPACE_END

