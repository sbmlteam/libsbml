/**
 * @file    CSGPseudoPrimitive.cpp
 * @brief   Implementation of CSGPseudoPrimitive, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGPseudoPrimitive.cpp 
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGPseudoPrimitive.cpp $
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

#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGPseudoPrimitive with the given level, version, and package version.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: CSGNode (SBML_SPATIAL_CSGPSEUDOPRIMITIVE, level,version)
  , mcsgObjectRef("")
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new CSGPseudoPrimitive with the given SpatialPkgNamespaces object.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive(SpatialPkgNamespaces* spatialns)
 : CSGNode (SBML_SPATIAL_CSGPSEUDOPRIMITIVE, spatialns)
  , mcsgObjectRef("")
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

  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive(const CSGPseudoPrimitive& source) : CSGNode(source)
{
	this->mcsgObjectRef=source.mcsgObjectRef;

}

/*
 * Assignment operator.
 */
CSGPseudoPrimitive& CSGPseudoPrimitive::operator=(const CSGPseudoPrimitive& source)
{
  if(&source!=this)
  {
    this->CSGNode::operator=(source);
	this->mcsgObjectRef = source.mcsgObjectRef;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGPseudoPrimitive::~CSGPseudoPrimitive ()
{
}

/*
  * Returns the value of the "CSGObjectRef" attribute of this CSGPseudoPrimitive.
  */
const std::string& 
CSGPseudoPrimitive::getCSGObjectRef () const
{
	return mcsgObjectRef;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGPseudoPrimitive's "CSGObjectRef" attribute has been set.
  */
bool 
CSGPseudoPrimitive::isSetCSGObjectRef () const
{
	return (mcsgObjectRef.empty() == false);
}

/*
  * Sets the value of the "CSGObjectRef" attribute of this CSGPseudoPrimitive.
  */
int 
CSGPseudoPrimitive::setCSGObjectRef (const std::string& primitiveType)
{
	return SyntaxChecker::checkAndSetSId(primitiveType , mcsgObjectRef);
}

 /*
  * Unsets the value of the "CSGObjectRef" attribute of this CSGPseudoPrimitive.
  */
int 
CSGPseudoPrimitive::unsetCSGObjectRef ()
{
	mcsgObjectRef.erase();
	if (mcsgObjectRef.empty())
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
CSGPseudoPrimitive::getElementName () const
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
CSGPseudoPrimitive::createObject (XMLInputStream& stream)
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
CSGPseudoPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);
  
  attributes.add("csgObjectRef");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGPseudoPrimitive::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGNode::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("csgObjectRef", mcsgObjectRef, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mcsgObjectRef.empty())
  {
    logEmptyString(mcsgObjectRef, sbmlLevel, sbmlVersion, "<CSGPseudoPrimitive>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mcsgObjectRef)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGPseudoPrimitive::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  stream.writeAttribute("csgObjectRef",   getPrefix(), mcsgObjectRef);

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
CSGPseudoPrimitive::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGPseudoPrimitive*
CSGPseudoPrimitive::clone() const
{
    return new CSGPseudoPrimitive(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGPseudoPrimitive::accept (SBMLVisitor& v) const
{
  // return false;
  // return v.visit(*this);

  bool result = v.visit(*this);

  return result;
 
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGPseudoPrimitive::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGPseudoPrimitive::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI,pkgPrefix,flag);
}


LIBSBML_CPP_NAMESPACE_END

