/**
 * @file:   CSGNode.cpp
 * @brief:  Implementation of the CSGNode class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGNode with the given level, version, and package version.
 */
CSGNode::CSGNode (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGNode with the given SpatialPkgNamespaces object.
 */
CSGNode::CSGNode (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGNode.
 */
CSGNode::CSGNode (const CSGNode& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
  }
}


/*
 * Assignment for CSGNode.
 */
CSGNode&
CSGNode::operator=(const CSGNode& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
  }
  return *this;
}


/*
 * Clone for CSGNode.
 */
CSGNode*
CSGNode::clone () const
{
  return new CSGNode(*this);
}


/*
 * Destructor for CSGNode.
 */
CSGNode::~CSGNode ()
{
}


/*
 * Returns the value of the "id" attribute of this CSGNode.
 */
const std::string&
CSGNode::getId() const
{
  return mId;
}


/*
 * Returns true/false if id is set.
 */
bool
CSGNode::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
CSGNode::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Unsets id and returns value indicating success.
 */
int
CSGNode::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Return @c true if of type CSGPrimitive.
 */
bool
CSGNode::isCSGPrimitive() const
{
  return dynamic_cast<const CSGPrimitive*>(this) != NULL;
}


/*
 * Return @c true if of type CSGTranslation.
 */
bool
CSGNode::isCSGTranslation() const
{
  return dynamic_cast<const CSGTranslation*>(this) != NULL;
}


/*
 * Return @c true if of type CSGRotation.
 */
bool
CSGNode::isCSGRotation() const
{
  return dynamic_cast<const CSGRotation*>(this) != NULL;
}


/*
 * Return @c true if of type CSGScale.
 */
bool
CSGNode::isCSGScale() const
{
  return dynamic_cast<const CSGScale*>(this) != NULL;
}


/*
 * Return @c true if of type CSGHomogeneousTransformation.
 */
bool
CSGNode::isCSGHomogeneousTransformation() const
{
  return dynamic_cast<const CSGHomogeneousTransformation*>(this) != NULL;
}


/*
 * Return @c true if of type CSGPseudoPrimitive.
 */
bool
CSGNode::isCSGPseudoPrimitive() const
{
  return dynamic_cast<const CSGPseudoPrimitive*>(this) != NULL;
}


/*
 * Return @c true if of type CSGSetOperator.
 */
bool
CSGNode::isCSGSetOperator() const
{
  return dynamic_cast<const CSGSetOperator*>(this) != NULL;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGNode::getElementName () const
{
  static const string name = "csgNode";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGNode::getTypeCode () const
{
  return SBML_SPATIAL_CSGNODE;
}


/*
 * check if all the required attributes are set
 */
bool
CSGNode::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGNode::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGNode::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGNode::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGNode::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGNode::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGNode::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfCSGNodes - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfCSGNodes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<CSGNode>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'csgNode' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CSGNode::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfCSGNodes::ListOfCSGNodes(unsigned int level, 
                 unsigned int version, 
                 unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfCSGNodes::ListOfCSGNodes(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfCSGNodes 
 */
ListOfCSGNodes* 
ListOfCSGNodes::clone () const
 {
  return new ListOfCSGNodes(*this);
}


/*
 * Get a CsgNode from the ListOfCSGNodes by index.
*/
CSGNode*
ListOfCSGNodes::get(unsigned int n)
{
  return static_cast<CSGNode*>(ListOf::get(n));
}


/*
 * Get a CsgNode from the ListOfCSGNodes by index.
 */
const CSGNode*
ListOfCSGNodes::get(unsigned int n) const
{
  return static_cast<const CSGNode*>(ListOf::get(n));
}


/*
 * Get a CsgNode from the ListOfCSGNodes by id.
 */
CSGNode*
ListOfCSGNodes::get(const std::string& sid)
{
	return const_cast<CSGNode*>(
    static_cast<const ListOfCSGNodes&>(*this).get(sid));
}


/*
 * Get a CsgNode from the ListOfCSGNodes by id.
 */
const CSGNode*
ListOfCSGNodes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGNode>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CSGNode*> (*result);
}


/**
 * Adds a copy the given "CSGNode" to this ListOfCSGNodes.
 *
 * @param csgn; the CSGNode object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfCSGNodes::addCsgNode(const CSGNode* csgn)
{
  if (csgn == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (csgn->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != csgn->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != csgn->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(csgn)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(csgn);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of CSGNode objects in this ListOfCSGNodes.
 *
 * @return the number of CSGNode objects in this ListOfCSGNodes
 */
unsigned int 
ListOfCSGNodes::getNumCsgNodes() const
{
	return size();
}

/**
 * Creates a new CSGPrimitive object, adds it to this ListOfCSGNodes
 * csgPrimitive and returns the CSGPrimitive object created. 
 *
 * @return a new CSGPrimitive object instance
 *
 * @see addCsgPrimitive(const CSGNode* csgn)
 */
CSGPrimitive* 
ListOfCSGNodes::createCsgPrimitive()
{
  CSGPrimitive* csgp = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgp = new CSGPrimitive(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgp != NULL)
  {
    appendAndOwn(csgp);
  }

  return csgp;
}

/**
 * Creates a new CSGTranslation object, adds it to this ListOfCSGNodes
 * csgTranslation and returns the CSGTranslation object created. 
 *
 * @return a new CSGTranslation object instance
 *
 * @see addCsgTranslation(const CSGNode* csgn)
 */
CSGTranslation* 
ListOfCSGNodes::createCsgTranslation()
{
  CSGTranslation* csgt = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgt = new CSGTranslation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgt != NULL)
  {
    appendAndOwn(csgt);
  }

  return csgt;
}

/**
 * Creates a new CSGRotation object, adds it to this ListOfCSGNodes
 * csgRotation and returns the CSGRotation object created. 
 *
 * @return a new CSGRotation object instance
 *
 * @see addCsgRotation(const CSGNode* csgn)
 */
CSGRotation* 
ListOfCSGNodes::createCsgRotation()
{
  CSGRotation* csgr = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgr = new CSGRotation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgr != NULL)
  {
    appendAndOwn(csgr);
  }

  return csgr;
}

/**
 * Creates a new CSGScale object, adds it to this ListOfCSGNodes
 * csgScale and returns the CSGScale object created. 
 *
 * @return a new CSGScale object instance
 *
 * @see addCsgScale(const CSGNode* csgn)
 */
CSGScale* 
ListOfCSGNodes::createCsgScale()
{
  CSGScale* csgs = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgs = new CSGScale(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgs != NULL)
  {
    appendAndOwn(csgs);
  }

  return csgs;
}

/**
 * Creates a new CSGHomogeneousTransformation object, adds it to this ListOfCSGNodes
 * csgHomogeneousTransformation and returns the CSGHomogeneousTransformation object created. 
 *
 * @return a new CSGHomogeneousTransformation object instance
 *
 * @see addCsgHomogeneousTransformation(const CSGNode* csgn)
 */
CSGHomogeneousTransformation* 
ListOfCSGNodes::createCsgHomogeneousTransformation()
{
  CSGHomogeneousTransformation* csght = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csght = new CSGHomogeneousTransformation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csght != NULL)
  {
    appendAndOwn(csght);
  }

  return csght;
}

/**
 * Creates a new CSGPseudoPrimitive object, adds it to this ListOfCSGNodes
 * csgPseudoPrimitive and returns the CSGPseudoPrimitive object created. 
 *
 * @return a new CSGPseudoPrimitive object instance
 *
 * @see addCsgPseudoPrimitive(const CSGNode* csgn)
 */
CSGPseudoPrimitive* 
ListOfCSGNodes::createCsgPseudoPrimitive()
{
  CSGPseudoPrimitive* csgpp = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgpp = new CSGPseudoPrimitive(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgpp != NULL)
  {
    appendAndOwn(csgpp);
  }

  return csgpp;
}

/**
 * Creates a new CSGSetOperator object, adds it to this ListOfCSGNodes
 * csgSetOperator and returns the CSGSetOperator object created. 
 *
 * @return a new CSGSetOperator object instance
 *
 * @see addCsgSetOperator(const CSGNode* csgn)
 */
CSGSetOperator* 
ListOfCSGNodes::createCsgSetOperator()
{
  CSGSetOperator* csgso = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgso = new CSGSetOperator(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgso != NULL)
  {
    appendAndOwn(csgso);
  }

  return csgso;
}

/*
 * Removes the nth CsgNode from this ListOfCSGNodes
 */
CSGNode*
ListOfCSGNodes::remove(unsigned int n)
{
  return static_cast<CSGNode*>(ListOf::remove(n));
}


/*
 * Removes the CsgNode from this ListOfCSGNodes with the given identifier
 */
CSGNode*
ListOfCSGNodes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGNode>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <CSGNode*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfCSGNodes::getElementName () const
{
  static const string name = "listOfCsgNodes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfCSGNodes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfCSGNodes::getItemTypeCode () const
{
  return SBML_SPATIAL_CSGNODE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CSGNode in this ListOfCSGNodes
 */
SBase*
ListOfCSGNodes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "csgNode")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGNode(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgPrimitive")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGPrimitive(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgTranslation")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGTranslation(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgRotation")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGRotation(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgScale")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGScale(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgHomogeneousTransformation")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGHomogeneousTransformation(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgPseudoPrimitive")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGPseudoPrimitive(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csgSetOperator")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGSetOperator(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Spatial package.
 */
void
ListOfCSGNodes::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGNode_t *
CSGNode_create(unsigned int level, unsigned int version,
               unsigned int pkgVersion)
{
  return new CSGNode(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGNode_free(CSGNode_t * csgn)
{
  if (csgn != NULL)
    delete csgn;
}


LIBSBML_EXTERN
CSGNode_t *
CSGNode_clone(CSGNode_t * csgn)
{
  if (csgn != NULL)
  {
    return static_cast<CSGNode_t*>(csgn->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
CSGNode_getId(const CSGNode_t * csgn)
{
	return (csgn != NULL && csgn->isSetId()) ? csgn->getId().c_str() : NULL;
}


LIBSBML_EXTERN
int
CSGNode_isSetId(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isSetId()) : 0;
}


LIBSBML_EXTERN
int
CSGNode_setId(CSGNode_t * csgn, const char * id)
{
  if (csgn != NULL)
    return (id == NULL) ? csgn->setId("") : csgn->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGNode_unsetId(CSGNode_t * csgn)
{
  return (csgn != NULL) ? csgn->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGNode_hasRequiredAttributes(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
CSGNode_t *
ListOfCSGNodes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCSGNodes *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
CSGNode_t *
ListOfCSGNodes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCSGNodes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


