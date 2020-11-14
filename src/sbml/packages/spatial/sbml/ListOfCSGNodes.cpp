/**
 * @file ListOfCSGNodes.cpp
 * @brief Implementation of the ListOfCSGNodes class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/spatial/sbml/ListOfCSGNodes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfCSGNodes using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfCSGNodes::ListOfCSGNodes(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
  : ListOf(level, version)
  , mElementName("csgNode")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfCSGNodes using the given SpatialPkgNamespaces object.
 */
ListOfCSGNodes::ListOfCSGNodes(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
  , mElementName("csgNode")
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfCSGNodes.
 */
ListOfCSGNodes::ListOfCSGNodes(const ListOfCSGNodes& orig)
  : ListOf( orig )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for ListOfCSGNodes.
 */
ListOfCSGNodes&
ListOfCSGNodes::operator=(const ListOfCSGNodes& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfCSGNodes object.
 */
ListOfCSGNodes*
ListOfCSGNodes::clone() const
{
  return new ListOfCSGNodes(*this);
}


/*
 * Destructor for ListOfCSGNodes.
 */
ListOfCSGNodes::~ListOfCSGNodes()
{
}


/*
 * Get a CSGNode from the ListOfCSGNodes.
 */
CSGNode*
ListOfCSGNodes::get(unsigned int n)
{
  return static_cast<CSGNode*>(ListOf::get(n));
}


/*
 * Get a CSGNode from the ListOfCSGNodes.
 */
const CSGNode*
ListOfCSGNodes::get(unsigned int n) const
{
  return static_cast<const CSGNode*>(ListOf::get(n));
}


/*
 * Get a CSGNode from the ListOfCSGNodes based on its identifier.
 */
CSGNode*
ListOfCSGNodes::get(const std::string& sid)
{
  return const_cast<CSGNode*>(static_cast<const
    ListOfCSGNodes&>(*this).get(sid));
}


/*
 * Get a CSGNode from the ListOfCSGNodes based on its identifier.
 */
const CSGNode*
ListOfCSGNodes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<CSGNode>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const CSGNode*> (*result);
}


/*
 * Removes the nth CSGNode from this ListOfCSGNodes and returns a pointer to
 * it.
 */
CSGNode*
ListOfCSGNodes::remove(unsigned int n)
{
  return static_cast<CSGNode*>(ListOf::remove(n));
}


/*
 * Removes the CSGNode from this ListOfCSGNodes based on its identifier and
 * returns a pointer to it.
 */
CSGNode*
ListOfCSGNodes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<CSGNode>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CSGNode*> (item);
}


/*
 * Adds a copy of the given CSGNode to this ListOfCSGNodes.
 */
int
ListOfCSGNodes::addCSGNode(const CSGNode* csgn)
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(csgn)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(csgn);
  }
}


/*
 * Get the number of CSGNode objects in this ListOfCSGNodes.
 */
unsigned int
ListOfCSGNodes::getNumCSGNodes() const
{
  return size();
}


/*
 * Creates a new CSGPrimitive object, adds it to this ListOfCSGNodes object and
 * returns the CSGPrimitive object created.
 */
CSGPrimitive*
ListOfCSGNodes::createCSGPrimitive()
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
  }

  if (csgp != NULL)
  {
    appendAndOwn(csgp);
  }

  return csgp;
}


/*
 * Creates a new CSGTranslation object, adds it to this ListOfCSGNodes object
 * and returns the CSGTranslation object created.
 */
CSGTranslation*
ListOfCSGNodes::createCSGTranslation()
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
  }

  if (csgt != NULL)
  {
    appendAndOwn(csgt);
  }

  return csgt;
}


/*
 * Creates a new CSGRotation object, adds it to this ListOfCSGNodes object and
 * returns the CSGRotation object created.
 */
CSGRotation*
ListOfCSGNodes::createCSGRotation()
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
  }

  if (csgr != NULL)
  {
    appendAndOwn(csgr);
  }

  return csgr;
}


/*
 * Creates a new CSGScale object, adds it to this ListOfCSGNodes object and
 * returns the CSGScale object created.
 */
CSGScale*
ListOfCSGNodes::createCSGScale()
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
  }

  if (csgs != NULL)
  {
    appendAndOwn(csgs);
  }

  return csgs;
}


/*
 * Creates a new CSGHomogeneousTransformation object, adds it to this
 * ListOfCSGNodes object and returns the CSGHomogeneousTransformation object
 * created.
 */
CSGHomogeneousTransformation*
ListOfCSGNodes::createCSGHomogeneousTransformation()
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
  }

  if (csght != NULL)
  {
    appendAndOwn(csght);
  }

  return csght;
}


/*
 * Creates a new CSGSetOperator object, adds it to this ListOfCSGNodes object
 * and returns the CSGSetOperator object created.
 */
CSGSetOperator*
ListOfCSGNodes::createCSGSetOperator()
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
  }

  if (csgso != NULL)
  {
    appendAndOwn(csgso);
  }

  return csgso;
}


/*
 * Returns the XML element name of this ListOfCSGNodes object.
 */
const std::string&
ListOfCSGNodes::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this ListOfCSGNodes object.
 */
void
ListOfCSGNodes::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this ListOfCSGNodes object.
 */
int
ListOfCSGNodes::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfCSGNodes object.
 */
int
ListOfCSGNodes::getItemTypeCode() const
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
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "csgNode")
  {
    object = new CSGNode(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgPrimitive")
  {
    object = new CSGPrimitive(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgTranslation")
  {
    object = new CSGTranslation(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgRotation")
  {
    object = new CSGRotation(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgScale")
  {
    object = new CSGScale(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgHomogeneousTransformation")
  {
    object = new CSGHomogeneousTransformation(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgSetOperator")
  {
    object = new CSGSetOperator(spatialns);
    appendAndOwn(object);
  }

  delete spatialns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Spatial package
 */
void
ListOfCSGNodes::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * checks concrete types
 */
bool
ListOfCSGNodes::isValidTypeForList(SBase* item)
{
  unsigned int tc = item->getTypeCode();

  return ((tc == SBML_SPATIAL_CSGPRIMITIVE) || (tc ==
    SBML_SPATIAL_CSGTRANSLATION) || (tc == SBML_SPATIAL_CSGROTATION) || (tc ==
      SBML_SPATIAL_CSGSCALE) || (tc == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)
        || (tc == SBML_SPATIAL_CSGSETOPERATOR));
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a CSGNode_t from the ListOf_t.
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_getCSGNode(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCSGNodes*>(lo)->get(n);
}


/*
 * Get a CSGNode_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCSGNodes*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth CSGNode_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCSGNodes*>(lo)->remove(n);
}


/*
 * Removes the CSGNode_t from this ListOf_t based on its identifier and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
CSGNode_t*
ListOfCSGNodes_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCSGNodes*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


