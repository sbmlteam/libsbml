/**
 * @file CSGNode.cpp
 * @brief Implementation of the CSGNode class.
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
#include <sbml/packages/spatial/sbml/CSGNode.h>
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
 * Creates a new CSGNode using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGNode::CSGNode(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion)
  : SBase(level, version)
  , mElementName("csgNode")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGNode using the given SpatialPkgNamespaces object.
 */
CSGNode::CSGNode(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mElementName("csgNode")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGNode.
 */
CSGNode::CSGNode(const CSGNode& orig)
  : SBase( orig )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for CSGNode.
 */
CSGNode&
CSGNode::operator=(const CSGNode& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGNode object.
 */
CSGNode*
CSGNode::clone() const
{
  return new CSGNode(*this);
}


/*
 * Destructor for CSGNode.
 */
CSGNode::~CSGNode()
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
 * Returns the value of the "name" attribute of this CSGNode.
 */
const std::string&
CSGNode::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this CSGNode's "id" attribute is set.
 */
bool
CSGNode::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this CSGNode's "name" attribute is set.
 */
bool
CSGNode::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this CSGNode.
 */
int
CSGNode::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this CSGNode.
 */
int
CSGNode::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this CSGNode.
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
 * Unsets the value of the "name" attribute of this CSGNode.
 */
int
CSGNode::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type
 * CSGPrimitive
 */
bool
CSGNode::isCSGPrimitive() const
{
  return dynamic_cast<const CSGPrimitive*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type
 * CSGTranslation
 */
bool
CSGNode::isCSGTranslation() const
{
  return dynamic_cast<const CSGTranslation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type
 * CSGRotation
 */
bool
CSGNode::isCSGRotation() const
{
  return dynamic_cast<const CSGRotation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type CSGScale
 */
bool
CSGNode::isCSGScale() const
{
  return dynamic_cast<const CSGScale*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type
 * CSGHomogeneousTransformation
 */
bool
CSGNode::isCSGHomogeneousTransformation() const
{
  return dynamic_cast<const CSGHomogeneousTransformation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGNode" is of type
 * CSGSetOperator
 */
bool
CSGNode::isCSGSetOperator() const
{
  return dynamic_cast<const CSGSetOperator*>(this) != NULL;
}


/*
 * Returns the XML element name of this CSGNode object.
 */
const std::string&
CSGNode::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this CSGNode object.
 */
void
CSGNode::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this CSGNode object.
 */
int
CSGNode::getTypeCode() const
{
  return SBML_SPATIAL_CSGNODE;
}


/*
 * Predicate returning @c true if all the required attributes for this CSGNode
 * object have been set.
 */
bool
CSGNode::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
CSGNode::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGNode::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGNode::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGNode::enablePackageInternal(const std::string& pkgURI,
                               const std::string& pkgPrefix,
                               bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::getAttribute(const std::string& attributeName,
                      unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::getAttribute(const std::string& attributeName,
                      std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGNode's attribute "attributeName" is
 * set.
 */
bool
CSGNode::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::setAttribute(const std::string& attributeName,
                      const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGNode.
 */
int
CSGNode::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
CSGNode::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGNode::readAttributes(const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfCSGNodes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGNodeAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGNodeAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialCSGNodeAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<csgNode>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<csgNode>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGNode::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGPrimitive using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGPrimitive_t *
CSGNode_createCSGPrimitive(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
{
  return new CSGPrimitive(level, version, pkgVersion);
}


/*
 * Creates a new CSGTranslation using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGTranslation_t *
CSGNode_createCSGTranslation(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
{
  return new CSGTranslation(level, version, pkgVersion);
}


/*
 * Creates a new CSGRotation using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGRotation_t *
CSGNode_createCSGRotation(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new CSGRotation(level, version, pkgVersion);
}


/*
 * Creates a new CSGScale using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGScale_t *
CSGNode_createCSGScale(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new CSGScale(level, version, pkgVersion);
}


/*
 * Creates a new CSGHomogeneousTransformation using the given SBML Level,
 * Version and &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGNode_createCSGHomogeneousTransformation(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
{
  return new CSGHomogeneousTransformation(level, version, pkgVersion);
}


/*
 * Creates a new CSGSetOperator using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGSetOperator_t *
CSGNode_createCSGSetOperator(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
{
  return new CSGSetOperator(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGNode_t object.
 */
LIBSBML_EXTERN
CSGNode_t*
CSGNode_clone(const CSGNode_t* csgn)
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


/*
 * Frees this CSGNode_t object.
 */
LIBSBML_EXTERN
void
CSGNode_free(CSGNode_t* csgn)
{
  if (csgn != NULL)
  {
    delete csgn;
  }
}


/*
 * Returns the value of the "id" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
char *
CSGNode_getId(const CSGNode_t * csgn)
{
  if (csgn == NULL)
  {
    return NULL;
  }

  return csgn->getId().empty() ? NULL : safe_strdup(csgn->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
char *
CSGNode_getName(const CSGNode_t * csgn)
{
  if (csgn == NULL)
  {
    return NULL;
  }

  return csgn->getName().empty() ? NULL : safe_strdup(csgn->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this CSGNode_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
CSGNode_isSetId(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGNode_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
CSGNode_isSetName(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
int
CSGNode_setId(CSGNode_t * csgn, const char * id)
{
  return (csgn != NULL) ? csgn->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
int
CSGNode_setName(CSGNode_t * csgn, const char * name)
{
  return (csgn != NULL) ? csgn->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
int
CSGNode_unsetId(CSGNode_t * csgn)
{
  return (csgn != NULL) ? csgn->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this CSGNode_t.
 */
LIBSBML_EXTERN
int
CSGNode_unsetName(CSGNode_t * csgn)
{
  return (csgn != NULL) ? csgn->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGPrimitive(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isCSGPrimitive()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGTranslation(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isCSGTranslation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGRotation(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isCSGRotation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type CSGScale_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGScale(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isCSGScale()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type
 * CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGHomogeneousTransformation(const CSGNode_t * csgn)
{
  return (csgn != NULL) ?
    static_cast<int>(csgn->isCSGHomogeneousTransformation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGNode_t is of type CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGNode_isCSGSetOperator(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->isCSGSetOperator()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGNode_t object have been set.
 */
LIBSBML_EXTERN
int
CSGNode_hasRequiredAttributes(const CSGNode_t * csgn)
{
  return (csgn != NULL) ? static_cast<int>(csgn->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


