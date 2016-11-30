/**
 * @file CSGPseudoPrimitive.cpp
 * @brief Implementation of the CSGPseudoPrimitive class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGPseudoPrimitive using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : CSGNode(level, version)
  , mCsgObjectRef ("")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGPseudoPrimitive using the given SpatialPkgNamespaces
 * object.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive(SpatialPkgNamespaces *spatialns)
  : CSGNode(spatialns)
  , mCsgObjectRef ("")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive(const CSGPseudoPrimitive& orig)
  : CSGNode( orig )
  , mCsgObjectRef ( orig.mCsgObjectRef )
{
}


/*
 * Assignment operator for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive&
CSGPseudoPrimitive::operator=(const CSGPseudoPrimitive& rhs)
{
  if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mCsgObjectRef = rhs.mCsgObjectRef;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGPseudoPrimitive object.
 */
CSGPseudoPrimitive*
CSGPseudoPrimitive::clone() const
{
  return new CSGPseudoPrimitive(*this);
}


/*
 * Destructor for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive::~CSGPseudoPrimitive()
{
}


/*
 * Returns the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive.
 */
const std::string&
CSGPseudoPrimitive::getCsgObjectRef() const
{
  return mCsgObjectRef;
}


/*
 * Predicate returning @c true if this CSGPseudoPrimitive's "csgObjectRef"
 * attribute is set.
 */
bool
CSGPseudoPrimitive::isSetCsgObjectRef() const
{
  return (mCsgObjectRef.empty() == false);
}


/*
 * Sets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setCsgObjectRef(const std::string& csgObjectRef)
{
  if (!(SyntaxChecker::isValidInternalSId(csgObjectRef)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCsgObjectRef = csgObjectRef;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::unsetCsgObjectRef()
{
  mCsgObjectRef.erase();

  if (mCsgObjectRef.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @copydoc doc_renamesidref_common
 */
void
CSGPseudoPrimitive::renameSIdRefs(const std::string& oldid,
                                  const std::string& newid)
{
  if (isSetCsgObjectRef() && mCsgObjectRef == oldid)
  {
    setCsgObjectRef(newid);
  }
}


/*
 * Returns the XML element name of this CSGPseudoPrimitive object.
 */
const std::string&
CSGPseudoPrimitive::getElementName() const
{
  static const string name = "csgPseudoPrimitive";
  return name;
}


/*
 * Returns the libSBML type code for this CSGPseudoPrimitive object.
 */
int
CSGPseudoPrimitive::getTypeCode() const
{
  return SBML_SPATIAL_CSGPSEUDOPRIMITIVE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGPseudoPrimitive object have been set.
 */
bool
CSGPseudoPrimitive::hasRequiredAttributes() const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetCsgObjectRef() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
CSGPseudoPrimitive::writeElements(XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGPseudoPrimitive::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGPseudoPrimitive::setSBMLDocument(SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGPseudoPrimitive::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "csgObjectRef")
  {
    value = getCsgObjectRef();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::getAttribute(const std::string& attributeName,
                                 const char* value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "csgObjectRef")
  {
    value = getCsgObjectRef().c_str();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGPseudoPrimitive's attribute
 * "attributeName" is set.
 */
bool
CSGPseudoPrimitive::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGNode::isSetAttribute(attributeName);

  if (attributeName == "csgObjectRef")
  {
    value = isSetCsgObjectRef();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  if (attributeName == "csgObjectRef")
  {
    return_value = setCsgObjectRef(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::setAttribute(const std::string& attributeName,
                                 const char* value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  if (attributeName == "csgObjectRef")
  {
    return_value = setCsgObjectRef(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * CSGPseudoPrimitive.
 */
int
CSGPseudoPrimitive::unsetAttribute(const std::string& attributeName)
{
  int value = CSGNode::unsetAttribute(attributeName);

  if (attributeName == "csgObjectRef")
  {
    value = unsetCsgObjectRef();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGPseudoPrimitive::createObject(XMLInputStream& stream)
{
  SBase* obj = CSGNode::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
CSGPseudoPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("csgObjectRef");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGPseudoPrimitive::readAttributes(const XMLAttributes& attributes,
                                   const ExpectedAttributes&
                                     expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGNode::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("spatial",
        SpatialCSGPseudoPrimitiveAllowedAttributes, pkgVersion, level, version,
          details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("spatial",
        SpatialCSGPseudoPrimitiveAllowedCoreAttributes, pkgVersion, level,
          version, details);
    }
  }

  // 
  // csgObjectRef SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("csgObjectRef", mCsgObjectRef);

  if (assigned == true)
  {
    if (mCsgObjectRef.empty() == true)
    {
      logEmptyString(mCsgObjectRef, level, version, "<CSGPseudoPrimitive>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCsgObjectRef) == false)
    {
      logError(SpatialCSGPseudoPrimitiveCsgObjectRefMustBeCSGObject, level,
        version, "The attribute csgObjectRef='" + mCsgObjectRef + "' does not "
          "conform to the syntax.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'csgObjectRef' is missing from the "
      "<CSGPseudoPrimitive> element.";
    log->logPackageError("spatial", SpatialCSGPseudoPrimitiveAllowedAttributes,
      pkgVersion, level, version, message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGPseudoPrimitive::writeAttributes(XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetCsgObjectRef() == true)
  {
    stream.writeAttribute("csgObjectRef", getPrefix(), mCsgObjectRef);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGPseudoPrimitive_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new CSGPseudoPrimitive(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGPseudoPrimitive_t object.
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t*
CSGPseudoPrimitive_clone(const CSGPseudoPrimitive_t* csgpp)
{
  if (csgpp != NULL)
  {
    return static_cast<CSGPseudoPrimitive_t*>(csgpp->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGPseudoPrimitive_t object.
 */
LIBSBML_EXTERN
void
CSGPseudoPrimitive_free(CSGPseudoPrimitive_t* csgpp)
{
  if (csgpp != NULL)
  {
    delete csgpp;
  }
}


/*
 * Returns the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive_t.
 */
LIBSBML_EXTERN
const char *
CSGPseudoPrimitive_getCsgObjectRef(const CSGPseudoPrimitive_t * csgpp)
{
  if (csgpp == NULL)
  {
    return NULL;
  }

  return csgpp->getCsgObjectRef().empty() ? NULL :
    safe_strdup(csgpp->getCsgObjectRef().c_str());
}


/*
 * Predicate returning @c 1 if this CSGPseudoPrimitive_t's "csgObjectRef"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_isSetCsgObjectRef(const CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? static_cast<int>(csgpp->isSetCsgObjectRef()) : 0;
}


/*
 * Sets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive_t.
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_setCsgObjectRef(CSGPseudoPrimitive_t * csgpp,
                                   const char * csgObjectRef)
{
  return (csgpp != NULL) ? csgpp->setCsgObjectRef(csgObjectRef) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive_t.
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_unsetCsgObjectRef(CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? csgpp->unsetCsgObjectRef() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this
 * CSGPseudoPrimitive_t object have been set.
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_hasRequiredAttributes(const CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? static_cast<int>(csgpp->hasRequiredAttributes()) :
    0;
}




LIBSBML_CPP_NAMESPACE_END


