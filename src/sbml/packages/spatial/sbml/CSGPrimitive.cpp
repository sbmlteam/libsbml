/**
 * @file CSGPrimitive.cpp
 * @brief Implementation of the CSGPrimitive class.
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
#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGPrimitive using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGPrimitive::CSGPrimitive(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : CSGNode(level, version, pkgVersion)
  , mPrimitiveType (SPATIAL_PRIMITIVEKIND_INVALID)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGPrimitive using the given SpatialPkgNamespaces object.
 */
CSGPrimitive::CSGPrimitive(SpatialPkgNamespaces *spatialns)
  : CSGNode(spatialns)
  , mPrimitiveType (SPATIAL_PRIMITIVEKIND_INVALID)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGPrimitive.
 */
CSGPrimitive::CSGPrimitive(const CSGPrimitive& orig)
  : CSGNode( orig )
  , mPrimitiveType ( orig.mPrimitiveType )
{
}


/*
 * Assignment operator for CSGPrimitive.
 */
CSGPrimitive&
CSGPrimitive::operator=(const CSGPrimitive& rhs)
{
  if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mPrimitiveType = rhs.mPrimitiveType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGPrimitive object.
 */
CSGPrimitive*
CSGPrimitive::clone() const
{
  return new CSGPrimitive(*this);
}


/*
 * Destructor for CSGPrimitive.
 */
CSGPrimitive::~CSGPrimitive()
{
}


/*
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive.
 */
PrimitiveKind_t
CSGPrimitive::getPrimitiveType() const
{
  return mPrimitiveType;
}


/*
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive.
 */
std::string
CSGPrimitive::getPrimitiveTypeAsString() const
{
  return PrimitiveKind_toString(mPrimitiveType);
}


/*
 * Predicate returning @c true if this CSGPrimitive's "primitiveType" attribute
 * is set.
 */
bool
CSGPrimitive::isSetPrimitiveType() const
{
  return (mPrimitiveType != SPATIAL_PRIMITIVEKIND_INVALID);
}


/*
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setPrimitiveType(const PrimitiveKind_t primitiveType)
{
  if (PrimitiveKind_isValid(primitiveType) == 0)
  {
    mPrimitiveType = SPATIAL_PRIMITIVEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mPrimitiveType = primitiveType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setPrimitiveType(const std::string& primitiveType)
{
  if (PrimitiveKind_isValidString(primitiveType.c_str()) == 0)
  {
    mPrimitiveType = SPATIAL_PRIMITIVEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mPrimitiveType = PrimitiveKind_fromString(primitiveType.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "primitiveType" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::unsetPrimitiveType()
{
  mPrimitiveType = SPATIAL_PRIMITIVEKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this CSGPrimitive object.
 */
const std::string&
CSGPrimitive::getElementName() const
{
  static const string name = "csgPrimitive";
  return name;
}


/*
 * Returns the libSBML type code for this CSGPrimitive object.
 */
int
CSGPrimitive::getTypeCode() const
{
  return SBML_SPATIAL_CSGPRIMITIVE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGPrimitive object have been set.
 */
bool
CSGPrimitive::hasRequiredAttributes() const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetPrimitiveType() == false)
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
CSGPrimitive::writeElements(XMLOutputStream& stream) const
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
CSGPrimitive::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGPrimitive::setSBMLDocument(SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGPrimitive::enablePackageInternal(const std::string& pkgURI,
                                    const std::string& pkgPrefix,
                                    bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::getAttribute(const std::string& attributeName,
                           std::string& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "primitiveType")
  {
    value = getPrimitiveTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGPrimitive's attribute "attributeName"
 * is set.
 */
bool
CSGPrimitive::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGNode::isSetAttribute(attributeName);

  if (attributeName == "primitiveType")
  {
    value = isSetPrimitiveType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setAttribute(const std::string& attributeName, double value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::setAttribute(const std::string& attributeName,
                           const std::string& value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  if (attributeName == "primitiveType")
  {
    return_value = setPrimitiveType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGPrimitive.
 */
int
CSGPrimitive::unsetAttribute(const std::string& attributeName)
{
  int value = CSGNode::unsetAttribute(attributeName);

  if (attributeName == "primitiveType")
  {
    value = unsetPrimitiveType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGPrimitive::createObject(XMLInputStream& stream)
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
CSGPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("primitiveType");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGPrimitive::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGNode::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGPrimitiveAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCSGPrimitiveAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  // 
  // primitiveType enum (use = "required" )
  // 

  std::string primitiveType;
  assigned = attributes.readInto("primitiveType", primitiveType);

  if (assigned == true)
  {
    if (primitiveType.empty() == true)
    {
      logEmptyString(primitiveType, level, version, "<csgPrimitive>");
    }
    else
    {
      mPrimitiveType = PrimitiveKind_fromString(primitiveType.c_str());

      if (PrimitiveKind_isValid(mPrimitiveType) == 0)
      {
        std::string msg = "The primitiveType on the <csgPrimitive> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + primitiveType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialCSGPrimitivePrimitiveTypeMustBePrimitiveKindEnum, pkgVersion,
            level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'primitiveType' is missing.";
    log->logPackageError("spatial", SpatialCSGPrimitiveAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGPrimitive::writeAttributes(XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetPrimitiveType() == true)
  {
    stream.writeAttribute("primitiveType", getPrefix(),
      PrimitiveKind_toString(mPrimitiveType));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGPrimitive_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGPrimitive_t *
CSGPrimitive_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion)
{
  return new CSGPrimitive(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGPrimitive_t object.
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGPrimitive_clone(const CSGPrimitive_t* csgp)
{
  if (csgp != NULL)
  {
    return static_cast<CSGPrimitive_t*>(csgp->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGPrimitive_t object.
 */
LIBSBML_EXTERN
void
CSGPrimitive_free(CSGPrimitive_t* csgp)
{
  if (csgp != NULL)
  {
    delete csgp;
  }
}


/*
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive_t.
 */
LIBSBML_EXTERN
PrimitiveKind_t
CSGPrimitive_getPrimitiveType(const CSGPrimitive_t * csgp)
{
  if (csgp == NULL)
  {
    return SPATIAL_PRIMITIVEKIND_INVALID;
  }

  return csgp->getPrimitiveType();
}


/*
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive_t.
 */
LIBSBML_EXTERN
char *
CSGPrimitive_getPrimitiveTypeAsString(const CSGPrimitive_t * csgp)
{
  return (char*)(PrimitiveKind_toString(csgp->getPrimitiveType()));
}


/*
 * Predicate returning @c 1 (true) if this CSGPrimitive_t's "primitiveType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGPrimitive_isSetPrimitiveType(const CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? static_cast<int>(csgp->isSetPrimitiveType()) : 0;
}


/*
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 */
LIBSBML_EXTERN
int
CSGPrimitive_setPrimitiveType(CSGPrimitive_t * csgp,
                              PrimitiveKind_t primitiveType)
{
  return (csgp != NULL) ? csgp->setPrimitiveType(primitiveType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 */
LIBSBML_EXTERN
int
CSGPrimitive_setPrimitiveTypeAsString(CSGPrimitive_t * csgp,
                                      const char * primitiveType)
{
  return (csgp != NULL) ? csgp->setPrimitiveType(primitiveType):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 */
LIBSBML_EXTERN
int
CSGPrimitive_unsetPrimitiveType(CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? csgp->unsetPrimitiveType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGPrimitive_t object have been set.
 */
LIBSBML_EXTERN
int
CSGPrimitive_hasRequiredAttributes(const CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? static_cast<int>(csgp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


