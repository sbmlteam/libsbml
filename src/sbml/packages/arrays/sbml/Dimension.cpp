/**
 * @file Dimension.cpp
 * @brief Implementation of the Dimension class.
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/arrays/sbml/Dimension.h>
#include <sbml/packages/arrays/sbml/ListOfDimensions.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Dimension using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
Dimension::Dimension(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
  : SBase(level, version)
  , mSize ("")
  , mArrayDimension (SBML_INT_MAX)
  , mIsSetArrayDimension (false)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new Dimension using the given ArraysPkgNamespaces object.
 */
Dimension::Dimension(ArraysPkgNamespaces *arraysns)
  : SBase(arraysns)
  , mSize ("")
  , mArrayDimension (SBML_INT_MAX)
  , mIsSetArrayDimension (false)
{
  setElementNamespace(arraysns->getURI());
  loadPlugins(arraysns);
}


/*
 * Copy constructor for Dimension.
 */
Dimension::Dimension(const Dimension& orig)
  : SBase( orig )
  , mSize ( orig.mSize )
  , mArrayDimension ( orig.mArrayDimension )
  , mIsSetArrayDimension ( orig.mIsSetArrayDimension )
{
}


/*
 * Assignment operator for Dimension.
 */
Dimension&
Dimension::operator=(const Dimension& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSize = rhs.mSize;
    mArrayDimension = rhs.mArrayDimension;
    mIsSetArrayDimension = rhs.mIsSetArrayDimension;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Dimension object.
 */
Dimension*
Dimension::clone() const
{
  return new Dimension(*this);
}


/*
 * Destructor for Dimension.
 */
Dimension::~Dimension()
{
}


/*
 * Returns the value of the "id" attribute of this Dimension.
 */
const std::string&
Dimension::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Dimension.
 */
const std::string&
Dimension::getName() const
{
  return mName;
}


/*
 * Returns the value of the "size" attribute of this Dimension.
 */
const std::string&
Dimension::getSize() const
{
  return mSize;
}


/*
 * Returns the value of the "arrayDimension" attribute of this Dimension.
 */
unsigned int
Dimension::getArrayDimension() const
{
  return mArrayDimension;
}


/*
 * Predicate returning @c true if this Dimension's "id" attribute is set.
 */
bool
Dimension::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Dimension's "name" attribute is set.
 */
bool
Dimension::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this Dimension's "size" attribute is set.
 */
bool
Dimension::isSetSize() const
{
  return (mSize.empty() == false);
}


/*
 * Predicate returning @c true if this Dimension's "arrayDimension" attribute
 * is set.
 */
bool
Dimension::isSetArrayDimension() const
{
  return mIsSetArrayDimension;
}


/*
 * Sets the value of the "id" attribute of this Dimension.
 */
int
Dimension::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this Dimension.
 */
int
Dimension::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "size" attribute of this Dimension.
 */
int
Dimension::setSize(const std::string& size)
{
  if (!(SyntaxChecker::isValidInternalSId(size)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSize = size;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "arrayDimension" attribute of this Dimension.
 */
int
Dimension::setArrayDimension(unsigned int arrayDimension)
{
  mArrayDimension = arrayDimension;
  mIsSetArrayDimension = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this Dimension.
 */
int
Dimension::unsetId()
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
 * Unsets the value of the "name" attribute of this Dimension.
 */
int
Dimension::unsetName()
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
 * Unsets the value of the "size" attribute of this Dimension.
 */
int
Dimension::unsetSize()
{
  mSize.erase();

  if (mSize.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "arrayDimension" attribute of this Dimension.
 */
int
Dimension::unsetArrayDimension()
{
  mArrayDimension = SBML_INT_MAX;
  mIsSetArrayDimension = false;

  if (isSetArrayDimension() == false)
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
Dimension::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetSize() && mSize == oldid)
  {
    setSize(newid);
  }
}


/*
 * Returns the XML element name of this Dimension object.
 */
const std::string&
Dimension::getElementName() const
{
  static const string name = "dimension";
  return name;
}


/*
 * Returns the libSBML type code for this Dimension object.
 */
int
Dimension::getTypeCode() const
{
  return SBML_ARRAYS_DIMENSION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Dimension object have been set.
 */
bool
Dimension::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetSize() == false)
  {
    allPresent = false;
  }

  if (isSetArrayDimension() == false)
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
Dimension::writeElements(XMLOutputStream& stream) const
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
Dimension::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Dimension::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Dimension::enablePackageInternal(const std::string& pkgURI,
                                 const std::string& pkgPrefix,
                                 bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::getAttribute(const std::string& attributeName,
                        unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "arrayDimension")
  {
    value = getArrayDimension();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "size")
  {
    value = getSize();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Dimension's attribute "attributeName" is
 * set.
 */
bool
Dimension::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "size")
  {
    value = isSetSize();
  }
  else if (attributeName == "arrayDimension")
  {
    value = isSetArrayDimension();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "arrayDimension")
  {
    return_value = setArrayDimension(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "size")
  {
    return_value = setSize(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Dimension.
 */
int
Dimension::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "size")
  {
    value = unsetSize();
  }
  else if (attributeName == "arrayDimension")
  {
    value = unsetArrayDimension();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Dimension::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("size");

  attributes.add("arrayDimension");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Dimension::readAttributes(const XMLAttributes& attributes,
                          const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDimensions*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("arrays", ArraysDimensionAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("arrays",
          ArraysSBaseLODimensionsAllowedCoreAttributes, pkgVersion, level,
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
        log->logPackageError("arrays", ArraysDimensionAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("arrays", ArraysDimensionAllowedCoreAttributes,
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
      logEmptyString(mId, level, version, "<Dimension>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("arrays", ArraysIdSyntaxRule, pkgVersion, level,
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
      logEmptyString(mName, level, version, "<Dimension>");
    }
  }

  // 
  // size SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("size", mSize);

  if (assigned == true)
  {
    if (mSize.empty() == true)
    {
      logEmptyString(mSize, level, version, "<Dimension>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSize) == false)
    {
      std::string msg = "The size attribute on the <" + getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mSize + "', which does not conform to the syntax.";
      log->logPackageError("arrays", ArraysDimensionSizeMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Arrays attribute 'size' is missing from the "
      "<Dimension> element.";
    log->logPackageError("arrays", ArraysDimensionAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // arrayDimension uint (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetArrayDimension = attributes.readInto("arrayDimension",
    mArrayDimension);

  if ( mIsSetArrayDimension == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Arrays attribute 'arrayDimension' from the "
        "<Dimension> element must be an integer.";
      log->logPackageError("arrays",
        ArraysDimensionArrayDimensionMustBeUnInteger, pkgVersion, level, version,
          message);
    }
    else
    {
      std::string message = "Arrays attribute 'arrayDimension' is missing from "
        "the <Dimension> element.";
      log->logPackageError("arrays", ArraysDimensionAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Dimension::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetSize() == true)
  {
    stream.writeAttribute("size", getPrefix(), mSize);
  }

  if (isSetArrayDimension() == true)
  {
    stream.writeAttribute("arrayDimension", getPrefix(), mArrayDimension);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Dimension_t using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
LIBSBML_EXTERN
Dimension_t *
Dimension_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion)
{
  return new Dimension(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Dimension_t object.
 */
LIBSBML_EXTERN
Dimension_t*
Dimension_clone(const Dimension_t* d)
{
  if (d != NULL)
  {
    return static_cast<Dimension_t*>(d->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Dimension_t object.
 */
LIBSBML_EXTERN
void
Dimension_free(Dimension_t* d)
{
  if (d != NULL)
  {
    delete d;
  }
}


/*
 * Returns the value of the "id" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
const char *
Dimension_getId(const Dimension_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getId().empty() ? NULL : safe_strdup(d->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
const char *
Dimension_getName(const Dimension_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getName().empty() ? NULL : safe_strdup(d->getName().c_str());
}


/*
 * Returns the value of the "size" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
const char *
Dimension_getSize(const Dimension_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getSize().empty() ? NULL : safe_strdup(d->getSize().c_str());
}


/*
 * Returns the value of the "arrayDimension" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
unsigned int
Dimension_getArrayDimension(const Dimension_t * d)
{
  return (d != NULL) ? d->getArrayDimension() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 if this Dimension_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Dimension_isSetId(const Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 if this Dimension_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
Dimension_isSetName(const Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 if this Dimension_t's "size" attribute is set.
 */
LIBSBML_EXTERN
int
Dimension_isSetSize(const Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetSize()) : 0;
}


/*
 * Predicate returning @c 1 if this Dimension_t's "arrayDimension" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Dimension_isSetArrayDimension(const Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetArrayDimension()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_setId(Dimension_t * d, const char * id)
{
  return (d != NULL) ? d->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_setName(Dimension_t * d, const char * name)
{
  return (d != NULL) ? d->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "size" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_setSize(Dimension_t * d, const char * size)
{
  return (d != NULL) ? d->setSize(size) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "arrayDimension" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_setArrayDimension(Dimension_t * d, unsigned int arrayDimension)
{
  return (d != NULL) ? d->setArrayDimension(arrayDimension) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_unsetId(Dimension_t * d)
{
  return (d != NULL) ? d->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_unsetName(Dimension_t * d)
{
  return (d != NULL) ? d->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "size" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_unsetSize(Dimension_t * d)
{
  return (d != NULL) ? d->unsetSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "arrayDimension" attribute of this Dimension_t.
 */
LIBSBML_EXTERN
int
Dimension_unsetArrayDimension(Dimension_t * d)
{
  return (d != NULL) ? d->unsetArrayDimension() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this Dimension_t
 * object have been set.
 */
LIBSBML_EXTERN
int
Dimension_hasRequiredAttributes(const Dimension_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


