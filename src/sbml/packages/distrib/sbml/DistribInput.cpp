/**
 * @file DistribInput.cpp
 * @brief Implementation of the DistribInput class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/DistribInput.h>
#include <sbml/packages/distrib/sbml/ListOfDistribInputs.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribInput using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribInput::DistribInput(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : SBase(level, version)
  , mIndex (SBML_INT_MAX)
  , mIsSetIndex (false)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribInput using the given DistribPkgNamespaces object.
 */
DistribInput::DistribInput(DistribPkgNamespaces *distribns)
  : SBase(distribns)
  , mIndex (SBML_INT_MAX)
  , mIsSetIndex (false)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribInput.
 */
DistribInput::DistribInput(const DistribInput& orig)
  : SBase( orig )
  , mIndex ( orig.mIndex )
  , mIsSetIndex ( orig.mIsSetIndex )
{
}


/*
 * Assignment operator for DistribInput.
 */
DistribInput&
DistribInput::operator=(const DistribInput& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mIndex = rhs.mIndex;
    mIsSetIndex = rhs.mIsSetIndex;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribInput object.
 */
DistribInput*
DistribInput::clone() const
{
  return new DistribInput(*this);
}


/*
 * Destructor for DistribInput.
 */
DistribInput::~DistribInput()
{
}


/*
 * Returns the value of the "id" attribute of this DistribInput.
 */
const std::string&
DistribInput::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribInput.
 */
const std::string&
DistribInput::getName() const
{
  return mName;
}


/*
 * Returns the value of the "index" attribute of this DistribInput.
 */
unsigned int
DistribInput::getIndex() const
{
  return mIndex;
}


/*
 * Predicate returning @c true if this DistribInput's "id" attribute is set.
 */
bool
DistribInput::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribInput's "name" attribute is set.
 */
bool
DistribInput::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this DistribInput's "index" attribute is set.
 */
bool
DistribInput::isSetIndex() const
{
  return mIsSetIndex;
}


/*
 * Sets the value of the "id" attribute of this DistribInput.
 */
int
DistribInput::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribInput.
 */
int
DistribInput::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "index" attribute of this DistribInput.
 */
int
DistribInput::setIndex(unsigned int index)
{
  mIndex = index;
  mIsSetIndex = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribInput.
 */
int
DistribInput::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribInput.
 */
int
DistribInput::unsetName()
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
 * Unsets the value of the "index" attribute of this DistribInput.
 */
int
DistribInput::unsetIndex()
{
  mIndex = SBML_INT_MAX;
  mIsSetIndex = false;

  if (isSetIndex() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this DistribInput object.
 */
const std::string&
DistribInput::getElementName() const
{
  static const string name = "input";
  return name;
}


/*
 * Returns the libSBML type code for this DistribInput object.
 */
int
DistribInput::getTypeCode() const
{
  return SBML_DISTRIB_DISTRIBINPUT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribInput object have been set.
 */
bool
DistribInput::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribInput::writeElements(XMLOutputStream& stream) const
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
DistribInput::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribInput::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribInput::enablePackageInternal(const std::string& pkgURI,
                                    const std::string& pkgPrefix,
                                    bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "index")
  {
    value = getIndex();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribInput's attribute "attributeName"
 * is set.
 */
bool
DistribInput::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "index")
  {
    value = isSetIndex();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "index")
  {
    return_value = setIndex(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this DistribInput.
 */
int
DistribInput::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "index")
  {
    value = unsetIndex();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribInput::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("index");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("index");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribInput::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDistribInputs*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribDistribInputAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribDrawFromDistributionLODistribInputsAllowedCoreAttributes,
            pkgVersion, level, version, details);
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
        log->logPackageError("distrib", DistribDistribInputAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribInputAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribInput::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribInput>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribInput>");
    }
  }

  // 
  // index uint (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetIndex = attributes.readInto("index", mIndex);

  if ( mIsSetIndex == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'index' from the <DistribInput> "
        "element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribInputIndexMustBeNonNegativeInteger, pkgVersion, level,
          version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribInput::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribInput>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;

  // 
  // index uint (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetIndex = attributes.readInto("index", mIndex);

  if ( mIsSetIndex == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'index' from the <DistribInput> "
        "element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribInputIndexMustBeNonNegativeInteger, pkgVersion, level,
          version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribInput::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribInput::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetIndex() == true)
  {
    stream.writeAttribute("index", getPrefix(), mIndex);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribInput::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
  if (isSetIndex() == true)
  {
    stream.writeAttribute("index", getPrefix(), mIndex);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribInput_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribInput_t *
DistribInput_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion)
{
  return new DistribInput(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribInput_t object.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribInput_clone(const DistribInput_t* di)
{
  if (di != NULL)
  {
    return static_cast<DistribInput_t*>(di->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribInput_t object.
 */
LIBSBML_EXTERN
void
DistribInput_free(DistribInput_t* di)
{
  if (di != NULL)
  {
    delete di;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
char *
DistribInput_getId(const DistribInput_t * di)
{
  if (di == NULL)
  {
    return NULL;
  }

  return di->getId().empty() ? NULL : safe_strdup(di->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
char *
DistribInput_getName(const DistribInput_t * di)
{
  if (di == NULL)
  {
    return NULL;
  }

  return di->getName().empty() ? NULL : safe_strdup(di->getName().c_str());
}


/*
 * Returns the value of the "index" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
unsigned int
DistribInput_getIndex(const DistribInput_t * di)
{
  return (di != NULL) ? di->getIndex() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this DistribInput_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribInput_isSetId(const DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribInput_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribInput_isSetName(const DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribInput_t's "index" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribInput_isSetIndex(const DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetIndex()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_setId(DistribInput_t * di, const char * id)
{
  return (di != NULL) ? di->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_setName(DistribInput_t * di, const char * name)
{
  return (di != NULL) ? di->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "index" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_setIndex(DistribInput_t * di, unsigned int index)
{
  return (di != NULL) ? di->setIndex(index) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_unsetId(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_unsetName(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "index" attribute of this DistribInput_t.
 */
LIBSBML_EXTERN
int
DistribInput_unsetIndex(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetIndex() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribInput_t object have been set.
 */
LIBSBML_EXTERN
int
DistribInput_hasRequiredAttributes(const DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


