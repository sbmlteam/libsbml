/**
 * @file KeyValuePair.cpp
 * @brief Implementation of the KeyValuePair class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/fbc/sbml/KeyValuePair.h>
#include <sbml/packages/fbc/sbml/ListOfKeyValuePairs.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new KeyValuePair using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
KeyValuePair::KeyValuePair(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : SBase(level, version)
  , mKey ("")
  , mValue ("")
  , mUri ("")
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new KeyValuePair using the given FbcPkgNamespaces object.
 */
KeyValuePair::KeyValuePair(FbcPkgNamespaces *fbcns)
  : SBase(fbcns)
  , mKey ("")
  , mValue ("")
  , mUri ("")
{
  setElementNamespace(fbcns->getURI());
  loadPlugins(fbcns);
}


/*
 * Copy constructor for KeyValuePair.
 */
KeyValuePair::KeyValuePair(const KeyValuePair& orig)
  : SBase( orig )
  , mKey ( orig.mKey )
  , mValue ( orig.mValue )
  , mUri ( orig.mUri )
{
}


/*
 * Assignment operator for KeyValuePair.
 */
KeyValuePair&
KeyValuePair::operator=(const KeyValuePair& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mKey = rhs.mKey;
    mValue = rhs.mValue;
    mUri = rhs.mUri;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this KeyValuePair object.
 */
KeyValuePair*
KeyValuePair::clone() const
{
  return new KeyValuePair(*this);
}


/*
 * Destructor for KeyValuePair.
 */
KeyValuePair::~KeyValuePair()
{
}


/*
 * Returns the value of the "id" attribute of this KeyValuePair.
 */
const std::string&
KeyValuePair::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this KeyValuePair.
 */
const std::string&
KeyValuePair::getName() const
{
  return mName;
}


/*
 * Returns the value of the "key" attribute of this KeyValuePair.
 */
const std::string&
KeyValuePair::getKey() const
{
  return mKey;
}


/*
 * Returns the value of the "value" attribute of this KeyValuePair.
 */
const std::string&
KeyValuePair::getValue() const
{
  return mValue;
}


/*
 * Returns the value of the "uri" attribute of this KeyValuePair.
 */
const std::string&
KeyValuePair::getUri() const
{
  return mUri;
}


/*
 * Predicate returning @c true if this KeyValuePair's "id" attribute is set.
 */
bool
KeyValuePair::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this KeyValuePair's "name" attribute is set.
 */
bool
KeyValuePair::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this KeyValuePair's "key" attribute is set.
 */
bool
KeyValuePair::isSetKey() const
{
  return (mKey.empty() == false);
}


/*
 * Predicate returning @c true if this KeyValuePair's "value" attribute is set.
 */
bool
KeyValuePair::isSetValue() const
{
  return (mValue.empty() == false);
}


/*
 * Predicate returning @c true if this KeyValuePair's "uri" attribute is set.
 */
bool
KeyValuePair::isSetUri() const
{
  return (mUri.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this KeyValuePair.
 */
int
KeyValuePair::setId(const std::string& id)
{
  unsigned int coreLevel = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && pkgVersion == 3)
  {
    return SyntaxChecker::checkAndSetSId(id, mId);
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "name" attribute of this KeyValuePair.
 */
int
KeyValuePair::setName(const std::string& name)
{
  unsigned int coreLevel = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && pkgVersion == 3)
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "key" attribute of this KeyValuePair.
 */
int
KeyValuePair::setKey(const std::string& key)
{
  unsigned int coreLevel = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && pkgVersion == 3)
  {
    mKey = key;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "value" attribute of this KeyValuePair.
 */
int
KeyValuePair::setValue(const std::string& value)
{
  unsigned int coreLevel = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && pkgVersion == 3)
  {
    mValue = value;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "uri" attribute of this KeyValuePair.
 */
int
KeyValuePair::setUri(const std::string& uri)
{
  unsigned int coreLevel = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && pkgVersion == 3)
  {
    mUri = uri;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Unsets the value of the "id" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetId()
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
 * Unsets the value of the "name" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetName()
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
 * Unsets the value of the "key" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetKey()
{
  mKey.erase();

  if (mKey.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "value" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetValue()
{
  mValue.erase();

  if (mValue.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "uri" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetUri()
{
  mUri.erase();

  if (mUri.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this KeyValuePair object.
 */
const std::string&
KeyValuePair::getElementName() const
{
  static const string name = "keyValuePair";
  return name;
}


/*
 * Returns the libSBML type code for this KeyValuePair object.
 */
int
KeyValuePair::getTypeCode() const
{
  return SBML_FBC_KEYVALUEPAIR;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * KeyValuePair object have been set.
 */
bool
KeyValuePair::hasRequiredAttributes() const
{
  bool allPresent = true;

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    if (isSetKey() == false)
    {
      allPresent = false;
    }
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
KeyValuePair::writeElements(XMLOutputStream& stream) const
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
KeyValuePair::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
KeyValuePair::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
KeyValuePair::enablePackageInternal(const std::string& pkgURI,
                                    const std::string& pkgPrefix,
                                    bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "key")
  {
    value = getKey();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "value")
  {
    value = getValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "uri")
  {
    value = getUri();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this KeyValuePair's attribute "attributeName"
 * is set.
 */
bool
KeyValuePair::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "key")
  {
    value = isSetKey();
  }
  else if (attributeName == "value")
  {
    value = isSetValue();
  }
  else if (attributeName == "uri")
  {
    value = isSetUri();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "key")
  {
    return_value = setKey(value);
  }
  else if (attributeName == "value")
  {
    return_value = setValue(value);
  }
  else if (attributeName == "uri")
  {
    return_value = setUri(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this KeyValuePair.
 */
int
KeyValuePair::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "key")
  {
    value = unsetKey();
  }
  else if (attributeName == "value")
  {
    value = unsetValue();
  }
  else if (attributeName == "uri")
  {
    value = unsetUri();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
KeyValuePair::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && pkgVersion == 3)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("key");
    attributes.add("value");
    attributes.add("uri");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
KeyValuePair::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfKeyValuePairs*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc", FbcSBaseLOKeyValuePairsAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc",
          FbcSBaseLOKeyValuePairsAllowedCoreAttributes, pkgVersion, level,
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
        log->logPackageError("fbc", FbcKeyValuePairAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc", FbcKeyValuePairAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    readL3V1V3Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
KeyValuePair::readL3V1V3Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<KeyValuePair>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version,
        "The id on the <" + getElementName() + "> is '" + mId + "', which does "
          "not conform to the syntax.", getLine(), getColumn());
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
      logEmptyString(mName, level, version, "<KeyValuePair>");
    }
  }

  // 
  // key string (use = "required" )
  // 

  assigned = attributes.readInto("key", mKey);

  if (assigned == true)
  {
    if (mKey.empty() == true)
    {
      logEmptyString(mKey, level, version, "<KeyValuePair>");
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'key' is missing from the "
        "<KeyValuePair> element.";
      log->logPackageError("fbc", FbcKeyValuePairAllowedAttributes, pkgVersion,
        level, version, message, getLine(), getColumn());
    }
  }

  // 
  // value string (use = "optional" )
  // 

  assigned = attributes.readInto("value", mValue);

  if (assigned == true)
  {
    if (mValue.empty() == true)
    {
      logEmptyString(mValue, level, version, "<KeyValuePair>");
    }
  }

  // 
  // uri string (use = "optional" )
  // 

  assigned = attributes.readInto("uri", mUri);

  if (assigned == true)
  {
    if (mUri.empty() == true)
    {
      logEmptyString(mUri, level, version, "<KeyValuePair>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
KeyValuePair::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && pkgVersion == 3)
  {
    writeL3V1V3Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
KeyValuePair::writeL3V1V3Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetKey() == true)
  {
    stream.writeAttribute("key", getPrefix(), mKey);
  }

  if (isSetValue() == true)
  {
    stream.writeAttribute("value", getPrefix(), mValue);
  }

  if (isSetUri() == true)
  {
    stream.writeAttribute("uri", getPrefix(), mUri);
  }
}

/** @endcond */


/*
* Creates an XMLNode object from this.
*/
XMLNode 
KeyValuePair::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple(getElementName(), "", "");
  XMLAttributes att = XMLAttributes();

  if (isSetId() == true)
  {
    att.add("id", mId);
  }

  if (isSetName() == true)
  {
    att.add("name", mName);
  }

  if (isSetKey() == true)
  {
    att.add("key", mKey);
  }

  if (isSetValue() == true)
  {
    att.add("value", mValue);
  }

  if (isSetUri() == true)
  {
    att.add("uri", mUri);
  }

  XMLToken token = XMLToken(triple, att, xmlns);
  XMLNode node(token);

  return node;
}




#endif /* __cplusplus */


/*
 * Creates a new KeyValuePair_t using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
KeyValuePair_t *
KeyValuePair_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion)
{
  return new KeyValuePair(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this KeyValuePair_t object.
 */
LIBSBML_EXTERN
KeyValuePair_t*
KeyValuePair_clone(const KeyValuePair_t* kvp)
{
  if (kvp != NULL)
  {
    return static_cast<KeyValuePair_t*>(kvp->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this KeyValuePair_t object.
 */
LIBSBML_EXTERN
void
KeyValuePair_free(KeyValuePair_t* kvp)
{
  if (kvp != NULL)
  {
    delete kvp;
  }
}


/*
 * Returns the value of the "id" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
char *
KeyValuePair_getId(const KeyValuePair_t * kvp)
{
  if (kvp == NULL)
  {
    return NULL;
  }

  return kvp->getId().empty() ? NULL : safe_strdup(kvp->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
char *
KeyValuePair_getName(const KeyValuePair_t * kvp)
{
  if (kvp == NULL)
  {
    return NULL;
  }

  return kvp->getName().empty() ? NULL : safe_strdup(kvp->getName().c_str());
}


/*
 * Returns the value of the "key" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
char *
KeyValuePair_getKey(const KeyValuePair_t * kvp)
{
  if (kvp == NULL)
  {
    return NULL;
  }

  return kvp->getKey().empty() ? NULL : safe_strdup(kvp->getKey().c_str());
}


/*
 * Returns the value of the "value" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
char *
KeyValuePair_getValue(const KeyValuePair_t * kvp)
{
  if (kvp == NULL)
  {
    return NULL;
  }

  return kvp->getValue().empty() ? NULL : safe_strdup(kvp->getValue().c_str());
}


/*
 * Returns the value of the "uri" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
char *
KeyValuePair_getUri(const KeyValuePair_t * kvp)
{
  if (kvp == NULL)
  {
    return NULL;
  }

  return kvp->getUri().empty() ? NULL : safe_strdup(kvp->getUri().c_str());
}


/*
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetId(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetName(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "key" attribute is
 * set.
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetKey(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->isSetKey()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "value" attribute
 * is set.
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetValue(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->isSetValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "uri" attribute is
 * set.
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetUri(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->isSetUri()) : 0;
}


/*
 * Sets the value of the "id" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_setId(KeyValuePair_t * kvp, const char * id)
{
  return (kvp != NULL) ? kvp->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_setName(KeyValuePair_t * kvp, const char * name)
{
  return (kvp != NULL) ? kvp->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "key" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_setKey(KeyValuePair_t * kvp, const char * key)
{
  return (kvp != NULL) ? kvp->setKey(key) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "value" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_setValue(KeyValuePair_t * kvp, const char * value)
{
  return (kvp != NULL) ? kvp->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "uri" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_setUri(KeyValuePair_t * kvp, const char * uri)
{
  return (kvp != NULL) ? kvp->setUri(uri) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetId(KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? kvp->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetName(KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? kvp->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "key" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetKey(KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? kvp->unsetKey() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetValue(KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? kvp->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "uri" attribute of this KeyValuePair_t.
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetUri(KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? kvp->unsetUri() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * KeyValuePair_t object have been set.
 */
LIBSBML_EXTERN
int
KeyValuePair_hasRequiredAttributes(const KeyValuePair_t * kvp)
{
  return (kvp != NULL) ? static_cast<int>(kvp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


