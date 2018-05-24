/**
 * @file DistribBase.cpp
 * @brief Implementation of the DistribBase class.
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
#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribBase using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribBase::DistribBase(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : SBase(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribBase using the given DistribPkgNamespaces object.
 */
DistribBase::DistribBase(DistribPkgNamespaces *distribns)
  : SBase(distribns)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribBase.
 */
DistribBase::DistribBase(const DistribBase& orig)
  : SBase( orig )
{
  connectToChild();
}


/*
 * Assignment operator for DistribBase.
 */
DistribBase&
DistribBase::operator=(const DistribBase& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribBase object.
 */
DistribBase*
DistribBase::clone() const
{
  return new DistribBase(*this);
}


/*
 * Destructor for DistribBase.
 */
DistribBase::~DistribBase()
{
}


/*
 * Returns the value of the "id" attribute of this DistribBase.
 */
const std::string&
DistribBase::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribBase.
 */
const std::string&
DistribBase::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribBase's "id" attribute is set.
 */
bool
DistribBase::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribBase's "name" attribute is
 * set.
 */
bool
DistribBase::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::unsetName()
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
 * Returns the XML element name of this DistribBase object.
 */
const std::string&
DistribBase::getElementName() const
{
  static const string name = "Base";
  return name;
}


/*
 * Returns the libSBML type code for this DistribBase object.
 */
int
DistribBase::getTypeCode() const
{
  return SBML_DISTRIB_BASE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribBase object have been set.
 */
bool
DistribBase::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribBase object have been set.
 */
bool
DistribBase::hasRequiredElements() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribBase::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribBase's attribute
 * "attributeName" is set.
 */
bool
DistribBase::isSetAttribute(const std::string& attributeName) const
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
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::unsetAttribute(const std::string& attributeName)
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
 * Returns the number of "elementName" in this DistribBase.
 */
unsigned int
DistribBase::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribBase.
 */
SBase*
DistribBase::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  return obj;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribBase::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
  }

}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribBase::readAttributes(const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  SBase::readAttributes(attributes, expectedAttributes);

  if (level == 3 && version == 1)
  {
    // 
    // id SId (use = "optional" )
    // 

    XMLTriple tripleID("id", mURI, getPrefix());
    assigned = attributes.readInto(tripleID, mId);

    if (assigned == true)
    {
      if (mId.empty() == true)
      {
        logEmptyString(mId, level, version, "<DistribBase>");
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
        logEmptyString(mName, level, version, "<DistribBase>");
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribBase::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  //unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1)
  {
    if (isSetId() == true)
    {
      stream.writeAttribute("id", getPrefix(), mId);
    }

    if (isSetName() == true)
    {
      stream.writeAttribute("name", getPrefix(), mName);
    }
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



#endif /* __cplusplus */


/*
 * Creates a new DistribBase_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new DistribBase(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribBase_t object.
 */
LIBSBML_EXTERN
DistribBase_t*
DistribBase_clone(const DistribBase_t* dc)
{
  if (dc != NULL)
  {
    return static_cast<DistribBase_t*>(dc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribBase_t object.
 */
LIBSBML_EXTERN
void
DistribBase_free(DistribBase_t* dc)
{
  if (dc != NULL)
  {
    delete dc;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getId(const DistribBase_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return dc->getId().empty() ? NULL : safe_strdup(dc->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getName(const DistribBase_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return dc->getName().empty() ? NULL : safe_strdup(dc->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "id" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetId(const DistribBase_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetName(const DistribBase_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setId(DistribBase_t * dc, const char * id)
{
  return (dc != NULL) ? dc->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setName(DistribBase_t * dc, const char * name)
{
  return (dc != NULL) ? dc->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetId(DistribBase_t * dc)
{
  return (dc != NULL) ? dc->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetName(DistribBase_t * dc)
{
  return (dc != NULL) ? dc->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBase_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredAttributes(const DistribBase_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribBase_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredElements(const DistribBase_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


