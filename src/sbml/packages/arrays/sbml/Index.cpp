/**
 * @file Index.cpp
 * @brief Implementation of the Index class.
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
#include <sbml/packages/arrays/sbml/Index.h>
#include <sbml/packages/arrays/sbml/ListOfIndices.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>
#include <sbml/math/MathML.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Index using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
Index::Index(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
  : SBase(level, version)
  , mReferencedAttribute ("")
  , mArrayDimension (SBML_INT_MAX)
  , mIsSetArrayDimension (false)
  , mMath (NULL)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Index using the given ArraysPkgNamespaces object.
 */
Index::Index(ArraysPkgNamespaces *arraysns)
  : SBase(arraysns)
  , mReferencedAttribute ("")
  , mArrayDimension (SBML_INT_MAX)
  , mIsSetArrayDimension (false)
  , mMath (NULL)
{
  setElementNamespace(arraysns->getURI());
  connectToChild();
  loadPlugins(arraysns);
}


/*
 * Copy constructor for Index.
 */
Index::Index(const Index& orig)
  : SBase( orig )
  , mReferencedAttribute ( orig.mReferencedAttribute )
  , mArrayDimension ( orig.mArrayDimension )
  , mIsSetArrayDimension ( orig.mIsSetArrayDimension )
  , mMath ( NULL )
{
  if (orig.mMath != NULL)
  {
    mMath = orig.mMath->deepCopy();
  }

  connectToChild();
}


/*
 * Assignment operator for Index.
 */
Index&
Index::operator=(const Index& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mReferencedAttribute = rhs.mReferencedAttribute;
    mArrayDimension = rhs.mArrayDimension;
    mIsSetArrayDimension = rhs.mIsSetArrayDimension;
    delete mMath;
    if (rhs.mMath != NULL)
    {
      mMath = rhs.mMath->deepCopy();
    }
    else
    {
      mMath = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Index object.
 */
Index*
Index::clone() const
{
  return new Index(*this);
}


/*
 * Destructor for Index.
 */
Index::~Index()
{
  delete mMath;
  mMath = NULL;
}


/*
 * Returns the value of the "referencedAttribute" attribute of this Index.
 */
const std::string&
Index::getReferencedAttribute() const
{
  return mReferencedAttribute;
}


/*
 * Returns the value of the "arrayDimension" attribute of this Index.
 */
unsigned int
Index::getArrayDimension() const
{
  return mArrayDimension;
}


/*
 * Predicate returning @c true if this Index's "referencedAttribute" attribute
 * is set.
 */
bool
Index::isSetReferencedAttribute() const
{
  return (mReferencedAttribute.empty() == false);
}


/*
 * Predicate returning @c true if this Index's "arrayDimension" attribute is
 * set.
 */
bool
Index::isSetArrayDimension() const
{
  return mIsSetArrayDimension;
}


/*
 * Sets the value of the "referencedAttribute" attribute of this Index.
 */
int
Index::setReferencedAttribute(const std::string& referencedAttribute)
{
  mReferencedAttribute = referencedAttribute;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "arrayDimension" attribute of this Index.
 */
int
Index::setArrayDimension(unsigned int arrayDimension)
{
  mArrayDimension = arrayDimension;
  mIsSetArrayDimension = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "referencedAttribute" attribute of this Index.
 */
int
Index::unsetReferencedAttribute()
{
  mReferencedAttribute.erase();

  if (mReferencedAttribute.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "arrayDimension" attribute of this Index.
 */
int
Index::unsetArrayDimension()
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
 * Returns the value of the "math" element of this Index.
 */
const ASTNode*
Index::getMath() const
{
  return mMath;
}


/*
 * Returns the value of the "math" element of this Index.
 */
ASTNode*
Index::getMath()
{
  return mMath;
}


/*
 * Predicate returning @c true if this Index's "math" element is set.
 */
bool
Index::isSetMath() const
{
  return (mMath != NULL);
}


/*
 * Sets the value of the "math" element of this Index.
 */
int
Index::setMath(const ASTNode* math)
{
  if (mMath == math)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != NULL) ? math->deepCopy() : NULL;
    if (mMath != NULL)
    {
      mMath->setParentSBMLObject(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "math" element of this Index.
 */
int
Index::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this Index object.
 */
const std::string&
Index::getElementName() const
{
  static const string name = "index";
  return name;
}


/*
 * Returns the libSBML type code for this Index object.
 */
int
Index::getTypeCode() const
{
  return SBML_ARRAYS_INDEX;
}


/*
 * Predicate returning @c true if all the required attributes for this Index
 * object have been set.
 */
bool
Index::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetReferencedAttribute() == false)
  {
    allPresent = false;
  }

  if (isSetArrayDimension() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this Index
 * object have been set.
 */
bool
Index::hasRequiredElements() const
{
  bool allPresent = true;

  if (isSetMath() == false)
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
Index::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (isSetMath() == true)
  {
    writeMathML(getMath(), stream, getSBMLNamespaces());
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Index::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Index::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Index::connectToChild()
{
  SBase::connectToChild();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Index::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix,
                             bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Index.
 */
int
Index::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Index.
 */
int
Index::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Index.
 */
int
Index::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Index.
 */
int
Index::getAttribute(const std::string& attributeName,
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
 * Gets the value of the "attributeName" attribute of this Index.
 */
int
Index::getAttribute(const std::string& attributeName,
                    std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "referencedAttribute")
  {
    value = getReferencedAttribute();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Index's attribute "attributeName" is
 * set.
 */
bool
Index::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "referencedAttribute")
  {
    value = isSetReferencedAttribute();
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
 * Sets the value of the "attributeName" attribute of this Index.
 */
int
Index::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Index.
 */
int
Index::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Index.
 */
int
Index::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Index.
 */
int
Index::setAttribute(const std::string& attributeName, unsigned int value)
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
 * Sets the value of the "attributeName" attribute of this Index.
 */
int
Index::setAttribute(const std::string& attributeName,
                    const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "referencedAttribute")
  {
    return_value = setReferencedAttribute(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Index.
 */
int
Index::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "referencedAttribute")
  {
    value = unsetReferencedAttribute();
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
Index::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("referencedAttribute");

  attributes.add("arrayDimension");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Index::readAttributes(const XMLAttributes& attributes,
                      const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (static_cast<ListOfIndices*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("arrays", ArraysIndexAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("arrays",
          ArraysSBaseLOIndicesAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("arrays", ArraysIndexAllowedAttributes, pkgVersion,
        level, version, details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("arrays", ArraysIndexAllowedCoreAttributes,
        pkgVersion, level, version, details);
    }
  }

  // 
  // referencedAttribute string (use = "required" )
  // 

  assigned = attributes.readInto("referencedAttribute", mReferencedAttribute);

  if (assigned == true)
  {
    if (mReferencedAttribute.empty() == true)
    {
      logEmptyString(mReferencedAttribute, level, version, "<Index>");
    }
  }
  else
  {
    std::string message = "Arrays attribute 'referencedAttribute' is missing "
      "from the <Index> element.";
    log->logPackageError("arrays", ArraysIndexAllowedAttributes, pkgVersion,
      level, version, message);
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
      std::string message = "Arrays attribute 'arrayDimension' from the <Index> "
        "element must be an integer.";
      log->logPackageError("arrays", ArraysIndexArrayDimensionMustBeUnInteger,
        pkgVersion, level, version, message);
    }
    else
    {
      std::string message = "Arrays attribute 'arrayDimension' is missing from "
        "the <Index> element.";
      log->logPackageError("arrays", ArraysIndexAllowedAttributes, pkgVersion,
        level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads other XML such as math/notes etc.
 */
bool
Index::readOtherXML(XMLInputStream& stream)
{
  bool read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);
    if (stream.getSBMLNamespaces() == NULL)
    {
      stream.setSBMLNamespaces(new SBMLNamespaces(getLevel(), getVersion()));
    }

    delete mMath;
    mMath = readMathML(stream, prefix);
    read = true;
  }

  if (SBase::readOtherXML(stream))
  {
    read = true;
  }

  return read;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Index::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetReferencedAttribute() == true)
  {
    stream.writeAttribute("referencedAttribute", getPrefix(),
      mReferencedAttribute);
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
 * Creates a new Index_t using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
LIBSBML_EXTERN
Index_t *
Index_create(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion)
{
  return new Index(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Index_t object.
 */
LIBSBML_EXTERN
Index_t*
Index_clone(const Index_t* i)
{
  if (i != NULL)
  {
    return static_cast<Index_t*>(i->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Index_t object.
 */
LIBSBML_EXTERN
void
Index_free(Index_t* i)
{
  if (i != NULL)
  {
    delete i;
  }
}


/*
 * Returns the value of the "referencedAttribute" attribute of this Index_t.
 */
LIBSBML_EXTERN
const char *
Index_getReferencedAttribute(const Index_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return i->getReferencedAttribute().empty() ? NULL :
    safe_strdup(i->getReferencedAttribute().c_str());
}


/*
 * Returns the value of the "arrayDimension" attribute of this Index_t.
 */
LIBSBML_EXTERN
unsigned int
Index_getArrayDimension(const Index_t * i)
{
  return (i != NULL) ? i->getArrayDimension() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 if this Index_t's "referencedAttribute" attribute
 * is set.
 */
LIBSBML_EXTERN
int
Index_isSetReferencedAttribute(const Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetReferencedAttribute()) : 0;
}


/*
 * Predicate returning @c 1 if this Index_t's "arrayDimension" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Index_isSetArrayDimension(const Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetArrayDimension()) : 0;
}


/*
 * Sets the value of the "referencedAttribute" attribute of this Index_t.
 */
LIBSBML_EXTERN
int
Index_setReferencedAttribute(Index_t * i, const char * referencedAttribute)
{
  return (i != NULL) ? i->setReferencedAttribute(referencedAttribute) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "arrayDimension" attribute of this Index_t.
 */
LIBSBML_EXTERN
int
Index_setArrayDimension(Index_t * i, unsigned int arrayDimension)
{
  return (i != NULL) ? i->setArrayDimension(arrayDimension) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "referencedAttribute" attribute of this Index_t.
 */
LIBSBML_EXTERN
int
Index_unsetReferencedAttribute(Index_t * i)
{
  return (i != NULL) ? i->unsetReferencedAttribute() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "arrayDimension" attribute of this Index_t.
 */
LIBSBML_EXTERN
int
Index_unsetArrayDimension(Index_t * i)
{
  return (i != NULL) ? i->unsetArrayDimension() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "math" element of this Index_t.
 */
LIBSBML_EXTERN
const ASTNode_t*
Index_getMath(const Index_t * i)
{
  if (i == NULL)
  {
    return NULL;
  }

  return (ASTNode_t*)(i->getMath());
}


/*
 * Predicate returning @c 1 if this Index_t's "math" element is set.
 */
LIBSBML_EXTERN
int
Index_isSetMath(const Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->isSetMath()) : 0;
}


/*
 * Sets the value of the "math" element of this Index_t.
 */
LIBSBML_EXTERN
int
Index_setMath(Index_t * i, const ASTNode_t* math)
{
  return (i != NULL) ? i->setMath(math) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "math" element of this Index_t.
 */
LIBSBML_EXTERN
int
Index_unsetMath(Index_t * i)
{
  return (i != NULL) ? i->unsetMath() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this Index_t
 * object have been set.
 */
LIBSBML_EXTERN
int
Index_hasRequiredAttributes(const Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 if all the required elements for this Index_t
 * object have been set.
 */
LIBSBML_EXTERN
int
Index_hasRequiredElements(const Index_t * i)
{
  return (i != NULL) ? static_cast<int>(i->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


