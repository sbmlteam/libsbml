/**
 * @file ListOfGlobalRenderInformation.cpp
 * @brief Implementation of the ListOfGlobalRenderInformation class.
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
#include <sbml/packages/render/sbml/ListOfGlobalRenderInformation.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGlobalRenderInformation using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(
                                                             unsigned int
                                                               level,
                                                             unsigned int
                                                               version,
                                                             unsigned int
                                                               pkgVersion)
  : ListOf(level, version)
  , mMajorVersion (SBML_INT_MAX)
  , mIsSetMajorVersion (false)
  , mMinorVersion (SBML_INT_MAX)
  , mIsSetMinorVersion (false)
  , mDefaultValues (NULL)
{
  //if (level == 3)
  //  mDefaultValues = new DefaultValues(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new ListOfGlobalRenderInformation using the given
 * RenderPkgNamespaces object.
 */
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(RenderPkgNamespaces
  *renderns)
  : ListOf(renderns)
  , mMajorVersion (SBML_INT_MAX)
  , mIsSetMajorVersion (false)
  , mMinorVersion (SBML_INT_MAX)
  , mIsSetMinorVersion (false)
  , mDefaultValues (NULL)
{
  //if (getLevel() == 3)
  //  mDefaultValues = new DefaultValues(renderns);
  setElementNamespace(renderns->getURI());
  connectToChild();
}


/*
 * Copy constructor for ListOfGlobalRenderInformation.
 */
ListOfGlobalRenderInformation::ListOfGlobalRenderInformation(const
  ListOfGlobalRenderInformation& orig)
  : ListOf( orig )
  , mMajorVersion ( orig.mMajorVersion )
  , mIsSetMajorVersion ( orig.mIsSetMajorVersion )
  , mMinorVersion ( orig.mMinorVersion )
  , mIsSetMinorVersion ( orig.mIsSetMinorVersion )
  , mDefaultValues ( NULL )
{
  if (orig.mDefaultValues != NULL)
  {
    mDefaultValues = orig.mDefaultValues->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for ListOfGlobalRenderInformation.
 */
ListOfGlobalRenderInformation&
ListOfGlobalRenderInformation::operator=(const ListOfGlobalRenderInformation&
  rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mMajorVersion = rhs.mMajorVersion;
    mIsSetMajorVersion = rhs.mIsSetMajorVersion;
    mMinorVersion = rhs.mMinorVersion;
    mIsSetMinorVersion = rhs.mIsSetMinorVersion;
    delete mDefaultValues;
    if (rhs.mDefaultValues != NULL)
    {
      mDefaultValues = rhs.mDefaultValues->clone();
    }
    else
    {
      mDefaultValues = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGlobalRenderInformation
 * object.
 */
ListOfGlobalRenderInformation*
ListOfGlobalRenderInformation::clone() const
{
  return new ListOfGlobalRenderInformation(*this);
}


/*
 * Destructor for ListOfGlobalRenderInformation.
 */
ListOfGlobalRenderInformation::~ListOfGlobalRenderInformation()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
}


/*
 * Returns the value of the "majorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
unsigned int
ListOfGlobalRenderInformation::getMajorVersion() const
{
  return mMajorVersion;
}


/*
 * Returns the value of the "minorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
unsigned int
ListOfGlobalRenderInformation::getMinorVersion() const
{
  return mMinorVersion;
}


std::string ListOfGlobalRenderInformation::getVersionString() const
{
  std::ostringstream os;
  os << this->mMajorVersion << "." << this->mMinorVersion;
  return os.str();
}


/*
 * Predicate returning @c true if this ListOfGlobalRenderInformation's
 * "majorVersion" attribute is set.
 */
bool
ListOfGlobalRenderInformation::isSetMajorVersion() const
{
  return mIsSetMajorVersion;
}

/** @cond doxygenLibsbmlInternal */
bool
ListOfGlobalRenderInformation::isSetVersionMajor() const
{
  return mIsSetMajorVersion;
}
/** @endcond */


/*
 * Predicate returning @c true if this ListOfGlobalRenderInformation's
 * "minorVersion" attribute is set.
 */
bool
ListOfGlobalRenderInformation::isSetMinorVersion() const
{
  return mIsSetMinorVersion;
}


/** @cond doxygenLibsbmlInternal */
bool
ListOfGlobalRenderInformation::isSetVersionMinor() const
{
  return mIsSetMinorVersion;
}
/** @endcond */


/*
 * Sets the value of the "majorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::setMajorVersion(unsigned int majorVersion)
{
  mMajorVersion = majorVersion;
  mIsSetMajorVersion = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int
ListOfGlobalRenderInformation::setVersionMajor(unsigned int majorVersion)
{
  return setMajorVersion(majorVersion);
}
/** @endcond */

/*
 * Sets the value of the "minorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::setMinorVersion(unsigned int minorVersion)
{
  mMinorVersion = minorVersion;
  mIsSetMinorVersion = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int
ListOfGlobalRenderInformation::setVersionMinor(unsigned int minorVersion)
{
  return setMinorVersion(minorVersion);
}
/** @endcond */


void ListOfGlobalRenderInformation::setVersion(unsigned int major, unsigned int minor)
{
  setMajorVersion(major);
  setMinorVersion(minor);
}


/*
 * Unsets the value of the "majorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::unsetMajorVersion()
{
  mMajorVersion = SBML_INT_MAX;
  mIsSetMajorVersion = false;

  if (isSetMajorVersion() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/** @cond doxygenLibsbmlInternal */
int
ListOfGlobalRenderInformation::unsetVersionMajor()
{
  return unsetMajorVersion();
}
/** @endcond */

/*
 * Unsets the value of the "minorVersion" attribute of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::unsetMinorVersion()
{
  mMinorVersion = SBML_INT_MAX;
  mIsSetMinorVersion = false;

  if (isSetMinorVersion() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/** @cond doxygenLibsbmlInternal */
int
ListOfGlobalRenderInformation::unsetVersionMinor()
{
  return unsetMinorVersion();
}
/** @endcond */


/*
 * Returns the value of the "defaultValues" element of this
 * ListOfGlobalRenderInformation.
 */
const DefaultValues*
ListOfGlobalRenderInformation::getDefaultValues() const
{
  return mDefaultValues;
}


/*
 * Returns the value of the "defaultValues" element of this
 * ListOfGlobalRenderInformation.
 */
DefaultValues*
ListOfGlobalRenderInformation::getDefaultValues()
{
  return mDefaultValues;
}


/*
 * Predicate returning @c true if this ListOfGlobalRenderInformation's
 * "defaultValues" element is set.
 */
bool
ListOfGlobalRenderInformation::isSetDefaultValues() const
{
  return (mDefaultValues != NULL);
}


/*
 * Sets the value of the "defaultValues" element of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::setDefaultValues(const DefaultValues*
  defaultValues)
{
  if (mDefaultValues == defaultValues)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (defaultValues == NULL)
  {
    delete mDefaultValues;
    mDefaultValues = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mDefaultValues;
    mDefaultValues = (defaultValues != NULL) ? defaultValues->clone() : NULL;
    if (mDefaultValues != NULL)
    {
      mDefaultValues->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DefaultValues object, adds it to this
 * ListOfGlobalRenderInformation object and returns the DefaultValues object
 * created.
 */
DefaultValues*
ListOfGlobalRenderInformation::createDefaultValues()
{
  if (mDefaultValues != NULL)
  {
    delete mDefaultValues;
  }

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());
  mDefaultValues = new DefaultValues(renderns);

  delete renderns;

  connectToChild();

  return mDefaultValues;
}


/*
 * Unsets the value of the "defaultValues" element of this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::unsetDefaultValues()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation.
 */
GlobalRenderInformation*
ListOfGlobalRenderInformation::get(unsigned int n)
{
  return static_cast<GlobalRenderInformation*>(ListOf::get(n));
}


/*
 * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation.
 */
const GlobalRenderInformation*
ListOfGlobalRenderInformation::get(unsigned int n) const
{
  return static_cast<const GlobalRenderInformation*>(ListOf::get(n));
}

/** @cond doxygenLibsbmlInternal */
/*
* Used by ListOf::get() to lookup an SBase based by its id.
*/
struct IdEqGlobalRenderInformation
{
  const std::string& id;

  IdEqGlobalRenderInformation(const std::string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
    return static_cast <GlobalRenderInformation *> (sb)->getId() == id;
  }
};
/** @endcond */


/*
 * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation based
 * on its identifier.
 */
GlobalRenderInformation*
ListOfGlobalRenderInformation::get(const std::string& sid)
{
  return const_cast<GlobalRenderInformation*>(static_cast<const
    ListOfGlobalRenderInformation&>(*this).get(sid));
}


/*
 * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation based
 * on its identifier.
 */
const GlobalRenderInformation*
ListOfGlobalRenderInformation::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEqGlobalRenderInformation(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    GlobalRenderInformation*> (*result);
}


/*
 * Removes the nth GlobalRenderInformation from this
 * ListOfGlobalRenderInformation and returns a pointer to it.
 */
GlobalRenderInformation*
ListOfGlobalRenderInformation::remove(unsigned int n)
{
  return static_cast<GlobalRenderInformation*>(ListOf::remove(n));
}


/*
 * Removes the GlobalRenderInformation from this ListOfGlobalRenderInformation
 * based on its identifier and returns a pointer to it.
 */
GlobalRenderInformation*
ListOfGlobalRenderInformation::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEqGlobalRenderInformation(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GlobalRenderInformation*> (item);
}


/*
 * Adds a copy of the given GlobalRenderInformation to this
 * ListOfGlobalRenderInformation.
 */
int
ListOfGlobalRenderInformation::addGlobalRenderInformation(const
  GlobalRenderInformation* gri)
{
  if (gri == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gri->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gri->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gri->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gri)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(gri);
  }
}


/*
 * Get the number of GlobalRenderInformation objects in this
 * ListOfGlobalRenderInformation.
 */
unsigned int
ListOfGlobalRenderInformation::getNumGlobalRenderInformation() const
{
  return size();
}


/*
 * Creates a new GlobalRenderInformation object, adds it to this
 * ListOfGlobalRenderInformation object and returns the GlobalRenderInformation
 * object created.
 */
GlobalRenderInformation*
ListOfGlobalRenderInformation::createGlobalRenderInformation()
{
  GlobalRenderInformation* gri = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    gri = new GlobalRenderInformation(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (gri != NULL)
  {
    appendAndOwn(gri);
  }

  return gri;
}


/*
 * Returns the XML element name of this ListOfGlobalRenderInformation object.
 */
const std::string&
ListOfGlobalRenderInformation::getElementName() const
{
  static const string name = "listOfGlobalRenderInformation";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfGlobalRenderInformation object.
 */
int
ListOfGlobalRenderInformation::getTypeCode() const
{
  return SBML_LIST_OF;
}


bool ListOfGlobalRenderInformation::isValidTypeForList(SBase * item)
{
  if (item == NULL) return false;
  int typeCode = item->getTypeCode();
  return (
    typeCode == SBML_RENDER_GLOBALRENDERINFORMATION
    );
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGlobalRenderInformation object.
 */
int
ListOfGlobalRenderInformation::getItemTypeCode() const
{
  return SBML_RENDER_GLOBALRENDERINFORMATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfGlobalRenderInformation object have been set.
 */
bool
ListOfGlobalRenderInformation::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
ListOfGlobalRenderInformation::writeElements(XMLOutputStream& stream) const
{
  ListOf::writeElements(stream);

  if (isSetDefaultValues() == true)
  {
    mDefaultValues->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
ListOfGlobalRenderInformation::connectToChild()
{
  ListOf::connectToChild();

  if (mDefaultValues != NULL)
  {
    mDefaultValues->connectToParent(this);
  }
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
ListOfGlobalRenderInformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDefaultValues != NULL)
  {
    if (mDefaultValues->getId() == id)
    {
      return mDefaultValues;
    }

    obj = mDefaultValues->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return ListOf::getElementBySId(id);
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
ListOfGlobalRenderInformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDefaultValues != NULL)
  {
    if (mDefaultValues->getMetaId() == metaid)
    {
      return mDefaultValues;
    }

    obj = mDefaultValues->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return ListOf::getElementByMetaId(metaid);
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
ListOfGlobalRenderInformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = ListOf::getAllElements(filter);

  ADD_FILTERED_POINTER(ret, sublist, mDefaultValues, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


XMLNode ListOfGlobalRenderInformation::toXML() const
{
  return getXmlNodeForSBase(this);
}

void ListOfGlobalRenderInformation::parseXML(const XMLNode& node)
{
  const XMLNode* child;
  unsigned int n = 0, nMax = node.getNumChildren();
  const XMLAttributes& attributes = node.getAttributes();
  ExpectedAttributes ea;
  addExpectedAttributes(ea);
  this->readAttributes(attributes, ea);
  while (n<nMax)
  {
    child = &node.getChild(n);
    const std::string& childName = child->getName();
    if (childName == "renderInformation")
    {
      GlobalRenderInformation* pGRI = new GlobalRenderInformation(this->getLevel(), this->getVersion());
      pGRI->parseXML(*child);
      this->appendAndOwn(pGRI);
    }
    ++n;
  }
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GlobalRenderInformation in this ListOfGlobalRenderInformation
 */
SBase*
ListOfGlobalRenderInformation::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "renderInformation")
  {
    object = new GlobalRenderInformation(renderns);
    appendAndOwn(object);
  }

  if (name == "defaultValues")
  {
    DefaultValues newDV(renderns);
    setDefaultValues(&newDV);
    object = getDefaultValues();
  }

  delete renderns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ListOfGlobalRenderInformation::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  ListOf::addExpectedAttributes(attributes);

  attributes.add("versionMajor");

  attributes.add("versionMinor");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfGlobalRenderInformation::readAttributes(const XMLAttributes& attributes,
                                              const ExpectedAttributes&
                                                expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  ListOf::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render",
          RenderListOfLayoutsLOGlobalRenderInformationAllowedAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // majorVersion uint (use = "optional" )
  // 

  if (log)
  {
    numErrs = log->getNumErrors();
  }
  mIsSetMajorVersion = attributes.readInto("versionMajor", mMajorVersion);

  if ( mIsSetMajorVersion == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Render attribute 'versionMajor' from the "
        "<ListOfGlobalRenderInformation> element must be an integer.";
      log->logPackageError("render", 
        RenderListOfLayoutsVersionMajorMustBeNonNegativeInteger, pkgVersion, 
        level, version, message, getLine(), getColumn());
    }
  }

  // 
  // minorVersion uint (use = "optional" )
  // 

  if (log)
  {
    numErrs = log->getNumErrors();
  }
  mIsSetMinorVersion = attributes.readInto("versionMinor", mMinorVersion);

  if ( mIsSetMinorVersion == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Render attribute 'versionMinor' from the "
        "<ListOfGlobalRenderInformation> element must be an integer.";
      log->logPackageError("render", 
        RenderListOfLayoutsVersionMinorMustBeNonNegativeInteger, pkgVersion, 
        level, version, message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfGlobalRenderInformation::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  if (isSetMajorVersion() == true)
  {
    stream.writeAttribute("majorVersion", getPrefix(), mMajorVersion);
  }

  if (isSetMinorVersion() == true)
  {
    stream.writeAttribute("minorVersion", getPrefix(), mMinorVersion);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Render package
 */
void
ListOfGlobalRenderInformation::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(RenderExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(RenderExtension::getXmlnsL3V1V1(), prefix);
    }
  }
  else
  {
    xmlns.add(getURI(), getPrefix());
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
unsigned int
ListOfGlobalRenderInformation_getMajorVersion(const ListOf_t * lo)
{
  return (lo != NULL) ? static_cast<const
    ListOfGlobalRenderInformation*>(lo)->getMajorVersion() : SBML_INT_MAX;
}


/*
 * Returns the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
unsigned int
ListOfGlobalRenderInformation_getMinorVersion(const ListOf_t * lo)
{
  return (lo != NULL) ? static_cast<const
    ListOfGlobalRenderInformation*>(lo)->getMinorVersion() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "majorVersion" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_isSetMajorVersion(const ListOf_t * lo)
{
  return (static_cast<const ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfGlobalRenderInformation*>(lo)->isSetMajorVersion()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "minorVersion" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_isSetMinorVersion(const ListOf_t * lo)
{
  return (static_cast<const ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfGlobalRenderInformation*>(lo)->isSetMinorVersion()) : 0;
}


/*
 * Sets the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_setMajorVersion(ListOf_t * lo,
                                              unsigned int majorVersion)
{
  return (static_cast<ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfGlobalRenderInformation*>(lo)->setMajorVersion(majorVersion)
      : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_setMinorVersion(ListOf_t * lo,
                                              unsigned int minorVersion)
{
  return (static_cast<ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfGlobalRenderInformation*>(lo)->setMinorVersion(minorVersion)
      : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_unsetMajorVersion(ListOf_t * lo)
{
  return (static_cast<ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfGlobalRenderInformation*>(lo)->unsetMajorVersion() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_unsetMinorVersion(ListOf_t * lo)
{
  return (static_cast<ListOfGlobalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfGlobalRenderInformation*>(lo)->unsetMinorVersion() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Get a GlobalRenderInformation_t from the ListOf_t.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_getGlobalRenderInformation(ListOf_t* lo,
                                                         unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGlobalRenderInformation*>(lo)->get(n);
}


/*
 * Get a GlobalRenderInformation_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfGlobalRenderInformation*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth GlobalRenderInformation_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGlobalRenderInformation*>(lo)->remove(n);
}


/*
 * Removes the GlobalRenderInformation_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfGlobalRenderInformation*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


