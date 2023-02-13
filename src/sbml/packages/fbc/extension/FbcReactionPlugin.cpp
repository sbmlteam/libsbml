/**
 * @file   FbcReactionPlugin.cpp
 * @brief  Implementation of the FbcReactionPlugin class
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new FbcReactionPlugin
 */
FbcReactionPlugin::FbcReactionPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               FbcPkgNamespaces* fbcns) :
    FbcSBasePlugin(uri, prefix, fbcns)
  , mGeneProductAssociation  ( NULL )
  , mLowerFluxBound ("")
  , mUpperFluxBound ("")
{
}


/*
 * Copy constructor for FbcReactionPlugin.
 */
FbcReactionPlugin::FbcReactionPlugin(const FbcReactionPlugin& orig) :
    FbcSBasePlugin(orig)
  , mGeneProductAssociation ( NULL )
{
  if (orig.mGeneProductAssociation != NULL)
  {
    mGeneProductAssociation = orig.mGeneProductAssociation->clone();
  }
    mLowerFluxBound  = orig.mLowerFluxBound;
    mUpperFluxBound  = orig.mUpperFluxBound;
}


/*
 * Assignment operator for FbcReactionPlugin.
 */
FbcReactionPlugin& 
FbcReactionPlugin::operator=(const FbcReactionPlugin& rhs)
{
  if (&rhs != this)
  {
    this->FbcSBasePlugin::operator=(rhs);
    delete mGeneProductAssociation;
    mGeneProductAssociation = NULL;
    if (rhs.mGeneProductAssociation != NULL)
      mGeneProductAssociation = rhs.mGeneProductAssociation->clone();
    mLowerFluxBound  = rhs.mLowerFluxBound;
    mUpperFluxBound  = rhs.mUpperFluxBound;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this FbcReactionPlugin object.
 */
FbcReactionPlugin* 
FbcReactionPlugin::clone () const
{
  return new FbcReactionPlugin(*this);
}


/*
 * Destructor for FbcReactionPlugin.
 */
FbcReactionPlugin::~FbcReactionPlugin()
{
  delete mGeneProductAssociation;
  mGeneProductAssociation = NULL;
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/** @cond doxygenLibsbmlInternal */
/*
 * create object
 */
SBase*
FbcReactionPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns1  = stream.peek().getNamespaces();
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns1.hasURI(mURI)) ? xmlns1.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    if (name == "geneProductAssociation" ) 
    { 
      if (mGeneProductAssociation != NULL)
      {
        getErrorLog()->logPackageError("fbc", FbcReactionOnlyOneGeneProdAss, 
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      delete mGeneProductAssociation;
      
      mGeneProductAssociation = new GeneProductAssociation(fbcns);

      object = mGeneProductAssociation;

    } 

    delete fbcns;
  } 

  return object; 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
FbcReactionPlugin::writeElements (XMLOutputStream& stream) const
{
  // dont want to write <fbc:geneProductAssociation/> 
  // so check it actual has something in
  if (isSetGeneProductAssociation() == true && getLevel() == 3 
    && getPackageVersion() > 1 && getGeneProductAssociation()->getAssociation() != NULL) 
  { 
    mGeneProductAssociation->write(stream);
  } 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Get the list of expected attributes for this element.
 */
void
FbcReactionPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  FbcSBasePlugin::addExpectedAttributes(attributes);

  attributes.add("lowerFluxBound");
  attributes.add("upperFluxBound");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcReactionPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs = 
    (getErrorLog() != NULL ? getErrorLog()->getNumErrors() : 0);

  FbcSBasePlugin::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    unsigned newNumErrs = getErrorLog()->getNumErrors();
    if (newNumErrs != numErrs)
    {
      for (unsigned int n = newNumErrs; n > numErrs; n--)
      {
        const SBMLError* err = getErrorLog()->getError(n-1);
        if (err->getErrorId() == UnknownPackageAttribute)
        {
          const std::string details = err->getMessage();
          getErrorLog()->remove(UnknownPackageAttribute);
          getErrorLog()->logPackageError("fbc", FbcReactionAllowedAttributes,
                         getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                         getLine(), getColumn());
        }
        else if (err->getErrorId() == UnknownCoreAttribute)
        {
          const std::string details = err->getMessage();
          getErrorLog()->remove(UnknownCoreAttribute);
          getErrorLog()->logPackageError("fbc", FbcReactionAllowedAttributes,
                         getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                         getLine(), getColumn());
        }
        else if (err->getErrorId() == NotSchemaConformant)
        {
          const std::string details = err->getMessage();
          getErrorLog()->remove(NotSchemaConformant);
          getErrorLog()->logPackageError("fbc", FbcReactionAllowedAttributes,
                         getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                         getLine(), getColumn());
        }
      }
    }
  }

  bool assigned = false;

  //
  // lowerFluxBound SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("lowerFluxBound", mLowerFluxBound);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mLowerFluxBound.empty() == true)
    {
      logEmptyString(mLowerFluxBound, getLevel(), getVersion(), 
        getPackageVersion(), "<Reaction>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mLowerFluxBound) == false 
      && getErrorLog() != NULL)
    {
          const std::string details = "The syntax of the attribute "
            "lowerFluxBound='" + mLowerFluxBound + "' does not conform.";
          getErrorLog()->logPackageError("fbc", FbcReactionLwrBoundSIdRef,
                         getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                         getLine(), getColumn());
    }
  }

  //
  // upperFluxBound SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("upperFluxBound", mUpperFluxBound);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mUpperFluxBound.empty() == true)
    {
      logEmptyString(mUpperFluxBound, getLevel(), getVersion(), 
        getPackageVersion(), "<Reaction>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUpperFluxBound) == false && getErrorLog() != NULL)
    {
          const std::string details = "The syntax of the attribute "
            "upperFluxBound='" + mUpperFluxBound + "' does not conform.";
          getErrorLog()->logPackageError("fbc", FbcReactionUpBoundSIdRef,
                         getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                         getLine(), getColumn());
    }
  }

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
FbcReactionPlugin::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  FbcSBasePlugin::renameSIdRefs(oldid, newid);
  if (isSetLowerFluxBound())
  {
    if (mLowerFluxBound==oldid) mLowerFluxBound=newid;
  }
  if (isSetUpperFluxBound())
  {
    if (mUpperFluxBound==oldid) mUpperFluxBound=newid;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcReactionPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if (getPackageVersion() == 1) return;

  FbcSBasePlugin::writeAttributes(stream);

  if (isSetLowerFluxBound() == true)
    stream.writeAttribute("lowerFluxBound", getPrefix(), mLowerFluxBound);

  if (isSetUpperFluxBound() == true)
    stream.writeAttribute("upperFluxBound", getPrefix(), mUpperFluxBound);

}
/** @endcond */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
FbcReactionPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mGeneProductAssociation, filter);

  return ret;
}


/*
 * Returns the GeneProductAssociation from this FbcReactionPlugin object.
 */
const GeneProductAssociation* 
FbcReactionPlugin::getGeneProductAssociation () const
{
  return mGeneProductAssociation;
}


/*
 * Returns the GeneProductAssociation from this FbcReactionPlugin object.
 */
GeneProductAssociation* 
FbcReactionPlugin::getGeneProductAssociation ()
{
  return mGeneProductAssociation;
}


/*
 * @return @c true if the "GeneProductAssociation" element has been set,
 */
bool 
FbcReactionPlugin::isSetGeneProductAssociation () const
{
  return (mGeneProductAssociation != NULL);
}


/*
 * Sets the GeneProductAssociation element in this FbcReactionPlugin object.
 */
int
FbcReactionPlugin::setGeneProductAssociation(const GeneProductAssociation* geneProductAssociation)
{
  if (geneProductAssociation == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (geneProductAssociation->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != geneProductAssociation->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != geneProductAssociation->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != geneProductAssociation->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mGeneProductAssociation;
    mGeneProductAssociation = static_cast<GeneProductAssociation*>(geneProductAssociation->clone());
    if (mGeneProductAssociation != NULL) mGeneProductAssociation->connectToParent(this->getParentSBMLObject());

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new GeneProductAssociation object and adds it to the FbcReactionPlugin object.
 */
GeneProductAssociation*
FbcReactionPlugin::createGeneProductAssociation()
{
  delete mGeneProductAssociation;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
  mGeneProductAssociation = new GeneProductAssociation(fbcns);

  mGeneProductAssociation->setSBMLDocument(this->getSBMLDocument());

  delete fbcns;

  return mGeneProductAssociation;
}


/*
 * Returns the value of the "lowerFluxBound" attribute of this FbcReactionPlugin.
 */
const std::string&
FbcReactionPlugin::getLowerFluxBound() const
{
  return mLowerFluxBound;
}


/*
 * Returns the value of the "upperFluxBound" attribute of this FbcReactionPlugin.
 */
const std::string&
FbcReactionPlugin::getUpperFluxBound() const
{
  return mUpperFluxBound;
}


/*
 * Returns true/false if lowerFluxBound is set.
 */
bool
FbcReactionPlugin::isSetLowerFluxBound() const
{
  return (mLowerFluxBound.empty() == false);
}


/*
 * Returns true/false if upperFluxBound is set.
 */
bool
FbcReactionPlugin::isSetUpperFluxBound() const
{
  return (mUpperFluxBound.empty() == false);
}


/*
 * Sets lowerFluxBound and returns value indicating success.
 */
int
FbcReactionPlugin::setLowerFluxBound(const std::string& lowerFluxBound)
{
  if (!(SyntaxChecker::isValidInternalSId(lowerFluxBound)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mLowerFluxBound = lowerFluxBound;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets upperFluxBound and returns value indicating success.
 */
int
FbcReactionPlugin::setUpperFluxBound(const std::string& upperFluxBound)
{
  if (!(SyntaxChecker::isValidInternalSId(upperFluxBound)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mUpperFluxBound = upperFluxBound;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets lowerFluxBound and returns value indicating success.
 */
int
FbcReactionPlugin::unsetLowerFluxBound()
{
  mLowerFluxBound.erase();

  if (mLowerFluxBound.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets upperFluxBound and returns value indicating success.
 */
int
FbcReactionPlugin::unsetUpperFluxBound()
{
  mUpperFluxBound.erase();

  if (mUpperFluxBound.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

int 
FbcReactionPlugin::unsetGeneProductAssociation()
{
  if (isSetGeneProductAssociation())
    delete mGeneProductAssociation;
  mGeneProductAssociation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}

//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
FbcReactionPlugin::setSBMLDocument(SBMLDocument* d)
{
  FbcSBasePlugin::setSBMLDocument(d);

  if (isSetGeneProductAssociation() == true)
  {
    mGeneProductAssociation->setSBMLDocument(d);
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
FbcReactionPlugin::connectToParent(SBase* sbase)
{
  FbcSBasePlugin::connectToParent(sbase);

  if (isSetGeneProductAssociation() == true)
  {
    mGeneProductAssociation->connectToParent(sbase);
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
FbcReactionPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetGeneProductAssociation() == true)
  {
    mGeneProductAssociation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "lowerFluxBound")
  {
    value = getLowerFluxBound();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "upperFluxBound")
  {
    value = getUpperFluxBound();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcReactionPlugin's attribute
 * "attributeName" is set.
 */
bool
FbcReactionPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = FbcSBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "lowerFluxBound")
  {
    value = isSetLowerFluxBound();
  }
  else if (attributeName == "upperFluxBound")
  {
    value = isSetUpperFluxBound();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "lowerFluxBound")
  {
    return_value = setLowerFluxBound(value);
  }
  else if (attributeName == "upperFluxBound")
  {
    return_value = setUpperFluxBound(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this FbcReactionPlugin.
 */
int
FbcReactionPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = FbcSBasePlugin::unsetAttribute(attributeName);

  if (attributeName == "lowerFluxBound")
  {
    value = unsetLowerFluxBound();
  }
  else if (attributeName == "upperFluxBound")
  {
    value = unsetUpperFluxBound();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this FbcReactionPlugin.
 */
SBase*
FbcReactionPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "geneProductAssociation")
  {
    return createGeneProductAssociation();
  }

  return FbcSBasePlugin::createChildObject(elementName);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this FbcReactionPlugin.
 */
unsigned int
FbcReactionPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "geneProductAssociation")
  {
    if (isSetGeneProductAssociation())
    {
      return 1;
    }
  }

  return FbcSBasePlugin::getNumObjects(elementName);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this FbcReactionPlugin.
 */
SBase*
FbcReactionPlugin::getObject(const std::string& elementName,
                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "geneProductAssociation")
  {
    return getGeneProductAssociation();
  }

  return FbcSBasePlugin::getObject(elementName, index);
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Accept the SBMLVisitor.
 */
bool
FbcReactionPlugin::accept(SBMLVisitor& v) const
{
  const Reaction * r = static_cast<const Reaction * >(this->getParentSBMLObject());

  v.visit(*r);

  for (unsigned int n = 0; n < r->getNumReactants(); n++)
  {
    v.visit(*(r->getReactant(n)));
  }

  for (unsigned int n = 0; n < r->getNumProducts(); n++)
  {
    v.visit(*(r->getProduct(n)));
  }

  v.leave(*r);

  if (mGeneProductAssociation != NULL)  mGeneProductAssociation->accept(v);

  return true;
}
/** @endcond */





#endif /* __cplusplus */


LIBSBML_EXTERN
GeneProductAssociation_t *
FbcReactionPlugin_createGeneProductAssociation(FbcSBasePlugin_t * fbc)
{
  return  (fbc != NULL) ? static_cast<FbcReactionPlugin*>(fbc)->createGeneProductAssociation() : NULL;
}

LIBSBML_EXTERN
char *
FbcReactionPlugin_getUpperFluxBound(FbcSBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcReactionPlugin*>(fbc)->getUpperFluxBound().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcReactionPlugin*>(fbc)->getUpperFluxBound().c_str());
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetUpperFluxBound(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetUpperFluxBound()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_setUpperFluxBound(FbcSBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setUpperFluxBound(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_unsetUpperFluxBound(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->unsetUpperFluxBound()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
char *
FbcReactionPlugin_getLowerFluxBound(FbcSBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcReactionPlugin*>(fbc)->getLowerFluxBound().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcReactionPlugin*>(fbc)->getLowerFluxBound().c_str());
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetLowerFluxBound(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetLowerFluxBound()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_setLowerFluxBound(FbcSBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setLowerFluxBound(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_unsetLowerFluxBound(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->unsetLowerFluxBound()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetGeneProductAssociation(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetGeneProductAssociation()) 
    : 0;
}


LIBSBML_EXTERN
GeneProductAssociation_t*
FbcReactionPlugin_getGeneProductAssociation(FbcSBasePlugin_t * fbc)
{
  return  (fbc != NULL) ? static_cast<FbcReactionPlugin*>(fbc)->getGeneProductAssociation() : NULL;

}

LIBSBML_EXTERN
int
FbcReactionPlugin_setGeneProductAssociation(FbcSBasePlugin_t * fbc, 
                                            GeneProductAssociation_t* gpa)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setGeneProductAssociation(gpa)
    : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END

