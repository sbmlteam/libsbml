/**
 * @file   FbcReactionPlugin.cpp
 * @brief  Implementation of the FbcReactionPlugin class
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
    SBasePlugin(uri, prefix, fbcns)
  , mGeneProductAssociation  ( NULL )
  , mLowerFluxBound ("")
  , mUpperFluxBound ("")
{
}


/*
 * Copy constructor for FbcReactionPlugin.
 */
FbcReactionPlugin::FbcReactionPlugin(const FbcReactionPlugin& orig) :
    SBasePlugin(orig)
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
    this->SBasePlugin::operator=(rhs);
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
          getPackageVersion(), getLevel(), getVersion());
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
  if (isSetGeneProductAssociation() == true && getLevel() == 3 && getPackageVersion() == 2) 
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
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("lowerFluxBound");
  attributes.add("upperFluxBound");
}


  /** @endcond doxygenLibsbmlInternal */


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

  SBasePlugin::readAttributes(attributes, expectedAttributes);

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
/** @endcond doxygenLibsbmlInternal */

/** @cond doxygenLibsbmlInternal */
void
FbcReactionPlugin::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBasePlugin::renameSIdRefs(oldid, newid);
  if (isSetLowerFluxBound())
  {
    if (mLowerFluxBound==oldid) mLowerFluxBound=newid;
  }
  if (isSetUpperFluxBound())
  {
    if (mUpperFluxBound==oldid) mUpperFluxBound=newid;
  }
}
/** @endcond doxygenLibsbmlInternal */


/** @cond doxygenLibsbmlInternal */
/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcReactionPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if (getPackageVersion() == 1) return;

  SBasePlugin::writeAttributes(stream);

  if (isSetLowerFluxBound() == true)
    stream.writeAttribute("lowerFluxBound", getPrefix(), mLowerFluxBound);

  if (isSetUpperFluxBound() == true)
    stream.writeAttribute("upperFluxBound", getPrefix(), mUpperFluxBound);

}
/** @endcond doxygenLibsbmlInternal */


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
  SBasePlugin::setSBMLDocument(d);

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
  SBasePlugin::connectToParent(sbase);

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
char *
FbcReactionPlugin_getUpperFluxBound(SBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcReactionPlugin*>(fbc)->getUpperFluxBound().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcReactionPlugin*>(fbc)->getUpperFluxBound().c_str());
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetUpperFluxBound(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetUpperFluxBound()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_setUpperFluxBound(SBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setUpperFluxBound(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_unsetUpperFluxBound(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->unsetUpperFluxBound()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
char *
FbcReactionPlugin_getLowerFluxBound(SBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcReactionPlugin*>(fbc)->getLowerFluxBound().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcReactionPlugin*>(fbc)->getLowerFluxBound().c_str());
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetLowerFluxBound(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetLowerFluxBound()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_setLowerFluxBound(SBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setLowerFluxBound(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_unsetLowerFluxBound(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->unsetLowerFluxBound()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcReactionPlugin_isSetGeneProductAssociation(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcReactionPlugin*>(fbc)->isSetGeneProductAssociation()) 
    : 0;
}


LIBSBML_EXTERN
GeneProductAssociation_t*
FbcReactionPlugin_getGeneProductAssociation(SBasePlugin_t * fbc)
{
  return  (fbc != NULL) ? static_cast<FbcReactionPlugin*>(fbc)->getGeneProductAssociation() : NULL;

}

LIBSBML_EXTERN
int
FbcReactionPlugin_setGeneProductAssociation(SBasePlugin_t * fbc, 
                                            GeneProductAssociation_t* gpa)
{
  return (fbc != NULL) 
    ? static_cast<FbcReactionPlugin*>(fbc)->setGeneProductAssociation(gpa)
    : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END

