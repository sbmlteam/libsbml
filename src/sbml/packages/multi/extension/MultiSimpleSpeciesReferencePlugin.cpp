/**
 * @file:   MultiSimpleSpeciesReferencePlugin.cpp
 * @brief:  Implementation of the MultiSimpleSpeciesReferencePlugin class
 * @author: SBMLTeam
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


#include <sbml/packages/multi/extension/MultiSimpleSpeciesReferencePlugin.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiSimpleSpeciesReferencePlugin
 */
MultiSimpleSpeciesReferencePlugin::MultiSimpleSpeciesReferencePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    SBasePlugin(uri, prefix, multins)
   ,mCompartmentReference ("")
{
}


/*
 * Copy constructor for MultiSimpleSpeciesReferencePlugin.
 */
MultiSimpleSpeciesReferencePlugin::MultiSimpleSpeciesReferencePlugin(const MultiSimpleSpeciesReferencePlugin& orig) :
    SBasePlugin(orig)
  , mCompartmentReference  ( orig.mCompartmentReference)
{  
}


/*
 * Assignment operator for MultiSimpleSpeciesReferencePlugin.
 */
MultiSimpleSpeciesReferencePlugin& 
MultiSimpleSpeciesReferencePlugin::operator=(const MultiSimpleSpeciesReferencePlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mCompartmentReference  = rhs.mCompartmentReference;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MultiSimpleSpeciesReferencePlugin object.
 */
MultiSimpleSpeciesReferencePlugin* 
MultiSimpleSpeciesReferencePlugin::clone () const
{
  return new MultiSimpleSpeciesReferencePlugin(*this);
}


/*
 * Destructor for MultiSimpleSpeciesReferencePlugin.
 */
MultiSimpleSpeciesReferencePlugin::~MultiSimpleSpeciesReferencePlugin()
{
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
MultiSimpleSpeciesReferencePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  //const std::string&      name   = stream.peek().getName();
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    delete multins;
  } 

  return object; 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
MultiSimpleSpeciesReferencePlugin::writeElements (XMLOutputStream& stream) const
{
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
MultiSimpleSpeciesReferencePlugin::hasRequiredElements () const
{
  bool allPresent = true; 

  // TO DO 

  return allPresent; 
}

  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
MultiSimpleSpeciesReferencePlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("compartmentReference");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
MultiSimpleSpeciesReferencePlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiExSplSpeRef_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // compartmentReference SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("compartmentReference", mCompartmentReference);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCompartmentReference.empty() == true)
    {
      logEmptyString(mCompartmentReference, getLevel(), getVersion(), 
        getPackageVersion(), "<MultiSimpleSpeciesReferencePlugin>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCompartmentReference) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute compartmentReference='" + mCompartmentReference + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
MultiSimpleSpeciesReferencePlugin::writeAttributes (XMLOutputStream& stream) const
{
  if (isSetCompartmentReference() == true)
    stream.writeAttribute("compartmentReference", getPrefix(), mCompartmentReference);
}


  /** @endcond */



//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------


/*
 * Returns the value of the "compartmentReference" attribute of this MultiSimpleSpeciesReferencePlugin.
 */
const std::string&
MultiSimpleSpeciesReferencePlugin::getCompartmentReference() const
{
  return mCompartmentReference;
}


/*
 * Returns true/false if compartmentReference is set.
 */
bool
MultiSimpleSpeciesReferencePlugin::isSetCompartmentReference() const
{
  return (mCompartmentReference.empty() == false);
}


/*
 * Sets compartmentReference and returns value indicating success.
 */
int
MultiSimpleSpeciesReferencePlugin::setCompartmentReference(const std::string& compartmentReference)
{
  if (!(SyntaxChecker::isValidInternalSId(compartmentReference)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartmentReference = compartmentReference;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets compartmentReference and returns value indicating success.
 */
int
MultiSimpleSpeciesReferencePlugin::unsetCompartmentReference()
{
  mCompartmentReference.erase();

  if (mCompartmentReference.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


List*
MultiSimpleSpeciesReferencePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  //List* sublist = NULL;


  return ret;
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
MultiSimpleSpeciesReferencePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
MultiSimpleSpeciesReferencePlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
MultiSimpleSpeciesReferencePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiSimpleSpeciesReferencePlugin::accept(SBMLVisitor& v) const
{
  const SimpleSpeciesReference * simpleSpeciesReference = static_cast<const SimpleSpeciesReference * >(this->getParentSBMLObject());
  v.visit(*simpleSpeciesReference);

  return true;
}
/** @endcond */





#endif /* __cplusplus */


/*
 * Returns the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
char *
MultiSimpleSpeciesReferencePlugin_getCompartmentReference(const
  MultiSimpleSpeciesReferencePlugin_t * mssrp)
{
  if (mssrp == NULL)
  {
    return NULL;
  }

  return mssrp->getCompartmentReference().empty() ? NULL :
    safe_strdup(mssrp->getCompartmentReference().c_str());
}


/*
 * Predicate returning @c 1 (true) if this
 * MultiSimpleSpeciesReferencePlugin_t's "compartmentReference" attribute is
 * set.
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_isSetCompartmentReference(const
  MultiSimpleSpeciesReferencePlugin_t * mssrp)
{
  return (mssrp != NULL) ? static_cast<int>(mssrp->isSetCompartmentReference())
    : 0;
}


/*
 * Sets the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_setCompartmentReference(
  MultiSimpleSpeciesReferencePlugin_t
  * mssrp,
  const char *
  compartmentReference)
{
  return (mssrp != NULL) ? mssrp->setCompartmentReference(compartmentReference)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_unsetCompartmentReference(MultiSimpleSpeciesReferencePlugin_t
  * mssrp)
{
  return (mssrp != NULL) ? mssrp->unsetCompartmentReference() :
    LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END
