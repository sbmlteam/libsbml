/**
 * @file    FbcSpeciesPlugin.cpp
 * @brief   Implementation of FbcSpeciesPlugin, the plugin class of
 *          the fbc package for the Species element.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------

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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#include <sbml/packages/fbc/extension/FbcSpeciesPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>

#include <iostream>
using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

static void
parseChemicalFormula(std::string& chemicalFormula, 
                     SBMLErrorLog& errLog, unsigned int packageVersion, 
                     unsigned int level, unsigned int version)
{
  std::vector< std::pair< std::string, int > > chemicalSymbols;

  int chemicalNumber;

  size_t index = 0;
  size_t sizeStr = chemicalFormula.size();
  char c = chemicalFormula[index];
  
  while (index < sizeStr)
  {

    stringstream elementName;
    stringstream elementCount;


    if (isupper(c) == 0)
    {
      std::string message = "Encountered '";
      message += c;
      message += "' when expecting a capital letter.";
      errLog.logPackageError("fbc", FbcSpeciesFormulaMustBeString, 
        packageVersion, level, version, message);

      // at this point we already *know* the formula is bad, at this point
      // the map generated woudl be invalid in any case, so we quit here.
      return;

    }
    else
    {
      elementName << c;
      index++;
    }

    // next char
    if (index < sizeStr) 
    {
      c = chemicalFormula[index];

      // is it a letter
      while (islower(c) != 0 && index < sizeStr)
      {
        elementName << c;
        index++;
        if (index < sizeStr)
        {
          c = chemicalFormula[index];
        }
        else
        {
          break;
        }
      }

      // is the next char a number
      while (isdigit(c) != 0 && index < sizeStr)
      {
        elementCount << c;
        index++;
        if (index < sizeStr)
        {
          c = chemicalFormula[index];
        }
        else
        {
          break;
        }
      }
    }

    const string& chemicalName = elementName.str();
    const string& chemicalNum = elementCount.str();

    // create the pair
    if (chemicalNum.empty() == true)
    {
      chemicalNumber = 1;
    }
    else
    {
      chemicalNumber = atoi(chemicalNum.c_str());
    }

    chemicalSymbols.push_back(make_pair(chemicalName, chemicalNumber));
  }

}


/*
 * Creates a new FbcSpeciesPlugin
 */
FbcSpeciesPlugin::FbcSpeciesPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               FbcPkgNamespaces* fbcns) :
    SBasePlugin(uri, prefix, fbcns)
  , mCharge (0)
  , mIsSetCharge (false)
  , mChemicalFormula ("")
{
}


/*
 * Copy constructor for FbcSpeciesPlugin.
 */
FbcSpeciesPlugin::FbcSpeciesPlugin(const FbcSpeciesPlugin& orig)
  : SBasePlugin(orig)
  , mCharge(orig.mCharge)
  , mIsSetCharge(orig.mIsSetCharge)
  , mChemicalFormula(orig.mChemicalFormula)
{
}


/*
 * Assignment operator for FbcSpeciesPlugin.
 */
FbcSpeciesPlugin& 
FbcSpeciesPlugin::operator=(const FbcSpeciesPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mCharge  = rhs.mCharge;
    mIsSetCharge  = rhs.mIsSetCharge;
    mChemicalFormula  = rhs.mChemicalFormula;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this FbcSpeciesPlugin object.
 */
FbcSpeciesPlugin* 
FbcSpeciesPlugin::clone () const
{
  return new FbcSpeciesPlugin(*this);
}


/*
 * Destructor for FbcSpeciesPlugin.
 */
FbcSpeciesPlugin::~FbcSpeciesPlugin()
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
FbcSpeciesPlugin::createObject (XMLInputStream&)
{
  return NULL; 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
FbcSpeciesPlugin::writeElements (XMLOutputStream&) const
{
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
FbcSpeciesPlugin::hasRequiredElements () const
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
FbcSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("charge");
  attributes.add("chemicalFormula");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcSpeciesPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
   // dont call this as all it does it log unknown attributes
//  SBasePlugin::readAttributes(attributes, expectedAttributes);
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::string name = attributes.getName(i);
    std::string uri  = attributes.getURI(i);

    if (uri != mURI) continue;

    if (!expectedAttributes.hasAttribute(name))
    {  
      getErrorLog()->logPackageError("fbc", FbcSpeciesAllowedL3Attributes,
        getPackageVersion(), getLevel(), getVersion());
    }      
  }
  
  if (mSBMLExt->getLevel(mURI) > 2)
  {
    XMLTriple tripleCharge("charge", mURI, mPrefix);
    unsigned int numErrs = getErrorLog()->getNumErrors();
    mIsSetCharge = attributes.readInto(tripleCharge, mCharge, getErrorLog(),
      false, getLine(), getColumn());
    if (mIsSetCharge == false)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
        getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("fbc", FbcSpeciesChargeMustBeInteger,
          getPackageVersion(), getLevel(), getVersion());
      }
    }

    XMLTriple tripleChemicalFormula("chemicalFormula", mURI, mPrefix);
    bool assigned = attributes.readInto(tripleChemicalFormula, mChemicalFormula);
    if (assigned == true)
    {
      parseChemicalFormula(mChemicalFormula, *(getErrorLog()), getPackageVersion(),
        getLevel(), getVersion());
    }

  }
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcSpeciesPlugin::writeAttributes (XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetCharge() == true)
    stream.writeAttribute("charge", getPrefix(), mCharge);

  if (isSetChemicalFormula() == true)
    stream.writeAttribute("chemicalFormula", getPrefix(), mChemicalFormula);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
FbcSpeciesPlugin::getAllElements(ElementFilter*)
{
  List* ret = new List();

  return ret;
}


/*
 * Returns the value of the "charge" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getCharge() const
{
  return mCharge;
}


/*
 * Returns the value of the "chemicalFormula" attribute of this FbcSpeciesPlugin.
 */
const std::string&
FbcSpeciesPlugin::getChemicalFormula() const
{
  return mChemicalFormula;
}


/*
 * Returns true/false if charge is set.
 */
bool
FbcSpeciesPlugin::isSetCharge() const
{
  return mIsSetCharge;
}


/*
 * Returns true/false if chemicalFormula is set.
 */
bool
FbcSpeciesPlugin::isSetChemicalFormula() const
{
  return (mChemicalFormula.empty() == false);
}


/*
 * Sets charge and returns value indicating success.
 */
int
FbcSpeciesPlugin::setCharge(int charge)
{
  mCharge = charge;
  mIsSetCharge = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets chemicalFormula and returns value indicating success.
 */
int
FbcSpeciesPlugin::setChemicalFormula(const std::string& chemicalFormula)
{
  {
    mChemicalFormula = chemicalFormula;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets charge and returns value indicating success.
 */
int
FbcSpeciesPlugin::unsetCharge()
{
  mCharge = SBML_INT_MAX;
  mIsSetCharge = false;

  if (isSetCharge() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets chemicalFormula and returns value indicating success.
 */
int
FbcSpeciesPlugin::unsetChemicalFormula()
{
  mChemicalFormula.erase();

  if (mChemicalFormula.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
FbcSpeciesPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
FbcSpeciesPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
FbcSpeciesPlugin::enablePackageInternal(const std::string& ,
                                   const std::string& , bool )
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
FbcSpeciesPlugin::accept(SBMLVisitor&) const
{

  return true;
}
/** @endcond */



#endif /* __cplusplus */
/** @cond doxygenIgnored */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_getCharge(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->getCharge() 
    : SBML_INT_MAX;
}

LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetCharge(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? 
    static_cast<int>(static_cast<FbcSpeciesPlugin*>(fbc)->isSetCharge()) : 0;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setCharge(SBasePlugin_t * fbc, int charge)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->setCharge(charge)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetCharge(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->unsetCharge()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
char *
FbcSpeciesPlugin_getChemicalFormula(SBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcSpeciesPlugin*>(fbc)->getChemicalFormula().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcSpeciesPlugin*>(fbc)->getChemicalFormula().c_str());
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetChemicalFormula(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcSpeciesPlugin*>(fbc)->isSetChemicalFormula()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChemicalFormula(SBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcSpeciesPlugin*>(fbc)->setChemicalFormula(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetChemicalFormula(SBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcSpeciesPlugin*>(fbc)->unsetChemicalFormula()
    : LIBSBML_INVALID_OBJECT;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END




