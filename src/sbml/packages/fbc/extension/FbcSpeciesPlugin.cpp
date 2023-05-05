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


bool
isWellFormedChemicalFormula(const std::string& chemicalFormula)
{
  size_t index = 0;
  size_t sizeStr = chemicalFormula.size();
  char c = chemicalFormula[index];
  bool valid = true;

  // first must be a capital letter
  if (isupper(c) == 0)
  {
    valid = false;
    return valid;
  }
  
  if (sizeStr == 1)
  { 
    return valid;
  }

  index++;
  bool prevNum = false;
  while (valid && index < sizeStr)
  {
    c = chemicalFormula[index];

    // if it is a letter it should be
    // upper if it follows a nume
    if (isalpha(c) != 0)
    {
      if (prevNum)
      {
        if (isupper(c) == 0)
        { 
          valid = false;
        }
      }
      prevNum = false;
    }
    else
    {
      prevNum = true;
    }

    index++;
  }
  return valid;
}
static void
parseChemicalFormula(std::string& chemicalFormula, 
                     SBMLErrorLog& errLog, unsigned int packageVersion, 
                     unsigned int level, unsigned int version, 
                     unsigned int line=0, unsigned int col=0,
                     Species* species = NULL)
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
      if (species)
      {
        message += " The chemicalFormula '";
        message += chemicalFormula;
        if (species->isSetId())
        {
          message += "' for the species with id '";
          message += species->getId();
        }
        message += "' has incorrect syntax.";
      }
      errLog.logPackageError("fbc", FbcSpeciesFormulaMustBeString, 
        packageVersion, level, version, message, line, col);

      // at this point we already *know* the formula is bad, at this point
      // the map generated would be invalid in any case, so we quit here.
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
    FbcSBasePlugin(uri, prefix, fbcns)
  , mCharge (0)
  , mChargeAsDouble (0.0)
  , mIsSetCharge (false)
  , mChemicalFormula ("")
{
}


/*
 * Copy constructor for FbcSpeciesPlugin.
 */
FbcSpeciesPlugin::FbcSpeciesPlugin(const FbcSpeciesPlugin& orig)
  : FbcSBasePlugin(orig)
  , mCharge(orig.mCharge)
  , mChargeAsDouble(orig.mChargeAsDouble)
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
    this->FbcSBasePlugin::operator=(rhs);
    mCharge  = rhs.mCharge;
    mChargeAsDouble = rhs.mChargeAsDouble;
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


/** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
FbcSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  FbcSBasePlugin::addExpectedAttributes(attributes);

  attributes.add("charge");
  attributes.add("chemicalFormula");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcSpeciesPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int pkgVersion = getPackageVersion();

   // dont call this as all it does it log unknown attributes
//  FbcSBasePlugin::readAttributes(attributes, expectedAttributes);
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::string name = attributes.getName(i);
    std::string uri  = attributes.getURI(i);

    if (uri != mURI) continue;

    if (!expectedAttributes.hasAttribute(name))
    {  
      getErrorLog()->logPackageError("fbc", FbcSpeciesAllowedL3Attributes,
        pkgVersion, getLevel(), getVersion(), "", getLine(), getColumn());
    }      
  }
  
  if (mSBMLExt->getLevel(mURI) > 2)
  {
    XMLTriple tripleCharge("charge", mURI, mPrefix);
    unsigned int numErrs = getErrorLog()->getNumErrors();
    if (pkgVersion < 3)
    {
      mIsSetCharge = attributes.readInto(tripleCharge, mCharge, getErrorLog(),
        false, getLine(), getColumn());
    }
    else
    {
      mIsSetCharge = attributes.readInto(tripleCharge, mChargeAsDouble, getErrorLog(),
        false, getLine(), getColumn());

    }
    if (mIsSetCharge == false)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
        getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        if (pkgVersion < 3)
        {
          getErrorLog()->logPackageError("fbc", FbcSpeciesChargeMustBeInteger,
            pkgVersion, getLevel(), getVersion(), "", getLine(), getColumn());
        }
        else
        {
          getErrorLog()->logPackageError("fbc", FbcSpeciesChargeMustBeDouble,
            pkgVersion, getLevel(), getVersion(), "", getLine(), getColumn());

        }
      }
    }

    

    XMLTriple tripleChemicalFormula("chemicalFormula", mURI, mPrefix);
    bool assigned = attributes.readInto(tripleChemicalFormula, mChemicalFormula);
    if (assigned == true)
    {
      Species * s = static_cast<Species*>(getParentSBMLObject());
      parseChemicalFormula(mChemicalFormula, *(getErrorLog()), pkgVersion,
        getLevel(), getVersion(), getLine(), getColumn(), s);
    }

  }
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcSpeciesPlugin::writeAttributes (XMLOutputStream& stream) const
{
  FbcSBasePlugin::writeAttributes(stream);

  if (isSetCharge() == true)
  {
    if (getPackageVersion() < 3)
    {
      stream.writeAttribute("charge", getPrefix(), mCharge);
    }
    else
    {
      stream.writeAttribute("charge", getPrefix(), mChargeAsDouble);
    }
  }
  if (isSetChemicalFormula() == true)
    stream.writeAttribute("chemicalFormula", getPrefix(), mChemicalFormula);

}


  /** @endcond */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

/*
 * Returns the value of the "charge" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getCharge() const
{
  return mCharge;
}


double
FbcSpeciesPlugin::getChargeAsDouble() const
{
  return mChargeAsDouble;
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
* Sets charge and returns value indicating success.
*/
int
FbcSpeciesPlugin::setCharge(double charge)
{
  mChargeAsDouble = charge;
  mIsSetCharge = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets chemicalFormula and returns value indicating success.
 */
int
FbcSpeciesPlugin::setChemicalFormula(const std::string& chemicalFormula)
{
  if (isWellFormedChemicalFormula(chemicalFormula))
  {
    mChemicalFormula = chemicalFormula;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    // since not setting an invalid formula would be a change 
    // in behaviour I set it anyway
    mChemicalFormula = chemicalFormula;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
}


/*
 * Unsets charge and returns value indicating success.
 */
int
FbcSpeciesPlugin::unsetCharge()
{
  mCharge = SBML_INT_MAX;
  mChargeAsDouble = util_NaN();
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
  FbcSBasePlugin::setSBMLDocument(d);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
FbcSpeciesPlugin::connectToParent(SBase* sbase)
{
  FbcSBasePlugin::connectToParent(sbase);

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


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getAttribute(const std::string& attributeName,
                               bool& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getAttribute(const std::string& attributeName,
                               int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "charge")
  {
    value = getCharge();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getAttribute(const std::string& attributeName,
                               double& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (attributeName == "charge")
  {
    value = getChargeAsDouble();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getAttribute(const std::string& attributeName,
                               unsigned int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::getAttribute(const std::string& attributeName,
                               std::string& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "chemicalFormula")
  {
    value = getChemicalFormula();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcSpeciesPlugin's attribute
 * "attributeName" is set.
 */
bool
FbcSpeciesPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = FbcSBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "charge")
  {
    value = isSetCharge();
  }
  else if (attributeName == "chemicalFormula")
  {
    value = isSetChemicalFormula();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "charge")
  {
    return_value = setCharge(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::setAttribute(const std::string& attributeName, double value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "charge")
  {
    return_value = setCharge(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::setAttribute(const std::string& attributeName,
                               unsigned int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::setAttribute(const std::string& attributeName,
                               const std::string& value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "chemicalFormula")
  {
    return_value = setChemicalFormula(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
 */
int
FbcSpeciesPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = FbcSBasePlugin::unsetAttribute(attributeName);

  if (attributeName == "charge")
  {
    value = unsetCharge();
  }
  else if (attributeName == "chemicalFormula")
  {
    value = unsetChemicalFormula();
  }

  return value;
}

/** @endcond */



#endif /* __cplusplus */
/** @cond doxygenIgnored */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_getCharge(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->getCharge() 
    : SBML_INT_MAX;
}

LIBSBML_EXTERN
double
FbcSpeciesPlugin_getChargeAsDouble(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->getChargeAsDouble()
    : util_NaN();
}

LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetCharge(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) ? 
    static_cast<int>(static_cast<FbcSpeciesPlugin*>(fbc)->isSetCharge()) : 0;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setCharge(FbcSBasePlugin_t * fbc, int charge)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->setCharge(charge)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChargeAsDouble(FbcSBasePlugin_t * fbc, double charge)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->setCharge(charge)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetCharge(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcSpeciesPlugin*>(fbc)->unsetCharge()
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
char *
FbcSpeciesPlugin_getChemicalFormula(FbcSBasePlugin_t * fbc)
{
  if (fbc == NULL) return NULL;

  return static_cast<FbcSpeciesPlugin*>(fbc)->getChemicalFormula().empty() 
    ? safe_strdup("")
    : safe_strdup(static_cast<FbcSpeciesPlugin*>(fbc)->getChemicalFormula().c_str());
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetChemicalFormula(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<int>
             (static_cast<FbcSpeciesPlugin*>(fbc)->isSetChemicalFormula()) 
    : 0;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChemicalFormula(FbcSBasePlugin_t * fbc, const char * chemform)
{
  return (fbc != NULL) 
    ? static_cast<FbcSpeciesPlugin*>(fbc)->setChemicalFormula(chemform)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetChemicalFormula(FbcSBasePlugin_t * fbc)
{
  return (fbc != NULL) 
    ? static_cast<FbcSpeciesPlugin*>(fbc)->unsetChemicalFormula()
    : LIBSBML_INVALID_OBJECT;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END




