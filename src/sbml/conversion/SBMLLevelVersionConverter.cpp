/**
 * @file    SBMLLevelVersionConverter.cpp
 * @brief   Implementation of SBMLLevelVersionConverter, the base class of package extensions.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include <sbml/conversion/SBMLLevelVersionConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/util/MathFilter.h>

#ifdef USE_COMP
#include <sbml/packages/comp/common/CompExtensionTypes.h>
#endif

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

  
/** @cond doxygenLibsbmlInternal */
void SBMLLevelVersionConverter::init()
{
  SBMLLevelVersionConverter converter;
  SBMLConverterRegistry::getInstance().addConverter(&converter);
}
/** @endcond */


SBMLLevelVersionConverter::SBMLLevelVersionConverter () 
  : SBMLConverter("SBML Level Version Converter")
  , mSRIds (NULL)
  , mMathElements (NULL)
{
}


/*
 * Copy constructor.
 */
SBMLLevelVersionConverter::SBMLLevelVersionConverter(const SBMLLevelVersionConverter& orig) :
    SBMLConverter(orig),
  mSRIds (NULL)
  , mMathElements (NULL)
{
}


/*
 * Destroy this object.
 */
SBMLLevelVersionConverter::~SBMLLevelVersionConverter ()
{
  delete mSRIds;
  delete mMathElements;
}


/*
 * Assignment operator for SBMLLevelVersionConverter.
 */
SBMLLevelVersionConverter& 
SBMLLevelVersionConverter::operator=(const SBMLLevelVersionConverter& rhs)
{  
  if(&rhs!=this)
  {
    this->SBMLConverter::operator =(rhs);
  }

  return *this;
}


SBMLLevelVersionConverter*
SBMLLevelVersionConverter::clone () const
{
  return new SBMLLevelVersionConverter(*this);
}


ConversionProperties
SBMLLevelVersionConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  static bool init = false;

  if (init) 
  {
    return prop;
  }
  else
  {
    SBMLNamespaces * sbmlns = new SBMLNamespaces(); // default namespaces
    prop.setTargetNamespaces(sbmlns); // this gets cloned
    prop.addOption("strict", true,
                   "Whether validity should be strictly preserved");
    prop.addOption("setLevelAndVersion", true, 
                   "Convert the model to a given Level and Version of SBML");
    prop.addOption("addDefaultUnits", true,
                   "Whether default units should be added when converting to L3");
    delete sbmlns;
    init = true;
    return prop;
  }
}


bool 
SBMLLevelVersionConverter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("setLevelAndVersion"))
    return false;
  return true;
}


unsigned int 
SBMLLevelVersionConverter::getTargetLevel()
{
  if (getTargetNamespaces() != NULL)
  {
    return getTargetNamespaces()->getLevel();
  }
  else
  {
    return SBML_DEFAULT_LEVEL;
  }
}


unsigned int 
SBMLLevelVersionConverter::getTargetVersion()
{
  if (getTargetNamespaces() != NULL)
  {
    return getTargetNamespaces()->getVersion();
  }
  else
  {
    return SBML_DEFAULT_VERSION;
  }
}


bool 
SBMLLevelVersionConverter::getValidityFlag()
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("strict") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("strict");
  }
}

bool
SBMLLevelVersionConverter::getAddDefaultUnits()
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("addDefaultUnits") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("addDefaultUnits");
  }
}


int
SBMLLevelVersionConverter::convert()
{
  SBMLNamespaces *ns = getTargetNamespaces();
  if (ns == NULL)
  {
    return LIBSBML_CONV_INVALID_TARGET_NAMESPACE;
  }
  bool hasValidNamespace = ns->isValidCombination();
  if (hasValidNamespace == false)
  {
    return LIBSBML_CONV_INVALID_TARGET_NAMESPACE;
  }
  
  if (mDocument == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  bool strict = getValidityFlag();

  //bool success = mDocument->setLevelAndVersion(mTargetNamespaces->getLevel(), 
  //  mTargetNamespaces->getVersion(), false);
  /* mDocument->check we are not already the level and version */

  unsigned int currentLevel = mDocument->getLevel();
  unsigned int currentVersion = mDocument->getVersion();
  unsigned int targetLevel = getTargetLevel(); 
  unsigned int targetVersion = getTargetVersion();
  bool resetAnnotations = false;
  // depending on the level and version vcard4 may be used
  // but for earlier levels only vcard3 was acceptable
  // if we are doing a down conversion we can reset the annotation
  if (currentLevel == 3 && targetLevel < 3)
    resetAnnotations = true;


  if (currentLevel == targetLevel && currentVersion == targetVersion)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  /* since this function will write to the error log we should
   * clear anything in the log first
   */
  mDocument->getErrorLog()->clearLog();
  Model * currentModel = mDocument->getModel();

  bool conversion = false;

  bool ignorePackages = getProperties()->getBoolValue("ignorePackages");

  /* if model has extensions we cannot convert 
   * except to L3V2 
   */
  if (targetLevel != 3 && !ignorePackages && mDocument->getNumPlugins() > 0)
  {
    // disable all unused packages
    SBMLExtensionRegistry::getInstance().disableUnusedPackages(mDocument);
    // if there are still plugins enabled fail
    // unless it is the l3v2 plugin and we are an l3v2 document
    if (currentLevel == 3 && currentVersion == 2)
    {
      if (mDocument->getNumPlugins() > 1 
        || (mDocument->getNumPlugins() == 1 && 
          mDocument->getPlugin(0)->getURI() != "http://www.sbml.org/sbml/level3/version2/core"))
      {
        mDocument->getErrorLog()->logError(PackageConversionNotSupported,
          currentLevel, currentVersion);
        return LIBSBML_CONV_PKG_CONVERSION_NOT_AVAILABLE;

      }

    }
    else
    {
      if (mDocument->getNumPlugins() > 0)
      {
        mDocument->getErrorLog()->logError(PackageConversionNotSupported,
          currentLevel, currentVersion);
        return LIBSBML_CONV_PKG_CONVERSION_NOT_AVAILABLE;

      }
    }
  }


  // deal with the case where a package that libsbml does not know about
  // has been read in
  // the model is not L3V1 core ONLY and so should not be
  // converted by this function

  // TO DO - SK Comment

  //if (mDocument->mAttributesOfUnknownPkg.isEmpty())
  //{
  //  mDocument->getErrorLog()->logError(PackageConversionNotSupported, 
  //                                     currentLevel, currentVersion);
  //  return LIBSBML_CONV_PKG_CONVERSION_NOT_AVAILABLE;
  //}
  unsigned char origValidators = mDocument->getApplicableValidators();
  unsigned char convValidators = mDocument->getConversionValidators();
  /* if strict = true we will only convert a valid model
   * to a valid model with a valid internal representation
   */
  /* see whether the unit validator is on */
  //bool strictSBO   = ((convValidators & 0x04) == 0x04);
  bool strictUnits = strict && ((convValidators & UnitsCheckON) == UnitsCheckON);
  
  if (strict == true)
  {
    /* use validators that the user has selected
    */
    /* hack to catch errors caught at read time */
    char* doc = writeSBMLToString(mDocument);
    SBMLDocument *d = readSBMLFromString(doc);
    util_free(doc);
    unsigned int errors = d->getNumErrors();

    for (unsigned int i = 0; i < errors; i++)
    {
      mDocument->getErrorLog()->add(*(d->getError(i)));
    }
    delete d;

    mDocument->checkConsistency();
    errors = mDocument->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);

    /* if the current model is not valid dont convert 
    */
    if (errors > 0)
    {
      return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    }

    mDocument->getErrorLog()->clearLog();
  }

  unsigned int i;
  bool duplicateAnn = false;
  //look at annotation on sbml element - since validation only happens on the model :-(
  XMLNode *ann = mDocument->getAnnotation();
  if (ann != NULL)
  {
    for (i = 0; i < ann->getNumChildren(); i++)
    {
      std::string name = ann->getChild(i).getPrefix();
      for( unsigned int n= i+1; n < ann->getNumChildren(); n++)
      {
        if (ann->getChild(n).getPrefix() == name)
          duplicateAnn = true;
      }
    }
  }

  if (currentModel != NULL)
  {
    unsigned int origLevel = 0;
    unsigned int origVersion = 0;
    Model origModel(3,2);
    if (strict)
    {
      /* here we are strict and only want to do
       * conversion if output will be valid
       *
       * save a copy of the model so it can be restored
       */
      origLevel = currentLevel;
      origVersion = currentVersion;
      origModel = *currentModel;
    }

    conversion = performConversion(strict, strictUnits, duplicateAnn);
      
    if (conversion == false)
    {
      /* if we were strict restore original model */

      if (strict)
      {
        mDocument->setApplicableValidators(origValidators);
        mDocument->updateSBMLNamespace("core", origLevel, origVersion);
      }
    }
    else
    {
      if (strict)
      {
        /* now we want to mDocument->check whether the resulting model is valid
         */
        validateConvertedDocument();
        bool errors = has_fatal_errors(origLevel, origVersion);
        if (errors)
        { /* error - we dont covert
           * restore original values and return
           */
          conversion = false;
          /* undo any changes */
          delete currentModel;
          currentModel = origModel.clone();
          mDocument->updateSBMLNamespace("core", origLevel, origVersion);
          mDocument->setApplicableValidators(origValidators);
        }
        else
        {
          if (resetAnnotations) 
          {
            // hack to force the model history to think it haschanged - this will
            // change the vacrd if necessary
            if (mDocument->isSetModel() && mDocument->getModel()->isSetModelHistory())
            {
              ModelHistory * history = mDocument->getModel()->getModelHistory()->clone();
              mDocument->getModel()->setModelHistory(history);
              delete history;
            }
          }
        }
      }
      else
      {
        if (resetAnnotations) 
        {
          // hack to force the model history to think it haschanged - this will
          // change the vacrd if necessary
          if (mDocument->isSetModel() && mDocument->getModel()->isSetModelHistory())
          {
            ModelHistory * history = mDocument->getModel()->getModelHistory()->clone();
            mDocument->getModel()->setModelHistory(history);
            delete history;
          }
        }
      }
    }
  }
  else
  {
    mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
    conversion = true;
  }

  /* restore original value */
  mDocument->setApplicableValidators(origValidators); 
  

  if (conversion)
    return LIBSBML_OPERATION_SUCCESS;
  else
    return LIBSBML_OPERATION_FAILED;
}
 

/** @cond doxygenLibsbmlInternal */
bool
SBMLLevelVersionConverter::performConversion(bool strict, bool strictUnits, 
                                        bool duplicateAnn)
{
  bool conversion = false;
 
  bool doConversion = false;

  bool addDefaultUnits = getAddDefaultUnits();
  
  unsigned int currentLevel = mDocument->getLevel();
  unsigned int currentVersion = mDocument->getVersion();
  unsigned int targetLevel = getTargetLevel(); 
  unsigned int targetVersion = getTargetVersion();
  Model * currentModel = mDocument->getModel();

  unsigned int i = 0;
  
  if (currentLevel == 1)
  {
    switch (targetLevel)
    {
    case 1:
      switch (targetVersion)
      {
      case 1:
        mDocument->getErrorLog()->logError(CannotConvertToL1V1);
        break;
      case 2:
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        conversion = true;
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      break;
    case 2:    
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL2v1Compatibility(true)))
        {
          doConversion = true;
        }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL2v2Compatibility(true)))
        {
          doConversion = true;
        }
        break;
      case 3:
        if (!conversion_errors(mDocument->checkL2v3Compatibility(true)))
        {
          doConversion = true;
        }
        break;
      case 4:
        if (!conversion_errors(mDocument->checkL2v4Compatibility()))
        {
          doConversion = true;
        }
        break;
      case 5:
        if (!conversion_errors(mDocument->checkL2v4Compatibility()))
        {
          doConversion = true;
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        currentModel->removeParameterRuleUnits(strict);
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        currentModel->convertL1ToL2();
        conversion = true;
      }
      break;
    case 3:
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL3v1Compatibility()))
        {
          doConversion = true;
        }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL3v2Compatibility()))
        {
          doConversion = true;
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
         
        currentModel->removeParameterRuleUnits(strict);
        currentModel->convertParametersToLocals(targetLevel, targetVersion);
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        currentModel->convertL1ToL3(addDefaultUnits);
        conversion = true;
      }
      break;
    default:
      mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
      break;
    }
  }
  else if (currentLevel == 2)
  {
    switch (targetLevel)
    {
    case 1:
      switch (targetVersion)
      {
      case 1:
        mDocument->getErrorLog()->logError(CannotConvertToL1V1);
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL1Compatibility(true)))
        {
          doConversion = true;
          /* if existing model is L2V4 need to mDocument->check that
          * units are strict
          */
          if (currentVersion == 4 && strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL1);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL1);
                doConversion = false;
              }
            }
          }
          else
          {
            doConversion = true;
          }
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        mDocument->expandFunctionDefinitions();
        mDocument->expandInitialAssignments();
        currentModel->convertL2ToL1(strict);
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        conversion = true;
      }
      break;
    case 2:
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL2v1Compatibility(true)))
        {
          doConversion = true;
          /* if existing model is L2V4 need to mDocument->check that
          * units are strict
          */
          if (currentVersion == 4 && strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v1);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v1);
                doConversion = false;
              }
            }
          }
          else
          {
            doConversion = true;
          }
        }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL2v2Compatibility(true)))
        {
          doConversion = true;
          /* if existing model is L2V4 need to mDocument->check that
          * units are strict
          */
          if (currentVersion == 4 && strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v2);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v2);
                doConversion = false;
              }
            }
          }
          
          if (currentVersion == 4 && !hasStrictSBO())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictSBORequiredInL2v2);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictSBORequiredInL2v2);
                doConversion = false;
              }
            }
          }
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId() 
                                == DuplicateAnnotationInvalidInL2v2)
              duplicateAnn = true;
          }
         }
        break;
      case 3:
        if (!conversion_errors(mDocument->checkL2v3Compatibility(true)))
        {
          doConversion = true;
          /* if existing model is L2V4 need to mDocument->check that
          * units are strict
          */
          if (currentVersion == 4 && strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v3);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v3);
                doConversion = false;
              }
            }
          }
          
          if (currentVersion == 4 && !hasStrictSBO())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictSBORequiredInL2v3);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictSBORequiredInL2v3);
                doConversion = false;
              }
            }
          }
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId() 
                            == DuplicateAnnotationInvalidInL2v3)
              duplicateAnn = true;
          }
         }
        break;
      case 4:
        if (!conversion_errors(mDocument->checkL2v4Compatibility()))
        {
          doConversion = true;
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId() 
                            == DuplicateAnnotationInvalidInL2v4)
              duplicateAnn = true;
          }
        }
        break;
      case 5:
        if (!conversion_errors(mDocument->checkL2v4Compatibility()))
        {
          doConversion = true;
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId() 
                            == DuplicateAnnotationInvalidInL2v4)
              duplicateAnn = true;
          }
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        if (duplicateAnn == true)
        {
          mDocument->removeDuplicateAnnotations();
          currentModel->removeDuplicateTopLevelAnnotations();
        }
        if (targetVersion == 1)
        {
          currentModel->removeSBOTerms(strict);
          mDocument->expandInitialAssignments();
        }
        else if (targetVersion == 2)
        {
          currentModel->removeSBOTermsNotInL2V2(strict);
        }
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        conversion = true;
      }
      break;
    case 3:
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL3v1Compatibility()))
        {
          doConversion = true;
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId() 
                            == DuplicateAnnotationInvalidInL2v4)
              duplicateAnn = true;
          }
        }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL3v2Compatibility()))
        {
          doConversion = true;
          // look for duplicate top targetLevel annotations
          for (i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
          {
            if (mDocument->getErrorLog()->getError(i)->getErrorId()
              == DuplicateAnnotationInvalidInL2v4)
              duplicateAnn = true;
          }
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        if (duplicateAnn == true)
        {
          mDocument->removeDuplicateAnnotations();
          currentModel->removeDuplicateTopLevelAnnotations();
        }
        currentModel->convertParametersToLocals(targetLevel, targetVersion);
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        currentModel->convertL2ToL3(strict, addDefaultUnits);
        conversion = true;
      }
      break;
    default:
      mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
      break;
    }      
  }
  else if (currentLevel == 3)
  {
    switch (targetLevel)
    {
    case 1:
      switch (targetVersion)
      {
      case 1:
        mDocument->getErrorLog()->logError(CannotConvertToL1V1);
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL1Compatibility(true), strictUnits))
        {
          doConversion = true;
          if (strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL1);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL1);
                doConversion = false;
              }
            }
          }
          if (doConversion == true)
          {
            mDocument->expandFunctionDefinitions();
            mDocument->expandInitialAssignments();
            mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
            if (currentVersion == 2)
              currentModel->convertFromL3V2(strict);
            currentModel->convertL3ToL1(strict);
            if (currentVersion > 1)
            {
              currentModel->dealWithFast();
            }
            conversion = true;
          }
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      break;
    case 2:
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL2v1Compatibility(true), strictUnits))
        {
          doConversion = true;
           if (strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v1);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v1);
                doConversion = false;
              }
            }
          }
       }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL2v2Compatibility(true), strictUnits))
        {
          doConversion = true;
          if (strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v2);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v2);
                doConversion = false;
              }
            }
          }
          if (!hasStrictSBO())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictSBORequiredInL2v2);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictSBORequiredInL2v2);
                doConversion = false;
              }
            }
          }
       }
        break;
      case 3:
        if (!conversion_errors(mDocument->checkL2v3Compatibility(true), strictUnits))
        {
          doConversion = true;
          if (strictUnits == true && !hasStrictUnits())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v3);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictUnitsRequiredInL2v3);
                doConversion = false;
              }
            }
          }
          if (!hasStrictSBO())
          {
            if (strict == false)
            {
              mDocument->getErrorLog()->logError(StrictSBORequiredInL2v3);
            }
            else
            {
              if (strictUnits == true)
              {
                mDocument->getErrorLog()->logError(StrictSBORequiredInL2v3);
                doConversion = false;
              }
            }
          }
        }
        break;
      case 4:
        if (!conversion_errors(mDocument->checkL2v4Compatibility(), strictUnits))
        {
          doConversion = true;
        }
        break;
      case 5:
        if (!conversion_errors(mDocument->checkL2v4Compatibility(), strictUnits))
        {
          doConversion = true;
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        if (targetVersion == 1)
        {
          mDocument->expandInitialAssignments();
        }
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        currentModel->convertL3ToL2(strict);
        if (currentVersion == 2)
        {
          // if v1 we have already expanded initial assignments
          if (targetVersion > 1)
          {
            SBMLTransforms::expandL3V2InitialAssignments(currentModel);
          }
          currentModel->convertFromL3V2(strict);
        }
        if (currentVersion > 1)
        {
          currentModel->dealWithFast();
        }
        conversion = true;
      }
      break;
    case 3:
      switch (targetVersion)
      {
      case 1:
        if (!conversion_errors(mDocument->checkL3v1Compatibility(), strictUnits))
        {
          doConversion = true;
        }
        break;
      case 2:
        if (!conversion_errors(mDocument->checkL3v2Compatibility(), strictUnits))
        {
          doConversion = true;
        }
        break;
      default:
        mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
        break;
      }
      if (doConversion == true)
      {
        mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
        if (currentVersion == 2)
        {
          SBMLTransforms::expandL3V2InitialAssignments(currentModel);
          currentModel->convertFromL3V2(strict);
        }
        currentModel->dealWithL3Fast(targetVersion);
        updatePackages(targetVersion);

#ifdef USE_COMP
        CompSBMLDocumentPlugin* compPlug = 
          static_cast<CompSBMLDocumentPlugin*>(mDocument->getPlugin("comp"));
        if (compPlug != NULL)
        {
          for (unsigned int ii = 0; ii < compPlug->getNumModelDefinitions(); ii++)
          {
            compPlug->getModelDefinition(ii)->dealWithL3Fast(targetVersion);
          }
        }
#endif
        
        conversion = true;
      }
      break;
    default:
      mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
      break;
    }

  }

  return conversion;

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

void
SBMLLevelVersionConverter::updatePackages(unsigned int targetVersion)
{
  // this will only work if we have a package with version 1 for both l3v1 and l3v2
  //std::string sbml = writeSBMLToStdString(mDocument);
  //SBMLDocument *tempdoc = readSBMLFromString(sbml.c_str());
  //if (!tempdoc->getErrorLog()->contains(InvalidPackageLevelVersion))
  //{
  //  delete tempdoc;
  //  return;
  //}
  //delete tempdoc;

//  const SBMLExtension* sbmlext = NULL;
  XMLNamespaces *xmlns = mDocument->getNamespaces();
  int numxmlns = xmlns->getLength();
  for (int i = numxmlns - 1; i >= 0; i--)
  {
    const std::string &prefix = xmlns->getPrefix(i);
    if (!prefix.empty())
      mDocument->updateSBMLNamespace(prefix, 3, targetVersion);
  }
}


/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
SBMLLevelVersionConverter::conversion_errors(unsigned int errors, bool strictUnits)
{  
  bool conversion_errors = false;
  // if people have declared that they want to convert, even should 
  // conversion errors occur, then return false, so the conversion will 
  // proceed. In that case we leave the error log in tact, so people are
  // notified about potential issues. 
  if (!getValidityFlag())
  {
    return conversion_errors;
  }


  /* if we are converting back from L3 and do not care about units
   * then we will allow a conversion where the spatialDimensions
   * has not been set
   */
  if (!strictUnits && errors > 0)
  {
    for (unsigned int n = 0; n < errors; n++)
    {
      if (mDocument->getErrorLog()->getError(n)->getErrorId() == L3SpatialDimensionsUnset)
      {
        mDocument->getErrorLog()->remove(NoNon3DCompartmentsInL1);
        mDocument->getErrorLog()->remove(IntegerSpatialDimensions);
      }
    }
    mDocument->getErrorLog()->remove(GlobalUnitsNotDeclared);
    // also allow extend units that are not in substance (or use undefined substance)
    mDocument->getErrorLog()->remove(ExtentUnitsNotSubstance);
  }
  /** 
   * changed this code in line with the rest of the validation 
   * errors: ie each now assigns a severity
   * Error would imply conversion not possible
   * Warning implies lose of data but conversion still possible
   */
  if (errors > 0)
  {
    if (mDocument->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      conversion_errors = true;
  }

  // need to check that we are not down converting something that used the
  // species reference id in math which is not allowed before l3
  if (!conversion_errors && mDocument->getLevel() > 2 && getTargetLevel() < 3)
  {
    if (speciesReferenceIdUsed())
    {
      conversion_errors = true;
      mDocument->getErrorLog()->logError(SpeciesRefIdInMathMLNotSupported,
        getTargetLevel(), getTargetVersion());
    }
  }

  return conversion_errors;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

bool
containsId(const ASTNode* ast, std::string id)
{
  bool present = false;
  List* variables = ast->getListOfNodes(ASTNode_isName);
  IdList vars;
  for (unsigned int i = 0; i < variables->getSize(); i++)
  {
    ASTNode* node = static_cast<ASTNode*>(variables->get(i));
    string   name = node->getName() ? node->getName() : "";
    vars.append(name);
  }
  if (vars.contains(id))
  {
    present = true;
  }
  delete variables;

  return present;
}

void
SBMLLevelVersionConverter::populateMathElements()
{
  MathFilter *mfilter = new MathFilter();
  if (mMathElements != NULL)
  {
    delete mMathElements;
  }
  mMathElements = mDocument->getAllElements(mfilter);
  delete mfilter;

}

bool
SBMLLevelVersionConverter::speciesReferenceIdUsed()
{
  bool used = false;
  // need to check that we are not down converting something that used the
  // species reference id in math
  //if (!mDocument->getErrorLog()->contains(NoIdOnSpeciesReferenceInL2v1))
  //{
  //  // if we havent logged this error then we have nothing to worry about
  //  return used;
  //}

  if (mSRIds == NULL)
  {
    mSRIds = collectSpeciesReferenceIds();
  }

  if (mMathElements == NULL)
  {
    populateMathElements();
  }

  unsigned int i = 0;
  while (!used && i < mMathElements->getSize())
  {
    const ASTNode* ast = static_cast<SBase*>(mMathElements->get(i))->getMath();
    for (unsigned int j = 0; j < mSRIds->size(); j++)
    {
      used = containsId(ast, mSRIds->at(j));
      if (used) break;
    }
    i++;
  }

  return used;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
IdList*
SBMLLevelVersionConverter::collectSpeciesReferenceIds()
{
  IdList* srids = new IdList();

  for (unsigned int i = 0; i < mDocument->getModel()->getNumReactions(); i++)
  {
    Reaction *r = mDocument->getModel()->getReaction(i);
    for (unsigned int j = 0; j < r->getNumReactants(); j++)
    {
      if (r->getReactant(j)->isSetId())
      {
        srids->append(r->getReactant(j)->getId());
      }
    }
    for (unsigned int j = 0; j < r->getNumProducts(); j++)
    {
      if (r->getProduct(j)->isSetId())
      {
        srids->append(r->getProduct(j)->getId());
      }
    }
  }

  return srids;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

bool
SBMLLevelVersionConverter::hasStrictUnits()
{
  unsigned int errors = 0;

  UnitConsistencyValidator unit_validator;
  unit_validator.init();
  errors = unit_validator.validate(*mDocument);

  /* only want to return true if there are errors
  * not warnings
  * but in a L2V4 model they will only be warnings
  * so need to go by ErrorId
  */
  if (errors > 0)
  {
    const std::list<SBMLError>& fails = unit_validator.getFailures();
    std::list<SBMLError>::const_iterator iter;

    for (iter = fails.begin(); iter != fails.end(); ++iter)
    {
      if ( iter->getErrorId() > UpperUnitBound)
      {
        --errors;
      }
    }
  }
    
  return (errors == 0);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLLevelVersionConverter::hasStrictSBO()
{
  unsigned int errors = 0;

  SBOConsistencyValidator sbo_validator;
  sbo_validator.init();
  errors = sbo_validator.validate(*mDocument);

  /* only want to return true if there are errors
  * not warnings
  * but in a L2V4 model they will only be warnings
  * so need to go by ErrorId
  * InvalidDelaySBOTerm is the largest errorId that
  * would be considered an error in other level/versions
  */
  if (errors > 0)
  {
    const std::list<SBMLError>& fails = sbo_validator.getFailures();
    std::list<SBMLError>::const_iterator iter;

    for (iter = fails.begin(); iter != fails.end(); ++iter)
    {
      if ( iter->getErrorId() > InvalidDelaySBOTerm)
      {
        --errors;
      }
    }
  }

  return (errors == 0);

}
/** @endcond */



/** @cond doxygenIgnored */
unsigned int
SBMLLevelVersionConverter::validateConvertedDocument()
{
  // force a read
  std::string sbml = writeSBMLToStdString(mDocument);
  SBMLDocument *tempdoc = readSBMLFromString(sbml.c_str());
  unsigned int nerrors = tempdoc->getErrorLog()->getNumErrors();
  for (unsigned int i = 0; i < nerrors; i++)
  {
    const SBMLError * error = tempdoc->getErrorLog()->getError(i);
    mDocument->getErrorLog()->add(*(error));
  }
  delete tempdoc;

  nerrors += mDocument->checkConsistency();

  if (mDocument->getLevel() < 2 || (mDocument->getLevel() == 2 && mDocument->getVersion() == 1))
  {
    if (mDocument->getModel()->getNumInitialAssignments() > 0)
    {
      std::string msg = "Initial assignment was not correctly converted.";
      mDocument->getErrorLog()->logError(InitialAssignNotValidComponent, mDocument->getLevel(), 
        mDocument->getVersion(), msg);
      nerrors += 1;
    }
  }

//  nerrors += mDocument->checkInternalConsistency();

  return nerrors;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
SBMLLevelVersionConverter::has_fatal_errors(unsigned int level, unsigned int version)
{ 
  if (mDocument->getNumErrors() == 0)
  {
    return false;
  }
  else if (mDocument->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
  {
    return true;
  }
  else
  {
    if (level == 3 && version == 2)
    {
      // there are a coupl of errors that will be logged as general warnings
      // since they were not in the relevant spec BUT should still stop a conversion
      if (mDocument->getErrorLog()->contains(MathResultMustBeNumeric) ||
        (mDocument->getErrorLog()->contains(PieceNeedsBoolean)) ||
        (mDocument->getErrorLog()->contains(NumericOpsNeedNumericArgs)) ||
        (mDocument->getErrorLog()->contains(ArgsToEqNeedSameType)) ||
        (mDocument->getErrorLog()->contains(PiecewiseNeedsConsistentTypes)) ||
        (mDocument->getErrorLog()->contains(ApplyCiMustBeUserFunction)) ||
        (mDocument->getErrorLog()->contains(ApplyCiMustBeModelComponent)) ||
        (mDocument->getErrorLog()->contains(KineticLawParametersAreLocalOnly)) ||
        (mDocument->getErrorLog()->contains(OpsNeedCorrectNumberOfArgs)) ||
        (mDocument->getErrorLog()->contains(BooleanOpsNeedBooleanArgs)))
      {
        return true;
      }
    }
    return false;
  }

}
/** @endcond */



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


