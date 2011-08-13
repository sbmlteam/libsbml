/**
 * @file    SBMLLevelVersionConverter.cpp
 * @brief   Implementation of SBMLLevelVersionConverter, the base class of package extensions.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
#include <sbml/conversion/SBMLConverterRegister.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>
#ifdef USE_LAYOUT
  #include <sbml/packages/layout/extension/LayoutExtension.h>
  #include <sbml/packages/layout/extension/LayoutModelPlugin.h>
#endif

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

  
void SBMLLevelVersionConverter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new SBMLLevelVersionConverter());
}


SBMLLevelVersionConverter::SBMLLevelVersionConverter () :
    SBMLConverter()
{
}


/*
 * Copy constructor.
 */
SBMLLevelVersionConverter::SBMLLevelVersionConverter(const SBMLLevelVersionConverter& orig) :
    SBMLConverter(orig)
{
}


/*
 * Destroy this object.
 */
SBMLLevelVersionConverter::~SBMLLevelVersionConverter ()
{
}


/*
 * Assignment operator for SBMLLevelVersionConverter.
 */
SBMLLevelVersionConverter& 
SBMLLevelVersionConverter::operator=(const SBMLLevelVersionConverter& rhs)
{  
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
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
  prop.setTargetNamespaces(new SBMLNamespaces()); // default namespaces
  prop.addOption("strict", true, "should validity be preserved");
  prop.addOption("setLevelAndVersion", true, 
                                        "this is checked by matchProperties");
  return prop;
}


bool 
SBMLLevelVersionConverter::matchesProperties(const ConversionProperties &props) const
{
  if (&props == NULL || !props.hasOption("setLevelAndVersion"))
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
  return getProperties()->getBoolValue("strict");
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

  /* if model has extensions we cannot convert */
  if (mDocument->getNumPlugins() > 0)
  {
    /* if it is layout extension and we are converting between l2 targetVersions
     * that is okay
     */
    if (currentLevel == 2 && mDocument->getPlugin("layout") != NULL)
    {
      bool problem = false;
      /* if there is no layout - no problem ! */
#ifdef USE_LAYOUT
      if (currentModel != NULL)
      {
        LayoutModelPlugin * mplugin = static_cast<LayoutModelPlugin*>
                      (currentModel->getPlugin("layout"));
      
        if (mplugin != NULL && mplugin->getNumLayouts() > 0 && targetLevel != 2)
        {
          problem = true;
        }
        else if (mDocument->getNumPlugins() > 1)
        {
          problem = true;
        }
      }
#endif      
      if (problem == true)
      {
        mDocument->getErrorLog()->logError(PackageConversionNotSupported, 
                                           currentLevel, currentVersion);
        return LIBSBML_CONV_PKG_CONVERSION_NOT_AVAILABLE;
      }
    }
    else
    {
      mDocument->getErrorLog()->logError(PackageConversionNotSupported, 
                                         currentLevel, currentVersion);
      return LIBSBML_CONV_PKG_CONVERSION_NOT_AVAILABLE;
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
  bool strictSBO   = ((convValidators & 0x04) == 0x04);
  bool strictUnits = ((convValidators & 0x10) == 0x10);
  
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

    errors += mDocument->checkConsistency();
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
    bool doConversion = false;
    unsigned int origLevel;
    unsigned int origVersion;
    Model *origModel;
    if (strict)
    {
      /* here we are strict and only want to do
       * contargetVersion if output will be valid
       *
       * save a copy of the model so it can be restored
       */
      origLevel = currentLevel;
      origVersion = currentVersion;
      origModel = currentModel->clone();
    }
      
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
          if (!conversion_errors(mDocument->checkL2v1Compatibility()))
          {
            doConversion = true;
          }
          break;
        case 2:
          if (!conversion_errors(mDocument->checkL2v2Compatibility()))
          {
            doConversion = true;
          }
          break;
        case 3:
          if (!conversion_errors(mDocument->checkL2v3Compatibility()))
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
        default:
          mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
          break;
        }
        if (doConversion == true)
        {
          currentModel->removeParameterRuleUnits(strict);
          currentModel->convertParametersToLocals(targetLevel, targetVersion);
          mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
          currentModel->convertL1ToL3();
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
          if (!conversion_errors(mDocument->checkL1Compatibility()))
          {
            doConversion = true;
            /* if existing model is L2V4 need to mDocument->check that
            * units are strict
            */
            if (currentVersion == 4 && !hasStrictUnits())
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
          currentModel->convertL2ToL1(strict);
          mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
          conversion = true;
        }
        break;
      case 2:
        switch (targetVersion)
        {
        case 1:
          if (!conversion_errors(mDocument->checkL2v1Compatibility()))
          {
            doConversion = true;
            /* if existing model is L2V4 need to mDocument->check that
            * units are strict
            */
            if (currentVersion == 4 && !hasStrictUnits())
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
          if (!conversion_errors(mDocument->checkL2v2Compatibility()))
          {
            doConversion = true;
            /* if existing model is L2V4 need to mDocument->check that
            * units are strict
            */
            if (currentVersion == 4 && !hasStrictUnits())
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
          if (!conversion_errors(mDocument->checkL2v3Compatibility()))
          {
            doConversion = true;
            /* if existing model is L2V4 need to mDocument->check that
            * units are strict
            */
            if (currentVersion == 4 && !hasStrictUnits())
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
          currentModel->convertL2ToL3();
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
            if (!conversion_errors(mDocument->checkL1Compatibility(), strictUnits))
            {
              mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
              currentModel->convertL3ToL1();
              conversion = true;
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
            if (!conversion_errors(mDocument->checkL2v1Compatibility(), strictUnits))
            {
              doConversion = true;
            }
            break;
          case 2:
            if (!conversion_errors(mDocument->checkL2v2Compatibility(), strictUnits))
            {
              doConversion = true;
            }
            break;
          case 3:
            if (!conversion_errors(mDocument->checkL2v3Compatibility(), strictUnits))
            {
              doConversion = true;
            }
            break;
          case 4:
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
            mDocument->updateSBMLNamespace("core", targetLevel, targetVersion);
            currentModel->convertL3ToL2(strict);
            conversion = true;
          }
          break;
        case 3:
          switch (targetVersion)
          {
          case 1:
            conversion = true;
            break;
          default:
            mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
            break;
          }
          break;
        default:
          mDocument->getErrorLog()->logError(InvalidTargetLevelVersion, currentLevel, currentVersion);
          break;
        }

      }

      
    if (conversion == false)
    {
      /* if we were strict restore original model */

      if (strict)
      {
        delete origModel;
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
        mDocument->checkConsistency();
        unsigned int errors = 
           mDocument->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
        if (errors > 0)
        { /* error - we dont covert
           * restore original values and return
           */
          conversion = false;
          /* undo any changes */
          currentModel = origModel->clone();
          mDocument->updateSBMLNamespace("core", origLevel, origVersion);
          mDocument->setApplicableValidators(origValidators);
          delete origModel;
        }
        else
        {
          delete origModel;
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
  
/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
SBMLLevelVersionConverter::conversion_errors(unsigned int errors, bool strictUnits)
{
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
        mDocument->getErrorLog()->remove(NoNon3DComparmentsInL1);
        mDocument->getErrorLog()->remove(IntegerSpatialDimensions);
      }
    }
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
      return true;
    else
      return false;
  }
  else
  {
    return false;
  }

}

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
    std::list<SBMLError> fails = unit_validator.getFailures();
    std::list<SBMLError>::iterator iter;

    for (iter = fails.begin(); iter != fails.end(); iter++)
    {
      if ( iter->getErrorId() > UpperUnitBound)
      {
        errors--;
      }
    }
  }
    
  return (errors == 0);
}


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
    std::list<SBMLError> fails = sbo_validator.getFailures();
    std::list<SBMLError>::iterator iter;

    for (iter = fails.begin(); iter != fails.end(); iter++)
    {
      if ( iter->getErrorId() > InvalidDelaySBOTerm)
      {
        errors--;
      }
    }
  }
    
  return (errors == 0);

}



/** @cond doxygen-c-only */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


