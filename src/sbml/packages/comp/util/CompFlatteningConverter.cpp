/**
 * @file    CompFlatteningConverter.cpp
 * @brief   Implementation of a first flattening converter.
 * @author  Frank T. Bergmann
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
 * ---------------------------------------------------------------------- -->
 */



#include <sbml/packages/comp/util/CompFlatteningConverter.h>
#include <sbml/packages/comp/util/SBMLFileResolver.h>
#include <sbml/packages/comp/util/SBMLResolverRegistry.h>
#include <sbml/packages/comp/validator/CompSBMLError.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>

#include <sbml/common/sbmlfwd.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/comp/common/CompExtensionTypes.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygen-libsbml-internal */
/*
 * SBML Converter stuff below
 */

void CompFlatteningConverter::init()
{
  SBMLConverterRegistry::getInstance().addConverter
                                      (new CompFlatteningConverter());
}
/** @endcond */


CompFlatteningConverter::CompFlatteningConverter() : SBMLConverter()
{

}


CompFlatteningConverter::CompFlatteningConverter
                         (const CompFlatteningConverter& orig) :
SBMLConverter(orig)
{
}

SBMLConverter* 
  CompFlatteningConverter::clone() const
{
  return new CompFlatteningConverter(*this);
}


ConversionProperties
CompFlatteningConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("flatten comp", true, "flatten comp");
  prop.addOption("basePath", ".", 
    "the base directory to find external references in");
  prop.addOption("leavePorts", false, 
    "unused ports should be listed in the flattened model");
  prop.addOption("listModelDefinitions", false, 
    "the model definitions should be listed");
  prop.addOption("ignorePackages", true, 
    "any packages that cannot be flattened should be ignored");
  prop.addOption("perform validation", true, 
    "do not perform validation before trying to flatten");
  return prop;
}


bool 
CompFlatteningConverter::matchesProperties
                        (const ConversionProperties &props) const
{
  if (&props == NULL || !props.hasOption("flatten comp"))
    return false;
  return true;
}

int 
CompFlatteningConverter::convert()
{  
  int result = LIBSBML_OPERATION_FAILED;

  if (mDocument == NULL) 
  {
    return LIBSBML_INVALID_OBJECT;
  }

  Model* mModel = mDocument->getModel();
  if (mModel == NULL) 
  {
    return LIBSBML_INVALID_OBJECT;
  }

  CompSBMLDocumentPlugin *plugin = 
                  (CompSBMLDocumentPlugin*)(mDocument->getPlugin("comp"));

  // if we don't have a comp model we are done already
  if (plugin == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  bool canFlatten = canBeFlattened();
  

  if (canFlatten == false)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  // need to keep track so we can delete it later;
  int basePathResolverIndex = -1;

  // need to set the base path if we have the option
  if (getProperties() != NULL && getProperties()->hasOption("basePath"))
  {
    string basePath = getProperties()->getValue("basePath");
    if(basePath != ".")
    {
      // temporarily add a new resolver with the new basePath
      SBMLFileResolver basePathResolver;
      basePathResolver.addAdditionalDir(basePath);
      basePathResolverIndex = 
                   SBMLResolverRegistry::getInstance().getNumResolvers();
      SBMLResolverRegistry::getInstance().addResolver(&basePathResolver);    
    }
  }  

  /* run the comp validation rules as flattening will fail
   * if there are bad or missing refernces between elements
   */

  if (getPerformValidation() == true)
  {
    unsigned char origValidators = mDocument->getApplicableValidators();
    mDocument->setApplicableValidators(AllChecksON);
    
    unsigned int errors = plugin->checkConsistency(true);
    errors = mDocument->getErrorLog()
                        ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
    
    mDocument->setApplicableValidators(origValidators);

    if (errors > 0)
    {
      return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    }
  }
  CompModelPlugin *modelPlugin = (CompModelPlugin*)(mModel->getPlugin("comp"));

  if (modelPlugin==NULL) 
  {
    return LIBSBML_OPERATION_FAILED;
  }

  mDocument->getErrorLog()->logPackageError("comp", CompModelFlatteningFailed,
    modelPlugin->getPackageVersion(), mDocument->getLevel(), 
    mDocument->getVersion(),
    "The subsequent errors are from this attempt.");

  Model* flatmodel = modelPlugin->flattenModel();
  
  if (basePathResolverIndex != -1)
  {
    // if we added a resolver remove it
    SBMLResolverRegistry::getInstance().removeResolver(basePathResolverIndex);
  }

  if (flatmodel == NULL) 
  {
    //'flattenModel' sets its own error messages.
    return LIBSBML_OPERATION_FAILED;
  }
  
  //Otherwise, transfer only errors 1090107->1090110 to a 'dummy' document.
  SBMLErrorLog* log = mDocument->getErrorLog();
  SBMLDocument dummy(mDocument->getSBMLNamespaces());
  for (unsigned int en=0; en<log->getNumErrors(); en++) {
    unsigned int errid = mDocument->getError(en)->getErrorId();
    if (errid == CompFlatteningNotRecognisedNotReqd ||
        errid == CompFlatteningNotRecognisedReqd ||
        errid == CompFlatteningNotImplementedNotReqd ||
        errid == CompFlatteningNotImplementedReqd) {
          dummy.getErrorLog()->add(*(mDocument->getError(en)));
    }
  }
  log->clearLog();

  //Now check to see if the flat model is valid
  // run regular validation on the flattened document if requested.
  if (getPerformValidation() == true)
  {
    result = dummy.setModel(flatmodel);
    //LS DEBUG:  check 'result'?
    dummy.disablePackage(modelPlugin->getURI(), "comp");
    dummy.checkConsistency();
    unsigned int errors = dummy.getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
    if (errors > 0)
    {
      //Transfer the errors to mDocument and don't reset the model.
      log->logPackageError("comp", CompLineNumbersUnreliable, 
        modelPlugin->getPackageVersion(), modelPlugin->getLevel(), modelPlugin->getVersion());
      std::string message = "Errors that follow relate to the flattened ";
      message += "document produced using the CompFlatteningConverter.";
      log->logPackageError("comp", CompFlatModelNotValid,
        modelPlugin->getPackageVersion(), modelPlugin->getLevel(), modelPlugin->getVersion(), message);
    }
    unsigned int nerrors = dummy.getErrorLog()->getNumErrors();
    for (unsigned int n = 0; n < nerrors; n++)
    {
      const SBMLError* error = dummy.getError(n);
      if (error->getSeverity() >= LIBSBML_SEV_ERROR) {
        log->add( *(error) );
      }
      if (error->getErrorId() >= CompFlatteningNotRecognisedNotReqd &&
        error->getErrorId() <= CompFlatteningNotImplementedReqd) {
          log->add( *(error) );
      }
    }
    if (errors > 0) {
      return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    }
  }

  // now reconstruct the document taking user options into account

  if (getLeavePorts() == true)
  {
    if (getLeaveDefinitions() == false)
    {
      int i;
      CompSBMLDocumentPlugin *docPlug = 
        static_cast<CompSBMLDocumentPlugin *>(mDocument->getPlugin("comp"));

      for (i = docPlug->getNumModelDefinitions() - 1; i >= 0; i--)
      {
        docPlug->removeModelDefinition(i);
      }
      for (i = docPlug->getNumExternalModelDefinitions() - 1; i >= 0; i--)
      {
        docPlug->removeExternalModelDefinition(i);
      }

      result = mDocument->setModel(flatmodel);
      mDocument->setPackageRequired("comp", true);
    }
    else
    {
      result = mDocument->setModel(flatmodel);
      mDocument->setPackageRequired("comp", true);
    }
  }
  else
  {
    if (getLeaveDefinitions() == false)
    {
      result = mDocument->setModel(flatmodel);
      mDocument->disablePackage(CompExtension::getXmlnsL3V1V1(), "comp");
    }
    else
    {
      flatmodel->disablePackage(CompExtension::getXmlnsL3V1V1(), "comp");
      result = mDocument->setModel(flatmodel);
      mDocument->setPackageRequired("comp", true);
    }
  }

  if (result != LIBSBML_OPERATION_SUCCESS) {
    return result;
  }

  //The error log may contain random warnings about the original document that probably
  // will no longer apply, so clear it and re-run validation.
  if (getPerformValidation() == true)
  {
    mDocument->checkConsistency();
  }

  return LIBSBML_OPERATION_SUCCESS;
}


bool
CompFlatteningConverter::getLeavePorts() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("leavePorts") == false)
  {
    return false;
  }
  else
  {
    return getProperties()->getBoolValue("leavePorts");
  }
}

bool
CompFlatteningConverter::getLeaveDefinitions() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("listModelDefinitions") == false)
  {
    return false;
  }
  else
  {
    return getProperties()->getBoolValue("listModelDefinitions");
  }
}


bool
CompFlatteningConverter::getIgnorePackages() const
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("ignorePackages") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("ignorePackages");
  }
}


bool
CompFlatteningConverter::getPerformValidation() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("perform validation") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("perform validation");
  }
}

bool
CompFlatteningConverter::canBeFlattened() const
{
  bool canFlatten = true;
  bool required = false;
  bool unrecognised = false;
  bool notImplemented = false;

  // check for unrecognised packages
  mDocument->getErrorLog()->clearLog();

  /* hack to catch errors caught at read time */
  char* doc = writeSBMLToString(mDocument);
  SBMLDocument *d = readSBMLFromString(doc);
  util_free(doc);
  unsigned int errors = d->getNumErrors();

  //for (unsigned int i = 0; i < errors; i++)
  //{
  //  mDocument->getErrorLog()->add(*(d->getError(i)));
  //}

  if (d->getErrorLog()->contains(RequiredPackagePresent))
  {
    canFlatten = false;
    required = true;
    unrecognised = true;
  }
  
  if (d->getErrorLog()->contains(UnrequiredPackagePresent))
  {
    unrecognised = true;
  }

  if (canFlatten == true)
  {
    // check that any other packages found in the model CAN be flattened
    for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
    {
      if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                        ->isFlatteningImplemented() == false)
      {
        notImplemented = true;
        if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                          ->getRequired() == true)
        {
          required = true;
          canFlatten = false;
        }
      }
      if (canFlatten == false)
      {
        break;
      }
    }
  }
  

  // add messages about flattening or not
  for (unsigned int i = 0; i < d->getErrorLog()->getNumErrors(); i++)
  {
    if (d->getError(i)->getErrorId() == RequiredPackagePresent 
      || d->getError(i)->getErrorId() == UnrequiredPackagePresent)
    {
      mDocument->getErrorLog()->add(*(d->getError(i)));
    }
  }
  delete d;

  if (canFlatten == false)
  {
    if (unrecognised == true)
    {
      if (required == true)
      {
        XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
        for (int i = 0; i < ns->getLength(); i++)
        {
          if (mDocument->isIgnoredPackage(ns->getURI(i)) == true)
          {
            std::string message;
            std::string nameOfPackage = ns->getPrefix(i);
            if (getIgnorePackages() == true)
            {
              message = "The require package ";
              message += nameOfPackage;
              message += " cannot be flattened and ";
              message += "the CompFlatteningConverter ";
              message += "has the 'ignore packages' option set to 'true'. ";
              message += "Thus flattening will not be attempted.";
            }
            else
            {
              message = "The package ";
              message += nameOfPackage;
              message += " cannot be flattened and ";
              message += "the CompFlatteningConverter ";
              message += "has the 'ignore packages' option set to 'false'. ";
              message += "Thus information from the ";
              message += nameOfPackage;
              message += " package will not appear in the flattened model.";
            }
            mDocument->getErrorLog()->logPackageError("comp", 
              CompFlatteningNotRecognisedReqd, 
              mDocument->getPlugin("comp")->getPackageVersion(), 
              mDocument->getLevel(), mDocument->getVersion(), message);
          }
        }
      }
    }
    else
    {
      // log error: required package but falttening not implemented
      for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
      {
        if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                                   ->getRequired() == true
          && static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                       ->isFlatteningImplemented() == false)
        {
          std::string nameOfPackage = 
                      mDocument->getPlugin(i)->getPackageName();
          std::string message;
          if (getIgnorePackages() == true)
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'true'. ";
            message += "Thus flattening will not be attempted.";
          }
          else
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'false'. Thus ";
            message += "flattening will be attempted but information from the ";
            message += nameOfPackage;
            message += " in the flattened model may not be accurate.";
          }
          mDocument->getErrorLog()->logPackageError("comp", 
            CompFlatteningNotImplementedNotReqd, 
            mDocument->getPlugin("comp")->getPackageVersion(), 
            mDocument->getLevel(), mDocument->getVersion(), message);
        }
      }
    }
    if (getIgnorePackages() == false)
    {
      canFlatten = true;
    }
  }
  else
  {
    if (unrecognised == true)
    {
      XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
      for (int i = 0; i < ns->getLength(); i++)
      {
        if (mDocument->isIgnoredPackage(ns->getURI(i)) == true)
        {
          std::string message;
          std::string nameOfPackage = ns->getPrefix(i);
          if (getIgnorePackages() == true)
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'true'. ";
            message += "Thus all information from the ";
            message += nameOfPackage;
            message += " package will be removed from the flattened model.";
            mDocument->enablePackageInternal(ns->getURI(i), 
                                             ns->getPrefix(i), false);
          }
          else
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'false'. ";
            message += "Thus information from the ";
            message += nameOfPackage;
            message += " will not appear in the flattened model.";
          }
          mDocument->getErrorLog()->logPackageError("comp", 
            CompFlatteningNotRecognisedNotReqd, 
            mDocument->getPlugin("comp")->getPackageVersion(), 
            mDocument->getLevel(), mDocument->getVersion(), message);
        }
      }
    }
    if (notImplemented == true)
    {
      for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
      {
        if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                                   ->getRequired() == false
          && static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                       ->isFlatteningImplemented() == false)
        {
          std::string nameOfPackage = 
                      mDocument->getPlugin(i)->getPackageName();
          std::string message;
          if (getIgnorePackages() == true)
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'true'. ";
            message += "Thus all information from the ";
            message += nameOfPackage;
            message += " package will be removed from the flattened model.";

            std::string pkgURI = mDocument->getPlugin(i)->getURI();
            std::string prefix = mDocument->getPlugin(i)->getPrefix();
            mDocument->disablePackage(pkgURI, prefix);
          }
          else
          {
            message = "The package ";
            message += nameOfPackage;
            message += " cannot be flattened and the CompFlatteningConverter ";
            message += "has the 'ignore packages' option set to 'false'. ";
            message += "Thus information from the ";
            message += nameOfPackage;
            message += " in the flattened model may not be accurate.";
          }
          mDocument->getErrorLog()->logPackageError("comp", 
            CompFlatteningNotImplementedNotReqd, 
            mDocument->getPlugin("comp")->getPackageVersion(), 
            mDocument->getLevel(), mDocument->getVersion(), message);
        }
      }
    }
  }

  return canFlatten;
}

/** @cond doxygen-c-only */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


