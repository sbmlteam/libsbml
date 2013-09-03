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


/** @cond doxygenLibsbmlInternal */
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
  mDisabledPackages.clear();
  mPackageRequired.clear();
}


CompFlatteningConverter::CompFlatteningConverter
                         (const CompFlatteningConverter& orig) :
SBMLConverter(orig)
  , mDisabledPackages(orig.mDisabledPackages)
  , mPackageRequired (orig.mPackageRequired)
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
    restoreNamespaces();
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
    
    unsigned int errors = mDocument->checkConsistency(true);
    errors = mDocument->getErrorLog()
                        ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
    
    mDocument->setApplicableValidators(origValidators);

    if (errors > 0)
    {
      restoreNamespaces();
      return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    }
  }
  CompModelPlugin *modelPlugin = (CompModelPlugin*)(mModel->getPlugin("comp"));

  if (modelPlugin==NULL) 
  {
    restoreNamespaces();
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
    restoreNamespaces();
    return LIBSBML_OPERATION_FAILED;
  }

  // we havent failed flattening so remove tht error message
  mDocument->getErrorLog()->remove(CompModelFlatteningFailed);
  

  if (getPerformValidation() == true)
  {
    // Otherwise, transfer only errors 1090107->1090110
    // and 99107/99108to a 'dummy' document.
    SBMLErrorLog* log = mDocument->getErrorLog();
    SBMLDocument *dummy = new SBMLDocument(mDocument->getSBMLNamespaces());
    for (unsigned int en=0; en<log->getNumErrors(); en++) 
    {
      unsigned int errid = mDocument->getError(en)->getErrorId();
      if (errid == CompFlatteningNotRecognisedNotReqd ||
          errid == CompFlatteningNotRecognisedReqd ||
          errid == CompFlatteningNotImplementedNotReqd ||
          errid == CompFlatteningNotImplementedReqd ||
          errid == UnrequiredPackagePresent ||
          errid == RequiredPackagePresent) 
      {
            dummy->getErrorLog()->add(*(mDocument->getError(en)));
      }
    }

    log->clearLog();
  
    // create a dummyDocument that will mirror what the user options are 
    //Now check to see if the flat model is valid
    // run regular validation on the flattened document if requested.
    result = reconstructDocument(flatmodel, *(dummy), true );
    if (result != LIBSBML_OPERATION_SUCCESS)
    {
      restoreNamespaces();
      return result;
    }

    dummy->checkConsistency(true);
    unsigned int errors = 
             dummy->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
    if (errors > 0)
    {
      // we have serious errors so we are going to bail on the
      // flattening - log ONLY the errors
      //Transfer the errors to mDocument and don't reset the model.
      log->logPackageError("comp", CompLineNumbersUnreliable, 
        modelPlugin->getPackageVersion(), modelPlugin->getLevel(), 
        modelPlugin->getVersion());
      std::string message = "Errors that follow relate to the flattened ";
      message += "document produced using the CompFlatteningConverter.";
      log->logPackageError("comp", CompFlatModelNotValid,
        modelPlugin->getPackageVersion(), modelPlugin->getLevel(), 
        modelPlugin->getVersion(), message);
    
      unsigned int nerrors = dummy->getErrorLog()->getNumErrors();
      for (unsigned int n = 0; n < nerrors; n++)
      {
        const SBMLError* error = dummy->getError(n);
        if (error->getSeverity() >= LIBSBML_SEV_ERROR) 
        {
          log->add( *(error) );
        }
        if (error->getErrorId() >= CompFlatteningNotRecognisedNotReqd &&
          error->getErrorId() <= CompFlatteningNotImplementedReqd) 
        {
          log->add( *(error) );
        }
        else if (error->getErrorId() == UnrequiredPackagePresent ||
          error->getErrorId() == RequiredPackagePresent)
        {
          log->add( *(error) );
        }
      }
      delete dummy;
      restoreNamespaces();
      return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    }
    else
    {
      // put any warnings into the document that will be have the
      // flat model
      unsigned int nerrors = dummy->getErrorLog()->getNumErrors();
      for (unsigned int n = 0; n < nerrors; n++)
      {
        const SBMLError* error = dummy->getError(n);
        log->add( *(error) );
      }
      delete dummy;
    }
  }

  // now reconstruct the document to be returned 
  // taking user options into account
  result = reconstructDocument(flatmodel);


  if (result != LIBSBML_OPERATION_SUCCESS) 
  {
    restoreNamespaces();
    return result;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


int
CompFlatteningConverter::reconstructDocument(Model * flatmodel)
{
  SBMLDocument * tempDoc = NULL;
  return reconstructDocument(flatmodel, *(tempDoc));
}


int
CompFlatteningConverter::reconstructDocument(Model * flatmodel, 
                           SBMLDocument& dummyDoc,  bool dummyRecon)
{
  int result = LIBSBML_OPERATION_FAILED;

  // to ensure correct validation need to force the model to recalculate units
  if (flatmodel->isPopulatedListFormulaUnitsData() == true)
  {
    flatmodel->populateListFormulaUnitsData();
  }

  // now reconstruct the document taking user options into account
  if (getLeavePorts() == true)
  {
    if (getLeaveDefinitions() == false)
    {
      int i;
      CompSBMLDocumentPlugin *docPlug = NULL;
      if (dummyRecon == true)
      {
        docPlug = static_cast<CompSBMLDocumentPlugin *>
                              (dummyDoc.getPlugin("comp"));
      }
      else
      {
        docPlug = static_cast<CompSBMLDocumentPlugin *>
                              (mDocument->getPlugin("comp"));
      }

      for (i = docPlug->getNumModelDefinitions() - 1; i >= 0; i--)
      {
        docPlug->removeModelDefinition(i);
      }
      for (i = docPlug->getNumExternalModelDefinitions() - 1; i >= 0; i--)
      {
        docPlug->removeExternalModelDefinition(i);
      }

    }
    if (dummyRecon == true)
    {
      result = dummyDoc.setModel(flatmodel);
    }
    else
    {
      result = mDocument->setModel(flatmodel);
    }
  }
  else
  {
    if (getLeaveDefinitions() == false)
    {
      if (dummyRecon == true)
      {
        result = dummyDoc.setModel(flatmodel);
        dummyDoc.disablePackage(CompExtension::getXmlnsL3V1V1(), "comp");
      }
      else
      {
        result = mDocument->setModel(flatmodel);
        mDocument->disablePackage(CompExtension::getXmlnsL3V1V1(), "comp");
      }
    }
    else
    {
      flatmodel->disablePackage(CompExtension::getXmlnsL3V1V1(), "comp");
      if (dummyRecon == true)
      {
        result = dummyDoc.setModel(flatmodel);
        dummyDoc.enablePackage
                   (CompExtension::getXmlnsL3V1V1(), "comp", true);
     }
      else
      {
        result = mDocument->setModel(flatmodel);
        mDocument->enablePackage
                   (CompExtension::getXmlnsL3V1V1(), "comp", true);
      }
    }
  }

  return result;
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
CompFlatteningConverter::canBeFlattened()
{
  bool canFlatten = true;

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

  if (d->getErrorLog()->contains(RequiredPackagePresent) 
    && getIgnorePackages() == false) 
  {
    canFlatten = false;
  }
  
  // add messages about required/unrequired packages 
  // being present but not readable
  for (unsigned int i = 0; i < d->getErrorLog()->getNumErrors(); i++)
  {
    if (d->getError(i)->getErrorId() == RequiredPackagePresent 
      || d->getError(i)->getErrorId() == UnrequiredPackagePresent)
    {
      mDocument->getErrorLog()->add(*(d->getError(i)));
    }
  }
  delete d;

  if (canFlatten == true)
  {
    // check that any other packages found in the model CAN be flattened
    for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
    {
      if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                        ->isFlatteningImplemented() == false)
      {
        if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                          ->getRequired() == true)
        {
          if (!getIgnorePackages()) {
            canFlatten = false;
            break;
          }
        }
      }
    }
  }
  

  string postmessage = " cannot be flattened and ";
  postmessage += "the CompFlatteningConverter ";
  postmessage += "has the 'ignore packages' option set to ";
  if (getIgnorePackages() == false)
  {
    postmessage += "'false'.  Thus, flattening will not be attempted.";
  }
  else
  {
    postmessage += "'true'. Thus, information from this";
    postmessage += " package will not appear in the flattened model.";
  }

  XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
  for (int i = 0; i < ns->getLength(); i++)
  {
    if (mDocument->isIgnoredPackage(ns->getURI(i)) == true)
    {
      unsigned int warningnumber = CompFlatteningNotRecognisedNotReqd;
      std::string message = "The ";
      bool required = mDocument->getPackageRequired(ns->getURI(i));
      if (required) {
        message += "required ";
        warningnumber = CompFlatteningNotRecognisedReqd;
      }
      message += "package '" + ns->getPrefix(i) + "'";
      message += postmessage;
      if (getIgnorePackages())
      {
        //LS DEBUG:  This will change the original document even if 
        //flattening fails for some other reason (BUG)
        // This is fixed with restoreNamespaces
        string nsURI = ns->getURI(i);
        string nsPrefix = ns->getPrefix(i);
        mDocument->enablePackageInternal(nsURI, nsPrefix, false);
        mDisabledPackages.insert(make_pair(nsURI, nsPrefix));
        mPackageRequired.insert(make_pair(nsURI, required));
      }
      mDocument->getErrorLog()->logPackageError("comp", 
        warningnumber, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
  }
  for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
  {
    if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
      ->isFlatteningImplemented() == false)
    {
      unsigned int warningnumber = CompFlatteningNotImplementedNotReqd;
      std::string message = "The ";
      bool required = static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
        ->getRequired();
      if (required)
      {
        message += "required ";
        warningnumber = CompFlatteningNotImplementedReqd;
      }
      message += "package '" + mDocument->getPlugin(i)->getPackageName() + "'";
      message += postmessage;
      if (getIgnorePackages())
      {
        //LS DEBUG:  This will change the original document even if 
        //flattening fails for some other reason (BUG)
        // This is fixed with restoreNamespaces
        std::string pkgURI = mDocument->getPlugin(i)->getURI();
        std::string prefix = mDocument->getPlugin(i)->getPrefix();
        mDocument->disablePackage(pkgURI, prefix);
        mDisabledPackages.insert(make_pair(pkgURI, prefix));
        mPackageRequired.insert(make_pair(pkgURI, required));
      }

      mDocument->getErrorLog()->logPackageError("comp", 
        warningnumber,
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
  }

  return canFlatten;
}


void 
CompFlatteningConverter::restoreNamespaces()
{
  for (set<pair<string, string> >::iterator pkg = mDisabledPackages.begin();
       pkg != mDisabledPackages.end(); pkg++)
  {
    mDocument->enablePackageInternal((*pkg).first, (*pkg).second, true);
  }
  for (set<pair<string, bool> >::iterator pkg = mPackageRequired.begin();
       pkg != mPackageRequired.end(); pkg++)
  {
    mDocument->setPackageRequired((*pkg).first, (*pkg).second);
  }
}

/** @cond doxygenCOnly */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


