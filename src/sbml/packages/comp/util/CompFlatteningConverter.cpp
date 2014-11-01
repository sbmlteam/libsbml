/**
 * @file    CompFlatteningConverter.cpp
 * @brief   Implementation of a first flattening converter.
 * @author  Frank T. Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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
#include <sbml/SBMLDocument.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/comp/common/CompExtensionTypes.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/util/IdList.h>

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
  //'addConverter' adds a clone, not the original.
  CompFlatteningConverter cfc;
  SBMLConverterRegistry::getInstance().addConverter(&cfc);
}
/** @endcond */


CompFlatteningConverter::CompFlatteningConverter() 
  : SBMLConverter("SBML Comp Flattening Converter")
  , mPkgsToStrip (NULL)
{
  mDisabledPackages.clear();
}


CompFlatteningConverter::CompFlatteningConverter
                         (const CompFlatteningConverter& orig) :
SBMLConverter(orig)
  , mDisabledPackages(orig.mDisabledPackages)
  , mPkgsToStrip (orig.mPkgsToStrip)
{
}

CompFlatteningConverter* 
CompFlatteningConverter::clone() const
{
  return new CompFlatteningConverter(*this);
}


/*
 * Destroy this object.
 */
CompFlatteningConverter::~CompFlatteningConverter ()
{
  if (mPkgsToStrip != NULL)
  {
    mPkgsToStrip->clear();
    delete mPkgsToStrip;
    mPkgsToStrip = NULL;
  }
}


ConversionProperties
CompFlatteningConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("flatten comp", true, "flatten comp");
  prop.addOption("basePath", ".", 
    "the base directory in which to search for external references");
  prop.addOption("leavePorts", false, 
    "unused ports should be listed in the flattened model");
  prop.addOption("listModelDefinitions", false, 
    "all model definitions and external model definitions should remain in the SBMLDocument");
  prop.addOption("ignorePackages", true, 
    "any packages that cannot be flattened should be stripped (note: this option replaced by 'stripUnflattenablePackages').");
  prop.addOption("performValidation", true, 
    "perform validation before and after trying to flatten");
  prop.addOption("abortIfUnflattenable", "requiredOnly", 
    "specify whether to abort if any unflattenable packages are encountered");
  prop.addOption("stripUnflattenablePackages", true, 
    "specify whether to strip any unflattenable packages ignored by 'abortIfUnflattenable'");
  prop.addOption("stripPackages", "", 
    "comma separated list of packages to be stripped before flattening is attempted");
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

  // remember number of registered callbacks

  int numRegisteredCallbacks = Submodel::getNumProcessingCallbacks();

  int result = performConversion();

  if (basePathResolverIndex != -1)
  {
    // if we added a resolver remove it
    SBMLResolverRegistry::getInstance().removeResolver(basePathResolverIndex);
  }

  // remove all registered callbacks
  for (int index = Submodel::getNumProcessingCallbacks()-1; index >= numRegisteredCallbacks; --index)
  {
    Submodel::removeProcessingCallback(index);
  }
  
  // clean up PkgsToStrip
  if (mPkgsToStrip != NULL)
  {
    mPkgsToStrip->clear();
    delete mPkgsToStrip;
    mPkgsToStrip = NULL;
  }


  return result;

}

struct disable_info {
  SBMLDocument * doc;
  IdList strippedPkgs;
  std::set<std::pair<std::string, std::string> > disabledPkgs;
  bool stripUnflattenable;
  bool abortForRequiredOnly;
};

// simple callback enabling packages on main doc
int EnablePackageOnParentDocument(Model* m, SBMLErrorLog *, void* userdata)
{
  if (m == NULL) return LIBSBML_OPERATION_FAILED;

  // pull information out of userdata
  disable_info * info = static_cast<disable_info*>(userdata);
  
  SBMLDocument *mainDoc = static_cast<SBMLDocument*>(info->doc);
  std::set<std::pair<std::string, std::string> > disabled = 
    static_cast<std::set <std::pair <std::string, std::string> > >(info->disabledPkgs);


  if (mainDoc == NULL) return LIBSBML_OPERATION_FAILED;

  XMLNamespaces *mainNS = mainDoc->getSBMLNamespaces()->getNamespaces();

  XMLNamespaces *ns = m->getSBMLNamespaces()->getNamespaces();
  for (int i = 0; i < ns->getLength(); i++)
  {
    std::string nsURI = ns->getURI(i);
    std::string prefix = ns->getPrefix(i);
    if (prefix.empty() == true)
    {
      continue;
    }
    else if (mainNS->containsUri(nsURI) == false)
    {
      bool alreadyDisabled = false;
      for (set<pair<string, string> >::iterator pkg = disabled.begin();
           pkg != disabled.end(); pkg++)
      {
        if ((*pkg).first == nsURI)
        {
          alreadyDisabled = true;
          break;
        }
      }
      // just in case
      if (m->getSBMLDocument() == NULL)
      {
        continue;
      }
      if (m->isPackageEnabled(prefix) == true)
      {
        mainNS->add(nsURI, prefix);
        mainDoc->enablePackage(nsURI, prefix, true);
        mainDoc->setPackageRequired(prefix, 
          m->getSBMLDocument()->getPackageRequired(prefix));
        

        // we also need to make sure that if m was a modelDefinition
        // that we enable the package on its parent model
        Model * parent = dynamic_cast<Model*>(m->getAncestorOfType(SBML_MODEL));
        if (parent != NULL)
        {
          parent->enablePackageInternal(nsURI, prefix, true);
        }
      }
      else if (m->getSBMLDocument()->hasUnknownPackage(nsURI) == true)
      {
        // here we are dealing with an unknown package
        // need to decide whether to add the ns or not
        bool addNS = true;
        // if it was listed to be stripped do not add
        if (info->strippedPkgs.contains(prefix) == true) 
        {
          addNS = false;
        }
        // if it has already been disabled do not add
        else if (alreadyDisabled == true) 
        {
          addNS = false;
        }
        // if it is an unflattenable package and flags dicatate do not add
        else if (info->stripUnflattenable == true)
        {
          if (info->abortForRequiredOnly == false)
          {
            addNS = false;
          }
          else if (m->getSBMLDocument()->getPackageRequired(nsURI) == true)
          {
            addNS = false;
          }
        }

        if (addNS == true)
        {
          // we have an unknown package so we cannot enable it
          // but we do tell the parent doc about it
          mainNS->add(nsURI, prefix);
          mainDoc->addUnknownPackageRequired(nsURI, prefix,
            m->getSBMLDocument()->getPackageRequired(nsURI));
        }
      }
    }
  }

  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int 
CompFlatteningConverter::performConversion()
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

  // strip packages as instructed by user
  int success = stripPackages();
  if (success != LIBSBML_OPERATION_SUCCESS)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  // look at the document and work out the status of any remaining packages
  mPackageValues.clear();
  analyseDocument();

  bool canFlatten = canBeFlattened();
  

  if (canFlatten == false)
  {
    return LIBSBML_OPERATION_FAILED;
  }

 

  /* strip any unflattenable packages before we run validation */
  if (getStripUnflattenablePackages() == true)
  {
    stripUnflattenablePackages();
  }

  /* run the comp validation rules as flattening will fail
   * if there are bad or missing references between elements
   */

  if (getPerformValidation() == true)
  {
    int result = validateOriginalDocument();
    if (result != LIBSBML_OPERATION_SUCCESS) 
    {
      return result;
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

  // setup callback that will enable the packages on submodels
  disable_info mainDoc;
  mainDoc.doc = mDocument;
  mainDoc.strippedPkgs = getPackagesToStrip();
  mainDoc.disabledPkgs = mDisabledPackages;
  mainDoc.stripUnflattenable = getStripUnflattenablePackages();
  mainDoc.abortForRequiredOnly = getAbortForRequired(); 
 
  Submodel::addProcessingCallback(&EnablePackageOnParentDocument, &(mainDoc));
  Model* flatmodel = modelPlugin->flattenModel();
  

  if (flatmodel == NULL) 
  {
    //'flattenModel' sets its own error messages.
    restoreNamespaces();
    return LIBSBML_OPERATION_FAILED;
  }

  // we haven't failed flattening so remove that error message
  mDocument->getErrorLog()->remove(CompModelFlatteningFailed);
  

  if (getPerformValidation() == true)
  {
    int result = validateFlatDocument(flatmodel, 
                    modelPlugin->getPackageVersion(), modelPlugin->getLevel(), 
                    modelPlugin->getVersion());
    if (result != LIBSBML_OPERATION_SUCCESS)
    {
      delete flatmodel;
      return result;
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

  delete flatmodel;
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
int
CompFlatteningConverter::reconstructDocument(Model * flatmodel)
{
  SBMLDocument tempDoc;
  return reconstructDocument(flatmodel, tempDoc);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
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
  CompSBMLDocumentPlugin *docPlug = NULL;
  if (getLeavePorts() == true)
  {
    if (getLeaveDefinitions() == false)
    {
      int i;
      if (dummyRecon == true)
      {
        docPlug = static_cast<CompSBMLDocumentPlugin *>
                              (dummyDoc.getPlugin("comp"));
      // need to make sure comp required attribute is set
      // or validation of dummyDoc will fail
        docPlug->setRequired(true);
      }
      else
      {
        docPlug = static_cast<CompSBMLDocumentPlugin *>
                              (mDocument->getPlugin("comp"));
      }

      for (i = docPlug->getNumModelDefinitions() - 1; i >= 0; i--)
      {
        delete docPlug->removeModelDefinition(i);
      }
      for (i = docPlug->getNumExternalModelDefinitions() - 1; i >= 0; i--)
      {
        delete docPlug->removeExternalModelDefinition(i);
      }

    }
    if (dummyRecon == true)
    {
      // need to make sure comp required attribute is set
      // or validation of dummyDoc will fail
      docPlug = static_cast<CompSBMLDocumentPlugin *>
                            (dummyDoc.getPlugin("comp"));
      docPlug->setRequired(true);
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
      // need to make sure comp required attribute is set
      // or validation of dummyDoc will fail
        docPlug = static_cast<CompSBMLDocumentPlugin *>
                              (dummyDoc.getPlugin("comp"));
        docPlug->setRequired(true);
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
int
CompFlatteningConverter::validateOriginalDocument()
{
  CompSBMLDocumentPlugin *plugin = 
                  (CompSBMLDocumentPlugin*)(mDocument->getPlugin("comp"));

  unsigned char origValidators = mDocument->getApplicableValidators();
  mDocument->setApplicableValidators(AllChecksON);
  
  // set the flag to ignore flattening when validating
  bool originalOverrideFlag = plugin->getOverrideCompFlattening();
  plugin->setOverrideCompFlattening(true);
  
  // force a read
  std::string sbml = writeSBMLToStdString(mDocument);
  SBMLDocument *tempdoc = readSBMLFromString(sbml.c_str());
  for (unsigned int i = 0; i < tempdoc->getErrorLog()->getNumErrors(); i++)
  {
    // do not add the requried package present error
    const SBMLError * error = tempdoc->getErrorLog()->getError(i);
    if (error->getErrorId() != RequiredPackagePresent)
    {
      mDocument->getErrorLog()->add(*(error));
    }
  }
  delete tempdoc;

  mDocument->checkConsistency();

  unsigned int errors = mDocument->getErrorLog()
                      ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  
  // take out the error about a requiredpackage
  // if the user has specified to not abort for any packages
  // NOTE: we cannot actually remove it since the flattening code
  // uses it to check whether references might come from
  // other packages
  if (getAbortForNone() == true)
  {
    if (mDocument->getErrorLog()->contains(RequiredPackagePresent))
    {
      errors--;
    }
  }

  // reset comp flattening flag and any validator
  plugin->setOverrideCompFlattening(originalOverrideFlag);
  mDocument->setApplicableValidators(origValidators);

  if (errors > 0)
  {
    restoreNamespaces();
    return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
  }
  else
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
int
CompFlatteningConverter::validateFlatDocument(Model * flatmodel,
            unsigned int pkgVersion, unsigned int level, unsigned int version)
{
  int result;
  // create a dummyDocument that will mirror what the user options are 
   // we need the dummyDoc to know things about unknown packages
  // but dont want teh original model/error log
  SBMLDocument dummy = SBMLDocument(*(mDocument));
  dummy.setModel(NULL);
  dummy.getErrorLog()->clearLog();
   
  // Otherwise, transfer only errors 1090107->1090110
  SBMLErrorLog* log = mDocument->getErrorLog();
  for (unsigned int en=0; en<log->getNumErrors(); en++) 
  {
    unsigned int errid = mDocument->getError(en)->getErrorId();
    if (errid == CompFlatteningNotRecognisedNotReqd ||
        errid == CompFlatteningNotRecognisedReqd ||
        errid == CompFlatteningNotImplementedNotReqd ||
        errid == CompFlatteningNotImplementedReqd)
    {
          dummy.getErrorLog()->add(*(mDocument->getError(en)));
    }
  }

  log->clearLog();

  result = reconstructDocument(flatmodel, dummy, true );
  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    //delete flatmodel;
    restoreNamespaces();
    return result;
  }

 //Now check to see if the flat model is valid
  // run regular validation on the flattened document if requested.

  // override comp flattening if necessary
  CompSBMLDocumentPlugin * dummyPlugin = static_cast<CompSBMLDocumentPlugin*>
                                         (dummy.getPlugin("comp"));

  if (dummyPlugin != NULL)
  {
    dummyPlugin->setOverrideCompFlattening(true);
  }

  std::string sbml = writeSBMLToStdString(&dummy);
  SBMLDocument *tempdoc = readSBMLFromString(sbml.c_str());
  unsigned int errors = 
           tempdoc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  
  // take out the error about a requiredpackage
  // if the user has specified to not abort for any packages
  // NOTE: we cannot actually remove it since the flattening code
  // uses it to check whether references might come from
  // other packages
  if (getAbortForNone() == true)
  {
    if (tempdoc->getErrorLog()->contains(RequiredPackagePresent))
    {
      errors--;
    }
  }

  if (errors > 0)
  {
    // we have serious errors so we are going to bail on the
    // flattening - log ONLY the errors
    //Transfer the errors to mDocument and don't reset the model.
    if (log->contains(CompLineNumbersUnreliable) == false)
    {
      log->logPackageError("comp", CompLineNumbersUnreliable, 
        pkgVersion, level, version);
    }
    std::string message = "Errors that follow relate to the flattened ";
    message += "document produced using the CompFlatteningConverter.";
    log->logPackageError("comp", CompFlatModelNotValid,
        pkgVersion, level, version);
  
    unsigned int nerrors = tempdoc->getErrorLog()->getNumErrors();
    for (unsigned int n = 0; n < nerrors; n++)
    {
      const SBMLError* error = tempdoc->getError(n);
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
    //delete flatmodel;
    restoreNamespaces();
    delete tempdoc;
    return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
  }
  else
  {
    delete tempdoc;
  }

  dummy.checkConsistency();

  if (dummyPlugin != NULL)
  {
    dummyPlugin->setOverrideCompFlattening(false);
  }

  errors = dummy.getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  if (errors > 0)
  {
    // we have serious errors so we are going to bail on the
    // flattening - log ONLY the errors
    //Transfer the errors to mDocument and don't reset the model.
    if (log->contains(CompLineNumbersUnreliable) == false)
    {
      log->logPackageError("comp", CompLineNumbersUnreliable, 
        pkgVersion, level, version);
    }
    std::string message = "Errors that follow relate to the flattened ";
    message += "document produced using the CompFlatteningConverter.";
    log->logPackageError("comp", CompFlatModelNotValid,
        pkgVersion, level, version);
  
    unsigned int nerrors = dummy.getErrorLog()->getNumErrors();
    for (unsigned int n = 0; n < nerrors; n++)
    {
      const SBMLError* error = dummy.getError(n);
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
    //delete flatmodel;
    restoreNamespaces();
    return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
  }
  else
  {
    // put any warnings into the document that will be have the
    // flat model
    unsigned int nerrors = dummy.getErrorLog()->getNumErrors();
    for (unsigned int n = 0; n < nerrors; n++)
    {
      const SBMLError* error = dummy.getError(n);
      log->add( *(error) );
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::getStripUnflattenablePackages() const
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("stripUnflattenablePackages") == false)
  {
    if (getProperties()->hasOption("ignorePackages") == false)
    {
      return true;
    }
    else
    {
      return getProperties()->getBoolValue("ignorePackages");
    }
  }
  else
  {
    return getProperties()->getBoolValue("stripUnflattenablePackages");
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::getPerformValidation() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("performValidation") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("performValidation");
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::getAbortForAll() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("abortIfUnflattenable") == false)
  {
    return false;
  }
  else
  {
    if (getProperties()->getValue("abortIfUnflattenable") == "all")
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::getAbortForRequired() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("abortIfUnflattenable") == false)
  {
    return true;
  }
  else
  {
    if (getProperties()->getValue("abortIfUnflattenable") == "requiredOnly")
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::getAbortForNone() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("abortIfUnflattenable") == false)
  {
    return false;
  }
  else
  {
    if (getProperties()->getValue("abortIfUnflattenable") == "none")
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
const std::string& 
CompFlatteningConverter::getPackagesToStrip() const
{
	static std::string empty = "";

  if (getProperties() == NULL)
  {
    return empty;
  }
  else if (getProperties()->hasOption("stripPackages") == false)
  {
    return empty;
  }
  else
  {
    return getProperties()->getValue("stripPackages");
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
CompFlatteningConverter::stripUnflattenablePackages()
{
  XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
  for (int i = 0; i < ns->getLength(); i++)
  {
    std::string nsURI = ns->getURI(i);
    std::string package = ns->getPrefix(i);
    if (package.empty() == true)
    {
      continue;
    }

    bool flattenable = getFlattenableStatus(package);

    // if we can flatten we dont need to strip
    if (flattenable == true)
    {
      continue;
    }

    bool required = getRequiredStatus(package);
    bool known = getKnownStatus(package);

    std::string message = "The ";
    if (required == true)
    {
      message += "required ";
    }
    message += "package ";
    message += package;
    message += " has been stripped from the resulting flat model.";
    unsigned int errorId;
    if (required == true)
    {
      if (known == true)
      {
        errorId = CompFlatteningNotImplementedReqd;
      }
      else
      {
        errorId = CompFlatteningNotRecognisedReqd;
      }
    }
    else
    {
      if (known == true)
      {
        errorId = CompFlatteningNotImplementedNotReqd;
      }
      else
      {
        errorId = CompFlatteningNotRecognisedNotReqd;
      }
    }

    //need to look at abortIfFlattenable option
    // not if option is all and there are any unflattenable packages 
    // we will already have aborted
    if (getAbortForNone() == true)
    {
      // disable all
      mDocument->enablePackage(nsURI, package, false);
      mDisabledPackages.insert(make_pair(nsURI, package));
      mDocument->getErrorLog()->logPackageError("comp", errorId, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
    else if (getAbortForRequired() == true)
    {
      // disable unrequired packages only
      if (required == false)
      {
        mDocument->enablePackage(nsURI, package, false);
        mDisabledPackages.insert(make_pair(nsURI, package));
        mDocument->getErrorLog()->logPackageError("comp", errorId, 
          mDocument->getPlugin("comp")->getPackageVersion(), 
          mDocument->getLevel(), mDocument->getVersion(), message);
      }
    }

  }
}
/** @endcond */

// simple callback disabling packages on child documents
int DisablePackageOnChildDocuments(Model* m, SBMLErrorLog *, void* userdata)
{
  if (m == NULL) return LIBSBML_OPERATION_FAILED;

  IdList *pkgsToStrip = static_cast<IdList*>(userdata);

  XMLNamespaces *ns = m->getSBMLNamespaces()->getNamespaces();
  for (int i = 0; i < ns->getLength(); i++)
  {
    std::string nsURI = ns->getURI(i);
    std::string package = ns->getPrefix(i);
    if (package.empty() == true)
    {
      continue;
    }
    else if (pkgsToStrip->contains(package) == true)
    {
      m->enablePackageInternal(nsURI, package, false);
    }
  }

  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int
CompFlatteningConverter::stripPackages()
{
  mPkgsToStrip = new IdList(getPackagesToStrip());

  unsigned int num = mPkgsToStrip->size();

  if (num == 0)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
  for (int i = 0; i < ns->getLength(); i++)
  {
    std::string nsURI = ns->getURI(i);
    std::string package = ns->getPrefix(i);
    if (package.empty() == true)
    {
      continue;
    }
    else if (mPkgsToStrip->contains(package) == true)
    {
      mDocument->enablePackage(nsURI, package, false);
      mDisabledPackages.insert(make_pair(nsURI, package));
    }
  }

  unsigned int count = 0;
  for (unsigned int i = 0; i < num; i++)
  {
    if (mDocument->isPackageEnabled(mPkgsToStrip->at(i)) == false)
    {
      count++;
    }
  }

  // setup callback that will disable the packages on submodels
  Submodel::addProcessingCallback(&DisablePackageOnChildDocuments, mPkgsToStrip);

  if (num == count)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
CompFlatteningConverter::canBeFlattened()
{
  bool canFlatten = true;

  // so we bail if we have depending on what flags and
  // what packages we have

  if (getAbortForAll() == true)
  {
    std::string message = "The CompFlatteningConverter has the ";
    message += "'abortIfUnflattenable' option set to 'all' ";
    message += " and thus flattening will not be attempted.";
    
    // so user says abort for any unflattenable package
    if (haveUnknownRequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotRecognisedReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
    else if (haveUnknownUnrequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotRecognisedNotReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
    else if (haveUnflattenableRequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotImplementedReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
    else if (haveUnflattenableUnrequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotImplementedNotReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
  }
  else if (getAbortForRequired() == true)
  {
    std::string message = "The CompFlatteningConverter has the ";
    message += "'abortIfUnflattenable' option set to 'requiredOnly' ";
    message += " and thus flattening will not be attempted.";

    // only bail if we have a required package we cannot flatten
    if (haveUnknownRequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotRecognisedReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
    else if (haveUnflattenableRequiredPackages() == true)
    {
      canFlatten = false;
      mDocument->getErrorLog()->logPackageError("comp", 
        CompFlatteningNotImplementedReqd, 
        mDocument->getPlugin("comp")->getPackageVersion(), 
        mDocument->getLevel(), mDocument->getVersion(), message);
    }
  }

  return canFlatten;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
CompFlatteningConverter::restoreNamespaces()
{
  for (set<pair<string, string> >::iterator pkg = mDisabledPackages.begin();
       pkg != mDisabledPackages.end(); pkg++)
  {
    mDocument->enablePackage((*pkg).first, (*pkg).second, true);
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
CompFlatteningConverter::analyseDocument()
{
  XMLNamespaces * ns = mDocument->getSBMLNamespaces()->getNamespaces();

  bool required = true;
  bool known = true;
  bool flattenable = true;

  for (int i = 0; i < ns->getNumNamespaces(); i++)
  {
    std::string package = ns->getPrefix(i);
    std::string uri = ns->getURI(i);

    // exclude the code namespace
    if (uri == SBMLNamespaces::getSBMLNamespaceURI(mDocument->getLevel(), 
                                                   mDocument->getVersion()))
    {
      continue;
    }
     
    // required flag
    if (mDocument->getPackageRequired(uri) == true)
    {
      required= true;
    }
    else
    {
      required = false;
    }

   
    // known status
    if (mDocument->isPkgURIEnabled(uri) == true)
    {
      known = true;

      // flattenable status
      if (static_cast<SBMLDocumentPlugin*>
        (mDocument->getPlugin(uri))->isCompFlatteningImplemented() == true)
      {
        flattenable  = true;
      }
      else
      {
        flattenable = false;
      }
    }
    else
    {
      // if we dont know it we ceratinly cannot flatten it !
      known = false;
      flattenable = false;
    }

    // add the values into the map
    ValueSet values;
    values.push_back(required);
    values.push_back(known);
    values.push_back(flattenable);

    mPackageValues.insert(pair<const std::string, ValueSet>(package, values));
  }
}
/** @endcond */

 
/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::getRequiredStatus(const std::string & package)
{
  bool required = true;

  required = mPackageValues.find(package)->second.at(0);

  return required;
}
/** @endcond */

  
/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::getKnownStatus(const std::string& package)
{
  bool known = false;
  
  known = mPackageValues.find(package)->second.at(1);
  
  return known;
}
/** @endcond */

  
/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::getFlattenableStatus(const std::string& package)
{
  bool flattenable = false;

  flattenable = mPackageValues.find(package)->second.at(2);

  return flattenable;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::haveUnknownRequiredPackages()
{
  bool haveUnknownReqd = false;

  PackageValueIter iter;
  
  for (iter = mPackageValues.begin(); iter != mPackageValues.end(); iter++)
  {
    if ((*iter).second.at(0) == true && (*iter).second.at(1) == false)
    {
      haveUnknownReqd = true;
      break;
    }
  }

  return haveUnknownReqd;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::haveUnknownUnrequiredPackages()
{
  bool haveUnknownUnreqd = false;

  PackageValueIter iter;
  
  for (iter = mPackageValues.begin(); iter != mPackageValues.end(); iter++)
  {
    if ((*iter).second.at(0) == false && (*iter).second.at(1) == false)
    {
      haveUnknownUnreqd = true;
      break;
    }
  }

  return haveUnknownUnreqd;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::haveUnflattenableRequiredPackages()
{
  bool haveUnflatReqd = false;

  PackageValueIter iter;
  
  for (iter = mPackageValues.begin(); iter != mPackageValues.end(); iter++)
  {
    if ((*iter).second.at(0) == true && (*iter).second.at(2) == false)
    {
      haveUnflatReqd = true;
      break;
    }
  }

  return haveUnflatReqd;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool 
CompFlatteningConverter::haveUnflattenableUnrequiredPackages()
{
  bool haveUnflatUnreqd = false;

  PackageValueIter iter;
  
  for (iter = mPackageValues.begin(); iter != mPackageValues.end(); iter++)
  {
    if ((*iter).second.at(0) == false && (*iter).second.at(2) == false)
    {
      haveUnflatUnreqd = true;
      break;
    }
  }

  return haveUnflatUnreqd;
}
/** @endcond */



/** @cond doxygenIgnored */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


