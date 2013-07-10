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
  SBMLConverterRegistry::getInstance().addConverter(new CompFlatteningConverter());
}
/** @endcond */


CompFlatteningConverter::CompFlatteningConverter() : SBMLConverter()
{

}


CompFlatteningConverter::CompFlatteningConverter(const CompFlatteningConverter& orig) :
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
  return prop;
}


bool 
  CompFlatteningConverter::matchesProperties(const ConversionProperties &props) const
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
      basePathResolverIndex = SBMLResolverRegistry::getInstance().getNumResolvers();
      SBMLResolverRegistry::getInstance().addResolver(&basePathResolver);    
    }
  }  

  /* run the comp validation rules as flattening will fail
   * if there are bad or missing refernces between elements
   */

  mDocument->getErrorLog()->clearLog();
  unsigned char origValidators = mDocument->getApplicableValidators();
  mDocument->setApplicableValidators(AllChecksON);
  
  unsigned int errors = plugin->checkConsistency();
  errors = mDocument->getErrorLog()
                      ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  
  mDocument->setApplicableValidators(origValidators);

  if (errors > 0)
  {
    return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
  }

  CompModelPlugin *modelPlugin = (CompModelPlugin*)(mModel->getPlugin("comp"));

  Model* flatmodel = modelPlugin->flattenModel();
  
  if (basePathResolverIndex != -1)
  {
    // if we added a resolver remove it
    SBMLResolverRegistry::getInstance().removeResolver(basePathResolverIndex);
  }

  if (flatmodel == NULL) 
  {
    return LIBSBML_OPERATION_FAILED;
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

  for (unsigned int i = 0; i < errors; i++)
  {
    mDocument->getErrorLog()->add(*(d->getError(i)));
  }
  delete d;

  if (mDocument->getErrorLog()->contains(RequiredPackagePresent))
  {
    canFlatten = false;
    required = true;
    unrecognised = true;
  }
  
  if (mDocument->getErrorLog()->contains(UnrequiredPackagePresent))
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
  

  // clear the error log so we can add messages about flattening or not
  if (canFlatten == false)
  {
    if (unrecognised == true)
    {
      if (required == true)
      {
        //mDocument->getErrorLog()->logError(CompFlatteningNotRecognisedReqd, 3, 1);
      // log error: unrecognised required package
      }
    }
    else
    {
      // log error: required package but falttening not implemented
      //mDocument->getErrorLog()->logError(CompFlatteningNotImplementedReqd, 3, 1);
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
      // log error: unrecognised unrequired INFO lost
      //mDocument->getErrorLog()->logError(CompFlatteningNotRecognisedNotReqd, 3, 1);

      if (getIgnorePackages() == true)
      {
        XMLNamespaces *ns = mDocument->getSBMLNamespaces()->getNamespaces();
        for (int i = 0; i < ns->getLength(); i++)
        {
          if (mDocument->isIgnoredPackage(ns->getURI(i)) == true)
          {
            mDocument->enablePackageInternal(ns->getURI(i), 
                                             ns->getPrefix(i), false);
          }
        }
      }
    }
    if (notImplemented == true)
    {
      // log error: unrequire not implemented: INFO lost
      //mDocument->getErrorLog()->logError(CompFlatteningNotImplementedNotReqd, 3, 1);

      // disable any non required unimplemented package
      if (getIgnorePackages() == true)
      {
        for (unsigned int i = 0; i < mDocument->getNumPlugins(); i++)
        {
          if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                            ->isFlatteningImplemented() == false)
          {
            if (static_cast<SBMLDocumentPlugin*>(mDocument->getPlugin(i))
                                              ->getRequired() == false)
            {
              std::string pkgURI = mDocument->getPlugin(i)->getURI();
              std::string prefix = mDocument->getPlugin(i)->getPrefix();
              mDocument->disablePackage(pkgURI, prefix);
            }
          }
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


