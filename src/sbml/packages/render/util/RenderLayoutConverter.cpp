/**
 * @file    RenderLayoutConverter.cpp
 * @brief   Simple converter to convert Layout + Render from l2 to L3 and vice versa
 * @author  Frank T. Bergmann
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
 * ---------------------------------------------------------------------- -->*/


#include <sbml/packages/render/util/RenderLayoutConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/common/sbmlfwd.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/packages/render/common/RenderExtensionTypes.h>
#include <sbml/packages/layout/common/LayoutExtensionTypes.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
* SBML Converter stuff below
*/

void RenderLayoutConverter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new RenderLayoutConverter());
}
/** @endcond */


RenderLayoutConverter::RenderLayoutConverter() 
  : SBMLConverter("Layout Converter L2 <=> L3")
{

}


RenderLayoutConverter::RenderLayoutConverter(const RenderLayoutConverter& orig) 
  : SBMLConverter(orig)
{
}

SBMLConverter* 
RenderLayoutConverter::clone() const
{
  return new RenderLayoutConverter(*this);
}


/*
 * Destroy this object.
 */
RenderLayoutConverter::~RenderLayoutConverter ()
{
}


ConversionProperties
  RenderLayoutConverter::getDefaultProperties() const
{
    static ConversionProperties prop;
    prop.addOption("convert layout", true, "convert the layout to the given namespaces");
    return prop;
}


bool 
  RenderLayoutConverter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("convert layout"))
    return false;
  return true;
}

/** @cond doxygenLibsbmlInternal */
int 
RenderLayoutConverter::convertToL3()
  {
    layoutNsUri = "http://www.sbml.org/sbml/level3/version1/layout/version1";

    renderNsUri = "http://www.sbml.org/sbml/level3/version1/render/version1";

    LayoutModelPlugin* plugin = (LayoutModelPlugin*)mDocument->getModel()->getPlugin("layout");
    if (plugin == NULL) 
      return LIBSBML_OPERATION_FAILED;


    ConversionProperties prop(getTargetNamespaces());
    prop.addOption("strict", false);
    prop.addOption("setLevelAndVersion", true);
    prop.addOption("ignorePackages", true);

    int result = mDocument->convert(prop);
    if (result != LIBSBML_OPERATION_SUCCESS)
    {
      return result;
    }

    SBMLDocumentPlugin *docPlugin = (SBMLDocumentPlugin*)mDocument->getPlugin("layout");
    if (docPlugin != NULL)
      docPlugin->setElementNamespace(layoutNsUri);


    mDocument->getSBMLNamespaces()->addPackageNamespace("layout", 1);
    mDocument->setPackageRequired("layout", false);

    SBMLDocumentPlugin *rdocPlugin = (SBMLDocumentPlugin*)mDocument->getPlugin("render");
    if (rdocPlugin != NULL)
    {
      mDocument->getSBMLNamespaces()->addPackageNamespace("render", 1);    
    }
    else
    {
      if (mDocument->getModel()->getPlugin("render") != NULL)
        plugin->getLayout(0)->getPlugin("render")->setElementNamespace(renderNsUri);
      
      mDocument->enablePackage(renderNsUri, "render", true);      
    }
    mDocument->setPackageRequired("render", false);

    return LIBSBML_OPERATION_SUCCESS;
  }
/** @endcond */

/** @cond doxygenLibsbmlInternal */
int 
RenderLayoutConverter::convertToL2()
  {
    layoutNsUri = "http://projects.eml.org/bcb/sbml/level2";

    renderNsUri = "http://projects.eml.org/bcb/sbml/render/level2";

    LayoutModelPlugin* plugin = (LayoutModelPlugin*)mDocument->getModel()->getPlugin("layout");
    if (plugin == NULL) 
       return LIBSBML_OPERATION_FAILED;


    ConversionProperties prop(getTargetNamespaces());
    prop.addOption("strict", false);
    prop.addOption("setLevelAndVersion", true);
    prop.addOption("ignorePackages", true);

    int result = mDocument->convert(prop);
    if (result != LIBSBML_OPERATION_SUCCESS)
    {
      return result;
    }

    SBMLDocumentPlugin *docPlugin = (SBMLDocumentPlugin*)mDocument->getPlugin("layout");
    if (docPlugin != NULL)
      docPlugin->setElementNamespace(layoutNsUri);

    mDocument->getSBMLNamespaces()->removePackageNamespace(3, 1, "layout", 1);        
    mDocument->getSBMLNamespaces()->addPackageNamespace("layout", 1);        

    SBMLDocumentPlugin *rdocPlugin = (SBMLDocumentPlugin*)mDocument->getPlugin("render");
    if (rdocPlugin!= NULL)
      rdocPlugin->setElementNamespace(renderNsUri);
    mDocument->getSBMLNamespaces()->removePackageNamespace(3, 1, "render", 1);        
    mDocument->getSBMLNamespaces()->addPackageNamespace("render", 1);        

    return LIBSBML_OPERATION_SUCCESS;
  }
/** @endcond */

int 
RenderLayoutConverter::convert()
  {
    if (mDocument == NULL || mDocument->getModel() == NULL)
      return LIBSBML_OPERATION_FAILED;

    targetLevel = 2;
    targetVersion = 4;

    if (getTargetNamespaces() != NULL)
    {
      targetLevel = getTargetNamespaces()->getLevel();
      targetVersion = getTargetNamespaces()->getVersion();
    } 
    else
    {
      // investigate the document if we have an l3 model convert to L2
      if (mDocument->getLevel() < 3)
      {
        targetLevel = 3;
        targetVersion = 1;
      }

      mProps->setTargetNamespaces(new SBMLNamespaces(targetLevel, targetVersion));
    }

    if (targetLevel == 3)
      return convertToL3();
    else 
      return convertToL2();

  }

/** @cond doxygenCOnly */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


