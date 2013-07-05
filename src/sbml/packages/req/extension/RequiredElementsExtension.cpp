/**
 * @file    RequiredElementsExtension.cpp
 * @brief   Implementation of RequiredElementsExtension, the core module of requiredElements package.
 * @author  
 *
 * $Id: RequiredElementsExtension.cpp 10667 2010-01-16 10:20:44Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/requiredElements/extension/RequiredElementsExtension.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPluginNotRequired.h>

#include <sbml/packages/req/extension/RequiredElementsExtension.h>
#include <sbml/packages/req/extension/RequiredElementsSBasePlugin.h>

#ifdef __cplusplus

#include <iostream>

LIBSBML_CPP_NAMESPACE_BEGIN

// -------------------------------------------------------------------------
//
// This block is global initialization code which should be automatically 
// executed before invoking main() block.
//
// -------------------------------------------------------------------------

//------------- (START) -----------------------------------

// The name of this package

const std::string& RequiredElementsExtension::getPackageName ()
{
	static const std::string pkgName = "req";
	return pkgName;
}

//
// Default SBML level, version, and package version
//
unsigned int RequiredElementsExtension::getDefaultLevel()
{
	return 3;
}  

unsigned int RequiredElementsExtension::getDefaultVersion()
{
	return 1; 
}

unsigned int RequiredElementsExtension::getDefaultPackageVersion()
{
	return 1;
} 

//
// XML namespaces of (1) package versions of requiredElements extension, and 
// (2) another XML namespace(XMLSchema-instance) required in the requiredElements 
//  extension.
//

const std::string& RequiredElementsExtension::getXmlnsL3V1V1 ()
{
	static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/requiredElements/version1";
	return xmlns;
}

//
// Adds this RequiredElementsExtension object to the SBMLExtensionRegistry class.
// RequiredElementsExtension::init() function is automatically invoked when this
// object is instantiated.
//
static SBMLExtensionRegister<RequiredElementsExtension> requiredElementsExtensionRegistry;

/*
static
const char* SBML_REQELEMENTS_TYPECODE_STRINGS[] =
{
};
*/

//------------- (END) -----------------------------------

// --------------------------------------------------------
//
// Instantiate SBMLExtensionNamespaces<RequiredElementsExtension>
// (RequiredElementsPkgNamespaces) for DLL.
//
// --------------------------------------------------------

template class LIBSBML_EXTERN SBMLExtensionNamespaces<RequiredElementsExtension>;



RequiredElementsExtension::RequiredElementsExtension ()
{
}


/*
 * Copy constructor.
 */
RequiredElementsExtension::RequiredElementsExtension(const RequiredElementsExtension& orig)
: SBMLExtension(orig)
{
}


/*
 * Destroy this object.
 */
RequiredElementsExtension::~RequiredElementsExtension ()
{
}


/*
 * Assignment operator for RequiredElementsExtension.
 */
RequiredElementsExtension& 
RequiredElementsExtension::operator=(const RequiredElementsExtension& orig)
{
  SBMLExtension::operator=(orig);
  return *this;
}


/*
 * Creates and returns a deep copy of this RequiredElementsExtension object.
 * 
 * @return a (deep) copy of this SBase object
 */
RequiredElementsExtension* 
RequiredElementsExtension::clone () const
{
  return new RequiredElementsExtension(*this);  
}


const std::string&
RequiredElementsExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package corresponding to the combination of the given sbml level,
 * sbml version, and package version.
 * Empty string will be returned if no corresponding URI exists.
 *
 * @return a string of the package URI
 */
const std::string& 
RequiredElementsExtension::getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V1V1();
      }
    }
  }

  static std::string empty = "";

  return empty;
}


/*
 * Returns the SBML level with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int 
RequiredElementsExtension::getLevel(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }
  
  return 0;
}


/*
 * Returns the SBML version with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int 
RequiredElementsExtension::getVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns the package version with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int
RequiredElementsExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<class SBMLExtensionType> object 
 * (e.g. SBMLExtensionNamespaces<RequiredElementsExtension> whose alias type is 
 * RequiredElementsPkgNamespaces) corresponding to the given uri.
 * NULL will be returned if the given uri is not defined in the corresponding package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
SBMLNamespaces*
RequiredElementsExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  RequiredElementsPkgNamespaces* pkgns = 0;
  if ( uri == getXmlnsL3V1V1())
  {
    pkgns = new RequiredElementsPkgNamespaces(3,1,1);    
  }  
  return pkgns;
}


/*
 * This method takes a type code of requiredElements package and returns a string representing
 * the code.
 */
const char* 
RequiredElementsExtension::getStringFromTypeCode(int typeCode) const
{
  /* 
  int min = SBML_SPATIAL_DOMAINTYPE;
  int max = SBML_SPATIAL_GEOMETRY;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML RequiredElements Type)";  
  }

  return SBML_SPATIAL_TYPECODE_STRINGS[typeCode - min];
  */
  
  return "(Unknown SBML RequiredElements Type)";  
}


/*
 *
 * Initialization function of requiredElements extension module which is automatically invoked 
 * by SBMLExtensionRegister class before main() function invoked.
 *
 */
void 
RequiredElementsExtension::init()
{
  //-------------------------------------------------------------------------
  //
  // 1. Checks if the requiredElements pacakge has already been registered.
  //
  //-------------------------------------------------------------------------

  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    // do nothing;
    return;
  }

  //-------------------------------------------------------------------------
  //
  // 2. Creates an SBMLExtension derived object.
  //
  //-------------------------------------------------------------------------

  RequiredElementsExtension requiredElementsExtension;

  //-------------------------------------------------------------------------------------
  //
  // 3. Creates SBasePluginCreatorBase derived objects required for this 
  //    extension. The derived classes can be instantiated by using the following 
  //     template class.
  //
  //    temaplate<class SBasePluginType> class SBasePluginCreator
  //
  //    The constructor of the creator class has two arguments:
  //
  //        (1) SBaseExtensionPoint : extension point to which the plugin object connected
  //        (2) std::vector<std::string> : a std::vector object that contains a list of URI
  //                                       (package versions) supported by the plugin object.
  //
  //    For example, two plugin objects (plugged in SBMLDocument and Model elements) are 
  //    required for the requiredElements extension.
  //
  //    Since only 'required' attribute is used in SBMLDocument by the requiredElements package, existing
  //    SBMLDocumentPluginNotRequired class can be used as-is for the plugin.
  //
  //    Since the lists of supported package versions (currently only L3V1-requiredElements-V1 supported )
  //    are equal in the both plugin objects, the same vector object is given to each 
  //    constructor.
  //
  //---------------------------------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core",SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core",SBML_MODEL);
  SBaseExtensionPoint compExtPoint("core",SBML_COMPARTMENT);
  SBaseExtensionPoint compTypeExtPoint("core",SBML_COMPARTMENT_TYPE);
  SBaseExtensionPoint constraintExtPoint("core",SBML_CONSTRAINT);
  SBaseExtensionPoint eventExtPoint("core",SBML_EVENT);
  SBaseExtensionPoint eventAssgnExtPoint("core",SBML_EVENT_ASSIGNMENT);
  SBaseExtensionPoint funcDefnExtPoint("core",SBML_FUNCTION_DEFINITION);
  SBaseExtensionPoint initAssgnExtPoint("core",SBML_INITIAL_ASSIGNMENT);
  SBaseExtensionPoint kLawExtPoint("core",SBML_KINETIC_LAW);
  SBaseExtensionPoint listOfExtPoint("core",SBML_LIST_OF);
  SBaseExtensionPoint paramExtPoint("core",SBML_PARAMETER);
  SBaseExtensionPoint reactionExtPoint("core",SBML_REACTION);
  SBaseExtensionPoint ruleExtPoint("core",SBML_RULE);
  SBaseExtensionPoint speciesExtPoint("core",SBML_SPECIES);
  SBaseExtensionPoint spRefExtPoint("core",SBML_SPECIES_REFERENCE);
  SBaseExtensionPoint speciesTypeExtPoint("core",SBML_SPECIES_TYPE);
  SBaseExtensionPoint modSpRefExtPoint("core",SBML_MODIFIER_SPECIES_REFERENCE);
  SBaseExtensionPoint unitDefnExtPoint("core",SBML_UNIT_DEFINITION);
  SBaseExtensionPoint unitExtPoint("core",SBML_UNIT);
  SBaseExtensionPoint algebraicRuleExtPoint("core",SBML_ALGEBRAIC_RULE);
  SBaseExtensionPoint assgnRuleExtPoint("core",SBML_ASSIGNMENT_RULE);
  SBaseExtensionPoint rateRuleExtPoint("core",SBML_RATE_RULE);
  SBaseExtensionPoint spConcRuleExtPoint("core",SBML_SPECIES_CONCENTRATION_RULE);
  SBaseExtensionPoint compVolRuleExtPoint("core",SBML_COMPARTMENT_VOLUME_RULE);
  SBaseExtensionPoint paramRuleExtPoint("core",SBML_PARAMETER_RULE);
  SBaseExtensionPoint triggerExtPoint("core",SBML_TRIGGER);
  SBaseExtensionPoint delayExtPoint("core",SBML_DELAY);
  SBaseExtensionPoint stoichMathExtPoint("core",SBML_STOICHIOMETRY_MATH);
  SBaseExtensionPoint localParamExtPoint("core",SBML_LOCAL_PARAMETER);


  SBasePluginCreator<SBMLDocumentPluginNotRequired, RequiredElementsExtension> sbmldocPluginCreator(sbmldocExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> modelPluginCreator(modelExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> compPluginCreator(compExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> compTypePluginCreator(compTypeExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> constraintPluginCreator(constraintExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> eventPluginCreator(eventExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> eventAssgnPluginCreator(eventAssgnExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> funcDefnPluginCreator(funcDefnExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> initAssgnPluginCreator(initAssgnExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> kLawPluginCreator(kLawExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> listOfPluginCreator(listOfExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> paramPluginCreator(paramExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> reactionPluginCreator(reactionExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> rulePluginCreator(ruleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> speciesPluginCreator(speciesExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> spRefPluginCreator(spRefExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> speciesTypePluginCreator(speciesTypeExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> modeSpRefPluginCreator(modSpRefExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> unitDefnPluginCreator(unitDefnExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> unitPluginCreator(unitExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> algebraicRulePluginCreator(algebraicRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> assignRulePluginCreator(assgnRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> rateRulePluginCreator(rateRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> spConcRulePluginCreator(spConcRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> compVolRulePluginCreator(compVolRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> paramRulePluginCreator(paramRuleExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> triggerPluginCreator(triggerExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> delayPluginCreator(delayExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> stoichMathPluginCreator(stoichMathExtPoint,packageURIs);
  SBasePluginCreator<RequiredElementsSBasePlugin, RequiredElementsExtension> localParamPluginCreator(localParamExtPoint,packageURIs);

  //--------------------------------------------------------------------------------------
  //
  // 4. Adds the above SBasePluginCreatorBase derived objects to the SBMLExtension derived object.
  //
  //--------------------------------------------------------------------------------------

  requiredElementsExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&modelPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&compPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&compTypePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&constraintPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&eventPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&eventAssgnPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&funcDefnPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&initAssgnPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&kLawPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&listOfPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&paramPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&reactionPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&rulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&speciesPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&spRefPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&speciesTypePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&modeSpRefPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&unitDefnPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&unitPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&algebraicRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&assignRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&rateRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&spConcRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&compVolRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&paramRulePluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&triggerPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&delayPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&stoichMathPluginCreator);
  requiredElementsExtension.addSBasePluginCreator(&localParamPluginCreator);


  //-------------------------------------------------------------------------
  //
  // 5. Registers the SBMLExtension derived object to SBMLExtensionRegistry
  //
  //-------------------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&requiredElementsExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] RequiredElementsExtension::init() failed." << std::endl;
  }
}

#endif  /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END

