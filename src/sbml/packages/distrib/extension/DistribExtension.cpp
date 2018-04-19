/**
 * @file DistribExtension.cpp
 * @brief Implementation of DistribExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/extension/DistribSBMLDocumentPlugin.h>
#include <sbml/packages/distrib/validator/DistribSBMLErrorTable.h>
#include <sbml/packages/distrib/extension/DistribFunctionDefinitionPlugin.h>
#include <sbml/packages/distrib/extension/DistribSBasePlugin.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
DistribExtension::getPackageName()
{
  static const std::string pkgName = "distrib";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
DistribExtension::getDefaultLevel()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
DistribExtension::getDefaultVersion()
{
  return 2;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
DistribExtension::getDefaultPackageVersion()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
DistribExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/distrib/version1";
  return xmlns;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
DistribExtension::getXmlnsL3V2V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version2/distrib/version1";
  return xmlns;
}


/**
 *
 * Adds this DistribExtension to the SBMLExtensionRegistry class
 *
 */
static SBMLExtensionRegister<DistribExtension> distribExtensionRegistry;

static
const char* SBML_DISTRIB_TYPECODE_STRINGS[] =
{
    "DistribDrawFromDistribution"
  , "DistribInput"
  , "DistribDistribution"
  , "DistribUnivariateDistribution"
  , "DistribMultivariateDistribution"
  , "DistribContinuousUnivariateDistribution"
  , "DistribDiscreteUnivariateDistribution"
  , "DistribCategoricalUnivariateDistribution"
  , "DistribUncertValue"
  , "DistribUncertBound"
  , "DistribExternalDistribution"
  , "DistribExternalParameter"
  , "DistribNormalDistribution"
  , "DistribUniformDistribution"
  , "DistribCategoricalDistribution"
  , "DistribCategory"
  , "DistribBernoulliDistribution"
  , "DistribBetaDistribution"
  , "DistribBinomialDistribution"
  , "DistribCauchyDistribution"
  , "DistribChiSquareDistribution"
  , "DistribExponentialDistribution"
  , "DistribFDistribution"
  , "DistribGammaDistribution"
  , "DistribGeometricDistribution"
  , "DistribHypergeometricDistribution"
  , "DistribInverseGammaDistribution"
  , "DistribLaPlaceDistribution"
  , "DistribLogNormalDistribution"
  , "DistribLogisticDistribution"
  , "DistribNegativeBinomialDistribution"
  , "DistribParetoDistribution"
  , "DistribPoissonDistribution"
  , "DistribRayleighDistribution"
  , "DistribStudentTDistribution"
  , "DistribWeibullDistribution"
  , "DistribUncertainty"
  , "DistribUncertStatistics"
  , "DistribUncertStatisticSpan"
};


/**
 *
 * Instantiate SBMLExtensionNamespaces<DistribExtension> for DLL
 *
 */
template class LIBSBML_EXTERN SBMLExtensionNamespaces<DistribExtension>;

/*
 * Creates a new DistribExtension instance.
 */
DistribExtension::DistribExtension()
{
}


/*
 * Copy constructor for DistribExtension.
 */
DistribExtension::DistribExtension(const DistribExtension& orig)
  : SBMLExtension( orig )
{
}


/*
 * Assignment operator for DistribExtension.
 */
DistribExtension&
DistribExtension::operator=(const DistribExtension& rhs)
{
  if (&rhs != this)
  {
    SBMLExtension::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribExtension object.
 */
DistribExtension*
DistribExtension::clone() const
{
  return new DistribExtension(*this);
}


/*
 * Destructor for DistribExtension.
 */
DistribExtension::~DistribExtension()
{
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("distrib").
 */
const std::string&
DistribExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
 */
const std::string&
DistribExtension::getURI(unsigned int sbmlLevel,
                         unsigned int sbmlVersion,
                         unsigned int pkgVersion) const
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

  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 2)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V2V1();
      }
    }
  }

  static std::string empty = "";
  return empty;
}


/*
 * Returns the SBML Level for the given URI of this package.
 */
unsigned int
DistribExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }

  if (uri == getXmlnsL3V2V1())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the Version within the SBML Level for the given URI of this package.
 */
unsigned int
DistribExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  if (uri == getXmlnsL3V2V1())
  {
    return 2;
  }

  return 0;
}


/*
 * Returns the SBML Level&nbsp;3 package version for the given URI of this
 * package.
 */
unsigned int
DistribExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  if (uri == getXmlnsL3V2V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns a DistribPkgNamespaces object.
 */
SBMLNamespaces*
DistribExtension::getSBMLExtensionNamespaces(const std::string& uri) const
{
  DistribPkgNamespaces* pkgns = NULL;

  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new DistribPkgNamespaces(3, 1, 1);
  }

  if (uri == getXmlnsL3V2V1())
  {
    pkgns = new DistribPkgNamespaces(3, 2, 1);
  }

  return pkgns;
}


/*
 * Takes a type code of the &ldquo;distrib&rdquo; package and returns a string
 * describing the code.
 */
const char*
DistribExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_DISTRIB_DRAWFROMDISTRIBUTION;
  int max = SBML_DISTRIB_UNCERTSTATISTICSPAN;

  if (typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Distrib Type)";
  }

  return SBML_DISTRIB_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
DistribExtension::getErrorTable(unsigned int index) const
{
  return distribErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
DistribExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(distribErrorTable)/sizeof(distribErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == distribErrorTable[i].code)
    {
      index = i;
      break;
    }
  }

  return index;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the offset for the errorId range for the "distrib" package.
 */
unsigned int
DistribExtension::getErrorIdOffset() const
{
  return 1500000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns true if the package has multiple versions.
 */
bool
DistribExtension::hasMultiplePackageVersions() const
{
  return false;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes distrib extension by creating an object of this class with the
 * required SBasePlugin derived objects and registering the object to the
 * SBMLExtensionRegistry class
 */
void
DistribExtension::init()
{
  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    return;
  }

  DistribExtension distribExtension;


  std::vector<std::string> packageURIs;

  packageURIs.push_back(getXmlnsL3V1V1());

  packageURIs.push_back(getXmlnsL3V2V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint functiondefinitionExtPoint("core",
    SBML_FUNCTION_DEFINITION);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<DistribSBMLDocumentPlugin, DistribExtension>
    sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<DistribFunctionDefinitionPlugin, DistribExtension>
    functiondefinitionPluginCreator(functiondefinitionExtPoint, packageURIs);
  SBasePluginCreator<DistribSBasePlugin, DistribExtension>
    sbasePluginCreator(sbaseExtPoint, packageURIs);

  distribExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  distribExtension.addSBasePluginCreator(&functiondefinitionPluginCreator);
  distribExtension.addSBasePluginCreator(&sbasePluginCreator);

  SBMLExtensionRegistry::getInstance().addExtension(&distribExtension);
}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


