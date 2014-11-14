/**
* @file    TestSpatialExtension.cpp
* @brief   TestSpatialExtension unit tests
* @author  Akiya Jouraku
*
* $Id: $
* $HeadURL: $
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/common/extern.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygenIgnored */


  CK_CPPSTART

  static SpatialExtension* S = 0;
static SpatialPkgNamespaces* SNS;
static string SPATIAL_XMLNS_L3V1V1;
static string SPATIAL_PACKAGE_NAME;
static string CORE_XMLNS_L2V4;

void
  SpatialExtensionTest_setup (void)
{
  try
  {
    S = new SpatialExtension();
    SNS = new SpatialPkgNamespaces();
    SPATIAL_PACKAGE_NAME = S->getName();
    SPATIAL_XMLNS_L3V1V1 = SNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);

  }
  catch(...)
  {
    fail("Failed to create a SpatialExtension object");
  }
}


void
  SpatialExtensionTest_teardown (void)
{
  delete S;
  delete SNS;
}


START_TEST (test_SpatialExtension_getName)
{
  fail_unless(S->getName() == "spatial");
  fail_unless(S->getName() == SPATIAL_PACKAGE_NAME);
}
END_TEST


  START_TEST (test_SpatialExtension_getURI)
{
  fail_unless(S->getURI(3,1,1) == SPATIAL_XMLNS_L3V1V1);
  fail_unless(S->getURI(2,1,1) == "");
  fail_unless(S->getURI(4,1,1) == "");
}
END_TEST


  START_TEST (test_SpatialExtension_getLevelVersion)
{
  fail_unless(S->getLevel(SPATIAL_XMLNS_L3V1V1) == 3);
  fail_unless(S->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(S->getLevel("")                          == 0);

  fail_unless(S->getVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(S->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(S->getVersion("")                          == 0);

  fail_unless(S->getPackageVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(S->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(S->getPackageVersion("")                          == 0);
}
END_TEST


  START_TEST (test_SpatialExtension_getSBMLExtensionNamespaces)
{
  SpatialPkgNamespaces *spatialns;
  spatialns = static_cast<SpatialPkgNamespaces*>(S->getSBMLExtensionNamespaces(SPATIAL_XMLNS_L3V1V1));

  fail_unless(spatialns->getLevel()          == 3);
  fail_unless(spatialns->getVersion()        == 1);
  fail_unless(spatialns->getPackageVersion() == 1);

  delete spatialns;
  spatialns = static_cast<SpatialPkgNamespaces*>(S->getSBMLExtensionNamespaces(""));

  fail_unless(spatialns == NULL);
}
END_TEST


  START_TEST(test_SpatialExtension_copy)
{
  SpatialExtension *s2 = new SpatialExtension(*S);

  fail_unless(s2->getName() == "spatial");
  fail_unless(s2->getName() == SPATIAL_PACKAGE_NAME);

  fail_unless(s2->getURI(3,1,1) == SPATIAL_XMLNS_L3V1V1);
  fail_unless(s2->getURI(2,1,1) == "");
  fail_unless(s2->getURI(4,1,1) == "");

  fail_unless(s2->getLevel(SPATIAL_XMLNS_L3V1V1) == 3);
  fail_unless(s2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getLevel("")                          == 0);

  fail_unless(s2->getVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getVersion("")                          == 0);

  fail_unless(s2->getPackageVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getPackageVersion("")                          == 0);

  delete s2;
}
END_TEST


  START_TEST(test_SpatialExtension_assignment)
{
  SpatialExtension* s2 = new SpatialExtension();

  (*s2) = (*S);

  fail_unless(s2->getName() == "spatial");
  fail_unless(s2->getName() == SPATIAL_PACKAGE_NAME);

  fail_unless(s2->getURI(3,1,1) == SPATIAL_XMLNS_L3V1V1);
  fail_unless(s2->getURI(2,1,1) == "");
  fail_unless(s2->getURI(4,1,1) == "");

  fail_unless(s2->getLevel(SPATIAL_XMLNS_L3V1V1) == 3);
  fail_unless(s2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getLevel("")                          == 0);

  fail_unless(s2->getVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getVersion("")                          == 0);

  fail_unless(s2->getPackageVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getPackageVersion("")                          == 0);

  delete s2;
}
END_TEST


  START_TEST(test_SpatialExtension_clone)
{
  SpatialExtension* s2 = S->clone();

  fail_unless(s2->getName() == "spatial");
  fail_unless(s2->getName() == SpatialExtension::getPackageName());

  fail_unless(s2->getURI(3,1,1) == SPATIAL_XMLNS_L3V1V1);
  fail_unless(s2->getURI(2,1,1) == "");
  fail_unless(s2->getURI(4,1,1) == "");

  fail_unless(s2->getLevel(SPATIAL_XMLNS_L3V1V1) == 3);
  fail_unless(s2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getLevel("")                          == 0);

  fail_unless(s2->getVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getVersion("")                          == 0);

  fail_unless(s2->getPackageVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(s2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(s2->getPackageVersion("")                          == 0);

  delete s2;
}
END_TEST


  START_TEST(test_SpatialExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("spatial");

  fail_unless(sbext != 0);

  fail_unless(sbext->getName() == "spatial");
  fail_unless(sbext->getName() == SPATIAL_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == SPATIAL_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(SPATIAL_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(SPATIAL_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


  START_TEST(test_SpatialExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("spatial");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_DOMAINTYPE)             , "DomainType") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_DOMAIN)                 , "Domain") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_INTERIORPOINT)          , "InteriorPoint") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_COORDINATECOMPONENT)    , "CoordinateComponent") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_BOUNDARY)               , "Boundary") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_COMPARTMENTMAPPING)     , "CompartmentMapping") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_ADJACENTDOMAINS)        , "AdjacentDomains") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_GEOMETRYDEFINITION)     , "GeometryDefinition") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_SAMPLEDFIELDGEOMETRY)   , "SampledFieldGeometry") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_SAMPLEDFIELD)           , "SampledField") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_SAMPLEDVOLUME)          , "SampledVolume") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_ANALYTICGEOMETRY)       , "AnalyticGeometry") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_ANALYTICVOLUME)         , "AnalyticVolume") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_SPATIALPOINT)           , "SpatialPoint") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_SPATIALSYMBOLREFERENCE) , "SpatialSymbolReference") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_DIFFUSIONCOEFFICIENT)   , "DiffusionCoefficient") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_ADVECTIONCOEFFICIENT)   , "AdvectionCoefficient") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_BOUNDARYCONDITION)      , "BoundaryCondition") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_GEOMETRY)               , "Geometry") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_COORDINATEREFERENCE), "CoordinateReference") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_DOMAINTYPE-1)           , "(Unknown SBML Spatial Type)") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_SPATIAL_ORDINALMAPPING + 1), "(Unknown SBML Spatial Type)") == 0);

  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAINTYPE,   "spatial")            , "DomainType") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAIN,  "spatial")                 , "Domain") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_INTERIORPOINT,   "spatial")         , "InteriorPoint") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COORDINATECOMPONENT,  "spatial")    , "CoordinateComponent") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_BOUNDARY,   "spatial")              , "Boundary") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COMPARTMENTMAPPING,  "spatial")     , "CompartmentMapping") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ADJACENTDOMAINS,   "spatial")       , "AdjacentDomains") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_GEOMETRYDEFINITION,  "spatial")     , "GeometryDefinition") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDFIELDGEOMETRY,   "spatial")  , "SampledFieldGeometry") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDFIELD,  "spatial")           , "SampledField") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDVOLUME,  "spatial")          , "SampledVolume") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ANALYTICGEOMETRY,   "spatial")      , "AnalyticGeometry") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ANALYTICVOLUME,  "spatial")         , "AnalyticVolume") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SPATIALPOINT,   "spatial")          , "SpatialPoint") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SPATIALSYMBOLREFERENCE,  "spatial") , "SpatialSymbolReference") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DIFFUSIONCOEFFICIENT,   "spatial")  , "DiffusionCoefficient") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ADVECTIONCOEFFICIENT,  "spatial")   , "AdvectionCoefficient") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_BOUNDARYCONDITION,   "spatial")     , "BoundaryCondition") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_GEOMETRY,  "spatial")               , "Geometry") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COORDINATEREFERENCE, "spatial"), "CoordinateReference") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAINTYPE-1, "spatial")            , "(Unknown SBML Spatial Type)") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ORDINALMAPPING + 1, "spatial"), "(Unknown SBML Spatial Type)") == 0);

  delete sbext;
}
END_TEST

  START_TEST(test_SpatialExtension_SBMLtypecode)
{	
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAINTYPE            ,"spatial"), "DomainType")                  == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAIN                 ,"spatial"), "Domain")                      == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_INTERIORPOINT          ,"spatial"), "InteriorPoint")               == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COORDINATECOMPONENT    ,"spatial"), "CoordinateComponent")         == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_BOUNDARY               ,"spatial"), "Boundary")                    == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COMPARTMENTMAPPING     ,"spatial"), "CompartmentMapping")          == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ADJACENTDOMAINS        ,"spatial"), "AdjacentDomains")             == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_GEOMETRYDEFINITION     ,"spatial"), "GeometryDefinition")          == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDFIELDGEOMETRY   ,"spatial"), "SampledFieldGeometry")        == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDFIELD           ,"spatial"), "SampledField")                == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SAMPLEDVOLUME          ,"spatial"), "SampledVolume")               == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ANALYTICGEOMETRY       ,"spatial"), "AnalyticGeometry")            == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ANALYTICVOLUME         ,"spatial"), "AnalyticVolume")              == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SPATIALPOINT           ,"spatial"), "SpatialPoint")                == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_SPATIALSYMBOLREFERENCE ,"spatial"), "SpatialSymbolReference")      == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DIFFUSIONCOEFFICIENT   ,"spatial"), "DiffusionCoefficient")        == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ADVECTIONCOEFFICIENT   ,"spatial"), "AdvectionCoefficient")        == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_BOUNDARYCONDITION      ,"spatial"), "BoundaryCondition")           == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_GEOMETRY               ,"spatial"), "Geometry")                    == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_COORDINATEREFERENCE, "spatial"), "CoordinateReference") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_DOMAINTYPE - 1         ,"spatial"), "(Unknown SBML Spatial Type)") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_SPATIAL_ORDINALMAPPING + 1, "spatial"), "(Unknown SBML Spatial Type)") == 0);

}
END_TEST

  Suite *
  create_suite_SpatialExtension (void)
{
  Suite *suite = suite_create("SpatialExtension");
  TCase *tcase = tcase_create("SpatialExtension");

  tcase_add_checked_fixture(tcase, SpatialExtensionTest_setup, SpatialExtensionTest_teardown);

  tcase_add_test( tcase, test_SpatialExtension_getName         );
  tcase_add_test( tcase, test_SpatialExtension_getURI          );
  tcase_add_test( tcase, test_SpatialExtension_getLevelVersion );
  tcase_add_test( tcase, test_SpatialExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_SpatialExtension_copy            );
  tcase_add_test( tcase, test_SpatialExtension_assignment      );
  tcase_add_test( tcase, test_SpatialExtension_clone           );
  tcase_add_test( tcase, test_SpatialExtension_registry        );
  tcase_add_test( tcase, test_SpatialExtension_typecode        );
  tcase_add_test( tcase, test_SpatialExtension_SBMLtypecode    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
