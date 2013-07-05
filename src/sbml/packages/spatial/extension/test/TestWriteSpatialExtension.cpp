/**
* @file    TestWriteSpatialExtension.cpp
* @brief   Unit tests of writing SpatialExtension 
* @author  Akiya Jouraku
*
* $Id: $
* $HeadURL: $
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include <sbml/packages/req/common/RequiredElementsExtensionTypes.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <string>

/** @cond doxygen-ignored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygen-ignored */

#define COMPARE_DOCUMENT_STRING(string1,string2)\
{\
  SBMLDocument *tempSBMLdoc = readSBMLFromString(string1);\
  char* tempStringXXXX = writeSBMLToString(tempSBMLdoc);\
  fail_unless(strcmp(tempStringXXXX,s2) == 0); \
  free (tempStringXXXX);\
  delete tempSBMLdoc;\
}




  CK_CPPSTART

  static string SPATIAL_XMLNS_L3V1V1;
static SpatialExtension* S; 
static SpatialPkgNamespaces* SNS;

void
  WriteSpatialExtensionTest_setup (void)
{
  try
  {
    S = new SpatialExtension();
    SNS = new SpatialPkgNamespaces();
    SPATIAL_XMLNS_L3V1V1 = SNS->getURI();
  }
  catch(...)
  {
    fail("Failed to create a SpatialExtension object");
  }
}


void
  WriteSpatialExtensionTest_teardown (void)
{
  delete S;
  delete SNS;
}


START_TEST (test_SpatialExtension_create_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"true\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:diffusionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:advectionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:boundaryCondition spatial:variable=\"ATPc\" spatial:coordinateBoundary=\"Xmin\" spatial:type=\"value\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:spatialSymbolReference spatial:spatialId=\"coordComp1\" spatial:type=\"coordinateComponent\"/>\n"
    "     </parameter>\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument* tempDoc = readSBMLFromString(s1);
  char* sbmlDoc = writeSBMLToString(tempDoc);


  // SBMLNamespaces of SBML Level 3 Version 1 with 'req' Version 1
  // then add 'spatial' package namespace.
  RequiredElementsPkgNamespaces sbmlns(3,1,1);
  sbmlns.addPkgNamespace("spatial",1);

  // create the L3V1 document with spatial package
  SBMLDocument *document = new SBMLDocument(&sbmlns);	

  // set 'required' attribute on document for 'spatial' and 'req' packages to 'true'

  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);
  document->setPackageRequired("spatial", true);
  document->setPackageRequired("req", true);
  fail_unless(document->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", true) == LIBSBML_OPERATION_SUCCESS);

  // create the Model
  Model* model=document->createModel();

  // create the Compartment
  Compartment* compartment = model->createCompartment();
  compartment->setId("cytosol");
  compartment->setConstant(true);

  // create the Species
  Species* species = model->createSpecies();
  species->setId("ATPc");
  species->setCompartment("cytosol");
  species->setInitialConcentration(1);
  species->setHasOnlySubstanceUnits(false);
  species->setBoundaryCondition(false);
  species->setConstant(false);
  // spatial package extension to species.
  // required elements package extention to parameter
  RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(species->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialSpeciesRxnPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // required elements package extention to diffusion parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // required elements package extention to advection parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // required elements package extention to boundary condition parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->getBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType("value") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->createSpecies();
  species->setId("ATPm");
  species->setCompartment("cytosol");
  species->setInitialConcentration(2);
  species->setHasOnlySubstanceUnits(false);
  species->setBoundaryCondition(false);
  species->setConstant(false);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->createReaction();
  reaction->setId("rxn1");
  reaction->setReversible(false);
  reaction->setFast(false);
  reaction->setCompartment("cytosol");
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

  //
  // Get a SpatialModelPlugin object plugged in the model object.
  //
  // The type of the returned value of SBase::getPlugin() function is 
  // SBasePlugin*, and thus the value needs to be casted for the 
  // corresponding derived class.
  //
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->getGeometry();
  fail_unless(geometry->setCoordinateSystem("XYZ") == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setSpatialId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setComponentType("cartesian") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setSbmlUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setIndex(1) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMin* minX = coordX->createBoundaryMin();
  fail_unless(minX->setSpatialId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMax* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setSpatialId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // required elements package extention to SpatialSymbolRef parameter
  // RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramX->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialId(coordX->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(spSymRef->setType(coordX->getElementName()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setSpatialId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(compartment->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->getCompartmentMapping();
  fail_unless(compMapping->setSpatialId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setCompartment(compartment->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setSpatialId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setSpatialId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setSpatialId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType("squareFn") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setSpatialId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = sfg->createSampledField();
  fail_unless(sampledField->setSpatialId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType("double") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setEncoding("encoding1") == LIBSBML_OPERATION_SUCCESS);
  //int samples[5] = {1, 2, 3, 4, 5};
  int samples[32] = {
    // z=0
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0,
    // z=1
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0
  };
  ImageData* id = sampledField->createImageData();
  fail_unless(id->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setSpatialId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(sbmlDoc,s2) == 0); 
  free(s2);

  // check clone()

  SBMLDocument* document2 = document->clone();
  s2 = writeSBMLToString(document2);
  fail_unless(strcmp(sbmlDoc,s2) == 0); 
  free(s2);

  // check operator=

  Model m(document->getSBMLNamespaces()); 
  m = *(document->getModel());
  document2->setModel(&m);
  s2 = writeSBMLToString(document2);

  fail_unless(strcmp(sbmlDoc,s2) == 0); 
  free(sbmlDoc);
  free(s2);
  delete document2;

  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_create_add_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\"  xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" level=\"3\" version=\"1\" spatial:required=\"true\" req:required=\"true\" >\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *tempDoc = readSBMLFromString(s1);
  char* sbmlDoc = writeSBMLToString(tempDoc);



  // SBMLNamespaces of SBML Level 3 Version 1 with 'req' Version 1
  // then add 'spatial' package namespace.
  SpatialPkgNamespaces sbmlns(3,1,1);
  sbmlns.addPkgNamespace("req",1);

  // create the L3V1 document with spatial package
  SBMLDocument *document = new SBMLDocument(&sbmlns);	

  // set 'required' attribute on document for 'spatial' and 'req' packages to 'true'

  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(document->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("spatial", true);
  document->setPackageRequired("req", true);

  // create the Model
  Model *model = new Model(&sbmlns);

  // create the Compartment
  Compartment *compartment = new Compartment(&sbmlns);
  compartment->setId("cytosol");
  compartment->setConstant(true);
  
  /*
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
  */
  SpatialCompartmentPlugin* compPlugin =  (SpatialCompartmentPlugin*)compartment->getPlugin("spatial");
  fail_unless(compPlugin != NULL);

  compPlugin->getCompartmentMapping()->setSpatialId("compMap1");
  compPlugin->getCompartmentMapping()->setCompartment("cytosol");
  compPlugin->getCompartmentMapping()->setDomainType("dtype1");
  compPlugin->getCompartmentMapping()->setUnitSize(1);

  RequiredElementsSBasePlugin* compReqPlugin = (RequiredElementsSBasePlugin*)compartment->getPlugin("req");
  fail_unless(compReqPlugin != NULL);

  compReqPlugin->setMathOverridden("spatial");
  compReqPlugin->setCoreHasAlternateMath("true");


  model->addCompartment(compartment);

  // create the Species
  Species *species1 = new Species(&sbmlns);
  species1->setId("ATPc");
  species1->setCompartment("cytosol");
  species1->setInitialConcentration(1);
  species1->setConstant(false);
  species1->setBoundaryCondition(false);
  species1->setHasOnlySubstanceUnits(false);
  // spatial package extension to species.
  // required elements package extention to parameter
  RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(species1->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialSpeciesRxnPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species1->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);
  model->addSpecies(species1);

  // add parameter for diff coeff of species
  Parameter *paramSp = new Parameter(&sbmlns);
  paramSp->setId(species1->getId()+"_dc");
  paramSp->setValue(1.0);
  // required elements package extention to diffusion parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species1->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);
  model->addParameter(paramSp);

  // add parameter for advection coeff of species
  paramSp = new Parameter(&sbmlns);
  paramSp->setId(species1->getId()+"_ac");
  paramSp->setValue(1.5);
  // required elements package extention to advection parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species1->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);
  model->addParameter(paramSp);

  // add parameter for boundary condition of species
  paramSp = new Parameter(&sbmlns);
  paramSp->setId(species1->getId()+"_bc");
  paramSp->setValue(2.0);
  // required elements package extention to boundary condition parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->getBoundaryCondition();
  fail_unless(boundCon->setVariable(species1->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType("value") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);
  model->addParameter(paramSp);

  Species *species2 = new Species(&sbmlns);
  species2->setId("ATPm");
  species2->setCompartment("cytosol");
  species2->setInitialConcentration(2);
  species2->setConstant(false);
  species2->setBoundaryCondition(false);
  species2->setHasOnlySubstanceUnits(false);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species2->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);
  model->addSpecies(species2);

  // add a reaction
  Reaction *reaction = new Reaction(&sbmlns);
  reaction->setId("rxn1");
  reaction->setReversible(false);
  reaction->setFast(false);
  reaction->setCompartment("cytosol");

  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);


  model->addReaction(reaction);

  //
  // Get a SpatialModelPlugin object plugged in the model object.
  //
  // The type of the returned value of SBase::getPlugin() function is 
  // SBasePlugin*, and thus the value needs to be casted for the 
  // corresponding derived class.
  //
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->getGeometry();
  fail_unless(geometry->setCoordinateSystem("XYZ") == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = new CoordinateComponent(&sbmlns);
  fail_unless(coordX->setSpatialId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setComponentType("cartesian") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setSbmlUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setIndex(1) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMin* minX = new BoundaryMin(&sbmlns);
  fail_unless(minX->setSpatialId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setBoundaryMin(minX) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMax* maxX = new BoundaryMax(&sbmlns);
  fail_unless(maxX->setSpatialId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setBoundaryMax(maxX) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addCoordinateComponent(coordX) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = new Parameter(&sbmlns);
  paramX->setId("x");
  paramX->setValue(8.0);
  // required elements package extention to SpatialSymbolRef parameter
  // RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramX->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialId(coordX->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(spSymRef->setType(coordX->getElementName()) == LIBSBML_OPERATION_SUCCESS);
  model->addParameter(paramX);

  DomainType* domainType = new DomainType(&sbmlns);
  fail_unless(domainType->setSpatialId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomainType(domainType) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(compartment->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->getCompartmentMapping();
  fail_unless(compMapping->setSpatialId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setCompartment(compartment->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain1 = new Domain(&sbmlns);
  fail_unless(domain1->setSpatialId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain1->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain1->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain1->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* interiorPt1 = new InteriorPoint(&sbmlns);
  fail_unless(interiorPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain1->addInteriorPoint(interiorPt1) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomain(domain1) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain2 = new Domain(&sbmlns);
  fail_unless(domain2->setSpatialId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain2->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain2->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain2->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = new InteriorPoint(&sbmlns);
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain2->addInteriorPoint(internalPt2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomain(domain2) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = new AdjacentDomains(&sbmlns);
  fail_unless(adjDomain->setSpatialId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addAdjacentDomains(adjDomain) == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = new AnalyticGeometry(&sbmlns);
  fail_unless(analyticGeom->setSpatialId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = new AnalyticVolume(&sbmlns);
  fail_unless(analyticVol->setSpatialId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType("squareFn") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticGeom->addAnalyticVolume(analyticVol) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addGeometryDefinition(analyticGeom) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = new SampledFieldGeometry(&sbmlns);
  fail_unless(sfg->setSpatialId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = new SampledField(&sbmlns);
  fail_unless(sampledField->setSpatialId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType("double") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setEncoding("encoding1") == LIBSBML_OPERATION_SUCCESS);
  //int samples[5] = {1, 2, 3, 4, 5};
  int samples[32] = {
    // z=0
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0,
    // z=1
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0
  };
  ImageData* id = new ImageData(&sbmlns);
  fail_unless(id->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setImageData(id)  == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sfg->setSampledField(sampledField)  == LIBSBML_OPERATION_SUCCESS);

  SampledVolume* sampledVol = new SampledVolume(&sbmlns);
  fail_unless(sampledVol->setSpatialId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sfg->addSampledVolume(sampledVol)  == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addGeometryDefinition(sfg) == LIBSBML_OPERATION_SUCCESS);

  // set the model to the document

  document->setModel(model);  

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(sbmlDoc ,s2) == 0); 
  free(sbmlDoc);
  free(s2);
}
END_TEST


  START_TEST (test_SpatialExtension_read_enable_via_model_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\"/>\n"
    "   </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"true\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:diffusionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:advectionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:boundaryCondition spatial:variable=\"ATPc\" spatial:coordinateBoundary=\"Xmin\" spatial:type=\"value\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:spatialSymbolReference spatial:spatialId=\"coordComp1\" spatial:type=\"coordinateComponent\"/>\n"
    "     </parameter>\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  //
  // enable the spatial package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", true) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("req", true);
  document->setPackageRequired("spatial", true);

  fail_unless(document->getNumPlugins() == 2);
  fail_unless(model->getNumPlugins()    == 2);

  // spatial package extension to species.
  // required elements package extention to parameter
  Species* species = model->getSpecies(0);
  RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(species->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialSpeciesRxnPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // required elements package extention to diffusion parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // required elements package extention to advection parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // required elements package extention to boundary condition parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->getBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType("value") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->getReaction(0);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

  // create a geometry object 
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->getGeometry();
  fail_unless(geometry->setCoordinateSystem("XYZ") == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setSpatialId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setComponentType("cartesian") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setSbmlUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setIndex(1) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMin* minX = coordX->createBoundaryMin();
  fail_unless(minX->setSpatialId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMax* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setSpatialId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // required elements package extention to SpatialSymbolRef parameter
  // RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramX->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialId(coordX->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(spSymRef->setType(coordX->getElementName()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setSpatialId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  Compartment* compartment = model->getCompartment(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(compartment->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->getCompartmentMapping();
  fail_unless(compMapping->setSpatialId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setCompartment(compartment->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setSpatialId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setSpatialId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setSpatialId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType("squareFn") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setSpatialId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = sfg->createSampledField();
  fail_unless(sampledField->setSpatialId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType("double") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setEncoding("encoding1") == LIBSBML_OPERATION_SUCCESS);
  //int samples[5] = {1, 2, 3, 4, 5};
  int samples[32] = {
    // z=0
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0,
    // z=1
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0
  };
  ImageData* id = sampledField->createImageData();
  fail_unless(id->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setSpatialId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  char *s2 = writeSBMLToString(document);

  SBMLDocument *tempDoc = readSBMLFromString(s1a);
  char* tempString = writeSBMLToString(tempDoc);

  fail_unless(strcmp(tempString ,s2) == 0); 

  free(s2);
  free(tempString);
  delete tempDoc;  
  delete document;
}
END_TEST


  START_TEST (test_SpatialExtension_read_disable_via_model_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"true\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:diffusionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:advectionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:boundaryCondition spatial:variable=\"ATPc\" spatial:coordinateBoundary=\"Xmin\" spatial:type=\"value\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:spatialSymbolReference spatial:spatialId=\"coordComp1\" spatial:type=\"coordinateComponent\"/>\n"
    "     </parameter>\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" />\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" />\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" />\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" />\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\"/>\n"
    "   </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 2);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 2);

  //
  // disable the spatial and requiredElements packages by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", false) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);
  fail_unless(model->getNumPlugins()    == 0);

  char *s2 = writeSBMLToString(document);

  SBMLDocument *tempDoc = readSBMLFromString(s1d);
  char* tempSBML = writeSBMLToString(tempDoc);

  fail_unless(strcmp(tempSBML ,s2) == 0); 
  free(tempSBML);
  free(s2);
  delete tempDoc;  
  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_read_enable_via_sbmldocument_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\"/>\n"
    "   </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"true\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:diffusionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:advectionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:boundaryCondition spatial:variable=\"ATPc\" spatial:coordinateBoundary=\"Xmin\" spatial:type=\"value\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:spatialSymbolReference spatial:spatialId=\"coordComp1\" spatial:type=\"coordinateComponent\"/>\n"
    "     </parameter>\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);

  //
  // enable the spatial and requiredElements packages by invoking enablePackage function with SBMLDocument object
  //
  fail_unless(document->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", true) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("req", true);
  document->setPackageRequired("spatial", true);

  fail_unless(document->getNumPlugins() == 2);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 2);

  // spatial package extension to species.
  // required elements package extention to parameter
  Species* species = model->getSpecies(0);
  RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(species->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialSpeciesRxnPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // required elements package extention to diffusion parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // required elements package extention to advection parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinateIndex(0) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // required elements package extention to boundary condition parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->getBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType("value") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->getReaction(0);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

  // create a geometry object 
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->getGeometry();
  fail_unless(geometry->setCoordinateSystem("XYZ") == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setSpatialId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setComponentType("cartesian") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setSbmlUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setIndex(1) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMin* minX = coordX->createBoundaryMin();
  fail_unless(minX->setSpatialId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  BoundaryMax* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setSpatialId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // required elements package extention to SpatialSymbolRef parameter
  // RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramX->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialId(coordX->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(spSymRef->setType(coordX->getElementName()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setSpatialId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  Compartment *compartment = model->getCompartment(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(compartment->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->setMathOverridden("spatial") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(reqplugin->setCoreHasAlternateMath(true) == LIBSBML_OPERATION_SUCCESS);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->getCompartmentMapping();
  fail_unless(compMapping->setSpatialId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setCompartment(compartment->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setSpatialId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setSpatialId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setSpatialId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setSpatialId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType("squareFn") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setSpatialId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = sfg->createSampledField();
  fail_unless(sampledField->setSpatialId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType("double") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setEncoding("encoding1") == LIBSBML_OPERATION_SUCCESS);
  //int samples[5] = {1, 2, 3, 4, 5};
  int samples[32] = {
    // z=0
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0,
    // z=1
    0,0,0,0,
    0,1,1,0,
    0,1,1,0,
    0,0,0,0
  };
  ImageData* id = sampledField->createImageData();
  fail_unless(id->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setSpatialId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getSpatialId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  char *s2 = writeSBMLToString(document);

  COMPARE_DOCUMENT_STRING(s1a, s2);

  free(s2);
  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_read_disable_via_sbmldocument_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"true\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" spatial:isSpatial=\"true\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:diffusionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:advectionCoefficient spatial:variable=\"ATPc\" spatial:coordinateIndex=\"0\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:boundaryCondition spatial:variable=\"ATPc\" spatial:coordinateBoundary=\"Xmin\" spatial:type=\"value\"/>\n"
    "     </parameter>\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:spatialSymbolReference spatial:spatialId=\"coordComp1\" spatial:type=\"coordinateComponent\"/>\n"
    "     </parameter>\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\" spatial:isLocal=\"true\"/>\n"
    "   </listOfReactions>\n"
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:listOfDomains>\n"
    "       <spatial:domain spatial:spatialId=\"domain1\" spatial:domainType=\"dtype1\" spatial:shapeId=\"circle\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"1\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "       <spatial:domain spatial:spatialId=\"domain2\" spatial:domainType=\"dtype1\" spatial:shapeId=\"square\" spatial:implicit=\"false\">\n"
    "         <spatial:listOfInteriorPoints>\n"
    "           <spatial:interiorPoint spatial:coord1=\"5\" spatial:coord2=\"0\" spatial:coord3=\"0\"/>\n"
    "         </spatial:listOfInteriorPoints>\n"
    "       </spatial:domain>\n"
    "     </spatial:listOfDomains>\n"
    "     <spatial:listOfAdjacentDomains>\n"
    "       <spatial:adjacentDomains spatial:spatialId=\"adjDomain1\" spatial:domain1=\"domain1\" spatial:domain2=\"domain2\"/>\n"
    "     </spatial:listOfAdjacentDomains>\n"
    "     <spatial:listOfGeometryDefinitions>\n"
    "       <spatial:analyticGeometry spatial:spatialId=\"analyticGeom1\">\n"
    "         <spatial:listOfAnalyticVolumes>\n"
    "           <spatial:analyticVolume spatial:spatialId=\"analyticVol1\" spatial:domainType=\"dtype1\" spatial:functionType=\"squareFn\" spatial:ordinal=\"1\">\n"
    "             <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "               <apply>\n"
    "                 <plus/>\n"
    "                   <apply>\n"
    "                     <times/>\n"
    "                       <ci> x </ci>\n"
    "                       <ci> x </ci>\n"
    "                   </apply>\n"
    "                   <apply>\n"
    "                     <minus/>\n"
    "                       <cn> 1 </cn>\n"
    "                   </apply>\n"
    "               </apply>\n"
    "             </math>\n"
    "           </spatial:analyticVolume>\n"
    "         </spatial:listOfAnalyticVolumes>\n"
    "       </spatial:analyticGeometry>\n"
    "       <spatial:sampledFieldGeometry spatial:spatialId=\"sampledFieldGeom1\">\n"
    "         <spatial:listOfSampledVolumes>\n"
    "           <spatial:sampledVolume spatial:spatialId=\"sv_1\" spatial:domainType=\"dtype1\" spatial:sampledValue=\"128\" spatial:minValue=\"0\" spatial:maxValue=\"255\"/>\n"
    "         </spatial:listOfSampledVolumes>\n"
    "         <spatial:sampledField spatial:spatialId=\"sampledField1\" spatial:dataType=\"double\" spatial:interpolationType=\"linear\" spatial:encoding=\"encoding1\" spatial:numSamples1=\"4\" spatial:numSamples2=\"4\" spatial:numSamples3=\"2\">\n"
    "           <spatial:imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </spatial:imageData>\n"
    "         </spatial:sampledField>\n"
    "       </spatial:sampledFieldGeometry>\n"
    "     </spatial:listOfGeometryDefinitions>\n"
    "   </spatial:geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "   </listOfSpecies>\n"
    "   <listOfParameters>\n"
    "     <parameter id=\"ATPc_dc\" value=\"1\" constant=\"true\" />\n"
    "     <parameter id=\"ATPc_ac\" value=\"1.5\" constant=\"true\" />\n"
    "     <parameter id=\"ATPc_bc\" value=\"2\" constant=\"true\" />\n"
    "     <parameter id=\"x\" value=\"8\" constant=\"true\" />\n"
    "   </listOfParameters>\n"
    "   <listOfReactions>\n"
    "     <reaction id=\"rxn1\" reversible=\"false\" fast=\"false\" compartment=\"cytosol\"/>\n"
    "   </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 2);

  //
  // disable the spatial and requiredElement packages by invoking enablePackage function with sbmlDocument object
  // 
  fail_unless(document->enablePackage(RequiredElementsExtension::getXmlnsL3V1V1(), "req", false) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  char *s2 = writeSBMLToString(document);

  COMPARE_DOCUMENT_STRING(s1d,s2);
  
  free(s2);
  delete document;  
}
END_TEST



  Suite *
  create_suite_WriteSpatialExtension (void)
{
  Suite *suite = suite_create("WriteSpatialExtension");
  TCase *tcase = tcase_create("WriteSpatialExtension");

  tcase_add_test( tcase, test_SpatialExtension_create_and_write_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_create_add_and_write_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_enable_via_model_and_write_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_disable_via_model_and_write_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_enable_via_sbmldocument_and_write_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_disable_via_sbmldocument_and_write_L3V1V1);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
