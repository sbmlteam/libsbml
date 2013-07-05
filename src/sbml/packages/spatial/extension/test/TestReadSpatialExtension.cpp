/**
* @file    TestReadSpatialExtension.cpp
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
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/req/common/RequiredElementsExtensionTypes.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <string>

/** @cond doxygen-ignored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygen-ignored */


  CK_CPPSTART


  extern char *TestDataDirectory;

START_TEST (test_SpatialExtension_read_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/requiredElements/version1\" xmlns:spatial=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" level=\"3\" version=\"1\" req:required=\"false\" spatial:required=\"true\">\n"
    "  <model>\n"
    "   <listOfCompartments>\n"
    "     <compartment id=\"cytosol\" constant=\"true\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\">\n"
    "       <spatial:compartmentMapping spatial:spatialId=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
    "   </listOfCompartments>\n"
    "   <listOfSpecies>\n"
    "     <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
    "     <species id=\"ATPm\" compartment=\"cytosol\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" req:mathOverridden=\"spatial\" req:coreHasAlternateMath=\"true\" spatial:isSpatial=\"true\"/>\n"
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
    "     <parameter id=\"x2\" value=\"8\" constant=\"true\" />\n"
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
  char *sbmlDoc = writeSBMLToString(document);
  fail_unless(document->getPackageName() == "core");

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  //document->printErrors();
  //fail_unless(document->getNumErrors() == 0);

  // model : compartment
  fail_unless(model->getNumCompartments() == 1);

  Compartment *comp = model->getCompartment(0);
  RequiredElementsSBasePlugin* reqplugin = static_cast<RequiredElementsSBasePlugin*>(comp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  if (!reqplugin->getMathOverridden().empty()) {
    fail_unless(reqplugin->getMathOverridden()		== "spatial");
    fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  }

  // compartment : compartmentMapping
  SpatialCompartmentPlugin* cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
  fail_unless(cplugin != NULL);

  CompartmentMapping *cMapping = cplugin->getCompartmentMapping();
  if (cMapping->isSetSpatialId()) {
    fail_unless(cMapping->getSpatialId()		 == "compMap1");
    fail_unless(cMapping->getCompartment()	 == "cytosol");
    fail_unless(cMapping->getDomainType()	 == "dtype1");
    fail_unless(cMapping->getUnitSize()		 == 1);
  }

  // model : species 1
  fail_unless(model->getNumSpecies() == 2);

  Species *sp = model->getSpecies(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(sp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  if (!reqplugin->getMathOverridden().empty()) {
    fail_unless(reqplugin->getMathOverridden()		== "spatial");
    fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  }

  SpatialSpeciesRxnPlugin* srplugin = static_cast<SpatialSpeciesRxnPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : species 2
  sp = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : parameters (species diffusion, advection coeffs, species boundary conditions, coordinate components from Geometry
  fail_unless(model->getNumParameters() == 5);


  // non spatial parameters should have a spatial parameter plugin ... 
  Parameter* nonSpatial = model->getParameter("x2");
  fail_unless(nonSpatial != NULL);

  SpatialParameterPlugin* pluginNonSpatial = (SpatialParameterPlugin*)nonSpatial->getPlugin("spatial");
  fail_unless(pluginNonSpatial  != NULL);
  // no special type should be set
  fail_unless(pluginNonSpatial->getType()  == -1);
  fail_unless(pluginNonSpatial->isSpatialParameter()  == false);

  // parameter 0 : diffusionCoefficient
  Parameter *param = model->getParameter(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  SpatialParameterPlugin* pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_DIFFUSIONCOEFFICIENT);   
  DiffusionCoefficient *diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff != NULL);

  fail_unless(diffCoeff->getVariable()		== "ATPc");
  fail_unless(diffCoeff->getCoordinateIndex() == 0);

  // parameter 1 : advectionCoefficient
  param = model->getParameter(1);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_ADVECTIONCOEFFICIENT);
  AdvectionCoefficient *advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->getVariable()		== "ATPc");
  fail_unless(advCoeff->getCoordinateIndex() == 0);

  // parameter 2 : boundaryCondition X
  param = model->getParameter(2);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_BOUNDARYCONDITION);
  BoundaryCondition *bc = pplugin->getBoundaryCondition();
  fail_unless(bc->getVariable()		   == "ATPc");
  fail_unless(bc->getCoordinateBoundary() == "Xmin");
  fail_unless(bc->getType() == "value");

  // parameter 3 : SpatialSymbolReference (coordinateComponent from geometry)
  param = model->getParameter(3);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_SPATIALSYMBOLREFERENCE);
  SpatialSymbolReference *spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->getSpatialId() == "coordComp1");
  fail_unless(spSymRef->getType()		== "coordinateComponent");

  // model : reaction	
  fail_unless(model->getNumReactions() == 1);

  Reaction *rxn = model->getReaction(0);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(rxn->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsLocal() == true);

  // get the Geometry
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));
  fail_unless(mplugin != NULL);

  Geometry *geometry = mplugin->getGeometry();
  fail_unless(geometry != NULL);
  fail_unless(geometry->getPackageName()		== "spatial");
  fail_unless(geometry->getCoordinateSystem()	== "XYZ");

  // geometry coordinateComponent
  fail_unless(geometry->getNumCoordinateComponents() == 1);
  fail_unless(geometry->getListOfCoordinateComponents()->getPackageName() == "spatial");

  CoordinateComponent* coordComp = geometry->getCoordinateComponent(0);
  fail_unless(coordComp->getSpatialId()        == "coordComp1");
  fail_unless(coordComp->getComponentType()	== "cartesian");
  fail_unless(coordComp->getSbmlUnit()			== "umeter");
  fail_unless(coordComp->getIndex()			== 1);
  fail_unless(coordComp->getPackageName()		== "spatial");

  // boundaryMin and boundayMax within coordinateComponent
  BoundaryMin *minX = coordComp->getBoundaryMin();
  fail_unless(minX->getSpatialId()	  == "Xmin");
  fail_unless(minX->getValue()		  == 0);
  fail_unless(minX->getPackageName() == "spatial");

  BoundaryMax *maxX = coordComp->getBoundaryMax();
  fail_unless(maxX->getSpatialId()   == "Xmax");
  fail_unless(maxX->getValue()		  == 10);
  fail_unless(maxX->getPackageName() == "spatial");

  // geometry domainType
  fail_unless(geometry->getNumDomainTypes() == 1);
  fail_unless(geometry->getListOfDomainTypes()->getPackageName() == "spatial");

  DomainType *domainType = geometry->getDomainType(0);
  fail_unless(domainType->getSpatialId()         == "dtype1");
  fail_unless(domainType->getSpatialDimensions() == 3);
  fail_unless(domainType->getPackageName()		  == "spatial");

  // geometry domains
  fail_unless(geometry->getNumDomains() == 2);
  fail_unless(geometry->getListOfDomains()->getPackageName() == "spatial");

  Domain* domain = geometry->getDomain(0);
  fail_unless(domain->getSpatialId()   == "domain1");
  fail_unless(domain->getDomainType() == "dtype1");
  fail_unless(domain->getImplicit()    == false);
  fail_unless(domain->getShapeId()     == "circle");
  fail_unless(domain->getPackageName() == "spatial");

  // interiorPoints in Domain
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  InteriorPoint* interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 1);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // second domain in geometry
  domain = geometry->getDomain(1);
  fail_unless(domain->getSpatialId()   == "domain2");
  fail_unless(domain->getDomainType() == "dtype1");
  fail_unless(domain->getImplicit()    == false);
  fail_unless(domain->getShapeId()     == "square");
  fail_unless(domain->getPackageName() == "spatial");

  // Domain : interiorPoints
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 5);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // geometry adjacentDomains
  fail_unless(geometry->getNumAdjacentDomains() == 1);
  fail_unless(geometry->getListOfAdjacentDomains()->getPackageName() == "spatial");

  AdjacentDomains* adjDomain = geometry->getAdjacentDomains(0);
  fail_unless(adjDomain->getSpatialId()   == "adjDomain1");
  fail_unless(adjDomain->getDomain1()     == "domain1");
  fail_unless(adjDomain->getDomain2()     == "domain2");
  fail_unless(adjDomain->getPackageName() == "spatial");

  // geometry : geometryDefinitions
  fail_unless(geometry->getNumGeometryDefinitions() == 2);
  fail_unless(geometry->getListOfGeometryDefinitions()->getPackageName() == "spatial");

  GeometryDefinition *gd = geometry->getGeometryDefinition(0);
  AnalyticGeometry *analyticGeom = static_cast<AnalyticGeometry*>(gd);
  fail_unless(analyticGeom->getSpatialId()   == "analyticGeom1");
  fail_unless(analyticGeom->getPackageName() == "spatial");

  // AnalyticGeometry : analyticVolumes
  fail_unless(analyticGeom->getNumAnalyticVolumes() == 1);
  fail_unless(analyticGeom->getListOfAnalyticVolumes()->getPackageName() == "spatial");

  AnalyticVolume* av = analyticGeom->getAnalyticVolume(0);
  fail_unless(av->getSpatialId()    == "analyticVol1");
  fail_unless(av->getDomainType()   == "dtype1");
  fail_unless(av->getFunctionType() == "squareFn");
  fail_unless(av->getOrdinal()      == 1);
  fail_unless(av->getPackageName()  == "spatial");
  // ??????Math????

  // geometry : sampledFieldGeometry
  gd = geometry->getGeometryDefinition(1);
  SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
  fail_unless(sfGeom->getSpatialId()   == "sampledFieldGeom1");
  fail_unless(sfGeom->getPackageName() == "spatial");

  // sampledFieldGeometry : SampledVolumes
  fail_unless(sfGeom->getNumSampledVolumes() == 1);
  fail_unless(sfGeom->getListOfSampledVolumes()->getPackageName() == "spatial");

  SampledVolume* sv = sfGeom->getSampledVolume(0);
  fail_unless(sv->getSpatialId()    == "sv_1");
  fail_unless(sv->getDomainType()   == "dtype1");
  fail_unless(sv->getSampledValue() == 128);
  fail_unless(sv->getMinValue()     == 0);
  fail_unless(sv->getMaxValue()     == 255);
  fail_unless(sv->getPackageName()  == "spatial");

  // sampledFieldGeometry : SampledField
  SampledField* sf = sfGeom->getSampledField();
  fail_unless(sf->getSpatialId()		  == "sampledField1");
  fail_unless(sf->getDataType()		  == "double");
  fail_unless(sf->getInterpolationType() == "linear");
  fail_unless(sf->getEncoding()          == "encoding1");
  fail_unless(sf->getNumSamples1()       == 4);
  fail_unless(sf->getNumSamples2()       == 4);
  fail_unless(sf->getNumSamples3()       == 2);
  fail_unless(sf->getPackageName()		  == "spatial");

  // sampledField : ImageData
  const ImageData* id = sf->getImageData();
  fail_unless(id->getSamplesLength()  == 32);
  int* samples = new int[id->getSamplesLength()];
  id->getSamples(samples);
  fail_unless(samples[0] == 0);


  char* s2 = writeSBMLToString(document);

  fail_unless(strcmp(sbmlDoc,s2) == 0);

  free(sbmlDoc);
  free(s2);
  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_read_L3V1V1_defaultNS)
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
    "   <geometry xmlns=\"http://www.sbml.org/sbml/level3/version1/spatial/version1\" coordinateSystem=\"XYZ\">\n"
    "     <listOfCoordinateComponents>\n"
    "       <coordinateComponent spatialId=\"coordComp1\" componentType=\"cartesian\" sbmlUnit=\"umeter\" index=\"1\">\n"
    "         <boundaryMin spatialId=\"Xmin\" value=\"0\"/>\n"
    "         <boundaryMax spatialId=\"Xmax\" value=\"10\"/>\n"
    "       </coordinateComponent>\n"
    "     </listOfCoordinateComponents>\n"
    "     <listOfDomainTypes>\n"
    "       <domainType spatialId=\"dtype1\" spatialDimensions=\"3\"/>\n"
    "     </listOfDomainTypes>\n"
    "     <listOfDomains>\n"
    "       <domain spatialId=\"domain1\" domainType=\"dtype1\" shapeId=\"circle\" implicit=\"false\">\n"
    "         <listOfInteriorPoints>\n"
    "           <interiorPoint coord1=\"1\" coord2=\"0\" coord3=\"0\"/>\n"
    "         </listOfInteriorPoints>\n"
    "       </domain>\n"
    "       <domain spatialId=\"domain2\" domainType=\"dtype1\" shapeId=\"square\" implicit=\"false\">\n"
    "         <listOfInteriorPoints>\n"
    "           <interiorPoint coord1=\"5\" coord2=\"0\" coord3=\"0\"/>\n"
    "         </listOfInteriorPoints>\n"
    "       </domain>\n"
    "     </listOfDomains>\n"
    "     <listOfAdjacentDomains>\n"
    "       <adjacentDomains spatialId=\"adjDomain1\" domain1=\"domain1\" domain2=\"domain2\"/>\n"
    "     </listOfAdjacentDomains>\n"
    "     <listOfGeometryDefinitions>\n"
    "       <analyticGeometry spatialId=\"analyticGeom1\">\n"
    "         <listOfAnalyticVolumes>\n"
    "           <analyticVolume spatialId=\"analyticVol1\" domainType=\"dtype1\" functionType=\"squareFn\" ordinal=\"1\">\n"
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
    "           </analyticVolume>\n"
    "         </listOfAnalyticVolumes>\n"
    "       </analyticGeometry>\n"
    "       <sampledFieldGeometry spatialId=\"sampledFieldGeom1\">\n"
    "         <listOfSampledVolumes>\n"
    "           <sampledVolume spatialId=\"sv_1\" domainType=\"dtype1\" sampledValue=\"128\" minValue=\"0\" maxValue=\"255\"/>\n"
    "         </listOfSampledVolumes>\n"
    "         <sampledField spatialId=\"sampledField1\" dataType=\"double\" interpolationType=\"linear\" encoding=\"encoding1\" numSamples1=\"4\" numSamples2=\"4\" numSamples3=\"2\">\n"
    "           <imageData>0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0 </imageData>\n"
    "         </sampledField>\n"
    "       </sampledFieldGeometry>\n"
    "     </listOfGeometryDefinitions>\n"
    "   </geometry>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);
  char* sbmlDoc = writeSBMLToString(document);
  Model *model = document->getModel();

  //document->printErrors();

  fail_unless(model != NULL);
  //fail_unless(document->getNumErrors() == 0);

  // model : compartment
  fail_unless(model->getNumCompartments() == 1);

  Compartment *comp = model->getCompartment(0);
  RequiredElementsSBasePlugin* reqplugin = static_cast<RequiredElementsSBasePlugin*>(comp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  if (!reqplugin->getMathOverridden().empty()) {
    fail_unless(reqplugin->getMathOverridden()		== "spatial");
    fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  }

  // compartment : compartmentMapping
  SpatialCompartmentPlugin* cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
  fail_unless(cplugin != NULL);

  CompartmentMapping *cMapping = cplugin->getCompartmentMapping();
  if (cMapping->isSetSpatialId()) {
    fail_unless(cMapping->getSpatialId()		 == "compMap1");
    fail_unless(cMapping->getCompartment()	 == "cytosol");
    fail_unless(cMapping->getDomainType()	 == "dtype1");
    fail_unless(cMapping->getUnitSize()		 == 1);
  }

  // model : species 1
  fail_unless(model->getNumSpecies() == 2);

  Species *sp = model->getSpecies(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(sp->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  if (!reqplugin->getMathOverridden().empty()) {
    fail_unless(reqplugin->getMathOverridden()		== "spatial");
    fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  }

  SpatialSpeciesRxnPlugin* srplugin = static_cast<SpatialSpeciesRxnPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : species 2
  sp = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : parameters (species diffusion, advection coeffs, species boundary conditions, coordinate components from Geometry
  fail_unless(model->getNumParameters() == 4);



  // parameter 0 : diffusionCoefficient
  Parameter *param = model->getParameter(0);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  SpatialParameterPlugin* pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_DIFFUSIONCOEFFICIENT);
  DiffusionCoefficient *diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->getVariable()		== "ATPc");
  fail_unless(diffCoeff->getCoordinateIndex() == 0);

  // parameter 1 : advectionCoefficient
  param = model->getParameter(1);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_ADVECTIONCOEFFICIENT);
  AdvectionCoefficient *advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->getVariable()		== "ATPc");
  fail_unless(advCoeff->getCoordinateIndex() == 0);

  // parameter 2 : boundaryCondition X
  param = model->getParameter(2);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_BOUNDARYCONDITION);
  BoundaryCondition *bc = pplugin->getBoundaryCondition();
  fail_unless(bc->getVariable()		   == "ATPc");
  fail_unless(bc->getCoordinateBoundary() == "Xmin");
  fail_unless(bc->getType() == "value");

  // parameter 3 : SpatialSymbolReference (coordinateComponent from geometry)
  param = model->getParameter(3);
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
  fail_unless(reqplugin != NULL);
  fail_unless(reqplugin->getMathOverridden()		== "spatial");
  fail_unless(reqplugin->getCoreHasAlternateMath() == true);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference *spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->getSpatialId() == "coordComp1");
  fail_unless(spSymRef->getType()		== "coordinateComponent");

  // model : reaction	
  fail_unless(model->getNumReactions() == 1);

  Reaction *rxn = model->getReaction(0);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(rxn->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsLocal() == true);

  // get the Geometry
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));
  fail_unless(mplugin != NULL);

  Geometry *geometry = mplugin->getGeometry();
  fail_unless(geometry != NULL);
  fail_unless(geometry->getPackageName()		== "spatial");
  fail_unless(geometry->getCoordinateSystem()	== "XYZ");

  // geometry coordinateComponent
  fail_unless(geometry->getNumCoordinateComponents() == 1);
  fail_unless(geometry->getListOfCoordinateComponents()->getPackageName() == "spatial");

  CoordinateComponent* coordComp = geometry->getCoordinateComponent(0);
  fail_unless(coordComp->getSpatialId()        == "coordComp1");
  fail_unless(coordComp->getComponentType()	== "cartesian");
  fail_unless(coordComp->getSbmlUnit()			== "umeter");
  fail_unless(coordComp->getIndex()			== 1);
  fail_unless(coordComp->getPackageName()		== "spatial");

  // boundaryMin and boundayMax within coordinateComponent
  BoundaryMin *minX = coordComp->getBoundaryMin();
  fail_unless(minX->getSpatialId()	  == "Xmin");
  fail_unless(minX->getValue()		  == 0);
  fail_unless(minX->getPackageName() == "spatial");

  BoundaryMax *maxX = coordComp->getBoundaryMax();
  fail_unless(maxX->getSpatialId()   == "Xmax");
  fail_unless(maxX->getValue()		  == 10);
  fail_unless(maxX->getPackageName() == "spatial");

  // geometry domainType
  fail_unless(geometry->getNumDomainTypes() == 1);
  fail_unless(geometry->getListOfDomainTypes()->getPackageName() == "spatial");

  DomainType *domainType = geometry->getDomainType(0);
  fail_unless(domainType->getSpatialId()         == "dtype1");
  fail_unless(domainType->getSpatialDimensions() == 3);
  fail_unless(domainType->getPackageName()		  == "spatial");

  // geometry domains
  fail_unless(geometry->getNumDomains() == 2);
  fail_unless(geometry->getListOfDomains()->getPackageName() == "spatial");

  Domain* domain = geometry->getDomain(0);
  fail_unless(domain->getSpatialId()   == "domain1");
  fail_unless(domain->getDomainType() == "dtype1");
  fail_unless(domain->getImplicit()    == false);
  fail_unless(domain->getShapeId()     == "circle");
  fail_unless(domain->getPackageName() == "spatial");

  // interiorPoints in Domain
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  InteriorPoint* interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 1);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // second domain in geometry
  domain = geometry->getDomain(1);
  fail_unless(domain->getSpatialId()   == "domain2");
  fail_unless(domain->getDomainType() == "dtype1");
  fail_unless(domain->getImplicit()    == false);
  fail_unless(domain->getShapeId()     == "square");
  fail_unless(domain->getPackageName() == "spatial");

  // Domain : interiorPoints
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 5);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // geometry adjacentDomains
  fail_unless(geometry->getNumAdjacentDomains() == 1);
  fail_unless(geometry->getListOfAdjacentDomains()->getPackageName() == "spatial");

  AdjacentDomains* adjDomain = geometry->getAdjacentDomains(0);
  fail_unless(adjDomain->getSpatialId()   == "adjDomain1");
  fail_unless(adjDomain->getDomain1()     == "domain1");
  fail_unless(adjDomain->getDomain2()     == "domain2");
  fail_unless(adjDomain->getPackageName() == "spatial");

  // geometry : geometryDefinitions
  fail_unless(geometry->getNumGeometryDefinitions() == 2);
  fail_unless(geometry->getListOfGeometryDefinitions()->getPackageName() == "spatial");

  GeometryDefinition *gd = geometry->getGeometryDefinition(0);
  AnalyticGeometry *analyticGeom = static_cast<AnalyticGeometry*>(gd);
  fail_unless(analyticGeom->getSpatialId()   == "analyticGeom1");
  fail_unless(analyticGeom->getPackageName() == "spatial");

  // AnalyticGeometry : analyticVolumes
  fail_unless(analyticGeom->getNumAnalyticVolumes() == 1);
  fail_unless(analyticGeom->getListOfAnalyticVolumes()->getPackageName() == "spatial");

  AnalyticVolume* av = analyticGeom->getAnalyticVolume(0);
  fail_unless(av->getSpatialId()    == "analyticVol1");
  fail_unless(av->getDomainType()   == "dtype1");
  fail_unless(av->getFunctionType() == "squareFn");
  fail_unless(av->getOrdinal()      == 1);
  fail_unless(av->getPackageName()  == "spatial");
  // ??????Math????

  // geometry : sampledFieldGeometry
  gd = geometry->getGeometryDefinition(1);
  SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
  fail_unless(sfGeom->getSpatialId()   == "sampledFieldGeom1");
  fail_unless(sfGeom->getPackageName() == "spatial");

  // sampledFieldGeometry : SampledVolumes
  fail_unless(sfGeom->getNumSampledVolumes() == 1);
  fail_unless(sfGeom->getListOfSampledVolumes()->getPackageName() == "spatial");

  SampledVolume* sv = sfGeom->getSampledVolume(0);
  fail_unless(sv->getSpatialId()    == "sv_1");
  fail_unless(sv->getDomainType()   == "dtype1");
  fail_unless(sv->getSampledValue() == 128);
  fail_unless(sv->getMinValue()     == 0);
  fail_unless(sv->getMaxValue()     == 255);
  fail_unless(sv->getPackageName()  == "spatial");

  // sampledFieldGeometry : SampledField
  SampledField* sf = sfGeom->getSampledField();
  fail_unless(sf->getSpatialId()		  == "sampledField1");
  fail_unless(sf->getDataType()		  == "double");
  fail_unless(sf->getInterpolationType() == "linear");
  fail_unless(sf->getEncoding()          == "encoding1");
  fail_unless(sf->getNumSamples1()       == 4);
  fail_unless(sf->getNumSamples2()       == 4);
  fail_unless(sf->getNumSamples3()       == 2);
  fail_unless(sf->getPackageName()		  == "spatial");

  // sampledField : ImageData
  const ImageData* id = sf->getImageData();
  fail_unless(id->getSamplesLength()  == 32);
  int* samples = new int[id->getSamplesLength()];
  id->getSamples(samples);
  fail_unless(samples[0] == 0);

  char* s2 = writeSBMLToString(document);

  fail_unless(strcmp(sbmlDoc,s2) == 0);

  free(s2);
  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_read_L3V1V1_unknown_elements)
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
    "   <spatial:geometry spatial:coordinateSystem=\"XYZ\" spatial:unknown=\"unknown\">\n"
    "     <spatial:listOfCoordinateComponents>\n"
    "       <spatial:coordinateComponent spatial:spatialId=\"coordComp1\" spatial:componentType=\"cartesian\" spatial:sbmlUnit=\"umeter\" spatial:index=\"1\">\n"
    "         <spatial:boundaryMin spatial:spatialId=\"Xmin\" spatial:value=\"0\"/>\n"
    "         <spatial:boundaryMax spatial:spatialId=\"Xmax\" spatial:value=\"10\"/>\n"
    "       </spatial:coordinateComponent>\n"
    "     </spatial:listOfCoordinateComponents>\n"
    "     <spatial:listOfDomainTypes>\n"
    "       <spatial:domainType spatial:spatialId=\"dtype1\" spatial:spatialDimensions=\"3\" spatial:unknown=\"unknown\"/>\n"
    "     </spatial:listOfDomainTypes>\n"
    "     <spatial:unknown>\n"
    "     </spatial:unknown>\n"
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
  Model *model = document->getModel();

  //   document->printErrors();

  fail_unless(model != NULL);
  //fail_unless(document->getNumErrors() == 3);

  delete document;
}
END_TEST

#ifdef USE_ZLIB
  START_TEST (test_SpatialExtension_read_compressed)
{
  string filename(TestDataDirectory);
  filename += "ImageTest3.xml";
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  fail_unless(doc->getModel() != NULL);
  SpatialModelPlugin* plugin = (SpatialModelPlugin*)doc->getModel()->getPlugin("spatial");
  fail_unless(plugin != NULL);
  fail_unless(plugin->getGeometry() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions()->size() == 1);
  SampledFieldGeometry* geometry = (SampledFieldGeometry*)plugin->getGeometry()->getListOfGeometryDefinitions()->get(0);
  fail_unless(geometry != NULL);
  SampledField* field = geometry->getSampledField();
  fail_unless(field->getNumSamples1() == 57);
  fail_unless(field->getNumSamples2() == 63);
  const ImageData *data = field->getImageData();
  fail_unless(data != NULL);
  fail_unless(data->getDataType() == "compressed");
  
  int* result; int resultLength;
  data->getUncompressedData(result, resultLength);
  
  fail_unless(resultLength == 3591);

  stringstream builder;
  for (int i =0 ; i < resultLength; ++i)
  {
    builder<< result[i] << " ";
    if ((i+1)%field->getNumSamples1() == 0) builder << "\n";
  }
  
  string resultString = builder.str();

  string expected(
    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 2 2 2 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 2 2 2 1 1 1 1 1 1 1 0 \n"
    "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 \n"
    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \n"
    );

  fail_unless(resultString == expected);

}
END_TEST
#endif

  Suite *
  create_suite_ReadSpatialExtension (void)
{
  Suite *suite = suite_create("ReadSpatialExtension");
  TCase *tcase = tcase_create("ReadSpatialExtension");

  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1_defaultNS);
  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1_unknown_elements);
#ifdef USE_ZLIB
  tcase_add_test( tcase, test_SpatialExtension_read_compressed);
#endif
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
