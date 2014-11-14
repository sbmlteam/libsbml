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
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygenIgnored */


std::string dataToString(int* field, int numSamples1, int length)
{
  stringstream builder;
  for (int i = 0; i < length; ++i)
  {
    builder << field[i] << " ";
    if ((i + 1) % numSamples1 == 0) builder << "\n";
  }
  return builder.str();
}


  CK_CPPSTART


  extern char *TestDataDirectory;

START_TEST (test_SpatialExtension_read_L3V1V1)
{
  string file = TestDataDirectory;
  file += "/read_l3v1v1.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  string sbmlDoc = writeSBMLToStdString(document);
  fail_unless(document->getPackageName() == "core");

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  //document->printErrors();
  //fail_unless(document->getNumErrors() == 0);

  // model : compartment
  fail_unless(model->getNumCompartments() == 1);

  Compartment *comp = model->getCompartment(0);

  // compartment : compartmentMapping
  SpatialCompartmentPlugin* cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
  fail_unless(cplugin != NULL);

  CompartmentMapping *cMapping = cplugin->getCompartmentMapping();
  if (cMapping->isSetId()) {
    fail_unless(cMapping->getId()		 == "compMap1");
    fail_unless(cMapping->getDomainType()	 == "dtype1");
    fail_unless(cMapping->getUnitSize()		 == 1);
  }

  // model : species 1
  fail_unless(model->getNumSpecies() == 2);

  Species *sp = model->getSpecies(0);

  SpatialSpeciesPlugin* srplugin = static_cast<SpatialSpeciesPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : species 2
  sp = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesPlugin*>(sp->getPlugin("spatial"));
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
  SpatialParameterPlugin* pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_DIFFUSIONCOEFFICIENT);   
  DiffusionCoefficient *diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff != NULL);

  fail_unless(diffCoeff->getVariable()		== "ATPc");
  fail_unless(diffCoeff->isSetCoordinateReference1());
  fail_unless(diffCoeff->getCoordinateReference1() == SPATIAL_COORDINATEKIND_CARTESIAN_X);

  // parameter 1 : advectionCoefficient
  param = model->getParameter(1);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_ADVECTIONCOEFFICIENT);
  AdvectionCoefficient *advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff != NULL);
  fail_unless(advCoeff->getVariable()		== "ATPc");
  fail_unless(advCoeff->getCoordinate() == SPATIAL_COORDINATEKIND_CARTESIAN_X);

  // parameter 2 : boundaryCondition X
  param = model->getParameter(2);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_BOUNDARYCONDITION);
  BoundaryCondition *bc = pplugin->getBoundaryCondition();
  fail_unless(bc->getVariable()		   == "ATPc");
  fail_unless(bc->getCoordinateBoundary() == "Xmin");
  fail_unless(bc->getType() == SPATIAL_BOUNDARYKIND_DIRICHLET);

  // parameter 3 : SpatialSymbolReference (coordinateComponent from geometry)
  param = model->getParameter(3);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_SPATIALSYMBOLREFERENCE);
  SpatialSymbolReference *spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->getSpatialRef() == "coordComp1");

  // model : reaction	
  fail_unless(model->getNumReactions() == 1);

  Reaction *rxn = model->getReaction(0);
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(rxn->getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->getIsLocal() == true);

  // get the Geometry
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));
  fail_unless(mplugin != NULL);

  Geometry *geometry = mplugin->getGeometry();
  fail_unless(geometry != NULL);
  fail_unless(geometry->getPackageName()		== "spatial");
  fail_unless(geometry->getCoordinateSystem()	== SPATIAL_GEOMETRYKIND_CARTESIAN);

  // geometry coordinateComponent
  fail_unless(geometry->getNumCoordinateComponents() == 1);
  fail_unless(geometry->getListOfCoordinateComponents()->getPackageName() == "spatial");

  CoordinateComponent* coordComp = geometry->getCoordinateComponent(0);
  fail_unless(coordComp->getId()        == "coordComp1");
  fail_unless(coordComp->getType()	==  SPATIAL_COORDINATEKIND_CARTESIAN_X );
  fail_unless(coordComp->getUnit()			== "umeter");
  fail_unless(coordComp->getPackageName()		== "spatial");

  // boundaryMin and boundayMax within coordinateComponent
  Boundary *minX = coordComp->getBoundaryMin();
  fail_unless(minX->getId()	  == "Xmin");
  fail_unless(minX->getValue()		  == 0);
  fail_unless(minX->getPackageName() == "spatial");

  Boundary *maxX = coordComp->getBoundaryMax();
  fail_unless(maxX->getId()   == "Xmax");
  fail_unless(maxX->getValue()		  == 10);
  fail_unless(maxX->getPackageName() == "spatial");

  // geometry domainType
  fail_unless(geometry->getNumDomainTypes() == 1);
  fail_unless(geometry->getListOfDomainTypes()->getPackageName() == "spatial");

  DomainType *domainType = geometry->getDomainType(0);
  fail_unless(domainType->getId()         == "dtype1");
  fail_unless(domainType->getSpatialDimensions() == 3);
  fail_unless(domainType->getPackageName()		  == "spatial");

  // geometry domains
  fail_unless(geometry->getNumDomains() == 2);
  fail_unless(geometry->getListOfDomains()->getPackageName() == "spatial");

  Domain* domain = geometry->getDomain(0);
  fail_unless(domain->getId()   == "domain1");
  fail_unless(domain->getDomainType() == "dtype1");
  //fail_unless(domain->getImplicit()    == false);
  //fail_unless(domain->getShapeId()     == "circle");
  fail_unless(domain->getPackageName() == "spatial");

  // interiorPoints in Domain
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  InteriorPoint* interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 1);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // second domain in geometry
  domain = geometry->getDomain(1);
  fail_unless(domain->getId()   == "domain2");
  fail_unless(domain->getDomainType() == "dtype1");
  //fail_unless(domain->getImplicit()    == false);
  //fail_unless(domain->getShapeId()     == "square");
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
  fail_unless(adjDomain->getId()   == "adjDomain1");
  fail_unless(adjDomain->getDomain1()     == "domain1");
  fail_unless(adjDomain->getDomain2()     == "domain2");
  fail_unless(adjDomain->getPackageName() == "spatial");

  // geometry : geometryDefinitions
  fail_unless(geometry->getNumGeometryDefinitions() == 2);
  fail_unless(geometry->getListOfGeometryDefinitions()->getPackageName() == "spatial");

  GeometryDefinition *gd = geometry->getGeometryDefinition(0);
  AnalyticGeometry *analyticGeom = static_cast<AnalyticGeometry*>(gd);
  fail_unless(analyticGeom != NULL);
  fail_unless(analyticGeom->getId()   == "analyticGeom1");
  fail_unless(analyticGeom->getPackageName() == "spatial");

  // AnalyticGeometry : analyticVolumes
  fail_unless(analyticGeom->getNumAnalyticVolumes() == 1);
  fail_unless(analyticGeom->getListOfAnalyticVolumes()->getPackageName() == "spatial");

  AnalyticVolume* av = analyticGeom->getAnalyticVolume(0);
  fail_unless(av->getId()    == "analyticVol1");
  fail_unless(av->getDomainType()   == "dtype1");
  fail_unless(av->getFunctionType() == SPATIAL_FUNCTIONKIND_LAYERED);
  fail_unless(av->getOrdinal()      == 1);
  fail_unless(av->getPackageName()  == "spatial");
  // ??????Math????

  // geometry : sampledFieldGeometry
  gd = geometry->getGeometryDefinition(1);
  SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
  fail_unless(sfGeom->getId()   == "sampledFieldGeom1");
  fail_unless(sfGeom->getPackageName() == "spatial");

  // sampledFieldGeometry : SampledVolumes
  fail_unless(sfGeom->getNumSampledVolumes() == 1);
  fail_unless(sfGeom->getListOfSampledVolumes()->getPackageName() == "spatial");

  SampledVolume* sv = sfGeom->getSampledVolume(0);
  fail_unless(sv->getId()    == "sv_1");
  fail_unless(sv->getDomainType()   == "dtype1");
  fail_unless(sv->getSampledValue() == 128);
  fail_unless(sv->getMinValue()     == 0);
  fail_unless(sv->getMaxValue()     == 255);
  fail_unless(sv->getPackageName()  == "spatial");

  // sampledFieldGeometry : SampledField
  SampledField* sf = geometry->getSampledField(sfGeom->getSampledField());
  fail_unless(sf != NULL);
  fail_unless(sf->getId()		  == "sampledField1");
  fail_unless(sf->getDataType()		  == SPATIAL_DATAKIND_UINT8);
  fail_unless(sf->getInterpolationType() == SPATIAL_INTERPOLATIONKIND_LINEAR);
  fail_unless(sf->getCompression()          == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  fail_unless(sf->getNumSamples1()       == 4);
  fail_unless(sf->getNumSamples2()       == 4);
  fail_unless(sf->getNumSamples3()       == 2);
  fail_unless(sf->getPackageName()		  == "spatial");

  // sampledField : ImageData
  fail_unless(sf->getSamplesLength() == 32);
  int* samples = new int[sf->getSamplesLength()];
  sf->getSamples(samples);
  fail_unless(samples[0] == 0);


  string s2 = writeSBMLToStdString(document);

  fail_unless(sbmlDoc==s2);

  delete document;
  delete[] samples;
}
END_TEST


  START_TEST (test_SpatialExtension_read_L3V1V1_defaultNS)
{
  string file = TestDataDirectory;
  file += "/read_L3V1V1_defaultNS.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  string sbmlDoc = writeSBMLToStdString(document);
  Model *model = document->getModel();

  //document->printErrors();

  fail_unless(model != NULL);
  //fail_unless(document->getNumErrors() == 0);

  // model : compartment
  fail_unless(model->getNumCompartments() == 1);

  Compartment *comp = model->getCompartment(0);

  // compartment : compartmentMapping
  SpatialCompartmentPlugin* cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
  fail_unless(cplugin != NULL);

  CompartmentMapping *cMapping = cplugin->getCompartmentMapping();
  if (cMapping->isSetId()) {
    fail_unless(cMapping->getId()		 == "compMap1");
    fail_unless(cMapping->getDomainType()	 == "dtype1");
    fail_unless(cMapping->getUnitSize()		 == 1);
  }

  // model : species 1
  fail_unless(model->getNumSpecies() == 2);

  Species *sp = model->getSpecies(0);

  SpatialSpeciesPlugin* srplugin = static_cast<SpatialSpeciesPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : species 2
  sp = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesPlugin*>(sp->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->getIsSpatial() == true);

  // model : parameters (species diffusion, advection coeffs, species boundary conditions, coordinate components from Geometry
  fail_unless(model->getNumParameters() == 5);



  // parameter 0 : diffusionCoefficient
  Parameter *param = model->getParameter(0);
  SpatialParameterPlugin* pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_DIFFUSIONCOEFFICIENT);
  DiffusionCoefficient *diffCoeff = pplugin->getDiffusionCoefficient();
  fail_unless(diffCoeff->getVariable()		== "ATPc");
  fail_unless(diffCoeff->isSetCoordinateReference1());
  fail_unless(diffCoeff->getCoordinateReference1() == SPATIAL_COORDINATEKIND_CARTESIAN_X);

  // parameter 1 : advectionCoefficient
  param = model->getParameter(1);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_ADVECTIONCOEFFICIENT);
  AdvectionCoefficient *advCoeff = pplugin->getAdvectionCoefficient();
  fail_unless(advCoeff->getVariable()		== "ATPc");
  fail_unless(advCoeff->getCoordinate() == SPATIAL_COORDINATEKIND_CARTESIAN_X);

  // parameter 2 : boundaryCondition X
  param = model->getParameter(2);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  fail_unless(pplugin->isSpatialParameter() == true);
  fail_unless(pplugin->getType() == SBML_SPATIAL_BOUNDARYCONDITION);
  BoundaryCondition *bc = pplugin->getBoundaryCondition();
  fail_unless(bc->getVariable()		   == "ATPc");
  fail_unless(bc->getCoordinateBoundary() == "Xmin");
  fail_unless(bc->getType() == SPATIAL_BOUNDARYKIND_DIRICHLET);

  // parameter 3 : SpatialSymbolReference (coordinateComponent from geometry)
  param = model->getParameter(3);
  pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference *spSymRef = pplugin->getSpatialSymbolReference();
  fail_unless(spSymRef->getSpatialRef() == "coordComp1");

  // model : reaction	
  fail_unless(model->getNumReactions() == 1);

  Reaction *rxn = model->getReaction(0);
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(rxn->getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->getIsLocal() == true);

  // get the Geometry
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));
  fail_unless(mplugin != NULL);

  Geometry *geometry = mplugin->getGeometry();
  fail_unless(geometry != NULL);
  fail_unless(geometry->getPackageName()		== "spatial");
  fail_unless(geometry->getCoordinateSystem()	== SPATIAL_GEOMETRYKIND_CARTESIAN);

  // geometry coordinateComponent
  fail_unless(geometry->getNumCoordinateComponents() == 1);
  fail_unless(geometry->getListOfCoordinateComponents()->getPackageName() == "spatial");

  CoordinateComponent* coordComp = geometry->getCoordinateComponent(0);
  fail_unless(coordComp->getId()        == "coordComp1");
  fail_unless(coordComp->getType()	== SPATIAL_COORDINATEKIND_CARTESIAN_X);
  fail_unless(coordComp->getUnit()			== "umeter");
  fail_unless(coordComp->getPackageName()		== "spatial");

  // boundaryMin and boundayMax within coordinateComponent
  Boundary *minX = coordComp->getBoundaryMin();
  fail_unless(minX->getId()	  == "Xmin");
  fail_unless(minX->getValue()		  == 0);
  fail_unless(minX->getPackageName() == "spatial");

  Boundary *maxX = coordComp->getBoundaryMax();
  fail_unless(maxX->getId()   == "Xmax");
  fail_unless(maxX->getValue()		  == 10);
  fail_unless(maxX->getPackageName() == "spatial");

  // geometry domainType
  fail_unless(geometry->getNumDomainTypes() == 1);
  fail_unless(geometry->getListOfDomainTypes()->getPackageName() == "spatial");

  DomainType *domainType = geometry->getDomainType(0);
  fail_unless(domainType->getId()         == "dtype1");
  fail_unless(domainType->getSpatialDimensions() == 3);
  fail_unless(domainType->getPackageName()		  == "spatial");

  // geometry domains
  fail_unless(geometry->getNumDomains() == 2);
  fail_unless(geometry->getListOfDomains()->getPackageName() == "spatial");

  Domain* domain = geometry->getDomain(0);
  fail_unless(domain->getId()   == "domain1");
  fail_unless(domain->getDomainType() == "dtype1");
  //fail_unless(domain->getImplicit()    == false);
  //fail_unless(domain->getShapeId()     == "circle");
  fail_unless(domain->getPackageName() == "spatial");

  // interiorPoints in Domain
  fail_unless(domain->getNumInteriorPoints() == 1);
  fail_unless(domain->getListOfInteriorPoints()->getPackageName() == "spatial");

  InteriorPoint* interiorPt = domain->getInteriorPoint(0);
  fail_unless(interiorPt->getCoord1()		== 1);
  fail_unless(interiorPt->getPackageName() == "spatial");

  // second domain in geometry
  domain = geometry->getDomain(1);
  fail_unless(domain->getId()   == "domain2");
  fail_unless(domain->getDomainType() == "dtype1");
  //fail_unless(domain->getImplicit()    == false);
  //fail_unless(domain->getShapeId()     == "square");
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
  fail_unless(adjDomain->getId()   == "adjDomain1");
  fail_unless(adjDomain->getDomain1()     == "domain1");
  fail_unless(adjDomain->getDomain2()     == "domain2");
  fail_unless(adjDomain->getPackageName() == "spatial");

  // geometry : geometryDefinitions
  fail_unless(geometry->getNumGeometryDefinitions() == 2);
  fail_unless(geometry->getListOfGeometryDefinitions()->getPackageName() == "spatial");

  GeometryDefinition *gd = geometry->getGeometryDefinition(0);
  AnalyticGeometry *analyticGeom = static_cast<AnalyticGeometry*>(gd);
  fail_unless(analyticGeom->getId()   == "analyticGeom1");
  fail_unless(analyticGeom->getPackageName() == "spatial");

  // AnalyticGeometry : analyticVolumes
  fail_unless(analyticGeom->getNumAnalyticVolumes() == 1);
  fail_unless(analyticGeom->getListOfAnalyticVolumes()->getPackageName() == "spatial");

  AnalyticVolume* av = analyticGeom->getAnalyticVolume(0);
  fail_unless(av->getId()    == "analyticVol1");
  fail_unless(av->getDomainType()   == "dtype1");
  fail_unless(av->getFunctionType() == SPATIAL_FUNCTIONKIND_LAYERED);
  fail_unless(av->getOrdinal()      == 1);
  fail_unless(av->getPackageName()  == "spatial");
  // ??????Math????

  // geometry : sampledFieldGeometry
  gd = geometry->getGeometryDefinition(1);
  SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
  fail_unless(sfGeom->getId()   == "sampledFieldGeom1");
  fail_unless(sfGeom->getPackageName() == "spatial");

  // sampledFieldGeometry : SampledVolumes
  fail_unless(sfGeom->getNumSampledVolumes() == 1);
  fail_unless(sfGeom->getListOfSampledVolumes()->getPackageName() == "spatial");

  SampledVolume* sv = sfGeom->getSampledVolume(0);
  fail_unless(sv->getId()    == "sv_1");
  fail_unless(sv->getDomainType()   == "dtype1");
  fail_unless(sv->getSampledValue() == 128);
  fail_unless(sv->getMinValue()     == 0);
  fail_unless(sv->getMaxValue()     == 255);
  fail_unless(sv->getPackageName()  == "spatial");

  // sampledFieldGeometry : SampledField
  SampledField* sf = geometry->getSampledField( sfGeom->getSampledField() );
  fail_unless(sf->getId()		  == "sampledField1");
  fail_unless(sf->getDataType()		  == SPATIAL_DATAKIND_UINT8);
  fail_unless(sf->getInterpolationType() == SPATIAL_INTERPOLATIONKIND_LINEAR);
  fail_unless(sf->getCompression()          == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  fail_unless(sf->getNumSamples1()       == 4);
  fail_unless(sf->getNumSamples2()       == 4);
  fail_unless(sf->getNumSamples3()       == 2);
  fail_unless(sf->getPackageName()		  == "spatial");

  // sampledField : ImageData
  fail_unless(sf->getSamplesLength()  == 32);
  int* samples = new int[sf->getSamplesLength()];
  sf->getSamples(samples);
  fail_unless(samples[0] == 0);

  string s2 = writeSBMLToStdString(document);

  fail_unless(sbmlDoc==s2);

  delete document;  
  delete[] samples;
}
END_TEST


  START_TEST (test_SpatialExtension_read_L3V1V1_unknown_elements)
{

  string file = TestDataDirectory;
  file += "/read_L3V1V1_unknown_elements.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
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
  const string expected(
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

  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  fail_unless(doc->getModel() != NULL);
  SpatialModelPlugin* plugin = (SpatialModelPlugin*)doc->getModel()->getPlugin("spatial");
  fail_unless(plugin != NULL);
  fail_unless(plugin->getGeometry() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions()->size() == 1);
  SampledFieldGeometry* geometry = (SampledFieldGeometry*)plugin->getGeometry()->getListOfGeometryDefinitions()->get(0);
  fail_unless(geometry != NULL);
  SampledField* field = plugin->getGeometry()->getSampledField( geometry->getSampledField() );
  fail_unless(field->getNumSamples1() == 57);
  fail_unless(field->getNumSamples2() == 63);
  fail_unless(field->getDataType() == SPATIAL_DATAKIND_UINT8);
  
  // test new API 
  int length1 = field->getUncompressedLength();
  int* array1 = new int[length1]; 
  fail_unless(length1 == 3591);
  field->getUncompressed(array1);
  string test1 = dataToString(array1, field->getNumSamples1(), length1);
  fail_unless(test1 == expected);

  int* result; int resultLength;
  field->getUncompressedData(result, resultLength);

  fail_unless(resultLength == length1);

  string resultString = dataToString(result, field->getNumSamples1(), resultLength);

  
  fail_unless(resultString == expected);

  // test new API
  int uncompressed = field->getUncompressedLength();
  fail_unless(resultLength == uncompressed);
  int* more = new int[uncompressed]; 
  field->getUncompressed(more);
  resultString = dataToString(more, field->getNumSamples1(), resultLength);
  fail_unless(resultString == expected);

  delete doc;
  delete[] array1;
  free(result);
  delete[] more;
}
END_TEST
#endif
  
 START_TEST (test_SpatialExtension_read_uncompressed)
{
  string filename(TestDataDirectory);
  filename += "ImageTest2.xml";
  const string expected(
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
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  fail_unless(doc->getModel() != NULL);
  SpatialModelPlugin* plugin = (SpatialModelPlugin*)doc->getModel()->getPlugin("spatial");
  fail_unless(plugin != NULL);
  fail_unless(plugin->getGeometry() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions() != NULL);
  fail_unless(plugin->getGeometry()->getListOfGeometryDefinitions()->size() == 1);
  SampledFieldGeometry* geometry = (SampledFieldGeometry*)plugin->getGeometry()->getListOfGeometryDefinitions()->get(0);
  fail_unless(geometry != NULL);
  SampledField* field = plugin->getGeometry()->getSampledField( geometry->getSampledField() );
  fail_unless(field != NULL);
  fail_unless(field->getNumSamples1() == 57);
  fail_unless(field->getNumSamples2() == 63);
  fail_unless(field->getDataType() == SPATIAL_DATAKIND_UINT8);
  
  // test new API 
  int length1 = field->getUncompressedLength();
  int* array1 = new int[length1]; 
  fail_unless(length1 == 3591);
  field->getUncompressed(array1);
  string test1 = dataToString(array1, field->getNumSamples1(), length1);
  fail_unless(test1 == expected);

  int* result; int resultLength;
  field->getUncompressedData(result, resultLength);

  fail_unless(resultLength == length1);

  string resultString = dataToString(result, field->getNumSamples1(), resultLength);


  fail_unless(resultString == expected);

  // test new API
  int uncompressed = field->getUncompressedLength();
  fail_unless(resultLength == uncompressed);
  int* more = new int[uncompressed]; 
  field->getUncompressed(more);
  resultString = dataToString(more, field->getNumSamples1(), resultLength);
  fail_unless(resultString == expected);

  delete doc;
  delete[] array1;
  free(result);
  delete[] more;
}
END_TEST


START_TEST (test_SpatialExtension_readwrite_meshonly)
{

  string file = TestDataDirectory;
  file += "/MeshOnly.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  SBMLErrorLog* errors = document->getErrorLog();
  if (errors->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) != 0) {
    fail_unless(false);
    for (unsigned long e=0; e<errors->getNumErrors(); e++) {
      const SBMLError* error = errors->getError(e);
      if (error->getSeverity() == LIBSBML_SEV_ERROR) {
        cout << error->getMessage() << endl;
      }
    }
  }
  file = TestDataDirectory;
  file += "/MeshOnly_rt.xml";
  writeSBMLToFile(document, file.c_str());

  delete document;
}
END_TEST


START_TEST (test_SpatialExtension_readwrite_mixedonly)
{

  string file = TestDataDirectory;
  file += "/MixedOnly.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  SBMLErrorLog* errors = document->getErrorLog();
  if (errors->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) != 0) {
    fail_unless(false);
    for (unsigned long e=0; e<errors->getNumErrors(); e++) {
      const SBMLError* error = errors->getError(e);
      if (error->getSeverity() == LIBSBML_SEV_ERROR) {
        cout << error->getMessage() << endl;
      }
    }
  }
  file = TestDataDirectory;
  file += "/MixedOnly_rt.xml";
  writeSBMLToFile(document, file.c_str());

  delete document;
}
END_TEST


START_TEST (test_SpatialExtension_readwrite_csgonly)
{

  string file = TestDataDirectory;
  file += "/CSGOnly.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  SBMLErrorLog* errors = document->getErrorLog();
  if (errors->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) != 0) {
    fail_unless(false);
    for (unsigned long e=0; e<errors->getNumErrors(); e++) {
      const SBMLError* error = errors->getError(e);
      if (error->getSeverity() == LIBSBML_SEV_ERROR) {
        cout << error->getMessage() << endl;
      }
    }
  }
  file = TestDataDirectory;
  file += "/CSGOnly_rt.xml";
  writeSBMLToFile(document, file.c_str());

  delete document;
}
END_TEST


Suite *
  create_suite_ReadSpatialExtension (void)
{
  Suite *suite = suite_create("ReadSpatialExtension");
  TCase *tcase = tcase_create("ReadSpatialExtension");

  //tcase_add_test( tcase, test_SpatialExtension_readwrite_meshonly);
  //tcase_add_test( tcase, test_SpatialExtension_readwrite_csgonly);
  //tcase_add_test( tcase, test_SpatialExtension_readwrite_mixedonly);

  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1);
  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1_defaultNS);
  tcase_add_test( tcase, test_SpatialExtension_read_L3V1V1_unknown_elements);
  tcase_add_test( tcase, test_SpatialExtension_read_uncompressed);
#ifdef USE_ZLIB
  tcase_add_test( tcase, test_SpatialExtension_read_compressed);  
#endif
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
