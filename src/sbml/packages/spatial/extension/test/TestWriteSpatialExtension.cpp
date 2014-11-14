/**
* @file    TestWriteSpatialExtension.cpp
* @brief   Unit tests of writing SpatialExtension 
* @author  Akiya Jouraku
*
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <string>

#include <fstream>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

#define COMPARE_DOCUMENTS(document1,document2)\
{\
  string tempStringXXXX1 = writeSBMLToStdString(document1);\
  string tempStringXXXX2 = writeSBMLToStdString(document2);\
  fail_unless(tempStringXXXX1 == tempStringXXXX2);\
}

#define COMPARE_DOCUMENT_WITH_STRING(document1,string2)\
{\
  SBMLDocument *tempSBMLdoc2 = readSBMLFromString(string2);\
  COMPARE_DOCUMENTS(document1,tempSBMLdoc2);\
  delete tempSBMLdoc2;\
}

#define COMPARE_DOCUMENT_WITH_FILE(document1,file)\
{\
  SBMLDocument *tempSBMLdoc2 = readSBMLFromFile(file);\
  COMPARE_DOCUMENTS(document1,tempSBMLdoc2);\
  delete tempSBMLdoc2;\
}

#define COMPARE_DOCUMENT_STRING(string1,string2)\
{\
  SBMLDocument *tempSBMLdoc1 = readSBMLFromString(string1);\
  SBMLDocument *tempSBMLdoc2 = readSBMLFromString(string2);\
  COMPARE_DOCUMENTS(tempSBMLdoc1,tempSBMLdoc2);\
  delete tempSBMLdoc1;\
  delete tempSBMLdoc2;\
}


  CK_CPPSTART

static string SPATIAL_XMLNS_L3V1V1;
static SpatialExtension* S; 
static SpatialPkgNamespaces* SNS;
extern char *TestDataDirectory;

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


  // SBMLNamespaces of SBML Level 3 Version 1 with 'spatial' Version 1
  SpatialPkgNamespaces sbmlns(3,1,1);

  // create the L3V1 document with spatial package
  SBMLDocument *document = new SBMLDocument(&sbmlns);	

  // set 'required' attribute on document for 'spatial' and 'req' packages to 'true'

  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);
  document->setPackageRequired("spatial", true);

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
  SpatialSpeciesPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->createDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setType(SPATIAL_DIFFUSIONKIND_ANISOTROPIC) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateReference1(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->createAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinate(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->createBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType(SPATIAL_BOUNDARYKIND_DIRICHLET) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->createSpecies();
  species->setId("ATPm");
  species->setCompartment("cytosol");
  species->setInitialConcentration(2);
  species->setHasOnlySubstanceUnits(false);
  species->setBoundaryCondition(false);
  species->setConstant(false);
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->createReaction();
  reaction->setId("rxn1");
  reaction->setReversible(false);
  reaction->setFast(false);
  reaction->setCompartment("cytosol");
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

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
  Geometry* geometry = mplugin->createGeometry();
  fail_unless(geometry->setCoordinateSystem(SPATIAL_GEOMETRYKIND_CARTESIAN) == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setType(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  Boundary* minX = coordX->createBoundaryMin();
  fail_unless(minX->setId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  Boundary* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->createSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialRef(coordX->getId()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->createCompartmentMapping();
  fail_unless(compMapping->setId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType(SPATIAL_FUNCTIONKIND_LAYERED) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  
  SampledField* sampledField = geometry->createSampledField();
  fail_unless(sampledField->setId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  sfg->setSampledField(sampledField->getId());
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType(SPATIAL_DATAKIND_UINT8) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType(SPATIAL_INTERPOLATIONKIND_LINEAR) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED) == LIBSBML_OPERATION_SUCCESS);
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
  fail_unless(sampledField->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  string s2 = writeSBMLToStdString(document);

  string file = TestDataDirectory;
  file += "/create_and_write_L3V1V1.xml";

  SBMLDocument *tempDoc = readSBMLFromFile(file.c_str());
  string sbmlDoc = writeSBMLToStdString(tempDoc);
  COMPARE_DOCUMENT_STRING(sbmlDoc.c_str(), s2.c_str());

  // check clone()

  SBMLDocument* document2 = document->clone();
  s2 = writeSBMLToStdString(document2);
  fail_unless(sbmlDoc ==s2); 

  // check operator=

  Model m(document->getSBMLNamespaces()); 
  m = *(document->getModel());
  document2->setModel(&m);
  s2 = writeSBMLToStdString(document2);

  COMPARE_DOCUMENT_STRING(sbmlDoc.c_str(), s2.c_str());

  delete tempDoc;
  delete document2;
  delete document;
  delete mathNode;
}
END_TEST


START_TEST (test_SpatialExtension_create_add_and_write_L3V1V1)
{

  // SBMLNamespaces of SBML Level 3 Version 1 with 'spatial' Version 1
  SpatialPkgNamespaces sbmlns(3,1,1);

  // create the L3V1 document with spatial package
  SBMLDocument *document = new SBMLDocument(&sbmlns);	

  // set 'required' attribute on document for 'spatial' packages to 'true'

  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("spatial", true);

  // create the Model
  Model model(&sbmlns);

  // create the Compartment
  Compartment compartment(&sbmlns);
  compartment.setId("cytosol");
  compartment.setConstant(true);
  
  /*
    "     <compartment id=\"cytosol\" constant=\"true\"  >\n"
    "       <spatial:compartmentMapping spatial:id=\"compMap1\" spatial:compartment=\"cytosol\" spatial:domainType=\"dtype1\" spatial:unitSize=\"1\"/>\n"
    "     </compartment>\n"
  */
  SpatialCompartmentPlugin* compPlugin =  (SpatialCompartmentPlugin*)compartment.getPlugin("spatial");
  fail_unless(compPlugin != NULL);

  CompartmentMapping* compMapping = compPlugin->createCompartmentMapping();
  compMapping->setId("compMap1");
  compMapping->setDomainType("dtype1");
  compMapping->setUnitSize(1);

  fail_unless(model.addCompartment(&compartment) == LIBSBML_OPERATION_SUCCESS);

  // create the Species
  Species species1(&sbmlns);
  species1.setId("ATPc");
  species1.setCompartment("cytosol");
  species1.setInitialConcentration(1);
  species1.setConstant(false);
  species1.setBoundaryCondition(false);
  species1.setHasOnlySubstanceUnits(false);
  // spatial package extension to species.
  SpatialSpeciesPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesPlugin*>(species1.getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model.addSpecies(&species1) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter paramSp(&sbmlns);
  paramSp.initDefaults();
  paramSp.setId(species1.getId()+"_dc");
  paramSp.setValue(1.0);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp.getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->createDiffusionCoefficient();
  fail_unless(diffCoeff->setType(SPATIAL_DIFFUSIONKIND_ANISOTROPIC) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setVariable(species1.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateReference1(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model.addParameter(&paramSp) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = Parameter(&sbmlns);
  paramSp.initDefaults();
  paramSp.setId(species1.getId()+"_ac");
  paramSp.setValue(1.5);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp.getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->createAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species1.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinate(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model.addParameter(&paramSp) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = Parameter(&sbmlns);
  paramSp.initDefaults();
  paramSp.setId(species1.getId()+"_bc");
  paramSp.setValue(2.0);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp.getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->createBoundaryCondition();
  fail_unless(boundCon->setVariable(species1.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType(SPATIAL_BOUNDARYKIND_DIRICHLET) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model.addParameter(&paramSp) == LIBSBML_OPERATION_SUCCESS);

  Species species2(&sbmlns);
  species2.setId("ATPm");
  species2.setCompartment("cytosol");
  species2.setInitialConcentration(2);
  species2.setConstant(false);
  species2.setBoundaryCondition(false);
  species2.setHasOnlySubstanceUnits(false);
  srplugin = static_cast<SpatialSpeciesPlugin*>(species2.getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);
  model.addSpecies(&species2);

  // add a reaction
  Reaction reaction(&sbmlns);
  reaction.setId("rxn1");
  reaction.setReversible(false);
  reaction.setFast(false);
  reaction.setCompartment("cytosol");

  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(reaction.getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);


  fail_unless(model.addReaction(&reaction) == LIBSBML_OPERATION_SUCCESS);

  //
  // Get a SpatialModelPlugin object plugged in the model object.
  //
  // The type of the returned value of SBase::getPlugin() function is 
  // SBasePlugin*, and thus the value needs to be casted for the 
  // corresponding derived class.
  //
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model.getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->createGeometry();
  fail_unless(geometry->setCoordinateSystem(SPATIAL_GEOMETRYKIND_CARTESIAN) == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent coordX(&sbmlns);
  fail_unless(coordX.setId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX.setType(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX.setUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  Boundary minX(&sbmlns);
  fail_unless(minX.setId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX.setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX.setBoundaryMin(&minX) == LIBSBML_OPERATION_SUCCESS);
  Boundary maxX(&sbmlns);
  fail_unless(maxX.setId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX.setValue(10.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX.setBoundaryMax(&maxX) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addCoordinateComponent(&coordX) == LIBSBML_OPERATION_SUCCESS);

  Parameter paramX(&sbmlns);
  paramX.initDefaults();
  paramX.setId("x");
  paramX.setValue(8.0);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX.getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->createSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialRef(coordX.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(model.addParameter(&paramX) == LIBSBML_OPERATION_SUCCESS);

  DomainType domainType(&sbmlns);
  fail_unless(domainType.setId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType.setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomainType(&domainType) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment.getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  compMapping = cplugin->createCompartmentMapping();
  fail_unless(compMapping->setId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain domain1(&sbmlns);
  fail_unless(domain1.setId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain1.setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain1.setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain1.setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint interiorPt1(&sbmlns);
  fail_unless(interiorPt1.setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain1.addInteriorPoint(&interiorPt1) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomain(&domain1) == LIBSBML_OPERATION_SUCCESS);

  Domain domain2(&sbmlns);
  fail_unless(domain2.setId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain2.setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain2.setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain2.setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint internalPt2(&sbmlns);
  fail_unless(internalPt2.setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain2.addInteriorPoint(&internalPt2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addDomain(&domain2) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains adjDomain(&sbmlns);
  fail_unless(adjDomain.setId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain.setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain.setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addAdjacentDomains(&adjDomain) == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry analyticGeom(&sbmlns);
  analyticGeom.setIsActive(true);
  fail_unless(analyticGeom.setId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume analyticVol(&sbmlns);
  fail_unless(analyticVol.setId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol.setDomainType(domainType.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol.setFunctionType(SPATIAL_FUNCTIONKIND_LAYERED) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol.setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol.setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticGeom.addAnalyticVolume(&analyticVol) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addGeometryDefinition(&analyticGeom) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry sfg(&sbmlns);
  sfg.setIsActive(false);
  fail_unless(sfg.setId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField sampledField(&sbmlns);
  fail_unless(sampledField.setId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setDataType("uint8") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField.setCompression("uncompressed") == LIBSBML_OPERATION_SUCCESS);
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
  fail_unless(sampledField.setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sfg.setSampledField(sampledField.getId())  == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addSampledField(&sampledField)  == LIBSBML_OPERATION_SUCCESS);


  SampledVolume sampledVol(&sbmlns);
  fail_unless(sampledVol.setId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol.setDomainType(domainType.getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol.setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol.setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol.setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sfg.addSampledVolume(&sampledVol)  == LIBSBML_OPERATION_SUCCESS);
  fail_unless(geometry->addGeometryDefinition(&sfg) == LIBSBML_OPERATION_SUCCESS);

  // set the model to the document

  document->setModel(&model);  

  string s2 = writeSBMLToStdString(document);

  string file = TestDataDirectory;
  file += "/create_add_and_write_L3V1V1.xml";
  SBMLDocument *tempDoc = readSBMLFromFile(file.c_str());

  string sbmlDoc = writeSBMLToStdString(tempDoc);

  COMPARE_DOCUMENT_STRING(sbmlDoc.c_str(), s2.c_str());

  delete document;
  delete tempDoc;
  delete mathNode;
}
END_TEST


  START_TEST (test_SpatialExtension_read_enable_via_model_and_write_L3V1V1)
{

  string file = TestDataDirectory;
  file += "/read_enable_via_model_and_write_L3V1V1_1.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());

  fail_unless(document->getNumPlugins()             == 0);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  //
  // enable the spatial package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("spatial", true);

  fail_unless(document->getNumPlugins() == 1);
  fail_unless(model->getNumPlugins()    == 1);

  // spatial package extension to species.
  // required elements package extention to parameter
  Species* species = model->getSpecies(0);
  SpatialSpeciesPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->initDefaults();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->createDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateReference1(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->initDefaults();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->createAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinate(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->initDefaults();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->createBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType(SPATIAL_BOUNDARYKIND_DIRICHLET) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->getReaction(0);
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

  // create a geometry object 
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->createGeometry();
  fail_unless(geometry->setCoordinateSystem(SPATIAL_GEOMETRYKIND_CARTESIAN) == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setType(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  Boundary* minX = coordX->createBoundaryMin();
  fail_unless(minX->setId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  Boundary* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->initDefaults();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->createSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialRef(coordX->getId()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  Compartment* compartment = model->getCompartment(0);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->createCompartmentMapping();
  fail_unless(compMapping->setId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType(SPATIAL_FUNCTIONKIND_LAYERED) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = geometry->createSampledField();
  fail_unless(sampledField->setId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  sfg->setSampledField(sampledField->getId());
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType(SPATIAL_DATAKIND_UINT8) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType(SPATIAL_INTERPOLATIONKIND_LINEAR) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED) == LIBSBML_OPERATION_SUCCESS);
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
  fail_unless(sampledField->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  string s2 = writeSBMLToStdString(document);

  file = TestDataDirectory;
  file += "/read_enable_via_model_and_write_L3V1V1_2.xml";

  SBMLDocument *tempDoc = readSBMLFromFile(file.c_str());
  string tempString = writeSBMLToStdString(tempDoc);
  
  COMPARE_DOCUMENT_STRING(tempString.c_str(), s2.c_str());

  delete tempDoc;  
  delete document;
  delete mathNode;
}
END_TEST


  START_TEST (test_SpatialExtension_read_disable_via_model_and_write_L3V1V1)
{

  string file = TestDataDirectory;
  file += "/read_disable_via_model_and_write_L3V1V1_1.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());
  

  fail_unless(document->getNumPlugins() == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 1);

  //
  // disable the spatial and requiredElements packages by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);
  fail_unless(model->getNumPlugins()    == 0);

  string s2 = writeSBMLToStdString(document);

  file = TestDataDirectory;
  file += "/read_disable_via_model_and_write_L3V1V1_2.xml";

  SBMLDocument *tempDoc = readSBMLFromFile(file.c_str());
  string tempSBML = writeSBMLToStdString(tempDoc);

  COMPARE_DOCUMENT_STRING(tempSBML.c_str(), s2.c_str());
  delete tempDoc;  
  delete document;  
}
END_TEST


  START_TEST (test_SpatialExtension_read_enable_via_sbmldocument_and_write_L3V1V1)
{

  string file = TestDataDirectory;
  file += "/read_enable_via_sbmldocument_and_write_L3V1V1_1.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());

  fail_unless(document->getNumPlugins()             == 0);

  //
  // enable the spatial and requiredElements packages by invoking enablePackage function with SBMLDocument object
  //
  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", true) == LIBSBML_OPERATION_SUCCESS);

  document->setPackageRequired("spatial", true);

  fail_unless(document->getNumPlugins() == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 1);

  // spatial package extension to species.
  // required elements package extention to parameter
  Species* species = model->getSpecies(0);
  SpatialSpeciesPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for diff coeff of species
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_dc");
  paramSp->setValue(1.0);
  paramSp->setConstant(true);
  // spatial package extension to diffusion parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  DiffusionCoefficient* diffCoeff = pplugin->createDiffusionCoefficient();
  fail_unless(diffCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(diffCoeff->setCoordinateReference1(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for advection coeff of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_ac");
  paramSp->setValue(1.5);
  paramSp->setConstant(true);
  // spatial package extension to advection parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  AdvectionCoefficient* advCoeff = pplugin->createAdvectionCoefficient();
  fail_unless(advCoeff->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(advCoeff->setCoordinate(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);

  // add parameter for boundary condition of species
  paramSp = model->createParameter();
  paramSp->setId(species->getId()+"_bc");
  paramSp->setValue(2.0);
  paramSp->setConstant(true);
  // spatial package extension to boundary condition parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  BoundaryCondition* boundCon = pplugin->createBoundaryCondition();
  fail_unless(boundCon->setVariable(species->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setType(SPATIAL_BOUNDARYKIND_DIRICHLET) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(boundCon->setCoordinateBoundary("Xmin") == LIBSBML_OPERATION_SUCCESS);

  // add another species
  species = model->getSpecies(1);
  srplugin = static_cast<SpatialSpeciesPlugin*>(species->getPlugin("spatial"));
  fail_unless(srplugin != NULL);
  fail_unless(srplugin->setIsSpatial(true) == LIBSBML_OPERATION_SUCCESS);

  // add a reaction
  Reaction* reaction = model->getReaction(0);
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(reaction->getPlugin("spatial"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->setIsLocal(true) == LIBSBML_OPERATION_SUCCESS);

  // create a geometry object 
  SpatialModelPlugin* mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  fail_unless(mplugin != NULL);

  // create the Geometry
  Geometry* geometry = mplugin->createGeometry();
  fail_unless(geometry->setCoordinateSystem(SPATIAL_GEOMETRYKIND_CARTESIAN) == LIBSBML_OPERATION_SUCCESS);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  fail_unless(coordX->setId("coordComp1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setType(SPATIAL_COORDINATEKIND_CARTESIAN_X) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(coordX->setUnit("umeter") == LIBSBML_OPERATION_SUCCESS);
  Boundary* minX = coordX->createBoundaryMin();
  fail_unless(minX->setId("Xmin") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(minX->setValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  Boundary* maxX = coordX->createBoundaryMax();
  fail_unless(maxX->setId("Xmax") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(maxX->setValue(10.0) == LIBSBML_OPERATION_SUCCESS);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  paramX->setConstant(true);
  // spatial package extension to SpatialSymbolRef parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  fail_unless(pplugin != NULL);
  SpatialSymbolReference* spSymRef = pplugin->createSpatialSymbolReference();
  fail_unless(spSymRef->setSpatialRef(coordX->getId()) == LIBSBML_OPERATION_SUCCESS);

  DomainType* domainType = geometry->createDomainType();
  fail_unless(domainType->setId("dtype1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domainType->setSpatialDimensions(3) == LIBSBML_OPERATION_SUCCESS);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  Compartment *compartment = model->getCompartment(0);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  fail_unless(cplugin != NULL);
  CompartmentMapping* compMapping = cplugin->createCompartmentMapping();
  fail_unless(compMapping->setId("compMap1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(compMapping->setUnitSize(1.0) == LIBSBML_OPERATION_SUCCESS);

  Domain* domain = geometry->createDomain();
  fail_unless(domain->setId("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("circle") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  fail_unless(internalPt1->setCoord1(1.0) == LIBSBML_OPERATION_SUCCESS);

  domain = geometry->createDomain();
  fail_unless(domain->setId("domain2") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(domain->setDomainType("dtype1") == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setImplicit(false) == LIBSBML_OPERATION_SUCCESS);
  //fail_unless(domain->setShapeId("square") == LIBSBML_OPERATION_SUCCESS);
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  fail_unless(internalPt2->setCoord1(5.0) == LIBSBML_OPERATION_SUCCESS);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  fail_unless(adjDomain->setId("adjDomain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain1("domain1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(adjDomain->setDomain2("domain2") == LIBSBML_OPERATION_SUCCESS);

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  fail_unless(analyticGeom->setId("analyticGeom1") == LIBSBML_OPERATION_SUCCESS);
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  fail_unless(analyticVol->setId("analyticVol1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setFunctionType(SPATIAL_FUNCTIONKIND_LAYERED) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(analyticVol->setOrdinal(1) == LIBSBML_OPERATION_SUCCESS);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  fail_unless(analyticVol->setMath(mathNode) == LIBSBML_OPERATION_SUCCESS);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  fail_unless(sfg->setId("sampledFieldGeom1") == LIBSBML_OPERATION_SUCCESS);
  SampledField* sampledField = geometry->createSampledField();
  fail_unless(sampledField->setId("sampledField1") == LIBSBML_OPERATION_SUCCESS);
  sfg->setSampledField(sampledField->getId());
  fail_unless(sampledField->setNumSamples1(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples2(4) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setNumSamples3(2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setDataType("double") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setInterpolationType("linear") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledField->setCompression("uncompressed") == LIBSBML_OPERATION_SUCCESS);
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
  fail_unless(sampledField->setSamples(samples, 32) == LIBSBML_OPERATION_SUCCESS);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  fail_unless(sampledVol->setId("sv_1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setDomainType(domainType->getId()) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setSampledValue(128.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMinValue(0.0) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(sampledVol->setMaxValue(255.0) == LIBSBML_OPERATION_SUCCESS);

  string s2 = writeSBMLToStdString(document);
  
  file = TestDataDirectory;
  file += "/read_enable_via_sbmldocument_and_write_L3V1V1_2.xml";
  delete document; 
  document = readSBMLFromFile(file.c_str());
  string s1a = writeSBMLToStdString(document);


  COMPARE_DOCUMENT_STRING(s1a.c_str(), s2.c_str());

  delete document;
  delete mathNode;
}
END_TEST


  START_TEST (test_SpatialExtension_read_disable_via_sbmldocument_and_write_L3V1V1)
{

  string file = TestDataDirectory;
  file += "/read_disable_via_sbmldocument_and_write_L3V1V1_1.xml";

  SBMLDocument *document = readSBMLFromFile(file.c_str());

  fail_unless(document->getNumPlugins() == 1);

  //
  // disable the spatial and requiredElement packages by invoking enablePackage function with sbmlDocument object
  // 
  fail_unless(document->enablePackage(SpatialExtension::getXmlnsL3V1V1(), "spatial", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  string s2 = writeSBMLToStdString(document);

  delete document;

  file = TestDataDirectory;
  file += "/read_disable_via_sbmldocument_and_write_L3V1V1_2.xml";

  document = readSBMLFromFile(file.c_str());
  string s1d = writeSBMLToStdString(document);

  COMPARE_DOCUMENT_STRING(s1d.c_str(),s2.c_str());
  
  delete document;  
}
END_TEST


START_TEST(test_write_csg)
{
  SpatialPkgNamespaces ns;
  SBMLDocument doc(&ns);
  Model* mod = doc.createModel();
  SpatialModelPlugin* mplug = dynamic_cast<SpatialModelPlugin*>(mod->getPlugin("spatial"));
  fail_unless(mplug != NULL);
  Geometry* geom = mplug->createGeometry();
  CSGeometry* csGeometry = geom->createCsGeometry();
  fail_unless(csGeometry->setId("cs1") == LIBSBML_OPERATION_SUCCESS);
  CSGObject* csObj = csGeometry->createCsgObject();
  fail_unless(csObj->setId("csObj1") == LIBSBML_OPERATION_SUCCESS);
  CSGPrimitive* csgPrim = csObj->createCsgPrimitive();
  fail_unless(csgPrim->setId("circle1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(csgPrim->setPrimitiveType("circle") == LIBSBML_OPERATION_SUCCESS);

  CSGObject* csObj2 = csGeometry->createCsgObject();
  fail_unless(csObj->setId("csObj2") == LIBSBML_OPERATION_SUCCESS);
  CSGHomogeneousTransformation* trans = csObj2->createCsgHomogeneousTransformation();
  CSGPseudoPrimitive* pseudo = trans->createCsgPseudoPrimitive();
  pseudo->setId("pseudo1");
  pseudo->setCsgObjectRef("csObj1");
  TransformationComponents* forward = trans->createForwardTransformation();
  double test1[] = { 0.1, 0.0, 1.0, 2.0, 3.0 };
  double test2[] = { 3.0, 2.0, 1.0, 0.0, 0.1 };
  fail_unless(forward->setComponents(test1, 5) == LIBSBML_OPERATION_SUCCESS);
  TransformationComponents* back = trans->createReverseTransformation();
  fail_unless(back->setComponents(test2, 5) == LIBSBML_OPERATION_SUCCESS);
  std::string test = writeSBMLToStdString(&doc);

  // now read it back
  SBMLDocument* doc2 = readSBMLFromString(test.c_str());
  fail_unless(doc2->getModel() != NULL);

  mplug = dynamic_cast<SpatialModelPlugin*>(doc2->getModel()->getPlugin("spatial"));
  fail_unless(mplug != NULL);

  geom = mplug->getGeometry();
  fail_unless(geom != NULL);

  std::string test3 = writeSBMLToStdString(doc2);
  fail_unless(test == test3);
  delete doc2;
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
  tcase_add_test(tcase, test_write_csg);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
