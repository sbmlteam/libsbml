
#include <iostream>

#include <sbml/SBMLTypes.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>

#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/packages/req/extension/RequiredElementsExtension.h>
#include <sbml/packages/req/common/RequiredElementsExtensionTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

void writeSpatialSBML();
void readSpatialSBML();

int main(int argc, char* argv[])
{
	writeSpatialSBML();
	readSpatialSBML();
}

void writeSpatialSBML() {
  
/*
  // SBMLNamespaces of SBML Level 3 Version 1 with Spatial Version 1
  SBMLNamespaces sbmlns(3,1,"spatial",1);
  // SpatialPkgNamespaces spatialns(3,1,1);

  // add Required Elements package namespace
  sbmlns.addPkgNamespace("req", 1);
*/

  // SBMLNamespaces of SBML Level 3 Version 1 with 'req' Version 1
  // then add 'spatial' package namespace.
  RequiredElementsPkgNamespaces sbmlns(3,1,1);
  sbmlns.addPkgNamespace("spatial",1);

  // create the L3V1 document with spatial package
  SBMLDocument document(&sbmlns);	


  // set 'required' attribute on document for 'spatial' and 'req' packages to 'T'??
  SBMLDocumentPlugin* dplugin;
  dplugin = static_cast<SBMLDocumentPlugin*>(document.getPlugin("spatial"));
  dplugin->setRequired(true);
  dplugin = static_cast<SBMLDocumentPlugin*>(document.getPlugin("req"));
  dplugin->setRequired(true);

  // create the Model 
  Model *model = document.createModel();
  model-> setId("trial_spatial");
  model-> setName("trial_spatial");

  // create the Compartments
  Compartment* compartment = model->createCompartment();
  compartment->setId("cytosol");
  compartment->setConstant(true);

  // create the Species
  Species* species1 = model->createSpecies();
  species1->setId("ATPc");
  species1->setCompartment("cytosol");
  species1->setInitialConcentration(1.0);
  species1->setHasOnlySubstanceUnits(false);
  species1->setBoundaryCondition(false);
  species1->setConstant(false);
  // spatial package extension to species.
  // required elements package extention to parameter
  RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(species1->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  SpatialSpeciesRxnPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species1->getPlugin("spatial"));
  srplugin->setIsSpatial(true);

  // add parameter for diff coeff of species1
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_dc");
  paramSp->setValue(1.0);
  // required elements package extention to parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  // spatial package extension to parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  DiffusionCoefficient* diffCoeff = pplugin->getDiffusionCoefficient();
  diffCoeff->setVariable(species1->getId());
  diffCoeff->setCoordinateIndex(0);
  // add parameter for adv coeff of species1
  paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_ac");
  paramSp->setValue(1.5);
  // required elements package extention to parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  // spatial package extension to parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  AdvectionCoefficient* advCoeff = pplugin->getAdvectionCoefficient();
  advCoeff->setVariable(species1->getId());
  advCoeff->setCoordinateIndex(0);
  // add parameter for boundary condition of species1
  paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_bc");
  paramSp->setValue(2.0);
  // required elements package extention to parameter
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramSp->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  // spatial package extension to parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  BoundaryCondition* boundCon = pplugin->getBoundaryCondition();
  boundCon->setVariable(species1->getId());
  boundCon->setType("value");
  boundCon->setCoordinateBoundary("Xmin");

  Species* species2 = model->createSpecies();
  species2->setId("ADPc");
  species2->setCompartment("cytosol");
  species2->setInitialConcentration(1);
  species2->setHasOnlySubstanceUnits(false);
  species2->setBoundaryCondition(false);
  species2->setConstant(false);
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(species2->getPlugin("spatial"));
  srplugin->setIsSpatial(true);

/*  // create a parameter
  Parameter* param = model->createParameter();
  param->setId("k_1");
  param->setValue(0.24);
  param->setConstant(true);

  // create an assignment rule
  AssignmentRule* assignRule = model->createAssignmentRule();
  assignRule->setVariable(species1->getId());
  assignRule->setFormula("species2+k_1");
*/
  /*
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(assignRule->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(false);
  */

  Reaction* reaction = model->createReaction();
  reaction->setId("rxn1");
  reaction->setReversible(false);
  reaction->setFast(false);
  reaction->setCompartment("cytosol");
  srplugin = static_cast<SpatialSpeciesRxnPlugin*>(reaction->getPlugin("spatial"));
  srplugin->setIsLocal(true);

  //
  // Get a SpatialModelPlugin object plugged in the model object.
  //
  // The type of the returned value of SBase::getPlugin() function is 
  // SBasePlugin*, and thus the value needs to be casted for the 
  // corresponding derived class.
  //
  SpatialModelPlugin* mplugin;
  mplugin = static_cast<SpatialModelPlugin*>(model->getPlugin("spatial"));

  //
  // Creates a geometry object via SpatialModelPlugin object.
  //
  Geometry* geometry = mplugin->getGeometry();
  geometry->setCoordinateSystem("XYZ");

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  coordX->setSpatialId("coordComp1");
  coordX->setComponentType("cartesian");
  coordX->setSbmlUnit("umeter");
  coordX->setIndex(1);
  BoundaryMin* minX = coordX->createBoundaryMin();
  minX->setSpatialId("Xmin");
  minX->setValue(0.0);
  BoundaryMax* maxX = coordX->createBoundaryMax();
  maxX->setSpatialId("Xmax");
  maxX->setValue(10.0);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  // required elements package extention to parameter
  // RequiredElementsSBasePlugin* reqplugin;
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(paramX->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  // spatial package extension to parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  SpatialSymbolReference* spSymRef = pplugin->getSpatialSymbolReference();
  spSymRef->setSpatialId(coordX->getSpatialId());
  spSymRef->setType(coordX->getElementName());

  DomainType* domainType = geometry->createDomainType();
  domainType->setSpatialId("dtype1");
  domainType->setSpatialDimensions(3);

  // Spatial package extension to compartment (mapping compartment with domainType)
  // required elements package extention to compartment
  reqplugin = static_cast<RequiredElementsSBasePlugin*>(compartment->getPlugin("req"));
  reqplugin->setMathOverridden("spatial");
  reqplugin->setCoreHasAlternateMath(true);
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  CompartmentMapping* compMapping = cplugin->getCompartmentMapping();
  compMapping->setSpatialId("compMap1");
  compMapping->setCompartment(compartment->getId());
  compMapping->setDomainType(domainType->getSpatialId());
  compMapping->setUnitSize(1.0);
  
  Domain* domain = geometry->createDomain();
  domain->setSpatialId("domain1");
  domain->setDomainType("dtype1");
  domain->setImplicit(false);
  domain->setShapeId("circle");
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  internalPt1->setCoord1(1.0);

  domain = geometry->createDomain();
  domain->setSpatialId("domain2");
  domain->setDomainType("dtype1");
  domain->setImplicit(false);
  domain->setShapeId("square");
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  internalPt2->setCoord1(5.0);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  adjDomain->setSpatialId("adjDomain1");
  adjDomain->setDomain1("domain1");
  adjDomain->setDomain2("domain2");

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  analyticGeom->setSpatialId("analyticGeom1");
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  analyticVol->setSpatialId("analyticVol1");
  analyticVol->setDomainType(domainType->getSpatialId());
  analyticVol->setFunctionType("squareFn");
  analyticVol->setOrdinal(1);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  analyticVol->setMath(mathNode);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  sfg->setSpatialId("sampledFieldGeom1");
  SampledField* sampledField = sfg->createSampledField();
  sampledField->setSpatialId("sampledField1");
  sampledField->setNumSamples1(4);
  sampledField->setNumSamples2(4);
  sampledField->setNumSamples3(2);
  sampledField->setDataType("double");
  sampledField->setInterpolationType("linear");
  sampledField->setEncoding("encoding1");
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
  id->setDataType("compressed");
  id->setSamples(samples, 32);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  sampledVol->setSpatialId("sv_1");
  sampledVol->setDomainType(domainType->getSpatialId());
  sampledVol->setSampledValue(128.0);
  sampledVol->setMinValue(0.0);
  sampledVol->setMaxValue(255.0);
  
  ParametricGeometry* pg = geometry->createParametricGeometry();
  pg->setSpatialId("parametricGeom1");
  ParametricObject* paramObj = pg->createParametricObject();
  paramObj->setSpatialId("po_1");
  paramObj->setDomain(domain->getSpatialId());
  paramObj->setPolygonType("hexagon");
  int ptIndices[5] = {1, 2, 3, 4, 5};
  PolygonObject* po = paramObj->createPolygonObject();
  po->setPointIndices(ptIndices, 5);
  SpatialPoint* spPt = pg->createSpatialPoint();
  spPt->setSpatialId("sp_1");
  spPt->setDomain(domain->getSpatialId());
  spPt->setCoord1(1);
  spPt->setCoord2(2);
  spPt->setCoord3(3);

  CSGeometry* csg = geometry->createCSGeometry();
  csg->setSpatialId("csGeom1");
  CSGObject* csgObj = csg->createCSGObject();
  csgObj->setSpatialId("csg_csgo_1");
  csgObj->setDomainType(domainType->getSpatialId());
  csgObj->setOrdinal(1);
  CSGScale* scale = csgObj->createCSGScale();
  scale->setScaleX(2.0);
  scale->setScaleY(3.0);
  scale->setScaleZ(4.0);
  CSGPrimitive* prim1 = scale->createCSGPrimitive();
  prim1->setPrimitiveType("SOLID_SPHERE");

  csgObj = csg->createCSGObject();
  csgObj->setSpatialId("csg_csgo_2");
  csgObj->setDomainType(domainType->getSpatialId());
  CSGSetOperator* setUnion = csgObj->createCSGSetOperator();
  setUnion->setOperationType("UNION");
/*  CSGPrimitive* prim = setUnion->createCSGPrimitive();
  prim->setPrimitiveType("SOLID_SPHERE");
  CSGPrimitive* prim2 = setUnion->createCSGPrimitive();
  prim2->setPrimitiveType("SOLID_CONE");
*/
  CSGPrimitive* prim2 = new CSGPrimitive(3,1,1);
  prim2->setSpatialId("cone0");
  prim2->setPrimitiveType("SOLID_CONE");
  CSGTranslation* translatedCone = new CSGTranslation(3,1,1);
  translatedCone->setSpatialId("translation0");
  translatedCone->setTranslateX(2.0);
  translatedCone->setTranslateY(2.0);
  translatedCone->setTranslateZ(2.0);
  translatedCone->setChild(prim2);
  int n = setUnion->addCSGNodeChild(translatedCone);
  CSGPrimitive* prim3 = new CSGPrimitive(3,1,1);
  prim3->setSpatialId("sphere0");
  prim3->setPrimitiveType("SOLID_SPHERE");
  n = setUnion->addCSGNodeChild(prim3);

  writeSBML(&document, "spatial_example0.xml");

}
void readSpatialSBML() {
	SBMLDocument *document2 = readSBML("spatial_example0.xml");
  
	Model *model2 = document2->getModel();
	Compartment *comp;
	SpatialCompartmentPlugin* cplugin;
	RequiredElementsSBasePlugin* reqplugin;
	for (unsigned int i = 0; i < model2->getNumCompartments(); i++) {
		comp = model2->getCompartment(i);
		cout << "Compartment" << i << ": "  << comp->getId() << endl;
		reqplugin = static_cast<RequiredElementsSBasePlugin*>(comp->getPlugin("req"));
		if (!reqplugin->getMathOverridden().empty()) {
			cout << "Comp" << i << "  req mathOverridden: "  << reqplugin->getMathOverridden() << endl;
		}
		cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
		if (cplugin->getCompartmentMapping()->isSetSpatialId()) {
			cout << "Comp" << i << "  CMSpId: "  << cplugin->getCompartmentMapping()->getSpatialId() << endl;
			cout << "Comp" << i << "  CM_Comp: "  << cplugin->getCompartmentMapping()->getCompartment() << endl;
			cout << "Comp" << i << "  CM_DType: "  << cplugin->getCompartmentMapping()->getDomainType() << endl;
			cout << "Comp" << i << "  CM_UnitSz: "  << cplugin->getCompartmentMapping()->getUnitSize() << endl;
		}
	}

	Species *sp;
	SpatialSpeciesRxnPlugin* srplugin;
	for (unsigned int i = 0; i < model2->getNumSpecies(); i++) {
		sp = model2->getSpecies(i);
		cout << "Species" << i << ": "      << sp->getId()      << endl;
		srplugin = static_cast<SpatialSpeciesRxnPlugin*>(sp->getPlugin("spatial"));
		if (srplugin->getIsSpatial()) {
			cout << "species" << i << "  isSpatial: "  << srplugin->getIsSpatial() << endl;
		}
	}

	Parameter *param;
	SpatialParameterPlugin* pplugin;
	for (unsigned int i = 0; i < model2->getNumParameters(); i++) {
		param = model2->getParameter(i);
		cout << "Parameter" << i << ": "  << param->getId() << endl;
		reqplugin = static_cast<RequiredElementsSBasePlugin*>(param->getPlugin("req"));
		if (!reqplugin->getMathOverridden().empty()) {
			cout << "Parameter" << i << "  req mathOverridden: "  << reqplugin->getMathOverridden() << endl;
		}
		pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
		if (pplugin->getSpatialSymbolReference()->isSetSpatialId()) {
			cout << "Parameter" << i << "  SpRefId: "  << pplugin->getSpatialSymbolReference()->getSpatialId() << endl;
			cout << "Parameter" << i << "  SpRefType: "  << pplugin->getSpatialSymbolReference()->getType() << endl;
		}
		if (pplugin->getDiffusionCoefficient()->isSetVariable()) {
			cout << "Diff_" << i << "  SpeciesVarId: "  << pplugin->getDiffusionCoefficient()->getVariable() << endl;
			cout << "Diff_" << i << "  SpCoordIndex: "  << pplugin->getDiffusionCoefficient()->getCoordinateIndex() << endl;
		}
		if (pplugin->getAdvectionCoefficient()->isSetVariable()) {
			cout << "Adv_" << i << "  SpeciesVarId: "  << pplugin->getAdvectionCoefficient()->getVariable() << endl;
			cout << "Adv_" << i << "  SpCoordIndex: "  << pplugin->getAdvectionCoefficient()->getCoordinateIndex() << endl;
		}
		if (pplugin->getBoundaryCondition()->isSetVariable()) {
			cout << "BC_" << i << "  SpeciesVarId: "  << pplugin->getBoundaryCondition()->getVariable() << endl;
			cout << "BC_" << i << "  SpCoordBoundary: "  << pplugin->getBoundaryCondition()->getCoordinateBoundary() << endl;
			cout << "BC_" << i << "  SpBoundaryType: "  << pplugin->getBoundaryCondition()->getType() << endl;
		}
	}

	Reaction *rxn;
	for (unsigned int i = 0; i < model2->getNumReactions(); i++) {
		rxn = model2->getReaction(i);
		cout << "Reaction" << i << ": "      << rxn->getId()      << endl;
		srplugin = static_cast<SpatialSpeciesRxnPlugin*>(rxn->getPlugin("spatial"));
		if (srplugin->getIsLocal()) {
			cout << "rxn" << i << "  isLocal: "  << srplugin->getIsLocal() << endl;
		}
	}

	Rule *rule;
	for (unsigned int i = 0; i < model2->getNumRules(); i++) {
		rule = model2->getRule(i);
		cout << "Rule" << i << ": "      << rule->getVariable()      << endl;
	}

	//
	// Get a SpatialModelPlugin object plugged in the model object.
	//
	// The type of the returned value of SBase::getPlugin() function is 
	// SBasePlugin*, and thus the value needs to be cast for the 
	// corresponding derived class.
	//
	SpatialModelPlugin* mplugin2;
	mplugin2 = static_cast<SpatialModelPlugin*>(model2->getPlugin("spatial"));
	cout << "URI: "      << mplugin2->getURI()      << endl;
	cout << "prefix: "      << mplugin2->getPrefix()      << endl;

	// get a Geometry object via SpatialModelPlugin object.
	Geometry* geometry2 = mplugin2->getGeometry();
	cout << "Geometry coordSystem: "      << geometry2->getCoordinateSystem()      << endl;
    
	// get a CoordComponent object via the Geometry object.	
	CoordinateComponent* coordComp = geometry2->getCoordinateComponent(0);
	std::cout << "CoordComponent spatialId: " << coordComp->getSpatialId() << std::endl;
	std::cout << "CoordComponent compType: " << coordComp->getComponentType() << std::endl;
	std::cout << "CoordComponent sbmlUnit: " << coordComp->getSbmlUnit() << std::endl;
	std::cout << "CoordComponent index: " << coordComp->getIndex() << std::endl;
	BoundaryMin* minX = coordComp->getBoundaryMin();
	std::cout << "minX name: " << minX->getSpatialId() << std::endl;
	std::cout << "minX value: " << minX->getValue() << std::endl;
	BoundaryMax* maxX = coordComp->getBoundaryMax();
	std::cout << "maxX name: " << maxX->getSpatialId() << std::endl;
	std::cout << "maxX value: " << maxX->getValue() << std::endl;

	// get a DomainType object via the Geometry object.	
	DomainType* domainType2 = geometry2->getDomainType(0);
	std::cout << "DomainType spatialId: " << domainType2->getSpatialId() << std::endl;
	std::cout << "DomainType spatialDim: " << domainType2->getSpatialDimensions() << std::endl;

	// get a Domain object via the Geometry object.	
	Domain* domain = geometry2->getDomain(0);
	std::cout << "Domain1 spatialId: " << domain->getSpatialId() << std::endl;
	std::cout << "Domain1 implicit: " << domain->getImplicit() << std::endl;
	std::cout << "Domain1 domainType: " << domain->getDomainType() << std::endl;
	std::cout << "Domain1 Shape: " << domain->getShapeId() << std::endl;
	// get an internal point via the domain object
	InteriorPoint* internalPt = domain->getInteriorPoint(0);
	std::cout << "InternalPt_1 coord1: " << internalPt->getCoord1() << std::endl;

	// get a Domain object via the Geometry object.	
	domain = geometry2->getDomain(1);
	std::cout << "Domain2 spatialId: " << domain->getSpatialId() << std::endl;
	std::cout << "Domain2 implicit: " << domain->getImplicit() << std::endl;
	std::cout << "Domain2 domainType: " << domain->getDomainType() << std::endl;
	std::cout << "Domain2 Shape: " << domain->getShapeId() << std::endl;
	// get an internal point via the domain object
	internalPt = domain->getInteriorPoint(0);
	std::cout << "InternalPt_2 coord1: " << internalPt->getCoord1() << std::endl;

	// get an AdjacentDomains object via the Geometry object.	
	AdjacentDomains* adjDomain = geometry2->getAdjacentDomains(0);
	std::cout << "AdjDomain spatialId: " << adjDomain->getSpatialId() << std::endl;
	std::cout << "AdjDomain domain1: " << adjDomain->getDomain1() << std::endl;
	std::cout << "AdjDomain domain2: " << adjDomain->getDomain2() << std::endl;

	// get the different GeometryDefinition objects via the Geometry object.
	GeometryDefinition* gd;
	for (unsigned int i = 0; i < geometry2->getNumGeometryDefinitions(); i++) {
		gd = geometry2->getGeometryDefinition(i);
		if (gd->isAnalyticGeometry()) {
			AnalyticGeometry* analyticalGeom = static_cast<AnalyticGeometry*>(gd);
			std::cout << "AnalGeom spatialId: " << analyticalGeom->getSpatialId() << std::endl;

			// analVol from analGeom.
			AnalyticVolume* av = analyticalGeom->getAnalyticVolume(0);
			std::cout << "AnalVol spatialId: " << av->getSpatialId() << std::endl;
			std::cout << "AnalVol domainType: " << av->getDomainType() << std::endl;
			std::cout << "AnalVol funcType: " << av->getFunctionType() << std::endl;
			std::cout << "AnalVol ordinal: " << av->getOrdinal() << std::endl;
			const ASTNode* mathNode = av->getMath();
			char* mathStr = writeMathMLToString(mathNode);
			std::cout << "AnalVol math: " << mathStr << std::endl;
		}
		if (gd->isSampledFieldGeometry()) {
			SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
			std::cout << "SampledFieldGeom spatialId: " << sfGeom->getSpatialId() << std::endl;
			
			// sampledField from sfGeom
			SampledField* sf = sfGeom->getSampledField();
			std::cout << "SampledField spatialId: " << sf->getSpatialId() << std::endl;
			std::cout << "SampledField dataType: " << sf->getDataType() << std::endl;
			std::cout << "SampledField interpolation: " << sf->getInterpolationType() << std::endl;
			std::cout << "SampledField encoding: " << sf->getEncoding() << std::endl;
			std::cout << "SampledField numSamples1: " << sf->getNumSamples1() << std::endl;
			std::cout << "SampledField numSamples2: " << sf->getNumSamples2() << std::endl;
			std::cout << "SampledField numSamples3: " << sf->getNumSamples3() << std::endl;
			const ImageData* id = sf->getImageData();
			int* samples = new int[id->getSamplesLength()];
			id->getSamples(samples);
			std::cout << "ImageData samples[0]: " << samples[0] << std::endl;
			std::cout << "ImageData samplesLen: " << id->getSamplesLength() << std::endl;
			std::cout << "ImageData dataType: " << id->getDataType() << std::endl;

			// sampledVolVol from sfGeom.
			SampledVolume* sv = sfGeom->getSampledVolume(0);
			std::cout << "SampledVol spatialId: " << sv->getSpatialId() << std::endl;
			std::cout << "SampledVol domainType: " << sv->getDomainType() << std::endl;
			std::cout << "SampledVol sampledVal: " << sv->getSampledValue() << std::endl;
			std::cout << "SampledVol min: " << sv->getMinValue() << std::endl;
			std::cout << "SampledVol max: " << sv->getMaxValue() << std::endl;
		}
		if (gd->isParametricGeometry()) {
			ParametricGeometry* pGeom = static_cast<ParametricGeometry*>(gd);
			std::cout << "ParametricGeometry spatialId: " << pGeom->getSpatialId() << std::endl;
			
			// parametricObject from pGeom
			ParametricObject* pObj = pGeom->getParametricObject(0);
			std::cout << "ParametricObj spatialId: " << pObj->getSpatialId() << std::endl;
			std::cout << "ParametricObj domain: " << pObj->getDomain() << std::endl;
			std::cout << "ParametricObj polygonType: " << pObj->getPolygonType() << std::endl;
			const PolygonObject* po = pObj->getPolygonObject();
			int* ptInd = new int[po->getIndicesLength()];
			po->getPointIndices(ptInd);
			std::cout << "PolygonObj ptIndices[0]: " << ptInd[0] << std::endl;
			std::cout << "PolygonObj indLen: " << po->getIndicesLength() << std::endl;

			// SpatialPoint from pGeom.
			SpatialPoint* sp = pGeom->getSpatialPoint(0);
			std::cout << "SpatialPt spatialId: " << sp->getSpatialId() << std::endl;
			std::cout << "SpatialPt domain: " << sp->getDomain() << std::endl;
			std::cout << "SpatialPt coord1: " << sp->getCoord1() << std::endl;
			std::cout << "SpatialPt coord2: " << sp->getCoord2() << std::endl;
			std::cout << "SpatialPt coord3: " << sp->getCoord3() << std::endl;
		}
		if (gd->isCSGeometry()) {
			CSGeometry* csGeom = static_cast<CSGeometry*>(gd);
			std::cout << "CSGeometry spatialId: " << csGeom->getSpatialId() << std::endl;
			
			// CSGObject-CSGOperator from csGeom
			CSGObject* csgo;
			for (unsigned int i = 0; i < csGeom->getNumCSGObjects(); i++) {
				csgo = csGeom->getCSGObject(i);
				std::cout << "CSGObject spatialId: " << csgo->getSpatialId() << std::endl;
				std::cout << "CSGObject domainType: " << csgo->getDomainType() << std::endl;
				const CSGNode* csgnode = csgo->getCSGNodeRoot();
				if (csgnode->isCSGTransformation()) {
					CSGTransformation* transf = (CSGTransformation*)csgnode;
					if (transf->isCSGScale()) {
						CSGScale* scale = static_cast<CSGScale*>(transf);
						std::cout << "CSGScale scaleX: " << scale->getScaleX() << std::endl;
						std::cout << "CSGScale scaleY: " << scale->getScaleY() << std::endl;
						std::cout << "CSGScale scaleZ: " << scale->getScaleZ() << std::endl;
						const CSGNode* scaleChild = scale->getChild();
						if (scaleChild->isCSGPrimitive()) {
							CSGPrimitive* prim = (CSGPrimitive*)scaleChild;
							std::cout << "CSGPrimitive primitiveType: " << prim->getPrimitiveType() << std::endl;
						}
					}
				}
				if (csgnode->isCSGSetOperator()) {
					CSGSetOperator* setop = (CSGSetOperator*)(csgnode);
					std::cout << "CSGSetOperator opType: " << setop->getOperationType() << std::endl;
					for (unsigned int k = 0; k < setop->getNumCSGNodeChildren(); k++) {
						CSGNode* csgNode = setop->getCSGNodeChild(k);
						std::cout << "CSGNode type: " << csgNode->getTypeCode() << std::endl;
					}
				}
			}
		}
	}

	delete document2;
}