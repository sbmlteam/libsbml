
#include <iostream>

#include <sbml/SBMLTypes.h>

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

void writeSpatialSBML();
void readSpatialSBML();

int main(int argc, char* argv[])
{
	writeSpatialSBML();
	readSpatialSBML();
}

void writeSpatialSBML() 
{
  // SBMLNamespaces of SBML Level 3 Version 1 with 'req' Version 1
  // then add 'spatial' package namespace.
  SpatialPkgNamespaces sbmlns(3,1,1);

  // create the L3V1 document with spatial package
  SBMLDocument document(&sbmlns);	


  // set 'required' attribute on document for 'spatial' and 'req' packages to 'T'??
  SBMLDocumentPlugin* dplugin;
  dplugin = static_cast<SBMLDocumentPlugin*>(document.getPlugin("spatial"));
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
  SpatialSpeciesPlugin* srplugin;
  srplugin = static_cast<SpatialSpeciesPlugin*>(species1->getPlugin("spatial"));
  srplugin->setIsSpatial(true);

  // add parameter for diff coeff of species1
  Parameter* paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_dc");
  paramSp->setValue(1.0);
  // spatial package extension to parameter.
  SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  DiffusionCoefficient* diffCoeff = pplugin->createDiffusionCoefficient();
  diffCoeff->setVariable(species1->getId());
  diffCoeff->setType(SPATIAL_DIFFUSIONKIND_ANISOTROPIC);
  diffCoeff->setCoordinateReference1(SPATIAL_COORDINATEKIND_CARTESIAN_X);
  // add parameter for adv coeff of species1
  paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_ac");
  paramSp->setValue(1.5);
  // spatial package extension to parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  AdvectionCoefficient* advCoeff = pplugin->createAdvectionCoefficient();
  advCoeff->setVariable(species1->getId());
  advCoeff->setCoordinate(SPATIAL_COORDINATEKIND_CARTESIAN_X);
  // add parameter for boundary condition of species1
  paramSp = model->createParameter();
  paramSp->setId(species1->getId()+"_bc");
  paramSp->setValue(2.0);
  // spatial package extension to parameter.
  pplugin = static_cast<SpatialParameterPlugin*>(paramSp->getPlugin("spatial"));
  BoundaryCondition* boundCon = pplugin->createBoundaryCondition();
  boundCon->setVariable(species1->getId());  
  boundCon->setType(SPATIAL_BOUNDARYKIND_DIRICHLET);
  boundCon->setCoordinateBoundary("Xmin");

  Species* species2 = model->createSpecies();
  species2->setId("ADPc");
  species2->setCompartment("cytosol");
  species2->setInitialConcentration(1);
  species2->setHasOnlySubstanceUnits(false);
  species2->setBoundaryCondition(false);
  species2->setConstant(false);
  srplugin = static_cast<SpatialSpeciesPlugin*>(species2->getPlugin("spatial"));
  srplugin->setIsSpatial(true);

  Reaction* reaction = model->createReaction();
  reaction->setId("rxn1");
  reaction->setReversible(false);
  reaction->setFast(false);
  reaction->setCompartment("cytosol");
  SpatialReactionPlugin* rplugin = static_cast<SpatialReactionPlugin*>(reaction->getPlugin("spatial"));
  rplugin->setIsLocal(true);

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
  Geometry* geometry = mplugin->createGeometry();
  geometry->setCoordinateSystem(SPATIAL_GEOMETRYKIND_CARTESIAN);

  CoordinateComponent* coordX = geometry->createCoordinateComponent();
  coordX->setId("coordComp1");
  coordX->setType(SPATIAL_COORDINATEKIND_CARTESIAN_X);
  coordX->setUnit("umeter");
  Boundary* minX = coordX->createBoundaryMin();
  minX->setId("Xmin");
  minX->setValue(0.0);
  Boundary* maxX = coordX->createBoundaryMax();
  maxX->setId("Xmax");
  maxX->setValue(10.0);

  Parameter* paramX = model->createParameter();
  paramX->setId("x");
  paramX->setValue(8.0);
  // spatial package extension to parameter.
  // SpatialParameterPlugin* pplugin;
  pplugin = static_cast<SpatialParameterPlugin*>(paramX->getPlugin("spatial"));
  SpatialSymbolReference* spSymRef = pplugin->createSpatialSymbolReference();
  spSymRef->setSpatialRef(coordX->getId());

  DomainType* domainType = geometry->createDomainType();
  domainType->setId("dtype1");
  domainType->setSpatialDimensions(3);

  // Spatial package extension to compartment (mapping compartment with domainType)
  SpatialCompartmentPlugin* cplugin;
  cplugin = static_cast<SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  CompartmentMapping* compMapping = cplugin->createCompartmentMapping();
  compMapping->setId("compMap1");
  compMapping->setDomainType(domainType->getId());
  compMapping->setUnitSize(1.0);
  
  Domain* domain = geometry->createDomain();
  domain->setId("domain1");
  domain->setDomainType("dtype1");
  InteriorPoint* internalPt1 = domain->createInteriorPoint();
  internalPt1->setCoord1(1.0);

  domain = geometry->createDomain();
  domain->setId("domain2");
  domain->setDomainType("dtype1");
  InteriorPoint* internalPt2 = domain->createInteriorPoint();
  internalPt2->setCoord1(5.0);

  AdjacentDomains* adjDomain = geometry->createAdjacentDomains();
  adjDomain->setId("adjDomain1");
  adjDomain->setDomain1("domain1");
  adjDomain->setDomain2("domain2");

  AnalyticGeometry* analyticGeom = geometry->createAnalyticGeometry();
  analyticGeom->setId("analyticGeom1");
  AnalyticVolume* analyticVol = analyticGeom->createAnalyticVolume();
  analyticVol->setId("analyticVol1");
  analyticVol->setDomainType(domainType->getId());
  analyticVol->setFunctionType(SPATIAL_FUNCTIONKIND_LAYERED);
  analyticVol->setOrdinal(1);
  const char* mathMLStr = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply xmlns=\"\"><plus /><apply><times /><ci>x</ci><ci>x</ci></apply><apply><minus /><cn>1.0</cn></apply></apply></math>";
  ASTNode* mathNode = readMathMLFromString(mathMLStr);
  analyticVol->setMath(mathNode);

  SampledFieldGeometry* sfg = geometry->createSampledFieldGeometry();
  sfg->setId("sampledFieldGeom1");
  sfg->setSampledField("sampledField1");
  SampledField* sampledField = geometry->createSampledField();
  sampledField->setId("sampledField1");
  sampledField->setNumSamples1(4);
  sampledField->setNumSamples2(4);
  sampledField->setNumSamples3(2);
  sampledField->setInterpolationType("linear");
  sampledField->setCompression("uncompressed");
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
  sampledField->setDataType("uint8");
  sampledField->setSamples(samples, 32);
  SampledVolume* sampledVol = sfg->createSampledVolume();
  sampledVol->setId("sv_1");
  sampledVol->setDomainType(domainType->getId());
  sampledVol->setSampledValue(128.0);
  sampledVol->setMinValue(0.0);
  sampledVol->setMaxValue(255.0);
  
  writeSBML(&document, "spatial_example2.xml");

}
void readSpatialSBML() {
	SBMLDocument *document2 = readSBML("spatial_example2.xml");
  
	Model *model2 = document2->getModel();
	Compartment *comp;
	SpatialCompartmentPlugin* cplugin;
	for (unsigned int i = 0; i < model2->getNumCompartments(); i++) {
		comp = model2->getCompartment(i);
		cout << "Compartment" << i << ": "  << comp->getId() << endl;
		cplugin = static_cast<SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
		if (cplugin->getCompartmentMapping()->isSetId()) {
			cout << "Comp" << i << "  CMSpId: "  << cplugin->getCompartmentMapping()->getId() << endl;
			cout << "Comp" << i << "  CM_DType: "  << cplugin->getCompartmentMapping()->getDomainType() << endl;
			cout << "Comp" << i << "  CM_UnitSz: "  << cplugin->getCompartmentMapping()->getUnitSize() << endl;
		}
	}

	Species *sp;
	SpatialSpeciesPlugin* srplugin;
	for (unsigned int i = 0; i < model2->getNumSpecies(); i++) {
		sp = model2->getSpecies(i);
		cout << "Species" << i << ": "      << sp->getId()      << endl;
		srplugin = static_cast<SpatialSpeciesPlugin*>(sp->getPlugin("spatial"));
		if (srplugin->getIsSpatial()) {
			cout << "species" << i << "  isSpatial: "  << srplugin->getIsSpatial() << endl;
		}
	}

	Parameter *param;
	SpatialParameterPlugin* pplugin;
	for (unsigned int i = 0; i < model2->getNumParameters(); i++) {
		param = model2->getParameter(i);
		cout << "Parameter" << i << ": "  << param->getId() << endl;
		pplugin = static_cast<SpatialParameterPlugin*>(param->getPlugin("spatial"));
		if (pplugin->isSetSpatialSymbolReference()) {
			cout << "Parameter" << i << "  SpRefId: "  << pplugin->getSpatialSymbolReference()->getSpatialRef() << endl;
		}
		if (pplugin->isSetDiffusionCoefficient()) {
			cout << "Diff_" << i << "  SpeciesVarId: "  << pplugin->getDiffusionCoefficient()->getVariable() << endl;
			cout << "Diff_" << i << "  Type: "  << DiffusionKind_toString(pplugin->getDiffusionCoefficient()->getType()) << endl;
      if (pplugin->getDiffusionCoefficient()->isSetCoordinateReference1())
        cout << "Diff_" << i << "  CoordinateReference1 : " << CoordinateKind_toString(pplugin->getDiffusionCoefficient()->getCoordinateReference1()) << endl;
      if (pplugin->getDiffusionCoefficient()->isSetCoordinateReference2())
        cout << "Diff_" << i << "  CoordinateReference2 : " << CoordinateKind_toString(pplugin->getDiffusionCoefficient()->getCoordinateReference2()) << endl;
		}
		if (pplugin->isSetAdvectionCoefficient()) {
			cout << "Adv_" << i << "  SpeciesVarId: "  << pplugin->getAdvectionCoefficient()->getVariable() << endl;
			cout << "Adv_" << i << "  SpCoordIndex: "  << CoordinateKind_toString(pplugin->getAdvectionCoefficient()->getCoordinate()) << endl;
		}
		if (pplugin->isSetBoundaryCondition()) {
			cout << "BC_" << i << "  SpeciesVarId: "  << pplugin->getBoundaryCondition()->getVariable() << endl;
			cout << "BC_" << i << "  SpCoordBoundary: "  << pplugin->getBoundaryCondition()->getCoordinateBoundary() << endl;
			cout << "BC_" << i << "  SpBoundaryType: "  << pplugin->getBoundaryCondition()->getType() << endl;
		}
	}

	Reaction *rxn;
	SpatialReactionPlugin* rplugin;
	for (unsigned int i = 0; i < model2->getNumReactions(); i++) {
		rxn = model2->getReaction(i);
		cout << "Reaction" << i << ": "      << rxn->getId()      << endl;
		rplugin = static_cast<SpatialReactionPlugin*>(rxn->getPlugin("spatial"));
		if (rplugin->getIsLocal()) {
			cout << "rxn" << i << "  isLocal: "  << rplugin->getIsLocal() << endl;
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
	std::cout << "CoordComponent Id: " << coordComp->getId() << std::endl;
  std::cout << "CoordComponent type: " << CoordinateKind_toString( coordComp->getType()) << std::endl;
	std::cout << "CoordComponent sbmlUnit: " << coordComp->getUnit() << std::endl;
  if (coordComp->isSetBoundaryMin())
  {
  Boundary* minX = coordComp->getBoundaryMin();
	std::cout << "minX name: " << minX->getId() << std::endl;
	std::cout << "minX value: " << minX->getValue() << std::endl;
  }
  if (coordComp->isSetBoundaryMax())
  {
	Boundary* maxX = coordComp->getBoundaryMax();
	std::cout << "maxX name: " << maxX->getId() << std::endl;
	std::cout << "maxX value: " << maxX->getValue() << std::endl;
  }

	// get a DomainType object via the Geometry object.	
	DomainType* domainType2 = geometry2->getDomainType(0);
	std::cout << "DomainType Id: " << domainType2->getId() << std::endl;
	std::cout << "DomainType spatialDim: " << domainType2->getSpatialDimensions() << std::endl;

	// get a Domain object via the Geometry object.	
	Domain* domain = geometry2->getDomain(0);
	std::cout << "Domain1 Id: " << domain->getId() << std::endl;
	std::cout << "Domain1 domainType: " << domain->getDomainType() << std::endl;
	// get an internal point via the domain object
	InteriorPoint* internalPt = domain->getInteriorPoint(0);
	std::cout << "InternalPt_1 coord1: " << internalPt->getCoord1() << std::endl;

	// get a Domain object via the Geometry object.	
	domain = geometry2->getDomain(1);
	std::cout << "Domain2 Id: " << domain->getId() << std::endl;
	std::cout << "Domain2 domainType: " << domain->getDomainType() << std::endl;
	// get an internal point via the domain object
	internalPt = domain->getInteriorPoint(0);
	std::cout << "InternalPt_2 coord1: " << internalPt->getCoord1() << std::endl;

	// get an AdjacentDomains object via the Geometry object.	
	AdjacentDomains* adjDomain = geometry2->getAdjacentDomains(0);
	std::cout << "AdjDomain Id: " << adjDomain->getId() << std::endl;
	std::cout << "AdjDomain domain1: " << adjDomain->getDomain1() << std::endl;
	std::cout << "AdjDomain domain2: " << adjDomain->getDomain2() << std::endl;

	// get an AnalyticGeometry object via the Geometry object.
	GeometryDefinition* gd;
	for (unsigned int i = 0; i < geometry2->getNumGeometryDefinitions(); i++) {
		gd = geometry2->getGeometryDefinition(i);
		if (gd->isAnalyticGeometry()) {
			AnalyticGeometry* analyticalGeom = static_cast<AnalyticGeometry*>(gd);
			std::cout << "AnalGeom Id: " << analyticalGeom->getId() << std::endl;

			// analVol from analGeom.
			AnalyticVolume* av = analyticalGeom->getAnalyticVolume(0);
			std::cout << "AnalVol Id: " << av->getId() << std::endl;
			std::cout << "AnalVol domainType: " << av->getDomainType() << std::endl;
			std::cout << "AnalVol funcType: " << av->getFunctionType() << std::endl;
			std::cout << "AnalVol ordinal: " << av->getOrdinal() << std::endl;
			const ASTNode* mathNode = av->getMath();
			char* mathStr = writeMathMLToString(mathNode);
			std::cout << "AnalVol math: " << mathStr << std::endl;
		}
		if (gd->isSampledFieldGeometry()) {
			SampledFieldGeometry* sfGeom = static_cast<SampledFieldGeometry*>(gd);
			std::cout << "SampledFieldGeom Id: " << sfGeom->getId() << std::endl;
      std::cout << "SampledFieldGeom sampledField: " << sfGeom->getSampledField() << std::endl;
			
			// sampledField from sfGeom
      SampledField* sf = geometry2->getSampledField(sfGeom->getSampledField());
			std::cout << "SampledField Id: " << sf->getId() << std::endl;
			std::cout << "SampledField dataType: " << DataKind_toString(sf->getDataType()) << std::endl;
			std::cout << "SampledField interpolation: " << InterpolationKind_toString(sf->getInterpolationType()) << std::endl;
			std::cout << "SampledField compression: " << CompressionKind_toString( sf->getCompression() ) << std::endl;
			std::cout << "SampledField numSamples1: " << sf->getNumSamples1() << std::endl;
			std::cout << "SampledField numSamples2: " << sf->getNumSamples2() << std::endl;
			std::cout << "SampledField numSamples3: " << sf->getNumSamples3() << std::endl;
			int* samples = new int[sf->getSamplesLength()];
			sf->getSamples(samples);
			std::cout << "SampledField samples[0]: " << samples[0] << std::endl;
			std::cout << "SampledField samplesLen: " << sf->getSamplesLength() << std::endl;
      
			// sampledVolVol from sfGeom.
			SampledVolume* sv = sfGeom->getSampledVolume(0);
			std::cout << "SampledVol Id: " << sv->getId() << std::endl;
			std::cout << "SampledVol domainType: " << sv->getDomainType() << std::endl;
			std::cout << "SampledVol sampledVal: " << sv->getSampledValue() << std::endl;
			std::cout << "SampledVol min: " << sv->getMinValue() << std::endl;
			std::cout << "SampledVol max: " << sv->getMaxValue() << std::endl;
		}
	}

	delete document2;
}
