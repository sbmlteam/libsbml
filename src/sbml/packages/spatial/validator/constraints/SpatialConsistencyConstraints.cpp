
/** @cond doxygenLibsbmlInternal */

/**
 * @file SpatialConsistencyConstraints.cpp
 * @brief Definition of SpatialConsistencyConstraints.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
#ifndef AddingConstraintsToValidator
#include <set>
#include <math.h>

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/sbml/Geometry.h>
#include <sbml/packages/spatial/validator/SpatialCompartmentMappingUnitSizesCheck.h>
#include <sbml/packages/spatial/validator/SpatialOrdinalMappingGeometryDefinitionCheck.h>
#include <sbml/packages/spatial/validator/SpatialSampledVolumeRangeOverlapCheck.h>
#include <sbml/packages/spatial/validator/SpatialSampledVolumeValueNotInRangeCheck.h>
#include <sbml/packages/spatial/validator/SpatialSpatialSymbolReferenceUniqueRefCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueAdvectionCoefficientsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueAnalyticVolumeOrdinalsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueBoundaryConditionsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueCSGObjectOrdinalsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueDiffusionCoefficientsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueOrdinalMappingOrdinalsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueSampledVolumeValueCheck.h>
#include <sbml/Species.h>

#endif /* AddingConstraintsToValidator */

#include <sbml/validator/ConstraintMacros.h>

using namespace std;

EXTERN_CONSTRAINT(ErrorEnumValue, SpatialCompartmentMappingUnitSizesCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialSpatialSymbolReferenceUniqueRefCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueDiffusionCoefficientsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueAdvectionCoefficientsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueAnalyticVolumeOrdinalsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueBoundaryConditionsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueCSGObjectOrdinalsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialOrdinalMappingGeometryDefinitionCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueOrdinalMappingOrdinalsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueSampledVolumeValueCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialSampledVolumeValueNotInRangeCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialSampledVolumeRangeOverlapCheck);



// 1220805
START_CONSTRAINT(SpatialDomainDomainTypeMustBeDomainType, Domain, domain)
{
  pre(domain.isSetDomainType());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));

  pre(plug != NULL);

  pre(plug->isSetGeometry());

  bool fail = false;

  const std::string dt = domain.getDomainType();

  msg = "Domain";
  if (domain.isSetId())
  {
    msg += " with id '";
    msg += domain.getId();
    msg += "'";
  }
  msg += " has 'domainType' set to '";
  msg += domain.getDomainType();
  msg += "' which is not the id of a DomainType object in the model.";

  if (plug->getGeometry()->getDomainType(dt) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221105
START_CONSTRAINT(SpatialAdjacentDomainsDomain1MustBeDomain, AdjacentDomains, domain)
{
  pre(domain.isSetDomain1());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));

  pre(plug != NULL);

  pre(plug->isSetGeometry());

  bool fail = false;

  const std::string dt = domain.getDomain1();

  msg = "AdjacentDomain";
  if (domain.isSetId())
  {
    msg += " with id '";
    msg += domain.getId();
    msg += "'";
  }
  msg += " has 'domain1' set to '";
  msg += domain.getDomain1();
  msg += "' which is not the id of a Domain object in the model.";

  if (plug->getGeometry()->getDomain(dt) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1221104
START_CONSTRAINT(SpatialAdjacentDomainsDomain2MustBeDomain, AdjacentDomains, domain)
{
  pre(domain.isSetDomain2());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));

  pre(plug != NULL);

  pre(plug->isSetGeometry());

  bool fail = false;

  const std::string dt = domain.getDomain2();
  msg = "AdjacentDomains";
  if (domain.isSetId())
  {
    msg += " with id '";
    msg += domain.getId();
    msg += "'";
  }
  msg += " has 'domain2' set to '";
  msg += domain.getDomain2();
  msg += "' which is not the id of a Domain object in the model.";

  if (plug->getGeometry()->getDomain(dt) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1221304
START_CONSTRAINT(SpatialCompartmentMappingDomainTypeMustBeDomainType, CompartmentMapping, mapping)
{
  pre(mapping.isSetDomainType());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));

  pre(plug != NULL);

  pre(plug->isSetGeometry());

  bool fail = false;

  const std::string dt = mapping.getDomainType();
  msg = "CompartmentMapping";
  if (mapping.isSetId())
  {
    msg += " with id '";
    msg += mapping.getId();
    msg += "'";
  }
  msg += " has 'domainType' set to '";
  msg += dt;
  msg += "' which is not the id of a DomainType object in the model.";

  if (plug->getGeometry()->getDomainType(dt) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221404
START_CONSTRAINT(SpatialCoordinateComponentAllowedElements, CoordinateComponent, cc)
{
  bool fail = false;

  msg = "CoordinateComponent";
  if (cc.isSetId())
  {
    msg += " with id '";
    msg += cc.getId();
    msg += "'";
  }

  if (cc.isSetBoundaryMax() == false)
  {
    if (cc.isSetBoundaryMin() == false)
    {
      fail = true;
      msg += " is missing both a boundaryMin and boundaryMax element.";
    }
    else
    {
      fail = true;
      msg += " is missing a boundaryMax element.";
    }
  }
  else if (cc.isSetBoundaryMin() == false)
  {
    fail = true;
    msg += " is missing a boundaryMin element.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221406
START_CONSTRAINT(SpatialCoordinateComponentUnitMustBeUnitSId, CoordinateComponent, cc)
{
  pre(cc.isSetUnit());

  bool fail = false;
  const string& units = cc.getUnit();

  msg = "CoordinateComponent";
  if (cc.isSetId())
  {
    msg += " with id '";
    msg += cc.getId();
    msg += "'";
  }
  msg += " uses a unit value of '";
  msg += units;
  msg += "' which is not a built in unit or the identifier of an existing <unitDefinition>.";

  if (!Unit::isUnitKind(units, cc.getLevel(), cc.getVersion()))
  {
    const UnitDefinition *ud = m.getUnitDefinition(units);
    if (ud == NULL)
    {
      fail = true;
    }
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221505
START_CONSTRAINT(SpatialSampledFieldGeometrySampledFieldMustBeSampledField, SampledFieldGeometry, sfg)
{
  pre(sfg.isSetSampledField());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));

  pre(plug != NULL);

  pre(plug->isSetGeometry());

  bool fail = false;

  const std::string sf = sfg.getSampledField();
  msg = "SampledFieldGeometry";
  if (sfg.isSetId())
  {
    msg += " with id '";
    msg += sfg.getId();
    msg += "'";
  }
  msg += " has 'sampledField' set to '";
  msg += sf;
  msg += "' which is not the id of a SampledField object in the model.";

  if (plug->getGeometry()->getSampledField(sf) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220650
START_CONSTRAINT(SpatialLocalReactionMustDefineCompartment, Reaction, rxn)
{
  const SpatialReactionPlugin* rxnplugin = static_cast<const SpatialReactionPlugin*>(rxn.getPlugin("spatial"));

  pre(rxnplugin != NULL);

  pre(rxnplugin->isSetIsLocal());
  pre(rxnplugin->getIsLocal() == true);

  bool fail = false;

  if (!rxn.isSetCompartment())
  {
    fail = true;
    msg = "A Reaction";
    if (rxn.isSetId())
    {
      msg += " with id '";
      msg += rxn.getId();
      msg += "'";
    }
    msg += " has a 'spatial:isLocal' attribute of 'true', but does not define the 'compartment' attribute.";
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1220750
START_CONSTRAINT(SpatialDomainTypeDimensionsMustMatch3DGeometry, DomainType, domaintype)
{
  pre(domaintype.isSetSpatialDimensions());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==3);

  int dim = domaintype.getSpatialDimensions();
  bool fail = false;
  if (dim < 2 || dim > 3) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A DomainType";
    if (domaintype.isSetId())
    {
      ss_msg << " with id '" << domaintype.getId() << "'";
    }
    ss_msg << " has a 'spatial:spatialDimensions' attribute of '";
    ss_msg << dim;
    ss_msg << "', but the ListOfCoordinateComponents has exactly three children.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220751
START_CONSTRAINT(SpatialDomainTypeDimensionsMustMatch2DGeometry, DomainType, domaintype)
{
  pre(domaintype.isSetSpatialDimensions());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==2);

  int dim = domaintype.getSpatialDimensions();
  bool fail = false;
  if (dim < 1 || dim > 2) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A DomainType";
    if (domaintype.isSetId())
    {
      ss_msg << " with id '" << domaintype.getId() << "'";
    }
    ss_msg << " has a 'spatial:spatialDimensions' attribute of '";
    ss_msg << dim;
    ss_msg << "', but the ListOfCoordinateComponents has exactly two children.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220752
START_CONSTRAINT(SpatialDomainTypeDimensionsMustMatch1DGeometry, DomainType, domaintype)
{
  pre(domaintype.isSetSpatialDimensions());

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==1);

  int dim = domaintype.getSpatialDimensions();
  bool fail = false;
  if (dim < 0 || dim > 1) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A DomainType";
    if (domaintype.isSetId())
    {
      ss_msg << " with id '" << domaintype.getId() << "'";
    }
    ss_msg << " has a 'spatial:spatialDimensions' attribute of '";
    ss_msg << dim;
    ss_msg << "', but the ListOfCoordinateComponents has exactly one child.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221650
START_CONSTRAINT(SpatialSampledFieldOneSampleIn1DGeometry, SampledField, sfield)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==1);

  bool fail = false;
  stringstream wrongvals;
  if (sfield.isSetNumSamples2()) {
    fail = true;
    wrongvals << "numSamples2 with a value of '";
    wrongvals << sfield.getNumSamples2();
    wrongvals << "'";
  }
  if (sfield.isSetNumSamples3()) {
    if (fail) {
      wrongvals << ", and a ";
    }
    wrongvals << "numSamples3 with a value of '";
    wrongvals << sfield.getNumSamples3();
    wrongvals << "'";
    fail = true;
  }
  if (fail) {
    msg = "A SampledField";
    if (sfield.isSetId())
    {
      msg += " with id '" + sfield.getId() + "'";
    }
    msg += " defines a " + wrongvals.str();
    msg += ", but the ListOfCoordinateComponents has exactly one child.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221651
START_CONSTRAINT(SpatialSampledFieldTwoSamplesIn2DGeometry, SampledField, sfield)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==2);

  bool fail = false;
  stringstream wrongvals;
  if (!sfield.isSetNumSamples2()) {
    fail = true;
    wrongvals << " doesn't define the numSamples2 attribute";
  }
  if (sfield.isSetNumSamples3()) {
    if (fail) {
      wrongvals << ", and has a ";
    }
    else {
      wrongvals << " defines a ";
    }
    wrongvals << "numSamples3 with a value of '";
    wrongvals << sfield.getNumSamples3();
    wrongvals << "'";
    fail = true;
  }
  if (fail) {
    msg = "A SampledField";
    if (sfield.isSetId())
    {
      msg += " with id '" + sfield.getId() + "'";
    }
    msg += wrongvals.str();
    msg += ", but the ListOfCoordinateComponents has exactly two children.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221652
START_CONSTRAINT(SpatialSampledFieldThreeSamplesIn3DGeometry, SampledField, sfield)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==3);

  bool fail = false;
  stringstream wrongvals;
  if (!sfield.isSetNumSamples2()) {
    fail = true;
    wrongvals << " doesn't define the numSamples2 attribute";
  }
  if (!sfield.isSetNumSamples3()) {
    if (fail) {
      wrongvals << ", and also";
    }
    wrongvals << " doesn't define the numSamples3 attribute";
    fail = true;
  }
  if (fail) {
    msg = "A SampledField";
    if (sfield.isSetId())
    {
      msg += " with id '" + sfield.getId() + "'";
    }
    msg += wrongvals.str();
    msg += ", but the ListOfCoordinateComponents has exactly three children.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221750
START_CONSTRAINT(SpatialSampledVolumeSampledValueMinMax, SampledVolume, svol)
{
  bool fail = false;
  if (svol.isSetMinValue() && !svol.isSetMaxValue() && !svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a minValue of '" << svol.getMinValue() << "', but does not set the maxValue attribute.";
    msg = ss_msg.str();
  }

  else if (svol.isSetMaxValue() && !svol.isSetMinValue() && !svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a maxValue of '" << svol.getMaxValue() << "', but does not set the minValue attribute.";
    msg = ss_msg.str();
  }

  else if (svol.isSetMaxValue() && svol.isSetMinValue() && svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a minValue of '" << svol.getMinValue() << "'";
    ss_msg << " and a maxValue of '" << svol.getMaxValue() << "',";
    ss_msg << " but also has a sampledValue of '" << svol.getSampledValue() << "'.  Either use the sampledValue, or the minValue with the maxValue.";
    msg = ss_msg.str();
  }

  else if (svol.isSetMaxValue() && !svol.isSetMinValue() && svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a sampledValue of '" << svol.getSampledValue() << "'";
    ss_msg << " but also has a maxValue of '" << svol.getMaxValue() << "'.  Either use the sampledValue, or a minValue with the maxValue.";
    msg = ss_msg.str();
  }

  else if (!svol.isSetMaxValue() && svol.isSetMinValue() && svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a sampledValue of '" << svol.getSampledValue() << "'";
    ss_msg << " but also has a minValue of '" << svol.getMinValue() << "'.  Either use the sampledValue, or a maxValue with the minValue.";
    msg = ss_msg.str();
  }

  else if (!svol.isSetMaxValue() && !svol.isSetMinValue() && !svol.isSetSampledValue()) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A SampledVolume";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " does not define a sampledValue, nor does it define a minValue and a maxValue.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221350
START_CONSTRAINT(SpatialCompartmentMappingUnitSizeMustBeFraction, CompartmentMapping, cmap)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  const Compartment *pComp = dynamic_cast<const Compartment*>(cmap.getParentSBMLObject());
  pre(pComp != NULL);
  pre(plug->isSetGeometry());
  const Compartment* pOtherComp = NULL;
  for (unsigned int i = 0; i < m.getNumCompartments(); ++i)
  {
     const Compartment* comp = m.getCompartment(i); 
     if (!comp) continue;
     const SpatialCompartmentPlugin *compPlugin = dynamic_cast<const SpatialCompartmentPlugin *>(comp->getPlugin("spatial"));
     if (!compPlugin) continue;
     const CompartmentMapping* otherMapping = compPlugin->getCompartmentMapping();
     if (!otherMapping) continue;
     if (otherMapping->getDomainType() != cmap.getDomainType()) continue;
     pOtherComp = comp;
     break;    
  }
  pre(pOtherComp != NULL);
  
  // this constraint should only apply when 
  // the domainType's compartment has the same dimension as the mapped compartment
  pre(pComp->getSpatialDimensions() == pOtherComp->getSpatialDimensions());

  bool fail = false;
  if (cmap.isSetUnitSize() && (cmap.getUnitSize() > 1 || cmap.getUnitSize() < 0)) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A CompartmentMapping";
    if (cmap.isSetId())
    {
      ss_msg << " with id '" << cmap.getId() << "'";
    }
    ss_msg << " has a unitSize of " << cmap.getUnitSize() << ".";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220450
START_CONSTRAINT(SpatialCompartmentsMustHaveCompartmentMapping, Species, species)
{
  bool fail = false;
  pre(species.isSetCompartment());

  const SpatialSpeciesPlugin* ssp = static_cast<const SpatialSpeciesPlugin*>(species.getPlugin("spatial"));
  pre(ssp->isSetIsSpatial() && ssp->getIsSpatial());

  const Compartment* compartment = m.getCompartment(species.getCompartment());
  pre(compartment != NULL);

  const SpatialCompartmentPlugin* scp = static_cast<const SpatialCompartmentPlugin*>(compartment->getPlugin("spatial"));
  if (scp == NULL || scp->isSetCompartmentMapping() == false) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A species";
    if (species.isSetId())
    {
      ss_msg << " with id '" << species.getId() << "'";
    }
    ss_msg << " is set 'isSpatial=true', but its compartment ('" << species.getCompartment() << "') does not have a child <compartmentMapping>.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223304
START_CONSTRAINT(SpatialSpatialSymbolReferenceSpatialRefMustReferenceMath, SpatialSymbolReference, ssr)
{
  pre(ssr.isSetSpatialRef());
  msg = "A <spatialSymbolReference>";
  if (ssr.isSetId()) {
    msg += " with the id '" + ssr.getId() + "'";
  }
  msg += " has a 'spatialRef' value of '" + ssr.getSpatialRef() + "'";
  bool fail = false;

  Model* model = const_cast<Model*>(&m);
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const SBase* ref = plug->getGeometry()->getElementBySId(ssr.getSpatialRef());
  if (ref == NULL) {
    ref = model->getElementBySId(ssr.getSpatialRef());
    if (ref == NULL) {
      fail = true;
      msg += ", but no object with that id could be found.";
    }
  }
  if (!fail) {
    switch (ref->getTypeCode()) {
    case SBML_SPATIAL_COMPARTMENTMAPPING:
    case SBML_SPATIAL_COORDINATECOMPONENT:
    case SBML_SPATIAL_BOUNDARY:
    case SBML_SPATIAL_DOMAINTYPE:
    case SBML_SPATIAL_DOMAIN:
    case SBML_SPATIAL_SAMPLEDFIELD:
      return;
    }
    fail = true;
    msg += ", which is not a spatial element with mathematical meaning.";
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223450
START_CONSTRAINT(SpatialDiffusionCoefficientNoCoordinateReferencesForIsotropic, DiffusionCoefficient, dc)
{
  bool fail = false;
  pre(dc.getType() == SPATIAL_DIFFUSIONKIND_ISOTROPIC);
  if (dc.isSetCoordinateReference1() && dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'isotropic', but defines both 'coordinateReference1' and 'coordinateReference2'.";
  }
  else if (dc.isSetCoordinateReference1()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'isotropic', but defines 'coordinateReference1'.";
  }
  else if (dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'isotropic', but defines 'coordinateReference2'.";
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223451
START_CONSTRAINT(SpatialDiffusionCoefficientTwoCoordinateReferencesForTensor, DiffusionCoefficient, dc)
{
  bool fail = false;
  pre(dc.getType() == SPATIAL_DIFFUSIONKIND_TENSOR);
  if (!dc.isSetCoordinateReference1() && !dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'tensor', but doesn't define 'coordinateReference1' or 'coordinateReference2'.";
  }
  else if (!dc.isSetCoordinateReference1()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'tensor', but doesn't define 'coordinateReference1'.";
  }
  else if (!dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'tensor', but doesn't define 'coordinateReference2'.";
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223452
START_CONSTRAINT(SpatialDiffusionCoefficientOneCoordinateReferencesForAnisotropic, DiffusionCoefficient, dc)
{
  bool fail = false;
  pre(dc.getType() == SPATIAL_DIFFUSIONKIND_ANISOTROPIC);
  if (!dc.isSetCoordinateReference1() && dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'anisotropic', but defines 'coordinateReference2' instead of 'coordinateReference2'.";
  }
  else if (!dc.isSetCoordinateReference1()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'anisotropic', but doesn't define 'coordinateReference1'.";
  }
  else if (dc.isSetCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a 'type' of 'anisotropic', but defines 'coordinateReference2'.";
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223454
START_CONSTRAINT(SpatialDiffusionCoefficientCoordinateReferenceDifference, DiffusionCoefficient, dc)
{
  bool fail = false;
  pre(dc.isSetCoordinateReference1());
  pre(dc.isSetCoordinateReference2());
  if (dc.getCoordinateReference1() == dc.getCoordinateReference2()) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a value of '" + dc.getCoordinateReference1AsString() + "' for both 'coordinateReference1' and 'coordinateReference2'.";
  }

  inv(fail == false);
}
END_CONSTRAINT

// 1223455
START_CONSTRAINT(SpatialDiffusionCoefficientCoordinateReferenceNoYIn1D, DiffusionCoefficient, dc)
{
  bool fail = false;

  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getNumCoordinateComponents()==1);
  
  if (dc.isSetCoordinateReference1() && dc.getCoordinateReference1() == SPATIAL_COORDINATEKIND_CARTESIAN_Y) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a value of 'cartesianY' for 'coordinateReference1', but the <geometry> only has one <coordinateComponent> child.";
  }

  else if (dc.isSetCoordinateReference2() && dc.getCoordinateReference2() == SPATIAL_COORDINATEKIND_CARTESIAN_Y) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a value of 'cartesianY' for 'coordinateReference2', but the <geometry> only has one <coordinateComponent> child.";
  }


  inv(fail == false);
}
END_CONSTRAINT

// 1223456
START_CONSTRAINT(SpatialDiffusionCoefficientCoordinateReferenceNoZIn2D, DiffusionCoefficient, dc)
{
  bool fail = false;

  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getNumCoordinateComponents()<3);
  stringstream ncc;
  ncc << geom->getNumCoordinateComponents();

  if (dc.isSetCoordinateReference1() && dc.getCoordinateReference1() == SPATIAL_COORDINATEKIND_CARTESIAN_Z) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a value of 'cartesianZ' for 'coordinateReference1', but the <geometry> only has " + ncc.str() + " <coordinateComponent> child";
    if (geom->getNumCoordinateComponents() == 2) {
      msg += "ren";
    }
    msg += ".";
  }

  else if (dc.isSetCoordinateReference2() && dc.getCoordinateReference2() == SPATIAL_COORDINATEKIND_CARTESIAN_Z) {
    fail = true;
    msg = "A <diffusionCoefficient>";
    if (dc.isSetId()) {
      msg += " with the id '" + dc.getId() + "'";
    }
    msg += " has a value of 'cartesianZ' for 'coordinateReference2', but the <geometry> only has " + ncc.str() + " <coordinateComponent> child";
    if (geom->getNumCoordinateComponents() == 2) {
      msg += "ren";
    }
    msg += ".";
  }


  inv(fail == false);
}
END_CONSTRAINT

// 1223504
START_CONSTRAINT(SpatialAdvectionCoefficientVariableMustBeSpeciesOrParam, AdvectionCoefficient, ac)
{
  bool fail = false;
  pre(ac.isSetVariable());

  if (m.getSpecies(ac.getVariable()) == NULL && m.getParameter(ac.getVariable()) == NULL) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "An <advectionCoefficient>";
    if (ac.isSetId())
    {
      ss_msg << " with id '" << ac.getId() << "'";
    }
    ss_msg << " references a variable '" << ac.getVariable() << "', which is not the ID of a <species> or <parameter> in the <model>.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1223552
START_CONSTRAINT(SpatialAdvectionCoefficientVariableMustNotBeSelf, AdvectionCoefficient, ac)
{
    bool fail = false;
    pre(ac.isSetVariable());
    const SBase* parent = ac.getParentSBMLObject();
    pre(parent != NULL);
    pre(parent->getId() == ac.getVariable());

    stringstream ss_msg;
    ss_msg << "An <advectionCoefficient>";
    if (ac.isSetId())
    {
        ss_msg << " with id '" << ac.getId() << "'";
    }
    ss_msg << " references its parent parameter '" << ac.getVariable() << "'.";
    msg = ss_msg.str();
    inv(false);
}
END_CONSTRAINT


// 1223604
START_CONSTRAINT(SpatialBoundaryConditionVariableMustBeSpecies, BoundaryCondition, bc)
{
  bool fail = false;
  pre(bc.isSetVariable());

  if (m.getSpecies(bc.getVariable()) == NULL) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A <boundaryCondition>";
    if (bc.isSetId())
    {
      ss_msg << " with id '" << bc.getId() << "'";
    }
    ss_msg << " references a variable '" << bc.getVariable() << "', which is not the ID of a <species> in the <model>.";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221050
START_CONSTRAINT(SpatialBoundaryMinLessThanMax, CoordinateComponent, cc)
{
  bool fail = false;
  pre(cc.isSetBoundaryMax());
  pre(cc.isSetBoundaryMin());
  const Boundary* boundary = cc.getBoundaryMax();
  pre(boundary->isSetValue());
  double maxval = boundary->getValue();
  boundary = cc.getBoundaryMin();
  pre(boundary->isSetValue());
  double minval = boundary->getValue();
  if (minval > maxval) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "A <coordinateComponent>";
    if (cc.isSetId())
    {
      ss_msg << " with id '" << cc.getId() << "'";
    }
    ss_msg << " has a child <maxBoundary> with a value of " << maxval << ", which is less than the child <maxBoundary> value of " << minval << ".";
    msg = ss_msg.str();
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221051
START_CONSTRAINT(SpatialBoundaryMustBeConstant, Parameter, param)
{
  bool fail = false;
  const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param.getPlugin("spatial"));
  pre(spp != NULL);
  pre(spp->isSetSpatialSymbolReference());
  const SpatialSymbolReference* ssr = spp->getSpatialSymbolReference();
  pre(ssr != NULL);
  pre(ssr->isSetSpatialRef());
  string refstr = ssr->getSpatialRef();
  const SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geom = plug->getGeometry();
  pre(geom != NULL);
  ListOfCoordinateComponents* locc = const_cast<ListOfCoordinateComponents*>(geom->getListOfCoordinateComponents());
  pre(locc != NULL);
  const SBase* ref = locc->getElementBySId(refstr);
  pre(ref != NULL);
  pre(ref->getTypeCode() == SBML_SPATIAL_BOUNDARY);
  if (!param.isSetConstant() || param.getConstant() == false) {
    fail = true;
    msg = "A <spatialSymbolReference> has a spatialRef of '";
    msg += refstr + "', which points to a boundary, but its parent <parameter>";
    if (param.isSetId()) {
      msg += " (with the id '" + param.getId() + "')";
    }
    msg += " is not set 'constant=true'.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220753
START_CONSTRAINT(SpatialDomainTypeNoAssignment, Parameter, param)
{
  bool fail = false;
  pre(param.isSetId());
  string paramid = param.getId();
  const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param.getPlugin("spatial"));
  pre(spp != NULL);
  pre(spp->isSetSpatialSymbolReference());
  const SpatialSymbolReference* ssr = spp->getSpatialSymbolReference();
  pre(ssr != NULL);
  pre(ssr->isSetSpatialRef());
  string refstr = ssr->getSpatialRef();
  const SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geom = plug->getGeometry();
  pre(geom != NULL);
  ListOfDomainTypes* lodt = const_cast<ListOfDomainTypes*>(geom->getListOfDomainTypes());
  pre(lodt != NULL);
  const SBase* ref = lodt->getElementBySId(refstr);
  pre(ref != NULL);
  pre(ref->getTypeCode() == SBML_SPATIAL_DOMAINTYPE);

  msg = "A <spatialSymbolReference> has a spatialRef of '";
  msg += refstr + "', which points to a domainType, but its parent <parameter>";
  if (param.isSetId()) {
    msg += " (with the id '" + param.getId() + "')";
  }

  if (param.isSetValue()) {
    msg += " sets its 'value' attribute.";
    fail = true;
  }

  else if (m.getInitialAssignment(paramid) != NULL) {
    msg += " is set by an <initialAssignment>.";
    fail = true;
  }

  else if (m.getRateRule(paramid) != NULL) {
    msg += " is set by a <rateRule>.";
    fail = true;
  }

  else if (m.getAssignmentRule(paramid) != NULL) {
    msg += " is set by an <assignmentRule>.";
    fail = true;
  }

  else {
    for (unsigned long e = 0; e < m.getNumEvents(); e++) {
      const Event* event = m.getEvent(e);
      if (event->getEventAssignment(paramid) != NULL) {
        msg += " is set by an <eventAssignment>";
        if (event->isSetId()) {
          msg += " in the <event> with an id of '";
          msg += event->getId() + "'";
        }
        msg += ".";
        fail = true;
        break;
      }
    }
  }

  //NOTE:  we don't check if it's determined by an algebraic rule, because that's too expensive and unlikely.

  inv(fail == false);
}
END_CONSTRAINT


// 1220753
START_CONSTRAINT(SpatialDomainNoAssignment, Parameter, param)
{
  bool fail = false;
  pre(param.isSetId());
  string paramid = param.getId();
  const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param.getPlugin("spatial"));
  pre(spp != NULL);
  pre(spp->isSetSpatialSymbolReference());
  const SpatialSymbolReference* ssr = spp->getSpatialSymbolReference();
  pre(ssr != NULL);
  pre(ssr->isSetSpatialRef());
  string refstr = ssr->getSpatialRef();
  const SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geom = plug->getGeometry();
  pre(geom != NULL);
  ListOfDomains* lod = const_cast<ListOfDomains*>(geom->getListOfDomains());
  pre(lod != NULL);
  const SBase* ref = lod->getElementBySId(refstr);
  pre(ref != NULL);
  pre(ref->getTypeCode() == SBML_SPATIAL_DOMAIN);

  msg = "A <spatialSymbolReference> has a spatialRef of '";
  msg += refstr + "', which points to a domain, but its parent <parameter>";
  if (param.isSetId()) {
    msg += " (with the id '" + param.getId() + "')";
  }

  if (param.isSetValue()) {
    msg += " sets its 'value' attribute.";
    fail = true;
  }

  else if (m.getInitialAssignment(paramid) != NULL) {
    msg += " is set by an <initialAssignment>.";
    fail = true;
  }

  else if (m.getRateRule(paramid) != NULL) {
    msg += " is set by a <rateRule>.";
    fail = true;
  }

  else if (m.getAssignmentRule(paramid) != NULL) {
    msg += " is set by an <assignmentRule>.";
    fail = true;
  }

  else {
    for (unsigned long e = 0; e < m.getNumEvents(); e++) {
      const Event* event = m.getEvent(e);
      if (event->getEventAssignment(paramid) != NULL) {
        msg += " is set by an <eventAssignment>";
        if (event->isSetId()) {
          msg += " in the <event> with an id of '";
          msg += event->getId() + "'";
        }
        msg += ".";
        fail = true;
        break;
      }
    }
  }

  //NOTE:  we don't check if it's determined by an algebraic rule, because that's too expensive and unlikely.

  inv(fail == false);
}
END_CONSTRAINT


// 1220950
START_CONSTRAINT(SpatialInteriorPointOneCoordIn1DGeometry, InteriorPoint, ipoint)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==1);

  bool fail = false;
  stringstream wrongvals;
  if (ipoint.isSetCoord2()) {
    fail = true;
    wrongvals << "coord2 with a value of '";
    wrongvals << ipoint.getCoord2();
    wrongvals << "'";
  }
  if (ipoint.isSetCoord3()) {
    if (fail) {
      wrongvals << ", and a ";
    }
    wrongvals << "coord3 with a value of '";
    wrongvals << ipoint.getCoord3();
    wrongvals << "'";
    fail = true;
  }
  if (fail) {
    msg = "An <interiorPoint>";
    if (ipoint.isSetId())
    {
      msg += " with id '" + ipoint.getId() + "'";
    }
    msg += " defines a " + wrongvals.str();
    msg += ", but the <listOfCoordinateComponents> has exactly one child.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220951
START_CONSTRAINT(SpatialInteriorPointTwoCoordsIn2DGeometry, InteriorPoint, ipoint)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==2);

  bool fail = false;
  stringstream wrongvals;
  if (!ipoint.isSetCoord2()) {
    fail = true;
    wrongvals << " doesn't define the coord2 attribute";
  }
  if (ipoint.isSetCoord3()) {
    if (fail) {
      wrongvals << ", and has a ";
    }
    else {
      wrongvals << " defines a ";
    }
    wrongvals << "coord3 with a value of '";
    wrongvals << ipoint.getCoord3();
    wrongvals << "'";
    fail = true;
  }
  if (fail) {
    msg = "An <interiorPoint>";
    if (ipoint.isSetId())
    {
      msg += " with id '" + ipoint.getId() + "'";
    }
    msg += wrongvals.str();
    msg += ", but the <listOfCoordinateComponents> has exactly two children.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1220952
START_CONSTRAINT(SpatialInteriorPointThreeCoordsIn3DGeometry, InteriorPoint, ipoint)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents()==3);

  bool fail = false;
  stringstream wrongvals;
  if (!ipoint.isSetCoord2()) {
    fail = true;
    wrongvals << " doesn't define the coord2 attribute";
  }
  if (!ipoint.isSetCoord3()) {
    if (fail) {
      wrongvals << ", and also";
    }
    wrongvals << " doesn't define the coord3 attribute";
    fail = true;
  }
  if (fail) {
    msg = "An <interiorPoint>";
    if (ipoint.isSetId())
    {
      msg += " with id '" + ipoint.getId() + "'";
    }
    msg += wrongvals.str();
    msg += ", but the <listOfCoordinateComponents> has exactly three children.";
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1221150
START_CONSTRAINT(SpatialAdjacentDomainsMustBeAdjacent, AdjacentDomains, adom)
{
  bool fail = false;
  pre(adom.isSetDomain1());
  pre(adom.isSetDomain2());
  if (adom.getDomain1() == adom.getDomain2()) {
    msg = "An <adjacentDomain>";
    if (adom.isSetId())
    {
      msg += " with id '" + adom.getId() + "'";
    }
    msg += " defines both 'domain1' and 'domain2' to be '";
    msg += adom.getDomain1() + "'.";
    fail = true;
  }

  //NOTE:  It would be fantastic if we could actually measure space and figure out if these
  // domains were adjacent, but that's well well beyond the capabilities of this 
  // software.  Other spatially-aware simulators can test this themselves and use this error
  // ID to report it, if they want.

  inv(fail == false);
}
END_CONSTRAINT


// 1221753
START_CONSTRAINT(SpatialSampledVolumeMinLessThanMax, SampledVolume, svol)
{
  bool fail = false;
  pre(svol.isSetMinValue());
  pre(svol.isSetMaxValue());
  if (svol.getMaxValue() < svol.getMinValue()) {
    stringstream ss_msg;
    ss_msg << "A <sampledVolume>";
    if (svol.isSetId())
    {
      ss_msg << " with id '" << svol.getId() << "'";
    }
    ss_msg << " has a 'spatial:maxValue' attribute of '";
    ss_msg << svol.getMaxValue();
    ss_msg << "', which is less than '";
    ss_msg << svol.getMinValue();
    ss_msg << "', the value of the attribute 'spatial:minValue'.";
    msg = ss_msg.str();
    fail = true;

  }

  inv(fail == false);
}
END_CONSTRAINT


// 1223150
START_CONSTRAINT(SpatialCSGPrimitive3DShapes, CSGPrimitive, csgp)
{
  bool fail = false;

  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  unsigned int dim = geom->getNumCoordinateComponents();
  pre(dim < 3);
  switch(csgp.getPrimitiveType()){
  case SPATIAL_PRIMITIVEKIND_SPHERE:
  case SPATIAL_PRIMITIVEKIND_CUBE:
  case SPATIAL_PRIMITIVEKIND_CYLINDER:
  case SPATIAL_PRIMITIVEKIND_CONE:
    msg = "A <csgPrimitive>";
    if (csgp.isSetId()) {
      msg += " with the id '" + csgp.getId() + "'";
    }
    msg += " has as 'primitiveType' of '";
    msg += csgp.getPrimitiveTypeAsString() + "', but the <geometry> only has ";
    if (dim == 1) {
      msg += "one <coordinateComponent> child.";
    }
    else {
      msg += "two <coordinateComponent> children.";
    }
    fail = true;
    break;
  case SPATIAL_PRIMITIVEKIND_CIRCLE:
  case SPATIAL_PRIMITIVEKIND_SQUARE:
  case SPATIAL_PRIMITIVEKIND_INVALID:
    break;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1223150
START_CONSTRAINT(SpatialCSGPrimitive2DShapes, CSGPrimitive, csgp)
{
  bool fail = false;

  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getNumCoordinateComponents()==1);
  switch(csgp.getPrimitiveType()){
  case SPATIAL_PRIMITIVEKIND_SPHERE:
  case SPATIAL_PRIMITIVEKIND_CUBE:
  case SPATIAL_PRIMITIVEKIND_CYLINDER:
  case SPATIAL_PRIMITIVEKIND_CONE:
  case SPATIAL_PRIMITIVEKIND_INVALID:
    break;
  case SPATIAL_PRIMITIVEKIND_CIRCLE:
  case SPATIAL_PRIMITIVEKIND_SQUARE:
    msg = "A <csgPrimitive>";
    if (csgp.isSetId()) {
      msg += " with the id '" + csgp.getId() + "'";
    }
    msg += " has as 'primitiveType' of '";
    msg += csgp.getPrimitiveTypeAsString();
    msg += "', but the <geometry> only has one <coordinateComponent> child.";
    fail = true;
    break;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1223250
START_CONSTRAINT(SpatialCSGSetOperatorTwoComplementsForDifference, CSGSetOperator, setop)
{
  bool fail = false;
  pre(setop.getOperationType() == SPATIAL_SETOPERATION_DIFFERENCE);
  msg = "A <csgSetOperator>";
  if (setop.isSetId()) {
    msg += " with the id '" + setop.getId() + "'";
  }
  msg += " has an 'operationType' of 'difference', but";
  if (setop.isSetComplementA() == false) {
    fail = true;
    msg += " does not have a value for its 'complementA' attribute";
  }
  if (setop.isSetComplementB() == false) {
    if (fail) {
      msg += ", and also";
    }
    fail = true;
    msg += " does not have a value for its 'complementB' attribute";
  }
  msg += ".";
  inv(fail == false);
}
END_CONSTRAINT


// 1223251
START_CONSTRAINT(SpatialCSGSetOperatorNoComplementsUnionIntersection, CSGSetOperator, setop)
{
  bool fail = false;
  SetOperation_t type = setop.getOperationType();
  pre(type == SPATIAL_SETOPERATION_INTERSECTION || type == SPATIAL_SETOPERATION_UNION);
  msg = "A <csgSetOperator>";
  if (setop.isSetId()) {
    msg += " with the id '" + setop.getId() + "'";
  }
  msg += " has an 'operationType' of '";
  msg += setop.getOperationTypeAsString() + "', but";
  if (setop.isSetComplementA()) {
    fail = true;
    msg += " has a value of '";
    msg += setop.getComplementA() + "' for its 'complementA' attribute";
  }
  if (setop.isSetComplementB()) {
    if (fail) {
      msg += ", and also";
    }
    fail = true;
    msg += " has a value of '";
    msg += setop.getComplementB() + "' for its 'complementB' attribute";
  }
  msg += ".";
  inv(fail == false);
}
END_CONSTRAINT


// 1223252
START_CONSTRAINT(SpatialCSGSetOperatorDifferenceMustHaveTwoChildren, CSGSetOperator, setop)
{
  bool fail = false;
  pre(setop.getOperationType() == SPATIAL_SETOPERATION_DIFFERENCE);
  unsigned int nchildren = setop.getNumCSGNodes();
  if (nchildren != 2) {
    stringstream ss_msg;
    ss_msg << "A <csgSetOperator>";
    if (setop.isSetId())
    {
      ss_msg << " with id '" << setop.getId() << "'";
    }
    ss_msg << " has an 'operationType' value of 'difference', but has ";
    ss_msg << nchildren << " children.";
    msg = ss_msg.str();

    fail = true;
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1223253
START_CONSTRAINT(SpatialCSGSetOperatorComplementsMustReferenceChildren, CSGSetOperator, setop)
{
  pre(setop.getOperationType() == SPATIAL_SETOPERATION_DIFFERENCE);
  pre(setop.getNumCSGNodes()==2);
  pre(setop.isSetComplementA());
  pre(setop.isSetComplementB());
  string child1 = setop.getCSGNode(0)->getId();
  string child2 = setop.getCSGNode(1)->getId();
  string compA = setop.getComplementA();
  string compB = setop.getComplementB();
  pre(!((child1 == compA && child2 == compB) || (child1 == compB && child2 == compA)));
  msg = "A <csgSetOperator>";
  if (setop.isSetId()) {
    msg += " with the id '" + setop.getId() + "'";
  }
  msg += " has as 'complementA' value of '";
  msg += compA + "', and a 'complementB' value of '" + compB;
  msg += "', which are not the two IDs of its two children: '";
  msg += child1 + "' and '" + child2 + "'.";
  inv(false);
}
END_CONSTRAINT


// 1223254
START_CONSTRAINT(SpatialCSGSetOperatorShouldHaveTwoPlusChildren, CSGSetOperator, setop)
{
  SetOperation_t type = setop.getOperationType();
  pre(type == SPATIAL_SETOPERATION_INTERSECTION || type == SPATIAL_SETOPERATION_UNION);
  unsigned long nchildren = setop.getNumCSGNodes();
  pre(nchildren < 2);
  stringstream ss_msg;
  ss_msg << "A <csgSetOperator>";
  if (setop.isSetId())
  {
    ss_msg << " with id '" << setop.getId() << "'";
  }
  ss_msg <<  " has an 'operationType' of '";
  ss_msg << setop.getOperationTypeAsString() + "', but has ";
  if (nchildren == 0) {
    ss_msg << "no children.  This is equivalent to not including the <csgSetOperator> at all.";
  }
  else if (nchildren == 1) {
    ss_msg << "one child.  This is equivalent to replacing the <csgSetOperator> with its single child.";
  }
  msg = ss_msg.str();
  inv(false);
}
END_CONSTRAINT


// 1222651
START_CONSTRAINT(SpatialCSGTranslationTranslateYRequiredIn2D, CSGTranslation, translation)
{
  bool fail = false;
  pre(!translation.isSetTranslateY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Y) {
      msg = "A <csgTranslation>";
      if (translation.isSetId()) {
        msg += " with the id '" + translation.getId() + "'";
      }
      msg += " has no 'translationY' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianY'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222652
START_CONSTRAINT(SpatialCSGTranslationTranslateZRequiredIn3D, CSGTranslation, translation)
{
  bool fail = false;
  pre(!translation.isSetTranslateZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Z) {
      msg = "A <csgTranslation>";
      if (translation.isSetId()) {
        msg += " with the id '" + translation.getId() + "'";
      }
      msg += " has no 'translationZ' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianZ'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222653
START_CONSTRAINT(SpatialCSGTranslationNoTranslateYIn1D, CSGTranslation, translation)
{
  pre(translation.isSetTranslateY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Y);
  }
  msg = "A <csgTranslation>";
  if (translation.isSetId()) {
    msg += " with the id '" + translation.getId() + "'";
  }
  msg += " has a 'translationY' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianY'.";
  inv(false);
}
END_CONSTRAINT


// 1222654
START_CONSTRAINT(SpatialCSGTranslationNoTranslateZIn2D, CSGTranslation, translation)
{
  pre(translation.isSetTranslateZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Z);
  }
  msg = "A <csgTranslation>";
  if (translation.isSetId()) {
    msg += " with the id '" + translation.getId() + "'";
  }
  msg += " has a 'translationZ' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianZ'.";
  inv(false);
}
END_CONSTRAINT


// 1222751
START_CONSTRAINT(SpatialCSGRotationRotateYRequiredIn2D, CSGRotation, rotation)
{
  bool fail = false;
  pre(!rotation.isSetRotateY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Y) {
      msg = "A <csgTranslation>";
      if (rotation.isSetId()) {
        msg += " with the id '" + rotation.getId() + "'";
      }
      msg += " has no 'rotateY' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianY'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222752
START_CONSTRAINT(SpatialCSGRotationRotateZRequiredIn3D, CSGRotation, rotation)
{
  bool fail = false;
  pre(!rotation.isSetRotateZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Z) {
      msg = "A <csgTranslation>";
      if (rotation.isSetId()) {
        msg += " with the id '" + rotation.getId() + "'";
      }
      msg += " has no 'rotateZ' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianY'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222753
START_CONSTRAINT(SpatialCSGRotationNoRotateYIn1D, CSGRotation, rotation)
{
  pre(rotation.isSetRotateY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Y);
  }
  msg = "A <csgTranslation>";
  if (rotation.isSetId()) {
    msg += " with the id '" + rotation.getId() + "'";
  }
  msg += " has a 'rotateY' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianY'.";
  inv(false);
}
END_CONSTRAINT


// 1222754
START_CONSTRAINT(SpatialCSGRotationNoRotateZIn2D, CSGRotation, rotation)
{
  pre(rotation.isSetRotateZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Z);
  }
  msg = "A <csgTranslation>";
  if (rotation.isSetId()) {
    msg += " with the id '" + rotation.getId() + "'";
  }
  msg += " has a 'rotateZ' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianZ'.";
  inv(false);
}
END_CONSTRAINT


// 1222755
START_CONSTRAINT(SpatialCSGRotationNoOriginIn3D, CSGRotation, rotation)
{
  pre(rotation.isSetRotateX());
  pre(rotation.isSetRotateY());
  pre(rotation.isSetRotateZ());
  pre(rotation.getRotateX() == 0);
  pre(rotation.getRotateY() == 0);
  pre(rotation.getRotateZ() == 0);
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents() == 3);
  msg = "A <csgTranslation>";
  if (rotation.isSetId()) {
    msg += " with the id '" + rotation.getId() + "'";
  }
  msg += " has values of '0' for its 'rotateX', 'rotateY', and 'rotateZ' attributes, but the <geometry> has three <coordinateComponent> children.";
  inv(false);
}
END_CONSTRAINT


// 1222851
START_CONSTRAINT(SpatialCSGScaleScaleYRequiredIn2D, CSGScale, scale)
{
  bool fail = false;
  pre(!scale.isSetScaleY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Y) {
      msg = "A <csgTranslation>";
      if (scale.isSetId()) {
        msg += " with the id '" + scale.getId() + "'";
      }
      msg += " has no 'scaleY' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianY'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222852
START_CONSTRAINT(SpatialCSGScaleScaleZRequiredIn3D, CSGScale, scale)
{
  bool fail = false;
  pre(!scale.isSetScaleZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    if (geometry->getCoordinateComponent(cc)->getType() == SPATIAL_COORDINATEKIND_CARTESIAN_Z) {
      msg = "A <csgTranslation>";
      if (scale.isSetId()) {
        msg += " with the id '" + scale.getId() + "'";
      }
      msg += " has no 'scaleZ' value, but the <geometry> has a <coordinateComponent> child of type 'cartesianZ'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1222853
START_CONSTRAINT(SpatialCSGScaleNoScaleYIn1D, CSGScale, scale)
{
  pre(scale.isSetScaleY());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Y);
  }
  msg = "A <csgTranslation>";
  if (scale.isSetId()) {
    msg += " with the id '" + scale.getId() + "'";
  }
  msg += " has a 'scaleY' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianY'.";
  inv(false);
}
END_CONSTRAINT


// 1222854
START_CONSTRAINT(SpatialCSGScaleNoScaleZIn2D, CSGScale, scale)
{
  pre(scale.isSetScaleZ());
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  for (unsigned long cc = 0; cc < geometry->getNumCoordinateComponents(); cc++) {
    pre(geometry->getCoordinateComponent(cc)->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_Z);
  }
  msg = "A <csgTranslation>";
  if (scale.isSetId()) {
    msg += " with the id '" + scale.getId() + "'";
  }
  msg += " has a 'scaleZ' value, but the <geometry> has no <coordinateComponent> child of type 'cartesianZ'.";
  inv(false);
}
END_CONSTRAINT


// 1223050
START_CONSTRAINT(SpatialTransformationComponentComponentsLengthMustBe16, TransformationComponent, tc)
{
  pre(tc.isSetComponentsLength());
  pre(tc.getComponentsLength() != 16);
  stringstream ss_msg;
  ss_msg << "A <csgTransformationComponent>";
  if (tc.isSetId())
  {
    ss_msg << " with id '" << tc.getId() << "'";
  }
  ss_msg <<  " has a 'componentsLength' of '";
  ss_msg << tc.getComponentsLength();
  ss_msg << "', instead of a value of '16'.";
  msg = ss_msg.str();
  inv(false);
}
END_CONSTRAINT


// 1223051
START_CONSTRAINT(SpatialTransformationComponentArrayLengthMustBe16, TransformationComponent, tc)
{
  pre(tc.isSetComponents());
  pre(tc.getActualComponentsLength() != 16);
  stringstream ss_msg;
  ss_msg << "A <csgTransformationComponent>";
  if (tc.isSetId())
  {
    ss_msg << " with id '" << tc.getId() << "'";
  }
  ss_msg <<  " has a 'components' with ";
  ss_msg << tc.getActualComponentsLength();
  ss_msg << " entries instead of 16.";
  msg = ss_msg.str();
  inv(false);
}
END_CONSTRAINT


// 1222050
START_CONSTRAINT(SpatialParametricGeometryNotIn1D, ParametricGeometry, pg)
{
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents() == 1);
  msg = "A <parametricGeometry>";
  if (pg.isSetId()) {
    msg += " with the id '" + pg.getId() + "'";
  }
  msg += " was found in the model, but the <geometry> has exactly one <coordinateComponent> child.";

  inv(false);
}
END_CONSTRAINT


// 1224050
START_CONSTRAINT(SpatialSpatialPointsDataLengthMustMatchUncompressed, SpatialPoints, sp)
{
  pre(sp.isSetCompression());
  pre(sp.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  pre(sp.isSetArrayDataLength());
  pre(sp.getArrayDataLength() != sp.getActualArrayDataLength());
  stringstream ss_msg;
  ss_msg << "A <spatialPoints>";
  if (sp.isSetId())
  {
    ss_msg << " with id '" << sp.getId() << "'";
  }
  ss_msg <<  " is set 'uncompressed' and has an 'arrayDataLength' of '";
  ss_msg << sp.getArrayDataLength() << "', but actually contains ";
  ss_msg << sp.getActualArrayDataLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1224051
START_CONSTRAINT(SpatialSpatialPointsDataLengthMustMatchCompressed, SpatialPoints, sp)
{
  pre(sp.isSetCompression());
  pre(sp.getCompression() == SPATIAL_COMPRESSIONKIND_DEFLATED);
  pre(sp.isSetArrayDataLength());
  pre(sp.getArrayDataLength() != sp.getActualArrayDataLength());
  stringstream ss_msg;
  ss_msg << "A <spatialPoints>";
  if (sp.isSetId())
  {
    ss_msg << " with id '" << sp.getId() << "'";
  }
  ss_msg <<  " is set 'deflated' and has an 'arrayDataLength' of '";
  ss_msg << sp.getArrayDataLength() << "', but actually contains ";
  ss_msg << sp.getActualArrayDataLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1224052
START_CONSTRAINT(SpatialSpatialPointsArrayDataMultipleOfDimensions, SpatialPoints, sp)
{
  pre(sp.getCompression() != SPATIAL_COMPRESSIONKIND_DEFLATED);
  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(sp.getActualArrayDataLength() % geometry->getNumCoordinateComponents() != 0);
  stringstream ss_msg;
  ss_msg << "A <spatialPoints>";
  if (sp.isSetId())
  {
    ss_msg << " with id '" << sp.getId() << "'";
  }
  ss_msg <<  " has " << sp.getActualArrayDataLength() << " entries, which is not a multiple of ";
  ss_msg << geometry->getNumCoordinateComponents() << ", the dimensionality of the <geometry>.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1224053
START_CONSTRAINT(SpatialSpatialPointsFloatArrayDataMustMatch, SpatialPoints, sp)
{
  pre(sp.getCompression() != SPATIAL_COMPRESSIONKIND_DEFLATED);
  bool fail = false;
  pre(sp.isSetDataType());
  pre(sp.getDataType() == SPATIAL_DATAKIND_FLOAT);
  size_t len = sp.getActualArrayDataLength();
  double* data = new double[len];
  sp.getArrayData(data);
  for (size_t d = 0; d < len; d++) {
    double val = data[d];
    if (val > 3.4028235e38 || val < -3.4028235e38 || (val > 0 && val < 1.17549e-38) || val < 0 && val > -1.17549e-38) {
      stringstream ss_msg;
      ss_msg << "A <spatialPoints>";
      if (sp.isSetId())
      {
        ss_msg << " with id '" << sp.getId() << "'";
      }
      ss_msg << " has an entry with the value '" << val;
      ss_msg << "', which is outside the range of single-precision 'float' values.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1224054
START_CONSTRAINT(SpatialSpatialPointsUIntArrayDataNotNegative, SpatialPoints, sp)
{
  pre(sp.getCompression() != SPATIAL_COMPRESSIONKIND_DEFLATED);
  bool fail = false;
  pre(sp.isSetDataType());
  pre(sp.getDataType() == SPATIAL_DATAKIND_UINT || sp.getDataType() == SPATIAL_DATAKIND_UINT8 || sp.getDataType() == SPATIAL_DATAKIND_UINT16 || sp.getDataType() == SPATIAL_DATAKIND_UINT32);
  size_t len = sp.getActualArrayDataLength();
  double* data = new double[len];
  sp.getArrayData(data);
  for (size_t d = 0; d < len; d++) {
    double val = data[d];
    if (val < 0) {
      stringstream ss_msg;
      ss_msg << "A <spatialPoints>";
      if (sp.isSetId())
      {
        ss_msg << " with id '" << sp.getId() << "'";
      }
      ss_msg << " has a data type of '" << sp.getDataTypeAsString();
      ss_msg << "', but has an entry with the value '" << val;
      ss_msg << "', which is negative.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1224055
START_CONSTRAINT(SpatialSpatialPointsIntArrayDataIntegers, SpatialPoints, sp)
{
  pre(sp.getCompression() != SPATIAL_COMPRESSIONKIND_DEFLATED);
  bool fail = false;
  pre(sp.isSetDataType());
  pre(sp.getDataType() == SPATIAL_DATAKIND_INT || sp.getDataType() == SPATIAL_DATAKIND_UINT || sp.getDataType() == SPATIAL_DATAKIND_UINT8 || sp.getDataType() == SPATIAL_DATAKIND_UINT16 || sp.getDataType() == SPATIAL_DATAKIND_UINT32);
  size_t len = sp.getActualArrayDataLength();
  double* data = new double[len];
  sp.getArrayData(data);
  for (size_t d = 0; d < len; d++) {
    double ival, val = data[d];
    if (modf(val, &ival) != 0) {
      stringstream ss_msg;
      ss_msg << "A <spatialPoints>";
      if (sp.isSetId())
      {
        ss_msg << " with id '" << sp.getId() << "'";
      }
      ss_msg << " has a data type of '" << sp.getDataTypeAsString();
      ss_msg << "', but has an entry with the value '" << val;
      ss_msg << "', which is not an integer.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1222150
START_CONSTRAINT(SpatialParametricObjectPointIndexLengthMustMatchUncompressed, ParametricObject, po)
{
  pre(po.isSetCompression());
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  pre(po.isSetPointIndexLength());
  pre(po.getPointIndexLength() != po.getActualPointIndexLength());
  stringstream ss_msg;
  ss_msg << "A <parametricObject>";
  if (po.isSetId())
  {
    ss_msg << " with id '" << po.getId() << "'";
  }
  ss_msg <<  " is set 'uncompressed' and has an 'arrayDataLength' of '";
  ss_msg << po.getPointIndexLength() << "', but actually contains ";
  ss_msg << po.getActualPointIndexLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1222151
START_CONSTRAINT(SpatialParametricObjectPointIndexLengthMustMatchCompressed, ParametricObject, po)
{
  pre(po.isSetCompression());
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_DEFLATED);
  pre(po.isSetPointIndexLength());
  pre(po.getPointIndexLength() != po.getActualPointIndexLength());
  stringstream ss_msg;
  ss_msg << "A <parametricObject>";
  if (po.isSetId())
  {
    ss_msg << " with id '" << po.getId() << "'";
  }
  ss_msg <<  " is set 'deflated' and has an 'arrayDataLength' of '";
  ss_msg << po.getPointIndexLength() << "', but actually contains ";
  ss_msg << po.getActualPointIndexLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1222152
START_CONSTRAINT(SpatialParametricObjectThreePointsForTriangles, ParametricObject, po)
{
  pre(po.getPolygonType() == SPATIAL_POLYGONKIND_TRIANGLE);
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  pre(po.getActualPointIndexLength() % 3 != 0);
  stringstream ss_msg;
  ss_msg << "A <parametricObject>";
  if (po.isSetId())
  {
    ss_msg << " with id '" << po.getId() << "'";
  }
  ss_msg << " has a polygonType of 'triangle' but " << po.getActualPointIndexLength();
  ss_msg << " entries, which is not a multiple of three.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1222155
START_CONSTRAINT(SpatialParametricObjectIndexesMustBePoints, ParametricObject, po)
{
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  bool fail = false;
  const SBase* parent = po.getParentSBMLObject();
  pre(parent != NULL);
  parent = parent->getParentSBMLObject();
  pre(parent != NULL);
  const ParametricGeometry* pg = static_cast<const ParametricGeometry*>(parent);
  const SpatialPoints* sp = pg->getSpatialPoints();
  size_t numPoints = sp->getActualArrayDataLength();

  SpatialModelPlugin *plug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(plug != NULL);
  pre(plug->isSetGeometry());
  const Geometry* geometry = plug->getGeometry();
  pre(geometry->getNumCoordinateComponents() != 0);
  pre(numPoints % geometry->getNumCoordinateComponents() == 0);
  numPoints = numPoints/(geometry->getNumCoordinateComponents());

  size_t len = po.getActualPointIndexLength();
  int* data = new int[len];
  po.getPointIndex(data);
  for (size_t d = 0; d < len; d++) 
  {
    if (data[d] >= (int)numPoints)
    {
      stringstream ss_msg;
      ss_msg << "A <parametricObject>";
      if (po.isSetId())
      {
        ss_msg << " with id '" << po.getId() << "'";
      }
      ss_msg << " has a point index value of '" << data[d];
      ss_msg << "', which is too large for the number of points in the <spatialPoints> object (";
      ss_msg << numPoints << ").";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1222156
START_CONSTRAINT(SpatialParametricObjectFacesSameChirality, ParametricObject, po)
{
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  bool fail = false;
  pre(po.isSetPolygonType());
  int groupsize;
  size_t len = po.getActualPointIndexLength();
  pre(po.getPolygonType() == SPATIAL_POLYGONKIND_TRIANGLE);
  groupsize = 3;
  pre(len % 3 == 0);
  set<pair<int, int> > borders;

  int* data = new int[len];
  po.getPointIndex(data);
  for (size_t d = 0; d < len; d++) 
  {
    pair<int, int> border;
    if ((d + 1) % groupsize == 0) {
      border = make_pair(data[d], data[d - groupsize + 1]);
    }
    else {
      border = make_pair(data[d], data[d + 1]);
    }
    if (borders.find(border) != borders.end())
    {
      stringstream ss_msg;
      ss_msg << "A <parametricObject>";
      if (po.isSetId())
      {
        ss_msg << " with id '" << po.getId() << "'";
      }
      ss_msg << " has a shared border (" << border.first;
      ss_msg << ", " << border.second << ") in the same order in two shapes.  This means that one of them is clockwise and the other is counter-clockwise.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
    borders.insert(border);
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1222157
START_CONSTRAINT(SpatialParametricObjectMaxTwoPointBorders, ParametricObject, po)
{
  pre(po.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  bool fail = false;
  pre(po.isSetPolygonType());
  int groupsize;
  size_t len = po.getActualPointIndexLength();
  pre(po.getPolygonType() == SPATIAL_POLYGONKIND_TRIANGLE);
  groupsize = 3;
  pre(len % 3 == 0);
  set<set<int> > triples;

  int* data = new int[len];
  po.getPointIndex(data);
  for (size_t d = 0; d < len; d += groupsize)
  {
    vector<set<int> > localtriples;
    set<int> triple;
    triple.insert(data[d]);
    triple.insert(data[d + 1]);
    triple.insert(data[d + 2]);
    localtriples.push_back(triple);
    if (groupsize == 4)
    {
      triple.clear();
      triple.insert(data[d + 1]);
      triple.insert(data[d + 2]);
      triple.insert(data[d + 3]);
      localtriples.push_back(triple);

      triple.clear();
      triple.insert(data[d]);
      triple.insert(data[d + 2]);
      triple.insert(data[d + 3]);
      localtriples.push_back(triple);

      triple.clear();
      triple.insert(data[d]);
      triple.insert(data[d + 1]);
      triple.insert(data[d + 3]);
      localtriples.push_back(triple);
    }

    for (size_t lt = 0; lt < localtriples.size(); lt++)
    {
      triple = localtriples[lt];
      if (triples.find(triple) != triples.end())
      {
        stringstream ss_msg;
        ss_msg << "A <parametricObject>";
        if (po.isSetId())
        {
          ss_msg << " with id '" << po.getId() << "'";
        }
        ss_msg << " has three points ( ";
        for (set<int>::iterator ti = triple.begin(); ti != triple.end(); ti++)
        {
          ss_msg << *ti;
          ss_msg << " ";
        }
        ss_msg << ") in two different faces.";
        msg = ss_msg.str();
        fail = true;
        break;
      }
      triples.insert(triple);
    }
    if (fail) {
      break;
    }
  }
  delete[] data;
  inv(fail == false);
}
END_CONSTRAINT


// 1221653
START_CONSTRAINT(SpatialSampledFieldSamplesLengthMustMatchUncompressed, SampledField, sf)
{
  pre(sf.isSetCompression());
  pre(sf.getCompression() == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  pre(sf.isSetSamplesLength());
  SampledField* sf_nc = const_cast<SampledField*>(&sf);
  pre(sf_nc->getSamplesLength() != (int) sf_nc->getUncompressedLength());
  stringstream ss_msg;
  ss_msg << "A <spatialPoints>";
  if (sf.isSetId())
  {
    ss_msg << " with id '" << sf.getId() << "'";
  }
  ss_msg <<  " is set 'uncompressed' and has a 'samplesLength' of '";
  ss_msg << sf.getSamplesLength() << "', but actually contains ";
  ss_msg << sf_nc->getUncompressedLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1221654
START_CONSTRAINT(SpatialSampledFieldSamplesLengthMustMatchCompressed, SampledField, sf)
{
  pre(sf.isSetCompression());
  pre(sf.getCompression() == SPATIAL_COMPRESSIONKIND_DEFLATED);
  pre(sf.isSetSamplesLength());
  SampledField* sf_nc = const_cast<SampledField*>(&sf);
  pre(sf_nc->getSamplesLength() != (int) sf_nc->getActualSamplesLength());
  stringstream ss_msg;
  ss_msg << "A <spatialPoints>";
  if (sf.isSetId())
  {
    ss_msg << " with id '" << sf.getId() << "'";
  }
  ss_msg <<  " is set 'deflated' and has a 'samplesLength' of '";
  ss_msg << sf.getSamplesLength() << "', but actually contains ";
  ss_msg << sf_nc->getActualSamplesLength() << " entries.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1221655
START_CONSTRAINT(SpatialSampledFieldFloatArrayDataMustMatch, SampledField, sf)
{
  bool fail = false;
  pre(sf.isSetDataType());
  pre(sf.getDataType() == SPATIAL_DATAKIND_FLOAT);
  SampledField* sf_nc = const_cast<SampledField*>(&sf);
  size_t len;
  double* data = NULL;
  sf_nc->getUncompressedData(data, len);
  for (size_t d = 0; d < len; d++) {
    double val = data[d];
    if (val > 3.4028235e38 || val < -3.4028235e38 || (val > 0 && val < 1.17549e-38) || val < 0 && val > -1.17549e-38) {
      stringstream ss_msg;
      ss_msg << "A <spatialPoints>";
      if (sf.isSetId())
      {
        ss_msg << " with id '" << sf.getId() << "'";
      }
      ss_msg << " has an entry with the value '" << val;
      ss_msg << "', which is outside the range of single-precision 'float' values.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  free(data);
  inv(fail == false);
}
END_CONSTRAINT


// 1221656
START_CONSTRAINT(SpatialSampledFieldUIntArrayDataNotNegative, SampledField, sf)
{
  bool fail = false;
  pre(sf.isSetDataType());
  pre(sf.getDataType() == SPATIAL_DATAKIND_UINT || sf.getDataType() == SPATIAL_DATAKIND_UINT8 || sf.getDataType() == SPATIAL_DATAKIND_UINT16 || sf.getDataType() == SPATIAL_DATAKIND_UINT32);
  SampledField* sf_nc = const_cast<SampledField*>(&sf);
  size_t len;
  double* data = NULL;
  sf_nc->getUncompressedData(data, len);
  for (size_t d = 0; d < len; d++) {
    double val = data[d];
    if (val < 0) {
      stringstream ss_msg;
      ss_msg << "A <sampledField>";
      if (sf.isSetId())
      {
        ss_msg << " with id '" << sf.getId() << "'";
      }
      ss_msg << " has a data type of '" << sf.getDataTypeAsString();
      ss_msg << "', but has an entry with the value '" << val;
      ss_msg << "', which is negative.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  free(data);
  inv(fail == false);
}
END_CONSTRAINT


// 1224057
START_CONSTRAINT(SpatialSampledFieldIntArrayDataIntegers, SampledField, sf)
{
  bool fail = false;
  pre(sf.isSetDataType());
  pre(sf.getDataType() == SPATIAL_DATAKIND_INT || sf.getDataType() == SPATIAL_DATAKIND_UINT || sf.getDataType() == SPATIAL_DATAKIND_UINT8 || sf.getDataType() == SPATIAL_DATAKIND_UINT16 || sf.getDataType() == SPATIAL_DATAKIND_UINT32);
  SampledField* sf_nc = const_cast<SampledField*>(&sf);
  size_t len;
  double* data = NULL;
  sf_nc->getUncompressedData(data, len);
  for (size_t d = 0; d < len; d++) {
    double ival, val = data[d];
    if (modf(val, &ival) != 0) {
      stringstream ss_msg;
      ss_msg << "A <spatialPoints>";
      if (sf.isSetId())
      {
        ss_msg << " with id '" << sf.getId() << "'";
      }
      ss_msg << " has a data type of '" << sf.getDataTypeAsString();
      ss_msg << "', but has an entry with the value '" << val;
      ss_msg << "', which is not an integer.";
      msg = ss_msg.str();
      fail = true;
      break;
    }
  }
  free(data);
  inv(fail == false);
}
END_CONSTRAINT


// 1221704
START_CONSTRAINT(SpatialSampledVolumeDomainTypeMustBeDomainType, SampledVolume, sv)
{
  pre(sv.isSetDomainType());
  string domaintype = sv.getDomainType();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getDomainType(domaintype)==NULL);
  msg = "A <sampledVolume>";
  if (sv.isSetId()) {
    msg += " with the id '" + sv.getId() + "'";
  }
  msg += " has a value of '" + domaintype + "' for its 'domainType', but the <geometry> does not contain a <domainType> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1221906
START_CONSTRAINT(SpatialAnalyticVolumeDomainTypeMustBeDomainType, AnalyticVolume, av)
{
  pre(av.isSetDomainType());
  string domaintype = av.getDomainType();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getDomainType(domaintype)==NULL);
  msg = "An <analyticVolume>";
  if (av.isSetId()) {
    msg += " with the id '" + av.getId() + "'";
  }
  msg += " has a value of '" + domaintype + "' for its 'domainType', but the <geometry> does not contain a <domainType> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1222105
START_CONSTRAINT(SpatialParametricObjectDomainTypeMustBeDomainType, ParametricObject, po)
{
  pre(po.isSetDomainType());
  string domaintype = po.getDomainType();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getDomainType(domaintype)==NULL);
  msg = "A <parametricObject>";
  if (po.isSetId()) {
    msg += " with the id '" + po.getId() + "'";
  }
  msg += " has a value of '" + domaintype + "' for its 'domainType', but the <geometry> does not contain a <domainType> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1222305
START_CONSTRAINT(SpatialCSGObjectDomainTypeMustBeDomainType, CSGObject, csgo)
{
  pre(csgo.isSetDomainType());
  string domaintype = csgo.getDomainType();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getDomainType(domaintype)==NULL);
  msg = "A <csgObject>";
  if (csgo.isSetId()) {
    msg += " with the id '" + csgo.getId() + "'";
  }
  msg += " has a value of '" + domaintype + "' for its 'domainType', but the <geometry> does not contain a <domainType> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1223607
START_CONSTRAINT(SpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType, BoundaryCondition, bc)
{
  pre(bc.isSetBoundaryDomainType());
  string domaintype = bc.getBoundaryDomainType();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  pre(geom->getDomainType(domaintype)==NULL);
  msg = "A <boundaryCondition>";
  if (bc.isSetId()) {
    msg += " with the id '" + bc.getId() + "'";
  }
  msg += " has a value of '" + domaintype + "' for its 'boundaryDomainType', but the <geometry> does not contain a <domainType> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1223404
START_CONSTRAINT(SpatialDiffusionCoefficientVariableMustBeSpeciesOrParam, DiffusionCoefficient, dc)
{
  pre(dc.isSetVariable());
  string variable = dc.getVariable();
  pre(m.getSpecies(variable)==NULL && m.getParameter(variable)==NULL);
  msg = "A <diffusionCoefficient>";
  if (dc.isSetId()) {
    msg += " with the id '" + dc.getId() + "'";
  }
  msg += " has a value of '" + variable + "' for its 'variable', but the model does not contain a <species> or <parameter> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1223458
START_CONSTRAINT(SpatialDiffusionCoefficientVariableMustNotBeSelf, DiffusionCoefficient, dc)
{
    pre(dc.isSetVariable());
    const SBase* parent = dc.getParentSBMLObject();
    pre(parent != NULL);
    pre(parent->getId() == dc.getVariable());

    stringstream ss_msg;
    ss_msg << "A <diffusionCoefficient>";
    if (dc.isSetId())
    {
        ss_msg << " with id '" << dc.getId() << "'";
    }
    ss_msg << " references its parent parameter '" << dc.getVariable() << "'.";
    msg = ss_msg.str();
    inv(false);
}
END_CONSTRAINT


// 1223606
START_CONSTRAINT(SpatialBoundaryConditionCoordinateBoundaryMustBeBoundary, BoundaryCondition, bc)
{
  pre(bc.isSetCoordinateBoundary());
  string boundary = bc.getCoordinateBoundary();
  SpatialModelPlugin *mplug = (SpatialModelPlugin*)(m.getPlugin("spatial"));
  pre(mplug != NULL);
  Geometry* geom = mplug->getGeometry();
  pre(geom != NULL);
  for (unsigned long cc = 0; cc < geom->getNumCoordinateComponents(); cc++)
  {
    const CoordinateComponent* coord = geom->getCoordinateComponent(cc);
    if (coord->isSetBoundaryMax()) {
        pre(coord->getBoundaryMax()->getId() != boundary);
    }
    if (coord->isSetBoundaryMin()) {
        pre(coord->getBoundaryMin()->getId() != boundary);
    }
  }
  msg = "A <boundaryCondition>";
  if (bc.isSetId()) {
    msg += " with the id '" + bc.getId() + "'";
  }
  msg += " has a value of '" + boundary+ "' for its 'coordinateBoundary', but the <geometry> does not contain a <boundaryMax> or <boundaryMin> with that id.";

  inv(false);
}
END_CONSTRAINT


// 1221250
START_CONSTRAINT(SpatialOneGeometryDefinitionMustBeActive, Geometry, g)
{
  pre(g.getNumGeometryDefinitions() > 0);
  for (unsigned long gdn = 0; gdn < g.getNumGeometryDefinitions(); gdn++)
  {
    const GeometryDefinition* gd = g.getGeometryDefinition(gdn);
    pre(gd->getIsActive() == false);
  }
  msg = "No <geometryDefinition> was found with an 'isActive' value of 'true'.";
  inv(false);
}
END_CONSTRAINT


// 1223850
START_CONSTRAINT(SpatialMixedGeometryChildrenNotActive, MixedGeometry, mg)
{
  bool fail = false;
  pre(mg.getNumGeometryDefinitions() > 0);
  for (unsigned long gdn = 0; gdn < mg.getNumGeometryDefinitions(); gdn++)
  {
    const GeometryDefinition* gd = mg.getGeometryDefinition(gdn);
    if (gd->getIsActive() == true)
    {
      msg = "A <mixedGeometry>";
      if (mg.isSetId())
      {
        msg += " with the id '" + mg.getId() + "'";
      }
      msg += " has a child <" + gd->getElementName() + ">";
      if (gd->isSetId())
      {
        msg += " with the id '" + gd->getId() + "'";
      }
      msg += " with an 'isActive' value of 'true'.";
      fail = true;
      break;
    }
  }
  inv(fail == false);
}
END_CONSTRAINT


// 1223650
START_CONSTRAINT(SpatialBoundaryConditionBoundaryDomainTypeOrCoordinateBoundary, BoundaryCondition, bc)
{
  bool fail = false;

  msg = "A <boundaryCondition>";
  if (bc.isSetId()) {
    msg += " with the id '" + bc.getId() + "'";
  }
  if (bc.isSetBoundaryDomainType() && bc.isSetCoordinateBoundary()) 
  {
    msg += " has a value of '" + bc.getBoundaryDomainType() + "' for its 'boundaryDomainType', and a value of '" + bc.getCoordinateBoundary() + "' for its 'coordinateBoundary'.  It must instead have one or the other.";
    fail = true;
  }
  else if (!bc.isSetBoundaryDomainType() && !bc.isSetCoordinateBoundary()) 
  {
    msg += " does not have a value for its 'boundaryDomainType' nor its 'coordinateBoundary' attributes.  It must have one or the other.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 1223751
START_CONSTRAINT(SpatialGeometryLOCoordinateComponentsOneToThreeChildren, Geometry, geom)
{
  unsigned int naxes = geom.getNumCoordinateComponents();
  pre(naxes < 1 || naxes>3);
  stringstream ss_msg;
  ss_msg << "The <geometry>";
  if (geom.isSetId())
  {
    ss_msg << " with id '" << geom.getId() << "'";
  }
  ss_msg <<  " has " << naxes << " child <coordinateComponents>, but must have 1 to 3 instead.";
  msg = ss_msg.str();


  inv(false);
}
END_CONSTRAINT


// 1223752
START_CONSTRAINT(SpatialGeometryCoordinateComponent1DisX, Geometry, geom)
{
  unsigned int naxes = geom.getNumCoordinateComponents();
  pre(naxes == 1);
  const CoordinateComponent* cc = geom.getCoordinateComponent(0);
  pre(cc->isSetType());
  pre(cc->getType() != SPATIAL_COORDINATEKIND_CARTESIAN_X);
  stringstream ss_msg;
  ss_msg << "The <geometry>";
  if (geom.isSetId())
  {
    ss_msg << " with id '" << geom.getId() << "'";
  }
  ss_msg << " has only one child <coordinateComponents>, but its type is '";
  ss_msg << cc->getTypeAsString() << "' instead of 'cartesianX'.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1223753
START_CONSTRAINT(SpatialGeometryCoordinateComponent2DisXY, Geometry, geom)
{
  unsigned int naxes = geom.getNumCoordinateComponents();
  pre(naxes == 2);
  const CoordinateComponent* cc1 = geom.getCoordinateComponent(0);
  const CoordinateComponent* cc2 = geom.getCoordinateComponent(1);
  pre(cc1->isSetType());
  pre(cc2->isSetType());
  CoordinateKind_t kind1 = cc1->getType();
  CoordinateKind_t kind2 = cc2->getType();
  std::set<CoordinateKind_t> kinds, rightkinds;
  kinds.insert(kind1);
  kinds.insert(kind2);
  rightkinds.insert(SPATIAL_COORDINATEKIND_CARTESIAN_X);
  rightkinds.insert(SPATIAL_COORDINATEKIND_CARTESIAN_Y);
  pre(kinds != rightkinds);

  stringstream ss_msg;
  ss_msg << "The <geometry>";
  if (geom.isSetId())
  {
    ss_msg << " with id '" << geom.getId() << "'";
  }
  ss_msg << " has two child <coordinateComponents>, but their two types are '";
  ss_msg << cc1->getTypeAsString() << "' and '";
  ss_msg << cc2->getTypeAsString() << "' instead of 'cartesianX' and 'cartesianY'.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 1223754
START_CONSTRAINT(SpatialGeometryCoordinateComponent3DisXYZ, Geometry, geom)
{
  unsigned int naxes = geom.getNumCoordinateComponents();
  pre(naxes == 3);
  const CoordinateComponent* cc1 = geom.getCoordinateComponent(0);
  const CoordinateComponent* cc2 = geom.getCoordinateComponent(1);
  const CoordinateComponent* cc3 = geom.getCoordinateComponent(2);
  pre(cc1->isSetType());
  pre(cc2->isSetType());
  pre(cc3->isSetType());
  CoordinateKind_t kind1 = cc1->getType();
  CoordinateKind_t kind2 = cc2->getType();
  CoordinateKind_t kind3 = cc3->getType();
  std::set<CoordinateKind_t> kinds, rightkinds;
  kinds.insert(kind1);
  kinds.insert(kind2);
  kinds.insert(kind3);
  rightkinds.insert(SPATIAL_COORDINATEKIND_CARTESIAN_X);
  rightkinds.insert(SPATIAL_COORDINATEKIND_CARTESIAN_Y);
  rightkinds.insert(SPATIAL_COORDINATEKIND_CARTESIAN_Z);
  pre(kinds != rightkinds);

  stringstream ss_msg;
  ss_msg << "The <geometry>";
  if (geom.isSetId())
  {
    ss_msg << " with id '" << geom.getId() << "'";
  }
  ss_msg << " has two child <coordinateComponents>, but their two types are '";
  ss_msg << cc1->getTypeAsString() << "', '";
  ss_msg << cc2->getTypeAsString() << "', and '";
  ss_msg << cc3->getTypeAsString() << "', instead of 'cartesianX', 'cartesianY', and 'cartesianZ'.";
  msg = ss_msg.str();

  inv(false);
}
END_CONSTRAINT


// 122__
//START_CONSTRAINT(Spatial, Class, class)
//{
//  bool fail = false;
//
//  inv(fail == false);
//}
//END_CONSTRAINT




/** @endcond */

