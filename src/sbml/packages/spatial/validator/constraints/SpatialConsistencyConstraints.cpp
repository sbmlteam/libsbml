
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/sbml/Geometry.h>
#include <sbml/packages/spatial/validator/SpatialCompartmentMappingUnitSizesCheck.h>
#include <sbml/packages/spatial/validator/SpatialSpatialSymbolReferenceUniqueRefCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueDiffusionCoefficientsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueAdvectionCoefficientsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueBoundaryConditionsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueSampledVolumeValueCheck.h>
#include <sbml/packages/spatial/validator/SpatialSampledVolumeValueNotInRangeCheck.h>
#include <sbml/packages/spatial/validator/SpatialSampledVolumeRangeOverlapCheck.h>
#include <sbml/Species.h>

#endif /* AddingConstraintsToValidator */

#include <sbml/validator/ConstraintMacros.h>

using namespace std;

EXTERN_CONSTRAINT(ErrorEnumValue, SpatialCompartmentMappingUnitSizesCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialSpatialSymbolReferenceUniqueRefCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueDiffusionCoefficientsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueAdvectionCoefficientsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueBoundaryConditionsCheck);
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

  const Geometry* geometry = static_cast<const Geometry*>(domaintype.getParentSBMLObject()->getParentSBMLObject());
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

  const Geometry* geometry = static_cast<const Geometry*>(domaintype.getParentSBMLObject()->getParentSBMLObject());
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

  const Geometry* geometry = static_cast<const Geometry*>(domaintype.getParentSBMLObject()->getParentSBMLObject());
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
  const Geometry* geometry = static_cast<const Geometry*>(sfield.getParentSBMLObject()->getParentSBMLObject());
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
  const Geometry* geometry = static_cast<const Geometry*>(sfield.getParentSBMLObject()->getParentSBMLObject());
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
  const Geometry* geometry = static_cast<const Geometry*>(sfield.getParentSBMLObject()->getParentSBMLObject());
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
  const SBase* ref = model->getElementBySId(ssr.getSpatialRef());
  if (ref == NULL) {
    fail = true;
    msg += ", but no object with that ID could be found.";
  }
  else {
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

// 1223455
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
START_CONSTRAINT(SpatialAdvectionCoefficientVariableMustBeSpecies, AdvectionCoefficient, ac)
{
  bool fail = false;
  pre(ac.isSetVariable());

  if (m.getSpecies(ac.getVariable()) == NULL) {
    fail = true;
    stringstream ss_msg;
    ss_msg << "An <advectionCoefficient>";
    if (ac.isSetId())
    {
      ss_msg << " with id '" << ac.getId() << "'";
    }
    ss_msg << " references a variable '" << ac.getVariable() << "', which is not the ID of a <species> in the <model>.";
    msg = ss_msg.str();
  }

  inv(fail == false);
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


// 122__
//START_CONSTRAINT(Spatial, Class, class)
//{
//  bool fail = false;
//
//  inv(fail == false);
//}
//END_CONSTRAINT


// 122__
//START_CONSTRAINT(Spatial, Class, class)
//{
//  bool fail = false;
//
//  inv(fail == false);
//}
//END_CONSTRAINT




/** @endcond */

