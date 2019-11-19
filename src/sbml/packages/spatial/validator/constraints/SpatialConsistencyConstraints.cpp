
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/sbml/Geometry.h>
#include <sbml/packages/spatial/validator/SpatialCompartmentMappingUnitSizesCheck.h>
#include <sbml/packages/spatial/validator/SpatialSpatialSymbolReferenceUniqueRefCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueDiffusionCoefficientsCheck.h>
#include <sbml/packages/spatial/validator/SpatialUniqueAdvectionCoefficientsCheck.h>
#include <sbml/Species.h>

#endif /* AddingConstraintsToValidator */

#include <sbml/validator/ConstraintMacros.h>

using namespace std;

EXTERN_CONSTRAINT(ErrorEnumValue, SpatialCompartmentMappingUnitSizesCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialSpatialSymbolReferenceUniqueRefCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueDiffusionCoefficientsCheck);
EXTERN_CONSTRAINT(ErrorEnumValue, SpatialUniqueAdvectionCoefficientsCheck);



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
START_CONSTRAINT(SpatialSampledVolumeNeedsMaxWithMin, SampledVolume, svol)
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

  inv(fail == false);
}
END_CONSTRAINT


// 1221751
START_CONSTRAINT(SpatialSampledVolumeNeedsMinWithMax, SampledVolume, svol)
{
  bool fail = false;
  if (svol.isSetMaxValue() && !svol.isSetMinValue() && !svol.isSetSampledValue()) {
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

  inv(fail == false);
}
END_CONSTRAINT


// 1221752
START_CONSTRAINT(SpatialSampledVolumeSampledValueMinMax, SampledVolume, svol)
{
  bool fail = false;
  if (svol.isSetMaxValue() && svol.isSetMinValue() && svol.isSetSampledValue()) {
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


// 122__
//START_CONSTRAINT(Spatial, Class, class)
//{
//  bool fail = false;
//
//  inv(fail == false);
//}
//END_CONSTRAINT




/** @endcond */

