
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

#endif /* AddingConstraintsToValidator */

#include <sbml/validator/ConstraintMacros.h>

using namespace std;

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


/** @endcond */

