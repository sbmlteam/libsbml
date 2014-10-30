/**
 * @file:   SpatialValidator.cpp
 * @brief:  Implementation of the SpatialValidator class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/validator/VConstraint.h>

#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/packages/spatial/validator/SpatialValidator.h>

  /** @cond doxygenLibsbmlInternal */

using namespace std;

  /** @endcond doxygenLibsbmlInternal */


LIBSBML_CPP_NAMESPACE_BEGIN

//
// NOTE: ConstraintSet, ValidatorConstraints, and ValidatingVisitor used to
// be in separate .cpp and .h files, but in order to link under MSVC6 (the
// compiler doesn't instantiate templates (i.e. generate code), even when
// told explicitly to do so), the classes needed to be combined into a single
// file.
//


// ----------------------------------------------------------------------
// Apply<T> and ConstraintSet<T>
// ----------------------------------------------------------------------


/*
 * Applies a Constraint<T> to an SBML object of type T.
 */
template <typename T>
struct Apply : public unary_function<TConstraint<T>*, void>
{
  Apply (const Model& m, const T& o) : model(m), object(o) { }


  void operator() (TConstraint<T>* constraint)
  {
    constraint->check(model, object);
  }


  const Model& model;
  const T&     object;
};


template <typename T>
class ConstraintSet
{
public:

   ConstraintSet () { }
  ~ConstraintSet () { }


  /*
   * Adds a Constraint to this ConstraintSet.
   */
  void add (TConstraint<T>* c)
  {
    constraints.push_back(c);
  }

  /*
   * Applies all Constraints in this ConstraintSet to the given SBML object
   * of type T.  Constraint violations are logged to Validator.
   */
  void applyTo (const Model& model, const T& object)
  {
    for_each(constraints.begin(), constraints.end(), Apply<T>(model, object));
  }

  /*
   * @return true if this ConstraintSet is empty, false otherwise.
   */
  bool empty () const
  {
    return constraints.empty();
  }


protected:

  std::list< TConstraint<T>* > constraints;
};



// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// ValidatorConstraints
// ----------------------------------------------------------------------

/*
 * ValidatorConstraints maintain a separate list of constraints for each
 * SBML type.  This is done so that constraints may be applied efficiently
 * during the validation process.
 */
struct SpatialValidatorConstraints
{
  ConstraintSet<SBMLDocument>             mSBMLDocument;
  ConstraintSet<Model>                    mModel;
  ConstraintSet<DomainType>      mDomainType;
  ConstraintSet<Domain>      mDomain;
  ConstraintSet<InteriorPoint>      mInteriorPoint;
  ConstraintSet<Boundary>      mBoundary;
  ConstraintSet<AdjacentDomains>      mAdjacentDomains;
  ConstraintSet<GeometryDefinition>      mGeometryDefinition;
  ConstraintSet<CompartmentMapping>      mCompartmentMapping;
  ConstraintSet<CoordinateComponent>      mCoordinateComponent;
  ConstraintSet<SampledFieldGeometry>      mSampledFieldGeometry;
  ConstraintSet<SampledField>      mSampledField;
  ConstraintSet<SampledVolume>      mSampledVolume;
  ConstraintSet<AnalyticGeometry>      mAnalyticGeometry;
  ConstraintSet<AnalyticVolume>      mAnalyticVolume;
  ConstraintSet<ParametricGeometry>      mParametricGeometry;
  ConstraintSet<ParametricObject>      mParametricObject;
  ConstraintSet<PolygonObject>      mPolygonObject;
  ConstraintSet<SpatialPoint>      mSpatialPoint;
  ConstraintSet<CSGeometry>      mCSGeometry;
  ConstraintSet<CSGObject>      mCSGObject;
  ConstraintSet<CSGNode>      mCSGNode;
  ConstraintSet<CSGTransformation>      mCSGTransformation;
  ConstraintSet<CSGTranslation>      mCSGTranslation;
  ConstraintSet<CSGRotation>      mCSGRotation;
  ConstraintSet<CSGScale>      mCSGScale;
  ConstraintSet<CSGHomogeneousTransformation>      mCSGHomogeneousTransformation;
  ConstraintSet<TransformationComponents>      mTransformationComponents;
  ConstraintSet<CSGPrimitive>      mCSGPrimitive;
  ConstraintSet<CSGPseudoPrimitive>      mCSGPseudoPrimitive;
  ConstraintSet<CSGSetOperator>      mCSGSetOperator;
  ConstraintSet<SpatialSymbolReference>      mSpatialSymbolReference;
  ConstraintSet<DiffusionCoefficient>      mDiffusionCoefficient;
  ConstraintSet<AdvectionCoefficient>      mAdvectionCoefficient;
  ConstraintSet<BoundaryCondition>      mBoundaryCondition;
  ConstraintSet<Geometry>      mGeometry;
  ConstraintSet<CoordinateReference>      mCoordinateReference;
  ConstraintSet<MixedGeometry>      mMixedGeometry;
  ConstraintSet<OrdinalMapping>      mOrdinalMapping;
  map<VConstraint*,bool> ptrMap;

  ~SpatialValidatorConstraints ();
  void add (VConstraint* c);
};


/*
 * Deletes constraints (TConstraint(T>*) which are stored in lists
 * (ConstraintSet<T>) of this struct.
 * Since the same pointer values could be stored in different lists
 * (e.g., TConstraint<SimpleSpeciesReference>* is stored in both
 * ConstraintSet<SimpleSpeciesReference> and
 * ConstraintSet<ModifierSimpleSpeciesReference>), a pointer map is used for
 * avoiding segmentation fault caused by deleting the same pointer twice.
 */
SpatialValidatorConstraints::~SpatialValidatorConstraints ()
{
  map<VConstraint*,bool>::iterator it = ptrMap.begin();

  while(it != ptrMap.end())
  {
     if(it->second) delete it->first;
     ++it;
  }
}


/*
 * Adds the given Contraint to the appropriate ConstraintSet.
 */
void
SpatialValidatorConstraints::add (VConstraint* c)
{
  if (c == NULL) return;

  ptrMap.insert(pair<VConstraint*,bool>(c,true));

  if (dynamic_cast< TConstraint<SBMLDocument>* >(c) != NULL)
  {
    mSBMLDocument.add( static_cast< TConstraint<SBMLDocument>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Model>* >(c) != NULL)
  {
    mModel.add( static_cast< TConstraint<Model>* >(c) );
    return;
  }
  if (dynamic_cast< TConstraint<DomainType>* >(c) != NULL)
  {
    mDomainType.add( static_cast< TConstraint<DomainType>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Domain>* >(c) != NULL)
  {
    mDomain.add( static_cast< TConstraint<Domain>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<InteriorPoint>* >(c) != NULL)
  {
    mInteriorPoint.add( static_cast< TConstraint<InteriorPoint>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Boundary>* >(c) != NULL)
  {
    mBoundary.add( static_cast< TConstraint<Boundary>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<AdjacentDomains>* >(c) != NULL)
  {
    mAdjacentDomains.add( static_cast< TConstraint<AdjacentDomains>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GeometryDefinition>* >(c) != NULL)
  {
    mGeometryDefinition.add( static_cast< TConstraint<GeometryDefinition>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CompartmentMapping>* >(c) != NULL)
  {
    mCompartmentMapping.add( static_cast< TConstraint<CompartmentMapping>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CoordinateComponent>* >(c) != NULL)
  {
    mCoordinateComponent.add( static_cast< TConstraint<CoordinateComponent>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SampledFieldGeometry>* >(c) != NULL)
  {
    mSampledFieldGeometry.add( static_cast< TConstraint<SampledFieldGeometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SampledField>* >(c) != NULL)
  {
    mSampledField.add( static_cast< TConstraint<SampledField>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SampledVolume>* >(c) != NULL)
  {
    mSampledVolume.add( static_cast< TConstraint<SampledVolume>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<AnalyticGeometry>* >(c) != NULL)
  {
    mAnalyticGeometry.add( static_cast< TConstraint<AnalyticGeometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<AnalyticVolume>* >(c) != NULL)
  {
    mAnalyticVolume.add( static_cast< TConstraint<AnalyticVolume>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<ParametricGeometry>* >(c) != NULL)
  {
    mParametricGeometry.add( static_cast< TConstraint<ParametricGeometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<ParametricObject>* >(c) != NULL)
  {
    mParametricObject.add( static_cast< TConstraint<ParametricObject>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<PolygonObject>* >(c) != NULL)
  {
    mPolygonObject.add( static_cast< TConstraint<PolygonObject>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SpatialPoint>* >(c) != NULL)
  {
    mSpatialPoint.add( static_cast< TConstraint<SpatialPoint>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGeometry>* >(c) != NULL)
  {
    mCSGeometry.add( static_cast< TConstraint<CSGeometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGObject>* >(c) != NULL)
  {
    mCSGObject.add( static_cast< TConstraint<CSGObject>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGNode>* >(c) != NULL)
  {
    mCSGNode.add( static_cast< TConstraint<CSGNode>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGTransformation>* >(c) != NULL)
  {
    mCSGTransformation.add( static_cast< TConstraint<CSGTransformation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGTranslation>* >(c) != NULL)
  {
    mCSGTranslation.add( static_cast< TConstraint<CSGTranslation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGRotation>* >(c) != NULL)
  {
    mCSGRotation.add( static_cast< TConstraint<CSGRotation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGScale>* >(c) != NULL)
  {
    mCSGScale.add( static_cast< TConstraint<CSGScale>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGHomogeneousTransformation>* >(c) != NULL)
  {
    mCSGHomogeneousTransformation.add( static_cast< TConstraint<CSGHomogeneousTransformation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<TransformationComponents>* >(c) != NULL)
  {
    mTransformationComponents.add( static_cast< TConstraint<TransformationComponents>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGPrimitive>* >(c) != NULL)
  {
    mCSGPrimitive.add( static_cast< TConstraint<CSGPrimitive>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGPseudoPrimitive>* >(c) != NULL)
  {
    mCSGPseudoPrimitive.add( static_cast< TConstraint<CSGPseudoPrimitive>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CSGSetOperator>* >(c) != NULL)
  {
    mCSGSetOperator.add( static_cast< TConstraint<CSGSetOperator>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SpatialSymbolReference>* >(c) != NULL)
  {
    mSpatialSymbolReference.add( static_cast< TConstraint<SpatialSymbolReference>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DiffusionCoefficient>* >(c) != NULL)
  {
    mDiffusionCoefficient.add( static_cast< TConstraint<DiffusionCoefficient>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<AdvectionCoefficient>* >(c) != NULL)
  {
    mAdvectionCoefficient.add( static_cast< TConstraint<AdvectionCoefficient>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<BoundaryCondition>* >(c) != NULL)
  {
    mBoundaryCondition.add( static_cast< TConstraint<BoundaryCondition>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Geometry>* >(c) != NULL)
  {
    mGeometry.add( static_cast< TConstraint<Geometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<CoordinateReference>* >(c) != NULL)
  {
    mCoordinateReference.add( static_cast< TConstraint<CoordinateReference>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<MixedGeometry>* >(c) != NULL)
  {
    mMixedGeometry.add( static_cast< TConstraint<MixedGeometry>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<OrdinalMapping>* >(c) != NULL)
  {
    mOrdinalMapping.add( static_cast< TConstraint<OrdinalMapping>* >(c) );
    return;
  }

}

// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// ValidatingVisitor
// ----------------------------------------------------------------------


/*
 * An SBMLVisitor visits each object in an SBML object tree, calling the
 * appropriate visit() method for the object visited.
 *
 * A ValidatingVisitor overrides each visit method to validate the given
 * SBML object.
 */
class SpatialValidatingVisitor: public SBMLVisitor
{
public:

  SpatialValidatingVisitor (SpatialValidator& v, const Model& m) : v(v), m(m) { }

  using SBMLVisitor::visit;

  bool visit (const DomainType &x)
  {
    v.mSpatialConstraints->mDomainType.applyTo(m, x);
    return !v.mSpatialConstraints->mDomainType.empty();
  }

  bool visit (const Domain &x)
  {
    v.mSpatialConstraints->mDomain.applyTo(m, x);
    return !v.mSpatialConstraints->mDomain.empty();
  }

  bool visit (const InteriorPoint &x)
  {
    v.mSpatialConstraints->mInteriorPoint.applyTo(m, x);
    return !v.mSpatialConstraints->mInteriorPoint.empty();
  }

  bool visit (const Boundary &x)
  {
    v.mSpatialConstraints->mBoundary.applyTo(m, x);
    return !v.mSpatialConstraints->mBoundary.empty();
  }

  bool visit (const AdjacentDomains &x)
  {
    v.mSpatialConstraints->mAdjacentDomains.applyTo(m, x);
    return !v.mSpatialConstraints->mAdjacentDomains.empty();
  }

  bool visit (const GeometryDefinition &x)
  {
    v.mSpatialConstraints->mGeometryDefinition.applyTo(m, x);
    return !v.mSpatialConstraints->mGeometryDefinition.empty();
  }

  bool visit (const CompartmentMapping &x)
  {
    v.mSpatialConstraints->mCompartmentMapping.applyTo(m, x);
    return !v.mSpatialConstraints->mCompartmentMapping.empty();
  }

  bool visit (const CoordinateComponent &x)
  {
    v.mSpatialConstraints->mCoordinateComponent.applyTo(m, x);
    return !v.mSpatialConstraints->mCoordinateComponent.empty();
  }

  bool visit (const SampledFieldGeometry &x)
  {
    v.mSpatialConstraints->mSampledFieldGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mSampledFieldGeometry.empty();
  }

  bool visit (const SampledField &x)
  {
    v.mSpatialConstraints->mSampledField.applyTo(m, x);
    return !v.mSpatialConstraints->mSampledField.empty();
  }

  bool visit (const SampledVolume &x)
  {
    v.mSpatialConstraints->mSampledVolume.applyTo(m, x);
    return !v.mSpatialConstraints->mSampledVolume.empty();
  }

  bool visit (const AnalyticGeometry &x)
  {
    v.mSpatialConstraints->mAnalyticGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mAnalyticGeometry.empty();
  }

  bool visit (const AnalyticVolume &x)
  {
    v.mSpatialConstraints->mAnalyticVolume.applyTo(m, x);
    return !v.mSpatialConstraints->mAnalyticVolume.empty();
  }

  bool visit (const ParametricGeometry &x)
  {
    v.mSpatialConstraints->mParametricGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mParametricGeometry.empty();
  }

  bool visit (const ParametricObject &x)
  {
    v.mSpatialConstraints->mParametricObject.applyTo(m, x);
    return !v.mSpatialConstraints->mParametricObject.empty();
  }

  bool visit (const PolygonObject &x)
  {
    v.mSpatialConstraints->mPolygonObject.applyTo(m, x);
    return !v.mSpatialConstraints->mPolygonObject.empty();
  }

  bool visit (const SpatialPoint &x)
  {
    v.mSpatialConstraints->mSpatialPoint.applyTo(m, x);
    return !v.mSpatialConstraints->mSpatialPoint.empty();
  }

  bool visit (const CSGeometry &x)
  {
    v.mSpatialConstraints->mCSGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGeometry.empty();
  }

  bool visit (const CSGObject &x)
  {
    v.mSpatialConstraints->mCSGObject.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGObject.empty();
  }

  bool visit (const CSGNode &x)
  {
    v.mSpatialConstraints->mCSGNode.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGNode.empty();
  }

  bool visit (const CSGTransformation &x)
  {
    v.mSpatialConstraints->mCSGTransformation.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGTransformation.empty();
  }

  bool visit (const CSGTranslation &x)
  {
    v.mSpatialConstraints->mCSGTranslation.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGTranslation.empty();
  }

  bool visit (const CSGRotation &x)
  {
    v.mSpatialConstraints->mCSGRotation.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGRotation.empty();
  }

  bool visit (const CSGScale &x)
  {
    v.mSpatialConstraints->mCSGScale.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGScale.empty();
  }

  bool visit (const CSGHomogeneousTransformation &x)
  {
    v.mSpatialConstraints->mCSGHomogeneousTransformation.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGHomogeneousTransformation.empty();
  }

  bool visit (const TransformationComponents &x)
  {
    v.mSpatialConstraints->mTransformationComponents.applyTo(m, x);
    return !v.mSpatialConstraints->mTransformationComponents.empty();
  }

  bool visit (const CSGPrimitive &x)
  {
    v.mSpatialConstraints->mCSGPrimitive.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGPrimitive.empty();
  }

  bool visit (const CSGPseudoPrimitive &x)
  {
    v.mSpatialConstraints->mCSGPseudoPrimitive.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGPseudoPrimitive.empty();
  }

  bool visit (const CSGSetOperator &x)
  {
    v.mSpatialConstraints->mCSGSetOperator.applyTo(m, x);
    return !v.mSpatialConstraints->mCSGSetOperator.empty();
  }

  bool visit (const SpatialSymbolReference &x)
  {
    v.mSpatialConstraints->mSpatialSymbolReference.applyTo(m, x);
    return !v.mSpatialConstraints->mSpatialSymbolReference.empty();
  }

  bool visit (const DiffusionCoefficient &x)
  {
    v.mSpatialConstraints->mDiffusionCoefficient.applyTo(m, x);
    return !v.mSpatialConstraints->mDiffusionCoefficient.empty();
  }

  bool visit (const AdvectionCoefficient &x)
  {
    v.mSpatialConstraints->mAdvectionCoefficient.applyTo(m, x);
    return !v.mSpatialConstraints->mAdvectionCoefficient.empty();
  }

  bool visit (const BoundaryCondition &x)
  {
    v.mSpatialConstraints->mBoundaryCondition.applyTo(m, x);
    return !v.mSpatialConstraints->mBoundaryCondition.empty();
  }

  bool visit (const Geometry &x)
  {
    v.mSpatialConstraints->mGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mGeometry.empty();
  }

  bool visit (const CoordinateReference &x)
  {
    v.mSpatialConstraints->mCoordinateReference.applyTo(m, x);
    return !v.mSpatialConstraints->mCoordinateReference.empty();
  }

  bool visit (const MixedGeometry &x)
  {
    v.mSpatialConstraints->mMixedGeometry.applyTo(m, x);
    return !v.mSpatialConstraints->mMixedGeometry.empty();
  }

  bool visit (const OrdinalMapping &x)
  {
    v.mSpatialConstraints->mOrdinalMapping.applyTo(m, x);
    return !v.mSpatialConstraints->mOrdinalMapping.empty();
  }

  virtual bool visit(const SBase &x)
  {
    if (&x == NULL || x.getPackageName() != "spatial")
    {
      return SBMLVisitor::visit(x);
    }

    int code = x.getTypeCode();

    const ListOf* list = dynamic_cast<const ListOf*>(&x);

    if (list != NULL)
    {
      return SBMLVisitor::visit(x);
    }
    else
    {
      if (code == SBML_SPATIAL_DOMAINTYPE)
      {
        return visit((const DomainType&)x);
      }
      else if (code == SBML_SPATIAL_DOMAIN)
      {
        return visit((const Domain&)x);
      }
      else if (code == SBML_SPATIAL_INTERIORPOINT)
      {
        return visit((const InteriorPoint&)x);
      }
      else if (code == SBML_SPATIAL_BOUNDARY)
      {
        return visit((const Boundary&)x);
      }
      else if (code == SBML_SPATIAL_ADJACENTDOMAINS)
      {
        return visit((const AdjacentDomains&)x);
      }
      else if (code == SBML_SPATIAL_GEOMETRYDEFINITION)
      {
        return visit((const GeometryDefinition&)x);
      }
      else if (code == SBML_SPATIAL_COMPARTMENTMAPPING)
      {
        return visit((const CompartmentMapping&)x);
      }
      else if (code == SBML_SPATIAL_COORDINATECOMPONENT)
      {
        return visit((const CoordinateComponent&)x);
      }
      else if (code == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY)
      {
        return visit((const SampledFieldGeometry&)x);
      }
      else if (code == SBML_SPATIAL_SAMPLEDFIELD)
      {
        return visit((const SampledField&)x);
      }
      else if (code == SBML_SPATIAL_SAMPLEDVOLUME)
      {
        return visit((const SampledVolume&)x);
      }
      else if (code == SBML_SPATIAL_ANALYTICGEOMETRY)
      {
        return visit((const AnalyticGeometry&)x);
      }
      else if (code == SBML_SPATIAL_ANALYTICVOLUME)
      {
        return visit((const AnalyticVolume&)x);
      }
      else if (code == SBML_SPATIAL_PARAMETRICGEOMETRY)
      {
        return visit((const ParametricGeometry&)x);
      }
      else if (code == SBML_SPATIAL_PARAMETRICOBJECT)
      {
        return visit((const ParametricObject&)x);
      }
      else if (code == SBML_SPATIAL_POLYGONOBJECT)
      {
        return visit((const PolygonObject&)x);
      }
      else if (code == SBML_SPATIAL_SPATIALPOINT)
      {
        return visit((const SpatialPoint&)x);
      }
      else if (code == SBML_SPATIAL_CSGEOMETRY)
      {
        return visit((const CSGeometry&)x);
      }
      else if (code == SBML_SPATIAL_CSGOBJECT)
      {
        return visit((const CSGObject&)x);
      }
      else if (code == SBML_SPATIAL_CSGNODE)
      {
        return visit((const CSGNode&)x);
      }
      else if (code == SBML_SPATIAL_CSGTRANSFORMATION)
      {
        return visit((const CSGTransformation&)x);
      }
      else if (code == SBML_SPATIAL_CSGTRANSLATION)
      {
        return visit((const CSGTranslation&)x);
      }
      else if (code == SBML_SPATIAL_CSGROTATION)
      {
        return visit((const CSGRotation&)x);
      }
      else if (code == SBML_SPATIAL_CSGSCALE)
      {
        return visit((const CSGScale&)x);
      }
      else if (code == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)
      {
        return visit((const CSGHomogeneousTransformation&)x);
      }
      else if (code == SBML_SPATIAL_TRANSFORMATIONCOMPONENTS)
      {
        return visit((const TransformationComponents&)x);
      }
      else if (code == SBML_SPATIAL_CSGPRIMITIVE)
      {
        return visit((const CSGPrimitive&)x);
      }
      else if (code == SBML_SPATIAL_CSGPSEUDOPRIMITIVE)
      {
        return visit((const CSGPseudoPrimitive&)x);
      }
      else if (code == SBML_SPATIAL_CSGSETOPERATOR)
      {
        return visit((const CSGSetOperator&)x);
      }
      else if (code == SBML_SPATIAL_SPATIALSYMBOLREFERENCE)
      {
        return visit((const SpatialSymbolReference&)x);
      }
      else if (code == SBML_SPATIAL_DIFFUSIONCOEFFICIENT)
      {
        return visit((const DiffusionCoefficient&)x);
      }
      else if (code == SBML_SPATIAL_ADVECTIONCOEFFICIENT)
      {
        return visit((const AdvectionCoefficient&)x);
      }
      else if (code == SBML_SPATIAL_BOUNDARYCONDITION)
      {
        return visit((const BoundaryCondition&)x);
      }
      else if (code == SBML_SPATIAL_GEOMETRY)
      {
        return visit((const Geometry&)x);
      }
      else if (code == SBML_SPATIAL_COORDINATEREFERENCE)
      {
        return visit((const CoordinateReference&)x);
      }
      else if (code == SBML_SPATIAL_MIXEDGEOMETRY)
      {
        return visit((const MixedGeometry&)x);
      }
      else if (code == SBML_SPATIAL_ORDINALMAPPING)
      {
        return visit((const OrdinalMapping&)x);
      }
      else 
      {
        return SBMLVisitor::visit(x);
      } 
    }
  }

protected:

  SpatialValidator&   v;
  const Model& m;
};


// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// Validator
// ----------------------------------------------------------------------


SpatialValidator::SpatialValidator (const SBMLErrorCategory_t category):
  Validator(category)
{
  mSpatialConstraints = new SpatialValidatorConstraints();
}


SpatialValidator::~SpatialValidator ()
{
  delete mSpatialConstraints;
}


/*
 * Adds the given Contraint to this validator.
 */
void
SpatialValidator::addConstraint (VConstraint* c)
{
  mSpatialConstraints->add(c);
}


/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
SpatialValidator::validate (const SBMLDocument& d)
{
  if (&d == NULL) return 0;

  const Model* m = d.getModel();

  if (m != NULL)
  {
    SpatialValidatingVisitor vv(*this, *m);

    const SpatialModelPlugin* plugin = 
      static_cast <const SpatialModelPlugin *> (m->getPlugin("spatial"));
      
    if (plugin != NULL)
    {
      plugin->accept(vv);
    }
  }

  /* ADD ANY OTHER OBJECTS THAT HAVE PLUGINS */
  
  return (unsigned int)mFailures.size();
}


/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
SpatialValidator::validate (const std::string& filename)
{
  if (&filename == NULL) return 0;

  SBMLReader    reader;
  SBMLDocument* d = reader.readSBML(filename);


  for (unsigned int n = 0; n < d->getNumErrors(); ++n)
  {
    logFailure( *d->getError(n) );
  }

  unsigned int ret = validate(*d);
  delete d;
  return ret;
}


LIBSBML_CPP_NAMESPACE_END

// ----------------------------------------------------------------------

