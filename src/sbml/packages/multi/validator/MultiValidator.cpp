/**
 * @file:   MultiValidator.cpp
 * @brief:  Implementation of the MultiValidator class
 * @author: SBMLTeam
 * @author: Fengkai Zhang
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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

#include <sbml/packages/multi/common/MultiExtensionTypes.h>
#include <sbml/packages/multi/validator/MultiValidator.h>

#include <sbml/SBMLReader.h>
#include <sbml/ModifierSpeciesReference.h>


/** @cond doxygenLibsbmlInternal */

using namespace std;

/** @endcond */


#ifdef __cplusplus

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
struct Apply
{
  Apply(const Model& m, const T& o) : model(m), object(o) { }


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

  ConstraintSet() { }
  ~ConstraintSet() { }


  /*
   * Adds a Constraint to this ConstraintSet.
   */
  void add(TConstraint<T>* c)
  {
    constraints.push_back(c);
  }

  /*
   * Applies all Constraints in this ConstraintSet to the given SBML object
   * of type T.  Constraint violations are logged to Validator.
   */
  void applyTo(const Model& model, const T& object)
  {
    for_each(constraints.begin(), constraints.end(), Apply<T>(model, object));
  }

  /*
   * @return @c true if this ConstraintSet is empty, false otherwise.
   */
  bool empty() const
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
struct MultiValidatorConstraints
{
  ConstraintSet<SBMLDocument>          mSBMLDocument;
  ConstraintSet<Model>                 mModel;
  ConstraintSet<PossibleSpeciesFeatureValue>      mPossibleSpeciesFeatureValue;
  ConstraintSet<SpeciesFeatureValue>   mSpeciesFeatureValue;
  ConstraintSet<CompartmentReference>  mCompartmentReference;
  ConstraintSet<SpeciesTypeInstance>   mSpeciesTypeInstance;
  ConstraintSet<InSpeciesTypeBond>     mInSpeciesTypeBond;
  ConstraintSet<OutwardBindingSite>    mOutwardBindingSite;
  ConstraintSet<SpeciesFeatureType>    mSpeciesFeatureType;
  ConstraintSet<SpeciesTypeComponentIndex>      mSpeciesTypeComponentIndex;
  ConstraintSet<SpeciesFeature>        mSpeciesFeature;
  ConstraintSet<SpeciesTypeComponentMapInProduct>      mSpeciesTypeComponentMapInProduct;
  ConstraintSet<MultiSpeciesType>      mMultiSpeciesType;
  ConstraintSet<Compartment>           mCompartment;
  ConstraintSet<Species>           	   mSpecies;
  ConstraintSet<SubListOfSpeciesFeatures> mSubListOfSpeciesFeatures;
  ConstraintSet<SpeciesReference>      mSpeciesReference;
  map<VConstraint*, bool> ptrMap;

  ~MultiValidatorConstraints();
  void add(VConstraint* c);
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
MultiValidatorConstraints::~MultiValidatorConstraints()
{
  map<VConstraint*, bool>::iterator it = ptrMap.begin();

  while (it != ptrMap.end())
  {
    if (it->second) delete it->first;
    ++it;
  }
}


/*
 * Adds the given Contraint to the appropriate ConstraintSet.
 */
void
MultiValidatorConstraints::add(VConstraint* c)
{
  if (c == NULL) return;

  ptrMap.insert(pair<VConstraint*, bool>(c, true));

  if (dynamic_cast<TConstraint<SBMLDocument>*>(c) != NULL)
  {
    mSBMLDocument.add(static_cast<TConstraint<SBMLDocument>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<Model>*>(c) != NULL)
  {
    mModel.add(static_cast<TConstraint<Model>*>(c));
    return;
  }
  if (dynamic_cast<TConstraint<PossibleSpeciesFeatureValue>*>(c) != NULL)
  {
    mPossibleSpeciesFeatureValue.add(static_cast<TConstraint<PossibleSpeciesFeatureValue>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesFeatureValue>*>(c) != NULL)
  {
    mSpeciesFeatureValue.add(static_cast<TConstraint<SpeciesFeatureValue>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<CompartmentReference>*>(c) != NULL)
  {
    mCompartmentReference.add(static_cast<TConstraint<CompartmentReference>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesTypeInstance>*>(c) != NULL)
  {
    mSpeciesTypeInstance.add(static_cast<TConstraint<SpeciesTypeInstance>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<InSpeciesTypeBond>*>(c) != NULL)
  {
    mInSpeciesTypeBond.add(static_cast<TConstraint<InSpeciesTypeBond>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<OutwardBindingSite>*>(c) != NULL)
  {
    mOutwardBindingSite.add(static_cast<TConstraint<OutwardBindingSite>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesFeatureType>*>(c) != NULL)
  {
    mSpeciesFeatureType.add(static_cast<TConstraint<SpeciesFeatureType>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesTypeComponentIndex>*>(c) != NULL)
  {
    mSpeciesTypeComponentIndex.add(static_cast<TConstraint<SpeciesTypeComponentIndex>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesFeature>*>(c) != NULL)
  {
    mSpeciesFeature.add(static_cast<TConstraint<SpeciesFeature>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesTypeComponentMapInProduct>*>(c) != NULL)
  {
    mSpeciesTypeComponentMapInProduct.add(static_cast<TConstraint<SpeciesTypeComponentMapInProduct>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<MultiSpeciesType>*>(c) != NULL)
  {
    mMultiSpeciesType.add(static_cast<TConstraint<MultiSpeciesType>*>(c));
    return;
  }
  if (dynamic_cast<TConstraint<Compartment>*>(c) != NULL)
  {
    mCompartment.add(static_cast<TConstraint<Compartment>*>(c));
    return;
  }
  if (dynamic_cast<TConstraint<Species>*>(c) != NULL)
  {
    mSpecies.add(static_cast<TConstraint<Species>*>(c));
    return;
  }
  if (dynamic_cast<TConstraint<SubListOfSpeciesFeatures>*>(c) != NULL)
  {
    mSubListOfSpeciesFeatures.add(static_cast<TConstraint<SubListOfSpeciesFeatures>*>(c));
    return;
  }

  if (dynamic_cast<TConstraint<SpeciesReference>*>(c) != NULL)
  {
    mSpeciesReference.add(static_cast<TConstraint<SpeciesReference>*>(c));
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
class MultiValidatingVisitor : public SBMLVisitor
{
public:

  MultiValidatingVisitor(MultiValidator& v, const Model& m) : v(v), m(m) { }


  using SBMLVisitor::visit;

  // model
  virtual void visit(const Model &x)
  {
    v.mMultiConstraints->mModel.applyTo(m, x);
    v.mMultiConstraints->mModel.empty();
  }

  bool visit(const PossibleSpeciesFeatureValue &x)
  {
    v.mMultiConstraints->mPossibleSpeciesFeatureValue.applyTo(m, x);
    return !v.mMultiConstraints->mPossibleSpeciesFeatureValue.empty();
  }

  bool visit(const SpeciesFeatureValue &x)
  {
    v.mMultiConstraints->mSpeciesFeatureValue.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesFeatureValue.empty();
  }

  bool visit(const CompartmentReference &x)
  {
    v.mMultiConstraints->mCompartmentReference.applyTo(m, x);
    return !v.mMultiConstraints->mCompartmentReference.empty();
  }

  bool visit(const SpeciesTypeInstance &x)
  {
    v.mMultiConstraints->mSpeciesTypeInstance.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesTypeInstance.empty();
  }

  bool visit(const InSpeciesTypeBond &x)
  {
    v.mMultiConstraints->mInSpeciesTypeBond.applyTo(m, x);
    return !v.mMultiConstraints->mInSpeciesTypeBond.empty();
  }

  bool visit(const OutwardBindingSite &x)
  {
    v.mMultiConstraints->mOutwardBindingSite.applyTo(m, x);
    return !v.mMultiConstraints->mOutwardBindingSite.empty();
  }

  bool visit(const SpeciesFeatureType &x)
  {
    v.mMultiConstraints->mSpeciesFeatureType.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesFeatureType.empty();
  }

  bool visit(const SpeciesTypeComponentIndex &x)
  {
    v.mMultiConstraints->mSpeciesTypeComponentIndex.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesTypeComponentIndex.empty();
  }

  bool visit(const SpeciesFeature &x)
  {
    v.mMultiConstraints->mSpeciesFeature.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesFeature.empty();
  }

  bool visit(const SpeciesTypeComponentMapInProduct &x)
  {
    v.mMultiConstraints->mSpeciesTypeComponentMapInProduct.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesTypeComponentMapInProduct.empty();
  }

  bool visit(const MultiSpeciesType &x)
  {
    v.mMultiConstraints->mMultiSpeciesType.applyTo(m, x);
    return !v.mMultiConstraints->mMultiSpeciesType.empty();
  }

  bool visit(const Compartment &x)
  {
    v.mMultiConstraints->mCompartment.applyTo(m, x);
    return !v.mMultiConstraints->mCompartment.empty();
  }
  bool visit(const Species &x)
  {
    v.mMultiConstraints->mSpecies.applyTo(m, x);
    return !v.mMultiConstraints->mSpecies.empty();
  }
  bool visit(const SubListOfSpeciesFeatures &x)
  {
    v.mMultiConstraints->mSubListOfSpeciesFeatures.applyTo(m, x);
    return !v.mMultiConstraints->mSubListOfSpeciesFeatures.empty();
  }

  bool visit(const SpeciesReference &x)
  {
    v.mMultiConstraints->mSpeciesReference.applyTo(m, x);
    return !v.mMultiConstraints->mSpeciesReference.empty();
  }
  //  bool visit (const Reaction &x)
  //  {
  //    v.mMultiConstraints->mReactions.applyTo(m, x);
  //    return !v.mMultiConstraints->mReactions.empty();
  //  }
  //  bool visit (const ASTNode &x)
  //  {
  //    v.mMultiConstraints->mASTNodes.applyTo(m, x);
  //    return !v.mMultiConstraints->mASTNodes.empty();
  //  }
  virtual bool visit(const SBase &x)
  {
    if (x.getPackageName() != "multi")
    {
      return SBMLVisitor::visit(x);
    }

    int code = x.getTypeCode();

    const ListOf* list = dynamic_cast<const ListOf*>(&x);

    if (list != NULL && code != SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES)
    {
      return SBMLVisitor::visit(x);
    }
    else
    {
      if (code == SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE)
      {
        return visit((const PossibleSpeciesFeatureValue&)x);
      }
      else if (code == SBML_MULTI_SPECIES_FEATURE_VALUE)
      {
        return visit((const SpeciesFeatureValue&)x);
      }
      else if (code == SBML_MULTI_COMPARTMENT_REFERENCE)
      {
        return visit((const CompartmentReference&)x);
      }
      else if (code == SBML_MULTI_SPECIES_TYPE_INSTANCE)
      {
        return visit((const SpeciesTypeInstance&)x);
      }
      else if (code == SBML_MULTI_IN_SPECIES_TYPE_BOND)
      {
        return visit((const InSpeciesTypeBond&)x);
      }
      else if (code == SBML_MULTI_OUTWARD_BINDING_SITE)
      {
        return visit((const OutwardBindingSite&)x);
      }
      else if (code == SBML_MULTI_SPECIES_FEATURE_TYPE)
      {
        return visit((const SpeciesFeatureType&)x);
      }
      else if (code == SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX)
      {
        return visit((const SpeciesTypeComponentIndex&)x);
      }
      else if (code == SBML_MULTI_SPECIES_FEATURE)
      {
        return visit((const SpeciesFeature&)x);
      }
      else if (code == SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT)
      {
        return visit((const SpeciesTypeComponentMapInProduct&)x);
      }
      else if (code == SBML_MULTI_SPECIES_TYPE || code == SBML_MULTI_BINDING_SITE_SPECIES_TYPE)
      {
        return visit((const MultiSpeciesType&)x);
      }
      else if (code == SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES)
      {
        return visit((const SubListOfSpeciesFeatures&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
  }

protected:

  MultiValidator&   v;
  const Model& m;
};


// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// Validator
// ----------------------------------------------------------------------


MultiValidator::MultiValidator(const SBMLErrorCategory_t category) :
Validator(category)
{
  mMultiConstraints = new MultiValidatorConstraints();
}


MultiValidator::~MultiValidator()
{
  delete mMultiConstraints;
}


/*
 * Adds the given Contraint to this validator.
 */
void
MultiValidator::addConstraint(VConstraint* c)
{
  mMultiConstraints->add(c);
}

#define ACCEPT_MULTI(x,vv)\
{\
   const SBasePlugin *plugin = static_cast <const SBasePlugin *>((x)->getPlugin(MultiExtension::getPackageName()));\
   if (plugin != NULL) plugin->accept(vv);\
}



/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
MultiValidator::validate(const SBMLDocument& d)
{

  const Model* m = d.getModel();

  // model
  if (m != NULL)
  {
    MultiValidatingVisitor vv(*this, *m);

    // Model
    ACCEPT_MULTI(m, vv);

    // Compartment
    for (unsigned int i = 0; i < m->getNumCompartments(); i++)
    {
      ACCEPT_MULTI(m->getCompartment(i), vv);
    }

    //  Species
    for (unsigned int i = 0; i < m->getNumSpecies(); i++)
    {
      ACCEPT_MULTI(m->getSpecies(i), vv);
    }

    // ListOfReactions
    ACCEPT_MULTI(m->getListOfReactions(), vv);

    // Reaction
    for (unsigned int i = 0; i < m->getNumReactions(); i++)
    {
      const Reaction * r = m->getReaction(i);

      for (unsigned int j = 0; j < r->getNumReactants(); j++)
      {
        ACCEPT_MULTI(r->getReactant(j), vv);
      }
      for (unsigned int j = 0; j < r->getNumProducts(); j++)
      {
        ACCEPT_MULTI(r->getProduct(j), vv);
      }
      for (unsigned int j = 0; j < r->getNumModifiers(); j++)
      {
        ACCEPT_MULTI(r->getModifier(j), vv);
      }

      const KineticLaw * kineticLaw = r->getKineticLaw();
      if (kineticLaw) {
        const ASTNode * math = kineticLaw->getMath();
        // TODO for validation of ci elements
        List * listNumberNodes = math->getListOfNodes((ASTNodePredicate)ASTNode_isNumber);

        for (unsigned int ii = 0; ii < listNumberNodes->getSize(); ii++)
        {
          const ASTNode * node = static_cast<const ASTNode *> (listNumberNodes->get(ii));
          if (node != NULL) {
            const MultiASTPlugin * astPlugin = static_cast<const MultiASTPlugin*>(node->getPlugin("multi"));
            if (astPlugin != NULL) {
              //						astPlugin-
            }
          }
        }

        delete listNumberNodes;

      }
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
MultiValidator::validate(const std::string& filename)
{

  SBMLReader    reader;
  SBMLDocument *d = reader.readSBML(filename);


  for (unsigned int n = 0; n < d->getNumErrors(); ++n)
  {
    logFailure(*d->getError(n));
  }

  int result = validate(*d);

  delete d;

  return result;
}


LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */

// ----------------------------------------------------------------------

