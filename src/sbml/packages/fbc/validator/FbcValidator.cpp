/**
 * @file    FbcValidator.cpp
 * @brief   Base class for SBML Validators
 * @author  Sarah Keating
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
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <algorithm>
#include <functional>
#include <typeinfo>

#include <sbml/SBMLVisitor.h>

#include <sbml/validator/VConstraint.h>
#include <sbml/validator/Validator.h>
#include <sbml/packages/fbc/validator/FbcValidator.h>
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>
#include <sbml/SBMLReader.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

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
   * @return @c true if this ConstraintSet is empty, @c false otherwise.
   */
  bool empty () const
  {
    return constraints.empty();
  }


protected:

  /** @cond doxygenLibsbmlInternal */
  std::list< TConstraint<T>* > constraints;
  /** @endcond */
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
struct FbcValidatorConstraints
{
  ConstraintSet<SBMLDocument>             mSBMLDocument;
  ConstraintSet<Model>                    mModel;
  ConstraintSet<FluxBound>                mFluxBound;
  ConstraintSet<FluxObjective>            mFluxObjective;
  ConstraintSet<Objective>                mObjective;
  ConstraintSet<Species>                  mSpecies;
  ConstraintSet<ListOfObjectives>         mListOfObjectives;
  ConstraintSet<Reaction>                 mReaction;
  ConstraintSet<SpeciesReference>         mSpeciesReference;
  ConstraintSet<GeneProductRef>           mGeneProductRef;
  ConstraintSet<GeneProductAssociation>   mGeneProductAssociation;
  ConstraintSet<GeneProduct>              mGeneProduct;
  ConstraintSet<FbcAnd>                   mFbcAnd;
  ConstraintSet<FbcOr>                    mFbcOr;
  ConstraintSet<UserDefinedConstraint>                    mUserDefinedConstraint;
  ConstraintSet<UserDefinedConstraintComponent>                    mUserDefinedConstraintComponent;
  ConstraintSet<KeyValuePair>                    mKeyValuePair;

  map<VConstraint*,bool> ptrMap;

  ~FbcValidatorConstraints ();
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
FbcValidatorConstraints::~FbcValidatorConstraints ()
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
FbcValidatorConstraints::add (VConstraint* c)
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

  if (dynamic_cast< TConstraint<FluxBound>* >(c) != NULL)
  {
    mFluxBound.add( static_cast< TConstraint<FluxBound>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<FluxObjective>* >(c) != NULL)
  {
    mFluxObjective.add( static_cast< TConstraint<FluxObjective>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Objective>* >(c) != NULL)
  {
    mObjective.add( static_cast< TConstraint<Objective>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Species>* >(c) != NULL)
  {
    mSpecies.add( static_cast< TConstraint<Species>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<ListOfObjectives>* >(c) != NULL)
  {
    mListOfObjectives.add( static_cast< TConstraint<ListOfObjectives>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Reaction>* >(c) != NULL)
  {
    mReaction.add( static_cast< TConstraint<Reaction>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<SpeciesReference>* >(c) != NULL)
  {
    mSpeciesReference.add( 
      static_cast< TConstraint<SpeciesReference>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GeneProductRef>* >(c) != NULL)
  {
    mGeneProductRef.add( 
      static_cast< TConstraint<GeneProductRef>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GeneProductAssociation>* >(c) != NULL)
  {
    mGeneProductAssociation.add(
      static_cast< TConstraint<GeneProductAssociation>* >(c));
    return;
  }

  if (dynamic_cast< TConstraint<GeneProduct>* >(c) != NULL)
  {
    mGeneProduct.add( 
      static_cast< TConstraint<GeneProduct>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<FbcAnd>* >(c) != NULL)
  {
    mFbcAnd.add( 
      static_cast< TConstraint<FbcAnd>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<FbcOr>* >(c) != NULL)
  {
    mFbcOr.add( 
      static_cast< TConstraint<FbcOr>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<UserDefinedConstraint>* >(c) != NULL)
  {
    mUserDefinedConstraint.add(
      static_cast< TConstraint<UserDefinedConstraint>* >(c));
    return;
  }

  if (dynamic_cast< TConstraint<UserDefinedConstraintComponent>* >(c) != NULL)
  {
    mUserDefinedConstraintComponent.add(
      static_cast< TConstraint<UserDefinedConstraintComponent>* >(c));
    return;
  }

  if (dynamic_cast< TConstraint<KeyValuePair>* >(c) != NULL)
  {
    mKeyValuePair.add(
      static_cast< TConstraint<KeyValuePair>* >(c));
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
class FbcValidatingVisitor: public SBMLVisitor
{
public:

  using SBMLVisitor::visit;
  
  FbcValidatingVisitor (FbcValidator& validator, const Model& model) : v(validator), m(model) { }

  bool visit (const FluxBound &x)
  {
    v.mFbcConstraints->mFluxBound.applyTo(m, x);
    return !v.mFbcConstraints->mFluxBound.empty();
  }

  bool visit (const FluxObjective &x)
  {
    v.mFbcConstraints->mFluxObjective.applyTo(m, x);
    return !v.mFbcConstraints->mFluxObjective.empty();
  }

  bool visit (const Objective &x)
  {
    v.mFbcConstraints->mObjective.applyTo(m, x);
    return !v.mFbcConstraints->mObjective.empty();
  }

  virtual bool visit (const Species &x)
  {
    v.mFbcConstraints->mSpecies.applyTo(m, x);
    return !v.mFbcConstraints->mSpecies.empty();
  }

  bool visit (const ListOfObjectives &x)
  {
    v.mFbcConstraints->mListOfObjectives.applyTo(m, x);
    return !v.mFbcConstraints->mListOfObjectives.empty();
  }

  virtual void visit (const Model &x)
  {
    v.mFbcConstraints->mModel.applyTo(m, x);
  }

  virtual bool visit (const Reaction &x)
  {
    v.mFbcConstraints->mReaction.applyTo(m, x);
    return !v.mFbcConstraints->mReaction.empty();
  }

  virtual bool visit (const SpeciesReference &x)
  {
    v.mFbcConstraints->mSpeciesReference.applyTo(m, x);
    return !v.mFbcConstraints->mSpeciesReference.empty();
  }

  virtual bool visit (const GeneProductRef &x)
  {
    v.mFbcConstraints->mGeneProductRef.applyTo(m, x);
    return !v.mFbcConstraints->mGeneProductRef.empty();
  }

  virtual bool visit(const GeneProductAssociation &x)
  {
    v.mFbcConstraints->mGeneProductAssociation.applyTo(m, x);
    return !v.mFbcConstraints->mGeneProductAssociation.empty();
  }

  virtual bool visit (const GeneProduct &x)
  {
    v.mFbcConstraints->mGeneProduct.applyTo(m, x);
    return !v.mFbcConstraints->mGeneProduct.empty();
  }

  virtual bool visit (const FbcAnd &x)
  {
    v.mFbcConstraints->mFbcAnd.applyTo(m, x);
    return !v.mFbcConstraints->mFbcAnd.empty();
  }

  virtual bool visit (const FbcOr &x)
  {
    v.mFbcConstraints->mFbcOr.applyTo(m, x);
    return !v.mFbcConstraints->mFbcOr.empty();
  }

  virtual bool visit(const UserDefinedConstraint &x)
  {
    v.mFbcConstraints->mUserDefinedConstraint.applyTo(m, x);
    return !v.mFbcConstraints->mUserDefinedConstraint.empty();
  }

  virtual bool visit(const UserDefinedConstraintComponent &x)
  {
    v.mFbcConstraints->mUserDefinedConstraintComponent.applyTo(m, x);
    return !v.mFbcConstraints->mUserDefinedConstraintComponent.empty();
  }

  virtual bool visit(const KeyValuePair &x)
  {
    v.mFbcConstraints->mKeyValuePair.applyTo(m, x);
    return !v.mFbcConstraints->mKeyValuePair.empty();
  }

  virtual bool visit (const SBase &x)
  {
    if (x.getPackageName() != "fbc")
    {
      return SBMLVisitor::visit(x);      
    }

    int code = x.getTypeCode(); 

    // check for list of since we need to check ListOfObjectives
    const ListOf* list = dynamic_cast<const ListOf*>(&x);

    if (list != NULL) 
    {
      code = list->getItemTypeCode();

      if (code == SBML_FBC_OBJECTIVE)
      {
        return visit((const ListOfObjectives&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
    else
    {
      if (code == SBML_FBC_OBJECTIVE)
      {
        return visit((const Objective&)x);
      } 
      else if (code == SBML_FBC_FLUXBOUND)
      {
        return visit((const FluxBound&)x);
      } 
      else if (code == SBML_FBC_FLUXOBJECTIVE)
      {
        return visit((const FluxObjective&)x);
      } 
      else if (code == SBML_FBC_GENEPRODUCTREF)
      {
        return visit((const GeneProductRef&)x);
      } 
      else if (code == SBML_FBC_GENEPRODUCTASSOCIATION)
      {
        return visit((const GeneProductAssociation&)x);
      }
      else if (code == SBML_FBC_GENEPRODUCT)
      {
        return visit((const GeneProduct&)x);
      } 
      else if (code == SBML_FBC_AND)
      {
        return visit((const FbcAnd&)x);
      } 
      else if (code == SBML_FBC_OR)
      {
        return visit((const FbcOr&)x);
      } 
      else if (code == SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT)
      {
        return visit((const UserDefinedConstraintComponent&)x);
      }
      else if (code == SBML_FBC_USERDEFINEDCONSTRAINT)
      {
        return visit((const UserDefinedConstraint&)x);
      }
      else if (code == SBML_FBC_KEYVALUEPAIR)
      {
        return visit((const KeyValuePair&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      } 
    }
  }

protected:

  /** @cond doxygenLibsbmlInternal */
  FbcValidator&   v;
  const Model& m;
  /** @endcond */
};


// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// Validator
// ----------------------------------------------------------------------


FbcValidator::FbcValidator (const SBMLErrorCategory_t category):
  Validator(category)
{
  mFbcConstraints = new FbcValidatorConstraints();
}


FbcValidator::~FbcValidator ()
{
  delete mFbcConstraints;
}


/*
 * Adds the given Contraint to this validator.
 */
void
FbcValidator::addConstraint (VConstraint* c)
{
  mFbcConstraints->add(c);
}


/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
FbcValidator::validate (const SBMLDocument& d)
{
  const Model* m = d.getModel();

  if (m != NULL)
  {
    FbcValidatingVisitor vv(*this, *m);

    const FbcModelPlugin* plugin = 
      static_cast <const FbcModelPlugin *> (m->getPlugin("fbc"));
      
    if (plugin != NULL)
    {
      plugin->accept(vv);
    }

    for (unsigned int i = 0; i < m->getNumReactions(); i++)
    {
      const FbcReactionPlugin* plugin1 = static_cast<const FbcReactionPlugin*>
        (m->getReaction(i)->getPlugin("fbc"));

      if (plugin1 != NULL)
      {
        plugin1->accept(vv);
      }

    }
  }

  return (unsigned int)mFailures.size();
}


/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
FbcValidator::validate (const std::string& filename)
{
  SBMLReader    reader;
  SBMLDocument* d = reader.readSBML(filename);


  for (unsigned int n = 0; n < d->getNumErrors(); ++n)
  {
    logFailure( *(d->getError(n)) );
  }

  unsigned int ret = validate(*d);
  delete d;
  return ret;
}


#endif /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END

// ----------------------------------------------------------------------

