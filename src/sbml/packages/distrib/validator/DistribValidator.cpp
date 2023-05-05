/**
 * @file DistribValidator.cpp
 * @brief Definition of DistribValidator.
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/distrib/common/DistribExtensionTypes.h>
#include <sbml/packages/distrib/validator/DistribValidator.h>

/** @cond doxygenLibsbmlInternal */

using namespace std;

/** @endcond */



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


// -------------------------------------------
// Apply<T>
// -------------------------------------------
/**
 * Applies a Constraint<T> to an SBML object of type T.
 */

template <typename T>
struct Apply
{
  Apply(const Model& m, const T& o)
    : model(m)
    , object(o)
  {
  }


  void
  operator()(TConstraint<T>* constraint)
  {
    constraint->check(model, object);
  }


  const Model& model;
  const T& object;
};


// -------------------------------------------
// ConstraintSet<T>
// -------------------------------------------
template <typename T>
class ConstraintSet
{
public:

  ConstraintSet() { }
  ~ConstraintSet() { }


  /*
   * Adds a Constraint to this ConstraintSet
   */
  void
  add(TConstraint<T>* c)
  {
    constraints.push_back(c);
  }


  /*
   * Applies all Constraints in this ConstraintSet to the given SBML object of
   * type T. Constraint violations are logged to Validator.
   */
  void
  applyTo(const Model& model, const T& object)
  {
    for_each(constraints.begin(), constraints.end(), Apply<T>(model, object));
  }


  /*
   * Returns true if the ConstraintSet is empty, false otherwise
   */
  bool
  empty() const
  {
    return constraints.empty();
  }


protected:

  std::list< TConstraint<T>* > constraints;
};


// -------------------------------------------
// ValidatorConstraints
// -------------------------------------------
struct DistribValidatorConstraints
{

  ConstraintSet<SBMLDocument>                   mSBMLDocument;
  ConstraintSet<Model>                          mModel;
  ConstraintSet<UncertParameter>                mUncertParameter;
  ConstraintSet<Uncertainty>                    mUncertainty;
  ConstraintSet<UncertSpan>                     mUncertSpan;
  ConstraintSet<DistribBase>                    mDistribBase;

  map<VConstraint*, bool> ptrMap;

  ~DistribValidatorConstraints();

  void add(VConstraint* c);

};


/*
 * Destroys this DistribValidatorConstraints object.
 */
DistribValidatorConstraints::~DistribValidatorConstraints()
{
  map<VConstraint*, bool>::iterator it = ptrMap.begin();

  while (it != ptrMap.end())
  {
    if (it->second)
    {
      delete it->first;
    }

    ++it;
  }
}


/*
 * Adds the given Constraint to the appropriate ConstraintSet
 */
void
DistribValidatorConstraints::add(VConstraint* c)
{
  if (c == NULL) return;

  ptrMap.insert(pair<VConstraint*, bool>(c, true));

  if (dynamic_cast< TConstraint<SBMLDocument>* >(c) != NULL)
  {
    mSBMLDocument.add(static_cast< TConstraint<SBMLDocument>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Model>* >(c) != NULL)
  {
    mModel.add(static_cast< TConstraint<Model>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<UncertParameter>* >(c) != NULL)
  {
    mUncertParameter.add(static_cast< TConstraint<UncertParameter>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Uncertainty>* >(c) != NULL)
  {
    mUncertainty.add(static_cast< TConstraint<Uncertainty>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<UncertSpan>* >(c) != NULL)
  {
    mUncertSpan.add(static_cast< TConstraint<UncertSpan>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribBase>* >(c) != NULL)
  {
    mDistribBase.add(static_cast< TConstraint<DistribBase>* >(c) );
    return;
  }
}


// -------------------------------------------
// ValidatingVisitor
// -------------------------------------------

class DistribValidatingVisitor: public SBMLVisitor
{
public:

  DistribValidatingVisitor(DistribValidator& v, const Model& m)
    : v(v)
    , m(m)
  {
  }


  using SBMLVisitor::visit;

  bool
  visit(const UncertParameter& x)
  {
    v.mDistribConstraints->mUncertParameter.applyTo(m, x);
    return !v.mDistribConstraints->mUncertParameter.empty();
  }


  bool
  visit(const Uncertainty& x)
  {
    v.mDistribConstraints->mUncertainty.applyTo(m, x);
    return !v.mDistribConstraints->mUncertainty.empty();
  }


  bool
  visit(const UncertSpan& x)
  {
    v.mDistribConstraints->mUncertSpan.applyTo(m, x);
    return !v.mDistribConstraints->mUncertSpan.empty();
  }


  bool
  visit(const DistribBase& x)
  {
    v.mDistribConstraints->mDistribBase.applyTo(m, x);
    return !v.mDistribConstraints->mDistribBase.empty();
  }


  virtual bool
  visit(const SBase& x)
  {
    if (x.getPackageName() != "distrib")
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
      if (code == SBML_DISTRIB_UNCERTPARAMETER)
      {
        return visit((const UncertParameter&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTAINTY)
      {
        return visit((const Uncertainty&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTSTATISTICSPAN)
      {
        return visit((const UncertSpan&)x);
      }
      else if (code == SBML_DISTRIB_DISTRIBBASE)
      {
        return visit((const DistribBase&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
  }


protected:

  DistribValidator& v;
  const Model& m;
};


// -------------------------------------------
// DistribValidator
// -------------------------------------------

/*
 * Creates a new DistribValidator object for the given category of validation.
 */
DistribValidator::DistribValidator(SBMLErrorCategory_t category)
  : Validator(category)
{
  mDistribConstraints = new DistribValidatorConstraints();
}


/*
 * Destroys this DistribValidator object.
 */
DistribValidator::~DistribValidator()
{
  delete mDistribConstraints;
}


/*
 * Adds the given VConstraint object to this DistribValidator.
 */
void
DistribValidator::addConstraint(VConstraint* c)
{
  mDistribConstraints->add(c);
}


/*
 * Validates the given SBMLDocument
 */
unsigned int
DistribValidator::validate(const SBMLDocument& d)
{
  const Model* m = d.getModel();

  if (m != NULL)
  {
    DistribValidatingVisitor vv(*this, *m);
    const DistribSBMLDocumentPlugin* plugin = static_cast<const
      DistribSBMLDocumentPlugin*>(d.getPlugin("distrib"));
    if (plugin != NULL)
    {
      plugin->accept(vv);
    }
  }

  // ADD ANY OTHER OBJECTS THAT HAS PLUGINS

  return (unsigned int)(mFailures.size());
}


/*
 * Validates the SBMLDocument located at the given filename
 */
unsigned int
DistribValidator::validate(const std::string& filename)
{
  SBMLReader reader;
  SBMLDocument* d = reader.readSBML(filename);


  unsigned int numErrors = d->getNumErrors();


  for (unsigned int n=0; n < numErrors; ++n)
  {
    logFailure(*d->getError(n));
  }

  numErrors = validate(*d);
  delete d;
  return numErrors;
}




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


