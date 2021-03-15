/**
 * @file ArraysValidator.cpp
 * @brief Definition of ArraysValidator.
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>
#include <sbml/packages/arrays/validator/ArraysValidator.h>

/** @cond doxygenLibsbmlInternal */

using namespace std;

/** @endcond */



LIBSBML_CPP_NAMESPACE_BEGIN


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
struct ArraysValidatorConstraints
{

  ConstraintSet<SBMLDocument>                   mSBMLDocument;
  ConstraintSet<Model>                          mModel;
  ConstraintSet<Index>                          mIndex;
  ConstraintSet<Dimension>                      mDimension;

  map<VConstraint*, bool> ptrMap;

  ~ArraysValidatorConstraints();

  void add(VConstraint* c);

};


/*
 * Destroys this ArraysValidatorConstraints object.
 */
ArraysValidatorConstraints::~ArraysValidatorConstraints()
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
ArraysValidatorConstraints::add(VConstraint* c)
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

  if (dynamic_cast< TConstraint<Index>* >(c) != NULL)
  {
    mIndex.add(static_cast< TConstraint<Index>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Dimension>* >(c) != NULL)
  {
    mDimension.add(static_cast< TConstraint<Dimension>* >(c) );
    return;
  }
}


// -------------------------------------------
// ValidatingVisitor
// -------------------------------------------

class ArraysValidatingVisitor: public SBMLVisitor
{
public:

  ArraysValidatingVisitor(ArraysValidator& v, const Model& m)
    : v(v)
    , m(m)
  {
  }


  using SBMLVisitor::visit;

  bool
  visit(const Index& x)
  {
    v.mArraysConstraints->mIndex.applyTo(m, x);
    return !v.mArraysConstraints->mIndex.empty();
  }


  bool
  visit(const Dimension& x)
  {
    v.mArraysConstraints->mDimension.applyTo(m, x);
    return !v.mArraysConstraints->mDimension.empty();
  }


  virtual bool
  visit(const SBase& x)
  {
    if (x.getPackageName() != "arrays")
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
      if (code == SBML_ARRAYS_INDEX)
      {
        return visit((const Index&)x);
      }
      else if (code == SBML_ARRAYS_DIMENSION)
      {
        return visit((const Dimension&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
  }


protected:

  ArraysValidator& v;
  const Model& m;
};


// -------------------------------------------
// ArraysValidator
// -------------------------------------------

/*
 * Creates a new ArraysValidator object for the given category of validation.
 */
ArraysValidator::ArraysValidator(SBMLErrorCategory_t category)
  : Validator(category)
{
  mArraysConstraints = new ArraysValidatorConstraints();
}


/*
 * Destroys this ArraysValidator object.
 */
ArraysValidator::~ArraysValidator()
{
  delete mArraysConstraints;
}


/*
 * Adds the given VConstraint object to this ArraysValidator.
 */
void
ArraysValidator::addConstraint(VConstraint* c)
{
  mArraysConstraints->add(c);
}


/*
 * Validates the given SBMLDocument
 */
unsigned int
ArraysValidator::validate(const SBMLDocument& d)
{
  const Model* m = d.getModel();

  if (m != NULL)
  {
    ArraysValidatingVisitor vv(*this, *m);
  }

  // ADD ANY OTHER OBJECTS THAT HAS PLUGINS

  return (unsigned int)(mFailures.size());
}


/*
 * Validates the SBMLDocument located at the given filename
 */
unsigned int
ArraysValidator::validate(const std::string& filename)
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




LIBSBML_CPP_NAMESPACE_END


