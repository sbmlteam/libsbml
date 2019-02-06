/**
 * @file L3v2extendedmathValidator.cpp
 * @brief Definition of L3v2extendedmathValidator.
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/l3v2extendedmath/common/L3v2extendedmathExtensionTypes.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathValidator.h>

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
struct Apply : public unary_function<TConstraint<T>*, void>
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
struct L3v2extendedmathValidatorConstraints
{

  ConstraintSet<SBMLDocument>                   mSBMLDocument;
  ConstraintSet<Model>                          mModel;

  map<VConstraint*, bool> ptrMap;

  ~L3v2extendedmathValidatorConstraints();

  void add(VConstraint* c);

};


/*
 * Destroys this L3v2extendedmathValidatorConstraints object.
 */
L3v2extendedmathValidatorConstraints::~L3v2extendedmathValidatorConstraints()
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
L3v2extendedmathValidatorConstraints::add(VConstraint* c)
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

}


// -------------------------------------------
// ValidatingVisitor
// -------------------------------------------

class L3v2extendedmathValidatingVisitor: public SBMLVisitor
{
public:

  L3v2extendedmathValidatingVisitor(L3v2extendedmathValidator& v,
                                    const Model& m)
    : v(v)
    , m(m)
  {
  }


  using SBMLVisitor::visit;

  // model
  virtual void visit(const Model &x)
  {
    v.mL3v2extendedmathConstraints->mModel.applyTo(m, x);
    v.mL3v2extendedmathConstraints->mModel.empty();
  }



  virtual bool
  visit(const SBase& x)
  {
    //if (x.getPackageName() != "l3v2extendedmath")
    //{
    //  return SBMLVisitor::visit(x);
    //}

    //int code = x.getTypeCode();

    //const ListOf* list = dynamic_cast<const ListOf*>(&x);

    return false;
  }


protected:

  L3v2extendedmathValidator& v;
  const Model& m;
};


// -------------------------------------------
// L3v2extendedmathValidator
// -------------------------------------------

/*
 * Creates a new L3v2extendedmathValidator object for the given category of
 * validation.
 */
L3v2extendedmathValidator::L3v2extendedmathValidator(SBMLErrorCategory_t
  category)
  : Validator(category)
{
  mL3v2extendedmathConstraints = new L3v2extendedmathValidatorConstraints();
}


/*
 * Destroys this L3v2extendedmathValidator object.
 */
L3v2extendedmathValidator::~L3v2extendedmathValidator()
{
  delete mL3v2extendedmathConstraints;
}


/*
 * Adds the given VConstraint object to this L3v2extendedmathValidator.
 */
void
L3v2extendedmathValidator::addConstraint(VConstraint* c)
{
  mL3v2extendedmathConstraints->add(c);
}


/*
 * Validates the given SBMLDocument
 */
unsigned int
L3v2extendedmathValidator::validate(const SBMLDocument& d)
{
  const Model* m = d.getModel();

  // get any objects with math and apply 
  if (m != NULL)
  {
    L3v2extendedmathValidatingVisitor vv(*this, *m);
    m->accept(vv);
    //const L3v2extendedmathSBMLDocumentPlugin* plugin = static_cast<const
    //  L3v2extendedmathSBMLDocumentPlugin*>(d.getPlugin("l3v2extendedmath"));
    //if (plugin != NULL)
    //{
    //  plugin->accept(vv);
    //}
  }

  // ADD ANY OTHER OBJECTS THAT HAS PLUGINS

  return (unsigned int)(mFailures.size());
}


/*
 * Validates the SBMLDocument located at the given filename
 */
unsigned int
L3v2extendedmathValidator::validate(const std::string& filename)
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


