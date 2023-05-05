/**
 * @file:   ReqValidator.cpp
 * @brief:  Implementation of the ReqValidator class
 * @author: SBMLTeam
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

#include <sbml/packages/req/common/ReqExtensionTypes.h>
#include <sbml/packages/req/validator/ReqValidator.h>

#include <sbml/SBMLReader.h>

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
struct ReqValidatorConstraints
{
  ConstraintSet<SBMLDocument>             mSBMLDocument;
  ConstraintSet<Model>                    mModel;
  ConstraintSet<ChangedMath>      mChangedMath;
  map<VConstraint*,bool> ptrMap;

  ~ReqValidatorConstraints ();
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
ReqValidatorConstraints::~ReqValidatorConstraints ()
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
ReqValidatorConstraints::add (VConstraint* c)
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
  if (dynamic_cast< TConstraint<ChangedMath>* >(c) != NULL)
  {
    mChangedMath.add( static_cast< TConstraint<ChangedMath>* >(c) );
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
class ReqValidatingVisitor: public SBMLVisitor
{
public:

  ReqValidatingVisitor (ReqValidator& v, const Model& m) : v(v), m(m) { }

  using SBMLVisitor::visit;

  bool visit (const ChangedMath &x)
  {
    v.mReqConstraints->mChangedMath.applyTo(m, x);
    return !v.mReqConstraints->mChangedMath.empty();
  }

  virtual bool visit(const SBase &x)
  {
    if (x.getPackageName() != "req")
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
      if (code == SBML_REQ_CHANGED_MATH)
      {
        return visit((const ChangedMath&)x);
      }
      else 
      {
        return SBMLVisitor::visit(x);
      } 
    }
  }

protected:

  ReqValidator&   v;
  const Model& m;
};


// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// Validator
// ----------------------------------------------------------------------


ReqValidator::ReqValidator (const SBMLErrorCategory_t category):
  Validator(category)
{
  mReqConstraints = new ReqValidatorConstraints();
}


ReqValidator::~ReqValidator ()
{
  delete mReqConstraints;
}


/*
 * Adds the given Contraint to this validator.
 */
void
ReqValidator::addConstraint (VConstraint* c)
{
  mReqConstraints->add(c);
}


/*
 * Validates the given SBMLDocument.  Failures logged during
 * validation may be retrieved via <code>getFailures()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
ReqValidator::validate (const SBMLDocument& d)
{

  const Model* m = d.getModel();

  if (m != NULL)
  {
    ReqValidatingVisitor vv(*this, *m);

    //const ReqModelPlugin* plugin = 
    //  static_cast <const ReqModelPlugin *> (m->getPlugin("req"));
    //  
    //if (plugin != NULL)
    //{
    //  plugin->accept(vv);
    //}
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
ReqValidator::validate (const std::string& filename)
{

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

