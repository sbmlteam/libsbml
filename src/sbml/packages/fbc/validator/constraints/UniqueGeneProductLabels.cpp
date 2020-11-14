/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueGeneProductLabels.cpp
 * @brief   Base class for Id constraints
 * @author  Ben Bornstein
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include "UniqueGeneProductLabels.h"
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
 * Creates a new UniqueGeneProductLabels with the given constraint id.
 */
UniqueGeneProductLabels::UniqueGeneProductLabels (unsigned int id, Validator& v) : TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueGeneProductLabels::~UniqueGeneProductLabels ()
{
}


/*
 * Checks that all ids for some given subset of the Model adhere to this
 * Constraint.  Override the doCheck() method to define your own subset.
 */
void
UniqueGeneProductLabels::check_ (const Model& m, const Model& )
{
  doCheck(m);
}


/*
 * Logs a message that the given @p id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
UniqueGeneProductLabels::logConflict (const std::string& label, const SBase& object)
{
  std::string message = "A GeneProduct with the label '";
  message += label;
  message += "' has already been declared.";
  logFailure(object, message);
}

/*
 * Checks that all ids on the following Model objects are unique:
 * FunctionDefinitions, Species, Compartments, global Parameters,
 * Reactions, and Events.
 */
void
UniqueGeneProductLabels::doCheck (const Model& m)
{

  mLabels.clear();

  const FbcModelPlugin * modelPlug = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));

  for (unsigned int n = 0; n < modelPlug->getNumGeneProducts(); n++)
  {
    std::string label = modelPlug->getGeneProduct(n)->getLabel();
    if (label.empty())
    {
      continue;
    }
    else
    {
      if (mLabels.find(label) == mLabels.end())
      {
        mLabels.insert(label);
      }
      else
      {
        logConflict(label, *(modelPlug->getGeneProduct(n)));
      }
    }
  }

}


#endif /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END
/** @endcond */
