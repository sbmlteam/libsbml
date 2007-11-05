/**
 * @file    SBMLConvert.cpp
 * @brief   Performs conversion between SBML levels
 * @author  Ben Bornstein and Sarah Keating
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/util/List.h>
#include <sbml/math/ASTNode.h>

#include <sbml/Model.h>
#include <sbml/KineticLaw.h>
#include <sbml/Compartment.h>
#include <sbml/SpeciesReference.h>


static const char* ASSIGNED_COMPARTMENT = "AssignedName";


/**
 * Converts the model to a from SBML L2 to L1.  Most of the necessary
 * changes occur during the various writeAttributes() methods, however
 * there are some difference between L1 and L2 that require the underlying
 * Model to be changed.
 */
void 
Model::convertToL1 ()
{
  //
  // Level 2 allows a model to be specified without a Compartment.  However
  // this is not valid in Level 1.  Thus if a L2 model has no Compartment
  // one must be included and any species are assumed to be within it.
  //
  if (getNumCompartments() == 0)
  {
    createCompartment()->setId(ASSIGNED_COMPARTMENT);

    for (unsigned int n = 0; n < getNumSpecies(); ++n) 
    {
      getSpecies(n)->setCompartment(ASSIGNED_COMPARTMENT);
    }
  }
}


/**
 * Converts the model to a from SBML L1 to L2.  Most of the necessary
 * changes occur during the various writeAttributes() methods, however
 * there are some difference between L1 and L2 that require the underlying
 * Model to be changed.
 */
void 
Model::convertToL2 ()
{
  //
  // Level 2 has a listOfModifiers associated with a Reaction
  // which are not listed in a L1 Model.
  // For each symbol in the Reaction's KineticLaw,
  // that symbol is a modifier iff:
  //
  //   1. It is defined as a Species in the Model
  //   2. It is not a Reactant or Product in this Reaction.
  //
  // Thus modifiers must be added where appropriate.
  //
  const char *id;

  unsigned int size;
  unsigned int n, l;

  const ASTNode *node;
  List          *names;
  KineticLaw* kl;

  for (n = 0; n < getNumReactions(); n++)
  {
    kl = getReaction(n)->getKineticLaw();

    if (kl == 0 || kl->isSetMath() == false) continue;
   
    node  = kl->getMath();
    names = node->getListOfNodes((ASTNodePredicate) ASTNode_isName);
    size  = names->getSize();

    for (l = 0; l < size; l++)
    {
      node = (ASTNode *) names->get(l);
      id   = node->getName();

      // 1. It is an AST_NAME (not AST_NAME_TIME), and
      if (node->getType() != AST_NAME) continue;

      // 2. It refers to a Species in this Model, and
      if (id == 0 || getSpecies(id) == 0) continue;

      // 3. It is not a Reactant, Product, or (already) a Modifier
      if (getReaction(n)->getReactant(id) != 0) continue;
      if (getReaction(n)->getProduct (id) != 0) continue;
      if (getReaction(n)->getModifier(id) != 0) continue;

      getReaction(n)->createModifier()->setSpecies(id);
    }

    delete names;
  }

}
