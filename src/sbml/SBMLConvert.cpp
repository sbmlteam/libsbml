/**
 * @file    SBMLConvert.cpp
 * @brief   Performs conversion between SBML levels
 * @author  Ben Bornstein and Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
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


/*
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


/*
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
  // parameters and compartments are declared to have constant=true
  // by default. Since in L1 the constant attribute didnt exist 
  // parameters/compartments that are the subjcet of rules must have
  // the value changed

  for ( n = 0; n < getNumParameters(); n++)
  {
    if (getRule(getParameter(n)->getId()) != NULL)
    {
      getParameter(n)->setConstant(false);
    }
  }

  for ( n = 0; n < getNumCompartments(); n++)
  {
    if (getRule(getCompartment(n)->getId()) != NULL)
    {
      getCompartment(n)->setConstant(false);
    }
  }

}
void
Model::removeDuplicateTopLevelAnnotations()
{
  unsigned int i, n;
  this->removeDuplicateAnnotations();

  if (getNumFunctionDefinitions() > 0)
  {
    getListOfFunctionDefinitions()->removeDuplicateAnnotations();
    for (i = 0; i < getNumFunctionDefinitions(); i++)
    {
      getFunctionDefinition(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumUnitDefinitions() > 0)
  {
    getListOfUnitDefinitions()->removeDuplicateAnnotations();
    for (i = 0; i < getNumUnitDefinitions(); i++)
    {
      getUnitDefinition(i)->removeDuplicateAnnotations();
      getUnitDefinition(i)->getListOfUnits()->removeDuplicateAnnotations();
      for (n = 0; n < getUnitDefinition(i)->getNumUnits(); n++)
      {
        getUnitDefinition(i)->getUnit(n)->removeDuplicateAnnotations();
      }
    }
  }
  if (getNumCompartmentTypes() > 0)
  {
    getListOfCompartmentTypes()->removeDuplicateAnnotations();
    for (i = 0; i < getNumCompartmentTypes(); i++)
    {
      getCompartmentType(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumSpeciesTypes() > 0)
  {
    getListOfSpeciesTypes()->removeDuplicateAnnotations();
    for (i = 0; i < getNumSpeciesTypes(); i++)
    {
      getSpeciesType(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumCompartments() > 0)
  {
    getListOfCompartments()->removeDuplicateAnnotations();
    for (i = 0; i < getNumCompartments(); i++)
    {
      getCompartment(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumSpecies() > 0)
  {
    getListOfSpecies()->removeDuplicateAnnotations();
    for (i = 0; i < getNumSpecies(); i++)
    {
      getSpecies(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumParameters() > 0)
  {
    getListOfParameters()->removeDuplicateAnnotations();
    for (i = 0; i < getNumParameters(); i++)
    {
      getParameter(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumInitialAssignments() > 0)
  {
    getListOfInitialAssignments()->removeDuplicateAnnotations();
    for (i = 0; i < getNumInitialAssignments(); i++)
    {
      getInitialAssignment(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumConstraints() > 0)
  {
    getListOfConstraints()->removeDuplicateAnnotations();
    for (i = 0; i < getNumConstraints(); i++)
    {
      getConstraint(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumRules() > 0)
  {
    getListOfRules()->removeDuplicateAnnotations();
    for (i = 0; i < getNumRules(); i++)
    {
      getRule(i)->removeDuplicateAnnotations();
    }
  }
  if (getNumReactions() > 0)
  {
    getListOfReactions()->removeDuplicateAnnotations();
    for (i = 0; i < getNumReactions(); i++)
    {
      Reaction * r = getReaction(i);
      r->removeDuplicateAnnotations();
      if (r->getNumReactants() > 0)
      {
        r->getListOfReactants()->removeDuplicateAnnotations();
        for (n = 0; n < r->getNumReactants(); n++)
        {
          r->getReactant(n)->removeDuplicateAnnotations();
        }
      }
      if (r->getNumProducts() > 0)
      {
        r->getListOfProducts()->removeDuplicateAnnotations();
        for (n = 0; n < r->getNumProducts(); n++)
        {
          r->getProduct(n)->removeDuplicateAnnotations();
        }
      }
      if (r->getNumModifiers() > 0)
      {
        r->getListOfModifiers()->removeDuplicateAnnotations();
        for (n = 0; n < r->getNumModifiers(); n++)
        {
          r->getModifier(n)->removeDuplicateAnnotations();
        }
      }
      if (r->isSetKineticLaw())
      {
        r->getKineticLaw()->removeDuplicateAnnotations();
        if (r->getKineticLaw()->getNumParameters() > 0)
        {
          r->getKineticLaw()->getListOfParameters()
                            ->removeDuplicateAnnotations();
          for (n = 0; n < r->getKineticLaw()->getNumParameters(); n++)
          {
            r->getKineticLaw()->getParameter(n)->removeDuplicateAnnotations();
          }
        }
      }
    }
  }
  if (getNumEvents() > 0)
  {
    getListOfEvents()->removeDuplicateAnnotations();
    for (i = 0; i < getNumEvents(); i++)
    {
      getEvent(i)->removeDuplicateAnnotations();
      if (getEvent(i)->getNumEventAssignments() > 0)
      {
        getEvent(i)->getListOfEventAssignments()->removeDuplicateAnnotations();
        for (n = 0; n < getEvent(i)->getNumEventAssignments(); n++)
        {
          getEvent(i)->getEventAssignment(n)->removeDuplicateAnnotations();
        }
      }
    }
  }
}


