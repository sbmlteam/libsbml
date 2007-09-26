/**
 * @file    LocalParameterShadowsIdInModel.cpp
 * @brief   Checks whether local parameters are shadowing another id
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Reaction.h>
#include <sbml/Species.h>

#include "LocalParameterShadowsIdInModel.h"
#include "IdList.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */




/**
 * Creates a new Constraint with the given constraint id.
 */
LocalParameterShadowsIdInModel::LocalParameterShadowsIdInModel (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
LocalParameterShadowsIdInModel::~LocalParameterShadowsIdInModel ()
{
}


/**
  * Checks that any species with boundary condition false
  * is not set by reaction and rules
  */
void
LocalParameterShadowsIdInModel::check_ (const Model& m, const Model& object)
{
  unsigned int n, size;

  /* populate list */
  //if (m.getId() != "") mAll.append(m.getId());
  
  size = m.getNumFunctionDefinitions();
  for (n = 0; n < size; ++n) mAll.append( m.getFunctionDefinition(n)->getId() );

  //size = m.getNumCompartmentTypes();
  //for (n = 0; n < size; ++n) mAll.append( m.getCompartmentType(n)->getId() );

  //size = m.getNumSpeciesTypes();
  //for (n = 0; n < size; ++n) mAll.append( m.getSpeciesType(n)->getId() );

  size = m.getNumCompartments();
  for (n = 0; n < size; ++n) mAll.append( m.getCompartment(n)->getId() );

  size = m.getNumSpecies();
  for (n = 0; n < size; ++n) mAll.append( m.getSpecies(n)->getId() );

  size = m.getNumParameters();
  for (n = 0; n < size; ++n) mAll.append( m.getParameter(n)->getId() );

  size = m.getNumReactions(); 
  for (n = 0; n < size; ++n) mAll.append( m.getReaction(n)->getId() );
  //for (n = 0; n < size; ++n) 
  //{
  //  mAll.append( m.getReaction(n)->getId() );

  //  sr_size = m.getReaction(n)->getNumReactants();
  //  for (sr = 0; sr < sr_size; sr++)
  //  {
  //    mAll.append(m.getReaction(n)->getReactant(sr)->getId());
  //  }

  //  sr_size = m.getReaction(n)->getNumProducts();
  //  for (sr = 0; sr < sr_size; sr++)
  //  {
  //    mAll.append(m.getReaction(n)->getProduct(sr)->getId());
  //  }

  //  sr_size = m.getReaction(n)->getNumModifiers();
  //  for (sr = 0; sr < sr_size; sr++)
  //  {
  //    mAll.append(m.getReaction(n)->getModifier(sr)->getId());
  //  }

  //}

  //size = m.getNumEvents();
  //for (n = 0; n < size; ++n) mAll.append( m.getEvent(n)->getId() );

  // at this point the list contains any ids in the model
  // loop thru each kineticlaw's listOfParameters and log conflicts


  size = m.getNumReactions();
  for (n = 0; n < size; ++n)
  {
    const KineticLaw* kl = m.getReaction(n)->getKineticLaw();
    if (!kl) continue;

    for (unsigned int p = 0; p < kl->getNumParameters(); ++p)
    {
      std::string id = kl->getParameter(p)->getId();
      if (mAll.contains(id))
      {
        // find the element of conflict
        const SBase * conflictObject = NULL;
        if (m.getFunctionDefinition(id)) 
          conflictObject = static_cast <const SBase *> (m.getFunctionDefinition(id));
        else if (m.getCompartment(id))
          conflictObject = static_cast <const SBase *> (m.getCompartment(id));
        else if (m.getSpecies(id))
          conflictObject = static_cast <const SBase *> (m.getSpecies(id));
        else if (m.getParameter(id))
          conflictObject = static_cast <const SBase *> (m.getParameter(id));
        else if (m.getReaction(id))
          conflictObject = static_cast <const SBase *> (m.getReaction(id));
        //else if (m.getCompartmentType(id))
        //  object = static_cast <const SBase *> (m.getCompartmentType(id));
        //else if (m.getSpeciesType(id))
        //  object = static_cast <const SBase *> (m.getSpeciesType(id));
        //else if (m.getEvent(id))
        //  object = static_cast <const SBase *> (m.getEvent(id));

        logConflict(*(kl->getParameter(p)), *conflictObject);
      }
    }
  }
}

/**
  * Logs a message about species with boundary condition false
  * being set by reaction and rules
  */
void
LocalParameterShadowsIdInModel::logConflict (const Parameter& p, 
                                             const SBase& object)
{
  msg =
    //"A <species>'s quantity cannot be determined simultaneously by both "
    //"reactions and rules. More formally, if the identifier of a <species> "
    //"definition having 'boundaryCondition'='false' and 'constant'='false' is "
    //"referenced by a <speciesReference> anywhere in a model, then this "
    //"identifier cannot also appear as the value of a 'variable' in an "
    //"<assignmentRule> or a <rateRule>. (References: L2V1 Section 4.6.5; L2V2 "
    //"Section 4.8.6; L2V3 Section 4.8.6.) 
    "The id '";

  msg += p.getId();
  msg += "' used for a local parameter also occurs as the id of a '";
  msg += SBMLTypeCode_toString(object.getTypeCode());
  msg += "'.";

  
  logFailure(p);
}
