/**
 * \file    FormulaUnitsData.cpp
 * \brief   Class for storing information relating to units of a formula
 * \author  SBML Team <sbml-team@caltech.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
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



#include "FormulaUnitsData.h"


using namespace std;

/**
  * Populates the ListFormulaDataUnits with the units of each 
  * set of math encountered in the model
  */
void
Model::createListFormulaUnitsData()
{
  unsigned int n, j;
  UnitFormulaFormatter *unitFormatter = new UnitFormulaFormatter(this);
  SBMLTypeCode_t typecode;
  char newId[12];
  unsigned int countAlg = 0, countEvents = 0;
  Compartment * c;
  Species * s;
  Parameter * p;
  Rule * r;
  InitialAssignment * ia;
  Event * e;
  EventAssignment * ea;
  Reaction * react;
  SpeciesReference * sr;

  FormulaUnitsData *fud;
  UnitDefinition *ud = new UnitDefinition();
  Unit *u;

  /*
   * put in a default unit for substance and time
   * this is necessary for validation
   */
  //fud = new FormulaUnitsData();
  //fud->setId("per_time");
  //fud->setTypecode(SBML_UNKNOWN);
  //u = new Unit("second", -1);
  //ud->addUnit(u);
  //fud->setUnitDefinition(ud);
  //addFormulaUnitsData(fud);

  //fud = new FormulaUnitsData();
  //fud->setId("substance");
  //fud->setTypecode(SBML_UNKNOWN);
  //ud = new UnitDefinition();
  //u = new Unit("mole", 1);
  //ud->addUnit(u);
  //fud->setUnitDefinition(ud);
  //addFormulaUnitsData(fud);

  fud = new FormulaUnitsData();
  fud->setId("subs_per_time");
  fud->setTypecode(SBML_UNKNOWN);
  u = new Unit("mole", 1);
  ud->addUnit(u);
  u = new Unit("second", -1);
  ud->addUnit(u);
  fud->setUnitDefinition(ud);
  addFormulaUnitsData(fud);

  /* get unit data from each compartment 
   * this is necessary for validation
   */
  for (n = 0; n < getNumCompartments(); n++)
  {
    c = getCompartment(n);
    fud = new FormulaUnitsData();
    fud->setId(c->getId());
    fud->setTypecode(SBML_COMPARTMENT);
    ud = unitFormatter->getUnitDefinitionFromCompartment(c);
    fud->setUnitDefinition(ud);

    ud = new UnitDefinition();
    for (j = 0; j < fud->getUnitDefinition()->getNumUnits(); j++)
      ud->addUnit(fud->getUnitDefinition()->getUnit(j));
    u = new Unit("second", -1);
    ud->addUnit(u);
    fud->setPerTimeUnitDefinition(ud);

    addFormulaUnitsData(fud);
  }

  /* get unit data from each species 
   * this is necessary for validation
   */
  for (n=0; n < getNumSpecies(); n++)
  {
    s = getSpecies(n);
    fud = new FormulaUnitsData();
    fud->setId(s->getId());
    fud->setTypecode(SBML_SPECIES);
    /* if the species has not been given a compartment
     * this will blow up although it is caught by another rule
     */
    if (getCompartment(s->getCompartment()) == NULL)
      ud = NULL;
    else
      ud = unitFormatter->getUnitDefinitionFromSpecies(s);
    fud->setUnitDefinition(ud);

    if (ud != NULL)
    {
      ud = new UnitDefinition();
      for (j = 0; j < fud->getUnitDefinition()->getNumUnits(); j++)
        ud->addUnit(fud->getUnitDefinition()->getUnit(j));
      u = new Unit("second", -1);
      ud->addUnit(u);
      fud->setPerTimeUnitDefinition(ud);
    }

    addFormulaUnitsData(fud);
  }

  /* get unit data from each parameter 
   * could be left out for now but will be necessary for manipulation of units
   */
  for (n=0; n < getNumParameters(); n++)
  {
    p = getParameter(n);
    fud = new FormulaUnitsData();
    fud->setId(p->getId());
    fud->setTypecode(SBML_PARAMETER);
    unitFormatter->resetFlags();
    ud = unitFormatter->getUnitDefinitionFromParameter(p);
    fud->setUnitDefinition(ud);
    fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
    fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());

    if (ud != NULL)
    {
      ud = new UnitDefinition();
      for (j = 0; j < fud->getUnitDefinition()->getNumUnits(); j++)
        ud->addUnit(fud->getUnitDefinition()->getUnit(j));
      u = new Unit("second", -1);
      ud->addUnit(u);
      simplifyUnitDefinition(ud);
    }
    fud->setPerTimeUnitDefinition(ud);

    addFormulaUnitsData(fud);
  }

   /* get units returned by the formula given for each initial assignment
   */
  for (n=0; n < getNumInitialAssignments(); n++)
  {
    ia = getInitialAssignment(n);
    fud = new FormulaUnitsData();
    fud->setId(ia->getSymbol());
    fud->setTypecode(SBML_INITIAL_ASSIGNMENT);
    unitFormatter->resetFlags();
    if (ia->isSetMath())
    {
      ud = unitFormatter->getUnitDefinition(ia->getMath());
      fud->setUnitDefinition(ud);
      fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
      fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
    }
    else
    {
      ud = NULL;
      fud->setUnitDefinition(ud);
    }
    addFormulaUnitsData(fud);
  }
 /* get units returned by the formula given for each rule
   */
  for (n=0; n < getNumRules(); n++)
  {
    r = getRule(n);
    fud = new FormulaUnitsData();
    typecode = r->getTypeCode();
    if (typecode == SBML_ALGEBRAIC_RULE)
    {
      sprintf(newId, "alg_rule_%u", countAlg);
      fud->setId(newId);
      countAlg++;
    }
    else
    {
      fud->setId(r->getVariable());
    }
    fud->setTypecode(typecode);
    unitFormatter->resetFlags();
    if (r->isSetMath())
    {
      ud = unitFormatter->getUnitDefinition(r->getMath());
      fud->setUnitDefinition(ud);
      fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
      fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
    }
    else
    {
      ud = NULL;
      fud->setUnitDefinition(ud);
    }
    addFormulaUnitsData(fud);
  }


  /**
   * math may occur in reactions in kineticLaw or as stoichiometryMath 
   * on reactants or products
   */
  for (n=0; n < getNumReactions(); n++)
  {
    react = getReaction(n);

    /* get units returned by kineticLaw formula */
    if (react->isSetKineticLaw())
    {
      fud = new FormulaUnitsData();
      fud->setId(react->getId());

      /* set the id of the kinetic law 
       * normally a kinetic law doesnt have an id
       * but since it is an sbase calss it can
       * so we set it to be the reaction id so 
       * that searching the listFormulaUnitsData can find it
       */
      react->getKineticLaw()->setId(react->getId());

      fud->setTypecode(SBML_KINETIC_LAW);
      unitFormatter->resetFlags();
      if(react->getKineticLaw()->isSetMath())
      {
        ud = unitFormatter->getUnitDefinition(react->getKineticLaw()->getMath());
        fud->setUnitDefinition(ud);
        fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
        fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
      }
      else
      {
        ud = NULL;
        fud->setUnitDefinition(ud);
      }
      addFormulaUnitsData(fud);
    }

    /* get units returned by any stoichiometryMath set */
    for (j = 0; j < react->getNumReactants(); j++)
    {
      sr = react->getReactant(j);

      if (sr->isSetStoichiometryMath())
      {
        fud = new FormulaUnitsData();
        fud->setId(sr->getSpecies());
        fud->setTypecode(SBML_SPECIES_REFERENCE);
        unitFormatter->resetFlags();
        ud = unitFormatter->getUnitDefinition(sr->getStoichiometryMath());
        fud->setUnitDefinition(ud);
        fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
        fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
        addFormulaUnitsData(fud);
      }
    }

    for (j = 0; j < react->getNumProducts(); j++)
    {
      sr = react->getProduct(j);

      if (sr->isSetStoichiometryMath())
      {
        fud = new FormulaUnitsData();
        fud->setId(sr->getId());
        fud->setTypecode(SBML_SPECIES_REFERENCE);
        unitFormatter->resetFlags();
        ud = unitFormatter->getUnitDefinition(sr->getStoichiometryMath());
        fud->setUnitDefinition(ud);
        fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
        fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
        addFormulaUnitsData(fud);
      }
    }
  }

  /**
   * math may occur in events as the trigger, the delay or in the eventAssignment
   */
  for (n=0; n < getNumEvents(); n++)
  {
    e = getEvent(n);

    /* dont need units returned by trigger formula - 
     * should be boolean
     */
    
    /* get units returned by dely */
    if (e->isSetDelay())
    {
      fud = new FormulaUnitsData();

      if (e->isSetId())
      {
        fud->setId(e->getId());
      }
      else
      {
        sprintf(newId, "event_%u", countEvents);
        fud->setId(newId);
        e->setId(newId);
      }
      countEvents++;

      fud->setTypecode(SBML_EVENT);
      unitFormatter->resetFlags();
      ud = unitFormatter->getUnitDefinition(e->getDelay()->getMath());
      fud->setUnitDefinition(ud);
      fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
      fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
      
      /* get event time definition */
      unitFormatter->resetFlags();
      ud = unitFormatter->getUnitDefinitionFromEventTime(e);
      fud->setEventTimeUnitDefinition(ud);

      addFormulaUnitsData(fud);
    }

    /* get units returned by any event assignments */
    for (j = 0; j < e->getNumEventAssignments(); j++)
    {
      ea = e->getEventAssignment(j);

      if (ea->isSetMath())    
      {
        fud = new FormulaUnitsData();
        fud->setId(ea->getVariable());
        fud->setTypecode(SBML_EVENT_ASSIGNMENT);
        unitFormatter->resetFlags();
        ud = unitFormatter->getUnitDefinition(ea->getMath());
        fud->setUnitDefinition(ud);
        fud->setContainsParametersWithUndeclaredUnits(unitFormatter->getUndeclaredUnits());
        fud->setCanIgnoreUndeclaredUnits(unitFormatter->getCanIgnoreUndeclaredUnits());
        addFormulaUnitsData(fud);
      }
    }
  }



}

/**
 * Adds a copy of the given FormulaUnitsData to this Model.
 */
void
Model::addFormulaUnitsData (const FormulaUnitsData* e)
{
  mFormulaUnitsData->add((void *)e);
}


/**
 * @return the nth FormulaUnitsData of this Model.
 */
const FormulaUnitsData*
Model::getFormulaUnitsData (unsigned int n) const
{
  return static_cast<const FormulaUnitsData*>( mFormulaUnitsData->get(n) );
}


/**
 * @return the nth FormulaUnitsData of this Model.
 */
FormulaUnitsData*
Model::getFormulaUnitsData (unsigned int n)
{
  return static_cast<FormulaUnitsData*>( mFormulaUnitsData->get(n) );
}



/**
 * @return the FormulaUnitsData in this Model with the given id or NULL if no such
 * FormulaUnitsData exists.
 */
const FormulaUnitsData*
Model::getFormulaUnitsData (const string& sid, SBMLTypeCode_t typecode) const
{
  const FormulaUnitsData * fud;

  for (unsigned int n = 0; n < mFormulaUnitsData->getSize(); n++)
  {
    fud = getFormulaUnitsData(n); 
    if (!strcmp(fud->getId().c_str(), sid.c_str()))
    {
      if (fud->getTypecode() == typecode)
      {
        return static_cast<const FormulaUnitsData*>( mFormulaUnitsData->get(n) );
      }
    }
  }
  return NULL;
}


/**
 * @return the FormulaUnitsData in this Model with the given id  and typecode 
 * or NULL if no such FormulaUnitsData exists.
 */
FormulaUnitsData*
Model::getFormulaUnitsData (const string& sid, SBMLTypeCode_t typecode)
{
  FormulaUnitsData * fud;

  for (unsigned int n = 0; n < mFormulaUnitsData->getSize(); n++)
  {
    fud = getFormulaUnitsData(n);
    if (!strcmp(fud->getId().c_str(), sid.c_str()))
    {
      if (fud->getTypecode() == typecode)
      {
        return static_cast<FormulaUnitsData*>( mFormulaUnitsData->get(n) );
      }
    }
  }
  return NULL;
}


/**
 * @return the number of FormulaUnitsDatas in this Model.
 */
unsigned int
Model::getNumFormulaUnitsData () const
{
  return mFormulaUnitsData->getSize();
}


/**
 * returns true if the list has been populated, false otherwise
 */
bool
Model::isWrittenFormulaUnitsData()
{
  if (mFormulaUnitsData->getSize() == 0)
    return false;
  else
    return true;
}


FormulaUnitsData::FormulaUnitsData()
{
  mContainsParametersWithUndeclaredUnits = 0;
  mCanIgnoreUndeclaredUnits = 1;
  mUnitDefinition = new UnitDefinition();
  mPerTimeUnitDefinition = new UnitDefinition();
}

FormulaUnitsData::~FormulaUnitsData()
{

}
