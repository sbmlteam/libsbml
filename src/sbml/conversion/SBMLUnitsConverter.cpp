/**
 * @file    SBMLUnitsConverter.cpp
 * @brief   Implementation of SBMLUnitsConverter, for converting units to SI.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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

#include <sbml/conversion/SBMLUnitsConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/extension/SBasePlugin.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

  
void SBMLUnitsConverter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new SBMLUnitsConverter());
}


SBMLUnitsConverter::SBMLUnitsConverter () :
    SBMLConverter()
{
  newIdCount = 0;
}


/*
 * Copy constructor.
 */
SBMLUnitsConverter::SBMLUnitsConverter(const SBMLUnitsConverter& orig) :
    SBMLConverter(orig)
{
  newIdCount = orig.newIdCount;
}


/*
 * Destroy this object.
 */
SBMLUnitsConverter::~SBMLUnitsConverter ()
{
}


/*
 * Assignment operator for SBMLUnitsConverter.
 */
SBMLUnitsConverter& 
SBMLUnitsConverter::operator=(const SBMLUnitsConverter& rhs)
{  
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    this->SBMLConverter::operator =(rhs);
  }

  return *this;
}


SBMLUnitsConverter*
SBMLUnitsConverter::clone () const
{
  return new SBMLUnitsConverter(*this);
}


ConversionProperties
SBMLUnitsConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("units", true, "this is checked by matchProperties");
  return prop;
}


bool 
SBMLUnitsConverter::matchesProperties(const ConversionProperties &props) const
{
  if (&props == NULL || !props.hasOption("units"))
    return false;
  return true;
}


int
SBMLUnitsConverter::convert()
{

  if (mDocument == NULL) return LIBSBML_INVALID_OBJECT;
  Model* m = mDocument->getModel();
  if (m == NULL) return LIBSBML_INVALID_OBJECT;

  /* do not yet deal with the following units */
  if (m->getLevel() > 2)
  {
    if (m->isSetSubstanceUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
    else if (m->isSetTimeUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
    else if (m->isSetLengthUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
    else if (m->isSetAreaUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
    else if (m->isSetVolumeUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
    else if (m->isSetExtentUnits() == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;

    if (hasCnUnits(*m) == true)
      return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
  }

  unsigned int i;
  if (m->getLevel() == 2 && m->getVersion() < 3)
  {
    for (i = 0; i < m->getNumSpecies(); i++)
    {
      if (m->getSpecies(i)->isSetSpatialSizeUnits() == true)
      {
        return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
      }
    }
    for (i = 0; i < m->getNumEvents(); i++)
    {
      if (m->getEvent(i)->isSetTimeUnits() == true)
      {
        return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
      }
    }
  }
  if (m->getLevel() == 1 ||(m->getLevel() == 2 && m->getVersion() == 1))
  {
    for (i = 0; i < m->getNumReactions(); i++)
    {
      if (m->getReaction(i)->isSetKineticLaw() == true
        && ((m->getReaction(i)->getKineticLaw()->isSetTimeUnits() == true)
        || (m->getReaction(i)->getKineticLaw()->isSetSubstanceUnits() == true)))
      {
        return LIBSBML_CONV_CONVERSION_NOT_AVAILABLE;
      }
    }

  }

  /* need to check consistency - cannot convert if units are not declared */
  mDocument->getErrorLog()->clearLog();
  unsigned char origValidators = mDocument->getApplicableValidators();

  mDocument->setApplicableValidators(AllChecksON);


  unsigned int errors = mDocument->checkConsistency();

  if (unacceptable_errors(errors) == true)
  {
    mDocument->setApplicableValidators(origValidators);
    return LIBSBML_CONV_INVALID_SRC_DOCUMENT;
  }


  bool conversion = true;

  for (i = 0; i < m->getNumParameters() && conversion == true; i++)
  {
    Parameter *p = m->getParameter(i);
    conversion = convertUnits(*p, *m);
  }

  for (i = 0; i < m->getNumCompartments() && conversion == true; i++)
  {
    Compartment *c = m->getCompartment(i);
    conversion = convertUnits(*c, *m);
  }
  for (i = 0; i < m->getNumSpecies() && conversion == true; i++)
  {
    Species *s = m->getSpecies(i);
    conversion = convertUnits(*s, *m);
  }
  for (i = 0; i < m->getNumReactions() && conversion == true; i++)
  {
    Reaction *r = m->getReaction(i);
    if (r->isSetKineticLaw() == true)
    {
      for (unsigned int j = 0; j < r->getKineticLaw()->getNumParameters(); j++)
      {
        Parameter *p = r->getKineticLaw()->getParameter(j);
        conversion = convertUnits(*p, *m);
      }
    }
  }

  removeUnusedUnitDefinitions(*m);
    
  mDocument->setApplicableValidators(origValidators);

  if (conversion == true)
    return LIBSBML_OPERATION_SUCCESS;
  else
    return LIBSBML_OPERATION_FAILED;
}
  
bool
SBMLUnitsConverter::convertUnits(SBase &sb, Model &m)
{
  /* NOTE: the getDerivedUnitDefinition functions will return
   * units from the original model - they do not get updated
   * until after the conversion is complete
   */
  bool conversion = false;

  int tc = sb.getTypeCode();

  UnitDefinition *ud = NULL;
  UnitDefinition *ud_vol = NULL;

  double oldValue = 0;
  switch(tc)
  {
    case SBML_PARAMETER:
      oldValue = static_cast<Parameter &>(sb).getValue();
      ud = static_cast<Parameter &>(sb).getDerivedUnitDefinition();
      break;
    case SBML_LOCAL_PARAMETER:
      oldValue = static_cast<Parameter &>(sb).getValue();
      ud = static_cast<Parameter &>(sb).getDerivedUnitDefinition();
      break;
    case SBML_COMPARTMENT:
      oldValue = static_cast<Compartment &>(sb).getSize();
      ud = static_cast<Compartment &>(sb).getDerivedUnitDefinition();
      break;
    case SBML_SPECIES:
      if (static_cast<Species &>(sb).isSetInitialAmount() == true)
      {
        oldValue = static_cast<Species &>(sb).getInitialAmount();
      }
      else
      {
        oldValue = static_cast<Species &>(sb).getInitialConcentration();
      }
      ud = static_cast<Species &>(sb).getDerivedUnitDefinition();
      /* the derived units will be concentration unless species
       * hasOnlySubstanceUnits is true or species is within
       * a 0D compartment
       */
      if (static_cast<Species &>(sb).getHasOnlySubstanceUnits() == false)
      {
        ud_vol = m.getCompartment(static_cast<Species &>(sb).getCompartment())
                                          ->getDerivedUnitDefinition();
      }
      else
      {
        ud_vol = NULL;
      }
      break;
    default:
      return conversion;
      break;
  }

  UnitDefinition *siud = UnitDefinition::convertToSI(ud);
  double newValue = oldValue * pow(siud->getUnit(0)->getMultiplier(),
    siud->getUnit(0)->getExponentAsDouble());
  for (unsigned int n = 1; n < siud->getNumUnits(); n++)
  {
    newValue = newValue * pow(siud->getUnit(n)->getMultiplier(),
                          siud->getUnit(n)->getExponentAsDouble());
  }
  if (ud_vol != NULL)
  {
    /* we are dealing in concentration 
    * but the unit of the species must be substance
    */
    ud = UnitDefinition::combine(ud, ud_vol);
    siud = UnitDefinition::convertToSI(ud);
  }

  int i;
  switch(tc)
  {
    case SBML_PARAMETER:
      i = static_cast<Parameter &>(sb).setValue(newValue);
      break;
    case SBML_LOCAL_PARAMETER:
      i = static_cast<Parameter &>(sb).setValue(newValue);
      break;
    case SBML_COMPARTMENT:
      i = static_cast<Compartment &>(sb).setSize(newValue);
      break;
    case SBML_SPECIES:
      if (static_cast<Species &>(sb).isSetInitialAmount() == true)
      {
        if (static_cast<Species &>(sb).getHasOnlySubstanceUnits() == false)
        {
          if (m.getCompartment(static_cast<Species &>(sb).getCompartment())
                                            ->getSpatialDimensions() != 0)
          {
            newValue = newValue * 
              m.getCompartment(static_cast<Species &>(sb).getCompartment())
                                            ->getSize();
          }
        }
        i = static_cast<Species &>(sb).setInitialAmount(newValue);
      }
      else
      {
        i = static_cast<Species &>(sb).setInitialConcentration(newValue);
      }
      break;
    default:
      i = LIBSBML_INVALID_OBJECT;
      break;
  }

  if (i == LIBSBML_OPERATION_SUCCESS)
  {
    /* si unit has more than one base unit and so will need a unitDefinition */
    if (siud->getNumUnits() > 1)
    {
      i = siud->getUnit(0)->setMultiplier(1.0);
      if (i == LIBSBML_OPERATION_SUCCESS)
      {
        i = applyNewUnitDefinition(sb, m, siud);
      }
    }
    else
    {
      Unit *u = siud->getUnit(0);
      i = u->setMultiplier(1.0);
      if (i == LIBSBML_OPERATION_SUCCESS)
      {
        /* only one base unit if exponent is 1 do not need definition */
        if (u->getExponentAsDouble() == 1)
        {
          std::string newUnit = UnitKind_toString(u->getKind());
          switch(tc)
          {
            case SBML_PARAMETER:
              i = static_cast<Parameter &>(sb).setUnits(newUnit);
              break;
            case SBML_LOCAL_PARAMETER:
              i = static_cast<Parameter &>(sb).setUnits(newUnit);
              break;
            case SBML_COMPARTMENT:
              i = static_cast<Compartment &>(sb).setUnits(newUnit);
              break;
            case SBML_SPECIES:
              i = static_cast<Species &>(sb).setSubstanceUnits(newUnit);
              break;
            default:
              i = LIBSBML_INVALID_OBJECT;
              break;
          }
        }
        else
        {
          i = applyNewUnitDefinition(sb, m, siud);
        }
      }
    }
  }


  if (i == LIBSBML_OPERATION_SUCCESS)
    conversion = true;

  return conversion;
}


int
SBMLUnitsConverter::applyNewUnitDefinition(SBase &sb, Model &m, 
                                           UnitDefinition* newUD)
{

  int tc = sb.getTypeCode();
  std::string oldUnits;

  switch(tc)
  {
    case SBML_PARAMETER:
      oldUnits = static_cast<Parameter &>(sb).getUnits();
      break;
    case SBML_LOCAL_PARAMETER:
      oldUnits = static_cast<Parameter &>(sb).getUnits();
      break;
    case SBML_COMPARTMENT:
      oldUnits = static_cast<Compartment &>(sb).getUnits();
      break;
    case SBML_SPECIES:
      oldUnits = static_cast<Species &>(sb).getSubstanceUnits();
      break;
    default:
      return LIBSBML_INVALID_OBJECT;
      break;
  }


  /* object already points to a unit def
   * check whether the existing defn matches the si ud */
  if (m.getUnitDefinition(oldUnits) != NULL)
  {
    if (UnitDefinition::areIdentical(m.getUnitDefinition(oldUnits),
               newUD, true))
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
  }

  /* does it the newUD already exist */


  std::string newId;
  char number[4];
  int i;
  newId = existsAlready(m, newUD);
  if (newId.empty())
  {
    if (newUD->isVariantOfDimensionless())
    {
      newId = "dimensionless";
    }
  }
  if (newId.empty())
  {
    sprintf(number, "%u", newIdCount);
    newId = "unitSid_" + string(number);
    newIdCount++;

    /* double check that this id has not been used */
    while (m.getUnitDefinition(newId) != NULL)
    {
      sprintf(number, "%u", newIdCount);
      newId = "unitSid_" + string(number);
      newIdCount++;
    }
  
    i = newUD->setId(newId);
    if (i == LIBSBML_OPERATION_SUCCESS)
    {
      switch(tc)
      {
        case SBML_PARAMETER:
          i = static_cast<Parameter &>(sb).setUnits(newId);
          break;
        case SBML_LOCAL_PARAMETER:
          i = static_cast<Parameter &>(sb).setUnits(newId);
          break;
        case SBML_COMPARTMENT:
          i = static_cast<Compartment &>(sb).setUnits(newId);
          break;
        case SBML_SPECIES:
          i = static_cast<Species &>(sb).setSubstanceUnits(newId);
          break;
        default:
          i = LIBSBML_INVALID_OBJECT;
          break;
      }
      if (i == LIBSBML_OPERATION_SUCCESS)
      {
        i = m.addUnitDefinition(newUD);
      }
    }
  }
  else
  {
    switch(tc)
    {
      case SBML_PARAMETER:
        i = static_cast<Parameter &>(sb).setUnits(newId);
        break;
      case SBML_LOCAL_PARAMETER:
        i = static_cast<Parameter &>(sb).setUnits(newId);
        break;
      case SBML_COMPARTMENT:
        i = static_cast<Compartment &>(sb).setUnits(newId);
        break;
      case SBML_SPECIES:
        i = static_cast<Species &>(sb).setSubstanceUnits(newId);
        break;
      default:
        i = LIBSBML_INVALID_OBJECT;
        break;
    }
  }

  return i;
}



std::string 
SBMLUnitsConverter::existsAlready(Model& m, UnitDefinition *newUD)
{
  std::string id = "";
  for (unsigned int i = 0; i < m.getNumUnitDefinitions(); i++)
  {
    if (UnitDefinition::areIdentical(m.getUnitDefinition(i), newUD, true))
    {
      return m.getUnitDefinition(i)->getId();
    }
  }

  return id;
}


void
SBMLUnitsConverter::removeUnusedUnitDefinitions(Model& m)
{
  unsigned int num = m.getNumUnitDefinitions()-1;
  for (int i = num; i >= 0; i--)
  {
    if (isUsed(m, m.getUnitDefinition(i)->getId()) == false)
    {
      m.removeUnitDefinition(i);
    }
  }

}

bool
SBMLUnitsConverter::isUsed(Model& m, std::string unitSId)
{
  unsigned int i = 0;

  if (m.getLevel() > 2)
  {
    if (unitSId == m.getSubstanceUnits())
      return true;
    else if (unitSId == m.getTimeUnits())
      return true;
    else if (unitSId == m.getLengthUnits())
      return true;
    else if (unitSId == m.getAreaUnits())
      return true;
    else if (unitSId == m.getVolumeUnits())
      return true;
    else if (unitSId == m.getExtentUnits())
      return true;
  }

  for (i = 0; i < m.getNumParameters(); i++)
  {
    if (unitSId == m.getParameter(i)->getUnits())
    {
      return true;
    }
  }
  for (i = 0; i < m.getNumCompartments(); i++)
  {
    if (unitSId == m.getCompartment(i)->getUnits())
    {
      return true;
    }
  }
  for (i = 0; i < m.getNumSpecies(); i++)
  {
    if (unitSId == m.getSpecies(i)->getSubstanceUnits())
    {
      return true;
    }
    if (m.getLevel() == 2 && m.getVersion() < 3)
    {
      if (unitSId == m.getSpecies(i)->getSpatialSizeUnits())
      {
        return true;
      }
    }
  }
  for (i = 0; i < m.getNumEvents(); i++)
  {
    if (m.getLevel() == 2 && m.getVersion() < 3)
    {
      if (unitSId == m.getEvent(i)->getTimeUnits())
      {
        return true;
      }
    }
  }
  for (i = 0; i < m.getNumReactions(); i++)
  {
    if (m.getReaction(i)->isSetKineticLaw() == true)
    {
      KineticLaw * kl = m.getReaction(i)->getKineticLaw();
      if ((m.getLevel() == 1 || (m.getLevel() == 2 && m.getVersion() == 1)))
      {
        if (unitSId == kl->getTimeUnits())
        {
          return true;
        }
        if (unitSId == kl->getSubstanceUnits())
        {
          return true;
        }
      }
      for (unsigned int j = 0; j < kl->getNumParameters(); j++)
      {
        if (unitSId == kl->getParameter(j)->getUnits())
        {
          return true;
        }
      }
    }
  }


  return false;
}

/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
SBMLUnitsConverter::unacceptable_errors(unsigned int errors)
{
  if (errors > 0)
  {
    if (mDocument->getErrorLog()
                              ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      return true;
    else
    {  
      for (unsigned int i = 0; i < mDocument->getErrorLog()->getNumErrors(); i++)
      {
        if (mDocument->getErrorLog()->getError(i)->getErrorId() == CompartmentShouldHaveSize)
        {
          return true;
        }
        else if (mDocument->getErrorLog()->getError(i)->getErrorId() == ParameterShouldHaveUnits)
        {
          return true;
        }
        else if (mDocument->getErrorLog()->getError(i)->getErrorId() == SpeciesShouldHaveValue)
        {
          return true;
        }
        else if (mDocument->getErrorLog()->getError(i)->getErrorId() == UndeclaredUnits)
        {
          return true;
        }
        else if (mDocument->getErrorLog()->getError(i)->getErrorId() > 10500 &&
          mDocument->getErrorLog()->getError(i)->getErrorId() < 10599)
        {
          return true;
        }
      }

      return false;
    }
  }
  else
  {
    return false;
  }
}

bool
SBMLUnitsConverter::hasCnUnits(Model& m)
{
  /* check all math within a model */

  unsigned int n;
  for (n = 0; n < m.getNumRules(); n++)
  {
    if (m.getRule(n)->isSetMath())
    {
      if (mathHasCnUnits(m.getRule(n)->getMath()) == true)
        return true;
    }
  }

  for (n = 0; n < m.getNumReactions(); n++)
  {
    if (m.getReaction(n)->isSetKineticLaw())
    {
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        if (mathHasCnUnits(m.getReaction(n)->getKineticLaw()->getMath()) == true)
          return true;
      }
    }
  }

  for (n = 0; n < m.getNumEvents(); n++)
  {
    if (m.getEvent(n)->isSetTrigger())
    {
      if (m.getEvent(n)->getTrigger()->isSetMath())
      {
        if (mathHasCnUnits(m.getEvent(n)->getTrigger()->getMath()) == true)
          return true;
      }
    }
    if (m.getEvent(n)->isSetDelay())
    {
      if (m.getEvent(n)->getDelay()->isSetMath())
      {
        if (mathHasCnUnits(m.getEvent(n)->getDelay()->getMath()) == true)
          return true;
      }
    }
    if (m.getEvent(n)->isSetPriority())
    {
      if (m.getEvent(n)->getPriority()->isSetMath())
      {
        if (mathHasCnUnits(m.getEvent(n)->getPriority()->getMath()) == true)
          return true;
      }
    }
    for (unsigned int ea = 0; ea < m.getEvent(n)->getNumEventAssignments(); ea++)
    {
      if (m.getEvent(n)->getEventAssignment(ea)->isSetMath())
      {
        if (mathHasCnUnits(m.getEvent(n)->getEventAssignment(ea)->getMath()) == true)
          return true;
      }
    }
  }

  for (n = 0; n < m.getNumInitialAssignments(); n++)
  {
    if (m.getInitialAssignment(n)->isSetMath())
    {
      if (mathHasCnUnits(m.getInitialAssignment(n)->getMath()) == true)
        return true;
    }
  }

  for (n = 0; n < m.getNumConstraints(); n++)
  {
    if (m.getConstraint(n)->isSetMath())
    {
      if (mathHasCnUnits(m.getConstraint(n)->getMath()) == true)
        return true;
    }
  }

  return false;
}

bool
SBMLUnitsConverter::mathHasCnUnits(const ASTNode * ast)
{
  bool hasCnUnits = false;
  if (ast->isNumber() && ast->hasUnits() == true)
  {
     return true;
  }
  unsigned int n = 0; 
  while(n < ast->getNumChildren() && hasCnUnits == false)
  {
    hasCnUnits = mathHasCnUnits(ast->getChild(n));
    n++;
  }
  return hasCnUnits;
}

/** @cond doxygen-c-only */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


