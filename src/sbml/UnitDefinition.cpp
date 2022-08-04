/**
 * @file    UnitDefinition.cpp
 * @brief   Implementations of SBML's UnitDefinition and ListOfUnitDefinitions.
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
 * ---------------------------------------------------------------------- -->*/

#include <sstream>


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/units/UnitKindList.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Unit.h>
#include <sbml/UnitDefinition.h>
#include <sbml/extension/SBaseExtensionPoint.h>

#include <sbml/util/ElementFilter.h>

#include <math.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

UnitDefinition::UnitDefinition (unsigned int level, unsigned int version) :
   SBase  ( level, version )
 , mUnits (level, version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  UnitDefinition::connectToChild();
}


UnitDefinition::UnitDefinition (SBMLNamespaces * sbmlns) :
   SBase  ( sbmlns )
 , mUnits (sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    throw SBMLConstructorException(UnitDefinition::getElementName(), sbmlns);
  }

  UnitDefinition::connectToChild();
  loadPlugins(sbmlns);
}


/*
 * Destroys this UnitDefinition.
 */
UnitDefinition::~UnitDefinition ()
{
}


/*
 * Copy constructor. Creates a copy of this UnitDefinition.
 */
UnitDefinition::UnitDefinition(const UnitDefinition& orig) :
    SBase     ( orig )
  , mUnits    ( orig.mUnits )
{
  UnitDefinition::connectToChild();
}


/*
 * Assignment operator.
 */
UnitDefinition& UnitDefinition::operator=(const UnitDefinition& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mUnits = rhs.mUnits;
  }

  UnitDefinition::connectToChild();

  return *this;
}


/** @cond doxygenLibsbmlInternal */
bool
UnitDefinition::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);
  mUnits.accept(v);

  return result;
}
/** @endcond */
\

/*
 * @return a (deep) copy of this UnitDefinition.
 */
UnitDefinition*
UnitDefinition::clone () const
{
  return new UnitDefinition(*this);
}


SBase*
UnitDefinition::getElementBySId(const std::string& id)
{
  if (id.empty()) return NULL;
  SBase* obj = mUnits.getElementBySId(id);
  if (obj != NULL) return obj;

  return getElementFromPluginsBySId(id);
}


SBase*
UnitDefinition::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty()) return NULL;
  if (mUnits.getMetaId() == metaid) return &mUnits;
  SBase* obj = mUnits.getElementByMetaId(metaid);
  if (obj != NULL) return obj;

  return getElementFromPluginsByMetaId(metaid);
}


List*
UnitDefinition::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mUnits, filter);  

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}

/*
 * @return the id of this SBML object.
 */
const string&
UnitDefinition::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
UnitDefinition::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/*
 * @return @c true if the id of this SBML object is set, false
 * otherwise.
 */
bool
UnitDefinition::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return @c true if the name of this SBML object is set, false
 * otherwise.
 */
bool
UnitDefinition::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * Sets the id of this SBML object to a copy of @p sid.
 */
int
UnitDefinition::setId (const std::string& sid)
{
  if (!(SyntaxChecker::isValidInternalSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the name of this SBML object to a copy of name.
 */
int
UnitDefinition::setName (const std::string& name)
{
  /* if this is setting an L2 name the type is string
   * whereas if it is setting an L1 name its type is SId
   */
  if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidInternalSId(name)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mId = name;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
UnitDefinition::unsetName ()
{
  if (getLevel() == 1) 
  {
    mId.erase();
  }
  else 
  {
    mName.erase();
  }

  if (getLevel() == 1 && mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * area. i.e. square metres with only arbitrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfArea (bool relaxed) const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed) 
  {
    // should be metre^2
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isMetre() && u->getExponent() == 2;
    }
  }
  else
  {
    // should be metre
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isMetre();
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * length. i.e. metres with only arbitrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfLength (bool relaxed) const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    //should be metre^1
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isMetre() && u->getExponent() == 1;
    }
  }
  else
  {
    //should be metre
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isMetre();
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * substance. i.e. moles or items with only arbitrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfSubstance (bool relaxed) const
{
  bool result = false;

  unsigned int level = getLevel();
  unsigned int version = getVersion();

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    // should be L1/L2V1: mole^1/item^1
    //                L2: mole/item/gram/kilogram ^1
    //                L3: mole/item/gram/kilogram/avogardo ^1
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      if (level == 2 && version > 1)
      {
        result = ((  u->isMole() || u->isItem() 
                  || u->isGram() || u->isKilogram())
                  && u->getExponent() == 1);
      }
      else if (level > 2)
      {
        result = ((  u->isMole() || u->isItem() 
                  || u->isGram() || u->isKilogram()
                  || u->isAvogadro())
                  && u->getExponent() == 1);
      }
      else
      {
        result        = (u->isMole() || u->isItem()) 
                      && u->getExponent() == 1;
      }
    }
  }
  else
  {
    // should be any combination of mole/item/gram/kilogram/avogardo ^1
    unsigned int i = 0;
    result = true;
    while (result && i < ud->getNumUnits())
    {
      const Unit* u = ud->getUnit(i);
      result = ((  u->isMole() || u->isItem() 
                || u->isGram() || u->isKilogram()
                || u->isAvogadro()));
      
      i++;
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * time. i.e. seconds with only arbitrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfTime (bool relaxed) const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    // unit should simplify to second with exponent 1
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isSecond() && u->getExponent() == 1;
    }
  }
  else
  {
    // from l3v1r2 unit should simplify to second with any exponent
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isSecond();
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * volume. i.e. litre or cubic metre with only arbitrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfVolume (bool relaxed) const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    // should be litre^1 or metre^3
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = (u->isLitre() && u->getExponent() == 1) ||
                      (u->isMetre() && u->getExponent() == 3);
    }
  }
  else
  {
    // should be any combination of litre/metre
    unsigned int i = 0;
    result = true;
    while (result && i < ud->getNumUnits())
    {
      const Unit* u = ud->getUnit(i);
      result = (  u->isLitre() || u->isMetre());
      
      i++;
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of dimensionless.
 * i.e. dimensionless with only arbitrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfDimensionless (bool relaxed) const
{
  bool result = false;

  // careful here if we have no units simplify will add dimensionless
  if (getNumUnits() == 0)
  {
    return result;
  }

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isDimensionless();
    }
  }
  else
  {
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = u->isDimensionless();
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of mass. ie gram or
 * kilogram with only arbitrary variations in scale or multiplier
 * values, false otherwise.
 */
bool
UnitDefinition::isVariantOfMass (bool relaxed) const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (!relaxed)
  {
    // should be gram/kilogram ^ 1
    if (ud->getNumUnits() == 1)
    {
      const Unit* u = ud->getUnit(0);
      result        = ((u->isGram() || u->isKilogram())
                  && u->getExponent() == 1);
    }
  }
  else
  {
    // should be any combination of kilogram/gram
    unsigned int i = 0;
    result = true;
    while (result && i < ud->getNumUnits())
    {
      const Unit* u = ud->getUnit(i);
      result = (  u->isGram() || u->isKilogram());
      
      i++;
    }
  }

  delete ud;
  return result;
}


/*
 * @return @c true if this UnitDefinition is a variant of the built-in type
 * substance per time, false otherwise.
 */
bool
UnitDefinition::isVariantOfSubstancePerTime (bool relaxed) const
{
  bool result = false;

  // this unitDefinition times second^1 should be a variant
  // of substance
  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  Unit *u = new Unit(ud->getSBMLNamespaces());
  u->setKind(UNIT_KIND_SECOND);
  u->initDefaults();
  ud->addUnit(u);

  UnitDefinition::simplify(ud);

  result = ud->isVariantOfSubstance(relaxed);
  
  delete ud;
  delete u;
  return result;
}


/*
 * Adds a copy of the given Unit to this UnitDefinition.
 */
int
UnitDefinition::addUnit (const Unit* u)
{
  int returnValue = checkCompatibility(static_cast<const SBase *>(u));
  if (returnValue != LIBSBML_OPERATION_SUCCESS)
  {
    return returnValue;
  }
  else if (u == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (u->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != u->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != u->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(u)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mUnits.append(u);
  }
}


/*
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
Unit*
UnitDefinition::createUnit ()
{
  Unit* u = NULL;

  try
  {
    u = new Unit(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
    return NULL;
  }
  
  if (u != NULL) mUnits.appendAndOwn(u);

  return u;
}


/*
 * @return the list of Units for this UnitDefinition.
 */
const ListOfUnits*
UnitDefinition::getListOfUnits () const
{
  return &mUnits;
}


/*
 * @return the list of Units for this UnitDefinition.
 */
ListOfUnits*
UnitDefinition::getListOfUnits ()
{
  return &mUnits;
}


/*
 * @return the nth Unit of this UnitDefinition
 */
const Unit*
UnitDefinition::getUnit (unsigned int n) const
{
  return static_cast<const Unit*>( mUnits.get(n) );
}


/*
 * @return the nth Unit of this UnitDefinition
 */
Unit*
UnitDefinition::getUnit (unsigned int n)
{
  return static_cast<Unit*>( mUnits.get(n) );
}


/*
 * @return the number of Units in this UnitDefinition.
 */
unsigned int
UnitDefinition::getNumUnits () const
{
  return mUnits.size();
}


/**
 * Removes the nth Unit object from this UnitDefinition object and
 * returns a pointer to it.
 */
Unit* 
UnitDefinition::removeUnit (unsigned int n)
{
  return mUnits.remove(n);  
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
UnitDefinition::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mUnits.setSBMLDocument(d);
}


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
UnitDefinition::connectToChild()
{
  SBase::connectToChild();
  mUnits.connectToParent(this);
}

/**
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePackage function)
 */
void 
UnitDefinition::enablePackageInternal(const std::string& pkgURI, 
                                      const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mUnits.enablePackageInternal(pkgURI,pkgPrefix,flag);
}


void
UnitDefinition::updateSBMLNamespace(const std::string& pkg, unsigned int level,
  unsigned int version)
{
  SBase::updateSBMLNamespace(pkg, level, version);

  mUnits.updateSBMLNamespace(pkg, level, version);
}
/** @endcond */


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
UnitDefinition::getTypeCode () const
{
  return SBML_UNIT_DEFINITION;
}


/*
 * @return the name of this element ie "unitDefinition".
 */
const string&
UnitDefinition::getElementName () const
{
  static const string name = "unitDefinition";
  return name;

}

bool 
UnitDefinition::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for unitDefinition: id (name in L1) */

  if (!isSetId())
    allPresent = false;

  return allPresent;
}


bool 
UnitDefinition::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for unitDefinition: listOfUnits (L2 only) */

  if (getLevel() > 1 && getNumUnits() == 0)
    allPresent = false;

  return allPresent;
}


/* utility functions originally in Utils_UnitDefinition
 * declared as static
 */

void
UnitDefinition::simplify(UnitDefinition * ud)
{
  if (ud == NULL) return;

  ListOfUnits *  units = ud->getListOfUnits();
  Unit * unit;
  const char * unitKind;
  int cancelFlag = 0;
  bool dimensionlessPresent = false;

  
  for (unsigned int n = 0; n < ud->getNumUnits(); n++)
  {
    Unit* unit = ud->getUnit(n);
    if (unit->getKind() == UNIT_KIND_DIMENSIONLESS)
    {
      dimensionlessPresent = true;
    }
  }
  
  double dimMultfactor = 1.0;
  double dimMultfactorSaved = 1.0;

  
  /* if only one unit cannot be simplified any further */
  if (units->size() > 1)
  {
    if (dimensionlessPresent)
    {
      /* if contains a dimensionless unit and any others then 
        dimensionless is unecessary 
        unless it has a multiplier attached
        */
      unsigned int origNumUnits = units->size();
      for (unsigned int n = origNumUnits; n > 0; n--)
      {
        unit = (Unit *) units->get(n-1);
        Unit::removeScale(unit);

        if (!strcmp(UnitKind_toString(unit->getKind()), "dimensionless"))
        {
          dimMultfactor = pow(unit->getMultiplier(), unit->getExponent());
          if (util_isEqual(dimMultfactor, 1.0) == false)
          {
            cancelFlag = 1;
            dimMultfactorSaved = dimMultfactorSaved * dimMultfactor;
          }
          delete units->remove(n-1);
        }
      }
    }

    /* if it contains two units with same kind these must be combined */
    for (unsigned int n = 0; n < units->size(); n++)
    {
      unit = units->get(n);
      unitKind = UnitKind_toString(unit->getKind());

      /* find other occurences and merge */
      for (unsigned int i = n+1; i < units->size();)
      {
        if (!strcmp(UnitKind_toString((units->get(i))->getKind()),
          unitKind))
        {
          Unit::merge(unit, units->get(i));
          delete units->remove(i);
        }
        else
        {
          i++;
        }
      }
    }
  }

  /* may have cancelled units - in which case exponent will be 0 */
  // might need to propagate a multiplier though
  double newMultiplier = dimMultfactorSaved;
  unsigned int numUnits = units->size();
  for (unsigned int n = numUnits; n > 0; n--)
  {
    unit = units->get(n-1);
    if (unit->isUnitChecking())
    {
      if (unit->getExponentUnitChecking() == 0)
      {
        newMultiplier = newMultiplier * unit->getMultiplier();
        delete units->remove(n-1);
        cancelFlag = 1;
      }
    }
    else if (unit->getExponent() == 0)
    {
      newMultiplier = newMultiplier * unit->getMultiplier();
      delete units->remove(n-1);
      cancelFlag = 1;
    }
  }

  /* if all units have been cancelled need to add dimensionless */
  /* or indeed if one or more have been cancelled need to
   * propagate any remaining multiplier */
  if (cancelFlag == 1 || (dimensionlessPresent && units->size() == 0))
  {
    if (units->size() == 0)
    {
      Unit tmpunit(ud->getSBMLNamespaces());
      tmpunit.setKind(UNIT_KIND_DIMENSIONLESS);
      tmpunit.initDefaults();
      tmpunit.setMultiplier(newMultiplier);
      ud->addUnit(&tmpunit);
    }
    else if (util_isEqual(newMultiplier, 1.0) == false)
    {
      unit = units->get(0);
      unit->setMultiplier(unit->getMultiplier() * 
        pow(newMultiplier, 1.0/unit->getExponentAsDouble()));
    }
  }

}

/** @cond doxygenLibsbmlInternal */
int compareKinds(const void * u1, const void * u2)
{
  return (*(const int*)u1 - *(const int*)u2);
}
/** @endcond */

/* 
 * Orders the listOfUnits within the UnitDefinition alphabetically.
 *
 * @param ud the UnitDefinition object to be ordered.
 */
void 
UnitDefinition::reorder(UnitDefinition *ud)
{
  if (ud == NULL) return;

  ListOfUnits * units = ud->getListOfUnits();
  const Unit * unit;
  unsigned int numUnits = units->size();

  int *indexArray = new int[units->size()];
  int *initialIndexArray = new int[units->size()];

  std::vector<unsigned int> used;

  for (unsigned int n = 0; n < numUnits; n++)
  {
    unit = units->get(n);
    int value = (int)(unit->getKind());
    indexArray[n] = value;
    initialIndexArray[n] = value;
  }

  qsort(indexArray, numUnits, sizeof(int), compareKinds);
 
  /* append units in correct order */
  for (unsigned int n = 0; n < numUnits; n++)
  {
    for (unsigned int p = 0; p < numUnits; p++)
    {
      if (indexArray[n] == initialIndexArray[p]
          && used.end() == std::find(used.begin(), used.end(), p))
      {
        unit = units->get(p);
        units->append(unit);
        used.push_back(p);
        break;
      }
    }
  }

  /* remove originals */
  for (unsigned int n = 0; n < numUnits; n++)
  {
    delete units->remove(0);
  }

  delete [] indexArray;
  delete [] initialIndexArray;
}


/*
 * Returns a UnitDefinition object which is the argument UnitDefinition
 * converted to the SI units.
 *
 * @param ud the UnitDefinition object to convert to SI.
 *
 * @return a UnitDefinition object converted to SI units.
 */
UnitDefinition * 
UnitDefinition::convertToSI(const UnitDefinition * ud)
{
  if (ud == NULL) return NULL;

  UnitDefinition * newUd = new UnitDefinition(ud->getSBMLNamespaces());
  UnitDefinition * tempUd;
  Unit * tempUnit;

  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (unsigned int n = 0; n < ud->getNumUnits(); n++)
  {
    tempUd = Unit::convertToSI(ud->getUnit(n));
    for (unsigned int p = 0; p < tempUd->getNumUnits(); p++)
    {
      tempUnit = new Unit(ud->getSBMLNamespaces());
      tempUnit->setKind(tempUd->getUnit(p)->getKind());
      if (tempUd->getUnit(p)->isUnitChecking())
      {
        tempUnit->setExponentUnitChecking(tempUd->getUnit(p)->getExponentUnitChecking());
      }
      else
      {
        tempUnit->setExponent(tempUd->getUnit(p)->getExponent());
      }
      tempUnit->setScale(tempUd->getUnit(p)->getScale());
      tempUnit->setMultiplier(tempUd->getUnit(p)->getMultiplier());
      newUd->addUnit(tempUnit);
      delete tempUnit;
    }
    delete tempUd;
  }

  UnitDefinition::simplify(newUd);
  return newUd;
}


double
extractMultiplier(UnitDefinition * ud)
{
  double multiplier = 1.0;

  unsigned int i = 0;
  while(i < ud->getNumUnits())
  {
    Unit::removeScale(ud->getUnit(i));
    multiplier = multiplier * pow(ud->getUnit(i)->getMultiplier(), 
                                  ud->getUnit(i)->getExponentAsDouble());
    ud->getUnit(i)->setMultiplier(1.0);
    ud->getUnit(i)->setScale(0);
    i++;
  }
  return multiplier;
}


/* 
 * Predicate returning @c true if 
 * UnitDefinition objects are identical (all units are identical).
 *
 * @param ud1 the first UnitDefinition object to compare.
 * @param ud2 the second UnitDefinition object to compare.
 *
 * @return @c true if all the units of ud1 are identical
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areEquivalent();
 */
bool 
UnitDefinition::areIdentical(const UnitDefinition * ud1, 
                             const UnitDefinition * ud2)
{
  bool identical = false;

  bool A = (ud1 == NULL);
  bool B = (ud2 == NULL);

  /* if one or other is NULL no need to check
   */
  if ((A || B) && !(A && B))
  {
    return identical;
  }

  /* if both NULL no need to check */
  if (A && B)
  {
    identical = true;
    return identical;
  }

  /* must be same level/version/ namespace
   */
  if ( (ud1->getLevel() != ud2->getLevel()) ||
       (ud1->getVersion() != ud2->getVersion()))
  {
    return identical;
  }
  unsigned int n;

  /* need to order the unitDefinitions so must make copies
   * since the arguments are const
   */
  UnitDefinition * ud1Temp = new UnitDefinition(ud1->getSBMLNamespaces());
  UnitDefinition * ud2Temp = new UnitDefinition(ud2->getSBMLNamespaces());

  for ( n = 0; n < ud1->getNumUnits(); n++)
    ud1Temp->addUnit(ud1->getUnit(n));
  for ( n = 0; n < ud2->getNumUnits(); n++)
    ud2Temp->addUnit(ud2->getUnit(n));

  UnitDefinition::simplify(ud1Temp);
  UnitDefinition::simplify(ud2Temp);

  if (ud1Temp->getNumUnits() == ud2Temp->getNumUnits())
  {
    UnitDefinition::reorder(ud1Temp);
    UnitDefinition::reorder(ud2Temp);

    if (ud1Temp->getNumUnits() > 1)
    {
      // different multipliers left on different units may not match
      // but overall they 
      // e.g (2m)(sec) is tha same unit as (m)(2sec) but unit by unit
      // comparison will fail
      double multiplier1 = extractMultiplier(ud1Temp);
      double multiplier2 = extractMultiplier(ud2Temp);

      if (util_isEqual(multiplier1, multiplier2) == false)
      {
        return identical;
      }
    }
    
    n = 0;
    while (n < ud1Temp->getNumUnits())
    {
      if (!Unit::areIdentical(ud1Temp->getUnit(n), ud2Temp->getUnit(n)))
      {
        break;
      }
      else
      {
        n++;
      }
    }
    if (n == ud1Temp->getNumUnits())
    {
      identical = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return identical;
}


/* 
 * Predicate returning @c true if 
 * UnitDefinition objects are equivalent (all units are equivalent).
 *
 * @param ud1 the first UnitDefinition object to compare.
 * @param ud2 the second UnitDefinition object to compare.
 *
 * @return @c true if all the units of ud1 are equivalent
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areIdentical();
 */
bool 
UnitDefinition::areEquivalent(const UnitDefinition * ud1, const UnitDefinition * ud2)
{
  bool equivalent = false;

  bool A = (ud1 == NULL);
  bool B = (ud2 == NULL);

  /* if one or other is NULL no need to check
   */
  if ((A || B) && !(A && B))
  {
    return equivalent;
  }

  /* if both NULL no need to check */
  if (A && B)
  {
    equivalent = true;
    return equivalent;
  }

  unsigned int n;

  UnitDefinition * ud1Temp = UnitDefinition::convertToSI(ud1);
  UnitDefinition * ud2Temp = UnitDefinition::convertToSI(ud2);

  if (ud1Temp->getNumUnits() == ud2Temp->getNumUnits())
  {
    UnitDefinition::reorder(ud1Temp);
    UnitDefinition::reorder(ud2Temp);
    
    n = 0;
    while (n < ud1Temp->getNumUnits())
    {
      if (!Unit::areEquivalent(ud1Temp->getUnit(n), ud2Temp->getUnit(n)))
      {
        break;
      }
      else
      {
        n++;
      }
    }
    if (n == ud1Temp->getNumUnits())
    {
      equivalent = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return equivalent;
}

/** @cond doxygenLibsbmlInternal */
bool 
UnitDefinition::areIdenticalSIUnits(const UnitDefinition * ud1, 
                               const UnitDefinition * ud2)
{
  bool identical = false;

  bool A = (ud1 == NULL);
  bool B = (ud2 == NULL);

  /* if one or other is NULL no need to check
   */
  bool one_or_other_is_null = (A || B) && !(A && B);
  if (one_or_other_is_null)
  {
    return identical;
  }

  /* if both NULL no need to check */
  bool both_null = A && B;
  if (both_null)
  {
    identical = true;
    return identical;
  }

  unsigned int n;

  /* need to order the unitDefinitions so must make copies
   * since the arguments are const
   */
  UnitDefinition * ud1Temp = UnitDefinition::convertToSI(ud1);
  UnitDefinition * ud2Temp = UnitDefinition::convertToSI(ud2);

  if (ud1Temp->getNumUnits() == ud2Temp->getNumUnits())
  {
    UnitDefinition::reorder(ud1Temp);
    UnitDefinition::reorder(ud2Temp);
    
    if (ud1Temp->getNumUnits() > 1)
    {
      // different multipliers left on different units may not match
      // but overall they 
      // e.g (2m)(sec) is tha same unit as (m)(2sec) but unit by unit
      // comparison will fail
      double multiplier1 = extractMultiplier(ud1Temp);
      double multiplier2 = extractMultiplier(ud2Temp);

      if (util_isEqual(multiplier1, multiplier2) == false)
      {
        delete ud1Temp;
        delete ud2Temp;
        return identical;
      }
    }

    n = 0;
    while (n < ud1Temp->getNumUnits())
    {
      Unit* u1 = ud1Temp->getUnit(n);
      Unit* u2 = ud2Temp->getUnit(n);
      // if the unit is dimensionless it does not matter 
      // what numerical factors it has
      // but put this check here rather than in the unit areIdentical
      // so that Unit::areIdentical is trully a test for identical
      bool both_dimensionless = 
           u1->getKind() == UNIT_KIND_DIMENSIONLESS 
        && u2->getKind() == UNIT_KIND_DIMENSIONLESS;

      if ( both_dimensionless || Unit::areIdentical(u1, u2) )
      {
        n++;
      }
      else
      {
        break;
      }
    }
    if (n == ud1Temp->getNumUnits())
    {
      identical = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return identical;
}
/** @endcond */

/* 
 * Combines two UnitDefinition objects into a single UnitDefinition object
 * which expresses the units of the two objects multiplied.
 *
 * @param ud1 the first UnitDefinition object into which the second is
 * combined.
 * @param ud2 the second UnitDefinition object.
 */
UnitDefinition *
UnitDefinition::combine(const UnitDefinition *ud1, const UnitDefinition *ud2)
{
  bool A = (ud1 == NULL);
  bool B = (ud2 == NULL);

  UnitDefinition * ud;

  if (A && B)
  {
    ud = NULL;
  }
  else if (A && !B)
  {
    ud = new UnitDefinition(*ud2);
  }
  else if (B && !A)
  {
    ud = new UnitDefinition(*ud1);
  }
  else  if ( (ud1->getLevel() != ud2->getLevel()) ||
       (ud1->getVersion() != ud2->getVersion()))
  {
    ud = NULL;
  }
  else
  {
    ud = new UnitDefinition(*ud1);
    for (unsigned int n = 0; n < ud2->getNumUnits(); n++)
    {
      ud->addUnit(ud2->getUnit(n));
    }

    UnitDefinition::simplify(ud);
  }
  return ud;
}


UnitDefinition *
UnitDefinition::divide(const UnitDefinition *ud1, const UnitDefinition *ud2)
{
  bool A = (ud1 == NULL);
  bool B = (ud2 == NULL);

  UnitDefinition * ud = NULL;

  if (A && B)
  {
    ud = NULL;
  }
  else if (A && !B)
  {
    // no longer true
    ud = new UnitDefinition(*ud2);
    for (unsigned int i = 0; i < ud->getNumUnits(); i++)
    {
      ud->getUnit(i)->setExponent(ud->getUnit(i)->getExponent() * -1);
    }
  }
  else if (B && !A)
  {
    ud = new UnitDefinition(*ud1);
  }
  else  if ( (ud1->getLevel() != ud2->getLevel()) ||
       (ud1->getVersion() != ud2->getVersion()))
  {
    ud = NULL;
  }
  else
  {
    ud = new UnitDefinition(*ud1);
    for (unsigned int n = 0; n < ud2->getNumUnits(); n++)
    {
      Unit * u = new Unit(*(ud2->getUnit(n)));
      u->setExponent(u->getExponent() * -1);
      ud->addUnit(u);
      delete u;
    }

    UnitDefinition::simplify(ud);
  }
  return ud;
}

/* 
 * Returns a string that expresses the units symbolised by the UnitDefinition.
 * For example printUnits applied to
 * @code
 * <unitDefinition>
 *  <listOfUnits>
 *    <unit kind="metre" exponent="1"/>
 *    <unit kind="second" exponent="-2"/>
 *  </listOfUnits>
 * <unitDefinition>
 * @endcode
 * returns the string 'metre (exponent = 1) second (exponent = -2)'
 *
 * @param ud the UnitDefinition object.
 *
 * @return a string expressing the units.
 */
std::string
UnitDefinition::printUnits(const UnitDefinition * ud, bool compact)
{
  std::stringstream unitDef;
  if (ud == NULL || ud->getNumUnits() == 0)
  {
    unitDef << "indeterminable";
  }
  else
  {
    if (!compact)
    {
      for (unsigned int p = 0; p < ud->getNumUnits(); p++)
      {
        UnitKind_t kind = ud->getUnit(p)->getKind();
        double exp = 0;
        if (ud->getUnit(p)->isUnitChecking())
        {
          exp = ud->getUnit(p)->getExponentUnitChecking();
        }
        else
        {
          exp = ud->getUnit(p)->getExponentAsDouble();
        }
        int scale = ud->getUnit(p)->getScale();
        double mult = ud->getUnit(p)->getMultiplier();
        
        unitDef << UnitKind_toString(kind) << " (exponent = " << exp
            << ", multiplier = " << mult << ", scale = " << scale << ")";

        if (p + 1 < ud->getNumUnits())
        {
          unitDef << ", ";
        }	  
      }
    }
    else
    {
      for (unsigned int p = 0; p < ud->getNumUnits(); p++)
      {
        UnitKind_t kind = ud->getUnit(p)->getKind();
        double exp = ud->getUnit(p)->getExponentAsDouble();
        int scale = ud->getUnit(p)->getScale();
        double mult = ud->getUnit(p)->getMultiplier();
        mult = mult * pow(10.0, scale);

        unitDef << "(" << mult << " " << UnitKind_toString(kind) << ")^" << exp;

        if (p + 1 < ud->getNumUnits())
        {
          unitDef << ", ";
        }	  
      }
    }
  }
  return unitDef.str();
}
/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this UnitDefinition's attribute
 * "attributeName" is set.
 */
bool
UnitDefinition::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this UnitDefinition.
 */
int
UnitDefinition::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  return value;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns an new "elementName" object in this UnitDefinition.
 */
SBase*
UnitDefinition::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "unit")
  {
    return createUnit();
  }

  return obj;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds an new "elementName" object in this UnitDefinition.
 */
int
UnitDefinition::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "unit" && element->getTypeCode() == SBML_UNIT)
  {
    return addUnit((const Unit*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this UnitDefinition.
 */
unsigned int
UnitDefinition::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "unit")
  {
    return getNumUnits();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this UnitDefinition.
 */
SBase*
UnitDefinition::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "unit")
  {
    return getUnit(index);
  }

  return obj;
}

/** @endcond */



/* @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or @c NULL if the token was not recognized.
 */
SBase*
UnitDefinition::createObject (XMLInputStream& stream)
{
  SBase* object = 0;

  const string& name = stream.peek().getName();
  if (name == "listOfUnits")
  {
    if (mUnits.size() != 0)
    {
      if (getLevel() < 3)
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
         "Only one <listOfUnits> elements is permitted in a "
         "given <unitDefinition>.");
      }
      else
      {
        logError(OneListOfUnitsPerUnitDef, getLevel(), getVersion());
      }
    }
    mUnits.setExplicitlyListed();
    object = &mUnits;
  }
  
  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
UnitDefinition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  const unsigned int level   = getLevel  ();

  attributes.add("name");

  if (level > 1)
  {
    attributes.add("id");
  }
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
UnitDefinition::readAttributes (const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  const unsigned int level   = getLevel  ();

  SBase::readAttributes(attributes, expectedAttributes);

  switch (level)
  {
  case 1:
    readL1Attributes(attributes);
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  default:
    readL3Attributes(attributes);
    break;
  }
}
/** @endcond */

 
/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
UnitDefinition::readL1Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  bool assigned;
  assigned = attributes.readInto("name", mId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("name", level, version, "<unitDefinition>");
  }
  if (!SyntaxChecker::isValidInternalSId(mId)) 
    logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");
}
/** @endcond */

 
/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
UnitDefinition::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  bool assigned;
  assigned = attributes.readInto("id", mId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<unitDefinition>");
  }
  if (!SyntaxChecker::isValidInternalSId(mId)) 
    logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");

  //
  // name: string  { use="optional" }  (L2v1->)
  //
  attributes.readInto("name", mName, getErrorLog(), false, getLine(), getColumn());
}
/** @endcond */

 
/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
UnitDefinition::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  bool assigned;
  // for l3v2 sbase will read this as generically optional
  // we want to log errors relating to the specific object
  if (version == 1)
  {
    assigned = attributes.readInto("id", mId, getErrorLog(), false, getLine(), getColumn());
    if (!assigned)
    {
      logError(AllowedAttributesOnUnitDefinition, level, version, 
               "The required attribute 'id' is missing.");
    }
    if (assigned && mId.size() == 0)
    {
      logEmptyString("id", level, version, "<unitDefinition>");
    }
    if (!SyntaxChecker::isValidInternalSId(mId)) 
      logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");
  }
  else
  {
    // need to check that id was present
    // it has already been read and checked for syntax/emptyness
    if (attributes.hasAttribute("id") == false)
    {
      logError(AllowedAttributesOnUnitDefinition, level, version, 
        "The required attribute 'id' is missing.");
    }
  }

  //
  // name: string  { use="optional" }  (L2v1->)
  //
  // for l3v2 sbase will read this
  if (version == 1)
  {
    attributes.readInto("name", mName, getErrorLog(), false, 
                                       getLine(), getColumn());
  }
}
/** @endcond */

 
/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parent's implementation
 * of this method as well.
 */
void
UnitDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  unsigned int version = getVersion();

  // for L3V2 and above SBase will write this out
  if (level < 3 || (level == 3 && version == 1))
  {
    //
    // name: SName   { use="required" }  (L1v1, L1v2)
    //   id: SId     { use="required" }  (L2v1, L2v2)
    //
    const string id = (level == 1) ? "name" : "id";
    stream.writeAttribute(id, mId);
  }
  
  if (level > 1)
  {
    // for L3V2 and above SBase will write this out
    if (level < 3 || (level == 3 && version == 1))
    {
      //
      // name: string  { use="optional" }  (L2v1->)
      //
      stream.writeAttribute("name", mName);
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  // is written in SBase::writeAttributes()
  //

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parent's
 * implementation of this method as well.
 */
void
UnitDefinition::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getLevel() == 3 && getVersion() > 1)
  {
    if (mUnits.hasOptionalElements() == true ||
        mUnits.hasOptionalAttributes() == true ||
        mUnits.isExplicitlyListed())
    {
      mUnits.write(stream);
    }
  }
  else
  {
    // use original code
    if ( getNumUnits() > 0 ) mUnits.write(stream);
  }

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}
/** @endcond */


/*
 * Creates a new ListOfUnitDefinitions items.
 */
ListOfUnitDefinitions::ListOfUnitDefinitions (unsigned int level, unsigned int version)
: ListOf(level,version)
{
}


/*
 * Creates a new ListOfUnitDefinitions items.
 */
ListOfUnitDefinitions::ListOfUnitDefinitions (SBMLNamespaces* sbmlns)
 : ListOf(sbmlns)
{
  loadPlugins(sbmlns);
}


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfUnitDefinitions*
ListOfUnitDefinitions::clone () const
{
  return new ListOfUnitDefinitions(*this);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfUnitDefinitions::getItemTypeCode () const
{
  return SBML_UNIT_DEFINITION;
}


/*
 * @return the name of this element ie "listOfUnitDefinitions".
 */
const string&
ListOfUnitDefinitions::getElementName () const
{
  static const string name = "listOfUnitDefinitions";
  return name;
}


/* return nth item in list */
UnitDefinition *
ListOfUnitDefinitions::get(unsigned int n)
{
  return static_cast<UnitDefinition*>(ListOf::get(n));
}


/* return nth item in list */
const UnitDefinition *
ListOfUnitDefinitions::get(unsigned int n) const
{
  return static_cast<const UnitDefinition*>(ListOf::get(n));
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqUD
{
  const string& mId;

  explicit IdEqUD (const string& id) : mId(id) { }
  bool operator() (SBase* sb) const
       { return static_cast <UnitDefinition *> (sb)->getId() == mId; }
};


/* return item by id */
UnitDefinition*
ListOfUnitDefinitions::get (const std::string& sid)
{
  return const_cast<UnitDefinition*>( 
    static_cast<const ListOfUnitDefinitions*>(this)->get(sid) );
}


/* return item by id */
const UnitDefinition*
ListOfUnitDefinitions::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqUD(sid) );
  return (result == mItems.end()) ? NULL : 
                   static_cast <UnitDefinition*> (*result);
}


SBase*
ListOfUnitDefinitions::getElementBySId(const std::string& id)
{
  for (unsigned int i = 0; i < size(); i++)
  {
    SBase* obj = get(i);
    //Units are not in the SId namespace, so don't check 'getId'.  However, their children (through plugins) may have the element we are looking for, so we still need to check all of them.
    obj = obj->getElementBySId(id);
    if (obj != NULL) return obj;
  }

  return getElementFromPluginsBySId(id);
}
  
/* Removes the nth item from this list */
UnitDefinition*
ListOfUnitDefinitions::remove (unsigned int n)
{
   return static_cast<UnitDefinition*>(ListOf::remove(n));
}


/* Removes item in this list by id */
UnitDefinition*
ListOfUnitDefinitions::remove (const std::string& sid)
{
  SBase* item = NULL;
  ListItemIter result;

  result = find_if( mItems.begin(), mItems.end(), IdEqUD(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <UnitDefinition*> (item);
}


/** @cond doxygenLibsbmlInternal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfUnitDefinitions::getElementPosition () const
{
  return 2;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or @c NULL if the token was not recognized.
 */
SBase*
ListOfUnitDefinitions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "unitDefinition")
  {
    try
    {
      object = new UnitDefinition(getSBMLNamespaces());
    }
    catch (SBMLConstructorException)
    {
      object = new UnitDefinition(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }

    if (object != NULL) mItems.push_back(object);
  }

  return object;
}
/** @endcond */


#endif /* __cplusplus */
/** @cond doxygenIgnored */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (unsigned int level, unsigned int version)
{
  try
  {
    UnitDefinition* obj = new UnitDefinition(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    UnitDefinition* obj = new UnitDefinition(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  delete ud;
}


LIBSBML_EXTERN
UnitDefinition_t*
UnitDefinition_clone (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->clone() : NULL;
}


LIBSBML_EXTERN
const XMLNamespaces_t *
UnitDefinition_getNamespaces(const UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->getNamespaces() : NULL;
}

LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  return (ud != NULL && ud->isSetId()) ? ud->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  return (ud != NULL && ud->isSetName()) ? ud->getName().c_str() : NULL;
}


LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isSetId() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isSetName() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfArea (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfArea() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfLength (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfLength() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstance (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfSubstance() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfTime (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfTime() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfVolume (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfVolume() ) : 0;
}


LIBSBML_EXTERN
int 
UnitDefinition_isVariantOfDimensionless (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfDimensionless() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfMass (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? static_cast<int>( ud->isVariantOfMass() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstancePerTime (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? 
    static_cast<int>( ud->isVariantOfSubstancePerTime() ) : 0;
}


LIBSBML_EXTERN
int
UnitDefinition_setId (UnitDefinition_t *ud, const char *id)
{
  if (ud != NULL)
    return (id == NULL) ? ud->setId("") : ud->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  if (ud != NULL)
    return (name == NULL) ? ud->unsetName() : ud->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u)
{
  return (ud != NULL) ? ud->addUnit(u) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
Unit_t *
UnitDefinition_createUnit (UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->createUnit() : NULL;
}


LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->getListOfUnits() : NULL;
}


LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (UnitDefinition_t *ud, unsigned int n)
{
  return (ud != NULL) ? ud->getUnit(n) : NULL;
}


LIBSBML_EXTERN
Unit_t *
UnitDefinition_removeUnit (UnitDefinition_t *ud, unsigned int n)
{
  if (ud != NULL) 
    return ud->removeUnit(n);
  else
    return NULL;
}


LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return (ud != NULL) ? ud->getNumUnits() : SBML_INT_MAX;
}


LIBSBML_EXTERN
void 
UnitDefinition_simplify(UnitDefinition_t * ud)
{
  if (ud != NULL) 
    UnitDefinition::simplify(ud);
}

LIBSBML_EXTERN
void 
UnitDefinition_reorder(UnitDefinition_t * ud)
{
  if (ud != NULL) 
    UnitDefinition::reorder(ud);
}

LIBSBML_EXTERN
UnitDefinition_t * 
UnitDefinition_convertToSI(UnitDefinition_t * ud)
{
  return (ud != NULL) ? 
    UnitDefinition::convertToSI(ud) : NULL;
}

LIBSBML_EXTERN
int 
UnitDefinition_areIdentical(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  if (ud1 != NULL && ud2 != NULL)
    return static_cast<int>
         (UnitDefinition::areIdentical(ud1, ud2));
  else
    return 0;
}

LIBSBML_EXTERN
int 
UnitDefinition_areEquivalent(UnitDefinition_t *ud1 , UnitDefinition_t * ud2)
{
  if (ud1 != NULL && ud2 != NULL)
    return static_cast<int>(
         UnitDefinition::areEquivalent(ud1, ud2));
  else
    return 0;
}

LIBSBML_EXTERN
UnitDefinition_t *  
UnitDefinition_combine(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  return UnitDefinition::combine(ud1, ud2);
}


LIBSBML_EXTERN
UnitDefinition_t *  
UnitDefinition_divide(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  return UnitDefinition::divide(ud1, ud2);
}


LIBSBML_EXTERN
char *
UnitDefinition_printUnits(UnitDefinition_t * ud, int compact)
{
  return (ud != NULL) ? 
    safe_strdup(UnitDefinition::printUnits(ud, compact).c_str()) : NULL;
}

LIBSBML_EXTERN
UnitDefinition_t *
ListOfUnitDefinitions_getById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfUnitDefinitions *> (lo)->get(sid) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
UnitDefinition_t *
ListOfUnitDefinitions_removeById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfUnitDefinitions *> (lo)->remove(sid) : NULL;
  else
    return NULL;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

