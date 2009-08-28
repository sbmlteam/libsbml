/**
 * @file    UnitDefinition.cpp
 * @brief   Implementations of %SBML's UnitDefinition and ListOfUnitDefinitions.
 * @author  Ben Bornstein
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

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

LIBSBML_CPP_NAMESPACE_BEGIN

UnitDefinition::UnitDefinition (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mId               ( ""       )
 , mName             ( ""       )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


UnitDefinition::UnitDefinition (SBMLNamespaces * sbmlns) :
   SBase ( sbmlns )
 , mId               ( ""       )
 , mName             ( ""       )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
UnitDefinition::UnitDefinition() :
  SBase()
{
}

/** @endcond doxygen-libsbml-internal */
                          

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
    SBase     ( orig                    )
  , mId       ( orig.mId                )  
  , mName     ( orig.mName              )
  , mUnits    ( orig.mUnits             )
{
  /* since a unit definition has children we need to re-establish the
   * parentage of these children
   */
  if (orig.getNumUnits() > 0)
  {
    mUnits.setParentSBMLObject(this);
  }
}


/*
 * Assignment operator.
 */
UnitDefinition& UnitDefinition::operator=(const UnitDefinition& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mId = rhs.mId;
    mName = rhs.mName;
    mUnits = rhs.mUnits;
    if (rhs.getNumUnits() > 0)
    {
      mUnits.setParentSBMLObject(this);
    }
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * UnitDefinition (if available).
 */
bool
UnitDefinition::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);
  mUnits.accept(v);

  return result;
}


/*
 * @return a (deep) copy of this UnitDefinition.
 */
UnitDefinition*
UnitDefinition::clone () const
{
  return new UnitDefinition(*this);
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
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
UnitDefinition::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
UnitDefinition::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * Sets the id of this SBML object to a copy of sid.
 */
int
UnitDefinition::setId (const std::string& sid)
{
  /* since the setId function has been used as an
   * alias for setName we cant require it to only
   * be used on a L2 model
   */
/*  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
*/
  if (!(SyntaxChecker::isValidSBMLSId(sid)))
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
    if (!(SyntaxChecker::isValidSBMLSId(name)))
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
 * @return true if this UnitDefinition is a variant of the built-in type
 * area. i.e. square metres with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfArea () const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (ud->getNumUnits() == 1)
  {
    const Unit* u = ud->getUnit(0);
    result        = u->isMetre() && u->getExponent() == 2;
  }

  delete ud;
  return result;
}


/*
 * @return true if this UnitDefinition is a variant of the built-in type
 * length. i.e. metres with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfLength () const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (ud->getNumUnits() == 1)
  {
    const Unit* u = ud->getUnit(0);
    result        = u->isMetre() && u->getExponent() == 1;
  }

  delete ud;
  return result;
}


/*
 * @return true if this UnitDefinition is a variant of the built-in type
 * substance. i.e. moles or items with only abritrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfSubstance () const
{
  bool result = false;

  unsigned int level = getLevel();
  unsigned int version = getVersion();

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (ud->getNumUnits() == 1)
  {
    const Unit* u = ud->getUnit(0);
    if (level == 2 && version > 1)
    {
      result = ((  u->isMole() || u->isItem() 
                || u->isGram() || u->isKilogram())
                && u->getExponent() == 1);
    }
    else
    {
      result        = (u->isMole() || u->isItem()) 
                    && u->getExponent() == 1;
    }
  }

  delete ud;
  return result;
}


/*
 * @return true if this UnitDefinition is a variant of the built-in type
 * time. i.e. seconds with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfTime () const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (ud->getNumUnits() == 1)
  {
    const Unit* u = ud->getUnit(0);
    result        = u->isSecond() && u->getExponent() == 1;
  }

  delete ud;
  return result;
}


/*
 * @return true if this UnitDefinition is a variant of the built-in type
 * volume. i.e. litre or cubic metre with only abritrary variations in
 * scale or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfVolume () const
{
  bool result = false;

  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  UnitDefinition::simplify(ud);

  if (ud->getNumUnits() == 1)
  {
    const Unit* u = ud->getUnit(0);
    result        = (u->isLitre() && u->getExponent() == 1) ||
                    (u->isMetre() && u->getExponent() == 3);
  }

  delete ud;
  return result;
}


/*
 * @return true if this UnitDefinition is a variant of dimensionless.
 * i.e. dimensionless with only abritrary variations in scale,
 * or multiplier values, false otherwise.
 */
bool
UnitDefinition::isVariantOfDimensionless () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = u->isDimensionless();
  }

  return result;
}


/*
 * @return true if this UnitDefinition is a variant of mass. ie gram or
 * kilogram with only abritrary variations in scale or multiplier
 * values, false otherwise.
 */
bool
UnitDefinition::isVariantOfMass () const
{
  bool result = false;


  if (getNumUnits() == 1)
  {
    const Unit* u = getUnit(0);
    result        = (u->isGram() || u->isKilogram());
  }

  return result;
}


/*
 * @return true if this UnitDefinition is a variant of the built-in type
 * substance per time, false otherwise.
 */
bool
UnitDefinition::isVariantOfSubstancePerTime () const
{
  bool result = false;

  // this unitDefinition times second^1 should be a variant
  // of substance
  UnitDefinition *ud = static_cast<UnitDefinition*>(this->clone());
  //Unit *u = new Unit(UNIT_KIND_SECOND);
  Unit *u = new Unit(ud->getSBMLNamespaces());
  u->setKind(UNIT_KIND_SECOND);
  ud->addUnit(u);

  UnitDefinition::simplify(ud);

  result = ud->isVariantOfSubstance();

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
  if (u == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(u->hasRequiredAttributes()) || !(u->hasRequiredElements()))
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
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mUnits.size() == 0)
    {
      mUnits.setSBMLDocument(this->getSBMLDocument());
      mUnits.setParentSBMLObject(this);
    }
    
    mUnits.append(u);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
Unit*
UnitDefinition::createUnit ()
{
  Unit* u = 0;

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
  }
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mUnits.size() == 0)
  {
    mUnits.setSBMLDocument(this->getSBMLDocument());
    mUnits.setParentSBMLObject(this);
  }
  
  if (u) mUnits.appendAndOwn(u);

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


/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
UnitDefinition::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mUnits.setSBMLDocument(d);
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
UnitDefinition::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
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

  unsigned int n, i;
  ListOfUnits *  units = ud->getListOfUnits();
  Unit * unit;
  UnitKindList kindsList;
  const char * unitKind;
  int cancelFlag = 0;

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    kindsList.append(UnitKind_toString(ud->getUnit(n)->getKind()));
  }
  
  /* if only one unit cannot be simplified any further */
  if (units->size() > 1)
  {
    if (kindsList.contains("dimensionless"))
    {
      /* if contains a dimensionless unit and any others then 
        dimensionless is unecessary */
      for (n = 0; n < units->size(); n++)
      {
        unit = (Unit *) units->get(n);
        if (!strcmp(UnitKind_toString(unit->getKind()), "dimensionless"))
        {
          delete units->remove(n);
          kindsList.removeUnitKind("dimensionless");
        }
      }
    }

    /* if it contains two units with same kind these must be combined */
    for (n = 0; n < units->size(); n++)
    {
      unit = (Unit *) units->get(n);
      unitKind = UnitKind_toString(unit->getKind());

      /* check that there is only one occurence */
      kindsList.removeUnitKind(unitKind);
      while (kindsList.contains(unitKind)) 
      {
        /* find next occurence and merge */
        for (i = n + 1; i < units->size(); i++)
        {
          if (!strcmp(UnitKind_toString(((Unit *) units->get(i))->getKind()), 
                                                                   unitKind))
          {
            Unit::merge(unit, (Unit *) units->get(i));
            delete units->remove(i);
            kindsList.removeUnitKind(unitKind);
          }
        }
      }
    }
  }

  /* may have cancelled units - in which case exponent will be 0 */
  unsigned int numUnits = units->size();
  for (n = numUnits; n > 0; n--)
  {
    unit = (Unit *) units->get(n-1);
    if (unit->getExponent() == 0)
    {
      delete units->remove(n-1);
      cancelFlag = 1;
    }
  }

  /* if all units have been cancelled need to add dimensionless */
  if (units->size() == 0 && cancelFlag == 1)
  {
    Unit tmpunit(ud->getSBMLNamespaces());
    tmpunit.setKind(UNIT_KIND_DIMENSIONLESS);
    ud->addUnit(&tmpunit);
  }
}

/** @cond doxygen-libsbml-internal */
int compareKinds(const void * u1, const void * u2)
{
  return (*(int*)u1 - *(int*)u2);
}
/** @endcond doxygen-libsbml-internal */

/* 
 * Orders the listOfUnits within the UnitDefinition alphabetically.
 *
 * @param ud the UnitDefinition object to be ordered.
 */
void 
UnitDefinition::reorder(UnitDefinition *ud)
{
  if (ud == NULL) return;

  unsigned int n, p;
  ListOfUnits * units = ud->getListOfUnits();
  Unit * unit;
  unsigned int numUnits = units->size();

  int *indexArray = NULL;
  indexArray = new int[units->size()];

  int *initialIndexArray = NULL;
  initialIndexArray = new int[units->size()];

  for (n = 0; n < numUnits; n++)
  {
    indexArray[n] = ((Unit *)units->get(n))->getKind();
    initialIndexArray[n] = ((Unit *)units->get(n))->getKind();
  }

  qsort(indexArray, numUnits, sizeof(int), compareKinds);
 
  /* append units in correct order */
  for (n = 0; n < numUnits; n++)
  {
    for (p = 0; p < numUnits; p++)
    {
      if (indexArray[n] == initialIndexArray[p])
      {
        unit = (Unit *) units->get(p);
        units->append(unit);
        break;
      }
    }
  }

  /* remove originals */
  for (n = 0; n < numUnits; n++)
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
 * @param ud the UnitDefinition object to convert to SI
 *
 * @return a UnitDefinition object converted to SI units.
 */
UnitDefinition * 
UnitDefinition::convertToSI(const UnitDefinition * ud)
{
  if (ud == NULL) return NULL;

  unsigned int n, p;
  UnitDefinition * newUd = new UnitDefinition(ud->getSBMLNamespaces());
  UnitDefinition * tempUd;
  Unit * tempUnit;

  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    tempUd = Unit::convertToSI(ud->getUnit(n));
    for (p = 0; p < tempUd->getNumUnits(); p++)
    {
      tempUnit = new Unit(ud->getSBMLNamespaces());
      tempUnit->setKind(tempUd->getUnit(p)->getKind());
      tempUnit->setExponent(tempUd->getUnit(p)->getExponent());
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


/* 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are identical (all units are identical).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
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
UnitDefinition::areIdentical(const UnitDefinition * ud1, const UnitDefinition * ud2)
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


  if (ud1->getNumUnits() == ud2->getNumUnits())
  {
    UnitDefinition::reorder(ud1Temp);
    UnitDefinition::reorder(ud2Temp);
    
    n = 0;
    while (n < ud1->getNumUnits())
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
    if (n == ud1->getNumUnits())
    {
      identical = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return identical;
}


/* 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are equivalent (all units are equivalent).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
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

/** @cond doxygen-libsbml-internal */

bool 
UnitDefinition::areIdenticalSIUnits(const UnitDefinition * ud1, 
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

/** @endcond doxygen-libsbml-internal */

/* 
 * Combines two UnitDefinition objects into a single UnitDefinition object
 * which expresses the units of the two objects multiplied.
 *
 * @param ud1 the first UnitDefinition object into which the second is
 * combined
 * @param ud2 the second UnitDefinition object
 */
UnitDefinition *
UnitDefinition::combine(UnitDefinition *ud1, UnitDefinition *ud2)
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
 * @param ud the UnitDefinition object
 *
 * @return a string expressing the units
 */
std::string
UnitDefinition::printUnits(const UnitDefinition * ud, bool compact)
{
  std::string unitDef;
  if (!ud || ud->getNumUnits() == 0)
  {
    unitDef = "indeterminable";
  }
  else
  {
    if (!compact)
    {
      for (unsigned int p = 0; p < ud->getNumUnits(); p++)
      {
	      UnitKind_t kind = ud->getUnit(p)->getKind();
	      int exp = ud->getUnit(p)->getExponent();
        int scale = ud->getUnit(p)->getScale();
        double mult = ud->getUnit(p)->getMultiplier();

        char unit[80];
        sprintf(unit, "%s (exponent = %i, multiplier = %.6g, scale = %i)", 
          UnitKind_toString(kind), exp, mult, scale);
        unitDef += unit;

	      if (p + 1 < ud->getNumUnits())
	      {
	        unitDef += ", ";
	      }	  
      }
    }
    else
    {
      for (unsigned int p = 0; p < ud->getNumUnits(); p++)
      {
	      UnitKind_t kind = ud->getUnit(p)->getKind();
	      int exp = ud->getUnit(p)->getExponent();
        int scale = ud->getUnit(p)->getScale();
        double mult = ud->getUnit(p)->getMultiplier();
        mult = mult * pow(10.0, scale);

        char unit[40];
        sprintf(unit, "(%.6g %s)^%i", mult,  
          UnitKind_toString(kind), exp);
        unitDef += unit;

	      if (p + 1 < ud->getNumUnits())
	      {
	        unitDef += ", ";
	      }	  
      }
    }
  }
  return unitDef;
}

/* @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
UnitDefinition::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  if (name == "listOfUnits")
  {
    if (mUnits.size() != 0)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfUnits> elements is permitted in a "
	       "given <unitDefinition>.");
    }
    return &mUnits;
  }
  
  return 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
UnitDefinition::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");

  if (level > 1)
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("id");
    if (!(level == 2 && version < 3))
    {
      expectedAttributes.push_back("sboTerm");
    }
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<unitDefinition>");
    }
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  bool assigned = attributes.readInto(id, mId, getErrorLog(), true);
  if (assigned && mId.size() == 0)
  {
    logEmptyString(id, level, version, "<unitDefinition>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // name: string  { use="optional" }  (L2v1->)
  //
  if (level > 1) attributes.readInto("name", mName);
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  //
  if (!(level == 2 && version < 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}
/** @endcond doxygen-libsbml-internal */

 
/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
UnitDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  stream.writeAttribute(id, mId);

  //
  // name: string  { use="optional" }  (L2v1->)
  //
  if (level > 1) stream.writeAttribute("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3->)
  //
  if (!(level == 2 && version < 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
UnitDefinition::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if ( getNumUnits() > 0 ) mUnits.write(stream);
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfUnitDefinitions*
ListOfUnitDefinitions::clone () const
{
  return new ListOfUnitDefinitions(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
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
struct IdEqUD : public unary_function<SBase*, bool>
{
  const string& id;

  IdEqUD (const string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <UnitDefinition *> (sb)->getId() == id; }
};


/* return item by id */
UnitDefinition*
ListOfUnitDefinitions::get (const std::string& sid)
{
  return const_cast<UnitDefinition*>( 
    static_cast<const ListOfUnitDefinitions&>(*this).get(sid) );
}


/* return item by id */
const UnitDefinition*
ListOfUnitDefinitions::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqUD(sid) );
  return (result == mItems.end()) ? 0 : static_cast <UnitDefinition*> (*result);
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
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqUD(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <UnitDefinition*> (item);
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfUnitDefinitions::getElementPosition () const
{
  return 2;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfUnitDefinitions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "unitDefinition")
  {
    try
    {
      object = new UnitDefinition(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new UnitDefinition(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new UnitDefinition(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new UnitDefinition_t structure using the given SBML @p level 
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * UnitDefinition
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UnitDefinition
 * 
 * @return a pointer to the newly created UnitDefinition_t structure.
 *
 * @note Once a UnitDefinition has been added to an SBMLDocument, the @p 
 * level and @p version for the document @em override those used to create 
 * the UnitDefinition.  Despite this, the ability to supply the values at 
 * creation time is an important aid to creating valid SBML.  Knowledge of 
 * the intended SBML Level and Version  determine whether it is valid to 
 * assign a particular value to an attribute, or whether it is valid to add 
 * an object to an existing SBMLDocument.
 */
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


/**
 * Creates a new UnitDefinition_t structure using the given 
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this UnitDefinition
 *
 * @return a pointer to the newly created UnitDefinition_t structure.
 *
 * @note Once a UnitDefinition has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the UnitDefinition.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version determine whether it is valid to assign
 * a particular value to an attribute, or whether it is valid to add an object
 * to an existing SBMLDocument.
 */
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


/**
 * Frees the given UnitDefinition_t.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  delete ud;
}


/**
 * Creates and returns a deep copy of the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure to copy
 * 
 * @return a (deep) copy of UnitDefinition_t.
 */
LIBSBML_EXTERN
UnitDefinition_t*
UnitDefinition_clone (const UnitDefinition_t *ud)
{
  return static_cast<UnitDefinition_t*>( ud->clone() );
}


/**
 * Returns a list of XMLNamespaces_t associated with this UnitDefinition_t
 * structure.
 *
 * @param ud the UnitDefinition_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
UnitDefinition_getNamespaces(UnitDefinition_t *ud)
{
  return ud->getNamespaces();
}

/**
 * Returns the identifier of this UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose identifier is sought
 * 
 * @return the value of the "id" attribute of this UnitDefinition_t.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  return ud->isSetId() ? ud->getId().c_str() : NULL;
}


/**
 * Returns the name of this UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose name is sought
 * 
 * @return the value of the "name" attribute of this UnitDefinition_t.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  return ud->isSetName() ? ud->getName().c_str() : NULL;
}


/**
 * Predicate to test whether the "id" attribute of the given UnitDefinition_t
 * @p ud has been set.
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return nonzero (true) if the "id" attribute of the given
 * UnitDefinition_t has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetId() );
}


/**
 * Predicate to test whether the "name" attribute of the given
 * UnitDefinition_t @p ud has been set.
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return nonzero (true) if the "name" attribute of the given
 * UnitDefinition_t has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isSetName() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "area".
 * 
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if the UnitDefinition_t structure @p ud is a variant of
 * the built-in unit @c area, meaning square metres with only abritrary
 * variations in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfArea (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfArea() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "length".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition_t is a variant of the built-in
 * unit @c length, meaning metres with only abritrary variations in scale
 * or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfLength (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfLength() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "substance".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit substance, meaning moles or items with only abritrary variations
 * in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstance (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfSubstance() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "time".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit time, meaning seconds with only abritrary variations in scale or
 * multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfTime (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfTime() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "volume".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit volume, meaning litre or cubic metre with only abritrary
 * variations in scale or multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfVolume (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfVolume() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the unit @c "dimensionless".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of dimensionless,
 * meaning dimensionless with only abritrary variations in scale or
 * multiplier values; @c false otherwise.
 */
LIBSBML_EXTERN
int 
UnitDefinition_isVariantOfDimensionless (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfDimensionless() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "mass".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of mass, meaning
 * gram or kilogram with only abritrary variations in scale or multiplier
 * values; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfMass (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfMass() );
}


/**
 * Convenience function for testing if a given unit definition is a
 * variant of the built-in unit @c "substance" divided by the built-in
 * unit @c "time".
 *
 * @param ud the UnitDefinition_t to query.
 *
 * @return @c true if this UnitDefinition is a variant of the built-in
 * unit substance per built-in unit time, meaning it contains two units
 * one of which is a variant of substance and the other is a variant of
 * time which an exponent of -1; @c false otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstancePerTime (const UnitDefinition_t *ud)
{
  return static_cast<int>( ud->isVariantOfSubstancePerTime() );
}


/**
 * Sets the attribute "id" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose id is to be set
 * @param id a string, the new identifier for the UnitDefinition_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "id" attribute.
 */
LIBSBML_EXTERN
int
UnitDefinition_setId (UnitDefinition_t *ud, const char *id)
{
  return (id == NULL) ? ud->setId("") : ud->setId(id);
}


/**
 * Sets the attribute "name" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose name is to be set
 * @param name a string, the new name for the UnitDefinition_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "name" attribute.
 */
LIBSBML_EXTERN
int
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  return (name == NULL) ? ud->unsetName() : ud->setName(name);
}


/**
 * Unsets the name of the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose name is to be unset.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  return ud->unsetName();
}


/**
 * Adds a copy of the given Unit to the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure.
 * @param u the Unit instance to add.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_DUPLICATE_OBJECT_ID
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u)
{
  return ud->addUnit(u);
}


/**
 * Creates a new and empty Unit_t structure, adds it to the given
 * UnitDefinition_t structure's list of units, and returns the Unit_t
 * structure.
 *
 * @return a newly constructed (and empty) Unit_t structure.
 * 
 * @note It is worth emphasizing that the attribute "kind" value of a
 * Unit_t is a required attribute for a valid Unit_t definition.  The
 * UnitDefinition_createUnit() method does not assign a valid kind to the
 * constructed unit (instead, it sets the "kind" to UNIT_KIND_INVALID).
 * Callers are cautioned to set the newly-constructed Unit's kind using
 * UnitDefinition_setKind() soon after calling this method.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_createUnit (UnitDefinition_t *ud)
{
  return ud->createUnit();
}


/**
 * Returns the list of Units for the given UnitDefinition_t structure.
 * 
 * @param ud the UnitDefinition_t to use
 *
 * @return the ListOfUnits value for the given UnitDefinition_t.
 */
LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud)
{
  return ud->getListOfUnits();
}


/**
 * Returns a specific Unit_t instance belonging to the given
 * UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure in question
 * @param n an integer, the index of the Unit_t structure to be returned.
 * 
 * @return the nth Unit_t of this UnitDefinition_t structure.
 *
 * @see UnitDefinition_getNumUnits()
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (UnitDefinition_t *ud, unsigned int n)
{
  return ud->getUnit(n);
}


/**
 * Returns the number of Unit_t structures contained within this
 * UnitDefinition_t.
 *
 * @param ud the UnitDefinition_t structure in question
 * 
 * @return an integer representing the number of Unit_t structures in this
 * UnitDefinition_t structure.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return ud->getNumUnits();
}


/**
 * Removes the nth Unit_t object from this UnitDefinition_t object and
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the UnitDefinition_t structure
 * @param n the integer index of the Unit_t sought
 *
 * @return the Unit_t object removed.  As mentioned above, 
 * the caller owns the returned item. NULL is returned if the given index 
 * is out of range.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_removeUnit (UnitDefinition_t *ud, unsigned int n)
{
  if (!ud) return 0;
  return ud->removeUnit(n);
}


LIBSBML_EXTERN
void 
UnitDefinition_simplify(UnitDefinition_t * ud)
{
  UnitDefinition::simplify(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
void 
UnitDefinition_reorder(UnitDefinition_t * ud)
{
  UnitDefinition::reorder(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
UnitDefinition_t * 
UnitDefinition_convertToSI(UnitDefinition_t * ud)
{
  return UnitDefinition::convertToSI(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
int 
UnitDefinition_areIdentical(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  return static_cast<int>(UnitDefinition::areIdentical(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2)));
}

LIBSBML_EXTERN
int 
UnitDefinition_areEquivalent(UnitDefinition_t *ud1 , UnitDefinition_t * ud2)
{
  return static_cast<int>(UnitDefinition::areEquivalent(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2)));
}

LIBSBML_EXTERN
UnitDefinition_t *  
UnitDefinition_combine(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  return UnitDefinition::combine(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2));
}


LIBSBML_EXTERN
const char *
UnitDefinition_printUnits(UnitDefinition_t * ud, int compact)
{
  return safe_strdup(UnitDefinition::printUnits(static_cast<UnitDefinition*>(ud)
    , compact).c_str());
}

/**
 * @return item in this ListOfUnitDefinition with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
UnitDefinition_t *
ListOfUnitDefinitions_getById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfUnitDefinitions *> (lo)->get(sid) : NULL;
}


/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is responsible for
 * deleting it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
ListOfUnitDefinitions_removeById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfUnitDefinitions *> (lo)->remove(sid) : NULL;
}


/** @endcond doxygen-c-only */

LIBSBML_CPP_NAMESPACE_END
