/**
 * @file    UnitDefinition.cpp
 * @brief   Implementations of %SBML's UnitDefinition and ListOfUnitDefinitions.
 * @author  Ben Bornstein
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


/*
 * Creates a new UnitDefinition, optionally with its id and name
 * attributes set.
 */
UnitDefinition::UnitDefinition (const std::string& sid, const std::string& name) :
  SBase(sid, name)
{
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
          SBase     (orig)
        , mUnits    (orig.mUnits)
{
}


/*
 * Assignment operator.
 */
UnitDefinition& UnitDefinition::operator=(const UnitDefinition& rhs)
{
  this->SBase::operator =(rhs);
  mUnits = rhs.mUnits;
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
SBase*
UnitDefinition::clone () const
{
  return new UnitDefinition(*this);
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
 * Adds a copy of the given Unit to this UnitDefinition.
 */
void
UnitDefinition::addUnit (const Unit* u)
{
  /* if the ListOf is empty it doesnt know its parent */
  if (mUnits.size() == 0)
  {
    mUnits.setSBMLDocument(this->getSBMLDocument());
    mUnits.setParentSBMLObject(this);
  }
  
  mUnits.append(u);
}


/*
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
Unit*
UnitDefinition::createUnit ()
{
  Unit* u = new Unit;
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mUnits.size() == 0)
    mUnits.setSBMLDocument(this->getSBMLDocument());
  
  mUnits.appendAndOwn(u);

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

/* utility functions originally in Utils_UnitDefinition
 * declared as static
 */

void
UnitDefinition::simplify(UnitDefinition * ud)
{
  unsigned int n, i;
  ListOfUnits *  units = ud->getListOfUnits();
  Unit * unit;
  UnitKindList kindsList;
  const char * unitKind;

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
          units->remove(n);
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
            units->remove(i);
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
      units->remove(n-1);
    }
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
    units->remove(0);
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
  unsigned int n, p;
  UnitDefinition * newUd = new UnitDefinition();
  UnitDefinition * tempUd;

  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    tempUd = Unit::convertToSI(ud->getUnit(n));
    for (p = 0; p < tempUd->getNumUnits(); p++)
    {
      newUd->addUnit(tempUd->getUnit(p));
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
  unsigned int n;

  /* need to order the unitDefinitions so must make copies
   * since the arguments are const
   */
  UnitDefinition * ud1Temp = new UnitDefinition();//(UnitDefinition*) ud1->clone();
  UnitDefinition * ud2Temp = new UnitDefinition();//(UnitDefinition*) ud2->clone();

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
  UnitDefinition * ud = new UnitDefinition(*ud1);
  for (unsigned int n = 0; n < ud2->getNumUnits(); n++)
  {
    ud->addUnit(ud2->getUnit(n));
  }

  UnitDefinition::simplify(ud);

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
UnitDefinition::printUnits(const UnitDefinition * ud)
{
  std::string unitDef;
  if (!ud || ud->getNumUnits() == 0)
  {
    unitDef = "indeterminable";
  }
  else
  {
    for (unsigned int p = 0; p < ud->getNumUnits(); p++)
    {
	    UnitKind_t kind = ud->getUnit(p)->getKind();
	    int exp = ud->getUnit(p)->getExponent();

      char unit[40];
      sprintf(unit, "%s (exponent = %i)", UnitKind_toString(kind), exp);
      unitDef += unit;
      //msg +=  UnitKind_toString(kind);
      //msg += " (exponent = ";
      //msg += exp;
      //msg +=  ")";

	    if (p + 1 < ud->getNumUnits())
	    {
	      unitDef += ", ";
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

  if (level == 2)
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("id");
    if (version == 3)
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
  attributes.readInto(id, mId, getErrorLog(), true);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 3) 
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
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) stream.writeAttribute("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
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
SBase*
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
    object = new UnitDefinition();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty UnitDefinition_t structure.
 *
 * @return the new UnitDefinition_t structure
 *
 * @note It is worth emphasizing that the attribute "id" value of a
 * UnitDefinition_t is a required attribute.  UnitDefinition_create() does
 * not assign a valid "id" to the constructed unit; instead, it leaves it
 * as the empty string.  Callers are cautioned to set the newly-constructed
 * UnitDefinition_t's "id" using UnitDefinition_setId() soon after calling
 * this function.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void)
{
  return new(nothrow) UnitDefinition;
}


/**
 * Creates a new UnitDefinition_t structure with the given identifier @p id
 * and returns a pointer to the structure.
 * 
 * The permitted values of the identifier @p id @em exclude the predefined
 * base units in SBML and two spelling variants @c "meter" and @c "liter".
 * The following is the set of base unit names which may @em not be used as
 * a value of @p id:
 *
 * <table align="center" style="font-family: Courier, fixed; font-weight: bold; font-size: 12px;" cellspacing="7" border="0">
 * <tr><td>ampere</td><td>gram</td><td>katal</td><td>metre</td><td>second</td><td>watt</td></tr>
 * <tr><td>becquerel</td><td>gray</td><td>kelvin</td><td>mole</td><td>siemens</td><td>weber</td></tr>
 * <tr><td>candela</td><td>henry</td><td>kilogram</td><td>newton</td><td>sievert</td><td>Celsius</td></tr>
 * <tr><td>coulomb</td><td>hertz</td><td>litre</td><td>ohm</td><td>steradian</td><td>meter</td></tr>
 * <tr><td>dimensionless</td><td>item</td><td>lumen</td><td>pascal</td><td>tesla</td><td>liter</td></tr>
 * <tr><td>farad</td><td>joule</td><td>lux</td><td>radian</td><td>volt</td></tr>
 * </table>
 *
 * In addition, there is a set of predefined identifiers for the built-in
 * default units in SBML.  These identifiers are @c substance, @c volume,
 * @c area, @c length, and @c time.  Using one of these values for the
 * attribute @p id of a UnitDefinition_t has the effect of redefining the
 * model-wide default units for the corresponding quantities.  The list
 * of built-in units is given in the table below:
 * @image html built-in-units.jpg "SBML's built-in units"
 * @image latex built-in-units.jpg "SBML's built-in units"
 * 
 * Finally, note that SBML imposes two limitations on redefining the
 * built-in units listed above:
 * 
 * - The UnitDefinition_t of a redefined built-in unit can only
 *   contain a single Unit object within it.
 * 
 * - The value of the "kind" attribute in a Unit instance must be drawn
 *   from one of the values in the second column of the table above.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   UnitDefinition_setId(UnitDefinition_create(), id);
 * @endcode
 *
 * @param id the identifier to assign to the new unit definition.
 *
 * @return the new UnitDefinition_t structure.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *id, const char *name)
{
  return new(nothrow) UnitDefinition(id ? id : "", name ? name : "");
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
 * Sets the attribute "id" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose id is to be set
 * @param id a string, the new identifier for the UnitDefinition_t structure
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *id)
{
  (id == NULL) ? ud->unsetId() : ud->setId(id);
}


/**
 * Sets the attribute "name" of the given UnitDefinition_t structure to a
 * copy of the given string.
 *
 * @param ud the UnitDefinition_t structure whose name is to be set
 * @param name a string, the new name for the UnitDefinition_t structure
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *name)
{
  (name == NULL) ? ud->unsetName() : ud->setName(name);
}


/**
 * Unsets the name of the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t whose name is to be unset.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  ud->unsetName();
}


/**
 * Adds a copy of the given Unit to the given UnitDefinition_t structure.
 *
 * @param ud the UnitDefinition_t structure.
 * @param u the Unit instance to add.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u)
{
  if (u != NULL) ud->addUnit(u);
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
UnitDefinition_printUnits(UnitDefinition_t * ud)
{
  return UnitDefinition::printUnits(static_cast<UnitDefinition*>(ud)).c_str();
}

/** @endcond doxygen-c-only */
