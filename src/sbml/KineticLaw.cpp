/**
 * @file    KineticLaw.cpp
 * @brief   Implementation of KineticLaw.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/KineticLaw.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

LIBSBML_CPP_NAMESPACE_BEGIN

KineticLaw::KineticLaw (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mMath          ( 0              )
 , mTimeUnits     ( ""             )
 , mSubstanceUnits( ""             )
 , mInternalId    ( ""             )

{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


KineticLaw::KineticLaw (SBMLNamespaces * sbmlns) :
   SBase          ( sbmlns         )
 , mMath          ( 0              )
 , mTimeUnits     ( ""             )
 , mSubstanceUnits( ""             )
 , mInternalId    ( ""             )

{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
KineticLaw::KineticLaw() :
  SBase()
{
}

/** @endcond doxygen-libsbml-internal */
                          

/*
 * Destroys this KineticLaw.
 */
KineticLaw::~KineticLaw ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this KineticLaw.
 */
KineticLaw::KineticLaw (const KineticLaw& orig) :
   SBase          ( orig                 )
 , mFormula       ( orig.mFormula        )
 , mMath          ( 0                    )
 , mParameters    ( orig.mParameters     )
 , mLocalParameters    ( orig.mLocalParameters     )
 , mTimeUnits     ( orig.mTimeUnits      )
 , mSubstanceUnits( orig.mSubstanceUnits )
 , mInternalId    ( orig.mInternalId     )
{
  if (orig.mMath) 
  {
    mMath = orig.mMath->deepCopy();
    mMath->setParentSBMLObject(this);
  }

  /* since a kinetic Law has children we need to re-establish the
   * parentage of these children
   */
  if (orig.getNumParameters() > 0)
  {
    mParameters.setParentSBMLObject(this);
  }
  if (orig.getNumLocalParameters() > 0)
  {
    mLocalParameters.setParentSBMLObject(this);
  }
}


/*
 * Assignment operator
 */
KineticLaw& KineticLaw::operator=(const KineticLaw& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mFormula        = rhs.mFormula        ;
    mTimeUnits      = rhs.mTimeUnits      ;
    mSubstanceUnits = rhs.mSubstanceUnits ;
    mParameters     = rhs.mParameters     ;
    mLocalParameters     = rhs.mLocalParameters     ;
    mInternalId     = rhs.mInternalId     ;
    
    if (rhs.getNumParameters() > 0)
    {
      mParameters.setParentSBMLObject(this);
    }
    
    if (rhs.getNumLocalParameters() > 0)
    {
      mLocalParameters.setParentSBMLObject(this);
    }
    
    delete mMath;
    if (rhs.mMath) 
    {
      mMath = rhs.mMath->deepCopy();
      mMath->setParentSBMLObject(this);
    }
    else
    {
      mMath = 0;
    }
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
KineticLaw::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  mParameters.accept(v);
  v.leave(*this);

  return true;
}


/*
 * @return a (deep) copy of this KineticLaw.
 */
KineticLaw*
KineticLaw::clone () const
{
  return new KineticLaw(*this);
}


/*
 * @return the formula of this KineticLaw.
 */
const string&
KineticLaw::getFormula () const
{
  if (mFormula.empty() == true && mMath != 0)
  {
    char* s  = SBML_formulaToString(mMath);
    mFormula = s;

    free(s);
  }

  return mFormula;
}


/*
 * @return the math of this KineticLaw.
 */
const ASTNode*
KineticLaw::getMath () const
{
  if (mMath == 0 && mFormula.empty() == false)
  {
    mMath = SBML_parseFormula( mFormula.c_str() );
  }

  return mMath;
}


/*
 * @return the timeUnits of this KineticLaw.
 */
const string&
KineticLaw::getTimeUnits () const
{
  return mTimeUnits;
}


/*
 * @return the substanceUnits of this KineticLaw.
 */
const string&
KineticLaw::getSubstanceUnits () const
{
  return mSubstanceUnits;
}


/*
 * @return true if the formula (or equivalently the math) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/*
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetMath () const
{
  /* if the formula has been set but it is not a correct formula
   * it cannot be correctly transferred to an ASTNode so in fact
   * getMath will return NULL
   *
   * this function needs to test for this
   */
  bool formula = isSetFormula();
  
  if (formula)
  {
    const ASTNode *temp = getMath();
    if (temp == NULL)
      formula = false;
  }
    
  return formula;
}


/*
 * @return true if the timeUnits of this KineticLaw has been set, false
 * otherwise.
 */
bool
KineticLaw::isSetTimeUnits () const
{
  return (mTimeUnits.empty() == false);
}


/*
 * @return true if the substanceUnits of this KineticLaw has been set,
 * false otherwise.
 */
bool
KineticLaw::isSetSubstanceUnits () const
{
  return (mSubstanceUnits.empty() == false);
}


/*
 * Sets the formula of this KineticLaw to a copy of formula.
 */
int
KineticLaw::setFormula (const std::string& formula)
{
  ASTNode * math = SBML_parseFormula(formula.c_str());
  if (formula == "")
  {
    mFormula.erase();
    delete mMath;
    mMath = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL || !(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    mFormula = formula;

    if (mMath)
    {
      delete mMath;
      mMath = 0;
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the math of this KineticLaw to a copy of the given ASTNode.
 */
int
KineticLaw::setMath (const ASTNode* math)
{
  if (mMath == math) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = 0;
    mFormula.erase();
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != 0) ? math->deepCopy() : 0;
    if (mMath) mMath->setParentSBMLObject(this);
    mFormula.erase();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
int
KineticLaw::setTimeUnits (const std::string& sid)
{
  /* only in L1 and L2V1 */
  if ((getLevel() == 2 && getVersion() > 1)
    || getLevel() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTimeUnits = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
int
KineticLaw::setSubstanceUnits (const std::string& sid)
{
  /* only in L1 and L2V1 */
  if ((getLevel() == 2 && getVersion() > 1)
    || getLevel() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSubstanceUnits = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the timeUnits of this KineticLaw.
 */
int
KineticLaw::unsetTimeUnits ()
{
  /* only in L1 and L2V1 */
  if ((getLevel() == 2 && getVersion() > 1)
    || getLevel() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  mTimeUnits.erase();

  if (mTimeUnits.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the substanceUnits of this KineticLaw.
 */
int
KineticLaw::unsetSubstanceUnits ()
{
  /* only in L1 and L2V1 */
  if ((getLevel() == 2 && getVersion() > 1)
    || getLevel() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
 
  mSubstanceUnits.erase();
  
  if (mSubstanceUnits.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Adds a copy of the given Parameter to this KineticLaw.
 */
int
KineticLaw::addParameter (const Parameter* p)
{
  if (p == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(p->hasRequiredAttributes()) || !(p->hasRequiredElements()))
  {
    /* 
     * in an attempt to make existing code work with the new localParameter
     * class this requires a further check
     */
    if (getLevel() < 3)
    {
      return LIBSBML_INVALID_OBJECT;
    }
    else
    {
      /* hack so this will deal with local parameters */
      LocalParameter *lp = new LocalParameter(*p);//->getSBMLNamespaces());

      if (!(lp->hasRequiredAttributes()) || !(lp->hasRequiredElements()))
      {
        return LIBSBML_INVALID_OBJECT;
      }
      else
      {

        /* if the ListOf is empty it doesnt know its parent */
        if (mLocalParameters.size() == 0)
        {
          mLocalParameters.setSBMLDocument(this->getSBMLDocument());
          mLocalParameters.setParentSBMLObject(this);
        }
        
        mLocalParameters.append(lp);

        return LIBSBML_OPERATION_SUCCESS;
      }
    }
  }
  else if (getLevel() != p->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != p->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getParameter(p->getId()) != NULL)
  {
    // an parameter with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mParameters.size() == 0)
    {
      mParameters.setSBMLDocument(this->getSBMLDocument());
      mParameters.setParentSBMLObject(this);
    }
    
    mParameters.append(p);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Adds a copy of the given LocalParameter to this KineticLaw.
 */
int
KineticLaw::addLocalParameter (const LocalParameter* p)
{
  if (p == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(p->hasRequiredAttributes()) || !(p->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != p->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != p->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getLocalParameter(p->getId()) != NULL)
  {
    // an parameter with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mLocalParameters.size() == 0)
    {
      mLocalParameters.setSBMLDocument(this->getSBMLDocument());
      mLocalParameters.setParentSBMLObject(this);
    }
    
    mLocalParameters.append(p);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Parameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
Parameter*
KineticLaw::createParameter ()
{
  if (getLevel() < 3)
  {
    Parameter* p = 0;
    
    try
    {
      p = new Parameter(getSBMLNamespaces());
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
    if (mParameters.size() == 0)
    {
      mParameters.setSBMLDocument(this->getSBMLDocument());
      mParameters.setParentSBMLObject(this);
    }
    
    if (p) mParameters.appendAndOwn(p);

    return p;
  }
  else
  {
    LocalParameter *p = 0;
    try
    {
      p = new LocalParameter(getSBMLNamespaces());
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
    if (mLocalParameters.size() == 0)
    {
      mLocalParameters.setSBMLDocument(this->getSBMLDocument());
      mLocalParameters.setParentSBMLObject(this);
    }
    
    if (p) mLocalParameters.appendAndOwn(p);

    return static_cast <Parameter *> (p);
  }
}


/*
 * Creates a new LocalParameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
LocalParameter*
KineticLaw::createLocalParameter ()
{
  LocalParameter* p = 0;

  try
  {
    p = new LocalParameter(getSBMLNamespaces());
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
  if (mLocalParameters.size() == 0)
  {
    mLocalParameters.setSBMLDocument(this->getSBMLDocument());
    mLocalParameters.setParentSBMLObject(this);
  }
  
  if (p) mLocalParameters.appendAndOwn(p);

  return p;
}


/*
 * @return the list of Parameters for this KineticLaw.
 */
const ListOfParameters*
KineticLaw::getListOfParameters () const
{
  if (getLevel() < 3)
    return &mParameters;
  else
    return static_cast <const ListOfParameters *> (&mLocalParameters);
}


/*
 * @return the list of Parameters for this KineticLaw.
 */
ListOfParameters*
KineticLaw::getListOfParameters ()
{
  if (getLevel() < 3)
    return &mParameters;
  else
    return static_cast <ListOfParameters *> (&mLocalParameters);
}


/*
 * @return the list of LocalParameters for this KineticLaw.
 */
const ListOfLocalParameters*
KineticLaw::getListOfLocalParameters () const
{
  return &mLocalParameters;
}


/*
 * @return the list of LocalParameters for this KineticLaw.
 */
ListOfLocalParameters*
KineticLaw::getListOfLocalParameters ()
{
  return &mLocalParameters;
}


/*
 * @return the nth Parameter of this KineticLaw.
 */
const Parameter*
KineticLaw::getParameter (unsigned int n) const
{
  if (getLevel() < 3)
    return static_cast<const Parameter*>( mParameters.get(n) );
  else
    return static_cast<const Parameter*>( mLocalParameters.get(n) );

}


/*
 * @return the nth Parameter of this KineticLaw.
 */
Parameter*
KineticLaw::getParameter (unsigned int n)
{
  if (getLevel() < 3)
    return static_cast<Parameter*>( mParameters.get(n) );
  else
    return static_cast<Parameter*>( mLocalParameters.get(n) );
}


/*
 * @return the nth LocalParameter of this KineticLaw.
 */
const LocalParameter*
KineticLaw::getLocalParameter (unsigned int n) const
{
  return static_cast<const LocalParameter*>( mLocalParameters.get(n) );
}


/*
 * @return the nth LocalParameter of this KineticLaw.
 */
LocalParameter*
KineticLaw::getLocalParameter (unsigned int n)
{
  return static_cast<LocalParameter*>( mLocalParameters.get(n) );
}


/*
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
const Parameter*
KineticLaw::getParameter (const std::string& sid) const
{
  if (getLevel() < 3)
    return static_cast<const Parameter*>( mParameters.get(sid) );
  else
    return static_cast<const Parameter*>( mLocalParameters.get(sid) );
}


/*
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
Parameter*
KineticLaw::getParameter (const std::string& sid)
{
  if (getLevel() < 3)
    return static_cast<Parameter*>( mParameters.get(sid) );
  else
    return static_cast<Parameter*>( mLocalParameters.get(sid) );
}


/*
 * @return the LocalParameter in this kineticLaw with the given id or NULL if
 * no such LocalParameter exists.
 */
const LocalParameter*
KineticLaw::getLocalParameter (const std::string& sid) const
{
  return static_cast<const LocalParameter*>( mLocalParameters.get(sid) );
}


/*
 * @return the LocalParameter in this kineticLaw with the given id or NULL if
 * no such LocalParameter exists.
 */
LocalParameter*
KineticLaw::getLocalParameter (const std::string& sid)
{
  return static_cast<LocalParameter*>( mLocalParameters.get(sid) );
}


/*
 * @return the number of Parameters in this KineticLaw.
 */
unsigned int
KineticLaw::getNumParameters () const
{
  if (getLevel() < 3)
    return mParameters.size();
  else
    return mLocalParameters.size();
}

/*
 * @return the number of LocalParameters in this KineticLaw.
 */
unsigned int
KineticLaw::getNumLocalParameters () const
{
  return mLocalParameters.size();
}

/*
  * Calculates and returns a UnitDefinition that expresses the units
  * returned by the math expression of this KineticLaw.
  */
UnitDefinition * 
KineticLaw::getDerivedUnitDefinition()
{
  if (!isSetMath())
    return NULL;
  /* if we have the whole model but it is not in a document
   * it is still possible to determine the units
   */
  Model * m = static_cast <Model *> (getAncestorOfType(SBML_MODEL));

  if (m)
  {
    if (!m->isPopulatedListFormulaUnitsData())
    {
      m->populateListFormulaUnitsData();
    }
    
    if (m->getFormulaUnitsData(getInternalId(), getTypeCode()))
    {
      return m->getFormulaUnitsData(getInternalId(), getTypeCode())
                                             ->getUnitDefinition();
    }
    else
    {
      return NULL;
    }  
  }
  else
  {
    return NULL;
  }
}


/*
  * Constructs and returns a UnitDefinition that expresses the units of this 
  * Compartment.
  */
const UnitDefinition *
KineticLaw::getDerivedUnitDefinition() const
{
  return const_cast <KineticLaw *> (this)->getDerivedUnitDefinition();
}


/*
 * Predicate returning @c true or @c false depending on whether 
 * the math expression of this KineticLaw contains
 * parameters/numbers with undeclared units that cannot be ignored.
 */
bool 
KineticLaw::containsUndeclaredUnits()
{
  if (!isSetMath())
    return false;
  /* if we have the whole model but it is not in a document
   * it is still possible to determine the units
   */
  Model * m = static_cast <Model *> (getAncestorOfType(SBML_MODEL));

  if (m)
  {
    if (!m->isPopulatedListFormulaUnitsData())
    {
      m->populateListFormulaUnitsData();
    }
    
    if (m->getFormulaUnitsData(getInternalId(), getTypeCode()))
    {
      return m->getFormulaUnitsData(getInternalId(), getTypeCode())
      ->getContainsUndeclaredUnits();
    }
    else
    {
      return false;
    }  
  }
  else
  {
    return false;
  }
}


bool 
KineticLaw::containsUndeclaredUnits() const
{
  return const_cast<KineticLaw *> (this)->containsUndeclaredUnits();
}

/**
 * Removes the nth Parameter object in the list of local parameters 
 * in this KineticLaw instance.
 */
Parameter* 
KineticLaw::removeParameter (unsigned int n)
{
  return mParameters.remove(n);  
}


/**
 * Removes the nth LocalParameter object in the list of local parameters 
 * in this KineticLaw instance.
 */
LocalParameter* 
KineticLaw::removeLocalParameter (unsigned int n)
{
  return mLocalParameters.remove(n);  
}


/**
 * Removes a Parameter object with the given identifier in the list of
 * local parameters in this KineticLaw instance.
 */
Parameter* 
KineticLaw::removeParameter (const std::string& sid)
{
  return mParameters.remove(sid);
}


/**
 * Removes a LocalParameter object with the given identifier in the list of
 * local parameters in this KineticLaw instance.
 */
LocalParameter* 
KineticLaw::removeLocalParameter (const std::string& sid)
{
  return mLocalParameters.remove(sid);
}


/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
KineticLaw::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mParameters.setSBMLDocument(d);
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
KineticLaw::setParentSBMLObject (SBase* sb)
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
KineticLaw::getTypeCode () const
{
  return SBML_KINETIC_LAW;
}


/*
 * @return the name of this element ie "kineticLaw".
 */
const string&
KineticLaw::getElementName () const
{
  static const string name = "kineticLaw";
  return name;
}


bool 
KineticLaw::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for kineticLaw: formula (L1 only) */

  if (getLevel() == 1 && !isSetFormula())
    allPresent = false;

  return allPresent;
}


bool 
KineticLaw::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for kineticlaw: math */

  if (!isSetMath())
    allPresent = false;

  return allPresent;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
KineticLaw::getElementPosition () const
{
  return 4;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
KineticLaw::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() > 1 && isSetMath() ) writeMathML(getMath(), stream);
  if ( getLevel() < 3 && getNumParameters() > 0 ) mParameters.write(stream);
  if ( getLevel() > 2 && getNumLocalParameters() > 0 ) 
    mLocalParameters.write(stream);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
KineticLaw::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "listOfParameters")
  {
    if (mParameters.size() != 0)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfParameters> elements is permitted "
	       "in a given <kineticLaw> element.");
    }
    return &mParameters;
  }
  
  if (name == "listOfLocalParameters")
  {
    if (mLocalParameters.size() != 0)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfLocalParameters> elements is permitted "
	       "in a given <kineticLaw> element.");
    }
    return &mLocalParameters;
  }
  return 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
KineticLaw::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    // if this is level 1 there shouldnt be any math!!!
    if (getLevel() == 1) 
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "SBML Level 1 does not support MathML.");
      delete mMath;
      return false;
    }

    if (getNumParameters() > 0) logError(IncorrectOrderInKineticLaw);

    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken elem = stream.peek();
    unsigned int match = 0;
    int n;
    if (elem.getNamespaces().getLength() != 0)
    {
      for (n = 0; n < elem.getNamespaces().getLength(); n++)
      {
        if (!strcmp(elem.getNamespaces().getURI(n).c_str(), "http://www.w3.org/1998/Math/MathML"))
        {
          match = 1;
          break;
        }
      }
    }
    if (match == 0)
    {
      if( mSBML->getNamespaces() != NULL)
      /* check for implicit declaration */
      {
        for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
        {
          if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(), 
                                                     "http://www.w3.org/1998/Math/MathML"))
          {
            match = 1;
            break;
          }
        }
      }
    }
    if (match == 0)
    {
      logError(InvalidMathElement);
    }
    delete mMath;
    mMath = readMathML(stream);
    if (mMath) mMath->setParentSBMLObject(this);
    read  = true;
  }

  return read;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
KineticLaw::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  if (level == 1)
  {
    expectedAttributes.push_back("formula");
    expectedAttributes.push_back("timeUnits");
    expectedAttributes.push_back("substanceUnits");
  }
  else
  {
    expectedAttributes.push_back("metaid");

    if (level == 2 && version == 1)
    {
      expectedAttributes.push_back("timeUnits");
      expectedAttributes.push_back("substanceUnits");
    }

    if (!(level == 2 && version == 1))
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
      logUnknownAttribute(name, level, version, "<kineticLaw>");
    }
  }

  if (level == 1) 
  {
    //
    // formula: string  { use="required" }  (L1v1->)
    //
    attributes.readInto("formula", mFormula, getErrorLog(), true);

    //
    // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
    // removed in l2v3
    //
    attributes.readInto("timeUnits", mTimeUnits);

    //
    // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
    // removed in l2v3
    //
    attributes.readInto("substanceUnits", mSubstanceUnits);
  }
  else
  {
    if (level == 2 && version == 1)
    {
      //
      // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
      // removed in l2v2
      //
      attributes.readInto("timeUnits", mTimeUnits);

      //
      // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
      // removed in l2v2
      //
      attributes.readInto("substanceUnits", mSubstanceUnits);
    }

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
    //
    if (!(level == 2 && version == 1)) 
      mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
KineticLaw::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (level == 1) 
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    stream.writeAttribute("formula", getFormula());

    //
    // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
    // removed in l2v2
    //
    stream.writeAttribute("timeUnits", mTimeUnits);

    //
    // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
    // removed in l2v2
    //
    stream.writeAttribute("substanceUnits", mSubstanceUnits);
  }
  else
  {
    if (level == 2 && version == 1)
    {
      //
      // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
      // removed in l2v2
      //
      stream.writeAttribute("timeUnits", mTimeUnits);

      //
      // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
      // removed in l2v2
      //
      stream.writeAttribute("substanceUnits", mSubstanceUnits);
    }

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
    //
    if (!(level == 2 && version == 1)) 
      SBO::writeTerm(stream, mSBOTerm);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new KineticLaw_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * KineticLaw
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * KineticLaw
 *
 * @return a pointer to the newly created KineticLaw_t structure.
 *
 * @note Once a KineticLaw has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the KineticLaw.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (unsigned int level, unsigned int version)
{
  try
  {
    KineticLaw* obj = new KineticLaw(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new KineticLaw_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this KineticLaw
 *
 * @return a pointer to the newly created KineticLaw_t structure.
 *
 * @note Once a KineticLaw has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the KineticLaw.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    KineticLaw* obj = new KineticLaw(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Frees the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl)
{
  delete kl;
}


/**
 * Returns a deep copy of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return a (deep) copy of this KineticLaw_t structure.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_clone (const KineticLaw_t *kl)
{
  return kl->clone();
}


/**
 * Returns a list of XMLNamespaces_t associated with this KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
KineticLaw_getNamespaces(KineticLaw_t *kl)
{
  return kl->getNamespaces();
}

/**
 * Gets the mathematical expression of this KineticLaw_t structure as a
 * formula in text-string form.
 *
 * This is fundamentally equivalent to KineticLaw_getMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the formula of this KineticLaw_t structure.
 *
 * @see KineticLaw_getMath().
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl)
{
  return kl->isSetFormula() ? kl->getFormula().c_str() : NULL;
}


/**
 * Gets the mathematical expression of this KineticLaw_t structure as an
 * ASTNode_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_getFormula().  The latter
 * is provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the formula in the form of an ASTNode_t structure
 *
 * @see KineticLaw_getFormula().
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl)
{
  return kl->getMath();
}


/**
 * Gets the value of the "timeUnits" attribute of the given
 * KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the "timeUnits" attribute value
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl)
{
  return kl->isSetTimeUnits() ? kl->getTimeUnits().c_str() : NULL;
}


/**
 * Gets the value of the "substanceUnits" attribute of the given
 * KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the "substanceUnits" attribute value
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl)
{
  return kl->isSetSubstanceUnits() ? kl->getSubstanceUnits().c_str() : NULL;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "formula" attribute of the given KineticLaw_t structure is
 * set.
 *
 * This is fundamentally equivalent to KineticLaw_isSetMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the formula (or equivalently the
 * "math" subelement) of the given KineticLaw_t structure has been set,
 * zero (meaning false) otherwise.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetFormula() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "math" subelement of the given KineticLaw_t structure is
 * set.
 *
 * This is fundamentally equivalent to KineticLaw_isSetFormula().  The
 * latter provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "math" subelement of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetMath() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "timeUnits" attribute of the given KineticLaw_t structure is
 * set.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "timeUnits" attribute of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetTimeUnits() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "timeUnits" attribute of the given KineticLaw_t structure is
 * set.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "timeUnits" attribute of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetSubstanceUnits() );
}


/**
 * Sets the formula of the given KineticLaw_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_setMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param formula the mathematical expression, in text-string form.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
int
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula)
{
  return kl->setFormula(formula ? formula : "");
}


/**
 * Sets the formula of the given KineticLaw_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_setFormula().  The latter
 * provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param math an ASTNode_t structure representing the mathematical formula
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math)
{
  return kl->setMath(math);
}


/**
 * Sets the "timeUnits" attribute of the given KineticLaw_t structure.
 *
 * The identifier string @p sid is copied.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the units
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "timeUnits" attribute.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid)
{
  return (sid == NULL) ? kl->unsetTimeUnits() : kl->setTimeUnits(sid);
}


/**
 * Sets the "substanceUnits" attribute of the given KineticLaw_t structure.
 *
 * The identifier string @p sid is copied.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the units
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "substanceUnits" attribute.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid)
{
  return (sid == NULL) ? kl->unsetSubstanceUnits() : kl->setSubstanceUnits(sid);
}


/**
 * Unsets the "timeUnits" attribute of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_unsetTimeUnits (KineticLaw_t *kl)
{
  return kl->unsetTimeUnits();
}


/**
 * Unsets the "substanceUnits" attribute of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl)
{
  return kl->unsetSubstanceUnits();
}


/**
 * Adds a copy of the given Parameter_t structure to the list of local
 * parameters in the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param p a pointer to a Parameter_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_DUPLICATE_OBJECT_ID
 */
LIBSBML_EXTERN
int
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p)
{
  return kl->addParameter(p);
}


/**
 * Adds a copy of the given LocalParameter_t structure to the list of local
 * parameters in the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param p a pointer to a LocalParameter_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_DUPLICATE_OBJECT_ID
 */
LIBSBML_EXTERN
int
KineticLaw_addLocalParameter (KineticLaw_t *kl, const LocalParameter_t *p)
{
  return kl->addLocalParameter(p);
}


/**
 * Creates a new Parameter_t structure, adds it to the given KineticLaw_t
 * structures's list of local parameters, and returns a pointer to the
 * Parameter_t created.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @return a pointer to a Parameter_t structure
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl)
{
  return kl->createParameter();
}


/**
 * Creates a new LocalParameter_t structure, adds it to the given KineticLaw_t
 * structures's list of local parameters, and returns a pointer to the
 * Parameter_t created.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @return a pointer to a LocalParameter_t structure
 */
LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_createLocalParameter (KineticLaw_t *kl)
{
  return kl->createLocalParameter();
}


/**
 * Get the list of local parameters defined for the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return a list of Parameters
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl)
{
  return kl->getListOfParameters();
}


/**
 * Get the list of local parameters defined for the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return a list of LocalParameters
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfLocalParameters (KineticLaw_t *kl)
{
  return kl->getListOfLocalParameters();
}


/**
 * Get the nth parameter in the list of local parameters in the
 * given KineticLaw_t structure.
 *
 * Callers should first find out how many parameters are in the list by
 * calling KineticLaw_getNumParameters().
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param n the index of the Parameter_t structure sought
 * 
 * @return a pointer to the Parameter_t structure
 *
 * @see KineticLaw_getNumParameters().
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n)
{
  return kl->getParameter(n);
}


/**
 * Get the nth parameter in the list of local parameters in the
 * given KineticLaw_t structure.
 *
 * Callers should first find out how many parameters are in the list by
 * calling KineticLaw_getNumLocalParameters().
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param n the index of the LocalParameter_t structure sought
 * 
 * @return a pointer to the LocalParameter_t structure
 *
 * @see KineticLaw_getNumLocalParameters().
 */
LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_getLocalParameter (KineticLaw_t *kl, unsigned int n)
{
  return kl->getLocalParameter(n);
}


/**
 * Get a parameter with identifier "id" out of the list of local
 * parameters defined for the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the Parameter_t structure sought
 * 
 * @return the Parameter_t structure with the given id, or NULL if no such
 * Parameter_t exists in the given KineticLaw_t structure.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid)
{
  return (sid != NULL) ? kl->getParameter(sid) : 0;
}


/**
 * Get a parameter with identifier "id" out of the list of local
 * parameters defined for the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the LocalParameter_t structure sought
 * 
 * @return the LocalParameter_t structure with the given id, or NULL if no such
 * LocalParameter_t exists in the given KineticLaw_t structure.
 */
LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_getLocalParameterById (KineticLaw_t *kl, const char *sid)
{
  return (sid != NULL) ? kl->getLocalParameter(sid) : 0;
}


/**
 * Get the number of local parameters defined in the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the number of Parameter_t structures in the given KineticLaw_t
 * structure.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return kl->getNumParameters();
}


/**
 * Get the number of local parameters defined in the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the number of LocalParameter_t structures in the given KineticLaw_t
 * structure.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumLocalParameters (const KineticLaw_t *kl)
{
  return kl->getNumLocalParameters();
}

/**
  * Calculates and returns a UnitDefinition_t that expresses the units
  * returned by the math expression of this KineticLaw_t.
  *
  * @return a UnitDefinition_t that expresses the units of the math 
  * expression of this KineticLaw_t.
  *
  * Note that the functionality that facilitates unit analysis depends 
  * on the model as a whole.  Thus, in cases where the object has not 
  * been added to a model or the model itself is incomplete,
  * unit analysis is not possible and this method will return NULL.
  *
  * @note The units are calculated by applying the mathematics 
  * from the expression to the units of the <ci> elements used 
  * within the expression. Where there are parameters/numbers
  * with undeclared units the UnitDefinition_t returned by this
  * function may not accurately represent the units of the expression.
  *
  * @see KineticLaw_containsUndeclaredUnits()
  */
LIBSBML_EXTERN
UnitDefinition_t * 
KineticLaw_getDerivedUnitDefinition(KineticLaw_t *kl)
{
  return kl->getDerivedUnitDefinition();
}


/**
  * Predicate returning @c true or @c false depending on whether 
  * the math expression of this KineticLaw_t contains
  * parameters/numbers with undeclared units.
  * 
  * @return @c true if the math expression of this KineticLaw_t
  * includes parameters/numbers 
  * with undeclared units, @c false otherwise.
  *
  * @note a return value of @c true indicates that the UnitDefinition_t
  * returned by the getDerivedUnitDefinition function may not 
  * accurately represent the units of the expression.
  *
  * @see KineticLaw_getDerivedUnitDefinition()
  */
LIBSBML_EXTERN
int 
KineticLaw_containsUndeclaredUnits(KineticLaw_t *kl)
{
  return static_cast<int>(kl->containsUndeclaredUnits());
}


/**
 * Removes the nth Parameter_t object from the list of local parameters
 * in this KineticLaw_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Parameter_t sought
 *
 * @return the Parameter_t object removed.  As mentioned above, 
 * the caller owns the returned item. NULL is returned if the given index 
 * is out of range.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_removeParameter (KineticLaw_t *kl, unsigned int n)
{
  if (!kl) return 0;
  return kl->removeParameter(n);
}


/**
 * Removes the nth LocalParameter_t object from the list of local parameters
 * in this KineticLaw_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the Model_t structure
 * @param n the integer index of the LocalParameter_t sought
 *
 * @return the LocalParameter_t object removed.  As mentioned above, 
 * the caller owns the returned item. NULL is returned if the given index 
 * is out of range.
 */
LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_removeLocalParameter (KineticLaw_t *kl, unsigned int n)
{
  if (!kl) return 0;
  return kl->removeLocalParameter(n);
}


/**
 * Removes the Parameter_t object with the given "id" attribute
 * from the list of local parameters in this KineticLaw_t object and 
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the KineticLaw_t structure
 * @param sid the string of the "id" attribute of the Parameter_t sought
 *
 * @return the Parameter_t object removed.  As mentioned above, the 
 * caller owns the returned object. NULL is returned if no KineticLaw_t
 * object with the identifier exists in this KineticLaw_t object.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_removeParameterById (KineticLaw_t *kl, const char *sid)
{
  if (!kl) return 0;
  return kl->removeParameter(sid);
}


/**
 * Removes the LocalParameter_t object with the given "id" attribute
 * from the list of local parameters in this KineticLaw_t object and 
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the KineticLaw_t structure
 * @param sid the string of the "id" attribute of the LocalParameter_t sought
 *
 * @return the LocalParameter_t object removed.  As mentioned above, the 
 * caller owns the returned object. NULL is returned if no KineticLaw_t
 * object with the identifier exists in this KineticLaw_t object.
 */
LIBSBML_EXTERN
LocalParameter_t *
KineticLaw_removeLocalParameterById (KineticLaw_t *kl, const char *sid)
{
  if (!kl) return 0;
  return kl->removeLocalParameter(sid);
}


/** @endcond doxygen-c-only */

LIBSBML_CPP_NAMESPACE_END

